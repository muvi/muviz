unit VisFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, PresetType, MFileUtil, StdTags, MStrings,
  TagType, AdvFunc, TVSPType, TVSPSources, TVSPSourceUtil, csl, HashMap,
  TVSPBasicSources, LinkedList, StdPermissions, ProgressHandler;

procedure LoadFromStream(AStream: TStream; AProgressHandler: TProgressHandler = nil; AReadHeader: Boolean = true);
procedure LoadFromFile(AFileName: string; AProgressHandler: TProgressHandler = nil);

procedure SaveToStream(AStream: TStream; ATag: IString; AProgressHandler: TProgressHandler = nil);
procedure SaveToStream(AStream: TStream; AProgressHandler: TProgressHandler = nil);
procedure SaveToFile(AFileName: string; ATag: IString; AProgressHandler: TProgressHandler = nil);
procedure SaveToFile(AFileName: string; AProgressHandler: TProgressHandler = nil);

const
  ParamOFileID    = 'Params  v1';
  ParamFileID     = MuviFileID + ParamOFileID;
  ParamFileIDSize = MuviFileIDSize + GenerelFileIDSize;

type
  EVisFileException = class (Exception)
  end;

  EVFInvalidHeader = class (EVisFileException)
  end;

  EVFInvalidSize   = class (EVisFileException)
  end;

implementation

type
  TParamSaveBuffer = packed record
    Sources: packed array [0..TVSPMAXSOURCES-1] of TTVSPSrcID;
    Bytes  : packed array [0..TVSPMAXPARAMVALUESIZE-1] of Byte;
  end;

var
  LParamSaveBuffer: TParamSaveBuffer;

{%REGION TCollectedSource}

type
  TCollectedSource = class (TTVSPSourceKey)
  strict private
    FSource: ITVSPSource;
  public
    constructor Create(AID: TTVSPSrcID);
    procedure Collect;
    property Source: ITVSPSource read FSource;
  end;

constructor TCollectedSource.Create(AID: TTVSPSrcID);
begin
  inherited Create(AID.Size, AID.ID);
end;

procedure TCollectedSource.Collect;
begin
  FSource:=SourceUtil[ID];
end;

{%ENDREGION}
{%REGION Load}

procedure LoadParamFromStream(AStream: TStream; AVisualisation: IPVisualisation);
var
  I     : Integer;
  AID   : TPParamID;
  AParam: IPParam;
begin
  //read id
  AID.Name:=LoadText(AStream);
  AStream.Read(AID.&Type, SizeOf(AID.&Type));
  AParam:=AVisualisation[AID];
  //read content
  AStream.Read(LParamSaveBuffer, TVSPParamMemSize(AID.&Type));
  AParam.FromStream(LParamSaveBuffer, TPNOLIMIT, NULLVISID);
end;

function LoadSourcesFromStream(AStream: TStream; AProgressHandler: TProgressHandler = nil): TList;
var
  AID          : TTVSPSrcID;
  ATmpSrc      : Pointer;
  ACollectedSrc: TCollectedSource;
  AStart       : Int64;
begin
  AStart:=AStream.Position;
  //have to keep the sources in memory to prevent them from beeing destroyed
  //by reference counting
  Result:=TLinkedList.Create;
  while AStream.Position < AStream.Size do begin;
    //read id
    if AStream.Read(AID, SizeOf(AID)) <> SizeOf(AID)
      then raise EVFInvalidSize.Create('Invalid source size');
    GetMem(ATmpSrc, AID.Size);

    //inform about progress
    if Assigned(AProgressHandler)
      then AProgressHandler.SetProgress(AStream.Position - AStart, 'Reading source [' + IntToStr(AID.Size) + ']' + GUIDToString(AID.ID));

    //read content
    if AStream.Read(ATmpSrc^, AID.Size) <> AID.Size then begin
      FreeMem(ATmpSrc, AID.Size);
      raise EVFInvalidSize.Create('Invalid size of source content');
    end;
    ACollectedSrc:=TCollectedSource.Create(AID);
    ACollectedSrc.Collect;
    Result.Add(ACollectedSrc);
    ACollectedSrc.Source.&Set(0, AID.Size, ATmpSrc^);
    FreeMem(ATmpSrc, AID.Size);
  end;
end;

procedure LoadFromStream(AStream: TStream; AProgressHandler: TProgressHandler = nil; AReadHeader: Boolean = true);
var
  AHeader                    : TMuviFileID;
  ASourcesStart, ASourcesSize: Int64;
  AID                        : TPPresetID;
  APreset                    : IPVisualisation;
  ATag                       : IString;
  ATagCount                  : LongInt;
  I                          : Cardinal;
  AParamCount                : LongWord;
  ASources                   : TList;
begin
  if Assigned(AProgressHandler)
    then AProgressHandler.Max:=AStream.Size;

  //read header
  if AReadHeader then begin
    AHeader:=ReadFileID(AStream);
    if AHeader <> ParamFileID
      then raise EVFInvalidHeader.Create('File header does not match: Expected "' + ParamFileID + '" but got "' + AHeader + '"');
  end;

  //read source start position
  if AStream.Read(ASourcesStart, SizeOf(ASourcesStart)) <> SizeOf(ASourcesStart)
    then raise EVFInvalidSize.Create('Invalid file size: Source start address not found');
  if (ASourcesStart < AStream.Position) or (ASourcesStart > AStream.Size)
    then raise EVFInvalidSize.Create('Invalid file size: Source start address invalid');

  //read sources
  //have to read the sources before the presets, because pointers use them.
  //Otherwise, the pointer direction is wrong and even the param map will
  //conatain wrong ids
  AStream.Position:=ASourcesStart;
  ASources:=LoadSourcesFromStream(AStream, AProgressHandler);
  AStream.Position:=ParamFileIDSize + SizeOf(ASourcesStart);
  ASourcesSize:=AStream.Size - ASourcesStart;

  //read presets
  while AStream.Position < ASourcesStart do begin
    //read ID
    if AStream.Read(AID, SizeOf(AID)) <> SizeOf(AID)
      then raise EVFInvalidSize.Create('Invalid file size: Preset id not found');
    APreset:=PresetUtil.Presets[AID];

    //inform about progress
    if Assigned(AProgressHandler)
      then AProgressHandler.SetProgress(AStream.Position + ASourcesSize, 'Reading preset ' + GUIDToString(AID));

    //read tags
    if AStream.Read(ATagCount, SizeOf(ATagCount)) <> SizeOf(ATagCount)
      then raise EVFInvalidSize.Create('Invalid file size: Tag count not found');
    for I:=0 to ATagCount-1
      do APreset.AddTag(LoadText(AStream));
    //read params
    if AStream.Read(AParamCount, SizeOf(AParamCount)) <> SizeOf(AParamCount)
    then raise EVFInvalidSize.Create('Invalid file size: Param count not found');
    //to prevent underflow
    if AParamCount>0
      then for I:=0 to AParamCount-1
        do LoadParamFromStream(AStream, APreset);
  end;
  APreset:=nil;

  //inform about progress
  if Assigned(AProgressHandler)
    then AProgressHandler.SetProgress(AStream.Position, 'Finishing');

  //read sources
  //have to read the sources afterwards, because... ???
  if AStream.Position <> ASourcesStart
    then raise EVFInvalidSize.Create('Invalid file size: Invalid source start position');
  //just hold the sources in memory until they are used...
  ASources.Clean;
  ASources.Destroy;
  //LoadSourcesFromStream(AStream);
  //AStream.Position:=ParamFileIDSize + SizeOf(ASourcesStart);
end;

procedure LoadFromFile(AFileName: string; AProgressHandler: TProgressHandler = nil);
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  LoadFromStream(FS, AProgressHandler);
  FS.Free;
end;

{%ENDREGION}
{%REGION Save}

procedure SaveParamToStream(AStream: TStream; AParam: IPParam; ASourceCollector: TMap);
var
  I      : Integer;
  ASource: TCollectedSource;
begin
  //write id
  SaveText(AParam.ID.Name, AStream);
  AStream.Write(AParam.ID.&Type, SizeOf(AParam.ID.&Type));
  //write content
  AParam.ToStream(LParamSaveBuffer);
  AStream.Write(LParamSaveBuffer, TVSPParamMemSize(AParam.ID.&Type));
  //collect sources
  for I:=0 to TVSPParamSourceCount(AParam.ID.&Type)-1 do begin
    ASource:=TCollectedSource.Create(LParamSaveBuffer.Sources[I]);
    if not ASourceCollector.Contains(ASource) then begin
      ASourceCollector.Add(ASource, ASource);
      ASource.Collect;
    end;
  end;
end;

function SaveSourcesToStream(AStream: TStream; ASources: TMap; AProgressHandler: TProgressHandler = nil; AProgressOffset: Integer = 0): Int64;
var
  AItem  : TObject;
  ATmpSrc: Pointer;
  I      : Integer;
begin
  Result:=AStream.Position;
  I:=0;
  for AItem in ASources do begin
    Assert(AItem is TCollectedSource);
    with TCollectedSource(AItem).Source do begin
      //inform about progress
      Inc(I);
      if Assigned(AProgressHandler)
        then AProgressHandler.SetProgress(AProgressOffset + I, 'Writing source [' + IntToStr(ID.Size) + ']' + GUIDToString(ID.ID));

      //write id
      AStream.Write(ID, SizeOf(ID));
      GetMem(ATmpSrc, Size);
      Get(0, Size, ATmpSrc^);
      //write content
      AStream.Write(ATmpSrc^, Size);
      FreeMem(ATmpSrc, Size);
    end;
  end;
end;

procedure SaveToStream(AStream: TStream; ATag: IString; AProgressHandler: TProgressHandler = nil);
var
  APresets                                    : IITaggedPresets;
  I, J, L                                     : Integer;
  ATagCount                                   : LongInt;
  ATags                                       : ITags;
  AIterator                                   : Pointer;
  AParam                                      : IPParam;
  ASourcesStart, ALastPosition, AParamCountPos: Int64;
  ASourceCollector                            : TMap;
  AParamCount                                 : LongWord;
begin
  Assert(ATag <> nil);
  ASourceCollector:=THashMap.Create;
  //write header
  AStream.Write(ParamFileID, ParamFileIDSize);
  //space for source start position
  AStream.Position:=AStream.Position + SizeOf(ASourcesStart);

  APresets:=PresetUtil.PresetsWithTag[ATag];

  if Assigned(AProgressHandler)
    then AProgressHandler.Max:=APresets.Count*2;

  for I:=0 to APresets.Count-1 do begin
    //inform about progress
    if Assigned(AProgressHandler)
      then AProgressHandler.SetProgress(I, 'Writing preset ' + GUIDToString(APresets[I].ID));

    //write ID
    AStream.Write(APresets[I].ID, SizeOf(TPPresetID));
    //write tags
    ATags:=APresets[I].Tags;
    ATagCount:=ATags.Count;
    AStream.Write(ATagCount, SizeOf(ATagCount));
    for J:=0 to ATagCount-1
      do SaveText(ATags[J], AStream);

    //space for param count
    AParamCountPos:=AStream.Position;
    AStream.Position:=AStream.Position + SizeOf(AParamCount);
    AParamCount:=0;
    //write params
    AIterator:=nil;
    AParam:=APresets[I].IterateInput(AIterator);
    while AParam <> nil do begin
      //write param
      SaveParamToStream(AStream, AParam, ASourceCollector);

      AParam:=APresets[I].IterateInput(AIterator);
      Inc(AParamCount);
    end;
    //write param count
    ALastPosition:=AStream.Position;
    AStream.Position:=AParamCountPos;
    AStream.Write(AParamCount, SizeOf(AParamCount));
    AStream.Position:=ALastPosition;
  end;

  //inform about progress
  L:=APresets.Count;
  if Assigned(AProgressHandler)
    then AProgressHandler.Max:=L + ASourceCollector.Count;

  ATags:=nil;
  APresets:=nil;

  //write sources
  ASourcesStart:=SaveSourcesToStream(AStream, ASourceCollector, AProgressHandler, L);
  ASourceCollector.Destroy;
  //write source start position
  ALastPosition:=AStream.Position;
  AStream.Position:=ParamFileIDSize;
  AStream.Write(ASourcesStart, SizeOf(ASourcesStart));
  AStream.Position:=ALastPosition;
end;

procedure SaveToStream(AStream: TStream; AProgressHandler: TProgressHandler = nil);
begin
  SaveToStream(AStream, TAGSAVE, AProgressHandler);
end;

procedure SaveToFile(AFileName: string; ATag: IString; AProgressHandler: TProgressHandler = nil);
var
  FS: TStream;
begin
  Assert(ATag <> nil);

  FS:=TFileStream.Create(AFileName, fmCreate or fmShareDenyNone);
  SaveToStream(FS, ATag, AProgressHandler);
  FS.Free;
end;

procedure SaveToFile(AFileName: string; AProgressHandler: TProgressHandler = nil);
begin
  SaveToFile(AFileName, TAGSAVE, AProgressHandler);
end;

{%ENDREGION}

end.

