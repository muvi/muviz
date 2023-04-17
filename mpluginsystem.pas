unit MPluginSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPluginType, PluginSystem, DllStr, PluginType, VisDrawUnit,
  VisType, VisTypeUnit, VisWinType, MNetUnit, MNetType, VisWinsUnit, AdvType,
  StdDllInterfaces, VPBuffers, SourceType, SrcUIDArray, CommandListImpl2,
  MFileUtil;

type
  TAddGUIArea    = function (const AHeight: IEInt): IEGUIArea of object;
  TMPluginSystem = class (TPluginSystem,IMPluginSystem2)
  private
    FNet          : IMuviClient;
    FStringManager: IStringManager;
    FPresets      : TPresets;
    FKeyboards    : PVisKeyboards;
    FVisWins      : TVisWins;
    FAddGUIArea   : TAddGUIArea;
    FBufferManager: IVPBufferManager;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    function GetStringManager: IStringManager; virtual; stdcall;
    function GetNet: IMuviClient; stdcall;
    function GetBufferManager: IVPBufferManager; stdcall;

    function MultiLoadList(const FileName: string; Append: Boolean = false): TMuviLoadResult;
  public
    constructor Create(ANet: IMuviClient; AStringManager: IStringManager; AVisWins: TVisWins; APresets: TPresets; var AKeyboards: TVisKeyboards; AAddGUIArea: TAddGUIArea; ABufferManager: IVPBufferManager); virtual; reintroduce;
    procedure DoFree; override;

    function InMVSet(ASet: MVSet; AItem: MVSetItem): Boolean; stdcall;
    procedure IncludeMVSet(var ASet: MVSet; AItems: MVSet); stdcall;
    procedure ExcludeMVSet(var ASet: MVSet; AItems: MVSet); stdcall;
    function SupportsParam(AParamType: TVisParamType): Boolean; stdcall;
    function SupportsOutput(AOutputType: TVisOutputType): Boolean; stdcall;

    procedure RegisterVis(const AName: ShortString; const ID; const AVisProc: TVisProc; const AParamNames: array of ShortString; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of ShortString; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
    function AddGUIArea(const AHeight: IEInt): IEGUIArea; stdcall;
    procedure GetMemory(var P: Pointer; const Size: LongWord); stdcall;
    procedure FreeMemory(var P: Pointer; const Size: LongWord); stdcall;
    function MultiLoad(const FileName: string; Append: Boolean = false): TMuviLoadResult;
    property StringManager: IStringManager read GetStringManager;
  end;

  TMPluginSystem3= class (TMPluginSystem, IMPluginSystem3, IMPluginSystem3_1)
  private
    FSources    : TSrcUIDArray;
    FSourceTypes: TCommandList;
  protected
    function GetSource(ID: MVSourceID): IMSource; stdcall;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    constructor Create(ANet: IMuviClient; AStringManager: IStringManager; AVisWins: TVisWins; APresets: TPresets; var AKeyboards: TVisKeyboards; AAddGUIArea: TAddGUIArea; ABufferManager: IVPBufferManager); override;
    procedure DoFree; override;
    procedure RegisterVisC(const AName: ShortString; const ID; const AVisProc: TVisProc; const AParamNames: array of ShortString; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of ShortString; const AOutputTypes: array of TVisOutputType; const ACanvasNames: array of ShortString; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
    function NewSource(SrcType: MVSourceStr): IMSource; stdcall;
    procedure DeleteSource(AID: MVSourceID); stdcall;
    procedure RegisterSourceType(SrcType: MVSourceStr; SrcConstructor: TNewSource); stdcall;
    function SupportsSource(SrcType: MVSourceStr): Boolean; stdcall;

    property Sources[ID: MVSourceID]: IMSource read GetSource;
  end;

  function NoNewSource: IMSource; stdcall;

implementation

{TMPluginSystem}

constructor TMPluginSystem.Create(ANet: IMuviClient; AStringManager: IStringManager; AVisWins: TVisWins; APresets: TPresets; var AKeyboards: TVisKeyboards; AAddGUIArea: TAddGUIArea; ABufferManager: IVPBufferManager);
begin
  inherited Create;
  FStringManager:=AStringManager;
  FNet:=ANet;
  FVisWins:=AVisWins;
  FPresets:=APresets;
  FKeyboards:=@AKeyboards;
  FAddGUIArea:=AAddGUIArea;
  FBufferManager:=ABufferManager;
end;

procedure TMPluginSystem.DoFree;
begin
  inherited DoFree;
  FNet:=nil;
  FStringManager:=nil;
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TMPluginSystem.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TMPluginSystem.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IMPluginSystem2(Self))
    else Result:=inherited Future(Version);
end;

function TMPluginSystem.InMVSet(ASet: MVSet; AItem: MVSetItem): Boolean; stdcall;
begin
  Result:=(ASet and AItem>0);
end;

procedure TMPluginSystem.IncludeMVSet(var ASet: MVSet; AItems: MVSet); stdcall;
begin
  ASet:=ASet or AItems;
end;

procedure TMPluginSystem.ExcludeMVSet(var ASet: MVSet; AItems: MVSet); stdcall;
begin
  ASet:=ASet and (not AItems)
end;

function TMPluginSystem.SupportsParam(AParamType: TVisParamType): Boolean; stdcall;
begin
  Result:=(AParamType<=vBuffer);
end;

function TMPluginSystem.SupportsOutput(AOutputType: TVisOutputType): Boolean; stdcall;
begin
  Result:=(AOutputType<=oBuffer);
end;

function TMPluginSystem.MultiLoadList(const FileName: string; Append: Boolean = false): TMuviLoadResult;
var
  F        : System.Text;
  TempStr  : string;
begin
  AssignFile(F,FileName);
  Reset(F);
  ReadLn(F,TempStr);
  Result.Success:=lplNoValidEntry;
  repeat
    ReadLn(F,TempStr);
    if TempStr<>'' then if (TempStr[1]<>CommentChar) and (TempStr[1]<>IniBrackets) then begin
      if not lplSucceeded(Result.Success)
        then Result:=MultiLoad(TempStr,Append)
        else MultiLoad(TempStr,Append);
    end;
  until EOF(F);
  CloseFile(F);
  if not lplSucceeded(Result.Success) then Result.Success:=lplNoValidEntry;
end;

function TMPluginSystem.MultiLoad(const FileName: string; Append: Boolean = false): TMuviLoadResult;
begin
  if not FileExists(FileName) then begin
    Result.Success:=lplFileNotFound;
    exit;
  end;
  Result.Success:=LoadPluginLibraryE(FileName);
  if lplFormatError(Result.Success) then begin
    Result:=LoadMuviStream(FileName,FPresets,FKeyboards^,Append);
    if lplFormatError(Result.Success)
      then Result:=MultiLoadList(FileName,Append);
  end;

//  if not lplSucceeded(Result) then
  {case TryLoadVis(FileName) of
    LoadVis_false : begin
        Result:=LoadMuviStream(FileName,APresets,AKeyboards,Append);
        if Result.Success=scFail
          then Result:=MultiLoadList(FileName,APresets,AKeyboards,Append);
      end;
    LoadVis_true  : Result.Success:=scSuccess;
    LoadVis_Exists: Result.Success:=scPluginExists;
  end;}
end;

procedure TMPluginSystem.RegisterVis(const AName: ShortString; const ID; const AVisProc: TVisProc; const AParamNames: array of ShortString; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of ShortString; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
var
  AParamNames2,AOutputNames2: array of string;
  I,L                       : Integer;
begin
  L:=Length(AParamNames);
  SetLength(AParamNames2,L);
  for I:=0 to L-1 do AParamNames2[I]:=AParamNames[I];
  L:=Length(AOutputNames);
  SetLength(AOutputNames2,L);
  for I:=0 to L-1 do AOutputNames2[I]:=AOutputNames[I];
  TVisualisation.CreateAndRegisterVis(AName,ID,AVisProc,AParamNames2,AParamTypes,AChangeParams,AInitialValues,AOutputNames2,AOutputTypes,[],AWorkspaceSize,AInitWorkspace,AFreeWorkspace);
  SetLength(AParamNames2,0);
  SetLength(AOutputNames2,0);
end;

function TMPluginSystem.AddGUIArea(const AHeight: IEInt): IEGUIArea; stdcall;
begin
  Result:=FAddGUIArea(AHeight);
end;

procedure TMPluginSystem.GetMemory(var P: Pointer; const Size: LongWord); stdcall;
begin
  GetMem(P,Size);
end;

procedure TMPluginSystem.FreeMemory(var P: Pointer; const Size: LongWord); stdcall;
begin
  FreeMem(P,Size);
end;

function TMPluginSystem.GetStringManager: IStringManager; stdcall;
begin
  Result:=FStringManager;
end;

function TMPluginSystem.GetNet: IMuviClient; stdcall;
begin
  Result:=FNet;
end;

function TMPluginSystem.GetBufferManager: IVPBufferManager; stdcall;
begin
  Result:=FBufferManager;
end;

{TMPluginSystem3}

constructor TMPluginSystem3.Create(ANet: IMuviClient; AStringManager: IStringManager; AVisWins: TVisWins; APresets: TPresets; var AKeyboards: TVisKeyboards; AAddGUIArea: TAddGUIArea; ABufferManager: IVPBufferManager);
var
  ANoNewSource: TNewSource;
begin
  inherited Create(ANet,AStringManager,AVisWins,APresets,AKeyboards,AAddGUIArea,ABufferManager);
  ANoNewSource:=@NoNewSource;
  FSourceTypes:=TCommandList.Create(SizeOf(TNewSource),ANoNewSource);
  FSources:=TSrcUIDArray.Create;
end;

procedure TMPluginSystem3.DoFree;
begin
  FSources.Destroy;
  FSourceTypes.Destroy;
  inherited DoFree;
end;

const
  Local_Version3  : MVVersion = (Version:0;MainVersion:3;SubVersion:0);
  Local_Version3_1: MVVersion = (Version:0;MainVersion:3;SubVersion:1);

function TMPluginSystem3.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version3_1;
end;

function TMPluginSystem3.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version3_1
    then Result:=IMInterface(IMPluginSystem3_1(Self))
    else if Version=Local_Version3
      then Result:=IMInterface(IMPluginSystem3(Self))
      else Result:=inherited Future(Version);
end;

procedure TMPluginSystem3.RegisterVisC(const AName: ShortString; const ID; const AVisProc: TVisProc; const AParamNames: array of ShortString; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of ShortString; const AOutputTypes: array of TVisOutputType; const ACanvasNames: array of ShortString; const AWorkspaceSize: cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
var
  AParamNames2,AOutputNames2,ACanvasNames2: array of string;
  I,L                                     : Integer;
begin
  L:=Length(AParamNames);
  SetLength(AParamNames2,L);
  for I:=0 to L-1 do AParamNames2[I]:=AParamNames[I];
  L:=Length(AOutputNames);
  SetLength(AOutputNames2,L);
  for I:=0 to L-1 do AOutputNames2[I]:=AOutputNames[I];
  L:=Length(ACanvasNames);
  SetLength(ACanvasNames2,L);
  for I:=0 to L-1 do ACanvasNames2[I]:=ACanvasNames[I];
  TVisualisation.CreateAndRegisterVis(AName,ID,AVisProc,AParamNames2,AParamTypes,AChangeParams,AInitialValues,AOutputNames2,AOutputTypes,ACanvasNames2,AWorkspaceSize,AInitWorkspace,AFreeWorkspace);
  SetLength(AParamNames2,0);
  SetLength(AOutputNames2,0);
  SetLength(ACanvasNames2,0);
end;

function TMPluginSystem3.GetSource(ID: MVSourceID): IMSource; stdcall;
begin
  if FSources.ValidID(ID)
    then Result:=FSources[ID]
    else Result:=nil;
end;

function TMPluginSystem3.NewSource(SrcType: MVSourceStr): IMSource; stdcall;
var
  ANewSource: TNewSource;
  ASourceID : MVSourceID;
begin
  FSourceTypes.GetCommand(SrcType,ANewSource);
  Result:=ANewSource();
  ASourceID:=FSources.AddItem(Result);
  Result.SrcID:=ASourceID;
end;

procedure TMPluginSystem3.DeleteSource(AID: MVSourceID); stdcall;
begin
  FSources.DeleteItem(AID);
end;

procedure TMPluginSystem3.RegisterSourceType(SrcType: MVSourceStr; SrcConstructor: TNewSource); stdcall;
var
  ANewSource: TNewSource;
begin
  FSourceTypes.AddCommand(SrcType,SrcConstructor);
end;

function TMPluginSystem3.SupportsSource(SrcType: MVSourceStr): Boolean; stdcall;
begin
  Result:=FSourceTypes.CommandExists(SrcType);
end;

{Allgemein}

function NoNewSource: IMSource; stdcall;
begin
  Result:=nil;
end;

end.

