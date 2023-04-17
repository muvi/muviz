unit MNetUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, MNetType, AuthNetUnit, MInterfacedObject,
  CommandListImpl2, UIDArray, ClientNetType, DllStr, DllStrUnit, VisWinType,
  AdvClasses, VisWinsUnit, VisTypeUnit, AuthNetType;

type
  TMuviNetData = class;

  TMuviVisWin  = class (TMInterfacedObject, IMuviVisWin)
  private
    FCaption    : ShortString;
    FPreset     : Cardinal;
    FResolution : TMuviPoint;
    FMuviNetData: TMuviNetData;
    FVWID       : Cardinal;
    function GetCaption: ShortString; stdcall;
    function GetPreset: LongWord; stdcall;
    function GetResolution: TMuviPoint; stdcall;
    function GetVisWinID: LongWord; stdcall;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
    procedure SetCaption(const ACaption: ShortString); stdcall;
    procedure SetPreset(const APreset: LongWord); stdcall;
    procedure SetResolution(const AResolution: TMuviPoint); stdcall;
  public
    constructor Create(const AMuviNetData: TMuviNetData; const AID: Cardinal; const ACaption: ShortString; const AResolution: TMuviPoint; const APreset: Cardinal); virtual; reintroduce;
    destructor Destroy; override;
    function ChangePreset(const APreset: LongWord): Boolean; stdcall;
    property Caption: ShortString read FCaption write FCaption;
    property Preset: LongWord read FPreset write FPreset;
    property Resolution: TMuviPoint read FResolution write FResolution;
    property VisWinID: LongWord read GetVisWinID;
  end;

  TMuviNet     = class;

  TMuviNetData = class (TMInterfacedObject, IMuviNetData)
  private
    FVisWins        : array of TMuviVisWin;
    FPresets        : array of ShortString;
    FData           : Pointer;
    FSupportedBlocks: TMuviBlockIDs;
    FAddDataSize    : Cardinal;
    FID             : Cardinal;
    FNet            : TMuviNet;
    function GetPresetCount: LongInt; stdcall;
    function GetPreset(const AIndex: LongInt): ShortString; stdcall;
    function GetVisWinCount: LongInt; stdcall;
    function GetVisWin(const AIndex: LongInt): IMuviVisWin; stdcall;
    procedure SetPresetCount(const ACount: LongInt); stdcall;
    procedure SetPreset(const AIndex: LongInt; const AValue: ShortString); stdcall;
    function GetVisWin2(const AIndex: LongInt): TMuviVisWin;
    procedure SetVisWinCount2(const ACount: LongInt);
    procedure SetVisWin2(const AIndex: LongInt; const AValue: TMuviVisWin);
    function GetID: LongWord; stdcall;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
    procedure GetMuviData(const ID: Int64; out AData); stdcall;
    procedure SetMuviData(const ID: Int64; const AData); stdcall;
    function MuviDataPtr(const ID: Int64): Pointer; stdcall;
  public
    constructor Create(const ANet: TMuviNet; const AID: Cardinal; const AAddDataSize: Cardinal; const AAddData); virtual; reintroduce;
    destructor Destroy; override;
    procedure SupportBlock(const BlockID: TMuviBlockID); inline;
    function SupportsBlock(const BlockID: TMuviBlockID): Boolean; stdcall;
    procedure ClearVisWins;
    property ID: LongWord read GetID;
    property PresetCount: LongInt read GetPresetCount write SetPresetCount;
    property Presets[const AIndex: LongInt]: ShortString read GetPreset write SetPreset;
    property VisWinCount: LongInt read GetVisWinCount write SetVisWinCount2;
    property VisWins[const AIndex: LongInt]: TMuviVisWin read GetVisWin2 write SetVisWin2;
    property VisWins2[const AIndex: LongInt]: IMuviVisWin read GetVisWin;
  end;

  TBlockIDsChanged = procedure of object;

  TBlockParserList = class (TCommandList,IBlockParserList)
  private
    FOnBlockIDsChanged: TBlockIDsChanged;
  protected
    function GetParser(const AID: TMuviBlockID): TMuviBlockListItem; overload;
    property Parsers[const AID: TMuviBlockID]: TMuviBlockListItem read GetParser;
  public
    constructor Create(const InvalidParser: TMuviBlockParser; const InvalidSender: TMuviBlockSender); virtual; reintroduce;
    procedure AddParser(const ABlockName: ShortString; const AParser: TMuviBlockParser; const ASender: TMuviBlockSender); stdcall;
    function GetParser(const ABlockName: ShortString): TMuviBlockListItem; stdcall; overload;
    function GetParser(const ABlockName: ShortString; out AID: TMuviBlockID): TMuviBlockListItem; stdcall; overload;
    function GetParserID(const ABlockName: ShortString): TMuviBlockID;
    procedure RemoveParser(const AParser: ShortString); stdcall;

    property OnBlockIDsChanged: TBlockIDsChanged read FOnBlockIDsChanged write FOnBlockIDsChanged;
  end;

  TMuviNet         = class (TAuthClient, IMuviClient)
  private
    FMuvis                : TUIDArray;
    FBlockParsers         : TBlockParserList;
    FAddDataSize          : Cardinal;
    FInitAddData          : Pointer;
    FVisWins              : TVisWins;
    FPresets              : TPresets;
    FVisWinBlockID        : TMuviBlockID;
    FPresetBlockID        : TMuviBlockID;
    FcidBlockIDsChanged   : Integer;
    FcidMuviNetDataAdded  : Integer;
    FcidMuviNetDataRemoved: Integer;
    procedure nvi_BlockData(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_BlocksRemove(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_SetPreset(ANet: INetClient; AParams: DString); stdcall;

    procedure bpp_Invalid(ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID); stdcall;
    function bps_Invalid(ANet: IMuviClient; const ABlockID: TMuviBlockID): DString; stdcall;
    procedure bpp_Presets(ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID); stdcall;
    function bps_Presets(ANet: IMuviClient; const ABlockID: TMuviBlockID): DString; stdcall;
    procedure bpp_VisWins(ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID); stdcall;
    function bps_VisWins(ANet: IMuviClient; const ABlockID: TMuviBlockID): DString; stdcall;

    function GetBlockParsers: IBlockParserList; stdcall;
    function GetMuviNetData(const AID: LongInt): IMuviNetData; stdcall;
    function GetMuviNetData2(const AID: LongInt): TMuviNetData;
    function GetPresetBlockID: TMuviBlockID; stdcall;
    function GetVisWinBlockID: TMuviBlockID; stdcall;
    function GetcidBlockIDsChanged: LongInt; stdcall;
    function GetcidMuviNetDataAdded: LongInt; stdcall;
    function GetcidMuviNetDataRemoved: LongInt; stdcall;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    procedure Disconnected; override;
    procedure BlockIDsChanged; virtual;

    function MuviAtID(const AID: Integer): TMuviNetData;
    function AddMuviData(const ASize: LongInt; const InitialValues): Int64; stdcall;

    property VisWins: TVisWins read FVisWins;
    property cidBlockIDsChanged: Integer read FcidBlockIDsChanged;
    property cidMuviNetDataAdded: Integer read FcidMuviNetDataAdded;
    property cidMuviNetDataRemoved: Integer read FcidMuviNetDataRemoved;
  public
    constructor Create(AOwner: TComponent; AStringManager: TStringManager; AVisWins: TVisWins; APresets: TPresets); virtual; reintroduce;
    destructor Destroy; override;
    procedure SendBlockDataUpdate(const UpdatedBlocks: TMuviBlockIDs); stdcall; overload;
    procedure SendBlockDataUpdate; stdcall; overload;
    function IterFirstMuvi: TMuviDataIterator; stdcall;
    function IterNextMuvi(var Iterator: TMuviDataIterator): IMuviNetData; stdcall;
    function IterNextMuvi2(var Iterator: TMuviDataIterator): TMuviNetData;

    property PresetBlockID: TMuviBlockID read FPresetBlockID;
    property VisWinBlockID: TMuviBlockID read FVisWinBlockID;

    property BlockParsers: IBlockParserList read GetBlockParsers;
    property Muvis[const AID: LongInt]: IMuviNetData read GetMuviNetData;
    property Muvis2[const AID: LongInt]: TMuviNetData read GetMuviNetData2;
  end;

  TOnlineDataID             = packed record
    Offset,Size: LongInt;
  end;

const
  nviBlockData   = 'mvBlockData';
  nviSetPreset   = 'setpreset';
  nviBlocksRemove= 'mvBlocksRemove';

  srvBlockData   = 'mvBlockData';
  srvSetPreset   = 'setpreset';

implementation

type
  TMuviIndex = LongInt;

{TMuviVisWin}

constructor TMuviVisWin.Create(const AMuviNetData: TMuviNetData; const AID: Cardinal; const ACaption: ShortString; const AResolution: TMuviPoint; const APreset: Cardinal);
begin
  inherited Create;
  FMuviNetData:=AMuviNetData;
  FCaption:=ACaption;
  FResolution:=AResolution;
  FPreset:=APreset;
  FVWID:=AID;
end;

destructor TMuviVisWin.Destroy;
begin
  inherited Destroy;
end;

const
  Local_TMuviVisWin_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TMuviVisWin.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_TMuviVisWin_Version;
end;

function TMuviVisWin.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_TMuviVisWin_Version
    then Result:=IMInterface(Self)
    else Result:=inherited Future(Version);
end;

function TMuviVisWin.GetCaption: ShortString; stdcall;
begin
  Result:=FCaption;
end;

function TMuviVisWin.GetPreset: LongWord; stdcall;
begin
  Result:=FPreset;
end;

function TMuviVisWin.GetResolution: TMuviPoint; stdcall;
begin
  Result:=FResolution;
end;

function TMuviVisWin.GetVisWinID: LongWord; stdcall;
begin
  Result:=FVWID;
end;

function TMuviVisWin.ChangePreset(const APreset: LongWord): Boolean; stdcall;
begin
  Result:=(APreset<FMuviNetData.PresetCount) and (APreset>=0);
  if not Result then exit;
  FPreset:=APreset;
  FMuviNetData.FNet.Send(srvSetPreset+CommandDivider+IntToStr(FMuviNetData.ID)+CommandDivider+IntToStr(FVWID)+CommandDivider+IntToStr(APreset));
end;

procedure TMuviVisWin.SetCaption(const ACaption: ShortString); stdcall;
begin
  FCaption:=ACaption;
end;

procedure TMuviVisWin.SetPreset(const APreset: LongWord); stdcall;
begin
  FPreset:=APreset;
end;

procedure TMuviVisWin.SetResolution(const AResolution: TMuviPoint); stdcall;
begin
  FResolution:=AResolution;
end;

{TMuviNetData}

constructor TMuviNetData.Create(const ANet: TMuviNet; const AID: Cardinal; const AAddDataSize: Cardinal; const AAddData);
begin
  inherited Create;
  FNet:=ANet;
  FID:=AID;
  FSupportedBlocks:=[]; //keinen Muvi Block unterstuetzen
  GetMem(FData,AAddDataSize);
  Move(AAddData,FData^,AAddDataSize);
  FAddDataSize:=AAddDataSize;
  ANet.Callbacks.Call(ANet.cidMuviNetDataAdded,NetEventData(InvalidDString,IMInterface(IMuviNetData(Self)),AID));
end;

destructor TMuviNetData.Destroy;
var
  I: Integer;
begin
  FNet.Callbacks.Call(FNet.cidMuviNetDataRemoved,NetEventData(InvalidDString,IMInterface(IMuviNetData(Self)),FID));
  FreeMem(FData,FAddDataSize);
  for I:=0 to Length(FVisWins)-1
    do FVisWins[I].Destroy;
  SetLength(FVisWins,0);
  SetLength(FPresets,0);
  inherited Destroy;
end;

const
  Local_TMuviNetData_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TMuviNetData.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_TMuviNetData_Version;
end;

function TMuviNetData.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_TMuviNetData_Version
    then Result:=IMInterface(Self)
    else Result:=inherited Future(Version);
end;

function TMuviNetData.GetPresetCount: LongInt; stdcall;
begin
  Result:=Length(FPresets);
end;

function TMuviNetData.GetPreset(const AIndex: LongInt): ShortString; stdcall;
begin
  Result:=FPresets[AIndex];
end;

function TMuviNetData.GetVisWinCount: LongInt; stdcall;
begin
  Result:=Length(FVisWins);
end;

function TMuviNetData.GetVisWin(const AIndex: LongInt): IMuviVisWin; stdcall;
begin
  if (AIndex>=Length(FVisWins)) or (AIndex<0)
    then Result:=nil
    else Result:=FVisWins[AIndex];
end;

procedure TMuviNetData.SetPresetCount(const ACount: LongInt); stdcall;
begin
  SetLength(FPresets,ACount);
end;

procedure TMuviNetData.SetPreset(const AIndex: LongInt; const AValue: ShortString); stdcall;
begin
  FPresets[AIndex]:=AValue;
end;

function TMuviNetData.GetVisWin2(const AIndex: LongInt): TMuviVisWin;
begin
  Result:=FVisWins[AIndex];
end;

procedure TMuviNetData.SetVisWinCount2(const ACount: LongInt);
begin
  SetLength(FVisWins,ACount);
end;

procedure TMuviNetData.SetVisWin2(const AIndex: LongInt; const AValue: TMuviVisWin);
begin
  FVisWins[AIndex]:=AValue;
end;

function TMuviNetData.GetID: LongWord; stdcall;
begin
  Result:=FID;
end;

procedure TMuviNetData.ClearVisWins;
var
  I: Integer;
begin
  for I:=0 to Length(FVisWins)-1
    do FVisWins[I].Destroy;
  SetLength(FVisWins,0);
end;

procedure TMuviNetData.GetMuviData(const ID: Int64; out AData); stdcall;
var
  AID2    : TOnlineDataID absolute ID;
begin
  Move((FData+AID2.Offset)^,AData,AID2.Size);
end;

procedure TMuviNetData.SetMuviData(const ID: Int64; const AData); stdcall;
var
  AID2    : TOnlineDataID absolute ID;
begin
  Move(AData,(FData+AID2.Offset)^,AID2.Size);
end;

function TMuviNetData.MuviDataPtr(const ID: Int64): Pointer; stdcall;
var
  AID2    : TOnlineDataID absolute ID;
begin
  Result:=(FData+AID2.Offset);
end;

procedure TMuviNetData.SupportBlock(const BlockID: TMuviBlockID); inline;
begin
  Include(FSupportedBlocks,BlockID);
end;

function TMuviNetData.SupportsBlock(const BlockID: TMuviBlockID): Boolean; stdcall;
begin
  Result:=BlockID in FSupportedBlocks;
end;

{TBlockParserList}

constructor TBlockParserList.Create(const InvalidParser: TMuviBlockParser; const InvalidSender: TMuviBlockSender);
var
  ABlockListItem: TMuviBlockListItem;
begin
  with ABlockListItem do begin
    Parser:=InvalidParser;
    Sender:=InvalidSender;
  end;
  inherited Create(SizeOf(ABlockListItem),ABlockListItem);
  FOnBlockIDsChanged:=nil;
end;

procedure TBlockParserList.AddParser(const ABlockName: ShortString; const AParser: TMuviBlockParser; const ASender: TMuviBlockSender); stdcall;
var
  ABlockListItem: TMuviBlockListItem;
begin
  with ABlockListItem do begin
    Parser:=AParser;
    Sender:=ASender;
  end;
  AddCommand(ABlockName,ABlockListItem);
  if Assigned(FOnBlockIDsChanged) then FOnBlockIDsChanged;
end;

function TBlockParserList.GetParser(const ABlockName: ShortString): TMuviBlockListItem; stdcall;
begin
  GetCommand(ABlockName,Result);
end;

function TBlockParserList.GetParser(const ABlockName: ShortString; out AID: TMuviBlockID): TMuviBlockListItem; stdcall; overload;
begin
  AID:=FindCommand(ABlockName);
  GetData(AID,Result);
end;

function TBlockParserList.GetParserID(const ABlockName: ShortString): TMuviBlockID;
begin
  Result:=FindCommand(ABlockName);
end;

function TBlockParserList.GetParser(const AID: TMuviBlockID): TMuviBlockListItem; overload;
begin
  GetData(AID,Result);
end;

procedure TBlockParserList.RemoveParser(const AParser: ShortString); stdcall;
begin
  RemoveCommand(AParser);
  if Assigned(FOnBlockIDsChanged) then FOnBlockIDsChanged;
end;

{TMuviNetClient}

constructor TMuviNet.Create(AOwner: TComponent; AStringManager: TStringManager; AVisWins: TVisWins; APresets: TPresets);
begin
  inherited Create(AOwner,AStringManager);
  FVisWins:=AVisWins;
  FPresets:=APresets;
  FcidBlockIDsChanged:=Callbacks.AddCallback('BlockIDsChanged');
  FcidMuviNetDataAdded:=Callbacks.AddCallback('MuviNetDataAdded');
  FcidMuviNetDataRemoved:=Callbacks.AddCallback('MuviNetDataRemoved');
  Commands.AddCommand(nviBlockData,@nvi_BlockData);
  Commands.AddCommand(nviBlocksRemove,@nvi_BlocksRemove);
  Commands.AddCommand(nviSetPreset,@nvi_SetPreset);
  FBlockParsers:=TBlockParserList.Create(@bpp_Invalid,@bps_Invalid);
  FBlockParsers.AddParser(bpPresets,@bpp_Presets,@bps_Presets);
  FBlockParsers.AddParser(bpVisWins,@bpp_VisWins,@bps_VisWins);
  FBlockParsers.OnBlockIDsChanged:=@BlockIDsChanged;
  BlockIDsChanged;
  FMuvis:=TUIDArray.Create;
  FAddDataSize:=0;
  GetMem(FInitAddData,0);
end;

destructor TMuviNet.Destroy;
begin
  FBlockParsers.Destroy;
  FMuvis.Destroy;
  FVisWins:=nil;
  FreeMem(FInitAddData,FAddDataSize);
  inherited Destroy;
end;

const
  Local_Version: MVVersion = (Version:1;MainVersion:0;SubVersion:0);

function TMuviNet.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TMuviNet.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IMuviClient(Self))
    else Result:=inherited Future(Version);
end;

procedure TMuviNet.Disconnected;
begin
  FMuvis.Clear;
  inherited Disconnected;
end;

procedure TMuviNet.BlockIDsChanged;
begin
  FVisWinBlockID:=BlockParsers.GetParserID(bpVisWins);
  FPresetBlockID:=BlockParsers.GetParserID(bpPresets);
  Callbacks.Call(FcidBlockIDsChanged,NetEventData);
end;

function TMuviNet.GetMuviNetData(const AID: LongInt): IMuviNetData; stdcall;
begin
  if (AID>=FMuvis.Count) or (AID<0)
    then Result:=nil
    else Result:=TMuviNetData(FMuvis[AID]);
end;

function TMuviNet.GetMuviNetData2(const AID: LongInt): TMuviNetData;
begin
  if AID>=FMuvis.Count
    then Result:=nil
    else Result:=TMuviNetData(FMuvis[AID]);
end;

function TMuviNet.GetPresetBlockID: TMuviBlockID; stdcall;
begin
  Result:=FPresetBlockID;
end;

function TMuviNet.GetVisWinBlockID: TMuviBlockID; stdcall;
begin
  Result:=FVisWinBlockID;
end;

function TMuviNet.GetcidBlockIDsChanged: LongInt; stdcall;
begin
  Result:=FcidBlockIDsChanged;
end;

function TMuviNet.GetcidMuviNetDataAdded: LongInt; stdcall;
begin
  Result:=FcidMuviNetDataAdded;
end;

function TMuviNet.GetcidMuviNetDataRemoved: LongInt; stdcall;
begin
  Result:=FcidMuviNetDataRemoved;
end;

function TMuviNet.IterFirstMuvi: TMuviDataIterator; stdcall;
begin
  Result:=FMuvis.IterFirst;
end;

function TMuviNet.IterNextMuvi(var Iterator: TMuviDataIterator): IMuviNetData; stdcall;
begin
  Result:=TMuviNetData(FMuvis.IterNext(Iterator));
end;

function TMuviNet.IterNextMuvi2(var Iterator: TMuviDataIterator): TMuviNetData;
begin
  Result:=TMuviNetData(FMuvis.IterNext(Iterator));
end;

function TMuviNet.AddMuviData(const ASize: LongInt; const InitialValues): Int64; stdcall;
var
  AResult2       : TOnlineDataID absolute Result;
  OldAddDataSize : Cardinal;
  ANewInitAddData: Pointer;
begin
  with AResult2 do begin
    Offset:=FAddDataSize;
    Size:=ASize;
  end;
  OldAddDataSize:=FAddDataSize;
  FAddDataSize+=ASize;
  //Initialisierungswerte neu setzen
  GetMem(ANewInitAddData,FAddDataSize);
  Move(InitialValues,(ANewInitAddData+OldAddDataSize)^,ASize);
  Move(FInitAddData^,ANewInitAddData^,OldAddDataSize);
  FreeMem(FInitAddData,OldAddDataSize);
  FInitAddData:=ANewInitAddData;
end;

function TMuviNet.MuviAtID(const AID: Integer): TMuviNetData;
begin
  if not FMuvis.ValidID(AID) then begin
    Result:=TMuviNetData.Create(Self,AID,FAddDataSize,FInitAddData^);
    FMuvis.SetToID(AID,Result);
  end else Result:=TMuviNetData(FMuvis[AID]);
end;

procedure TMuviNet.SendBlockDataUpdate(const UpdatedBlocks: TMuviBlockIDs); stdcall;
var
  ASendStr: string;
  ABlock  : TMuviBlockID;
  ASender : TMuviBlockSender;
  ADStr   : DString;
begin
  if not Active then exit;
  //zu sendenden Befehl vorbereiten
  ASendStr:=srvBlockData+CommandDivider+':';
  for ABlock:=Low(TMuviBlockID) to High(TMuviBlockID) do if ABlock in UpdatedBlocks then begin
    ASender:=FBlockParsers.Parsers[ABlock].Sender;
    ADStr:=ASender(Self,ABlock);
    ASendStr+=#$D#$A+'['+FBlockParsers.Items[ABlock]+']'+#$D#$A+StringManager.Strings[ADStr];
    StringManager.DisposeStr(ADStr);
  end;
  //Befehl verschicken
  Send(ASendStr);
end;

procedure TMuviNet.SendBlockDataUpdate; stdcall;
begin
  SendBlockDataUpdate([1..FBlockParsers.Count-1]);
end;

{TMuviNet - Commands}

procedure TMuviNet.nvi_SetPreset(ANet: INetClient; AParams: DString); stdcall;
var
  ATempInt      : Integer;
  AParam        : ShortString;
  AVisWin       : IVisWin;
begin
  AParam:=NextParam(AParams);
  if not TryStrToInt(AParam,ATempInt) then begin
    Send('!err visualisation window with index '+AParam+' not existing');
    exit;
  end;
  if (ATempInt<0) or (ATempInt>=FVisWins.Count) then begin
    Send('!err visualisation window with index '+AParam+' not existing');
    exit;
  end;
  AVisWin:=FVisWins.VisWins[ATempInt];
  AParam:=NextParam(AParams);
  if not TryStrToInt(AParam,ATempInt) then begin
    Send('!err Preset with index '+AParam+' not existing');
    exit;
  end;
  if (ATempInt>=FPresets.Count) or (ATempInt<0) then begin
    Send('!err Preset with index '+AParam+' not existing');
    exit;
  end;
  AVisWin.Preset:=ATempInt;
end;

procedure TMuviNet.nvi_BlockData(ANet: INetClient; AParams: DString); stdcall;
var
  AParams2,ABlockName: string;
  APos               : Integer;
  ABlockParser       : TMuviBlockParser;
  ABlock             : DString;
  AMuviNetData       : TMuviNetData;
  ABlockID           : TMuviBlockID;
  AMuviID            : TMuviIndex;
begin
  //Bloecke verarbeiten
  AMuviNetData:=nil;
  AParams2:=StringManager.Strings[AParams];
  ABlock:=StringManager.NewString('');
  APos:=Pos(#$D#$A+'[',AParams2);
  while APos>0 do begin
    Delete(AParams2,1,APos+2);
    APos:=Pos(']'+#$D#$A,AParams2);
    if APos<0 then break;
    ABlockName:=Copy(AParams2,1,APos-1);
    Delete(AParams2,1,APos+2);
    APos:=Pos(#$D#$A+'[',AParams2);
    if APos>0
      then StringManager.Strings[ABlock]:=Copy(AParams2,1,APos-1)
      else StringManager.Strings[ABlock]:=AParams2;

    if TryStrToInt(ABlockName,AMuviID) then begin
      AMuviNetData:=MuviAtID(AMuviID);
    end else if AMuviNetData<>nil then begin
      ABlockParser:=FBlockParsers.GetParser(ABlockName,ABlockID).Parser;
      AMuviNetData.SupportBlock(ABlockID);
      ABlockParser(Self,ABlock,AMuviNetData,ABlockID);
    end;
  end;
  StringManager.DisposeStr(ABlock);
end;

procedure TMuviNet.nvi_BlocksRemove(ANet: INetClient; AParams: DString); stdcall;
var
  AID   : TMuviIndex;
  AParam: ShortString;
begin
  AParam:=NextParam(AParams);
  while AParam<>'' do begin
    if TryStrToInt(AParam,AID) then begin
      if FMuvis.ValidID(AID) then FMuvis.DeleteItem(AID);
    end;
    AParam:=NextParam(AParams);
  end;
end;

{TMuviNet - BlockParsers}

procedure TMuviNet.bpp_Invalid(ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID); stdcall;
begin
  //do nothing
end;

function TMuviNet.bps_Invalid(ANet: IMuviClient; const ABlockID: TMuviBlockID): DString; stdcall;
begin
  Result:=StringManager.NewString('');
end;

procedure TMuviNet.bpp_Presets(ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID); stdcall;
var
  APresets: TLQueue;
  AStr    : ShortString;
  I       : Integer;
begin
  APresets:=TLQueue.Create(SizeOf(ShortString));
  AStr:=NextParam(ABlock);
  while AStr<>'' do begin
    APresets.Push(AStr);
    AStr:=NextParam(ABlock);
  end;
  AMuvi.PresetCount:=APresets.Count;
  for I:=0 to APresets.Count-1 do begin
    APresets.Pop(AStr);
    AMuvi.Presets[I]:=AStr;
  end;
  APresets.Destroy;
end;

function TMuviNet.bps_Presets(ANet: IMuviClient; const ABlockID: TMuviBlockID): DString; stdcall;
var
  ASendStr: string;
  I       : Integer;
begin
  ASendStr:='';
  for I:=0 to FPresets.Count-1 do ASendStr+=CommandIntegrator+FPresets[I].Name+CommandIntegrator+CommandDivider;
  Result:=StringManager.NewString_(ASendStr);
end;

procedure TMuviNet.bpp_VisWins(ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID); stdcall;
var
  AMuvi2     : TMuviNetData;
  ACaption   : ShortString;
  APreset    : LongWord;
  AResolution: TMuviPoint;

  function NextData: Boolean;
  var
    AInt: Integer;
    AStr: ShortString;
  begin
    ACaption:=NextParam(ABlock);
    AStr:=NextParam(ABlock);
    if TryStrToInt(AStr,AInt) then APreset:=AInt else begin
      Result:=false;
      exit;
    end;
    AStr:=NextParam(ABlock);
    if TryStrToInt(AStr,AInt) then AResolution.X:=AInt else begin
      Result:=false;
      exit;
    end;
    AStr:=NextParam(ABlock);
    if TryStrToInt(AStr,AInt) then AResolution.Y:=AInt else begin
      Result:=false;
      exit;
    end;
  end;

var
  I: Integer;
begin
  AMuvi2:=Muvis2[AMuvi.ID];
  AMuvi2.ClearVisWins;
  AMuvi2.VisWinCount:=10;
  I:=0;
  while NextData do begin
    if I mod 10 = 0 then AMuvi2.VisWinCount:=I+10;
    AMuvi2.VisWins[I]:=TMuviVisWin.Create(AMuvi2,I,ACaption,AResolution,APreset);
    Inc(I);
  end;
  AMuvi2.VisWinCount:=I;
end;

function TMuviNet.bps_VisWins(ANet: IMuviClient; const ABlockID: TMuviBlockID): DString; stdcall;
var
  ASendStr: string;
  I       : Integer;
  AVisWin : TVisWin;
begin
  ASendStr:='';
  for I:=0 to VisWins.Count-1 do begin
    AVisWin:=VisWins[I];
    ASendStr+=CommandIntegrator+AVisWin.Name+CommandIntegrator
        +CommandDivider+IntToStr(AVisWin.Preset)
        +CommandDivider+IntToStr(AVisWin.Resolution.X)
        +CommandDivider+IntToStr(AVisWin.Resolution.Y)+CommandDivider;
  end;
  Result:=StringManager.NewString_(ASendStr);
end;

function  TMuviNet.GetBlockParsers: IBlockParserList; stdcall;
begin
  Result:=FBlockParsers;
end;

end.

