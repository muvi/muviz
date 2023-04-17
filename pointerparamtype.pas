unit PointerParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, PointerParamConverters, ParamFunc, MStrings, TVSPSources,
  TVSPSourceUtil, Doors, StdPermissions;

type
  TVChangedPointer       = class (specialize TPCustomChangedValue<TVPointer>, IChangedPointer)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVPointerValueStorage  = class (TPParamValueStorage, IPPointer)
  strict private
    procedure DoValAddRef;
    procedure DoValRelease;
  private
    FValue: TVPointer;
    FLock : TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure DoSet(AValue: TPChangedValue); override;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from ASrc and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode =  nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    procedure &Set(Value: TVPointer); cdecl;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVPointer; cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property Param: IPParam read GetParam;
    property Value: TVPointer read Get write &Set;
  end;

  TVPointerType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyPointerConverter= class (TVPointerParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'s'+#0+'s'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'>'+#0+'>'+#0#0#0+'s'+#0+'s'+#0#0#0+'>'+#0+'>'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'U'+#0+'q'+#0+'N'+#0+'s'+#0+'s'+#0+'N'+#0+'q'+#0+'U'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'K'+#0+'q'+#0+'s'+#0+'s'+#0+'s'+#0+'s'+#0+'q'+#0+'K'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'C'+#0+'p'+#0+'s'+#0+'s'+#0+'p'+#0+'C'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'<'+#0+'k'+#0+'k'+#0+'<'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'<'+#0+'<'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedPointer}

function TVChangedPointer.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedPointer(Self);
end;

{%ENDREGION}
{%REGION TLinearizedPointer}

type
  TLinearizedPointer = packed record
    OutputPresetName: TTVSPSrcID;
    OutputParamName : TTVSPSrcID;
    InputPresetName : TTVSPSrcID;
    InputParamName  : TTVSPSrcID;
    OutputPresetType: TPParamType;
    OutputParamType : TPParamType;
    InputPresetType : TPParamType;
    InputParamType  : TPParamType;
    InversePriority : LongInt;
  end;

{%ENDREGION}
{%REGION TVPointerValueStorage}

constructor TVPointerValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FValue:=DEFAULTPOINTER;
  DoValAddRef;
end;

destructor TVPointerValueStorage.Destroy;
begin
  DoValRelease;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TVPointerValueStorage.DoSet(AValue: TPChangedValue);
begin
  FLock.Beginwrite;
  DoValRelease;
  FValue:=TVChangedPointer(AValue).Value;
  DoValAddRef;
  FLock.Endwrite;

  inherited DoSet(AValue);
end;

procedure TVPointerValueStorage.DoValAddRef;
begin
  FValue.Output.Preset.Name.TVSPSource._ValAddRef;
  FValue.Output.Param.Name.TVSPSource._ValAddRef;
  FValue.Input.Preset.Name.TVSPSource._ValAddRef;
  FValue.Input.Param.Name.TVSPSource._ValAddRef;
end;

procedure TVPointerValueStorage.DoValRelease;
begin
  FValue.Output.Preset.Name.TVSPSource._ValRelease;
  FValue.Output.Param.Name.TVSPSource._ValRelease;
  FValue.Input.Preset.Name.TVSPSource._ValRelease;
  FValue.Input.Param.Name.TVSPSource._ValRelease;
end;

procedure TVPointerValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = TPParamType(vPointer));
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedPointer.Create(IPPointer(ASrc).Get, DEFAULTPOINTER), APermission, APermissionGroup, AEntryCode);
end;

procedure TVPointerValueStorage.ToStream(out ADest); cdecl;
begin
  FLock.Beginread;
  with TLinearizedPointer(ADest) do begin
    OutputPresetName:=FValue.Output.Preset.Name.TVSPSource.ID;
    OutputParamName:=FValue.Output.Param.Name.TVSPSource.ID;
    InputPresetName:=FValue.Input.Preset.Name.TVSPSource.ID;
    InputParamName:=FValue.Input.Param.Name.TVSPSource.ID;
    OutputPresetType:=FValue.Output.Preset.&Type;
    OutputParamType:=FValue.Output.Param.&Type;
    InputPresetType:=FValue.Input.Preset.&Type;
    InputParamType:=FValue.Input.Param.&Type;
    InversePriority:=FValue.InversePriority;
  end;
  FLock.Endread;
end;

procedure TVPointerValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
var
  AValue: TVPointer;
begin
  with TLinearizedPointer(ASrc) do begin
    AValue.Output.Preset.Name:=EmptyString.FromTVSPSource(SourceUtil[OutputPresetName]);
    AValue.Output.Param.Name:=EmptyString.FromTVSPSource(SourceUtil[OutputParamName]);
    AValue.Input.Preset.Name:=EmptyString.FromTVSPSource(SourceUtil[InputPresetName]);
    AValue.Input.Param.Name:=EmptyString.FromTVSPSource(SourceUtil[InputParamName]);
    AValue.Output.Preset.&Type:=OutputPresetType;
    AValue.Output.Param.&Type:=OutputParamType;
    AValue.Input.Preset.&Type:=InputPresetType;
    AValue.Input.Param.&Type:=InputParamType;
    AValue.InversePriority:=InversePriority;
  end;
  RegisterChange(TVChangedPointer.Create(AValue, DEFAULTPOINTER), APermission, APermissionGroup, AEntryCode);
end;

function TVPointerValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPPointer(Self);
end;

procedure TVPointerValueStorage.&Set(Value: TVPointer); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVPointerValueStorage.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedPointer.Create(Value, DEFAULTPOINTER), APermission, APermissionGroup, AEntryCode);
end;

function TVPointerValueStorage.Get: TVPointer; cdecl;
begin
  FLock.Beginread;
  Result:=FValue;
  FLock.Endread;
end;

procedure TVPointerValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTPOINTER, APermission, APermissionGroup, AEntryCode);
end;

{%ENDREGION}
{%REGION TVPointerType}

function TVPointerType.GetColor: TMColor; cdecl;
begin
  Result:=$FF0096D6//$FF00C6C6//$FF00FFA5;
end;

function TVPointerType.GetName: IString; cdecl;
begin
  Result:='Pointer';
end;

function TVPointerType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVPointerType.GetType: TPParamType; cdecl;
begin
  Result:=vPointer;
end;

function TVPointerType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVPointerValueStorage.Create(APrototype);
end;

function TVPointerType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall        : Result:=IPPointer(TVCallToPointer.Create(IPCall(ADest)));
    vInteger     : Result:=IPPointer(TVIntegerToPointer.Create(IPInteger(ADest)));
    vFloat       : Result:=IPPointer(TVFloatToPointer.Create(IPFloat(ADest)));
    vColor       : Result:=IPPointer(TVColorToPointer.Create(IPColor(ADest)));
    vBoolean     : Result:=IPPointer(TVBooleanToPointer.Create(IPBoolean(ADest)));
    vBuffer      : Result:=IPPointer(TVBufferToPointer.Create(IPBuffer(ADest)));
    vString      : Result:=IPPointer(TVStringToPointer.Create(IPString(ADest)));
    vPreset      : Result:=IPPointer(TVPresetToPointer.Create(IPPreset(ADest)));
    vPointer     : Result:=ADest;
    else           Result:=IPPointer(TVDummyPointerConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyPointerConverter}

constructor TVDummyPointerConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyPointerConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyPointerConverter.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyPointerConverter.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVPointerType.Create));
end;

{%ENDREGION}

end.

