unit StringParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, StringParamConverters, ParamFunc, MStrings, TVSPSources,
  TVSPSourceUtil, Dialogs, Doors, StdPermissions;

type
  TVChangedString       = class (specialize TPCustomChangedValue<TVString>, IChangedString)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVStringValueStorage  = class (TPParamValueStorage, IPString)
  private
    FValue: TVString;
    FLock : TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure DoSet(AValue: TPChangedValue); override;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    function Get: TVString; cdecl;
    procedure &Set(Value: TVString); cdecl;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property Param: IPParam read GetParam;
    property Value: TVString read Get write &Set;
  end;

  TVStringType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyStringConverter= class (TVStringParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr  : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#231#254#232#254#230#254#148#250#201#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#247#255#128#248#225#254#128#249#186#252#128#248#249#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#213#253#171#251#254#255#128#249#0#0#139#250#246#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#216#253#135#250#217#254#155#251#220#254#135#250#0#0#240#255#172#251#155#250#184#252#250#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#143#250#128#248#128#248#128#248#128#248#250#255#128#248#177#251#201#253#168#251#130#249#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#252#255#212#253#169#251#0#0#0#0#0#0#138#249#245#255#0#0#0#0#0#0#0#0#0#0#247#255#193#252#193#252#193#252#193#252#193#252#193#252#193#252#231#254#131#249#247#255#0#0#240#255#128#248#253#255#0#0#0#0#0#0#0#0#0#0#242#255#158#251#158#251#128#247#141#250#168#251#133#250#128#248#0#0#185#252#202#253#0#0#139#250#209#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#222#254#156#251#0#0#0#0#0#0#128#249#249#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#225#254#131#249#254#255#0#0#247#255#128#248#252#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#160#251#128#248#128#248#128#248#197#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#250#255#236#254#254#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedString}

function TVChangedString.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedString(Self);
end;

{%ENDREGION}
{%REGION TVStringValueStorage}

constructor TVStringValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FValue:=DEFAULTSTRING;
  FValue.TVSPSource._ValAddRef;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TVStringValueStorage.Destroy;
begin
  Assert(FValue <> nil);
  FValue.TVSPSource._ValRelease;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TVStringValueStorage.DoSet(AValue: TPChangedValue);
begin
  FLock.Beginwrite;
  FValue.TVSPSource._ValRelease;
  FValue:=TVChangedString(AValue).Value;
  FValue.TVSPSource._ValAddRef;
  FLock.Endwrite;

  Assert(FValue <> nil);
  inherited DoSet(AValue);
end;

procedure TVStringValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
var
  AStr: TVString;
begin
  Assert(ASrc.ID.&Type = TPParamType(vString));
  if Prototype.CheckEntryCode(AEntryCode) then begin
    AStr:=IPString(ASrc).Get;
    RegisterChange(TVChangedString.CreateDefault(AStr, AStr.Length = 0), APermission, APermissionGroup, AEntryCode);
  end;
end;

procedure TVStringValueStorage.ToStream(out ADest); cdecl;
begin
  FLock.Beginread;
  TTVSPSrcID(ADest):=FValue.TVSPSource.ID;
  FLock.Endread;
end;

procedure TVStringValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
var
  ANewStr: IString;
begin
  ANewStr:=EmptyString.FromTVSPSource(SourceUtil[TTVSPSrcID(ASrc)]);
  RegisterChange(TVChangedString.CreateDefault(ANewStr, ANewStr.Length = 0), APermission, APermissionGroup, AEntryCode);
end;

function TVStringValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPString(Self);
end;

function TVStringValueStorage.Get: TVString; cdecl;
begin
  FLock.Beginread;
  Result:=FValue;
  FLock.Endread;
end;

procedure TVStringValueStorage.&Set(Value: TVString); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVStringValueStorage.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedString.CreateDefault(Value, Value.Length = 0), APermission, APermissionGroup, AEntryCode);
end;

procedure TVStringValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTSTRING, APermission, APermissionGroup, AEntryCode);
end;

{%ENDREGION}
{%REGION TVStringType}

function TVStringType.GetColor: TMColor; cdecl;
begin
  Result:=$FFFF0000;
end;

function TVStringType.GetName: IString; cdecl;
begin
  Result:='String';
end;

function TVStringType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVStringType.GetType: TPParamType; cdecl;
begin
  Result:=vString;
end;

function TVStringType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVStringValueStorage.Create(APrototype);
end;

function TVStringType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall        : Result:=IPString(TVCallToString.Create(IPCall(ADest)));
    vInteger     : Result:=IPString(TVIntegerToString.Create(IPInteger(ADest)));
    vFloat       : Result:=IPString(TVFloatToString.Create(IPFloat(ADest)));
    vColor       : Result:=IPString(TVColorToString.Create(IPColor(ADest)));
    vBoolean     : Result:=IPString(TVBooleanToString.Create(IPBoolean(ADest)));
    vBuffer      : Result:=IPString(TVBufferToString.Create(IPBuffer(ADest)));
    vString      : Result:=ADest;
    vPreset      : Result:=IPString(TVPresetToString.Create(IPPreset(ADest)));
    vPointer     : Result:=IPString(TVPointerToString.Create(IPPointer(ADest)));
    else           Result:=IPString(TVDummyStringConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyStringConverter}

constructor TVDummyStringConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyStringConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyStringConverter.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyStringConverter.Get: TVString; cdecl;
begin
  Result:=DEFAULTSTRING;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVStringType.Create));
end;

{%ENDREGION}

end.

