unit BooleanParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, BooleanParamConverters, MStrings, TVSPSources, Doors,
  StdPermissions;

type
  TVChangedBoolean       = class (specialize TPCustomChangedValue<TVBoolean>, IChangedBoolean)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVBooleanValueStorage  = class (TPParamValueStorage, IPBoolean)
  private
    //for faster and easier thread synchronizing use cardinal instead of TVBoolean
    FValue: Cardinal;//TVBoolean
  protected
    procedure DoSet(AValue: TPChangedValue); override;
  public
    constructor Create(APrototype: IPParamPrototype);
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    function Get: TVBoolean; cdecl;
    procedure &Set(Value: TVBoolean); cdecl;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property Param: IPParam read GetParam;
    property Value: TVBoolean read Get write &Set;
  end;

  TVBooleanType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyBooleanConverter= class (TVBooleanParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#253#227#253#224#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#253#250#200#247#189#255#237#0#247#246#182#244#178#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#249#251#215#244#176#244#174#245#180#244#172#253#223#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#253#221#243#172#243#172#251#213#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#254#232#244#172#246#187#247#190#244#174#255#240#0#0#250#208#255#241#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#248#198#246#184#0#249#0#247#244#174#250#208#0#0#246#181#244#174#250#207#255#243#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#240#254#234#0#0#0#0#251#213#255#245#0#0#0#248#250#202#243#172#245#179#0#250#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#254#230#245#179#245#180#254#234#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#251#212#244#173#251#213#0#252#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#250#203#246#184#255#239#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#250#205#250#200#0#252#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#251#215#251#212#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#237#251#215#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#238#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedBoolean}

function TVChangedBoolean.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedBoolean(Self);
end;

{%ENDREGION}
{%REGION TVBooleanValueStorage}

constructor TVBooleanValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FValue:=Ord(DEFAULTBOOLEAN);
end;

procedure TVBooleanValueStorage.DoSet(AValue: TPChangedValue);
begin
  InterLockedExchange(FValue, Ord(TVChangedBoolean(AValue).Value));
  inherited DoSet(AValue);
end;

procedure TVBooleanValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = TPParamType(vBoolean));
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedBoolean.Create(IPBoolean(ASrc).Value, DEFAULTBOOLEAN), APermission, APermissionGroup, AEntryCode);
end;

procedure TVBooleanValueStorage.ToStream(out ADest); cdecl;
var
  AValue: Cardinal;
begin
  InterLockedExchange(AValue, FValue);
  TVBoolean(ADest):=TVBoolean(AValue);
end;

procedure TVBooleanValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  RegisterChange(TVChangedBoolean.Create(TVBoolean(ASrc), DEFAULTBOOLEAN), APermission, APermissionGroup, AEntryCode);
end;

function TVBooleanValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPBoolean(Self);
end;

function TVBooleanValueStorage.Get: TVBoolean; cdecl;
var
  AResult: Cardinal;
begin
  InterLockedExchange(AResult, FValue);
  Result:=TVBoolean(AResult);
end;

procedure TVBooleanValueStorage.&Set(Value: TVBoolean); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVBooleanValueStorage.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedBoolean.Create(Value, DEFAULTBOOLEAN), APermission, APermissionGroup, AEntryCode);
end;

procedure TVBooleanValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTBOOLEAN, APermission, APermissionGroup, AEntryCode);
end;

{%ENDREGION}
{%REGION TVBooleanType}

function TVBooleanType.GetColor: TMColor; cdecl;
begin
  Result:=$FFD8D822
end;

function TVBooleanType.GetName: IString; cdecl;
begin
  Result:='Boolean';
end;

function TVBooleanType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVBooleanType.GetType: TPParamType; cdecl;
begin
  Result:=vBoolean;
end;

function TVBooleanType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVBooleanValueStorage.Create(APrototype);
end;

function TVBooleanType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall          : Result:=IPBoolean(TVCallToBoolean.Create(IPCall(ADest)));
    vInteger       : Result:=IPBoolean(TVIntegerToBoolean.Create(IPInteger(ADest)));
    vFloat         : Result:=IPBoolean(TVFloatToBoolean.Create(IPFloat(ADest)));
    vColor         : Result:=IPBoolean(TVColorToBoolean.Create(IPColor(ADest)));
    vBoolean       : Result:=ADest;
    vBuffer        : Result:=IPBoolean(TVBufferToBoolean.Create(IPBuffer(ADest)));
    vString        : Result:=IPBoolean(TVStringToBoolean.Create(IPString(ADest)));
    vPreset        : Result:=IPBoolean(TVPresetToBoolean.Create(IPPreset(ADest)));
    vPointer       : Result:=IPBoolean(TVPointerToBoolean.Create(IPPointer(ADest)));
    else             Result:=IPBoolean(TVDummyBooleanConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyBooleanConverter}

constructor TVDummyBooleanConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyBooleanConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyBooleanConverter.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyBooleanConverter.Get: TVBoolean; cdecl;
begin
  Result:=DEFAULTBOOLEAN;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVBooleanType.Create));
end;

{%ENDREGION}

end.

