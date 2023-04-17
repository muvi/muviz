unit IntegerParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, IntegerParamConverters, MStrings, AdvParamType, TVSPSources,
  Doors, StdPermissions;

type
  TVChangedInteger       = class (specialize TPCustomChangedValue<TVInteger>, IChangedInteger)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVIntegerValueStorage  = class (TPParamValueStorage, IPInteger, IPIntegerSettings)
  private
    FMax       : TVInteger;
    FMin       : TVInteger;
    FBoundsLock: TMultiReadExclusiveWriteSynchronizer;
    FValue     : TVInteger;
    function GetMax: TVInteger; cdecl;
    function GetMin: TVInteger; cdecl;
  protected
    procedure DoSet(AValue: TPChangedValue); override;
    function CutValue(AValue: TVInteger): TVInteger; inline;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    function Get: TVInteger; cdecl;
    procedure &Set(Value: TVInteger); cdecl;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure SetBounds(AMin, AMax: TVInteger); cdecl;

    property Max: TVInteger read FMax;
    property Min: TVInteger read FMin;
    property Param: IPParam read GetParam;
    property Value: TVInteger read Get write &Set;
  end;

  TVIntegerType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyIntegerConverter= class (TVIntegerParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#208#0#193#0#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#234#0#182#0#216#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#230#0#185#0#185#0#224#0#226#0#226#0#226#0#226#0#226#0#226#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#252#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#254#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedInteger}

function TVChangedInteger.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedInteger(Self);
end;

{%ENDREGION}
{%REGION TVIntegerValueStorage}

constructor TVIntegerValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FBoundsLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FMin:=not MaxInt;
  FMax:=MaxInt;
  //set default value
  FValue:=DEFAULTINTEGER;
end;

destructor TVIntegerValueStorage.Destroy;
begin
  FBoundsLock.Destroy;
  inherited Destroy;
end;

procedure TVIntegerValueStorage.SetBounds(AMin, AMax: TVInteger); cdecl;
var
  AValue: TVInteger;
begin
  FBoundsLock.Beginwrite;
  FMin:=AMin;
  FMax:=AMax;
  FBoundsLock.Endwrite;
  //update value
  AValue:=Value;
  if AValue < AMin
    then Value:=AMin
    else if AValue > AMax
      then Value:=AMax;
end;

function TVIntegerValueStorage.GetMax: TVInteger; cdecl;
begin
  FBoundsLock.Beginread;
  Result:=FMax;
  FBoundsLock.Endread;
end;

function TVIntegerValueStorage.GetMin: TVInteger; cdecl;
begin
  FBoundsLock.Beginread;
  Result:=FMin;
  FBoundsLock.Endread;
end;

procedure TVIntegerValueStorage.DoSet(AValue: TPChangedValue);
begin
  InterLockedExchange(FValue, CutValue(TVChangedInteger(AValue).Value));
  inherited DoSet(AValue);
end;

procedure TVIntegerValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = TPParamType(vInteger));
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedInteger.Create(IPInteger(ASrc).Get, DEFAULTINTEGER), APermission, APermissionOwner, AEntryCode);
end;

procedure TVIntegerValueStorage.ToStream(out ADest); cdecl;
begin
  InterLockedExchange(TVInteger(ADest), FValue);
end;

procedure TVIntegerValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: IEntryCode = nil); cdecl;
begin
  RegisterChange(TVChangedInteger.Create(TVInteger(ASrc), DEFAULTINTEGER), APermission, APermissionOwner, AEntryCode);
end;

function TVIntegerValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPInteger(Self);
end;

function TVIntegerValueStorage.Get: TVInteger; cdecl;
begin
  InterLockedExchange(Result, FValue);
end;

procedure TVIntegerValueStorage.&Set(Value: TVInteger); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVIntegerValueStorage.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedInteger.Create(Value, DEFAULTINTEGER), APermission, APermissionOwner, AEntryCode);
end;

procedure TVIntegerValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTINTEGER, APermission, APermissionOwner, AEntryCode);
end;

function TVIntegerValueStorage.CutValue(AValue: TVInteger): TVInteger; inline;
begin
  FBoundsLock.Beginread;
  if AValue < FMin
    then Result:=FMin
    else if AValue > FMax
      then Result:=FMax
      else Result:=AValue;
  FBoundsLock.Endread;
end;

{%ENDREGION}
{%REGION TVIntegerType}

function TVIntegerType.GetColor: TMColor; cdecl;
begin
  Result:=$FF0000FF
end;

function TVIntegerType.GetName: IString; cdecl;
begin
  Result:='Integer';
end;

function TVIntegerType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVIntegerType.GetType: TPParamType; cdecl;
begin
  Result:=vInteger;
end;

function TVIntegerType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=IPIntegerSettings(TVIntegerValueStorage.Create(APrototype));
end;

function TVIntegerType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall          : Result:=IPInteger(TVCallToInteger.Create(IPCall(ADest)));
    vInteger       : Result:=ADest;
    vFloat         : Result:=IPInteger(TVFloatToInteger.Create(IPFloat(ADest)));
    vColor         : Result:=IPInteger(TVColorToInteger.Create(IPColor(ADest)));
    vBoolean       : Result:=IPInteger(TVBooleanToInteger.Create(IPBoolean(ADest)));
    vBuffer        : Result:=IPInteger(TVBufferToInteger.Create(IPBuffer(ADest)));
    vString        : Result:=IPInteger(TVStringToInteger.Create(IPString(ADest)));
    vPreset        : Result:=IPInteger(TVPresetToInteger.Create(IPPreset(ADest)));
    vPointer       : Result:=IPInteger(TVPointerToInteger.Create(IPPointer(ADest)));
    else             Result:=IPInteger(TVDummyIntegerConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyIntegerConverter}

constructor TVDummyIntegerConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyIntegerConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyIntegerConverter.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionOwner, AEntryCode);
end;

function TVDummyIntegerConverter.Get: TVInteger; cdecl;
begin
  Result:=DEFAULTINTEGER;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(TVIntegerType.Create);
end;

{%ENDREGION}

end.

