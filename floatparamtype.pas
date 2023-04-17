unit FloatParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, FloatParamConverters, MStrings, AdvParamType, Math, Dialogs,
  TVSPSources, Doors, StdPermissions;

type
  TVChangedFloat       = class (specialize TPCustomChangedValue<TVFloat>, IChangedFloat)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVFloatValueStorage  = class (TPParamValueStorage, IPFloat, IPFloatSettings)
  private
    FMin       : TVFloat;
    FMax       : TVFloat;
    FNan       : TVFloat;
    FBoundsLock: TMultiReadExclusiveWriteSynchronizer;
    FValue     : TVFloat;
    FLock      : TMultiReadExclusiveWriteSynchronizer;
    function GetMin: TVFloat; cdecl;
    function GetMax: TVFloat; cdecl;
    function GetNan: TVFloat; cdecl;
  protected
    procedure DoSet(AValue: TPChangedValue); override;
    function CutValue(AValue: TVFloat): TVFloat;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    function Get: TVFloat; cdecl;
    procedure &Set(Value: TVFloat); cdecl;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure SetBounds(AMin, AMax, ANan: TVFloat); cdecl;

    property Min: TVFloat read FMin;
    property Max: TVFloat read FMax;
    property LNan: TVFloat read FNan;
    property Param: IPParam read GetParam;
    property Value: TVFloat read Get write &Set;
  end;

  TVFloatType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyFloatConverter= class (TVFloatParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVFloat; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#238#2#195#10#177#13#176#13#188#11#225#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#207#7#128#25#128#26#128#26#128#26#128#26#128#26#177#13#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#135#20#155#17#232#3#246#1#248#1#238#2#179#12#128#25#250#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#134#21#158#16#239#2#254#0#255#0#244#1#185#11#128#25#248#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#202#8#128#25#128#26#128#26#128#26#128#26#128#26#171#14#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#235#3#188#11#171#14#170#14#183#12#222#5#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#236#3#216#6#0#0#229#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#175#13#128#26#150#17#196#9#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#235#3#215#6#235#3#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#238#2#248#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#221#5#128#25#229#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#232#3#128#22#150#17#226#4#226#4#226#4#226#4#226#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#128#23#128#26#128#26#128#26#128#26#128#26#128#26#128#26#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#208#7#204#8#204#8#204#8#204#8#204#8#204#8#204#8#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedFloat}

function TVChangedFloat.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedFloat(Self);
end;

{%ENDREGION}
{%REGION TVFloatValueStorage}

constructor TVFloatValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FMin:=NegInfinity;
  FMax:=Infinity;
  FNan:=Nan;
  FValue:=DEFAULTFLOAT;
  FBoundsLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TVFloatValueStorage.Destroy;
begin
  FLock.Destroy;
  FBoundsLock.Destroy;
  inherited Destroy;
end;

procedure TVFloatValueStorage.SetBounds(AMin, AMax, ANan: TVFloat); cdecl;
var
  AValue: TVFloat;
begin
  FBoundsLock.Beginwrite;
  FMin:=AMin;
  FMax:=AMax;
  FNan:=ANan;
  FBoundsLock.Endwrite;
  //update value
  AValue:=Value;
  if isNan(AValue) then begin
    if not isNan(ANan)
      then Value:=ANan;
  end else if AValue < AMin
      then Value:=AMin
      else if AValue > AMax
        then Value:=AMax;
end;

function TVFloatValueStorage.GetMin: TVFloat; cdecl;
begin
  FBoundsLock.Beginread;
  Result:=FMin;
  FBoundsLock.Endread;
end;

function TVFloatValueStorage.GetMax: TVFloat; cdecl;
begin
  FBoundsLock.Beginread;
  Result:=FMax;
  FBoundsLock.Endread;
end;

function TVFloatValueStorage.GetNan: TVFloat; cdecl;
begin
  FBoundsLock.Beginread;
  Result:=FNan;
  FBoundsLock.Endread;
end;

procedure TVFloatValueStorage.DoSet(AValue: TPChangedValue);
var
  AValue2: TVFloat;
begin
  AValue2:=CutValue(TVChangedFloat(AValue).Value);

  FLock.Beginwrite;
  FValue:=AValue2;
  FLock.Endwrite;
  inherited DoSet(AValue);
end;

procedure TVFloatValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
var
  AValue: TVFloat;
begin
  Assert(ASrc <> nil);
  Assert(ASrc.ID.&Type = TPParamType(vFloat));
  if Prototype.CheckEntryCode(AEntryCode) then begin
    AValue:=IPFloat(ASrc).Get;
    RegisterChange(TVChangedFloat.CreateDefault(AValue, isNan(AValue)), APermission, APermissionGroup, AEntryCode);
  end;
end;

procedure TVFloatValueStorage.ToStream(out ADest); cdecl;
begin
  FLock.Beginread;
  TVFloat(ADest):=FValue;
  FLock.Endread;
end;

procedure TVFloatValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  RegisterChange(TVChangedFloat.CreateDefault(TVFloat(ASrc), isNan(TVFloat(ASrc))), APermission, APermissionGroup, AEntryCode);
end;

function TVFloatValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPFloat(Self);
end;

function TVFloatValueStorage.Get: TVFloat; cdecl;
begin
  FLock.Beginread;
  Result:=FValue;
  FLock.Endread;
end;

procedure TVFloatValueStorage.&Set(Value: TVFloat); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVFloatValueStorage.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedFloat.CreateDefault(Value, isNan(Value)), APermission, APermissionGroup, AEntryCode);
end;

procedure TVFloatValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTFLOAT, APermission, APermissionGroup, AEntryCode);
end;

function TVFloatValueStorage.CutValue(AValue: TVFloat): TVFloat;
begin
  FBoundsLock.Beginread;
  if isNan(AValue)
    then Result:=FNan
    else if AValue < FMin
      then Result:=FMin
      else if AValue > FMax
        then Result:=FMax
        else Result:=AValue;
  FBoundsLock.Endread;
end;

{%ENDREGION}
{%REGION TVFloatType}

function TVFloatType.GetColor: TMColor; cdecl;
begin
  Result:=$FF00D305
end;

function TVFloatType.GetName: IString; cdecl;
begin
  Result:='Float';
end;

function TVFloatType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVFloatType.GetType: TPParamType; cdecl;
begin
  Result:=vFloat;
end;

function TVFloatType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=IPFloatSettings(TVFloatValueStorage.Create(APrototype));
end;

function TVFloatType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall          : Result:=IPFloat(TVCallToFloat.Create(IPCall(ADest)));
    vInteger       : Result:=IPFloat(TVIntegerToFloat.Create(IPInteger(ADest)));
    vFloat         : Result:=ADest;
    vColor         : Result:=IPFloat(TVColorToFloat.Create(IPColor(ADest)));
    vBoolean       : Result:=IPFloat(TVBooleanToFloat.Create(IPBoolean(ADest)));
    vBuffer        : Result:=IPFloat(TVBufferToFloat.Create(IPBuffer(ADest)));
    vString        : Result:=IPFloat(TVStringToFloat.Create(IPString(ADest)));
    vPreset        : Result:=IPFloat(TVPresetToFloat.Create(IPPreset(ADest)));
    vPointer       : Result:=IPFloat(TVPointerToFloat.Create(IPPointer(ADest)));
    else             Result:=IPFloat(TVDummyFloatConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyFloatConverter}

constructor TVDummyFloatConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyFloatConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyFloatConverter.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyFloatConverter.Get: TVFloat; cdecl;
begin
  Result:=DEFAULTFLOAT;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(TVFloatType.Create);
end;

{%ENDREGION}

end.

