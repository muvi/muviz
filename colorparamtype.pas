unit ColorParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, ColorParamConverters, MStrings, TVSPSources, Doors,
  StdPermissions;

type
  TVChangedColor       = class (specialize TPCustomChangedValue<TVColor>, IChangedColor)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVColorValueStorage  = class (TPParamValueStorage, IPColor)
  private
    FValue: TVColor;
  protected
    procedure DoSet(AValue: TPChangedValue); override;
  public
    constructor Create(APrototype: IPParamPrototype);
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    function Get: TVColor; cdecl;
    procedure &Set(Value: TVColor); cdecl;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property Param: IPParam read GetParam;
    property Value: TVColor read Get write &Set;
  end;

  TVColorType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyColorConverter= class (TVColorParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedColor}

function TVChangedColor.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedColor(Self);
end;

{%ENDREGION}
{%REGION TVColorValueStorage}

constructor TVColorValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FValue:=DEFAULTCOLOR;
end;

procedure TVColorValueStorage.DoSet(AValue: TPChangedValue);
begin
  InterLockedExchange(FValue, TVChangedColor(AValue).Value);
  inherited DoSet(AValue);
end;

procedure TVColorValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = TPParamType(vColor));
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedColor.Create(IPColor(ASrc).Get, DEFAULTCOLOR), APermission, APermissionGroup, AEntryCode);
end;

procedure TVColorValueStorage.ToStream(out ADest); cdecl;
begin
  InterLockedExchange(TVColor(ADest), FValue);
end;

procedure TVColorValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  RegisterChange(TVChangedColor.Create(TVColor(ASrc), DEFAULTCOLOR), APermission, APermissionGroup, AEntryCode);
end;

function TVColorValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPColor(Self);
end;

function TVColorValueStorage.Get: TVColor; cdecl;
begin
  InterLockedExchange(Result, FValue);
end;

procedure TVColorValueStorage.&Set(Value: TVColor); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVColorValueStorage.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedColor.Create(Value, DEFAULTCOLOR), APermission, APermissionGroup, AEntryCode);
end;

procedure TVColorValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTCOLOR, APermission, APermissionGroup, AEntryCode);
end;

{%ENDREGION}
{%REGION TVColorType}

function TVColorType.GetColor: TMColor; cdecl;
begin
  Result:=$FF00E6E6
end;

function TVColorType.GetName: IString; cdecl;
begin
  Result:='Color';
end;

function TVColorType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVColorType.GetType: TPParamType; cdecl;
begin
  Result:=vColor;
end;

function TVColorType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVColorValueStorage.Create(APrototype);
end;

function TVColorType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall          : Result:=IPColor(TVCallToColor.Create(IPCall(ADest)));
    vInteger       : Result:=IPColor(TVIntegerToColor.Create(IPInteger(ADest)));
    vFloat         : Result:=IPColor(TVFloatToColor.Create(IPFloat(ADest)));
    vColor         : Result:=ADest;
    vBoolean       : Result:=IPColor(TVBooleanToColor.Create(IPBoolean(ADest)));
    vBuffer        : Result:=IPColor(TVBufferToColor.Create(IPBuffer(ADest)));
    vString        : Result:=IPColor(TVStringToColor.Create(IPString(ADest)));
    vPreset        : Result:=IPColor(TVPresetToColor.Create(IPPreset(ADest)));
    vPointer       : Result:=IPColor(TVPointerToColor.Create(IPPointer(ADest)));
    else             Result:=IPColor(TVDummyColorConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyColorConverter}

constructor TVDummyColorConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyColorConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyColorConverter.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyColorConverter.Get: TVColor; cdecl;
begin
  Result:=DEFAULTCOLOR;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVColorType.Create));
end;

{%ENDREGION}

end.

