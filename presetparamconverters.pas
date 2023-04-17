unit PresetParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  GUIDop, PresetType, MStrings, Doors, StdPermissions;

type
  TVCallToPreset        = class (TVPresetParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVIntegerToPreset     = class (TVPresetParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVFloatToPreset       = class (TVPresetParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVColorToPreset       = class (TVPresetParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVBooleanToPreset     = class (TVPresetParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVBufferToPreset      = class (TVPresetParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVStringToPreset      = class (TVPresetParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

  TVPointerToPreset     = class (TVPresetParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

implementation

{%REGION TVCallToPreset}

constructor TVCallToPreset.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionGroup, AEntryCode);
end;

function TVCallToPreset.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVIntegerToPreset}

constructor TVIntegerToPreset.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTINTEGER, APermission, APermissionGroup, AEntryCode);
end;

function TVIntegerToPreset.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVFloatToPreset}

constructor TVFloatToPreset.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTFLOAT, APermission, APermissionGroup, AEntryCode);
end;

function TVFloatToPreset.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVColorToPreset}

constructor TVColorToPreset.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTCOLOR, APermission, APermissionGroup, AEntryCode);
end;

function TVColorToPreset.Get: TVPreset; cdecl;
begin
  //TODO: maybe return full-color preset?
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVBooleanToPreset}

constructor TVBooleanToPreset.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value<>DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

function TVBooleanToPreset.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVBufferToPreset}

constructor TVBufferToPreset.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(EmptyBuffer, APermission, APermissionGroup, AEntryCode);
end;

function TVBufferToPreset.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVStringToPreset}

constructor TVStringToPreset.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(IPString(PresetUtil.Presets[Value].Inputs[ParamID(NAMEINPUTNAME, vString)]).Value, APermission, APermissionGroup, AEntryCode);
end;

function TVStringToPreset.Get: TVPreset; cdecl;
var
  APreset: IPVisualisation;
begin
  APreset:=PresetUtil.PresetsByName[FConvertFrom.Get];
  if APreset<>nil
    then Result:=APreset.ID
    else Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION TVPointerToPreset}

constructor TVPointerToPreset.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToPreset.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToPreset.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPOINTER, APermission, APermissionGroup, AEntryCode);
end;

function TVPointerToPreset.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}

end.

