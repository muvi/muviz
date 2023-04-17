unit StringParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  ParamFunc, MStrings, PresetType, Doors, StdPermissions;

type
  TVCallToString        = class (TVStringParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVIntegerToString     = class (TVStringParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVFloatToString       = class (TVStringParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVColorToString       = class (TVStringParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVBooleanToString     = class (TVStringParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVBufferToString      = class (TVStringParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVPresetToString      = class (TVStringParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

  TVPointerToString     = class (TVStringParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVString; cdecl; override;
  end;

implementation

{%REGION TVCallToString}

constructor TVCallToString.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionGroup, AEntryCode);
end;

function TVCallToString.Get: TVString; cdecl;
begin
  Result:=DEFAULTString;
end;

{%ENDREGION}
{%REGION TVIntegerToString}

constructor TVIntegerToString.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(StrToIntE(Value, DEFAULTINTEGER), APermission, APermissionGroup, AEntryCode);
end;

function TVIntegerToString.Get: TVString; cdecl;
begin
  Result:=IntToStr(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVFloatToString}

constructor TVFloatToString.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(StrToFloatE(Value, DEFAULTFLOAT), APermission, APermissionGroup, AEntryCode);
end;

function TVFloatToString.Get: TVString; cdecl;
begin
  Result:=FloatToStrF(FConvertFrom.Get, ffFixed, 7, 2);
end;

{%ENDREGION}
{%REGION TVColorToString}

constructor TVColorToString.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(StrToIntB(Value, 16, DEFAULTCOLOR), APermission, APermissionGroup, AEntryCode);
end;

function TVColorToString.Get: TVString; cdecl;
begin
  Result:=IntToHex(FConvertFrom.Get,8);
end;

{%ENDREGION}
{%REGION TVBooleanToString}

constructor TVBooleanToString.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Value.Length>0
    then FConvertFrom.&Set(Value[0] in TrueChars, APermission, APermissionGroup, AEntryCode)
    else FConvertFrom.&Set(DEFAULTBOOLEAN, APermission, APermissionGroup, AEntryCode);
end;

function TVBooleanToString.Get: TVString; cdecl;
begin
  if FConvertFrom.Get
    then Result:='Yes'
    else Result:='No';
end;

{%ENDREGION}
{%REGION TVBufferToString}

constructor TVBufferToString.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(StringToFloatBuffer(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVBufferToString.Get: TVString; cdecl;
begin
  Result:=FloatBufferToString(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVPresetToString}

constructor TVPresetToString.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
var
  APreset: IPVisualisation;
begin
  APreset:=PresetUtil.PresetsByName[Value];
  if APreset<>nil
    then FConvertFrom.&Set(APreset.ID, APermission, APermissionGroup, AEntryCode)
    else FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

function TVPresetToString.Get: TVString; cdecl;
begin
  Result:=IPString(PresetUtil[FConvertFrom.ExecutedValue].Inputs[ParamID(NAMEINPUTNAME, vString)]).Value;
end;

{%ENDREGION}
{%REGION TVPointerToString}

constructor TVPointerToString.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToString.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToString.&Set(Value: TVString; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(StrToPointer(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVPointerToString.Get: TVString; cdecl;
begin
  Result:=PointerToStr(FConvertFrom.Get);
end;

{%ENDREGION}

end.

