unit BooleanParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  ParamFunc, MStrings, Buffers, Math, Doors, StdPermissions;

type
  TVCallToBoolean        = class (TVBooleanParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVIntegerToBoolean     = class (TVBooleanParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVFloatToBoolean       = class (TVBooleanParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVColorToBoolean       = class (TVBooleanParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVBufferToBoolean      = class (TVBooleanParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVStringToBoolean      = class (TVBooleanParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVPresetToBoolean     = class (TVBooleanParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

  TVPointerToBoolean     = class (TVBooleanParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBoolean; cdecl; override;
  end;

implementation

{%REGION TVCallToBoolean}

constructor TVCallToBoolean.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionGroup, AEntryCode);
end;

function TVCallToBoolean.Get: TVBoolean; cdecl;
begin
  Result:=DEFAULTBOOLEAN;
end;

{%ENDREGION}
{%REGION TVIntegerToBoolean}

constructor TVIntegerToBoolean.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Ord(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVIntegerToBoolean.Get: TVBoolean; cdecl;
begin
  Result:=FConvertFrom.Get>0;
end;

{%ENDREGION}
{%REGION TVFloatToBoolean}

constructor TVFloatToBoolean.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Ord(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVFloatToBoolean.Get: TVBoolean; cdecl;
var
  AValue: TVFloat;
begin
  AValue:=FConvertFrom.Value;
  Result:=(not isNan(AValue)) and (AValue > 0.0);
end;

{%ENDREGION}
{%REGION TVColorToBoolean}

constructor TVColorToBoolean.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Value
    then FConvertFrom.&Set($FFFFFFFF, APermission, APermissionGroup, AEntryCode)
    else FConvertFrom.&Set($00000000, APermission, APermissionGroup, AEntryCode);
end;

function TVColorToBoolean.Get: TVBoolean; cdecl;
begin
  //true, falls die Farbe sichtbar ist
  Result:=(FConvertFrom.Get and $FF000000>0);
end;

{%ENDREGION}
{%REGION TVBufferToBoolean}

constructor TVBufferToBoolean.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
var
  ABuffer: TVBuffer;
  ASize  : Integer;
begin
  ABuffer:=FConvertFrom.Get;
  ASize:=ABuffer.Size;
  if Value and (ASize = 0) then begin
    FConvertFrom.&Set(TVFloatBufferImpl.Create([0.0]), APermission, APermissionGroup, AEntryCode);
    exit;
  end;
  if (not Value) and (ASize > 0) then begin
    FConvertFrom.&Set(EmptyBuffer, APermission, APermissionGroup, AEntryCode);
    exit;
  end;
  FConvertFrom.&Set(ABuffer, APermission, APermissionGroup, AEntryCode);
end;

function TVBufferToBoolean.Get: TVBoolean; cdecl;
begin
  Result:=(FConvertFrom.Get.Size>0);
end;

{%ENDREGION}
{%REGION TVStringToBoolean}

constructor TVStringToBoolean.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Value
    then FConvertFrom.&Set('Yes', APermission, APermissionGroup, AEntryCode)
    else FConvertFrom.&Set('No', APermission, APermissionGroup, AEntryCode);
end;

function TVStringToBoolean.Get: TVBoolean; cdecl;
begin
  if FConvertFrom.Get.Length>0
    then Result:=FConvertFrom.Get[0] in TrueChars
    else Result:=false;
end;

{%ENDREGION}
{%REGION TVPresetToBoolean}

constructor TVPresetToBoolean.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

function TVPresetToBoolean.Get: TVBoolean; cdecl;
begin
  //TODO: maybe check if the preset exists
  Result:=DEFAULTBOOLEAN;
end;

{%ENDREGION}
{%REGION TVPointerToBoolean}

constructor TVPointerToBoolean.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToBoolean.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToBoolean.&Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPOINTER, APermission, APermissionGroup, AEntryCode);
end;

function TVPointerToBoolean.Get: TVBoolean; cdecl;
var
  AVal: TVPointer;
begin
  AVal:=FConvertFrom.Get;
  Result:=AVal.Input.Param.Name <> '';
end;

{%ENDREGION}

end.

