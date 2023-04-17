unit ColorParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  MStrings, ParamFunc, Doors, StdPermissions;

type
  TVCallToColor        = class (TVColorParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVIntegerToColor     = class (TVColorParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVFloatToColor       = class (TVColorParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVBooleanToColor     = class (TVColorParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVBufferToColor      = class (TVColorParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVStringToColor      = class (TVColorParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVPresetToColor     = class (TVColorParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

  TVPointerToColor     = class (TVColorParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVColor; cdecl; override;
  end;

implementation

{%REGION TVCallToColor}

constructor TVCallToColor.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionGroup, AEntryCode);
end;

function TVCallToColor.Get: TVColor; cdecl;
begin
  Result:=DEFAULTCOLOR;
end;

{%ENDREGION}
{%REGION TVIntegerToColor}

constructor TVIntegerToColor.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value, APermission, APermissionGroup, AEntryCode);
end;

function TVIntegerToColor.Get: TVColor; cdecl;
begin
  Result:=FConvertFrom.Get;
end;

{%ENDREGION}
{%REGION TVFloatToColor}

constructor TVFloatToColor.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value, APermission, APermissionGroup, AEntryCode);
end;

function TVFloatToColor.Get: TVColor; cdecl;
begin
  Result:=FloatToColor(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVBooleanToColor}

constructor TVBooleanToColor.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  //true, falls die Farbe sichtbar ist
  FConvertFrom.&Set(Value and $FF000000>0, APermission, APermissionGroup, AEntryCode);
end;

function TVBooleanToColor.Get: TVColor; cdecl;
begin
  if FConvertFrom.Get
    then Result:=$FFFFFFFF
    else Result:=$00000000;
end;

{%ENDREGION}
{%REGION TVBufferToColor}

constructor TVBufferToColor.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(EmptyBuffer, APermission, APermissionGroup, AEntryCode);
end;

function TVBufferToColor.Get: TVColor; cdecl;
begin
  Result:=DEFAULTCOLOR;
end;

{%ENDREGION}
{%REGION TVStringToColor}

constructor TVStringToColor.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(IntToStr(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVStringToColor.Get: TVColor; cdecl;
begin
  Result:=StrToIntE(FConvertFrom.Get,DEFAULTCOLOR);
end;

{%ENDREGION}
{%REGION TVPresetToColor}

constructor TVPresetToColor.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

function TVPresetToColor.Get: TVColor; cdecl;
begin
  Result:=DEFAULTCOLOR;
end;

{%ENDREGION}
{%REGION TVPointerToColor}

constructor TVPointerToColor.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToColor.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToColor.&Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPOINTER, APermission, APermissionGroup, AEntryCode);
end;

function TVPointerToColor.Get: TVColor; cdecl;
begin
  Result:=DEFAULTCOLOR;
end;

{%ENDREGION}

end.

