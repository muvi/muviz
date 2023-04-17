unit PointerParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  ParamFunc, MStrings, Doors, StdPermissions;

type
  TVCallToPointer        = class (TVPointerParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVIntegerToPointer     = class (TVPointerParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVFloatToPointer       = class (TVPointerParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVColorToPointer       = class (TVPointerParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVBooleanToPointer     = class (TVPointerParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVBufferToPointer      = class (TVPointerParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVStringToPointer      = class (TVPointerParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

  TVPresetToPointer     = class (TVPointerParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPointer; cdecl; override;
  end;

implementation

{%REGION TVCallToPointer}

constructor TVCallToPointer.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionGroup, AEntryCode);
end;

function TVCallToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION TVIntegerToPointer}

constructor TVIntegerToPointer.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  //TODO: test this
  FConvertFrom.&Set(Value.InversePriority, APermission, APermissionGroup, AEntryCode);
end;

function TVIntegerToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION TVFloatToPointer}

constructor TVFloatToPointer.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTFLOAT, APermission, APermissionGroup, AEntryCode);
end;

function TVFloatToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION TVColorToPointer}

constructor TVColorToPointer.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTCOLOR, APermission, APermissionGroup, AEntryCode);
end;

function TVColorToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION TVBooleanToPointer}

constructor TVBooleanToPointer.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  //true, if Value is not the default pointer
  FConvertFrom.&Set(Value.Input.Param.Name<>'', APermission, APermissionGroup, AEntryCode);
end;

function TVBooleanToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION TVBufferToPointer}

constructor TVBufferToPointer.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(EmptyBuffer, APermission, APermissionGroup, AEntryCode);
end;

function TVBufferToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}
{%REGION TVStringToPointer}

constructor TVStringToPointer.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(PointerToStr(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVStringToPointer.Get: TVPointer; cdecl;
begin
  Result:=StrToPointer(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVPresetToPointer}

constructor TVPresetToPointer.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToPointer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToPointer.&Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

function TVPresetToPointer.Get: TVPointer; cdecl;
begin
  Result:=DEFAULTPOINTER;
end;

{%ENDREGION}

end.

