unit IntegerParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  MStrings, ParamFunc, Doors, StdPermissions;

type
  TVCallToInteger        = class (TVIntegerParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVFloatToInteger       = class (TVIntegerParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVColorToInteger       = class (TVIntegerParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVBooleanToInteger     = class (TVIntegerParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVBufferToInteger      = class (TVIntegerParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVStringToInteger      = class (TVIntegerParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVPresetToInteger     = class (TVIntegerParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

  TVPointerToInteger     = class (TVIntegerParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVInteger; cdecl; override;
  end;

implementation

{%REGION TVCallToInteger}

constructor TVCallToInteger.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionOwner, AEntryCode);
end;

function TVCallToInteger.Get: TVInteger; cdecl;
begin
  Result:=DEFAULTINTEGER;
end;

{%ENDREGION}
{%REGION TVFloatToInteger}

constructor TVFloatToInteger.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value, APermission, APermissionOwner, AEntryCode);
end;

function TVFloatToInteger.Get: TVInteger; cdecl;
begin
  Result:=FloatToInteger(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVColorToInteger}

constructor TVColorToInteger.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value, APermission, APermissionOwner, AEntryCode);
end;

function TVColorToInteger.Get: TVInteger; cdecl;
begin
  Result:=FConvertFrom.Get;
end;

{%ENDREGION}
{%REGION TVBooleanToInteger}

constructor TVBooleanToInteger.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value>0, APermission, APermissionOwner, AEntryCode);
end;

function TVBooleanToInteger.Get: TVInteger; cdecl;
begin
  Result:=Ord(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVBufferToInteger}

constructor TVBufferToInteger.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
var
  ABuffer   : TVBuffer;
  AOldSize,I: Integer;
begin
  if Value<0
    then Value:=0;
  ABuffer:=FConvertFrom.Get;
  AOldSize:=ABuffer.Size;
  if AOldSize<>Value then begin
    ABuffer:=ABuffer.Resize(Value);
    for I:=AOldSize to Value-1
      do ABuffer[I]:=0.0;
  end;
  FConvertFrom.&Set(ABuffer, APermission, APermissionOwner, AEntryCode);
end;

function TVBufferToInteger.Get: TVInteger; cdecl;
begin
  Result:=FConvertFrom.Get.Size;
end;

{%ENDREGION}
{%REGION TVStringToInteger}

constructor TVStringToInteger.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(IntToStr(Value), APermission, APermissionOwner, AEntryCode);
end;

function TVStringToInteger.Get: TVInteger; cdecl;
begin
  Result:=StrToIntE(FConvertFrom.Get,DEFAULTINTEGER);
end;

{%ENDREGION}
{%REGION TVPresetToInteger}

constructor TVPresetToInteger.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionOwner, AEntryCode);
end;

function TVPresetToInteger.Get: TVInteger; cdecl;
begin
  //NOTE: how to sensefully convert a preset into an int ?!?
  Result:=DEFAULTINTEGER;
end;

{%ENDREGION}
{%REGION TVPointerToInteger}

constructor TVPointerToInteger.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToInteger.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToInteger.&Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
var
  AValue: TVPointer;
begin
  //TODO: test this
  AValue:=FConvertFrom.Get;
  AValue.InversePriority:=Value;
  FConvertFrom.&Set(AValue, APermission, APermissionOwner, AEntryCode);
end;

function TVPointerToInteger.Get: TVInteger; cdecl;
begin
  //TODO: test this
  Result:=FConvertFrom.Value.InversePriority;
end;

{%ENDREGION}

end.

