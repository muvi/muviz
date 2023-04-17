unit BufferParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  Buffers, ParamFunc, MStrings, Doors, StdPermissions;

type
  TVCallToBuffer        = class (TVBufferParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVIntegerToBuffer     = class (TVBufferParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVFloatToBuffer       = class (TVBufferParam)
  private
     FConvertFrom: IPFloat;
  public
    constructor Create(AConvertFrom: IPFloat);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVColorToBuffer       = class (TVBufferParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVBooleanToBuffer     = class (TVBufferParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVStringToBuffer      = class (TVBufferParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVPresetToBuffer     = class (TVBufferParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

  TVPointerToBuffer     = class (TVBufferParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

implementation

{%REGION TVCallToBuffer}

constructor TVCallToBuffer.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionGroup, AEntryCode);
end;

function TVCallToBuffer.Get: TVBuffer; cdecl;
begin
  Result:=EmptyBuffer;
end;

{%ENDREGION}
{%REGION TVIntegerToBuffer}

constructor TVIntegerToBuffer.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value.Size, APermission, APermissionGroup, AEntryCode);
end;

function TVIntegerToBuffer.Get: TVBuffer; cdecl;
var
  ASize,I: Integer;
begin
  ASize:=FConvertFrom.Get;
  if ASize<0
    then ASize:=0;
  Result:=EmptyBuffer.Resize(ASize);
  for I:=0 to ASize-1
    do Result[I]:=0.0;
end;

{%ENDREGION}
{%REGION TVFloatToBuffer}

constructor TVFloatToBuffer.Create(AConvertFrom: IPFloat);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVFloatToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVFloatToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Value.Size<1
    then FConvertFrom.&Set(DEFAULTFLOAT, APermission, APermissionGroup, AEntryCode)
    else FConvertFrom.&Set(Value[0], APermission, APermissionGroup, AEntryCode);
end;

function TVFloatToBuffer.Get: TVBuffer; cdecl;
begin
  Result:=TVFloatBufferImpl.Create([FConvertFrom.Get]);
end;

{%ENDREGION}
{%REGION TVColorToBuffer}

constructor TVColorToBuffer.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTCOLOR, APermission, APermissionGroup, AEntryCode);
end;

function TVColorToBuffer.Get: TVBuffer; cdecl;
begin
  Result:=EmptyBuffer;
end;

{%ENDREGION}
{%REGION TVBooleanToBuffer}

constructor TVBooleanToBuffer.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(Value.Size>0, APermission, APermissionGroup, AEntryCode);
end;

function TVBooleanToBuffer.Get: TVBuffer; cdecl;
begin
  if FConvertFrom.Get
    then Result:=TVFloatBufferImpl.Create([0.0])
    else Result:=EmptyBuffer;
end;

{%ENDREGION}
{%REGION TVStringToBuffer}

constructor TVStringToBuffer.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(FloatBufferToString(Value), APermission, APermissionGroup, AEntryCode);
end;

function TVStringToBuffer.Get: TVBuffer; cdecl;
begin
  Result:=StringToFloatBuffer(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVPresetToBuffer}

constructor TVPresetToBuffer.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

function TVPresetToBuffer.Get: TVBuffer; cdecl;
begin
  Result:=EmptyBuffer;
end;

{%ENDREGION}
{%REGION TVPointerToBuffer}

constructor TVPointerToBuffer.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToBuffer.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToBuffer.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPOINTER, APermission, APermissionGroup, AEntryCode);
end;

function TVPointerToBuffer.Get: TVBuffer; cdecl;
begin
  Result:=EmptyBuffer;
end;

{%ENDREGION}

end.

