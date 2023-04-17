unit FloatParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc,
  Buffers, MStrings, ParamFunc, Math, Doors, StdPermissions;

type
  TVNoNanFloatParam    = class (TVFloatParam)
  protected
    procedure SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); virtual; abstract;
  public
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
  end;

  TVCallToFloat        = class (TVFloatParam)
  private
    FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVIntegerToFloat     = class (TVNoNanFloatParam)
  private
    FConvertFrom: IPInteger;
  protected
    procedure SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); override;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVColorToFloat       = class (TVNoNanFloatParam)
  private
    FConvertFrom: IPColor;
  protected
    procedure SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); override;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVBooleanToFloat     = class (TVNoNanFloatParam)
  private
    FConvertFrom: IPBoolean;
  protected
    procedure SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); override;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVBufferToFloat      = class (TVFloatParam)
  private
    FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVStringToFloat      = class (TVFloatParam)
  private
    FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVPresetToFloat     = class (TVFloatParam)
  private
    FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVFloat; cdecl; override;
  end;

  TVPointerToFloat     = class (TVFloatParam)
  private
    FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVFloat; cdecl; override;
  end;

implementation

{%REGION TVNoNanFloatParam}

procedure TVNoNanFloatParam.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if isNan(Value)
    then ConvertFrom.GetFrom(ConvertFrom, APermission, APermissionOwner, AEntryCode)
    else SetNN(Value, APermission, APermissionOwner, AEntryCode);
end;

{%ENDREGION}
{%REGION TVCallToFloat}

constructor TVCallToFloat.Create(AConvertFrom: IPCall);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToFloat.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(APermission, APermissionOwner, AEntryCode);
end;

function TVCallToFloat.Get: TVFloat; cdecl;
begin
  Result:=DEFAULTFLOAT;
end;

{%ENDREGION}
{%REGION TVIntegerToFloat}

constructor TVIntegerToFloat.Create(AConvertFrom: IPInteger);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToFloat.SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode);
begin
  FConvertFrom.&Set(FloatToInteger(Value), APermission, APermissionOwner, AEntryCode);
end;

function TVIntegerToFloat.Get: TVFloat; cdecl;
begin
  Result:=FConvertFrom.Get;
end;

{%ENDREGION}
{%REGION TVColorToFloat}

constructor TVColorToFloat.Create(AConvertFrom: IPColor);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToFloat.SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode);
begin
  FConvertFrom.&Set(FloatToColor(Value), APermission, APermissionOwner, AEntryCode);
end;

function TVColorToFloat.Get: TVFloat; cdecl;
begin
  Result:=FConvertFrom.Get;
end;

{%ENDREGION}
{%REGION TVBooleanToFloat}

constructor TVBooleanToFloat.Create(AConvertFrom: IPBoolean);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToFloat.SetNN(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode);
begin
  FConvertFrom.&Set(Value > 0.0, APermission, APermissionOwner, AEntryCode);
end;

function TVBooleanToFloat.Get: TVFloat; cdecl;
begin
  Result:=Ord(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVBufferToFloat}

constructor TVBufferToFloat.Create(AConvertFrom: IPBuffer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToFloat.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(TVBuffer(TVFloatBufferImpl.Create([Value])), APermission, APermissionOwner, AEntryCode);
end;

function TVBufferToFloat.Get: TVFloat; cdecl;
var
  ABuffer: TVBuffer;
begin
  ABuffer:=FConvertFrom.Get;
  if ABuffer.Size<1
    then Result:=DEFAULTFloat
    else Result:=ABuffer[0];
end;

{%ENDREGION}
{%REGION TVStringToFloat}

constructor TVStringToFloat.Create(AConvertFrom: IPString);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToFloat.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(FloatToStrF(Value, ffFixed, 7, 2), APermission, APermissionOwner, AEntryCode);
end;

function TVStringToFloat.Get: TVFloat; cdecl;
begin
  Result:=StrToFloatE(FConvertFrom.Get,DEFAULTFloat);
end;

{%ENDREGION}
{%REGION TVPresetToFloat}

constructor TVPresetToFloat.Create(AConvertFrom: IPPreset);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToFloat.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET, APermission, APermissionOwner, AEntryCode);
end;

function TVPresetToFloat.Get: TVFloat; cdecl;
begin
  //NOTE: how to sensefully convert a preset into a Float ?!?
  Result:=DEFAULTFLOAT;
end;

{%ENDREGION}
{%REGION TVPointerToFloat}

constructor TVPointerToFloat.Create(AConvertFrom: IPPointer);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToFloat.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToFloat.&Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPOINTER, APermission, APermissionOwner, AEntryCode);
end;

function TVPointerToFloat.Get: TVFloat; cdecl;
begin
  Result:=DEFAULTFLOAT;
end;

{%ENDREGION}

end.

