unit StdParamTypeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, VisType2, AttachedParams, Math, MStrings,
  TVSPSources, LogUnit, Doors, StdPermissions;

type
                                                           //do not implement this to avoid casting issues
  TVParam                        = class (TInterfacedObject{, IPParam})
  private
    FConvertFrom: IPParam;
  protected
    function GetType: TPParamType; virtual; abstract;
    function GetID: TPParamID; cdecl;
    function GetOwner: IPVisualisation; cdecl;
    function GetAttachedInterface(AID: TGUID): IInterface; cdecl;
    function IsDeleted: Boolean; cdecl;
    property ConvertFrom: IPParam read FConvertFrom;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure AttachInterface(AInterface: IInterface); cdecl;
    procedure Attach(AParam: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AInversePriority: LongInt); cdecl;
    procedure Detach(AParam: IPParam); cdecl;
    function LowestInversePriority: LongInt; cdecl;
    function HighestInversePriority: LongInt; cdecl;
    procedure AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl;
    procedure RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; virtual; abstract;
    procedure ToStream(out ADest); cdecl; unimplemented;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; EntryCode: TEntryCode = nil); cdecl; unimplemented;
    procedure Delete; cdecl;
  end;

  TVCallParam                    = class (TVParam, IPCall)
  protected
    function GetType: TPParamType; override;
  public
    procedure &Set; cdecl;
    procedure &Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVIntegerParam                 = class (TVParam, IPInteger)
  protected
    function GetType: TPParamType; override;
    function GetMin: TVInteger; cdecl;
    function GetMax: TVInteger; cdecl;
  public
    procedure &Set(Value: TVInteger); cdecl;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVInteger; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVFloatParam                   = class (TVParam, IPFloat)
  protected
    function GetType: TPParamType; override;
    function GetMin: TVFloat; cdecl;
    function GetMax: TVFloat; cdecl;
  public
    procedure &Set(Value: TVFloat); cdecl;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVFloat; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVColorParam                   = class (TVParam, IPColor)
  protected
    function GetType: TPParamType; override;
  public
    procedure &Set(Value: TVColor); cdecl;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVColor; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVBooleanParam                 = class (TVParam, IPBoolean)
  protected
    function GetType: TPParamType; override;
  public
    procedure &Set(Value: TVBoolean); cdecl;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVBoolean; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVBufferParam                  = class (TVParam, IPBuffer)
  protected
    function GetType: TPParamType; override;
  public
    procedure &Set(Value: TVBuffer); cdecl;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVBuffer; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVStringParam                  = class (TVParam, IPString)
  protected
    function GetType: TPParamType; override;
  public
    procedure &Set(Value: TVString); cdecl;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVString; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

  TVPresetParam                  = class (TVParam, IPPreset)
  protected
    function GetType: TPParamType; override;
    function IsForceAlive: Boolean; cdecl; virtual;
  public
    procedure AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
    procedure RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
    procedure &Set(Value: TVPreset); cdecl;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVPreset; cdecl; virtual; abstract;
    function GetExecutedValue: TVPreset; cdecl;
    function GetEnvironment: IPVisualisationEnvironment; cdecl;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property Environment: IPVisualisationEnvironment read GetEnvironment;
    property ForceAlive: Boolean read IsForceAlive;
    property &Type: TPParamType read GetType;
  end;

  TVPointerParam                 = class (TVParam, IPPointer)
  protected
    function GetType: TPParamType; override;
  public
    procedure &Set(Value: TVPointer); cdecl;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; virtual; abstract;
    function Get: TVPointer; cdecl; virtual; abstract;
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
    property &Type: TPParamType read GetType;
  end;

implementation

{%REGION TVParam}

constructor TVParam.Create(AConvertFrom: IPParam);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVParam.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVParam.AttachInterface(AInterface: IInterface); cdecl;
begin
  FConvertFrom.AttachInterface(AInterface);
end;

procedure TVParam.Attach(AParam: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AInversePriority: LongInt); cdecl;
begin
  FConvertFrom.Attach(AParam, APermission, APermissionOwner, AInversePriority);
end;

procedure TVParam.Detach(AParam: IPParam); cdecl;
begin
  FConvertFrom.Detach(AParam);
end;

function TVParam.LowestInversePriority: LongInt; cdecl;
begin
  Result:=FConvertFrom.LowestInversePriority;
end;

function TVParam.HighestInversePriority: LongInt; cdecl;
begin
  Result:=FConvertFrom.HighestInversePriority;
end;

function TVParam.GetID: TPParamID; cdecl;
begin
  Result:=ParamID(FConvertFrom.ID.Name, GetType);
end;

function TVParam.GetOwner: IPVisualisation; cdecl;
begin
  Result:=FConvertFrom.Owner;
end;

function TVParam.GetAttachedInterface(AID: TGUID): IInterface; cdecl;
begin
  Result:=FConvertFrom.AttachedInterfaces[AID];
end;

procedure TVParam.AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl;
begin
  FConvertFrom.AddListener(AListener, AContext, AThread);
end;

procedure TVParam.RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  FConvertFrom.RemoveListener(AListener, AContext);
end;

procedure TVParam.ToStream(out ADest); cdecl;
begin
  raise ENotImplemented.Create('ToStream() of converter not implemented.');
end;

procedure TVParam.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; EntryCode: TEntryCode = nil); cdecl;
begin
  raise ENotImplemented.Create('FromStream() of converter not implemented.');
end;

function TVParam.IsDeleted: Boolean; cdecl;
begin
  Result:=FConvertFrom.Deleted;
end;

procedure TVParam.Delete; cdecl;
begin
  FConvertFrom.Delete;
end;

{%ENDREGION}
{%REGION TVCallParam}

function TVCallParam.GetType: TPParamType;
begin
  Result:=vCall;
end;

procedure TVCallParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vCall));
  &Set(APermission, APermissionOwner, AEntryCode);
end;

procedure TVCallParam.&Set; cdecl;
begin
  &Set(TPNOLIMIT, NULLVISID, nil);
end;

procedure TVCallParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(APermission, APermissionOwner, AEntryCode);
end;

{%ENDREGION}
{%REGION TVIntegerParam}

function TVIntegerParam.GetType: TPParamType;
begin
  Result:=vInteger;
end;

function TVIntegerParam.GetMin: TVInteger; cdecl;
begin
  Result:=not MaxInt;
end;

function TVIntegerParam.GetMax: TVInteger; cdecl;
begin
  Result:=MaxInt;
end;

procedure TVIntegerParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vInteger));
  &Set(IPInteger(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVIntegerParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  &Set(DEFAULTINTEGER, APermission, APermissionOwner, AEntryCode);
end;

procedure TVIntegerParam.&Set(Value: TVInteger); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVFloatParam}

function TVFloatParam.GetType: TPParamType;
begin
  Result:=vFloat;
end;

function TVFloatParam.GetMin: TVFloat; cdecl;
begin
  Result:=NegInfinity;
end;

function TVFloatParam.GetMax: TVFloat; cdecl;
begin
  Result:=Infinity
end;

procedure TVFloatParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  //LogUnit.Log('Converter::'+ClassName);
  Assert(ASrc.ID.&Type=TPParamType(vFloat));
  &Set(IPFloat(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVFloatParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  &Set(DEFAULTFLOAT, APermission, APermissionOwner, AEntryCode);
end;

procedure TVFloatParam.&Set(Value: TVFloat); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVColorParam}

function TVColorParam.GetType: TPParamType;
begin
  Result:=vColor;
end;

procedure TVColorParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vColor));
  &Set(IPColor(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVColorParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTCOLOR, APermission, APermissionOwner, AEntryCode);
end;

procedure TVColorParam.&Set(Value: TVColor); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVBooleanParam}

function TVBooleanParam.GetType: TPParamType;
begin
  Result:=vBoolean;
end;

procedure TVBooleanParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vBoolean));
  &Set(IPBoolean(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVBooleanParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTBOOLEAN, APermission, APermissionOwner, AEntryCode);
end;

procedure TVBooleanParam.&Set(Value: TVBoolean); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVBufferParam}

function TVBufferParam.GetType: TPParamType;
begin
  Result:=vBuffer;
end;

procedure TVBufferParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vBuffer));
  &Set(IPBuffer(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVBufferParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTBUFFER, APermission, APermissionOwner, AEntryCode);
end;

procedure TVBufferParam.&Set(Value: TVBuffer); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVStringParam}

function TVStringParam.GetType: TPParamType;
begin
  Result:=vString;
end;

procedure TVStringParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vString));
  &Set(IPString(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVStringParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTSTRING, APermission, APermissionOwner, AEntryCode);
end;

procedure TVStringParam.&Set(Value: TVString); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVPresetParam}

procedure TVPresetParam.AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
begin
  //default behaviour, because converters don't have an environment
  AddListener(AListener, AContext, AThread);
end;

procedure TVPresetParam.RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  //default behaviour, because converters don't have an environment
  RemoveListener(AListener, AContext);
end;

function TVPresetParam.GetType: TPParamType;
begin
  Result:=vPreset;
end;

function TVPresetParam.IsForceAlive: Boolean; cdecl;
begin
  Result:=false;
end;

procedure TVPresetParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vPreset));
  &Set(IPPreset(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

function TVPresetParam.GetEnvironment: IPVisualisationEnvironment; cdecl;
begin
  //default behaviour, because converters don't have one
  Result:=nil;
end;

function TVPresetParam.GetExecutedValue: TVPreset; cdecl;
begin
  //default behaviour, because converters don't have an environment
  Result:=Get;
end;

procedure TVPresetParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTPRESET, APermission, APermissionOwner, AEntryCode);
end;

procedure TVPresetParam.&Set(Value: TVPreset); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}
{%REGION TVPointerParam}

function TVPointerParam.GetType: TPParamType;
begin
  Result:=vPointer;
end;

procedure TVPointerParam.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type=TPParamType(vPointer));
  &Set(IPPointer(ASrc).Value, APermission, APermissionOwner, AEntryCode);
end;

procedure TVPointerParam.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTPOINTER, APermission, APermissionOwner, AEntryCode);
end;

procedure TVPointerParam.&Set(Value: TVPointer); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

{%ENDREGION}

end.

