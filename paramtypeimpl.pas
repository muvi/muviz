unit ParamTypeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamType2, VisType2, MTypes, DebugTools, Guidop,
  PluginType, MStrings, StdParamTypes, TVSPSources, Doors, StdPermissions,
  VisDebugUtils;

const
  ParamPicValueStrLength = 800;

type
  TPParamValueStorage                = class;

  TPChangedValue                     = class (TInterfacedObject)
  private
    FIsDefault: Boolean;
  public
    constructor Create(AIsDefault: Boolean);
    function GetIChangedValue: IChangedValue; virtual; abstract;
    function IsDefault: Boolean; cdecl;
  end;

  generic TPCustomChangedValue<AType>= class (TPChangedValue)
  private
    FValue: AType;
  public
    constructor Create(AValue: AType; ADefaultValue: AType);
    constructor CreateDefault(AValue: AType; AIsDefault: Boolean);
    function Get: AType; cdecl;
    property Value: AType read FValue write FValue;
  end;

  TPParamValueStorage                = class (TInterfacedObject, IPParamSettings, IDebugParam)
  private
    FPrototype         : IPParamPrototype;
    //for debugging only
    function GetPrototype: IPParamPrototype; experimental;
  strict protected
    function GetParam: IPParam; cdecl; virtual; abstract;
  protected
    function GetID: TPParamID; cdecl;
    function GetOwner: IPVisualisation; cdecl;
    function GetAttachedInterface(AID: TGUID): IInterface; cdecl;
    function IsDeleted: Boolean; cdecl;
    //registers a change class to execute
    procedure RegisterChange(AChange: TPChangedValue; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode);
    procedure DoSet(AValue: TPChangedValue); virtual;
    property Prototype: IPParamPrototype read FPrototype;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; virtual; abstract;
    //procedure GetFrom(ASrc: IPParam; AEntryCode: TEntryCode); cdecl;
    procedure ToStream(out ADest); cdecl; virtual; abstract;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; virtual; abstract;
    procedure AttachInterface(AInterface: IInterface); cdecl;
    procedure Attach(AParam: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AInversePriority: LongInt); cdecl;
    procedure Detach(AParam: IPParam); cdecl;
    function LowestInversePriority: LongInt; cdecl;
    function HighestInversePriority: LongInt; cdecl;
    procedure AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
    procedure RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; virtual; abstract;
    procedure Delete; cdecl;

    property AttachedIntefaces[AID: TGUID]: IInterface read GetAttachedInterface;
    property Deleted: Boolean read IsDeleted;
    property ID: TPParamID read GetID;
    property Param: IPParam read GetParam;
  end;

  TPParamTypeImpl                    = class (TInterfacedObject, IPParamType)
  protected
    function GetColor: TMColor; cdecl; virtual; abstract;
    function GetName: IString; cdecl; virtual; abstract;
    function GetPicture: TPParamPic; cdecl; virtual; abstract;
    function GetType: TPParamType; cdecl; virtual; abstract;
  protected
    function GetVersion: MVVersion; stdcall; virtual;
    function Future(const Version: MVVersion): IMInterface; stdcall; virtual;
    //not (sensefully) implemented yet
    procedure AddConverter(AConvertFrom: TPParamType; AConverter: TPParamConverter); cdecl; virtual; unimplemented;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; virtual; abstract;
    function Convert(ADest: IPParam): IPParam; cdecl; virtual; abstract;

    property Color: TMColor read GetColor;
    property Name: IString read GetName;
    property Picture: TPParamPic read GetPicture;
    property &Type: TPParamType read GetType;
  end;

  TPParamPicValueStr                 = array [0..ParamPicValueStrLength-1] of Char;

operator := (m: TPParamPicValueStr) r: TPParamPic;
operator := (m: string) r: TPParamPic;
function ParamCount: Cardinal;

implementation

var
  LParamCount: Cardinal = 0;

procedure IncParamCount;
begin
  InterLockedIncrement(LParamCount);
end;

procedure DecParamCount;
begin
  InterLockedDecrement(LParamCount);
end;

function ParamCount: Cardinal;
begin
  InterLockedExchange(Result, LParamCount);
end;

{%REGION TPChangedValue}

constructor TPChangedValue.Create(AIsDefault: Boolean);
begin
  inherited Create;
  FIsDefault:=AIsDefault;
end;

function TPChangedValue.IsDefault: Boolean; cdecl;
begin
  Result:=FIsDefault;
end;

{%ENDREGION}
{%REGION TPCustomChangedValue}

constructor TPCustomChangedValue.Create(AValue: AType; ADefaultValue: AType);
begin
  inherited Create(AValue = ADefaultValue);
  FValue:=AValue;
end;

constructor TPCustomChangedValue.CreateDefault(AValue: AType; AIsDefault: Boolean);
begin
  inherited Create(AIsDefault);
  FValue:=AValue;
end;

function TPCustomChangedValue.Get: AType; cdecl;
begin
  Result:=FValue;
end;

{%ENDREGION}
{%REGION TPParamValueStorage}

constructor TPParamValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create;
  IncParamCount;
  Assert(APrototype <> nil);
  FPrototype:=APrototype;
end;

destructor TPParamValueStorage.Destroy;
begin
  FPrototype:=nil;
  DecParamCount;
  //do not destroy the indexer. this is done automatically with the attached params.
  inherited Destroy;
end;

function TPParamValueStorage.GetPrototype: IPParamPrototype; experimental;
begin
  Result:=FPrototype;
end;

procedure TPParamValueStorage.RegisterChange(AChange: TPChangedValue; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode);
var
  AChangedValue: IChangedValue;
begin
  Assert(AChange<>nil);
  Assert(FPrototype <> nil);
  //zo prevent destruction if the interface is used in DoSet
  AChangedValue:=AChange.GetIChangedValue;
  DoSet(AChange);
  FPrototype.ValueChanged(AChangedValue, AEntryCode, APermission, APermissionOwner);
  //destruction of AChange not needed, because AChange is reference counted...
end;

procedure TPParamValueStorage.DoSet(AValue: TPChangedValue);
begin
  //do nothing
end;

function TPParamValueStorage.GetID: TPParamID; cdecl;
begin
  Assert(FPrototype <> nil);
  Assert(FPrototype.ID.Name <> nil);
  Result:=FPrototype.ID;
end;

function TPParamValueStorage.GetOwner: IPVisualisation; cdecl;
begin
  Assert(FPrototype <> nil);
  Result:=FPrototype.Owner;
  Assert(Result <> nil);
end;

function TPParamValueStorage.GetAttachedInterface(AID: TGUID): IInterface; cdecl;
begin
  Assert(FPrototype <> nil);
  Result:=FPrototype.AttachedInterfaces[AID];
end;

procedure TPParamValueStorage.AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
begin
  FPrototype.AddListener(AListener, AContext, AThread);
end;

procedure TPParamValueStorage.RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  FPrototype.RemoveListener(AListener, AContext);
end;

procedure TPParamValueStorage.AttachInterface(AInterface: IInterface); cdecl;
begin
  Assert(AInterface <> nil);
  FPrototype.AttachInterface(AInterface);
end;

procedure TPParamValueStorage.Attach(AParam: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AInversePriority: LongInt); cdecl;
begin
  Assert(AParam <> nil);
  FPrototype.Attach(AParam, APermission, APermissionOwner, AInversePriority);
end;

procedure TPParamValueStorage.Detach(AParam: IPParam); cdecl;
begin
  Assert(AParam <> nil);
  FPrototype.Detach(AParam);
end;

function TPParamValueStorage.LowestInversePriority: LongInt; cdecl;
begin
  Result:=FPrototype.LowestInversePriority;
end;

function TPParamValueStorage.HighestInversePriority: LongInt; cdecl;
begin
  Result:=FPrototype.HighestInversePriority;
end;

function TPParamValueStorage.IsDeleted: Boolean; cdecl;
begin
  Result:=FPrototype.Deleted;
end;

procedure TPParamValueStorage.Delete; cdecl;
begin
  FPrototype.Delete;
end;

{%ENDREGION}
{%REGION TPParamTypeImpl}

function TPParamTypeImpl.GetVersion: MVVersion; stdcall;
begin
  Result:='1.0.0';
end;

function TPParamTypeImpl.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version='0.0.0'
    then Result:=IPParamType(Self)
    else Result:=nil;
end;

procedure TPParamTypeImpl.AddConverter(AConvertFrom: TPParamType; AConverter: TPParamConverter); cdecl;
begin
  //TODO implement this
  //do nothing
  //it's not the perfect solution
  raise ENotImplemented.Create(TMethod(@AddConverter));
end;

{%ENDREGION}
{%REGION General}

operator := (m: TPParamPicValueStr) r: TPParamPic;
begin
  Move(m,r,800);
end;

operator := (m: string) r: TPParamPic;
var
  AStr: TPParamPicValueStr;
begin
  AStr:=m;
  Move(AStr,r,800);
end;

{%ENDREGION}

end.

