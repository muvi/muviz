unit TestValues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPIndexedValues, csl, HashMap, TVSPClient, SyncObjs,
  VisType2, MapKeys, GUIDop;

type
  TIndexedValueEvent = procedure (Sender: TTVSPIndexedValue; ANewValue: Pointer) of object;
  TValueIndexMap     = class;

  TIndexedValue      = class (TTVSPIndexedValue)
  strict private
    FOwner   : TValueIndexMap;
    FGroup   : TGUID;
    FName    : string;
    FType    : TPParamType;
  strict protected
    function GetGroup: TGUID; override;
    function GetName: string; override;
    function GetType: TPParamType; override;
  public
    constructor Create(AOwner: TValueIndexMap; AGroup: TGUID; AName: string; AType: TPParamType);
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    procedure ReceiveChange(ANewValue: Pointer); override;
  end;

  TValueIndexMap     = class
  strict private
    FMap     : TMap;
    FLock    : TCriticalSection;
    FClient  : TTVSPClient;
    FOnChange: TIndexedValueEvent;
  private
    procedure Changed(AItem: TIndexedValue; ANewValue: Pointer);
  public
    constructor Create(AClient: TTVSPClient);
    destructor Destroy; override;
    function GetItem(AGroup: TGUID; AType: TPParamType; AName: string): TTVSPIndexedValue;
    property Items[AGroup: TGUID; AType: TPParamType; AName: string]: TTVSPIndexedValue read GetItem; default;
    property OnChange: TIndexedValueEvent read FOnChange write FOnChange;
  end;

implementation

{%REGION TIndexedValue}

constructor TIndexedValue.Create(AOwner: TValueIndexMap; AGroup: TGUID; AName: string; AType: TPParamType);
begin
  inherited Create;
  FOwner:=AOwner;
  FGroup:=AGroup;
  FName:=AName;
  FType:=AType;
end;

function TIndexedValue.Equals(Obj: TObject): Boolean;
var
  AOther: TTVSPIndexedValue;
begin
  Result:=(Obj<>nil) and Obj.InheritsFrom(TTVSPIndexedValue);
  if Result then begin
    AOther:=TTVSPIndexedValue(Obj);
    Result:=(AOther.Group = Group) and (AOther.Name = Name) and (AOther.&Type = &Type);
  end;
end;

function TIndexedValue.GetHashCode: PtrInt;
begin
  Result:=GetGUIDHash(FGroup) xor GetStringHash(FName) xor FType;
end;

procedure TIndexedValue.ReceiveChange(ANewValue: Pointer);
begin
  FOwner.Changed(Self, ANewValue);
end;

function TIndexedValue.GetGroup: TGUID;
begin
  Result:=FGroup;
end;

function TIndexedValue.GetName: string;
begin
  Result:=FName;
end;

function TIndexedValue.GetType: TPParamType;
begin
  Result:=FType;
end;

{%ENDREGION}
{%REGION TValueIndexMap}

constructor TValueIndexMap.Create(AClient: TTVSPClient);
begin
  inherited Create;
  FClient:=AClient;
  FMap:=THashMap.Create;
  FLock:=TCriticalSection.Create;
end;

destructor TValueIndexMap.Destroy;
begin
  FMap.Clean;
  FMap.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TValueIndexMap.Changed(AItem: TIndexedValue; ANewValue: Pointer);
begin
  Assert(Assigned(FOnChange));
  FOnChange(AItem, ANewValue);
end;

function TValueIndexMap.GetItem(AGroup: TGUID; AType: TPParamType; AName: string): TTVSPIndexedValue;
var
  AKey   : TIndexedValue;
  AResult: TObject;
begin
  FLock.Enter;

  AKey:=TIndexedValue.Create(Self, AGroup, AName, AType);
  AResult:=FMap[AKey];
  if AResult = nil then begin
    Result:=AKey;
    FClient.RegisterIndex(Result);
    FMap.Add(Result, Result);
  end else begin
    AKey.Destroy;
    Result:=TTVSPIndexedValue(AResult);
  end;

  FLock.Leave;
end;

{%ENDREGION}

end.

