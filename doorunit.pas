unit DoorUnit;

{$mode objfpc}{$H+}

interface

(*
  Doorway implementation for entering methods exactly once.
*)

uses
  Classes, SysUtils, Doors, DoorUtils;

type
  TDoorBlock     = UInt64;

const
  DoorCount       = 256;
  DoorQueueLength = 10*DoorCount;

type
  TDoorValue   = Cardinal;

  IDoorKey     = interface (IEntryCode)
    function GetDoorIndex: Cardinal;
    function GetDoorValue: TDoorValue;
    property DoorIndex: Cardinal read GetDoorIndex;
    property DoorValue: TDoorValue read GetDoorValue;
  end;

  TDoorKey     = class (IDoorKey)
  private
    FRefCount: Cardinal;
    FIndex   : Cardinal;
    FValue   : Cardinal;
    function GetDoorIndex: Cardinal;
    function GetDoorValue: TDoorValue;
  public
    constructor Create(AIndex: Cardinal);
    destructor Destroy; override;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out Obj): LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  EOutOfDoors  = class (Exception)
  end;

  TDoorway     = class (TInterfacedObject, IDoorway)
  private
    FDoors: array [0..DoorCount-1] of TDoorValue;
  public
    constructor Create;
    destructor Destroy; override;
    //enters the doorway. If the Doorway was already enetered using the same
    //code, the method will return false.
    function Enter(AEntryCode: TEntryCode): Boolean; cdecl;
  end;

  TDoorManager = class (TInterfacedObject, IDoorManager)
  private
    FDoorQueue     : array [0..DoorQueueLength-1] of TDoorKey;
    FDoorQueueFront: Cardinal;
    FDoorQueueBack : Cardinal;
    //used for destruction
    FAllDoors      : array [0..DoorCount-1] of TDoorKey;
    function IncPos(var X: Cardinal): Cardinal; inline;
    procedure Init; inline;
    procedure Done; inline;
  protected
    procedure KeyReleased(ADoorKey: TDoorKey);
  public
    constructor Create;
    destructor Destroy; override;
    function NewDoorway: IDoorway; cdecl;
    //for debugging only
    procedure Reset;
    //creates a unique entry code. This can can open every doorway exactly once.
    //CAUTION: After getting 255 new entry codes, the entry code which was
    //returned before is invalid.
    //so don't use to much entry codes in parallel.
    function NewEntryCode: TEntryCode; cdecl;
  end;

var
  DoorManager: TDoorManager;

implementation

{%REGION TDoorKey}

constructor TDoorKey.Create(AIndex: Cardinal);
begin
  inherited Create;
  FRefCount:=0;
  FIndex:=AIndex;
  FValue:=0;
end;

destructor TDoorKey.Destroy;
begin
  Assert(FRefCount = 0);
  inherited Destroy;
end;

function TDoorKey.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out Obj): LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, Obj)
    then Result:=S_OK
    else Result:=LongInt(E_NOINTERFACE);
end;

function TDoorKey._AddRef: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=InterLockedIncrement(FRefCount);
end;

function TDoorKey._Release: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=InterLockedDecrement(FRefCount);
  if Result=0 then begin
    Inc(FValue);
    DoorManager.KeyReleased(Self);
  end;
end;

function TDoorKey.GetDoorIndex: Cardinal;
begin
  Result:=FIndex;
end;

function TDoorKey.GetDoorValue: Cardinal;
begin
  //value is never 0 because doors are initialized with 0
  Result:=(FValue shl 1) or 1;
end;

{%ENDREGION}
{%REGION TDoorManager}

constructor TDoorManager.Create;
begin
  inherited Create;
  Init;
end;

destructor TDoorManager.Destroy;
begin
  Done;
  inherited Destroy;
end;

procedure TDoorManager.Init;
var
  I: Integer;
begin
  FDoorQueueFront:=DoorQueueLength-1;
  FDoorQueueBack:=FDoorQueueFront;
  for I:=0 to DoorCount-1 do begin
    FAllDoors[I]:=TDoorKey.Create(I);
    KeyReleased(FAllDoors[I]);
  end;
end;

procedure TDoorManager.Done;
var
  I: Integer;
begin
  for I:=0 to DoorCount-1
    do FAllDoors[I].Destroy;
end;

function TDoorManager.NewDoorway: IDoorway; cdecl;
begin
  Result:=TDoorway.Create;
end;

function TDoorManager.IncPos(var X: Cardinal): Cardinal; inline;
begin
  Result:=(InterLockedIncrement(X)) mod DoorQueueLength;
end;

function TDoorManager.NewEntryCode: TEntryCode; cdecl;
var
  AIndex : Cardinal;
  AResult: TDoorKey;
begin
  AIndex:=IncPos(FDoorQueueFront);
  AResult:=FDoorQueue[AIndex];
  FDoorQueue[AIndex]:=nil;
  if AResult = nil
    then raise EOutOfDoors.Create('Out of doors');
  Result:=IEntryCode(IDoorKey(AResult));
end;

procedure TDoorManager.KeyReleased(ADoorKey: TDoorKey);
begin
  FDoorQueue[IncPos(FDoorQueueBack)]:=ADoorKey;
end;

procedure TDoorManager.Reset;
var
  I: Integer;
begin
  Done;
  for I:=0 to Length(FDoorQueue)-1
    do FDoorQueue[I]:=nil;
  Init;
end;

{%ENDREGION}
{%REGION TDoorway}

constructor TDoorway.Create;
begin
  inherited Create;
  FillChar(FDoors, SizeOf(FDoors), 0);
end;

destructor TDoorway.Destroy;
begin
  inherited Destroy;
end;

function TDoorway.Enter(AEntryCode: TEntryCode): Boolean; cdecl;
begin
  with IDoorKey(AEntryCode)
    do Result:=InterLockedExchange(FDoors[DoorIndex], DoorValue) <> DoorValue;
end;

{%ENDREGION}

initialization
  DoorManager:=TDoorManager.Create;
  DoorUtils.DoorManager:=DoorManager;
finalization
  DoorUtils.DoorManager:=nil;
end.

