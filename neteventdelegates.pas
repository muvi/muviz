unit NetEventDelegates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AuthNetType, AdvClasses;

type
  TNetEventDelegate = class
  protected
    FProcList: TLList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const AProc: TNetEvent);
    procedure Remove(const AProc: TNetEvent);
    procedure Call(const AData: TNetEventData);
  end;

  TNetCallbackData  = record
    Name    : string;
    Callback: TNetEventDelegate;
  end;

  TNetEventCallbacks= class (TObject,INetCallbacks)
  protected
    FCallbacks: array of TNetCallbackData;
    procedure Clear; inline;
    function QueryInterface(constref iid: TGuid; out obj): LongInt; stdcall;
    function _AddRef: LongInt;stdcall;
    function _Release: LongInt;stdcall;
  public
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    function AddCallback(const AName: ShortString): LongInt; stdcall;
    function CallbackID(const AName: ShortString): LongInt; stdcall;
    //procedure RemoveCallback(const ID: LongInt); stdcall;
    procedure Add(const ID: LongInt; const AProc: TNetEvent); stdcall;
    procedure Remove(const ID: LongInt; const AProc: TNetEvent); stdcall;
    procedure Call(const ID: LongInt; const AData: TNetEventData); stdcall;
  end;

implementation

{TNetEventDelegate}

constructor TNetEventDelegate.Create;
begin
  inherited Create;
  FProcList:=TLList.Create(SizeOf(TNetEvent));
end;

destructor TNetEventDelegate.Destroy;
begin
  FProcList.Destroy;
  inherited Destroy;
end;

procedure TNetEventDelegate.Add(const AProc: TNetEvent);
begin
  FProcList.InsertLast(AProc);
end;

procedure TNetEventDelegate.Remove(const AProc: TNetEvent);
var
  AFoundProc: TNetEvent;
begin
  if FProcList.Empty then exit;
  FProcList.ToFirst;
  FProcList.Get(AFoundProc);
  while AFoundProc<>AProc do begin
    if FProcList.IsLast then exit;
    FProcList.Next;
    FProcList.Get(AFoundProc);
  end;
  FProcList.Delete;
end;

procedure TNetEventDelegate.Call(const AData: TNetEventData);
var
  AFoundProc: TNetEvent;
begin
  if FProcList.Empty then exit;
  FProcList.ToFirst;
  while not FProcList.isLast do begin
    FProcList.Get(AFoundProc);
    AFoundProc(AData);
    FProcList.Next;
  end;
  FProcList.Get(AFoundProc);
  AFoundProc(AData);
end;

{TNetEventCallbacks}

constructor TNetEventCallbacks.Create;
begin
  inherited Create;
end;

destructor TNetEventCallbacks.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TNetEventCallbacks.AddCallback(const AName: ShortString): LongInt; stdcall;
begin
  Result:=Length(FCallbacks);
  SetLength(FCallbacks,Result+1);
  with FCallbacks[Result] do begin
    Name:=AName;
    Callback:=TNetEventDelegate.Create;
  end;
end;

function TNetEventCallbacks.CallbackID(const AName: ShortString): LongInt; stdcall;
var
  I: Integer;
begin
  //lineare Suche, damit die Callback IDs beim sortieren nicht verändert werden
  for I:=0 to Length(FCallbacks)-1 do if FCallbacks[I].Name=AName then begin
    Result:=I;
    exit;
  end;
end;

{procedure TNetEventCallbacks.RemoveCallback(const ID: LongInt); stdcall;
var
  I,L: Integer;
begin
  L:=Length(FCallbacks);
  FCallbacks[ID].Destroy;
end;}

procedure TNetEventCallbacks.Add(const ID: LongInt; const AProc: TNetEvent); stdcall;
begin
  FCallbacks[ID].Callback.Add(AProc);
end;

procedure TNetEventCallbacks.Remove(const ID: LongInt; const AProc: TNetEvent); stdcall;
begin
  FCallbacks[ID].Callback.Remove(AProc);
end;

procedure TNetEventCallbacks.Call(const ID: LongInt; const AData: TNetEventData); stdcall;
begin
  FCallbacks[ID].Callback.Call(AData);
end;

procedure TNetEventCallbacks.Clear; inline;
var
  I: Integer;
begin
  for I:=0 to Length(FCallbacks)-1 do FCallbacks[I].Callback.Destroy;
  SetLength(FCallbacks,0);
end;

function TNetEventCallbacks.QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
begin
  Result:=0;
end;

function TNetEventCallbacks._AddRef : longint;stdcall;
begin
  Result:=1;
end;

function TNetEventCallbacks._Release : longint;stdcall;
begin
  Result:=1;
end;

end.

