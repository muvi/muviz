unit ThreadImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, MExceptions, SyncObjs, Queue;

type
  TPThread = class (TInterfacedObject, IPThread, IPThreadPrototype)
  strict private
    FExecutionQueue: TQueue;
    FEmergencyQueue: TQueue;
    FQueues        : array [Boolean] of TQueue; //FQueues[Emergency]
    FLock          : TCriticalSection;
    FTaskArrived   : TEvent;
    FAttached      : IInterface;
    function IsCurrent: Boolean; inline;
    function _PrototypeAddRefRelease: Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function IPThreadPrototype._AddRef = _PrototypeAddRefRelease;
    function IPThreadPrototype._Release = _PrototypeAddRefRelease;
  protected
    property Current: Boolean read IsCurrent;
    procedure DoExecuteEmergency;
    procedure DoExecute;
    function GetThread: IPThread; cdecl;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AttachInterface(AInterface: IInterface); cdecl;
    procedure ExecutionBreak(AEmergency: Boolean = true); cdecl;
    procedure Execute; cdecl;
    procedure Push(AProc: TPParamNotification; AContext: Pointer; ASender, ASenderData: IInterface; AEmergency: Boolean = false); cdecl;
    procedure CancelAll(AProc: TPParamNotification; AContext: Pointer); cdecl;
    procedure Started; cdecl;
    procedure Stopped; cdecl;
    procedure WaitForTask; cdecl;

    property Thread: IPThread read GetThread;
  end;

function GetCurrentIPThread: IPThread; inline;

implementation

{%REGION Misc}

threadvar
  CurrentThread: TPThread;

function GetCurrentIPThread: IPThread; inline;
begin
  Result:=CurrentThread;
end;

{%ENDREGION}
{%REGION TTask}

type
  TTask = class
  strict private
    FProc      : TPParamNotification;
    FContext   : Pointer;
    FSender    : IInterface;
    FSenderData: IInterface;
    FCanceled  : Cardinal;
  public
    constructor Create(AProc: TPParamNotification; AContext: Pointer; ASender, ASenderData: IInterface);
    destructor Destroy; override;
    procedure Cancel;
    //executes and destroys the task
    procedure Execute;

    property Proc: TPParamNotification read FProc;
    property Context: Pointer read FContext;
  end;

constructor TTask.Create(AProc: TPParamNotification; AContext: Pointer; ASender, ASenderData: IInterface);
begin
  inherited Create;
  FProc:=AProc;
  FContext:=AContext;
  FSender:=ASender;
  FSenderData:=ASenderData;
  FCanceled:=Cardinal(false);
end;

destructor TTask.Destroy;
begin
  FSenderData:=nil;
  FSender:=nil;
  inherited Destroy;
end;

procedure TTask.Cancel;
begin
  InterLockedExchange(FCanceled, Cardinal(true));
end;

procedure TTask.Execute;
var
  ACanceled: Cardinal;
begin
  InterLockedExchange(ACanceled, FCanceled);
  if not Boolean(ACanceled)
    then FProc(FContext, FSender, FSenderData);
  Destroy;
end;

{%ENDREGION TTask}
{%REGION TPThread}

constructor TPThread.Create;
begin
  inherited Create;
  FExecutionQueue:=TQueue.Create;
  FEmergencyQueue:=TQueue.Create;
  FQueues[false]:=FExecutionQueue;
  FQueues[true]:=FEmergencyQueue;
  FLock:=TCriticalSection.Create;
  FTaskArrived:=TSimpleEvent.Create;
end;

destructor TPThread.Destroy;
var
  ATrashTask: TTask;
begin
  FAttached:=nil;
  FTaskArrived.Destroy;
  FLock.Destroy;
  while not FEmergencyQueue.Empty do begin
    ATrashTask:=TTask(FEmergencyQueue.Pop);
    ATrashTask.Destroy;
  end;
  FEmergencyQueue.Destroy;
  while not FExecutionQueue.Empty do begin
    ATrashTask:=TTask(FExecutionQueue.Pop);
    ATrashTask.Destroy;
  end;
  FExecutionQueue.Destroy;
  inherited Destroy;
end;

function TPThread.IsCurrent: Boolean; inline;
begin
  Result:=CurrentThread = Self;
end;

procedure TPThread.DoExecuteEmergency;
var
  ATask : TTask;
begin
  //do not leave entries in the queue. This is an emergency case.
  //true is the appropriate condition here because of FLock...
  while true do begin
    FLock.Enter;
    if FEmergencyQueue.Empty then begin
      FLock.Leave;
      exit;
    end;
    ATask:=TTask(FEmergencyQueue.Pop);
    FLock.Leave;
    ATask.Execute;
  end;
end;

procedure TPThread.DoExecute;
var
  ACount, I: Cardinal;
  ATask    : TTask;
begin
  DoExecuteEmergency;

  FLock.Enter;
  ACount:=FExecutionQueue.Count;
  FLock.Leave;

  //leave new entries in the queue. This prevents endless iteration
  for I:=0 to ACount-1 do begin
    FLock.Enter;
    //may happen due to ExecutionBreaks
    if FExecutionQueue.Empty then begin
      FLock.Leave;
      exit;
    end;
    ATask:=TTask(FExecutionQueue.Pop);
    FLock.Leave;
    ATask.Execute;
  end;
end;

procedure TPThread.Push(AProc: TPParamNotification; AContext: Pointer; ASender, ASenderData: IInterface; AEmergency: Boolean = false); cdecl;
begin
  if Current then begin
    DoExecuteEmergency;
    AProc(AContext, ASender, ASenderData);
  end else begin
    FLock.Enter;
    FQueues[AEmergency].Push(TTask.Create(AProc, AContext, ASender, ASenderData));
    FLock.Leave;

    FTaskArrived.SetEvent;
  end;
end;

procedure TPThread.CancelAll(AProc: TPParamNotification; AContext: Pointer); cdecl;
var
  AItem: TObject;

  procedure CancelIfSame(ATask: TTask); inline;
  begin
    with ATask
      do if (Proc = AProc) and (Context = AContext)
        then Cancel;
  end;

begin
  FLock.Enter;

  for AItem in FExecutionQueue
    do CancelIfSame(TTask(AItem));
  for AItem in FEmergencyQueue
    do CancelIfSame(TTask(AItem));

  FLock.Leave;
end;

procedure TPThread.ExecutionBreak(AEmergency: Boolean = true); cdecl;
begin
  if not Current
    then raise EInternalException.Create(EIWRONGTHREAD, TMethod(@ExecutionBreak));
  if AEmergency
    then DoExecuteEmergency
    else DoExecute;
end;

procedure TPThread.Started; cdecl;
begin
  CurrentThread:=Self;
end;

procedure TPThread.Stopped; cdecl;
begin
  if not Current
    then raise EInternalException.Create(EIWRONGTHREAD, TMethod(@Stopped));
  CurrentThread:=nil;
end;

procedure TPThread.Execute; cdecl;
begin
  DoExecute;
end;

procedure TPThread.WaitForTask; cdecl;
begin
  FTaskArrived.ResetEvent;
  FLock.Enter;
  if (not FExecutionQueue.Empty) or (not FEmergencyQueue.Empty)
    then FTaskArrived.SetEvent;
  FLock.Leave;
  FTaskArrived.WaitFor(INFINITE);
end;

function TPThread.GetThread: IPThread; cdecl;
begin
  Result:=Self;
end;

procedure TPThread.AttachInterface(AInterface: IInterface); cdecl;
begin
  FAttached:=AInterface;
end;

function TPThread._PrototypeAddRefRelease: Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=1;
end;

{%ENDREGION}

end.

