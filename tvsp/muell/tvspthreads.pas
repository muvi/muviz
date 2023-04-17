unit TVSPThreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, TVSPCommand;

const
  TVSPTHREADCOUNT = 16;

type
  TTVSPThread      = class (TThread)
  strict private
    FEvent   : TEvent;
    FOnFinish: TNotifyEvent;
    FCommand : TTVSPCommand;
  protected
    procedure Execute; override;
  public
    constructor Create(AOnFinish: TNotifyEvent);
    destructor Destroy; override;
    procedure Terminate; reintroduce;
    procedure SetTask(ACommand: TTVSPCommand);
    property Terminated;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  TTVSPThreadArray = array [0..TVSPTHREADCOUNT-1] of TTVSPThread;

  TTVSPThreads     = class
  strict private
    FThreads        : TTVSPThreadArray;
    FOpenThreads    : TTVSPThreadArray;
    FOpenThreadPos  : Integer;
    FOpenThreadCount: Integer;
    FThreadLock     : TCriticalSection;
    FThreadFreeEvent: TEvent;
    procedure ThreadFinished(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCommand(ACommand: TTVSPCommand);
  end;

implementation

{%REGION TTVSPThread}

constructor TTVSPThread.Create(AOnFinish: TNotifyEvent);
begin
  FOnFinish:=AOnFinish;
  inherited Create(true);
  FreeOnTerminate:=true;
  FEvent:=TEvent.Create(nil, false, false, '');
end;

destructor TTVSPThread.Destroy;
begin
  FEvent.Destroy;
  inherited Destroy;
end;

procedure TTVSPThread.Terminate;
begin
  inherited Terminate;
  FEvent.SetEvent;
end;

procedure TTVSPThread.Execute;
begin
  while true do begin
    FEvent.WaitFor(INFINITE);
    if Terminated
      then exit;
    FCommand.Execute;
    FOnFinish(Self);
  end;
end;

procedure TTVSPThread.SetTask(ACommand: TTVSPCommand);
begin
  FCommand:=ACommand;
  FEvent.SetEvent;
end;

{%ENDREGION}
{%REGION TTVSPThreads}

constructor TTVSPThreads.Create;
var
  I: Integer;
begin
  inherited Create;
  FOpenThreadPos:=0;
  FOpenThreadCount:=TVSPTHREADCOUNT;
  FThreadLock:=TCriticalSection.Create;
  FThreadFreeEvent:=TEvent.Create(nil, false, false, '');
  for I:=0 to TVSPTHREADCOUNT-1 do begin
    FThreads[I]:=TTVSPThread.Create(@ThreadFinished);
    FOpenThreads[I]:=FThreads[I];
  end;
end;

destructor TTVSPThreads.Destroy;
var
  I: Integer;
begin
  for I:=0 to TVSPTHREADCOUNT-1
    do FThreads[I].Terminate;
  inherited Destroy;
  FThreadLock.Destroy;
  FThreadFreeEvent.Destroy;
end;

procedure TTVSPThreads.ThreadFinished(Sender: TObject);
var
  AThread: TTVSPThread;
begin
  AThread:=TTVSPThread(Sender);
  if not AThread.Terminated then begin
    FThreadLock.Enter;

    FOpenThreads[(FOpenThreadPos + FOpenThreadCount) mod TVSPTHREADCOUNT]:=AThread;
    Inc(FOpenThreadCount);

    FThreadLock.Leave;
    FThreadFreeEvent.SetEvent;
  end;
end;

procedure TTVSPThreads.AddCommand(ACommand: TTVSPCommand);
var
  AThread: TTVSPThread;
begin
  FThreadLock.Enter;

  while FOpenThreadCount = 0 do begin
    FThreadLock.Leave;

    FThreadFreeEvent.WaitFor(INFINITE);

    FThreadLock.Enter;
  end;

  AThread:=FOpenThreads[FOpenThreadPos];
  FOpenThreadPos:=(FOpenThreadPos+1) mod TVSPTHREADCOUNT;
  Dec(FOpenThreadCount);

  FThreadLock.Leave;

end;

{%ENDREGION}

end.

