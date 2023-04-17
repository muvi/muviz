unit AdvThreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, AdvClasses, SyncObjs, LCLAdvFunc;

type
  TRThread   = class (TThread)
  public
    constructor Create(CreateSuspended: Boolean = true); virtual;
    destructor Destroy; override;
  end;

  //Zeiten jeweils in Millisekunden
  TFPSThread = class (TRThread)
  private
    FTimeNeeded    : LongWord;
    FFinished      : Boolean;
    FExeTime       : LongWord;
    function GetFPS: Double;
    procedure SetFPS(Value: Double);
    function GetMaxFPS: Double;
    function GetActiveFPS: Double;
    function GetSlow: Boolean;
  protected
    procedure Execute; override;
    procedure DoExecute; virtual; abstract;
    function GetTime: LongWord; virtual; abstract;
  public
    constructor Create(CreateSuspended: Boolean = true); override;
    destructor Destroy; override;
    procedure WaitForFinish;
    property ActiveFPS: Double read GetActiveFPS;
    property ExeTime: LongWord read FExeTime write FExeTime;
    property FPS: Double read GetFPS write SetFPS;
    property MaxFPS: Double read GetMaxFPS;
    property Slow: Boolean read GetSlow;
    property TimeNeeded: LongWord read FTimeNeeded;
  end;

function GetThisThread: TThread;
property ThisThread: TThread read GetThisThread;
function ThreadMsg(Txt: string; MessageType: TMessageType):integer;

implementation

threadvar GThisThread: TRThread;

{TRThread}

constructor TRThread.Create(CreateSuspended: Boolean = true);
begin
  inherited Create(true);
  GThisThread:=Self;
  if not CreateSuspended then Start;
end;

destructor TRThread.Destroy;
begin
  GThisThread:=nil;
  inherited Destroy;
end;

{TFPSThread}

constructor TFPSThread.Create(CreateSuspended: Boolean = true);
begin
  inherited Create(true);
  FFinished:=false;
  FreeOnTerminate:=false;
  FTimeNeeded:=0;
  FExeTime:=40; //25 FPS...
  if not CreateSuspended then Resume;
end;

destructor TFPSThread.Destroy;
begin
  inherited Destroy;
end;

procedure TFPSThread.Execute;
var
  AStartTime,AEndTime: LongWord;
  ASleepTime         : Int64;
begin
  FFinished:=false;
  while not Terminated do begin
    AStartTime:=GetTime;
    DoExecute;
    AEndTime:=GetTime;
    FTimeNeeded:=AEndTime-AStartTime;
    if not Terminated then begin
      ASleepTime:=FExeTime-FTimeNeeded;
      if ASleepTime>0
        then Sleep(ASleepTime)
        else ASleepTime:=0;
      //AStartTime:=AEndTime+ASleepTime;
    end;
  end;
  FFinished:=true;
end;

procedure TFPSThread.WaitForFinish;
begin
  while not FFinished do ;
end;

function TFPSThread.GetFPS: Double;
begin
  if FExeTime=0
    then Result:=Infinity
    else Result:=1000.0/FExeTime;
end;

procedure TFPSThread.SetFPS(Value: Double);
begin
  if IsInfinite(Value)
    then FExeTime:=0
    else FExeTime:=Round(1000.0/Value);
end;

function TFPSThread.GetMaxFPS: Double;
begin
  if FTimeNeeded=0
    then Result:=Infinity
    else Result:=1000.0/FTimeNeeded;
end;

function TFPSThread.GetActiveFPS: Double;
begin
  if FTimeNeeded<=FExeTime
    then Result:=FPS
    else Result:=MaxFPS;
end;

function TFPSThread.GetSlow: Boolean;
begin
  Result:=(FTimeNeeded>FExeTime);
end;

{Allgemein}

function GetThisThread: TThread;
begin
  Result:=GThisThread;
end;

{Allgemein - ThreadMsg}

type
  TThreadMsg  = record
    Txt    : string;
    MsgType: TMessageType;
  end;

  TThreadMsgs = class
  private
    FMsgQueue: TLQueue;
    FLock    : TCriticalSection;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure Activate;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushMsg(constref AMsg: TThreadMsg);
  end;

constructor TThreadMsgs.Create;
begin
  inherited Create;
  FLock:=TCriticalSection.Create;
  FMsgQueue:=TLQueue.Create(SizeOf(TThreadMsg));
end;

destructor TThreadMsgs.Destroy;
begin
  FMsgQueue.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TThreadMsgs.AppOnIdle(Sender: TObject; var Done: Boolean);
var
  AMsg: TThreadMsg;
begin
  Done:=false;
  FLock.Acquire;
  FMsgQueue.Pop(AMsg);
  if FMsgQueue.Empty then begin
    FLock.Release;
    Application.RemoveOnIdleHandler(@AppOnIdle);
  end else FLock.Release;
  with AMsg do AdvMsg(Txt,MsgType);
end;

procedure TThreadMsgs.Activate;
begin
  Application.AddOnIdleHandler(@AppOnIdle);
end;

procedure TThreadMsgs.PushMsg(constref AMsg: TThreadMsg);
begin
  FLock.Acquire;
  if FMsgQueue.Empty then begin
    FMsgQueue.Push(AMsg);
    FLock.Release;
    GThisThread.Synchronize(@Activate);
  end else begin
    FMsgQueue.Push(AMsg);
    FLock.Release;
  end;
end;

var
  GThreadMsgs: TThreadMsgs;

function ThreadMsg(Txt: string; MessageType: TMessageType): Integer;
var
  AMsg: TThreadMsg;
begin
  if GThisThread=nil then begin
    AdvMsg(Txt,MessageType);
    exit;
  end;
  AMsg.Txt:=Txt;
  AMsg.MsgType:=MessageType;
  GThreadMsgs.PushMsg(AMsg);
end;

initialization
  GThreadMsgs:=TThreadMsgs.Create;
finalization
  GThreadMsgs.Destroy;
end.

