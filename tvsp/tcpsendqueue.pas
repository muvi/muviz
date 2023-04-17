unit TCPSendQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Queue, AbstractNet, SyncObjs, TCPType;

type
  TTCPSendMessage  = class
  strict private
    FMsg   : Pointer;
    FSize  : Cardinal;
  public
    constructor Create(AMsg: Pointer; ASize: Cardinal);
    destructor Destroy; override;
    property Msg: Pointer read FMsg;
    property Size: Cardinal read FSize;
  end;

  TTCPSendQueue    = class
  strict private
    FQueue     : TQueue;
    FLock      : TCriticalSection;
    FCanSend   : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(AMsg: Pointer; ASize: Cardinal);
    function Get: TTCPSendMessage;
    procedure Pop;
    function Empty: Boolean;
    property CanSend: Boolean read FCanSend write FCanSend;
    property Lock: TCriticalSection read FLock;
  end;

implementation

{%REGION TTCPSendMessage}

constructor TTCPSendMessage.Create(AMsg: Pointer; ASize: Cardinal);
var
  AMsg2 : ^TTCPSize;
begin
  inherited Create;
  FSize:=ASize + SizeOf(TTCPSize);
  GetMem(FMsg, FSize);

  AMsg2:=FMsg;
  AMsg2^:=ASize;
  Move(AMsg^, (AMsg2 + 1)^, ASize);
end;

destructor TTCPSendMessage.Destroy;
begin
  FreeMem(FMsg, FSize);
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTCPSendQueue}

constructor TTCPSendQueue.Create;
begin
  inherited Create;
  CanSend:=false;
  FLock:=TCriticalSection.Create;
  FQueue:=TQueue.Create;
end;

destructor TTCPSendQueue.Destroy;
begin
  Assert(FQueue.Empty);
  FQueue.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TTCPSendQueue.Push(AMsg: Pointer; ASize: Cardinal);
begin
  FQueue.Push(TTCPSendMessage.Create(AMsg, ASize));
end;

function TTCPSendQueue.Get: TTCPSendMessage;
begin
  Assert(CanSend);
  Result:=TTCPSendMessage(FQueue.Get);
end;

procedure TTCPSendQueue.Pop;
begin
  Assert(CanSend);
  FQueue.Pop.Destroy;
end;

function TTCPSendQueue.Empty: Boolean;
begin
  Result:=FQueue.Empty;
end;

{%ENDREGION}

end.

