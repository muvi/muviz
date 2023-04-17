//TODO: use a secure net instead!!!
//this is insecure and slow...
unit TCPNet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractNet, LNet, LNetComponents, TCPSocketProperties,
  SyncObjs, TCPType, TCPSendQueue;

type
  ETCP        = class (Exception);

  TTCPNet     = class (TANet)
  strict private
    FNet         : TLTCP;
    FPort        : Word;
    procedure DoSend(ASocket: TLSocket);
  strict protected
    procedure Receive(ASocket: TLSocket); virtual;
    procedure Error(const Msg: string; ASocket: TLSocket); virtual;
    procedure CanSendProc(ASocket: TLSocket); virtual;
    function GetSocketProperties(ASocket: TASocket): TTCPSocketProperties; virtual; abstract;
    property Net: TLTCP read FNet;
    property Port: Word read FPort;
    property SocketProperties[ASocket: TASocket]: TTCPSocketProperties read GetSocketProperties;
  public
    constructor Create(AOwner: TComponent; APort: Word);
    destructor Destroy; override;
    procedure Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal); override;
  end;

implementation

{%REGION TTCPNet}

constructor TTCPNet.Create(AOwner: TComponent; APort: Word);
begin
  inherited Create;
  FPort:=APort;
  FNet:=TLTCPComponent.Create(AOwner);
  FNet.OnReceive:=@Receive;
  FNet.OnError:=@Error;
  FNet.OnCanSend:=@CanSendProc;
  //FNet.SocketNet:=LAF_INET;
end;

destructor TTCPNet.Destroy;
begin
  FNet.Disconnect;
  FNet.Destroy;
  inherited Destroy;
end;

procedure TTCPNet.Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
begin
  with SocketProperties[ASocket].SendQueue do begin
    Lock.Enter;

    Push(AMsg, ASize);
    DoSend(TLSocket(ASocket));

    Lock.Leave;
  end;
end;

procedure TTCPNet.Receive(ASocket: TLSocket);
var
  ASize: TTCPSize;

  function ReadSize: Boolean;
  var
    ASizeOfSize: Cardinal;
  begin
    ASizeOfSize:=FNet.Get(ASize, SizeOf(ASize), ASocket);
    Result:=ASizeOfSize = SizeOf(ASize);
    if (not Result) and (ASizeOfSize <> 0)
      then raise ETCP.Create('Invalid size field.');
  end;

begin
  with SocketProperties[ASocket].ReceiveBuffer do begin
    Lock.Enter;

    //complete last block
    if BytesRemaining > 0 then begin
      BytesAdded(FNet.Get(WriteBuffer^, BytesRemaining, ASocket));
      if BytesRemaining > 0 then begin
        Lock.Leave;
        exit;
      end;
      OnReceive(ASocket, CompleteBuffer, ExpectedSize);
    end;

    while ReadSize do begin
      if (ASize = 0)
        then raise ETCP.Create('Size field may not be 0.');

      ExpectedSize:=ASize;

      BytesAdded(FNet.Get(WriteBuffer^, BytesRemaining, ASocket));
      if BytesRemaining > 0 then begin
        Lock.Leave;
        exit;
      end;
      OnReceive(ASocket, CompleteBuffer, ExpectedSize);
    end;

    Lock.Leave;
  end;
end;

procedure TTCPNet.CanSendProc(ASocket: TLSocket);
begin
  with SocketProperties[ASocket].SendQueue do begin
    Lock.Enter;

    CanSend:=true;
    DoSend(ASocket);

    Lock.Leave;
  end;
end;

procedure TTCPNet.DoSend(ASocket: TLSocket);
var
  AMsg     : TTCPSendMessage;
  ASendSize: Cardinal;
  AQueue   : TTCPSendQueue;
begin
  AQueue:=SocketProperties[ASocket].SendQueue;
  if not AQueue.CanSend
    then exit;

  while not AQueue.Empty do begin
    AMsg:=AQueue.Get;
    ASendSize:=FNet.Send(AMsg.Msg^, AMsg.Size, ASocket);
    if ASendSize = 0 then begin
      AQueue.CanSend:=false;
      exit;
    end;
    Assert(ASendSize = AMsg.Size);
    AQueue.Pop;
  end;
end;

procedure TTCPNet.Error(const Msg: string; ASocket: TLSocket);
begin
  raise ETCP.Create(Msg);
end;

{%ENDREGION}

end.

