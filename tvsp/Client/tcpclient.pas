//TODO: use a secure net instead!!!
//this is insecure and slow...
unit TCPClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractNet, LNet, TCPNet, SyncObjs, Forms,
  TCPSocketProperties;

type
  EConnectionFailed = class (Exception)

  end;

  TTCPClient        = class (TTCPNet)
  strict private
    FAddress         : string;
    FConnectedEvent  : TEvent;
    FSocketProperties: TTCPSocketProperties;
  strict protected
    procedure Connect; override;
    procedure Receive(ASocket: TLSocket); override;
    function GetSocketProperties(ASocket: TASocket): TTCPSocketProperties; override;
  public
    constructor Create(AOwner: TComponent; AAddress: string; APort: Word);
    destructor Destroy; override;
    procedure Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal); override;
  end;

implementation

{%REGION TTCPClient}

constructor TTCPClient.Create(AOwner: TComponent; AAddress: string; APort: Word);
begin
  inherited Create(AOwner, APort);
  FAddress:=AAddress;
  FSocketProperties:=TTCPSocketProperties.Create(SocketAdded(nil));
  FSocketProperties.SendQueue.CanSend:=true;
end;

destructor TTCPClient.Destroy;
begin
  FSocketProperties.Destroy;
  inherited Destroy;
end;

procedure TTCPClient.Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
begin
  Assert(ASocket = nil);
  inherited Send(nil, AMsg, ASize);
end;

procedure TTCPClient.Connect;
begin
  if not Net.Connect(FAddress, Port)
    then raise EConnectionFailed.Create('Connection failed');

  while not Net.Connected do begin
    Application.ProcessMessages;
    Sleep(5);
  end;
end;

procedure TTCPClient.Receive(ASocket: TLSocket);
begin
  inherited Receive(nil);
end;

function TTCPClient.GetSocketProperties(ASocket: TASocket): TTCPSocketProperties;
begin
  Result:=FSocketProperties;
end;

{%ENDREGION}

end.

