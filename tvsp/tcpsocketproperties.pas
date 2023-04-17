unit TCPSocketProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TCPSendQueue, TCPReceiveBuffer;

type
  TTCPSocketProperties = class
  strict private
    FAttachment   : TObject;
    FSendQueue    : TTCPSendQueue;
    FReceiveBuffer: TTCPReceiveBuffer;
  public
    constructor Create(AAttachment: TObject);
    destructor Destroy; override;
    property Attachment: TObject read FAttachment;
    property ReceiveBuffer: TTCPReceiveBuffer read FReceiveBuffer;
    property SendQueue: TTCPSendQueue read FSendQueue;
  end;

implementation

{%REGION TTCPSocketProperties}

constructor TTCPSocketProperties.Create(AAttachment: TObject);
begin
  inherited Create;
  FAttachment:=AAttachment;
  FSendQueue:=TTCPSendQueue.Create;
  FReceiveBuffer:=TTCPReceiveBuffer.Create;
end;

destructor TTCPSocketProperties.Destroy;
begin
  FReceiveBuffer.Destroy;
  FSendQueue.Destroy;
  if FAttachment <> nil
    then FAttachment.Destroy;
  inherited Destroy;
end;

{%ENDREGION}

end.

