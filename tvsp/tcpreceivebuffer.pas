unit TCPReceiveBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TTCPReceiveBuffer = class
  strict private
    FBuffer      : Pointer;
    FCurrentSize : Cardinal;
    FExpectedSize: Cardinal;
    FBufferSize  : Cardinal;
    FLock        : TCriticalSection;
    function GetBytesRemaining: Cardinal;
    procedure SetExpectedSize(Value: Cardinal);
    function GetWriteBuffer: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BytesAdded(ACount: Cardinal);
    property CompleteBuffer: Pointer read FBuffer;
    property BytesRemaining: Cardinal read GetBytesRemaining;
    property ExpectedSize: Cardinal read FExpectedSize write SetExpectedSize;
    property Lock: TCriticalSection read FLock;
    property WriteBuffer: Pointer read GetWriteBuffer;
  end;

const
  INITIALBUFFERSIZE = 1024;

implementation

{%REGION TTCPReceiveBuffer}

constructor TTCPReceiveBuffer.Create;
begin
  inherited Create;
  FLock:=TCriticalSection.Create;
  FBufferSize:=INITIALBUFFERSIZE;
  FExpectedSize:=0;
  FCurrentSize:=0;
  GetMem(FBuffer, INITIALBUFFERSIZE);
end;

destructor TTCPReceiveBuffer.Destroy;
begin
  Assert(FCurrentSize = FExpectedSize);
  FreeMem(FBuffer, FBufferSize);
  FLock.Destroy;
  inherited Destroy;
end;

procedure TTCPReceiveBuffer.BytesAdded(ACount: Cardinal);
begin
  Inc(FCurrentSize, ACount);
  Assert(FCurrentSize <= FExpectedSize);
end;

function TTCPReceiveBuffer.GetBytesRemaining: Cardinal;
begin
  Assert(FExpectedSize >= FCurrentSize);
  Result:=FExpectedSize - FCurrentSize;
end;

procedure TTCPReceiveBuffer.SetExpectedSize(Value: Cardinal);
begin
  //buffer has to be read completely
  Assert(FCurrentSize = FExpectedSize);
  FCurrentSize:=0;
  FExpectedSize:=Value;

  if FBufferSize < Value then begin
    FreeMem(FBuffer, FBufferSize);
    FBufferSize:=Value;
    GetMem(FBuffer, FBufferSize);
  end;
end;

function TTCPReceiveBuffer.GetWriteBuffer: Pointer;
begin
  Result:=FBuffer + FCurrentSize;
end;

{%ENDREGION}

end.

