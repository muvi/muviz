unit OpenDMX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DMXBUFFERSIZE = 513;

type
  FT_Result = LongInt;
  /// Enumaration containing the varios return status for the DLL functions.
  //enum FT_STATUS
  {
      FT_OK = 0,
      FT_INVALID_HANDLE,
      FT_DEVICE_NOT_FOUND,
      FT_DEVICE_NOT_OPENED,
      FT_IO_ERROR,
      FT_INSUFFICIENT_RESOURCES,
      FT_INVALID_PARAMETER,
      FT_INVALID_BAUD_RATE,
      FT_DEVICE_NOT_OPENED_FOR_ERASE,
      FT_DEVICE_NOT_OPENED_FOR_WRITE,
      FT_FAILED_TO_WRITE_DEVICE,
      FT_EEPROM_READ_FAILED,
      FT_EEPROM_WRITE_FAILED,
      FT_EEPROM_ERASE_FAILED,
      FT_EEPROM_NOT_PRESENT,
      FT_EEPROM_NOT_PROGRAMMED,
      FT_INVALID_ARGS,
      FT_OTHER_ERROR
  }

  TOpenDMX = class (TThread)
  strict private
    FBuffer      : array [0..DMXBUFFERSIZE-1] of Byte;
    FDMXHandle   : LongWord;
    FBytesWritten: Integer;
    FStatus      : FT_Result;
    function GetBuffer(AChannel: Integer): Byte;
    procedure SetBuffer(AChannel: Integer; AValue: Byte);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Write(AHandle: Cardinal): Integer;
    procedure InitOpenDMX;
    property Buffer[AChannel: Integer]: Byte read GetBuffer write SetBuffer; default;
  end;

implementation

const
  FT_DLL_Name = 'FTD2XX.dll';

function FT_Open(Index: Integer; ftHandle: Pointer): FT_Result; stdcall; external FT_DLL_Name name 'FT_Open';
function FT_Close(ftHandle:Dword):FT_Result; stdcall; External FT_DLL_Name name 'FT_Close';
function FT_Read(ftHandle:Dword; FTInBuf:Pointer; BufferSize:LongInt; ResultPtr:Pointer):FT_Result; stdcall; external FT_DLL_Name name 'FT_Read';
function FT_Write(ftHandle:Dword; FTOutBuf:Pointer; BufferSize:LongInt; ResultPtr:Pointer):FT_Result; stdcall; external FT_DLL_Name name 'FT_Write';
function FT_SetDataCharacteristics(ftHandle:Dword; WordLength,StopBits,Parity:Byte):FT_Result; stdcall; external FT_DLL_Name name 'FT_SetDataCharacteristics';
function FT_SetFlowControl(ftHandle:Dword; FlowControl:Word; XonChar,XoffChar:Byte):FT_Result; stdcall; external FT_DLL_Name name 'FT_SetFlowControl';
function FT_GetModemStatus(ftHandle:Dword; ModemStatus:Pointer):FT_Result; stdcall; external FT_DLL_Name name 'FT_GetModemStatus';
function FT_Purge(ftHandle:Dword; Mask:Dword):FT_Result; stdcall; external FT_DLL_Name name 'FT_Purge';
function FT_ClrRts(ftHandle:Dword):FT_Result; stdcall; external FT_DLL_Name name 'FT_ClrRts';
function FT_SetBreakOn(ftHandle:Dword) : FT_Result; stdcall; external FT_DLL_Name name 'FT_SetBreakOn';
function FT_SetBreakOff(ftHandle:Dword) : FT_Result; stdcall; external FT_DLL_Name name 'FT_SetBreakOff';
function FT_GetStatus(ftHandle:DWord; RxBytes,TxBytes,EventStatus:Pointer):FT_Result; stdcall; external FT_DLL_Name name 'FT_GetStatus';
function FT_ResetDevice(ftHandle:Dword):FT_Result; stdcall; external FT_DLL_Name name 'FT_ResetDevice';
function FT_SetDivisor(ftHandle:Dword; Divisor:DWord):FT_Result; stdcall; external FT_DLL_Name name 'FT_SetDivisor';

const
  BITS_8      : Byte = 8;
  STOP_BITS_2 : Byte = 2;
  PARITY_NONE : Byte = 0;
  FLOW_NONE   : Word = 0;
  PURGE_RX    : Byte = 1;
  PURGE_TX    : Byte = 2;

constructor TOpenDMX.Create;
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  FBytesWritten:=0;
  FDMXHandle:=0;

  Buffer[0]:=0;  //Set DMX Start Code
  Start;
end;

destructor TOpenDMX.Destroy;
begin
  inherited Destroy;
end;

function TOpenDMX.GetBuffer(AChannel: Integer): Byte;
begin
  Result:=FBuffer[AChannel];
end;

procedure TOpenDMX.SetBuffer(AChannel: Integer; AValue: Byte);
begin
  FBuffer[AChannel]:=AValue;
end;

procedure TOpenDMX.Execute;
begin
  FStatus:=FT_Open(0, @FDMXHandle);
  while not Terminated do begin
    InitOpenDMX;
    FT_SetBreakOn(FDMXHandle);
    FT_SetBreakOff(FDMXHandle);
    FBytesWritten:=Write(FDMXHandle);
    Sleep(20);
  end;
  FT_Close(FDMXHandle);
end;

function TOpenDMX.Write(AHandle: Cardinal): Integer;
var
  ABytesWritten: Cardinal;
  APtr         : Pointer;
begin
  APtr:=GetMem(DMXBUFFERSIZE);
  Move(FBuffer, APtr^, DMXBUFFERSIZE);
  ABytesWritten:=0;
  FStatus:=FT_Write(AHandle, APtr{@FBuffer}, DMXBUFFERSIZE, @ABytesWritten);
  FreeMem(APtr, DMXBUFFERSIZE);
  Result:=ABytesWritten;
end;

procedure TOpenDMX.InitOpenDMX;
begin
  FT_ResetDevice(FDMXHandle);
  FT_SetDivisor(FDMXHandle, 12);  // set baud rate
  FT_SetDataCharacteristics(FDMXHandle, BITS_8, STOP_BITS_2, PARITY_NONE);
  FT_SetFlowControl(FDMXHandle, FLOW_NONE, 0, 0);
  FT_ClrRts(FDMXHandle);
  FT_Purge(FDMXHandle, PURGE_TX);
  FStatus:=FT_Purge(FDMXHandle, PURGE_RX);
end;

end.

