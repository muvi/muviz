unit AsioVis2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Asio, AsioList, Dialogs,
  LMessages, LCLType, LCLIntf, Windows, WaveDest, SpectrumData, PluginType,
  BufCon, AdvFunc;

const
  // private message
  PM_ASIO                 = WM_User + 1652;   // unique we hope

  // asio message(s), as wParam for PM_ASIO
  AM_ResetRequest         = 0;
  AM_BufferSwitch         = 1;     // new buffer index in lParam
  AM_BufferSwitchTimeInfo = 2;     // new buffer index in lParam
                                   // time passed in MainForm.BufferTime
  AM_LatencyChanged       = 3;

  PM_UpdateSamplePos      = PM_ASIO + 1;  // sample pos in wParam (hi) and lParam (lo)

type
  TAsioInput = class (TFreqAnaInput2)
  private
    FDrivers       : TAsioDriverList;
    FDriver        : TOpenASIO;
    FDriverIndex   : Integer;
    FCallbacks     : TASIOCallbacks;
    FBufferInfo    : PAsioBufferInfo;
    FBufferTime    : TAsioTime;
    FBufSize       : Cardinal;
    FHandle        : HWND;
    FSampleRate    : Cardinal;
    FBufferSize    : Cardinal;
    FChannels      : Integer;
    FSampleRates   : array of Cardinal;
    FPrefSampleRate: Cardinal;
    FPrefBufSize   : LongInt;
    FBufferSizes   : array of LongInt;
    FChannelInfos  : array[0..1] of TASIOChannelInfo;
    function SetDriver(ADriver: MVInt): Boolean; stdcall; override;
    procedure BufferSwitch(index: integer);
    procedure BufferSwitchTimeInfo(index: integer; const params: TAsioTime);

    function GetMaxChannels: MVInt; stdcall; override;
    function GetSupportedBufferSizeCount: MVInt; stdcall; override;
    function GetSupportedBufferSize(Index: MVInt): MVInt; stdcall; override;
    function GetPreferedBufferSize: MVInt; stdcall; override;
    function GetSupportedSampleRateCount: MVInt; stdcall; override;
    function GetSupportedSampleRate(Index: MVInt): MVInt; stdcall; override;
    function GetPreferedSampleRate: MVInt; stdcall; override;

    function SetBufferSize(ABufferSize: MVInt): Boolean; stdcall; override;
    function SetChannels(AChannels: MVInt): Boolean; stdcall; override;
    function SetSampleRate(ASampleRate: MVInt): Boolean; stdcall; override;
  protected
    procedure PMAsio(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure WndProc(var TheMessage: TLMessage);
    function GetFeatures: TfaiFeatures; stdcall; override;
    function GetDriverCount: MVInt; stdcall; override;
    function GetDriver(Index: MVInt): TMDriverData; stdcall; override;
  public
    constructor Create(AWaveDest: IWaveDest); override;
    destructor Destroy; override;
    function Run: Boolean; stdcall; override;
    procedure Stop; stdcall; override;
    procedure CloseDriver; stdcall; override;
    function ShowControlPanel: Boolean; stdcall; override;
  end;

function NewAsioInput(AWaveDest: IWaveDest): IFreqAnaInput2; stdcall;

const
  AID_ASIO: TGUID = '{E016C843-7250-40E3-AC0B-14953C8503AF}';

implementation

var
  AAsioManager: TAsioInput;

procedure AsioBufferSwitch(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl; forward;
procedure AsioSampleRateDidChange(sRate: TASIOSampleRate); cdecl; forward;
function AsioMessage(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl; forward;
function AsioBufferSwitchTimeInfo(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl; forward;

{TAsioInput}

constructor TAsioInput.Create(AWaveDest: IWaveDest);
begin
  inherited Create(AWaveDest);
  FHandle:=AllocateHWnd(@WndProc);
  FBufferInfo := nil;
  // init the driver list
  SetLength(FDrivers, 0);
  ListAsioDrivers(FDrivers);
  // set the callbacks record fields
  FCallbacks.bufferSwitch:=@AsioBufferSwitch;
  FCallbacks.sampleRateDidChange:=@AsioSampleRateDidChange;
  FCallbacks.asioMessage:=@AsioMessage;
  FCallbacks.bufferSwitchTimeInfo:=@AsioBufferSwitchTimeInfo;
  // set the driver itself to nil for now
  FDriver:=nil;
  AAsioManager:=Self;
end;

destructor TAsioInput.Destroy;
begin
  AAsioManager:=nil;
  SetLength(FDrivers,0);
  DeallocateHWnd(FHandle);
  inherited Destroy;
end;

function TAsioInput.GetFeatures: TfaiFeatures; stdcall;
begin
  Result:=[fifControlPanel];
end;

function TAsioInput.Run: Boolean; stdcall;
var
  currentbuffer    : PAsioBufferInfo;
  I                : Integer;
begin
  // two input channels
  GetMem(FBufferInfo, SizeOf(TAsioBufferInfo)*2);
  currentbuffer:=FBufferInfo;
  for I:=0 to 1 do begin
    currentbuffer^.isInput := ASIOTrue;  // create an input buffer
    currentbuffer^.channelNum := i;
    currentbuffer^.buffers[0] := nil;
    currentbuffer^.buffers[1] := nil;
    inc(currentbuffer);
  end;

  // actually create the buffers
  Result:=(FDriver.CreateBuffers(FBufferInfo, 2, FBufSize, FCallbacks) = ASE_OK);
  if not Result then exit;

  for i:=0 to 1 do begin
    FChannelInfos[I].channel:=I;
    FChannelInfos[I].isInput:=ASIOTrue;
    FDriver.GetChannelInfo(FChannelInfos[I]);
  end;

  Result:=(FDriver.Start = ASE_OK);
end;

procedure TAsioInput.Stop; stdcall;
begin
  FDriver.Stop;
  FDriver.DisposeBuffers;
  FreeMem(FBufferInfo);
  FBufferInfo:=nil;
end;

function TAsioInput.ShowControlPanel: Boolean; stdcall;
begin
  Result:=true;
  FDriver.ControlPanel;
end;

function TAsioInput.SetDriver(ADriver: MVInt): Boolean; stdcall;
var
  ASampleRate    : Double;
  I,L            : Integer;
  AMin,AMax,AGran: LongInt;
begin
  FDriverIndex:=ADriver;
  FDriver:=TOpenAsio.Create(FDrivers[ADriver].id);
  Result:=(FDriver.Init(FHandle)=ASIOTrue);
  //Buffer Sizes
  FDriver.GetBufferSize(AMin,AMax,FPrefBufSize,AGran);
  FBufSize:=FPrefBufSize;
  if AGran<0 then begin
    L:=Trunc(ld(AMax))-Trunc(ld(AMin))+1;
    SetLength(FBufferSizes,L);
    FBufferSizes[0]:=AMin;
    for I:=1 to L-1 do FBufferSizes[I]:=FBufferSizes[I-1]*2;
  end else if AGran>0 then begin
    L:=((AMax-AMin) div AGran)+1;
    SetLength(FBufferSizes,L);
    FBufferSizes[0]:=AMin;
    for I:=1 to L-1 do FBufferSizes[I]:=FBufferSizes[I-1]+AGran;
  end else begin
    SetLength(FBufferSizes,1);
    FBufferSizes[0]:=AMin;
  end;
  //SampleRate
  FDriver.GetSampleRate(ASampleRate);
  FPrefSampleRate:=Trunc(ASampleRate);
  SetLength(FSampleRates,1);   (***) //ggf per CanSampleRate die unterschiedlichen standards abfragen wie z.B. 44.100 und 48.000
  FSampleRates[0]:=FPrefSampleRate;
  FSampleRate:=FPrefSampleRate;
  //Channels
  FChannels:=2;
end;

procedure TAsioInput.CloseDriver; stdcall;
begin
  SetLength(FSampleRates,0);
  SetLength(FBufferSizes,0);
  FDriver.Destroy;
end;

procedure TAsioInput.BufferSwitch(index: integer);
begin
  FillChar(FBufferTime,SizeOf(TAsioTime),0);
  // get the time stamp of the buffer, not necessary if no
  // synchronization to other media is required
  if FDriver.GetSamplePosition(FBufferTime.timeInfo.samplePosition,FBufferTime.timeInfo.systemTime) = ASE_OK
    then FBufferTime.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;

  BufferSwitchTimeInfo(index,FBufferTime);
end;

procedure TAsioInput.BufferSwitchTimeInfo(index: integer; const params: TAsioTime);
var
   i, ndx        : integer;
   info          : PAsioBufferInfo;
   outputInt16   : PSmallint;
   outputInt32   : PInteger;
   outputFloat32 : PSingle;
begin
  //Wave-Puffer darf nicht gefüllt werden, wenn die Größen verändert werden
  if WaveDest.Locked then exit;
  // this is where processing occurs, with the buffers provided by Driver.CreateBuffers
  // beware of the buffer output format, of course
  info:=FBufferInfo;

  for i:=0 to 0{1} do
  begin
    case FChannelInfos[i].vType of
      ASIOSTInt16MSB   :  begin end;
      ASIOSTInt24MSB   :  begin end;
      ASIOSTInt32MSB   :  begin end;
      ASIOSTFloat32MSB :  begin end;
      ASIOSTFloat64MSB :  begin end;

      ASIOSTInt32MSB16 :  begin end;
      ASIOSTInt32MSB18 :  begin end;
      ASIOSTInt32MSB20 :  begin end;
      ASIOSTInt32MSB24 :  begin end;

      ASIOSTInt16LSB   :
        begin
          // example:

          //Move(info^.buffers[index],FSpectrumData.FWData,FSpectrumData.
          {outputInt16 := info^.buffers[index];
          for ndx:=0 to FBufSize-1 do
          begin
            outputInt16^ := Round(sin(ndx)*32767);   // here we actually fill the output buffer (with zeroes)
            inc(outputInt16);
          end;}
        end;
      ASIOSTInt24LSB   :  begin end;
      ASIOSTInt32LSB   :
        begin
          Int32ToSingle(info^.buffers[index]^,WaveDest.GetBuffer(I)^, FBufSize);
          WaveDest.Analyse;
        end;
      ASIOSTFloat32LSB :
        begin
          {// example:
          outputFloat32 := info^.buffers[index];
          for ndx := 0 to FBufSize-1 do
          begin
            outputFloat32^ := sin(ndx);   // here we actually fill the output buffer (with zeroes)
            inc(outputFloat32);
          end;}
        end;
      ASIOSTFloat64LSB :  begin end;
      ASIOSTInt32LSB16 :  begin end;
      ASIOSTInt32LSB18 :  begin end;
      ASIOSTInt32LSB20 :  begin end;
      ASIOSTInt32LSB24 :  begin end;
    end;

    inc(info);  // don't forget to go to the next buffer in this loop
  end;


  // tell the interface that the sample position has changed
  PostMessage(FHandle, PM_UpdateSamplePos, params.timeInfo.samplePosition.hi, params.timeInfo.samplePosition.lo);

  //FDriver.OutputReady;    // some asio drivers require this
end;

function TAsioInput.GetDriverCount: MVInt; stdcall;
begin
  Result:=Length(FDrivers);
end;

function TAsioInput.GetDriver(Index: MVInt): TMDriverData; stdcall;
var
   AStr: string;
begin
  with Result do begin
    AStr:=FDrivers[Index].name;
    Name:=AStr;
    ID:=FDrivers[Index].ID;
  end;
end;

function TAsioInput.GetMaxChannels: MVInt; stdcall;
begin
  Result:=2; (***)
end;

function TAsioInput.GetSupportedBufferSizeCount: MVInt; stdcall;
begin
  Result:=Length(FBufferSizes);
end;

function TAsioInput.GetSupportedBufferSize(Index: MVInt): MVInt; stdcall;
begin
  Result:=FBufferSizes[Index];
end;

function TAsioInput.GetPreferedBufferSize: MVInt; stdcall;
begin
  Result:=FPrefBufSize;
end;

function TAsioInput.GetSupportedSampleRateCount: MVInt; stdcall;
begin
  Result:=Length(FSampleRates);
end;

function TAsioInput.GetSupportedSampleRate(Index: MVInt): MVInt; stdcall;
begin
  Result:=FSampleRates[Index];
end;

function TAsioInput.GetPreferedSampleRate: MVInt; stdcall;
begin
  Result:=FPrefSampleRate;
end;

function TAsioInput.SetBufferSize(ABufferSize: MVInt): Boolean; stdcall;
begin
  FBufSize:=ABufferSize;
end;

function TAsioInput.SetChannels(AChannels: MVInt): Boolean; stdcall;
begin
  FChannels:=AChannels;
end;

function TAsioInput.SetSampleRate(ASampleRate: MVInt): Boolean; stdcall;
begin
  FSampleRate:=ASampleRate;
end;

{TAsioInput - Messages}

procedure TAsioInput.WndProc(var TheMessage: TLMessage);
begin
  case TheMessage.msg of
    PM_ASIO           : PMAsio(TheMessage);
    PM_UpdateSamplePos: PMUpdateSamplePos(TheMessage);
  end;
end;

procedure TAsioInput.PMAsio(var Message: TMessage);
begin
  case Message.WParam of
    AM_ResetRequest         :  begin // restart the driver
        Stop;
        CloseDriver;
        SetDriver(FDriverIndex);
        Run;
      end;
    AM_BufferSwitch         :  BufferSwitch(Message.LParam);                      // process a buffer
    AM_BufferSwitchTimeInfo :  BufferSwitchTimeInfo(Message.LParam, FBufferTime);  // process a buffer with time
    //AM_LatencyChanged       :  ;
  end;
end;

procedure TAsioInput.PMUpdateSamplePos(var Message: TMessage);
var
   Samples     : TAsioSamples;
   SampleCount : Int64;
begin
  Samples.hi := Message.wParam;
  Samples.lo := Message.lParam;
  SampleCount := ASIOSamplesToInt64(Samples);
end;

{asio callbacks}

procedure AsioBufferSwitch(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
begin
  case directProcess of
    ASIOFalse :  PostMessage(AAsioManager.FHandle, PM_ASIO, AM_BufferSwitch, doubleBufferIndex);
    ASIOTrue  :  AAsioManager.BufferSwitch(doubleBufferIndex);
  end;
end;

procedure AsioSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
begin
  //MessageDlg('The sample rate has been changed to ' + FloatToStr(sRate), mtInformation, [mbOK], 0);
end;

function AsioMessage(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;

  case selector of
    kAsioSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kAsioEngineVersion        :  Result := 1;
          kAsioResetRequest         :  Result := 1;
          kAsioBufferSizeChange     :  Result := 0;
          kAsioResyncRequest        :  Result := 1;
          kAsioLatenciesChanged     :  Result := 1;
          kAsioSupportsTimeInfo     :  Result := 1;
          kAsioSupportsTimeCode     :  Result := 1;
          kAsioSupportsInputMonitor :  Result := 0;
        end;
      end;
    kAsioEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kAsioResetRequest         :
      begin
        PostMessage(AAsioManager.FHandle, PM_Asio, AM_ResetRequest, 0);
        Result := 1;
      end;
    kAsioBufferSizeChange     :
      begin
        PostMessage(AAsioManager.FHandle, PM_Asio, AM_ResetRequest, 0);
        Result := 1;
      end;
    kAsioResyncRequest        :  ;
    kAsioLatenciesChanged     :
      begin
        PostMessage(AAsioManager.FHandle, PM_Asio, AM_LatencyChanged, 0);
        Result := 1;
      end;
    kAsioSupportsTimeInfo     :  Result := 1;
    kAsioSupportsTimeCode     :  Result := 0;
    kAsioSupportsInputMonitor :  ;
  end;
end;

function AsioBufferSwitchTimeInfo(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  case directProcess of
    ASIOFalse :
      begin
        AAsioManager.FBufferTime := params;
        PostMessage(AAsioManager.FHandle, PM_ASIO, AM_BufferSwitchTimeInfo, doubleBufferIndex);
      end;
    ASIOTrue  :  AAsioManager.BufferSwitchTimeInfo(doubleBufferIndex, params);
  end;

  Result := nil;
end;

function NewAsioInput(AWaveDest: IWaveDest): IFreqAnaInput2; stdcall;
begin
  Result:=TAsioInput.Create(AWaveDest);
end;

end.

