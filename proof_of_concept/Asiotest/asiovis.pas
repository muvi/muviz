unit AsioVis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Asio, AsioList, OpenAsio, LMessages, LCLType, LCLIntf,
  Windows, {ACS_Procs, ACS_Types,} WaveDest, {SpectrumData,} PluginType{, BufCon};

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
  TAsioManager = class (TFreqAnaInput)
  private
    FDrivers       : TAsioDriverList;
    FDriver        : IOpenAsio;
    FCallbacks     : TASIOCallbacks;
    FBufferInfo    : PAsioBufferInfo;
    FBufferTime    : TAsioTime;
    FSampleRate    : TAsioSampleRate;
    FABufSize      : Integer;
    FBuffersCreated: boolean;
    FIsStarted     : boolean;
    FDrvIndex      : Integer;
    FHandle        : HWND;
    FChannelInfos  : array[0..1] of TASIOChannelInfo;
    //FSpectrumData  : TSpectrumData4;
    (*FHandle           : HWND;
    Fdriverlist       : TAsioDriverList;
    FDriver           : IOpenAsio;
    FBuffersCreated   : boolean;
    FIsStarted        : boolean;
    Fcallbacks        : TASIOCallbacks;
    Fbufferinfo       : PAsioBufferInfo;
    FBufferTime       : TAsioTime;
    FChannelInfos     : array[0..1] of TASIOChannelInfo;
    FSampleRate       : TASIOSampleRate;
    FCurrentBufferSize: integer; *)
    FSampleCount     : Cardinal;
    procedure SetDriverIndex(const Value: MVInt); stdcall; override;
    function GetDriverIndex:  MVInt; stdcall; override;
    procedure CloseDriver;
    procedure BufferSwitch(index: integer);
    procedure BufferSwitchTimeInfo(index: integer; const params: TAsioTime);
  protected
    procedure AfterDestroy; override;
    procedure PMAsio(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure WndProc(var TheMessage: TLMessage);
    function GetFeatures: TfaiFeatures; stdcall; override;
  public
    constructor Create(AOwner: TComponent; AHandle: HWND); virtual; reintroduce;
    destructor Destroy; override;
    procedure Init(AWaveDest: IWaveDest); stdcall; override;
    procedure Done; stdcall; override;
    //procedure AssignSpectrumData(ASpectrumData: TSpectrumData4);
    procedure CreateBuffers;
    procedure DestroyBuffers;
    procedure Run; stdcall; override;
    procedure Stop; stdcall; override;
    procedure ShowControlPanel; stdcall; override;
    function GetDriverCount: MVInt; stdcall; override;
    function GetDriverNames(const Index: MVInt): ShortString; stdcall; override;
    (*class procedure AsioSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
    class function ChannelTypeToString(vType: TAsioSampleType): string;
    function AsioBufferSwitchTimeInfo(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
    procedure AsioBufferSwitch(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
    procedure PMAsio(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    function AsioMessage(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;

    procedure BufferSwitch(index: integer);
    procedure BufferSwitchTimeInfo(index: integer; const params: TAsioTime);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);*)
  published
    //property DriverIndex: Integer read FDrvIndex write SetDriverIndex;
  end;

implementation

var
  AAsioManager: TAsioManager;

procedure AsioBufferSwitch(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl; forward;
procedure AsioSampleRateDidChange(sRate: TASIOSampleRate); cdecl; forward;
function AsioMessage(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl; forward;
function AsioBufferSwitchTimeInfo(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl; forward;

{TAsioManager}

constructor TAsioManager.Create(AOwner: TComponent; AHandle: HWND);
begin
  inherited Create(AOwner);
  FHandle:=AHandle;
  OpenAsio.Init;
end;

destructor TAsioManager.Destroy;
begin
  inherited Destroy;
end;

procedure TAsioManager.AfterDestroy;
begin
  inherited AfterDestroy;
  OpenAsio.Done;
end;

function TAsioManager.GetFeatures: TfaiFeatures; stdcall;
begin
  Result:=[fifControlPanel,fifSetChannels,fifSetSampleRate];
end;

procedure TAsioManager.Init(AWaveDest: IWaveDest); stdcall;
begin
  inherited Init(AWaveDest);

  //FHandle:=AllocateHWnd(@WndProc); (***)


  FBufferInfo := nil;

  // init the driver list
  SetLength(FDrivers, 0);
  ListAsioDrivers(FDrivers);
  {for i := Low(FDrivers) to High(FDrivers) do
    DriverCombo.Items.Add(driverlist[i].name);}

  // set the callbacks record fields
  FCallbacks.bufferSwitch:=@AsioBufferSwitch;
  FCallbacks.sampleRateDidChange:=@AsioSampleRateDidChange;
  FCallbacks.asioMessage:=@AsioMessage;
  FCallbacks.bufferSwitchTimeInfo:=@AsioBufferSwitchTimeInfo;

  // set the driver itself to nil for now
  FDriver:=nil;
  FBuffersCreated:=false;
  FIsStarted:=false;
  FDrvIndex:=-1;

  AAsioManager:=Self;

  //CreateBuffers;
end;

procedure TAsioManager.Done; stdcall;
begin

  CloseDriver;

  AAsioManager:=nil;
  //DestroyBuffers;

  SetLength(FDrivers,0);
  //DeallocateHWnd(FHandle); (***)

  inherited Done;
end;

{procedure TAsioManager.AssignSpectrumData(ASpectrumData: TSpectrumData4);
begin
  FSpectrumData:=ASpectrumData;
end;}

procedure TAsioManager.CreateBuffers;
var
   min,max,pref,gran: Integer;
   currentbuffer    : PAsioBufferInfo;
   I                : Integer;
begin
  if FDriver=nil then exit;

  if FBuffersCreated then DestroyBuffers;

  FDriver.GetBufferSize(min, max, pref, gran);

  // two output channels
  GetMem(FBufferInfo, SizeOf(TAsioBufferInfo)*2);
  currentbuffer:=FBufferInfo;
  for I:=0 to 1 do begin
    currentbuffer^.isInput := ASIOTrue{False};  // create an output buffer
    currentbuffer^.channelNum := i;
    currentbuffer^.buffers[0] := nil;
    currentbuffer^.buffers[1] := nil;
    inc(currentbuffer);
  end;

  // actually create the buffers
  FBuffersCreated:= (FDriver.CreateBuffers(FBufferInfo, 2, pref, FCallbacks) = ASE_OK);
  if FBuffersCreated then
    FABufSize:=pref
  else
    FABufSize:=0;

  for i:=0 to 1 do begin
    FChannelInfos[I].channel:=I;
    FChannelInfos[I].isInput:=ASIOTrue{False};   //  output
    FDriver.GetChannelInfo(FChannelInfos[I]);
    {if i = 0 then
      lblLeftChannelType.Caption := 'left type : ' + ChannelTypeToString(ChannelInfos[i].vType)
    else
      lblRightChannelType.Caption := 'right type : ' + ChannelTypeToString(ChannelInfos[i].vType);}
  end;
end;

procedure TAsioManager.DestroyBuffers;
begin
  if (FDriver=nil) or not FBuffersCreated then exit;
  if FIsStarted then Stop;

  FDriver.DisposeBuffers;
  FreeMem(FBufferInfo);
  FBufferInfo:=nil;
  FBuffersCreated:=false;
  FABufSize:=0;
end;

procedure TAsioManager.Run; stdcall;
var
  TempSampleRate: TASIOSampleRate;
  minsize,maxsize,preferredsize,granularity: longint;
begin
  //if FDriver=nil then exit;
  inherited Run;
  {FDriver.GetSampleRate(TempSampleRate);
  //FSpectrumData.SampleRate:=Trunc(TempSampleRate);
  FDriver.GetBufferSize(minsize,maxsize,preferredsize,granularity);
  //FSpectrumData.InputBufferSampleCount:=preferredsize;
  FSampleCount:=preferredsize;
  WaveDest.Init(Trunc(TempSampleRate),1,FSampleCount);
  FIsStarted:=(FDriver.Start = ASE_OK);}
end;

procedure TAsioManager.Stop; stdcall;
begin
  inherited Stop;
  {if FDriver=nil then exit;
  if FIsStarted then begin
    FDriver.Stop;
    FIsStarted:=false;
  end;}
end;

procedure TAsioManager.ShowControlPanel; stdcall;
begin
  if FDriver<>nil then FDriver.ControlPanel;
end;

procedure TAsioManager.SetDriverIndex(const Value: MVInt); stdcall;
var
  AIsStarted: Boolean;
begin
  AIsStarted:=FIsStarted;
  if FDriver <> nil then CloseDriver;
  if Value{DriverCombo.ItemIndex} >= 0 then begin
    if OpenAsioCreate(FDrivers[Value{DriverCombo.ItemIndex}].id,FDriver) then begin
      if (FDriver <> nil) then if FDriver.Init(FHandle)<>ASIOTrue then begin
        FDriver:=nil;  // RELEASE
        FDrvIndex:=-1;
        exit;
      end;
    end else begin
      FDriver:=nil;  // RELEASE
      FDrvIndex:=-1;
      exit;
    end;
  end;

  FDriver:=nil;
  FDrvIndex:=-1;
  //FDrvIndex:=Value;
  //CreateBuffers;
  //if AIsStarted then Run;
end;

function TAsioManager.GetDriverIndex:  MVInt; stdcall;
begin
  Result:=FDrvIndex;
end;

procedure TAsioManager.CloseDriver;
begin
  if FDriver<>nil then
  begin
    DestroyBuffers;
    {if FIsStarted then begin
      FDriver.Stop;
      FIsStarted:=false;
    end;
    if FBuffersCreated then begin
      FreeMem(FBufferInfo);
      FBufferInfo:=nil;
      FDriver.DisposeBuffers;
      FBuffersCreated:=false;
      FABufSize:=0;
    end; }
    FDriver:=nil;  // RELEASE;
    FDrvIndex:=-1;
  end;
end;

procedure TAsioManager.BufferSwitch(index: integer);
begin
  FillChar(FBufferTime,SizeOf(TAsioTime),0);

  // get the time stamp of the buffer, not necessary if no
  // synchronization to other media is required
  if FDriver.GetSamplePosition(FBufferTime.timeInfo.samplePosition,FBufferTime.timeInfo.systemTime) = ASE_OK
    then FBufferTime.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;

  BufferSwitchTimeInfo(index,FBufferTime);
end;

procedure TAsioManager.BufferSwitchTimeInfo(index: integer; const params: TAsioTime);
var
   i, ndx        : integer;
   info          : PAsioBufferInfo;
   outputInt16   : PSmallint;
   outputInt32   : PInteger;
   outputFloat32 : PSingle;
begin
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
          outputInt16 := info^.buffers[index];
          for ndx:=0 to FABufSize-1 do
          begin
            outputInt16^ := Round(sin(ndx)*32767);   // here we actually fill the output buffer (with zeroes)
            inc(outputInt16);
          end;
        end;
      ASIOSTInt24LSB   :  begin end;
      ASIOSTInt32LSB   :
        begin
          // example:
          {outputInt32 := info^.buffers[index];
          for ndx := 0 to FABufSize-1 do
          begin
            outputInt32^ := Round(sin(ndx)*$FFFFFFF{2147483648});   // here we actually fill the output buffer (with zeroes)
            inc(outputInt32);
          end; }
          //Int32ToSingle(PBuffer32(info^.buffers[index]), {OutBuf}{FSpectrumData.FWData}WaveDest.GetBuffer(I), FSampleCount{WaveDest.InputBufferSampleCount} {FSpectrumData.WaveDataCount});
          //Int32ToSingle(info^.buffers[index]^, {OutBuf}{FSpectrumData.FWData}WaveDest.GetBuffer(I)^, FSampleCount{WaveDest.InputBufferSampleCount} {FSpectrumData.WaveDataCount});
          //WaveDest.Analyse;
          //FSpectrumData.Analyse;
        end;
      ASIOSTFloat32LSB :
        begin
          // example:
          outputFloat32 := info^.buffers[index];
          for ndx := 0 to FABufSize-1 do
          begin
            outputFloat32^ := sin(ndx);   // here we actually fill the output buffer (with zeroes)
            inc(outputFloat32);
          end;
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

function TAsioManager.GetDriverCount: MVInt; stdcall;
begin
  Result:=Length(FDrivers);
end;

function TAsioManager.GetDriverNames(const Index: MVInt): ShortString; stdcall;
var
  AResult: string;
begin
  AResult:=FDrivers[Index].name;
  Result:=AResult;
end;

{TAsioManager - Messages}

procedure TAsioManager.WndProc(var TheMessage: TLMessage);
begin
  case TheMessage.msg of
    PM_ASIO           : PMAsio(TheMessage);
    PM_UpdateSamplePos: PMUpdateSamplePos(TheMessage);
  end;
end;

procedure TAsioManager.PMAsio(var Message: TMessage);
//var
   //inp, outp: integer;
begin
  case Message.WParam of
    AM_ResetRequest         :  SetDriverIndex(FDrvIndex){DriverComboChange(DriverCombo)};                    // restart the driver
    AM_BufferSwitch         :  BufferSwitch(Message.LParam);                      // process a buffer
    AM_BufferSwitchTimeInfo :  BufferSwitchTimeInfo(Message.LParam, FBufferTime);  // process a buffer with time
    AM_LatencyChanged       :
(*      if (FDriver<>nil) then
      begin
        //FDriver.GetLatencies(inp,outp);
        {lblInputLatency.Caption := 'input : ' + IntToStr(inp);
        lblOutputLatency.Caption := 'output : ' + IntToStr(outp);}      end;*)
  end;
end;

procedure TAsioManager.PMUpdateSamplePos(var Message: TMessage);
var
   Samples     : TAsioSamples;
   SampleCount : Int64;
   //seconds     : Int64;
   //minutes     : Int64;
   //hours       : Int64;
begin
  Samples.hi := Message.wParam;
  Samples.lo := Message.lParam;
  SampleCount := ASIOSamplesToInt64(Samples);
  //lblSamplePos.Caption := Format('sample pos : %d (hi:%d) (lo:%d)', [SampleCount, Samples.hi, Samples.lo]);

  {seconds := SampleCount div 44100;
  hours := seconds div 3600;
  minutes := (seconds mod 3600) div 60;
  seconds := seconds mod 60;}
  //lblTime.Caption := Format('time : %d:%.2d:%.2d', [hours, minutes, seconds]);
end;

{asio callbacks}

{var
  AAsioManager: TAsioManager;}


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

end.

