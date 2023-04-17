unit PseudoSpectrumData; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpectrumData, PluginType, ExtPtrType, MInterfacedObject,
  VPBuffers;

type
  TPseudoSpectrumData = class (TMInterfacedObject,IBasicSpectrumData,ISpectrumData,ISpectrumData2)
  private
    FWaveBuffer   : Pointer;
    FLevelBuffer  : Pointer;
    FBufferManager: IVPBufferManager;
  protected
    {function QueryInterface(const iid : tguid; out obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;}

    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    function GetChannels: MVInt; stdcall;
    function GetSampleRate: MVInt; stdcall;
    function GetLocked: Boolean; stdcall;

    function GetBuffer(const AChannel: MVInt): vpRealBuffer; stdcall; overload;
    function GetBuffer(const ABufferIndex,AChannel: MVInt): vpRealBuffer; stdcall; overload;

    function GetWaveData(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetWaveDataLong(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetLevels(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetPeak(const AChannel: MVInt): MVFloat; stdcall;
    function GetFreqCount: MVInt; stdcall;
    function GetWaveDataCount: MVInt; stdcall;
    function GetLongWaveDataCount: MVInt; stdcall;
    function GetLevelBuffer(const AChannel: MVInt): vpRealBuffer; stdcall;

    function GetBeatBufCount: MVInt; stdcall;
    function GetBPMAnalysisTime: MVInt; stdcall;
    function GetSyncBufferCount: MVInt; stdcall;

    function GetBeatAt(const Subband: MVInt): Boolean; stdcall;
    function GetBeatAtH(const Subband,History: MVInt): Boolean; stdcall;
    function GetBeatState: MVBeatState; stdcall;
    function GetBeatPos: MVFloat; stdcall;
    function GetBPM: MVFloat; stdcall;
    function GetBPMFound: TBPMFoundMode; stdcall;

    function GetBufferCount: MVInt; stdcall;
    function GetFFTMode: TFFTMode; stdcall;
    function GetAnalyseMode: TSDAnalyseMode; stdcall;
    function GetLevelsC(const Index: MVInt): MVFloat; stdcall;
    function GetWaveDataC(const Index: MVInt): MVFloat; stdcall;
    function GetWaveDataLongB(const Index,Channel,Buffer: MVInt): MVFloat; stdcall;
    function GetWaveDataLongC(const Index: MVInt): MVFloat; stdcall;
    function GetWaveDataLongBC(const Index,Buffer: MVInt): MVFloat; stdcall;

    property BufferManager: IVPBufferManager read FBufferManager;
  public
    constructor Create(ABufferManager: IVPBufferManager);
    destructor Destroy; override;
  end;

implementation

{TPseudoSpectrumData}

const
  _HighestBufSize    = 2048;
  _WaveDataCount     = 2048;
  _FreqCount         = 512;
  _ShortWaveDataCount= 512;
  _BeatBufCount      = 600;
  _SyncBufferCount   = 2048;
  _SampleRate        = 44100;
  _WaveBufferSize    = _WaveDataCount*SD2BufElemSize;
  _FFTBufferSize     = _FreqCount*SD2BufElemSize;
  _UsedMainFreq      = _SampleRate/_FreqCount;
  _SinFac            = 2*pi*_UsedMainFreq;

constructor TPseudoSpectrumData.Create(ABufferManager: IVPBufferManager);
var
  I,J      : Integer;
  AWBuffer2: PSD2BufferFormat;// absolute FWaveBuffer;
  ALBuffer2: PSD2BufferFormat;// absolute FLevelBuffer;
  AVal     : Real;
begin
  inherited Create;
  FBufferManager:=ABufferManager;
  GetMem(FWaveBuffer,_WaveBufferSize);
  AWBuffer2:=FWaveBuffer;
  GetMem(FLevelBuffer,_FFTBufferSize);
  ALBuffer2:=FLevelBuffer;
  AVal:=0.05;
  for I:=0 to _FreqCount-1 do begin
    ALBuffer2^[I]:=AVal;
    AVal/=2.0;
  end;
  for I:=0 to _HighestBufSize-1 do begin
    AWBuffer2^[I]:=0.0;
    for J:=0 to _FreqCount-1
      do AWBuffer2^[I]+=ALBuffer2^[J]*sin(I*J*_SinFac);
  end;
end;

destructor TPseudoSpectrumData.Destroy;
begin
  FreeMem(FLevelBuffer,_FFTBufferSize);
  FreeMem(FWaveBuffer,_WaveBufferSize);
  inherited Destroy;
end;

{function TPseudoSpectrumData.QueryInterface(const iid : tguid; out obj) : longint; stdcall;
begin
  Result:=0;
end;

function TPseudoSpectrumData._AddRef : longint;stdcall;
begin
  Result:=1;
end;

function TPseudoSpectrumData._Release : longint;stdcall;
begin
  Result:=1;
end;}

const
  Local_Version : MVVersion = (Version:0;MainVersion:0;SubVersion:65535);
  Local_Version4: MVVersion = (Version:0;MainVersion:4;SubVersion:0);

function TPseudoSpectrumData.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TPseudoSpectrumData.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version4
    then Result:=IMInterface(ISpectrumData2(Self))
    else if Version=Local_Version
      then Result:=IMInterface(ISpectrumData(Self))
      else Result:=inherited Future(Version);
end;

function TPseudoSpectrumData.GetChannels: MVInt; stdcall;
begin
  Result:=1;
end;

function TPseudoSpectrumData.GetSampleRate: MVInt; stdcall;
begin
  Result:=_SampleRate;
end;

function TPseudoSpectrumData.GetLocked: Boolean; stdcall;
begin
  Result:=false;
end;

function TPseudoSpectrumData.GetBuffer(const AChannel: MVInt): vpRealBuffer; stdcall; overload;
begin
  Result:=BufferManager.ToVPRealBuffer(_WaveDataCount,FWaveBuffer);
end;

function TPseudoSpectrumData.GetBuffer(const ABufferIndex,AChannel: MVInt): vpRealBuffer; stdcall; overload;
begin
  case ABufferIndex of
    WaveDataBI    : Result:=BufferManager.ToVPRealBuffer(_ShortWaveDataCount,FWaveBuffer+((_HighestBufSize-_ShortWaveDataCount)*SD2BufElemSize));
    WaveDataLongBI: Result:=BufferManager.ToVPRealBuffer(_WaveDataCount,FWaveBuffer+((_HighestBufSize-_WaveDataCount)*SD2BufElemSize));
    FreqDataBI    : Result:=BufferManager.ToVPRealBuffer(_FreqCount,FWaveBuffer+((_HighestBufSize-_FreqCount)*SD2BufElemSize));
    SyncBufferBI  : Result:=BufferManager.ToVPRealBuffer(_SyncBufferCount,FWaveBuffer+((_HighestBufSize-_SyncBufferCount)*SD2BufElemSize));
    else Result:=BufferManager.ToVPRealBuffer(_WaveDataCount,FWaveBuffer);
  end;
end;

function TPseudoSpectrumData.GetWaveData(const AIndex,AChannel: MVInt): MVFloat; stdcall;
//var
  //AWData2: PSD2BufferFormat absolute FWaveBuffer;
begin
  Result:=PSD2BufferFormat(FWaveBuffer)^[AIndex];
end;

function TPseudoSpectrumData.GetWaveDataLong(const AIndex,AChannel: MVInt): MVFloat; stdcall;
//var
  //AWData2: PSD2BufferFormat absolute FWaveBuffer;
begin
  Result:=PSD2BufferFormat(FWaveBuffer)^[AIndex];
end;

function TPseudoSpectrumData.GetLevels(const AIndex,AChannel: MVInt): MVFloat; stdcall;
//var
//  AFFTData2: PSD2BufferFormat absolute FLevelBuffer;
begin
  Result:=PSD2BufferFormat(FLevelBuffer)^[AIndex];
end;

function TPseudoSpectrumData.GetPeak(const AChannel: MVInt): MVFloat; stdcall;
begin
  Result:=0.3;
end;

function TPseudoSpectrumData.GetFreqCount: MVInt; stdcall;
begin
  Result:=_FreqCount;
end;

function TPseudoSpectrumData.GetWaveDataCount: MVInt; stdcall;
begin
  Result:=_ShortWaveDataCount;
end;

function TPseudoSpectrumData.GetLongWaveDataCount: MVInt; stdcall;
begin
  Result:=_WaveDataCount
end;

function TPseudoSpectrumData.GetLevelBuffer(const AChannel: MVInt): vpRealBuffer; stdcall;
begin
  Result:=BufferManager.ToVPRealBuffer(_FreqCount,FLevelBuffer);
end;

function TPseudoSpectrumData.GetBeatBufCount: MVInt; stdcall;
begin
  Result:=_BeatBufCount;
end;

function TPseudoSpectrumData.GetBPMAnalysisTime: MVInt; stdcall;
begin
  Result:=1;
end;

{function TPseudoSpectrumData.GetSoundHistory(const Index: MVInt): MVFloat; stdcall;
begin

end; }

function TPseudoSpectrumData.GetSyncBufferCount: MVInt; stdcall;
begin
  Result:=_SyncBufferCount;
end;

function TPseudoSpectrumData.GetBeatAt(const Subband: MVInt): Boolean; stdcall;
begin
  Result:=(Subband mod 3=0);
end;

function TPseudoSpectrumData.GetBeatAtH(const Subband,History: MVInt): Boolean; stdcall;
begin
  Result:=(Subband mod 3=0) and (History mod 43<5);
end;

function TPseudoSpectrumData.GetBeatState: MVBeatState; stdcall;
begin
  with Result do begin
    Togg:=true;
    Free:=false;
    Max:=3;
    Pos:=1;
    BeatLength:=0.5;
    Reserved:=0.0;
    BeatStart:=0;
    SyncTogg:=false;
  end;
end;

function TPseudoSpectrumData.GetBeatPos: MVFloat; stdcall;
begin
  Result:=1.1;
end;

function TPseudoSpectrumData.GetBPM: MVFloat; stdcall;
begin
  Result:=120.0
end;

function TPseudoSpectrumData.GetBPMFound: TBPMFoundMode; stdcall;
begin
  Result:=bpmTrue;
end;

function TPseudoSpectrumData.GetBufferCount: MVInt; stdcall;
begin
  Result:=4;
end;

function TPseudoSpectrumData.GetFFTMode: TFFTMode; stdcall;
begin
  Result:=fftAllChannels;
end;

function TPseudoSpectrumData.GetAnalyseMode: TSDAnalyseMode; stdcall;
begin
  Result:=[afFFT,afPeak];
end;

function TPseudoSpectrumData.GetLevelsC(const Index: MVInt): MVFloat; stdcall;
//var
//  AFFTData2: PSD2BufferFormat absolute FLevelBuffer;
begin
  Result:=PSD2BufferFormat(FLevelBuffer)^[Index];
end;

function TPseudoSpectrumData.GetWaveDataC(const Index: MVInt): MVFloat; stdcall;
//var
//  AWData2: PSD2BufferFormat absolute FWaveBuffer;
begin
  Result:=PSD2BufferFormat(FWaveBuffer)^[Index];
end;

function TPseudoSpectrumData.GetWaveDataLongB(const Index,Channel,Buffer: MVInt): MVFloat; stdcall;
//var
  //AWData2: PSD2BufferFormat absolute FWaveBuffer;
begin
  Result:=PSD2BufferFormat(FWaveBuffer)^[Index+(Buffer*_ShortWaveDataCount)];
end;

function TPseudoSpectrumData.GetWaveDataLongC(const Index: MVInt): MVFloat; stdcall;
//var
  //AWData2: PSD2BufferFormat absolute FWaveBuffer;
begin
  Result:=PSD2BufferFormat(FWaveBuffer)^[Index];
end;

function TPseudoSpectrumData.GetWaveDataLongBC(const Index,Buffer: MVInt): MVFloat; stdcall;
//var
  //AWData2: PSD2BufferFormat absolute FWaveBuffer;
begin
  Result:=PSD2BufferFormat(FWaveBuffer)^[Index+(Buffer*_ShortWaveDataCount)];
end;

end.

