unit SpectrumData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, VPBuffers;

const
  BeatAnaSubbandCount   = 32;

type
  TFFTMode       = (fftCombined,fftAllChannels,fftFirst);
  TSDAnalyseFlag = (afFFT,afPeak);
  TSDAnalyseMode = set of TSDAnalyseFlag;
  TBPMFoundMode  = (bpmFalse = 0, bpmTrue = 1, bpmScanning = 2);
  MVBeatState    = record
    Togg,Free          : Boolean;
    Max,Pos            : MVInt;
    BeatLength,BeatTime: MVFloat;
    BeatStart          : MVTime;
    SyncTogg           : Boolean;
  end;
  TCSubbandSize  = record
    First,Last: Single;
  end;
  TCSubbandSizes = array [0..BeatAnaSubbandCount-1] of TCSubbandSize;
  PCSubbandSizes = ^TCSubbandSizes;
  TSubbandSizes  = array [0..BeatAnaSubbandCount-1] of MVInt;
  TSubbandValues = array [0..BeatAnaSubbandCount-1] of MVInt;
  IBasicSpectrumData= interface (IMInterface)
    ['{B9FB0613-0F9F-414C-B3DE-1A05027C6C84}']
    function GetChannels: MVInt; stdcall;
    function GetSampleRate: MVInt; stdcall;
    function GetLocked: Boolean; stdcall;

    property Channels: MVInt read GetChannels;
    property Locked: Boolean read GetLocked;
    property SampleRate: MVInt read GetSampleRate;
  end;
  IBasicFFTSpectrumData= interface (IBasicSpectrumData)
    ['{39361EED-ABCD-4C09-94F7-8B793221E08D}']
    function GetWaveData(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetWaveDataLong(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetLevels(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetFreqCount: MVInt; stdcall;
    function GetWaveDataCount: MVInt; stdcall;
    function GetLongWaveDataCount: MVInt; stdcall;

    property Levels[const AIndex,AChannel: MVInt]: MVFloat read GetLevels;
    property WaveData[const AIndex,AChannel: MVInt]: MVFloat read GetWaveData;
    property WaveDataLong[const AIndex,AChannel: MVInt]: MVFloat read GetWaveDataLong;
    property FreqCount: MVInt read GetFreqCount;
    property LongWaveDataCount: MVInt read GetLongWaveDataCount;
    property WaveDataCount: MVInt read GetWaveDataCount;
  end;

  ISpectrumData        = interface (IBasicFFTSpectrumData)
    ['{C53D164F-7DD2-42EE-B657-EE5C42F6EEB8}']
    function GetBuffer(const ABufferIndex,AChannel: MVInt): vpRealBuffer; stdcall;
    function GetLevelBuffer(const AChannel: MVInt): vpRealBuffer; stdcall;

    function GetSyncBufferCount: MVInt; stdcall;

    function GetPeak(const Channel: MVInt): MVFloat; stdcall;
    function GetBeatAt(const Subband: MVInt): Boolean; stdcall;
    function GetBeatAtH(const Subband,History: MVInt): Boolean; stdcall;
    function GetBeatState: MVBeatState; stdcall;
    function GetBPM: MVFloat; stdcall;
    function GetBPMFound: TBPMFoundMode; stdcall;

    property Beat: MVBeatState read GetBeatState;
    property BPM: MVFloat read GetBPM;
    property BPMFound: TBPMFoundMode read GetBPMFound;

    property Peak[Channel: MVInt]: MVFloat read GetPeak;
    property BeatAt[Subband: MVInt]: Boolean read GetBeatAt;
    property BeatAtH[Index,Subband: MVInt]: Boolean read GetBeatAtH;
  end;

const
  WaveDataBI       = 0;
  WaveDataLongBI   = 1;
  FreqDataBI       = 2;
  SyncBufferBI     = 3;
  SyncBufferSaverBI= 4;
  TOISyncBufferBI  = 5;

implementation

end.

