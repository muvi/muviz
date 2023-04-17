unit FreqAna2;

{$mode objfpc}{$H+}

interface

{$DEFINE EAT} //EnAble Threads
{$DEFINE DST} //Do SomeThing

uses
  Classes, SysUtils, SpectrumData, FFTFreqAna, BasicFreqAna,
  UltraSort, Math, ExtPtrType, PluginType, Windows, AdvClasses, AdvFunc,
  GenericMemoryArrays, VPBuffers, MPFFreqAna, AdvMPFFreqAna,
  GenericMemoryArraysXD;

type
  TMVIntBuffer           = specialize TGenericBufferArray<MVInt>;
  TMVFloatBufferXD       = specialize TGenericBufferArrayXD<MVFloat>;
  TSubbandBitsItem       = LongInt;
  PSubbandBitsItem       = TSubbandBitsItem;
  TSubbandBitBuffer      = specialize TGenericBufferArray<TSubbandBitsItem>;

  TBeat600Block          = record
    Value : MVFloat;
    Offset: MVInt;
    Valid : Boolean;
    Time  : Integer;
  end;

  TFSBeatBuffers         = record
    SSubbandHistory,SAverageSoundEnergys,{SBeatEquality,}STestSubbandHistory,{STakeFirstEquality,}SSyncBuffer: TMVFloatBuffer;
    STakeFirstHistory                                                                                      : TMVFloatBufferXD;
    SCoreSyncBuffer                                                                                        : TMVIntBuffer; //CoreSyncBuffer: enthält das Offset vom autokorellieren, bei dem das höchste multiplikationsergebnis erziehlt wurde. (SCoreSyncBuffer.Count=STakeFirstEquality.Count)
    SBeatAtHistory,SBeatAtHistory2,SBeatAtHistory600                                                       : TSubbandBitBuffer;
    SIsBeatAt,SIsBeatAt600,STakeFirstHistoryPos                                                            : TBam;
    SBeatAtPos,SBeatAtPos600,SBeatAtPos2,SBeatCompareCount,SBPMContinuity{,STakeFirstHistoryPos}             : MVInt;
    SFirstBands,SLastBands                                                                                 : TSubbandSizes;
    SAverageBeatEquality,SLastBPM                                                                          : MVFloat;
    SFirstBPM,SSyncLength                                                                                  : MVInt;
  end;

  TBeatSyncInfo          = record
    T1,T2,T3: Real;
    Time    : UInt64;
  end;

  TPeakBlock             = record
    Value : MVFloat;
    Offset: MVInt;
  end;

  TNormalizedBPM         = record
    BPM   : MVFloat;
    Factor: MVSInt;
  end;

  TEmptyEvent            = procedure of object;
  TResultEvent           = function: Boolean of object;
  TCorellationFunction   = function (const X: Integer; var AHighestPos: MVInt): MVFloat of object;
  TBeatSyncMode          = (sbSin = 0, sbTOI = 1, sbMPF = 2, sbAutoCor = 3);

  TBPMThread             = class;
  TAnalysingThread       = class;
  TMAnalysingThread      = class;
  TSyncThread            = class;

  TBeatSpectrumData      = class (TAdvMPFSpectrumData,ISpectrumData,ISpectrumData2)
  private
    FBeatRecognized     : Boolean;
    FBeat               : MVBeatState;
    FBeatFreq           : MVFloat;
    FBeatSensibility    : MVFloat;
    FSoundHistory       : TMVFloatBuffer;
    FBPMHistory         : TMVFloatBuffer;
    FSoundEnergyAverage : MVFloat;
    FBeatSyncHCount     : MVInt;
    FNormBeatDiff       : MVFloat;
    FABeatBuf           : Cardinal;
    FFSSensibility      : MVFloat;
    FBPMSensibility     : MVFloat;
    FBPM                : MVFLoat;
    FBPMFound           : TBPMFoundMode;
    FBPMHistoryPos      : MVInt;
    FFSBeatBuffers      : TFSBeatBuffers;
    FBPMThread          : TBPMThread;
    FBeatOptimizer      : Boolean;
    FBeatOptimizerH     : Boolean;
    FBPMFac             : MVFloat;
    FNewBeatBufCount    : MVInt;
    FNewBeatHistoryCount: MVInt;
    FNewBeatCompareCount: MVInt;
    FNewBPMHistoryCount : MVInt;
    FSyncBufferCount    : MVInt;

    FTSBPos             : Cardinal;
    FSyncThread         : TSyncThread;
    FSaveSyncCount      : MVInt;

    FTempBeatBufList    : TLList;
    FBeat600Blocks      : TLCFVector;
    FBeat600ConCount    : MVInt;
    FBeat600Average     : MVFloat;
    FAOffset            : MVInt;
    FUseBeat600         : Boolean;
    _CalcBandWidths     : TEmptyEvent;
    FBandFrequencys     : TCSubbandSizes;
    FDoGetBPM           : TEmptyEvent;
    FDoSync             : TResultEvent;
    FMakeBPM            : Double;
    FTakeFirstDelta     : MVFloat;
    FDoTFsqr            : Boolean;
    FSyncEnabled        : Boolean;
    FBPMEnabled         : Boolean;
    //temporär/nur zur Anzeige... sollte später gelöscht werden
    //FTakeFirst          : Boolean;
    FTakeFirstMax       : MVFloat;
    FTakeFirstMaxPos    : MVInt;
    FMaxPosOffset       : MVInt;
    FUseDirectCor       : Boolean;
    FDoCorellate        : TCorellationFunction;
    FPeakHistory        : array of TPeakBlock;
    FPeakHistoryPos     : Cardinal;
    FPeakHistory2       : array of TPeakBlock;
    FPeakHistory2Pos    : Cardinal;
    FDoDPS              : Boolean;
    FDPSPos             : MVInt; (**)
    FSyncSeconds        : MVInt;
    FBeatSamples        : MVInt;
    FBeatFrames         : MVInt;
    FBeatFFTFrames      : MVFloat;
    FOldSyncTime        : MVFloat;
    FBeatTime           : MVFloat;
    FLastTap            : MVFloat;
    //Zeit in Sekunden, die in der STakeFirstHistorygespeichert werden soll
    FBeatHistoryTime    : MVFloat;

    FSyncStartTime      : MVFloat;
    FNewSyncOffset      : MVFloat; //Zeit seit dem letzten Beat (gemessen zu Anfang des Sync-Threads) in Sekunden
    //FExistsNewSync      : Boolean;
    FExistsNewSync      : Boolean;
    FTestBeatTime       : MVFloat;
    FAutoSync           : Boolean;
    //FSyncTOI            : Boolean;
    FSyncMode           : TBeatSyncMode;
    FVPB                : MVFloat;
    procedure DoGetPeakHistory; inline;
    procedure DoGetBeat; inline;
    procedure DoGetBeatFS; inline;
    procedure DoSync;
    procedure DoGetBPM;
    //procedure DoGetBPM_Default;
    procedure DoGetBPM_TakeFirst;
    function AutocorellateBeat(const X: Integer; var AHighestPos: MVInt): MVFloat;
    function AutocorellateBeat_Direct(const X: Integer; var AHighestPos: MVInt): MVFloat;
    //function AutocorellateBeat_Default(const X: Integer): MVFloat;
    procedure AfterDetectBPM;
    procedure DoGetBeatPos; inline;

    function GetMPFHistoryItem(Index: Cardinal): TSD2BufElem;

    function SyncBeatTOI2: Boolean;
    function SyncBeatSin: Boolean;
    function SyncBeatMPF: Boolean;
    function SyncBeatNot: Boolean;

    function GetBeatBufCount: MVInt; stdcall;
    function GetSoundHistory(const Index: MVInt): MVFloat; stdcall;
    function GetSyncBufferCount: MVInt; stdcall;

    function GetBeatAt(const Subband: MVInt): Boolean; stdcall;
    function GetBeatAtH(const Subband,History: MVInt): Boolean; stdcall;
    function GetBeatAt600(const Subband: MVInt): Boolean; stdcall;
    function GetBeatAtH600(const Subband,History: MVInt): Boolean; stdcall;
    function GetFSAverage(const Subband: MVInt): MVFloat; stdcall;
    function GetFSHistory(const Index,Subband: MVInt): MVFloat; stdcall;
    function GetFSTHistory(const Index,Subband: MVInt): MVFloat; stdcall;
    function GetSyncHistory(const Index: MVInt): MVFloat; stdcall;
    function GetBeat600Block(const Index,Subband: MVInt): TBeat600Block; stdcall;
    function GetBandWidths(const Subband: MVInt): MVInt;
    function GetFirstBands(const Subband: MVInt): MVInt;
    function GetLastBands(const Subband: MVInt): MVInt;
    function GetTakeFirstCount: MVInt;
    function GetTakeFirstEquality(const Index: MVInt): MVFloat; overload;
    function GetBeat600BlockCount: MVInt; stdcall;
    function GetBeatState: MVBeatState; stdcall;
    function GetBeatPos: MVFloat; stdcall;
    function GetBPM: MVFloat; stdcall;
    function GetBPMFound: TBPMFoundMode; stdcall;
    //function GetTakeFirstHistoryCount: MVInt;
    function GetTakeFirstHistoryPos: TBam;
    function GetTakeFirstHistory: TMVFloatBufferXD;


    function GetBeatHistoryCount: MVInt;
    //function GetBeatEqualityCount: MVInt;
    function GetBeatCompareCount: MVInt;
    function GetBPMHistoryCount: MVInt;

    function GetSyncEnabled: Boolean;
    procedure SetSyncEnabled(Value: Boolean);
    function GetBPMEnabled: Boolean;
    procedure SetBPMEnabled(Value: Boolean);
    procedure SetSyncMode(Value: TBeatSyncMode);

    //function GetBeatEquality(const History: MVInt): MVFloat;
    function GetAverageBeatEquality: MVFloat;

    procedure CalcBandWidths;
    procedure CalcBandWidths_Default;
    procedure CalcBandWidths_FreqList;
    function InstantSoundEnergy: MVFloat;
    function SoundEnergyVariance(const ANewEnergy: MVFloat): MVFloat;

    function SD2BufElemCanBefore(const ANew,AExisting): Boolean; stdcall;
    function NormalizeBPM(ABPM: MVFloat): TNormalizedBPM;
  protected
    procedure DoAnalyse; override;
    procedure InitCalcBeat; inline;
    procedure DoInit; override;
    //procedure BeforeInit; override;
    procedure Launched(var ACounts: array of MVInt); override;

    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
  public
    (***)
    FSyncHistory        : TMVFloatBuffer;
    FRSyncHistory       : TMVFloatBuffer;
    FSyncHistoryPos     : MVInt;
    FSyncRelation       : MVFloat;
    FBeatSubPos         : MVFloat;
    FLastBeatSubPos     : MVFloat;

    constructor Create(ABufferManager: IVPBufferManager); override;
    destructor Destroy; override;
    procedure Run; override;
    procedure Stop; override;
    procedure SetCounts(const AWaveDataCount,ALongWaveDataCount,AFreqCount,AFFTHistoryCount,ABeatBufCount{,ABeatSubbandCount},ABeatHistoryCount,ABeatCompareCount,ABPMHistoryCount,ASyncBufferCount,ASaveSyncCount: Cardinal; ABeatHistoryTime: MVFloat);
    procedure SyncTap;
    procedure BPMTap;
    procedure SetBandWidthsCalcMode(const AUseFreqList: Boolean);
    function TakeFirstIndexToBPM(AIndex: MVInt): MVFloat;
    property BandFrequencys: TCSubbandSizes read FBandFrequencys write FBandFrequencys;
    property Beat: MVBeatState read FBeat;
    property SoundHistory[Index: MVInt]: MVFloat read GetSoundHistory;
    property NormBeatDiff: MVFloat read FNormBeatDiff;
    property AverageBeatEquality: MVFloat read GetAverageBeatEquality;

    property BandWidths[const Subband: MVInt]: MVInt read GetBandWidths;
    property FirstBands[const Subband: MVInt]: MVInt read GetFirstBands;
    property LastBands[const Subband: MVInt]: MVInt read GetLastBands;
    property Beat600Blocks[const Index,Subband: MVInt]: TBeat600Block read GetBeat600Block;
    property BeatAt[Subband,History: MVInt]: Boolean read GetBeatAtH;
    property BeatAt600[Subband,History: MVInt]: Boolean read GetBeatAtH600;
    //property BeatEquality[History: MVInt]: MVFloat read GetBeatEquality;
    property BPMThread: TBPMThread read FBPMThread;
    property FSAverage[Subband: MVInt]: MVFloat read GetFSAverage;
    property FSHistory[Index,Subband: MVInt]: MVFloat read GetFSHistory;
    property FSTHistory[Index,Subband: MVInt]: MVFloat read GetFSTHistory;
    property SyncHistory[const Index: MVInt]: MVFloat read GetSyncHistory;
    property SyncThread: TSyncThread read FSyncThread;
    property TakeFirstEquality[const Index: MVInt]: MVFloat read GetTakeFirstEquality;
    property TakeFirstHistory: TMVFloatBufferXD read GetTakeFirstHistory;
    property TakeFirstHistoryPos: TBam read GetTakeFirstHistoryPos;
  published
    property AOffset: MVInt read FAOffset;
    property AutoSync: Boolean read FAutoSync write FAutoSync;
    property BeatHistoryTime: MVFloat read FBeatHistoryTime;
    property BeatOptimizer: Boolean read FBeatOptimizer write FBeatOptimizer;
    property BeatOptimizerH: Boolean read FBeatOptimizerH write FBeatOptimizerH;
    property BeatSamples: MVInt read FBeatSamples;
    property BeatSyncHCount: MVInt read FBeatSyncHCount write FBeatSyncHCount;
    property BPM: MVFloat read FBPM;
    property BPMFac: MVFloat read FBPMFac write FBPMFac;
    property BPMFound: TBPMFoundMode read FBPMFound;
    property BPMHistoryCount: MVInt read GetBPMHistoryCount;
    property BPMSensibility: MVFLoat read FBPMSensibility write FBPMSensibility;
    property Beat600BlockCount: MVInt read GetBeat600BlockCount;
    property Beat600ConCount: MVInt read FBeat600ConCount;
    property BeatBufferCount: MVInt read GetBeatBufCount;
    property BeatCompareCount: MVInt read GetBeatCompareCount;
    //property BeatEqualityCount: MVInt read GetBeatEqualityCount;
    property BeatHistoryCount: MVInt read GetBeatHistoryCount;
    //property BeatIdleTime: MVInt read GetBeatIdleTime write SetBeatIdleTime;
    property BeatPos: MVFloat read GetBeatPos;
    property BeatSensibility: MVFloat read FBeatSensibility write FBeatSensibility;
    property DoTFsqr: Boolean read FDOTFsqr write FDoTFsqr;
    property DPS: Boolean read FDoDPS write FDoDPS;
    property DPSPosTemp: MVInt read FDPSPos;
    property FSSensibility: MVFloat read FFSSensibility write FFSSensibility;
    property BPMEnabled: Boolean read GetBPMEnabled write SetBPMEnabled;
    property SyncEnabled: Boolean read GetSyncEnabled write SetSyncEnabled;
    //property TakeFirst: Boolean read FTakeFirst;
    property TakeFirstCount: MVInt read GetTakeFirstCount;
    property TakeFirstDelta: MVFloat read FTakeFirstDelta write FTakeFirstDelta;
    //property TakeFirstHistoryPos: MVInt read GetTakeFirstHistoryPos;
    property TakeFirstMax: MVFLoat read FTakeFirstMax;
    property TakeFirstMaxPos: MVInt read FTakeFirstMaxPos;
    property TakeFirstMaxPosOffset: MVInt read FMaxPosOffset write FMaxPosOffset;
    property SaveSyncCount: MVInt read FSaveSyncCount;
    property SyncBufferCount: MVInt read GetSyncBufferCount;
    property SyncSeconds: MVInt read FSyncSeconds write FSyncSeconds;
    property SyncMode: TBeatSyncMode read FSyncMode write SetSyncMode;
    property UseBeat600: Boolean read FUseBeat600 write FUseBeat600;
    property VPB: MVFloat read FVPB;
  end;

  TAnalysingThread    = class (TThread)
  private
    FSpectrumData  : TBeatSpectrumData;
    FFramesUsed    : MVInt;
    FFramesNeeded  : MVInt;
    FTimeNeeded    : MVFloat;
    FFullTimeNeeded: MVFloat;
    FTimeUsed      : MVFloat;
    FStartTime     : MVFloat;
    FReady         : Byte;
    FExeTime       : MVFloat;
    function GetFPS: MVFloat;
    procedure SetFPS(Value: MVFloat);
    function GetMaxFPS: MVFloat;
    function GetActiveFPS: MVFloat;
  protected
    procedure Execute; override;
    procedure DoInit; virtual; abstract;
    procedure DoExecute; virtual; abstract;
    property SpectrumData: TBeatSpectrumData read FSpectrumData;
  public
    constructor Create(AOwner: TBeatSpectrumData); virtual; reintroduce;
    destructor Destroy; override;
    procedure NextFrame(ATime: MVFloat);
    procedure BlockingNextFrame;
    procedure WaitForFrame;
    property ActiveFPS: MVFloat read GetActiveFPS;
    property ExeTime: MVFloat read FExeTime write FExeTime;
    property FPS: MVFloat read GetFPS write SetFPS;
    property FramesNeeded: MVInt read FFramesNeeded;
    property FullTimeNeeded: MVFloat read FFullTimeNeeded;
    property MaxFPS: MVFloat read GetMaxFPS;
    property StartTime: MVFloat read FStartTime;
    property TimeNeeded: MVFloat read FTimeNeeded;
  end;

  TEvent              = procedure of object;

  TMAnalysingThread   = class (TAnalysingThread)
  private
    FOnExecute: TEvent;
  protected
    procedure DoInit; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TBeatSpectrumData; AOnExecute: TEvent); virtual; reintroduce;
  end;

  TBPMThread          = class (TMAnalysingThread)
  protected
    FBPMList           : TUltraSortList;
    FBPMHistoryList    : TUltraSortList;
    procedure DoInit; override;
  public
    constructor Create(AOwner: TBeatSpectrumData); virtual; reintroduce;
    destructor Destroy; override;
  end;

  TSyncThread         = class (TMAnalysingThread)
  protected
    procedure DoInit; override;
  public
    constructor Create(AOwner: TBeatSpectrumData); virtual; reintroduce;
  end;

  TSignalFunc = function(Index: Cardinal): TSD2BufElem of object;

function SignalPeakOffset(const ASignal; const ASize: Cardinal; APeakFreq: Real): Real;
function SignalPeakOffsetF(ASignal: TSignalFunc; const ASize: Cardinal; APeakFreq: Real): Real;

const
  minBPM           = 50.0;
  maxBPM           = 300.0;
  ln2              = ln(2);
  maxBPMDHTimeCount= Trunc(ln(maxBPM/minBPM)/ln(2));
  minBPS           = minBPM/60.0;
  maxBPS           = maxBPM/60.0;
  minBeatLength    = 1.0/minBPS;
  maxBeatLength    = 1.0/maxBPS;
  minBPM2          = 80.0;
  maxBPM2          = minBPM2*2.0;

procedure CopySubbandBits(ASource,ADest: TSubbandBitBuffer; const ASourceIndex,ADestIndex,ACount: MVInt); inline;

procedure SBI_Reset(var SBI: TSubbandBitsItem); inline;
procedure SBI_Set(var SBI: TSubbandBitsItem; const Index: MVInt); inline;
function SBI_Get(const SBI: TSubbandBitsItem; const Index: MVInt): Boolean; inline;
function SBI_Compare(const SBI1,SBI2: TSubbandBitsItem): TSubbandBitsItem; inline;
function SBI_Evaluate(const SBI: TSubbandBitsItem): MVInt; inline;
procedure SBI_Copy(const Source: TSubbandBitsItem; var Dest: TSubbandBitsItem); inline;

implementation

{TBeatSpectrumData}

constructor TBeatSpectrumData.Create(ABufferManager: IVPBufferManager);
begin
  inherited Create(ABufferManager);
  {$IFDEF DST}
  FTempBeatBufList:=TLList.Create(SizeOf(MVFloat));
  FTempBeatBufList.OnCanBefore:=@SD2BufElemCanBefore;
  _CalcBandWidths:=@CalcBandWidths_Default;
  FDoGetBPM:=@DoGetBPM_TakeFirst;
  FDoCorellate:=@AutocorellateBeat_Direct;
  FDoTFsqr:=true;
  FUseDirectCor:=false;
  FMaxPosOffset:=2;
  //FTakeFirst:=false;
  FTakeFirstDelta:=0.75;
  FMakeBPM:=0.0;
  FPeakHistoryPos:=0;
  FPeakHistory2Pos:=0;
  FDoDPS:=false;
  FDPSPos:=0;
  FSyncSeconds:=5;
  FBeatSamples:=44100;
  FBeatFrames:=86;
  FBeatFFTFrames:=86.0;
  FOldSyncTime:=0.0;
  FBeatTime:=0.0;

  FTestBeatTime:=0.0;
  FExistsNewSync:=false;
  FSyncStartTime:=0.0;
  FNewSyncOffset:=0.0;
  FBeatHistoryTime:=0.0;
  SetSyncMode(sbSin);

  FSoundHistory:=TMVFloatBuffer.Create(0,Self);
  FBPMHistory:=TMVFloatBuffer.Create(0,Self);
  with FFSBeatBuffers do begin
    SSubbandHistory:=TMVFloatBuffer.Create(0,Self);
    SAverageSoundEnergys:=TMVFloatBuffer.Create(0,Self);
    //SBeatEquality:=TMVFloatBuffer.Create(0,Self);
    STestSubbandHistory:=TMVFloatBuffer.Create(0,Self);
    //STakeFirstEquality:=TMVFloatBuffer.Create(0,Self);
    SCoreSyncBuffer:=TMVIntBuffer.Create(0,Self);
    SSyncBuffer:=TMVFloatBuffer.Create(0,Self);
    STakeFirstHistory:=TMVFloatBufferXD.Create([],Self);
    STakeFirstHistoryPos:=EMPTYBAM;
    SBeatAtHistory:=TSubbandBitBuffer.Create(0,Self);
    SBeatAtHistory2:=TSubbandBitBuffer.Create(0,Self);
    SBeatAtHistory600:=TSubbandBitBuffer.Create(0,Self);
  end;

  FBeatRecognized:=true;
  FSyncBufferCount:=0;
  FABeatBuf:=0;
  FNewBeatBufCount:=0;
  FNewBeatHistoryCount:=0;
  FNewBeatCompareCount:=0;
  FNewBPMHistoryCount:=0;
  FBeatFreq:=132300.0; //20 BPM bei 44100 Samples/Sekunde: (60/20)*44100
  with FFSBeatBuffers do begin
    SBeatAtPos:=0;
    SBeatAtPos2:=0;
    SBeatAtPos600:=0;
    SBeatCompareCount:=1;
    SBPMContinuity:=0;
    SLastBPM:=0.0;
    SFirstBPM:=0;
    SSyncLength:=0;
  end;
  FSoundEnergyAverage:=0.0;
  FBeatSensibility:=1.3;
  FFSSensibility:=3.0;
  FBPMSensibility:=1.3;
  FBPM:=0.0;
  FBPMFound:=bpmFalse;
  FBeatOptimizer:=true;
  FBeatOptimizerH:=true;
  FBPMFac:=1.0;
  FBPMHistoryPos:=0;
  FBeatSyncHCount:=150;
  with FBeat do begin
    SyncTogg:=false;
    Max:=4;
    Pos:=0;
    BeatLength:=20.0;
    Reserved:=0.0;
    Free:=true;
    Togg:=false;
    BeatStart:=0;
  end;
  FNormBeatDiff:=0.0;
  FTSBPos:=0;
  FSaveSyncCount:=10;
  FBeat600Blocks:=TLCFVector.Create(SizeOf(TBeat600Block));
  //FBeat600Blocks:=TBeat600BlockBuffer.Create(0,Self);
  FAOffset:=0;
  FUseBeat600:=true;

  FSyncHistory:=TMVFloatBuffer.Create(250,Self);
  FSyncHistory.Fill2(0.0);
  FRSyncHistory:=TMVFloatBuffer.Create(250,Self);
  FRSyncHistory.Fill2(0.0);
  FSyncHistoryPos:=249;
  FSyncRelation:=0.0;
  FBeatSubPos:=0.0;
  FLastBeatSubPos:=0.0;
  FAutoSync:=true;
  FVPB:=0.0;
  FLastTap:=0.0;

  FSyncEnabled:=true;
  FBPMEnabled:=true;

  {$IFDEF EAT}
  FBPMThread:=TBPMThread.Create(Self);
  FSyncThread:=TSyncThread.Create(Self);
  {$ELSE}
  FBPMThread:=nil;
  FSyncThread:=nil;
  {$ENDIF}

  {$ENDIF}
end;

destructor TBeatSpectrumData.Destroy;
begin
  {$IFDEF DST}
  Lock;

  {$IFDEF EAT}
  if not FBPMThread.Suspended then begin
    //FBPMThread.WaitFor;
    FBPMThread.Terminate;
    FBPMThread.WaitForFrame;
  end;
  FBPMThread.Destroy;

  if not FSyncThread.Suspended then begin
    //FSyncThread.WaitFor;
    FSyncThread.Terminate;
    FSyncThread.WaitForFrame;
  end;
  FSyncThread.Destroy;
  {$ENDIF}

  FBeat600Blocks.Destroy;
  //FSyncThread.Terminate;
  //FSyncThread.Destroy; (***)


  FTempBeatBufList.Destroy;
  {$ENDIF}
  inherited Destroy;
end;

const
  Local_Version : MVVersion = (Version:0;MainVersion:3;SubVersion:0);
  Local_Version4: MVVersion = (Version:0;MainVersion:4;SubVersion:0);

function TBeatSpectrumData.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version4;
end;

function TBeatSpectrumData.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version4
    then Result:=IMInterface(ISpectrumData2(Self))
    else if Version=Local_Version
      then Result:=IMInterface(ISpectrumData(Self))
      else Result:=inherited Future(Version);
end;

function TBeatSpectrumData.SyncBeatSin: Boolean;
begin
  with FBeat do begin
    if IsZero(BeatLength) or (BeatLength>132300.0) or IsZero(FBeatFreq) then begin
      Result:=false;
      exit;
    end;
    FNewSyncOffset:=SignalPeakOffset(GetBufferPtr(SyncBufferBI,0)^,FSyncBufferCount,FBeatFreq)/SampleRate;
  end;
  Result:=true;
end;

function TBeatSpectrumData.GetMPFHistoryItem(Index: Cardinal): TSD2BufElem;
begin
  with FMPRTHistoryW do Result:=Items[(FMPFHistoryPos+(Count-Index-1)) mod Count];
end;

function TBeatSpectrumData.SyncBeatMPF: Boolean;
var
  AResult: Real;
begin
  with FBeat do begin
    if IsZero(BeatLength) or (BeatLength>132300.0) or IsZero(FBeatFreq) then begin
      Result:=false;
      exit;
    end;
    with FMPRTHistoryW do AResult:=SignalPeakOffsetF(@GetMPFHistoryItem,Count,FBeatFFTFrames);
    FNewSyncOffset:=(AResult*FreqCount)/SampleRate;
  end;
  Result:=true;
end;

function TBeatSpectrumData.SyncBeatNot: Boolean;
begin
  Result:=false;
end;

procedure TBeatSpectrumData.DoSync;
begin
  if FDoSync() then begin
    FSyncStartTime:=FSyncThread.StartTime;
    FExistsNewSync:=true;
  end;
end;

procedure TBeatSpectrumData.DoAnalyse;
begin
  inherited DoAnalyse;
  {$IFDEF DST}
  if Recognized then FBeatRecognized:=true;
  DoGetPeakHistory;
  DoGetBeat;
  DoGetBeatFS;
  //DoSync;
  DoGetBeatPos;
  //FSyncThread.{Blocking}NextFrame;
  //DoTempSync;
  {$IFDEF EAT}
  FBPMThread.NextFrame(SyncTime);
  FSyncThread.NextFrame(SyncTime);
  {$ENDIF}
  {$ENDIF}
end;

procedure TBeatSpectrumData.SetBandWidthsCalcMode(const AUseFreqList: Boolean);
begin
  if AUseFreqList
    then _CalcBandWidths:=@CalcBandWidths_FreqList
    else _CalcBandWidths:=@CalcBandWidths_Default;
end;

{procedure TBeatSpectrumData.SetBPMMode(const ATakeFirst: Boolean);
begin
  if ATakeFirst
    then FDoGetBPM:=@DoGetBPM_TakeFirst
    else FDoGetBPM:=@DoGetBPM_Default;
  FTakeFirst:=ATakeFirst;
end;}

{procedure TBeatSpectrumData.SetCorellationMode(const ADirectCor: Boolean);
begin
  if ADirectCor
    then FDoCorellate:=@AutoCorellateBeat_Direct
    else FDoCorellate:=@AutocorellateBeat_Default;
end;}

procedure TBeatSpectrumData.CalcBandWidths;
begin
  _CalcBandWidths;
end;

procedure TBeatSpectrumData.CalcBandWidths_FreqList;
var
  I                         : Integer;
begin
  for I:=0 to BeatAnaSubbandCount-1 do with FFSBeatBuffers do begin
    SFirstBands[I]:=Round(FrequencyLevel(FBandFrequencys[I].First));
    SLastBands[I]:=Round(FrequencyLevel(FBandFrequencys[I].Last));
  end;
end;

procedure TBeatSpectrumData.CalcBandWidths_Default;
var
  I                                       : Integer;
  a,BandRest,APreWidth                    : MVFloat;
  ASum,AWidth                             : Integer;
begin
  a:=FreqCount/(BeatAnaSubbandCount*(BeatAnaSubbandCount-1)){/2*2};
  BandRest:=0;
  ASum:=0;
  for I:=0 to BeatAnaSubbandCount-1 do with FFSBeatBuffers do begin
    APreWidth:=(a*I)+BandRest;
    AWidth:=Round(APreWidth);
    if AWidth<=0 then AWidth:=1;
    BandRest:=APreWidth-AWidth;
    SFirstBands[I]:=ASum;
    ASum+=AWidth;
    SLastBands[I]:=ASum-1;
  end;
end;

function TBeatSpectrumData.InstantSoundEnergy: MVFloat;
var
  I,J    : Integer;
  AWData : Pointer;
  AWData2: PSD2BufElem absolute AWData;
begin
  Result:=0;
  for I:=0 to Channels-1 do begin
  AWData:=GetBufferPtr(WaveDataBI,I);
    for J:=0 to WaveDataCount-1 do begin
      Result+=sqr(AWData2^);
      AWData+=SD2BufElemSize;
    end;
  end;
  Result/=WaveDataCount;
end;

function TBeatSpectrumData.SoundEnergyVariance(const ANewEnergy: MVFloat): MVFloat;
var
  I            : Integer;
begin
  Result:=0;
  for I:=0 to FSoundHistory.Count-1 do begin
    Result+=sqr(FSoundHistory[(FABeatBuf+I) mod FSoundHistory.Count]-ANewEnergy);
  end;
  Result/=FSoundHistory.Count;
end;

function TBeatSpectrumData.SD2BufElemCanBefore(const ANew,AExisting): Boolean; stdcall;
var
  ANew2     : TSD2BufElem absolute ANew;
  AExisting2: TSD2BufElem absolute AExisting;
begin
  Result:=(ANew2>AExisting2);
end;

{function TBeatSpectrumData.NormalizeBPM(ABPM: MVFloat): TNormalizedBPM;
var
  AFactor: Real;
begin
  AFactor:=ld(ABPM/minBPM2);
  if AFactor>=0
    then Result.Factor:=Trunc(AFactor)
    else Result.Factor:=Trunc(AFactor)-1;
  Result.BPM:=ABPM/Result.Factor;
end;}

function TBeatSpectrumData.NormalizeBPM(ABPM: MVFloat): TNormalizedBPM;
begin
  Result.BPM:=ABPM;
  Result.Factor:=0;
  while Result.BPM>=maxBPM2 do begin
    Inc(Result.Factor);
    Result.BPM/=2.0;
  end;
  while Result.BPM<minBPM2 do begin
    Dec(Result.Factor);
    Result.BPM*=2.0;
  end;
end;

procedure TBeatSpectrumData.DoGetPeakHistory; inline;
begin
  FPeakHistoryPos:=(FPeakHistoryPos+1) mod FSoundHistory.Count;
  with FPeakHistory[FPeakHistoryPos] do begin
    Value:=Peak[0];
    Offset:=PeakPos[0];
  end;
end;

procedure TBeatSpectrumData.DoGetBeat; inline;
var
  NewSoundEnergy  : MVFloat;

  procedure DGB_CompareEnergy;
  begin
    if NewSoundEnergy>FNormBeatDiff*FBeatSensibility then begin
      FBeat.Free:=false;
      FBeat.Togg:=not FBeat.Togg;
      FBeat.BeatStart:=Trunc(SyncTime*1000);
      FBeatRecognized:=false;
    end else begin
      if ((SyncTime*1000)-FBeat.BeatStart>30) and FBeatRecognized then FBeat.Free:=true;
    end;
  end;

  procedure DGB_RenewEnergy;
  var
    ASoundHistory: TBam;
  begin
    FABeatBuf:=(FABeatBuf+1) mod FSoundHistory.Count;
    ASoundHistory:=FSoundHistory.ToItem(FABeatBuf);
    FNormBeatDiff-=FSoundHistory.Bams[ASoundHistory]/FSoundHistory.Count;
    FSoundHistory.Bams[ASoundHistory]:=NewSoundEnergy;
    FNormBeatDiff+=FSoundHistory.Bams[ASoundHistory]/FSoundHistory.Count;
  end;

begin
  NewSoundEnergy:=InstantSoundEnergy;
  FBeatSensibility:=(-0.0025714*SoundEnergyVariance(NewSoundEnergy))+1.5142857;
  DGB_CompareEnergy;
  DGB_RenewEnergy;
end;

procedure TBeatSpectrumData.DoGetBeatPos; inline;
var
  ASyncTime   : MVFloat;
  APosData    : TRealDivMod;
  ABeatLength : MVFloat;
  ABeatLength2: MVFloat;
  ATimePassed : MVFloat;

  {function MissedBeats(ABeatTime: Real): Real;
  var
    AMissedTime: Real;
  begin
    AMissedTime:=SyncTime-FSyncStartTime-ABeatTime;
    if AMissedTime>=0
      then Result:=(Int(AMissedTime/ABeatLength)+1.0)*ABeatLength
      else Result:=0.0;
  end; }

  function BeatTimeDiff(BeatTime1,BeatTime2: Real): Real;
  var
    AMaxPosChange: Real;
  begin
    if BeatTime1<BeatTime2 then begin
      Result:=ABeatLength-(BeatTime2-BeatTime1);
    end else begin
      Result:=(BeatTime1-BeatTime2);
    end;
    //Änderung der Beatposition um 0,5 / -0,5 Beats anstatt 0 / 1 Beat
    AMaxPosChange:=ABeatLength/2.0;
    if Result>AMaxPosChange then Result-=ABeatLength;
    //Vermeiden, dass die Beatposition sich zu stark ändert
    //bs: Anzahl der Senkunden, bis ein Beat vollständig angeglichen ist (=1)
    //Achtung: Recycling von "AMaxPosChange"!
    AMaxPosChange:=sqr(ABeatLength)/(4.0{2.0*bs});//(AMaxPosChange/0.125{bs})*ATimePassed;
    if Abs(Result)>AMaxPosChange
      then Result:=Sign(Result)*AMaxPosChange;
  end;

begin
  with FBeat do begin
    ABeatLength:=BeatLength; //BPM könnte während der Ausführung dieser Methode im Thread verändert werden...
    if IsZero(ABeatLength) then exit;
    ASyncTime:=SyncTime;
    ATimePassed:=ASyncTime-FOldSyncTime;
    FBeatTime+=ATimePassed;
    FTestBeatTime+=ATimePassed;
    FOldSyncTime:=ASyncTime;

    if FExistsNewSync then begin
      FTestBeatTime:=RealMod((SyncTime-FSyncStartTime)+FNewSyncOffset,ABeatLength);//-MissedBeats(FTestBeatTime);
      FExistsNewSync:=false;
    end;
    if FTestBeatTime>ABeatLength then begin
      {if FExistsNewSync then begin
        FExistsNewSync:=false;
        FTestBeatTime:=RealMod((SyncTime-FSyncStartTime)+FNewSyncOffset,ABeatLength);
      end else begin}
        APosData:=RealDivMod(FTestBeatTime,ABeatLength);
        //if FAutoSync then Pos:=(Pos+APosData.ADiv) mod Max;
        FTestBeatTime:=APosData.AMod;
      //end;
    end;

    FVPB:=BeatTimeDiff(FBeatTime,FTestBeatTime);
    if FAutoSync
      then ABeatLength2:=ABeatLength+FVPB
      else ABeatLength2:=ABeatLength;
    //FVPB:=ATimePassed*ABeatLength;
    //fvpb:=1;

    //muss auf jeden Fall "größer" als FLastBeatSubPos sein
    FBeatSubPos:=FBeatTime/ABeatLength2;

    if FBeatTime>ABeatLength2 then begin
      APosData:=RealDivMod(FBeatTime,ABeatLength2);
      {if not FAutoSync then }Pos:=(Pos+APosData.ADiv) mod Max;
      FBeatTime:=APosData.AMod;
    end;

    //Pos:=Trunc((SyncTime+BeatTime)/ABeatLength) mod Max;
    FSyncHistoryPos:=(FSyncHistoryPos+1) mod FSyncHistory.Count;
    FSyncHistory[FSyncHistoryPos]:=FTestBeatTime;
    FRSyncHistory[FSyncHistoryPos]:=FBeatTime;
    FSyncRelation:=(FTestBeatTime/ABeatLength)-0.5;

    if FBeatSubPos>FLastBeatSubPos then begin
      FBeatSubPos:=FBeatTime/ABeatLength2;
      FLastBeatSubPos:=FBeatSubPos;
    end;
  end;
end;

{procedure TBeatSpectrumData.DoSync;
begin
  {ABeatVal:=1.0;
  ABeatVal:=SD2Bits_Evaluate(FFSBeatBuffers.SIsBeatAt,FFSBeatBuffers.SBeatAtCount,FBeatSubbandCount);
  with FBeatSyncInfo do begin
    Inc(SUsedSince);
    if ABeatVal>=SMaxBeatValue then begin
      SMaxBeatValue:=ABeatVal;
      SMaxBeatPos:=0;
      if (SUsedSince>=FBeatSyncHCount) and (not IsZero(SMaxBeatValue)) then begin
        SyncTap;
        FBeat.SyncTogg:=not FBeat.SyncTogg;
      end;
    end;
    Inc(SMaxBeatPos);
    if SMaxBeatPos>=FBeatSyncHCount then begin
      SMaxBeatPos:=0;
      SMaxBeatValue:=0.0;
      SUsedSince:=0;
    end;
  end;}
end;}

procedure TBeatSpectrumData.BPMTap;
var
  ALastTap: MVFloat;
begin
  ALastTap:=FLastTap;
  SyncTap;
  if FLastTap-ALastTap<2.1 then begin
    FBPM:=60.0/(FLastTap-ALastTap);
    AfterDetectBPM;
  end;
end;

procedure TBeatSpectrumData.SyncTap;
begin
  with FBeat do begin
    if FBeatTime<BeatLength/2
      then FBeatTime:=0.0
      else FBeatTime:=BeatLength;
  end;
  FLastTap:=SyncTime;
end;

function TBeatSpectrumData.SyncBeatTOI2: Boolean;
var
  ALength,ASyncCount,I,J,AMax,L,APeakHistoryPos: Integer;
  ASync                                        : TBam;
  AShould,AIs,AMaxVal,AWFac,AWDec              : Real;
begin
  L:=Length(FPeakHistory2);
  ASyncCount:=L div FBeatFrames;
  ALength:=FBeatFrames; //um bei multi-threading mehrfachen zugriff und veränderte werte zu vermeiden... sollte in doinit() ausgelagert werden (ist wirklich WICHTIG auch wenns nicht so aussieht) //Round({ARLength}(60.0/FBPM)*SampleRate);
  FFSBeatBuffers.SSyncBuffer.Fill2(0.0);
  AWDec:=1/ASyncCount;

  AWFac:=1.0;

  APeakHistoryPos:=FPeakHistory2Pos;
  for I:=0 to ASyncCount-1 do begin
    ASync:=FFSBeatBuffers.SSyncBuffer.ToFirst;
    for J:=0 to ALength-1 do begin
      FFSBeatBuffers.SSyncBuffer.Bams[ASync]:=FFSBeatBuffers.SSyncBuffer.Bams[ASync]+(sqr(FPeakHistory2[APeakHistoryPos].Value)*AWFac);
      FFSBeatBuffers.SSyncBuffer.Next(ASync);
      Dec(APeakHistoryPos);
      if APeakHistoryPos<0 then APeakHistoryPos:=L-1;
    end;
    AWFac-=AWDec;
  end;

  //höchste Position suchen
  ASync:=FFSBeatBuffers.SSyncBuffer.ToFirst;
  AMaxVal:=0.0;
  AMax:=0;
  for I:=0 to ALength-1 do begin
    if FFSBeatBuffers.SSyncBuffer.Bams[ASync]>AMaxVal then begin
      AMax:=I;
      AMaxVal:=FFSBeatBuffers.SSyncBuffer.Bams[ASync];
    end;
    FFSBeatBuffers.SSyncBuffer.Next(ASync);
  end;

  {FSyncHistoryPos:=(FSyncHistoryPos+1) mod FSyncHistory.Count;
  FSyncHistory[FSyncHistoryPos]:=(AMax*WaveDataCount)/SampleRate;
  FSyncRelation:=(FSyncHistory[FSyncHistoryPos]/FBeat.BeatLength)-0.5;}
  FNewSyncOffset:=(AMax*WaveDataCount)/SampleRate;

  //Beat Offset errechnen
  {with FBeat do begin
    AIs:=RealMod(SyncTime,BeatLength);
    AShould:=AMax/SampleRate;
    BeatTime:=AShould-AIs;
  end;}
  Result:=true;
end;

procedure TBeatSpectrumData.DoGetBPM;
begin
  FDoGetBPM;
  AfterDetectBPM;
end;

procedure TBeatSpectrumData.DoGetBPM_TakeFirst;
var
  I,MaxPos,DPSPos{DPSMax,}{DPSTemp}: Integer;
  ABufVal,ALastBufVal,MaxBuf    : TSD2BufElem;
  //ABufPointer                   : Pointer;
  ABuf                          : TBam;//PSD2BufElem absolute ABufPointer;
  ACoreSyncBuf                  : TBam;
  ADummyInt                     : MVInt;
//const
  //ADPSFac = 0.75;

  {procedure FollowMontain(var SearchPos: Integer; const SearchMax,SearchMin: Integer; const BufferStart: TBam);
  var
    SearchPos1,SearchPos2: Integer;
    SearchVal1,SearchVal2: TSD2BufElem;
  begin
    ABuf:=BufferStart;
    SearchVal1:=FFSBeatBuffers.STakeFirstEquality.Bams[ABuf];
    SearchPos1:=SearchPos;
    while SearchPos1<SearchMax do begin
      //ABufPointer+=SD2BufElemSize;
      FFSBeatBuffers.STakeFirstEquality.Next(ABuf);
      if FFSBeatBuffers.STakeFirstEquality.Bams[ABuf]<SearchVal1 then break;
      Inc(SearchPos1);
    end;
    ABuf:=BufferStart;
    SearchVal2:=FFSBeatBuffers.STakeFirstEquality.Bams[ABuf];
    SearchPos2:=SearchPos;
    while SearchPos2>SearchMin do begin
      //ABufPointer-=SD2BufElemSize;
      FFSBeatBuffers.STakeFirstEquality.Prev(ABuf);
      if FFSBeatBuffers.STakeFirstEquality.Bams[ABuf]<SearchVal2 then break;
      Dec(SearchPos2);
    end;
    if SearchVal1>SearchVal2
      then SearchPos:=SearchPos1
      else SearchPos:=SearchPos2;
  end; }

begin
  //ABufPointer:=FFSBeatBuffers.STakeFirstEquality;
  with FFSBeatBuffers do begin
    STakeFirstHistory.RotateNext(STakeFirstHistoryPos,0);

    ABuf:=STakeFirstHistoryPos;//STakeFirstHistory.ToFirst;
    ACoreSyncBuf:=SCoreSyncBuffer.ToFirst;
    MaxBuf:=0.0;
    MaxPos:=SFirstBPM-1;
    ALastBufVal:=AutocorellateBeat(MaxPos,ADummyInt);
    for I:=SFirstBPM to SFirstBPM-1+FFSBeatBuffers.STakeFirstHistory.Count[1] do begin
      ABufVal:=AutocorellateBeat(I,MVInt(SCoreSyncBuffer.AdressOfBam[ACoreSyncBuf]^));
      if ABufVal<ALastBufVal
        then STakeFirstHistory.Bams[ABuf]:=0.0
        else if FDoTFsqr then STakeFirstHistory.Bams[ABuf]:=sqr((ABufVal)-(ALastBufVal)) else STakeFirstHistory.Bams[ABuf]:=ABufVal-ALastBufVal;

      if STakeFirstHistory.Bams[ABuf]>=MaxBuf then begin
        MaxBuf:=STakeFirstHistory.Bams[ABuf];
        MaxPos:=I;
      end;

      ALastBufVal:=ABufVal;
      //ABufPointer+=SD2BufElemSize;
      STakeFirstHistory.Next(ABuf);
      SCoreSyncBuffer.Next(ACoreSyncBuf);
    end;

    MaxBuf*=FTakeFirstDelta;
    FTakeFirstMax:=MaxBuf;
    //if not FDoDPS then begin
      //ABufPointer:=FFSBeatBuffers.STakeFirstEquality;
      ABuf:=STakeFirstHistoryPos;//STakeFirstHistory.ToFirst;
      MaxPos:=SFirstBPM;
      while STakeFirstHistory.Bams[ABuf]<MaxBuf do begin
        //ABufPointer+=SD2BufElemSize;
        STakeFirstHistory.Next(ABuf);
        Inc(MaxPos);
      end;
      MaxBuf:=STakeFirstHistory.Bams[ABuf];
    {end else  begin
      DPSPos:=Round((MaxPos+FMaxPosOffset)/2.0)-FMaxPosOffset;
      FDPSPos:=DPSPos-FFSBeatBuffers.SFirstBPM; (***)
      while DPSPos>=FFSBeatBuffers.SFirstBPM do begin
        //DPSMax:=MaxPos;
        DPSTemp:=Round(DPSPos/1.5);
        if DPSTemp<FFSBeatBuffers.SFirstBPM then DPSTemp:=FFSBeatBuffers.SFirstBPM;
        FollowMontain(DPSPos,DPSTemp,Round(DPSPos/0.75),FFSBeatBuffers.STakeFirstEquality+((DPSPos-FFSBeatBuffers.SFirstBPM)*SD2BufElemSize));
        ABufPointer:=FFSBeatBuffers.STakeFirstEquality+((DPSPos-FFSBeatBuffers.SFirstBPM)*SD2BufElemSize);
        if ABuf^<=MaxBuf then break;
        MaxPos:=DPSPos;
        DPSPos:=Round((DPSPos+FMaxPosOffset)/2.0)-FMaxPosOffset;
      end;
    end;}

    if FDoDPS and (MaxPos<SFirstBPM+STakeFirstHistory.Count[1]-1) then begin
      //ABufPointer:=FFSBeatBuffers.STakeFirstEquality+((MaxPos-FFSBeatBuffers.SFirstBPM+1)*SD2BufElemSize);
      ABuf:=STakeFirstHistory.ToItem([STakeFirstHistory.IndexOf(STakeFirstHistoryPos,0),MaxPos-FFSBeatBuffers.SFirstBPM+1]);
      //FFSBeatBuffers.STakeFirstHistory.ToCoord()

      DPSPos:=Trunc((MaxPos+FMaxPosOffset)/0.75)-FMaxPosOffset;
      if DPSPos>=SFirstBPM+FFSBeatBuffers.STakeFirstHistory.Count[1]
        then DPSPos:=SFirstBPM+FFSBeatBuffers.STakeFirstHistory.Count[1]-1
        else if DPSPos<=MaxPos then DPSPos:=MaxPos+1;   //= statt <= reicht auch...
      for I:=MaxPos+1 to DPSPos do begin
        if STakeFirstHistory.Bams[ABuf]>MaxBuf then begin
          MaxPos:=I;
          MaxBuf:=STakeFirstHistory.Bams[ABuf];
        end;
        //ABufPointer+=SD2BufElemSize;
        STakeFirstHistory.Next(ABuf);
      end;
      FDPSPos:=DPSPos-SFirstBPM;
    end;

    //FBPM:=FMakeBPM/(MaxPos+FMaxPosOffset);
    FBPM:=TakeFirstIndexToBPM(MaxPos+FMaxPosOffset-SFirstBPM);

    //Debugvariablen setzen
    //FTakeFirstMax:=MaxBuf;
    FTakeFirstMaxPos:=MaxPos-SFirstBPM;


    //sync
    if FSyncMode=sbAutoCor then begin
      FNewSyncOffset:=(SCoreSyncBuffer[FTakeFirstMaxPos]*FreqCount)/SampleRate;
      FSyncStartTime:=FBPMThread.StartTime;
      FExistsNewSync:=true;
    end;
  end;
end;

function TBeatSpectrumData.TakeFirstIndexToBPM(AIndex: MVInt): MVFloat;
begin
  Result:=FMakeBPM/(AIndex+FFSBeatBuffers.SFirstBPM);
end;

function TBeatSpectrumData.AutocorellateBeat(const X: Integer; var AHighestPos: MVInt): TSD2BufElem;
begin
  Result:=FDoCorellate(X,AHighestPos);
end;

function TBeatSpectrumData.AutocorellateBeat_Direct(const X: Integer; var AHighestPos: MVInt): TSD2BufElem;
var
  ASubband1,ASubband2: TBam;
  I,J                : Integer;
  AHighestVal,AVal   : Real;
begin
  Result:=0;
  AHighestVal:=0.0;
  AHighestPos:=0;
  for I:=0 to FFSBeatBuffers.SBeatCompareCount-1 do begin
    AVal:=0.0;
    with FFSBeatBuffers do begin
      ASubband1:=SSubbandHistory.ToItem(((FABeatBuf+FSoundHistory.Count-I) mod FSoundHistory.Count)*BeatAnaSubbandCount);
      ASubband2:=SSubbandHistory.ToItem(((FABeatBuf+FSoundHistory.Count-X-I) mod FSoundHistory.Count)*BeatAnaSubbandCount);
      for J:=0 to BeatAnaSubbandCount-1 do begin
        AVal+=SSubbandHistory.Bams[ASubband1]*SSubbandHistory.Bams[ASubband2];
        SSubbandHistory.Next(ASubband1);
        SSubbandHistory.Next(ASubband2);
      end;
    end;
    Result+=AVal;
    if (AVal>AHighestVal) and (I<=X) then begin
      AHighestVal:=AVal;
      AHighestPos:=I;
    end;
  end;
end;

procedure TBeatSpectrumData.AfterDetectBPM;
var
  AHistoryBuf: TBam;
begin
  if (FBPM<10.0) or (FBPM>1500.0)
    then FBPM:=0.0
    else FBPM:=NormalizeBPM(FBPM).BPM; (***)
  FFSBeatBuffers.SLastBPM:=FBPM;
  FBPMHistoryPos:=(FBPMHistoryPos+1) mod FBPMHistory.Count;
  AHistoryBuf:=FBPMHistory.ToItem(FBPMHistoryPos);

  FBPMThread.FBPMHistoryList.DeleteBPM(FBPMHistory.Bams[AHistoryBuf],1);
  FBPMHistory.Bams[AHistoryBuf]:=FBPM;
  FBPMThread.FBPMHistoryList.AddBPM(FBPM{FBPMHistory.Bams[AHistoryBuf]},1);

  if FBeatOptimizerH then FBPM:=FBPMThread.FBPMHistoryList.FindBPM(FBPMThread.FramesNeeded);

  FBPMFound:=TBPMFoundMode((FBPM>10.0) and (FBPM<1500.0));
  FBPM*=FBPMFac;
  if IsZero(FBPM)
    then FBeat.BeatLength:=1.0
    else FBeat.BeatLength:=60.0/FBPM{<> 1/(FBPM/ABPMFac)};
  FBeatSamples:=Round(FBeat.BeatLength*SampleRate);
  FBeatFrames:=Round(FBeat.BeatLength*(SampleRate/WaveDataCount));
  FBeatFFTFrames:=FBeat.BeatLength*(SampleRate/FreqCount);
  //FBeatSinFac:=GetPeakFreq(FBeat.BeatLength*SampleRate);
  FBeatFreq:=FBeat.BeatLength*SampleRate;
end;

procedure TBeatSpectrumData.DoGetBeatFS; inline;
var
  I                                    : Integer;
  ASubband                             : MVFloat;
  AAverageEnergy                       : TBam;
  AInstantEnergy                       : TBam;
  ATempEnergy                          : TBam;
  ABeat600Block,OldBeat600Block        : TBeat600Block;

  function GetSubband(const I: Integer): MVFloat;
  var
    OldJ: Integer;
  begin
    Result:=0.0;
    for OldJ:=FFSBeatBuffers.SFirstBands[I] to FFSBeatBuffers.SLastBands[I] do Result+=sqr(Levels[OldJ,0]);
  end;

begin
  with FFSBeatBuffers do begin
    AAverageEnergy:=SAverageSoundEnergys.ToFirst;
    AInstantEnergy:=SSubbandHistory.ToItem(FABeatBuf*BeatAnaSubbandCount);
    ATempEnergy:=STestSubbandHistory.ToItem(FABeatBuf*BeatAnaSubbandCount);

    SBeatAtPos:=(SBeatAtPos+1) mod SBeatAtHistory.Count;
    SIsBeatAt:=SBeatAtHistory.ToItem(SBeatAtPos);

    SBI_Reset(TSubbandBitsItem(FFSBeatBuffers.SBeatAtHistory.AdressOfBam[SIsBeatAt]^));
    SBeatAtPos600:=(SBeatAtPos600+1) mod SBeatAtHistory600.Count;
    SIsBeatAt600:=SBeatAtHistory600.ToItem(SBeatAtPos600);

    SBI_Reset(TSubbandBitsItem(SBeatAtHistory600.AdressOfBam[SIsBeatAt600]^));
  end;

  FAOffset:=(FAOffset+1) mod FBeat600ConCount;
  if FAOffset=0 then FBeat600Blocks.Circulate;
  FBeat600Blocks.ToFirst;

  for I:=0 to BeatAnaSubbandCount-1 do begin
    ASubband:=GetSubband(I);
    if ASubband>FFSBeatBuffers.SAverageSoundEnergys.Bams[AAverageEnergy]*FFSSensibility then SBI_Set(TSubbandBitsItem(FFSBeatBuffers.SBeatAtHistory.AdressOfBam[FFSBeatBuffers.SIsBeatAt]^),I); //SD2Bits_Set(FFSBeatBuffers.SIsBeatAt,I);

    FFSBeatBuffers.SAverageSoundEnergys.Bams[AAverageEnergy]:=FFSBeatBuffers.SAverageSoundEnergys.Bams[AAverageEnergy]+(((ASubband/FSoundHistory.Count)-(FFSBeatBuffers.SSubbandHistory.Bams[AInstantEnergy]/FSoundHistory.Count)));
    FFSBeatBuffers.SSubbandHistory.Bams[AInstantEnergy]:=ASubband;

    if FAOffset=0 then begin
      //validieren des alten Blocks
      FBeat600Blocks.PrevOuter;
      FBeat600Blocks.GetItem(ABeat600Block);
      FBeat600Blocks.PrevOuter;
      FBeat600Blocks.GetItem(OldBeat600Block);
      ABeat600Block.Valid:=(ABeat600Block.Value>OldBeat600Block.Value);
      if OldBeat600Block.Valid and ABeat600Block.Valid then begin
        OldBeat600Block.Valid:=false;
        FBeat600Blocks.SetItem(OldBeat600Block);
      end;
      FBeat600Blocks.NextOuter;
      FBeat600Blocks.SetItem(ABeat600Block);
      FBeat600Blocks.NextOuter;
      //Beat 600
      with ABeat600Block do begin
        Value:=ASubband;
        Offset:=0;
        Time:=1;
      end;
      FBeat600Blocks.SetItem(ABeat600Block);
      //ATempEnergy^:=ABeat600Block.Value;
      FFSBeatBuffers.STestSubbandHistory.Bams[ATempEnergy]:=ABeat600Block.Value;
    end else begin
      FBeat600Blocks.GetItem(ABeat600Block);
      if ASubband>ABeat600Block.Value then begin;
        with ABeat600Block do begin
          Value:=ASubband;
          Offset:=FAOffset;
        end;
        FBeat600Blocks.SetItem(ABeat600Block);
      end;

      Inc(ABeat600Block.Time);
      FBeat600Blocks.SetItem(ABeat600Block);

      FFSBeatBuffers.STestSubbandHistory.Bams[ATempEnergy]:=ABeat600Block.Value;
    end;

    FBeat600Blocks.PrevOuter;
    FBeat600Blocks.PrevOuter;

    FBeat600Blocks.GetItem(ABeat600Block);
    if ABeat600Block.Valid and (ABeat600Block.Offset=FAOffset) then SBI_Set(TSubbandBitsItem(FFSBeatBuffers.SBeatAtHistory600.AdressOfBam[FFSBeatBuffers.SIsBeatAt600]^),I);//SD2Bits_Set(FFSBeatBuffers.SIsBeatAt600,I);

    FBeat600Blocks.NextOuter;
    FBeat600Blocks.NextOuter;

    FBeat600Blocks.NextInner;


    FFSBeatBuffers.SAverageSoundEnergys.Next(AAverageEnergy);
    FFSBeatBuffers.SSubbandHistory.Next(AInstantEnergy);
    FFSBeatBuffers.STestSubbandHistory.Next(ATempEnergy);
  end;
end;

{procedure TBeatSpectrumData.BeforeInit;
begin
  inherited BeforeInit;
  {$IFDEF DST}
  {$IFDEF EAT}
  FSyncThread.WaitForFrame;
  FSyncThread.Suspend;
  FBPMThread.WaitForFrame;
  FBPMThread.Suspend;
  {$ENDIF}
  {$ENDIF}
end;}

procedure TBeatSpectrumData.Run;
begin
  {$IFDEF EAT}
  FBPMThread.Resume;
  FSyncThread.Resume;
  {$ENDIF}
end;

procedure TBeatSpectrumData.Stop;
begin
  {$IFDEF EAT}
  FSyncThread.WaitForFrame;
  FSyncThread.Suspend;
  FBPMThread.WaitForFrame;
  FBPMThread.Suspend;
  {$ENDIF}
end;

procedure TBeatSpectrumData.Launched(var ACounts: array of MVInt);
begin
  inherited Launched(ACounts);
  ACounts[SyncBufferSaverBI]:=ACounts[SyncBufferBI]+(FSaveSyncCount*InputBufferSampleCount);
  FFSBeatBuffers.SSyncLength:=FSyncSeconds*SampleRate;
  ACounts[TOISyncBufferBI]:=FFSBeatBuffers.SSyncLength;
end;

procedure TBeatSpectrumData.InitCalcBeat; inline;
begin
  FBeat600ConCount:=Trunc(SampleRate/(InputBufferSampleCount*10.0))+1;
  FBeat600Average:=0.0;
  FAOffset:=0;
  FMakeBPM:=(SampleRate*60)/InputBufferSampleCount;
  FFSBeatBuffers.SFirstBPM:=Trunc(FMakeBPM/maxBPM);
end;

procedure TBeatSpectrumData.DoInit;
var
  ATempSize: MVInt;
begin
  inherited DoInit;
  {$IFDEF DST}
  FNormBeatDiff:=0.0;
  FSoundEnergyAverage:=0.0;
  FABeatBuf:=0;
  FBPMHistoryPos:=0;
  with FFSBeatBuffers do begin
    SBeatAtHistory.Resize(FNewBeatHistoryCount);
    SBeatAtHistory2.Resize(FNewBeatHistoryCount);
    SBeatAtHistory600.Resize(FNewBeatHistoryCount);
    SIsBeatAt:=SBeatAtHistory.ToFirst;
    SIsBeatAt600:=SBeatAtHistory600.ToFirst;
    SBeatAtPos:=0;
    SBeatAtPos2:=0;
    SBeatAtPos600:=0;
    SBeatCompareCount:=FNewBeatCompareCount;
    STestSubbandHistory.Resize(FNewBeatBufCount*BeatAnaSubbandCount);
    SAverageSoundEnergys.Resize(BeatAnaSubbandCount); (***)
    //SBeatEquality.Resize(FNewBeatHistoryCount-FNewBeatCompareCount);

    CalcBandWidths;

    SSubbandHistory.Resize(FNewBeatBufCount*BeatAnaSubbandCount);
    SSubbandHistory.Fill2(0.0);
    STestSubbandHistory.Fill2(0.0);
    SAverageSoundEnergys.Fill2(0.0);
  end;

  {$IFDEF EAT}
  FBPMThread.FBPMList.Size:=Trunc(sqrt(0.25+(2*FNewBeatHistoryCount))-0.5)+10; //berechnet, wie viele Zahlen summiert werden müssen, um auf ABeatHistoryCount zu kommen (so viele verschiedene Beats gibt es maximal)
  FBPMThread.FBPMHistoryList.Size:=FNewBPMHistoryCount{FBPMHistory.Count}+10;
  {$ENDIF}

  InitCalcBeat;

  with FFSBeatBuffers do begin
    SSyncBuffer.Resize(Trunc(minBeatLength*SampleRate)+1);
    ATempSize:=Trunc(FMakeBPM/minBPM)-FFSBeatBuffers.SFirstBPM+1;
    STakeFirstHistory.Resize([Round((SampleRate/FreqCount)*FBeatHistoryTime),ATempSize]);
    STakeFirstHistory.Fill2(Nan);
    STakeFirstHistoryPos:=STakeFirstHistory.ToFirst;
    //STakeFirstEquality.Resize(ATempSize);
    SCoreSyncBuffer.Resize(ATempSize);
  end;
  SetLength(FPeakHistory,FNewBeatBufCount);
  SetLength(FPeakHistory2,FNewBeatBufCount);
  FPeakHistoryPos:=0;
  FPeakHistory2Pos:=0;
  FBeat600Blocks.SetLength((FNewBeatBufCount div FBeat600ConCount)+1,BeatAnaSubbandCount);

  FSoundHistory.Resize(FNewBeatBufCount);
  FBPMHistory.Resize(FNewBPMHistoryCount);
  FBPMHistory.Fill2(0.0);
  FSoundHistory.Fill2(0.0);

  {{$IFDEF EAT}
  FBPMThread.Resume;
  FSyncThread.Resume;
  {$ENDIF} }
  FBeatRecognized:=true;
  {$ENDIF}
end;

procedure TBeatSpectrumData.SetCounts(const AWaveDataCount,ALongWaveDataCount,AFreqCount,AFFTHistoryCount,ABeatBufCount{,ABeatSubbandCount},ABeatHistoryCount,ABeatCompareCount,ABPMHistoryCount,ASyncBufferCount,ASaveSyncCount: Cardinal; ABeatHistoryTime: MVFloat);
begin
  FNewBeatBufCount:=ABeatBufCount;
  FNewBeatHistoryCount:=ABeatHistoryCount;
  FNewBeatCompareCount:=ABeatCompareCount;
  FNewBPMHistoryCount:=ABPMHistoryCount;
  FSyncBufferCount:=ASyncBufferCount;
  FSaveSyncCount:=ASaveSyncCount;
  FBeatHistoryTime:=ABeatHistoryTime;
  inherited SetCounts(AWaveDataCount,ALongWaveDataCount,AFreqCount,AFFTHistoryCount,[ASyncBufferCount,0,0(*wird in Launched() gesetzt*)]);
end;

{procedure TBeatSpectrumData.SetBeatIdleTime(const Value: MVInt);
begin
  FBPMThread.FSleepTime:=Value;
end;}

{function TBeatSpectrumData.GetBeatIdleTime: MVInt;
begin
  Result:=FBPMThread.FSleepTime;
end;}

function TBeatSpectrumData.GetBeatBufCount: MVInt; stdcall;
begin
  Result:=FSoundHistory.Count;
end;

function TBeatSpectrumData.GetSoundHistory(const Index: MVInt): MVFloat; stdcall;
begin
   Result:=FSoundHistory[(FABeatBuf+Index+1) mod FSoundHistory.Count];
end;

function TBeatSpectrumData.GetBeatAtH(const Subband,History: MVInt): Boolean; stdcall;
begin
  with FFSBeatBuffers do Result:=SBI_Get(SBeatAtHistory[(SBeatAtPos+1+History) mod SBeatAtHistory.Count],Subband);
end;

function TBeatSpectrumData.GetBeatAt(const Subband: MVInt): Boolean; stdcall;
begin
  with FFSBeatBuffers do Result:=SBI_Get(SBeatAtHistory.Bams[SIsBeatAt],Subband);
end;

function TBeatSpectrumData.GetBeatAtH600(const Subband,History: MVInt): Boolean; stdcall;
begin
  with FFSBeatBuffers do Result:=SBI_Get(SBeatAtHistory600[(SBeatAtPos600+1+History) mod SBeatAtHistory600.Count],Subband);
end;

function TBeatSpectrumData.GetBeatAt600(const Subband: MVInt): Boolean; stdcall;
begin
  with FFSBeatBuffers do Result:=SBI_Get(SBeatAtHistory600.Bams[SIsBeatAt600],Subband);
end;

function TBeatSpectrumData.GetBeatState: MVBeatState; stdcall;
begin
  Result:=FBeat;
end;

function TBeatSpectrumData.GetBeatPos: MVFloat; stdcall;
begin
  Result:=FBeat.Pos+FBeatSubPos;
  //if FSyncRelation<0.0 then Result+=1.0;
end;

function TBeatSpectrumData.GetBPM: MVFloat; stdcall;
begin
  Result:=FBPM;
end;

function TBeatSpectrumData.GetBPMFound: TBPMFoundMode; stdcall;
begin
  Result:=FBPMFound;
end;

function TBeatSpectrumData.GetTakeFirstHistoryPos: TBam;
begin
  Result:=FFSBeatBuffers.STakeFirstHistoryPos;
end;

function TBeatSpectrumData.GetTakeFirstHistory: TMVFloatBufferXD;
begin
  Result:=FFSBeatBuffers.STakeFirstHistory;
end;

function TBeatSpectrumData.GetFSAverage(const Subband: MVInt): MVFloat; stdcall;
begin
  FFSBeatBuffers.SAverageSoundEnergys[Subband];
end;

function TBeatSpectrumData.GetFSHistory(const Index,Subband: MVInt): MVFloat; stdcall;
begin
  Result:=FFSBeatBuffers.SSubbandHistory[((FABeatBuf+Index+1) mod FSoundHistory.Count)*BeatAnaSubbandCount];
end;

function TBeatSpectrumData.GetFSTHistory(const Index,Subband: MVInt): MVFloat; stdcall;
begin
  with FFSBeatBuffers do Result:=STestSubbandHistory[((FABeatBuf+Index+1) mod STestSubbandHistory.Count)*BeatAnaSubbandCount];
end;

function TBeatSpectrumData.GetSyncHistory(const Index: MVInt): MVFloat; stdcall;
begin
  Result:=FFSBeatBuffers.SSyncBuffer[Index];
end;

function TBeatSpectrumData.GetBeat600Block(const Index,Subband: MVInt): TBeat600Block; stdcall;
begin
  FBeat600Blocks.GetItem(Index,Subband,Result);
end;

function TBeatSpectrumData.GetBandWidths(const Subband: MVInt): MVInt;
begin
  with FFSBeatBuffers do Result:=SLastBands[Subband]-SFirstBands[Subband]+1;
end;

function TBeatSpectrumData.GetFirstBands(const Subband: MVInt): MVInt;
begin
  Result:=FFSBeatBuffers.SFirstBands[Subband];
end;

function TBeatSpectrumData.GetLastBands(const Subband: MVInt): MVInt;
begin
  Result:=FFSBeatBuffers.SLastBands[Subband];
end;

function TBeatSpectrumData.GetTakeFirstCount: MVInt;
begin
  Result:=FFSBeatBuffers.STakeFirstHistory.Count[1];
end;

function TBeatSpectrumData.GetTakeFirstEquality(const Index: MVInt): MVFloat;
begin
  with FFSBeatBuffers do Result:=STakeFirstHistory.GetItem([STakeFirstHistory.IndexOf(STakeFirstHistoryPos,0),Index]);
end;

function TBeatSpectrumData.GetBeat600BlockCount: MVInt; stdcall;
begin
  Result:=FBeat600Blocks.OuterLength;
end;

function TBeatSpectrumData.GetBeatHistoryCount: MVInt;
begin
  Result:=FFSBeatBuffers.SBeatAtHistory.Count;
end;

{function TBeatSpectrumData.GetBeatEqualityCount: MVInt;
begin
  Result:=FFSBeatBuffers.SBeatEquality.Count;
end;}

function TBeatSpectrumData.GetBeatCompareCount: MVInt;
begin
  Result:=FFSBeatBuffers.SBeatCompareCount;
end;

function TBeatSpectrumData.GetBPMHistoryCount: MVInt;
begin
  Result:=FBPMHistory.Count;
end;

function TBeatSpectrumData.GetSyncEnabled: Boolean;
begin
  Result:=FSyncEnabled;
end;

procedure TBeatSpectrumData.SetSyncEnabled(Value: Boolean);
begin
  if Value=FSyncEnabled then exit;
  {$IFDEF EAT}
  if InputStarted then if Value
    then FSyncThread.Resume
    else FSyncThread.Suspend;
  {$ENDIF}
  FSyncEnabled:=Value;
end;

function TBeatSpectrumData.GetBPMEnabled: Boolean;
begin
  Result:=FBPMEnabled;
end;

procedure TBeatSpectrumData.SetBPMEnabled(Value: Boolean);
begin
  if Value=FBPMEnabled then exit;
  {$IFDEF EAT}
  if InputStarted then if Value
    then FBPMThread.Resume
    else FBPMThread.Suspend;
  {$ENDIF}
  FBPMEnabled:=Value;
end;

procedure TBeatSpectrumData.SetSyncMode(Value: TBeatSyncMode);
begin
  FSyncMode:=Value;
  case Value of
    sbSin    : FDoSync:=@SyncBeatSin;
    sbTOI    : FDoSync:=@SyncBeatTOI2;
    sbMPF    : FDoSync:=@SyncBeatMPF;
    sbAutoCor: FDoSync:=@SyncBeatNot;
  end;
end;

{function TBeatSpectrumData.GetBeatEquality(const History: MVInt): MVFloat;
{var
  ABufPointer: Pointer;
  ABuf       : PSD2BufferFormat absolute ABufPointer;}
begin
  {ABufPointer:=FFSBeatBuffers.SBeatEquality;
  Result:=ABuf^[History];}
  Result:=FFSBeatBuffers.SBeatEquality[History];
end;}

function TBeatSpectrumData.GetAverageBeatEquality: MVFloat;
begin
  Result:=FFSBeatBuffers.SAverageBeatEquality;
end;

{function TBeatSpectrumData.GetBPMAnalysisTime: MVInt; stdcall;
begin
  Result:=FBPMThread.FFramesNeeded;
end;}

{function TBeatSpectrumData.GetSyncFrames: MVInt; stdcall;
begin
  Result:=FSyncThread.FFramesNeeded; (***)
  //Result:=0;
end;}

function TBeatSpectrumData.GetSyncBufferCount: MVInt; stdcall;
begin
  Result:=FSyncBufferCount;
end;

{TAnalysingThread}

const
  anaCalc  = 0;
  anaReady = 1;
  anaWait  = 2;

constructor TAnalysingThread.Create(AOwner: TBeatSpectrumData{; const CreateSuspended: Boolean = false});
begin
  inherited Create(true);
  //FSleepTime:=0;
  FSpectrumData:=AOwner;
  FReady:=anaReady;
  FreeOnTerminate:=false;
  FFramesNeeded:=1;
  FFramesUsed:=1;
  FTimeNeeded:=0.0;
  FTimeUsed:=0.0;
  FFullTimeNeeded:=0.0;
  FExeTime:=0.3333; //3 FPS...
  //if not CreateSuspended then Resume;
end;

destructor TAnalysingThread.Destroy;
begin
  {Terminate;
  WaitFor;}
  FSpectrumData:=nil;
  inherited Destroy;
end;

procedure TAnalysingThread.Execute;
var
  ASleepTime: Integer;
begin
  while not Terminated do begin
    if FReady<>anaReady then begin
      DoExecute;
      FFramesNeeded:=FFramesUsed;
      FTimeNeeded:=FTimeUsed;
      FReady:=anaWait;
      if not Terminated then begin
        ASleepTime:=Round((FExeTime-FTimeNeeded)*1000.0);
        if ASleepTime>0 then Sleep(ASleepTime);
      end;
      FFullTimeNeeded:=FTimeUsed;
      FReady:=anaReady;
    end;
  end;
end;

procedure TAnalysingThread.NextFrame(ATime: MVFloat);
begin
  if FReady=anaReady then begin
    FStartTime:=ATime;
    FTimeUsed:=0.0;
    DoInit;
    FFramesUsed:=1;
    FReady:=anaCalc;
  end else begin
    FTimeUsed:=ATime-FStartTime;
    Inc(FFramesUsed);
  end;
end;

procedure TAnalysingThread.BlockingNextFrame;
begin
  DoInit;
  DoExecute;
end;

procedure TAnalysingThread.WaitForFrame;
begin
  while FReady=anaCalc do ;
end;

function TAnalysingThread.GetFPS: MVFloat;
begin
  if IsZero(FExeTime)
    then Result:=Infinity
    else Result:=1.0/FExeTime;
end;

procedure TAnalysingThread.SetFPS(Value: MVFloat);
begin
  if IsInfinite(Value)
    then FExeTime:=0.0
    else FExeTime:=1.0/Value;
end;

function TAnalysingThread.GetMaxFPS: MVFloat;
begin
  if IsZero(FTimeNeeded)
    then Result:=Infinity
    else Result:=1.0/FTimeNeeded;
end;

function TAnalysingThread.GetActiveFPS: MVFloat;
begin
  if IsZero(FFullTimeNeeded)
    then Result:=Infinity
    else Result:=1.0/FFullTimeNeeded;
end;

{TMAnalysingThread}

constructor TMAnalysingThread.Create(AOwner: TBeatSpectrumData; AOnExecute: TEvent{; const CreateSuspended: Boolean = false});
begin
  inherited Create(AOwner{,true});
  FOnExecute:=AOnExecute;
  //if not CreateSuspended then Resume;
end;

procedure TMAnalysingThread.DoInit;
begin
  //do nothing
end;

procedure TMAnalysingThread.DoExecute;
begin
  FOnExecute;
end;

{TBPMThread}

{const
  bpmCalc  = 0;
  bpmReady = 1;
  bpmWait  = 2;}

constructor TBPMThread.Create(AOwner: TBeatSpectrumData);
begin
  inherited Create({true}AOwner,@AOwner.DoGetBPM{,true});
  FBPMHistoryList:=TUltraSortList.Create;
  FBPMList:=TUltraSortList.Create;
  //Resume;
  {FSleepTime:=0;
  FSpectrumData:=AOwner;
  FReady:=bpmReady;
  FreeOnTerminate:=false;
  FFramesNeeded:=1;
  FFramesUsed:=1;
  Resume;}
end;

destructor TBPMThread.Destroy;
begin
  {Terminate;
  WaitFor;}
  FBPMList.Destroy;
  FBPMHistoryList.Destroy;
  inherited Destroy;
end;

{procedure TBPMThread.DoExecute;
begin
  while not Terminated do begin
    if FReady<>bpmReady then begin
      FSpectrumData.DoGetBPM;
      FFramesNeeded:=FFramesUsed;
      FReady:=bpmWait;
      Sleep(FSleepTime);
      FReady:=bpmReady;
    end;
  end;
end;}

procedure TBPMThread.DoInit{NextFrame};

  procedure CopyBeatHistory; inline;
  var
    I: Integer;
  begin
    with FSpectrumData.FFSBeatBuffers do begin
      for I:=0 to FFramesUsed-1 do begin
        SBeatAtPos2:=(SBeatAtPos2+1) mod SBeatAtHistory2.Count;
        SBeatAtHistory2[SBeatAtPos2]:=SBeatAtHistory[SBeatAtPos2];
        //CopySubbandBits(SBeatAtHistory,ABeatAtHistory,SBeatAtPos2,SBeatAtPos2,);
        //SD2Bits_Copy(SBeatAtHistory+(SBeatAtSize*SBeatAtPos2),SBeatAtHistory2+(SBeatAtSize*SBeatAtPos2),SBeatAtCount);
      end;
    end;
  end;

  procedure CopyBeatHistory600; inline;
  var
    I: Integer;
  begin
    with FSpectrumData.FFSBeatBuffers do begin
      for I:=0 to FFramesUsed-1 do begin
        SBeatAtPos2:=(SBeatAtPos2+1) mod SBeatAtHistory2.Count;
        SBeatAtHistory2[SBeatAtPos2]:=SBeatAtHistory600[SBeatAtPos2];
        //SD2Bits_Copy(SBeatAtHistory600+(SBeatAtSize*SBeatAtPos2),SBeatAtHistory2+(SBeatAtSize*SBeatAtPos2),SBeatAtCount);
      end;
    end;
  end;

begin
  {if FReady=bpmReady then begin}
  if FSpectrumData.FUseBeat600
    then CopyBeatHistory600
    else CopyBeatHistory;

    //if Self.FSleepTime<>0 then Meldung('<>0',TdMInfo);
    {FFramesUsed:=1;
    FReady:=bpmCalc;
  end else Inc(FFramesUsed);}
end;

{procedure TBPMThread.WaitForFrame;
begin
  while FReady=bpmCalc do ;
end;}

{TSyncThread}

constructor TSyncThread.Create(AOwner: TBeatSpectrumData);
begin
  inherited Create(AOwner,@AOwner.DoSync{,true});
  //FIs:=0.0;
  //FSleepTime:=000;
  //Resume;
end;

procedure TSyncThread.DoInit;
var
  L: Integer;
begin
  with FSpectrumData do begin
    L:=Length(FPeakHistory);
    while FPeakHistory2Pos<>FPeakHistoryPos do begin
      FPeakHistory2Pos:=(FPeakHistory2Pos+1) mod L;
      FPeakHistory2[FPeakHistory2Pos]:=FPeakHistory[FPeakHistory2Pos];
    end;
    //FSyncStartTime:=SyncTime;
  end;
  {with SpectrumData do with Beat do begin
    //FIs:=Frac(SyncTime/BeatLength)*BeatLength;
  end;}
end;

{TBeatSynchronizer}

{constructor TBeatSynchronizer.Create(AOwner: TBeatSpectrumData);
begin
  inherited Create;
  FOwner:=AOwner;
  FSyncHistorySize:=0;
end;

destructor TBeatSynchronizer.Destroy;
begin
  inherited Destroy;
end;

procedure TBeatSynchronizer.Init(const AAnalyseFreq: Cardinal; const ASyncSeconds: Real);
const
  pi2div60 = pi/30;
begin
  with FOwner do begin
    FSinFac:=(pi2div60*BPM)/SampleRate {=(2*pi)/((60*FOwner.SampleRate)/FOwner.BPM)};
    FSinLength:=(60*SampleRate)/BPM;
  end;
  FSinStart:=0;
  FSin:=0;
  FCos:=0;
  FSyncSeconds:=ASyncSeconds;
  FAnalyseFreq:=AAnalyseFreq;
  FRequiredWBufferCount:=FOwner.InputBufferSampleCount*AAnalyseFreq;

  if FSyncHistorySize>0 then FreeMem(FSyncHistory,FSyncHistorySize);
  FSyncHistoryCount:=Round((ASyncSeconds*FOwner.SampleRate)/FRequiredWBufferCount){=ASyncSeconds/(FOwner.InputBufferSampleCount/FOwner.SampleRate)};
  FSyncHistorySize:=FSyncHistoryCount*2*SD2BufElemSize;
  GetMem(FSyncHistory,FSyncHistorySize);
  ZeroMVFloat(FSyncHistory,FSyncHistoryCount*2);
  FASin:=FSyncHistory;
  FACos:=FSyncHistory+SD2BufElemSize;
  FSyncHistoryPos
end;

procedure TBeatSynchronizer.Execute(const AWData);
var
  AWData2  : TSD2BufferFormat absolute AWData;
  I        : Integer;
  ACos,ASin: Real;
begin
  ASin:=0.0;
  ACos:=0.0;
  for I:=FSinStart to FRequiredWBufferCount+FSinStart-1 do begin
    ASin+=sin(I*FSinFac);
    ACos+=cos(I*FSinFac);
  end;
  ASin/=FRequiredWBufferCount;
  ACos/=FRequiredWBufferCount;
  //Puffern... noch machen

  FSin+=ASin;
  FCos+=ACos;

  FOwner.Beat.BeatTime:=Arctan2(FSin,FCos)/FSinFac;
end; }

{TSpectrumDataImplementor}

(*constructor TSpectrumDataImplementor.Create(ABufferManager: IVPBufferManager);
begin
  inherited Create(ABufferManager);
end;

destructor TSpectrumDataImplementor.Destroy;
begin
  inherited Destroy;
end;

procedure TSpectrumDataImplementor.SetCounts(const {AChannels,}AWaveDataCount,AFreqCount,ABufferCount,ABeatBufCount,ABB4Count,ABeatIterCount,{ABeatSubbandCount,}ABeatHistoryCount,ABeatCompareCount,ABPMHistoryCount,ASyncBufferCount,ASaveSyncCount: Cardinal);
begin
  inherited SetCounts(AWaveDataCount,AWaveDataCount*ABufferCount,AFreqCount,ABeatBufCount,{ABeatSubbandCount,}ABeatHistoryCount,ABeatCompareCount,ABPMHistoryCount,ASyncBufferCount,ASaveSyncCount);
end;

{procedure TSpectrumDataImplementor.DoAnalyse;
begin
  inherited DoAnalyse;
end;

procedure TSpectrumDataImplementor.DoInit;
begin
  inherited DoInit;
end;}

function TSpectrumDataImplementor.GetVersion: MVVersion; stdcall;
begin
  Result:=inherited GetVersion;
  Inc(Result.MainVersion);
end;

{function TSpectrumDataImplementor.Future: IBasicSpectrumData; stdcall;
begin
  Result:=IBasicSpectrumData(ISpectrumData2(Self));
end;}

function TSpectrumDataImplementor.GetBufferCount: MVInt; stdcall;
begin
  if WaveDataCount>0
    then Result:=LongWaveDataCount div WaveDataCount
    else Result:=0;
end;

function TSpectrumDataImplementor.GetFFTMode: TFFTMode; stdcall;
begin
  Result:=fftFirst;
end;

function TSpectrumDataImplementor.GetAnalyseMode: TSDAnalyseMode; stdcall;
begin
  Result:=[afFFT,afPeak];
end;

function TSpectrumDataImplementor.GetLevelsC(const Index: MVInt): MVFloat; stdcall;
begin
  Result:=Levels[Index,0];
end;

function TSpectrumDataImplementor.GetWaveDataC(const Index: MVInt): MVFloat; stdcall;
begin
  Result:=WaveData[Index,0];
end;

function TSpectrumDataImplementor.GetWaveDataLongB(const Index,Channel,Buffer: MVInt): MVFloat; stdcall;
begin
  Result:=WaveDataLong[Index,Channel];
end;

function TSpectrumDataImplementor.GetWaveDataLongC(const Index: MVInt): MVFloat; stdcall;
begin
  Result:=WaveDataLong[Index,0];
end;

function TSpectrumDataImplementor.GetWaveDataLongBC(const Index,Buffer: MVInt): MVFloat; stdcall;
begin
  Result:=WaveDataLong[Index,0];
end;*)

{Allgemein}

//Gibt die Anzahl Elemente aus ASignal zurück, die seit dem letzten Peak vergangen sind (Result.T3)
function SignalPeakOffset(const ASignal; const ASize: Cardinal; APeakFreq: Real): Real;
const
  pi2 = 2*pi;
var
  ASignal2                : TSD2BufferFormat absolute ASignal;
  I,J,AMin                : Integer;
  ACos,ASin,ASinFac,B,ASig: Real;

begin
  //Init
  ASinFac:=pi2/APeakFreq;
  B:=Trunc(ASize/APeakFreq);
  //Run
  ASin:=0.0;
  ACos:=0.0;
  AMin:=Trunc(ASize-(B*APeakFreq));
  for I:=ASize-1 downto AMin do begin
    J:=ASize-I;
    ASig:=Abs(ASignal2[I]);
    ASin+=ASig*sin(J*ASinFac);
    ACos+=ASig*cos(J*ASinFac);
  end;
  ASin/=ASize+1;
  ACos/=ASize+1;
  Result:=Arctan2(ASin,ACos)/ASinFac;

  if Result<0.0 then Result:=APeakFreq+Result;
end;

function SignalPeakOffsetF(ASignal: TSignalFunc; const ASize: Cardinal; APeakFreq: Real): Real;
const
  pi2 = 2*pi;
var
  //ASignal2                : TSD2BufferFormat absolute ASignal;
  I,J,AMin                : Integer;
  ACos,ASin,ASinFac,B,ASig: Real;

begin
  //Init
  ASinFac:=pi2/APeakFreq;
  B:=Trunc(ASize/APeakFreq);
  //Run
  ASin:=0.0;
  ACos:=0.0;
  AMin:=Trunc(ASize-(B*APeakFreq));
  for I:=ASize-1 downto AMin do begin
    J:=ASize-I;
    ASig:=Abs(ASignal(I));
    ASin+=ASig*sin(J*ASinFac);
    ACos+=ASig*cos(J*ASinFac);
  end;
  ASin/=ASize+1;
  ACos/=ASize+1;
  Result:=Arctan2(ASin,ACos)/ASinFac;

  if Result<0.0 then Result:=APeakFreq+Result;
end;

{function SignalPeakOffset2(const ASignal; const ASize: Cardinal; APeakFreq: Real; AAverageBuffer: TMVFloatBuffer): TBeatSyncInfo;
const
  pi2 = 2*pi;
var
  ASignal2                : TSD2BufferFormat absolute ASignal;
  I,J,AMin                : Integer;
  ACos,ASin,ASinFac,B,ASig: Real;
  Result2                 : TBeatSyncInfo;

  function fp(const x: integer): Real; inline;
  begin
    Result:=Round(B-(X/APeakFreq)+0.5)/B;
  end;

begin
  //Init
  Result2.Time:=GetTickCount;
  ASinFac:=pi2/APeakFreq;
  B:=Trunc(ASize/APeakFreq);
  //Run
  ASin:=0.0;
  ACos:=0.0;
  AMin:=Trunc(ASize-(B*APeakFreq));
  J:=0;
  {for I:=0 to B-1 do begin
    ASig:=Abs(ASignal2[I]);
    AAverage
    Inc(J);
  end;}
  for I:=ASize-1 downto AMin do begin
    J:=ASize-I;
    ASig:=Abs{sqr}(ASignal2[I]);
    ASin+=ASig*sin(J*ASinFac);
    ACos+=ASig*cos(J*ASinFac);
  end;
  ASin/=ASize+1;
  ACos/=ASize+1;
  Result2.T3:=Arctan2(ASin,ACos)/ASinFac;
  Result2.T1:=ASin;
  Result2.T2:=ACos;

  if Result2.T3<0.0 then Result2.T3:=APeakFreq+Result2.T3;

  Result:=Result2;
end;}

procedure SBI_Reset(var SBI: TSubbandBitsItem); inline;
begin
  SBI:=0;
end;

procedure SBI_Set(var SBI: TSubbandBitsItem; const Index: MVInt); inline;
begin
  SBI:=SBI or (1 shl Index);
end;

function SBI_Get(const SBI: TSubbandBitsItem; const Index: MVInt): Boolean; inline;
begin
  Result:=Boolean((SBI shr Index) and 1);
end;

function SBI_Compare(const SBI1,SBI2: TSubbandBitsItem): TSubbandBitsItem; inline;
begin
  Result:=SBI1 and SBI2;
end;

function SBI_Evaluate(const SBI: TSubbandBitsItem): MVInt; inline;
var
  I: Integer;
begin
  Result:=0;
  for I:=0 to BeatAnaSubbandCount-1
    do Result+=(SBI shr I) and 1;
end;

procedure SBI_Copy(const Source: TSubbandBitsItem; var Dest: TSubbandBitsItem); inline;
begin
  Dest:=Source;
end;

procedure CopySubbandBits(ASource,ADest: TSubbandBitBuffer; const ASourceIndex,ADestIndex,ACount: MVInt); inline;
var
  ASourceIndex2,ADestIndex2: Integer;
begin
  ADestIndex2:=ADestIndex;
  for ASourceIndex2:=ASourceIndex to ASourceIndex+ACount-1
    do ADest[ADestIndex2 mod ADest.Count]:=ASource[ASourceIndex2 mod ASource.Count];
end;

end.

