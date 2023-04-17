unit FFTFreqAna; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasicFreqAna, SpectrumData, PluginType, ExtPtrType,
  VPBuffers;

type
  TFFTFreqAna = class (TBasicSpectrumData,IBasicFFTSpectrumData)
  private
    //Einstellungen
    FWaveDataCount    : MVInt;
    FLongWaveDataCount: MVInt;
    FFreqCount        : MVInt;
    FFFTHistoryCount  : MVInt;
    //Puffer
    FFFTHistory       : TSD2WData;
    FLevels           : TSD2WData;
    FReBuf            : TSD2WData;
    FImBuf            : TSD2WData;
    FPeak             : TSD2WData;
    FPeakPos          : TSD2WData;
    //Puffergroessen
    FFFTBufSize       : Cardinal;
    FFFTBufsSize      : Cardinal;
    FFFTFrameSize     : Cardinal;
    FPeakSize         : Cardinal;
    FPeakPosSize      : Cardinal;
    FFFTHistoryPos    : MVInt;
    //Vorberechnetes
    FMainFreq         : MVFloat;
    function GetWaveData(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetWaveDataLong(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetLevels(const AIndex,AChannel: MVInt): MVFloat; stdcall;
    function GetPeak(const AChannel: MVInt): MVFloat; stdcall;
    function GetPeakPos(const AChannel: MVInt): MVInt; stdcall;
    function GetFreqCount: MVInt; stdcall;
    function GetWaveDataCount: MVInt; stdcall;
    function GetLongWaveDataCount: MVInt; stdcall;
    //Analyse
    procedure DoFFT; inline;
    procedure DoGetPeak; inline;
  protected
    procedure FFT(var AFR, AFI; const ASize: Cardinal; const Signum: ShortInt = 1);
    procedure DoAnalyse; override;
    procedure DoInit; override;
    procedure FreeOldLevels; virtual; //wird ausgeführt, bevor das älteste Element des FFT History Buffers überschrieben wird (FFTHistoryPos steht dann schon auf diesem Element)
    function GetLevelBuffer(const AChannel: MVInt): vpRealBuffer; stdcall;
    //function GetHistoricLevelBuffer(const AHistoryIndex,AChannel: MVInt): vpRealBuffer; stdcall;

    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
  public
    constructor Create(ABufferManager: IVPBufferManager); override;
    destructor Destroy; override;
    procedure SetCounts(const AWaveDataCount,ALongWaveDataCount,AFreqCount,AFFTHistoryCount: MVInt; ACounts: array of MVInt);
    function LevelFrequency(const ALevel: MVInt): MVFloat;
    function FrequencyLevel(const AFrequency: MVFloat): MVFloat;
    property Levels[const AIndex,AChannel: MVInt]: MVFloat read GetLevels;
    property Peak[const AChannel: MVInt]: MVFloat read GetPeak;
    property PeakPos[const AChannel: MVInt]: MVInt read GetPeakPos;
    property WaveData[const AIndex,AChannel: MVInt]: MVFloat read GetWaveData;
    property WaveDataLong[const AIndex,AChannel: MVInt]: MVFloat read GetWaveDataLong;
  published
    property FFTHistoryCount: MVInt read FFFTHistoryCount;
    property FreqCount: MVInt read FFreqCount;
    property LongWaveDataCount: MVInt read FLongWaveDataCount;
    property WaveDataCount: MVInt read FWaveDataCount;
  end;

implementation

{TFFTFreqAna}

const
  ln2           = ln(2);

constructor TFFTFreqAna.Create(ABufferManager: IVPBufferManager);
begin
  inherited Create(ABufferManager);
  FWaveDataCount:=0;
  FLongWaveDataCount:=0;
  FFreqCount:=0;
  FFFTHistoryCount:=0;

  FFFTBufSize:=0;
  FFFTBufsSize:=0;
  FFFTFrameSize:=0;
  FPeakSize:=0;
  FPeakPosSize:=0;
end;

destructor TFFTFreqAna.Destroy;
begin
  if FFFTBufSize>0 then begin
    FreeMem(FReBuf,FFFTBufSize);
    FreeMem(FImBuf,FFFTBufSize);
    if FFFTBufsSize>0 then FreeMem(FFFTHistory,FFFTBufsSize);
  end;
  if FPeakSize>0 then begin
    FreeMem(FPeak,FPeakSize);
    FreeMem(FPeakPos,FPeakPosSize);
  end;
  inherited Destroy;
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TFFTFreqAna.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TFFTFreqAna.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IBasicFFTSpectrumData(Self))
    else Result:=inherited Future(Version);
end;

procedure TFFTFreqAna.SetCounts(const AWaveDataCount,ALongWaveDataCount,AFreqCount,AFFTHistoryCount: MVInt; ACounts: array of MVInt);
var
  ACounts2: array of MVInt;
  I       : Integer;
begin
  FWaveDataCount:=AWaveDataCount;
  FLongWaveDataCount:=ALongWaveDataCount;
  FFreqCount:=AFreqCount;
  FFFTHistoryCount:=AFFTHistoryCount;

  SetLength(ACounts2,Length(ACounts)+3);
  ACounts2[0]:=AWaveDataCount;
  ACounts2[1]:=ALongWaveDataCount;
  ACounts2[2]:=AFreqCount;
  for I:=0 to Length(ACounts)-1 do ACounts2[I+3]:=ACounts[I];

  inherited SetCounts({[AWaveDataCount,ALongWaveDataCount,AFreqCount]}ACounts2);
  SetLength(ACounts2,0);
end;

procedure TFFTFreqAna.DoFFT; inline;
var
  I{,J,IJ} : Integer;
  AReBuf : PSD2BufferFormat;// absolute FReBuf;
  AImBuf : PSD2BufferFormat;// absolute FImBuf;
  //AWData : PSD2BufferFormat absolute AWData2;
  ALevels: PSD2BufferFormat;// absolute FLevels;
begin
  {AWData2:=FAInputBuffers[0]
  IJ:=0;
  for I:=0 to FFreqCount-1 do begin
    AReBuf^[I]:=0.0;
    for J:=0 to FChannels-1 do begin
      AReBuf^[I]+=AWData^[IJ];
      Inc(IJ);
    end;
    AReBuf^[I]/=FChannels;
    AImBuf^[I]:=0.0;
  end;}

  AReBuf:=FReBuf;
  AImBuf:=FImBuf;
  ALevels:=FLevels;
  for I:=0 to FFreqCount-1 do AImBuf^[I]:=0.0;
  Move(GetBufferPtr(FreqDataBI,0)^,FReBuf^,FFFTBufSize);

  FFT(FReBuf^,FImBuf^,FFreqCount);

  for I:=0 to FFreqCount-1 do ALevels^[I]:=Sqrt(Sqr(AReBuf^[I])+Sqr(AImBuf^[I]));
end;

procedure TFFTFreqAna.FFT(var AFR, AFI; const ASize: Cardinal; const Signum: ShortInt = 1);
var
  EL,I,I1,IR,IR1,J,L,M,NN: Integer;
  WI,WR,TI,TR,fftA       : Real;
  FR: TSD2BufferFormat absolute AFR;
  FI: TSD2BufferFormat absolute AFI;
begin
  //fftA:=ln(n)/0.69314718;
  fftA:=ln(ASize)/ln2;
  if (abs(fftA/round(fftA)-1)>1e-8) then begin
    {Signum:=0;} exit;
  end;
  IR:=-1; NN:=pred(ASize);
  for I:=0 to pred(NN) do begin
    L:=ASize;
    repeat
      L:=L div 2
    until (IR+1+L)<=NN;
    IR:=(L+(IR+1) mod L)-1;
    if IR+1>I+1 then begin
      I1:=succ(I+1);
      IR1:=succ(IR+1);

      TR:=Fr[I1];
      Fr[I1]:=Fr[IR1];
      Fr[IR1]:=TR; {Swap von Fr[I1], Fr[Ir1]}

      TI:=Fi[I1];
      Fi[I1]:=Fi[IR1];
      Fi[IR1]:=TI
    end;
  end;
  L:=1;
  while L<ASize do begin
    El:=L;
    L:=L shl 1;
    for M:=0 to pred(El) do begin
      fftA:=-SIGNUM*PI*M/succ(EL); WR:=COS(fftA); WI:=sin(fftA);
      I:=M;
      J:=M+El;
      while I<ASize do begin
        TR:=WR*Fr[J]-WI*Fi[J];
        TI:=WR*Fi[J]+WI*Fr[J];
        Fr[J]:=Fr[I]-TR; Fi[J]:=Fi[I]-TI;
        Fr[I]:=Fr[I]+TR; Fi[I]:=Fi[I]+TI;
        inc(I,L); inc(J,L)
      end;
    end;
  end;
  if SIGNUM=1 then for I:=0 to pred(ASize) do begin
    Fr[I]:=Fr[I]/ASize;
    Fi[I]:=Fi[I]/ASize;
  end;
end;

procedure TFFTFreqAna.DoGetPeak; inline;
var
  I,J                  : Integer;
  APeak,AWData,APeakPos: Pointer;
  APeak2               : PSD2BufElem absolute APeak;
  AWData2              : PSD2BufElem absolute AWData;
  Temp                 : TSD2BufElem;
  APeakPos2            : PSD2IntBufElem absolute APeakPos;
begin
  APeak:=FPeak;
  APeakPos:=FPeakPos;
  //AWData:=FWData;
  for I:=0 to Channels-1 do begin
    APeak2^:=0.0;
    APeakPos2^:=0;
    //AWData:=GetLastBuffer(I);
    AWData:=GetBufferPtr(WaveDataBI,I);
    for J:=0 to {InputBufferSampleCount}FWaveDataCount-1 do begin
      //APeak2^+=Abs(AWData2^);
      Temp:=Abs(AWData2^);
      if Temp>APeak2^ then begin
        APeak2^:=Temp;
        APeakPos2^:=J;
      end;
      AWData+=SD2BufElemSize;
    end;
    //APeak2^/=FWaveDataCount;
    APeak+=SD2BufElemSize;
    APeakPos+=SD2IntBufElemSize;
  end;
end;

procedure TFFTFreqAna.DoAnalyse;
begin
  inherited DoAnalyse;
  FFFTHistoryPos:=(FFFThistoryPos+1) mod FFFTHistoryCount;
  FLevels:=FFFTHistory+(FFFTHistoryPos*FFFTFrameSize);
  FreeOldLevels;
  DoFFT;
  DoGetPeak;
end;

procedure TFFTFreqAna.FreeOldLevels;
begin
  //do nothing
end;

procedure TFFTFreqAna.DoInit;
begin
  if FFFTBufSize>0 then begin
    FreeMem(FReBuf,FFFTBufSize);
    FreeMem(FImBuf,FFFTBufSize);
    if FFFTBufsSize>0 then FreeMem(FFFTHistory,FFFTBufsSize);
  end;
  if FPeakSize>0 then begin
    FreeMem(FPeak,FPeakSize);
    FreeMem(FPeakPos,FPeakPosSize);
  end;
  inherited DoInit;
  FFFTBufSize:=FFreqCount*SD2BufElemSize;
  FFFTFrameSize:=FFFTBufSize*Channels;
  FFFTBufsSize:=FFFTFrameSize*FFFTHistoryCount;
  FPeakSize:=Channels*SD2BufElemSize;
  FPeakPosSize:=Channels*SD2IntBufElemSize;
  GetMem(FReBuf,FFFTBufSize);
  GetMem(FImBuf,FFFTBufSize);
  GetMem(FFFTHistory,FFFTBufsSize);
  ZeroMVFloat(FFFTHistory,FFFTBufsSize div SD2BufElemSize);

  FLevels:=FFFTHistory;
  GetMem(FPeak,FPeakSize);
  GetMem(FPeakPos,FPeakPosSize);
  FMainFreq:=SampleRate/FreqCount;
  FFFTHistoryPos:=0;
end;

function TFFTFreqAna.LevelFrequency(const ALevel: MVInt): MVFloat;
begin
  //nur für ALevel<FFreqCount div 2
  Result:=(FMainFreq {= FSampleRate/FFreqCount} )*ALevel;
end;

function TFFTFreqAna.FrequencyLevel(const AFrequency: MVFloat): MVFloat;
begin
  Result:=(AFrequency/FMainFreq);
end;

function TFFTFreqAna.GetWaveData(const AIndex,AChannel: MVInt): MVFloat; stdcall;
begin
  Result:=PSD2BufferFormat(GetBufferPtr(WaveDataBI,AChannel))^[AIndex];
end;

function TFFTFreqAna.GetWaveDataLong(const AIndex,AChannel: MVInt): MVFloat; stdcall;
begin
  Result:=PSD2BufferFormat(GetBufferPtr(WaveDataLongBI,AChannel))^[AIndex];
end;

function TFFTFreqAna.GetLevels(const AIndex,AChannel: MVInt): MVFloat; stdcall;
begin
  Result:=PSD2BufElem(FLevels+(FFFTBufSize*AChannel)+(AIndex*SD2BufElemSize))^;
end;

function TFFTFreqAna.GetPeak(const AChannel: MVInt): MVFloat; stdcall;
begin
  Result:=PSD2BufElem(FPeak+(AChannel*SD2BufElemSize))^;
end;

function TFFTFreqAna.GetPeakPos(const AChannel: MVInt): MVInt; stdcall;
begin
  Result:=PSD2IntBufElem(FPeakPos+(AChannel*SD2IntBufElemSize))^;
end;

function TFFTFreqAna.GetFreqCount: MVInt; stdcall;
begin
  Result:=FFreqCount;
end;

function TFFTFreqAna.GetWaveDataCount: MVInt; stdcall;
begin
  Result:=FWaveDataCount;
end;

function TFFTFreqAna.GetLongWaveDataCount: MVInt; stdcall;
begin
  Result:=FLongWaveDataCount;
end;

function TFFTFreqAna.GetLevelBuffer(const AChannel: MVInt): vpRealBuffer; stdcall;
begin
  Result:=BufferManager.ToVPRealBuffer(FFreqCount,FLevels+(FFFTBufSize*AChannel));
end;

end.

