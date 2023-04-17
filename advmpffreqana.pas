unit AdvMPFFreqAna;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fftFreqAna, PluginType, SpectrumData, Math, VPBuffers,
  MPFFreqAna, GenericMemoryArrays;

type
  TMVFloatBuffer         = specialize TGenericBufferArray<MVFloat>;

  TAdvMPFSpectrumData = class (TMPFSpectrumData)
  private
    procedure MPFFeatureInit; inline;
    procedure DoGetMPFFeatures; inline;
    procedure DoGetFFTAverage; inline;
    procedure GetHistoryDifs(ANewPos,AOldPos,ALevelIndex: MVInt; AFreqHistory,AFreqHistoryDif,AToneHistory,AToneHistoryDif: TMVFloatBuffer);
    //procedure GetWeightedHistoryDifs(APos,ALevelIndex)
  protected
    procedure FreeOldLevels; override;
    procedure DoAnalyse; override;
    procedure DoInit; override;
    //function GetMPHistoryItem(Index: Cardinal; ABuf: TMVFloatBuffer): MVFloat; inline;
    //function GetMPRTHistoryWItem(Index: Cardinal): MVFloat;
  public
    FFFTAverage     : TMVFloatBuffer;

    FMPFHistory     : TMVFloatBuffer;
    FMPFHistoryDif  : TMVFloatBuffer;
    FMPTHistory     : TMVFloatBuffer; //MPF = Most Present Frequency; MPT = Most Present Tone
    FMPTHistoryDif  : TMVFloatBuffer;

    FMPRHistory     : TMVFloatBuffer; //MPR: Relative MPF
    FMPRFHistory    : TMVFloatBuffer;
    FMPRFHistoryDif : TMVFloatBuffer;
    FMPRTHistory    : TMVFloatBuffer;
    FMPRTHistoryDif : TMVFloatBuffer;
    FMPRFHistoryW   : TMVFloatBuffer; //W: gewichtet
    FMPRTHistoryW   : TMVFloatBuffer;

    FMPRRHistory    : TMVFloatBuffer; //MPRR: "doppelt" relative MPF (Abweichung wird auch relativ bestimmt)
    FMPRRFHistory   : TMVFloatBuffer;
    FMPRRFHistoryDif: TMVFloatBuffer;
    FMPRRTHistory   : TMVFloatBuffer;
    FMPRRTHistoryDif: TMVFloatBuffer;
    FMPRRFHistoryW  : TMVFloatBuffer; //W: gewichtet
    FMPRRTHistoryW  : TMVFloatBuffer;

    FMPFHistoryPos  : MVInt;

    constructor Create(ABufferManager: IVPBufferManager); override;
    destructor Destroy; override;
  published
  end;

implementation

{AdvTMPFSpectrumData}

constructor TAdvMPFSpectrumData.Create(ABufferManager: IVPBufferManager);
begin
  inherited Create(ABufferManager);
  FFFTAverage:=TMVFloatBuffer.Create(0,Self);
  FMPFHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPFHistoryDif:=TMVFloatBuffer.Create(1337,Self);
  FMPTHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPTHistoryDif:=TMVFloatBuffer.Create(1337,Self);
  FMPRHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPRFHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPRFHistoryDif:=TMVFloatBuffer.Create(1337,Self);
  FMPRTHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPRTHistoryDif:=TMVFloatBuffer.Create(1337,Self);
  FMPRFHistoryW:=TMVFloatBuffer.Create(1337,Self);
  FMPRTHistoryW:=TMVFloatBuffer.Create(1337,Self);
  FMPRRHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPRRFHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPRRFHistoryDif:=TMVFloatBuffer.Create(1337,Self);
  FMPRRTHistory:=TMVFloatBuffer.Create(1337,Self);
  FMPRRTHistoryDif:=TMVFloatBuffer.Create(1337,Self);
  FMPRRFHistoryW:=TMVFloatBuffer.Create(1337,Self);
  FMPRRTHistoryW:=TMVFloatBuffer.Create(1337,Self);
  FMPFHistoryPos:=0;
end;

destructor TAdvMPFSpectrumData.Destroy;
begin
  inherited Destroy;
end;

procedure TAdvMPFSpectrumData.MPFFeatureInit; inline;
begin
  FFFTAverage.Resize(FreqCount div 2);
  FFFTAverage.Fill2(0.0);
end;

procedure TAdvMPFSpectrumData.GetHistoryDifs(ANewPos,AOldPos,ALevelIndex: MVInt; AFreqHistory,AFreqHistoryDif,AToneHistory,AToneHistoryDif: TMVFloatBuffer);
var
  AFreq            : MVFloat;
  ATone            : TToneFloat;
begin
  AFreq:=LevelFrequency(ALevelIndex);
  ATone:=FrequencyToToneFloat(AFreq);

  AFreqHistory[ANewPos]:=AFreq;
  AFreqHistoryDif[ANewPos]:=AFreq-AFreqHistory[AOldPos];
  AToneHistory[ANewPos]:=ATone;
  AToneHistoryDif[ANewPos]:=ATone-AToneHistory[AOldPos];
end;

procedure TAdvMPFSpectrumData.DoGetMPFFeatures; inline;
var
  {AFreq            : MVFloat;
  ATone            : TToneFloat;}
  AOldMPFHistoryPos: MVInt;

  I                           : Integer;
  AHighLevel,AHighLevelR      : MVInt;
  AHighVal,AHighValR,AVal     : MVFloat;
begin
  {AFreq:=LevelFrequency(MPF);
  ATone:=FrequencyToToneFloat(AFreq);}

  AOldMPFHistoryPos:=FMPFHistoryPos;
  FMPFHistoryPos:=(FMPFHistoryPos+1) mod FMPFHistory.Count;

  GetHistoryDifs(FMPFHistoryPos,AOldMPFHistoryPos,MPF,FMPFHistory,FMPFHistoryDif,FMPTHistory,FMPTHistoryDif);

  {FMPFHistory[FMPFHistoryPos]:=AFreq;
  FMPFHistoryDif[FMPFHistoryPos]:=AFreq-FMPFHistory[AOldMPFHistoryPos];
  FMPTHistory[FMPFHistoryPos]:=ATone;
  FMPTHistoryDif[FMPFHistoryPos]:=ATone-FMPTHistory[AOldMPFHistoryPos];}

  AHighVal:=-Infinity;
  AHighValR:=-Infinity;
  for I:=MPFMinBand+1 to MPFMaxBand do begin
    AVal:=Levels[I,0]-FFFTAverage[I];
    if AHighVal<AVal then begin
      AHighVal:=AVal;
      AHighLevel:=I;
    end;
    AVal:=Levels[I,0]/FFFTAverage[I];
    if AHighValR<AVal then begin
      AHighValR:=AVal;
      AHighLevelR:=I;
    end;
  end;
  FMPRHistory[FMPFHistoryPos]:=AHighVal;
  FMPRRHistory[FMPFHistoryPos]:=AHighValR;

  GetHistoryDifs(FMPFHistoryPos,AOldMPFHistoryPos,AHighLevel,FMPRFHistory,FMPRFHistoryDif,FMPRTHistory,FMPRTHistoryDif);
  //AVal:=AHighVal-FMPRHistory[AOldMPFHistoryPos];
  FMPRFHistoryW[FMPFHistoryPos]:=sqr(FMPRFHistoryDif[FMPFHistoryPos])*FMPRHistory[FMPFHistoryPos];
  FMPRTHistoryW[FMPFHistoryPos]:=sqr(FMPRTHistoryDif[FMPFHistoryPos])*FMPRHistory[FMPFHistoryPos];
  GetHistoryDifs(FMPFHistoryPos,AOldMPFHistoryPos,AHighLevelR,FMPRRFHistory,FMPRRFHistoryDif,FMPRRTHistory,FMPRRTHistoryDif);
  FMPRRFHistoryW[FMPFHistoryPos]:=sqr(FMPRRFHistoryDif[FMPFHistoryPos])*FMPRRHistory[FMPFHistoryPos];
  FMPRRTHistoryW[FMPFHistoryPos]:=sqr(FMPRRTHistoryDif[FMPFHistoryPos])*FMPRRHistory[FMPFHistoryPos];
end;



procedure TAdvMPFSpectrumData.DoGetFFTAverage;
var
  I      : Integer;
  ABuf   : vpRealBuffer;
  ABufVal: vpRealBufferItem;
begin
  ABuf:=GetLevelBuffer(0);
  for I:=0 to FFFTAverage.Count-1
    do FFFTAverage[I]:=FFFTAverage[I]+(BufferManager.Items[ABuf,I]/FFTHistoryCount);
end;

procedure TAdvMPFSpectrumData.FreeOldLevels;
var
  I   : Integer;
  ABuf: vpRealBuffer;
begin
  ABuf:=GetLevelBuffer(0);
  for I:=0 to FFFTAverage.Count-1
    do FFFTAverage[I]:=FFFTAverage[I]-(BufferManager.Items[ABuf,I]/FFTHistoryCount);
end;

procedure TAdvMPFSpectrumData.DoInit;
begin
  inherited DoInit;
  MPFFeatureInit;
end;

procedure TAdvMPFSpectrumData.DoAnalyse;
begin
  inherited DoAnalyse;
  DoGetFFTAverage;
  DoGetMPFFeatures;
end;

{TAdvMPFSpectrumData - Get & Set Methoden}

{function TAdvMPFSpectrumData.GetMPHistoryItem(Index: Cardinal; ABuf: TMVFloatBuffer): MVFloat;
begin
  Result:=
end;

function TAdvMPFSpectrumData.GetMPRTHistoryWItem(Index: Cardinal): MVFloat;
begin
  Result:=GetMPHistoryItem(Index,FMPRTHistoryW);
end;}

end.

