unit MPFFreqAna;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fftFreqAna, PluginType, SpectrumData, Math, VPBuffers;

type
  TMPFSpectrumData = class (TFFTFreqAna)
    FMPF       : MVInt;
    FMPFMinBand: MVInt;
    FMPFMaxBand: MVInt;
    FMainFreq  : Real;
  private
    procedure MPFInit; inline;
    procedure DoGetMPF; inline;
  protected
    procedure DoAnalyse; override;
    procedure DoInit; override;

    property MPFMinBand: MVInt read FMPFMinBand;
    property MPFMaxBand: MVInt read FMPFMaxBand;
  public
    constructor Create(ABufferManager: IVPBufferManager); override;
    destructor Destroy; override;
    function LevelFrequency(const ALevel: MVInt): MVFloat;
  published
    property MPF: MVInt read FMPF;
  end;

  TToneFloat         = MVFloat;
  TTone              = record
    ToneIndex: MVSInt;
    Pitch    : MVFloat;
  end;

function FrequencyToTone(const AFreq: MVFloat; const a: MVFloat = 440.0): TTone; inline;
function FrequencyToToneFloat(const AFreq: MVFloat; const a: MVFloat = 440.0): TToneFloat; inline;
function ToneFloatToTone(const ATone: MVFloat): TTone; inline;
function ToneToStr(const ATone: TTone): ShortString;
function TonePitchToStr(const ATone: TTone): ShortString;

implementation

{TMPFSpectrumData}

constructor TMPFSpectrumData.Create(ABufferManager: IVPBufferManager);
begin
  inherited Create(ABufferManager);
end;

destructor TMPFSpectrumData.Destroy;
begin
  (*Fehler hier....*)
  inherited Destroy;
end;

function TMPFSpectrumData.LevelFrequency(const ALevel: MVInt): MVFloat;
begin
  //nur für ALevel<FFreqCount div 2
  Result:=(FMainFreq {= FSampleRate/FFreqCount} )*ALevel;
end;

procedure TMPFSpectrumData.MPFInit; inline;
const
  MinFreq = 65.40639132; //C aus der Oktave -1
  MaxFreq = 4186.009045; //c aus der Oktave 5
var
  AMPFMaxBand: Integer;
begin
  FMainFreq:=SampleRate/FreqCount;
  FMPFMinBand:=Trunc(MinFreq/FMainFreq);
  FMPFMaxBand:=Trunc(MaxFreq/FMainFreq)+1;
  AMPFMaxBand:=(FreqCount div 2)-1;
  if FMPFMaxBand>AMPFMaxBand then FMPFMaxBand:=AMPFMaxBand;
end;

procedure TMPFSpectrumData.DoGetMPF; inline;
var
  I         : Integer;
  AFreq,AMPF: MVFloat;
begin
  AMPF:=Levels[FMPFMinBand,0];
  FMPF:=FMPFMinBand;
  for I:=FMPFMinBand+1 to FMPFMaxBand do begin
    AFreq:=Levels[I,0];
    if AFreq>=AMPF then begin
       AMPF:=AFreq;
       FMPF:=I;
    end;
  end;
end;

procedure TMPFSpectrumData.DoInit;
begin
  inherited DoInit;
  MPFInit;
end;

procedure TMPFSpectrumData.DoAnalyse;
begin
  inherited DoAnalyse;
  DoGetMPF;
end;

{Allgemein}

const
  ToneNames : array [0..11] of string = ('c','c#','d','eb','e','f','f#','g','ab','a','b','h');
  ToneNames2: array [-12..-1] of string = ('C','C#','D','Eb','E','F','F#','G','Ab','A','B','H');
  ToneaIndex     = 21;
  ToneIncrement  = 1.0594630943592952645618252949463; //2^(1/12)
  lnToneIncrement= ln(ToneIncrement);

function FrequencyToToneFloat(const AFreq: MVFloat; const a: MVFloat = 440.0): TToneFloat; inline;
begin
  if isZero(AFreq)
    then Result:=Nan
    else Result:=ln(AFreq/a)/lnToneIncrement;
end;

function ToneFloatToTone(const ATone: MVFloat): TTone; inline;
begin
  with Result do begin
    if IsNan(ATone) then begin
      Result.Pitch:=Nan;
      exit;
    end;
    ToneIndex:=Round(ATone);
    Pitch:=Frac(ATone)*100.0;
    if Pitch>=50.0
      then Pitch-=100.0
      else if Pitch<=-50.0
        then Pitch+=100.0;
  end;
end;

function FrequencyToTone(const AFreq: MVFloat; const a: MVFloat = 440.0): TTone; inline;
begin
  Result:=ToneFloatToTone(FrequencyToToneFloat(AFreq,a));
end;

function ToneToStr(const ATone: TTone): ShortString;
var
  I,AToneIndex: Integer;
begin
  if isNan(ATone.Pitch) then begin
    Result:='';
    exit;
  end;
  AToneIndex:=ATone.ToneIndex+ToneaIndex;
  if AToneIndex>=0 then begin
    Result:=ToneNames[AToneIndex mod 12];
    for I:=1 to AToneIndex div 12 do Result+='''';
  end else begin
    if AToneIndex>=-12
      then Result:=ToneNames2[AToneIndex]
      else Result:='';
  end;
end;

function TonePitchToStr(const ATone: TTone): ShortString;
begin
  if isNan(ATone.Pitch) then begin
    Result:='';
    exit;
  end;
  if ATone.Pitch>=0.0
    then Result:='+'+FloatToStrF(ATone.Pitch,ffFixed,7,2)+' Cent'
    else Result:=FloatToStrF(ATone.Pitch,ffFixed,7,2)+' Cent';
end;

end.

