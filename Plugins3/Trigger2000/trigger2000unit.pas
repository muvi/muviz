unit Trigger2000Unit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, AdvCoord, AdvParamType,
  GraphX32, MPluginType4, SimpleVis, ParamOp, Math, AdvFunc, VisAddInput,
  SysUtils, ParamTypes, VisualisationUtils;

type
  TBPM                    = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBPMExists              = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBeatposition           = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBeatMax                = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBeatTrigger            = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBeatStopCaller         = class (TVisualisationEvents)
  private
    FIsBeat   : Boolean;
    FMain     : IPCall;
    FOutput   : IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TFrequencyBeatTrigger   = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FFrequency: IPInteger;
    FOutput   : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TWavedata               = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPBuffer;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TFFT                    = class (TVisualisationEvents)
  private
    FMain     : IPCall;
    FOutput   : IPBuffer;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBuffer10               = class;

  TBuffer10Value          = class
  strict private
    FIndex: Integer;
    FParam: IPFloat;
    FOwner: TBuffer10;
  public
    constructor Create(AParam: IPFloat; AIndex: Integer; AOwner: TBuffer10; AEnvironment: IPVisualisationEnvironment);
    destructor Destroy; override;
    property Index: Integer read FIndex;
    property Owner: TBuffer10 read FOwner;
    property Param: IPFloat read FParam;
  end;

  TBuffer10               = class (TVisualisationEvents)
  private
    FUsedCount      : IPInteger;
    FRepeatCount    : IPInteger;
    FValues         : array [0..9] of TBuffer10Value;
    FOutput         : IPBuffer;
    FBuffer         : IVFloatBuffer;
    FLastUsedCount  : Integer;
    FLastRepeatCount: Integer;
    procedure DoInit;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TMultibufferer          = class (TVisualisationEvents)
  private
    FInput  : IPBuffer;
    FOutputs: array [0..3] of IPBuffer;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBufferCutter           = class (TVisualisationEvents)
  private
     FStart : IPInteger;
     FEnd   : IPInteger;
     FInput : IPBuffer;
     FOutput: IPBuffer;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRelativeBufferCutter   = class (TVisualisationEvents)
  private
    FStart : IPFloat;
    FEnd   : IPFloat;
    FInput : IPBuffer;
    FOutput: IPBuffer;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBufferConnector        = class (TVisualisationEvents)
  private
    FMain        : IPCall;
    FInputs      : array [0..4] of IPBuffer;
    FOutput      : IPBuffer;
    FBuffer      : IVFloatBuffer;
    //to prevent threading issues
    FStoredInputs: array [0..4] of TVBuffer;
    procedure UpdateBufferSize(ASize: Cardinal); inline;
    procedure UpdateBuffer;
    function ExpectedBufferSize: Cardinal; inline;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TLinearBuffer           = class (TVisualisationEvents)
  private
    FStart : IPFloat;
    FEnd   : IPFloat;
    FSize  : IPInteger;
    FOutput: IPBuffer;
    FBuffer: IVFloatBuffer;
    procedure UpdateBuffer;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBufferGetter           = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FBuffer     : IPBuffer;
    FIndex      : IPInteger;
    FOutput     : IPFloat;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRelativeBufferGetter   = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FBuffer     : IPBuffer;
    FPosition   : IPFloat;
    FOutput     : IPFloat;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBufferCalculation      = class (TVisualisationEvents)
  private
    FMain             : IPCall;
    FInput            : IPBuffer;
    FCalculationInput : IPFloat;
    FOutput           : IPBuffer;
    FCalculationOutput: IPFloat;
    FNextOutput       : IPCall;

    FBuffer           : IVFloatBuffer;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TSmoother               = class (TVisualisationEvents)
  private
    FMain        : IPCall;
    FInput       : IPFloat;
    FIntensity   : IPFloat;
    FOutput      : IPFloat;

    FCurrentValue: Real;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRealBeatPosition       = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FOutput     : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBSG                    = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FMax        : IPFloat;
    FSpeed      : IPFloat;

    FLastBeatPos: TVFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TNow                    = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FSecond     : IPInteger;
    FMinute     : IPInteger;
    FHour       : IPInteger;
    FDay        : IPInteger;
    FMonth      : IPInteger;
    FYear       : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  {%REGION IDs}
  VIDBPM                   : TGUID = '{2C75A06B-5A8D-471A-2142-504D54726967}';
  VIDBPMEXISTS             : TGUID = '{2C75A06B-5A8D-471A-2142-504D466F756E}';
  VIDBEATPOSITION          : TGUID = '{2C75A06B-5A8D-471A-2142-656174506F73}';
  VIDBEATMAX               : TGUID = '{2C75A06B-5A8D-471A-2142-6561744D6178}';
  VIDBEATTRIGGER           : TGUID = '{2C75A06B-5A8D-471A-2142-656174547267}';
  VIDBEATSTOPCALLER        : TGUID = '{2C75A06B-5A8D-471A-2142-65617443616C}';
  VIDFREQUENCYBEATTRIGGER  : TGUID = '{2C75A06B-5A8D-471A-2146-532D42656174}';
  VIDWAVEDATA              : TGUID = '{2C75A06B-5A8D-471A-2142-554657415645}';
  VIDFFT                   : TGUID = '{2C75A06B-5A8D-471A-2142-55465F464654}';
  VIDBUFFER10              : TGUID = '{2C75A06B-5A8D-471A-2142-55465F5F3130}';
  VIDMULTIBUFFERER         : TGUID = '{2C75A06B-5A8D-471A-214D-756C74694266}';
  VIDBUFFERCUTTER          : TGUID = '{2C75A06B-5A8D-471A-2142-756666437574}';
  VIDRELATIVEBUFFERCUTTER  : TGUID = '{2C75A06B-5A8D-471A-2142-756643757452}';
  VIDBUFFERCONNECTOR       : TGUID = '{2C75A06B-5A8D-471A-2142-7566436F6E6E}';
  VIDLINEARBUFFER          : TGUID = '{2C75A06B-5A8D-471A-214C-696E5F427566}';
  VIDBUFFERGETTER          : TGUID = '{2C75A06B-5A8D-471A-2147-65744275665F}';
  VIDRELATIVEBUFFERGETTER  : TGUID = '{2C75A06B-5A8D-471A-2147-657442756652}';
  VIDBUFFERCALCULATION     : TGUID = '{2C75A06B-5A8D-471A-2142-756643616C63}';
  VIDSMOOTHER              : TGUID = '{2C75A06B-5A8D-471A-2153-6D6F6F746872}';
  VIDREALBEATPOSITION      : TGUID = '{2C75A06B-5A8D-471A-2142-656174506F46}';
  VIDBSG                   : TGUID = '{2C75A06B-5A8D-471A-2142-534720202020}';
  VIDNOW                   : TGUID = '{2C75A06B-5A8D-471A-214E-4F5720202020}';

  {
  PIDBPM                   : TGUID = '{A6AAB480-0302-46A5-8DDD-A95810E0FE2D}';
  PIDBPMEXISTS             : TGUID = '{7BBF0AC2-54FA-43C2-ABC7-763BF9332BBB}';
  PIDBEATPOSITION          : TGUID = '{F547585B-B363-4979-A2F1-36E9C3DA3A5A}';
  PIDBEATMAX               : TGUID = '{FBD0C136-5CB4-4257-8E5D-38DE13B525CC}';
  PIDBEATTRIGGER           : TGUID = '{E448D936-1C14-4D65-A092-7AE683243F3F}';
  PIDBEATSTOPCALLER        : TGUID = '{4A519322-AB02-45C7-BDED-8679DB611C7E}';
  PIDFREQUENCYBEATTRIGGER  : TGUID = '{C74387D0-12C7-4254-806E-AF9294F2E82A}';
  PIDWAVEDATA              : TGUID = '{6EA0C485-3E8B-4DD8-8224-1B7D533D88C8}';
  PIDFFT                   : TGUID = '{AF101D29-9FB4-4A8B-AFEC-CF5F28A93A7F}';
  PIDBUFFER10              : TGUID = '{9B6B0147-B3CD-443A-A461-05192AFC0322}';
  PIDMULTIBUFFERER         : TGUID = '{D3DDE4D8-9049-4D0D-B5B3-9964BFFF6D40}';
  PIDBUFFERCUTTER          : TGUID = '{22CFF8DC-155C-47D6-B631-F63C7A8BF1FE}';
  PIDRELATIVEBUFFERCUTTER  : TGUID = '{96B1DD8B-D40F-4D4B-B25D-A5A78593033D}';
  PIDBUFFERCONNECTOR       : TGUID = '{C2F4C34F-71E9-4C37-A2E0-7E3F86855726}';
  PIDLINEARBUFFER          : TGUID = '{3D1FF0B0-DBAC-44FB-86E9-84F63C01E1C0}';
  PIDBUFFERGETTER          : TGUID = '{CFA015BE-FCBE-4949-A273-10AE1647B3F1}';
  PIDRELATIVEBUFFERGETTER  : TGUID = '{D51AA56B-D7B6-46A0-A6C9-44D4EC560CD7}';
  PIDBUFFERCALCULATION     : TGUID = '{5041CAAC-509F-4BF2-AF91-17A1BDFF2B6F}';
  PIDSMOOTHER              : TGUID = '{6C85A69F-B098-4308-B2D1-F83B862F256F}';
  PIDREALBEATPOSITION      : TGUID = '{432E03AE-D4D3-44CD-A0CD-AFE2D8F05CDB}';
  PIDBSG                   : TGUID = '{875060E8-48ED-4526-85D0-176FBF375CFA}';
  PIDNOW                   : TGUID = '{B72D73ED-983E-433E-B8AE-8DF0D8948132}';
  }
  {%ENDREGION}
  {%REGION Names}
  BPMOUTPUTNAME                           = 'BPM';
  BPMEXISTSOUTPUTNAME                     = 'BPM gefunden';
  BEATPOSITIONOUTPUTNAME                  = 'Beatposition';
  BEATMAXOUTPUTNAME                       = 'BeatMax';
  BEATTRIGGEROUTPUTNAME                   = 'Beat';
  BEATSTOPCALLEROUTPUTNAME                = 'Anschluss';
  FREQUENCYBEATTRIGGERFREQUENCYNAME       = 'Frequenzband';
  FREQUENCYBEATTRIGGEROUTPUTNAME          = 'Beat';
  WAVEDATAOUTPUTNAME                      = 'Wavedaten';
  FFTOUTPUTNAME                           = 'FFT';
  BUFFER10USEDCOUNTNAME                   = 'Verwendet';
  BUFFER10REPEATCOUNTNAME                 = 'Wiederholen';
  BUFFER10VALUE1NAME                      = 'Wert 1';
  BUFFER10VALUE2NAME                      = 'Wert 2';
  BUFFER10VALUE3NAME                      = 'Wert 3';
  BUFFER10VALUE4NAME                      = 'Wert 4';
  BUFFER10VALUE5NAME                      = 'Wert 5';
  BUFFER10VALUE6NAME                      = 'Wert 6';
  BUFFER10VALUE7NAME                      = 'Wert 7';
  BUFFER10VALUE8NAME                      = 'Wert 8';
  BUFFER10VALUE9NAME                      = 'Wert 9';
  BUFFER10VALUE10NAME                     = 'Wert 10';
  BUFFER10OUTPUTNAME                      = 'Puffer';
  MULTIBUFFERERINPUTNAME                  = 'Input';
  MULTIBUFFEREROUTPUT1NAME                = 'Ausgang 1';
  MULTIBUFFEREROUTPUT2NAME                = 'Ausgang 2';
  MULTIBUFFEREROUTPUT3NAME                = 'Ausgang 3';
  MULTIBUFFEREROUTPUT4NAME                = 'Ausgang 4';
  BUFFERCUTTERSTARTNAME                   = 'Anfang';
  BUFFERCUTTERENDNAME                     = 'Ende';
  BUFFERCUTTERINPUTNAME                   = 'Input';
  BUFFERCUTTEROUTPUTNAME                  = 'Output';
  BUFFERCONNECTORINPUT1NAME               = 'Buffer 1';
  BUFFERCONNECTORINPUT2NAME               = 'Buffer 2';
  BUFFERCONNECTORINPUT3NAME               = 'Buffer 3';
  BUFFERCONNECTORINPUT4NAME               = 'Buffer 4';
  BUFFERCONNECTORINPUT5NAME               = 'Buffer 5';
  BUFFERCONNECTOROUTPUTNAME               = 'Output';
  LINEARBUFFERSTARTNAME                   = 'Start';
  LINEARBUFFERENDNAME                     = 'Ende';
  LINEARBUFFERSIZENAME                    = 'Größe';
  LINEARBUFFEROUTPUTNAME                  = 'Output';
  BUFFERGETTERINPUTNAME                   = 'Puffer';
  BUFFERGETTERINDEXNAME                   = 'Index';
  BUFFERGETTEROUTPUTNAME                  = 'Output';
  RELATIVEBUFFERGETTERINPUTNAME           = 'Puffer';
  RELATIVEBUFFERGETTERPOSITIONNAME        = 'Position';
  RELATIVEBUFFERGETTEROUTPUTNAME          = 'Output';
  BUFFERCALCULATIONINPUTNAME              = 'Buffer Input';
  BUFFERCALCULATIONCALCULATIONINPUTNAME   = 'Calculation Input';
  BUFFERCALCULATIONOUTPUTNAME             = 'Buffer Output';
  BUFFERCALCULATIONCALCULATIONOUTPUTNAME  = 'Calculation Output';
  BUFFERCALCULATIONCALLNEXTNAME           = 'Next Output';
  SMOOTHERINPUTNAME                       = 'Input';
  SMOOTHERINTENSITYNAME                   = 'Faktor';
  SMOOTHEROUTPUTNAME                      = 'Output';
  REALBEATPOSITIONOUTPUTNAME              = 'Beatposition';
  BSGMAXNAME                              = 'Max per Beat';
  BSGSPEEDNAME                            = 'Speed';
  NOWSECONDNAME                           = 'Second';
  NOWMINUTENAME                           = 'Minute';
  NOWHOURNAME                             = 'Hour';
  NOWDAYNAME                              = 'Day';
  NOWMONTHNAME                            = 'Month';
  NOWYEARNAME                             = 'Year';
  SETINPUTNAME                            = 'Input';
  SETOUTPUTNAME                           = 'Output';
  {%ENDREGION}


procedure Register;

implementation

{%REGION TBPM}

procedure BPMMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBPM(Context)
    do FOutput.&Set(AdvSpectrumData.BPM);
end;

procedure CreateBPM(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBPM.Create(APrototype);
end;

constructor TBPM.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BPMMainCalled, Self, Environment.Thread);
  FOutput:=FloatInputs[BPMOUTPUTNAME];
end;

destructor TBPM.Destroy;
begin
  FMain.RemoveListener(@BPMMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBPMExists}

procedure BPMExistsMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBPMExists(Context)
    do FOutput.&Set(AdvSpectrumData.BPMFound = bpmTrue);
end;

procedure CreateBPMExists(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBPMExists.Create(APrototype);
end;

constructor TBPMExists.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BPMExistsMainCalled, Self, Environment.Thread);
  FOutput:=BooleanInputs[BPMEXISTSOUTPUTNAME];
end;

destructor TBPMExists.Destroy;
begin
  FMain.RemoveListener(@BPMExistsMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBeatposition}

procedure BeatpositionMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBeatposition(Context) do begin
    FOutput.&Set(AdvSpectrumData.Beat.Pos);
  end;
end;

procedure CreateBeatposition(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBeatposition.Create(APrototype);
end;

constructor TBeatposition.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BeatpositionMainCalled, Self, Environment.Thread);
  FOutput:=IntegerInputs[BEATPOSITIONOUTPUTNAME];
end;

destructor TBeatposition.Destroy;
begin
  FMain.RemoveListener(@BeatpositionMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBeatMax}

procedure BeatMaxMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBeatMax(Context)
    do FOutput.&Set(AdvSpectrumData.Beat.Max);
end;

procedure CreateBeatMax(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBeatMax.Create(APrototype);
end;

constructor TBeatMax.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BeatMaxMainCalled, Self, Environment.Thread);
  FOutput:=IntegerInputs[BEATMAXOUTPUTNAME];
end;

destructor TBeatMax.Destroy;
begin
  FMain.RemoveListener(@BeatMaxMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBeatTrigger}

procedure BeatTriggerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBeatTrigger(Context) do begin
    FOutput.&Set(not AdvSpectrumData.Beat.Free);
  end;
end;

procedure CreateBeatTrigger(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBeatTrigger.Create(APrototype);
end;

constructor TBeatTrigger.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BeatTriggerMainCalled, Self, Environment.Thread);
  FOutput:=BooleanInputs[BEATTRIGGEROUTPUTNAME];
end;

destructor TBeatTrigger.Destroy;
begin
  FMain.RemoveListener(@BeatTriggerMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBeatStopCaller}

procedure BeatStopCallerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AIsBeat: Boolean;
begin
  with TBeatStopCaller(Context) do begin
    AIsBeat:=AdvSpectrumData.Beat.Togg;
    if FIsBeat<>AIsBeat then begin
      FIsBeat:=AIsBeat;
      FOutput.&Set;
    end;
  end;
end;

procedure CreateBeatStopCaller(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBeatStopCaller.Create(APrototype);
end;

constructor TBeatStopCaller.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FIsBeat:=false;
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BeatStopCallerMainCalled, Self, Environment.Thread);
  FOutput:=CallInputs[BEATSTOPCALLEROUTPUTNAME];
end;

destructor TBeatStopCaller.Destroy;
begin
  FMain.RemoveListener(@BeatStopCallerMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TFrequencyBeatTrigger}

procedure FrequencyBeatTriggerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AFrequency: Integer;
begin
  with TFrequencyBeatTrigger(Context) do begin
    AFrequency:=FFrequency;
    if (AFrequency >= 0) or (AFrequency < 32)
      then FOutput.&Set(AdvSpectrumData.BeatAt[AFrequency]);
  end;
end;

procedure CreateFrequencyBeatTrigger(APrototype: IPVisualisationPrototype); cdecl;
begin
  TFrequencyBeatTrigger.Create(APrototype);
end;

constructor TFrequencyBeatTrigger.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@FrequencyBeatTriggerMainCalled, Self, Environment.Thread);
  FFrequency:=IntegerInputs[FREQUENCYBEATTRIGGERFREQUENCYNAME];
  FOutput:=BooleanInputs[FREQUENCYBEATTRIGGEROUTPUTNAME];
end;

destructor TFrequencyBeatTrigger.Destroy;
begin
  FMain.RemoveListener(@FrequencyBeatTriggerMainCalled, Self);
  FMain:=nil;
  FFrequency:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TWavedata}

procedure WavedataMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TWavedata(Context) do begin
    FOutput.&Set(AdvSpectrumData.GetBuffer2(WaveDataBI, 0));
  end;
end;

procedure CreateWavedata(APrototype: IPVisualisationPrototype); cdecl;
begin
  TWavedata.Create(APrototype);
end;

constructor TWavedata.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@WavedataMainCalled, Self, Environment.Thread);
  FOutput:=BufferInputs[WAVEDATAOUTPUTNAME];
end;

destructor TWavedata.Destroy;
begin
  FMain.RemoveListener(@WavedataMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TFFT}

procedure FFTMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TFFT(Context) do begin
    FOutput.&Set(AdvSpectrumData.GetLevelBuffer2(0));
  end;
end;

procedure CreateFFT(APrototype: IPVisualisationPrototype); cdecl;
begin
  TFFT.Create(APrototype);
end;

constructor TFFT.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@FFTMainCalled, Self, Environment.Thread);
  FOutput:=BufferInputs[FFTOUTPUTNAME];
end;

destructor TFFT.Destroy;
begin
  FMain.RemoveListener(@FFTMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBuffer10Value}

procedure Buffer10ValueChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I: Integer;
begin
  with TBuffer10Value(Context) do with Owner do begin
    if FBuffer<>nil
      then with Owner
        do for I:=0 to FLastRepeatCount-1
          do FBuffer[(I*FLastUsedCount) + Index]:=Param;
    FOutput.&Set(FBuffer);
  end;
end;

constructor TBuffer10Value.Create(AParam: IPFloat; AIndex: Integer; AOwner: TBuffer10; AEnvironment: IPVisualisationEnvironment);
begin
  inherited Create;
  FParam:=AParam;
  FIndex:=AIndex;
  FOwner:=AOwner;
  FParam.AddListener(@Buffer10ValueChanged, Self, AEnvironment.Thread);
end;

destructor TBuffer10Value.Destroy;
begin
  FParam.RemoveListener(@Buffer10ValueChanged, Self);
  FParam:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBuffer10}

procedure Buffer10Changed(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBuffer10(Context)
    do DoInit;
end;

procedure CreateBuffer10(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBuffer10.Create(APrototype);
end;

constructor TBuffer10.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FBuffer:=nil;
  FUsedCount:=IntegerInputs[BUFFER10USEDCOUNTNAME];
  FRepeatCount:=IntegerInputs[BUFFER10REPEATCOUNTNAME];
  FUsedCount.AddListener(@Buffer10Changed, Self, Environment.Thread);
  FRepeatCount.AddListener(@Buffer10Changed, Self, Environment.Thread);
  FOutput:=BufferInputs[BUFFER10OUTPUTNAME];
  //init values
  FValues[0]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE1NAME], 0, Self, Environment);
  FValues[1]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE2NAME], 1, Self, Environment);
  FValues[2]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE3NAME], 2, Self, Environment);
  FValues[3]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE4NAME], 3, Self, Environment);
  FValues[4]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE5NAME], 4, Self, Environment);
  FValues[5]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE6NAME], 5, Self, Environment);
  FValues[6]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE7NAME], 6, Self, Environment);
  FValues[7]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE8NAME], 7, Self, Environment);
  FValues[8]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE9NAME], 8, Self, Environment);
  FValues[9]:=TBuffer10Value.Create(FloatInputs[BUFFER10VALUE10NAME], 9, Self, Environment);
  //init
  DoInit;
end;

destructor TBuffer10.Destroy;
var
  I: Integer;
begin
  FRepeatCount.RemoveListener(@Buffer10Changed, Self);
  FUsedCount.RemoveListener(@Buffer10Changed, Self);
  FUsedCount:=nil;
  FRepeatCount:=nil;
  for I:=0 to 9
    do FValues[I].Destroy;
  FBuffer:=nil;
  inherited Destroy;
end;

procedure TBuffer10.DoInit;
var
  I,J: Integer;
begin
  FLastUsedCount:=FUsedCount;
  FLastRepeatCount:=FRepeatCount;
  Assert((FLastUsedCount >= 0) and (FLastUsedCount <= 10) and (FLastRepeatCount >= 0));
  FBuffer:=EmptyBuffer.Resize(FLastUsedCount * FLastRepeatCount);
  for I:=0 to FLastRepeatCount-1
    do for J:=0 to FLastUsedCount-1
      do FBuffer[(I*FLastUsedCount)+J]:=FValues[J].Param;
  FOutput.&Set(FBuffer);
end;

procedure TBuffer10.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID = ParamID(BUFFER10USEDCOUNTNAME, vInteger)
    then IPIntegerSettings(ASettings).SetBounds(0, 10)
    else if ASettings.Param.ID = ParamID(BUFFER10REPEATCOUNTNAME, vInteger)
      then IPIntegerSettings(ASettings).SetBounds(0, MaxInt)
      else inherited GetInput(ASettings);
end;

{%ENDREGION}
{%REGION TMultiBufferer}

procedure MultiBuffererInputChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ABuffer: IVFloatBuffer;
begin
  with TMultiBufferer(Context) do begin
    ABuffer:=FInput;
    FOutputs[0].&Set(ABuffer);
    FOutputs[1].&Set(ABuffer);
    FOutputs[2].&Set(ABuffer);
    FOutputs[3].&Set(ABuffer);
  end;
end;

procedure CreateMultiBufferer(APrototype: IPVisualisationPrototype); cdecl;
begin
  TMultiBufferer.Create(APrototype);
end;

constructor TMultiBufferer.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FOutputs[0]:=BufferInputs[MULTIBUFFEREROUTPUT1NAME];
  FOutputs[1]:=BufferInputs[MULTIBUFFEREROUTPUT2NAME];
  FOutputs[2]:=BufferInputs[MULTIBUFFEREROUTPUT3NAME];
  FOutputs[3]:=BufferInputs[MULTIBUFFEREROUTPUT4NAME];
  FInput:=BufferInputs[MULTIBUFFERERINPUTNAME];
  FInput.AddListener(@MultiBuffererInputChanged, Self, Environment.Thread);
end;

destructor TMultiBufferer.Destroy;
var
  I: Integer;
begin
  FInput.RemoveListener(@MultiBuffererInputChanged, Self);
  FInput:=nil;
  for I:=0 to 3
    do FOutputs[I]:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBufferCutter}

function DoCutBuffer(ABuffer: IVFloatBuffer; AStart, AEnd: Integer): IVFloatBuffer;
var
  ACount: Integer;
begin
  Assert(AStart >= 0);
  ACount:=AEnd - AStart;
  if ACount < 0 then ACount:=0;
  if AStart < ABuffer.Size then begin
    if AStart + ACount > ABuffer.Size
      then ACount:=ABuffer.Size - AStart;
    Result:=ABuffer.Cut(ACount, AStart);
  end else Result:=EmptyBuffer;
end;

procedure BufferCutterChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBufferCutter(Context)
    do FOutput.&Set(DoCutBuffer(FInput, FStart, FEnd));
end;

procedure CreateBufferCutter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBufferCutter.Create(APrototype);
end;

constructor TBufferCutter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FStart:=IntegerInputs[BUFFERCUTTERSTARTNAME];
  FEnd:=IntegerInputs[BUFFERCUTTERENDNAME];
  FInput:=BufferInputs[BUFFERCUTTERINPUTNAME];
  FOutput:=BufferInputs[BUFFERCUTTEROUTPUTNAME];
  FStart.AddListener(@BufferCutterChanged, Self, Environment.Thread);
  FEnd.AddListener(@BufferCutterChanged, Self, Environment.Thread);
  FInput.AddListener(@BufferCutterChanged, Self, Environment.Thread);
end;

destructor TBufferCutter.Destroy;
begin
  FStart.RemoveListener(@BufferCutterChanged, Self);
  FEnd.RemoveListener(@BufferCutterChanged, Self);
  FInput.RemoveListener(@BufferCutterChanged, Self);
  FStart:=nil;
  FEnd:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

procedure TBufferCutter.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if (ASettings.Param.ID = ParamID(BUFFERCUTTERSTARTNAME, vInteger)) or (ASettings.Param.ID = ParamID(BUFFERCUTTERENDNAME, vInteger))
    then IPIntegerSettings(ASettings).SetBounds(0, MaxInt)
    else inherited GetInput(ASettings);
end;

{%ENDREGION}
{%REGION TRelativeBufferCutter}

procedure RelativeBufferCutterChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ABufferSize: Real;
  AInput     : TVBuffer;
begin
  with TRelativeBufferCutter(Context) do begin
    AInput:=FInput.Value;
    ABufferSize:=AInput.Size;
    FOutput.&Set(DoCutBuffer(AInput,
      Round((FStart / 100.0)*ABufferSize),
      Round((FEnd / 100.0)*ABufferSize)));
  end;
end;

procedure CreateRelativeBufferCutter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRelativeBufferCutter.Create(APrototype);
end;

constructor TRelativeBufferCutter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FStart:=FloatInputs[BUFFERCUTTERSTARTNAME];
  FEnd:=FloatInputs[BUFFERCUTTERENDNAME];
  FInput:=BufferInputs[BUFFERCUTTERINPUTNAME];
  FOutput:=BufferInputs[BUFFERCUTTEROUTPUTNAME];
  FStart.AddListener(@RelativeBufferCutterChanged, Self, Environment.Thread);
  FEnd.AddListener(@RelativeBufferCutterChanged, Self, Environment.Thread);
  FInput.AddListener(@RelativeBufferCutterChanged, Self, Environment.Thread);
end;

destructor TRelativeBufferCutter.Destroy;
begin
  FStart.RemoveListener(@RelativeBufferCutterChanged, Self);
  FEnd.RemoveListener(@RelativeBufferCutterChanged, Self);
  FInput.RemoveListener(@RelativeBufferCutterChanged, Self);
  FStart:=nil;
  FEnd:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

procedure TRelativeBufferCutter.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if (ASettings.Param.ID = ParamID(BUFFERCUTTERSTARTNAME, vFloat)) or (ASettings.Param.ID = ParamID(BUFFERCUTTERENDNAME, vFloat))
    then IPFloatSettings(ASettings).SetBounds(0.0, 100.0, 0.0)
    else inherited GetInput(ASettings);
end;

{%ENDREGION}
{%REGION TBufferConnector}

procedure BufferConnectorMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ASize: Cardinal;
begin
  with TBufferConnector(Context) do begin
    ASize:=ExpectedBufferSize;
    if FBuffer.Size=ASize
      then UpdateBuffer;
  end;
end;

procedure BufferConnectorInputChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I: Integer;
begin
  with TBufferConnector(Context) do begin
    for I:=0 to 4
      do FStoredInputs[I]:=FInputs[I].Value;
    UpdateBufferSize(ExpectedBufferSize);
  end;
end;

procedure CreateBufferConnector(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBufferConnector.Create(APrototype);
end;

function TBufferConnector.ExpectedBufferSize: Cardinal; inline;
begin
  Result:=FStoredInputs[0].Size
      + FStoredInputs[1].Size
      + FStoredInputs[2].Size
      + FStoredInputs[3].Size
      + FStoredInputs[4].Size;
end;

procedure TBufferConnector.UpdateBufferSize(ASize: Cardinal); inline;
begin
  FBuffer:=EmptyBuffer.Resize(ASize);
  UpdateBuffer;
  FOutput.&Set(FBuffer);
end;

procedure TBufferConnector.UpdateBuffer;
var
  ABuffer: IVFloatBuffer;
  I, J, K: Integer;
begin
  K:=0;
  for I:=0 to 4 do begin
    ABuffer:=FStoredInputs[I];
    for J:=0 to ABuffer.Size-1 do begin
      FBuffer[K]:=ABuffer[J];
      Inc(K);
    end;
  end;
end;

constructor TBufferConnector.Create(APrototype: IPVisualisationPrototype);
var
  I: Integer;
begin
  inherited Create(APrototype);
  for I:=0 to 4
    do FStoredInputs[I]:=EmptyBuffer;

  FMain:=CallInputs[MAININPUTNAME];
  FOutput:=BufferInputs[BUFFERCONNECTOROUTPUTNAME];
  FInputs[0]:=BufferInputs[BUFFERCONNECTORINPUT1NAME];
  FInputs[1]:=BufferInputs[BUFFERCONNECTORINPUT2NAME];
  FInputs[2]:=BufferInputs[BUFFERCONNECTORINPUT3NAME];
  FInputs[3]:=BufferInputs[BUFFERCONNECTORINPUT4NAME];
  FInputs[4]:=BufferInputs[BUFFERCONNECTORINPUT5NAME];
  FMain.AddListener(@BufferConnectorMainCalled, Self, Environment.Thread);
  FInputs[0].AddListener(@BufferConnectorInputChanged, Self, Environment.Thread);
  FInputs[1].AddListener(@BufferConnectorInputChanged, Self, Environment.Thread);
  FInputs[2].AddListener(@BufferConnectorInputChanged, Self, Environment.Thread);
  FInputs[3].AddListener(@BufferConnectorInputChanged, Self, Environment.Thread);
  FInputs[4].AddListener(@BufferConnectorInputChanged, Self, Environment.Thread);

  FBuffer:=EmptyBuffer;
end;

destructor TBufferConnector.Destroy;
var
  I: Integer;
begin
  FMain.RemoveListener(@BufferConnectorMainCalled, Self);
  for I:=0 to 4
    do FInputs[I].RemoveListener(@BufferConnectorInputChanged, Self);
  FMain:=nil;
  for I:=0 to 4
    do FInputs[I]:=nil;
  FOutput:=nil;

  FBuffer:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TLinearBuffer}

procedure LinearBufferValueChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TLinearBuffer(Context) do begin
    UpdateBuffer;
    FOutput.Value:=FBuffer;
  end;
end;

procedure LinearBufferSizeChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TLinearBuffer(Context) do begin
    FBuffer:=EmptyBuffer.Resize(IChangedInteger(SenderData).Value);
    UpdateBuffer;
    FOutput.Value:=FBuffer;
  end;
end;

procedure CreateLinearBuffer(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLinearBuffer.Create(APrototype);
end;

procedure TLinearBuffer.UpdateBuffer;
var
  AVal , AStep: TVFloat;
  I           : Cardinal;
begin
  if FBuffer.Size > 0 then begin;
    AStep:=(FEnd - FStart) / FBuffer.Size;
    AVal:=FStart;
    for I:=0 to FBuffer.Size-1 do begin
      FBuffer[I]:=AVal;
      AVal+=AStep;
    end;
  end;
end;

constructor TLinearBuffer.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FBuffer:=EmptyBuffer;

  FOutput:=BufferInputs[BUFFERCONNECTOROUTPUTNAME];
  FStart:=FloatInputs[LINEARBUFFERSTARTNAME];
  FEnd:=FloatInputs[LINEARBUFFERENDNAME];
  FSize:=IntegerInputs[LINEARBUFFERSIZENAME];
  FSize.AddListener(@LinearBufferSizeChanged, Self, Environment.Thread);
  FStart.AddListener(@LinearBufferValueChanged, Self, Environment.Thread);
  FEnd.AddListener(@LinearBufferValueChanged, Self, Environment.Thread);
end;

destructor TLinearBuffer.Destroy;
begin
  FStart.RemoveListener(@LinearBufferValueChanged, Self);
  FEnd.RemoveListener(@LinearBufferValueChanged, Self);
  FSize.RemoveListener(@LinearBufferSizeChanged, Self);
  FStart:=nil;
  FEnd:=nil;
  FSize:=nil;
  FOutput:=nil;

  FBuffer:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBufferGetter}

procedure BufferGetterMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBufferGetter(Context) do begin
    if (FIndex >= 0) and (FIndex < FBuffer.Value.Size)
      then FOutput.&Set(FBuffer.Value[FIndex.Value])
      else FOutput.&Set(0.0);
  end;
end;

procedure CreateBufferGetter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBufferGetter.Create(APrototype);
end;

constructor TBufferGetter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BufferGetterMainCalled, Self, Environment.Thread);
  FOutput:=FloatInputs[BUFFERGETTEROUTPUTNAME];
  FBuffer:=BufferInputs[BUFFERGETTERINPUTNAME];
  FIndex:=IntegerInputs[BUFFERGETTERINDEXNAME];
end;

destructor TBufferGetter.Destroy;
begin
  FMain.RemoveListener(@BufferGetterMainCalled, Self);
  FMain:=nil;
  FIndex:=nil;
  FBuffer:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

procedure TBufferGetter.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID = ParamID(BUFFERGETTERINDEXNAME, vInteger)
    then IPIntegerSettings(ASettings).SetBounds(0, MaxInt)
    else inherited GetInput(ASettings);
end;

{%ENDREGION}
{%REGION TRelativeBufferGetter}

procedure RelativeBufferGetterMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ASize: Cardinal;
begin
  with TRelativeBufferGetter(Context) do begin
    ASize:=FBuffer.Value.Size;
    if (FPosition >= 0.0) and (FPosition <= 100.0) and (ASize>0)
      then FOutput.&Set(FBuffer.Value[Round(ASize * (FPosition/100.0))])
      else FOutput.&Set(0.0);
  end;
end;

procedure CreateRelativeBufferGetter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRelativeBufferGetter.Create(APrototype);
end;

constructor TRelativeBufferGetter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RelativeBufferGetterMainCalled, Self, Environment.Thread);
  FOutput:=FloatInputs[RELATIVEBUFFERGETTEROUTPUTNAME];
  FBuffer:=BufferInputs[RELATIVEBUFFERGETTERINPUTNAME];
  FPosition:=FloatInputs[RELATIVEBUFFERGETTERPOSITIONNAME];
end;

destructor TRelativeBufferGetter.Destroy;
begin
  FMain.RemoveListener(@RelativeBufferGetterMainCalled, Self);
  FMain:=nil;
  FPosition:=nil;
  FBuffer:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

procedure TRelativeBufferGetter.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID = ParamID(RELATIVEBUFFERGETTERPOSITIONNAME, vFloat)
    then IPFloatSettings(ASettings).SetBounds(0.0, 100.0, 0.0)
    else inherited GetInput(ASettings);
end;

{%ENDREGION}
{%REGION TBufferCalculation}

procedure BufferCalculationMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I           : Integer;
  AInputBuffer: IVFloatBuffer;
begin
  with TBufferCalculation(Context) do begin
    Assert(FBuffer.Size = FInput.Value.Size);
    AInputBuffer:=FInput.Value;

    for I:=0 to AInputBuffer.Size-1 do begin
      FCalculationOutput.Value:=AInputBuffer[I];
      FNextOutput.&Set;
      //FCalculationInput will be updated because it is done in the same thread...
      FBuffer[I]:=FCalculationInput.Value;
    end;
    FOutput.Value:=FBuffer;
  end;
end;

procedure BufferCalculationInputChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  Assert(EmptyBuffer <> nil);
  with TBufferCalculation(Context)
    do FBuffer:=EmptyBuffer.Resize(IChangedBuffer(SenderData).Value.Size);
end;

procedure CreateBufferCalculation(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBufferCalculation.Create(APrototype);
end;

constructor TBufferCalculation.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FBuffer:=EmptyBuffer;

  FMain:=CallInputs[MAININPUTNAME];
  FInput:=BufferInputs[BUFFERCALCULATIONINPUTNAME];
  FCalculationInput:=FloatInputs[BUFFERCALCULATIONCALCULATIONINPUTNAME];
  FOutput:=BufferInputs[BUFFERCALCULATIONOUTPUTNAME];
  FCalculationOutput:=FloatInputs[BUFFERCALCULATIONCALCULATIONOUTPUTNAME];
  FNextOutput:=CallInputs[BUFFERCALCULATIONCALLNEXTNAME];
  FMain.AddListener(@BufferCalculationMainCalled, Self, Environment.Thread);
  FInput.AddListener(@BufferCalculationInputChanged, Self, Environment.Thread);
end;

destructor TBufferCalculation.Destroy;
begin
  FInput.RemoveListener(@BufferCalculationInputChanged, Self);
  FMain.RemoveListener(@BufferCalculationMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FCalculationInput:=nil;
  FOutput:=nil;
  FCalculationOutput:=nil;
  FNextOutput:=nil;

  FBuffer:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TSmoother}

procedure SmootherMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AIntensity: TVFloat;
begin
  with TSmoother(Context) do begin
    AIntensity:=FIntensity;
    FCurrentValue:=((FInput*AIntensity) + (FCurrentValue*(1.0/AIntensity)))/2.0;
    FOutput.Value:=FCurrentValue;
  end;
end;

procedure CreateSmoother(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSmoother.Create(APrototype);
end;

constructor TSmoother.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FCurrentValue:=0.0;

  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SmootherMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[SMOOTHERINPUTNAME];
  FIntensity:=FloatInputs[SMOOTHERINTENSITYNAME];
  FOutput:=FloatInputs[SMOOTHEROUTPUTNAME];
end;

destructor TSmoother.Destroy;
begin
  FMain.RemoveListener(@SmootherMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FIntensity:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRealBeatposition}

procedure RealBeatpositionMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TRealBeatposition(Context)
    do FOutput.Value:=AdvSpectrumData.BeatPos;
end;

procedure CreateRealBeatposition(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRealBeatposition.Create(APrototype);
end;

constructor TRealBeatposition.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RealBeatpositionMainCalled, Self, Environment.Thread);
  FOutput:=FloatInputs[REALBEATPOSITIONOUTPUTNAME];
end;

destructor TRealBeatposition.Destroy;
begin
  FMain.RemoveListener(@RealBeatpositionMainCalled, Self);
  FMain:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBSG}

procedure BSGMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AResult, ANewBeatPos: TVFloat;
begin
  with TBSG(Context) do begin
    ANewBeatPos:=AdvSpectrumData.BeatPos;
    if isNan(FLastBeatPos) then begin
      AResult:=0.0;
    end else begin
      AResult:=ANewBeatPos-FLastBeatPos;
      if AResult<0.0
        then AResult+=AdvSpectrumData.Beat.Max;
      AResult*=FMax;
    end;
    FLastBeatPos:=ANewBeatPos;
    FSpeed.Value:=AResult;
  end;
end;

procedure CreateBSG(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBSG.Create(APrototype);
end;

constructor TBSG.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FLastBeatPos:=NaN;

  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BSGMainCalled, Self, Environment.Thread);
  FMax:=FloatInputs[BSGMAXNAME];
  FSpeed:=FloatInputs[BSGSpeedName];
end;

destructor TBSG.Destroy;
begin
  FMain.RemoveListener(@BSGMainCalled, Self);
  FMain:=nil;
  FMax:=nil;
  FSpeed:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TNow}

procedure NowMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ANow          : TDateTime;
  A1, A2, A3, A4: Word;
begin
  with TNow(Context) do begin
    ANow:=Now;
    DecodeDate(ANow, A1, A2, A3);
    FYear.Value:=A1;
    FMonth.Value:=A2;
    FDay.Value:=A3;
    DecodeTime(ANow, A1, A2, A3, A4);
    FHour.Value:=A1;
    FMinute.Value:=A2;
    FSecond.Value:=A3;
  end;
end;

procedure CreateNow(APrototype: IPVisualisationPrototype); cdecl;
begin
  TNow.Create(APrototype);
end;

constructor TNow.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@NowMainCalled, Self, Environment.Thread);
  FSecond:=IntegerInputs[NOWSECONDNAME];
  FMinute:=IntegerInputs[NOWMINUTENAME];
  FHour:=IntegerInputs[NOWHOURNAME];
  FDay:=IntegerInputs[NOWDAYNAME];
  FMonth:=IntegerInputs[NOWMONTHNAME];
  FYear:=IntegerInputs[NOWYEARNAME];
end;

destructor TNow.Destroy;
begin
  FMain.RemoveListener(@NowMainCalled, Self);
  FMain:=nil;
  FSecond:=nil;
  FMinute:=nil;
  FHour:=nil;
  FDay:=nil;
  FMonth:=nil;
  FYear:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDBPM, @CreateBPM);
    with CreatePreset('BPM', VIDBPM) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BPMOUTPUTNAME, 120.0);
    end;
    RegisterVis(VIDBPMEXISTS, @CreateBPMExists);
    with CreatePreset('BPM Exists', VIDBPMEXISTS) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BPMOUTPUTNAME, false);
    end;
    RegisterVis(VIDBEATPOSITION, @CreateBeatposition);
    with CreatePreset('Beatposition', VIDBEATPOSITION) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BEATPOSITIONOUTPUTNAME, 0);
    end;
    RegisterVis(VIDBEATMAX, @CreateBeatMax);
    with CreatePreset('Beat Max', VIDBEATMAX) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BEATMAXOUTPUTNAME, 4);
    end;
    RegisterVis(VIDBEATTRIGGER, @CreateBeatTrigger);
    with CreatePreset('Beat Trigger', VIDBEATTRIGGER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BEATTRIGGEROUTPUTNAME, false);
    end;
    RegisterVis(VIDBEATSTOPCALLER, @CreateBeatStopCaller);
    with CreatePreset('Beat Stop Caller', VIDBEATSTOPCALLER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BEATSTOPCALLEROUTPUTNAME);
    end;
    RegisterVis(VIDFREQUENCYBEATTRIGGER, @CreateFrequencyBeatTrigger);
    with CreatePreset('Frequency Beat Trigger', VIDFREQUENCYBEATTRIGGER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, FREQUENCYBEATTRIGGERFREQUENCYNAME, 0);
      AddInput(This, FREQUENCYBEATTRIGGEROUTPUTNAME, false);
    end;
    RegisterVis(VIDWAVEDATA, @CreateWavedata);
    with CreatePreset('Wavedata', VIDWAVEDATA) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, WAVEDATAOUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDFFT, @CreateFFT);
    with CreatePreset('FFT', VIDFFT) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, FFTOUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDBUFFER10, @CreateBuffer10);
    with CreatePreset('Buffer 10', VIDBUFFER10) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, BUFFER10USEDCOUNTNAME, 10);
      AddInput(This, BUFFER10REPEATCOUNTNAME, 1);
      AddInput(This, BUFFER10VALUE1NAME, 0.0);
      AddInput(This, BUFFER10VALUE2NAME, 0.0);
      AddInput(This, BUFFER10VALUE3NAME, 0.0);
      AddInput(This, BUFFER10VALUE4NAME, 0.0);
      AddInput(This, BUFFER10VALUE5NAME, 0.0);
      AddInput(This, BUFFER10VALUE6NAME, 0.0);
      AddInput(This, BUFFER10VALUE7NAME, 0.0);
      AddInput(This, BUFFER10VALUE8NAME, 0.0);
      AddInput(This, BUFFER10VALUE9NAME, 0.0);
      AddInput(This, BUFFER10VALUE10NAME, 0.0);
      AddInput(This, BUFFER10OUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDMULTIBUFFERER, @CreateMultibufferer);
    with CreatePreset('Multibufferer', VIDMULTIBUFFERER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, MULTIBUFFERERINPUTNAME, EmptyBuffer);
      AddInput(This, MULTIBUFFEREROUTPUT1NAME, EmptyBuffer);
      AddInput(This, MULTIBUFFEREROUTPUT2NAME, EmptyBuffer);
      AddInput(This, MULTIBUFFEREROUTPUT3NAME, EmptyBuffer);
      AddInput(This, MULTIBUFFEREROUTPUT4NAME, EmptyBuffer);
    end;
    RegisterVis(VIDBUFFERCUTTER, @CreateBufferCutter);
    with CreatePreset('Buffer Cutter', VIDBUFFERCUTTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, BUFFERCUTTERSTARTNAME, 0);
      AddInput(This, BUFFERCUTTERENDNAME, 100000);
      AddInput(This, BUFFERCUTTERINPUTNAME, EmptyBuffer);
      AddInput(This, BUFFERCUTTEROUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDRELATIVEBUFFERCUTTER, @CreateRelativeBufferCutter);
    with CreatePreset('Relative Buffer Cutter', VIDRELATIVEBUFFERCUTTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, BUFFERCUTTERSTARTNAME, 0.0);
      AddInput(This, BUFFERCUTTERENDNAME, 100.0);
      AddInput(This, BUFFERCUTTERINPUTNAME, EmptyBuffer);
      AddInput(This, BUFFERCUTTEROUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDBUFFERCONNECTOR, @CreateBufferConnector);
    with CreatePreset('Buffer Connector', VIDBUFFERCONNECTOR) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BUFFERCONNECTORINPUT1NAME, EmptyBuffer);
      AddInput(This, BUFFERCONNECTORINPUT2NAME, EmptyBuffer);
      AddInput(This, BUFFERCONNECTORINPUT3NAME, EmptyBuffer);
      AddInput(This, BUFFERCONNECTORINPUT4NAME, EmptyBuffer);
      AddInput(This, BUFFERCONNECTORINPUT5NAME, EmptyBuffer);
      AddInput(This, BUFFERCONNECTOROUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDLINEARBUFFER, @CreateLinearBuffer);
    with CreatePreset('Linear Buffer', VIDLINEARBUFFER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, LINEARBUFFERSTARTNAME, 0.0);
      AddInput(This, LINEARBUFFERENDNAME, 100.0);
      AddInput(This, LINEARBUFFERSIZENAME, 100);
      AddInput(This, LINEARBUFFEROUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDBUFFERGETTER, @CreateBufferGetter);
    with CreatePreset('Buffer Getter', VIDBUFFERGETTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BUFFERGETTERINPUTNAME, EmptyBuffer);
      AddInput(This, BUFFERGETTERINDEXNAME, 0);
      AddInput(This, BUFFERGETTEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDRELATIVEBUFFERGETTER, @CreateRelativeBufferGetter);
    with CreatePreset('Relative Buffer Getter', VIDRELATIVEBUFFERGETTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, RELATIVEBUFFERGETTERINPUTNAME, EmptyBuffer);
      AddInput(This, RELATIVEBUFFERGETTERPOSITIONNAME, 0.0);
      AddInput(This, RELATIVEBUFFERGETTEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDBUFFERCALCULATION, @CreateBufferCalculation);
    with CreatePreset('Buffer Calculation', VIDBUFFERCALCULATION) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BUFFERCALCULATIONINPUTNAME, EmptyBuffer);
      AddInput(This, BUFFERCALCULATIONCALCULATIONINPUTNAME, 0.0);
      AddInput(This, BUFFERCALCULATIONOUTPUTNAME, EmptyBuffer);
      AddInput(This, BUFFERCALCULATIONCALCULATIONOUTPUTNAME, 0.0);
      AddInput(This, BUFFERCALCULATIONCALLNEXTNAME);
    end;
    RegisterVis(VIDSMOOTHER, @CreateSmoother);
    with CreatePreset('Smoother', VIDSMOOTHER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Buffer');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SMOOTHERINPUTNAME, EmptyBuffer);
      AddInput(This, SMOOTHERINTENSITYNAME, 2.0);
      AddInput(This, SMOOTHEROUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDREALBEATPOSITION, @CreateRealBeatPosition);
    with CreatePreset('Beat Position (Real)', VIDREALBEATPOSITION) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, REALBEATPOSITIONOUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDBSG, @CreateBSG);
    with CreatePreset('BSG', VIDBSG) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BSGMAXNAME, 1.0);
      AddInput(This, BSGSPEEDNAME, 1.0);
    end;
    RegisterVis(VIDNOW, @CreateNow);
    with CreatePreset('Now', VIDNOW) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Time');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, NOWSECONDNAME, 0);
      AddInput(This, NOWMINUTENAME, 0);
      AddInput(This, NOWHOURNAME, 0);
      AddInput(This, NOWDAYNAME, 0);
      AddInput(This, NOWMONTHNAME, 0);
      AddInput(This, NOWYEARNAME, 2014);
    end;
  end;
end;

{%ENDREGION}

end.

