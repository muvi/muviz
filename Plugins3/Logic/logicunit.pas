unit LogicUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags,
  SimpleVis, VisAddInput, ParamOp, Math, VisualisationUtils;

type
  TIf                  = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FCondition  : IPBoolean;
    FConsequence: IPCall;
    FAlternative: IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TGeneralLoop         = class (TVisualisationEvents)
  private
    FMain           : IPCall;
  protected
    FCondition      : IPBoolean;
    FCallLoopContent: IPCall;
    procedure MainCalled; virtual; abstract;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TWhile               = class (TGeneralLoop)
  protected
    procedure MainCalled; override;
  end;

  TRepeat              = class (TGeneralLoop)
  protected
    procedure MainCalled; override;
  end;

  TGeneralForLoop      = class (TVisualisationEvents)
  private
    FMain           : IPCall;
  protected
    FStart          : IPInteger;
    FEnd            : IPInteger;
    FCallLoopContent: IPCall;
    FIndex          : IPInteger;
    procedure MainCalled; virtual; abstract;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TFor                 = class (TGeneralForLoop)
  private
    FStep: IPInteger;
  protected
    procedure MainCalled; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TForStep1            = class (TGeneralForLoop)
  protected
    procedure MainCalled; override;
  end;

  TForStepNeg1         = class (TGeneralForLoop)
  protected
    procedure MainCalled; override;
  end;

  TGeneralComparison   = class (TVisualisationEvents)
  private
    FValue1: IPFloat;
    FValue2: IPFloat;
    FResult: IPBoolean;
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; virtual; abstract;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TCalledComparison    = class (TGeneralComparison)
  private
    FMain: IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TAutomaticComparison = class (TGeneralComparison)
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TEqualCall           = class (TCalledComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TEqual               = class (TAutomaticComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TNotEqualCall        = class (TCalledComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TNotEqual            = class (TAutomaticComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TLessCall            = class (TCalledComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TLess                = class (TAutomaticComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TGreaterCall         = class (TCalledComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TGreater             = class (TAutomaticComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TLessOrEqualCall     = class (TCalledComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TLessOrEqual         = class (TAutomaticComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TGreaterOrEqualCall  = class (TCalledComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

  TGreaterOrEqual      = class (TAutomaticComparison)
  protected
    function DoCompare(AValue1, AValue2: TVFloat): TVBoolean; override;
  end;

const
  VIDIF                  : TGUID = '{2C75A06B-5A8D-471A-2120-494620202020}';
  VIDIFELSE              : TGUID = '{2C75A06B-5A8D-471A-2120-49462F212020}';
  VIDWHILE               : TGUID = '{2C75A06B-5A8D-471A-2120-5748494C4520}';
  VIDREPEAT              : TGUID = '{2C75A06B-5A8D-471A-2120-524550454154}';
  VIDFORSTEP1            : TGUID = '{2C75A06B-5A8D-471A-2120-464F522B3120}';
  VIDFORSTEPNEG1         : TGUID = '{2C75A06B-5A8D-471A-2120-464F522D3120}';
  VIDFOR                 : TGUID = '{2C75A06B-5A8D-471A-2120-464F52202020}';
  VIDEQUALCALL           : TGUID = '{2C75A06B-5A8D-471A-2120-5265616C3D20}';
  VIDNOTEQUALCALL        : TGUID = '{2C75A06B-5A8D-471A-2120-5265616C3C3E}';
  VIDLESSCALL            : TGUID = '{2C75A06B-5A8D-471A-2120-5265616C3C20}';
  VIDGREATERCALL         : TGUID = '{2C75A06B-5A8D-471A-2120-5265616C3E20}';
  VIDLESSOREQUALCALL     : TGUID = '{2C75A06B-5A8D-471A-2120-5265616C3C3D}';
  VIDGREATEROREQUALCALL  : TGUID = '{2C75A06B-5A8D-471A-2120-5265616C3E3D}';
  VIDEQUAL               : TGUID = '{2C75A06B-5A8D-471A-2141-5265616C3D20}';
  VIDNOTEQUAL            : TGUID = '{2C75A06B-5A8D-471A-2141-5265616C3C3E}';
  VIDLESS                : TGUID = '{2C75A06B-5A8D-471A-2141-5265616C3C20}';
  VIDGREATER             : TGUID = '{2C75A06B-5A8D-471A-2141-5265616C3E20}';
  VIDLESSOREQUAL         : TGUID = '{2C75A06B-5A8D-471A-2141-5265616C3C3D}';
  VIDGREATEROREQUAL      : TGUID = '{2C75A06B-5A8D-471A-2141-5265616C3E3D}';

  {
  PIDIF                  : TGUID = '{D1CE3347-1092-478A-9CD1-9D9067FDEF77}';
  PIDIFELSE              : TGUID = '{DFEFEC60-0761-46E4-B00A-7A370C6BB369}';
  PIDWHILE               : TGUID = '{36481F13-02E7-45E6-9EA4-1F6D11AA24BD}';
  PIDREPEAT              : TGUID = '{ED152F37-F81F-4524-8DE7-CB3ED7CDE1B7}';
  PIDFORSTEP1            : TGUID = '{3F4BFC9D-4E6D-4DA9-A222-0BA8197AE909}';
  PIDFORSTEPNEG1         : TGUID = '{A32F4475-F494-45D8-8430-D4A9B8135669}';
  PIDFOR                 : TGUID = '{C7BAFBE6-AA96-47E1-9492-950924ED0CB7}';
  PIDEQUALCALL           : TGUID = '{6164ED13-1E9C-4751-B347-4D447A75273B}';
  PIDNOTEQUALCALL        : TGUID = '{7478B19A-48F7-4DF9-B22B-2B2740D7807B}';
  PIDLESSCALL            : TGUID = '{BD0B8721-8FB8-406E-99C0-2D03C84996A7}';
  PIDGREATERCALL         : TGUID = '{3FC96F25-C0DC-432F-8CC8-857659412ADA}';
  PIDLESSOREQUALCALL     : TGUID = '{242DC47B-E764-4900-8193-9A6BAC660BD8}';
  PIDGREATEROREQUALCALL  : TGUID = '{B6B07F0F-7F33-4A6E-BAFB-9F62A4833FCF}';
  PIDEQUAL               : TGUID = '{2F7EE91D-2559-43ED-ABF0-FC57BB8B6C8A}';
  PIDNOTEQUAL            : TGUID = '{9FFB65D0-8874-4FB4-AE52-B62C67A4599C}';
  PIDLESS                : TGUID = '{76441912-EF42-4816-80B0-100089A0EB40}';
  PIDGREATER             : TGUID = '{ADC0A4A1-78BA-4411-A281-03F8604BEAC0}';
  PIDLESSOREQUAL         : TGUID = '{BF26AC34-C500-4CF7-9E42-2495CF255B2A}';
  PIDGREATEROREQUAL      : TGUID = '{65391370-B17E-4761-811B-65405B834BF2}';
  }

  CONDITIONNAME       = 'Bedingung';
  CONSEQUENCENAME     = 'Then';
  ALTERNATIVENAME     = 'Else';
  LOOPCONTENTCALLNAME = 'Do';
  INDEXNAME           = 'I';
  FORSTARTNAME        = 'Startwert';
  FORENDNAME          = 'Endwert';
  FORSTEPNAME         = 'Schrittweite';
  COMPAREVALUE1NAME   = 'Wert 1';
  COMPAREVALUE2NAME   = 'Wert 2';
  COMPARERESULTNAME   = 'stimmt';

procedure Register;

implementation

{%REGION TIf}

procedure IfMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TIf(Context) do begin
    if FCondition.Value
      then FConsequence.&Set
      else FAlternative.&Set;
  end;
end;

procedure CreateIf(APrototype: IPVisualisationPrototype); cdecl;
begin
  TIf.Create(APrototype);
end;

constructor TIf.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@IfMainCalled, Self, Environment.Thread);
  FCondition:=BooleanInputs[CONDITIONNAME];
  FConsequence:=CallInputs[CONSEQUENCENAME];
  FAlternative:=CallInputs[ALTERNATIVENAME];
end;

destructor TIf.Destroy;
begin
  FMain.RemoveListener(@IfMainCalled, Self);
  FMain:=nil;
  FCondition:=nil;
  FConsequence:=nil;
  FAlternative:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TGeneralLoop}

procedure GeneralLoopMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGeneralLoop(Context)
    do MainCalled;
end;

constructor TGeneralLoop.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@GeneralLoopMainCalled, Self, Environment.Thread);
  FCondition:=BooleanInputs[CONDITIONNAME];
  FCallLoopContent:=CallInputs[LOOPCONTENTCALLNAME];
end;

destructor TGeneralLoop.Destroy;
begin
  FMain.RemoveListener(@GeneralLoopMainCalled, Self);
  FMain:=nil;
  FCondition:=nil;
  FCallLoopContent:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TWhile}

procedure CreateWhile(APrototype: IPVisualisationPrototype); cdecl;
begin
  TWhile.Create(APrototype);
end;

procedure TWhile.MainCalled;
begin
  while FCondition.Value
    do FCallLoopContent.&Set;
end;

{%ENDREGION}
{%REGION TRepeat}

procedure CreateRepeat(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRepeat.Create(APrototype);
end;

procedure TRepeat.MainCalled;
begin
  repeat
    FCallLoopContent.&Set;
  until FCondition.Value;
end;

{%ENDREGION}
{%REGION TGeneralForLoop}

procedure GeneralForLoopMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGeneralForLoop(Context)
    do MainCalled;
end;

constructor TGeneralForLoop.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@GeneralForLoopMainCalled, Self, Environment.Thread);
  FStart:=IntegerInputs[FORSTARTNAME];
  FEnd:=IntegerInputs[FORENDNAME];
  FIndex:=IntegerInputs[INDEXNAME];
  FCallLoopContent:=CallInputs[LOOPCONTENTCALLNAME];
end;

destructor TGeneralForLoop.Destroy;
begin
  FMain.RemoveListener(@GeneralForLoopMainCalled, Self);
  FMain:=nil;
  FStart:=nil;
  FEnd:=nil;
  FIndex:=nil;
  FCallLoopContent:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TFor}

procedure CreateFor(APrototype: IPVisualisationPrototype); cdecl;
begin
  TFor.Create(APrototype);
end;

constructor TFor.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FStep:=IntegerInputs[FORSTEPNAME];
end;

destructor TFor.Destroy;
begin
  FStep:=nil;
  inherited Destroy;
end;

procedure TFor.MainCalled;
var
  AMax, I, J: Integer;
begin
  AMax:=((FEnd - FStart) div FStep.Value) + FStart;
  for I:=FStart to AMax do begin
    J:=I*FStep;
    FIndex.Value:=J;
    FCallLoopContent.&Set;
  end;
end;

{%ENDREGION}
{%REGION TForStep1}

procedure CreateForStep1(APrototype: IPVisualisationPrototype); cdecl;
begin
  TForStep1.Create(APrototype);
end;

procedure TForStep1.MainCalled;
var
  I: Integer;
begin
  for I:=FStart to FEnd do begin
    FIndex.Value:=I;
    FCallLoopContent.&Set;
  end;
end;

{%ENDREGION}
{%REGION TStepNeg1}

procedure CreateForStepNeg1(APrototype: IPVisualisationPrototype); cdecl;
begin
  TForStepNeg1.Create(APrototype);
end;

procedure TForStepNeg1.MainCalled;
var
  I: Integer;
begin
  for I:=FStart downto FEnd do begin
    FIndex.Value:=I;
    FCallLoopContent.&Set;
  end;
end;

{%ENDREGION}
{%REGION TGeneralComparison}

procedure ComparisonCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGeneralComparison(Context)
    do FResult.Value:=DoCompare(FValue1, FValue2);
end;

constructor TGeneralComparison.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FValue1:=FloatInputs[COMPAREVALUE1NAME];
  FValue2:=FloatInputs[COMPAREVALUE2NAME];
  FResult:=BooleanInputs[COMPARERESULTNAME];
end;

destructor TGeneralComparison.Destroy;
begin
  FResult:=nil;
  FValue1:=nil;
  FValue2:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TCalledComparison}

constructor TCalledComparison.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ComparisonCalled, Self, Environment.Thread);
end;

destructor TCalledComparison.Destroy;
begin
  FMain.RemoveListener(@ComparisonCalled, Self);
  FMain:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TAutomaticComparison}

constructor TAutomaticComparison.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FValue1.AddListener(@ComparisonCalled, Self, Environment.Thread);
  FValue2.AddListener(@ComparisonCalled, Self, Environment.Thread);
end;

destructor TAutomaticComparison.Destroy;
begin
  FValue1.RemoveListener(@ComparisonCalled, Self);
  FValue2.RemoveListener(@ComparisonCalled, Self);
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TEqualCall}

procedure CreateEqualCall(APrototype: IPVisualisationPrototype); cdecl;
begin
  TEqualCall.Create(APrototype);
end;

function TEqualCall.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=isZero(AValue1 - AValue2);
end;

{%ENDREGION}
{%REGION TEqual}

procedure CreateEqual(APrototype: IPVisualisationPrototype); cdecl;
begin
  TEqual.Create(APrototype);
end;

function TEqual.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=isZero(AValue1 - AValue2);
end;

{%ENDREGION}
{%REGION TNotEqualCall}

procedure CreateNotEqualCall(APrototype: IPVisualisationPrototype); cdecl;
begin
  TNotEqualCall.Create(APrototype);
end;

function TNotEqualCall.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 <> AValue2;
end;

{%ENDREGION}
{%REGION TNotEqual}

procedure CreateNotEqual(APrototype: IPVisualisationPrototype); cdecl;
begin
  TNotEqual.Create(APrototype);
end;

function TNotEqual.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 <> AValue2;
end;

{%ENDREGION}
{%REGION TLessCall}

procedure CreateLessCall(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLessCall.Create(APrototype);
end;

function TLessCall.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 < AValue2;
end;

{%ENDREGION}
{%REGION TLess}

procedure CreateLess(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLess.Create(APrototype);
end;

function TLess.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 < AValue2;
end;

{%ENDREGION}
{%REGION TGreaterCall}

procedure CreateGreaterCall(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGreaterCall.Create(APrototype);
end;

function TGreaterCall.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 > AValue2;
end;

{%ENDREGION}
{%REGION TGreater}

procedure CreateGreater(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGreater.Create(APrototype);
end;

function TGreater.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 > AValue2;
end;

{%ENDREGION}
{%REGION TLessOrEqualCall}

procedure CreateLessOrEqualCall(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLessOrEqualCall.Create(APrototype);
end;

function TLessOrEqualCall.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 <= AValue2;
end;

{%ENDREGION}
{%REGION TLessOrEqual}

procedure CreateLessOrEqual(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLessOrEqual.Create(APrototype);
end;

function TLessOrEqual.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 <= AValue2;
end;

{%ENDREGION}
{%REGION TGreaterOrEqualCall}

procedure CreateGreaterOrEqualCall(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGreaterOrEqualCall.Create(APrototype);
end;

function TGreaterOrEqualCall.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 >= AValue2;
end;

{%ENDREGION}
{%REGION TGreaterOrEqual}

procedure CreateGreaterOrEqual(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGreaterOrEqual.Create(APrototype);
end;

function TGreaterOrEqual.DoCompare(AValue1, AValue2: TVFloat): TVBoolean;
begin
  Result:=AValue1 >= AValue2;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDIF, @CreateIf);
    with CreatePreset('if', VIDIF) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, CONDITIONNAME, false);
      AddInput(This, CONSEQUENCENAME);
    end;
    RegisterVis(VIDIFELSE, @CreateIf);
    with CreatePreset('if with else', VIDIFELSE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, CONDITIONNAME, false);
      AddInput(This, CONSEQUENCENAME);
      AddInput(This, ALTERNATIVENAME);
    end;
    RegisterVis(VIDWHILE, @CreateWhile);
    with CreatePreset('while', VIDWHILE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, CONDITIONNAME, false);
      AddInput(This, LOOPCONTENTCALLNAME);
    end;
    RegisterVis(VIDREPEAT, @CreateRepeat);
    with CreatePreset('repeat', VIDREPEAT) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, CONDITIONNAME, true);
      AddInput(This, LOOPCONTENTCALLNAME);
    end;
    RegisterVis(VIDFORSTEP1, @CreateForStep1);
    with CreatePreset('for (step 1)', VIDFORSTEP1) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, FORSTARTNAME, 0);
      AddInput(This, FORENDNAME, 10);
      AddInput(This, LOOPCONTENTCALLNAME);
      AddInput(This, INDEXNAME, 0);
    end;
    RegisterVis(VIDFORSTEPNEG1, @CreateForStepNeg1);
    with CreatePreset('for (step -1)', VIDFORSTEPNEG1) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, FORSTARTNAME, 0);
      AddInput(This, FORENDNAME, 10);
      AddInput(This, LOOPCONTENTCALLNAME);
      AddInput(This, INDEXNAME, 0);
    end;
    RegisterVis(VIDFOR, @CreateFor);
    with CreatePreset('for', VIDFOR) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Structure.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, FORSTARTNAME, 0);
      AddInput(This, FORENDNAME, 10);
      AddInput(This, FORSTEPNAME, 1);
      AddInput(This, LOOPCONTENTCALLNAME);
      AddInput(This, INDEXNAME, 0);
    end;
    //called comparing operators
    RegisterVis(VIDEQUALCALL, @CreateEqualCall);
    with CreatePreset('= (called)', VIDEQUALCALL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDNOTEQUALCALL, @CreateNotEqualCall);
    with CreatePreset('<> (called)', VIDNOTEQUALCALL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDLESSCALL, @CreateLessCall);
    with CreatePreset('< (called)', VIDLESSCALL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDGREATERCALL, @CreateGreaterCall);
    with CreatePreset('> (called)', VIDGREATERCALL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDLESSOREQUALCALL, @CreateLessOrEqualCall);
    with CreatePreset('<= (called)', VIDLESSOREQUALCALL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDGREATEROREQUALCALL, @CreateGreaterOrEqualCall);
    with CreatePreset('>= (called)', VIDGREATEROREQUALCALL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    //automatic comparing operators
    RegisterVis(VIDEQUAL, @CreateEqual);
    with CreatePreset('=', VIDEQUAL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDNOTEQUAL, @CreateNotEqual);
    with CreatePreset('<>', VIDNOTEQUAL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDLESS, @CreateLess);
    with CreatePreset('<', VIDLESS) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDGREATER, @CreateGreater);
    with CreatePreset('>', VIDGREATER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDLESSOREQUAL, @CreateLessOrEqual);
    with CreatePreset('<=', VIDLESSOREQUAL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
    RegisterVis(VIDGREATEROREQUAL, @CreateGreaterOrEqual);
    with CreatePreset('>=', VIDGREATEROREQUAL) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, COMPAREVALUE1NAME, 0.0);
      AddInput(This, COMPAREVALUE2NAME, 0.0);
      AddInput(This, COMPARERESULTNAME, false);
    end;
  end;
end;

{%ENDREGION}

end.

