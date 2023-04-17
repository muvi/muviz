unit TriggerUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, AdvCoord,
  GraphX32, MPluginType4, SimpleVis, ParamOp, Math, AdvFunc, VisAddInput,
  VisualisationUtils, StdPermissions;

type
  TBasicFFTTrigger    = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FIndex      : IPInteger;
    FScale      : IPFloat;
    FOutput     : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRounder            = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FInput      : IPFloat;
    FScale      : IPFloat;
    FOutput     : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TInt                = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FInput      : IPFloat;
    FOutput     : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TMathBinExp         = class (TVisualisationEvents)
  protected
    FMain       : IPCall;
    FInput1     : IPFloat;
    FInput2     : IPFloat;
    FOutput     : IPFloat;
    function GetOnMainCalled: TPParamNotification; virtual; abstract;
    property OnMainCalled: TPParamNotification read GetOnMainCalled;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TAdder              = class (TMathBinExp)
  protected
    function GetOnMainCalled: TPParamNotification; override;
  end;

  TSwitch             = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FIndex      : IPInteger;
    FOutputs    : array [0..3] of IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TSwitch10           = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FIndex        : IPInteger;
    FOutputs      : array [0..9] of IPCall;
    FOverflow     : IPCall;
    FOverflowCount: IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TMulticaller        = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FOutputs      : array [0..3] of IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TCounter            = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FMax          : IPInteger;
    FOutput       : IPInteger;

    FPosition     : Cardinal;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TLimitCaller        = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInput        : IPFloat;
    FLimit        : IPFloat;
    FOutput       : IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TSetColor           = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInputs       : array [0..2] of IPColor;
    FOutputs      : array [0..2] of IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TLimitStopCaller    = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInput        : IPFloat;
    FLimit        : IPFloat;
    FOutput       : IPCall;

    FIsCalled     : Boolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TPeakTrigger        = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FScale        : IPFloat;
    FPeakOutput   : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TColorizer          = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FColor1       : IPColor;
    FColor2       : IPColor;
    FBeta         : IPInteger;
    FOutput       : IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBackCounter        = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FMin          : IPInteger;
    FMax          : IPInteger;
    FStep         : IPInteger;
    FOutput       : IPInteger;

    FPosition     : Integer;
    FDirection    : SmallInt;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TMultiplyer         = class (TMathBinExp)
  protected
    function GetOnMainCalled: TPParamNotification; override;
  end;

  TIntToFloat         = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInput        : IPInteger;
    FOutput       : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TMultinumberer      = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInput        : IPFloat;
    FOutputs      : array [0..3] of IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TSqrt               = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInput        : IPFloat;
    FOutput       : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBooleanCaller      = class (TVisualisationEvents)
  private
    FMain         : IPCall;
    FInput        : IPBoolean;
    FOutput       : IPCall;

    FCalled       : Boolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TStepCounter        = class (TVisualisationEvents)
  strict private
    FPosition     : TVFloat;
  protected
    FMain         : IPCall;
    FMax          : IPFloat;
    FMin          : IPFloat;
    FStart        : IPFloat;
    FStep         : IPFloat;
    FOutput       : IPFloat;
    function GetOnMainCalled: TPParamNotification; virtual;
    property Position: TVFloat read FPosition write FPosition;
    property OnMainCalled: TPParamNotification read GetOnMainCalled;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TStepBackCounter    = class (TStepCounter)
  private
    FDirection: TVFloat;
  protected
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    function GetOnMainCalled: TPParamNotification; override;
  end;

  TNotTrigger         = class (TVisualisationEvents)
  protected
    FMain         : IPCall;
    FInput        : IPBoolean;
    FOutput       : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TDivider            = class (TMathBinExp)
  protected
    function GetOnMainCalled: TPParamNotification; override;
  end;

  TModulu             = class (TMathBinExp)
  protected
    function GetOnMainCalled: TPParamNotification; override;
  end;

  TChangeCaller       = class (TVisualisationEvents)
  protected
    FInput        : IPFloat;
    FOutput       : IPCall;

    FValue        : TVFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TColorToRGB         = class (TVisualisationEvents)
  protected
    FMain         : IPCall;
    FColor        : IPColor;
    FRed          : IPInteger;
    FGreen        : IPInteger;
    FBlue         : IPInteger;
    FAlpha        : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRGBToColor         = class (TVisualisationEvents)
  protected
    FMain         : IPCall;
    FRed          : IPInteger;
    FGreen        : IPInteger;
    FBlue         : IPInteger;
    FAlpha        : IPInteger;
    FColor        : IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TColorToHSV         = class (TVisualisationEvents)
  protected
    FMain         : IPCall;
    FColor        : IPColor;
    FHue          : IPFloat;
    FSaturation   : IPInteger;
    FValue        : IPInteger;
    FAlpha        : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  THSVToColor         = class (TVisualisationEvents)
  protected
    FMain         : IPCall;
    FHue          : IPFloat;
    FSaturation   : IPInteger;
    FValue        : IPInteger;
    FAlpha        : IPInteger;
    FColor        : IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TGeneralStorage     = class (TVisualisationEvents)
  private
    FMain   : IPCall;
    FInputs : array of IPParam;
    FOutputs: array of IPParam;
  protected
    procedure GotInput(Param: IPParam); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TUniversalStorage   = class (TVisualisationEvents)
  private
    FMain   : IPCall;
    FInputs : array of IPParam;
  protected
    procedure GotInput(Param: IPParam); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDBASICFFTTRIGGER : TGUID = '{2C75A06B-5A8D-471A-2146-465454726967}';
  VIDROUNDER         : TGUID = '{2C75A06B-5A8D-471A-2152-6F756E646572}';
  VIDINT             : TGUID = '{2C75A06B-5A8D-471A-2146-6C32496E7420}';
  VIDADDER           : TGUID = '{2C75A06B-5A8D-471A-2141-646465725F5F}';
  VIDSWITCH          : TGUID = '{2C75A06B-5A8D-471A-2153-776974636834}';
  VIDSWITCH10        : TGUID = '{2C75A06B-5A8D-471A-2153-776974636841}';
  VIDMULTICALLER     : TGUID = '{2C75A06B-5A8D-471A-214D-756C74692034}';
  VIDCOUNTER         : TGUID = '{2C75A06B-5A8D-471A-2143-6F756E746572}';
  VIDLIMITCALLER     : TGUID = '{2C75A06B-5A8D-471A-214C-6D7443616C6C}';
  VIDSETCOLOR        : TGUID = '{2C75A06B-5A8D-471A-2153-6574436F6C72}';
  VIDLIMITSTOPCALLER : TGUID = '{2C75A06B-5A8D-471A-2153-747043616C6C}';
  VIDPEAKTRIGGER     : TGUID = '{2C75A06B-5A8D-471A-2150-65616B547267}';
  VIDCOLORIZER       : TGUID = '{2C75A06B-5A8D-471A-2143-6F6C6F72697A}';
  VIDBACKCOUNTER     : TGUID = '{2C75A06B-5A8D-471A-2143-6F756E742032}';
  VIDMULTIPLYER      : TGUID = '{2C75A06B-5A8D-471A-214D-756C7469706C}';
  VIDINTTOFLOAT      : TGUID = '{2C75A06B-5A8D-471A-2149-6E745265616C}';
  VIDMULTINUMBERER   : TGUID = '{2C75A06B-5A8D-471A-214D-756C74694E72}';
  VIDSQRT            : TGUID = '{2C75A06B-5A8D-471A-2153-717274547267}';
  VIDBOOLEANCALLER   : TGUID = '{2C75A06B-5A8D-471A-2142-6F6F6C547267}';
  VIDSTEPCOUNTER     : TGUID = '{2C75A06B-5A8D-471A-2143-6F756E743220}';
  VIDSTEPBACKCOUNTER : TGUID = '{2C75A06B-5A8D-471A-2143-6F756E743242}';
  VIDNOTTRIGGER      : TGUID = '{2C75A06B-5A8D-471A-214E-6F7454726967}';
  VIDDIVIDER         : TGUID = '{2C75A06B-5A8D-471A-2144-697669646572}';
  VIDMODULU          : TGUID = '{2C75A06B-5A8D-471A-214D-6F64756C7520}';
  VIDCHANGECALLER    : TGUID = '{2C75A06B-5A8D-471A-2143-68616E676543}';
  VIDCOLORTORGB      : TGUID = '{2C75A06B-5A8D-471A-2143-6C7232524742}';
  VIDRGBTOCOLOR      : TGUID = '{2C75A06B-5A8D-471A-2152-474232436C72}';
  VIDCOLORTOHSV      : TGUID = '{2C75A06B-5A8D-471A-2143-6C7232485356}';
  VIDHSVTOCOLOR      : TGUID = '{2C75A06B-5A8D-471A-2148-535632436C72}';
  VIDCALLSTORAGE     : TGUID = '{2C75A06B-5A8D-471A-2143-6C6C344D656D}';
  VIDINTEGERSTORAGE  : TGUID = '{2C75A06B-5A8D-471A-2149-6E74344D656D}';
  VIDFLOATSTORAGE    : TGUID = '{2C75A06B-5A8D-471A-2152-616C344D656D}';
  VIDSTRINGSTORAGE   : TGUID = '{2C75A06B-5A8D-471A-2153-7472344D656D}';
  VIDCOLORSTORAGE    : TGUID = '{2C75A06B-5A8D-471A-2143-6C72344D656D}';
  VIDBOOLEANSTORAGE  : TGUID = '{2C75A06B-5A8D-471A-2142-6F6C344D656D}';
  VIDBUFFERSTORAGE   : TGUID = '{2C75A06B-5A8D-471A-2142-7566344D656D}';
  VIDUNIVERSALSTORAGE: TGUID = '{E361F87E-1AD2-4CD9-A26A-8391D95E178E}';

  {
  PIDBASICFFTTRIGGER : TGUID = '{35ED1492-DF31-4899-8BD7-F38378C6D73C}';
  PIDROUNDER         : TGUID = '{5C3A94F6-C77C-407F-9291-0B76B790745E}';
  PIDINT             : TGUID = '{3480A695-1B51-4EA7-BD6F-700FF0C0DB34}';
  PIDADDER           : TGUID = '{A06661EC-6B3D-4386-890A-5885F6081CFC}';
  PIDSWITCH          : TGUID = '{19F06BB9-4380-4075-BF8C-2A746E15793A}';
  PIDSWITCH10        : TGUID = '{9D427939-BF42-48F8-A7D9-B02EF8E3F65A}';
  PIDMULTICALLER     : TGUID = '{77295BD3-244D-46CE-BDC1-62CFAD1B0627}';
  PIDCOUNTER         : TGUID = '{5741BACE-C74B-4A08-A3DE-2E2C73194A8E}';
  PIDLIMITCALLER     : TGUID = '{53F25751-128E-4CE8-BF28-2D5E96B8F431}';
  PIDSETCOLOR        : TGUID = '{A93C81F8-A2D7-489B-B92A-DD14DC0B6370}';
  PIDLIMITSTOPCALLER : TGUID = '{5018B348-C019-42E9-992D-9197C7E1B724}';
  PIDPEAKTRIGGER     : TGUID = '{FDBB7435-D726-458D-9462-90D71C29561B}';
  PIDCOLORIZER       : TGUID = '{DD1EE841-58B2-46A3-9BFE-5906AF3124F1}';
  PIDBACKCOUNTER     : TGUID = '{222C5159-3AE4-48CE-92E8-E8610808A8EF}';
  PIDMULTIPLYER      : TGUID = '{96C6B353-5811-4E22-8DD9-95C6D2AA8E4E}';
  PIDINTTOFLOAT      : TGUID = '{AC77FA68-4AF3-40D0-8FE4-17B1C036F306}';
  PIDMULTINUMBERER   : TGUID = '{2A6C9684-EADE-4A74-B194-EC6DD0427DC2}';
  PIDSQRT            : TGUID = '{056779C6-DF7A-4015-8BA2-D4DF02FF6536}';
  PIDBOOLEANCALLER   : TGUID = '{C3318A9A-CB6D-43AF-82D2-97EBFEEE59E3}';
  PIDSTEPCOUNTER     : TGUID = '{CABBB88F-D8A7-4FE6-A373-B0A00B26D6F3}';
  PIDSTEPBACKCOUNTER : TGUID = '{F275B484-F87D-4763-BB5B-95B1825BF101}';
  PIDNOTTRIGGER      : TGUID = '{C07CB8D7-ECEB-4182-9E1A-3ED811BE9791}';
  PIDDIVIDER         : TGUID = '{CAF44D28-C48E-4833-9F4A-887F61F36987}';
  PIDMODULU          : TGUID = '{FBA8BDD9-5EE7-44D5-B421-7980158258DD}';
  PIDCHANGECALLER    : TGUID = '{B68A13D9-9342-4E15-BD2E-FC215696E02F}';
  PIDCOLORTORGB      : TGUID = '{B3D88289-D746-4C96-A479-9601EBCA388F}';
  PIDRGBTOCOLOR      : TGUID = '{FAD4B6F7-EF42-4A68-8BD2-38039A9529F7}';
  PIDCOLORTOHSV      : TGUID = '{FCA80B60-D78C-4866-B079-4D0373330C31}';
  PIDHSVTOCOLOR      : TGUID = '{F786CE4C-A4E1-41D2-96E7-B77319EEED9A}';
  PIDCALLSTORAGE     : TGUID = '{695B8825-6AC9-4EAD-9776-639F68A3ECF2}';
  PIDINTEGERSTORAGE  : TGUID = '{D6F41570-96E5-4774-8917-9637F894573B}';
  PIDFLOATSTORAGE    : TGUID = '{BF54AA17-07D6-47CE-9488-AEE66252AE04}';
  PIDSTRINGSTORAGE   : TGUID = '{606A5068-9755-45EA-BEDA-DC469BE22CBD}';
  PIDCOLORSTORAGE    : TGUID = '{ECCAA615-51A4-46D9-8565-7756E7C39E01}';
  PIDBOOLEANSTORAGE  : TGUID = '{6098F9E1-86F4-4F7C-899C-B3D24E9D3B57}';
  PIDBUFFERSTORAGE   : TGUID = '{32A697E8-1287-47EE-89D3-7C3875666AC7}';
  PIDUNIVERSALSTORAGE: TGUID = '{BE5C15F3-5827-4700-A15F-7A84DA26A3FC}';
  }

  BASICFFTTRIGGERINDEXNAME          = 'Index';
  BASICFFTTRIGGERSCALENAME          = 'Streckfaktor';
  BASICFFTTRIGGERFFTDATAOUTPUTNAME  = 'FFTData';
  ROUNDERINPUTNAME                  = 'Input';
  ROUNDERSCALENAME                  = 'Streckfaktor';
  ROUNDEROUTPUTNAME                 = 'Output';
  INTINPUTNAME                      = 'Input';
  INTOUTPUTNAME                     = 'Output';
  ADDERINPUT1NAME                   = 'Eingang 1';
  ADDERINPUT2NAME                   = 'Eingang 2';
  ADDEROUTPUTNAME                   = 'Ergebnis';
  SWITCHINDEXNAME                   = 'Anschluss';
  SWITCHOUTPUT1NAME                 = 'Anschluss 0';
  SWITCHOUTPUT2NAME                 = 'Anschluss 1';
  SWITCHOUTPUT3NAME                 = 'Anschluss 2';
  SWITCHOUTPUT4NAME                 = 'Anschluss 3';
  SWITCH10INDEXNAME                 = 'Anschluss';
  SWITCH10OUTPUT1NAME               = 'Anschluss 0';
  SWITCH10OUTPUT2NAME               = 'Anschluss 1';
  SWITCH10OUTPUT3NAME               = 'Anschluss 2';
  SWITCH10OUTPUT4NAME               = 'Anschluss 3';
  SWITCH10OUTPUT5NAME               = 'Anschluss 4';
  SWITCH10OUTPUT6NAME               = 'Anschluss 5';
  SWITCH10OUTPUT7NAME               = 'Anschluss 6';
  SWITCH10OUTPUT8NAME               = 'Anschluss 7';
  SWITCH10OUTPUT9NAME               = 'Anschluss 8';
  SWITCH10OUTPUT10NAME              = 'Anschluss 9';
  SWITCH10OVERFLOWOUTPUTNAME        = 'Überlauf';
  SWITCH10OVERFLOWCOUNTOUTPUTNAME   = 'Überlaufanzahl';
  MULTICALLEROUTPUT1NAME            = 'Anschluss 1';
  MULTICALLEROUTPUT2NAME            = 'Anschluss 2';
  MULTICALLEROUTPUT3NAME            = 'Anschluss 3';
  MULTICALLEROUTPUT4NAME            = 'Anschluss 4';
  COUNTERMAXNAME                    = 'Maximalwert';
  COUNTERCALLCOUNTOUTPUTNAME        = 'Aufrufanzahl';
  LIMITCALLINPUTNAME                = 'Input';
  LIMITCALLLIMITNAME                = 'Grenzwert';
  LIMITCALLOUTPUTNAME               = 'Anschluss';
  SETCOLORINPUT1NAME                = C1NAME;
  SETCOLORINPUT2NAME                = C2NAME;
  SETCOLORINPUT3NAME                = C3NAME;
  SETCOLOROUTPUT1NAME               = 'Linienfarbe 1';
  SETCOLOROUTPUT2NAME               = 'Linienfarbe 2';
  SETCOLOROUTPUT3NAME               = 'Hintergrundfarbe';
  LIMITSTOPCALLERINPUTNAME          = 'Input';
  LIMITSTOPCALLERLIMITNAME          = 'Grenzwert';
  LIMITSTOPCALLEROUTPUTNAME         = 'Anschluss';
  PEAKTRIGGERSCALENAME              = 'Streckfaktor';
  PEAKTRIGGEROUTPUTNAME             = 'Lautstärke';
  COLORIZERCOLOR1NAME               = C1NAME;
  COLORIZERCOLOR2NAME               = C2NAME;
  COLORIZERBETANAME                 = 'Blendstärke';
  COLORIZEROUTPUTNAME               = 'Farbe';
  BACKCOUNTERMINNAME                = 'Minimalwert';
  BACKCOUNTERMAXNAME                = 'Maximalwert';
  BACKCOUNTERSTEPNAME               = 'Schrittweite';
  BACKCOUNTERCOUNTOUTPUTNAME        = 'Aufrufanzahl';
  MULTIPLYERINPUT1NAME              = 'Eingang 1';
  MULTIPLYERINPUT2NAME              = 'Eingang 2';
  MULTIPLYEROUTPUTNAME              = 'Ergebnis';
  INTTOFLOATINPUTNAME               = 'Input';
  INTTOFLOATOUTPUTNAME              = 'Output';
  MULTINUMBERERINPUTNAME            = 'Input';
  MULTINUMBEREROUTPUT1NAME          = 'Ausgang 1';
  MULTINUMBEREROUTPUT2NAME          = 'Ausgang 2';
  MULTINUMBEREROUTPUT3NAME          = 'Ausgang 3';
  MULTINUMBEREROUTPUT4NAME          = 'Ausgang 4';
  SQRTINPUTNAME                     = 'Eingang';
  SQRTOUTPUTNAME                    = 'Ausgang';
  BOOLEANCALLERINPUTNAME            = 'Eingang';
  BOOLEANCALLEROUTPUTNAME           = 'Anschluss';
  STEPCOUNTERMAXNAME                = 'Maximalwert';
  STEPCOUNTERMINNAME                = 'Minimalwert';
  STEPCOUNTERSTARTNAME              = 'Startwert';
  STEPCOUNTERSTEPNAME               = 'Schrittweite';
  STEPCOUNTEROUTPUTNAME             = 'Position';
  STEPBACKCOUNTERMAXNAME            = 'Maximalwert';
  STEPBACKCOUNTERMINNAME            = 'Minimalwert';
  STEPBACKCOUNTERSTARTNAME          = 'Startwert';
  STEPBACKCOUNTERSTEPNAME           = 'Schrittweite';
  STEPBACKCOUNTEROUTPUTNAME         = 'Position';
  NOTTRIGGERINPUTNAME               = 'Input';
  NOTTRIGGEROUTPUTNAME              = 'Ausgang';
  DIVIDERINPUT1NAME                 = 'Eingang 1';
  DIVIDERINPUT2NAME                 = 'Eingang 2';
  DIVIDEROUTPUTNAME                 = 'Ergebnis';
  MODULUINPUT1NAME                  = 'Eingang 1';
  MODULUINPUT2NAME                  = 'Eingang 2';
  MODULUOUTPUTNAME                  = 'Ergebnis';
  CHANGECALLERINPUTNAME             = 'Eingang';
  CHANGECALLERCHANGEDOUTPUTNAME     = 'Anschluss';
  COLORTORGBCOLORNAME               = 'Farbe';
  COLORTORGBREDOUTPUTNAME           = 'Rot';
  COLORTORGBGREENOUTPUTNAME         = 'Grün';
  COLORTORGBBLUEOUTPUTNAME          = 'Blau';
  COLORTORGBALPHAOUTPUTNAME         = 'Deckkraft';
  RGBTOCOLORREDNAME                 = 'Rot';
  RGBTOCOLORGREENNAME               = 'Grün';
  RGBTOCOLORBLUENAME                = 'Blau';
  RGBTOCOLORALPHANAME               = 'Deckkraft';
  RGBTOCOLOROUTPUTNAME              = 'Farbe';
  COLORTOHSVCOLORNAME               = 'Farbe';
  COLORTOHSVHUEOUTPUTNAME           = 'Farbton';
  COLORTOHSVSATURATIONOUTPUTNAME    = 'Sättigung';
  COLORTOHSVVALUEOUTPUTNAME         = 'Helligkeit';
  COLORTOHSVALPHAOUTPUTNAME         = 'Deckkraft';
  HSVTOCOLORHUENAME                 = 'Farbton';
  HSVTOCOLORSATURATIONNAME          = 'Sättigung';
  HSVTOCOLORVALUENAME               = 'Helligkeit';
  HSVTOCOLORALPHANAME               = 'Deckkraft';
  HSVTOCOLOROUTPUTNAME              = 'Farbe';
  CALLSTORAGEINPUTNAME              = 'Input';
  CALLSTORAGEOUTPUTNAME             = 'Output';
  INTEGERSTORAGEINPUTNAME           = 'Input';
  INTEGERSTORAGEOUTPUTNAME          = 'Output';
  FloatSTORAGEINPUTNAME             = 'Input';
  FloatSTORAGEOUTPUTNAME            = 'Output';
  STRINGSTORAGEINPUTNAME            = 'Input';
  STRINGSTORAGEOUTPUTNAME           = 'Output';
  COLORSTORAGEINPUTNAME             = 'Input';
  COLORSTORAGEOUTPUTNAME            = 'Output';
  BOOLEANSTORAGEINPUTNAME           = 'Input';
  BOOLEANSTORAGEOUTPUTNAME          = 'Output';
  BUFFERSTORAGEINPUTNAME            = 'Input';
  BUFFERSTORAGEOUTPUTNAME           = 'Output';
  GENERALSTORAGEINPUTNAME           = 'Input';
  GENERALSTORAGEOUTPUTNAME          = 'Output';
  UNIVERSALSTORAGEVARNAME           = 'Variable 1';

procedure Register;

implementation

{%REGION TBasicFFTTrigger}

procedure BasicFFTTriggerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBasicFFTTrigger(Context) do begin
    FOutput.&Set(AdvSpectrumData.Levels[FIndex.Get,0]*FScale);
  end;
end;

procedure CreateBasicFFTTrigger(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBasicFFTTrigger.Create(APrototype);
end;

constructor TBasicFFTTrigger.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BasicFFTTriggerMainCalled, Self, Environment.Thread);
  FIndex:=IntegerInputs[BASICFFTTRIGGERINDEXNAME];
  FScale:=FloatInputs[BASICFFTTRIGGERSCALENAME];
  FOutput:=FloatInputs[BASICFFTTRIGGERFFTDATAOUTPUTNAME];
end;

destructor TBasicFFTTrigger.Destroy;
begin
  FMain.RemoveListener(@BasicFFTTriggerMainCalled, Self);
  FMain:=nil;
  FIndex:=nil;
  FScale:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRounder}

procedure RounderMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TRounder(Context) do begin
    FOutput.&Set(Round(FInput*FScale));
  end;
end;

procedure CreateRounder(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRounder.Create(APrototype);
end;

constructor TRounder.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RounderMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[ROUNDERINPUTNAME];
  FScale:=FloatInputs[ROUNDERSCALENAME];
  FOutput:=IntegerInputs[ROUNDEROUTPUTNAME];
end;

destructor TRounder.Destroy;
begin
  FMain.RemoveListener(@RounderMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FScale:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TInt}

procedure IntMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TInt(Context) do begin
    FOutput.&Set(Trunc(FInput.Get));
  end;
end;

procedure CreateInt(APrototype: IPVisualisationPrototype); cdecl;
begin
  TInt.Create(APrototype);
end;

constructor TInt.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@IntMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[INTINPUTNAME];
  FOutput:=IntegerInputs[INTOUTPUTNAME];
end;

destructor TInt.Destroy;
begin
  FMain.RemoveListener(@IntMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TMathBinExp}

constructor TMathBinExp.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(OnMainCalled, Self, Environment.Thread);
  FInput1:=FloatInputs[ADDERINPUT1NAME];
  FInput2:=FloatInputs[ADDERINPUT2NAME];
  FOutput:=FloatInputs[ADDEROUTPUTNAME];
end;

destructor TMathBinExp.Destroy;
begin
  FMain.RemoveListener(OnMainCalled, Self);
  FMain:=nil;
  FInput1:=nil;
  FInput2:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TAdder}

procedure AdderMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TAdder(Context) do begin
    FOutput.&Set(FInput1 + FInput2);
  end;
end;

procedure CreateAdder(APrototype: IPVisualisationPrototype); cdecl;
begin
  TAdder.Create(APrototype);
end;

function TAdder.GetOnMainCalled: TPParamNotification;
begin
  Result:=@AdderMainCalled;
end;

{%ENDREGION}
{%REGION TSwitch}

procedure SwitchMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AIndex: TVInteger;
begin
  with TSwitch(Context) do begin
    AIndex:=FIndex.Value;
    if (AIndex>=0) and (AIndex<4)
      then FOutputs[AIndex].&Set;
  end;
end;

procedure CreateSwitch(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSwitch.Create(APrototype);
end;

constructor TSwitch.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SwitchMainCalled, Self, Environment.Thread);
  FIndex:=IntegerInputs[SWITCHINDEXNAME];
  FOutputs[0]:=CallInputs[SWITCHOUTPUT1NAME];
  FOutputs[1]:=CallInputs[SWITCHOUTPUT2NAME];
  FOutputs[2]:=CallInputs[SWITCHOUTPUT3NAME];
  FOutputs[3]:=CallInputs[SWITCHOUTPUT4NAME];
end;

destructor TSwitch.Destroy;
begin
  FMain.RemoveListener(@SwitchMainCalled, Self);
  FMain:=nil;
  FIndex:=nil;
  FOutputs[0]:=nil;
  FOutputs[1]:=nil;
  FOutputs[2]:=nil;
  FOutputs[3]:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TSwitch10}

procedure Switch10MainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AIndex: TVInteger;
begin
  with TSwitch10(Context) do begin
    AIndex:=FIndex.Value;
    if AIndex>=0 then begin
      if AIndex>=10 then begin
        FOverflowCount.&Set(AIndex-10);
        FOverflow.&Set;
      end else FOutputs[AIndex].&Set
    end;
  end;
end;

procedure CreateSwitch10(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSwitch10.Create(APrototype);
end;

constructor TSwitch10.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@Switch10MainCalled, Self, Environment.Thread);
  FIndex:=IntegerInputs[SWITCH10INDEXNAME];
  FOutputs[0]:=CallInputs[SWITCH10OUTPUT1NAME];
  FOutputs[1]:=CallInputs[SWITCH10OUTPUT2NAME];
  FOutputs[2]:=CallInputs[SWITCH10OUTPUT3NAME];
  FOutputs[3]:=CallInputs[SWITCH10OUTPUT4NAME];
  FOutputs[4]:=CallInputs[SWITCH10OUTPUT5NAME];
  FOutputs[5]:=CallInputs[SWITCH10OUTPUT6NAME];
  FOutputs[6]:=CallInputs[SWITCH10OUTPUT7NAME];
  FOutputs[7]:=CallInputs[SWITCH10OUTPUT8NAME];
  FOutputs[8]:=CallInputs[SWITCH10OUTPUT9NAME];
  FOutputs[9]:=CallInputs[SWITCH10OUTPUT10NAME];
  FOverflow:=CallInputs[SWITCH10OVERFLOWOUTPUTNAME];
  FOverflowCount:=IntegerInputs[SWITCH10OVERFLOWCOUNTOUTPUTNAME];
end;

destructor TSwitch10.Destroy;
begin
  FMain.RemoveListener(@Switch10MainCalled, Self);
  FMain:=nil;
  FIndex:=nil;
  FOutputs[0]:=nil;
  FOutputs[1]:=nil;
  FOutputs[2]:=nil;
  FOutputs[3]:=nil;
  FOutputs[4]:=nil;
  FOutputs[5]:=nil;
  FOutputs[6]:=nil;
  FOutputs[7]:=nil;
  FOutputs[8]:=nil;
  FOutputs[9]:=nil;
  FOverflow:=nil;
  FOverflowCount:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TMulticaller}

procedure MulticallerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I: Integer;
begin
  with TMulticaller(Context) do begin
    for I:=0 to 3
      do FOutputs[I].&Set;
  end;
end;

procedure CreateMulticaller(APrototype: IPVisualisationPrototype); cdecl;
begin
  TMulticaller.Create(APrototype);
end;

constructor TMulticaller.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@MulticallerMainCalled, Self, Environment.Thread);
  FOutputs[0]:=CallInputs[MULTICALLEROUTPUT1NAME];
  FOutputs[1]:=CallInputs[MULTICALLEROUTPUT2NAME];
  FOutputs[2]:=CallInputs[MULTICALLEROUTPUT3NAME];
  FOutputs[3]:=CallInputs[MULTICALLEROUTPUT4NAME];
end;

destructor TMulticaller.Destroy;
begin
  FMain.RemoveListener(@MulticallerMainCalled, Self);
  FMain:=nil;
  FOutputs[0]:=nil;
  FOutputs[1]:=nil;
  FOutputs[2]:=nil;
  FOutputs[3]:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TCounter}

procedure CounterMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AMax: TVInteger;
begin
  with TCounter(Context) do begin
    AMax:=FMax.Value;
    Inc(FPosition);
    if AMax <> 0
      then FPosition:=FPosition mod AMax;
    FOutput.&Set(FPosition);
  end;
end;

procedure CreateCounter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TCounter.Create(APrototype);
end;

constructor TCounter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@CounterMainCalled, Self, Environment.Thread);
  FMax:=IntegerInputs[COUNTERMAXNAME];
  FOutput:=IntegerInputs[COUNTERCALLCOUNTOUTPUTNAME];

  FPosition:=0;
end;

destructor TCounter.Destroy;
begin
  FMain.RemoveListener(@CounterMainCalled, Self);
  FMain:=nil;
  FMax:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TLimitCaller}

procedure LimitCallerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TLimitCaller(Context) do begin
    if FInput.Value>FLimit.Value then FOutput.&Set;
  end;
end;

procedure CreateLimitCaller(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLimitCaller.Create(APrototype);
end;

constructor TLimitCaller.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@LimitCallerMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[LIMITCALLINPUTNAME];
  FLimit:=FloatInputs[LIMITCALLLIMITNAME];
  FOutput:=CallInputs[LIMITCALLOUTPUTNAME];
end;

destructor TLimitCaller.Destroy;
begin
  FMain.RemoveListener(@LimitCallerMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FLimit:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TSetColor}

procedure SetColorMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I: Integer;
begin
  with TSetColor(Context) do begin
    for I:=0 to 2
      do FOutputs[I].&Set(FInputs[I]);
  end;
end;

procedure CreateSetColor(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSetColor.Create(APrototype);
end;

constructor TSetColor.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SetColorMainCalled, Self, Environment.Thread);
  FInputs[0]:=ColorInputs[SETCOLORINPUT1NAME];
  FInputs[1]:=ColorInputs[SETCOLORINPUT2NAME];
  FInputs[2]:=ColorInputs[SETCOLORINPUT3NAME];
  FOutputs[0]:=ColorInputs[SETCOLOROUTPUT1NAME];
  FOutputs[1]:=ColorInputs[SETCOLOROUTPUT2NAME];
  FOutputs[2]:=ColorInputs[SETCOLOROUTPUT3NAME];
end;

destructor TSetColor.Destroy;
var
  I: Integer;
begin
  FMain.RemoveListener(@SetColorMainCalled, Self);
  FMain:=nil;
  for I:=0 to 2 do begin
    FInputs[I]:=nil;
    FOutputs[I]:=nil;
  end;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TLimitStopCaller}

procedure LimitStopCallerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ADoCall: Boolean;
begin
  with TLimitStopCaller(Context) do begin
    ADoCall:=(FInput.Value>FLimit.Value);
    if ADoCall and (not FIsCalled) then FOutput.&Set;
    // then TriggerCall2(bmp,Source,Visualisation.VisOutputs[0]);
    FIsCalled:=ADoCall;
  end;
end;

procedure CreateLimitStopCaller(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLimitStopCaller.Create(APrototype);
end;

constructor TLimitStopCaller.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@LimitStopCallerMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[LIMITSTOPCALLERINPUTNAME];
  FLimit:=FloatInputs[LIMITSTOPCALLERLIMITNAME];
  FOutput:=CallInputs[LIMITSTOPCALLEROUTPUTNAME];

  FIsCalled:=false;
end;

destructor TLimitStopCaller.Destroy;
begin
  FMain.RemoveListener(@LimitStopCallerMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FLimit:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TPeakTrigger}

procedure PeakTriggerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TPeakTrigger(Context) do begin
    FPeakOutput.&Set(AdvSpectrumData.Peak[0]*FScale);
  end;
end;

procedure CreatePeakTrigger(APrototype: IPVisualisationPrototype); cdecl;
begin
  TPeakTrigger.Create(APrototype);
end;

constructor TPeakTrigger.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@PeakTriggerMainCalled, Self, Environment.Thread);
  FScale:=FloatInputs[PEAKTRIGGERSCALENAME];
  FPeakOutput:=FloatInputs[PEAKTRIGGEROUTPUTNAME];
end;

destructor TPeakTrigger.Destroy;
begin
  FMain.RemoveListener(@PeakTriggerMainCalled, Self);
  FMain:=nil;
  FScale:=nil;
  FPeakOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TColorizer}

procedure ColorizerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ABeta: TVInteger;
begin
  with TColorizer(Context) do begin
    ABeta:=FBeta;
    if ABeta>255
      then ABeta:=255
      else if ABeta<0 then ABeta:=0;
    FOutput.&Set(BetaBlend(FColor1.Value,FColor2.Value,ABeta));
  end;
end;

procedure CreateColorizer(APrototype: IPVisualisationPrototype); cdecl;
begin
  TColorizer.Create(APrototype);
end;

constructor TColorizer.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ColorizerMainCalled, Self, Environment.Thread);
  FColor1:=ColorInputs[COLORIZERCOLOR1NAME];
  FColor2:=ColorInputs[COLORIZERCOLOR2NAME];
  FBeta:=IntegerInputs[COLORIZERBETANAME];
  FOutput:=ColorInputs[COLORIZEROUTPUTNAME];
end;

destructor TColorizer.Destroy;
begin
  FMain.RemoveListener(@ColorizerMainCalled, Self);
  FMain:=nil;
  FColor1:=nil;
  FColor2:=nil;
  FBeta:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBackCounter}

procedure BackCounterMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AStep: TVInteger;
begin
  with TBackCounter(Context) do begin
    AStep:=FStep;
    FPosition+=AStep*FDirection;
    if (FPosition > FMax) or (FPosition<FMin) then begin
      FDirection:=-FDirection;
      FPosition+=AStep*FDirection;
    end;
    FOutput.Value:=FPosition;
  end;
end;

procedure CreateBackCounter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBackCounter.Create(APrototype);
end;

constructor TBackCounter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BackCounterMainCalled, Self, Environment.Thread);
  FMin:=IntegerInputs[BACKCOUNTERMINNAME];
  FMax:=IntegerInputs[BACKCOUNTERMAXNAME];
  FStep:=IntegerInputs[BACKCOUNTERSTEPNAME];
  FOutput:=IntegerInputs[BACKCOUNTERCOUNTOUTPUTNAME];

  FDirection:=1;
  FPosition:=0;
end;

destructor TBackCounter.Destroy;
begin
  FMain.RemoveListener(@BackCounterMainCalled, Self);
  FMain:=nil;
  FMin:=nil;
  FMax:=nil;
  FStep:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TMultiplyer}

procedure MultiplyerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TMultiplyer(Context) do begin
    FOutput.&Set(FInput1 * FInput2);
  end;
end;

procedure CreateMultiplyer(APrototype: IPVisualisationPrototype); cdecl;
begin
  TMultiplyer.Create(APrototype);
end;

function TMultiplyer.GetOnMainCalled: TPParamNotification;
begin
  Result:=@MultiplyerMainCalled;
end;

{%ENDREGION}
{%REGION TIntToFloat}

procedure IntToFloatMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TIntToFloat(Context) do begin
    FOutput.Value:=FInput.Value;
  end;
end;

procedure CreateIntToFloat(APrototype: IPVisualisationPrototype); cdecl;
begin
  TIntToFloat.Create(APrototype);
end;

constructor TIntToFloat.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@IntToFloatMainCalled, Self, Environment.Thread);
  FInput:=IntegerInputs[INTTOFLOATINPUTNAME];
  FOutput:=FloatInputs[INTTOFLOATOUTPUTNAME];
end;

destructor TIntToFloat.Destroy;
begin
  FMain.RemoveListener(@IntToFloatMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TMultinumberer}

procedure MultinumbererMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I     : Integer;
  AInput: TVFloat;
begin
  with TMultinumberer(Context) do begin
    AInput:=FInput.Value;
    for I:=0 to 3
      do FOutputs[I].Value:=AInput;
  end;
end;

procedure CreateMultinumberer(APrototype: IPVisualisationPrototype); cdecl;
begin
  TMultinumberer.Create(APrototype);
end;

constructor TMultinumberer.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@MultinumbererMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[MULTINUMBERERINPUTNAME];
  FOutputs[0]:=FloatInputs[MULTINUMBEREROUTPUT1NAME];
  FOutputs[1]:=FloatInputs[MULTINUMBEREROUTPUT2NAME];
  FOutputs[2]:=FloatInputs[MULTINUMBEREROUTPUT3NAME];
  FOutputs[3]:=FloatInputs[MULTINUMBEREROUTPUT4NAME];
end;

destructor TMultinumberer.Destroy;
var
  I: Integer;
begin
  FMain.RemoveListener(@MultinumbererMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  for I:=0 to 3
    do FOutputs[I]:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TSqrt}

procedure SqrtMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TSqrt(Context) do begin
    FOutput.Value:=sqrt(FInput.Value);
  end;
end;

procedure CreateSqrt(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSqrt.Create(APrototype);
end;

constructor TSqrt.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SqrtMainCalled, Self, Environment.Thread);
  FInput:=FloatInputs[SQRTINPUTNAME];
  FOutput:=FloatInputs[SQRTOUTPUTNAME];
end;

destructor TSqrt.Destroy;
begin
  FMain.RemoveListener(@SqrtMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBooleanCaller}

procedure BooleanCallerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TBooleanCaller(Context) do begin
    if FCalled and (not FInput.Value) then FOutput.&Set;
    FCalled:=FInput;
  end;
end;

procedure CreateBooleanCaller(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBooleanCaller.Create(APrototype);
end;

constructor TBooleanCaller.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BooleanCallerMainCalled, Self, Environment.Thread);
  FInput:=BooleanInputs[BOOLEANCALLERINPUTNAME];
  FOutput:=CallInputs[BOOLEANCALLEROUTPUTNAME];

  FCalled:=false;
end;

destructor TBooleanCaller.Destroy;
begin
  FMain.RemoveListener(@BooleanCallerMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TStepCounter}

procedure StepCounterMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TStepCounter(Context) do begin
    Position:=Position + FStep.Get;
    if Position >= FMax
      then Position:=FMin + (Position - FMax);
    if Position <= FMin
      then Position:=FMax - (FMin - Position);
    FOutput.Value:=Position;
  end;
end;

procedure StepCounterStartChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TStepCounter(Context)
    do Position:=FStart;
end;

procedure CreateStepCounter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TStepCounter.Create(APrototype);
end;

constructor TStepCounter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(OnMainCalled, Self, Environment.Thread);
  FMax:=FloatInputs[STEPCOUNTERMAXNAME];
  FMin:=FloatInputs[STEPCOUNTERMINNAME];
  FStart:=FloatInputs[STEPCOUNTERSTARTNAME];
  FStart.AddListener(@StepCounterStartChanged, Self, Environment.Thread);
  FStep:=FloatInputs[STEPCOUNTERSTEPNAME];
  FOutput:=FloatInputs[STEPCOUNTEROUTPUTNAME];

  FPosition:=FStart;
end;

destructor TStepCounter.Destroy;
begin
  FMain.RemoveListener(OnMainCalled, Self);
  FMain:=nil;
  FMax:=nil;
  FMin:=nil;
  FStart:=nil;
  FStep:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

function TStepCounter.GetOnMainCalled: TPParamNotification;
begin
  Result:=@StepCounterMainCalled;
end;

{%ENDREGION}
{%REGION TStepBackCounter}

procedure StepBackCounterMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TStepBackCounter(Context) do begin
    Position:=Position + FDirection;
    if Position >= FMax then begin
      Position:=FMax;
      FDirection:=-FDirection;
    end;
    if Position <= FMin then begin
      Position:=FMin;
      FDirection:=-FDirection;
    end;
    FOutput.Value:=Position;
  end;
end;

procedure StepBackCounterStepChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TStepBackCounter(Context) do begin
    FDirection:=Sign(FDirection) * FStep;
  end;
end;

procedure CreateStepBackCounter(APrototype: IPVisualisationPrototype); cdecl;
begin
  TStepBackCounter.Create(APrototype);
end;

constructor TStepBackCounter.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FStep.AddListener(@StepBackCounterStepChanged, Self, Environment.Thread);
  FDirection:=FStep;
end;

destructor TStepBackCounter.Destroy;
begin
  FStep.RemoveListener(@StepBackCounterStepChanged, Self);
  inherited Destroy;
end;

function TStepBackCounter.GetOnMainCalled: TPParamNotification;
begin
  Result:=@StepBackCounterMainCalled;
end;

{%ENDREGION}
{%REGION TNotTrigger}

procedure NotTriggerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TNotTrigger(Context) do begin
    FOutput.Value:=not FInput.Value;
  end;
end;

procedure CreateNotTrigger(APrototype: IPVisualisationPrototype); cdecl;
begin
  TNotTrigger.Create(APrototype);
end;

constructor TNotTrigger.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@NotTriggerMainCalled, Self, Environment.Thread);
  FInput:=BooleanInputs[NOTTRIGGERINPUTNAME];
  FOutput:=BooleanInputs[NOTTRIGGEROUTPUTNAME];
end;

destructor TNotTrigger.Destroy;
begin
  FMain.RemoveListener(@NotTriggerMainCalled, Self);
  FMain:=nil;
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TDivider}

procedure DividerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TDivider(Context) do begin
    FOutput.&Set(FInput1 / FInput2);
  end;
end;

procedure CreateDivider(APrototype: IPVisualisationPrototype); cdecl;
begin
  TDivider.Create(APrototype);
end;

function TDivider.GetOnMainCalled: TPParamNotification;
begin
  Result:=@DividerMainCalled;
end;

{%ENDREGION}
{%REGION TModulu}

procedure ModuluMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TModulu(Context) do begin
    FOutput.Value:=RealMod(FInput1.Get, FInput2.Get);
  end;
end;

procedure CreateModulu(APrototype: IPVisualisationPrototype); cdecl;
begin
  TModulu.Create(APrototype);
end;

function TModulu.GetOnMainCalled: TPParamNotification;
begin
  Result:=@ModuluMainCalled;
end;

{%ENDREGION}
{%REGION TChangeCaller}

procedure ChangeCallerInputChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TChangeCaller(Context) do begin
    if FInput.Value<>FValue then begin
      FValue:=FInput;
      FOutput.&Set;
    end;
  end;
end;

procedure CreateChangeCaller(APrototype: IPVisualisationPrototype); cdecl;
begin
  TChangeCaller.Create(APrototype);
end;

constructor TChangeCaller.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FInput:=FloatInputs[CHANGECALLERINPUTNAME];
  FInput.AddListener(@ChangeCallerInputChanged, Self, Environment.Thread);
  FOutput:=CallInputs[CHANGECALLERCHANGEDOUTPUTNAME];

  FValue:=0.0;
end;

destructor TChangeCaller.Destroy;
begin
  FInput.RemoveListener(@ChangeCallerInputChanged, Self);
  FInput:=nil;
  FOutput:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TColorToRGB}

procedure ColorToRGBChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TColorToRGB(Context) do begin
    FRed.Value:=RedComponent(FColor.Value);
    FGreen.Value:=GreenComponent(FColor.Value);
    FBlue.Value:=BlueComponent(FColor.Value);
    FAlpha.Value:=AlphaComponent(FColor.Value);
  end;
end;

procedure CreateColorToRGB(APrototype: IPVisualisationPrototype); cdecl;
begin
  TColorToRGB.Create(APrototype);
end;

constructor TColorToRGB.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ColorToRGBChanged, Self, Environment.Thread);
  FColor:=ColorInputs[COLORTORGBCOLORNAME];
  //FColor.AddListener(@ColorToRGBChanged, Self, Environment.Thread);
  FRed:=IntegerInputs[COLORTORGBREDOUTPUTNAME];
  FGreen:=IntegerInputs[COLORTORGBGREENOUTPUTNAME];
  FBlue:=IntegerInputs[COLORTORGBBLUEOUTPUTNAME];
  FAlpha:=IntegerInputs[COLORTORGBALPHAOUTPUTNAME];
end;

destructor TColorToRGB.Destroy;
begin
  FMain.RemoveListener(@ColorToRGBChanged, Self);
  FMain:=nil;
  //FColor.RemoveListener(@ColorToRGBChanged, Self);
  FColor:=nil;
  FRed:=nil;
  FGreen:=nil;
  FBlue:=nil;
  FAlpha:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRGBToColor}

procedure RGBToColorChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TRGBToColor(Context) do begin
    FColor.Value:=Color32(IntCut(FRed.Value, 0, $FF), IntCut(FGreen.Value, 0, $FF), IntCut(FBlue.Value, 0, $FF), IntCut(FAlpha.Value, 0, $FF));
  end;
end;

procedure CreateRGBToColor(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRGBToColor.Create(APrototype);
end;

constructor TRGBToColor.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RGBToColorChanged, Self, Environment.Thread);
  FRed:=IntegerInputs[RGBTOCOLORREDNAME];
  //FRed.AddListener(@RGBToColorChanged, Self, Environment.Thread);
  FGreen:=IntegerInputs[RGBTOCOLORGREENNAME];
  //FGreen.AddListener(@RGBToColorChanged, Self, Environment.Thread);
  FBlue:=IntegerInputs[RGBTOCOLORBLUENAME];
  //FBlue.AddListener(@RGBToColorChanged, Self, Environment.Thread);
  FAlpha:=IntegerInputs[RGBTOCOLORALPHANAME];
  //FAlpha.AddListener(@RGBToColorChanged, Self, Environment.Thread);
  FColor:=ColorInputs[RGBTOCOLOROUTPUTNAME];
end;

destructor TRGBToColor.Destroy;
begin
  FMain.RemoveListener(@RGBToColorChanged, Self);
  FMain:=nil;
  //FRed.RemoveListener(@RGBToColorChanged, Self);
  FRed:=nil;
  //FGreen.RemoveListener(@RGBToColorChanged, Self);
  FGreen:=nil;
  //FBlue.RemoveListener(@RGBToColorChanged, Self);
  FBlue:=nil;
  //FAlpha.RemoveListener(@RGBToColorChanged, Self);
  FAlpha:=nil;
  FColor:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TColorToHSV}

procedure ColorToHSVChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AHSV: TAHSV;
begin
  with TColorToHSV(Context) do begin
    AHSV:=ARGBToAHSV(FColor.Value);
    FHue.Value:=AHSV.H*Degree;
    FSaturation.Value:=AHSV.S;
    FValue.Value:=AHSV.V;
    FAlpha.Value:=AHSV.A;
  end;
end;

procedure CreateColorToHSV(APrototype: IPVisualisationPrototype); cdecl;
begin
  TColorToHSV.Create(APrototype);
end;

constructor TColorToHSV.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ColorToHSVChanged, Self, Environment.Thread);
  FColor:=ColorInputs[COLORTOHSVCOLORNAME];
  //FColor.AddListener(@ColorToHSVChanged, Self, Environment.Thread);
  FHue:=FloatInputs[COLORTOHSVHUEOUTPUTNAME];
  FSaturation:=IntegerInputs[COLORTOHSVSATURATIONOUTPUTNAME];
  FValue:=IntegerInputs[COLORTOHSVVALUEOUTPUTNAME];
  FAlpha:=IntegerInputs[COLORTOHSVALPHAOUTPUTNAME];
end;

destructor TColorToHSV.Destroy;
begin
  FMain.RemoveListener(@ColorToHSVChanged, Self);
  FMain:=nil;
  //FColor.RemoveListener(@ColorToHSVChanged, Self);
  FColor:=nil;
  FHue:=nil;
  FSaturation:=nil;
  FValue:=nil;
  FAlpha:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION THSVToColor}

procedure HSVToColorChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AHSV: TAHSV;
begin
  with THSVToColor(Context) do begin
    AHSV.A:=IntCut(FAlpha.Value, 0, $FF);
    AHSV.H:=RealMod(FHue / Degree, 2*Pi);
    if AHSV.H<0
      then AHSV.H:=2*Pi - AHSV.H;
    AHSV.S:=IntCut(FSaturation.Value, 0, $FF);
    AHSV.V:=IntCut(FValue.Value, 0, $FF);
    FColor.Value:=AHSVToARGB(AHSV);
  end;
end;

procedure CreateHSVToColor(APrototype: IPVisualisationPrototype); cdecl;
begin
  THSVToColor.Create(APrototype);
end;

constructor THSVToColor.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@HSVToColorChanged, Self, Environment.Thread);
  FHue:=FloatInputs[HSVTOCOLORHUENAME];
  //FHue.AddListener(@HSVToColorChanged, Self, Environment.Thread);
  FSaturation:=IntegerInputs[HSVTOCOLORSATURATIONNAME];
  //FSaturation.AddListener(@HSVToColorChanged, Self, Environment.Thread);
  FValue:=IntegerInputs[HSVTOCOLORVALUENAME];
  //FValue.AddListener(@HSVToColorChanged, Self, Environment.Thread);
  FAlpha:=IntegerInputs[HSVTOCOLORALPHANAME];
  //FAlpha.AddListener(@HSVToColorChanged, Self, Environment.Thread);
  FColor:=ColorInputs[HSVTOCOLOROUTPUTNAME];
end;

destructor THSVToColor.Destroy;
begin
  FMain.RemoveListener(@HSVToColorChanged, Self);
  FMain:=nil;
  //FHue.RemoveListener(@HSVToColorChanged, Self);
  FHue:=nil;
  //FSaturation.RemoveListener(@HSVToColorChanged, Self);
  FSaturation:=nil;
  //FValue.RemoveListener(@HSVToColorChanged, Self);
  FValue:=nil;
  //FAlpha.RemoveListener(@HSVToColorChanged, Self);
  FAlpha:=nil;
  FColor:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TGeneralStorage}

procedure GeneralStorageMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I     : Integer;
begin
  with TGeneralStorage(Context)
    do for I:=0 to Length(FInputs)-1
      do FOutputs[I].GetFrom(FInputs[I], TPNOLIMIT, NULLVISID);
end;

procedure CreateGeneralStorage(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGeneralStorage.Create(APrototype);
end;

constructor TGeneralStorage.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@GeneralStorageMainCalled, Self, Environment.Thread);
end;

destructor TGeneralStorage.Destroy;
var
  I: Integer;
begin
  FMain.RemoveListener(@GeneralStorageMainCalled, Self);
  FMain:=nil;
  for I:=0 to Length(FInputs)-1 do begin
    FInputs[I]:=nil;
    FOutputs[I]:=nil;
  end;
  SetLength(FInputs, 0);
  SetLength(FOutputs, 0);
  inherited Destroy;
end;

procedure TGeneralStorage.GotInput(Param: IPParam); cdecl;
var
  L: Integer;
begin
  if (Param.ID.Name = GENERALSTORAGEINPUTNAME) then begin
    L:=Length(FInputs);
    SetLength(FInputs, L+1);
    SetLength(FOutputs, L+1);
    FInputs[L]:=Param;
    FOutputs[L]:=Prototype.Inputs[ParamID(GENERALSTORAGEOUTPUTNAME, Param.ID.&Type)];
  end;
end;

{%ENDREGION}
{%REGION TUniversalStorage}

procedure UniversalStorageMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I     : Integer;
begin
  with TUniversalStorage(Context)
    do for I:=0 to Length(FInputs)-1
      do FInputs[I].GetFrom(FInputs[I], TPNOLIMIT, NULLVISID);
end;

procedure CreateUniversalStorage(APrototype: IPVisualisationPrototype); cdecl;
begin
  TUniversalStorage.Create(APrototype);
end;

constructor TUniversalStorage.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@UniversalStorageMainCalled, Self, Environment.Thread);
end;

destructor TUniversalStorage.Destroy;
var
  I: Integer;
begin
  FMain.RemoveListener(@UniversalStorageMainCalled, Self);
  FMain:=nil;
  for I:=0 to Length(FInputs)-1
    do FInputs[I]:=nil;
  SetLength(FInputs, 0);
  inherited Destroy;
end;

procedure TUniversalStorage.GotInput(Param: IPParam); cdecl;
var
  L: Integer;
begin
  if (Param.ID.Name <> MAININPUTNAME) then begin
    L:=Length(FInputs);
    SetLength(FInputs, L+1);
    FInputs[L]:=Param;
  end;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDBASICFFTTRIGGER, @CreateBasicFFTTrigger);
    with CreatePreset('Basic FFT Trigger', VIDBASICFFTTRIGGER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BASICFFTTRIGGERINDEXNAME, 0);
      AddInput(This, BASICFFTTRIGGERSCALENAME, 20.0);
      AddInput(This, BASICFFTTRIGGERFFTDATAOUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDROUNDER, @CreateRounder);
    with CreatePreset('Rounder', VIDROUNDER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Deprecated');
      AddTag(TAGDEPRECATED);
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, ROUNDERINPUTNAME, 0.0);
      AddInput(This, ROUNDERSCALENAME, 1.0);
      AddInput(This, ROUNDEROUTPUTNAME, 0);
    end;
    RegisterVis(VIDINT, @CreateInt);
    with CreatePreset('Int (Not Round)', VIDINT) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Deprecated');
      AddTag(TAGDEPRECATED);
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, INTINPUTNAME, 0.0);
      AddInput(This, INTOUTPUTNAME, 0);
    end;
    RegisterVis(VIDADDER, @CreateAdder);
    with CreatePreset('+', VIDADDER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Math.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, ADDERINPUT1NAME, 0.0);
      AddInput(This, ADDERINPUT2NAME, 0.0);
      AddInput(This, ADDEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDSWITCH, @CreateSwitch);
    with CreatePreset('Switch', VIDSWITCH) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SWITCHINDEXNAME, 0);
      AddInput(This, SWITCHOUTPUT1NAME);
      AddInput(This, SWITCHOUTPUT2NAME);
      AddInput(This, SWITCHOUTPUT3NAME);
      AddInput(This, SWITCHOUTPUT4NAME);
    end;
    RegisterVis(VIDSWITCH10, @CreateSwitch10);
    with CreatePreset('Switch (10x)', VIDSWITCH10) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SWITCH10INDEXNAME, 0);
      AddInput(This, SWITCH10OUTPUT1NAME);
      AddInput(This, SWITCH10OUTPUT2NAME);
      AddInput(This, SWITCH10OUTPUT3NAME);
      AddInput(This, SWITCH10OUTPUT4NAME);
      AddInput(This, SWITCH10OUTPUT5NAME);
      AddInput(This, SWITCH10OUTPUT6NAME);
      AddInput(This, SWITCH10OUTPUT7NAME);
      AddInput(This, SWITCH10OUTPUT8NAME);
      AddInput(This, SWITCH10OUTPUT9NAME);
      AddInput(This, SWITCH10OUTPUT10NAME);
      AddInput(This, SWITCH10OVERFLOWCOUNTOUTPUTNAME, 0);
      AddInput(This, SWITCH10OVERFLOWOUTPUTNAME);
    end;
    RegisterVis(VIDMULTICALLER, @CreateMulticaller);
    with CreatePreset('Multicaller (4x)', VIDMULTICALLER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, MULTICALLEROUTPUT1NAME);
      AddInput(This, MULTICALLEROUTPUT2NAME);
      AddInput(This, MULTICALLEROUTPUT3NAME);
      AddInput(This, MULTICALLEROUTPUT4NAME);
    end;
    RegisterVis(VIDCOUNTER, @CreateCounter);
    with CreatePreset('Counter', VIDCOUNTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Counting');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COUNTERMAXNAME, 4);
      AddInput(This, COUNTERCALLCOUNTOUTPUTNAME, 0);
    end;
    RegisterVis(VIDLIMITCALLER, @CreateLimitCaller);
    with CreatePreset('Limit Caller', VIDLIMITCALLER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, LIMITCALLINPUTNAME, 0.0);
      AddInput(This, LIMITCALLLIMITNAME, 0.3);
      AddInput(This, LIMITCALLOUTPUTNAME);
    end;
    RegisterVis(VIDSETCOLOR, @CreateSetColor);
    with CreatePreset('Set Color', VIDSETCOLOR) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SETCOLORINPUT1NAME, $FFFF0000);
      AddInput(This, SETCOLORINPUT2NAME, $FF00FF00);
      AddInput(This, SETCOLORINPUT3NAME, $FF0000FF);
      AddInput(This, SETCOLOROUTPUT1NAME, $FFFF0000);
      AddInput(This, SETCOLOROUTPUT2NAME, $FF00FF00);
      AddInput(This, SETCOLOROUTPUT3NAME, $FF0000FF);
    end;
    RegisterVis(VIDLIMITSTOPCALLER, @CreateLimitStopCaller);
    with CreatePreset('Limit Stop Caller', VIDLIMITSTOPCALLER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, LIMITSTOPCALLERINPUTNAME, 0.0);
      AddInput(This, LIMITSTOPCALLERLIMITNAME, 0.3);
      AddInput(This, LIMITSTOPCALLEROUTPUTNAME);
    end;
    RegisterVis(VIDPEAKTRIGGER, @CreatePeakTrigger);
    with CreatePreset('Peak Trigger', VIDPEAKTRIGGER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Audio');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, PEAKTRIGGERSCALENAME, 20.0);
      AddInput(This, PEAKTRIGGEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDCOLORIZER, @CreateColorizer);
    with CreatePreset('Colorizer', VIDCOLORIZER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Colors');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COLORIZERCOLOR1NAME, $FFFF0000);
      AddInput(This, COLORIZERCOLOR2NAME, $FF00FF00);
      AddInput(This, COLORIZERBETANAME, 127);
      AddInput(This, COLORIZEROUTPUTNAME, $FF7F7F00);
    end;
    RegisterVis(VIDBACKCOUNTER, @CreateBackCounter);
    with CreatePreset('Back Counter', VIDBACKCOUNTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Counting');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BACKCOUNTERMINNAME, 0);
      AddInput(This, BACKCOUNTERMAXNAME, 3);
      AddInput(This, BACKCOUNTERSTEPNAME, 1);
      AddInput(This, BACKCOUNTERCOUNTOUTPUTNAME, 0);
    end;
    RegisterVis(VIDMULTIPLYER, @CreateMultiplyer);
    with CreatePreset('*', VIDMULTIPLYER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Math.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, MULTIPLYERINPUT1NAME, 0.0);
      AddInput(This, MULTIPLYERINPUT2NAME, 0.0);
      AddInput(This, MULTIPLYEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDINTTOFLOAT, @CreateIntToFloat);
    with CreatePreset('integer to float converter', VIDINTTOFLOAT) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Deprecated');
      AddTag(TAGDEPRECATED);
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, INTTOFLOATINPUTNAME, 0);
      AddInput(This, INTTOFLOATOUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDMULTINUMBERER, @CreateMultiNumberer);
    with CreatePreset('Multi Numberer', VIDMULTINUMBERER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, MULTINUMBERERINPUTNAME, 0.0);
      AddInput(This, MULTINUMBEREROUTPUT1NAME, 0.0);
      AddInput(This, MULTINUMBEREROUTPUT2NAME, 0.0);
      AddInput(This, MULTINUMBEREROUTPUT3NAME, 0.0);
      AddInput(This, MULTINUMBEREROUTPUT4NAME, 0.0);
    end;
    RegisterVis(VIDSQRT, @CreateSqrt);
    with CreatePreset('Sqrt', VIDSQRT) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Math.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SQRTINPUTNAME, 1.0);
      AddInput(This, SQRTOUTPUTNAME, 1.0);
    end;
    RegisterVis(VIDBOOLEANCALLER, @CreateBooleanCaller);
    with CreatePreset('Boolean Caller', VIDBOOLEANCALLER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BOOLEANCALLERINPUTNAME, false);
      AddInput(This, BOOLEANCALLEROUTPUTNAME);
    end;
    RegisterVis(VIDSTEPCOUNTER, @CreateStepCounter);
    with CreatePreset('Step Counter', VIDSTEPCOUNTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Counting');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, STEPCOUNTERMAXNAME, 100.0);
      AddInput(This, STEPCOUNTERMINNAME, 0.0);
      AddInput(This, STEPCOUNTERSTARTNAME, 0.0);
      AddInput(This, STEPCOUNTERSTEPNAME, 1.0);
      AddInput(This, STEPCOUNTEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDSTEPBACKCOUNTER, @CreateStepBackCounter);
    with CreatePreset('Step Back Counter', VIDSTEPBACKCOUNTER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Counting');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, STEPBACKCOUNTERMAXNAME, 100.0);
      AddInput(This, STEPBACKCOUNTERMINNAME, 0.0);
      AddInput(This, STEPBACKCOUNTERSTARTNAME, 0.0);
      AddInput(This, STEPBACKCOUNTERSTEPNAME, 1.0);
      AddInput(This, STEPBACKCOUNTEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDNOTTRIGGER, @CreateNotTrigger);
    with CreatePreset('Not', VIDNOTTRIGGER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Logic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, NOTTRIGGERINPUTNAME, false);
      AddInput(This, NOTTRIGGEROUTPUTNAME, true);
    end;
    RegisterVis(VIDDIVIDER, @CreateDivider);
    with CreatePreset('/', VIDDIVIDER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Math.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, DIVIDERINPUT1NAME, 0.0);
      AddInput(This, DIVIDERINPUT2NAME, 1.0);
      AddInput(This, DIVIDEROUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDMODULU, @CreateModulu);
    with CreatePreset('Modulu', VIDMODULU) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Math.Basic');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, MODULUINPUT1NAME, 0.0);
      AddInput(This, MODULUINPUT2NAME, 1.0);
      AddInput(This, MODULUOUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDCHANGECALLER, @CreateChangeCaller);
    with CreatePreset('Change Caller', VIDCHANGECALLER) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Calling');
      AddTag(TAGPREDEFINED);
      AddInput(This, CHANGECALLERINPUTNAME, 0.0);
      AddInput(This, CHANGECALLERCHANGEDOUTPUTNAME);
    end;
    RegisterVis(VIDCOLORTORGB, @CreateColorToRGB);
    with CreatePreset('Color to RGB', VIDCOLORTORGB) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Colors');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COLORTORGBCOLORNAME, $FFFFFF00);
      AddInput(This, COLORTORGBREDOUTPUTNAME, TVInteger($FF));
      AddInput(This, COLORTORGBGREENOUTPUTNAME, TVInteger($FF));
      AddInput(This, COLORTORGBBLUEOUTPUTNAME, TVInteger($00));
      AddInput(This, COLORTORGBALPHAOUTPUTNAME, TVInteger($FF));
    end;
    RegisterVis(VIDRGBTOCOLOR, @CreateRGBToColor);
    with CreatePreset('RGB to Color', VIDRGBTOCOLOR) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Colors');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, RGBTOCOLORREDNAME, TVInteger($FF));
      AddInput(This, RGBTOCOLORGREENNAME, TVInteger($00));
      AddInput(This, RGBTOCOLORBLUENAME, TVInteger($00));
      AddInput(This, RGBTOCOLORALPHANAME, TVInteger($FF));
      AddInput(This, RGBTOCOLOROUTPUTNAME, $FFFF0000);
    end;
    RegisterVis(VIDCOLORTOHSV, @CreateColorToHSV);
    with CreatePreset('Color to HSV', VIDCOLORTOHSV) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Colors');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COLORTOHSVCOLORNAME, $FFFFFF00);
      AddInput(This, COLORTOHSVHUEOUTPUTNAME, 60.0);
      AddInput(This, COLORTOHSVSATURATIONOUTPUTNAME, TVInteger($FF));
      AddInput(This, COLORTOHSVVALUEOUTPUTNAME, TVInteger($FF));
      AddInput(This, COLORTOHSVALPHAOUTPUTNAME, TVInteger($FF));
    end;
    RegisterVis(VIDHSVTOCOLOR, @CreateHSVToColor);
    with CreatePreset('HSV to Color', VIDHSVTOCOLOR) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Colors');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, HSVTOCOLORHUENAME, 180.0);
      AddInput(This, HSVTOCOLORSATURATIONNAME, TVInteger($FF));
      AddInput(This, HSVTOCOLORVALUENAME, TVInteger($FF));
      AddInput(This, HSVTOCOLORALPHANAME, TVInteger($FF));
      AddInput(This, HSVTOCOLOROUTPUTNAME, $FF00FFFF);
    end;
    RegisterVis(VIDCALLSTORAGE, @CreateGeneralStorage);
    with CreatePreset('Call Storage', VIDCALLSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Deprecated');
      AddTag(TAGDEPRECATED);
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, CALLSTORAGEINPUTNAME);
      AddInput(This, CALLSTORAGEOUTPUTNAME);
    end;
    RegisterVis(VIDINTEGERSTORAGE, @CreateGeneralStorage);
    with CreatePreset('Integer Storage', VIDINTEGERSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, INTEGERSTORAGEINPUTNAME, 0);
      AddInput(This, INTEGERSTORAGEOUTPUTNAME, 0);
    end;
    RegisterVis(VIDFLOATSTORAGE, @CreateGeneralStorage);
    with CreatePreset('Float Storage', VIDFLOATSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, FloatSTORAGEINPUTNAME, 0.0);
      AddInput(This, FLOATSTORAGEOUTPUTNAME, 0.0);
    end;
    RegisterVis(VIDSTRINGSTORAGE, @CreateGeneralStorage);
    with CreatePreset('String Storage', VIDSTRINGSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, STRINGSTORAGEINPUTNAME, '');
      AddInput(This, STRINGSTORAGEOUTPUTNAME, '');
    end;
    RegisterVis(VIDCOLORSTORAGE, @CreateGeneralStorage);
    with CreatePreset('Color Storage', VIDCOLORSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, COLORSTORAGEINPUTNAME, $FF000000);
      AddInput(This, COLORSTORAGEOUTPUTNAME, $FF000000);
    end;
    RegisterVis(VIDBOOLEANSTORAGE, @CreateGeneralStorage);
    with CreatePreset('Boolean Storage', VIDBOOLEANSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BOOLEANSTORAGEINPUTNAME, false);
      AddInput(This, BOOLEANSTORAGEOUTPUTNAME, false);
    end;
    RegisterVis(VIDBUFFERSTORAGE, @CreateGeneralStorage);
    with CreatePreset('Buffer Storage', VIDBUFFERSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, BUFFERSTORAGEINPUTNAME, EmptyBuffer);
      AddInput(This, BUFFERSTORAGEOUTPUTNAME, EmptyBuffer);
    end;
    RegisterVis(VIDUNIVERSALSTORAGE, @CreateUniversalStorage);
    with CreatePreset('Universal Storage', VIDUNIVERSALSTORAGE) do begin
      AddTag(TAGLISTED);
      AddTag('Trigger.Storage');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, UNIVERSALSTORAGEVARNAME, 100);
    end;
  end;
end;

{%ENDREGION}

end.

