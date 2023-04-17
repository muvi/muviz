unit VisTypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MInterfacedObject, VisType, PluginType, SpectrumData,
  VPBuffers, VPBufferUnit, AdvFunc, AdvClasses, ParamType, Dialogs,
  MFileUtil, AdvType;

const
  KeyboardOFileID       = 'Key Layout';
  KeyboardFileID        = MuviFileID+KeyboardOFileID;
  KeyboardFileIDSize    = MuviFileIDSize+GenerelFileIDSize;
  PresetOFileID     = 'VisPresets';
  Preset2OFileID    = 'Presets v2';
  Preset3OFileID    = 'Presets v3';
  Preset4OFileID    = 'Presets v4';
  Preset1FileID     = MuviFileID+PresetOFileID;
  Preset2FileID     = MuviFileID+Preset2OFileID;
  Preset3FileID     = MuviFileID+Preset3OFileID;
  Preset4FileID     = MuviFileID+Preset4OFileID;
  PresetFileIDSize  = MuviFileIDSize+GenerelFileIDSize;
  VIS_ID_SIZE       = 8;
  lplPluginMissing  = 251;

type
  TBasicVisualisation    = class;
  TVisualisation         = class;
  TVisWinEvent           = procedure of object;
  TVisKeyType            = (ktNothing,ktPreset,ktKeyboard);
  TVisKeyType2           = (ktNothing2,ktPreset2,ktKeyboard2,ktParam2);
  TVisKeyValue           = Integer;
  //TVisKeyType2           = (ktNothing,ktPreset,ktKeyboard)
  TVisIndex              = Integer;

  TVisOutputConverters   = record
    _Set: array [Boolean] of TVisOutputSet;
    _Get: TVisOutputGet;
  end;

  TVisParamTypeDesc      = packed record
    Size                  : Cardinal;
    Name                  : TParamName;
    Color                 : CvColor;
    Picture               : TParamPic;
    DefaultConverters     : TVisOutputConverters;
  end;

  PVisParamTypeDesc      = ^TVisParamTypeDesc;
  TVisParamTypeDescs     = packed array [0..0] of TVisParamTypeDesc;
  PVisParamTypeDescs     = ^TVisParamTypeDescs;
  TVisOutputConverterDesc= packed array [0..0] of TVisOutputConverters;
  PVisOutputConverterDesc= ^TVisOutputConverterDesc;

  TVisParamTypeMem       = record
    Count     : Integer;
    Size      : Cardinal;
    Types     : PVisParamTypeDescs;
    Converters: PVisOutputConverterDesc;
  end;

  TVisParamTypes         = class
  strict private
    FData: TVisParamTypeMem;
    function CreateTypeMem(ACount: Integer): TVisParamTypeMem; inline;
    procedure DestroyTypeMem(var AMem: TVisParamTypeMem); inline;
    procedure CopyTypes(const Src: TVisParamTypeMem; var Dst: TVisParamTypeMem);
    procedure CopyConverters(const Src: TVisParamTypeMem; var Dst: TVisParamTypeMem);

    function GetConverters(Param: TVisParamType; Output: TVisOutputType): TVisOutputConverters; inline;
    procedure SetConverters(Param: TVisParamType; Output: TVisOutputType; Value: TVisOutputConverters); inline;
    function GetParamType(AType: TVisParamType): TVisParamTypeDesc; inline;
    function GetParamTypePtr(AType: TVisParamType): PVisParamTypeDesc; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function AddParamType(AType: TVisParamType; ASize: Cardinal; const AName: TParamName; AColor: CvColor; constref APicture: TParamPic; ADefaultConverters: TVisOutputConverters): Boolean;
    procedure SetConverter(AParamType: TVisParamType; AOutputType: TVisOutputType; ASet,ASet_CallChanged: TVisOutputSet; AGet: TVisOutputGet);
    function SizeOfParam(AType: TVisParamType): Cardinal; inline;
    function SizeOfOutput(AType: TVisOutputType): Cardinal; inline;
    function ParamTypeExists(AType: TVisParamType): Boolean; inline;
    property Count: Integer read FData.Count;
    property Converters[Param: TVisParamType; Output: TVisOutputType]: TVisOutputConverters read GetConverters write SetConverters;
    property ParamTypes[AType: TVisParamType]: TVisParamTypeDesc read GetParamType; default;
    property ParamTypePtr[AType: TVisParamType]: PVisParamTypeDesc read GetParamTypePtr;
  end;

  {TVisKey2               = class
  private
    FKeyType   : TVisKeyType2;
    FBackChange: Boolean;
    FValueIndex: Integer;
    FValueData : Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveKeyboardsToStream(const Keyboards: TVisKeyboards; Stream: TStream);
    function LoadKeyboardsFromStream(var Keyboards: TVisKeyboards; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TLoadFileResult;
    procedure SaveKeyboardsToFile(const Keyboards: TVisKeyboards; const FileName: string);
    function LoadKeyboardsFromFile(var Keyboards: TVisKeyboards; const FileName: string; const Append: Boolean = false): TLoadFileResult;

    property BackChange: Boolean read FBackChange write FBackChange;
    property KeyType: TVisKeyType2 read FKeyType write SetKeyType;
    property ValueIndex: Integer read FValueIndex write FValueIndex;
  end;}

  TVisKey                = record
    KeyType   : TVisKeyType;
    BackChange: Boolean;
    Value     : TVisKeyValue;
  end;

  TVisKeys               = array [Char] of TVisKey;

  TVisKeyboard           = record
    Name: string;
    Keys: TVisKeys;
  end;

  PVisKeyboard           = ^TVisKeyboard;
  TVisKeyboards          = array of TVisKeyboard;
  PVisKeyboards          = ^TVisKeyboards;
  TVisID                 = type UInt64;
  TVisIDStr              = array [1..8] of Char;
  TVisInputPos           = type Pointer;

  TSVisParamDesc         = record
    Name  : ShortString;
    AType : TVisParamType;
    Change: TVisProc;
    Offset: TVisInputPos;
  end;

  PSVisParamDesc         = ^TSVisParamDesc;

  TSVisOutputDesc        = record
    Name:  ShortString;
    AType: TVisOutputType;
  end;

  TVisOutputDesc         = array of TSVisOutputDesc;
  TVisParamDesc          = array of TSVisParamDesc;
  TVisCanvasDesc         = array of ShortString;

  TVisType               = object
  strict private
    function GetParamDesc(Index: Integer): TSVisParamDesc;
  public
    Name         : ShortString;
    VisParamSize : Cardinal;
    VisParamDesc : TVisParamDesc;
    VisProc      : TVisProc;
    WorkspaceSize: Cardinal;
    VisID        : TVisID;
    InitWorkspace: TVisProc;
    FreeWorkspace: TVisProc;
    FirstAction  : TVisProc;
    InitialValues: Pointer;
    OutputDesc   : TVisOutputDesc;
    C1Desc       : ShortString;
    C2Desc       : ShortString;
    C3Desc       : ShortString;
    CanvasDesc   : TVisCanvasDesc;

    property ParamDesc[Index: Integer]: TSVisParamDesc read GetParamDesc;
  end;

  PVisType               = ^TVisType;
  TPresetIndex           = LongInt;

  TPresetOutputInfo      = packed record
    Layer,Param: TPresetIndex;
  end;

  TNamedPresetOutput     = record
    OutputInfo: TPresetOutputInfo;
    Name      : string;
  end;

  PNamedPresetOutput     = ^TNamedPresetOutput;
  TPresetVisOutputs      = array of TPresetOutputInfo;
  TNamedPresetOutputs    = array of TNamedPresetOutput;
  TVisParams             = type Pointer;
  TVisWorkspace          = type Pointer;

  TPresetVis             = object
    VisID     : Integer;
    C1,C2,C3  : vpColor;
    VisParams : TVisParams;
    VisOutputs: TPresetVisOutputs;
    DesignPos : TPoint;
    Expanded  : Boolean;
    DestCanvas: TPresetOutputInfo;
    class function Create(const AIndex: TVisIndex; AInit: Boolean = true): TPresetVis;
    class function Create_ID(const AVisID; AInit: Boolean = true): TPresetVis;
    procedure Produce(const AIndex: TVisIndex; AInit: Boolean = true);
    procedure Produce_ID(const AVisID; AInit: Boolean = true);
    function Copy: TPresetVis;
    procedure Destroy;
  end;

  TVisParam              = type Pointer;
  TPresetLayers          = array of TPresetVis;
  PPresetLayers          = ^TPresetLayers;
  TVisLayers             = array of TBasicVisualisation;
  TPresetReconCheck      = function (var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
  PPresetVis             = ^TPresetVis;

  TPreset                = class
  public
    Name                  : string;
    Layers,Triggers       : TPresetLayers;
    WSize                 : Cardinal;
    Params,Outputs        : TNamedPresetOutputs;
  private
    procedure Reconnect(AReconFunc: TPresetReconCheck; AIndex1: TPresetIndex = 0; AIndex2: TPresetIndex = 0);
    function GetItem(Index: TPresetIndex): PPresetVis;
    //Ohne Reconnect:
    procedure DeleteNR(AIndex: TPresetIndex); inline;
    procedure InsertNR(AIndex: TPresetIndex; const AVis: TPresetVis); inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(AIndex: TPresetIndex);
    procedure Swap(AIndex1,AIndex2: TPresetIndex);
    procedure Insert(AIndex: TPresetIndex; const AVis: TPresetVis);
    procedure Add(const AVis: TPresetVis; AAutoDraw: Boolean = true);
    procedure Move(ASource,ADest: TPresetIndex);
    procedure CopyFrom(ASource: TPreset);
    procedure CopyTo(Dest: TPreset);

    procedure AddParam(const AInfo: TPresetOutputInfo; const AName: string = '');
    procedure DeleteParam(AIndex: Integer);
    procedure InsertParam(AIndex: Integer; const AInfo: TPresetOutputInfo; const AName: string = '');
    function IsDefaultParamName(const AName: string): Boolean;
    procedure AddOutput(const AInfo: TPresetOutputInfo; const AName: string = '');
    procedure DeleteOutput(AIndex: Integer);
    procedure InsertOutput(AIndex: Integer; const AInfo: TPresetOutputInfo; const AName: string = '');
    function IsDefaultOutputName(const AName: string): Boolean;

    function Copy: TPreset;
    property Items[Index: TPresetIndex]: PPresetVis read GetItem;
    //procedure CopyLayer(const Src: TPresetVis; out Dest: TPresetVis);
    //procedure CopyPresetParam(const Src: TNamedPresetOutput; out Dest: TNamedPresetOutput);
    //procedure InsertLayer_(var ALayers: TPresetLayers; AIndex: Integer; AVisIndex: TVisIndex);
  end;

  TMuviLoadResult   = record
    Success : TLoadFileResult;
    FailInfo: TVisID;
  end;

  TPresets               = class
  private
    FPresets: array of TPreset;
    function GetPreset(AIndex: Integer): TPreset; inline;
    function GetCount: Integer; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToStream1(Stream: TStream);
    function LoadFromStream1(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;
    procedure SaveToFile1(const FileName: string);
    function LoadFromFile1(const FileName: string; Append: Boolean = false): TMuviLoadResult;

    procedure SaveToStream2(Stream: TStream);
    function LoadFromStream2(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;
    procedure SaveToFile2(const FileName: string);
    function LoadFromFile2(const FileName: string; Append: Boolean = false): TMuviLoadResult;

    procedure SaveToStream3(Stream: TStream);
    function LoadFromStream3(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;
    procedure SaveToFile3(const FileName: string);
    function LoadFromFile3(const FileName: string; Append: Boolean = false): TMuviLoadResult;

    procedure SaveToStream4(Stream: TStream);
    function LoadFromStream4(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;
    procedure SaveToFile4(const FileName: string);
    function LoadFromFile4(const FileName: string; Append: Boolean = false): TMuviLoadResult;

    procedure SaveToStream(Stream: TStream);
    function LoadFromStream(Stream: TStream; Append: Boolean = false): TMuviLoadResult;
    procedure SaveToFile(const FileName: string);
    function LoadFromFile(const FileName: string; Append: Boolean = false): TMuviLoadResult;

    function Add: TPreset;
    procedure Add(APreset: TPreset);
    procedure Clear;
    procedure Remove(Index: Integer);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TPreset read GetPreset; default;
  end;

  //PPresets               = ^TPresets;
  TVisSetFunc            = procedure (const APreset: Integer) of object;
  TVisGetFunc            = function: Integer of object;
  TVisComposition        = class;

  TVisOutput             = record
    AType: TVisOutputType;
    Data : TVisOutputData;
    DoSet: TVisOutputSet;
    DoGet: TVisOutputGet;
  end;

  TVCOutput              = record
    Output: TVisOutput;
    VC    : TVisComposition;
  end;

  PVisOutput             = ^TVisOutput;
  PVCOutput              = ^TVCOutput;
  TVisOutputs            = array of TVisOutput;
  TCColors               = packed array [0..2] of vpColor;

  TVisBGThread           = class (TThread)
  private
    FThreadProcs: TLQueue;
    FFinishProcs: TLQueue;
    FSleepTime  : Cardinal;
    FFinished   : Boolean;
    function GetTaskCount: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SyncFinish;
    procedure Push(AVis: TVisualisation; AProc: TVisProc; AFinished: TVisProc = nil);
    procedure Clear;

    property SleepTime: Cardinal read FSleepTime write FSleepTime;
    property TaskCount: Cardinal read GetTaskCount;
  end;

  TBGThreadPush          = procedure (AVis: TVisualisation; AProc: TVisProc; AFinished: TVisProc = nil) of object;

  TBasicVisualisation    = class (TMInterfacedObject, IVisualisation)
  private
    FVisID       : TVisIndex;
    FCColors     : TCColors;
    FCanvas      : IMCanvas;
    FSpectrumData: ISpectrumData;
    FBGThread    : TBGThreadPush;
    FOpenCanvas  : array of IMCanvas;
    function GetCanvas(Index: MVIndex): IMCanvas;
    function GetOpenCanvasCount: Integer;
    procedure SetOpenCanvasCount(Value: Integer);
  protected
    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
    function GetC1: vpColor; stdcall;
    function GetC2: vpColor; stdcall;
    function GetC3: vpColor; stdcall;
    procedure SetC1(const AValue: vpColor);
    procedure SetC2(const AValue: vpColor);
    procedure SetC3(const AValue: vpColor);
    procedure OutputOnParam(var AOutput: TVisOutput; AParam: Integer); virtual; abstract;
    procedure OutputOnParamDirect(var AOutput: TVisOutput; AParam: Integer); virtual; abstract;
    procedure SetCanvas(Index: MVIndex; ACanvas: IMCanvas); stdcall;

    property BGThread: TBGThreadPush read FBGThread;
    property Canvas: IMCanvas read FCanvas;
    property OpenCanvas[Index: MVIndex]: IMCanvas read GetCanvas write SetCanvas;
    property OpenCanvasCount: Integer read GetOpenCanvasCount write SetOpenCanvasCount;
    property SpectrumData: ISpectrumData read FSpectrumData;
  public
    constructor Create(const ASource: TPresetVis; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; APreQueue: TLQueue; ABGThread: TBGThreadPush); virtual;
    destructor Destroy; override;

    function OutputPtr(AIndex: Cardinal): PVisOutput; virtual; abstract;
    procedure SetOutput(const Index: LongWord; const Value); stdcall; virtual; abstract;
    procedure Visualize; virtual; abstract;

    property C1: vpColor read GetC1 write SetC1;
    property C2: vpColor read GetC2 write SetC2;
    property C3: vpColor read GetC3 write SetC3;
    property VisIndex: TVisIndex read FVisID;
  end;

  TVisualisation         = class (TBasicVisualisation,IVisualisation2, IVisualisation3)
  private
    FVisParams   : TVisParams;
    FWorkspace   : TVisWorkspace;
    FVisOutputs  : TVisOutputs;

    function GetTypeOf: TVisType;
  protected
    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
    procedure OutputOnParam(var AOutput: TVisOutput; AParam: Integer); override;
    procedure OutputOnParamDirect(var AOutput: TVisOutput; AParam: Integer); override;

    class procedure DestroyVisType(const Index: Integer);
    class procedure ClearVis;
  public
    constructor Create(const ASource: TPresetVis; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; APreQueue: TLQueue; ABGThread: TBGThreadPush);  override;
    destructor Destroy; override;

    procedure CallVisProc(const AVisProc: TVisProc);
    function OutputPtr(AIndex: Cardinal): PVisOutput; override;
    procedure SetOutput(const Index: LongWord; const Value); stdcall; override;
    procedure Visualize; override;
    procedure ThreadPush(AProc: TVisProc; AFinished: TVisProc = nil); stdcall;

    class function RegisterVis(const AVisType: TVisType): Integer;
    class procedure CreateAndRegisterVis(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const ACanvasNames: array of string; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
    class procedure CreateAndRegisterVis2(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil); stdcall;
    class procedure UnRegisterVis(const ID: Cardinal);
    class function VisCount: Integer;
    class function Visualisations(const Index: TVisIndex): TVisType;
    class function VisPtr(const Index: TVisIndex): PVisType;
    class function FindVisID(const VisID): TVisIndex;
    class function ParamType(const AVisIndex: TVisIndex; AParam: Integer): TVisParamType;

    property TypeOf: TVisType read GetTypeOf;
  end;

  TBasicVisComposition   = class
  private
    FLayers      : TVisLayers;
    FTriggers    : TVisLayers;
    F_Vising     : Boolean;
    FCanvas      : IMCanvas;
    FSpectrumData: ISpectrumData;
    FCreateOrder : array of Integer;
  protected
    procedure ToVisOutput(const ASource: TPresetOutputInfo; var ADest: TVisOutput);
    procedure ToVisOutputDirect(const ASource: TPresetOutputInfo; var ADest: TVisOutput);
    procedure MatchOutputs(const Source: TPreset);
    procedure SetPreOuts(AQueue: TLQueue; var ACanvas: IMCanvas; const ASource: ISpectrumData);
    function GetCanvasAt(ACanvasSource: TPresetOutputInfo): IMCanvas;

    property Canvas: IMCanvas read FCanvas;
    property SpectrumData: ISpectrumData read FSpectrumData;
  public
    constructor Create(const ASource: TPreset; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; AQueue: TLQueue; ABGThread: TBGThreadPush);
    destructor Destroy; override;
    procedure Visualize; virtual;
  end;

  TVisComposition        = class (TBasicVisComposition)
  private
    FParams       : array of TVCOutput;
    FPreset       : TPreset;
    FChangedParams: TLQueue;
    //Gibt an, ob die Visualisierung noch Teil einer TVisCompositions Liste ist
    //und somit nicht durch z.B. den TVisThread freigegeben werden darf
    //TVisComposition wird mit FDependent=true erzeugt
    FDependent    : Boolean;
    function GetParamType(Index: Cardinal): TVisOutputType;
    function GetParamCount: Cardinal;
    function GetParamName(Index: Cardinal): string;
    function GetName: string;
  protected
    procedure PushChange(const AOutput: TVisOutput; const AValue);
  public
    constructor Create(ASource: TPreset; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush);
    destructor Destroy; override;
    procedure Free; inline;
    procedure SetIndependence; inline;
    function ParamPtr(Index: LongWord): PVCOutput;
    procedure SetParam(Index: LongWord; const Value); stdcall;
    procedure GetParam(Index: LongWord; out Value); stdcall;
    procedure ChangeParams;
    procedure Visualize; override;

    property Name: string read GetName;
    property ParamCount: Cardinal read GetParamCount;
    property ParamTypes[Index: Cardinal]: TVisOutputType read GetParamType;
    property ParamNames[Index: Cardinal]: string read GetParamName;
  end;

  TPresetLoader          = class;
  TPresetLoadedProc      = procedure (Index: Cardinal) of object;
  TPresetBufferMode      = (bmManual = 0,bmKeep = 1,bmReuse = 2);

  TPresetLoaderThread    = class (TThread)
  private
    FOwner       : TPresetLoader;
    FTasks       : TLQueue;
    FActivePreset: TPreset;
    FTasksSolved : Integer;
    function GetTaskCount: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPresetLoader);
    destructor Destroy; override;
    procedure PushTask(APreset: TPreset; var ADest: TVisComposition; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
    property ActivePreset: TPreset read FActivePreset;
    property TaskCount: Integer read GetTaskCount;
    property TasksSolved: Integer read FTasksSolved;
  end;

  TPresetLoader          = class
  private
    FPresetLoaderThread: TPresetLoaderThread;
    FOnStarted         : TNotifyEvent;
    FOnFinished        : TNotifyEvent;
    function GetActivePreset: TPreset;
    function GetIsRunning: Boolean;
    function GetMaxProgress: Integer;
    function GetProgress: Integer;
  protected
    procedure Finished; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadPreset(const APreset: TPreset; var Dest: TVisComposition; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
    procedure PreLoad;
    procedure AfterLoad;
    procedure PushPreset(const APreset: TPreset; var Dest: TVisComposition; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
    //procedure LoadPresets(const APresets: TPresets; var Dest: TVisCompositions; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
    property ActivePreset: TPreset read GetActivePreset;
    property IsRunning: Boolean read GetIsRunning;
    property MaxProgress: Integer read GetMaxProgress;
    property Progress: Integer read GetProgress;

    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
    property OnStarted: TNotifyEvent read FOnStarted write FOnStarted;
  end;

  TVisCompositions       = class
  private
    FItems       : array of TVisComposition;
    FMode        : TPresetBufferMode;
    FPresets     : TPresets;
    FCanvas      : IMCanvas;
    FSpectrumData: ISpectrumData;
    FBGThread    : TBGThreadPush;
    FPresetLoader: TPresetLoader;
    function GetItem(Index: Integer): TVisComposition;
    function GetItemDirect(Index: Integer): TVisComposition;
    function GetPresets: TPresets;
  protected
  public
    constructor Create(APresets: TPresets; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; APresetLoader: TPresetLoader; AMode: TPresetBufferMode = bmManual);
    destructor Destroy; override;
    //Läd ein Preset und verlässt die Methode schon befor es fertig ist
    procedure LoadItem(AIndex: Integer);
    //Läd ein Preset und verlässt die Methode erst wenn es fertig ist
    procedure LoadItemHard(AIndex: Integer);
    procedure UnloadItem(AIndex: Integer);
    function Loaded(AIndex: Integer): Boolean;
    procedure Clear;
    procedure LoadAll;
    procedure LoadAllHard;
    property DirectItems[Index: Integer]: TVisComposition read GetItemDirect;
    property Items[Index: Integer]: TVisComposition read GetItem; default;
    property Mode: TPresetBufferMode read FMode write FMode;
    property Presets: TPresets read GetPresets;
  end;

function VisOutputConverters(ASet,ASet_CallChanged: TVisOutputSet; AGet: TVisOutputGet): TVisOutputConverters; inline;
function VisType(const AName: string; const ID; const AVisProc: TVisProc; const AParamDesc: TVisParamDesc; const AInitialValues; const AVisParamSize: Cardinal; const AOutputDesc: TVisOutputDesc; const ACanvasDesc: TVisCanvasDesc; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil; const AC1Desc: ShortString = '1. Linienfarbe'; const AC2Desc: ShortString = '2. Linienfarbe'; const AC3Desc: ShortString = 'Hintergrundfarbe'): TVisType; overload;
function VisType(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil; const ACanvasNames: array of string): TVisType; overload;
function ToVisParamDesc(AName: string; AAType: TVisParamType; AChange: TVisProc; AOffset: TVisInputPos): TSVisParamDesc;
//function SizeOfParam(const AType: TVisParamType): Cardinal; inline;
//function SizeOfOutput(const AType: TVisOutputType): Cardinal; inline;
function PresetOutputInfo(ALayer,AParam: Integer): TPresetOutputInfo;
procedure SetVisOutput(const AOutput: TVisOutput; const Value);
procedure GetVisOutput(const AOutput: TVisOutput; out Value);
procedure SetVCOutput(const AOutput: TVCOutput; const Value);
procedure GetVCOutput(const AOutput: TVCOutput; out Value);
function PresetBufferModeToStr(AMode: TPresetBufferMode): string;

operator < (constref m1: TVisType; m2: TVisID) r: Boolean;
operator < (m1: TVisID; constref m2: TVisType) r: Boolean;
operator < (constref m1: TVisType; constref m2: TVisType) r: Boolean;

const
  poiUNDEFINED    : TPresetOutputInfo = (Layer:0; Param: -3);
  poiDEFAULTCANVAS: TPresetOutputInfo = (Layer:0; Param: -1);

var
  VisParamTypes: TVisParamTypes; (***)

implementation

{%REGION Allgemein - VisCompositions}

{$I visconvert.inc}
{$I visconvert2.inc}

type
  TPreOutData = record
    Dest  : PVisOutput;
    Size  : Cardinal;
    AValue: Pointer;
  end;

procedure _DoSetVisOutput__BeforeInit(const Data: TVisOutputData; const Value); stdcall;
var
  AQueue  : TLQueue absolute Data.P;
  ASize   : Cardinal absolute Data.V;
  APreData: TPreOutData;
begin
  with APreData do begin
    Dest:=Data.D;
    Size:=ASize;
    GetMem(AValue,ASize);
    Move(Value,AValue^,ASize);
  end;
  AQueue.Push(APreData);
end;

{%ENDREGION}
{%REGION TVisParamTypes}

{$MACRO ON}
{$DEFINE VisParamTypesDataSize := _VisParamTypeCount*SizeOf(TVisParamTypeDesc)}
{$DEFINE VisOutputConverterDataSize := _VisParamTypeCount*_VisParamTypeCount*2*(SizeOf(TVisOutputGet)+SizeOf(TVisOutputSet))}

const
  EMPTYPARAM      : TVisParamTypeDesc = (Size: $FFFFFFFF; Name: 'Undefined'; Color: $FF000000; Picture: {$I EmptyParamPic.Inc}; DefaultConverters: (_Set: (@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call); _Get: @_DoGetVisOutput__));

  DefaultParamCount     = vBuffer+1;
  {$DEFINE _VisParamTypeCount := DefaultParamCount}
  DefaultConverterOffset= VisParamTypesDataSize;
  DefaultParamSize      = DefaultConverterOffset+VisOutputConverterDataSize;

  vCallPicStr   : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#32#0+'6'+#0+'6'+#0+'6'+#0+'6'+#0+'.'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+')'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+';'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+')'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+';'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#10#0+'@'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'E'+#0#23#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#7#0+'>'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'D'+#0#18#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#5#0+';'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'B'+#0#14#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#3#0+'8'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'@'+#0#10#0#0#0#0#0#0#0#0#0#0#0#0#0#11#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#16#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  vIntPicStr    : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#208#0#193#0#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#234#0#182#0#216#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#230#0#185#0#185#0#224#0#226#0#226#0#226#0#226#0#226#0#226#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#252#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#182#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#254#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#218#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  vRealPicStr   : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#238#2#195#10#177#13#176#13#188#11#225#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#207#7#128#25#128#26#128#26#128#26#128#26#128#26#177#13#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#135#20#155#17#232#3#246#1#248#1#238#2#179#12#128#25#250#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#134#21#158#16#239#2#254#0#255#0#244#1#185#11#128#25#248#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#202#8#128#25#128#26#128#26#128#26#128#26#128#26#171#14#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#235#3#188#11#171#14#170#14#183#12#222#5#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#236#3#216#6#0#0#229#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#175#13#128#26#150#17#196#9#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#235#3#215#6#235#3#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#238#2#248#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#221#5#128#25#229#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#232#3#128#22#150#17#226#4#226#4#226#4#226#4#226#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#128#23#128#26#128#26#128#26#128#26#128#26#128#26#128#26#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#208#7#204#8#204#8#204#8#204#8#204#8#204#8#204#8#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  vStringPicStr : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#231#254#232#254#230#254#148#250#201#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#247#255#128#248#225#254#128#249#186#252#128#248#249#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#213#253#171#251#254#255#128#249#0#0#139#250#246#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#216#253#135#250#217#254#155#251#220#254#135#250#0#0#240#255#172#251#155#250#184#252#250#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#143#250#128#248#128#248#128#248#128#248#250#255#128#248#177#251#201#253#168#251#130#249#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#252#255#212#253#169#251#0#0#0#0#0#0#138#249#245#255#0#0#0#0#0#0#0#0#0#0#247#255#193#252#193#252#193#252#193#252#193#252#193#252#193#252#231#254#131#249#247#255#0#0#240#255#128#248#253#255#0#0#0#0#0#0#0#0#0#0#242#255#158#251#158#251#128#247#141#250#168#251#133#250#128#248#0#0#185#252#202#253#0#0#139#250#209#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#222#254#156#251#0#0#0#0#0#0#128#249#249#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#225#254#131#249#254#255#0#0#247#255#128#248#252#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#160#251#128#248#128#248#128#248#197#253#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#250#255#236#254#254#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  vColorPicStr  : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#165#219#165#219#165#219#165#219#165#219#128#0#128#0#128#0#128#0#128#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#189#0#189#0#189#0#189#0#189#0#25#0#25#0#25#0#25#0#25#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  vBooleanPicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#253#227#253#224#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#253#250#200#247#189#255#237#0#247#246#182#244#178#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#249#251#215#244#176#244#174#245#180#244#172#253#223#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#253#221#243#172#243#172#251#213#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#254#232#244#172#246#187#247#190#244#174#255#240#0#0#250#208#255#241#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#248#198#246#184#0#249#0#247#244#174#250#208#0#0#246#181#244#174#250#207#255#243#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#240#254#234#0#0#0#0#251#213#255#245#0#0#0#248#250#202#243#172#245#179#0#250#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#254#230#245#179#245#180#254#234#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#251#212#244#173#251#213#0#252#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#250#203#246#184#255#239#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#250#205#250#200#0#252#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#251#215#251#212#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#237#251#215#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#255#238#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  vBufferPicStr : string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#255#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0;


constructor TVisParamTypes.Create;
begin
  inherited Create;
  //entspricht FData:=CreateTypeMem(DefaultParamCount);
  with FData do begin
    Count:=DefaultParamCount;
    Size:=DefaultParamSize;
    GetMem(Types,DefaultParamSize);
    Converters:=Pointer(Types)+DefaultConverterOffset;
  end;

  AddParamType(vCall,SizeOf(vpCall),'Call',$FF414141,vCallPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__));
  AddParamType(vInteger,SizeOf(vpInt),'Integer',$FF0000FF,vIntPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Integer_Call));
  AddParamType(vReal,SizeOf(vpReal),'Real',$FF00D305,vRealPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Real_Call));
  AddParamType(vString,SizeOf(vpString),'String',$FFFF0000,vStringPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__String_Call));
  AddParamType(vColor,SizeOf(vpColor),'Color',$FF00E6E6,vColorPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Color_Call));
  AddParamType(vBoolean,SizeOf(vpBool),'Boolean',$FFD8D822,vBooleanPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Boolean_Call));
  AddParamType(vBuffer,SizeOf(vpBuffer),'Buffer',$FFFF00FF,vBufferPicStr,VisOutputConverters(@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__RealBuffer_));

  {//vCall
  SetConverter(vCall,vInteger,@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Integer_Call);
  SetConverter(vCall,vReal,@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Real_Call);
  SetConverter(vCall,vString,@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__String_Call);
  SetConverter(vCall,vColor,@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Color_Call);
  SetConverter(vCall,vBoolean,@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__Boolean_Call);
  SetConverter(vCall,vBuffer,@_DoSetVisOutput__,@_DoSetVisOutput_CallChanged_Call,@_DoGetVisOutput__RealBuffer_);}
  //vInteger
  SetConverter(vInteger,vInteger,@_DoSetVisOutput__Integer,@_DoSetVisOutput_CallChanged_Integer,@_DoGetVisOutput__Integer);
  SetConverter(vInteger,vReal,@_DoSetVisOutput__Real_Integer,@_DoSetVisOutput_CallChanged_Real_Integer,@_DoGetVisOutput__Real_Integer);
  SetConverter(vInteger,vString,@_DoSetVisOutput__String_Integer,@_DoSetVisOutput_CallChanged_String_Integer,@_DoGetVisOutput__String_Integer);
  SetConverter(vInteger,vColor,@_DoSetVisOutput__Color_Integer,@_DoSetVisOutput_CallChanged_Color_Integer,@_DoGetVisOutput__Color_Integer);
  SetConverter(vInteger,vBoolean,@_DoSetVisOutput__Boolean_Integer,@_DoSetVisOutput_CallChanged_Boolean_Integer,@_DoGetVisOutput__Boolean_Integer);
  SetConverter(vInteger,vBuffer,@_DoSetVisOutput__RealBuffer_Integer,@_DoSetVisOutput_CallChanged_RealBuffer_Integer,@_DoGetVisOutput__RealBuffer_);
  //vReal
  SetConverter(vReal,vInteger,@_DoSetVisOutput__Integer_Real,@_DoSetVisOutput_CallChanged_Integer_Real,@_DoGetVisOutput__Integer_Real);
  SetConverter(vReal,vReal,@_DoSetVisOutput__Real,@_DoSetVisOutput_CallChanged_Real,@_DoGetVisOutput__Real);
  SetConverter(vReal,vString,@_DoSetVisOutput__String_Real,@_DoSetVisOutput_CallChanged_String_Real,@_DoGetVisOutput__String_Real);
  SetConverter(vReal,vColor,@_DoSetVisOutput__Color_Real,@_DoSetVisOutput_CallChanged_Color_Real,@_DoGetVisOutput__Color_Real);
  SetConverter(vReal,vBoolean,@_DoSetVisOutput__Boolean_Real,@_DoSetVisOutput_CallChanged_Boolean_Real,@_DoGetVisOutput__Boolean_Real);
  SetConverter(vReal,vBuffer,@_DoSetVisOutput__RealBuffer_Real,@_DoSetVisOutput_CallChanged_RealBuffer_Real,@_DoGetVisOutput__RealBuffer_);
  //vString
  SetConverter(vString,vInteger,@_DoSetVisOutput__Integer_String,@_DoSetVisOutput_CallChanged_Integer_String,@_DoGetVisOutput__Integer_String);
  SetConverter(vString,vReal,@_DoSetVisOutput__Real_String,@_DoSetVisOutput_CallChanged_Real_String,@_DoGetVisOutput__Real_String);
  SetConverter(vString,vString,@_DoSetVisOutput__String,@_DoSetVisOutput_CallChanged_String,@_DoGetVisOutput__String);
  SetConverter(vString,vColor,@_DoSetVisOutput__Color_String,@_DoSetVisOutput_CallChanged_Color_String,@_DoGetVisOutput__Color_String);
  SetConverter(vString,vBoolean,@_DoSetVisOutput__Boolean_String,@_DoSetVisOutput_CallChanged_Boolean_String,@_DoGetVisOutput__Boolean_String);
  SetConverter(vString,vBuffer,@_DoSetVisOutput__RealBuffer_String,@_DoSetVisOutput_CallChanged_RealBuffer_String,@_DoGetVisOutput__RealBuffer_);
  //vColor
  SetConverter(vColor,vInteger,@_DoSetVisOutput__Integer_Color,@_DoSetVisOutput_CallChanged_Integer_Color,@_DoGetVisOutput__Integer_Color);
  SetConverter(vColor,vReal,@_DoSetVisOutput__Real_Color,@_DoSetVisOutput_CallChanged_Real_Color,@_DoGetVisOutput__Real_Color);
  SetConverter(vColor,vString,@_DoSetVisOutput__String_Color,@_DoSetVisOutput_CallChanged_String_Color,@_DoGetVisOutput__String_Color);
  SetConverter(vColor,vColor,@_DoSetVisOutput__Color,@_DoSetVisOutput_CallChanged_Color,@_DoGetVisOutput__Color);
  SetConverter(vColor,vBoolean,@_DoSetVisOutput__Boolean_Color,@_DoSetVisOutput_CallChanged_Boolean_Color,@_DoGetVisOutput__Boolean_Color);
  SetConverter(vColor,vBuffer,@_DoSetVisOutput__RealBuffer_Color,@_DoSetVisOutput_CallChanged_RealBuffer_Color,@_DoGetVisOutput__RealBuffer_);
  //vBoolean
  SetConverter(vBoolean,vInteger,@_DoSetVisOutput__Integer_Boolean,@_DoSetVisOutput_CallChanged_Integer_Boolean,@_DoGetVisOutput__Integer_Boolean);
  SetConverter(vBoolean,vReal,@_DoSetVisOutput__Real_Boolean,@_DoSetVisOutput_CallChanged_Real_Boolean,@_DoGetVisOutput__Real_Boolean);
  SetConverter(vBoolean,vString,@_DoSetVisOutput__String_Boolean,@_DoSetVisOutput_CallChanged_String_Boolean,@_DoGetVisOutput__String_Boolean);
  SetConverter(vBoolean,vColor,@_DoSetVisOutput__Color_Boolean,@_DoSetVisOutput_CallChanged_Color_Boolean,@_DoGetVisOutput__Color_Boolean);
  SetConverter(vBoolean,vBoolean,@_DoSetVisOutput__Boolean,@_DoSetVisOutput_CallChanged_Boolean,@_DoGetVisOutput__Boolean);
  SetConverter(vBoolean,vBuffer,@_DoSetVisOutput__RealBuffer_Boolean,@_DoSetVisOutput_CallChanged_RealBuffer_Boolean,@_DoGetVisOutput__RealBuffer_);
  //vBuffer
  SetConverter(vBuffer,vInteger,@_DoSetVisOutput__Integer_RealBuffer,@_DoSetVisOutput_CallChanged_Integer_RealBuffer,@_DoGetVisOutput__Integer_RealBuffer);
  SetConverter(vBuffer,vReal,@_DoSetVisOutput__Real_RealBuffer,@_DoSetVisOutput_CallChanged_Real_RealBuffer,@_DoGetVisOutput__Real_RealBuffer);
  SetConverter(vBuffer,vString,@_DoSetVisOutput__String_RealBuffer,@_DoSetVisOutput_CallChanged_String_RealBuffer,@_DoGetVisOutput__String_RealBuffer);
  SetConverter(vBuffer,vColor,@_DoSetVisOutput__Color_RealBuffer,@_DoSetVisOutput_CallChanged_Color_RealBuffer,@_DoGetVisOutput__Color_RealBuffer);
  SetConverter(vBuffer,vBoolean,@_DoSetVisOutput__Boolean_RealBuffer,@_DoSetVisOutput_CallChanged_Boolean_RealBuffer,@_DoGetVisOutput__Boolean_RealBuffer);
  SetConverter(vBuffer,vBuffer,@_DoSetVisOutput__RealBuffer,@_DoSetVisOutput_CallChanged_RealBuffer,@_DoGetVisOutput__RealBuffer);
end;

destructor TVisParamTypes.Destroy;
begin
  DestroyTypeMem(FData);
  inherited Destroy;
end;

function TVisParamTypes.CreateTypeMem(ACount: Integer): TVisParamTypeMem; inline;
var
  AConverterOffset: Cardinal;
begin
  with Result do begin
    Count:=ACount;
    {$DEFINE _VisParamTypeCount := ACount}
    AConverterOffset:=VisParamTypesDataSize;
    Size:=AConverterOffset+VisOutputConverterDataSize;
    GetMem(Types,Size);
    Converters:=Pointer(Types)+AConverterOffset;
  end;
end;

{$MACRO OFF}

procedure TVisParamTypes.DestroyTypeMem(var AMem: TVisParamTypeMem); inline;
begin
  with AMem do FreeMem(Types,Size);
end;

procedure TVisParamTypes.CopyTypes(const Src: TVisParamTypeMem; var Dst: TVisParamTypeMem);
var
  I: Integer;
begin
  //Dst.Count muss > Src.Count sein...
  for I:=0 to Src.Count-1 do Dst.Types^[I]:=Src.Types^[I];
  for I:=Src.Count to Dst.Count-1 do Dst.Types^[I]:=EMPTYPARAM;
end;

procedure TVisParamTypes.CopyConverters(const Src: TVisParamTypeMem; var Dst: TVisParamTypeMem);
var
  I,J,SrcPos,DstPos: Integer;
begin
  SrcPos:=0;
  DstPos:=0;
  for I:=0 to Src.Count-1 do begin
    for J:=0 to Src.Count-1 do begin
      Dst.Converters^[DstPos]:=Src.Converters^[SrcPos];
      Inc(DstPos);
      Inc(SrcPos);
    end;
    for J:=Src.Count to Dst.Count-1 do begin
      with Dst.Converters^[DstPos] do begin
        _Set:=Dst.Types^[I].DefaultConverters._Set;
        _Get:=Dst.Types^[J].DefaultConverters._Get;
      end;
      Inc(DstPos);
    end;
  end;
  for I:=Src.Count to Dst.Count-1 do for J:=0 to Dst.Count-1 do begin
    with Dst.Converters^[DstPos] do begin
      _Set:=Dst.Types^[I].DefaultConverters._Set;
      _Get:=Dst.Types^[J].DefaultConverters._Get;
    end;
    Inc(DstPos);
  end;
end;

function TVisParamTypes.AddParamType(AType: TVisParamType; ASize: Cardinal; const AName: TParamName; AColor: CvColor; constref APicture: TParamPic; ADefaultConverters: TVisOutputConverters): Boolean;
var
  ANewType: TVisParamType;
  ANewData: TVisParamTypeMem;
  I       : Integer;
begin
  if AType>=FData.Count then begin
    ANewData:=CreateTypeMem(AType+1);
    CopyTypes(FData,ANewData);
    //neuen Typ initialisieren
    with ANewData.Types^[AType] do begin
      Size:=ASize;
      Name:=AName;
      Color:=AColor;
      Picture:=APicture;
      DefaultConverters:=ADefaultConverters;
    end;
    CopyConverters(FData,ANewData);
    DestroyTypeMem(FData);
    FData:=ANewData;
  end else begin
    //neuen Typ initialisieren
    with FData do with Types^[AType] do begin
      Size:=ASize;
      Name:=AName;
      Color:=AColor;
      Picture:=APicture;
      DefaultConverters:=ADefaultConverters;
      //Konverter des neuen Typs initialisieren
      for I:=0 to Count-1 do begin
        //Converters[AType,I]._Set:=DefaultConverters._Set;
        Converters^[AType*Count+I]._Set:=DefaultConverters._Set;
        //Converters[I,AType]._Get:=DefaultConverters._Get;
        Converters^[I*Count+AType]._Get:=DefaultConverters._Get;
      end;
    end;
  end;
end;

procedure TVisParamTypes.SetConverter(AParamType: TVisParamType; AOutputType: TVisOutputType; ASet,ASet_CallChanged: TVisOutputSet; AGet: TVisOutputGet);
begin
  Converters[AParamType,AOutputType]:=VisOutputConverters(ASet,ASet_CallChanged,AGet);
end;

function TVisParamTypes.SizeOfParam(AType: TVisParamType): Cardinal; inline;
begin
  Result:=FData.Types^[AType].Size;
end;

function TVisParamTypes.SizeOfOutput(AType: TVisOutputType): Cardinal; inline;
begin
  Result:=FData.Types^[AType].Size;
end;

function TVisParamTypes.ParamTypeExists(AType: TVisParamType): Boolean; inline;
begin
  Result:=(FData.Types^[AType].Size<>EMPTYPARAM.Size);
end;

function TVisParamTypes.GetConverters(Param: TVisParamType; Output: TVisOutputType): TVisOutputConverters; inline;
begin
  Result:=FData.Converters^[Param*FData.Count+Output];
end;

procedure TVisParamTypes.SetConverters(Param: TVisParamType; Output: TVisOutputType; Value: TVisOutputConverters); inline;
begin
  FData.Converters^[Param*FData.Count+Output]:=Value;
end;

function TVisParamTypes.GetParamType(AType: TVisParamType): TVisParamTypeDesc; inline;
begin
  Result:=FData.Types^[AType];
end;

function TVisParamTypes.GetParamTypePtr(AType: TVisParamType): PVisParamTypeDesc; inline;
begin
  Result:=@FData.Types^[AType];
end;

{%ENDREGION}
{%REGION TVisType}

function TVisType.GetParamDesc(Index: Integer): TSVisParamDesc;
begin
  case Index of
    -1: Result:=ToVisParamDesc(Name,vCall,VisProc,TVisInputPos(0));
    0 : Result:=ToVisParamDesc(C1Desc,vColor,nil,TVisInputPos(-1*VisParamTypes.SizeOfParam(vColor)));
    1 : Result:=ToVisParamDesc(C2Desc,vColor,nil,TVisInputPos(-2*VisParamTypes.SizeOfParam(vColor)));
    2 : Result:=ToVisParamDesc(C3Desc,vColor,nil,TVisInputPos(-3*VisParamTypes.SizeOfParam(vColor)));
    else Result:=VisParamDesc[Index-3];
  end;
end;

{%ENDREGION}
{%REGION TPresetVis}

class function TPresetVis.Create(const AIndex: TVisIndex; AInit: Boolean = true): TPresetVis;
begin
  Result.Produce(AIndex,AInit);
end;

class function TPresetVis.Create_ID(const AVisID; AInit: Boolean = true): TPresetVis;
begin
  Result.Produce_ID(AVisID,AInit);
end;

procedure TPresetVis.Produce(const AIndex: TVisIndex; AInit: Boolean = true);
var
  I: Integer;
begin
  VisID:=AIndex;
  with TVisualisation.Visualisations(AIndex) do begin
    GetMem(VisParams,VisParamSize);
    SetLength(VisOutputs,Length(OutputDesc));
  end;
  if not AInit then exit;
  C1:=$FFFF0000;
  C2:=$FF00FF00;
  C3:=$FF0000FF;
  with TVisualisation.Visualisations(AIndex) do begin
    Move(InitialValues^,VisParams^,VisParamSize);
  end;
  DesignPos:=Point(0,0);
  for I:=0 to Length(VisOutputs)-1 do VisOutputs[I].Param:=-2;
  DestCanvas:=poiDEFAULTCANVAS;
end;

procedure TPresetVis.Produce_ID(const AVisID; AInit: Boolean = true);
begin
  Produce(TVisualisation.FindVisID(AVisID),AInit);
end;

function TPresetVis.Copy: TPresetVis;
begin

end;

procedure TPresetVis.Destroy;
begin
  with TVisualisation.Visualisations(VisID) do begin
    FreeMem(VisParams,VisParamSize);
    SetLength(VisOutputs,0);
  end;
  VisParams:=nil;
end;

{%ENDREGION}
{%REGION TPreset}

procedure RemoveLayer_(var Layers: TPresetLayers; Index: Integer); forward;
procedure Insert_(var Layers: TPresetLayers; Index: Integer; const AVis: TPresetVis); forward;
procedure _CopyLayer(const Src: TPresetVis; out Dest: TPresetVis); forward;

function Delete_Do_ReconCheck_LayersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Delete_Do_ReconCheck_TriggersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Insert_Do_ReconCheck_LayersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Insert_Do_ReconCheck_TriggersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Swap_Do_ReconCheck(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
//SLDL: Src: Layer | Dst: Layer | Src<Dst
//STDT: Src: Trigger | Dst: Trigger | Src<Dst
function Move_Do_ReconCheck_SLDL_STDT(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Move_Do_ReconCheck_DLSL_DTST(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Move_Do_ReconCheck_DTSL(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;
function Move_Do_ReconCheck_STDL(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean; forward;

procedure AddPresetOP(var APresetOP: TNamedPresetOutputs; const AInfo: TPresetOutputInfo; const APrefix: string; const AName: string = ''); forward;
procedure DeletePresetOP(var APresetOP: TNamedPresetOutputs; AIndex: Integer; const APrefix: string); forward;
procedure InsertPresetOP(var APresetOP: TNamedPresetOutputs; AIndex: Integer; const AInfo: TPresetOutputInfo; const APrefix: string; const AName: string = ''); forward;
function IsDefaultOPName(const AName,APrefix: string): Boolean; forward;

const
  ParamNamePrefix  = 'Parameter ';
  OutputNamePrefix = 'Output ';

constructor TPreset.Create;
begin
  inherited Create;
end;

destructor TPreset.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPreset.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(Layers)-1 do Layers[I].Destroy;
  for I:=0 to Length(Triggers)-1 do Triggers[I].Destroy;
  SetLength(Layers,0);
  SetLength(Triggers,0);
  WSize:=0;
  SetLength(Params,0);
  SetLength(Outputs,0);
end;

procedure TPreset.Reconnect(AReconFunc: TPresetReconCheck; AIndex1: TPresetIndex = 0; AIndex2: TPresetIndex = 0);

  procedure Do_Reconnect(var ALayers: TPresetLayers); inline;
  var
    I,J: TPresetIndex;
  begin
    for I:=0 to Length(ALayers)-1 do with ALayers[I] do begin
      for J:=0 to Length(VisOutputs)-1
        do with VisOutputs[J]
          do if not AReconFunc(Layer,AIndex1,AIndex2)
            then {Param:=-2}VisOutputs[J]:=poiUNDEFINED;
      with DestCanvas
        do if not AReconFunc(Layer,AIndex1,AIndex2)
          then DestCanvas:=poiDEFAULTCANVAS;
    end;
  end;

  procedure Do_Reconnect_Outputs(var AOutputs: TNamedPresetOutputs); inline;
  var
    I,J: TPresetIndex;
  begin
    //J zählt die "gültigen Einträge", während I alle Einträge zählt...
    J:=0;
    for I:=0 to Length(AOutputs)-1 do begin
      if I>J then AOutputs[J]:=AOutputs[I];
      if AReconFunc(AOutputs[J].OutputInfo.Layer,AIndex1,AIndex2) then Inc(J);
    end;
    if J<Length(AOutputs) then SetLength(AOutputs,J);
  end;

begin
  Do_Reconnect(Layers);
  Do_Reconnect(Triggers);
  Do_Reconnect_Outputs(Params);
  Do_Reconnect_Outputs(Outputs);
end;

procedure TPreset.DeleteNR(AIndex: TPresetIndex);
begin
  if AIndex>=0
    then RemoveLayer_(Layers,AIndex)
    else RemoveLayer_(Triggers,not AIndex);
end;

procedure TPreset.Delete(AIndex: TPresetIndex);
begin
  if AIndex>=0 then begin
    RemoveLayer_(Layers,AIndex);
    Reconnect(@Delete_Do_ReconCheck_LayersChanged,AIndex);
  end else begin
    RemoveLayer_(Triggers,not AIndex);
    Reconnect(@Delete_Do_ReconCheck_TriggersChanged,AIndex);
  end;
end;

procedure TPreset.Swap(AIndex1,AIndex2: TPresetIndex);
var
  TempLayer      : TPresetVis;
  ALayer1,ALayer2: PPresetVis;
begin
  if AIndex1>=0
    then ALayer1:=@Layers[AIndex1]
    else ALayer1:=@Triggers[not AIndex1];
  if AIndex2>=0
    then ALayer2:=@Layers[AIndex2]
    else ALayer2:=@Triggers[not AIndex2];

  TempLayer:=ALayer1^;
  ALayer1^:=ALayer2^;
  ALayer2^:=TempLayer;

  Reconnect(@Swap_Do_ReconCheck,AIndex1,AIndex2);
end;

procedure TPreset.InsertNR(AIndex: TPresetIndex; const AVis: TPresetVis); inline;
begin
  if AIndex>=0
    then Insert_(Layers,AIndex,AVis)
    else Insert_(Triggers,not AIndex,AVis);
end;

procedure TPreset.Insert(AIndex: TPresetIndex; const AVis: TPresetVis);
begin
  if AIndex>=0 then begin
    Insert_(Layers,AIndex,AVis);
    Reconnect(@Insert_Do_ReconCheck_LayersChanged,AIndex);
  end else begin
    Insert_(Triggers,not AIndex,AVis);
    Reconnect(@Insert_Do_ReconCheck_TriggersChanged,AIndex);
  end;
end;

procedure TPreset.Add(const AVis: TPresetVis; AAutoDraw: Boolean = true);
var
  L: Integer;
begin
  if AAutoDraw then begin
    L:=Length(Layers);
    SetLength(Layers,L+1);
    Layers[L]:=AVis;
  end else begin
    L:=Length(Triggers);
    SetLength(Triggers,L+1);
    Triggers[L]:=AVis;
  end;
end;

procedure TPreset.Move(ASource,ADest: TPresetIndex);
var
  ATempLayer       : TPresetVis;
  I,ASource2,ADest2: TPresetIndex;
  ALayers          : PPresetLayers;
const
  //[false]: Src<Dst; [true]: Src>Dst
  AReconProcsLLTT: array [Boolean] of TPresetReconCheck = (@Move_do_ReconCheck_SLDL_STDT,@Move_do_ReconCheck_DLSL_DTST);
  AReconProcsTLLT: array [Boolean] of TPresetReconCheck = (@Move_do_ReconCheck_STDL,@Move_do_ReconCheck_DTSL);
begin
  if ASource=ADest then exit;
  //beide Layer (>=0) oder beide Trigger (<0)
  if (ASource>=0) xor (ADest<0) then begin
    if ASource>=0 then begin
      ALayers:=@Layers;
      ASource2:=ASource;
      ADest2:=ADest;
    end else begin
      ALayers:=@Triggers;
      ASource2:=not ASource;
      ADest2:=not ADest;
    end;
    ATempLayer:=ALayers^[ASource2];
    if ASource2>ADest2
      then for I:=ASource2 downto ADest2+1 do ALayers^[I]:=ALayers^[I-1]
      else for I:=ASource2 to ADest2-1 do ALayers^[I]:=ALayers^[I+1];
    ALayers^[ADest2]:=ATempLayer;
    Reconnect(AReconProcsLLTT[ASource>ADest],ASource,ADest);
  end else begin
    ATempLayer:=Items[ASource]^;
    DeleteNR(ASource);
    InsertNR(ADest,ATempLayer);
    Reconnect(AReconProcsTLLT[ASource>ADest],ASource,ADest);
  end;
end;

procedure TPreset.CopyFrom(ASource: TPreset);
begin
  ASource.CopyTo(Self);
end;

procedure TPreset.CopyTo(Dest: TPreset);
var
  I,L: Integer;
begin
  L:=Length(Layers);
  SetLength(Dest.Layers,L);
  for I:=0 to L-1 do _CopyLayer(Layers[I],Dest.Layers[I]);
  L:=Length(Triggers);
  SetLength(Dest.Triggers,L);
  for I:=0 to L-1 do _CopyLayer(Triggers[I],Dest.Triggers[I]);
  Dest.WSize:=WSize;
  L:=Length(Params);
  SetLength(Dest.Params,L);
  for I:=0 to L-1 do Dest.Params[I]:=Params[I];
  L:=Length(Outputs);
  SetLength(Dest.Outputs,L);
  for I:=0 to L-1 do Dest.Outputs[I]:=Outputs[I];
end;

function TPreset.Copy: TPreset;
begin
  Result:=TPreset.Create;
  CopyTo(Result);
end;

function TPreset.GetItem(Index: TPresetIndex): PPresetVis;
begin
  if Index>=0
    then Result:=@Layers[Index]
    else Result:=@Triggers[not Index];
end;

procedure TPreset.AddParam(const AInfo: TPresetOutputInfo; const AName: string = '');
begin
  AddPresetOP(Params,AInfo,ParamNamePrefix,AName);
end;

procedure TPreset.DeleteParam(AIndex: Integer);
begin
  DeletePresetOP(Params,AIndex,ParamNamePrefix);
end;

procedure TPreset.InsertParam(AIndex: Integer; const AInfo: TPresetOutputInfo; const AName: string = '');
begin
  InsertPresetOP(Params,AIndex,AInfo,ParamNamePrefix,AName)
end;

function TPreset.IsDefaultParamName(const AName: string): Boolean;
begin
  Result:=IsDefaultOPName(AName,ParamNamePrefix);
end;

procedure TPreset.AddOutput(const AInfo: TPresetOutputInfo; const AName: string = '');
begin
  AddPresetOP(Outputs,AInfo,OutputNamePrefix,AName);
end;

procedure TPreset.DeleteOutput(AIndex: Integer);
begin
  DeletePresetOP(Outputs,AIndex,OutputNamePrefix);
end;

procedure TPreset.InsertOutput(AIndex: Integer; const AInfo: TPresetOutputInfo; const AName: string = '');
begin
  InsertPresetOP(Outputs,AIndex,AInfo,OutputNamePrefix,AName)
end;

function TPreset.IsDefaultOutputName(const AName: string): Boolean;
begin
  Result:=IsDefaultOPName(AName,OutputNamePrefix);
end;

{%REGION TPreset - Hilfsmethoden}

procedure RemoveLayer_(var Layers: TPresetLayers; Index: Integer);
var
  I     : TPresetIndex;
  ALayer: PPresetVis;
begin
  ALayer:=@Layers[Index];
  ALayer^.Destroy;
  for I:=Index to Length(Layers)-2 do Layers[I]:=Layers[I+1];
  SetLength(Layers,Length(Layers)-1);
end;

procedure Insert_(var Layers: TPresetLayers; Index: Integer; const AVis: TPresetVis);
var
  I,L: TPresetIndex;
begin
  L:=Length(Layers);
  SetLength(Layers,L+1);
  for I:=L downto Index do Layers[I]:=Layers[I-1];
  Layers[Index]:=AVis;
end;

procedure _CopyLayer(const Src: TPresetVis; out Dest: TPresetVis);
var
  I,L: Integer;
begin
  Dest.Produce(Src.VisID,false);
  Dest.C1:=Src.C1;
  Dest.C2:=Src.C2;
  Dest.C3:=Src.C3;
  Dest.DesignPos:=Src.DesignPos;
  L:=Length(Src.VisOutputs);
  SetLength(Dest.VisOutputs,L);
  for I:=0 to L-1 do begin
    Dest.VisOutputs[I]:=Src.VisOutputs[I];
  end;
  Move(Src.VisParams^,Dest.VisParams^,TVisualisation.Visualisations(Src.VisID).VisParamSize);
  Dest.DestCanvas:=Src.DestCanvas;
end;

procedure AddPresetOP(var APresetOP: TNamedPresetOutputs; const AInfo: TPresetOutputInfo; const APrefix: string; const AName: string = '');
var
  L     : Integer;
begin
  L:=Length(APresetOP);
  SetLength(APresetOP,L+1);
  with APresetOP[L] do begin
    OutputInfo:=AInfo;
    if AName<>''
      then Name:=AName
      else Name:=APrefix+IntToStr(L+1);
  end;
end;

procedure DeletePresetOP(var APresetOP: TNamedPresetOutputs; AIndex: Integer; const APrefix: string);
var
  I,L: Integer;
begin
  L:=Length(APresetOP);
  for I:=AIndex to L-2 do begin
    APresetOP[I]:=APresetOP[I+1];
    if IsDefaultOPName(APresetOP[I].Name,APrefix)
      then APresetOP[I].Name:=APrefix+IntToStr(I+1);
  end;
  SetLength(APresetOP,L-1);
end;

procedure InsertPresetOP(var APresetOP: TNamedPresetOutputs; AIndex: Integer; const AInfo: TPresetOutputInfo; const APrefix: string; const AName: string = '');
var
  I,L: Integer;
begin
  L:=Length(APresetOP);
  SetLength(APresetOP,L+1);
  for I:=L downto AIndex+1 do begin
    APresetOP[I]:=APresetOP[I-1];
    if IsDefaultOPName(APresetOP[I].Name,APrefix)
      then APresetOP[I].Name:=APrefix+IntToStr(I+1);
  end;
  with APresetOP[AIndex] do begin
    OutputInfo:=AInfo;
    if AName<>''
      then Name:=AName
      else Name:=APrefix+IntToStr(AIndex+1);
  end;
end;

function IsDefaultOPName(const AName,APrefix: string): Boolean;
var
  AName2: string;
  AOut  : Integer;
begin
  Result:=(Pos(APrefix,AName)=1);
  if Result then begin
    AName2:=AName;
    Delete(AName2,1,Length(APrefix));
    Result:=TryStrToInt(AName2,AOut);
  end;
end;

{%ENDREGION}
{%REGION TPreset - Reconnection methods}

function Delete_Do_ReconCheck_LayersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if ACheckLayer>AIndex1 then begin
    Dec(ACheckLayer);
    Result:=true;
  end else Result:=(not (ACheckLayer=AIndex1));
end;

function Delete_Do_ReconCheck_TriggersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if ACheckLayer<AIndex1 then begin
    Inc(ACheckLayer);
    Result:=true;
  end else Result:=(not (ACheckLayer=AIndex1));
end;

function Insert_Do_ReconCheck_LayersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if ACheckLayer>=AIndex1 then Inc(ACheckLayer);
  Result:=true;
end;

function Insert_Do_ReconCheck_TriggersChanged(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if ACheckLayer<=AIndex1 then Dec(ACheckLayer);
  Result:=true;
end;

function Swap_Do_ReconCheck(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if ACheckLayer=AIndex1
    then ACheckLayer:=AIndex2
    else if ACheckLayer=AIndex2
      then ACheckLayer:=AIndex1;
  Result:=true;
end;

function Move_Do_ReconCheck_SLDL_STDT(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if (ACheckLayer>AIndex1) and (ACheckLayer<=AIndex2)
    then Dec(ACheckLayer)
    else if ACheckLayer=AIndex1
      then ACheckLayer:=AIndex2;
  Result:=true;
end;

function Move_Do_ReconCheck_DLSL_DTST(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if (ACheckLayer<AIndex1) and (ACheckLayer>=AIndex2)
    then Inc(ACheckLayer)
    else if ACheckLayer=AIndex1
      then ACheckLayer:=AIndex2;
  Result:=true;
end;

function Move_Do_ReconCheck_DTSL(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if (ACheckLayer>AIndex1) or (ACheckLayer<=AIndex2)
    then Dec(ACheckLayer)
    else if ACheckLayer=AIndex1
      then ACheckLayer:=AIndex2;
  Result:=true;
end;

function Move_Do_ReconCheck_STDL(var ACheckLayer: TPresetIndex; AIndex1,AIndex2: TPresetIndex): Boolean;
begin
  if (ACheckLayer<AIndex1) or (ACheckLayer>=AIndex2)
    then Inc(ACheckLayer)
    else if ACheckLayer=AIndex1
      then ACheckLayer:=AIndex2;
  Result:=true;
end;

{%ENDREGION}
{%ENDREGION}
{%REGION TPresets}

constructor TPresets.Create;
begin
  {SetLength(FPresets,1);
  FPresets[0]:=TPreset.Create;}
  inherited Create;
end;

destructor TPresets.Destroy;
{var
  I: Integer;}
begin
  {for I:=0 to Length(FPresets)-1
    do FPresets[I].Destroy;
  SetLength(FPresets,0);}
  Clear;
  inherited Destroy;
end;

{$I TPresets_Load_Save.inc}

function TPresets.Add: TPreset;
var
  L: Integer;
begin
  L:=Length(FPresets);
  SetLength(FPresets,L+1);
  Result:=TPreset.Create;
  FPresets[L]:=Result;
end;

procedure TPresets.Add(APreset: TPreset);
var
  L: Integer;
begin
  L:=Length(FPresets);
  SetLength(FPresets,L+1);
  FPresets[L]:=APreset;
end;

procedure TPresets.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(FPresets)-1
    do FPresets[I].Destroy;
  SetLength(FPresets,0);
end;

procedure TPresets.Remove(Index: Integer);
var
 I,L: Integer;
begin
  FPresets[Index].Destroy;
  L:=Length(FPresets)-1;
  for I:=Index to L-1
    do FPresets[I]:=FPresets[I+1];
  SetLength(FPresets,L);
end;

function TPresets.GetCount: Integer; inline;
begin
  Result:=Length(FPresets);
end;

function TPresets.GetPreset(AIndex: Integer): TPreset; inline;
begin
  Result:=FPresets[AIndex];
end;

{%ENDREGION}
{%REGION TVisKey2}

{constructor TVisKey2.Create;
begin
  inherited Create;
end;

destructor TVisKey2.Destroy;
begin
  inherited Destroy;

end;

procedure TVisKey2.SaveKeyboardsToStream(const Keyboards: TVisKeyboards; Stream: TStream);
function TVisKey2.LoadKeyboardsFromStream(var Keyboards: TVisKeyboards; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TLoadFileResult;
procedure TVisKey2.SaveKeyboardsToFile(const Keyboards: TVisKeyboards; const FileName: string);
function TVisKey2.LoadKeyboardsFromFile(var Keyboards: TVisKeyboards; const FileName: string; const Append: Boolean = false): TLoadFileResult;
}
{%ENDREGION}
{%REGION TBasicVisualisation}

constructor TBasicVisualisation.Create(const ASource: TPresetVis; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; APreQueue: TLQueue; ABGThread: TBGThreadPush);
begin
  inherited Create;
  FCanvas:=ACanvas;
  FSpectrumData:=ASpectrumData;
  FBGThread:=ABGThread;

  FVisID:=ASource.VisID;
  FCColors[0]:=ASource.C1;
  FCColors[1]:=ASource.C2;
  FCColors[2]:=ASource.C3;
end;

destructor TBasicVisualisation.Destroy;
begin
  //OpenCanvas Interfaces freigeben
  OpenCanvasCount:=0;
  inherited Destroy;
end;

const
  Local_Basic_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TBasicVisualisation.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Basic_Version;
end;

function TBasicVisualisation.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Basic_Version
    then Result:=IMInterface(IVisualisation(Self))
    else Result:=inherited Future(Version);
end;

function TBasicVisualisation.GetC1: vpColor; stdcall;
begin
  Result:=FCColors[0];
end;

function TBasicVisualisation.GetC2: vpColor; stdcall;
begin
  Result:=FCColors[1];
end;

function TBasicVisualisation.GetC3: vpColor; stdcall;
begin
  Result:=FCColors[2];
end;

procedure TBasicVisualisation.SetC1(const AValue: vpColor);
begin
  FCColors[0]:=AValue;
end;

procedure TBasicVisualisation.SetC2(const AValue: vpColor);
begin
  FCColors[1]:=AValue;
end;

procedure TBasicVisualisation.SetC3(const AValue: vpColor);
begin
  FCColors[2]:=AValue;
end;

function TBasicVisualisation.GetCanvas(Index: MVIndex): IMCanvas;
begin
  Result:=FOpenCanvas[Index];
end;

procedure TBasicVisualisation.SetCanvas(Index: MVIndex; ACanvas: IMCanvas); stdcall;
begin
  FOpenCanvas[Index]:=ACanvas;
end;

function TBasicVisualisation.GetOpenCanvasCount: Integer;
begin
  Result:=Length(FOpenCanvas);
end;

procedure TBasicVisualisation.SetOpenCanvasCount(Value: Integer);
var
  I: Integer;
begin
  //alte Interfaces freigeben
  for I:=Value to Length(FOpenCanvas)-1 do FOpenCanvas[I]:=nil;
  SetLength(FOpenCanvas,Value);
end;

{%ENDREGION}
{%REGION TVisualisation}

type
  TVisTypeList    = array of TVisType;
var
  FRegisteredVis: TVisTypeList;

function RegisterVisTypeListElement(const AVisType: TVisType; var AList: TVisTypeList): Integer; forward;
procedure DestroyVisTypeListElement(const Index: Integer; var AList: TVisTypeList); forward;
procedure UnRegisterVisTypeListElement(const ID: Cardinal; var AList: TVisTypeList); forward;
procedure ClearVisTypeList(var AList: TVisTypeList); forward;

constructor TVisualisation.Create(const ASource: TPresetVis; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; APreQueue: TLQueue; ABGThread: TBGThreadPush);
var
  I   : Integer;
  AOut: PVisOutput;
begin
  inherited Create(ASource,ACanvas,ASpectrumData,APreQueue,ABGThread);

  with FRegisteredVis[FVisID] do begin
    GetMem(FVisParams,VisParamSize);
    Move(ASource.VisParams^,FVisParams^,VisParamSize);
    SetLength(FVisOutputs,Length(ASource.VisOutputs));
    GetMem(FWorkspace,WorkspaceSize);
    OpenCanvasCount:=Length(CanvasDesc);
  end;

  //Outputs vorinitialisieren

  for I:=0 to Length(FVisOutputs)-1 do begin
    AOut:=@FVisOutputs[I];
    with AOut^ do begin
      DoSet:=@_DoSetVisOutput__BeforeInit;
      DoGet:=@_DoGetVisOutput__;
      AType:=FRegisteredVis[FVisID].OutputDesc[I].AType;
      with Data do begin
        D:=AOut;
        P:=APreQueue;
        V:=Pointer(VisParamTypes.SizeOfOutput(AType));
      end;
    end;
  end;

  //Visualisierung Initilisieren

  with FRegisteredVis[FVisID]
    do InitWorkspace(ACanvas,ASpectrumData,Self,FVisParams^,FWorkspace^);
end;

destructor TVisualisation.Destroy;
begin
  with FRegisteredVis[FVisID] do begin
    FreeWorkspace(FCanvas,FSpectrumData,Self,FVisParams^,FWorkspace^);
    FreeMem(FVisParams,VisParamSize);
    SetLength(FVisOutputs,0);
    FreeMem(FWorkspace,WorkspaceSize);
  end;
  inherited Destroy;
end;

const
  Local_Version2: MVVersion = (Version:0;MainVersion:2;SubVersion:0);
  Local_Version3: MVVersion = (Version:0;MainVersion:3;SubVersion:0);

function TVisualisation.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version3;
end;

function TVisualisation.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version3
    then  Result:=IMInterface(IVisualisation3(Self))
    else if Version=Local_Version2
      then Result:=IMInterface(IVisualisation2(Self))
      else Result:=inherited Future(Version);
end;

procedure TVisualisation.ThreadPush(AProc: TVisProc; AFinished: TVisProc = nil); stdcall;
begin
  BGThread(Self,AProc,AFinished);
  //BGThread.Push(Self,AProc,AFinished);
end;

function TVisualisation.GetTypeOf: TVisType;
begin
  Result:=FRegisteredVis[FVisID];
end;

procedure TVisualisation.SetOutput(const Index: LongWord; const Value); stdcall;
begin
  with FVisOutputs[Index] do DoSet(Data,Value);
end;

procedure TVisualisation.Visualize;
begin
  FRegisteredVis[FVisID].VisProc(FCanvas,FSpectrumData,Self,FVisParams^,FWorkspace^);
end;

procedure TVisualisation.CallVisProc(const AVisProc: TVisProc);
begin
  AVisProc(FCanvas,FSpectrumData,Self,FVisParams^,FWorkspace^);
end;

function TVisualisation.OutputPtr(AIndex: Cardinal): PVisOutput;
begin
  Result:=@FVisOutputs[AIndex];
end;

procedure TVisualisation.OutputOnParam(var AOutput: TVisOutput; AParam: Integer);
begin
  if AParam>=3 then begin
    with FRegisteredVis[FVisID].VisParamDesc[AParam-3] do begin
      AOutput.Data.D:=FVisParams+Cardinal(Offset);
      AOutput.Data.P:=Change;
      with VisParamTypes.Converters[AType,AOutput.AType] do begin;
        AOutput.DoSet:=_Set[AOutput.Data.P<>nil];
        AOutput.DoGet:=_Get;
      end;
      {AOutput.DoSet:=_DoSetVisOutput_Converters_[AOutput.Data.P<>nil][AType][AOutput.AType];
      AOutput.DoGet:=_DoGetVisOutput_Converters_[AType][AOutput.AType];}
    end;
  end else if AParam>=0
    then begin
      AOutput.Data.D:=@FCColors[AParam];
      with VisParamTypes.Converters[vColor,AOutput.AType] do begin
        AOutput.DoSet:=_Set[false];
        AOutput.DoGet:=_Get;
      end;
      {AOutput.DoSet:=_DoSetVisOutput_Converters_[false][vColor][AOutput.AType];
      AOutput.DoGet:=_DoGetVisOutput_Converters_[vColor][AOutput.AType];}
    end else begin
      AOutput.Data.P:=FRegisteredVis[FVisID].VisProc;
      AOutput.DoSet:=@_DoSetVisOutput_CallChanged_Call;
      AOutput.DoGet:=VisParamTypes.Converters[vCall,AOutput.AType]._Get;
      //:=_DoGetVisOutput_Converters_[vCall][AOutput.AType];
    end;
  AOutput.Data.V:=Self;
end;

procedure TVisualisation.OutputOnParamDirect(var AOutput: TVisOutput; AParam: Integer);
begin
  if AParam>=3 then begin
    with FRegisteredVis[FVisID].VisParamDesc[AParam-3] do begin
      AOutput.AType:=AType;
      AOutput.Data.D:=FVisParams+Cardinal(Offset);
      AOutput.Data.P:=Change;
      with VisParamTypes.Converters[AType,AType] do begin
        AOutput.DoSet:=_Set[AOutput.Data.P<>nil];
        AOutput.DoGet:=_Get;
      end;
      {AOutput.DoSet:=_DoSetVisOutput_Converters_[AOutput.Data.P<>nil][AType][AType];
      AOutput.DoGet:=_DoGetVisOutput_Converters_[AType][AType];}
    end;
  end else if AParam>=0
    then begin
      AOutput.AType:=oColor;
      AOutput.Data.D:=@FCColors[AParam];
      with VisParamTypes.Converters[vColor,oColor] do begin
        AOutput.DoSet:=_Set[false];
        AOutput.DoGet:=_Get;
      end;
      {AOutput.DoSet:=_DoSetVisOutput_Converters_[false][vColor][oColor];
      AOutput.DoGet:=_DoGetVisOutput_Converters_[vColor][oColor];}
    end else begin
      AOutput.AType:=oCall;
      AOutput.Data.P:=FRegisteredVis[FVisID].VisProc;
      AOutput.DoSet:=@_DoSetVisOutput_CallChanged_Call;
      AOutput.DoGet:=@_DoGetVisOutput__;
    end;
  AOutput.Data.V:=Self;
end;

{Allgemein - TVisType - Registrieren - Visualisierungen}

class function TVisualisation.RegisterVis(const AVisType: TVisType): Integer;
begin
  Result:=RegisterVisTypeListElement(AVisType,FRegisteredVis);
end;

class procedure TVisualisation.CreateAndRegisterVis(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const ACanvasNames: array of string; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
begin
  RegisterVis(VisType(AName,ID,AVisProc,AParamNames,AParamTypes,AChangeParams,AInitialValues,AOutputNames,AOutputTypes,AWorkspaceSize,AInitWorkspace,AFreeWorkspace,nil,ACanvasNames));
end;

class procedure TVisualisation.CreateAndRegisterVis2(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil); stdcall;
begin
  RegisterVis(VisType(AName,ID,AVisProc,AParamNames,AParamTypes,AChangeParams,AInitialValues,AOutputNames,AOutputTypes,AWorkspaceSize,AInitWorkspace,AFreeWorkspace,AFirstAction,[]));
end;

class procedure TVisualisation.UnRegisterVis(const ID: Cardinal);
begin
  UnRegisterVisTypeListElement(ID,FRegisteredVis);
end;

class procedure TVisualisation.DestroyVisType(const Index: Integer);
begin
  DestroyVisTypeListElement(Index,FRegisteredVis);
end;

class procedure TVisualisation.ClearVis;
begin
  ClearVisTypeList(FRegisteredVis);
end;

class function TVisualisation.VisCount: Integer;
begin
  Result:=Length(FRegisteredVis);
end;

class function TVisualisation.Visualisations(const Index: TVisIndex): TVisType;
begin
  Result:=FRegisteredVis[Index];
end;

class function TVisualisation.VisPtr(const Index: TVisIndex): PVisType;
begin
  Result:=@FRegisteredVis[Index];
end;

{$I advfunc.inc}

type
  TVisTypeFunc = specialize TOperatorFunc<TVisTypeList,TVisType>;

class function TVisualisation.FindVisID(const VisID): TVisIndex;
var
  AVisID  : TVisID absolute VisID;
  AVisType: TVisType;
  {AVisID    : TVisID;
  First,Last: TVisIndex;}
begin
  AVisType.VisID:=AVisID;
  Result:=TVisTypeFunc.BinarySearch(AVisType,FRegisteredVis,Length(FRegisteredVis)-1,0,0);
  //binäre Suche
  {First:=0;
  Last:=Length(FRegisteredVis)-1;
  Result:=Last div 2;
  while Last-First>1 do begin
    AVisID:=FRegisteredVis[Result].VisID;
    if AVisID>VisID2
      then Last:=Result
      else if AVisID<VisID2
        then First:=Result
        else exit;
    Result:=(First+Last) div 2;
  end;
  if FRegisteredVis[First].VisID=VisID2
    then Result:=First
    else if FRegisteredVis[Last].VisID=VisID2
      then Result:=Last
      else Result:=0;}
  //lineare Version
  {for First:=0 to Length(FRegisteredVis)-1 do begin
    if FRegisteredVis[First].VisID=VisID2 then begin
      Result:=First;
      exit;
    end;
  end;
  Result:=0}
end;

class function TVisualisation.ParamType(const AVisIndex: TVisIndex; AParam: Integer): TVisParamType;
begin
  if AParam>=0 then begin
    if AParam>=3
      then Result:=Visualisations(AVisIndex).VisParamDesc[AParam-3].AType
      else Result:=vColor;
  end else Result:=vCall;
end;

{%ENDREGION}
{%REGION TBasicVisComposition}

constructor TBasicVisComposition.Create(const ASource: TPreset; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; AQueue: TLQueue; ABGThread: TBGThreadPush);
var
  I,LT,LL,CI: Integer;

  procedure PushCreation(Layer: Integer);
  begin
    FCreateOrder[CI]:=Layer;
    Inc(CI);
  end;

  //TODO: rekursive Methode in iterative Form umschreiben
  function CreateGetCanvasAt(ACanvasSource: TPresetOutputInfo): IMCanvas;
  var
    ACanvasLayer: ^TBasicVisualisation;
  begin
    if ACanvasSource.Param<0
      then Result:=FCanvas
      else if ACanvasSource.Layer>=0
        then begin
          ACanvasLayer:=@FLayers[ACanvasSource.Layer];
          if ACanvasLayer^=nil then begin
            ACanvasLayer^:=TVisualisation.Create(ASource.Layers[ACanvasSource.Layer],CreateGetCanvasAt(ASource.Layers[ACanvasSource.Layer].DestCanvas),ASpectrumData,AQueue,ABGThread);
            PushCreation(ACanvasSource.Layer);
          end;
          Result:=ACanvasLayer^.OpenCanvas[ACanvasSource.Param]
        end else begin
          ACanvasLayer:=@FTriggers[not ACanvasSource.Layer];
          if ACanvasLayer^=nil then begin
            ACanvasLayer^:=TVisualisation.Create(ASource.Triggers[not ACanvasSource.Layer],CreateGetCanvasAt(ASource.Triggers[not ACanvasSource.Layer].DestCanvas),ASpectrumData,AQueue,ABGThread);
            PushCreation(ACanvasSource.Layer);
          end;
          Result:=ACanvasLayer^.OpenCanvas[ACanvasSource.Param];
        end;
  end;

begin
  inherited Create;
  FCanvas:=ACanvas;
  FSpectrumData:=ASpectrumData;
  F_Vising:=false;

  LL:=Length(ASource.Layers);
  SetLength(FLayers,LL);
  LT:=Length(ASource.Triggers);
  SetLength(FTriggers,LT);
  SetLength(FCreateOrder,LT+LL);
  CI:=0;
  //downtos nicht mehr nötig!
  for I:=LL-1 downto 0 do if FLayers[I]=nil then begin
    FLayers[I]:=TVisualisation.Create(ASource.Layers[I],CreateGetCanvasAt(ASource.Layers[I].DestCanvas),ASpectrumData,AQueue,ABGThread);
    PushCreation(I);
  end;
  for I:=LT-1 downto 0 do if FTriggers[I]=nil then begin
    FTriggers[I]:=TVisualisation.Create(ASource.Triggers[I],CreateGetCanvasAt(ASource.Triggers[I].DestCanvas),ASpectrumData,AQueue,ABGThread);
    PushCreation(not I);
  end;

  MatchOutputs(ASource);

  SetPreOuts(AQueue,ACanvas,ASpectrumData);
end;

destructor TBasicVisComposition.Destroy;
var
  I,ALayer: Integer;
begin
  //for I:=0 to Length(FLayers)-1 do FLayers[I].Destroy;
  //for I:=0 to Length(FTriggers)-1 do FTriggers[I].Destroy;
  //rückwärts wieder zerstören
  for I:=Length(FCreateOrder)-1 downto 0 do begin
    ALayer:=FCreateOrder[I];
    if ALayer>=0
      then FLayers[ALayer].Destroy
      else FTriggers[not ALayer].Destroy;
  end;
  SetLength(FCreateOrder,0);
  SetLength(FLayers,0);
  SetLength(FTriggers,0);
  inherited Destroy;
end;

procedure TBasicVisComposition.Visualize;
var
  I     : Integer;
begin
  while F_Vising do ;
  F_Vising:=true;
  for I:=0 to Length(FLayers)-1 do begin
    FLayers[I].Visualize;
  end;
  F_Vising:=false;
end;

procedure TBasicVisComposition.SetPreOuts(AQueue: TLQueue; var ACanvas: IMCanvas; const ASource: ISpectrumData);
var
  APreData: TPreOutData;
begin
  while not AQueue.Empty do begin
    AQueue.Pop(APreData);
    with APreData do begin
      with Dest^ do DoSet(Data,AValue^);
      FreeMem(AValue,Size);
    end;
  end;
end;

function TBasicVisComposition.GetCanvasAt(ACanvasSource: TPresetOutputInfo): IMCanvas;
begin
  if ACanvasSource.Param<0
    then Result:=FCanvas
    else if ACanvasSource.Layer>=0
      then Result:=FLayers[ACanvasSource.Layer].OpenCanvas[ACanvasSource.Param]
      else Result:=FTriggers[not ACanvasSource.Layer].OpenCanvas[ACanvasSource.Param];
end;

procedure TBasicVisComposition.ToVisOutput(const ASource: TPresetOutputInfo; var ADest: TVisOutput);
var
  AInputLayer: TBasicVisualisation;
begin
  with ASource do begin
    if Param<-1 then begin
      ADest.DoSet:=@_DoSetVisOutput__;
      ADest.DoGet:=VisParamTypes.Converters[vCall,ADest.AType]._Get;
      //:=_DoGetVisOutput_Converters_[vCall][ADest.AType];
      exit;
    end;
    if Layer>=0
      then AInputLayer:=FLayers[Layer]
      else AInputLayer:=FTriggers[not Layer];
    AInputLayer.OutputOnParam(ADest,Param);
  end;
end;

procedure TBasicVisComposition.ToVisOutputDirect(const ASource: TPresetOutputInfo; var ADest: TVisOutput);
var
  AInputLayer: TBasicVisualisation;
begin
  with ASource do begin
    if Param<-1 then begin
      ADest.DoSet:=@_DoSetVisOutput__;
      ADest.DoGet:=@_DoGetVisOutput__;
      ADest.AType:=oCall;
      exit;
    end;
    if Layer>=0
      then AInputLayer:=FLayers[Layer]
      else AInputLayer:=FTriggers[not Layer];
    AInputLayer.OutputOnParamDirect(ADest,Param);
  end;
end;

procedure TBasicVisComposition.MatchOutputs(const Source: TPreset);

  procedure DoMatch(const ASource: TPresetVis; var ADest: TBasicVisualisation); inline;
  var
    J          : Integer;
  begin
    with ASource
      do for J:=0 to Length(VisOutputs)-1
        do ToVisOutput(VisOutputs[J],ADest.OutputPtr(J)^);
  end;

var
  I: Integer;
begin
  for I:=0 to Length(FLayers)-1 do DoMatch(Source.Layers[I],FLayers[I]);
  for I:=0 to Length(FTriggers)-1 do DoMatch(Source.Triggers[I],FTriggers[I]);
end;

{%ENDREGION}
{%REGION TVisComposition}

type
  TChangedParamData = record
    Output: TVisOutput;
    Value : Pointer;
  end;

constructor TVisComposition.Create(ASource: TPreset; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush);
var
  AQueue: TLQueue;
  I,L   : Integer;
begin
  AQueue:=TLQueue.Create(SizeOf(TPreOutData));
  inherited Create(ASource,ACanvas,ASpectrumData,AQueue,ABGThread);
  AQueue.Destroy;
  FPreset:=ASource;
  //Match Params
  L:=Length(ASource.Params);
  SetLength(FParams,L);
  for I:=0 to L-1 do with FParams[I] do begin
    ToVisOutputDirect(ASource.Params[I].OutputInfo,Output);
    VC:=Self;
  end;
  //Damit das Wechseln der Parameter live über GUI synchron ist
  FChangedParams:=TLQueue.Create(SizeOf(TChangedParamData));
  FDependent:=true;
end;

destructor TVisComposition.Destroy;
begin
  FChangedParams.Destroy;
  SetLength(FParams,0);
  inherited Destroy;
end;

procedure TVisComposition.Free;
begin
  if not FDependent then Destroy;
end;

procedure TVisComposition.SetIndependence;
begin
  FDependent:=false;
end;

procedure TVisComposition.PushChange(const AOutput: TVisOutput; const AValue);
var
  AChange: TChangedParamData;
  ValSize: Cardinal;
begin
  ValSize:=VisParamTypes.SizeOfOutput(AOutput.AType);
  with AChange do begin
    Output:=AOutput;
    GetMem(Value,ValSize);
    Move(AValue,Value^,ValSize);
  end;
  FChangedParams.Push(AChange);
end;

procedure TVisComposition.ChangeParams;
var
  AChange: TChangedParamData;
begin
  while not FChangedParams.Empty do begin
    FChangedParams.Pop(AChange);
    with AChange do begin
      with Output do DoSet(Data,Value^);
      FreeMem(Value,VisParamTypes.SizeOfOutput(Output.AType));
    end;
  end;
end;

procedure TVisComposition.Visualize;
begin
  inherited Visualize;
  ChangeParams;
end;

function TVisComposition.ParamPtr(Index: LongWord): PVCOutput;
begin
  Result:=@FParams[Index];
end;

procedure TVisComposition.SetParam(Index: LongWord; const Value); stdcall;
begin
  SetVCOutput(FParams[Index],Value);
end;

procedure TVisComposition.GetParam(Index: LongWord; out Value); stdcall;
begin
  GetVCOutput(FParams[Index],Value);
end;

function TVisComposition.GetParamType(Index: Cardinal): TVisOutputType;
begin
  Result:=FParams[Index].Output.AType;
end;

function TVisComposition.GetParamCount: Cardinal;
begin
  Result:=Length(FParams);
end;

function TVisComposition.GetParamName(Index: Cardinal): string;
begin
  Result:=FPreset.Params[Index].Name;
end;

function TVisComposition.GetName: string;
begin
  Result:=FPreset.Name;
end;

{%ENDREGION}
{%REGION TVisBGThread}

type
  TThreadProcs = packed record
    Proc,Finished: TVisProc;
    Vis          : TVisualisation;
  end;

constructor TVisBGThread.Create;
begin
  inherited Create(true);
  FSleepTime:=0;
  FreeOnTerminate:=false;
  Self.Priority:=tpLower;
  FThreadProcs:=TLQueue.Create(SizeOf(TThreadProcs));
  FFinishProcs:=TLQueue.Create(SizeOf(TThreadProcs));
  FFinished:=true;
end;

destructor TVisBGThread.Destroy;
begin
  Terminate;
  if Suspended then Resume;
  while not FFinished do ;
  FFinishProcs.Destroy;
  FThreadProcs.Destroy;
  inherited Destroy;
end;

procedure TVisBGThread.Execute;
var
  AProcs: TThreadProcs;
begin
  FFinished:=false;
  while not Terminated do begin
    FThreadProcs.Pop(AProcs);
    with AProcs do Vis.CallVisProc(Proc);
    FFinishProcs.Push(AProcs);
    if (FSleepTime>0) and (not Terminated) then Sleep(FSleepTime);
    if FThreadProcs.Empty then Suspend;
  end;
  FFinished:=true;
end;

function TVisBGThread.GetTaskCount: Cardinal;
begin
  Result:=FThreadProcs.Count;
end;

procedure TVisBGThread.SyncFinish;
var
  AProcs: TThreadProcs;
begin
  while not FFinishProcs.Empty do begin
    FFinishProcs.Pop(AProcs);
    with AProcs do Vis.CallVisProc(Finished);
  end;
end;

procedure TVisBGThread.Push(AVis: TVisualisation; AProc: TVisProc; AFinished: TVisProc = nil);
var
  AProcs: TThreadProcs;
begin
  with AProcs do begin
    Proc:=AProc;
    Finished:=AFinished;
    Vis:=AVis;
  end;
  FThreadProcs.Push(AProcs);
  if Suspended then Resume;
end;

procedure TVisBGThread.Clear;
begin
  FThreadProcs.Clear;
  FFinishProcs.Clear;
end;

{%ENDREGION}
{%REGION TVisCompositions}

constructor TVisCompositions.Create(APresets: TPresets; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; APresetLoader: TPresetLoader; AMode: TPresetBufferMode = bmManual);
begin
  inherited Create;
  FPresets:=APresets;
  FMode:=AMode;
  FCanvas:=ACanvas;
  FSpectrumData:=ASpectrumData;
  FBGThread:=ABGThread;
  FPresetLoader:=APresetLoader;
  if AMode=bmKeep then LoadAll;
end;

destructor TVisCompositions.Destroy;
begin
  Clear;
  FSpectrumData:=nil;
  FCanvas:=nil;
  inherited Destroy;
end;

procedure TVisCompositions.LoadItem(AIndex: Integer);
begin
  if Length(FItems)<=AIndex then SetLength(FItems,AIndex+1);
  FPresetLoader.LoadPreset(FPresets[AIndex],FItems[AIndex],FCanvas,FSpectrumData,FBGThread);
end;

procedure TVisCompositions.LoadItemHard(AIndex: Integer);
begin
  if Length(FItems)<=AIndex then SetLength(FItems,AIndex+1);
  FItems[AIndex]:=TVisComposition.Create(FPresets[AIndex],FCanvas,FSpectrumData,FBGThread);
end;

procedure TVisCompositions.UnloadItem(AIndex: Integer);
var
  I,L: Integer;
begin
  FItems[AIndex].Destroy;
  FItems[AIndex]:=nil;
  L:=Length(FItems)-1;
  if AIndex=L then begin
    I:=L;
    if I>=1 then while (FItems[I-1]=nil) do begin
      Dec(I);
      if I<1 then break;
    end;
    SetLength(FItems,I);
  end;
end;

function TVisCompositions.Loaded(AIndex: Integer): Boolean;
begin
  if Length(FItems)<=AIndex
    then Result:=false
    else Result:=FItems[AIndex]<>nil;
end;

procedure TVisCompositions.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(FItems)-1 do if FItems[I]<>nil then FItems[I].Destroy;
  SetLength(FItems,0);
end;

procedure TVisCompositions.LoadAll;
var
  I,L: Integer;
begin
  FPresetLoader.PreLoad;
  L:=FPresets.Count;
  SetLength(FItems,L);
  for I:=0 to L-1
    do if FItems[I]=nil
      then FPresetLoader.LoadPreset(FPresets[I],FItems[I],FCanvas,FSpectrumData,FBGThread);
  FPresetLoader.AfterLoad;
end;

procedure TVisCompositions.LoadAllHard;
var
  I,L: Integer;
begin
  FPresetLoader.PreLoad;
  L:=FPresets.Count;
  SetLength(FItems,L);
  for I:=0 to L-1
    do if FItems[I]=nil
      then FItems[I]:=TVisComposition.Create(FPresets[I],FCanvas,FSpectrumData,FBGThread);
  FPresetLoader.AfterLoad;
end;

function TVisCompositions.GetItem(Index: Integer): TVisComposition;
begin
  if Index>=Length(FItems)
    then LoadItemHard(Index)
    else if FItems[Index]=nil
      then LoadItemHard(Index);
  Result:=FItems[Index];
  case FMode of
    bmManual: begin
        Result.SetIndependence;
        FItems[Index]:=nil;
      end;
    bmKeep  : begin
        Result.SetIndependence;
        FItems[Index]:=nil;
        LoadItem(Index);
      end;
  end;
end;

function TVisCompositions.GetItemDirect(Index: Integer): TVisComposition;
begin
  if (Index>=Length(FItems)) or (Index<0)
    then Result:=nil
    else Result:=FItems[Index];
end;

function TVisCompositions.GetPresets: TPresets;
begin
  Result:=FPresets;
end;

{%ENDREGION}
{%REGION TPresetLoaderThread}

type
  TPresetLoadInfo = packed record
    Preset      : TPreset;
    Dest        : ^TVisComposition;
    Canvas      : IMCanvas;
    SpectrumData: ISpectrumData;
    BGThread    : TBGThreadPush;
    OnLoaded    : TPresetLoadedProc;
    PresetIndex : Integer;
  end;

constructor TPresetLoaderThread.Create(AOwner: TPresetLoader);
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  Priority:=tpLower;
  FOwner:=AOwner;
  FTasks:=TLQueue.Create(SizeOf(TPresetLoadInfo));
  FTasksSolved:=0;
  FActivePreset:=nil;
end;

destructor TPresetLoaderThread.Destroy;
begin
  FOwner.Finished;
  FTasks.Destroy;
  inherited Destroy;
end;

procedure TPresetLoaderThread.Execute;
var
  ATask: TPresetLoadInfo;
begin
  while (not FTasks.Empty) or Terminated do begin
    FTasks.Pop(ATask);
    FActivePreset:=ATask.Preset;
    with ATask do begin
      Dest^:=TVisComposition.Create(Preset,Canvas,SpectrumData,BGThread);
      if Assigned(OnLoaded) then OnLoaded(PresetIndex);
    end;
    Inc(FTasksSolved);
  end;
end;

procedure TPresetLoaderThread.PushTask(APreset: TPreset; var ADest: TVisComposition; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
var
  ATask: TPresetLoadInfo;
begin
  with ATask do begin
    Preset:=APreset;
    Dest:=@ADest;
    Canvas:=ACanvas;
    SpectrumData:=ASpectrumData;
    BGThread:=ABGThread;
    OnLoaded:=AOnLoaded;
    PresetIndex:=APresetIndex;
  end;
  //Error hier.... (schlecht reproduzierbar)
  FTasks.Push(ATask);
end;

function TPresetLoaderThread.GetTaskCount: Integer;
begin
  Result:=FTasks.Count;
end;

{%ENDREGION}
{%REGION TPresetLoader}

constructor TPresetLoader.Create;
begin
  inherited Create;
  FPresetLoaderThread:=nil;
end;

destructor TPresetLoader.Destroy;
begin
  if FPresetLoaderThread<>nil then FPresetLoaderThread.WaitFor;
  inherited Destroy;
end;

procedure TPresetLoader.PreLoad;
begin
  if FPresetLoaderThread=nil then begin
    FPresetLoaderThread:=TPresetLoaderThread.Create(Self);
    if Assigned(FOnStarted) then FOnStarted(Self);
  end;
  if not FPresetLoaderThread.Suspended then FPresetLoaderThread.Suspend;
end;

procedure TPresetLoader.AfterLoad;
begin
  FPresetLoaderThread.Resume;
end;

procedure TPresetLoader.LoadPreset(const APreset: TPreset; var Dest: TVisComposition; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
begin
  PreLoad;
  FPresetLoaderThread.PushTask(APreset,Dest,ACanvas,ASpectrumData,ABGThread,AOnLoaded,APresetIndex);
  AfterLoad;
end;

procedure TPresetLoader.PushPreset(const APreset: TPreset; var Dest: TVisComposition; ACanvas: IMCanvas; ASpectrumData: ISpectrumData; ABGThread: TBGThreadPush; AOnLoaded: TPresetLoadedProc = nil; APresetIndex: Integer = -1);
begin
  FPresetLoaderThread.PushTask(APreset,Dest,ACanvas,ASpectrumData,ABGThread,AOnLoaded,APresetIndex);
end;

procedure TPresetLoader.Finished;
begin
  FPresetLoaderThread:=nil;
  if Assigned(FOnFinished) then FOnFinished(Self);
end;

function TPresetLoader.GetActivePreset: TPreset;
begin
  if FPresetLoaderThread<>nil
    then Result:=FPresetLoaderThread.ActivePreset
    else Result:=nil;
end;

function TPresetLoader.GetIsRunning: Boolean;
begin
  Result:=(FPresetLoaderThread<>nil);
end;

function TPresetLoader.GetMaxProgress: Integer;
begin
  if FPresetLoaderThread<>nil
    then with FPresetLoaderThread do Result:=TaskCount+TasksSolved
    else Result:=0;
end;

function TPresetLoader.GetProgress: Integer;
begin
  if FPresetLoaderThread<>nil
    then Result:=FPresetLoaderThread.TasksSolved
    else Result:=0;
end;

{%ENDREGION}
{%REGION Allgemein}

function VisOutputConverters(ASet,ASet_CallChanged: TVisOutputSet; AGet: TVisOutputGet): TVisOutputConverters; inline;
begin
  with Result do begin
    _Set[false]:=ASet;
    _Set[true]:=ASet_CallChanged;
    _Get:=AGet;
  end;
end;

{%ENDREGION}
{%REGION Allgemein - TVisType}

procedure WS_DoNothing(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  //do nothing
end;

function VisType(const AName: string; const ID; const AVisProc: TVisProc; const AParamDesc: TVisParamDesc; const AInitialValues; const AVisParamSize: Cardinal; const AOutputDesc: TVisOutputDesc; const ACanvasDesc: TVisCanvasDesc; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil; const AC1Desc: ShortString = '1. Linienfarbe'; const AC2Desc: ShortString = '2. Linienfarbe'; const AC3Desc: ShortString = 'Hintergrundfarbe'): TVisType;
var
  AVisID    : TVisID absolute ID;
  I,L       : Integer;
begin
  with Result do begin
    Name:=AName;
    if AVisProc<>nil
      then VisProc:=AVisProc
      else VisProc:=@WS_DoNothing;
    VisParamDesc:=AParamDesc;

    L:=Length(VisParamDesc);
    if L>0 then VisParamDesc[0].Offset:=TVisInputPos(0);
    for I:=1 to L-1
      do with VisParamDesc[I-1]
        do VisParamDesc[I].Offset:=Offset+VisParamTypes.SizeOfParam(AType);

    OutputDesc:=AOutputDesc;
    VisParamSize:=AVisParamSize;
    CanvasDesc:=ACanvasDesc;
    WorkspaceSize:=AWorkspaceSize;
    C1Desc:=AC1Desc;
    C2Desc:=AC2Desc;
    C3Desc:=AC3Desc;
    VisID:=AVisID;
    if AInitWorkspace<>nil
      then InitWorkspace:=AInitWorkspace
      else InitWorkspace:=@WS_DoNothing;
    if AFreeWorkspace<>nil
      then FreeWorkspace:=AFreeWorkspace
      else FreeWorkspace:=@WS_DoNothing;
    if AFirstAction<>nil
      then FirstAction:=AFirstAction
      else FirstAction:=@WS_DoNothing;
    GetMem(InitialValues,AVisParamSize);
    Move(AInitialValues,InitialValues^,AVisParamSize);
  end;
end;

function VisType(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil; const ACanvasNames: array of string): TVisType;
var
  I,L,AVisParamSize,PseudoParamCount: Integer;
  AVisParamDesc                     : TVisParamDesc;
  AVisOutputDesc                    : TVisOutputDesc;
  AVisCanvasDesc                    : TVisCanvasDesc;
  AC1Desc,AC2Desc,AC3Desc           : ShortString;
begin
  SetLength(AVisParamDesc,Length(AParamTypes));
  PseudoParamCount:=Length(AParamNames)-Length(AParamTypes);
  AVisParamSize:=0;
  for I:=0 to Length(AParamTypes)-1 do with AVisParamDesc[I] do begin
    Name:=AParamNames[I+PseudoParamCount];
    AType:=AParamTypes[I];
    AVisParamSize+=VisParamTypes.SizeOfParam(AType);
  end;
  if PseudoParamCount>0 then begin
    AC1Desc:=AParamNames[0];
    if PseudoParamCount>1 then begin
      AC2Desc:=AParamNames[1];
      if PseudoParamCount>2
        then AC3Desc:=AParamNames[2]
        else AC3Desc:='Hintergrundfarbe';
    end else begin
      AC2Desc:='2. Linienfarbe';
      AC3Desc:='Hintergrundfarbe';
    end;
  end else begin
    AC1Desc:='1. Linienfarbe';
    AC2Desc:='2. Linienfarbe';
    AC3Desc:='Hintergrundfarbe';
  end;
  L:=Length(AChangeParams);
  for I:=0 to L-1 do AVisParamDesc[I].Change:=AChangeParams[I];
  for I:=L to Length(AParamTypes)-1 do AVisParamDesc[I].Change:=nil;

  SetLength(AVisOutputDesc,Length(AOutputTypes));
  for I:=0 to Length(AOutputTypes)-1 do with AVisOutputDesc[I] do begin
    Name:=AOutputNames[I];
    AType:=AOutputTypes[I];
  end;
  SetLength(AVisCanvasDesc,Length(ACanvasNames));
  for I:=0 to Length(ACanvasNames)-1 do AVisCanvasDesc[I]:=ACanvasNames[I];
  Result:=VisType(AName,ID,AVisProc,AVisParamDesc,AInitialValues,AVisParamSize,AVisOutputDesc,AVisCanvasDesc,AWorkspaceSize,AInitWorkspace,AFreeWorkspace,AFirstAction,AC1Desc,AC2Desc,AC3Desc);
end;

function ToVisParamDesc(AName: string; AAType: TVisParamType; AChange: TVisProc; AOffset: TVisInputPos): TSVisParamDesc;
begin
  with Result do begin
    Name:=AName;
    AType:=AAType;
    Change:=AChange;
    Offset:=AOffset;
  end;
end;

{%ENDREGION}
{%REGION Allgemein - TVisualisations - Registrieren}

function RegisterVisTypeListElement(const AVisType: TVisType; var AList: TVisTypeList): Integer;
begin
  Result:=Length(AList);
  SetLength(AList,Result+1);
  if Result>0 then while AList[Result-1].VisID>AVisType.VisID do begin
    AList[Result]:=AList[Result-1];
    Dec(Result);
    if Result=0 then break;
  end;
  AList[Result]:=AVisType;
end;

procedure DestroyVisTypeListElement(const Index: Integer; var AList: TVisTypeList);
begin
  with AList[Index] do FreeMem(InitialValues,VisParamSize);
end;

procedure UnRegisterVisTypeListElement(const ID: Cardinal; var AList: TVisTypeList);
var
  L,I: Integer;
begin
  DestroyVisTypeListElement(ID,AList);
  L:=Length(AList);
  for I:=ID to L-2 do AList[I]:=AList[I+1];
  SetLength(AList,L-1);
end;

procedure ClearVisTypeList(var AList: TVisTypeList);
var
  I: Integer;
begin
  for I:=0 to Length(AList)-1 do DestroyVisTypeListElement(I,AList);
  SetLength(AList,0);
end;

function PresetOutputInfo(ALayer,AParam: Integer): TPresetOutputInfo;
begin
  with Result do begin
    Layer:=ALayer;
    Param:=AParam;
  end;
end;

procedure SetVisOutput(const AOutput: TVisOutput; const Value);
begin
  with AOutput do DoSet(Data,Value);
end;

procedure GetVisOutput(const AOutput: TVisOutput; out Value);
begin
  with AOutput do DoGet(Data,Value);
end;

procedure SetVCOutput(const AOutput: TVCOutput; const Value);
begin
  with AOutput do VC.PushChange(Output,Value);
end;

procedure GetVCOutput(const AOutput: TVCOutput; out Value);
begin
  with AOutput.Output do DoGet(Data,Value);
end;

const
  _PresetBufferModeStrings: array [TPresetBufferMode] of String = ('Manual','Keep','Reuse');

function PresetBufferModeToStr(AMode: TPresetBufferMode): string;
begin
  Result:=_PresetBufferModeStrings[AMode];
end;

{%ENDREGION}
{%REGION Allgemein - Operatoren}

operator < (constref m1: TVisType; m2: TVisID) r: Boolean;
begin
  Result:=(m1.VisID<m2);
end;

operator < (m1: TVisID; constref m2: TVisType) r: Boolean;
begin
  Result:=(m1<m2.VisID);
end;

operator < (constref m1: TVisType; constref m2: TVisType) r: Boolean;
begin
  Result:=(m1.VisID<m2.VisID);
end;

{%ENDREGION}

const
  _AID: TVisID  = 0;
initialization
  VisParamTypes:=TVisParamTypes.Create;
  TVisualisation.CreateAndRegisterVis('',_AID,nil,[],[],[],vpDefault,[],[],[]);
finalization
  TVisualisation.ClearVis;
  VisParamTypes.Destroy;
end.

