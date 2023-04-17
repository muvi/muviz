unit PresetUtil2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisTypeUnit, Controls, Graphics, VisType, Dialogs,
  ArrayListedObjects, SqrFunc, PhysicsWire, AdvCoord, AdvClasses,
  ObjectClasses, ObjectClassBasic, AdvGraphCtrl, VisDrawUnit, ParamPainter,
  SplitImageButtons, ChooseColor, ExtPtrType, GraphX32, ParamType, AdvFunc;

type
  TPresetEditor          = class;
  TEvent                 = procedure of object;
  TVPVMouseMoveEvent     = procedure (Shift: TShiftState; X,Y: Integer) of object;
  TPWContainer           = class;
  TPresetControl         = class;
  TVPVPlug               = class;
  TViewPresetVis         = class;
  TRepaintRequest        = procedure (ACtrl: TPresetControl) of object;
  TSlidebar              = class;
  TSetBoundsEvent        = procedure (var ANewBounds: TBoundsRect) of object;
  TDeletePresetOP        = procedure (AIndex: Integer) of object;
  TInsertPresetOP        = procedure (AIndex: Integer; const AInfo: TPresetOutputInfo; const AName: string = '') of object;
  TPresetControlEvent    = procedure (Sender: TPresetControl) of object;
  TVPVSlidePlugClass     = class of TVPVSlidePlug;
  TRLRepaintEvent        = procedure (ACtrl: TPresetControl) of object;

  //Usage: PlugStateArray[Plugged][Highlighted]...
  generic TPlugStateArray<AGType>= array [Boolean] of record
    Normal      : array [Boolean] of AGType;
    ExtHighlight: AGType;
  end;

  TBitmapPlugStateArray  = specialize TPlugStateArray<TBitmap>;

  TMouseMoveData         = record
    Shift: TShiftState;
    Pos  : TPoint;
  end;

  TPlugModifierPixel     = packed record
    //Gray, Alpha, Result Alpha (=Deckkraft des Ergebnisbildes)
    RA,A,G: Byte;
  end;

  TPlugModifier          = object
  strict private
    FModifier: array [0..ParamPicWidth-1,0..ParamPicHeight-1] of TPlugModifierPixel;
  public
    procedure ImportFromBitmap(ABmp: TBitmap);
    procedure ModifyPlugPic(var APic: TParamPic; AColor: TColor32; Dest: TBitmap);
  end;

  TPWContainer           = class (TObjectItem)
  strict private
    FWire          : TPhysicsWire;
    FParam         : TVPVPlug;
    FOutput        : TVPVPlug;
    FEditor        : TPresetEditor;
    FParamColor    : TColor32;
    FOutputColor   : TColor32;
    procedure SetParam(Value: TVPVPlug); inline;
    procedure SetOutput(Value: TVPVPlug); inline;
    function GetOpenEndIsOutput: Boolean; inline;
    function GetOpenEndIsParam: Boolean; inline;
    function GetFirstKnot: TRealPoint; inline;
    procedure SetFirstKnot(Value: TRealPoint); inline;
    function GetLastKnot: TRealPoint; inline;
    procedure SetLastKnot(Value: TRealPoint); inline;
    function GetOpenEndPos: TRealPoint; inline;
    procedure SetOpenEndPos(Value: TRealPoint); inline;

    procedure DetermineFType;
  public
    constructor Create(AEditor: TPresetEditor; AWireSettings: TPhysicsWireSettings);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; ARect: TRect);
    property FirstKnot: TRealPoint read GetFirstKnot write SetFirstKnot;
    property LastKnot: TRealPoint read GetLastKnot write SetLastKnot;
    property OpenEndIsOutput: Boolean read GetOpenEndIsOutput;
    property OpenEndIsParam: Boolean read GetOpenEndIsParam;
    property OpenEndPos: TRealPoint read GetOpenEndPos write SetOpenEndPos;
    property Output: TVPVPlug read FOutput write SetOutput;
    property Param: TVPVPlug read FParam write SetParam;
    property Wire: TPhysicsWire read FWire;
  end;

  TPresetControl         = class (TObjectItem)
  strict private
    FOwner                  : TPresetControl;
    FEditor                 : TPresetEditor;
    FControlUnderMouse      : TPresetControl;
    FOwnersControlUnderMouse: ^TPresetControl;
    FViewRect               : TRect;
  private
    FOwnersViewRect         : PRect;
    FCanvas                 : TCanvas;
    FAbsolutePos            : TPoint;
    FBoundsRect             : TBoundsRect;
    FOnBoundsChanged        : TPresetControlEvent;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetBoundsRect(Value: TRect);
    procedure SetFBounds(Value: TBoundsRect);
    function GetDirectControlUnderMouse: TPresetControl;
    procedure BoundsChanged(var ANewBounds: TBoundsRect); inline;
  private
    //inline repaint list
    FRepaintNecessary       : Boolean;
    FRepaintCtrl            : TPresetControl;
    FNextRepaintCtrl        : TPresetControl;
    procedure PaintConnectedIfNecessary(ACanvas: TCanvas);
    procedure PaintIfNecessary(ACanvas: TCanvas);

    function ViewRectPtr: PRect; inline;
  strict protected
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoPaint; virtual; abstract;
    procedure DoAssignViewRect; virtual;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); virtual;
    procedure DoAlign; virtual;
    function Highlighted: Boolean;
    procedure Repaint; virtual;

    property Canvas: TCanvas read FCanvas;
    property FBounds: TBoundsRect read FBoundsRect write SetFBounds;
    property ViewRect: TRect read FViewRect;
  protected
    property AbsolutePos: TPoint read FAbsolutePos;
    property Editor: TPresetEditor read FEditor;
    property Owner: TPresetControl read FOwner;
  public
    constructor Create(AOwner: TPresetControl; const AInitialBounds: TBoundsRect; AEditor: TPresetEditor = nil); virtual; reintroduce;
    destructor Destroy; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure AssignViewRect;
    procedure Paint(ACanvas: TCanvas);
    function CheckMouseMove(Shift: TShiftState; X,Y: Integer): Boolean;
    procedure MouseLeave;

    property BoundsRect: TRect read FBoundsRect.Rect write SetBoundsRect;
    property ControlUnderMouse: TPresetControl read FControlUnderMouse;
    property DirectControlUnderMouse: TPresetControl read GetDirectControlUnderMouse;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read FBoundsRect.Rect.Left write SetLeft;
    property Top: Integer read FBoundsRect.Rect.Top write SetTop;
    property Width: Integer read GetWidth write SetWidth;

    property OnBoundsChanged: TPresetControlEvent read FOnBoundsChanged write FOnBoundsChanged;
  end;

  TVPVPlug               = class (TPresetControl)
  strict private
    FExtHighlightCount: Integer;
    function GetExtHighlight: Boolean;
    procedure SetExtHighlight(Value: Boolean);
  strict protected
    function GetConnected: Boolean; virtual; abstract;
    function GetIsOutput: Boolean; virtual; abstract;
    function GetInfo: TPresetOutputInfo; virtual; abstract;
    function GetPlugType: TVisOutputType; virtual; abstract;
    function GetName: string; virtual; abstract;
    procedure SetName(Value: string); virtual; abstract;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoPaint; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TPresetControl; const AInitialBounds: TBoundsRect); reintroduce;
    destructor Destroy; override;
    //procedure WireUpdate; virtual; abstract;
    procedure WireDestroyed; virtual;
    function ConnectTo(APlug: TVPVPlug): Boolean; virtual;
    property ExtHighlight: Boolean read GetExtHighlight write SetExtHighlight;
    property Info: TPresetOutputInfo read GetInfo;
    property IsOutput: Boolean read GetIsOutput;
    property Name: string read GetName write SetName;
    property PlugType: TVisOutputType read GetPlugType;
  end;

  TVPVSlidePlug          = class (TVPVPlug)
  strict private
    FOutput   : PNamedPresetOutput;
    FWire     : TPWContainer;
    FIndex    : Integer;
  strict protected
    function GetName: string; override;
    procedure SetName(Value: string); override;
    function GetConnected: Boolean; override;
    function GetInfo: TPresetOutputInfo; override;
    procedure DoMouseLeave; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    property Output: PNamedPresetOutput read FOutput;
  protected
    procedure DoFree; override;
  public
    constructor Create(AOwner: TSlidebar; const AInitialBounds: TBoundsRect; var AOutput: TNamedPresetOutput; AIndex: Integer; AWire: TPWContainer); virtual;
    destructor Destroy; override;
    procedure WireDestroyed; override;
    procedure RemoveFromWire; virtual;
    function ConnectTo(APlug: TVPVPlug): Boolean; override;
    procedure SetOutput(var AOutput: TNamedPresetOutput; AIndex: Integer);
    property Index: Integer read FIndex;
    property Wire: TPWContainer read FWire;
  end;

  TVPVParamSlidePlug     = class (TVPVSlidePlug)
  strict protected
    function GetIsOutput: Boolean; override;
    function GetPlugType: TVisOutputType; override;
    //procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoAlign; override;
    procedure DoAssignViewRect; override;
  public
    constructor Create(AOwner: TSlidebar; const AInitialBounds: TBoundsRect; var AOutput: TNamedPresetOutput; AIndex: Integer; AWire: TPWContainer); override;
    destructor Destroy; override;
    procedure RemoveFromWire; override;
  end;

  TVPVOutputSlidePlug    = class (TVPVSlidePlug)
  strict protected
    function GetIsOutput: Boolean; override;
    function GetPlugType: TVisOutputType; override;
    //procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoAlign; override;
    procedure DoAssignViewRect; override;
  public
    constructor Create(AOwner: TSlidebar; const AInitialBounds: TBoundsRect; var AOutput: TNamedPresetOutput; AIndex: Integer; AWire: TPWContainer); override;
    destructor Destroy; override;
    procedure RemoveFromWire; override;
  end;

  TVPVLayerPlug          = class (TVPVPlug)
  strict private
    FIndex: TPresetIndex;
    FType : TVisParamType;
  strict protected
    function GetInfo: TPresetOutputInfo; override;
    function GetPlugType: TVisOutputType; override;
    procedure SetName(Value: string); override;
  public
    constructor Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; AIndex: TPresetIndex; AType: TVisParamType);
    procedure AssignWire(AWire: TPWContainer); virtual; abstract;
    property Index: TPresetIndex read FIndex;
  end;

  TVPVParamPlug          = class (TVPVLayerPlug)
  strict private
    FWires     : TObjectList;
  strict protected
    function GetConnected: Boolean; override;
    function GetIsOutput: Boolean; override;
    function GetName: string; override;
    procedure DoAssignViewRect; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; AParamIndex: TPresetIndex; AType: TVisParamType); virtual;
    destructor Destroy; override;
    function ConnectTo(APlug: TVPVPlug): Boolean; override;
    //procedure WireDestroyed; override;
    procedure AssignWire(AWire: TPWContainer); override;
  end;

  TVPVOutputPlug         = class (TVPVLayerPlug)
  strict private
    FOutput     : ^TPresetOutputInfo;
    FWire       : TPWContainer;
  strict protected
    function GetConnected: Boolean; override;
    function GetIsOutput: Boolean; override;
    function GetName: string; override;
    procedure DoAssignViewRect; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; var AOutput: TPresetOutputInfo; AOutputIndex: TPresetIndex; AType: TVisOutputType); virtual;
    destructor Destroy; override;
    procedure WireDestroyed; override;
    function ConnectTo(APlug: TVPVPlug): Boolean; override;
    procedure AssignWire(AWire: TPWContainer); override;
  end;

  TVPVDummyPlug          = class (TVPVPlug)
  strict protected
    function GetConnected: Boolean; override;
    function GetInfo: TPresetOutputInfo; override;
    function GetPlugType: TVisOutputType; override;
    procedure SetName(Value: string); override;
  end;

  TVPVDummyOutputPlug    = class (TVPVDummyPlug)
  strict protected
    function GetIsOutput: Boolean; override;
    function GetName: string; override;
  end;

  TVPVDummyParamPlug     = class (TVPVDummyPlug)
  strict protected
    function GetIsOutput: Boolean; override;
    function GetName: string; override;
  end;

  TExpanderButton        = class (TPresetControl)
  strict private
    FExpanded  : Boolean;
    FOnClick   : TNotifyEvent;
    procedure SetExpanded(Value: Boolean);
  strict protected
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
  protected
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; AExpanded: Boolean = true); reintroduce;
    destructor Destroy; override;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TViewPresetVis         = class (TPresetControl)
  strict private
    FLayer            : PPresetVis;
    FLayerIndex       : TPresetIndex;
    FVisType          : PVisType;
    FEditPos          : Integer;
    FMainParam        : TVPVParamPlug;
    FOutputs          : array of TVPVOutputPlug;
    FParams           : array of TVPVParamPlug;
    FDummyOutputPlug  : TVPVDummyOutputPlug;
    FDummyParamPlug   : TVPVDummyParamPlug;
    FParamEdits       : array of TPPParam;
    FDoPaint          : TEvent;
    FDoMouseMove      : TVPVMouseMoveEvent;
    FExpanderBtn      : TExpanderButton;
    procedure DoSetExpanded;
    function GetExpanded: Boolean;
    procedure SetExpanded(Value: Boolean);
    function GetParam(Index: TPresetIndex): TVPVParamPlug;
    function GetOutput(Index: TPresetIndex): TVPVOutputPlug;
    procedure ExpanderBtnClick(Sender: TObject);

    procedure DoPaint_Expanded;
    procedure DoPaint_Not_Expanded;
    procedure DoMouseMove_Expanded(Shift: TShiftState; X,Y: Integer);
    procedure DoMouseMove_Not_Expanded(Shift: TShiftState; X,Y: Integer);
  strict protected
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoPaint; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoAlign; override;
    procedure DoAssignViewRect; override;
  public
    constructor Create(AOwner: TPresetEditor; const AInitialBounds: TBoundsRect; var ALayer: TPresetVis; ALayerIndex: TPresetIndex); reintroduce;
    destructor Destroy; override;
    procedure InitLayer;

    property Expanded: Boolean read GetExpanded write SetExpanded;
    property LayerIndex: TPresetIndex read FLayerIndex;
    property Outputs[Index: TPresetIndex]: TVPVOutputPlug read GetOutput;
    property Params[Index: TPresetIndex]: TVPVParamPlug read GetParam;
  end;

  TSlidebar              = class (TPresetControl)
  strict private
    FOutputs       : ^TNamedPresetOutputs;
    FPlugs         : TObjectArray;
    FPlugClass     : TVPVSlidePlugClass;
    FDeleteOP      : TDeletePresetOP;
    FInsertOP      : TInsertPresetOP;
    procedure OutputRemoved(AObject: TObjectItem; Index: Integer);
  strict protected
    procedure DoPaint; override;
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoAssignViewRect; override;
    //Gibt den einzigen Plug an einem halb angeschlossenen Kabel zurück
    function GetPluggedEnd(AWire: TPWContainer): TVPVPlug; virtual; abstract;
    function GetIsOutput: Boolean; virtual; abstract;
    function GetPlug(Layer,Param: TPresetIndex): TVPVLayerPlug; virtual; abstract;
    procedure SetPresetProcs(AInsert: TInsertPresetOP; ADelete: TDeletePresetOP);
    procedure AssignOutputs(var AOutputs: TNamedPresetOutputs);
  protected
    function DropWire(Index: Integer): Boolean;
    procedure DeleteOutput(Index: Integer);
  public
    constructor Create(AOwner: TPresetEditor; APlugClass: TVPVSlidePlugClass; const AInitialBounds: TBoundsRect); reintroduce;
    destructor Destroy; override;
    procedure Clear; inline;
    procedure AssignPreset(var APreset: TPreset); virtual; abstract;
    property IsOutput: Boolean read GetIsOutput;
  end;

  TParamSlidebar         = class (TSlidebar)
  strict protected
    function GetPluggedEnd(AWire: TPWContainer): TVPVPlug; override;
    function GetIsOutput: Boolean; override;
    function GetPlug(Layer,Param: TPresetIndex): TVPVLayerPlug; override;
  public
    constructor Create(AOwner: TPresetEditor; const AInitialBounds: TBoundsRect);
    procedure AssignPreset(var APreset: TPreset); override;
  end;

  TOutputSlidebar        = class (TSlidebar)
  strict protected
    function GetPluggedEnd(AWire: TPWContainer): TVPVPlug; override;
    function GetIsOutput: Boolean; override;
    function GetPlug(Layer,Param: TPresetIndex): TVPVLayerPlug; override;
  public
    constructor Create(AOwner: TPresetEditor; const AInitialBounds: TBoundsRect);
    procedure AssignPreset(var APreset: TPreset); override;
  end;

  TPresetEditorImages    = object
  strict private
    FTempDrawBitmap                           : TBitmap;
    FImageDestBmp                             : TBitmap;
    FViewPresetHeaderHeight                   : Integer;
    FViewPresetItemHeight                     : Integer;
    procedure SetImages(Value: TImageList);
    procedure SetSlidebarImageIndex(Value: Integer);
    procedure SetHighlightedSlidebarImageIndex(Value: Integer);
  protected
    //Misc
    FImages                                   : TImageList;
    FImageCenter                              : TPoint;
    //fertige Bilder für die Plugs
    FPlugBmps                                 : array of TBitmapPlugStateArray;
    //Buffered Bitmaps
    FSlidebar                                 : TBitmap;
    FSlidebarTop                              : TBitmap;
    FSlidebarBottom                           : TBitmap;
    FSlidebarH                                : TBitmap;
    FSlidebarTopH                             : TBitmap;
    FSlidebarBottomH                          : TBitmap;
    //Image Indices
    FSlidebarImageIndex                       : Integer;
    FHighlightedSlidebarImageIndex            : Integer;
    FExpandImageIndex                         : Integer;
    FExpandHImageIndex                        : Integer;
    FExpandDImageIndex                        : Integer;
    FDeexpandImageIndex                       : Integer;
    FDeexpandHImageIndex                      : Integer;
    FDeexpandDImageIndex                      : Integer;
    FUnplugedImageIndex                       : Integer;
    FPlugedImageIndex                         : Integer;
    FHighlightedPlugedImageIndex              : Integer;
    FHighlightedUnplugedImageIndex            : Integer;
    FExtHighlightedPlugedImageIndex           : Integer;
    FExtHighlightedUnplugedImageIndex         : Integer;
    FUnplugedUndefinedImageIndex              : Integer;
    FPlugedUndefinedImageIndex                : Integer;
    FHighlightedPlugedUndefinedImageIndex     : Integer;
    FHighlightedUnplugedUndefinedImageIndex   : Integer;
    FExtHighlightedPlugedUndefinedImageIndex  : Integer;
    FExtHighlightedUnplugedUndefinedImageIndex: Integer;
  public
    procedure Produce;
    procedure Destroy;
    procedure InitPlugBmps;
    procedure FreePlugBmps;
    procedure LoadImage(Pluged,Highlighted,ExtHighlight: Boolean; AType: TVisOutputType); overload;
    procedure LoadImage(Index: Integer); overload;

    //property BackgroundBitmap: TBitmap read FBackgroundBitmap write FBackgroundBitmap;
    property DeexpandImageIndex: Integer read FDeexpandImageIndex write FDeexpandImageIndex default -1;
    property DeexpandHImageIndex: Integer read FDeexpandHImageIndex write FDeexpandHImageIndex default -1;
    property DeexpandDImageIndex: Integer read FDeexpandDImageIndex write FDeexpandDImageIndex default -1;
    property ExpandImageIndex: Integer read FExpandImageIndex write FExpandImageIndex default -1;
    property ExpandHImageIndex: Integer read FExpandHImageIndex write FExpandHImageIndex default -1;
    property ExpandDImageIndex: Integer read FExpandDImageIndex write FExpandDImageIndex default -1;
    property ExtHighlightedPlugedImageIndex: Integer read FExtHighlightedPlugedImageIndex write FExtHighlightedPlugedImageIndex default -1;
    property ExtHighlightedPlugedUndefinedImageIndex: Integer read FExtHighlightedPlugedUndefinedImageIndex write FExtHighlightedPlugedUndefinedImageIndex;
    property ExtHighlightedUnplugedImageIndex: Integer read FExtHighlightedUnplugedImageIndex write FExtHighlightedUnplugedImageIndex default -1;
    property ExtHighlightedUnplugedUndefinedImageIndex: Integer read FExtHighlightedUnplugedUndefinedImageIndex write FExtHighlightedUnplugedUndefinedImageIndex;
    property HighlightedPlugedImageIndex: Integer read FHighlightedPlugedImageIndex write FHighlightedPlugedImageIndex default -1;
    property HighlightedPlugedUndefinedImageIndex: Integer read FHighlightedPlugedUndefinedImageIndex write FHighlightedPlugedUndefinedImageIndex;
    property HighlightedSlidebarImageIndex: Integer read FHighlightedSlidebarImageIndex write SetHighlightedSlidebarImageIndex default -1;
    property HighlightedUnplugedImageIndex: Integer read FHighlightedUnplugedImageIndex write FHighlightedUnplugedImageIndex default -1;
    property HighlightedUnplugedUndefinedImageIndex: Integer read FHighlightedUnplugedUndefinedImageIndex write FHighlightedUnplugedUndefinedImageIndex;
    property ImageCenter: TPoint read FImageCenter;
    property Images: TImageList read FImages write SetImages;
    property PlugedImageIndex: Integer read FPlugedImageIndex write FPlugedImageIndex default -1;
    property PlugedUndefinedImageIndex: Integer read FPlugedUndefinedImageIndex write FPlugedUndefinedImageIndex default -1;
    property SlidebarImageIndex: Integer read FSlidebarImageIndex write SetSlidebarImageIndex default -1;
    property UnplugedImageIndex: Integer read FUnplugedImageIndex write FUnplugedImageIndex default -1;
    property UnplugedUndefinedImageIndex: Integer read FUnplugedUndefinedImageIndex write FUnplugedUndefinedImageIndex default -1;
    property ViewPresetHeaderHeight: Integer read FViewPresetHeaderHeight;
    property ViewPresetItemHeight: Integer read FViewPresetItemHeight;

    property Slidebar: TBitmap read FSlidebar;
    property SlidebarBottom: TBitmap read FSlidebarBottom;
    property SlidebarBottomH: TBitmap read FSlidebarBottomH;
    property SlidebarH: TBitmap read FSlidebarH;
    property SlidebarTop: TBitmap read FSlidebarTop;
    property SlidebarTopH: TBitmap read FSlidebarTopH;
    property TempDrawBitmap: TBitmap read FImageDestBmp;
  end;

  TPresetEditor          = class (TCustomControl)
  strict private
    FLocked                       : Boolean;
    FPictures                     : TPresetEditorImages;
    FPreset                       : TPreset;
    FViewPreset                   : TObjectArray;
    FLayerPosX                    : Integer;
    FScrollPos                    : Integer;
    FPaintBuffer                  : TBitmap;
    FPaintBufferWithWires         : TBitmap;
    FViewRect                     : TRect;
    FTriggerStartIndex            : Integer;
    FMouseIsDown                  : Boolean;
    FWires                        : TObjectList;
    FWireSettings                 : TPhysicsWireSettings;
    FBottom                       : Integer;
    FWireAtMouse                  : TPWContainer;
    FParamSlidebar                : TParamSlidebar;
    FOutputSlidebar               : TOutputSlidebar;
    FHint                         : TGraphicsHint;
    FNextSlideName                : string;
    FPParams                      : TParamPainter;
    FBackgroundBitmap             : TBitmap;
    FMovingCount                  : Cardinal;
    FRendering                    : Boolean;
    FMaxScrollPos                 : Integer;
    FMouseMoveData                : TMouseMoveData;
    FPixelSize                    : Real; //Pixelgröße für das benutzen der Kabel
    //events
    FOnStartRender                : TNotifyEvent;
    FOnEndRender                  : TNotifyEvent;
    FOnMaxScrollPosChanged        : TNotifyEvent;
    //repaint list
    FRepaintCtrl                  : TPresetControl;
    FRINEventStarted              : Boolean;
    FRepaintNecessary             : Boolean;
    FRenderNecessary              : Boolean;

    procedure SetImages(Value: TImageList);
    procedure SetSlidebarImageIndex(Value: Integer);
    procedure SetHighlightedSlidebarImageIndex(Value: Integer);

    procedure SetScrollPos(Value: Integer);
    function GetMaxScrollPos: Integer;
    function GetHintColor: TColor;
    procedure SetHintColor(Value: TColor);
    function GetBoolBoxUI: Integer;
    procedure SetBoolBoxUI(const Value: Integer);
    function GetBoolBoxUHI: Integer;
    procedure SetBoolBoxUHI(const Value: Integer);
    function GetBoolBoxDI: Integer;
    procedure SetBoolBoxDI(const Value: Integer);
    function GetBoolBoxDHI: Integer;
    procedure SetBoolBoxDHI(const Value: Integer);

    procedure GetParamVal(Param: Pointer; AType: TVisParamType; out Value);
    procedure SetParamVal(Param: Pointer; AType: TVisParamType; const Value);

    function GetViewPresetByLayer(Layer: TPresetIndex): TViewPresetVis; inline;
    function GetViewPresetIndex(Layer: TPresetIndex): Integer; inline;
    function GetDirectControlUnderMouse: TPresetControl; inline;

    procedure ParamPainted(Sender: TObject; Param: TPPParam);
    function ParamRequestBG(Sender: TObject; Param: TPPParam): Boolean;
    procedure FHintDrawHide(Sender: TObject);
    procedure ViewPresetBoundsChanged(Sender: TPresetControl);
    procedure SetSubcontrolViewRects; inline;
    procedure CalcMaxScrollPos; inline;
  private
    FControlUnderMouse            : TPresetControl;
    procedure ReCheckMouseMove;
    function ViewRectPtr: PRect; inline;
  strict protected
    procedure PreDraw; inline;
    procedure CreateViewList; inline;
  public
    procedure Draw; virtual;
  strict protected
    property RepaintNecessary: Boolean read FRepaintNecessary;
    property TriggerStartIndex: Integer read FTriggerStartIndex;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;

    procedure StartRender; inline;
    procedure EndRender; inline;
    //Repaint If Necessary
    procedure RIN_StartEvent; inline;
    procedure RIN;
    procedure Repaint(ACtrl: TPresetControl); reintroduce;
    procedure ReRender; inline;
    procedure Render; virtual;
    procedure RemoveMouseWire;
    function NewWire: TPWContainer;
    property ControlUnderMouse: TPresetControl read FControlUnderMouse write FControlUnderMouse;
    property DirectControlUnderMouse: TPresetControl read GetDirectControlUnderMouse;
    property Hint: TGraphicsHint read FHint;
    property MouseIsDown: Boolean read FMouseIsDown;
    property NextSlideName: string read FNextSlideName write FNextSlideName;
    property Pictures: TPresetEditorImages read FPictures;
    property PParams: TParamPainter read FPParams;
    property RinEventStarted: Boolean read FRINEventStarted;
    property ViewPresetIndex[Layer: TPresetIndex]: Integer read GetViewPresetIndex;
    property ViewPresetByLayer[Layer: TPresetIndex]: TViewPresetVis read GetViewPresetByLayer;
    property ViewRect: TRect read FViewRect;
    property WireAtMouse: TPWContainer read FWireAtMouse write FWireAtMouse;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rewire(const t: Real = 1.0; const Steps: Cardinal = 1);
    procedure AssignPreset(APreset: TPreset);
    procedure RemovePreset;
    //Unbedingt mindestens einmal aufrufen!!!
    procedure ConfirmPlugTypes;

    property MaxScrollPos: Integer read GetMaxScrollPos;
    property Rendering: Boolean read FRendering;
    property WireSettings: TPhysicsWireSettings read FWireSettings;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BackgroundBitmap: TBitmap read FBackgroundBitmap write FBackgroundBitmap;
    property BoolBoxUI: Integer read GetBoolBoxUI write SetBoolBoxUI;
    property BoolBoxUHI: Integer read GetBoolBoxUHI write SetBoolBoxUHI;
    property BoolBoxDI: Integer read GetBoolBoxDI write SetBoolBoxDI;
    property BoolBoxDHI: Integer read GetBoolBoxDHI write SetBoolBoxDHI;
    property BorderSpacing;
    property Caption;
    property Color default clBtnFace;
    property Constraints;
    property DeexpandImageIndex: Integer read FPictures.FDeexpandImageIndex write FPictures.FDeexpandImageIndex default -1;
    property DeexpandHImageIndex: Integer read FPictures.FDeexpandHImageIndex write FPictures.FDeexpandHImageIndex default -1;
    property DeexpandDImageIndex: Integer read FPictures.FDeexpandDImageIndex write FPictures.FDeexpandDImageIndex default -1;
    property Enabled;
    property ExpandImageIndex: Integer read FPictures.FExpandImageIndex write FPictures.FExpandImageIndex default -1;
    property ExpandHImageIndex: Integer read FPictures.FExpandHImageIndex write FPictures.FExpandHImageIndex default -1;
    property ExpandDImageIndex: Integer read FPictures.FExpandDImageIndex write FPictures.FExpandDImageIndex default -1;
    property ExtHighlightedPlugedImageIndex: Integer read FPictures.FExtHighlightedPlugedImageIndex write FPictures.FExtHighlightedPlugedImageIndex;
    property ExtHighlightedPlugedUndefinedImageIndex: Integer read FPictures.FExtHighlightedPlugedUndefinedImageIndex write FPictures.FExtHighlightedPlugedUndefinedImageIndex;
    property ExtHighlightedUnplugedImageIndex: Integer read FPictures.FExtHighlightedUnplugedImageIndex write FPictures.FExtHighlightedUnplugedImageIndex;
    property ExtHighlightedUnplugedUndefinedImageIndex: Integer read FPictures.FExtHighlightedUnplugedUndefinedImageIndex write FPictures.FExtHighlightedUnplugedUndefinedImageIndex;
    property Font;
    property HighlightedPlugedImageIndex: Integer read FPictures.FHighlightedPlugedImageIndex write FPictures.FHighlightedPlugedImageIndex;
    property HighlightedPlugedUndefinedImageIndex: Integer read FPictures.FHighlightedPlugedUndefinedImageIndex write FPictures.FHighlightedPlugedUndefinedImageIndex;
    property HighlightedSlidebarImageIndex: Integer read FPictures.FHighlightedSlidebarImageIndex write SetHighlightedSlidebarImageIndex default -1;
    property HighlightedUnplugedImageIndex: Integer read FPictures.FHighlightedUnplugedImageIndex write FPictures.FHighlightedUnplugedImageIndex;
    property HighlightedUnplugedUndefinedImageIndex: Integer read FPictures.FHighlightedUnplugedUndefinedImageIndex write FPictures.FHighlightedUnplugedUndefinedImageIndex;
    property HintColor: TColor read GetHintColor write SetHintColor default clDefaultParamHint;
    property Images: TImageList read FPictures.FImages write SetImages;
    property MovingCount: Cardinal read FMovingCount;
    property PixelSize: Real read FPixelSize write FPixelSize;
    property PlugedImageIndex: Integer read FPictures.FPlugedImageIndex write FPictures.FPlugedImageIndex default -1;
    property PlugedUndefinedImageIndex: Integer read FPictures.FPlugedUndefinedImageIndex write FPictures.FPlugedUndefinedImageIndex default -1;
    property PopupMenu;
    property ScrollPos: Integer read FScrollPos write SetScrollPos default 0;
    property ShowHint;
    property SlidebarImageIndex: Integer read FPictures.FSlidebarImageIndex write SetSlidebarImageIndex default -1;
    property UnplugedImageIndex: Integer read FPictures.FUnplugedImageIndex write FPictures.FUnplugedImageIndex default -1;
    property UnplugedUndefinedImageIndex: Integer read FPictures.FUnplugedUndefinedImageIndex write FPictures.FUnplugedUndefinedImageIndex default -1;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnEndRender: TNotifyEvent read FOnEndRender write FOnEndRender;
    property OnMaxScrollPosChanged: TNotifyEvent read FOnMaxScrollPosChanged write FOnMaxScrollPosChanged;
    property OnResize;
    property OnShowHint;
    property OnStartRender: TNotifyEvent read FOnStartRender write FOnStartRender;
  end;

{%REGION Tests}
  {TBla = class helper for TPresetEditor
  end;}

  {TBla = class
    &Type: Integer;
  end;}

  {generic TBla3<&Type> = array[Char] of &Type;

  generic TBla<&Type> = class

  end;

  generic TBla2<&Type> = class (specialize TBla<&Type>)

  end;

  generic TBlaProc<&Type> = function (X: &Type): &Type of object;


  procedure Test;}

{%ENDREGION}

function MouseMoveData(AX: Integer = 0; AY: Integer = 0; AShift: TShiftState = []): TMouseMoveData;
procedure DrawWire(Canvas: TCanvas; ARect: TRect; AWire: TPhysicsWire; AFirstColor,ALastColor: TColor32; APixelSize: Real);

implementation

const
  LayerWidth                     = 500;
  ParamWidth                     = 150;
  ParamEditWidth                 = 200;
  ViewPresetHeaderSpace          = 10;
  ViewPresetItemSpace            = 6;
  ViewPresetHeaderItemDifference = ViewPresetHeaderSpace-ViewPresetItemSpace;
  ViewPresetHeaderSpaceHalf      = ViewPresetHeaderSpace div 2;
  ViewPresetItemSpaceHalf        = ViewPresetItemSpace div 2;
  oUndefined                     = $FFFF{oBuffer+1};
  oUndefinedColor                = $FF888888;

  InnerCableLighten: Byte        = 24;
  //WireColorsO: array [oCall..oUndefined] of TColor32 = ($414141,$0000FF,$00D305,$FF0000,$00E6E6,$D8D822,$FF00FF,$888888);

function GetWireColor(AType: TVisOutputType): TColor32; forward;
property WireColorsO[AType: TVisOutputType]: TColor32 read GetWireColor;

{%REGION TPlugModifier}

procedure TPlugModifier.ImportFromBitmap(ABmp: TBitmap);
begin
  ABmp.PixelFormat:=pf24Bit;
  ABmp.SetSize(ParamPicWidth,ParamPicHeight);
  ABmp.Canvas.Pixels[1,1]:=clBlack;
  Move(ABmp.RawImage.Data^,FModifier,SizeOf(FModifier));
end;

function GetSDefault(BaseS: Byte; S: ShortInt): Byte;
begin
  Result:=IntCut(Integer(BaseS)+S,0,$FF);
end;

function GetSBlack(BaseS: Byte; S: ShortInt): Byte;
begin
  Result:=0;
end;

procedure TPlugModifier.ModifyPlugPic(var APic: TParamPic; AColor: TColor32; Dest: TBitmap);
const
  ATransparentColor = $963EE2; //andersrum ?!? (jetzt: BGR)
  ATransparentRGB: TBGR = (B:$96;G:$3E;R:$E2);
var
  I,J        : Integer;
  ARGB       : PBGR;
  AColorHSV  : TAHSV;
  ABlendColor: TColor32;
  AGetS      : function (BaseS: Byte; S: ShortInt): Byte;
begin
  Dest.SetSize(ParamPicWidth,ParamPicHeight);
  Dest.PixelFormat:=pf24Bit;
  Dest.Transparent:=true;
  Dest.TransparentColor:=ATransparentColor;
  AColorHSV:=ARGBToAHSV(AColor);
  if AColorHSV.S>0
    then AGetS:=@GetSDefault
    else AGetS:=@GetSBlack;
  ARGB:=Pointer(Dest.RawImage.Data);

  for J:=0 to ParamPicHeight-1 do begin
    for I:=0 to ParamPicWidth-1 do begin
      with FModifier[I,J] do if RA>=$7F
        then with APic[I,J]
          do ARGB^:=Color32ToBGR(AlphaBlendGray(A,G,AHSVToARGB(AHSV($FF,AColorHSV.H,AGetS(AColorHSV.S,S),IntCut(Integer(AColorHSV.V)+V,0,$FF)))))
        else ARGB^:=ATransparentRGB;
      Inc(ARGB);
    end;
  end;
end;

{%ENDREGION}
{%REGION TPresetControl}

constructor TPresetControl.Create(AOwner: TPresetControl; const AInitialBounds: TBoundsRect; AEditor: TPresetEditor = nil);
begin
  inherited Create;
  FOwner:=AOwner;
  if AOwner<>nil then begin
    FEditor:=AOwner.Editor;
    FOwnersControlUnderMouse:=@AOwner.FControlUnderMouse;
    FOwnersViewRect:=AOwner.ViewRectPtr;
  end else begin
    FEditor:=AEditor;
    FOwnersControlUnderMouse:=@AEditor.FControlUnderMouse;
    FOwnersViewRect:=AEditor.ViewRectPtr;
  end;
  //FAbsolutePos:=Point(0,0);
  FControlUnderMouse:=nil;
  FRepaintCtrl:=nil;
  FNextRepaintCtrl:=nil;
  FRepaintNecessary:=false;
  //set initial bounds
  FBoundsRect:=AInitialBounds;
  DoSetBounds(FBoundsRect);
  FViewRect:=FOwnersViewRect^-FBoundsRect.Rect.TopLeft;
  FAbsolutePos:=FEditor.ViewRect.TopLeft-FViewRect.TopLeft;
end;

destructor TPresetControl.Destroy;
begin
  if Highlighted then begin
    FOwnersControlUnderMouse^:=nil;
    DoMouseLeave;
  end;
  inherited Destroy;
end;

function TPresetControl.GetHeight: Integer;
begin
  Result:=FBoundsRect.Height;
end;

function TPresetControl.GetWidth: Integer;
begin
  Result:=FBoundsRect.Width;
end;

procedure TPresetControl.BoundsChanged(var ANewBounds: TBoundsRect);
begin
  DoSetBounds(ANewBounds);
  if FBoundsRect.Pos<>ANewBounds.Pos then begin
    FViewRect-=(ANewBounds.Pos-FBoundsRect.Pos);
    FBoundsRect:=ANewBounds;
    FAbsolutePos:=Editor.ViewRect.TopLeft-FViewRect.TopLeft;
    DoAssignViewRect;
  end else FBoundsRect:=ANewBounds;
  DoAlign;
end;

procedure TPresetControl.SetHeight(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Height:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TPresetControl.SetLeft(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Left:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TPresetControl.SetTop(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Top:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TPresetControl.SetWidth(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Width:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TPresetControl.SetBoundsRect(Value: TRect);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TPresetControl.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds.SetBounds(ALeft,ATop,AWidth,AHeight);
  BoundsChanged(ANewBounds);
end;

function TPresetControl.GetDirectControlUnderMouse: TPresetControl;
var
  ALastCtrl: TPresetControl;
begin
  if Highlighted then begin
    ALastCtrl:=Self;
    while ALastCtrl<>nil do begin
      Result:=ALastCtrl;
      ALastCtrl:=ALastCtrl.FControlUnderMouse;
    end;
  end else Result:=nil;
end;

procedure TPresetControl.SetFBounds(Value: TBoundsRect);
begin
  //In beiden Blöcken "FBoundsRect:=Value;", damit die Überprüfung noch mit dem
  //alten Wert von FBoundsRect durchgeführt werden kann
  if FBoundsRect.Pos<>Value.Pos then begin
    FViewRect-=(Value.Pos-FBoundsRect.Pos);
    FBoundsRect:=Value;
    FAbsolutePos:=Editor.ViewRect.TopLeft-FViewRect.TopLeft;
    DoAssignViewRect;
  end else FBoundsRect:=Value;
  if Assigned(FOnBoundsChanged) then FOnBoundsChanged(Self);
end;

function TPresetControl.Highlighted: Boolean;
begin
  Result:=(FOwnersControlUnderMouse^=Self);
end;

procedure TPresetControl.Repaint;
begin
  FEditor.Repaint(Self);
end;

procedure TPresetControl.PaintConnectedIfNecessary(ACanvas: TCanvas);
var
  ACtrl : TPresetControl;
  APCtrl: ^TPresetControl;
begin
  ACtrl:=Self;
  while ACtrl<>nil do begin
    ACtrl.PaintIfNecessary(ACanvas);
    APCtrl:=@ACtrl.FNextRepaintCtrl;
    ACtrl:=APCtrl^;
    APCtrl^:=nil;
  end;
end;

procedure TPresetControl.PaintIfNecessary(ACanvas: TCanvas);
begin
  if FRepaintNecessary then Paint(ACanvas) else begin
    FRepaintCtrl.PaintConnectedIfNecessary(ACanvas);
    FRepaintCtrl:=nil;
  end;
end;

function TPresetControl.ViewRectPtr: PRect;
begin
  Result:=@FViewRect;
end;

procedure TPresetControl.AssignViewRect;
begin
  FViewRect:=FOwnersViewRect^-FBoundsRect.Rect.TopLeft;
  FAbsolutePos:=Editor.ViewRect.TopLeft-FViewRect.TopLeft;
  DoAssignViewRect;
end;

procedure TPresetControl.Paint(ACanvas: TCanvas);
begin
  FRepaintCtrl:=nil;
  FNextRepaintCtrl:=nil;
  FRepaintNecessary:=false;
  FCanvas:=ACanvas;
  if FBoundsRect><FOwnersViewRect^ then DoPaint;

  {ACanvas.Brush.Style:=bsClear;
  ACanvas.Pen.Width:=3;
  if FBoundsRect><FOwnersViewRect^ then ACanvas.Pen.Color:=clGreen else begin
    //ACanvas.Pen.Color:=clBlue;
    //ACanvas.Rectangle(FOwnersViewRect^.Left,FOwnersViewRect^.Top,FOwnersViewRect^.Right,FOwnersViewRect^.Bottom);
    ACanvas.Pen.Color:=clRed;
  end;
  with FBoundsRect.Rect
    do ACanvas.Rectangle(Left-FOwnersViewRect^.Left,Top-FOwnersViewRect^.Top,Right-FOwnersViewRect^.Left,Bottom-FOwnersViewRect^.Top);
  ACanvas.Pen.Width:=1;
  ACanvas.Brush.Style:=bsSolid;}
end;

procedure TPresetControl.MouseLeave;
begin
  FOwnersControlUnderMouse^:=nil;
  if FControlUnderMouse<>nil then FControlUnderMouse.MouseLeave;
  DoMouseLeave;
end;

function TPresetControl.CheckMouseMove(Shift: TShiftState; X,Y: Integer): Boolean;
begin
  if Editor.MouseIsDown then exit;
  Result:=Point(X,Y)><FBoundsRect;
  if Result then begin
    if not Highlighted then begin
      if FOwnersControlUnderMouse^<>nil then FOwnersControlUnderMouse^.MouseLeave;
      FOwnersControlUnderMouse^:=Self;
      DoMouseEnter;
    end;
    DoMouseMove(Shift,X-FBoundsRect.Left,Y-FBoundsRect.Top);
  end else if Highlighted then MouseLeave;

  {if Highlighted then begin
    Canvas.Brush.Style:=bsClear;
    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=clBlue;
    with FOwnersViewRect^ do Canvas.Rectangle(-Left,-Top,Right-Left,Bottom-Top);
    Canvas.Pen.Width:=1;
    Canvas.Brush.Style:=bsSolid;
  end;}
end;

procedure TPresetControl.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if FControlUnderMouse<>nil
    then FControlUnderMouse.MouseDown(Button,Shift,X-BoundsRect.Left,Y-BoundsRect.Top)
    else DoMouseDown(Button,Shift,X-BoundsRect.Left,Y-BoundsRect.Top);
end;

procedure TPresetControl.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if FControlUnderMouse<>nil
    then FControlUnderMouse.MouseUp(Button,Shift,X-BoundsRect.Left,Y-BoundsRect.Top)
    else DoMouseUp(Button,Shift,X-BoundsRect.Left,Y-BoundsRect.Top);
  Editor.ReCheckMouseMove;
end;

procedure TPresetControl.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  //do nothing
end;

procedure TPresetControl.DoAlign;
begin
  //do nothing
end;

procedure TPresetControl.DoMouseMove(Shift: TShiftState; X,Y: Integer);
begin
  //do nothing
end;

procedure TPresetControl.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  with Editor do if WireAtMouse<>nil then begin
    WireAtMouse.Destroy;
    RemoveMouseWire;
  end;
end;

procedure TPresetControl.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //do nothing
end;

procedure TPresetControl.DoMouseEnter;
begin
  Repaint;
end;

procedure TPresetControl.DoMouseLeave;
begin
  Repaint;
end;

procedure TPresetControl.DoAssignViewRect;
begin
  //do nothing
end;

{%ENDREGION}

{%REGION TSlidebar}

constructor TSlidebar.Create(AOwner: TPresetEditor; APlugClass: TVPVSlidePlugClass; const AInitialBounds: TBoundsRect);
begin
  inherited Create(nil,AInitialBounds,AOwner);
  FOutputs:=nil;
  FPlugClass:=APlugClass;
  FPlugs:=TObjectArray.Create;
  FPlugs.OnClosed:=@OutputRemoved;
end;

destructor TSlidebar.Destroy;
begin
  FPlugs.OnClosed:=nil;
  FPlugs.Destroy;
  inherited Destroy;
end;

procedure TSlidebar.Clear;
begin
  FPlugs.OnClosed:=nil;
  FPlugs.Clear;
  FPlugs.OnClosed:=@OutputRemoved;
end;

procedure TSlidebar.AssignOutputs(var AOutputs: TNamedPresetOutputs);
var
  I,L,AY: Integer;
  APlug : TVPVSlidePlug;
  AWire : TPWContainer;
begin
  Clear;
  FOutputs:=@AOutputs;
  L:=Length(AOutputs);
  FPlugs.AddEmptyItems(L);
  AY:=0;
  for I:=0 to Length(AOutputs)-1 do begin
    AWire:=Editor.NewWire;
    with AOutputs[I].OutputInfo
      do GetPlug(Layer,Param).AssignWire(AWire);
    APlug:=FPlugClass.Create(Self,AdvCoord.BoundsRect(0,AY,0,0),AOutputs[I],I,AWire);
    //APlug.SetBounds(0,AY,0,0);
    AWire.Wire.KnotCount:=50;
    FPlugs[I]:=APlug;
    AY:=APlug.BoundsRect.Bottom;
  end;
end;

procedure TSlidebar.DoMouseMove(Shift: TShiftState; X,Y: Integer);
var
  I: Integer;
begin
  for I:=0 to FPlugs.Count-1
    do if TVPVSlidePlug(FPlugs[I]).CheckMouseMove(Shift,X,Y)
      then break;
end;

procedure TSlidebar.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //inherited DoMouseDown(...) hier auf keinen Fall aufrufen, weil dann
  //WireAtMouse gelöscht würde
  //FPlugs.Count (ohne -1), weil Plug hinter dem letzten Plug eingefügt werden soll
  if Editor.WireAtMouse<>nil
    then DropWire(FPlugs.Count);
  Repaint;
end;

procedure TSlidebar.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin

end;

procedure TSlidebar.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Width:=Editor.Pictures.TempDrawBitmap.Width;
end;

procedure TSlidebar.DoAssignViewRect;
var
  I: Integer;
begin
  for I:=0 to FPlugs.Count-1 do TVPVSlidePlug(FPlugs[I]).AssignViewRect;
end;

procedure TSlidebar.DoPaint;
var
  I: Integer;
begin
  with Editor.Pictures do if Highlighted
    then DrawSIButtonV(Canvas,ViewRect,SlidebarTopH,SlidebarH,SlidebarBottomH,0,0,Height)
    else DrawSIButtonV(Canvas,ViewRect,SlidebarTop,Slidebar,SlidebarBottom,0,0,Height);
  for I:=0 to FPlugs.Count-1
    do TVPVPlug(FPlugs[I]).Paint(Canvas);
end;

function TSlidebar.DropWire(Index: Integer): Boolean;
var
  I,AY,AY2   : Integer;
  APlug      : TVPVSlidePlug;
  APluggedEnd: TVPVPlug;
begin
  Result:=(Editor.WireAtMouse.OpenEndIsOutput=IsOutput);
  if not Result then exit;
  //Plug einstöpseln
  APluggedEnd:=GetPluggedEnd(Editor.WireAtMouse);
  FInsertOP(Index,APluggedEnd.Info,Editor.NextSlideName);

  AY:=Index*Editor.Pictures.TempDrawBitmap.Height;
  AY2:=AY+Editor.Pictures.TempDrawBitmap.Height;

  APlug:=FPlugClass.Create(Self,AdvCoord.BoundsRect(0,AY,Editor.Pictures.TempDrawBitmap.Width,AY2),FOutputs^[Index],Index,Editor.WireAtMouse);
  APluggedEnd.ConnectTo(APlug);
  APlug.AssignViewRect;
  FPlugs.InsertItem(APlug,Index);

  Editor.RemoveMouseWire;

  //Alle Outputs setzen, weil sich durch das Hinzufügen eines Elements die
  //Adresse des Arrays geändert hat
  for I:=0 to Index-1 do TVPVSlidePlug(FPlugs[I]).SetOutput(FOutputs^[I],I);
  AY:=AY2;//Index*Editor.Pictures.TempDrawBitmap.Height;
  for I:=Index+1 to FPlugs.Count-1 do begin
    with TVPVSlidePlug(FPlugs[I]) do begin
      SetOutput(FOutputs^[I],I);
      AY2:=AY+Editor.Pictures.TempDrawBitmap.Height;
      SetBounds(0,AY,Editor.Pictures.TempDrawBitmap.Width,AY2);
      AY:=AY2;
    end;
  end;
end;

procedure TSlidebar.OutputRemoved(AObject: TObjectItem; Index: Integer);
var
  I,AY,AY2   : Integer;
  APlug      : TVPVSlidePlug;
begin
  APlug:=TVPVSlidePlug(AObject);
  Editor.NextSlideName:=APlug.Name;
  FDeleteOP(Index);
  Editor.WireAtMouse:=APlug.Wire;
  if APlug.Wire<>nil then APlug.RemoveFromWire;

  //Alle Outputs setzen, weil sich durch das Löschen eines Elements die
  //Adresse des Arrays geändert hat
  for I:=0 to Index-1 do TVPVSlidePlug(FPlugs[I]).SetOutput(FOutputs^[I],I);
  AY:=Index*Editor.Pictures.TempDrawBitmap.Height;
  for I:=Index to FPlugs.Count-1 do begin
    with TVPVSlidePlug(FPlugs[I]) do begin
      SetOutput(FOutputs^[I],I);
      AY2:=AY+Editor.Pictures.TempDrawBitmap.Height;
      SetBounds(0,AY,Editor.Pictures.TempDrawBitmap.Width,AY2);
      AY:=AY2;
    end;
  end;
end;

procedure TSlidebar.DeleteOutput(Index: Integer);
begin
  FPlugs[Index].Destroy;
end;

procedure TSlidebar.SetPresetProcs(AInsert: TInsertPresetOP; ADelete: TDeletePresetOP);
begin
  FInsertOP:=AInsert;
  FDeleteOP:=ADelete;
end;

{%ENDREGION}
{%REGION TParamSlidebar}

constructor TParamSlidebar.Create(AOwner: TPresetEditor; const AInitialBounds: TBoundsRect);
begin
  inherited Create(AOwner,TVPVParamSlidePlug,AInitialBounds);
end;

procedure TParamSlidebar.AssignPreset(var APreset: TPreset);
begin
  AssignOutputs(APreset.Params);
  SetPresetProcs(@APreset.InsertParam,@APreset.DeleteParam);
end;

function TParamSlidebar.GetPluggedEnd(AWire: TPWContainer): TVPVPlug;
begin
  Result:=AWire.Param;
end;

function TParamSlidebar.GetIsOutput: Boolean;
begin
  Result:=true;
end;

function TParamSlidebar.GetPlug(Layer,Param: TPresetIndex): TVPVLayerPlug;
begin
  Result:=Editor.ViewPresetByLayer[Layer].Params[Param];
end;

{%ENDREGION}
{%REGION TOutputSlidebar}

constructor TOutputSlidebar.Create(AOwner: TPresetEditor; const AInitialBounds: TBoundsRect);
begin
  inherited Create(AOwner,TVPVOutputSlidePlug,AInitialBounds);
end;

procedure TOutputSlidebar.AssignPreset(var APreset: TPreset);
begin
  AssignOutputs(APreset.Outputs);
  SetPresetProcs(@APreset.InsertOutput,@APreset.DeleteOutput);
end;

function TOutputSlidebar.GetPluggedEnd(AWire: TPWContainer): TVPVPlug;
begin
  Result:=AWire.Output;
end;

function TOutputSlidebar.GetIsOutput: Boolean;
begin
  Result:=false;
end;

function TOutputSlidebar.GetPlug(Layer,Param: TPresetIndex): TVPVLayerPlug;
begin
  Result:=Editor.ViewPresetByLayer[Layer].Outputs[Param];
end;

{%ENDREGION}

{%REGION TVPVPlug}

constructor TVPVPlug.Create(AOwner: TPresetControl; const AInitialBounds: TBoundsRect);
begin
  inherited Create(AOwner,AInitialBounds);
  FExtHighlightCount:=0;
end;

destructor TVPVPlug.Destroy;
begin
  inherited Destroy;
end;

function TVPVPlug.GetExtHighlight: Boolean;
begin
  Result:=(FExtHighlightCount>0);
end;

procedure TVPVPlug.SetExtHighlight(Value: Boolean);
{var
  AOldValue: Boolean;}
begin
  //Fehler, wenn ein Kabel entfernt wird, deshalb erstmal auskommentiert...
  {AOldValue:=(FExtHighlightCount>0);
  if Value
    then Inc(FExtHighlightCount)
    else Dec(FExtHighlightCount);
  if AOldValue xor Value //AOldValue<>Value
    then Self.Repaint;  }
end;

procedure TVPVPlug.WireDestroyed;
begin
  Repaint;
end;

function TVPVPlug.ConnectTo(APlug: TVPVPlug): Boolean;
begin
  Result:=false;
end;

procedure TVPVPlug.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  with Editor.Pictures.TempDrawBitmap do ANewBounds.Size:=Point(Width,Height);
end;

procedure TVPVPlug.DoPaint;
begin
  Editor.Pictures.LoadImage(GetConnected,Highlighted,ExtHighlight,PlugType);
  Canvas.Draw(-ViewRect.Left,-ViewRect.Top,Editor.Pictures.TempDrawBitmap);
end;

procedure TVPVPlug.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //tue nichts, entferne also auch nicht Editor.WireAtMouse
  //(wie inherited DoMouseDown(...) es tut)
end;

{%ENDREGION}
{%REGION TVPVSlidePlug}

constructor TVPVSlidePlug.Create(AOwner: TSlidebar; const AInitialBounds: TBoundsRect; var AOutput: TNamedPresetOutput; AIndex: Integer; AWire: TPWContainer);
begin
  inherited Create(AOwner,AInitialBounds);
  FOutput:=@AOutput;
  FIndex:=AIndex;
  FWire:=AWire;
end;

destructor TVPVSlidePlug.Destroy;
begin
  if Highlighted then Editor.Hint.ClearHint;
  inherited Destroy;
end;

procedure TVPVSlidePlug.DoFree;
var
  AWire: TPWContainer;
begin
  if FWire<>nil then begin
    AWire:=FWire;
    RemoveFromWire;
    AWire.Destroy;
  end;
end;

procedure TVPVSlidePlug.SetOutput(var AOutput: TNamedPresetOutput; AIndex: Integer);
begin
  FOutput:=@AOutput;
  FIndex:=AIndex;
end;

function TVPVSlidePlug.GetConnected: Boolean;
begin
  Result:=true;
end;

function TVPVSlidePlug.GetInfo: TPresetOutputInfo;
begin
  Result:=PresetOutputInfo(FIndex,-2);
end;

function TVPVSlidePlug.GetName: string;
begin
  Result:=FOutput^.Name;
end;

procedure TVPVSlidePlug.SetName(Value: string);
begin
  FOutput^.Name:=Value;
end;

procedure TVPVSlidePlug.WireDestroyed;
var
  AOwner: TPresetControl;
begin
  AOwner:=Owner;
  FWire:=nil;
  Destroy;
  AOwner.Repaint;
end;

procedure TVPVSlidePlug.RemoveFromWire;
begin
  FWire:=nil;
end;

function TVPVSlidePlug.ConnectTo(APlug: TVPVPlug): Boolean;
begin
  Result:=(APlug.IsOutput<>IsOutput);
  if Result then FOutput^.OutputInfo:=APlug.Info;
end;

procedure TVPVSlidePlug.DoMouseLeave;
begin
  Editor.Hint.ClearHint;
  inherited DoMouseLeave;
end;

procedure TVPVSlidePlug.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Editor.WireAtMouse<>nil
    then TSlidebar(Owner).DropWire(FIndex)
    else TSlidebar(Owner).DeleteOutput(FIndex);
end;

{%ENDREGION}
{%REGION TVPVParamSlidePlug}

constructor TVPVParamSlidePlug.Create(AOwner: TSlidebar; const AInitialBounds: TBoundsRect; var AOutput: TNamedPresetOutput; AIndex: Integer; AWire: TPWContainer);
begin
  inherited Create(AOwner,AInitialBounds,AOutput,AIndex,AWire);
  //Output statt Param wg Slidebar
  AWire.Output:=Self;
  DoAssignViewRect;
end;

destructor TVPVParamSlidePlug.Destroy;
begin
  inherited Destroy;
end;

function TVPVParamSlidePlug.GetIsOutput: Boolean;
begin
  Result:=true;
end;

function TVPVParamSlidePlug.GetPlugType: TVisOutputType;
begin
  if Wire.Param<>nil
    then Result:=Wire.Param.PlugType
    else Result:=oUndefined;
end;

procedure TVPVParamSlidePlug.RemoveFromWire;
begin
  //Output statt Param wg Slidebar
  Wire.Output:=nil;
  inherited RemoveFromWire;
end;

procedure TVPVParamSlidePlug.DoMouseEnter;
begin
  if Wire.Param<>nil
    then Wire.Param.ExtHighlight:=true;
  inherited DoMouseEnter;
end;

procedure TVPVParamSlidePlug.DoMouseLeave;
begin
  if Wire.Param<>nil
    then Wire.Param.ExtHighlight:=false;
  inherited DoMouseLeave;
end;

procedure TVPVParamSlidePlug.DoMouseMove(Shift: TShiftState; X,Y: Integer);
var
  AStr     : ^string;
  AHintSize: TPoint;
begin
  with Editor do begin
    AStr:=@Output^.Name;
    AHintSize:=Hint.HintSize(AStr^);
    Hint.DrawHint(AbsolutePos.X-ViewRect.Left+10+Pictures.TempDrawBitmap.Width,AbsolutePos.Y-ViewRect.Top-((AHintSize.Y-Pictures.TempDrawBitmap.Height) div 2),AHintSize,AStr^,Classes.Rect(10,0,0,0),true);
  end;
  inherited DoMouseMove(Shift,X,Y);
end;

procedure TVPVParamSlidePlug.DoAssignViewRect;
begin
  Wire.FirstKnot:=RealPoint(AbsolutePos+Editor.Pictures.ImageCenter);
end;

{procedure TVPVParamSlidePlug.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  inherited DoSetBounds(ANewBounds);
  with Wire do FirstKnot:=FirstKnot+(ANewBounds.Rect.TopLeft-BoundsRect.TopLeft);
end;}

procedure TVPVParamSlidePlug.DoAlign;
begin
  inherited DoAlign;
  with Wire do FirstKnot:=RealPoint(AbsolutePos+Editor.Pictures.ImageCenter);
  //FirstKnot+(FBounds.Rect.TopLeft-BoundsRect.TopLeft);
end;

{%ENDREGION}
{%REGION TVPVOutputSlidePlug}

constructor TVPVOutputSlidePlug.Create(AOwner: TSlidebar; const AInitialBounds: TBoundsRect; var AOutput: TNamedPresetOutput; AIndex: Integer; AWire: TPWContainer);
begin
  inherited Create(AOwner,AInitialBounds,AOutput,AIndex,AWire);
  //Param statt Output wg Slidebar
  AWire.Param:=Self;
  DoAssignViewRect;
end;

destructor TVPVOutputSlidePlug.Destroy;
begin
  inherited Destroy;
end;

function TVPVOutputSlidePlug.GetIsOutput: Boolean;
begin
  Result:=false;
end;

function TVPVOutputSlidePlug.GetPlugType: TVisOutputType;
begin
  if Wire.Output<>nil
    then Result:=Wire.Output.PlugType
    else Result:=oUndefined;
end;

procedure TVPVOutputSlidePlug.RemoveFromWire;
begin
  //Param statt Output wg Slidebar
  Wire.Param:=nil;
  inherited RemoveFromWire;
end;

procedure TVPVOutputSlidePlug.DoMouseEnter;
begin
  if Wire.Output<>nil
    then Wire.Output.ExtHighlight:=true;
  inherited DoMouseEnter;
end;

procedure TVPVOutputSlidePlug.DoMouseLeave;
begin
  if Wire.Output<>nil
    then Wire.Output.ExtHighlight:=false;
  inherited DoMouseLeave;
end;

procedure TVPVOutputSlidePlug.DoMouseMove(Shift: TShiftState; X,Y: Integer);
var
  AStr     : ^string;
  AHintSize: TPoint;
begin
  with Editor do begin
    AStr:=@Output^.Name;
    AHintSize:=Hint.HintSize(AStr^);
    Hint.DrawHint(AbsolutePos.X-ViewRect.Left-10-AHintSize.X,AbsolutePos.Y-ViewRect.Top-((AHintSize.Y-Pictures.TempDrawBitmap.Height) div 2),AHintSize,AStr^,Classes.Rect(0,0,10,0),true);
  end;
  inherited DoMouseMove(Shift,X,Y);
end;

procedure TVPVOutputSlidePlug.DoAssignViewRect;
begin
  Wire.LastKnot:=RealPoint(AbsolutePos+Editor.Pictures.ImageCenter);
end;

{procedure TVPVOutputSlidePlug.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  inherited DoSetBounds(ANewBounds);
  with Wire do LastKnot:=LastKnot+(ANewBounds.Rect.TopLeft-BoundsRect.TopLeft);
end;}

procedure TVPVOutputSLidePlug.DoAlign;
begin
  inherited DoAlign;
  with Wire do LastKnot:=RealPoint(AbsolutePos+Editor.Pictures.ImageCenter);
end;

{%ENDREGION}
{%REGION TVPVLayerPlug}

constructor TVPVLayerPlug.Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; AIndex: TPresetIndex; AType: TVisParamType);
begin
  inherited Create(AOwner,AInitialBounds);
  FIndex:=AIndex;
  FType:=AType;
end;

function TVPVLayerPlug.GetInfo: TPresetOutputInfo;
begin
  Result:=PresetOutputInfo(TViewPresetVis(Owner).LayerIndex,FIndex);
end;

function TVPVLayerPlug.GetPlugType: TVisOutputType;
begin
  Result:=FType;
end;

procedure TVPVLayerPlug.SetName(Value: string);
begin
  //der Name eines Parameters kann nicht geändert werden, tue also nichts...
end;

{%ENDREGION}
{%REGION TVPVParamPlug}

constructor TVPVParamPlug.Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; AParamIndex: TPresetIndex; AType: TVisParamType);
begin
  inherited Create(AOwner,AInitialBounds,AParamIndex,AType);
  FWires:=TObjectList.Create;
  DoAssignViewRect;
end;

destructor TVPVParamPlug.Destroy;
var
  AItem: TObjectListItem;
  I    : Integer;
begin
  AItem:=FWires.First;
  for I:=0 to FWires.Count-1 do begin
    TPWContainer(AItem.Content).Param:=nil;
    AItem:=AItem.Next;
  end;
  FWires.Destroy;
  inherited Destroy;
end;

function TVPVParamPlug.GetConnected: Boolean;
begin
  Result:=(FWires.Count>0);
end;

function TVPVParamPlug.GetIsOutput: Boolean;
begin
  Result:=false;
end;

function TVPVParamPlug.GetName: string;
begin
  Result:='Parameter '+IntToStr(Index);
end;

procedure TVPVParamPlug.AssignWire(AWire: TPWContainer);
begin
  FWires.Add(AWire);
  AWire.Param:=Self;
end;

function TVPVParamPlug.ConnectTo(APlug: TVPVPlug): Boolean;
begin
  Result:=(APlug.IsOutput<>IsOutput);
end;

procedure TVPVParamPlug.DoAssignViewRect;
var
  AItem: TObjectListItem;
begin
  AItem:=FWires.First;
  while AItem<>nil do begin
    TPWContainer(AItem.Content).LastKnot:=RealPoint(AbsolutePos+Editor.Pictures.ImageCenter);
    AItem:=AItem.Next;
  end;
end;

procedure TVPVParamPlug.DoMouseEnter;
var
  AItem: TObjectListItem;
begin
  AItem:=FWires.First;
  while AItem<>nil do begin
    with TPWContainer(AItem.Content)
      do if Output<>nil
        then Output.ExtHighlight:=true;
    AItem:=AItem.Next;
  end;
  inherited DoMouseEnter;
end;

procedure TVPVParamPlug.DoMouseLeave;
var
  AItem: TObjectListItem;
begin
  AItem:=FWires.First;
  while AItem<>nil do begin
    with TPWContainer(AItem.Content)
      do if Output<>nil
        then Output.ExtHighlight:=false;
    AItem:=AItem.Next;
  end;
  inherited DoMouseLeave;
end;

procedure TVPVParamPlug.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  with Editor do if WireAtMouse<>nil then begin
    if WireAtMouse.OpenEndIsParam
      then if WireAtMouse.Output.ConnectTo(Self) then begin
        FWires.Add(WireAtMouse);
        WireAtMouse.Param:=Self;
        ConnectTo(WireAtMouse.Output);
        RemoveMouseWire;
      end;
  end else begin
    if FWires.Count<>1 then begin
      WireAtMouse:=Editor.NewWire;
      FWires.Add(WireAtMouse);
      WireAtMouse.Wire.KnotCount:=50;
      WireAtMouse.Param:=Self;
    end else with FWires.First do begin
      WireAtMouse:=TPWContainer(Content);
      WireAtMouse.Param:=nil;
      Destroy;
    end;
  end;
  Repaint;
end;

{%ENDREGION}
{%REGION TVPVOutputPlug}

constructor TVPVOutputPlug.Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; var AOutput: TPresetOutputInfo; AOutputIndex: TPresetIndex; AType: TVisOutputType);
begin
  inherited Create(AOwner,AInitialBounds,AOutputIndex,AType);
  FOutput:=@AOutput;
  FWire:=nil;
  DoAssignViewRect;
end;

destructor TVPVOutputPlug.Destroy;
begin
  if FWire<>nil then FWire.Output:=nil;
  inherited Destroy;
end;

procedure TVPVOutputPlug.WireDestroyed;
begin
  FWire:=nil;
  FOutput^:=poiUNDEFINED;
  inherited WireDestroyed;
end;

function TVPVOutputPlug.ConnectTo(APlug: TVPVPlug): Boolean;
begin
  Result:=(APlug.IsOutput<>IsOutput);
  if Result then FOutput^:=APlug.Info;
end;

function TVPVOutputPlug.GetConnected: Boolean;
begin
  Result:=(FWire<>nil);
end;

function TVPVOutputPlug.GetIsOutput: Boolean;
begin
  Result:=true;
end;

function TVPVOutputPlug.GetName: string;
begin
  Result:='Output '+IntToStr(Index);
end;

procedure TVPVOutputPlug.AssignWire(AWire: TPWContainer);
begin
  FWire:=AWire;
  AWire.Output:=Self;
end;

procedure TVPVOutputPlug.DoAssignViewRect;
begin
  if FWire<>nil
    then FWire.FirstKnot:=RealPoint(AbsolutePos+Editor.Pictures.ImageCenter);
end;

procedure TVPVOutputPlug.DoMouseEnter;
begin
  if FWire<>nil
    then if FWire.Param<>nil
      then FWire.Param.ExtHighlight:=true;
  inherited DoMouseEnter;
end;

procedure TVPVOutputPlug.DoMouseLeave;
begin
  if FWire<>nil
    then if FWire.Param<>nil
      then FWire.Param.ExtHighlight:=false;
  inherited DoMouseLeave;
end;

procedure TVPVOutputPlug.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  with Editor do if WireAtMouse<>nil then begin
    if (FWire=nil) and WireAtMouse.OpenEndIsOutput
      then if WireAtMouse.Param.ConnectTo(Self) then begin
        FWire:=WireAtMouse;
        WireAtMouse.Output:=Self;
        ConnectTo(WireAtMouse.Param);
        RemoveMouseWire;
      end;
  end else begin
    if FWire=nil then begin
      WireAtMouse:=Editor.NewWire;
      FWire:=WireAtMouse;
      WireAtMouse.Wire.KnotCount:=50;
      WireAtMouse.Output:=Self;
    end else begin
      WireAtMouse:=FWire;
      WireAtMouse.Output:=nil;
      WireDestroyed;
    end;
  end;
  Repaint;
end;

{%ENDREGION}
{%REGION TVPVDummyPlug}

function TVPVDummyPlug.GetConnected: Boolean;
begin
  Result:=false;
end;

function TVPVDummyPlug.GetInfo: TPresetOutputInfo;
begin
  Result:=poiUNDEFINED;
end;

function TVPVDummyPlug.GetPlugType: TVisOutputType;
begin
  Result:=oUndefined;
end;

procedure TVPVDummyPlug.SetName(Value: string);
begin
  //Dummy Plugs unterstützen das setzen des Namens nicht, tue also nichts
end;

{%ENDREGION}
{%REGION TVPVDummyOutputPlug}

function TVPVDummyOutputPlug.GetIsOutput: Boolean;
begin
  Result:=true;
end;

function TVPVDummyOutputPlug.GetName: string;
begin
  Result:='Outputs';
end;

{%ENDREGION}
{%REGION TVPVDummyParamPlug}

function TVPVDummyParamPlug.GetIsOutput: Boolean;
begin
  Result:=false;
end;

function TVPVDummyParamPlug.GetName: string;
begin
  Result:='Parameter';
end;

{%ENDREGION}

{%REGION TPWContainer}

constructor TPWContainer.Create(AEditor: TPresetEditor; AWireSettings: TPhysicsWireSettings);
begin
  inherited Create;
  FEditor:=AEditor;
  FParam:=nil;
  FOutput:=nil;
  FWire:=TPhysicsWire.Create(ZEROCENTER,ZEROCENTER,AWireSettings);
  //FType:=oUndefined;
  FParamColor:=WireColorsO[oUndefined];
  FOutputColor:=WireColorsO[oUndefined];
end;

destructor TPWContainer.Destroy;
begin
  if FParam<>nil then FParam.WireDestroyed;
  if FOutput<>nil then FOutput.WireDestroyed;
  FWire.Destroy;
  inherited Destroy;
end;

procedure TPWContainer.Draw(Canvas: TCanvas; ARect: TRect);
begin
  DrawWire(Canvas,ARect,FWire,FOutputColor,FParamColor,FEditor.PixelSize);
end;

procedure TPWContainer.DetermineFType;
begin
  if FOutput<>nil then begin
    FOutputColor:=WireColorsO[FOutput.PlugType];
    if FParam<>nil
      then FParamColor:=WireColorsO[FParam.PlugType]
      else FParamColor:=FOutputColor;
  end else begin
    if FParam<>nil
      then FParamColor:=WireColorsO[FParam.PlugType]
      else FParamColor:=WireColorsO[oUndefined];
    FOutputColor:=FParamColor;
  end;
end;

procedure TPWContainer.SetParam(Value: TVPVPlug);
begin
  FParam:=Value;
  if Value<>nil then with Value
    do LastKnot:=RealPoint(Value.AbsolutePos+Editor.Pictures.ImageCenter);
  DetermineFType;
end;

procedure TPWContainer.SetOutput(Value: TVPVPlug);
begin
  FOutput:=Value;
  if Value<>nil then with Value
    do FirstKnot:=RealPoint(Value.AbsolutePos+Editor.Pictures.ImageCenter);
  DetermineFType;
end;

function TPWContainer.GetOpenEndIsOutput: Boolean;
begin
  Result:=(FOutput=nil);
end;

function TPWContainer.GetOpenEndIsParam: Boolean;
begin
  Result:=(FParam=nil);
end;

function TPWContainer.GetFirstKnot: TRealPoint;
begin
  Result:=FWire.FirstKnot/FEditor.PixelSize;
end;

procedure TPWContainer.SetFirstKnot(Value: TRealPoint);
begin
  FWire.FirstKnot:=Value*FEditor.PixelSize;
  FEditor.StartRender;
end;

function TPWContainer.GetLastKnot: TRealPoint;
begin
  Result:=FWire.LastKnot/FEditor.PixelSize;
end;

procedure TPWContainer.SetLastKnot(Value: TRealPoint);
begin
  FWire.LastKnot:=Value*FEditor.PixelSize;
  FEditor.StartRender;
end;

function TPWContainer.GetOpenEndPos: TRealPoint;
begin
  if OpenEndIsOutput
    then Result:=FirstKnot
    else Result:=LastKnot;
end;

procedure TPWContainer.SetOpenEndPos(Value: TRealPoint);
begin
  if OpenEndIsOutput
    then FirstKnot:=Value
    else LastKnot:=Value;
end;

{%ENDREGION}
{%REGION TExpanderButton}

constructor TExpanderButton.Create(AOwner: TViewPresetVis; const AInitialBounds: TBoundsRect; AExpanded: Boolean = true);
begin
  inherited Create(AOwner,AInitialBounds);
  FExpanded:=AExpanded;
end;

destructor TExpanderButton.Destroy;
begin
  inherited Destroy;
end;

procedure TExpanderButton.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //inherited DoMouseDown(...) hier auf keinen Fall aufrufen, weil dann
  //WireAtMouse gelöscht würde
  Repaint;
end;

procedure TExpanderButton.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  FExpanded:=not FExpanded;
  Repaint;
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TExpanderButton.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  with Editor.Pictures.TempDrawBitmap do ANewBounds.Size:=Point(Width,Height);
end;

procedure TExpanderButton.DoPaint;
begin
  with Editor.Pictures do begin
    if FExpanded then begin
      if not Highlighted
        then LoadImage(DeexpandImageIndex)
        else if Editor.MouseIsDown
          then LoadImage(DeexpandDImageIndex)
          else LoadImage(DeexpandHImageIndex);
    end else begin
      if not Highlighted
        then LoadImage(ExpandImageIndex)
        else if Editor.MouseIsDown
          then LoadImage(ExpandDImageIndex)
          else LoadImage(ExpandHImageIndex);
    end;
    Canvas.Draw(-ViewRect.Left,-ViewRect.Top,TempDrawBitmap);
  end;
end;

procedure TExpanderButton.SetExpanded(Value: Boolean);
begin
  FExpanded:=Value;
  Repaint;
end;

{%ENDREGION}
{%REGION TViewPresetVis}

constructor TViewPresetVis.Create(AOwner: TPresetEditor; const AInitialBounds: TBoundsRect; var ALayer: TPresetVis; ALayerIndex: TPresetIndex);
var
  I,PL,OL,AX,AY              : Integer;
  AParam                     : TVisParam;
  AVisType                   : TVisType;
  AParamEdit                 : TPPParam;
  AParamDesc                 : TSVisParamDesc;
  AOutputDesc                : TSVisOutputDesc;
  AParamPlug                 : TVPVParamPlug;
  AOutputPlug                : TVPVOutputPlug;
  //ASingleHeight,AHeaderHeight: Integer;
begin
  inherited Create(nil,AInitialBounds,AOwner);
  FLayer:=@ALayer;
  FLayerIndex:=ALayerIndex;
  FVisType:=TVisualisation.VisPtr(ALayer.VisID);
  with FVisType^ do begin
    PL:=Length(VisParamDesc)+3;
    OL:=Length(OutputDesc);
  end;
  SetLength(FParams,PL);
  SetLength(FParamEdits,PL);
  SetLength(FOutputs,OL);
  AVisType:=TVisualisation.Visualisations(ALayer.VisID);

  //ASingleHeight:=Editor.Pictures.TempDrawBitmap.Height+6;
  //AHeaderHeight:=Editor.Pictures.TempDrawBitmap.Height+10;

  FMainParam:=TVPVParamPlug.Create(Self,AdvCoord.BoundsRect(5,5,0,0),-1,vCall);
  //FMainParam.SetBounds(5,5,0,0);
  AY:=FMainParam.BoundsRect.Bottom+8;
  //C1
  AParamPlug:=TVPVParamPlug.Create(Self,AdvCoord.BoundsRect(5,AY,0,0),0,vColor);
  //AParamPlug.SetBounds(5,AY,0,0);

  FParams[0]:=AParamPlug;
  AParam:=NextParamPosCCC(ALayer,0,AParam);
  AParamEdit:=Editor.PParams.Add(AParam,AVisType.C1Desc,vColor,0,0);
  AParamEdit.Visible:=ALayer.Expanded;
  FParamEdits[0]:=AParamEdit;

  AY:=AParamPlug.BoundsRect.Bottom+6;
  //C2
  AParamPlug:=TVPVParamPlug.Create(Self,AdvCoord.BoundsRect(5,AY,0,0),1,vColor);
  //AParamPlug.SetBounds(5,AY,0,0);

  FParams[1]:=AParamPlug;
  AParam:=NextParamPosCCC(ALayer,1,AParam);
  AParamEdit:=Editor.PParams.Add(AParam,AVisType.C2Desc,vColor,0,0);
  AParamEdit.Visible:=ALayer.Expanded;
  FParamEdits[1]:=AParamEdit;

  AY:=AParamPlug.BoundsRect.Bottom+6;
  //C3
  AParamPlug:=TVPVParamPlug.Create(Self,AdvCoord.BoundsRect(5,AY,0,0),2,vColor);
  //AParamPlug.SetBounds(5,AY,0,0);

  FParams[2]:=AParamPlug;
  AParam:=NextParamPosCCC(ALayer,2,AParam);
  AParamEdit:=Editor.PParams.Add(AParam,AVisType.C3Desc,vColor,0,0);
  AParamEdit.Visible:=ALayer.Expanded;
  FParamEdits[2]:=AParamEdit;

  AY:=AParamPlug.BoundsRect.Bottom+6;

  for I:=3 to PL-1 do begin
    AParamDesc:=AVisType.VisParamDesc[I-3];
    AParamPlug:=TVPVParamPlug.Create(Self,AdvCoord.BoundsRect(5,AY,0,0),I,AParamDesc.AType);
    //AParamPlug.Left:=ViewPreset;
    //AParamPlug.SetBounds(5,AY,0,0);

    FParams[I]:=AParamPlug;
    AParam:=NextParamPosCCC(ALayer,I,AParam);
    AParamEdit:=Editor.PParams.Add(AParam,AParamDesc.Name,AParamDesc.AType,0,0);
    AParamEdit.Visible:=ALayer.Expanded;
    FParamEdits[I]:=AParamEdit;

    AY:=AParamPlug.BoundsRect.Bottom+6;
  end;

  //OutputPlug.SetBounds wird beim größe verändern durchgeführt... (***)
  AX:=FBounds.Width-Editor.Pictures.TempDrawBitmap.Width-5;
  AY:=FMainParam.BoundsRect.Bottom+8;
  for I:=0 to OL-1 do begin
    AOutputDesc:=AVisType.OutputDesc[I];
    AOutputPlug:=TVPVOutputPlug.Create(Self,AdvCoord.BoundsRect(AX,AY,0,0),ALayer.VisOutputs[I],I,AOutputDesc.AType);
    FOutputs[I]:=AOutputPlug;
    AY:=AOutputPlug.BoundsRect.Bottom+6;
  end;

  FDummyOutputPlug:=TVPVDummyOutputPlug.Create(Self,AdvCoord.BoundsRect(AX,5,0,0));
  FDummyParamPlug:=TVPVDummyParamPlug.Create(Self,AdvCoord.BoundsRect(5,5,0,0));
  //FDummyParamPlug.SetBounds(5,5,0,0);

  DoSetExpanded;

  FExpanderBtn:=TExpanderButton.Create(Self,AdvCoord.BoundsRect(AX-Editor.Pictures.TempDrawBitmap.Width-5,5,0,0),ALayer.Expanded);
  FExpanderBtn.OnClick:=@ExpanderBtnClick;
  DoAssignViewRect;
end;

destructor TViewPresetVis.Destroy;
var
  I    : Integer;
begin
  FExpanderBtn.Destroy;
  FDummyParamPlug.Destroy;
  FDummyOutputPlug.Destroy;
  for I:=0 to Length(FOutputs)-1 do FOutputs[I].Destroy;
  SetLength(FOutputs,0);
  for I:=0 to Length(FParams)-1 do begin
    FParams[I].Destroy;
    FParamEdits[I].Destroy;
  end;
  SetLength(FParams,0);
  FMainParam.Destroy;
  inherited Destroy;
end;

procedure TViewPresetVis.InitLayer;
var
  I       : Integer;
  AWire   : TPWContainer;
  //AVisType: TVisType;
begin
  with FLayer^ do begin
    //AVisType:=TVisualisation.Visualisations(FLayer^.VisID);
    for I:=0 to Length(VisOutputs)-1 do with VisOutputs[I] do if Param>=-1 then begin
      AWire:=Editor.NewWire;
      FOutputs[I].AssignWire(AWire);
      {with AWire do begin
        //Wire.FirstKnot:=RealPoint(FPosition.Right-5-FOwner.Pictures.FImageCenter.X,FPosition.Top+HeaderHeight+(I*SingleHeight)+3+FOwner.Pictures.FImageCenter.Y);
        //AType:=AVisType.OutputDesc[I].AType;
        //FOutput.Param:=I;
        //FOutput.Layer:=FLayerIndex;
      end;}
      Editor.ViewPresetByLayer[Layer].Params[Param].AssignWire(AWire);
      //FOwner.ViewPresetByLayer[Layer].AddParamCon(AWire,Param);
      AWire.Wire.KnotCount:=50;
    end;
  end;
end;

procedure TViewPresetVis.DoSetBounds(var ANewBounds: TBoundsRect);
{var
  I,AX: Integer;
  APlug  : TVPVLayerPlug;}
begin
  ANewBounds.Height:=Height;
  {AX:=ANewBounds.Width-Editor.Pictures.TempDrawBitmap.Width-5;
  //AY:=Editor.Pictures.TempDrawBitmap.Height+13;
  for I:=0 to Length(FOutputs)-1 do begin
    APlug:=FOutputs[I];
    //APlug.SetBounds(AX,AY,0,0);
    APlug.Left:=AX;
    //AY:=APlug.BoundsRect.Bottom+6;
  end;
  FDummyOutputPlug.SetBounds(AX,5,0,0);
  FExpanderBtn.SetBounds(ANewBounds.Width-Editor.Pictures.TempDrawBitmap.Width*2-10,5,0,0);}
end;

procedure TViewPresetVis.DoAlign;
var
  I,AX : Integer;
  APlug: TVPVLayerPlug;
begin
  AX:=Width-Editor.Pictures.TempDrawBitmap.Width-5;
  //AY:=Editor.Pictures.TempDrawBitmap.Height+13;
  for I:=0 to Length(FOutputs)-1 do begin
    APlug:=FOutputs[I];
    //APlug.SetBounds(AX,AY,0,0);
    APlug.Left:=AX;
    //AY:=APlug.BoundsRect.Bottom+6;
  end;
  FDummyOutputPlug.SetBounds(AX,5,0,0);
  FExpanderBtn.SetBounds(AX-Editor.Pictures.TempDrawBitmap.Width-5,5,0,0);
end;

procedure TViewPresetVis.DoAssignViewRect;
var
  AX,AY,I: Integer;
begin
  FExpanderBtn.AssignViewRect{(ViewRect)};
  FMainParam.AssignViewRect{(ViewRect)};
  for I:=0 to Length(FParams)-1 do FParams[I].AssignViewRect{(ViewRect)};
  for I:=0 to Length(FOutputs)-1 do FOutputs[I].AssignViewRect{(ViewRect)};
  FDummyParamPlug.AssignViewRect{(ViewRect)};
  FDummyOutputPlug.AssignViewRect{(ViewRect)};

  AX:=AbsolutePos.X+ParamWidth;
  AY:=Editor.Pictures.TempDrawBitmap.Height+13+AbsolutePos.Y;
  for I:=0 to Length(FParamEdits)-1 do with FParamEdits[I] do begin
    Left:=AX;
    Top:=AY;
    AY+=Editor.Pictures.TempDrawBitmap.Height+6
  end;
end;

procedure TViewPresetVis.DoPaint_Expanded;
var
  I,{LP,LO,}ASingleTextStart,ASingleHeight,AHeaderHeight: Integer;
begin
  with FVisType^ do begin
    //LP:=Length(VisParamDesc)+3;
    //LO:=Length(OutputDesc);

    Canvas.Font.Style:=[];
    ASingleHeight:=Editor.Pictures.TempDrawBitmap.Height+6;
    AHeaderHeight:=Editor.Pictures.TempDrawBitmap.Height+10;
    ASingleTextStart:=(ASingleHeight-Canvas.TextHeight('Wg')) div 2;

    FMainParam.Paint(Canvas);

    for I:=0 to Length(FParams)-1 do with FParams[I] do begin
      Paint(Self.Canvas);
      Self.Canvas.TextOut(Left+Width+10-Self.ViewRect.Left,Top+ASingleTextStart-Self.ViewRect.Top,ParamDesc[I].Name);
    end;

    for I:=0 to Length(FOutputs)-1 do with FOutputs[I] do begin
      Paint(Self.Canvas);
      Self.Canvas.TextOut(Left-10-Self.Canvas.TextWidth(OutputDesc[I].Name)-Self.ViewRect.Left,Top+ASingleTextStart-Self.ViewRect.Top,OutputDesc[I].Name);
    end;

    for I:=0 to Length(FParamEdits)-1 do FParamEdits[I].Paint(Editor.ViewRect);
  end;
end;

procedure TViewPresetVis.DoPaint_Not_Expanded;
begin
  with FVisType^ do begin
    FDummyParamPlug.Paint(Canvas);
    FDummyOutputPlug.Paint(Canvas);
    Canvas.Font.Style:=[];
  end;
end;

procedure TViewPresetVis.DoPaint;
begin
  Canvas.Pen.Width:=3;
  Canvas.Pen.Color:=$555555;
  Canvas.Brush.Color:=$999999;
  Canvas.RoundRect(-ViewRect.Left,-ViewRect.Top,Width-ViewRect.Left,Height-ViewRect.Top,30,30);

  Canvas.Font.Color:=clBlack;
  Canvas.Font.Style:=[fsBold];
  with FVisType^ do Canvas.TextOut(((Width-Canvas.TextWidth(Name)) div 2)-ViewRect.Left,3-ViewRect.Top,Name);
  FExpanderBtn.Paint(Canvas);
  FDoPaint;
end;

procedure TViewPresetVis.DoMouseMove_Not_Expanded(Shift: TShiftState; X,Y: Integer);
begin
  if not FDummyParamPlug.CheckMouseMove(Shift,X,Y)
    then FDummyOutputPlug.CheckMouseMove(Shift,X,Y);
end;

procedure TViewPresetVis.DoMouseMove_Expanded(Shift: TShiftState; X,Y: Integer);
var
  I: Integer;
begin
  if FMainParam.CheckMouseMove(Shift,X,Y) then exit;
  for I:=0 to Length(FParams)-1
    do if FParams[I].CheckMouseMove(Shift,X,Y)
      then exit;
  for I:=0 to Length(FOutputs)-1
    do if FOutputs[I].CheckMouseMove(Shift,X,Y)
      then exit;
end;

procedure TViewPresetVis.DoMouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if FExpanderBtn.CheckMouseMove(Shift,X,Y) then exit;
  FDoMouseMove(Shift,X,Y);
end;

procedure TViewPresetVis.DoMouseEnter;
begin
  //Repaint; (***, weil Repaint nicht nötig wäre, wenn sich die Hintergrundfarbe nicht ändert*) //do nothing
end;

procedure TViewPresetVis.DoMouseLeave;
begin
  //Repaint; (***, weil Repaint nicht nötig wäre, wenn sich die Hintergrundfarbe nicht ändert*) //do nothing
end;

function TViewPresetVis.GetExpanded: Boolean;
begin
  Result:=FLayer^.Expanded;
end;

procedure TViewPresetVis.ExpanderBtnClick(Sender: TObject);
begin
  SetExpanded(FExpanderBtn.Expanded);
  Repaint;
end;

procedure TViewPresetVis.DoSetExpanded;
var
  AExpanded: Boolean;

  procedure SetPlugPos_Expanded; inline;
  var
    AY,I : Integer;
    APlug: TVPVLayerPlug;
  begin
    AY:=Editor.Pictures.ViewPresetHeaderHeight+ViewPresetItemSpaceHalf;
    for I:=0 to Length(FParams)-1 do begin
      APlug:=FParams[I];
      APlug.Top:=AY;
      AY:=APlug.BoundsRect.Bottom+ViewPresetItemSpace;
    end;
    AY:=Editor.Pictures.ViewPresetHeaderHeight+ViewPresetItemSpaceHalf;
    for I:=0 to Length(FOutputs)-1 do begin
      APlug:=FOutputs[I];
      APlug.Top:=AY;
      AY:=APlug.BoundsRect.Bottom+ViewPresetItemSpace;
    end;
  end;

var
  I,PL,OL: Integer;
begin
  AExpanded:=FLayer^.Expanded;
  for I:=0 to Length(FParamEdits)-1 do FParamEdits[I].Visible:=AExpanded;
  if AExpanded then begin
    FDoPaint:=@DoPaint_Expanded;
    FDoMouseMove:=@DoMouseMove_Expanded;
    PL:=Length(FParams);
    OL:=Length(FOutputs);
    with Editor.Pictures do if PL>=OL
      then FBounds.Height:=ViewPresetHeaderHeight+PL*ViewPresetItemHeight
      else FBounds.Height:=ViewPresetHeaderHeight+OL*ViewPresetItemHeight;
    SetPlugPos_Expanded;
  end else begin
    FDoPaint:=@DoPaint_Not_Expanded;
    FDoMouseMove:=@DoMouseMove_Not_Expanded;

    FBounds.Height:=Editor.Pictures.ViewPresetHeaderHeight;
    for I:=0 to Length(FParams)-1 do FParams[I].Top:=ViewPresetHeaderSpaceHalf;
    for I:=0 to Length(FOutputs)-1 do FOutputs[I].Top:=ViewPresetHeaderSpaceHalf;
  end;
  //sinnvoll!!!!!
  FBounds:=FBounds;
end;

procedure TViewPresetVis.SetExpanded(Value: Boolean);
begin
  if Value=FLayer^.Expanded then exit;
  FLayer^.Expanded:=Value;
  DoSetExpanded;
end;

function TViewPresetVis.GetParam(Index: TPresetIndex): TVPVParamPlug;
begin
  if Index>=0
    then Result:=FParams[Index]
    else Result:=FMainParam;
end;

function TViewPresetVis.GetOutput(Index: TPresetIndex): TVPVOutputPlug;
begin
  Result:=FOutputs[Index];
end;

{%ENDREGION}
{%REGION TPresetEditorImages}

procedure TPresetEditorImages.Produce;
begin
  FSlidebar:=TBitmap.Create;
  FSlidebarTop:=TBitmap.Create;
  FSlidebarBottom:=TBitmap.Create;
  FSlidebarH:=TBitmap.Create;
  FSlidebarTopH:=TBitmap.Create;
  FSlidebarBottomH:=TBitmap.Create;
  FTempDrawBitmap:=TBitmap.Create;
  SetImages(nil);
  FExpandImageIndex:=-1;
  FExpandHImageIndex:=-1;
  FExpandDImageIndex:=-1;
  FDeexpandImageIndex:=-1;
  FDeexpandHImageIndex:=-1;
  FDeexpandDImageIndex:=-1;
  FUnplugedImageIndex:=-1;
  FUnplugedUndefinedImageIndex:=-1;
  FPlugedImageIndex:=-1;
  FPlugedUndefinedImageIndex:=-1;
  FHighlightedUnplugedImageIndex:=-1;
  FHighlightedUnplugedUndefinedImageIndex:=-1;
  FExtHighlightedUnplugedImageIndex:=-1;
  FExtHighlightedUnplugedUndefinedImageIndex:=-1;
  FHighlightedPlugedImageIndex:=-1;
  FHighlightedPlugedUndefinedImageIndex:=-1;
  FExtHighlightedPlugedImageIndex:=-1;
  FExtHighlightedPlugedUndefinedImageIndex:=-1;
end;

procedure TPresetEditorImages.Destroy;
begin
  FreePlugBmps;
  FTempDrawBitmap.Destroy;
  FSlidebarBottom.Destroy;
  FSlidebarTop.Destroy;
  FSlidebar.Destroy;
  FSlidebarBottomH.Destroy;
  FSlidebarTopH.Destroy;
  FSlidebarH.Destroy;
end;

procedure TPresetEditorImages.InitPlugBmps;
var
  I    : Integer;
  AMods: specialize TPlugStateArray<TPlugModifier>;
  ABmp : TBitmap;
begin
  ABmp:=TBitmap.Create;
  ABmp.PixelFormat:=pf24Bit;
  ABmp.SetSize(ParamPicWidth,ParamPicHeight);

  LoadImage(FUnplugedImageIndex);
  ABmp.Canvas.Draw(0,0,FTempDrawBitmap);
  AMods[false].Normal[false].ImportFromBitmap(ABmp);

  LoadImage(FHighlightedUnplugedImageIndex);
  ABmp.Canvas.Draw(0,0,FTempDrawBitmap);
  AMods[false].Normal[true].ImportFromBitmap(ABmp);

  LoadImage(FExtHighlightedUnplugedImageIndex);
  ABmp.Canvas.Draw(0,0,FTempDrawBitmap);
  AMods[false].ExtHighlight.ImportFromBitmap(ABmp);

  LoadImage(FPlugedImageIndex);
  ABmp.Canvas.Draw(0,0,FTempDrawBitmap);
  AMods[true].Normal[false].ImportFromBitmap(ABmp);

  LoadImage(FHighlightedPlugedImageIndex);
  ABmp.Canvas.Draw(0,0,FTempDrawBitmap);
  AMods[true].Normal[true].ImportFromBitmap(ABmp);

  LoadImage(FExtHighlightedPlugedImageIndex);
  ABmp.Canvas.Draw(0,0,FTempDrawBitmap);
  AMods[true].ExtHighlight.ImportFromBitmap(ABmp);

  ABmp.Destroy;

  SetLength(FPlugBmps,VisParamTypes.Count);
  for I:=0 to VisParamTypes.Count-1 do if VisParamTypes.ParamTypeExists(I) then begin
    with FPlugBmps[I][false] do begin
      Normal[false]:=TBitmap.Create;
      Normal[true]:=TBitmap.Create;
      ExtHighlight:=TBitmap.Create;
    end;
    with FPlugBmps[I][true] do begin
      Normal[false]:=TBitmap.Create;
      Normal[true]:=TBitmap.Create;
      ExtHighlight:=TBitmap.Create;
    end;
    with VisParamTypes.ParamTypePtr[I]^ do begin;
      with FPlugBmps[I][false] do begin
        AMods[false].Normal[false].ModifyPlugPic(Picture,Color,Normal[false]);
        AMods[false].Normal[true].ModifyPlugPic(Picture,Color,Normal[true]);
        AMods[false].ExtHighlight.ModifyPlugPic(Picture,Color,ExtHighlight);
      end;
      with FPlugBmps[I][true] do begin
        AMods[true].Normal[false].ModifyPlugPic(Picture,Color,Normal[false]);
        AMods[true].Normal[true].ModifyPlugPic(Picture,Color,Normal[true]);
        AMods[true].ExtHighlight.ModifyPlugPic(Picture,Color,ExtHighlight);
      end;
      {AMods[false,false].ModifyPlugPic(Picture,Color,FPlugBmps[I][false][false]);
      AMods[true,false].ModifyPlugPic(Picture,Color,FPlugBmps[I][true][false]);
      AMods[false,true].ModifyPlugPic(Picture,Color,FPlugBmps[I][false][true]);
      AMods[true,true].ModifyPlugPic(Picture,Color,FPlugBmps[I][true][true]);}
    end;
  end else begin
    with FPlugBmps[I][false] do begin
      Normal[false]:=nil;
      Normal[true]:=nil;
      ExtHighlight:=nil;
    end;
    with FPlugBmps[I][true] do begin
      Normal[false]:=nil;
      Normal[true]:=nil;
      ExtHighlight:=nil;
    end;
  end;
end;

procedure TPresetEditorImages.FreePlugBmps;
var
  I: Integer;
begin
  for I:=0 to Length(FPlugBmps)-1 do if FPlugBmps[I][false].Normal[false]<>nil then begin
    with FPlugBmps[I][false] do begin
      Normal[false].Destroy;
      Normal[true].Destroy;
      ExtHighlight.Destroy;
      Normal[false]:=nil;
      Normal[true]:=nil;
      ExtHighlight:=nil;
    end;
    with FPlugBmps[I][true] do begin
      Normal[false].Destroy;
      Normal[true].Destroy;
      ExtHighlight.Destroy;
      Normal[false]:=nil;
      Normal[true]:=nil;
      ExtHighlight:=nil;
    end;
    {FPlugBmps[I][false][false].Destroy;
    FPlugBmps[I][true][false].Destroy;
    FPlugBmps[I][false][true].Destroy;
    FPlugBmps[I][true][true].Destroy;
    FPlugBmps[I][false][false]:=nil;
    FPlugBmps[I][true][false]:=nil;
    FPlugBmps[I][false][true]:=nil;
    FPlugBmps[I][true][true]:=nil;}
  end;
  SetLength(FPlugBmps,0);
end;

procedure TPresetEditorImages.SetImages(Value: TImageList);
begin
  FImages:=Value;
  //FreePlugBmps;
  if Value<>nil then begin
    FTempDrawBitmap.SetSize(Value.Width,Value.Height);
    FImageCenter:=Classes.Point(Value.Width div 2,Value.Height div 2);
    //InitPlugBmps;
  end else begin
    FTempDrawBitmap.SetSize(10,10);
    FImageCenter:=Classes.Point(5,5);
  end;
  FViewPresetItemHeight:=FTempDrawBitmap.Height+ViewPresetItemSpace;
  FViewPresetHeaderHeight:=FViewPresetItemHeight+ViewPresetHeaderItemDifference;
  SetSlidebarImageIndex(-1);
  SetHighlightedSlidebarImageIndex(-1);
  FImageDestBmp:=FTempDrawBitmap;
end;

procedure TPresetEditorImages.SetSlidebarImageIndex(Value: Integer);
begin
  if FImages=nil then Value:=-1;
  FSlidebarImageIndex:=Value;
  if FSlidebarImageIndex>=0
    then FImages.GetBitmap(Value,FTempDrawBitmap)
    else FTempDrawBitmap.Canvas.Rectangle(0,0,FTempDrawBitmap.Width-1,FTempDrawBitmap.Height-1);
  SplitImageV(FTempDrawBitmap,FSlidebarTop,FSlidebar,FSlidebarBottom);
end;

procedure TPresetEditorImages.SetHighlightedSlidebarImageIndex(Value: Integer);
begin
  if FImages=nil then Value:=-1;
  FHighlightedSlidebarImageIndex:=Value;
  if FHighlightedSlidebarImageIndex>=0
    then FImages.GetBitmap(Value,FTempDrawBitmap)
    else FTempDrawBitmap.Canvas.Rectangle(0,0,FTempDrawBitmap.Width-1,FTempDrawBitmap.Height-1);
  SplitImageV(FTempDrawBitmap,FSlidebarTopH,FSlidebarH,FSlidebarBottomH);
end;

procedure TPresetEditorImages.LoadImage(Index: Integer);
begin
  if Index>=0 then FImages.GetBitmap(Index,FTempDrawBitmap) else begin
    with FTempDrawBitmap do begin
      Canvas.Pen.Color:=clGray;
      Canvas.Brush.Color:=clSilver;
      Canvas.Rectangle(0,0,Width,Height);
    end;
  end;
  FImageDestBmp:=FTempDrawBitmap;
end;

procedure TPresetEditorImages.LoadImage(Pluged,Highlighted,ExtHighlight: Boolean; AType: TVisOutputType);
var
  AIndex: Integer;
begin
  if AType>=VisParamTypes.Count then begin
    if Pluged then begin
      if ExtHighlight
        then AIndex:=FExtHighlightedPlugedUndefinedImageIndex
        else if Highlighted
          then AIndex:=FHighlightedPlugedUndefinedImageIndex
          else AIndex:=FPlugedUndefinedImageIndex;
    end else begin
      if ExtHighlight
        then AIndex:=FExtHighlightedUnplugedUndefinedImageIndex
        else if Highlighted
          then AIndex:=FHighlightedUnplugedUndefinedImageIndex
          else AIndex:=FUnplugedUndefinedImageIndex;
    end;
    FImages.GetBitmap(AIndex,FTempDrawBitmap);
    FImageDestBmp:=FTempDrawBitmap;
  end else begin
    if ExtHighlight
      then FImageDestBmp:=FPlugBmps[AType][Pluged].ExtHighlight
      else FImageDestBmp:=FPlugBmps[AType][Pluged].Normal[Highlighted];
  end;
end;

{%ENDREGION}
{%REGION TPresetEditor}

{%REGION Create & Destroy}

constructor TPresetEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocked:=true;
  FControlUnderMouse:=nil;
  FRendering:=false;
  FViewPreset:=TObjectArray.Create;
  FPreset:=nil;
  Color:=clBtnFace;
  TabStop:=true;
  FBottom:=0;
  FBackgroundBitmap:=nil;
  FPaintBuffer:=TBitmap.Create;
  FPaintBufferWithWires:=TBitmap.Create;
  FPictures.Produce;
  FPParams:=TParamPainter.Create(Self,FPaintBuffer.Canvas,@GetParamVal,@SetParamVal,0,0{,false});
  FPParams.OnRequestBackground:=@ParamRequestBG;
  FLayerPosX:=0;
  FScrollPos:=0;
  FTriggerStartIndex:=0;
  FMouseIsDown:=false;
  FHint:=TGraphicsHint.Create(Self,FPaintBufferWithWires.Canvas);
  FPixelSize:=1.0;
  FWireSettings:=TPhysicsWireSettings.Create;
  FWires:=TObjectList.Create(true);
  FMovingCount:=0;
  FMaxScrollPos:=0;
  FMouseMoveData.Shift:=[];
  FMouseMoveData.Pos:=ZEROPOINT;
  RemoveMouseWire;

  FPParams.OnPaint:=@ParamPainted;
  FHint.OnHide:=@FHintDrawHide;
  FHint.OnDraw:=@FHintDrawHide;
  //Coords werden später in CalcMaxScrollPos und CreateViewList gesetzt
  FParamSlidebar:=TParamSlidebar.Create(Self,AdvCoord.BoundsRect(0,0,0,0));
  FOutputSlidebar:=TOutputSlidebar.Create(Self,AdvCoord.BoundsRect(0,0,0,0));
  FLocked:=false;
  FRepaintCtrl:=nil;
  FRINEventStarted:=false;
  FRepaintNecessary:=false;
  FRenderNecessary:=false;
end;

destructor TPresetEditor.Destroy;
begin
  FRendering:=false;
  FOutputSlidebar.Destroy;
  FParamSlidebar.Destroy;
  FViewPreset.Destroy;
  FWires.Destroy;
  FWireSettings.Destroy;
  FPParams.Destroy;
  FPictures.Destroy;
  FHint.Destroy;
  FPaintBuffer.Destroy;
  FPaintBufferWithWires.Destroy;
  inherited Destroy;
end;

procedure TPresetEditor.AssignPreset(APreset: TPreset);
begin
  FPreset:=APreset;
  CreateViewList;
end;

procedure TPresetEditor.RemovePreset;
begin
  FPreset:=nil;
  Paint;
end;

procedure TPresetEditor.ConfirmPlugTypes;
begin
  with FPictures do begin
    FreePlugBmps;
    InitPlugBmps;
  end;
end;

{procedure TPresetEditor.Rewire_Internal(const t: Real = 1.0);
var
  AItem: TObjectListItem;
begin
  AItem:=FWires.First;
  while AItem<>nil do begin
    TPWContainer(AItem.Content).Wire.Calc(t);
    AItem:=AItem.Next;
  end;
end;}

procedure TPresetEditor.Rewire(const t: Real = 1.0; const Steps: Cardinal = 1);
var
  at   : Real;
  I    : Integer;
  AItem: TObjectListItem;
begin
  at:=t/Steps;
  FMovingCount:=FWires.Count;

  AItem:=FWires.First;
  while AItem<>nil do begin
    with TPWContainer(AItem.Content).Wire do begin
      if InRest
        then Dec(FMovingCount)
        else for I:=1 to Steps do Calc(at);
    end;
    AItem:=AItem.Next;
  end;

  if FMovingCount=0 then EndRender;
  //Draw;
  Render;
end;

procedure TPresetEditor.ParamPainted(Sender: TObject; Param: TPPParam);
begin
  Repaint(nil);
end;

function TPresetEditor.ParamRequestBG(Sender: TObject; Param: TPPParam): Boolean;
begin
  Result:=false;
end;

procedure TPresetEditor.FHintDrawHide(Sender: TObject);
begin
  //if not FRendering then Render;
  if RinEventStarted
    then FRenderNecessary:=true
    else Render;
end;

procedure TPresetEditor.ViewPresetBoundsChanged(Sender: TPresetControl);
var
  I,AY       : Integer;
  AViewPreset: TViewPresetVis;
begin
  AViewPreset:=TViewPresetVis(Sender);
  AY:=AViewPreset.BoundsRect.Bottom+10;
  for I:=ViewPresetIndex[AViewPreset.LayerIndex]+1 to FViewPreset.Count-1 do begin
    AViewPreset:=TViewPresetVis(FViewPreset[I]);
    AViewPreset.Top:=AY;
    AY:=AViewPreset.BoundsRect.Bottom+10;
  end;
  CalcMaxScrollPos;
  Repaint(nil);
end;

{%ENDREGION}
{%REGION Helpprocs}

procedure TPresetEditor.StartRender;
begin
  if FRendering then exit;
  FRendering:=true;
  if Assigned(FOnStartRender) then FOnStartRender(Self);
end;

procedure TPresetEditor.PreDraw;
begin
  FPParams.OnPaint:=nil;
  FPParams.AutoRepaint:=false;
  //FHint.OnHide:=nil;
end;

procedure TPresetEditor.EndRender;
begin
  if not FRendering then exit;
  FRendering:=false;
  if Assigned(FOnEndRender) then FOnEndRender(Self);
end;

function TPresetEditor.NewWire: TPWContainer;
begin
  Result:=TPWContainer.Create(Self,FWireSettings);
  FWires.Add(Result);
  Inc(FMovingCount);
  StartRender;
end;

function TPresetEditor.GetViewPresetByLayer(Layer: TPresetIndex): TViewPresetVis;
begin
  Result:=TViewPresetVis(FViewPreset[GetViewPresetIndex(Layer)]);
end;

function TPresetEditor.GetViewPresetIndex(Layer: TPresetIndex): Integer;
begin
  if Layer>=0
    then Result:=Layer
    else Result:=FTriggerStartIndex+(not Layer);
end;

procedure TPresetEditor.CreateViewList;
var
  I,LL,LT    : TPresetIndex;
  AWire      : TPWContainer;
  //AViewPreset: TViewPresetVis;
  AY         : Integer;

  procedure HandleLayer(ALayer: TViewPresetVis; AIndex: Integer); inline;
  begin
    FViewPreset[AIndex]:=ALayer;
    //ALayer.SetBounds(FParamSlidebar.Width+20,AY,LayerWidth,0);
    AY:=ALayer.BoundsRect.Bottom+10;
    ALayer.OnBoundsChanged:=@ViewPresetBoundsChanged;
    //ALayer.InitLayer;
  end;

begin
  PreDraw;
  //EndNoRender;
  //FParamSlidebarWires.OnClosed:=nil;
  //FOutputSlidebarWires.OnClosed:=nil;
  //FParamSlideBar.Clear;
  //FOutputSlideBar.Clear;
  FParamSlidebar.Clear;
  FOutputSlidebar.Clear;
  //damit die Slidebarbreite automatisch angepasst wird;
  FParamSlidebar.Width:=0;
  FOutputSlidebar.Width:=0;

  FViewPreset.Clear;
  //FParamSlidebarWires.Clear;
  //FOutputSlidebarWires.Clear;
  FWires.Clear;
  //if (FWireAtMouse<>nil) and OpenEndIsOutput then WireAtMouse.Destroy;
  RemoveMouseWire;
  if FPreset=nil then exit;
  LL:=Length(FPreset.Layers);
  FTriggerStartIndex:=LL;
  LT:=Length(FPreset.Triggers);
  FViewPreset.Clear;
  FViewPreset.AddEmptyItems(LL+LT);
  AY:=10;
  for I:=0 to LL-1 do HandleLayer(TViewPresetVis.Create(Self,AdvCoord.BoundsRect(FParamSlidebar.Width+20,AY,LayerWidth,0),FPreset.Layers[I],I),I);
  for I:=0 to LT-1 do HandleLayer(TViewPresetVis.Create(Self,AdvCoord.BoundsRect(FParamSlidebar.Width+20,AY,LayerWidth,0),FPreset.Triggers[I],not I),I+LL);
  //InitLayer muss seperat aufgerufen werden nachdem alle Layer erstellt wurden,
  //weil sonst die Verbindungen nicht erstellt werden können
  for I:=0 to LL+LT-1 do TViewPresetVis(FViewPreset[I]).InitLayer;
  //SetPositions;
  {AY:=10;
  for I:=0 to LL+LT-1 do begin
    AViewPreset:=TViewPresetVis(FViewPreset[I]);
    AViewPreset.SetBounds(FParamSlidebar.Width+20,AY,LayerWidth,0);
    AY:=AViewPreset.BoundsRect.Bottom+10;
    AViewPreset.OnBoundsChanged:=@ViewPresetBoundsChanged;
    AViewPreset.InitLayer;
  end;}
  FParamSlidebar.AssignPreset(FPreset);
  FOutputSlidebar.AssignPreset(FPreset);
  CalcMaxScrollPos;
  {if LL+LT>0
    then FMaxScrollPos:=AViewPreset.BoundsRect.Bottom+10
    else FMaxScrollPos:=0;
  if FMaxScrollPos<Height then begin
    //FParamSlidebar.Height:=Height-20;
    FParamSlidebar.SetBounds(10,10,Height-20,0);
    FOutputSlidebar.Height:=Height-20;
  end else begin
    FParamSlidebar.SetBounds(10,10,FMaxScrollPos-20,0);
    //FParamSlidebar.Height:=FMaxScrollPos-20;
    FOutputSlidebar.Height:=FMaxScrollPos-20;
  end;}
  Draw;
  StartRender;
end;

procedure TPresetEditor.CalcMaxScrollPos; inline;
begin
  if FViewPreset.Count>0
    then FMaxScrollPos:=TViewPresetVis(FViewPreset[FViewPreset.Count-1]).BoundsRect.Bottom+10
    else FMaxScrollPos:=0;
  if FMaxScrollPos<Height then begin
    FParamSlidebar.SetBounds(10,10,0,Height-20);
    FOutputSlidebar.Height:=Height-20;
  end else begin
    FParamSlidebar.SetBounds(10,10,0,FMaxScrollPos-20);
    FOutputSlidebar.Height:=FMaxScrollPos-20;
  end;
  with FWireSettings do Box:=RealRect(FPixelSize,FPixelSize,(Width-2)*FPixelSize,(FMaxScrollPos-2)*FPixelSize);
  if Assigned(FOnMaxScrollPosChanged) then FOnMaxScrollPosChanged(Self);
end;

procedure TPresetEditor.RemoveMouseWire;
begin
  FWireAtMouse:=nil;
  FNextSlideName:='';
end;

procedure TPresetEditor.GetParamVal(Param: Pointer; AType: TVisParamType; out Value);
begin
  Move(Param^,Value,VisParamTypes.SizeOfParam(AType));
end;

procedure TPresetEditor.SetParamVal(Param: Pointer; AType: TVisParamType; const Value);
begin
  Move(Value,Param^,VisParamTypes.SizeOfParam(AType));
end;

{%ENDREGION}
{%REGION Events}

procedure TPresetEditor.RIN_StartEvent;
begin
  FRINEventStarted:=true;
end;

procedure TPresetEditor.RIN;
begin
  FRINEventStarted:=false;
  if not FRepaintNecessary then begin
    if FRenderNecessary and (not FRendering) then begin
      Render;
      FRendernecessary:=false;
    end;
    exit;
  end;
  FRenderNecessary:=false;
  FRepaintNecessary:=false;
  if FRepaintCtrl=nil then begin
    Draw;
    exit;
  end;
  FRepaintCtrl:=nil;
  FRepaintCtrl.PaintConnectedIfNecessary(Canvas);
  if not FRendering then Render;
end;

procedure TPresetEditor.Repaint(ACtrl: TPresetControl);
var
  ACtrl2: TPresetControl;
  AOwner: TPresetControl;
begin
  if not FRepaintNecessary then begin
    FRepaintNecessary:=true;
    FRepaintCtrl:=nil;
  end;
  if ACtrl=nil then exit;
  ACtrl.FRepaintNecessary:=true;
  ACtrl2:=ACtrl;
  AOwner:=ACtrl2.Owner;
  while AOwner<>nil do begin
    if AOwner.FRepaintNecessary then exit else begin
      ACtrl2.FNextRepaintCtrl:=AOwner.FRepaintCtrl;
      AOwner.FRepaintCtrl:=ACtrl2;
      ACtrl2:=AOwner;
      AOwner:=AOwner.Owner;
    end;
  end;
end;

procedure TPresetEditor.ReRender;
begin
  FRenderNecessary:=true;
end;

procedure TPresetEditor.Render;
var
  AItem: TObjectListItem;
begin
  if FLocked then exit;
  with FPaintBufferWithWires do begin
    Canvas.Draw(0,0,FPaintBuffer);
    //Canvas.Pen.Width:=3;
    AItem:=FWires.First;
    while AItem<>nil do begin
      TPWContainer(AItem.Content).Draw(Canvas,FViewRect);
      AItem:=AItem.Next;
    end;

    FHint.FinallyDrawHint;
  end;
  Canvas.Draw(0,0,FPaintBufferWithWires);
end;

procedure TPresetEditor.Paint;
begin
  //inherited Paint;
  Canvas.Draw(0,0,FPaintBufferWithWires);
  // do nothing
end;

procedure TPresetEditor.Draw;
var
  I           : Integer;
  AStr        : ^string;
  AHintSize   : TPoint;
begin
  if FLocked then exit;
  FRepaintNecessary:=false;
  PreDraw;
  inherited Paint;
  with FPaintBuffer do begin
    Canvas.AntialiasingMode:=amOn;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Color:=Color;
    Canvas.Brush.Color:=Color;
    //Canvas.Brush.Bitmap:=FBackgroundBitmap;
    Canvas.Rectangle(0,0,Width,Height);
    //Canvas.Brush.Bitmap:=nil;
    Canvas.Font.Color:=clBlack;
    if (FPictures.Images=nil) or (FPreset=nil) then begin
      Canvas.TextOut(0,0,'nothing to paint');
      exit;
    end;

    for I:=0 to FViewPreset.Count-1
      do TViewPresetVis(FViewPreset[I]).Paint(Canvas);
    FParamSlidebar.Paint(Canvas);
    FOutputSlidebar.Paint(Canvas);
  end;
  FPParams.OnPaint:=@ParamPainted;
  //FHint.OnHide:=@FHintHide;
  FPParams.AutoRepaint:=true;
  if not FRendering then Render;
end;

procedure TPresetEditor.Resize;
var
  AOldViewRect: TRect;
  I           : Cardinal;
begin
  inherited Resize;
  with FWireSettings do Box:=RealRect(FPixelSize,FPixelSize,(Width-2)*FPixelSize,(FMaxScrollPos-2)*FPixelSize);
  AOldViewRect:=FViewRect;
  with FViewRect do begin
    Left:=0;
    Right:=Self.Width;
    Top:=FScrollPos;
    Bottom:=FScrollPos+Self.Height;
  end;
  if AOldViewRect=FViewRect then exit;
  SetSubcontrolViewRects;
  if FMaxScrollPos<Height then begin
    //FParamSlideBar.SetBounds(10,10,0,Height-20);
    FParamSlidebar.Height:=Height-20;
    FOutputSlideBar.SetBounds(Width-Pictures.TempDrawBitmap.Width-10,10,0,Height-20);
  end else begin
    //FParamSlideBar.SetBounds(10,10,0,FMaxScrollPos-20);
    FParamSlidebar.Height:=FMaxScrollPos-20;
    FOutputSlideBar.SetBounds(Width-Pictures.TempDrawBitmap.Width-10,10,0,FMaxScrollPos-20);
  end;
  PreDraw;
  FLayerPosX:=(Width-LayerWidth) div 2;
  FPaintBuffer.SetSize(Width,Height);
  FPaintBufferWithWires.SetSize(Width,Height);

  Draw;
  StartRender;
end;

function TPresetEditor.GetDirectControlUnderMouse: TPresetControl;
begin
  if FControlUnderMouse<>nil
    then Result:=FControlUnderMouse.DirectControlUnderMouse
    else Result:=nil;
end;

procedure TPresetEditor.ReCheckMouseMove;
begin
  MouseMove(FMouseMoveData.Shift,FMouseMoveData.Pos.X,FMouseMoveData.Pos.Y);
end;

procedure TPresetEditor.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  I           : Integer;
  ARelativePos: TPoint;
label
  AExit;
begin
  RIN_StartEvent;
  inherited MouseMove(Shift,X,Y);
  //WireAtMouse bewegen und falls nötig rendern
  ARelativePos:=Point(X,Y)+FViewRect.TopLeft;
  FMouseMoveData:=MouseMoveData(X,Y,Shift);
  if FHint.CheckMouseOver(X,Y) then goto AExit;
  if FParamSlidebar.CheckMouseMove(Shift,ARelativePos.X,ARelativePos.Y) then goto AExit;
  if FOutputSlidebar.CheckMouseMove(Shift,ARelativePos.X,ARelativePos.Y) then goto AExit;
  FPParams.AutoRepaint:=true;
  FPParams.OnPaint:=@ParamPainted;
  FPParams.OnRequestBackground:=@ParamRequestBG;
  if FPParams.MouseMove(X+FViewRect.Left,Y+FViewRect.Top) then goto AExit;
  for I:=0 to FViewPreset.Count-1
    do if TPresetControl(FViewPreset[I]).CheckMouseMove(Shift,ARelativePos.X,ARelativePos.Y) then goto AExit;
  AExit:
  if WireAtMouse<>nil then begin
    WireAtMouse.OpenEndPos:=RealPoint(ARelativePos);
    StartRender;
  end;
  RIN;
end;

procedure TPresetEditor.Click;
begin
  inherited Click;
end;

procedure TPresetEditor.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
//var
  //ADirectControlUnderMouse: TPresetControl;
begin
  RIN_StartEvent;
  inherited MouseDown(Button,Shift,X,Y);
  SetFocus;
  FMouseIsDown:=true;
  FPParams.MouseDown;
  if FControlUnderMouse<>nil then begin
    {DirectControlUnderMouse:=DirectControlUnderMouse;
    if (FWireAtMouse<>nil) and (not ADirectControlUnderMouse.InheritsFrom(TVPVPlug)) and (not ADirectControlUnderMouse.InheritsFrom(TSlidebar)) then begin
      FWireAtMouse.Destroy;
      RemoveMouseWire;
    end;}
    FControlUnderMouse.MouseDown(Button,Shift,X,Y);
  end else if FWireAtMouse<>nil then begin
    FWireAtMouse.Destroy;
    RemoveMouseWire;
  end;
  RIN;
end;

procedure TPresetEditor.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  RIN_StartEvent;
  inherited MouseUp(Button,Shift,X,Y);
  FMouseIsDown:=false;
  FPParams.MouseUp;
  if FControlUnderMouse<>nil
    then FControlUnderMouse.MouseUp(Button,Shift,X,Y);
  RIN;
end;

procedure TPresetEditor.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TPresetEditor.MouseLeave;
begin
  RIN_StartEvent;
  FMouseIsDown:=false;
  if FControlUnderMouse<>nil then FControlUnderMouse.MouseLeave;
  FPParams.MouseLeave;
  inherited MouseLeave;
  RIN;
end;

procedure TPresetEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
end;

procedure TPresetEditor.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key,Shift);
end;

procedure TPresetEditor.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
end;
{%ENDREGION}
{%REGION Get & Set Methods}

function TPresetEditor.ViewRectPtr: PRect;
begin
  Result:=@FViewRect;
end;

procedure TPresetEditor.SetSubcontrolViewRects;
var
  I: Integer;
begin
  FPParams.SetViewRect(FViewRect);
  FParamSlidebar.AssignViewRect;
  FOutputSlidebar.AssignViewRect;
  for I:=0 to FViewPreset.Count-1 do TPresetControl(FViewPreset[I]).AssignViewRect{(FViewRect)};
end;

procedure TPresetEditor.SetScrollPos(Value: Integer);
var
  L: Integer;
begin
  L:=GetMaxScrollPos;
  if Value<=L then begin
    if Value>=0
      then FScrollPos:=Value
      else FScrollPos:=0;
  end else FScrollPos:=L;
  with FViewRect do begin
    Top:=FScrollPos;
    Bottom:=Height+FScrollPos;
  end;
  SetSubcontrolViewRects;

  Draw;
end;

function TPresetEditor.GetMaxScrollPos: Integer;
begin
  if FViewPreset.Count>0 then begin
    Result:=FMaxScrollPos-Height;
    if Result<0 then Result:=0;
  end else Result:=0;
end;

function TPresetEditor.GetHintColor: TColor;
begin
  Result:=FHint.Color;
end;

procedure TPresetEditor.SetHintColor(Value: TColor);
begin
  FHint.Color:=Value;
end;

function TPresetEditor.GetBoolBoxUI: Integer;
begin
  Result:=FPParams.BoolBoxUI;
end;

procedure TPresetEditor.SetBoolBoxUI(const Value: Integer);
begin
  FPParams.BoolBoxUI:=Value;
end;

function TPresetEditor.GetBoolBoxUHI: Integer;
begin
  Result:=FPParams.BoolBoxUHI;
end;

procedure TPresetEditor.SetBoolBoxUHI(const Value: Integer);
begin
  FPParams.BoolBoxUHI:=Value;
end;

function TPresetEditor.GetBoolBoxDI: Integer;
begin
  Result:=FPParams.BoolBoxDI;
end;

procedure TPresetEditor.SetBoolBoxDI(const Value: Integer);
begin
  FPParams.BoolBoxDI:=Value;
end;

function TPresetEditor.GetBoolBoxDHI: Integer;
begin
  Result:=FPParams.BoolBoxDHI;
end;

procedure TPresetEditor.SetBoolBoxDHI(const Value: Integer);
begin
  FPParams.BoolBoxDHI:=Value;
end;

procedure TPresetEditor.SetImages(Value: TImageList);
begin
  FPictures.Images:=Value;
  FPParams.Images:=Value;
  FPParams.SetWH(ParamEditWidth,Pictures.TempDrawBitmap.Height);
  Draw;
end;

procedure TPresetEditor.SetSlidebarImageIndex(Value: Integer);
begin
  FPictures.SlidebarImageIndex:=Value;
end;

procedure TPresetEditor.SetHighlightedSlidebarImageIndex(Value: Integer);
begin
  FPictures.HighlightedSlidebarImageIndex:=Value;
end;

{%ENDREGION}

{%ENDREGION}
{%REGION Misc}

function MouseMoveData(AX: Integer = 0; AY: Integer = 0; AShift: TShiftState = []): TMouseMoveData;
begin
  with Result do begin
    with Pos do begin
      X:=AX;
      Y:=AY;
    end;
    Shift:=AShift;
  end;
end;

procedure DrawWire(Canvas: TCanvas; ARect: TRect; AWire: TPhysicsWire; AFirstColor,ALastColor: TColor32; APixelSize: Real);
var
  AP      : TPoint;
  AClrStep: Single;

  procedure DoDrawWire(AFirstColor2,ALastColor2: TColor32); inline;
  var
    I: Integer;
  begin
    Canvas.Pen.Color:=Color32ToColor(AFirstColor2);
    Canvas.MoveTo(Round(AWire.FirstKnot.X/APixelSize)-ARect.Left,Round(AWire.FirstKnot.Y/APixelSize)-ARect.Top);
    for I:=0 to AWire.KnotCount-1 do begin
      AP:=Classes.Point(Round(AWire.Knots[I].X/APixelSize)-ARect.Left,Round(AWire.Knots[I].Y/APixelSize)-ARect.Top);
      Canvas.LineTo(AP);
      Canvas.Pen.Color:=Color32ToColor(BlendAlpha(Round(sqr(I)*AClrStep),AFirstColor2,ALastColor2));
    end;
    Canvas.LineTo(Round(AWire.LastKnot.X/APixelSize)-ARect.Left,Round(AWire.LastKnot.Y/APixelSize)-ARect.Top);
  end;

begin
  if not InView(Rect(AWire.ViewRect/APixelSize),ARect) then exit;
  AClrStep:=$FF/sqr(AWire.KnotCount-1);
  Canvas.Pen.Width:=3;
  DoDrawWire(AFirstColor,ALastColor);
  Canvas.Pen.Width:=1;
  DoDrawWire(Lighten(AFirstColor,InnerCableLighten),Lighten(ALastColor,InnerCableLighten))
end;

function GetWireColor(AType: TVisOutputType): TColor32;
begin
  if AType<VisParamTypes.Count
    then Result:=VisParamTypes.ParamTypePtr[AType]^.Color
    else Result:=oUndefinedColor;
end;

{%ENDREGION}

end.

