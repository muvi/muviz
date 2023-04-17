unit PresetUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisTypeUnit, Controls, Graphics, VisType, Dialogs,
  ArrayListedObjects, SqrFunc, PhysicsWire, AdvCoord, AdvClasses,
  ObjectClasses, ObjectClassBasic, AdvGraphCtrl, VisDrawUnit, ParamPainter,
  SplitImageButtons, ChooseColor;

type
  TPresetEditor         = class;
  TEmptyEvent           = procedure of object;
  TVPVPaintEvent        = procedure (Canvas: TCanvas; APosition: TRect) of object;
  TVPVMouseMoveEvent    = procedure (MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer) of object;
  TExpanderBtnState     = (ebsUp,ebsMouseOver,ebsDown);

  TPWContainer          = class (TObjectItem)
  private
    FParam         : TPresetOutputInfo;
    FOutput        : TPresetOutputInfo;
    FWire          : TPhysicsWire;
    FType          : TVisOutputType;
    FSlideConnector: Boolean;
  public
    constructor Create(const AFirstKnot,ALastKnot: TRealPoint; AWireSettings: TPhysicsWireSettings; const AOutput,AParam: TPresetOutputInfo; AAType: TVisOutputType = oCall);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; ARect: TRect);
    property AType: TVisOutputType read FType write FType;
    property Output: TPresetOutputInfo read FOutput write FOutput;
    property Param: TPresetOutputInfo read FParam write FParam;
    property SlideConnector: Boolean read FSlideConnector write FSlideConnector;
    property Wire: TPhysicsWire read FWire;
  end;

  TViewPresetVis        = class (TArrayListedObject)
  private
    FLayer            : PPresetVis;
    FLayerIndex       : TPresetIndex;
    FPosition         : TRect;
    FOwner            : TPresetEditor;
    FEditPos          : Integer;
    FMainParamWires   : TObjectList;
    FOutputWires      : TNilObjectArray;
    FParamWires       : array of TObjectList;
    FParamEdits       : array of TPPParam;
    FDoRewire         : TEmptyEvent;
    FDoPaint          : TVPVPaintEvent;
    FDoMouseMove      : TVPVMouseMoveEvent;
    FExpanderBtnState : TExpanderBtnState;
    function GetHeaderHeight: Integer;
    function GetSingleHeight: Integer;
    function GetExpanded: Boolean;
    procedure SetExpanded(Value: Boolean);
    procedure Rewire_Expanded;
    procedure Rewire_Not_Expanded;
    procedure Paint_Expanded(Canvas: TCanvas; APosition: TRect);
    procedure Paint_Not_Expanded(Canvas: TCanvas; APosition: TRect);
    procedure MouseMove_Expanded(MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove_Not_Expanded(MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer);
  protected
    procedure MouseMove(MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure AddParamCon(Wire: TPWContainer; AParam: Integer);
    procedure ConnectToOutput(Wire: TPWContainer; AOutput: Integer);
    function Highlighted(Param: Integer; IsOutput: Boolean): Boolean;

    property HeaderHeight: Integer read GetHeaderHeight;
    property SingleHeight: Integer read GetSingleHeight;
  public
    constructor Create(AOwner: TPresetEditor; var ALayer: TPresetVis; ALayerIndex: TPresetIndex);
    destructor Destroy; override;
    procedure Rewire;
    procedure InitLayer;
    procedure SetPosition(X,Y,AWidth: Integer);
    procedure Paint(Canvas: TCanvas; ARect: TRect);

    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Position: TRect read FPosition;
  end;

  TParamHighlightInfo   = record
    Param   : TPresetOutputInfo;
    IsOutput: Boolean;
  end;

  TPresetEditor         = class (TCustomControl)
  private
    FPreset                       : TPreset;
    FSlidebar                     : TBitmap;
    FSlidebarTop                  : TBitmap;
    FSlidebarBottom               : TBitmap;
    FSlidebarH                    : TBitmap;
    FSlidebarTopH                 : TBitmap;
    FSlidebarBottomH              : TBitmap;
    FImages                       : TImageList;
    FSlidebarImageIndex           : Integer;
    FHighlightedSlidebarImageIndex: Integer;
    FTempDrawBitmap               : TBitmap;
    FViewPreset                   : TObjectArray;
    FLayerPosX                    : Integer;
    FUnplugedImageIndex           : Integer;
    FPlugedImageIndex             : Integer;
    FHighlightedPlugedImageIndex  : Integer;
    FHighlightedUnplugedImageIndex: Integer;
    FExpandImageIndex             : Integer;
    FExpandHImageIndex            : Integer;
    FExpandDImageIndex            : Integer;
    FDeexpandImageIndex           : Integer;
    FDeexpandHImageIndex          : Integer;
    FDeexpandDImageIndex          : Integer;
    FNExpandedImageIndex          : Integer;
    FNExpandedHImageIndex         : Integer;
    FScrollPos                    : Integer;
    FPaintBuffer                  : TBitmap;
    FViewRect                     : TRect;
    FTriggerStartIndex            : Integer;
    FMouseIsDown                  : Boolean;
    FWires                        : TObjectList;
    FWireSettings                 : TPhysicsWireSettings;
    FBottom                       : Integer;
    FImageCenter                  : TPoint;
    FHighlightedParam             : TParamHighlightInfo;
    FWireAtMouse                  : TPWContainer;
    FParamSlidebarWires           : TObjectArray;
    FOutputSlidebarWires          : TObjectArray;
    FHint                         : TGraphicsHint;
    FNextSlideName                : string;
    FPParams                      : TParamPainter;
    FHeaderHeight                 : Integer;
    FSingleHeight                 : Integer;
    FBackgroundBitmap             : TBitmap;
    FMovingCount                  : Cardinal;
    FRendering                    : Boolean;
    FOnStartRender                : TNotifyEvent;
    FOnEndRender                  : TNotifyEvent;
    FSelectionChanged             : Boolean;
    procedure SetImages(Value: TImageList);
    procedure SetSlidebarImageIndex(Value: Integer);
    procedure SetHighlightedSlidebarImageIndex(Value: Integer);
    procedure SetUnplugedImageIndex(Value: Integer);
    procedure SetPlugedImageIndex(Value: Integer);
    procedure SetScrollPos(Value: Integer);
    function GetMaxScrollPos: Integer;
    function GetHintColor: TColor;
    procedure SetHintColor(Value: TColor);
    function GetOpenEndIsOutput: Boolean;
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
    procedure DrawSlidebar(ACanvas: TCanvas; AViewRect: TRect; X,Y,AHeight: Integer; ATop,AMiddle,ABottom: TBitmap);

    function GetViewPresetByLayer(Layer: TPresetIndex): TViewPresetVis;

    procedure ParamPainted(Sender: TObject; Param: TPPParam);
    function ParamRequestBG(Sender: TObject; Param: TPPParam): Boolean;
    procedure FHintHide(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Draw; virtual;
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

    procedure StartRender;
    procedure EndNoRender;
    procedure EndRender;
    procedure ParamRemoved(AObject: TObjectItem; Index: Integer);
    procedure OutputRemoved(AObject: TObjectItem; Index: Integer);
    function NewWire: TPWContainer;
    procedure RewireAll;
    procedure DrawSqrFunc(ACanvas: TCanvas; const ARect: TRect; const AFunc: TSqrFunc; X1,X2: Integer);
    procedure LoadImage(Pluged,Highlighted: Boolean; AType: TVisOutputType);
    procedure LoadImage(Index: Integer);
    function Highlighted(Layer,Param: TPresetIndex; IsOutput: Boolean): Boolean;
    procedure SetPositions;
    procedure CreateViewList;
    function ViewPresetAtCursor(X,Y: Integer): TViewPresetVis;
    procedure RemoveMouseWire;
    procedure SelectionChanged;
    property HighlightedParam: TParamHighlightInfo read FHighlightedParam write FHighlightedParam;
    property OpenEndIsOutput: Boolean read GetOpenEndIsOutput;
    property PParams: TParamPainter read FPParams;
    property TempDrawBitmap: TBitmap read FTempDrawBitmap;
    property TriggerStartIndex: Integer read FTriggerStartIndex;
    property ViewPresetByLayer[Layer: TPresetIndex]: TViewPresetVis read GetViewPresetByLayer;
    property WireAtMouse: TPWContainer read FWireAtMouse write FWireAtMouse;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rewire(const t: Real = 1.0; const Steps: Cardinal = 1);
    procedure AssignPreset(APreset: TPreset);
    procedure RemovePreset;

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
    property DeexpandImageIndex: Integer read FDeexpandImageIndex write FDeexpandImageIndex default -1;
    property DeexpandHImageIndex: Integer read FDeexpandHImageIndex write FDeexpandHImageIndex default -1;
    property DeexpandDImageIndex: Integer read FDeexpandDImageIndex write FDeexpandDImageIndex default -1;
    property Enabled;
    property ExpandImageIndex: Integer read FExpandImageIndex write FExpandImageIndex default -1;
    property ExpandHImageIndex: Integer read FExpandHImageIndex write FExpandHImageIndex default -1;
    property ExpandDImageIndex: Integer read FExpandDImageIndex write FExpandDImageIndex default -1;
    property Font;
    property HighlightedPlugedImageIndex: Integer read FHighlightedPlugedImageIndex write FHighlightedPlugedImageIndex;
    property HighlightedSlidebarImageIndex: Integer read FHighlightedSlidebarImageIndex write SetHighlightedSlidebarImageIndex default -1;
    property HighlightedUnplugedImageIndex: Integer read FHighlightedUnplugedImageIndex write FHighlightedUnplugedImageIndex;
    property HintColor: TColor read GetHintColor write SetHintColor default clDefaultParamHint;
    property Images: TImageList read FImages write SetImages;
    property MovingCount: Cardinal read FMovingCount;
    property NotExpandedImageIndex: Integer read FNExpandedImageIndex write FNExpandedImageIndex default -1;
    property NotExpandedHImageIndex: Integer read FNExpandedHImageIndex write FNExpandedHImageIndex default -1;
    property PlugedImageIndex: Integer read FPlugedImageIndex write SetPlugedImageIndex default -1;
    property PopupMenu;
    property ScrollPos: Integer read FScrollPos write SetScrollPos default 0;
    property ShowHint;
    property SlidebarImageIndex: Integer read FSlidebarImageIndex write SetSlidebarImageIndex default -1;
    property UnplugedImageIndex: Integer read FUnplugedImageIndex write SetUnplugedImageIndex default -1;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnEndRender: TNotifyEvent read FOnEndRender write FOnEndRender;
    property OnResize;
    property OnShowHint;
    property OnStartRender: TNotifyEvent read FOnStartRender write FOnStartRender;
  end;

procedure DrawWire(Canvas: TCanvas; ARect: TRect; AWire: TPhysicsWire);
operator = (m1,m2: TParamHighlightInfo): Boolean;

implementation

const
  LayerWidth    = 500;
  ParamWidth    = 150;
  ParamEditWidth= 200;
  oUndefined    = oBuffer+1;

  WireColorsO: array [oCall..oUndefined] of TColor = ($414141,$FF0000,$05D300,$0000FF,$E6E600,$22D8D8,$FF00FF,$888888);

{TPWContainer}

constructor TPWContainer.Create(const AFirstKnot,ALastKnot: TRealPoint; AWireSettings: TPhysicsWireSettings; const AOutput,AParam: TPresetOutputInfo; AAType: TVisOutputType = oCall);
begin
  inherited Create;
  FWire:=TPhysicsWire.Create(AFirstKnot,ALastKnot,AWireSettings);
  FOutput:=AOutput;
  FParam:=AParam;
  FType:=AAType;
  FSlideConnector:=false;
end;

destructor TPWContainer.Destroy;
begin
  FWire.Destroy;
  inherited Destroy;
end;

procedure TPWContainer.Draw(Canvas: TCanvas; ARect: TRect);
begin
  Canvas.Pen.Color:=WireColorsO[FType];
  DrawWire(Canvas,ARect,FWire);
end;

{TViewPresetVis}

constructor TViewPresetVis.Create(AOwner: TPresetEditor; var ALayer: TPresetVis; ALayerIndex: TPresetIndex);
var
  I,PL,OL   : Integer;
  AParam    : TVisParam;
  AVisType  : TVisType;
  AParamEdit: TPPParam;
begin
  inherited Create;
  FOwner:=AOwner;
  FLayer:=@ALayer;
  FLayerIndex:=ALayerIndex;
  with TVisualisation.Visualisations(ALayer.VisID) do begin
    PL:=Length(VisParamDesc)+3;
    OL:=Length(OutputDesc);
  end;
  SetLength(FParamWires,PL);
  SetLength(FParamEdits,PL);
  FOutputWires:=TNilObjectArray.Create;
  FOutputWires.Count:=OL;
  for I:=0 to PL-1 do FParamWires[I]:=TObjectList.Create;
  FMainParamWires:=TObjectList.Create;

  AVisType:=TVisualisation.Visualisations(ALayer.VisID);

  AParam:=NextParamPosCCC(ALayer,0,AParam);
  AParamEdit:=FOwner.PParams.Add(AParam,AVisType.C1Desc,vColor,0,0);
  AParamEdit.Visible:=ALayer.Expanded;
  FParamEdits[0]:=AParamEdit;
  AParam:=NextParamPosCCC(ALayer,1,AParam);
  AParamEdit:=FOwner.PParams.Add(AParam,AVisType.C2Desc,vColor,0,0);
  AParamEdit.Visible:=ALayer.Expanded;
  FParamEdits[1]:=AParamEdit;
  AParam:=NextParamPosCCC(ALayer,2,AParam);
  AParamEdit:=FOwner.PParams.Add(AParam,AVisType.C3Desc,vColor,0,0);
  AParamEdit.Visible:=ALayer.Expanded;
  FParamEdits[2]:=AParamEdit;

  for I:=0 to PL-4 do begin
    AParam:=NextParamPosCCC(ALayer,I+3,AParam);
    AParamEdit:=FOwner.PParams.Add(AParam,AVisType.VisParamDesc[I].Name,AVisType.VisParamDesc[I].AType,0,0);
    AParamEdit.Visible:=ALayer.Expanded;
    FParamEdits[I+3]:=AParamEdit;
  end;

  if ALayer.Expanded then begin
    FDoRewire:=@Rewire_Expanded;
    FDoPaint:=@Paint_Expanded;
    FDoMouseMove:=@MouseMove_Expanded;
  end else begin
    FDoRewire:=@Rewire_Not_Expanded;
    FDoPaint:=@Paint_Not_Expanded;
    FDoMouseMove:=@MouseMove_Not_Expanded;
  end;
  FExpanderBtnState:=ebsUp;
end;

destructor TViewPresetVis.Destroy;
var
  I    : Integer;
  AWire: TPWContainer;
begin
  for I:=0 to FOutputWires.Count-1 do begin
    AWire:=TPWContainer(FOutputWires[I]);
    if AWire<>nil then AWire.Destroy;
  end;
  FOutputWires.Destroy;
  for I:=0 to Length(FParamWires)-1 do begin
    FParamWires[I].Destroy;
    FParamEdits[I].Destroy;
  end;
  FMainParamWires.Destroy;
  SetLength(FParamWires,0);
  inherited Destroy;
end;

procedure TViewPresetVis.InitLayer;
var
  I       : Integer;
  AWire   : TPWContainer;
  AVisType: TVisType;
begin
  with FLayer^ do begin
    AVisType:=TVisualisation.Visualisations(FLayer^.VisID);
    for I:=0 to Length(VisOutputs)-1 do with VisOutputs[I] do if Param>=-1 then begin
      AWire:=FOwner.NewWire;
      FOutputWires[I]:=AWire;
      with AWire do begin
        Wire.FirstKnot:=RealPoint(FPosition.Right-5-FOwner.FImageCenter.X,FPosition.Top+HeaderHeight+(I*SingleHeight)+3+FOwner.FImageCenter.Y);
        AType:=AVisType.OutputDesc[I].AType;
        FOutput.Param:=I;
        FOutput.Layer:=FLayerIndex;
      end;
      FOwner.ViewPresetByLayer[Layer].AddParamCon(AWire,Param);
      {if Layer>=0
        then TViewPresetVis(FOwner.FViewPreset[Layer]).AddParamCon(AWire,Param)
        else TViewPresetVis(FOwner.FViewPreset[FOwner.TriggerStartIndex+(not Layer)]).AddParamCon(AWire,Param);}
      AWire.Wire.KnotCount:=50;
    end;
  end;
end;

procedure TViewPresetVis.AddParamCon(Wire: TPWContainer; AParam: Integer);
begin
  if AParam=-1 then begin
    FMainParamWires.Add(Wire);
    Wire.Wire.LastKnot:=RealPoint(FPosition.Left+5+FOwner.FImageCenter.X,FPosition.Top+5+FOwner.FImageCenter.Y);
  end else begin
    FParamWires[AParam].Add(Wire);
    Wire.Wire.LastKnot:=RealPoint(FPosition.Left+5+FOwner.FImageCenter.X,FPosition.Top+HeaderHeight+(AParam*SingleHeight)+3+FOwner.FImageCenter.Y);
  end;
  Wire.FParam.Param:=AParam;
  Wire.FParam.Layer:=FLayerIndex;
end;

procedure TViewPresetVis.ConnectToOutput(Wire: TPWContainer; AOutput: Integer);
begin
  FOutputWires[AOutput]:=Wire;
  Wire.FOutput.Param:=AOutput;
  Wire.FOutput.Layer:=FLayerIndex;
  Wire.Wire.FirstKnot:=RealPoint(FPosition.Right-5-FOwner.FImageCenter.X,FPosition.Top+HeaderHeight+(AOutput*SingleHeight)+FOwner.FImageCenter.Y);
end;

function TViewPresetVis.Highlighted(Param: Integer; IsOutput: Boolean): Boolean;
begin
  Result:=FOwner.Highlighted(FLayerIndex,Param,IsOutput);
end;

procedure TViewPresetVis.Rewire_Expanded;
var
  I     : Integer;
  AItem : TObjectListItem;
  AWire : TPWContainer;
  APoint: TRealPoint;
begin
  for I:=0 to FOutputWires.Count-1 do begin
    AWire:=TPWContainer(FOutputWires[I]);
    if AWire<>nil then AWire.Wire.FirstKnot:=RealPoint(FPosition.Right-5-FOwner.FImageCenter.X,FPosition.Top+HeaderHeight+(I*SingleHeight)+3+FOwner.FImageCenter.Y);
  end;
  for I:=0 to Length(FParamWires)-1 do begin
    AItem:=FParamWires[I].First;
    APoint:=RealPoint(FPosition.Left+5+FOwner.FImageCenter.X,FPosition.Top+HeaderHeight+(I*SingleHeight)+3+FOwner.FImageCenter.Y);
    while AItem<>nil do begin
      TPWContainer(AItem.Content).Wire.LastKnot:=APoint;
      AItem:=AItem.Next;
    end;
  end;
  AItem:=FMainParamWires.First;
  APoint:=RealPoint(FPosition.Left+5+FOwner.FImageCenter.X,FPosition.Top+5+FOwner.FImageCenter.Y);
  while AItem<>nil do begin
    TPWContainer(AItem.Content).Wire.LastKnot:=APoint;
    AItem:=AItem.Next;
  end;
end;

procedure TViewPresetVis.Rewire_Not_Expanded;
var
  I     : Integer;
  AItem : TObjectListItem;
  AWire : TPWContainer;
  APoint: TRealPoint;
begin
  APoint:=RealPoint(FPosition.Right-5-FOwner.FImageCenter.X,FPosition.Top+5+FOwner.FImageCenter.Y);
  for I:=0 to FOutputWires.Count-1 do begin
    AWire:=TPWContainer(FOutputWires[I]);
    if AWire<>nil then AWire.Wire.FirstKnot:=APoint;
  end;
  APoint:=RealPoint(FPosition.Left+5+FOwner.FImageCenter.X,FPosition.Top+5+FOwner.FImageCenter.Y);
  for I:=0 to Length(FParamWires)-1 do begin
    AItem:=FParamWires[I].First;
    while AItem<>nil do begin
      TPWContainer(AItem.Content).Wire.LastKnot:=APoint;
      AItem:=AItem.Next;
    end;
  end;
  AItem:=FMainParamWires.First;
  while AItem<>nil do begin
    TPWContainer(AItem.Content).Wire.LastKnot:=APoint;
    AItem:=AItem.Next;
  end;
end;

procedure TViewPresetVis.Rewire;
begin
  FDoRewire;
end;

procedure TViewPresetVis.SetPosition(X,Y,AWidth: Integer);
var
  I,L,LO: Integer;
begin
  with TVisualisation.Visualisations(FLayer^.VisID) do begin
    L:=Length(VisParamDesc)+3;
    LO:=Length(OutputDesc);
    if LO>L
      then L:=LO;
    {HeaderHeight:=FOwner.FTempDrawBitmap.Height+10;
    SingleHeight:=FOwner.FTempDrawBitmap.Height+6;}
    FEditPos:=X+((AWidth-ParamEditWidth) div 2);
    for I:=0 to Length(FParamEdits)-1
      do FParamEdits[I].SetWH(FEditPos,Y+HeaderHeight+3+I*SingleHeight{,ParamEditWidth,SingleHeight-6});

    with FPosition do begin
      Left:=X;
      Top:=Y;
      Right:=X+AWidth;
      if Expanded
        then Bottom:=Y+(L*SingleHeight)+HeaderHeight
        else Bottom:=Y+HeaderHeight;
    end;
  end;
end;

procedure TViewPresetVis.Paint_Expanded(Canvas: TCanvas; APosition: TRect);
var
  I,LP,LO,AY,ASingleTextStart: Integer;
begin
  with TVisualisation.Visualisations(FLayer^.VisID) do begin
    LP:=Length(VisParamDesc)+3;
    LO:=Length(OutputDesc);

    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=$555555;
    Canvas.Brush.Color:=$999999;
    Canvas.RoundRect(APosition,30,30);

    Canvas.Font.Color:=clBlack;
    Canvas.Font.Style:=[fsBold];
    Canvas.TextOut(((APosition.Right+APosition.Left-Canvas.TextWidth(Name)) div 2),APosition.Top+3,Name);
    FOwner.LoadImage(FMainParamWires.Count>0,Highlighted(-1,false),vCall);
    Canvas.Draw(APosition.Left+5,APosition.Top+5,FOwner.TempDrawBitmap);
    Canvas.Font.Style:=[];
    ASingleTextStart:=(SingleHeight-Canvas.TextHeight('Wg')) div 2;

    AY:=APosition.Top+HeaderHeight;

    FOwner.LoadImage(FParamWires[0].Count>0,Highlighted(0,false),vColor);
    Canvas.Draw(APosition.Left+5,AY+3,FOwner.FTempDrawBitmap);
    Canvas.TextOut(APosition.Left+10+FOwner.FTempDrawBitmap.Width,AY+ASingleTextStart,C1Desc);
    AY+=SingleHeight;
    FOwner.LoadImage(FParamWires[1].Count>0,Highlighted(1,false),vColor);
    Canvas.Draw(APosition.Left+5,AY+3,FOwner.FTempDrawBitmap);
    Canvas.TextOut(APosition.Left+10+FOwner.FTempDrawBitmap.Width,AY+ASingleTextStart,C2Desc);
    AY+=SingleHeight;
    FOwner.LoadImage(FParamWires[2].Count>0,Highlighted(2,false),vColor);
    Canvas.Draw(APosition.Left+5,AY+3,FOwner.FTempDrawBitmap);
    Canvas.TextOut(APosition.Left+10+FOwner.FTempDrawBitmap.Width,AY+ASingleTextStart,C3Desc);
    AY+=SingleHeight;

    for I:=0 to LP-4 do with VisParamDesc[I] do begin
      FOwner.LoadImage(FParamWires[I+3].Count>0,Highlighted(I+3,false),AType);
      Canvas.Draw(APosition.Left+5,AY+3,FOwner.FTempDrawBitmap);
      Canvas.TextOut(APosition.Left+10+FOwner.FTempDrawBitmap.Width,AY+ASingleTextStart,Name);
      AY+=SingleHeight;
    end;

    AY:=APosition.Top+HeaderHeight;
    for I:=0 to LO-1 do with OutputDesc[I] do begin
      FOwner.LoadImage(FOutputWires[I]<>nil,Highlighted(I,true),AType);
      Canvas.Draw(APosition.Right-5-FOwner.TempDrawBitmap.Width,AY+3,FOwner.TempDrawBitmap);
      Canvas.TextOut(APosition.Right-10-FOwner.TempDrawBitmap.Width-Canvas.TextWidth(Name),AY+ASingleTextStart,Name);
      AY+=SingleHeight;
    end;

    case FExpanderBtnState of
      ebsUp       : FOwner.LoadImage(FOwner.FDeexpandImageIndex);
      ebsMouseOver: FOwner.LoadImage(FOwner.FDeexpandHImageIndex);
      ebsDown     : FOwner.LoadImage(FOwner.FDeexpandDImageIndex);
    end;
    Canvas.Draw(APosition.Right-FOwner.FTempDrawBitmap.Width*2-10,APosition.Top+5,FOwner.TempDrawBitmap);

    for I:=0 to LP-1 do FParamEdits[I].Paint(FOwner.FViewRect);
  end;
end;

procedure TViewPresetVis.Paint_Not_Expanded(Canvas: TCanvas; APosition: TRect);
begin
  with TVisualisation.Visualisations(FLayer^.VisID) do begin
    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=$555555;
    Canvas.Brush.Color:=$999999;
    Canvas.RoundRect(APosition,30,30);

    Canvas.Font.Color:=clBlack;
    Canvas.Font.Style:=[fsBold];
    Canvas.TextOut(((APosition.Right+APosition.Left-Canvas.TextWidth(Name)) div 2),APosition.Top+3,Name);

    if Highlighted(-1,false)
      then FOwner.LoadImage(FOwner.FNExpandedHImageIndex)
      else FOwner.LoadImage(FOwner.FNExpandedImageIndex);
    Canvas.Draw(APosition.Left+5,APosition.Top+5,FOwner.TempDrawBitmap);
    if Highlighted(-1,true)
      then FOwner.LoadImage(FOwner.FNExpandedHImageIndex)
      else FOwner.LoadImage(FOwner.FNExpandedImageIndex);
    Canvas.Draw(APosition.Right-FOwner.FTempDrawBitmap.Width-5,APosition.Top+5,FOwner.TempDrawBitmap);
    Canvas.Font.Style:=[];

    case FExpanderBtnState of
      ebsUp       : FOwner.LoadImage(FOwner.FExpandImageIndex);
      ebsMouseOver: FOwner.LoadImage(FOwner.FExpandHImageIndex);
      ebsDown     : FOwner.LoadImage(FOwner.FExpandDImageIndex);
    end;
    Canvas.Draw(APosition.Right-FOwner.FTempDrawBitmap.Width*2-10,APosition.Top+5,FOwner.TempDrawBitmap);
  end;
end;

procedure TViewPresetVis.Paint(Canvas: TCanvas; ARect: TRect);
begin
  if not InView(FPosition,ARect) then exit;
  FDoPaint(Canvas,ToViewRect(FPosition,ARect));
end;

procedure TViewPresetVis.MouseMove_Not_Expanded(MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer);
var
  ARight: Integer;
begin
  FOwner.FHighlightedParam.Param.Layer:=FLayerIndex;
  if (Y>=5) and (Y<=FOwner.FTempDrawBitmap.Height+5) then begin
    if (X>=5) and (X<=FOwner.FTempDrawBitmap.Width+5) then begin
      FOwner.FHighlightedParam.IsOutput:=false;
      FOwner.FHighlightedParam.Param.Param:=-1;
    end else begin
      ARight:=(FPosition.Right-FPosition.Left)-5;
      if (X>=ARight-FOwner.FTempDrawBitmap.Width) and (X<=ARight) then begin
        FOwner.FHighlightedParam.IsOutput:=true;
        FOwner.FHighlightedParam.Param.Param:=-1;
      end else FOwner.FHighlightedParam.Param.Param:=-3;
    end;
  end;
end;

procedure TViewPresetVis.MouseMove_Expanded(MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer);
var
  ARight,APPos: Integer;
  AVis        : PVisType;
begin
  AVis:=TVisualisation.VisPtr(FLayer^.VisID);
  FOwner.FHighlightedParam.Param.Layer:=FLayerIndex;
  if (X>=5) and (X<=5+FOwner.FTempDrawBitmap.Width) then begin
    FOwner.FHighlightedParam.IsOutput:=false;
    if Y<=HeaderHeight then begin
      if (Y>=5) and (Y<=FOwner.FTempDrawBitmap.Height+5)
        then FOwner.FHighlightedParam.Param.Param:=-1
        else FOwner.FHighlightedParam.Param.Param:=-3;
    end else begin
      FOwner.FHighlightedParam.Param.Param:=(Y-HeaderHeight) div SingleHeight;
      if FOwner.FHighlightedParam.Param.Param<Length(AVis^.VisParamDesc)+3 then begin
        APPos:=(FOwner.FHighlightedParam.Param.Param*SingleHeight)+HeaderHeight+3;
        if (Y<APPos) or (Y>APPos+FOwner.FTempDrawBitmap.Height)
          then FOwner.FHighlightedParam.Param.Param:=-3;
      end else FOwner.FHighlightedParam.Param.Param:=-3;
    end;
  end else begin
    ARight:=(FPosition.Right-FPosition.Left)-5;
    if (X>=ARight-FOwner.FTempDrawBitmap.Width) and (X<=ARight) and (Y>=HeaderHeight) then begin
      FOwner.FHighlightedParam.IsOutput:=true;
      FOwner.FHighlightedParam.Param.Param:=(Y-HeaderHeight) div SingleHeight;
      if FOwner.FHighlightedParam.Param.Param<Length(AVis^.OutputDesc) then begin
        APPos:=(FOwner.FHighlightedParam.Param.Param*SingleHeight)+HeaderHeight+3;
        if (Y<APPos) or (Y>APPos+FOwner.FTempDrawBitmap.Height)
          then FOwner.FHighlightedParam.Param.Param:=-3;
      end else FOwner.FHighlightedParam.Param.Param:=-3;
    end else FOwner.FHighlightedParam.Param.Param:=-3;
  end;
end;

procedure TViewPresetVis.MouseMove(MouseIsDown: Boolean; Shift: TShiftState; X,Y: Integer);
var
  ARight,APPos: Integer;
begin
  ARight:=(FPosition.Right-FPosition.Left)-5;
  APPos:=ARight-FOwner.FtempDrawBitmap.Width*2-5;
  if (Y>=5) and (Y<=FOwner.FTempDrawBitmap.Height+5) and (X>=APPos) and (X<=APPos+FOwner.FTempDrawBitmap.Width) then begin
    if FExpanderBtnState<>ebsMouseOver then begin
      FExpanderBtnState:=ebsMouseOver;
      FOwner.SelectionChanged;
    end;
    with FOwner.FHighlightedParam.Param do begin
      Layer:=FLayerIndex;
      Param:=-3;
    end;
    exit;
  end;
  if FExpanderBtnState=ebsMouseOver then begin
    FExpanderBtnState:=ebsUp;
    FOwner.SelectionChanged;
  end;
  FDoMouseMove(MouseIsDown,Shift,X,Y);
end;

procedure TViewPresetVis.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  with FOwner do begin
    if FExpanderBtnState=ebsMouseOver then begin
      FExpanderBtnState:=ebsDown;
      FOwner.SelectionChanged;
      exit;
    end;
    if not Expanded then exit;
    with HighlightedParam do begin
      if Param.Param<-1 then begin
        if WireAtMouse<>nil then begin
          WireAtMouse.Destroy;
          RemoveMouseWire;
        end;
      end else begin
        if WireAtMouse<>nil then begin
          if IsOutput then begin
            if OpenEndIsOutput and (FOutputWires[Param.Param]=nil) then begin
              FOutputWires[Param.Param]:=WireAtMouse;
              WireAtMouse.FOutput:=Param;
              WireAtMouse.Wire.FirstKnot:=RealPoint(FPosition.Right-5-FImageCenter.X,FPosition.Top+FHeaderHeight+(FSingleHeight*Param.Param)+3+FImageCenter.Y);
              FLayer^.VisOutputs[Param.Param]:=WireAtMouse.FParam;
              if WireAtMouse.FParam.Param=-2
                then FPreset.Outputs[WireAtMouse.Param.Layer].OutputInfo:=Param;
              WireAtMouse.AType:=TVisualisation.Visualisations(FLayer^.VisID).OutputDesc[Param.Param].AType;
              RemoveMouseWire;
            end;
          end else begin
            if not OpenEndIsOutput then begin
              WireAtMouse.Param:=Param;
              if Param.Param>=0 then begin
                WireAtMouse.Wire.LastKnot:=RealPoint(FPosition.Left+5+FImageCenter.X,FPosition.Top+FHeaderHeight+(FSingleHeight*Param.Param)+3+FImageCenter.Y);
                FParamWires[Param.Param].Add(WireAtMouse);
              end else begin
                WireAtMouse.Wire.LastKnot:=RealPoint(FPosition.Left+5+FImageCenter.X,FPosition.Top+5+FImageCenter.Y);
                FMainParamWires.Add(WireAtMouse);
              end;
              if WireAtMouse.Output.Param=-2 then begin
                FPreset.Params[WireAtMouse.Output.Layer].OutputInfo:=Param;
                WireAtMouse.AType:=TVisualisation.ParamType(FLayer^.VisID,Param.Param);
              end else SetVisOutput(FPreset,WireAtMouse.Output,Param);
              RemoveMouseWire;
            end;
          end;
        end else begin
          if IsOutput then begin
            WireAtMouse:=TPWContainer(FOutputWires[Param.Param]);
            if WireAtMouse<>nil then begin
              WireAtMouse.FOutput.Param:=-3;
              if WireAtMouse.Param.Param>-2 then begin
                if WireAtMouse.Param.Layer>=0
                  then WireAtMouse.AType:=TVisualisation.ParamType(FPreset.Layers[WireAtMouse.Param.Layer].VisID,WireAtMouse.Param.Param)
                  else WireAtMouse.AType:=TVisualisation.ParamType(FPreset.Triggers[not WireAtMouse.Param.Layer].VisID,WireAtMouse.Param.Param);
              end else WireAtMouse.AType:=oUndefined;
              FOutputWires[Param.Param]:=nil;
              FLayer^.VisOutputs[Param.Param]:=poiUNDEFINED;
            end else begin
              WireAtMouse:=NewWire;
              FOutputWires[Param.Param]:=WireAtMouse;
              WireAtMouse.Output:=Param;
              WireAtMouse.AType:=TVisualisation.Visualisations(FLayer^.VisID).OutputDesc[Param.Param].AType;
              WireAtMouse.Wire.FirstKnot:=RealPoint(FPosition.Right-5-FImageCenter.X,FPosition.Top+FHeaderHeight+(FSingleHeight*Param.Param)+3+FImageCenter.Y);
              WireAtMouse.Wire.LastKnot:=RealPoint(FPosition.Left+X,FPosition.Top+Y);
              WireAtMouse.Wire.KnotCount:=50;
            end;
          end else begin
            if Param.Param>=0 then begin
              if FParamWires[Param.Param].Count=1 then begin
                WireAtMouse:=TPWContainer(FParamWires[Param.Param].First.Content);
                FParamWires[Param.Param].First.Destroy;
              end else WireAtMouse:=nil;
            end else begin
              if FMainParamWires.Count=1 then begin
                WireAtMouse:=TPWContainer(FMainParamWires.First.Content);
                FMainParamWires.First.Destroy;
              end else WireAtMouse:=nil;
            end;
            if WireAtMouse<>nil then begin
              WireAtMouse.Wire.LastKnot:=RealPoint(FPosition.Left+X,FPosition.Top+Y);
              WireAtMouse.Param:=poiUNDEFINED;
              if WireAtMouse.Output.Param=-2 then WireAtMouse.AType:=oUndefined;
            end else begin
              WireAtMouse:=NewWire;
              WireAtMouse.Param:=Param;
              WireAtMouse.AType:=TVisualisation.ParamType(FLayer^.VisID,Param.Param);
              WireAtMouse.Wire.FirstKnot:=RealPoint(FPosition.Left+X,FPosition.Top+Y);
              if Param.Param>=0 then begin
                WireAtMouse.Wire.LastKnot:=RealPoint(FPosition.Left+5+FImageCenter.X,FPosition.Top+FHeaderHeight+(FSingleHeight*Param.Param)+3+FImageCenter.Y);
                FParamWires[Param.Param].Add(WireAtMouse);
              end else begin
                WireAtMouse.Wire.LastKnot:=RealPoint(FPosition.Left+5+FImageCenter.X,FPosition.Top+5+FImageCenter.Y);
                FMainParamWires.Add(WireAtMouse);
              end;
              WireAtMouse.Wire.KnotCount:=50;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TViewPresetVis.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if FExpanderBtnState=ebsDown then begin
    FExpanderBtnState:=ebsMouseOver;
    Expanded:=not Expanded;
  end;
end;

function TViewPresetVis.GetHeaderHeight: Integer;
begin
  Result:=FOwner.FHeaderHeight;
end;

function TViewPresetVis.GetSingleHeight: Integer;
begin
  Result:=FOwner.FSingleHeight;
end;

function TViewPresetVis.GetExpanded: Boolean;
begin
  Result:=FLayer^.Expanded;
end;

procedure TViewPresetVis.SetExpanded(Value: Boolean);
var
  I: Integer;
begin
  FLayer^.Expanded:=Value;
  if Value then begin
    FDoRewire:=@Rewire_Expanded;
    FDoPaint:=@Paint_Expanded;
    FDoMouseMove:=@MouseMove_Expanded;
  end else begin
    FDoRewire:=@Rewire_Not_Expanded;
    FDoPaint:=@Paint_Not_Expanded;
    FDoMouseMove:=@MouseMove_Not_Expanded;
  end;
  FOwner.EndNoRender;
  for I:=0 to Length(FParamEdits)-1 do FParamEdits[I].Visible:=Value;
  with FOwner do begin
    SetPositions;
    RewireAll;
    StartRender;
  end;
  if Assigned(FOwner.OnResize) then FOwner.OnResize(FOwner);
end;

{TPresetEditor}

constructor TPresetEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectionChanged:=false;
  FRendering:=true;
  FViewPreset:=TObjectArray.Create;
  FPreset:=nil;
  Color:=clBtnFace;
  TabStop:=true;
  FBottom:=0;
  FBackgroundBitmap:=nil;
  FImageCenter:=Classes.Point(5,5);
  FPaintBuffer:=TBitmap.Create;
  FSlidebar:=TBitmap.Create;
  FSlidebarTop:=TBitmap.Create;
  FSlidebarBottom:=TBitmap.Create;
  FSlidebarH:=TBitmap.Create;
  FSlidebarTopH:=TBitmap.Create;
  FSlidebarBottomH:=TBitmap.Create;
  FTempDrawBitmap:=TBitmap.Create;
  FPParams:=TParamPainter.Create(Self,FPaintBuffer.Canvas,@GetParamVal,@SetParamVal,0,0,false);
  FPParams.OnRequestBackground:=@ParamRequestBG;
  SetImages(nil);
  FExpandImageIndex:=-1;
  FExpandHImageIndex:=-1;
  FExpandDImageIndex:=-1;
  FDeexpandImageIndex:=-1;
  FDeexpandHImageIndex:=-1;
  FDeexpandDImageIndex:=-1;
  FLayerPosX:=0;
  FUnplugedImageIndex:=-1;
  FPlugedImageIndex:=-1;
  FNExpandedImageIndex:=-1;
  FNExpandedHImageIndex:=-1;
  FScrollPos:=0;
  FTriggerStartIndex:=0;
  FMouseIsDown:=false;
  FHint:=TGraphicsHint.Create(Self,FPaintBuffer.Canvas);
  FWireSettings:=TPhysicsWireSettings.Create;
  FWires:=TObjectList.Create(true);
  FHighlightedParam.Param.Param:=-3;
  FMovingCount:=0;
  RemoveMouseWire;
  FParamSlidebarWires:=TObjectArray.Create(false);
  FParamSlidebarWires.OnClosed:=@ParamRemoved;
  FOutputSlidebarWires:=TObjectArray.Create(false);
  FOutputSlidebarWires.OnClosed:=@OutputRemoved;
end;

destructor TPresetEditor.Destroy;
begin
  FRendering:=false;
  FParamSlidebarWires.OnClosed:=nil;
  FOutputSlidebarWires.OnClosed:=nil;
  if (FWireAtMouse<>nil) and OpenEndIsOutput then FWireAtMouse.Destroy;
  FViewPreset.Destroy;
  FWires.Destroy;
  FWireSettings.Destroy;
  FParamSlidebarWires.Destroy;
  FOutputSlidebarWires.Destroy;
  FPParams.Destroy;
  FTempDrawBitmap.Destroy;
  FSlidebarBottom.Destroy;
  FSlidebarTop.Destroy;
  FSlidebar.Destroy;
  FSlidebarBottomH.Destroy;
  FSlidebarTopH.Destroy;
  FSlidebarH.Destroy;
  FHint.Destroy;
  FPaintBuffer.Destroy;
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

  //for I:=1 to Steps do begin
  AItem:=FWires.First;
  while AItem<>nil do begin
    with TPWContainer(AItem.Content).Wire do begin
      if InRest
        then Dec(FMovingCount)
        else for I:=1 to Steps do Calc(at);
    end;
    AItem:=AItem.Next;
  end;
  //end;

  //Rewire_Internal(at);
  if FMovingCount=0 then EndRender;
  Draw;
end;

procedure TPresetEditor.ParamPainted(Sender: TObject; Param: TPPParam);
begin
  Paint;
end;

function TPresetEditor.ParamRequestBG(Sender: TObject; Param: TPPParam): Boolean;
begin
  Result:=false;
  Draw;
end;

procedure TPresetEditor.FHintHide(Sender: TObject);
begin
  Draw;
end;

{Helpprocs}

procedure TPresetEditor.StartRender;
begin
  if FRendering then exit;
  FRendering:=true;
  FPParams.AutoRepaint:=false;
  FPParams.OnPaint:=nil;
  FHint.OnHide:=nil;
  if Assigned(FOnStartRender) then FOnStartRender(Self);
end;

procedure TPresetEditor.EndNoRender;
begin
  if FRendering then exit;
  FPParams.AutoRepaint:=false;
  FPParams.OnPaint:=nil;
  FHint.OnHide:=nil;
end;

procedure TPresetEditor.EndRender;
begin
  if not FRendering then exit;
  if Assigned(FOnEndRender) then FOnEndRender(Self);
  FPParams.AutoRepaint:=true;
  FPParams.OnPaint:=@ParamPainted;
  FHint.OnHide:=@FHintHide;
  FPParams.SetViewRect(FViewRect);
  FRendering:=false;
end;

procedure TPresetEditor.ParamRemoved(AObject: TObjectItem; Index: Integer);
var
  I,L: Integer;
begin
  with FPreset do begin
    L:=Length(Params);
    FNextSlideName:=Params[Index].Name;
    if IsDefaultParamName(FNextSlideName) then FNextSlideName:='';
    for I:=Index to L-2 do begin
      with TPWContainer(FParamSlidebarWires[I]) do begin
        FOutput.Layer:=I;
        Wire.FirstKnot:=RealPoint(20+FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*I));
      end;
    end;
  end;
  FPreset.DeleteParam(Index);
  //DeletePresetParam(FPreset,Index);
end;

procedure TPresetEditor.OutputRemoved(AObject: TObjectItem; Index: Integer);
var
  I,L: Integer;
begin
  with FPreset do begin
    L:=Length(Outputs);
    FNextSlideName:=Outputs[Index].Name;
    if IsDefaultOutputName(FNextSlideName) then FNextSlideName:='';
    for I:=Index to L-2 do begin
      with TPWContainer(FOutputSlidebarWires[I]) do begin
        FParam.Layer:=I;
        Wire.LastKnot:=RealPoint(Width-20-FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*I));
      end;
    end;
  end;
  FPreset.DeleteOutput(Index);
  //DeletePresetOutput(FPreset,Index);
end;

function TPresetEditor.NewWire: TPWContainer;
begin
  Result:=TPWContainer.Create(ZeroCenter,ZeroCenter,FWireSettings,poiUNDEFINED,poiUNDEFINED);
  FWires.Add(Result);
  Inc(FMovingCount);
  StartRender;
end;

procedure TPresetEditor.RewireAll;
var
  I: Integer;
begin
  for I:=0 to FViewPreset.Count-1
    do TViewPresetVis(FViewPreset[I]).Rewire;
  for I:=0 to FOutputSlidebarWires.Count-1
    do with TPWContainer(FOutputSlidebarWires[I]).Wire do LastKnot:=RealPoint(Width-20-FImageCenter.X,LastKnot.Y);
end;

procedure TPresetEditor.DrawSqrFunc(ACanvas: TCanvas; const ARect: TRect; const AFunc: TSqrFunc; X1,X2: Integer);
var
  I,AY: Integer;
begin
  AY:=Round(ExecFunc(AFunc,X1));
  ACanvas.MoveTo(X1-ARect.Left,AY-ARect.Top);
  for I:=X1+1 to X2 do begin
    AY:=Round(ExecFunc(AFunc,I));
    ACanvas.LineTo(I-ARect.Left,AY-ARect.Top);
  end;
end;

procedure TPresetEditor.LoadImage(Index: Integer);
begin
  if Index>=0 then FImages.GetBitmap(Index,FTempDrawBitmap) else begin
    with FTempDrawBitmap do begin
      Canvas.Pen.Color:=clGray;
      Canvas.Brush.Color:=clSilver;
      Canvas.Rectangle(0,0,Width,Height);
    end;
  end;
end;

function TPresetEditor.Highlighted(Layer,Param: TPresetIndex; IsOutput: Boolean): Boolean;
begin
  Result:=(Layer=FHighlightedParam.Param.Layer) and (Param=FHighlightedParam.Param.Param) and (IsOutput=FHighlightedParam.IsOutput);
end;

procedure TPresetEditor.LoadImage(Pluged,Highlighted: Boolean; AType: TVisOutputType);
var
  AIndex: Integer;
begin
  if Pluged then begin
    if Highlighted
      then AIndex:=FHighlightedPlugedImageIndex+AType
      else AIndex:=FPlugedImageIndex+AType;
  end else begin
    if Highlighted
      then AIndex:=FHighlightedUnplugedImageIndex+AType
      else AIndex:=FUnplugedImageIndex+AType;
  end;
  FImages.GetBitmap(AIndex,FTempDrawBitmap);
end;

procedure TPresetEditor.SetPositions;
var
  I: Integer;
begin
  FBottom:=20;
  FHeaderHeight:=FTempDrawBitmap.Height+10;
  FSingleHeight:=FTempDrawBitmap.Height+6;
  FPParams.SetWH(ParamEditWidth,FTempDrawBitmap.Height);
  for I:=0 to FViewPreset.Count-1 do with TViewPresetVis(FViewPreset[I]) do begin
    SetPosition(FLayerPosX,FBottom,LayerWidth);
    FBottom:=Position.Bottom+10;
  end;
end;

function TPresetEditor.GetViewPresetByLayer(Layer: TPresetIndex): TViewPresetVis;
begin
  if Layer>=0
    then Result:=TViewPresetVis(FViewPreset[Layer])
    else Result:=TViewPresetVis(FViewPreset[FTriggerStartIndex+(not Layer)]);
end;

procedure TPresetEditor.CreateViewList;
var
  I,LL,LT    : TPresetIndex;
  AWire      : TPWContainer;
  AViewPreset: TViewPresetVis;
begin
  EndNoRender;
  FParamSlidebarWires.OnClosed:=nil;
  FOutputSlidebarWires.OnClosed:=nil;
  FViewPreset.Clear;
  FParamSlidebarWires.Clear;
  FOutputSlidebarWires.Clear;
  FWires.Clear;
  if (FWireAtMouse<>nil) and OpenEndIsOutput then WireAtMouse.Destroy;
  RemoveMouseWire;
  if FPreset=nil then exit;
  LL:=Length(FPreset.Layers);
  FTriggerStartIndex:=LL;
  LT:=Length(FPreset.Triggers);
  FViewPreset.Clear;
  FViewPreset.AddEmptyItems(LL+LT);
  for I:=0 to LL-1 do FViewPreset[I]:=TViewPresetVis.Create(Self,FPreset.Layers[I],I);
  for I:=0 to LT-1 do FViewPreset[I+LL]:=TViewPresetVis.Create(Self,FPreset.Triggers[I],not I);
  SetPositions;
  for I:=0 to LL+LT-1 do TViewPresetVis(FViewPreset[I]).InitLayer;
  for I:=0 to Length(FPreset.Params)-1 do begin
    AWire:=NewWire;
    AWire.SlideConnector:=true;
    AWire.Param:=FPreset.Params[I].OutputInfo;
    AWire.FOutput.Param:=-2;
    AWire.FOutput.Layer:=FParamSlidebarWires.Count;
    if AWire.Param.Param>=-1 then begin
      AViewPreset:=ViewPresetByLayer[AWire.Param.Layer];
      {if AWire.Param.Layer>=0
        then AViewPreset:=TViewPresetVis(FViewPreset[AWire.Param.Layer])
        else AViewPreset:=TViewPresetVis(FViewPreset[LL+(not AWire.Param.Layer)]);}
      AViewPreset.AddParamCon(AWire,AWire.Param.Param);
      AWire.AType:=TVisualisation.ParamType(AViewPreset.FLayer^.VisID,AWire.Param.Param);
    end;
    AWire.Wire.FirstKnot:=RealPoint(20+FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*AWire.Output.Layer));
    AWire.Wire.KnotCount:=50;
    FParamSlidebarWires.AddItem(AWire);
  end;
  for I:=0 to Length(FPreset.Outputs)-1 do begin
    AWire:=NewWire;
    AWire.SlideConnector:=true;
    AWire.Output:=FPreset.Outputs[I].OutputInfo;
    AWire.FParam.Param:=-2;
    AWire.FParam.Layer:=FOutputSlidebarWires.Count;
    if AWire.Output.Param>=-1 then begin
      AViewPreset:=ViewPresetByLayer[AWire.Output.Layer];
      {if AWire.Output.Layer>=0
        then AViewPreset:=TViewPresetVis(FViewPreset[AWire.Output.Layer])
        else AviewPreset:=TViewPresetVis(FViewPreset[LL+(not AWire.Output.Layer)]);}
      AViewPreset.ConnectToOutput(AWire,AWire.Output.Param);
      AWire.AType:=TVisualisation.Visualisations(AViewPreset.FLayer^.VisID).OutputDesc[AWire.Output.Param].AType;
    end;
    AWire.Wire.LastKnot:=RealPoint(Width-20-FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*AWire.Param.Layer));
    AWire.Wire.KnotCount:=50;
    FOutputSlidebarWires.AddItem(AWire);
  end;
  FParamSlidebarWires.OnClosed:=@ParamRemoved;
  FOutputSlidebarWires.OnClosed:=@OutputRemoved;
  StartRender;
end;

function TPresetEditor.ViewPresetAtCursor(X,Y: Integer): TViewPresetVis;
var
  I,Y2: Integer;
begin
  if (X<FLayerPosX) or (X>FLayerPosX+LayerWidth) then begin
    Result:=nil;
    exit;
  end;

  Y2:=Y+FViewRect.Top;
  for I:=0 to FViewPreset.Count-1 do begin
    Result:=TViewPresetVis(FViewPreset[I]);
    if Result.Position.Bottom>=Y2 then begin
      if Result.Position.Top>Y2 then Result:=nil;
      exit;
    end;
  end;
  Result:=nil;
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

procedure TPresetEditor.DrawSlidebar(ACanvas: TCanvas; AViewRect: TRect; X,Y,AHeight: Integer; ATop,AMiddle,ABottom: TBitmap);
begin
  DrawSIButtonV(ACanvas,AViewRect,ATop,AMiddle,ABottom,X,Y,AHeight);
end;

procedure TPresetEditor.SelectionChanged;
begin
  FSelectionChanged:=true;
end;

{Events}

procedure TPresetEditor.Paint;
begin
  //inherited Paint;
  Canvas.Draw(0,0,FPaintBuffer);
  // do nothing
end;

procedure TPresetEditor.Draw;
var
  I,AY        : Integer;
  AItem       : TObjectListItem;
  AHighlighted: Boolean;
  AStr        : ^string;
  AHintSize   : TPoint;
begin
  if not FRendering then begin
    FPParams.OnPaint:=nil;
    FPParams.AutoRepaint:=false;
  end;
  inherited Paint;
  with FPaintBuffer do begin
    FHint.ClearHint;
    Canvas.AntialiasingMode:=amOn;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Color:=Color;
    Canvas.Brush.Color:=Color;
    //Canvas.Brush.Bitmap:=FBackgroundBitmap;
    Canvas.Rectangle(0,0,Width,Height);
    //Canvas.Brush.Bitmap:=nil;
    Canvas.Font.Color:=clBlack;
    if (FImages=nil) or (FPreset=nil) then begin
      Canvas.TextOut(0,0,'nothing to paint');
      exit;
    end;

    for I:=0 to FViewPreset.Count-1
      do with TViewPresetVis(FViewPreset[I])
        do Paint(Canvas,FViewRect);

    if (FHighlightedParam.Param.Param<>-2) or FHighlightedParam.IsOutput or (FHighlightedParam.Param.Layer>=0)
      then DrawSlidebar(Canvas,FViewRect,20,20,FBottom-30,FSlidebarTop,FSlidebar,FSlidebarBottom)
      else DrawSlidebar(Canvas,FViewRect,20,20,FBottom-30,FSlidebarTopH,FSlidebarH,FSlidebarBottomH);
    if (FHighlightedParam.Param.Param<>-2) or (not FHighlightedParam.IsOutput) or (FHighlightedParam.Param.Layer>=0)
      then DrawSlidebar(Canvas,FViewRect,Width-20-FSlidebarTop.Width,20,FBottom-30,FSlidebarTop,FSlidebar,FSlidebarBottom)
      else DrawSlidebar(Canvas,FViewRect,Width-20-FSlidebarTop.Width,20,FBottom-30,FSlidebarTopH,FSlidebarH,FSlidebarBottomH);

    for I:=0 to FParamSlidebarWires.Count-1 do with TPWContainer(FParamSlidebarWires[I]) do begin
      AHighlighted:=Highlighted(I,-2,false);
      LoadImage(true,AHighlighted,AType);
      AY:=Round(Wire.FirstKnot.Y)-FImageCenter.Y-FViewRect.Top;
      Canvas.Draw(20-FViewRect.Left,AY,FTempDrawBitmap);
      if AHighlighted then begin
        AStr:=@FPreset.Params[I].Name;
        AHintSize:=FHint.HintSize(AStr^);
        FHint.DrawHint(30-FViewRect.Left+FTempDrawBitmap.Width,AY-((AHintSize.Y-FTempDrawBitmap.Height) div 2),AHintSize,AStr^,Classes.Rect(10,0,0,0),true);
      end;
    end;
    for I:=0 to FOutputSlidebarWires.Count-1 do with TPWContainer(FOutputSlidebarWires[I]) do begin
      AHighlighted:=Highlighted(I,-2,true);
      LoadImage(true,AHighlighted,AType);
      AY:=Round(Wire.LastKnot.Y)-FImageCenter.Y-FViewRect.Top;
      Canvas.Draw(Width-FTempDrawBitmap.Width-20-FViewRect.Left,AY,FTempDrawBitmap);
      if AHighlighted then begin
        AStr:=@FPreset.Outputs[I].Name;
        AHintSize:=FHint.HintSize(AStr^);
        FHint.DrawHint(Width-FTempDrawBitmap.Width-30-FViewRect.Left-AHintSize.X,AY-((AHintSize.Y-FTempDrawBitmap.Height) div 2),AHintSize,AStr^,Classes.Rect(0,0,10,0),true);
      end;
    end;

    Canvas.Pen.Width:=3;
    AItem:=FWires.First;
    while AItem<>nil do begin
      TPWContainer(AItem.Content).Draw(Canvas,FViewRect);
      AItem:=AItem.Next;
    end;

    FHint.FinallyDrawHint;
  end;
  Canvas.Draw(0,0,FPaintBuffer);
  if not FRendering then begin
    FPParams.OnPaint:=@ParamPainted;
    FPParams.AutoRepaint:=true;
  end;
end;

procedure TPresetEditor.Resize;
var
  AOldViewRect: TRect;
begin
  inherited Resize;
  AOldViewRect:=FViewRect;
  with FViewRect do begin
    Left:=0;
    Right:=Width;
    Top:=FScrollPos;
    Bottom:=FScrollPos+Height;
  end;
  if AOldViewRect=FViewRect then exit;
  EndNoRender;
  FLayerPosX:=(Width-LayerWidth) div 2;
  FPaintBuffer.SetSize(Width,Height);
  SetPositions;
  RewireAll;
  StartRender;
end;

procedure TPresetEditor.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  AViewPreset  : TViewPresetVis;
  AOldSelection: TParamHighlightInfo;

  procedure CheckSelection;
  begin
    if FHint.CheckMouseOver(X,Y) then exit;
    if (X>=20-FViewRect.Left) and (X<=20-FViewRect.Left+FSlidebar.Width) and (Y>=20-FViewRect.Top) and (Y<=FBottom-FViewRect.Top) then begin
      with FHighlightedParam do begin
        Param.Param:=-2;
        IsOutput:=false;
        Param.Layer:=(Y-20+FViewRect.Top) div FTempDrawBitmap.Height;
        if Param.Layer>=FParamSlidebarWires.Count then Param.Layer:=-1;
      end;
      exit;
    end;
    if (X>=Width-20-FSlidebar.Width-FViewRect.Left) and (X<=Width-20-FViewRect.Left) and (Y>=20-FViewRect.Top) and (Y<=FBottom-FViewRect.Top) then begin
      with FHighlightedParam do begin
        Param.Param:=-2;
        IsOutput:=true;
        Param.Layer:=(Y-20+FViewRect.Top) div FTempDrawBitmap.Height;
        if Param.Layer>=FOutputSlidebarWires.Count then Param.Layer:=-1;
      end;
      exit;
    end;
    if FPParams.MouseMove(X+FViewRect.Left,Y+FViewRect.Top) then begin
      FHighlightedParam.Param:=poiUNDEFINED;
      exit;
    end;
    if AViewPreset<>nil
      then AViewPreset.MouseMove(FMouseIsDown,Shift,X+FViewRect.Left-AViewPreset.Position.Left,Y+FViewRect.Top-AViewPreset.Position.Top)
      else FHighlightedParam.Param.Param:=-3;
  end;

begin
  inherited MouseMove(Shift,X,Y);
  AOldSelection:=FHighlightedParam;
  AViewPreset:=ViewPresetAtCursor(X,Y);
  if FWireAtMouse<>nil then begin
    if OpenEndIsOutput
      then FWireAtMouse.Wire.FirstKnot:=RealPoint(FViewRect.Left+X,FViewRect.Top+Y)
      else FWireAtMouse.Wire.LastKnot:=RealPoint(FViewRect.Left+X,FViewRect.Top+Y);
  end;
  CheckSelection;
  if not FRendering then begin
    if FSelectionChanged or (AOldSelection<>FHighlightedParam) then Draw;
    if FWireAtMouse<>nil then StartRender;
  end;
  FSelectionChanged:=false;
end;

procedure TPresetEditor.Click;
begin
  inherited Click;
end;

procedure TPresetEditor.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  AViewPreset    : TViewPresetVis;
  AHadWireAtMouse: Boolean;

  procedure CheckMouseDown;
  var
    I,L            : Integer;
  begin
    if FHighlightedParam.Param.Param=-2 then begin
      if FWireAtMouse<>nil then begin
        if (FHighlightedParam.Param.Layer>=0) and (FHighlightedParam.IsOutput<>OpenEndIsOutput) then begin
          if FHighlightedParam.IsOutput then begin
            if WireAtMouse.Output.Param>=-1 then begin
              //InsertPresetOutput(FPreset,FHighlightedParam.Param.Layer,WireAtMouse.Output,FNextSlideName);
              FPreset.InsertOutput(FHighlightedParam.Param.Layer,WireAtMouse.Output,FNextSlideName);
              WireAtMouse.FParam.Param:=-2;
              WireAtMouse.FParam.Layer:=FHighlightedParam.Param.Layer;
              WireAtMouse.Wire.LastKnot:=RealPoint(Width-20-FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*FHighlightedParam.Param.Layer));
              WireAtMouse.SlideConnector:=true;
              FOutputSlidebarWires.InsertItem(WireAtMouse,FHighlightedParam.Param.Layer);
              for I:=FOutputSlidebarWires.Count-1 downto FHighlightedParam.Param.Layer+1 do with TPWContainer(FOutputSlidebarWires[I]) do begin
                FParam.Layer:=I;
                Wire.LastKnot:=RealPoint(Width-20-FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*I));
              end;
              RemoveMouseWire;
            end;
          end else begin
            if WireAtMouse.Param.Param>=-1 then begin
              FPreset.InsertParam(FHighlightedParam.Param.Layer,WireAtMouse.Param,FNextSlideName);
              WireAtMouse.FOutput.Param:=-2;
              WireAtMouse.FOutput.Layer:=FHighlightedParam.Param.Layer;
              WireAtMouse.Wire.FirstKnot:=RealPoint(20+FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*FHighlightedParam.Param.Layer));
              WireAtMouse.SlideConnector:=true;
              FParamSlidebarWires.InsertItem(WireAtMouse,FHighlightedParam.Param.Layer);
              for I:=FParamSlidebarWires.Count-1 downto FHighlightedParam.Param.Layer+1 do with TPWContainer(FParamSlidebarWires[I]) do begin
                FOutput.Layer:=I;
                Wire.FirstKnot:=RealPoint(20+FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*I));
              end;
              RemoveMouseWire;
            end;
          end;
        end else begin
          if OpenEndIsOutput then begin
            if WireAtMouse.Param.Param>=-1 then begin
              L:=Length(FPreset.Params);
              FPreset.AddParam(WireAtMouse.Param,FNextSlideName);
              WireAtMouse.FOutput.Param:=-2;
              WireAtMouse.FOutput.Layer:=L;
              WireAtMouse.Wire.FirstKnot:=RealPoint(20+FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*L));
              WireAtMouse.SlideConnector:=true;
              FParamSlidebarWires.AddItem(WireAtMouse);
              RemoveMouseWire;
            end;
          end else begin
            if WireAtMouse.Output.Param>=-1 then begin
              L:=Length(FPreset.Outputs);
              FPreset.AddOutput(WireAtMouse.Output,FNextSlideName);
              WireAtMouse.FParam.Param:=-2;
              WireAtMouse.FParam.Layer:=L;
              WireAtMouse.Wire.LastKnot:=RealPoint(Width-20-FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*L));
              WireAtMouse.SlideConnector:=true;
              FOutputSlidebarWires.AddItem(WireAtMouse);
              RemoveMouseWire;
            end;
          end;
        end;
      end else begin
        if FHighlightedParam.Param.Layer>=0 then begin
          if FHighlightedParam.IsOutput then begin
            FWireAtMouse:=TPWContainer(FOutputSlidebarWires[FHighlightedParam.Param.Layer]);
            FWireAtMouse.SlideConnector:=false;
            FWireAtMouse.Param:=poiUNDEFINED;
            FWireAtMouse.Wire.LastKnot:=RealPoint(X+FViewRect.Left,Y+FViewRect.Top);
            FOutputSlidebarWires.DeleteItem(FHighlightedParam.Param.Layer);
          end else begin
            FWireAtMouse:=TPWContainer(FParamSlidebarWires[FHighlightedParam.Param.Layer]);
            FWireAtMouse.SlideConnector:=false;
            FWireAtMouse.Output:=poiUNDEFINED;
            FWireAtMouse.Wire.FirstKnot:=RealPoint(X+FViewRect.Left,Y+FViewRect.Top);
            FParamSlidebarWires.DeleteItem(FHighlightedParam.Param.Layer);
          end;
        end else begin
          FWireAtMouse:=NewWire;
          FWireAtMouse.AType:=oUndefined;
          if FHighlightedParam.IsOutput then begin
            L:=Length(FPreset.Outputs);
            FWireAtMouse.Wire.LastKnot:=RealPoint(Width-20-FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*L));
            FWireAtMouse.Wire.FirstKnot:=RealPoint(X+FViewRect.Left,Y+FViewRect.Top);
            FWireAtMouse.FParam.Param:=-2;
            FWireAtMouse.FParam.Layer:=L;
            FWireAtMouse.Wire.KnotCount:=50;
            FPreset.AddOutput(poiUndefined);
            FOutputSlidebarWires.AddItem(FWireAtMouse);
          end else begin
            L:=Length(FPreset.Params);
            FWireAtMouse.Wire.FirstKnot:=RealPoint(20+FImageCenter.X,20+FImageCenter.Y+(FTempDrawBitmap.Width*L));
            FWireAtMouse.Wire.LastKnot:=RealPoint(X+FViewRect.Left,Y+FViewRect.Top);
            FWireAtMouse.FOutput.Param:=-2;
            FWireAtMouse.FOutput.Layer:=L;
            FWireAtMouse.Wire.KnotCount:=50;
            FPreset.AddParam(poiUndefined);
            FParamSlidebarWires.AddItem(FWireAtMouse);
          end;
        end;
      end;
      exit;
    end;

    AViewPreset:=ViewPresetAtCursor(X,Y);
    if AViewPreset<>nil
      then AViewPreset.MouseDown(Button,Shift,X+FViewRect.Left-AViewPreset.Position.Left,Y+FViewRect.Top-AViewPreset.Position.Top)
      else if FWireAtMouse<>nil then begin
        WireAtMouse.Destroy;
        RemoveMouseWire;
      end;
  end;

begin
  inherited MouseDown(Button,Shift,X,Y);
  SetFocus;
  AHadWireAtMouse:=(FWireAtMouse<>nil);
  FMouseIsDown:=true;
  FPParams.MouseDown;
  CheckMouseDown;
  if not Rendering then begin
    if AHadWireAtMouse<>(FWireAtMouse<>nil) then StartRender;
  end;
end;

procedure TPresetEditor.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  AViewPreset: TViewPresetVis;
begin
  inherited MouseUp(Button,Shift,X,Y);
  FMouseIsDown:=false;
  FPParams.MouseUp;
  AViewPreset:=ViewPresetAtCursor(X,Y);
  if AViewPreset<>nil then AViewPreset.MouseUp(Button,Shift,X+FViewRect.Left-AViewPreset.Position.Left,Y+FViewRect.Top-AViewPreset.Position.Top);
end;

procedure TPresetEditor.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TPresetEditor.MouseLeave;
begin
  FMouseIsDown:=false;
  FHighlightedParam.Param.Param:=-3;
  FPParams.MouseLeave;
  inherited MouseLeave;
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

{Get & Set Methods}

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
  if not FRendering then begin
    FPParams.SetViewRect(FViewRect);
    Draw;
  end;
end;

function TPresetEditor.GetMaxScrollPos: Integer;
begin
  if FViewPreset.Count>0 then begin
    Result:=TViewPresetVis(FViewPreset[FViewPreset.Count-1]).Position.Bottom-Height+20;
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

function TPresetEditor.GetOpenEndIsOutput: Boolean;
begin
  Result:=(FWireAtMouse.Param.Param>=-2);
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

{procedure TPresetEditor.SetCCVisible(Value: Boolean);
begin
  FTCC.Visible:=Value;
end;}

procedure TPresetEditor.SetImages(Value: TImageList);
begin
  FImages:=Value;
  if Value<>nil then begin
    FTempDrawBitmap.SetSize(Value.Width,Value.Height);
    FImageCenter:=Classes.Point(Value.Width div 2,Value.Height div 2);
  end else begin
    FTempDrawBitmap.SetSize(10,10);
    FImageCenter:=Classes.Point(5,5);
  end;
  SetSlidebarImageIndex(-1);
  SetHighlightedSlidebarImageIndex(-1);
  FPParams.Images:=Value;
  if not FRendering then Draw;
end;

procedure TPresetEditor.SetSlidebarImageIndex(Value: Integer);
begin
  if FImages=nil then Value:=-1;
  FSlidebarImageIndex:=Value;
  if FSlidebarImageIndex>=0
    then FImages.GetBitmap(Value,FTempDrawBitmap)
    else FTempDrawBitmap.Canvas.Rectangle(0,0,FTempDrawBitmap.Width-1,FTempDrawBitmap.Height-1);
  SplitImageV(FTempDrawBitmap,FSlidebarTop,FSlidebar,FSlidebarBottom);
end;

procedure TPresetEditor.SetHighlightedSlidebarImageIndex(Value: Integer);
begin
  if FImages=nil then Value:=-1;
  FHighlightedSlidebarImageIndex:=Value;
  if FHighlightedSlidebarImageIndex>=0
    then FImages.GetBitmap(Value,FTempDrawBitmap)
    else FTempDrawBitmap.Canvas.Rectangle(0,0,FTempDrawBitmap.Width-1,FTempDrawBitmap.Height-1);
  SplitImageV(FTempDrawBitmap,FSlidebarTopH,FSlidebarH,FSlidebarBottomH);
end;

procedure TPresetEditor.SetUnplugedImageIndex(Value: Integer);
begin
  FUnplugedImageIndex:=Value;
end;

procedure TPresetEditor.SetPlugedImageIndex(Value: Integer);
begin
  FPlugedImageIndex:=Value;
end;

{Allgemein}

procedure DrawWire(Canvas: TCanvas; ARect: TRect; AWire: TPhysicsWire);
var
  I : Integer;
  AP: TPoint;
begin
  if not InView(Rect(AWire.ViewRect),ARect) then exit;
  Canvas.MoveTo(Round(AWire.FirstKnot.X)-ARect.Left,Round(AWire.FirstKnot.Y)-ARect.Top);
  for I:=0 to AWire.KnotCount-1 do begin
    AP:=Classes.Point(Round(AWire.Knots[I].X)-ARect.Left,Round(AWire.Knots[I].Y)-ARect.Top);
    Canvas.LineTo(AP);
  end;
  Canvas.LineTo(Round(AWire.LastKnot.X)-ARect.Left,Round(AWire.LastKnot.Y)-ARect.Top);
end;

{operator <> (m1,m2: TParamHighlightInfo): Boolean;
begin
  Result:=(m1.IsOutput<>m2.IsOutput)
      or (m1.Param.Layer<>m2.Param.Layer)
      or (m1.Param.Param<>m2.Param.Param)
      or ((m1.Param=-3) xor (m2.Param=-3));
end;}

operator = (m1,m2: TParamHighlightInfo): Boolean;
begin
  Result:=((m1.IsOutput=m2.IsOutput)
      and (m1.Param.Layer=m2.Param.Layer)
      and (m1.Param.Param=m2.Param.Param))
      or ((m1.Param.Param=-3) and (m2.Param.Param=-3));
end;

end.

