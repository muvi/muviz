unit ParamPainter2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, VisType2, PresetUtil3_Configuration, ParamType2,
  AdvCoord, Controls, StdCtrls, Dialogs, SplitImageButtons, AdvFunc,
  Spin, Math, ColorChooseDlg, SlaveControl, ImageConfig, csl, HashMap,
  ParamTypeKey, ControlPlacer, PresetUtil3_Wires, MExceptions, MStrings,
  CanvasGraphX32, PresetUtil3_Connections, PresetUtil3_ConnectionRouter,
  PresetUtil3_ConnectedWireManager, PresetUtil3_Path, ResizeBlockableMaster,
  DragPlugUnit, StdPermissions;

type
  TParamPainter = class;
  TPPParam      = class;
  TPPParamClass = class of TPPParam;

  TPPParam      = class (TPlacingControl)
  private
    FParam       : IPParam;
    FPath        : IParamPath;
    FConnections : TRoutableConnections;
    FParentRouter: TConnectionRouter;
    function GetParamType: TPParamType; inline;
    function GetPainter: TParamPainter; inline;
  strict protected
    procedure FillBG; inline;
    function GetSubRouter: TConnectionRouter; virtual;
    function GetMinWidth: Integer; virtual;
    property Connections: TRoutableConnections read FConnections;
  protected
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure SetState(AValue: TPlacingControlState); override;
    procedure DoUpdate; virtual; abstract;
    procedure DoPaint; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    property Painter: TParamPainter read GetPainter;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; AParentRouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); virtual; reintroduce;
    destructor Destroy; override;
    //gets new values and shows them
    //this is NOT done automatically in the constructor
    //because a pointer parameter needs the other parameters being created first
    procedure Update; inline;
    //Updates this parameter and nested parameters, too
    procedure UpdateAll; virtual;
    //call after every parameter is created
    procedure ContextCreated; virtual;
    property Owner;
    property MinWidth: Integer read GetMinWidth;
    property ParentRouter: TConnectionRouter read FParentRouter;
    property Param: IPParam read FParam;
    property ParamType: TPParamType read GetParamType;
    property Path: IParamPath read FPath;
    property SubRouter: TConnectionRouter read GetSubRouter;
  end;

  //param which does an update in the constructor
  TPPUParam     = class (TPPParam)
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
  end;

  TSplittedBitmap     = record
    Left, Middle, Right: TBitmap;
  end;

  TParamPainterImages = class (TImageConfig)
  private
    FBoolBoxUI          : Integer;
    FBoolBoxUHI         : Integer;
    FBoolBoxDI          : Integer;
    FBoolBoxDHI         : Integer;
    FButtons            : array [TPlacingControlState] of Integer;
    //helper bmps
    FButtonBmps         : array [TPlacingControlState] of TSplittedBitmap;
    procedure SetButton(Index: TPlacingControlState; Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadImage(State: TPlacingControlState; Width: Integer; Dest: TBitmap); overload;
  published
    property BoolBoxUI: Integer read FBoolBoxUI write FBoolBoxUI;
    property BoolBoxUHI: Integer read FBoolBoxUHI write FBoolBoxUHI;
    property BoolBoxDI: Integer read FBoolBoxDI write FBoolBoxDI;
    property BoolBoxDHI: Integer read FBoolBoxDHI write FBoolBoxDHI;
    property ButtonUI: Integer index ppDefault read FButtons[ppDefault] write SetButton;
    property ButtonUHI: Integer index ppMouseOver read FButtons[ppMouseOver] write SetButton;
    property ButtonDI: Integer index ppDown read FButtons[ppDown] write SetButton;
  end;

  TParamPainter       = class (TEditPlacer)
  private
    FImages             : TParamPainterImages;
    FOwnsImages         : Boolean;
    //controls for Usage from Params
    FSpinEdit           : TSpinEdit;
    FFloatSpinEdit      : TFloatSpinEdit;
    FColorDlg           : TChooseColorDialog;
    FConfig             : TPresetEditorConfiguration;
    //needed because some params need it
    FWireManager        : TWireManager;
  public
    constructor Create(AOwner: TResizeBlockableMaster; AConfig: TPresetEditorConfiguration; AWireManager: TWireManager; AImages: TParamPainterImages = nil);
    destructor Destroy; override;
    function NewParam(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil): TPPParam;
    class function AddTypeEdit(AType: TPParamType; AEdit: TPPParamClass): Boolean;

    property ColorDlg: TChooseColorDialog read FColorDlg;
    property ValueSpinEdit: TSpinEdit read FSpinEdit;
    property ValueFloatSpinEdit: TFloatSpinEdit read FFloatSpinEdit;
  published
    property Config: TPresetEditorConfiguration read FConfig;
    property Images: TParamPainterImages read FImages;
    property WireManager: TWireManager read FWireManager;
  end;

  TPPPDummy       = class (TPPParam)
  strict private
    function GetTextWidth(const S: string): Integer;
  strict protected
    function GetMinWidth: Integer; override;
  protected
    procedure DoPaint; override;
    procedure DoUpdate; override;
  end;

const
  DEFAULTPARAMHEIGHT    = 21;

  PARAM_BGCOLOR         = $999999;

implementation

procedure ParamChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl; forward;

{%REGION TParamPainterImages}

constructor TParamPainterImages.Create;
var
  AState: TPlacingControlState;
begin
  inherited Create;
  FBoolBoxUI:=-1;
  FBoolBoxUHI:=-1;
  FBoolBoxDI:=-1;
  FBoolBoxDHI:=-1;
  for AState in TPlacingControlState do begin
    FButtons[AState]:=-1;
    FButtonBmps[AState].Left:=TBitmap.Create;
    FButtonBmps[AState].Middle:=TBitmap.Create;
    FButtonBmps[AState].Right:=TBitmap.Create;
  end;
end;

destructor TParamPainterImages.Destroy;
var
  AState: TPlacingControlState;
begin
  for AState in TPlacingControlState do begin
    FButtonBmps[AState].Left.Destroy;
    FButtonBmps[AState].Middle.Destroy;
    FButtonBmps[AState].Right.Destroy;
  end;
  inherited Destroy;
end;

procedure TParamPainterImages.LoadImage(State: TPlacingControlState; Width: Integer; Dest: TBitmap);
begin
  SIBtnImgH(Dest, FButtonBmps[State].Left, FButtonBmps[State].Middle, FButtonBmps[State].Right, Width);
end;

procedure TParamPainterImages.SetButton(Index: TPlacingControlState; Value: Integer);
begin
  Assert(Images<>nil);
  Assert(Value < Images.Count);

  FButtons[Index]:=Value;
  LoadImage(Value);
  SplitImageH(OutputBmp, FButtonBmps[Index].Left, FButtonBmps[Index].Middle, FButtonBmps[Index].Right);
end;

{%ENDREGION}
{%REGION TParamPainter}

var
  ParamTypeEdits: TMap;

constructor TParamPainter.Create(AOwner: TResizeBlockableMaster; AConfig: TPresetEditorConfiguration; AWireManager: TWireManager; AImages: TParamPainterImages = nil);
begin
  inherited Create(AOwner);
  FOwnsImages:=(AImages=nil);
  if FOwnsImages
    then FImages:=TParamPainterImages.Create
    else FImages:=AImages;
  FConfig:=AConfig;
  FWireManager:=AWireManager;

  FSpinEdit:=TSpinEdit.Create(Owner);
  FSpinEdit.Height:=DEFAULTPARAMHEIGHT;
  FSpinEdit.MaxValue:=MaxInt;
  FSpinEdit.MinValue:=-MaxInt;
  AddPlacableControl(FSpinEdit);

  FFloatSpinEdit:=TFloatSpinEdit.Create(Owner);
  FFloatSpinEdit.Height:=DEFAULTPARAMHEIGHT;
  FFloatSpinEdit.MaxValue:=Infinity;
  FFloatSpinEdit.MinValue:=NegInfinity;
  FFloatSpinEdit.DecimalPlaces:=4;
  AddPlacableControl(FFloatSpinEdit);

  ValueEdit.Height:=DEFAULTPARAMHEIGHT;

  FColorDlg:=TChooseColorDialog.Create(Owner);
  FColorDlg.AutoHide:=true;
end;

destructor TParamPainter.Destroy;
begin
  FColorDlg.Destroy;
  RemovePlacableControl(FFloatSpinEdit);
  RemovePlacableControl(FSpinEdit);

  if FOwnsImages
    then FImages.Destroy;
  inherited Destroy;
end;

function TParamPainter.NewParam(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil): TPPParam;
var
  AType: TPParamType;
  AKey : TPParamTypeKey;
  AItem: TObject;
begin
  AType:=AParam.ID.&Type;
  AKey:=TPParamTypeKey.Create(AType);
  AItem:=ParamTypeEdits.Items[AKey];
  if AItem<>nil
    //casting a class to an object is possible... tested.
    then Result:=TPPParamClass(AItem).Create(AOwner, AConnections, ARouter, Self, AParam, APath, AMaster, AAdditionalInformation)
    else Result:=TPPPDummy.Create(AOwner, AConnections, ARouter, Self, AParam, APath, AMaster, AAdditionalInformation);
  AKey.Destroy;
end;

class function TParamPainter.AddTypeEdit(AType: TPParamType; AEdit: TPPParamClass): Boolean;
var
  AKey: TPParamTypeKey;
begin
  AKey:=TPParamTypeKey.Create(AType);
  //TODO: is this cast really possible?
  Result:=not ParamTypeEdits.Contains(AKey);
  if Result
    then ParamTypeEdits.Add(AKey,TObject(AEdit))
    else AKey.Destroy;
end;

{%ENDREGION}
{%REGION TPPParam}

constructor TPPParam.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; AParentRouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  inherited Create(AOwner, APainter, AMaster);
  Assert(APath <> nil);
  FPath:=APath;
  FParentRouter:=AParentRouter;
  FConnections:=AConnections;
  FParam:=AParam;
  //FParam.AddListener(@ParamChanged, Self, VisualisationUtil.MainThread);
end;

destructor TPPParam.Destroy;
begin
  //FParam.RemoveListener(@ParamChanged, Self);
  //FParam is an interface
  FParam:=nil;
  inherited Destroy;
end;

procedure TPPParam.DoPaint;
begin
  inherited DoPaint;
  //to set a sensefull color for the params DoPaint method
  Canvas.Brush.Color:=PARAM_BGCOLOR;
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPParam.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if DragPlugUnit.DraggingValue and (DragPlugUnit.DraggedValue.ID.&Type = FParam.ID.&Type) then begin
    FParam.GetFrom(DragPlugUnit.StopDraggingValue, TPNOLIMIT, NULLVISID);
  end else inherited DoMouseDown(Button, Shift, X, Y);
end;

procedure TPPParam.SetState(AValue: TPlacingControlState);
begin
  //it is really annoying if things happen when you just try to get rid of a wire
  if Painter.WireManager.OpenConnector.Connections.Empty
    then inherited SetState(AValue);
end;

procedure TPPParam.FillBG; inline;
begin
  Canvas.Pen.Color:=PARAM_BGCOLOR;
  Canvas.Rectangle(AbsoluteRect);
end;

procedure TPPParam.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Height:=DEFAULTPARAMHEIGHT;
  ClickRect:=Rect(0, 0, ANewBounds.Width, ANewBounds.Height);
end;

function TPPParam.GetParamType: TPParamType; inline;
begin
  Result:=FParam.ID.&Type;
end;

function TPPParam.GetPainter: TParamPainter; inline;
begin
  Result:=TParamPainter(Placer);
end;

procedure TPPParam.Update; inline;
begin
  DoUpdate;
end;

procedure TPPParam.UpdateAll;
begin
  DoUpdate;
end;

procedure TPPParam.ContextCreated;
begin
  //do nothing
end;

function TPPParam.GetSubRouter: TConnectionRouter;
begin
  Result:=nil;
end;

function TPPParam.GetMinWidth: Integer;
begin
  Result:=50;
end;

{%ENDREGION}
{%REGION TPPUParam}

constructor TPPUParam.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster);
  DoUpdate;
end;

{%ENDREGION}
{%REGION TPPPDummy}

function TPPPDummy.GetMinWidth: Integer;
begin
  Result:=0;
end;

function TPPPDummy.GetTextWidth(const S: string): Integer;
begin
  Result:=Canvas.TextWidth(S);
end;

procedure TPPPDummy.DoPaint;
var
  AY   : Integer;
  AType: IPParamType;
begin
  inherited DoPaint;
  Canvas.Brush.Style:=bsClear;
  AY:=(Height-Canvas.TextHeight('Wg')) div 2;
  Canvas.Font.Style:=[fsItalic];
  Canvas.Font.Color:=$000099;
  AType:=ParamTypeUtil[Param.ID.&Type];
  if AType<>nil
    then Canvas.TextOut(AbsoluteRect.Left,AbsoluteRect.Top+AY, StrShorten('editing of type '+AType.Name+' not possible.', @GetTextWidth, Width))
    else Canvas.TextOut(AbsoluteRect.Left,AbsoluteRect.Top+AY, StrShorten('unknown type.', @GetTextWidth, Width));
  Canvas.Font.Style:=[];
  Canvas.Font.Color:=clBlack;
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPDummy.DoUpdate;
begin
  //do nothing
end;

{%ENDREGION}
{%REGION Notifications}

procedure ParamChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TPPParam(Context).Update;
end;

{%ENDREGION}

initialization
  ParamTypeEdits:=THashMap.Create;
finalization
  //do not clean here... The hashmap contains classes which are automatically destroyed
  ParamTypeEdits.Destroy;
end.

