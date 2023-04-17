(*
  Controls, which can have non-real (directly painted on canvas) subcontrols
  makes wires between controls and other things possible
*)

unit SlaveControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic, Controls, AdvCoord, Graphics,
  ObjectClasses, AdvFunc, TouchControl;

type
  TSlaveControl           = class;
  TSlaveControlEvent      = procedure (Sender: TSlaveControl) of object;
  TMouseLeaveIgnoreHandler= function : Boolean of object;

  TMouseMoveData          = record
    Shift: TShiftState;
    Pos  : TPoint;
  end;

(*
  Rects:
  - ViewRect: the visible area of the active control
    (if it is (0, 0, width, height), the whole control is visible)
  - BoundsRect: the position of the control relative to its owner
  - AbsoluteRect: the absolute Position of the control

  scrolling is a combination of change of the ViewRect and the AbsoluteRect
*)

  TMasterControl          = class (TTouchControl)
  strict private
    FLocked                       : Boolean;
    FScrollPos                    : TPoint;
    FAbsoluteMaxScrollPos         : TPoint;
    FMouseIsDown                  : Boolean;
    FMouseMoveData                : TMouseMoveData;
    //events
    FOnMaxScrollPosChanged        : TNotifyEvent;
    FOnVirtualBoundsChanged       : TNotifyEvent;
    //repaint list
    FRepaintCtrl                  : TSlaveControl;
    FRINEventCount                : Integer;
    FRepaintNecessary             : Boolean;
    //control list
    FSlaveControls                : TObjectList;
    //Buffers
    FPaintBuffer                  : TBitmap;
    //Rects
    FViewRect                     : TRect;
    FAbsoluteRect                 : TRect;
    //Ignore mouse leave events
    FIgnoreMouseLeave             : TMouseLeaveIgnoreHandler;

    procedure SetScrollPos(Value: TPoint);
    procedure SetAbsoluteMaxScrollPos(Value: TPoint);
    //gets the relative max. Scollposition: result:=ScrollPos+VirtualBounds.Pos
    function GetMaxScrollPos: TPoint;

    procedure UpdateViewRect;
    procedure UpdateAbsoluteRect;
    procedure SetAbsoluteRect(Value: TRect);
    procedure SetViewRect(Value: TRect);
    function GetIgnoreMouseLeave: Boolean; inline;
  private
    FControlUnderMouse            : TSlaveControl;
    procedure ReCheckMouseMove;
    function ViewRectPtr: PRect; inline;
    function AbsoluteRectPtr: PRect; inline;
  strict protected
    property IgnoreMouseLeave: Boolean read GetIgnoreMouseLeave;
    property PaintBuffer: TBitmap read FPaintBuffer;
    property RepaintNecessary: Boolean read FRepaintNecessary;
  protected
    //use instead of direct call to Paint
    procedure NeedsPaint; virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoResize; virtual;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;

    procedure AddSlaveControl(ACtrl: TSlaveControl);
    property AbsoluteMaxScrollPos: TPoint read FAbsoluteMaxScrollPos write SetAbsoluteMaxScrollPos;
    property DAbsoluteRect: TRect read FAbsoluteRect write SetAbsoluteRect;
    property MouseIsDown: Boolean read FMouseIsDown;
    property SlaveControls: TObjectList read FSlaveControls;
    property ViewRect: TRect read FViewRect write SetViewRect;
  public
    //AInitialDraw: if draw should be called after construction
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    //if you really want to paint, use Draw
    procedure Draw; virtual;
    procedure Repaint(ACtrl: TSlaveControl); reintroduce;
    //Repaint If Necessary
    procedure RIN_StartEvent; inline;
    procedure RIN;
    //ignore mouse leave event (used for overlay controls)
    procedure SetMouseLeaveIgnoreHandler(AHandler: TMouseLeaveIgnoreHandler);
    procedure RemoveMouseLeaveIgnoreHandler(AHandler: TMouseLeaveIgnoreHandler);

    property AbsoluteRect: TRect read FAbsoluteRect;
    property ControlUnderMouse: TSlaveControl read FControlUnderMouse;
    property MaxScrollPos: TPoint read GetMaxScrollPos;
    property ScrollPos: TPoint read FScrollPos write SetScrollPos;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property Color default clBtnFace;
    property Constraints;
    property Enabled;
    property Font;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnMaxScrollPosChanged: TNotifyEvent read FOnMaxScrollPosChanged write FOnMaxScrollPosChanged;
    property OnResize;
    property OnShowHint;
    property OnVirtualBoundsChanged: TNotifyEvent read FOnVirtualBoundsChanged write FOnVirtualBoundsChanged;
  end;

  TSlaveControl           = class (TObjectItem)
  strict private
    FOwner                  : TSlaveControl;
    FMaster                 : TMasterControl;
    FControls               : TObjectList;
    FVisible                : Boolean;
  private
    FCanvas                 : TCanvas;
    FOnBoundsChanged        : TSlaveControlEvent;
    //Rects
    FAbsoluteRect           : TRect;
    FBoundsRect             : TBoundsRect;
    FViewRect               : TRect;
    //rects of owner
    FOwnersViewRect         : PRect;
    FOwnersAbsoluteRect     : PRect;

    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure BoundsChanged(var ANewBounds: TBoundsRect; AForceReset: Boolean = false); inline;
    //Rects
    procedure UpdateViewRect;
    procedure UpdateAbsoluteRect;
    procedure SetAbsoluteRect(Value: TRect);
    procedure SetBoundsRect(Value: TRect);
    procedure SetViewRect(Value: TRect);

    function GetHighlighted: Boolean;
    procedure SetVisible(Value: Boolean);
  private
    //inline repaint list
    FRepaintNecessary       : Boolean;
    FRepaintCtrl            : TSlaveControl;
    procedure PaintIfNecessary;
    function ViewRectPtr: PRect; inline;
    function AbsoluteRectPtr: PRect; inline;
  strict protected
    procedure DoAbsolutePositionChanged; virtual;
    procedure DoAlign; virtual;
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoPaint; virtual; abstract;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); virtual;
    procedure Repaint; virtual;
    function IsClickThrough: Boolean; virtual;
    property Canvas: TCanvas read FCanvas;
    property ClickThrough: Boolean read IsClickThrough;
  protected
    procedure AddControl(ACtrl: TSlaveControl);
    property AbsoluteRect: TRect read FAbsoluteRect write SetAbsoluteRect;
    property Controls: TObjectList read FControls;
    property Master: TMasterControl read FMaster;
    property Owner: TSlaveControl read FOwner;
    property ViewRect: TRect read FViewRect write SetViewRect;
  public
    constructor Create(AOwner: TSlaveControl; AMaster: TMasterControl = nil); virtual; reintroduce;
    destructor Destroy; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
    procedure CheckMouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure Paint;
    procedure MouseEnter; inline;
    procedure MouseLeave; inline;

    property BoundsRect: TRect read FBoundsRect.Rect write SetBoundsRect;
    property Height: Integer read GetHeight write SetHeight;
    property Highlighted: Boolean read GetHighlighted;
    property Left: Integer read FBoundsRect.Rect.Left write SetLeft;
    property Top: Integer read FBoundsRect.Rect.Top write SetTop;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Width: Integer read GetWidth write SetWidth;

    property OnBoundsChanged: TSlaveControlEvent read FOnBoundsChanged write FOnBoundsChanged;
  end;

function MouseMoveData(AX: Integer = 0; AY: Integer = 0; AShift: TShiftState = []): TMouseMoveData;

implementation

{%REGION TMasterControl}

{%REGION Create & Destroy}

constructor TMasterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocked:=true;

  FControlUnderMouse:=nil;
  Color:=clBtnFace;
  TabStop:=true;
  FMouseIsDown:=false;
  FIgnoreMouseLeave:=nil;
  FAbsoluteMaxScrollPos:=Point(0,0);
  FMouseMoveData.Shift:=[];
  FMouseMoveData.Pos:=ZEROPOINT;
  FRepaintCtrl:=nil;
  FRINEventCount:=0;
  FRepaintNecessary:=false;
  FPaintBuffer:=TBitmap.Create;
  FSlaveControls:=TObjectList.Create(true);
  //sets AbsoluteRect and ViewRect, too
  ScrollPos:=Point(0,0);

  FLocked:=false;
end;

destructor TMasterControl.Destroy;
begin
  FSlaveControls.Destroy;
  FPaintBuffer.Destroy;
  inherited Destroy;
end;

procedure TMasterControl.AddSlaveControl(ACtrl: TSlaveControl);
begin
  SlaveControls.Add(ACtrl);
  ACtrl.FCanvas:=FPaintBuffer.Canvas;
end;

{%ENDREGION}
{%REGION Events}

procedure TMasterControl.RIN_StartEvent;
begin
  Inc(FRINEventCount);
end;

procedure TMasterControl.RIN;
begin
  Dec(FRINEventCount);
  if (FRINEventCount>0) or (not FRepaintNecessary)
    then exit;
  FRepaintNecessary:=false;
  if FRepaintCtrl=nil then begin
    Draw;
    exit;
  end;
  FRepaintCtrl.PaintIfNecessary;
  NeedsPaint;
end;

procedure TMasterControl.Repaint(ACtrl: TSlaveControl);
var
  AOwner: TSlaveControl;
begin
  //check if RIN event started
  if FRINEventCount<1 then begin
    if ACtrl<>nil then begin
      ACtrl.Paint;
      NeedsPaint;
    end else Draw;
    exit;
  end;
  //check if master already needs to be repainted
  if FRepaintNecessary and (FRepaintCtrl=nil)
    then exit;
  //check if a higher level control already needs to be repainted
  AOwner:=ACtrl;
  while AOwner<>nil do begin
    if AOwner.FRepaintNecessary
      then exit;
    AOwner:=AOwner.Owner;
  end;
  //check if master should be repainted
  if ACtrl=nil then begin
    FRepaintNecessary:=true;
    FRepaintCtrl:=nil;
    exit;
  end else begin
    if not FRepaintNecessary then begin
      FRepaintNecessary:=true;
      //set to nil, because the last item needs to be nil
      FRepaintCtrl:=nil;
    end;
    ACtrl.FRepaintNecessary:=true;
    ACtrl.FRepaintCtrl:=FRepaintCtrl;
    FRepaintCtrl:=ACtrl;
  end;
end;

procedure TMasterControl.NeedsPaint;
begin
  Paint;
end;

procedure TMasterControl.Paint;
begin
  //only paint the buffer
  //used to avoid unnecessary redrawing
  Canvas.Draw(0,0,FPaintBuffer);
end;

procedure TMasterControl.Draw;
var
  AItem   : TObjectListItem;
  AControl: TSlaveControl;
begin
  if FLocked then exit;
  FRepaintNecessary:=false;
  AItem:=FSlaveControls.First;
  while AItem<>nil do begin
    AControl:=TSlaveControl(AItem.Content);
    AControl.Paint;
    AItem:=AItem.Next;
  end;
  NeedsPaint;
end;

procedure TMasterControl.Resize;
var
  AMaxScrollPos: TPoint;
begin
  RIN_StartEvent;
  Repaint(nil);
  inherited Resize;
  AMaxScrollPos:=MaxScrollPos;
  //if the resizing forces the window into another scrollposition
  //updates AbsoluteRect and ViewRect, too
  ScrollPos:=Point(IntCut(FScrollPos.X, 0, AMaxScrollPos.X), IntCut(FScrollPos.Y, 0, AMaxScrollPos.Y));
  FPaintBuffer.SetSize(Width,Height);
  //MaxScrollPos may have Changed because of greater size (not the AbsoluteMaxScrollPos...)
  if Assigned(FOnMaxScrollPosChanged)
    then FOnMaxScrollPosChanged(Self);
  DoResize;
  RIN;
end;

procedure TMasterControl.DoResize;
begin
  //do nothing
end;

function TMasterControl.GetIgnoreMouseLeave: Boolean; inline;
begin
  Result:=(FIgnoreMouseLeave <> nil) and FIgnoreMouseLeave();
end;

procedure TMasterControl.SetMouseLeaveIgnoreHandler(AHandler: TMouseLeaveIgnoreHandler);
begin
  Assert(FIgnoreMouseLeave = nil);
  FIgnoreMouseLeave:=AHandler;
end;

procedure TMasterControl.RemoveMouseLeaveIgnoreHandler(AHandler: TMouseLeaveIgnoreHandler);
begin
  Assert(FIgnoreMouseLeave = AHandler);
  FIgnoreMouseLeave:=nil;
end;

procedure TMasterControl.ReCheckMouseMove;
begin
  MouseMove(FMouseMoveData.Shift,FMouseMoveData.Pos.X,FMouseMoveData.Pos.Y);
end;

procedure TMasterControl.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  AItem       : TObjectListItem;
  AOldControlUnderMouse: TSlaveControl;
begin
  RIN_StartEvent;
  inherited MouseMove(Shift,X,Y);
  if not FMouseIsDown then begin
    AOldControlUnderMouse:=FControlUnderMouse;
    FControlUnderMouse:=nil;
    //check subcontrols
    AItem:=FSlaveControls.Last;
    while AItem<>nil do begin
      TSlaveControl(AItem.Content).CheckMouseMove(Shift,X,Y);
      if FControlUnderMouse<>nil
        then break;
      AItem:=AItem.Prev;
    end;
    //Mouse Enter / Leave
    if FControlUnderMouse<>AOldControlUnderMouse then begin
      if AOldControlUnderMouse<>nil
        then AOldControlUnderMouse.MouseLeave;
      if FControlUnderMouse<>nil
        then FControlUnderMouse.MouseEnter;
    end;
  end;

  FMouseMoveData:=MouseMoveData(X,Y,Shift);
  //execute the SlaveControls event
  if FControlUnderMouse<>nil
    then FControlUnderMouse.MouseMove(Shift, X, Y);
  RIN;
end;

procedure TMasterControl.Click;
begin
  inherited Click;
end;

procedure TMasterControl.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  RIN_StartEvent;
  inherited MouseDown(Button,Shift,X,Y);
  SetFocus;
  FMouseIsDown:=true;
  if FControlUnderMouse<>nil
    then FControlUnderMouse.MouseDown(Button,Shift,X,Y);
  RIN;
end;

procedure TMasterControl.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  RIN_StartEvent;
  inherited MouseUp(Button,Shift,X,Y);
  FMouseIsDown:=false;
  if FControlUnderMouse<>nil
    then FControlUnderMouse.MouseUp(Button,Shift,X,Y);
  RIN;
end;

procedure TMasterControl.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TMasterControl.MouseLeave;
begin
  if not IgnoreMouseLeave then begin
    RIN_StartEvent;
    FMouseIsDown:=false;
    if FControlUnderMouse<>nil then FControlUnderMouse.MouseLeave;
    FControlUnderMouse:=nil;
    inherited MouseLeave;
    RIN;
  end;
end;

procedure TMasterControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
end;

procedure TMasterControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key,Shift);
end;

procedure TMasterControl.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
end;

{%ENDREGION}
{%REGION Getters & Setters}

function TMasterControl.ViewRectPtr: PRect;
begin
  Result:=@FViewRect;
end;

function TMasterControl.AbsoluteRectPtr: PRect;
begin
  Result:=@FAbsoluteRect;
end;

procedure TMasterControl.UpdateViewRect;
begin
  ViewRect:=Rect(FScrollPos, Point(Width, Height));
end;

procedure TMasterControl.UpdateAbsoluteRect;
begin
  //no size limit
  DAbsoluteRect:=Rect(-FScrollPos.X, -FScrollPos.Y, MaxInt, MaxInt);
end;

procedure TMasterControl.SetAbsoluteRect(Value: TRect);
var
  AItem: TObjectListItem;
begin
  FAbsoluteRect:=Value;
  //update children
  AItem:=FSlaveControls.First;
  while AItem<>nil do begin
    TSlaveControl(AItem.Content).UpdateAbsoluteRect;
    AItem:=AItem.Next;
  end;
end;

procedure TMasterControl.SetViewRect(Value: TRect);
var
  AItem: TObjectListItem;
begin
  FViewRect:=Value;
  //update children
  AItem:=FSlaveControls.First;
  while AItem<>nil do begin
    TSlaveControl(AItem.Content).UpdateViewRect;
    AItem:=AItem.Next;
  end;
end;

procedure TMasterControl.SetAbsoluteMaxScrollPos(Value: TPoint);
begin
  if FAbsoluteMaxScrollPos<>Value then begin
    FAbsoluteMaxScrollPos:=Value;
    if Assigned(OnMaxScrollPosChanged)
      then OnMaxScrollPosChanged(Self);
  end;
end;

procedure TMasterControl.SetScrollPos(Value: TPoint);
var
  AMaxScrollPos: TPoint;
begin
  //do not get this twice
  AMaxScrollPos:=MaxScrollPos;
  FScrollPos:=Point(IntCut(Value.X, 0, AMaxScrollPos.X), IntCut(Value.Y, 0, AMaxScrollPos.Y));
  UpdateViewRect;
  UpdateAbsoluteRect;
  Draw;
end;

function TMasterControl.GetMaxScrollPos: TPoint;
begin
  Result:=FAbsoluteMaxScrollPos-Point(Width, Height);
  if Result.X<0
    then Result.X:=0;
  if Result.Y<0
    then Result.Y:=0;
end;

{%ENDREGION}

{%ENDREGION}
{%REGION TSlaveControl}

{%REGION Create & Destroy}

constructor TSlaveControl.Create(AOwner: TSlaveControl; AMaster: TMasterControl = nil);
begin
  inherited Create;
  FVisible:=true;
  FOwner:=AOwner;
  FControls:=TObjectList.Create(true);
  if AOwner<>nil then begin
    Assert(AMaster=nil);
    FMaster:=AOwner.Master;
    AOwner.AddControl(Self);
    FOwnersViewRect:=AOwner.ViewRectPtr;
    FOwnersAbsoluteRect:=AOwner.AbsoluteRectPtr;
  end else begin
    Assert(AMaster<>nil);
    FMaster:=AMaster;
    AMaster.AddSlaveControl(Self);
    FOwnersViewRect:=AMaster.ViewRectPtr;
    FOwnersAbsoluteRect:=AMaster.AbsoluteRectPtr;
  end;
  FRepaintCtrl:=nil;
  //FNextRepaintCtrl:=nil;
  FRepaintNecessary:=false;
  //set initial bounds
  //FBoundsRect:=AInitialBounds;
  //setzt ViewRect und AbsolutePos automatisch mit
  //BoundsChanged(FBoundsRect, true);
end;

destructor TSlaveControl.Destroy;
begin
  if Master.ControlUnderMouse = Self
    then Master.FControlUnderMouse:=nil;
  FControls.Destroy;
  inherited Destroy;
end;

procedure TSlaveControl.AddControl(ACtrl: TSlaveControl);
begin
  Controls.Add(ACtrl);
  ACtrl.FCanvas:=FCanvas;
end;

{%ENDREGION}
{%REGION Getters & Setters}

function TSlaveControl.GetHeight: Integer;
begin
  Result:=FBoundsRect.Height;
end;

function TSlaveControl.GetWidth: Integer;
begin
  Result:=FBoundsRect.Width;
end;

procedure TSlaveControl.SetHeight(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Height:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TSlaveControl.SetLeft(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Left:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TSlaveControl.SetTop(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Top:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TSlaveControl.SetWidth(Value: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds:=FBoundsRect;
  ANewBounds.Width:=Value;
  BoundsChanged(ANewBounds);
end;

procedure TSlaveControl.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
var
  ANewBounds: TBoundsRect;
begin
  ANewBounds.SetBounds(ALeft,ATop,AWidth,AHeight);
  BoundsChanged(ANewBounds);
end;

procedure TSlaveControl.UpdateViewRect;
begin
  ViewRect:=FitTo(FBoundsRect.Rect, FOwnersViewRect^)-FBoundsRect.Pos;
end;

procedure TSlaveControl.UpdateAbsoluteRect;
var
  AOldAbsoluteRect: TRect;
begin
  AOldAbsoluteRect:=AbsoluteRect;
  AbsoluteRect:=FBoundsRect.Rect+FOwnersAbsoluteRect^.TopLeft;
  if AbsoluteRect<>AOldAbsoluteRect
    then DoAbsolutePositionChanged;
end;

procedure TSlaveControl.BoundsChanged(var ANewBounds: TBoundsRect; AForceReset: Boolean = false);
begin
  Master.RIN_StartEvent;
  DoSetBounds(ANewBounds);
  if (FBoundsRect<>ANewBounds) or AForceReset then begin
    FBoundsRect:=ANewBounds;
    UpdateViewRect;
    UpdateAbsoluteRect;
    DoAlign;
    if Assigned(FOnBoundsChanged)
      then FOnBoundsChanged(Self);
  end;
  Master.RIN;
end;

procedure TSlaveControl.SetAbsoluteRect(Value: TRect);
var
  AItem: TObjectListItem;
begin
  FAbsoluteRect:=Value;
  //update children
  AItem:=FControls.First;
  while AItem<>nil do begin
    TSlaveControl(AItem.Content).UpdateAbsoluteRect;
    AItem:=AItem.Next;
  end;
end;

procedure TSlaveControl.SetBoundsRect(Value: TRect);
var
  ANewRect: TBoundsRect;
begin
  ANewRect:=Value;
  BoundsChanged(ANewRect);
end;

procedure TSlaveControl.SetViewRect(Value: TRect);
var
  AItem: TObjectListItem;
begin
  FViewRect:=Value;
  //update children
  AItem:=FControls.First;
  while AItem<>nil do begin
    TSlaveControl(AItem.Content).UpdateViewRect;
    AItem:=AItem.Next;
  end;
end;

function TSlaveControl.GetHighlighted: Boolean;
begin
  Result:=Master.ControlUnderMouse = Self;
end;

procedure TSlaveControl.SetVisible(Value: Boolean);
begin
  if FVisible<>Value then begin
    FVisible:=Value;
    Repaint;
  end;
end;

{%ENDREGION}
{%REGION Painting}

procedure TSlaveControl.Repaint;
begin
  FMaster.Repaint(Self);
end;

procedure TSlaveControl.PaintIfNecessary;
begin
  //FRepaintNecessary may be set to nil by painting subcontrols
  if FRepaintNecessary
    then Paint;
  if FRepaintCtrl<>nil
    then FRepaintCtrl.PaintIfNecessary;
end;

function TSlaveControl.ViewRectPtr: PRect;
begin
  Result:=@FViewRect;
end;

function TSlaveControl.AbsoluteRectPtr: PRect;
begin
  Result:=@FAbsoluteRect;
end;

procedure TSlaveControl.Paint;
var
  AItem: TObjectListItem;
begin
  //Do NOT set the FRepaintControl to nil here. You may be in a repaint list.
  FRepaintNecessary:=false;
  if FVisible and (FBoundsRect><FOwnersViewRect^) then begin
    DoPaint;
    //paint subcontrols
    AItem:=FControls.First;
    while AItem<>nil do begin
      TSlaveControl(AItem.Content).Paint;
      AItem:=AItem.Next;
    end;
  end;
end;

{%ENDREGION}
{%REGION Mouse}

function TSlaveControl.IsClickThrough: Boolean;
begin
  Result:=false;
end;

procedure TSlaveControl.MouseEnter; inline;
begin
  DoMouseEnter;
end;

procedure TSlaveControl.MouseLeave; inline;
begin
  DoMouseLeave;
end;

procedure TSlaveControl.CheckMouseMove(Shift: TShiftState; X,Y: Integer);
var
  AItem: TObjectListItem;
begin
  if Master.MouseIsDown then exit;
  if Point(X,Y)><FAbsoluteRect then begin
    //check subcontrols
    //start with last because it's the one in the front
    AItem:=FControls.Last;
    while AItem<>nil do begin
      TSlaveControl(AItem.Content).CheckMouseMove(Shift, X, Y);
      if Master.ControlUnderMouse<>nil
        then break;
      AItem:=AItem.Prev;
    end;
    if (Master.ControlUnderMouse=nil) and (not ClickThrough)
      then Master.FControlUnderMouse:=Self;
  end;
end;

procedure TSlaveControl.MouseMove(Shift:TShiftState; X,Y:Integer);
begin
  DoMouseMove(Shift,X-FAbsoluteRect.Left,Y-FAbsoluteRect.Top);
end;

procedure TSlaveControl.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  DoMouseDown(Button,Shift,X-FAbsoluteRect.Left,Y-FAbsoluteRect.Top);
end;

procedure TSlaveControl.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  DoMouseUp(Button,Shift,X-FAbsoluteRect.Left,Y-FAbsoluteRect.Top);
  Master.ReCheckMouseMove;
end;

procedure TSlaveControl.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  //do nothing
end;

procedure TSlaveControl.DoAbsolutePositionChanged;
begin
  //do nothing
end;

procedure TSlaveControl.DoAlign;
begin
  //do nothing
end;

procedure TSlaveControl.DoMouseMove(Shift: TShiftState; X,Y: Integer);
begin
  //do nothing
end;

procedure TSlaveControl.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //do nothing
end;

procedure TSlaveControl.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //do nothing
end;

procedure TSlaveControl.DoMouseEnter;
begin
  Repaint;
end;

procedure TSlaveControl.DoMouseLeave;
begin
  Repaint;
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

{%ENDREGION}

end.

