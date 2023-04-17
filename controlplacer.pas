unit ControlPlacer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, AdvCoord, Controls, StdCtrls, Dialogs, GraphX32,
  AdvFunc, SlaveControl, LCLIntf, ResizeBlockableMaster;

type
  TPlacingControlState = (ppDefault, ppMouseOver, ppDown);
  TControlPlacer       = class;

  TPlacingControl = class (TSlaveControl)
  private
    FPlacer    : TControlPlacer;
    FState     : TPlacingControlState;
    //limits the clickable area
    FClickRect : TRect;
  protected
    procedure SetState(AValue: TPlacingControlState); virtual;
    procedure DoPaint; override;
    procedure DoAbsolutePositionChanged; override;
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseLeave; override;
    procedure DoMouseEnter; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoSetState(ANewState: TPlacingControlState); virtual;
    procedure UpdateControlBounds; inline;
    procedure ControlChanged; virtual;
    //initialize edits here to prevent weird value changes
    procedure UsingStarted; virtual;
    procedure UsingEnded; virtual;
    function IsUsingControl: Boolean;

    property ClickRect: TRect read FClickRect write FClickRect;
    property Placer: TControlPlacer read FPlacer;
  public
    constructor Create(AOwner: TSlaveControl; APlacer: TControlPlacer; AMaster: TMasterControl = nil); reintroduce;
    destructor Destroy; override;
    property State: TPlacingControlState read FState;
  end;

  TControlPlacer  = class
  private
    FOwner              : TResizeBlockableMaster;
    FActiveControl      : TWinControl;
    FPlacedBy           : TPlacingControl;
    function GetControlFocused: Boolean;
    //needed because the control immediately disappears after showing
    //because of the MouseLeave event
    function GetControlMouseOver: Boolean;
  protected
    procedure AddPlacableControl(AControl: TWinControl); virtual;
    procedure RemovePlacableControl(AControl: TWinControl); virtual;
    procedure FControlEnter(Sender: TObject); virtual;
    procedure FControlExit(Sender: TObject); virtual;
    procedure FControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoStartUse; virtual;
    procedure DoEndUse; virtual;
    function UsingControl: Boolean;

    property Owner: TResizeBlockableMaster read FOwner;
  public
    constructor Create(AOwner: TResizeBlockableMaster);
    destructor Destroy; override;
    function StartUse(AControl: TWinControl; APlacedBy: TPlacingControl; AFocused: Boolean = false): Boolean;
    procedure EndUse;
    property ActiveControl: TWinControl read FActiveControl;
    property ControlFocused: Boolean read GetControlFocused;
    property ControlMouseOver: Boolean read GetControlMouseOver;
    property PlacedBy: TPlacingControl read FPlacedBy;
  end;

  TEditPlacer     = class (TControlPlacer)
  private
    FEdit: TEdit;
  protected
    procedure FControlChange(Sender: TObject); virtual;
    procedure DoStartUse; override;
    procedure DoEndUse; override;
  public
    constructor Create(AOwner: TResizeBlockableMaster);
    destructor Destroy; override;

    property ValueEdit: TEdit read FEdit;
  end;

implementation

{%REGION TControlPlacer}

constructor TControlPlacer.Create(AOwner: TResizeBlockableMaster);
begin
  inherited Create;
  FOwner:=AOwner;
  FActiveControl:=nil;
  FPlacedBy:=nil;
end;

destructor TControlPlacer.Destroy;
begin
  inherited Destroy;
end;

procedure TControlPlacer.AddPlacableControl(AControl: TWinControl);
begin
  AControl.Visible:=false;
  AControl.AutoSize:=false;
  AControl.OnEnter:=@FControlEnter;
  AControl.OnKeyDown:=@FControlKeyDown;
  FOwner.InsertControl(AControl);
end;

procedure TControlPlacer.RemovePlacableControl(AControl: TWinControl);
begin
  FOwner.RemoveControl(AControl);
  AControl.Destroy;
end;

function TControlPlacer.StartUse(AControl: TWinControl; APlacedBy: TPlacingControl; AFocused: Boolean = false): Boolean;
begin
  if FPlacedBy<>nil then begin
    Result:=false;
    exit;
  end;
  FPlacedBy:=APlacedBy;
  FActiveControl:=AControl;
  FPlacedBy.UsingStarted;
  APlacedBy.UpdateControlBounds;
  FOwner.SetMouseLeaveIgnoreHandler(@GetControlMouseOver);
  DoStartUse;
  FActiveControl.OnExit:=@FControlExit;
  FActiveControl.Visible:=true;
  if AFocused
    then FActiveControl.SetFocus;
  Result:=true;
end;

procedure TControlPlacer.EndUse;
begin
  if FActiveControl<>nil then begin
    FOwner.RemoveMouseLeaveIgnoreHandler(@GetControlMouseOver);
    DoEndUse;
    FActiveControl.OnExit:=nil;

    FOwner.ResizingEnabled:=false;
    FActiveControl.Visible:=false;
    FOwner.ResizingEnabled:=true;

    FPlacedBy.UsingEnded;
    FActiveControl:=nil;
    FPlacedBy:=nil;
  end;
end;

function TControlPlacer.UsingControl: Boolean;
begin
  Result:=(FActiveControl<>nil);
end;

procedure TControlPlacer.FControlEnter(Sender: TObject);
begin
  //do nothing... maybe use this later
end;

procedure TControlPlacer.FControlExit(Sender: TObject);
begin
  EndUse;
end;

procedure TControlPlacer.FControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=13
    then EndUse;
end;

procedure TControlPlacer.DoStartUse;
begin
  //do nothing
end;

procedure TControlPlacer.DoEndUse;
begin
  //do nothing
end;

function TControlPlacer.GetControlFocused: Boolean;
begin
  if FActiveControl = nil then begin
    Result:=false;
    exit;
  end;
  Result:=FActiveControl.Focused;
end;

function TControlPlacer.GetControlMouseOver: Boolean;
var
  AMousePos: TPoint;
begin
  if FActiveControl = nil then begin
    Result:=false;
    exit;
  end;
  GetCursorPos(AMousePos);
  AMousePos:=FActiveControl.ScreenToClient(AMousePos);
  Result:=(AMousePos.X >= 0) and (AMousePos.Y >= 0) and (AMousePos.X < FActiveControl.Width) and (AMousePos.Y < FActiveCOntrol.height);
end;

{%ENDREGION}
{%REGION TPlacingControl}

constructor TPlacingControl.Create(AOwner: TSlaveControl; APlacer: TControlPlacer; AMaster: TMasterControl = nil);
begin
  FPlacer:=APlacer;
  FState:=ppDefault;
  inherited Create(AOwner, AMaster);
end;

destructor TPlacingControl.Destroy;
begin
  if IsUsingControl
    then Placer.EndUse;
  inherited Destroy;
end;

procedure TPlacingControl.DoPaint;
begin
  //do nothing
end;

procedure TPlacingControl.DoAbsolutePositionChanged;
begin
  UpdateControlBounds;
end;

procedure TPlacingControl.DoMouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if Point(X,Y)><FClickRect then begin
    if FState<>ppDown
      then SetState(ppMouseOver);
  end else begin
    if not Placer.ControlMouseOver
      then SetState(ppDefault);
  end;
end;

procedure TPlacingControl.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Point(X,Y)><FClickRect
    then SetState(ppDown);
end;

procedure TPlacingControl.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Point(X,Y)><FClickRect
    then SetState(ppMouseOver);
end;

procedure TPlacingControl.DoMouseEnter;
begin
  //do nothing
end;

procedure TPlacingControl.DoMouseLeave;
begin
  SetState(ppDefault);
end;

procedure TPlacingControl.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  FClickRect:=Rect(0, 0, ANewBounds.Width, ANewBounds.Height);
end;

procedure TPlacingControl.UpdateControlBounds;
begin
  if IsUsingControl then with FPlacer.FActiveControl do begin
    Left:=AbsoluteRect.Left+FClickRect.Left;
    Top:=AbsoluteRect.Top+FClickRect.Top;
    Width:=FClickRect.Right-FClickRect.Left;
    Height:=FClickRect.Bottom-FClickRect.Top;
  end;
end;

function TPlacingControl.IsUsingControl: Boolean;
begin
  Assert(FPlacer<>nil);
  Result:=(FPlacer.FPlacedBy=Self);
end;

procedure TPlacingControl.ControlChanged;
begin
  //do nothing
end;

procedure TPlacingControl.UsingStarted;
begin
  //do nothing
end;

procedure TPlacingControl.UsingEnded;
begin
  //do nothing
end;

procedure TPlacingControl.DoSetState(ANewState: TPlacingControlState);
begin
  if isUsingControl and (ANewState = ppDefault) and (not FPlacer.ControlFocused)
    then FPlacer.EndUse;
end;

procedure TPlacingControl.SetState(AValue: TPlacingControlState);
begin
  if AValue = FState
    then exit;
  DoSetState(AValue);
  FState:=AValue;
  Repaint;
end;

{%ENDREGION}
{%REGION TEditPlacer}

constructor TEditPlacer.Create(AOwner: TResizeBlockableMaster);
begin
  inherited Create(AOwner);
  FEdit:=TEdit.Create(FOwner);
  AddPlacableControl(FEdit);
end;

destructor TEditPlacer.Destroy;
begin
  RemovePlacableControl(FEdit);
  inherited Destroy;
end;

procedure TEditPlacer.FControlChange(Sender: TObject);
begin
  FPlacedBy.ControlChanged;
end;

procedure TEditPlacer.DoStartUse;
begin
  if ActiveControl.InheritsFrom(TCustomEdit)
    then TCustomEdit(ActiveControl).OnChange:=@FControlChange;
end;

procedure TEditPlacer.DoEndUse;
begin
  if ActiveControl.InheritsFrom(TCustomEdit)
    then TCustomEdit(ActiveControl).OnChange:=nil;
end;

{%ENDREGION}

end.

