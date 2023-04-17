unit DropButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MiscSlaveControls, Controls, SlaveControl, CanvasGraphX32,
  Graphics;

type
  TDropButton            = class;
  TCanDropEvent          = function (Sender: TDropButton): Boolean of object;

  EConcurrentModification= class (Exception)

  end;

  TDropButton            = class (TSlaveControl)
  strict private
    FOnDrop               : TSlaveControlEvent;
    FOnDropped            : TSlaveControlEvent;
    FOnCanDrop            : TCanDropEvent;
    FDown                 : Boolean;
    FDraggingOver         : Boolean;
    FTag                  : Integer;
    function CanDrop: Boolean; inline;
  strict protected
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  protected
    procedure DoPaint; override;
    procedure Repaint; override;
  public
    constructor Create(AOwner: TSlaveControl; AMaster: TMasterControl = nil); reintroduce;
    destructor Destroy; override;
    property Down: Boolean read FDown;
    property DraggingOver: Boolean read FDraggingOver;
    property Tag: Integer read FTag write FTag;
    property OnCanDrop: TCanDropEvent read FOnCanDrop write FOnCanDrop;
    //do not destroy this button in OnDrop. Use OnDropped instead.
    property OnDrop: TSlaveControlEvent read FOnDrop write FOnDrop;
    //do not paint on the button here. Use OnDrop instead.
    property OnDropped: TSlaveControlEvent read FOnDropped write FOnDropped;
  end;

implementation

{%REGION TDropButton}

constructor TDropButton.Create(AOwner: TSlaveControl; AMaster: TMasterControl = nil);
begin
  inherited Create(AOwner);
  FDown:=false;
  FDraggingOver:=false;
end;

destructor TDropButton.Destroy;
begin
  if FDown
    then raise EConcurrentModification.Create('Tried to destroy the drop button while clicking');
  inherited Destroy;
end;

function TDropButton.CanDrop: Boolean; inline;
begin
  Result:=(not Assigned(FOnCanDrop)) or FOnCanDrop(Self)
end;

procedure TDropButton.DoMouseEnter;
begin
  if CanDrop then begin
    FDraggingOver:=true;
    inherited DoMouseEnter;
  end;
end;

procedure TDropButton.DoMouseLeave;
begin
  if FDraggingOver then begin
    FDraggingOver:=false;
    inherited DoMouseLeave;
  end;
end;

procedure TDropButton.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if FDraggingOver then begin
    //inherited DoMouseDown(...) hier auf keinen Fall aufrufen, weil dann
    //WireAtMouse gelöscht würde
    FDown:=true;
    Repaint;
  end;
end;

procedure TDropButton.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //to make touch possible
  if CanDrop then begin
    if Assigned(FOnDrop)
      then FOnDrop(Self);
    FDown:=false;
    Repaint;
    if Assigned(FOnDropped)
      then FOnDropped(Self);
  end;
end;

procedure TDropButton.DoPaint;
begin
  if FDraggingOver then begin
    if Down
      then Canvas.Pen.Color:=$A0A0A0
      else Canvas.Pen.Color:=clSilver;
    Canvas.Brush.Color:=Canvas.Pen.Color;
    DrawCutRect(Canvas, AbsoluteRect.Left, AbsoluteRect.Top, AbsoluteRect.Left + Width, AbsoluteRect.Top + Height, 2, 2);
  end;
end;

procedure TDropButton.Repaint;
begin
  //this button may need a background
  Master.Repaint(Owner);
end;

{%ENDREGION}

end.

