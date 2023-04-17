unit SlaveRotaryFader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MiscSlaveControls, SlaveControl, Controls, AdvCoord;

type
  TSlaveRotaryFader = class (TSlaveButton)
  strict private
    FMin           : Real;
    FMax           : Real;
    FValue         : Real;
    FSpeed         : Real;
    FSmallFade     : Integer;
    FLargeFade     : Integer;
    FOnFade        : TSlaveControlEvent;
    //mouse
    FMouseDownValue: Real;
    FMouseDownPos  : TPoint;
    procedure SetValue(AValue: Real);
  strict protected
    procedure DoMouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TSlaveControl; AImages: TSlaveButtonImages; AMaster: TMasterControl = nil);
    property LargeFade: Integer read FLargeFade write FLargeFade;
    property Max: Real read FMax write FMax;
    property Min: Real read FMin write FMin;
    property SmallFade: Integer read FSmallFade write FSmallFade;
    property Value: Real read FValue write SetValue;
    property OnFade: TSlaveControlEvent read FOnFade write FOnFade;
    //use OnClick as "OnFadingEnded" event
  end;

implementation

{%REGION TSlaveRotaryFader}

constructor TSlaveRotaryFader.Create(AOwner: TSlaveControl; AImages: TSlaveButtonImages; AMaster: TMasterControl = nil);
begin
  inherited Create(AOwner, AImages, AMaster);
  FMin:=0.0;
  FMax:=100.0;
  FSmallFade:=200;
  FLargeFade:=100;
end;

procedure TSlaveRotaryFader.SetValue(AValue: Real);
begin
  if AValue < FMin
    then AValue:=FMin
    else if AValue > FMax
      then AValue:=FMax;
  if FValue <> AValue then begin
    FValue:=AValue;
    if Assigned(FOnFade)
      then FOnFade(Self);
    Repaint;
  end;
end;

procedure TSlaveRotaryFader.DoMouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
  if Down
    then Value:=FMouseDownValue + (FMouseDownPos.Y - Y)*FSpeed;
end;

procedure TSlaveRotaryFader.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if Button = mbRight
    then FSpeed:=(FMax-FMin) / FSmallFade
    else FSpeed:=(FMax-FMin) / FLargeFade;
  FMouseDownValue:=FValue;
  FMouseDownPos:=Point(X, Y);
end;

procedure TSlaveRotaryFader.DoPaint;
var
  APhi, ALineLength: Real;
  ACenter          : TPoint;
begin
  inherited DoPaint;
  if Width < Height
    then ALineLength:=Width * 0.25
    else ALineLength:=Height * 0.25;
  APhi:=((FValue - FMin) / FMax) * 1.5*Pi + Pi * 0.75;
  ACenter:=Point(AbsoluteRect.Left + Width div 2, AbsoluteRect.Top + Height div 2);
  Canvas.Pen.Width:=3;
  Canvas.Pen.Color:=$444444;
  Canvas.MoveTo(ACenter);
  Canvas.LineTo(
    ACenter.X + Round(ALineLength * Cos(APhi)),
    ACenter.Y + Round(ALineLength * Sin(APhi)));
  Canvas.Pen.Width:=1;
end;

{%ENDREGION}

end.

