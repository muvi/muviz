unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DrawBtn: TButton;
    Image: TImage;
    procedure DrawBtnClick(Sender: TObject);
  private
    function f(x: Real): Real;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  MaxFX = 1000;

procedure TForm1.DrawBtnClick(Sender: TObject);
const
  AStepAngle= (2.0*pi)/MaxFX;
  AR        = 100.0;
var
  ACenter,AInitialPos: TPoint;
  I                  : Integer;
  APhi,AYVal         : Real;
begin
  Image.Picture.Bitmap.Width:=Image.Width;
  Image.Picture.Bitmap.Height:=Image.Height;
  ACenter:=Point(Image.Width div 2,Image.Height div 2);
  with Image.Picture.Bitmap do begin
    Canvas.Pen.Color:=clBlack;
    Canvas.Brush.Color:=clWhite;
    Canvas.Rectangle(0,0,Image.Width,Image.Height);
    Canvas.Pen.Color:=clGreen;
    Canvas.MoveTo(0,ACenter.Y);
    Canvas.LineTo(Image.Width,ACenter.Y);
    Canvas.MoveTo(ACenter.X,0);
    Canvas.LineTo(ACenter.X,Image.Height);

    APhi:=0;
    Canvas.Pen.Color:=clBlack;
    AYVal:=f(0)*AR;
    AInitialPos:=Point(ACenter.X,Round(AYVal)+ACenter.Y);
    Canvas.MoveTo(AInitialPos.X,AInitialPos.Y);
    for I:=1 to MaxFX-1 do begin
      APhi+=AStepAngle;
      AYVal:=f(I)*AR;
      Canvas.LineTo(Round(AYVal*Sin(APhi))+ACenter.X,Round(AYVal*Cos(APhi))+ACenter.Y);
    end;
    Canvas.Pixels[AInitialPos.X,AInitialPos.Y]:=clRed;
  end;
end;

function TForm1.f(x: Real): Real;
begin
  Result:=Sin(((2*Pi)/MaxFX)*X+(Pi/3));
end;

end.

