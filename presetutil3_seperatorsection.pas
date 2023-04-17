unit PresetUtil3_SeperatorSection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetUtil3_BasicParamSection, ParamPainter2, SlaveControl,
  CanvasGraphX32, Graphics, AdvFunc;

type
  TSeperatorSection = class (TBasicParamSection)
  strict private
    FName: string;
  strict protected
    function GetExpectedHeight: Integer; override;
    function GetMinWidth: Integer; override;
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TSlaveControl; AName: string; AMaster: TMasterControl = nil);
    property Name: string read FName;
  end;

const
  SEPERATOR_BGCOLOR  = $BBBBBB;
  SEPERATORLINEWIDTH = 2;
  SEPERATORLINESPACE = 5;

implementation

{%REGION TSeperatorSection}

constructor TSeperatorSection.Create(AOwner: TSlaveControl; AName: string; AMaster: TMasterControl = nil);
begin
  inherited Create(AOwner, AMaster);
  FName:=AName;
end;

function TSeperatorSection.GetExpectedHeight: Integer;
begin
  Result:=2*PARAMTOPSPACE + SEPERATORNAMESIZE;
end;

function TSeperatorSection.GetMinWidth: Integer;
begin
  //do not wrap anything...
  Result:=-1;
end;

procedure TSeperatorSection.DoPaint;
var
  AStr                                        : string;
  ATextWidth, ALineLength, ATextLeft, ALineTop: Integer;
begin
  Canvas.Pen.Width:=1;
  Canvas.Pen.Color:=PARAM_BGCOLOR;
  Canvas.Brush.Color:=PARAM_BGCOLOR;
  DrawCutRect(Canvas, AbsoluteRect.Left, AbsoluteRect.Top, AbsoluteRect.Right, AbsoluteRect.Bottom, 4, 4);

  Canvas.Font.Color:=clBlack;
  Canvas.Font.Height:=SEPERATORNAMESIZE;
  Canvas.Font.Style:=[fsBold];

  AStr:=StrShorten(FName, @GetTextWidth, Width - 2*KNOBSPACE);
  ATextWidth:=GetTextWidth(AStr);
  ATextLeft:=(Width - ATextWidth) div 2;
  Canvas.TextOut(
    AbsoluteRect.Left + ATextLeft,
    ABsoluteRect.Top + PARAMTOPSPACE,
    AStr);

  ALineLength:=ATextLeft - 2*SEPERATORLINESPACE;
  if ALineLength > 0 then begin
    ALineTop:=AbsoluteRect.Top + (Height - SEPERATORLINEWIDTH) div 2;
    Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Width:=SEPERATORLINEWIDTH;
    Canvas.Line(AbsoluteRect.Left + SEPERATORLINESPACE, ALineTop, AbsoluteRect.Left + ATextLeft - SEPERATORLINESPACE, ALineTop);
    Canvas.Line(AbsoluteRect.Left + Width + SEPERATORLINESPACE - ATextLeft, ALineTop, AbsoluteRect.Left + Width - SEPERATORLINESPACE, ALineTop);
  end;

  Canvas.Pen.Width:=1;
  Canvas.Font.Height:=0;
  Canvas.Font.Style:=[];
end;

{%ENDREGION}

end.

