unit DiagDrawUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, AdvMPFFreqAna, PluginType, Math;

type

  TDiagDrawData = record
    Buffer          : TMVFloatBuffer;
    StartIndex      : ^MVInt;
    SimpleStartIndex: MVInt;
    Color,LineColor : TColor;
    YMax,YMin       : MVFloat;
    IsEndIndex      : Boolean;
    DropValues      : Boolean;
  end;

  TDiagShowProc = procedure (ADiags: array of TDiagDrawData) of object;

  { TDiagDrawForm }

  TDiagDrawForm = class(TForm)
    DiagImage: TImage;
    Timer: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DiagImageResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FDiags   : array of TDiagDrawData;
    FDestRect: TRect;
  public
    procedure ShowWithDiags(ADiags: array of TDiagDrawData);
    procedure AddDiags(ADiags: array of TDiagDrawData);
    procedure ClearDiags;
  end;

var
  DiagDrawForm: TDiagDrawForm;

function DiagDrawData(ABuffer: TMVFloatBuffer; var AStartIndex: MVInt; AYMax: MVFloat = 1.0; AYMin: MVFloat = -1.0; AColor: TColor = clBlack; ALineColor: TColor = $FFFFFFFF; AIsEndIndex: Boolean = true; ADropValues: Boolean = true): TDiagDrawData;
function DiagDrawDataSimple(ABuffer: TMVFloatBuffer; AStartIndex: MVInt; AYMax: MVFloat = 1.0; AYMin: MVFloat = -1.0; AColor: TColor = clBlack; ALineColor: TColor = $FFFFFFFF; AIsEndIndex: Boolean = true; ADropValues: Boolean = true): TDiagDrawData;
procedure DrawDiag(Dest: TBitmap;  DestRect: TRect; Buf: TMVFloatBuffer; AColor: TColor = clBlack; AOColor: TColor = $FFFFFFFF; StartIndex: Cardinal = 0; YMax: MVFloat = 1.0; YMin: MVFloat = -1.0; ADropValues: Boolean = false); overload;
procedure DrawDiag(Dest: TBitmap; Buf: TMVFloatBuffer; AColor: TColor = clBlack; AOColor: TColor = $FFFFFFFF; StartIndex: Cardinal = 0; YMax: MVFloat = 1.0; YMin: MVFloat = -1.0; ADropValues: Boolean = false); overload;

implementation

{ TDiagDrawForm }

procedure TDiagDrawForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled:=false;
  ClearDiags;
end;

procedure TDiagDrawForm.DiagImageResize(Sender: TObject);
begin
  DiagImage.Picture.Bitmap.Width:=DiagImage.Width;
  DiagImage.Picture.Bitmap.Height:=DiagImage.Height;
  with FDestRect do begin
    Right:=DiagImage.Width;
    Bottom:=DiagImage.Height;
  end;
end;

procedure TDiagDrawForm.FormCreate(Sender: TObject);
begin
  with FDestRect do begin
    Left:=0;
    Top:=0;
  end;
  DiagImageResize(nil);
end;

procedure TDiagDrawForm.TimerTimer(Sender: TObject);
var
  I          : Integer;
  AStartIndex: MVInt;
begin
  with DiagImage.Picture.Bitmap do begin
    Canvas.Brush.Color:=clWhite;
    Canvas.Pen.Color:=clWhite;
    Canvas.Rectangle(0,0,Width,Height);
    for I:=0 to Length(FDiags)-1 do with FDiags[I] do begin
      if StartIndex<>nil
        then if IsEndIndex
          then AStartIndex:=(StartIndex^+1) mod Buffer.Count
          else AStartIndex:=StartIndex^
        else AStartIndex:=SimpleStartIndex;
      DrawDiag(DiagImage.Picture.Bitmap,FDestRect,Buffer,Color,LineColor,AStartIndex,YMax,YMin,DropValues);
    end;
  end;
end;

procedure TDiagDrawForm.ShowWithDiags(ADiags: array of TDiagDrawData);
var
  I,L: Integer;
begin
  L:=Length(ADiags);
  SetLength(FDiags,L);
  for I:=0 to L-1 do FDiags[I]:=ADiags[I];
  Timer.Enabled:=true;
  TimerTimer(nil);
  Show;
end;

procedure TDiagDrawForm.AddDiags(ADiags: array of TDiagDrawData);
var
  I,L,OL: Integer;
begin
  OL:=Length(FDiags);
  L:=Length(ADiags);
  SetLength(FDiags,OL+L);
  for I:=OL to OL+L-1 do FDiags[I]:=ADiags[I-OL];
end;

procedure TDiagDrawForm.ClearDiags;
begin
  SetLength(FDiags,0);
end;

{Allgemein}

function DiagDrawData(ABuffer: TMVFloatBuffer; var AStartIndex: MVInt; AYMax: MVFloat = 1.0; AYMin: MVFloat = -1.0; AColor: TColor = clBlack; ALineColor: TColor = $FFFFFFFF; AIsEndIndex: Boolean = true; ADropValues: Boolean = true): TDiagDrawData;
begin
  with Result do begin
    Buffer:=ABuffer;
    StartIndex:=@AStartIndex;
    SimpleStartIndex:=0;
    Color:=AColor;
    LineColor:=ALineColor;
    YMax:=AYMax;
    YMin:=AYMin;
    IsEndIndex:=AIsEndIndex;
    DropValues:=ADropValues;
  end;
end;

function DiagDrawDataSimple(ABuffer: TMVFloatBuffer; AStartIndex: MVInt; AYMax: MVFloat = 1.0; AYMin: MVFloat = -1.0; AColor: TColor = clBlack; ALineColor: TColor = $FFFFFFFF; AIsEndIndex: Boolean = true; ADropValues: Boolean = true): TDiagDrawData;
begin
  with Result do begin
    Buffer:=ABuffer;
    StartIndex:=nil;
    SimpleStartIndex:=AStartIndex;
    Color:=AColor;
    LineColor:=ALineColor;
    YMax:=AYMax;
    YMin:=AYMin;
    IsEndIndex:=AIsEndIndex;
    DropValues:=ADropValues;
  end;
end;

procedure DrawDiag(Dest: TBitmap; DestRect: TRect; Buf: TMVFloatBuffer; AColor: TColor = clBlack; AOColor: TColor = $FFFFFFFF; StartIndex: Cardinal = 0; YMax: MVFloat = 1.0; YMin: MVFloat = -1.0; ADropValues: Boolean = false);
var
  I                           : Integer;
  AScale,AOffset,XStep,ABufVal: Real;
  ADidPaint                   : Boolean;
begin
  AScale:=(DestRect.Bottom-DestRect.Top)/(YMax-YMin);
  AOffset:=DestRect.Bottom+(YMin*AScale);
  XStep:=(DestRect.Right-DestRect.Left)/Buf.Count;
  with Dest.Canvas do begin
    {Pen.Color:=clBlack;
    Brush.Color:=clWhite;
    Rectangle(DestRect);
    Pen.Color:=clRed;}
    if AOColor<>$FFFFFFFF then begin
      Pen.Color:=AOColor;
      MoveTo(DestRect.Left,Round(AOffset));
      LineTo(DestRect.Right,Round(AOffset));
    end;
    Pen.Color:=AColor;
    //Pen.Color:=clBlack;

    ABufVal:=Buf[StartIndex];
    if not IsNan(ABufVal) then begin
      ABufVal*=AScale;
      if ABufVal>MaxInt then ABufVal:=MaxInt;
      if ABufVal<-MaxInt then ABufVal:=-MaxInt;
      MoveTo(DestRect.Left,Round(AOffset-ABufVal));
      ADidPaint:=true;
    end else ADidPaint:=false;
  end;
  if (XStep<1.0) and ADropValues then begin
    XStep:=1/XStep;
    for I:=1 to DestRect.Right-DestRect.Left-1 do begin
      ABufVal:=Buf[(Round(I*XStep)+StartIndex) mod Buf.Count];
      if not IsNan(ABufVal) then begin
        ABufVal*=AScale;
        if ABufVal>MaxInt then ABufVal:=MaxInt;
        if ABufVal<-MaxInt then ABufVal:=-MaxInt;
        if ADidPaint
          then Dest.Canvas.LineTo(DestRect.Left+I,Round(AOffset-ABufVal))
          else begin
            Dest.Canvas.MoveTo(DestRect.Left+I,Round(AOffset-ABufVal));
            ADidPaint:=true;
          end;
      end else ADidPaint:=false;
    end;
  end else begin
    for I:=1 to Buf.Count-1 do begin
      ABufVal:=Buf[(I+StartIndex) mod Buf.Count];
      if not IsNan(ABufVal) then begin
        ABufVal*=AScale;
        if ABufVal>MaxInt then ABufVal:=MaxInt;
        if ABufVal<-MaxInt then ABufVal:=-MaxInt;
        if ADidPaint
          then Dest.Canvas.LineTo(DestRect.Left+Round(I*XStep),Round(AOffset-ABufVal))
          else begin
            Dest.Canvas.MoveTo(DestRect.Left+Round(I*XStep),Round(AOffset-ABufVal));
            ADidPaint:=true;
          end;
        ADidPaint:=true;
      end else ADidPaint:=false;
    end;
  end;
end;

procedure DrawDiag(Dest: TBitmap; Buf: TMVFloatBuffer; AColor: TColor = clBlack; AOColor: TColor = $FFFFFFFF; StartIndex: Cardinal = 0; YMax: MVFloat = 1.0; YMin: MVFloat = -1.0; ADropValues: Boolean = false);
begin
  DrawDiag(Dest,Rect(0,0,Dest.Width,Dest.Height),Buf,AColor,AOColor,StartIndex,YMax,YMin,ADropValues);
end;

initialization
  {$I diagdrawunit.lrs}

end.

