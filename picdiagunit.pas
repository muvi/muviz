unit PicDiagUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Spin, StdCtrls, ComCtrls, GR32_Image, FreqAna2,
  GenericMemoryArraysXD, GenericMemoryArrays, GraphX32, PluginType, Math,
  DynamicBase, GR32_Layers;

type
  TPicDiagOption         = (doMax,doMin);
  TPicDiagOptions        = set of TPicDiagOption;
  TPicDiagDescriptionFunc= function (X,Y: dsi): string of object;
  TPicDiagChangeBamProc  = procedure (var ABam: TBam) of object;

  { TPicDiagForm }

  TPicDiagForm           = class(TForm)
    DiagImage: TImage32;
    MaxEdit: TFloatSpinEdit;
    MaxLbl: TLabel;
    MinEdit: TFloatSpinEdit;
    MinLbl: TLabel;
    SettingPanel: TPanel;
    StatusBar: TStatusBar;
    Timer    : TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure MaxEditChange(Sender: TObject);
    procedure MinEditChange(Sender: TObject);
    procedure DiagImageResize(Sender: TObject);
    procedure DiagImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure DiagImageMouseLeave(Sender: TObject);
  private
    FBuffer      : TMVFloatBufferXD;
    FMaxVal      : MVFloat;
    FMinVal      : MVFloat;
    FStartBam    : TBam;
    FColors      : array of GraphX32.TColor32;
    FNanColor    : GraphX32.TColor32;
    FPosInfColor : GraphX32.TColor32;
    FNegInfColor : GraphX32.TColor32;
    FColorInc    : MVFloat;
    FXStep,FYStep: Real;
    FMouseItem   : TPoint;
    FDescriptions: TPicDiagDescriptionFunc;
    FChangeBam   : TPicDiagChangeBamProc;
    function ValColor(AVal: MVFloat): GraphX32.TColor32; inline;
    procedure DrawDiag;
  public
    //nur für ZWEIdimensionale TMVFloatBufferXD's
    procedure ShowWithDiag(ABuf: TMVFloatBufferXD; AStartBam: TBam; AMaxVal,AMinVal: MVFloat; AColors: array of GraphX32.TColor32; AOptions: TPicDiagOptions=[]; AChangeBam: TPicDiagChangeBamProc = nil; APosInfColor: GraphX32.TColor32 = $FFFFFFFF; ANegInfColor: GraphX32.TColor32 = $FF000000; ANanColor: GraphX32.TColor32 = $FF777777; ADescriptions: TPicDiagDescriptionFunc = nil);
  end;

var
  PicDiagForm: TPicDiagForm;

implementation

{ TPicDiagForm }

procedure TPicDiagForm.ShowWithDiag(ABuf: TMVFloatBufferXD; AStartBam: TBam; AMaxVal,AMinVal: MVFloat; AColors: array of GraphX32.TColor32; AOptions: TPicDiagOptions=[]; AChangeBam: TPicDiagChangeBamProc = nil; APosInfColor: GraphX32.TColor32 = $FFFFFFFF; ANegInfColor: GraphX32.TColor32 = $FF000000; ANanColor: GraphX32.TColor32 = $FF777777; ADescriptions: TPicDiagDescriptionFunc = nil);
var
  I,L  : Integer;
begin
  FBuffer:=ABuf;
  FMaxVal:=AMaxVal;
  FMinVal:=AMinVal;
  if BamAssigned(AStartBam)
    then FStartBam:=AStartBam
    else FStartBam:=ABuf.ToFirst;
  L:=Length(AColors);
  SetLength(FColors,L);
  for I:=0 to L-1 do FColors[I]:=AColors[I];
  //FColorInc:=[Anzahl der Farben] / [Werterbereich]
  FColorInc:=((L-1)*$100-1)/(FMaxVal-FMinVal);
  FPosInfColor:=APosInfColor;
  FNegInfColor:=ANegInfColor;
  FNanColor:=ANanColor;

  if doMax in AOptions then begin
    MaxEdit.Value:=AMaxVal;
    MaxLbl.Visible:=true;
    MaxEdit.Visible:=true;
  end else begin
    MaxEdit.Visible:=false;
    MaxLbl.Visible:=false;
  end;
  if doMin in AOptions then begin
    MinEdit.Value:=AMinVal;
    MinLbl.Visible:=true;
    MinEdit.Visible:=true;
    if doMax in AOptions then begin
      MinLbl.Left:=160;
      MinEdit.Left:=200;
    end else begin
      MinLbl.Left:=8;
      MinEdit.Left:=48;
    end;
  end else begin
    MinEdit.Visible:=false;
    MinLbl.Visible:=false;
  end;
  FChangeBam:=AChangeBam;
  FDescriptions:=ADescriptions;
  StatusBar.Visible:=(ADescriptions<>nil);
  SettingPanel.Visible:=((doMax in AOptions) or (doMin in AOptions));
  FMouseItem.X:=-1;
  DiagImageResize(nil);
  Timer.Enabled:=true;
  Show;
end;

procedure TPicDiagForm.FormCreate(Sender: TObject);
begin
  FBuffer:=nil;
end;

procedure TPicDiagForm.TimerTimer(Sender: TObject);
begin
  if Assigned(FChangeBam) then FChangeBam(FStartBam);
  DrawDiag;
end;

procedure TPicDiagForm.MaxEditChange(Sender: TObject);
begin
  if isZero(MaxEdit.Value-FMinVal) then exit;
  FMaxVal:=MaxEdit.Value;
  FColorInc:=((Length(FColors)-1)*$100-1)/(FMaxVal-FMinVal);
end;

procedure TPicDiagForm.MinEditChange(Sender: TObject);
begin
  if isZero(FMaxVal-MinEdit.Value) then exit;
  FMinVal:=MinEdit.Value;
  FColorInc:=((Length(FColors)-1)*$100-1)/(FMaxVal-FMinVal);
end;

procedure TPicDiagForm.DiagImageResize(Sender: TObject);
begin
  if FBuffer=nil then exit;
  DiagImage.BeginUpdate;
  DiagImage.Bitmap.BeginUpdate;
  DiagImage.Bitmap.SetSize(DiagImage.Width,DiagImage.Height);
  FXStep:=DiagImage.Width/FBuffer.Count[0];
  FYStep:=DiagImage.Height/FBuffer.Count[1];
  DiagImage.Bitmap.EndUpdate;
  DiagImage.Bitmap.Changed;
  DiagImage.EndUpdate;
  DiagImage.Changed;
end;

procedure TPicDiagForm.DiagImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  ANewCoord: dsi;
  AChanged : Boolean;
begin
  //X
  ANewCoord:=Round(X/FXStep);
  AChanged:=(ANewCoord<>FMouseItem.X);
  FMouseItem.X:=ANewCoord;
  //Y
  ANewCoord:=Round(Y/FYStep);
  AChanged:=(AChanged or (ANewCoord<>FMouseItem.Y));
  FMouseItem.Y:=ANewCoord;
  if AChanged and Assigned(FDescriptions)
    then StatusBar.SimpleText:=FDescriptions(FMouseItem.X,FMouseItem.Y);
end;

procedure TPicDiagForm.DiagImageMouseLeave(Sender: TObject);
begin
  FMouseItem.X:=-1;
  StatusBar.SimpleText:='';
end;

procedure TPicDiagForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled:=false;
  FChangeBam:=nil;
  if StatusBar.Visible then StatusBar.SimpleText:='';
  FBuffer:=nil;
  SetLength(FColors,0);
end;

function TPicDiagForm.ValColor(AVal: MVFloat): GraphX32.TColor32;
var
  AClrIndex,AClrArrayIndex: Cardinal;
begin
  if not isNan(AVal) then begin
    if AVal>=FMinVal then begin
      if AVal<=FMaxVal then begin
        AClrIndex:=Round((AVal-FMinVal)*FColorInc);
        AClrArrayIndex:=AClrIndex div $100;
        Result:=BetaBlend(FColors[AClrArrayIndex+1],FColors[AClrArrayIndex],AClrIndex mod $100);
      end else Result:=FPosInfColor;
    end else Result:=FNegInfColor;
  end else Result:=FNanColor;
end;

procedure TPicDiagForm.DrawDiag;
var
  I,J        : Integer;
  ABam       : TBam;
  X1,Y1,X2,Y2: MVFloat; //das aktuelle Rechteck: (X1,Y1,X2,Y2)
begin
  DiagImage.BeginUpdate;
  DiagImage.Bitmap.BeginUpdate;

  X2:=0;
  ABam:=FStartBam;
  for I:=0 to FBuffer.Count[0]-1 do begin
    X1:=X2;
    X2+=FXStep;
    Y2:=0;
    for J:=0 to FBuffer.Count[1]-1 do begin
      Y1:=Y2;
      Y2+=FYStep;
      DiagImage.Bitmap.FillRectTS(Round(X1),Round(Y1),Round(X2),Round(Y2),ValColor(FBuffer.Bams[ABam]));
      FBuffer.RotateNext(ABam);
    end;
  end;

  DiagImage.Bitmap.EndUpdate;
  DiagImage.Bitmap.Changed;
  DiagImage.EndUpdate;
  DiagImage.Changed;
end;

initialization
  {$I picdiagunit.lrs}

end.

