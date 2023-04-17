unit BeatMon;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,LResources,Forms,Controls,Graphics,Dialogs,ExtCtrls,
  Spin,StdCtrls, ComCtrls,FreqAna2, AdvFunc, SpectrumData, Math;

type

  TMonitor_DrawBeat = procedure of object;

  { TBeatMonForm }

  TBeatMonForm      = class(TForm)
    BeatMonitor: TImage;
    BeatMonitor2: TImage;
    BeatHistoryCB: TCheckBox;
    BPMSensibilityEdit: TFloatSpinEdit;
    BeatZoomEdit: TFloatSpinEdit;
    Beat600CB: TCheckBox;
    DPSCB: TCheckBox;
    TFsqrCB: TCheckBox;
    TakeFirstMaxPosOffsetEdit: TSpinEdit;
    TakeFirstDeltaEdit: TFloatSpinEdit;
    HistoryCountEdit: TSpinEdit;
    TakeFirstDeltaLbl: TLabel;
    TestCB1: TCheckBox;
    DrawCB: TCheckBox;
    HistoryZoomEdit: TFloatSpinEdit;
    HistoryZoomLbl: TLabel;
    SensibilityEdit: TFloatSpinEdit;
    StatusBar: TStatusBar;
    SubbandEdit: TSpinEdit;
    TestCB2: TCheckBox;
    TestCB3: TCheckBox;
    procedure BPMSensibilityEditChange(Sender: TObject);
    procedure BeatZoomEditChange(Sender: TObject);
    //procedure DirectCorCBChange(Sender: TObject);
    procedure DPSCBChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HistoryZoomEditChange(Sender: TObject);
    procedure SensibilityEditChange(Sender: TObject);
    procedure TakeFirstDeltaEditChange(Sender: TObject);
    procedure TakeFirstMaxPosOffsetEditChange(Sender: TObject);
    procedure TFsqrCBChange(Sender: TObject);
  private
    FSpectrumData: TBeatSpectrumData;
    FGBuffer     : TBitmap;
    procedure DoDrawBeat;
    procedure DoDrawNothing;
  public
    DrawFrame   : TMonitor_DrawBeat;
    FHistoryZoom: Real;
    FBeatZoom   : Real;
    procedure InitMonitor(SpectrumData: TBeatSpectrumData);
  end;

var
  BeatMonForm: TBeatMonForm;

implementation

{ TBeatMonForm }

procedure TBeatMonForm.FormHide(Sender: TObject);
begin
  DrawFrame:=@DoDrawNothing;
end;

procedure TBeatMonForm.FormResize(Sender: TObject);
begin
  FGBuffer.Width:=BeatMonitor.Width;
  FGBuffer.Height:=BeatMonitor.Height;
  BeatMonitor.Picture.Bitmap.Width:=BeatMonitor.Width;
  BeatMonitor.Picture.Bitmap.Height:=BeatMonitor.Height;
  BeatMonitor2.Picture.Bitmap.Width:=BeatMonitor2.Width;
  BeatMonitor2.Picture.Bitmap.Height:=BeatMonitor2.Height;
end;

procedure TBeatMonForm.FormCreate(Sender: TObject);
begin
  FHistoryZoom:=60.0;
  FBeatZoom:=0.1;
  FGBuffer:=TBitmap.Create;
  FGBuffer.Width:=BeatMonitor.Width;
  FGBuffer.Height:=BeatMonitor.Height;
  BeatMonitor.Picture.Bitmap.Width:=BeatMonitor.Width;
  BeatMonitor.Picture.Bitmap.Height:=BeatMonitor.Height;
  BeatMonitor2.Picture.Bitmap.Width:=BeatMonitor2.Width;
  BeatMonitor2.Picture.Bitmap.Height:=BeatMonitor2.Height;
end;

procedure TBeatMonForm.BPMSensibilityEditChange(Sender: TObject);
begin
  FSpectrumData.BPMSensibility:=BPMSensibilityEdit.Value;
end;

procedure TBeatMonForm.BeatZoomEditChange(Sender: TObject);
begin
  FBeatZoom:=BeatZoomEdit.Value;
end;

{procedure TBeatMonForm.DirectCorCBChange(Sender: TObject);
begin
  FSpectrumData.SetCorellationMode(DirectCorCB.Checked);
end;}

procedure TBeatMonForm.DPSCBChange(Sender: TObject);
begin
  FSpectrumData.DPS:=DPSCB.Checked;
end;

procedure TBeatMonForm.FormDestroy(Sender: TObject);
begin
  FGBuffer.Destroy;
end;

procedure TBeatMonForm.FormShow(Sender: TObject);
begin
  DrawFrame:=@DoDrawBeat;
end;

procedure TBeatMonForm.HistoryZoomEditChange(Sender: TObject);
begin
  FHistoryZoom:=HistoryZoomEdit.Value;
end;

procedure TBeatMonForm.SensibilityEditChange(Sender: TObject);
begin
  FSpectrumData.FSSensibility:=SensibilityEdit.Value;
end;

procedure TBeatMonForm.TakeFirstDeltaEditChange(Sender: TObject);
begin
  FSpectrumData.TakeFirstDelta:=TakeFirstDeltaEdit.Value;
end;

procedure TBeatMonForm.TakeFirstMaxPosOffsetEditChange(Sender: TObject);
begin
  FSpectrumData.TakeFirstMaxPosOffset:=TakeFirstMaxPosOffsetEdit.Value;
end;

procedure TBeatMonForm.TFsqrCBChange(Sender: TObject);
begin
  FSpectrumData.DoTFsqr:=TFsqrCB.Checked;
end;

procedure TBeatMonForm.DoDrawBeat;
var
  I,J,TempY,BeatTop,BeatBottom,ABeatHistoryCount: Integer;
  BeatStep,HistoryStep,BeatHeight               : Single;

  function Beat600Value(const Index,Subband: Integer): Real;
  var
    ABeat600Offset,AIndex: Integer;
    ABeat600Block        : TBeat600Block;
  begin
    with FSpectrumData do begin
      ABeat600Offset:=(Beat600BlockCount*Beat600ConCount)-BeatBufferCount;
      AIndex:=Index+1+AOffset+ABeat600Offset;
      AIndex:=(AIndex div Beat600ConCount){+1};
      ABeat600Block:=Beat600Blocks[AIndex,Subband];
      if ABeat600Block.Valid
        then Result:=ABeat600Block.Value
        else Result:=0.0;
    end;
  end;

  function Beat600Value2(const Index,Subband: Integer): Real;
  var
    ABeat600Offset,AIndex,AIndex2: Integer;
    ABeat600Block                : TBeat600Block;
  begin
    with FSpectrumData do begin
      ABeat600Offset:=(Beat600BlockCount*Beat600ConCount)-BeatBufferCount;
      AIndex:=Index+1+AOffset+ABeat600Offset;
      AIndex2:=(AIndex div Beat600ConCount){+1};
      ABeat600Block:=Beat600Blocks[AIndex2,Subband];
      if (ABeat600Block.Valid) and (ABeat600Block.Offset=AIndex mod Beat600ConCount)
        then Result:=ABeat600Block.Value
        else Result:=0.0;
    end;
  end;

  function TestBeatAt(const Subband,History: Integer): Boolean;
  begin
    if Beat600CB.Checked
      then Result:=FSpectrumData.BeatAt600[Subband,History]
      else Result:=FSpectrumData.BeatAt[Subband,History];
  end;

begin
  if not DrawCB.Checked then exit;
  with BeatMonitor.Picture.Bitmap{FGBuffer} do begin
    Canvas.Brush.Color:=clWhite;
    Canvas.Pen.Color:=clWhite;
    Canvas.Rectangle(0,0,Width,Height);
    BeatStep:=Width/{FSpectrumData.}BeatAnaSubbandCount;
    HistoryStep:=Width/(FSpectrumData.BeatBufferCount-1);
    if BeatHistoryCB.Checked then begin
      ABeatHistoryCount:=FSpectrumData.BeatHistoryCount;
      if ABeatHistoryCount>HistoryCountEdit.Value
        then ABeatHistoryCount:=HistoryCountEdit.Value;
      BeatHeight:=Height/ABeatHistoryCount;
      for J:=0 to ABeatHistoryCount-1 do begin
        BeatTop:=Round(J*BeatHeight);
        BeatBottom:=Round((J+1)*BeatHeight);
        for I:=0 to {FSpectrumData.}BeatAnaSubbandCount-1 do begin
          if {FSpectrumData.BeatAt[I,J]}TestBeatAt(I,J) then begin
            Canvas.Brush.Color:=clRed;
            Canvas.Pen.Color:=clRed;
            Canvas.Rectangle(Round(I*BeatStep),BeatTop,Round((I+1)*BeatStep),BeatBottom);
          end else begin
            Canvas.Brush.Color:=clBlue;
            Canvas.Pen.Color:=clBlue;
            Canvas.Rectangle(Round(I*BeatStep),BeatTop,Round((I+1)*BeatStep),BeatBottom);
          end;
        end;
      end;
      Canvas.Pen.Color:=clBlack;
    end else begin
      Canvas.Pen.Color:=clBlack;
      BeatBottom:=Round(BeatStep);
      for I:=0 to {FSpectrumData.}BeatAnaSubbandCount-1 do begin
        if {FSpectrumData.BeatAt[I,-1]}TestBeatAt(I,-1) then begin
          Canvas.Brush.Color:=clRed;
          Canvas.Rectangle(Round(I*BeatStep),0,Round((I+1)*BeatStep),BeatBottom);
        end else begin
          Canvas.Brush.Color:=clBlue;
          Canvas.Rectangle(Round(I*BeatStep),0,Round((I+1)*BeatStep),BeatBottom);
        end;
      end;
    end;

    {Canvas.Pen.Color:=clGray;
    Canvas.Pixels[0,Height-Round(FSpectrumData.FSHistory[0,SubbandEdit.Value]*Height*FHistoryZoom)]:=clGray;
    //Canvas.MoveTo(0,Height-Round(FSpectrumData.FSHistory[0,SubbandEdit.Value]*Height*FHistoryZoom));
    for I:=1 to FSpectrumData.BeatBufferCount-1 do begin
      //Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.FSHistory[I,SubbandEdit.Value]*Height*FHistoryZoom));
      Canvas.Pixels[Round(I*HistoryStep),Height-Round(FSpectrumData.FSHistory[I,SubbandEdit.Value]*Height*FHistoryZoom)]:=clGray;
    end;}
    Canvas.Pen.Color:=clRed;
    TempY:=Height-Round(FSpectrumData.FSAverage[SubbandEdit.Value]*Height*FHistoryZoom);
    Canvas.MoveTo(0,TempY);
    Canvas.LineTo(Width,TempY);

    {Canvas.Pen.Color:=$22FF22{clGreen};
    Canvas.MoveTo(0,Height-Round(FSpectrumData.FSTHistory[0,SubbandEdit.Value]*Height*FHistoryZoom));
    for I:=1 to FSpectrumData.BeatBufferCount-1 do begin
      Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.FSTHistory[I,SubbandEdit.Value]*Height*FHistoryZoom));
    end;}

    {Canvas.Pen.Color:=clBlack;
    Canvas.MoveTo(0,Height-Round(FSpectrumData.Beat600Blocks[0,SubbandEdit.Value].Value*Height*FHistoryZoom));
    for I:=1 to FSpectrumData.Beat600BlockCount-1 do begin
      Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.Beat600Blocks[I,SubbandEdit.Value].Value*Height*FHistoryZoom));
    end;}

    if TestCB1.Checked then begin
      Canvas.Pen.Color:=clBlack;
      Canvas.MoveTo(0,Height-Round(Beat600Value(0,SubbandEdit.Value)*Height*FHistoryZoom));
      for I:=1 to FSpectrumData.BeatBufferCount-1 do begin
        Canvas.LineTo(Round(I*HistoryStep),Height-Round(Beat600Value(I,SubbandEdit.Value)*Height*FHistoryZoom));
      end;
    end;

    if TestCB3.Checked then begin
      Canvas.Pen.Color:=clGray;
      //Canvas.Pixels[0,Height-Round(FSpectrumData.FSHistory[0,SubbandEdit.Value]*Height*FHistoryZoom)]:=clBlack;
      Canvas.MoveTo(0,Height-Round(FSpectrumData.FSHistory[0,SubbandEdit.Value]*Height*FHistoryZoom));
      for I:=1 to FSpectrumData.BeatBufferCount-1 do begin
        Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.FSHistory[I,SubbandEdit.Value]*Height*FHistoryZoom));
        //Canvas.Pixels[Round(I*HistoryStep),Height-Round(FSpectrumData.FSHistory[I,SubbandEdit.Value]*Height*FHistoryZoom)]:=clBlack;
      end;
    end;

    if TestCB2.Checked then begin
      Canvas.Pen.Color:=clBlue;
      Canvas.MoveTo(0,Height-Round(Beat600Value2(0,SubbandEdit.Value)*Height*FHistoryZoom));
      for I:=1 to FSpectrumData.BeatBufferCount-1 do begin
        Canvas.LineTo(Round(I*HistoryStep),Height-Round(Beat600Value2(I,SubbandEdit.Value)*Height*FHistoryZoom));
      end;
    end;
    {Canvas.Pen.Color:=clBlack;
    Canvas.MoveTo(Width-Round((FSpectrumData.AOffset+1-FSpectrumData.Beat600ConCount)*HistoryStep),Height-Round(FSpectrumData.Beat600Blocks[FSpectrumData.Beat600BlockCount,SubbandEdit.Value].Value*Height*FHistoryZoom));
    Canvas.LineTo(Width-Round((FSpectrumData.AOffset+1)*HistoryStep),Height-Round(FSpectrumData.Beat600Blocks[FSpectrumData.Beat600BlockCount,SubbandEdit.Value].Value*Height*FHistoryZoom));
    J:=1;
    for I:=FSpectrumData.Beat600BlockCount-1 downto 1 do begin
      Canvas.LineTo(Width-Round(((FSpectrumData.Beat600ConCount+1)*(J-1)+FSpectrumData.AOffset+1)*HistoryStep),Height-Round(FSpectrumData.Beat600Blocks[I,SubbandEdit.Value].Value*Height*FHistoryZoom));
      Canvas.LineTo(Width-Round(((FSpectrumData.Beat600ConCount+1)*(J)+FSpectrumData.AOffset+1)*HistoryStep),Height-Round(FSpectrumData.Beat600Blocks[I,SubbandEdit.Value].Value*Height*FHistoryZoom));
      Inc(J);
      //Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.FSTHistory[I,SubbandEdit.Value]*Height*FHistoryZoom));
    end;}

    {Canvas.Pen.Color:=clRed;
    TempY:=Height-Round(FSpectrumData.FSAverage[SubbandEdit.Value]*Height*FHistoryZoom);
    Canvas.MoveTo(0,TempY);
    Canvas.LineTo(Width,TempY);}

    {Canvas.MoveTo(0,Height-Round(FSpectrumData.FSHistory[0,SubbandEdit.Value]*Height*60));
    for I:=1 to FSpectrumData.BeatBufferCount-1 do begin
      Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.FSHistory[I,SubbandEdit.Value]*Height*60));
    end;
    Canvas.Pen.Color:=clRed;
    TempY:=Height-Round(FSpectrumData.FSAverage[SubbandEdit.Value]*Height*60);
    Canvas.MoveTo(0,TempY);
    Canvas.LineTo(Width,TempY);}
  end;
  with BeatMonitor2.Picture.Bitmap do begin
    Canvas.Brush.Color:=clWhite;
    Canvas.Pen.Color:=clBlack;
    Canvas.Rectangle(0,0,Width,Height);
    HistoryStep:=Width/(FSpectrumData.{BeatEqualityCount}TakeFirstCount-1);

    //if FSpectrumData.TakeFirst then begin --> FSpectrumData.TakeFirst=true (IMMER!!)
      HistoryStep:=1.0;
      if not isNan(FSpectrumData.TakeFirstEquality[0]) then begin
        Canvas.MoveTo(0,Height);
        Canvas.LineTo(0,Height-Round(FSpectrumData.TakeFirstEquality[0]*FBeatZoom));
      end;
      for I:=1 to FSpectrumData.TakeFirstCount-1 do if not isNan(FSpectrumData.TakeFirstEquality[I]) then begin
        Canvas.MoveTo(Round(I*HistoryStep),Height);
        Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.TakeFirstEquality[I]*FBeatZoom));
      end;
      Canvas.Pen.Color:=clRed;
      Canvas.MoveTo(0,Height-Round(FSpectrumData.TakeFirstMax*FBeatZoom));
      Canvas.LineTo(Width,Height-Round(FSpectrumData.TakeFirstMax*FBeatZoom));
      Canvas.Pen.Color:=clLime;
      if not isNan(FSpectrumData.TakeFirstEquality[FSpectrumData.DPSPosTemp]) then begin
        Canvas.MoveTo(Round(FSpectrumData.DPSPosTemp*HistoryStep),Height-Round(FSpectrumData.TakeFirstEquality[FSpectrumData.DPSPosTemp]*FBeatZoom));
        Canvas.LineTo(Round(FSpectrumData.DPSPosTemp*HistoryStep),Height);
      end;
      Canvas.Pen.Color:=clBlue;
      if not isNan(FSpectrumData.TakeFirstEquality[FSpectrumData.TakeFirstMaxPos]) then begin
        Canvas.MoveTo(Round(FSpectrumData.TakeFirstMaxPos*HistoryStep),Height-Round(FSpectrumData.TakeFirstEquality[FSpectrumData.TakeFirstMaxPos]*FBeatZoom));
        Canvas.LineTo(Round(FSpectrumData.TakeFirstMaxPos*HistoryStep),Height);
      end;
      {end else begin
      Canvas.MoveTo(0,Height-Round(FSpectrumData.BeatEquality[0]*FBeatZoom));
      for I:=1 to FSpectrumData.BeatEqualityCount-1 do begin
        Canvas.LineTo(Round(I*HistoryStep),Height-Round(FSpectrumData.BeatEquality[I]*FBeatZoom));
      end;
      Canvas.Pen.Color:=clRed;
      Canvas.MoveTo(0,Height-Round(FSpectrumData.AverageBeatEquality*BPMSensibilityEdit.Value*FBeatZoom));
      Canvas.LineTo(Width,Height-Round(FSpectrumData.AverageBeatEquality*BPMSensibilityEdit.Value*FBeatZoom));
    end;}
  end;
  //BeatMonitor.Picture.Bitmap.Canvas.Draw(0,0,FGBuffer);
end;

procedure TBeatMonForm.DoDrawNothing;
begin

end;

procedure TBeatMonForm.InitMonitor(SpectrumData: TBeatSpectrumData);
begin
  DrawFrame:=@DoDrawNothing;
  FSpectrumData:=SpectrumData;
end;

initialization
  {$I beatmon.lrs}
end.

