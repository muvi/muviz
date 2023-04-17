unit SubbandEditUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, Spin, SpectrumData, LCLType, Math,
  FreqAna2;

type

  { TSubbandEditForm }

  TSubbandEditForm = class(TForm)
    LoadFirstBtn: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveFirstBtn: TButton;
    LoadLastBtn: TButton;
    SaveLastBtn: TButton;
    UseFreqListCB: TCheckBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    FirstEdit: TFloatSpinEdit;
    LastEdit: TFloatSpinEdit;
    SLHC: THeaderControl;
    SLLB: TListBox;
    SLPanel: TPanel;
    procedure FirstEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LastEditChange(Sender: TObject);
    procedure LoadFirstBtnClick(Sender: TObject);
    procedure LoadLastBtnClick(Sender: TObject);
    procedure SaveFirstBtnClick(Sender: TObject);
    procedure SaveLastBtnClick(Sender: TObject);
    procedure SLLBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure SLLBSelectionChange(Sender: TObject; User: boolean);
    procedure UseFreqListCBChange(Sender: TObject);
  private
    FSubbands: PCSubbandSizes;
    FSpectrumData: TBeatSpectrumData;
    //procedure ShowSubbands
  public
    function EditSubbands(var Subbands: TCSubbandSizes; SpectrumData: TBeatSpectrumData): Boolean;
  end;

var
  SubbandEditForm: TSubbandEditForm;

implementation

{ TSubbandEditForm }

procedure TSubbandEditForm.SLLBSelectionChange(Sender: TObject; User: boolean);
var
  ATop: Integer;
begin
  if SLLB.ItemIndex>=0 then begin;
    ATop:=(SLLB.ItemIndex-SLLB.TopIndex)*SLLB.ItemHeight;
    FirstEdit.Visible:=true;
    FirstEdit.Top:=ATop;
    FirstEdit.Value:=FSubbands^[SLLB.ItemIndex].First;
    LastEdit.Visible:=true;
    LastEdit.Top:=ATop;
    LastEdit.Value:=FSubbands^[SLLB.ItemIndex].Last;
  end else begin
    FirstEdit.Visible:=false;
    LastEdit.Visible:=false;
  end;
end;

procedure TSubbandEditForm.UseFreqListCBChange(Sender: TObject);
begin
  FSpectrumData.SetBandWidthsCalcMode(UseFreqListCB.Checked);
end;

procedure TSubbandEditForm.SLLBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  TempRect   : TRect;
  TextY      : Integer;

  procedure NextSection(ASection: Integer);
  begin
    TempRect.Left:=TempRect.Right;
    TempRect.Right+=SLHC.Sections[ASection].Width;
  end;

  procedure WriteText(const S: string);
  begin
    SLLB.Canvas.TextRect(TempRect,TempRect.Left+5,TextY,S);
  end;

begin
  with SLLB.Canvas do begin
    if odSelected in State then begin
      Brush.Color:=clMenuHighlight;
      Pen.Color:=Brush.Color;
      Font.Color:=clWindow;
    end else begin
      Brush.Color:=clWindow;
      Pen.Color:=Brush.Color;
      Font.Color:=clWindowText;
    end;
    TextY:=ARect.Top+((ARect.Bottom-ARect.Top-Canvas.TextHeight('Wg')) div 2);
    FillRect(ARect);
    TempRect:=ARect;
    TempRect.Right:=TempRect.Left+SLHC.Sections[0].Width;

    WriteText(IntToStr(Index)+'.');
    NextSection(1);
    WriteText(FloatToStrF(FSubbands^[Index].First,ffFixed,7,2)+' Hz');
    NextSection(2);
    WriteText(FloatToStrF(FSubbands^[Index].Last,ffFixed,7,2)+' Hz');
    NextSection(3);
    WriteText(IntToStr(FSpectrumData.BandWidths[Index]));
    NextSection(4);
    WriteText(IntToStr(FSpectrumData.FirstBands[Index])+' ('+FloatToStrF(FSpectrumData.LevelFrequency(FSpectrumData.FirstBands[Index]),ffFixed,7,2)+' Hz)');
    NextSection(5);
    WriteText(IntToStr(FSpectrumData.LastBands[Index])+' ('+FloatToStrF(FSpectrumData.LevelFrequency(FSpectrumData.LastBands[Index]),ffFixed,7,2)+' Hz)');
  end;
end;

procedure TSubbandEditForm.FormCreate(Sender: TObject);
begin
  FirstEdit.MaxValue:=Infinity;
  LastEdit.MaxValue:=Infinity;
end;

procedure TSubbandEditForm.LastEditChange(Sender: TObject);
begin
  FSubbands^[SLLB.ItemIndex].Last:=LastEdit.Value;
end;

procedure TSubbandEditForm.LoadFirstBtnClick(Sender: TObject);
var
  F: System.text;
  I: Integer;
  S: String;
begin
  if OpenDialog.Execute then begin
    AssignFile(F,OpenDialog.FileName);
    Reset(F);
    for I:=0 to BeatAnaSubbandCount-1 do begin
      ReadLn(F,S);
      FSubbands^[I].First:=StrToFloat(S);
    end;
    CloseFile(F);
  end;
end;

procedure TSubbandEditForm.LoadLastBtnClick(Sender: TObject);
var
  F: System.text;
  I: Integer;
  S: String;
begin
  if OpenDialog.Execute then begin
    AssignFile(F,OpenDialog.FileName);
    Reset(F);
    for I:=0 to BeatAnaSubbandCount-1 do begin
      ReadLn(F,S);
      FSubbands^[I].Last:=StrToFloat(S);
    end;
    CloseFile(F);
  end;
end;

procedure TSubbandEditForm.SaveFirstBtnClick(Sender: TObject);
var
  F: System.text;
  I: Integer;
  S: String;
begin
  if SaveDialog.Execute then begin
    AssignFile(F,SaveDialog.FileName);
    Rewrite(F);
    for I:=0 to BeatAnaSubbandCount-1 do begin
      S:=FloatToStrF(FSubbands^[I].First,ffFixed,7,2);
      WriteLn(F,S);
    end;
    CloseFile(F);
  end;
end;

procedure TSubbandEditForm.SaveLastBtnClick(Sender: TObject);
var
  F: System.text;
  I: Integer;
  S: String;
begin
  if SaveDialog.Execute then begin
    AssignFile(F,SaveDialog.FileName);
    Rewrite(F);
    for I:=0 to BeatAnaSubbandCount-1 do begin
      S:=FloatToStrF(FSubbands^[I].Last,ffFixed,7,2);
      WriteLn(F,S);
    end;
    CloseFile(F);
  end;
end;

procedure TSubbandEditForm.FirstEditChange(Sender: TObject);
begin
  FSubbands^[SLLB.ItemIndex].First:=FirstEdit.Value;
end;

function TSubbandEditForm.EditSubbands(var Subbands: TCSubbandSizes; SpectrumData: TBeatSpectrumData): Boolean;
begin
  FSubbands:=@Subbands;
  FSpectrumData:=SpectrumData;
  //Result:=(ShowModal=mrOK);
  Show;
  while Visible do Application.ProcessMessages;
  Result:=true;
end;

initialization
  {$I subbandeditunit.lrs}

end.

