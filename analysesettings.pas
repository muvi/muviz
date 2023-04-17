unit AnalyseSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, FreqAna2, ThreadOptions;

type

  { TAnalyseSettingForm }

  TAnalyseSettingForm = class(TForm)
    BCCEdit: TSpinEdit;
    BCCLbl: TLabel;
    BeatOptimizerCB: TCheckBox;
    BeatOptimizerHCB: TCheckBox;
    BeatHistoryTimeEdit: TFloatSpinEdit;
    BeatHistoryTimeLbl: TLabel;
    BHCEdit: TSpinEdit;
    BHCLbl: TLabel;
    BPMHCEdit: TSpinEdit;
    SyncBufferCountEdit: TSpinEdit;
    BPMHCLbl: TLabel;
    BeatSensibilityLbl: TLabel;
    SyncBufferCountLbl: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    SetBtn: TButton;
    BeatSensibilityEdit: TFloatSpinEdit;
    SyncHCountEdit: TSpinEdit;
    SaveSyncCountEdit: TSpinEdit;
    SyncHCountLbl: TLabel;
    SaveSyncCountLbl: TLabel;
    BPMThreadOptionFrame: TThreadOptionFrame;
    SyncThreadOptionFrame: TThreadOptionFrame;
    procedure SetBtnClick(Sender: TObject);
  private
    FSpectrumData: TBeatSpectrumData;
    procedure SetSettings;
  public
    function SetAnalyseSettings(ASpectrumData: TBeatSpectrumData): Boolean;
  end;

var
  AnalyseSettingForm: TAnalyseSettingForm;

implementation

{ TAnalyseSettingForm }

function TAnalyseSettingForm.SetAnalyseSettings(ASpectrumData: TBeatSpectrumData): Boolean;
begin
  FSpectrumData:=ASpectrumData;
  SyncHCountEdit.Value:=ASpectrumData.BeatSyncHCount;
  //SubbandEdit.Value:=ASpectrumData.}BeatSubbandCount;
  BHCEdit.Value:=ASpectrumData.BeatHistoryCount;
  BCCEdit.Value:=ASpectrumData.BeatCompareCount;
  BPMHCEdit.Value:=ASpectrumData.BPMHistoryCount;
  //BeatIdleEdit.Value:=ASpectrumData.BeatIdleTime;
  BeatSensibilityEdit.Value:=ASpectrumData.BeatSensibility;
  BeatOptimizerCB.Checked:=ASpectrumData.BeatOptimizer;
  BeatOptimizerHCB.Checked:=ASpectrumData.BeatOptimizerH;
  SyncBufferCountEdit.Value:=ASpectrumData.SyncBufferCount;
  SaveSyncCountEdit.Value:=ASpectrumData.SaveSyncCount;
  BeatHistoryTimeEdit.Value:=ASpectrumData.BeatHistoryTime;
  BPMThreadOptionFrame.AssignThread(ASpectrumData.BPMThread,'BPM');
  SyncThreadOptionFrame.AssignThread(ASpectrumData.SyncThread,'Sync');
  Result:=(ShowModal=mrOK);
end;

procedure TAnalyseSettingForm.SetBtnClick(Sender: TObject);
begin
  SetSettings;
end;

procedure TAnalyseSettingForm.SetSettings;
begin
  with FSpectrumData do begin
    SetCounts(WaveDataCount,LongWaveDataCount,FreqCount,FFTHistoryCount,BeatBufferCount,{SubbandEdit.Value,}BHCEdit.Value,BCCEdit.Value,BPMHCEdit.Value,SyncBufferCountEdit.Value,SaveSyncCountEdit.Value,BeatHistoryTimeEdit.Value);
    BeatSyncHCount:=SyncHCountEdit.Value;
    //BeatIdleTime:=BeatIdleEdit.Value;
    BeatSensibility:=BeatSensibilityEdit.Value;
    BeatOptimizer:=BeatOptimizerCB.Checked;
    BeatOptimizerH:=BeatOptimizerHCB.Checked;
    BPMThreadOptionFrame.SetSettings;
    SyncThreadOptionFrame.SetSettings;
  end;
end;

initialization
  {$I analysesettings.lrs}

end.

