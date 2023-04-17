unit ThreadOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, Spin,
  FreqAna2, Math;

type

  { TThreadOptionFrame }

  TThreadOptionFrame = class(TFrame)
    DynamicCB: TCheckBox;
    FPSEdit: TFloatSpinEdit;
    PriorityCB: TComboBox;
    PriorityLbl: TLabel;
    FPSLbl: TLabel;
    GroupBox: TGroupBox;
    procedure DynamicCBChange(Sender: TObject);
  private
    FThread: TAnalysingThread;
  public
    procedure AssignThread(AThread: TAnalysingThread; const AName: string);
    procedure Update;
    procedure SetSettings;
  end;

implementation

{ TThreadOptionFrame }

procedure TThreadOptionFrame.DynamicCBChange(Sender: TObject);
begin
  FPSEdit.Enabled:=not DynamicCB.Checked;
  FPSLbl.Enabled:=FPSEdit.Enabled;
  FPSEdit.Value:=1.0
end;

procedure TThreadOptionFrame.AssignThread(AThread: TAnalysingThread; const AName: string);
begin
  FThread:=AThread;
  GroupBox.Caption:=AName;
  FPSEdit.MinValue:=1E-100;
  FPSEdit.MaxValue:=1E100;
  Update;
end;

const
  PriorityCBIndices: array [TThreadPriority] of Integer = (6,5,4,3,2,1,0);
  ThreadPriorities : array [0..6] of TThreadPriority = (tpTimeCritical,tpHighest,tpHigher,tpNormal,tpLower,tpLowest,tpIdle);

procedure TThreadOptionFrame.Update;
var
  AFPS: Real;
begin
  PriorityCB.ItemIndex:=PriorityCBIndices[FThread.Priority];
  AFPS:=FThread.FPS;
  if IsInfinite(AFPS) then begin
    FPSEdit.Enabled:=false;
    FPSLbl.Enabled:=false;
    DynamicCB.Checked:=true;
    FPSEdit.Value:=1.0;
  end else begin
    FPSEdit.Enabled:=true;
    FPSLbl.Enabled:=true;
    DynamicCB.Checked:=false;
    FPSEdit.Value:=AFPS;
  end;
end;

procedure TThreadOptionFrame.SetSettings;
begin
  FThread.Priority:=ThreadPriorities[PriorityCB.ItemIndex];
  if DynamicCB.Checked
    then FThread.FPS:=Infinity
    else FThread.FPS:=FPSEdit.Value;
end;

initialization
  {$I threadoptions.lrs}

end.

