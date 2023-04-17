unit EditPresetSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, PresetUtil3_Wires;

type
  TWireModeSetter         = procedure (AWireMode: TWireVisibilityMode) of object;

  { TPresetEditSettingsForm }

  TPresetEditSettingsForm = class(TForm)
    AllWiresBtn: TBitBtn;
    SaveBtn: TBitBtn;
    LoadBtn: TBitBtn;
    BothEndWiresBtn: TBitBtn;
    OneEndWiresBtn: TBitBtn;
    Timer: TTimer;
    procedure AllWiresBtnClick(Sender: TObject);
    procedure BothEndWiresBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure OneEndWiresBtnClick(Sender: TObject);
    procedure FadeTimerTimer(Sender: TObject);
    procedure HideTimerTimer(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FWireModeSetter: TWireModeSetter;
    FLoadMethod    : TNotifyEvent;
    FSaveMethod    : TNotifyEvent;
    FTimeToHide    : Integer;
    procedure DoHide;
  end;

var
  PresetEditSettingsForm: TPresetEditSettingsForm = nil;

procedure EditWireMode(APos: TPoint; AWireModeSetter: TWireModeSetter; ALoadMethod, ASaveMethod: TNotifyEvent);

implementation

procedure EditWireMode(APos: TPoint; AWireModeSetter: TWireModeSetter; ALoadMethod, ASaveMethod: TNotifyEvent);
begin
  with PresetEditSettingsForm do begin
    FWireModeSetter:=AWireModeSetter;
    FLoadMethod:=ALoadMethod;
    FSaveMethod:=ASaveMethod;
    Left:=APos.X;
    Top:=APos.Y;
    AlphaBlendValue:=$D0;
    Show;
    FTimeToHide:=30;
    Timer.OnTimer:=@HideTimerTimer;
    Timer.Enabled:=true;
  end;
end;

{ TPresetEditSettingsForm }

procedure TPresetEditSettingsForm.AllWiresBtnClick(Sender: TObject);
begin
  FWireModeSetter(wvmAll);
  DoHide;
end;

procedure TPresetEditSettingsForm.BothEndWiresBtnClick(Sender: TObject);
begin
  FWireModeSetter(wvmBoth);
  DoHide;
end;

procedure TPresetEditSettingsForm.LoadBtnClick(Sender: TObject);
begin
  Timer.Enabled:=false;
  FLoadMethod(Self);
  DoHide;
end;

procedure TPresetEditSettingsForm.OneEndWiresBtnClick(Sender: TObject);
begin
  FWireModeSetter(wvmOne);
  DoHide;
end;

procedure TPresetEditSettingsForm.FadeTimerTimer(Sender: TObject);
begin
  if AlphaBlendValue > 0 then begin
    AlphaBlendValue:=AlphaBlendValue - $8;
  end else begin
    Timer.Enabled:=false;
    Hide;
  end;
end;

procedure TPresetEditSettingsForm.HideTimerTimer(Sender: TObject);
begin
  Dec(FTimeToHide);
  if FTimeToHide <= 0
    then DoHide;
end;

procedure TPresetEditSettingsForm.SaveBtnClick(Sender: TObject);
begin
  FSaveMethod(Self);
  DoHide;
end;

procedure TPresetEditSettingsForm.DoHide;
begin
  Timer.Enabled:=false;
  Timer.OnTimer:=@FadeTimerTimer;
  Timer.Enabled:=true;
end;

initialization
  {$I editpresetsettings.lrs}

end.

