unit SelectPresetUnit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  presetlistUnit, StdParamTypes, VisType2, ComCtrls, LCLIntf, LCLType;

type

  { TSelectPresetForm2 }

  TSelectPresetForm2 = class(TForm)
    PresetLF: TPresetListFrame2;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PresetLFPresetChosen(Sender: TObject; APreset: TPPresetID);
  private
    FOnPresetSelected: TNotifyEvent;
    FSuccess         : Boolean;
    FSelected        : TVPreset;
  public
    procedure SelectPreset(X, Y: Integer; AOnSelected: TNotifyEvent);
    property Selected: TVPreset read FSelected;
    property Success: Boolean read FSuccess;
  end;

var
  SelectPresetForm2: TSelectPresetForm2 = nil;

implementation

{ TSelectPresetForm2 }

procedure TSelectPresetForm2.FormDeactivate(Sender: TObject);
begin
  FSuccess:=false;
  Close;
  if Assigned(FOnPresetSelected)
    then FOnPresetSelected(Self);
end;

procedure TSelectPresetForm2.FormCreate(Sender: TObject);
var
  ARegion: TRegion;
begin
  ARegion:=TRegion.Create;
  with PresetLF do begin
    ARegion.AddRectangle(PresetTV.Left, PresetTV.Top, PresetTV.Left + PresetTV.Width, PresetTV.Top + PresetTV.Height);
    ARegion.AddRectangle(FilterEdit.Left, FilterEdit.Top, FilterEdit.Left + FilterEdit.Width, FilterEdit.Top + FilterEdit.Height);
    ARegion.AddRectangle(FilterEdit.Left + FilterEdit.Width + 1, FilterEdit.Top + 1, FilterEdit.Left + FilterEdit.Width + FilterEdit.ButtonWidth - 1, FilterEdit.Top + FilterEdit.Height - 1);
  end;
  //this is really needed, no idea why...
  Handle;
  SetShape(ARegion);
  ARegion.Destroy;
  PresetLF.OnPresetChosen:=@PresetLFPresetChosen;
end;

procedure TSelectPresetForm2.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
end;

procedure TSelectPresetForm2.FormDestroy(Sender: TObject);
begin
  PresetLF.Clear;
end;

procedure TSelectPresetForm2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    OnDeactivate:=nil;
    FSuccess:=false;
    Close;
    if Assigned(FOnPresetSelected)
      then FOnPresetSelected(Self);
    OnDeactivate:=@FormDeactivate;
  end;
end;

procedure TSelectPresetForm2.FormShow(Sender: TObject);
begin
  PresetLF.FilterEdit.SelectAll;
  PresetLF.FilterEdit.SetFocus;
end;

procedure TSelectPresetForm2.PresetLFPresetChosen(Sender: TObject; APreset: TPPresetID);
begin
  OnDeactivate:=nil;
  FSuccess:=true;
  FSelected:=APreset;
  Close;
  Deactivate;
  if Assigned(FOnPresetSelected)
    then FOnPresetSelected(Self);
  OnDeactivate:=@FormDeactivate;
end;

procedure TSelectPresetForm2.SelectPreset(X, Y: Integer; AOnSelected: TNotifyEvent);
begin
  FOnPresetSelected:=AOnSelected;
  PresetLF.UpdatePresets;
  Left:=X;
  Top:=Y;
  Application.AddOnDeactivateHandler(@FormDeactivate);
  Show;
end;

initialization
  {$I selectpresetunit2.lrs}

end.

