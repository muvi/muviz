unit LoadSelectUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, CheckLst, StdCtrls, VisTypeUnit;

type

  { TLoadSelectForm }

  TLoadSelectForm = class(TForm)
    LoadBtn: TButton;
    SelectCLB: TCheckListBox;
    SettingPanel: TPanel;
    UnloadBtn: TButton;
    procedure LoadBtnClick(Sender: TObject);
    procedure UnloadBtnClick(Sender: TObject);
  private
    FCompositions: TVisCompositions;
  public
    procedure SelectAndLoad(ACompositions: TVisCompositions);
  end;

var
  LoadSelectForm: TLoadSelectForm;

implementation

procedure TLoadSelectForm.UnloadBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I:=0 to SelectCLB.Items.Count-1
    do if SelectCLB.Checked[I]
      then if FCompositions.Loaded(I) then FCompositions.UnloadItem(I);
end;

procedure TLoadSelectForm.LoadBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I:=0 to SelectCLB.Items.Count-1
    do if SelectCLB.Checked[I] then begin
      FCompositions.LoadItem(I);
    end;
end;

procedure TLoadSelectForm.SelectAndLoad(ACompositions: TVisCompositions);
var
  I       : Integer;
  APresets: TPresets;
  AStr    : string;
begin
  FCompositions:=ACompositions;
  SelectCLB.Items.Clear;
  APresets:=FCompositions.Presets;
  for I:=0 to APresets.Count-1 do begin
    AStr:=APresets[I].Name;
    if ACompositions.Loaded(I) then AStr:='[OK] '+AStr;
    SelectCLB.Items.Add(AStr);
  end;
  Show;
end;

initialization
  {$I loadselectunit.lrs}

end.

