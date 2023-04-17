unit PluginListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, StdCtrls, PluginSystem, PluginType;

type

  { TPluginForm }

  TPluginForm = class(TForm)
    CloseBtn: TButton;
    OpenDialog: TOpenDialog;
    OpenPluginBtn: TBitBtn;
    PluginLV: TListView;
    procedure FormCreate(Sender: TObject);
    procedure OpenPluginBtnClick(Sender: TObject);
  private
    FPluginSystem: TPluginSystem;
  public
    procedure ShowPlugins(const APluginSystem: TPluginSystem);
  end;

var
  PluginForm: TPluginForm;

implementation

const
  VisFileFilter   = 'Bekannte Dateitypen (*.mvl;*.vll;*.dll;*.ini;*.txt)|*.mvl;*.'
                   +'vll;*.dll;*.ini;*.txt|Visualisierungen (*.mvl;*.dll)|*.mvl;*'
                   +'.dll|Visualisierungslisten (*.vll;*.ini;*.txt)|*.vll;*.ini;*'
                   +'.txt|Muvi Visualisierungsdateien (*.mvl)|*.mvl|Muvi Visualis'
                   +'ierungslisten (*.vll)|*.vll|Anwendungserweiterungen (*.dll)|'
                   +'*.dll|Konfigurationseinstellungen (*.ini)|*.ini|Textdokument'
                   +'e (*.txt)|*.txt';

procedure TPluginForm.OpenPluginBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then FPluginSystem.LoadPluginLibrarys(OpenDialog.FileName);
end;

procedure TPluginForm.FormCreate(Sender: TObject);
begin
  OpenDialog.Filter:=VisFileFilter;
end;

procedure TPluginForm.ShowPlugins(const APluginSystem: TPluginSystem);
var
  I          : Integer;
  APluginData: TPluginLibData;
  AStr       : ShortString;
begin
  PluginLV.Items.Clear;
  for I:=0 to APluginSystem.PluginLibCount-1 do begin
    PluginLV.Items.Add;
    APluginData:=APluginSystem.PluginLibData[I];
    with PluginLV.Items[I] do begin
      Caption:=APluginData.Name;
      AStr:=APluginData.Version;
      SubItems.Add(AStr);
      SubItems.Add(APluginData.Description);
      SubItems.Add(APluginData.FileName);
    end;
  end;
  FPluginSystem:=APluginSystem;
  ShowModal;
end;

initialization
  {$I pluginlistunit.lrs}

end.

