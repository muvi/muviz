program MuviGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetvisual, MainUnit, EditPreset3, VisualisationMain,
  SelectPresetUnit2, EditPresetSettings, DragPlugUnit, ToolboxUnit,
  PresetTagUnit, advguipack, LCLAdvFunc;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPresetEdit3Form, PresetEdit3Form);
  Application.CreateForm(TSelectPresetForm2, SelectPresetForm2);
  Application.CreateForm(TPresetEditSettingsForm, PresetEditSettingsForm);
  Application.CreateForm(TDragPlugForm, DragPlugForm);
  Application.CreateForm(TToolboxForm, ToolboxForm);
  Application.CreateForm(TPresetTagEditForm, PresetTagEditForm);
  Application.Run;
end.

