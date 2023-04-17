unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lNetComponents, VisualisationMain, GlobalTVSP, TVSPClient, TVSPConst,
  MStringImpl, Buffers, EditPreset3, PresetUtil3_Path, VisType2, PresetType,
  StdParamTypes, MStrings, CallParamType, IntegerParamType, FloatParamType,
  StringParamType, BooleanParamType, PresetParamType, PointerParamType,
  ColorParamType, BufferParamType, StdParamEdits, PresetParamEdit,
  PointerParamEdit, TagEditor, LCLAdvFunc, ToolboxUnit, DragPlugUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    ToolboxBtn: TButton;
    EditorBtn: TButton;
    procedure EditorBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolboxBtnClick(Sender: TObject);
  private
    FPreset: IPVisualisation;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

const
  VIDCONFIG: TGUID         = '{FF5EC525-5DE9-49F3-8EBB-415FDDF73363}';
  MAINPRESETNAME           = 'Main Preset';

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GlobalTVSP.Init(TTVSPClient.Create(Self, 'localhost', DEFAULTPORT));
  MStringImpl.Init;
  Buffers.Init;

  InitVisualisationSystem;

  //register param types
  CallParamType.Register;
  IntegerParamType.Register;
  FloatParamType.Register;
  ColorParamType.Register;
  BooleanParamType.Register;
  BufferParamType.Register;
  StringParamType.Register;
  PresetParamType.Register;
  PointerParamType.Register;

  StdParamEdits.Register;
  PresetParamEdit.Register;
  PointerParamEdit.Register;
end;

procedure TMainForm.EditorBtnClick(Sender: TObject);
var
  AID: TPParamID;
begin
  AID:=ParamID(MAINPRESETNAME, vPreset);
  FPreset:=PresetUtil[VIDCONFIG];
  PresetEdit3Form.EditParam(NewParamPath(AID), FPreset[AID]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPreset:=nil;
  VisualisationSystemDone;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DragPlugForm.Init;
end;

procedure TMainForm.ToolboxBtnClick(Sender: TObject);
begin
  ToolboxForm.UpdateAll;
  ToolboxForm.Show;
end;

end.

