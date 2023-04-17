unit EditPreset3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, Spin, ComCtrls, VisTypeUnit, PresetUtil3,
  ChooseColor, types, VisType2, LMessages, DragPlugUnit, PresetUtil3_Path,
  PresetUtil3_WiringEditor, EditPresetSettings,PResetUtil3_Wires, VisFiles,
  InfoMsgs, ProgressHandler, InfoMsgTypes, StdTags, MStrings;

type

  { TPresetEdit3Form }

  TPresetEdit3Form = class(TForm)
    KnotDistanceEdit: TFloatSpinEdit;
    FrictionEdit: TFloatSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    DEdit: TFloatSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog: TOpenDialog;
    RestForceEdit: TFloatSpinEdit;
    SaveDialog: TSaveDialog;
    SimStepsEdit: TSpinEdit;
    StatusBar: TStatusBar;
    Timer: TTimer;
    WeightEdit: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SettingPanel: TPanel;
    PlugImageList: TImageList;
    PixelSizeEdit: TFloatSpinEdit;
    procedure DEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FrictionEditChange(Sender: TObject);
    procedure KnotDistanceEditChange(Sender: TObject);
    procedure PixelSizeEditChange(Sender: TObject);
    procedure RestForceEditChange(Sender: TObject);
    procedure SimStepsEditChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure WeightEditChange(Sender: TObject);
    procedure PresetEditorStartRender(Sender: TObject);
    procedure PresetEditorEndRender(Sender: TObject);
    procedure PresetEditorRender(Sender: TObject);
    procedure LoadFromFile(Sender: TObject);
    procedure SaveToFile(Sender: TObject);
  private
    FPresetEditor: TWiringPresetEditor;
    FEasterStr   : string;
    procedure SetWireMode(AWireMode: TWireVisibilityMode);
  public
    function EditParam(AParamPath: IParamPath; AParam: IPParam): Boolean;
    procedure ShowAdvancedControls;
  end;

var
  PresetEdit3Form: TPresetEdit3Form;

implementation

{$R *.lfm}

const
  EasterShowAdvancedControls = 'ADVCONTROLS ON';
  EasterHideAdvancedControls = 'ADVCONTROLS OFF';

{%REGION TPresetEdit3Form}

procedure TPresetEdit3Form.FormCreate(Sender: TObject);
begin
  FPresetEditor:=TWiringPresetEditor.Create(Self);
  InsertControl(FPresetEditor);
  with FPresetEditor do begin
    Align:=alClient;
    OnStartRender:=@PresetEditorStartRender;
    OnEndRender:=@PresetEditorEndRender;
    OnRender:=@PresetEditorRender;
    with PresetEditor do begin
      with Config.Pictures do begin
        Images:=PlugImageList;
        PlugedImageIndex:=0;
        UnplugedImageIndex:=2;
        HighlightedPlugedImageIndex:=1;
        HighlightedUnplugedImageIndex:=3;
        ExtHighlightedPlugedImageIndex:=15;
        ExtHighlightedUnplugedImageIndex:=16;
        PlugedUndefinedImageIndex:=4;
        UnplugedUndefinedImageIndex:=6;
        HighlightedPlugedUndefinedImageIndex:=5;
        HighlightedUnplugedUndefinedImageIndex:=7;
        ExtHighlightedPlugedUndefinedImageIndex:=5;
        ExtHighlightedUnplugedUndefinedImageIndex:=7;
        with ExpanderBtnImages do begin
          NormalIndex:=9;
          HighlightedIndex:=10;
          DownIndex:=11;
          SwitchedNormalIndex:=12;
          SwitchedHighlightedIndex:=13;
          SwitchedDownIndex:=14;
        end;
        with RotaryImages do begin
          NormalIndex:=4;
          HighlightedIndex:=5;
          DownIndex:=5;
        end;
        with AddPresetImages do begin
          NormalIndex:=25;
          HighlightedIndex:=26;
          DownIndex:=27;
        end;
        with SavePresetImages do begin
          NormalIndex:=22;
          HighlightedIndex:=23;
          DownIndex:=24;
        end;
        with EditTagsImages do begin
          NormalIndex:=19;
          HighlightedIndex:=20;
          DownIndex:=21;
        end;
        InitPlugBmps;
      end;
      with ParamImages do begin
        Images:=PlugImageList;
        BoolBoxUI:=6;
        BoolBoxUHI:=7;
        BoolBoxDI:=4;
        BoolBoxDHI:=5;
        ButtonUI:=18;
        ButtonUHI:=4;
        ButtonDI:=17;
      end;
    end;
  end;
  ActiveControl:=FPresetEditor;
  StatusBar.Panels[0].Text:='Rendering';
  FEasterStr:='';

  DragPlugUnit.AddDestControl(Self);
end;

procedure TPresetEdit3Form.DEditChange(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Config.WireSettings.D:=DEdit.Value;
end;

procedure TPresetEdit3Form.FormDestroy(Sender: TObject);
begin
  //do not destroy the preset editor here... this is done automatically
  RemoveControl(FPresetEditor);
  FPresetEditor.Destroy;
end;

procedure TPresetEdit3Form.FormHide(Sender: TObject);
begin
  Timer.Enabled:=false;
  FPresetEditor.PresetEditor.RemoveParam;
end;

procedure TPresetEdit3Form.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AWireModeEditorPos: TPoint;
begin
  if Key=17 {Strg} then begin
    Assert(PresetEditSettingsForm <> nil);
    AWireModeEditorPos:=Point(FPresetEditor.Width - PresetEditSettingsForm.Width, 0);
    AWireModeEditorPos:=FPresetEditor.ClientToScreen(AWireModeEditorPos);
    EditPresetSettings.EditWireMode(AWireModeEditorPos, @SetWireMode, @LoadFromFile, @SaveToFile);
  end;
  //#189 entspricht der '-' Taste
  if (Key=189) or (Length(FEasterStr)>255) then begin
    if Pos(EasterShowAdvancedControls,FEasterStr)>0
      then ShowAdvancedControls;
    if Pos(EasterHideAdvancedControls,FEasterStr)>0 then begin
      SettingPanel.Visible:=false;
      StatusBar.Visible:=false;
    end;
    FEasterStr:='';
  end else FEasterStr+=Char(Key);
end;

procedure TPresetEdit3Form.SetWireMode(AWireMode: TWireVisibilityMode);
begin
  FPresetEditor.PresetEditor.Config.WireVisibilityMode:=AWireMode;
end;

procedure TPresetEdit3Form.FormShow(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Draw;
  Timer.Enabled:=true;
end;

procedure TPresetEdit3Form.ShowAdvancedControls;
begin
  SettingPanel.Visible:=true;
  StatusBar.Visible:=true;
  with FPresetEditor.PresetEditor.Config.WireSettings do begin
    FrictionEdit.Value:=Friction;
    KnotDistanceEdit.Value:=KnotDistance;
    WeightEdit.Value:=Weight;
    DEdit.Value:=D;
    SimStepsEdit.Value:=FPresetEditor.RenderSteps;
    PixelSizeEdit.Value:=FPresetEditor.PresetEditor.Config.PixelSize;
    RestForceEdit.Value:=RestForce;
  end;
end;

procedure TPresetEdit3Form.FrictionEditChange(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Config.WireSettings.Friction:=FrictionEdit.Value;
end;

procedure TPresetEdit3Form.KnotDistanceEditChange(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Config.WireSettings.KnotDistance:=KnotDistanceEdit.Value;
end;

procedure TPresetEdit3Form.PixelSizeEditChange(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Config.PixelSize:=PixelSizeEdit.Value;
end;

procedure TPresetEdit3Form.RestForceEditChange(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Config.WireSettings.RestForce:=RestForceEdit.Value;
end;

procedure TPresetEdit3Form.SimStepsEditChange(Sender: TObject);
begin
  FPresetEditor.RenderSteps:=SimStepsEdit.Value;
end;

procedure TPresetEdit3Form.TimerTimer(Sender: TObject);
begin
  FPresetEditor.PresetEditor.UpdateParams;
end;

procedure TPresetEdit3Form.WeightEditChange(Sender: TObject);
begin
  FPresetEditor.PresetEditor.Config.WireSettings.Weight:=WeightEdit.Value;
end;

procedure TPresetEdit3Form.PresetEditorStartRender(Sender: TObject);
begin
  StatusBar.Panels[0].Text:='Rendering';
end;

procedure TPresetEdit3Form.PresetEditorEndRender(Sender: TObject);
begin
  StatusBar.Panels[0].Text:='---';
end;

procedure TPresetEdit3Form.PresetEditorRender(Sender: TObject);
begin
  StatusBar.Panels[1].Text:='Moving Wires: '+IntToStr(FPresetEditor.PresetEditor.MovingWires);
end;

function TPresetEdit3Form.EditParam(AParamPath: IParamPath; AParam: IPParam): Boolean;
begin
  FPresetEditor.PresetEditor.AssignParam(AParam, AParamPath);
  Result:=true;
  Caption:=AParamPath.ToString;
  Show;
end;

procedure TPresetEdit3Form.LoadFromFile(Sender: TObject);
var
  AMsg            : TProgressInfoMsg;
  AProgressHandler: TProgressHandler;
begin
  if OpenDialog.Execute then begin
    AMsg:=TProgressInfoMsg.Create('Loading ' + OpenDialog.FileName, IMT_OPEN);
    AProgressHandler:=AMsg.GetProgressHandler;
    VisFiles.LoadFromFile(OpenDialog.FileName, AProgressHandler);
    AProgressHandler.Destroy;
    AMsg.Destroy;
  end;
end;

procedure TPresetEdit3Form.SaveToFile(Sender: TObject);
var
  AMsg            : TProgressInfoMsg;
  AProgressHandler: TProgressHandler;
  ATag            : string;
begin
  if SaveDialog.Execute then begin
    ATag:=TAGSAVE;
    if InputQuery('Enter Base Tag', 'Enter Base Tag', ATag) then begin
      AMsg:=TProgressInfoMsg.Create('Saving ' + OpenDialog.FileName, IMT_OPEN);
      AProgressHandler:=AMsg.GetProgressHandler;
      VisFiles.SaveToFile(SaveDialog.FileName, ATag, AProgressHandler);
      AProgressHandler.Destroy;
      AMsg.Destroy;
    end;
  end;
end;

end.

