unit EditPreset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, Spin, ComCtrls, VisTypeUnit, PresetUtil,
  ChooseColor;

type

  { TPresetEditForm }

  TPresetEditForm = class(TForm)
    KnotDistanceEdit: TFloatSpinEdit;
    FrictionEdit: TFloatSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    DEdit: TFloatSpinEdit;
    Label5: TLabel;
    SimStepsEdit: TSpinEdit;
    StatusBar: TStatusBar;
    WeightEdit: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SettingPanel: TPanel;
    PlugImageList: TImageList;
    ScrollBar: TScrollBar;
    WireTimer: TTimer;
    procedure DEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrictionEditChange(Sender: TObject);
    procedure KnotDistanceEditChange(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SimStepsEditChange(Sender: TObject);
    procedure WeightEditChange(Sender: TObject);
    procedure WireTimerTimer(Sender: TObject);
    procedure PresetEditorStartRender(Sender: TObject);
    procedure PresetEditorEndRender(Sender: TObject);
    procedure PresetEditorResize(Sender: TObject);
  private
    FPreset      : TPreset;
    FPresetEditor: TPresetEditor;
    FSimSteps    : Integer;
    FBGBmp       : TBitmap;
  public
    function EditPreset(APreset: TPreset): Boolean;
  end;

var
  PresetEditForm: TPresetEditForm;

implementation

{$R *.lfm}

{TPresetEditForm}

procedure TPresetEditForm.FormCreate(Sender: TObject);
begin
  FBGBmp:=TBitmap.Create;
  PlugImageList.GetBitmap(PlugImageList.Tag,FBGBmp);
  FPresetEditor:=TPresetEditor.Create(Self);
  InsertControl(FPresetEditor);
  with FPresetEditor do begin
    Align:=alClient;
    Images:=PlugImageList;
    SlidebarImageIndex:=15;
    HighlightedSlidebarImageIndex:=16;
    PlugedImageIndex:=17;
    UnplugedImageIndex:=25;
    HighlightedPlugedImageIndex:=0;
    HighlightedUnplugedImageIndex:=8;
    BoolBoxUI:=8;
    BoolBoxUHI:=15;
    BoolBoxDI:=0;
    BoolBoxDHI:=24;
    ExpandImageIndex:=33;
    ExpandHImageIndex:=34;
    ExpandDImageIndex:=35;
    DeexpandImageIndex:=36;
    DeexpandHImageIndex:=37;
    DeexpandDImageIndex:=38;
    NotExpandedImageIndex:=15;
    NotExpandedHImageIndex:=16;
    BackgroundBitmap:=FBGBmp;
    OnStartRender:=@PresetEditorStartRender;
    OnEndRender:=@PresetEditorEndRender;
    OnResize:=@PresetEditorResize;
  end;
  FSimSteps:=3;
  StatusBar.Panels[0].Text:='Rendering';
end;

procedure TPresetEditForm.DEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.D:=DEdit.Value;
end;

procedure TPresetEditForm.FormDestroy(Sender: TObject);
begin
  RemoveControl(FPresetEditor);
  FPresetEditor.Destroy;
  FBGBmp.Destroy;
end;

procedure TPresetEditForm.FormHide(Sender: TObject);
begin
  WireTimer.Enabled:=false;
  FPresetEditor.RemovePreset;
end;

procedure TPresetEditForm.FormShow(Sender: TObject);
begin
  WireTimer.Enabled:=true;
end;

procedure TPresetEditForm.FrictionEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.Friction:=FrictionEdit.Value;
end;

procedure TPresetEditForm.KnotDistanceEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.KnotDistance:=KnotDistanceEdit.Value;
end;

procedure TPresetEditForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FPresetEditor.ScrollPos:=ScrollPos;
end;

procedure TPresetEditForm.SimStepsEditChange(Sender: TObject);
begin
  FSimSteps:=SimStepsEdit.Value;
end;

procedure TPresetEditForm.WeightEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.Weight:=WeightEdit.Value;
end;

procedure TPresetEditForm.PresetEditorStartRender(Sender: TObject);
begin
  StatusBar.Panels[0].Text:='Rendering';
  WireTimer.Enabled:=true;
end;

procedure TPresetEditForm.PresetEditorEndRender(Sender: TObject);
begin
  WireTimer.Enabled:=false;
  StatusBar.Panels[0].Text:='---';
end;

procedure TPresetEditForm.PresetEditorResize(Sender: TObject);
var
  AMaxScrollPos: Integer;
begin
  AMaxScrollPos:=FPresetEditor.MaxScrollPos;
  ScrollBar.Max:=AMaxScrollPos+FPresetEditor.Height;
  ScrollBar.PageSize:=FPresetEditor.Height;
  if FPresetEditor.ScrollPos>ScrollBar.Max then FPresetEditor.ScrollPos:=ScrollBar.Max;
  ScrollBar.Visible:=(AMaxScrollPos>0);
end;

procedure TPresetEditForm.WireTimerTimer(Sender: TObject);
begin
  FPresetEditor.Rewire(WireTimer.Interval/1000,FSimSteps);
  StatusBar.Panels[1].Text:='Moving Wires: '+IntToStr(FPresetEditor.MovingCount);
end;

function TPresetEditForm.EditPreset(APreset: TPreset): Boolean;
begin
  FPreset:=APreset;
  FPresetEditor.AssignPreset(APreset);
  PresetEditorResize(nil);
  Result:=true;
  Show;
end;

end.

