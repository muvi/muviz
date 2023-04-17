unit EditPresetNew;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, Spin, ComCtrls, VisTypeUnit, PresetUtil2,
  ChooseColor, types;

type

  { TNewPresetEditForm }

  TNewPresetEditForm = class(TForm)
    KnotDistanceEdit: TFloatSpinEdit;
    FrictionEdit: TFloatSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    DEdit: TFloatSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    RestForceEdit: TFloatSpinEdit;
    SimStepsEdit: TSpinEdit;
    StatusBar: TStatusBar;
    WeightEdit: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SettingPanel: TPanel;
    PlugImageList: TImageList;
    ScrollBar: TScrollBar;
    PixelSizeEdit: TFloatSpinEdit;
    WireTimer: TTimer;
    procedure DEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FrictionEditChange(Sender: TObject);
    procedure KnotDistanceEditChange(Sender: TObject);
    procedure PixelSizeEditChange(Sender: TObject);
    procedure RestForceEditChange(Sender: TObject);
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
    FEasterStr   : string;
  public
    function EditPreset(APreset: TPreset): Boolean;
    procedure ShowAdvancedControls;
  end;

var
  NewPresetEditForm: TNewPresetEditForm;

implementation

{$R *.lfm}

const
  EasterShowAdvancedControls = 'ADVCONTROLS ON';
  EasterHideAdvancedControls = 'ADVCONTROLS OFF';

{TNewPresetEditForm}

procedure TNewPresetEditForm.FormCreate(Sender: TObject);
begin
  FBGBmp:=TBitmap.Create;
  PlugImageList.GetBitmap(PlugImageList.Tag,FBGBmp);
  FPresetEditor:=TPresetEditor.Create(Self);
  InsertControl(FPresetEditor);
  with FPresetEditor do begin
    Align:=alClient;
    Images:=PlugImageList;
    SlidebarImageIndex:=6;
    HighlightedSlidebarImageIndex:=7;
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
    BoolBoxUI:=6;
    BoolBoxUHI:=7;
    BoolBoxDI:=4;
    BoolBoxDHI:=5;
    ExpandImageIndex:=9;
    ExpandHImageIndex:=10;
    ExpandDImageIndex:=11;
    DeexpandImageIndex:=12;
    DeexpandHImageIndex:=13;
    DeexpandDImageIndex:=14;
    BackgroundBitmap:=FBGBmp;
    ConfirmPlugTypes;
    OnStartRender:=@PresetEditorStartRender;
    OnEndRender:=@PresetEditorEndRender;
    OnMaxScrollPosChanged:=@PresetEditorResize;
    OnResize:=@PresetEditorResize;
    OnMouseWheel:=@FormMouseWheel;
  end;
  ActiveControl:=FPresetEditor;
  FSimSteps:=3;
  StatusBar.Panels[0].Text:='Rendering';
  FEasterStr:='';
end;

procedure TNewPresetEditForm.DEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.D:=DEdit.Value;
end;

procedure TNewPresetEditForm.FormDestroy(Sender: TObject);
begin
  RemoveControl(FPresetEditor);
  FPresetEditor.Destroy;
  FBGBmp.Destroy;
end;

procedure TNewPresetEditForm.FormHide(Sender: TObject);
begin
  WireTimer.Enabled:=false;
  FPresetEditor.RemovePreset;
end;

procedure TNewPresetEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //#198 entspricht der '-' Taste
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

procedure TNewPresetEditForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FPresetEditor.ScrollPos:=FPresetEditor.ScrollPos+WheelDelta*ScrollBar.LargeChange;
end;

procedure TNewPresetEditForm.FormShow(Sender: TObject);
begin
  WireTimer.Enabled:=true;
end;

procedure TNewPresetEditForm.ShowAdvancedControls;
begin
  SettingPanel.Visible:=true;
  StatusBar.Visible:=true;
  with FPresetEditor.WireSettings do begin
    FrictionEdit.Value:=Friction;
    KnotDistanceEdit.Value:=KnotDistance;
    WeightEdit.Value:=Weight;
    DEdit.Value:=D;
    SimStepsEdit.Value:=FSimSteps;
    PixelSizeEdit.Value:=FPresetEditor.PixelSize;
    RestForceEdit.Value:=RestForce;
  end;
end;

procedure TNewPresetEditForm.FrictionEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.Friction:=FrictionEdit.Value;
end;

procedure TNewPresetEditForm.KnotDistanceEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.KnotDistance:=KnotDistanceEdit.Value;
end;

procedure TNewPresetEditForm.PixelSizeEditChange(Sender: TObject);
begin
  FPresetEditor.PixelSize:=PixelSizeEdit.Value;
end;

procedure TNewPresetEditForm.RestForceEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.RestForce:=RestForceEdit.Value;
end;

procedure TNewPresetEditForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FPresetEditor.ScrollPos:=ScrollPos;
end;

procedure TNewPresetEditForm.SimStepsEditChange(Sender: TObject);
begin
  FSimSteps:=SimStepsEdit.Value;
end;

procedure TNewPresetEditForm.WeightEditChange(Sender: TObject);
begin
  FPresetEditor.WireSettings.Weight:=WeightEdit.Value;
end;

procedure TNewPresetEditForm.PresetEditorStartRender(Sender: TObject);
begin
  StatusBar.Panels[0].Text:='Rendering';
  WireTimer.Enabled:=true;
end;

procedure TNewPresetEditForm.PresetEditorEndRender(Sender: TObject);
begin
  WireTimer.Enabled:=false;
  StatusBar.Panels[0].Text:='---';
end;

procedure TNewPresetEditForm.PresetEditorResize(Sender: TObject);
var
  AMaxScrollPos: Integer;
begin
  AMaxScrollPos:=FPresetEditor.MaxScrollPos;
  ScrollBar.Max:=AMaxScrollPos+FPresetEditor.Height;
  ScrollBar.PageSize:=FPresetEditor.Height;
  if FPresetEditor.ScrollPos>ScrollBar.Max then FPresetEditor.ScrollPos:=ScrollBar.Max;
  ScrollBar.Visible:=(AMaxScrollPos>0);
end;

procedure TNewPresetEditForm.WireTimerTimer(Sender: TObject);
begin
  FPresetEditor.Rewire(WireTimer.Interval/1000,FSimSteps);
  StatusBar.Panels[1].Text:='Moving Wires: '+IntToStr(FPresetEditor.MovingCount);
end;

function TNewPresetEditForm.EditPreset(APreset: TPreset): Boolean;
begin
  FPreset:=APreset;
  FPresetEditor.AssignPreset(APreset);
  PresetEditorResize(nil);
  Result:=true;
  Caption:=APreset.Name;
  Show;
end;

end.

