unit MainUnit;

{$mode objfpc}{$H+}

interface

{$DEFINE UPDATESCREEN}
{$DEFINE DAX}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VisUnit, Spin, BasicFreqAna, ExtPtrType, AdvType, ComCtrls,
  VisDrawUnit, PresetUnit, MNetUnit, MPluginSystem, LCLType, Buttons, AdvFunc,
  SpectrumData, Menus, EditBtn, Grids, CheckLst, VisWinsUnit, VisWinType,
  KeyboardUnit, AdvGui, StyleControls, CXCtrls, BeatMon, ClientNetType,
  VisTypeUnit, KeyPreUnit, DllStrUnit, DllStr, AuthNetType, StdDllCtrls,
  StdDllInterfaces, FreqAna3, WaveDest, AnalyseSettings, PresetList, Math,
  SubbandEditUnit, AuthNetUnit, VPBufferUnit, ArrayListedObjects, FreqAna2,
  PluginListUnit, PluginType, ParamPainter, VisType, DividerBevel, PictureType,
  PictureTypeUnit, LoadSelectUnit, MPFFreqAna, DiagDrawUnit, PicDiagUnit,
  GenericMemoryArrays, DynamicBase, InputManager, AsioVis2, PlugPicTestUnit,
  GLVisUnit, AboutUnit, SettingUnit, AdjustUnit, SelectPresetUnit, ImportUnit,
  EditPreset3, VisualisationMain, DebugTools, Configuration, CallParamType,
  IntegerParamType, FloatParamType, ColorParamType, PointerParamEdit,
  BooleanParamType, BufferParamType, StringParamType, PresetParamType,
  PointerParamType, VisType2, StdParamTypes, StdParamEdits, PresetParamEdit,
  GraphX32, DragPlugUnit, PresetType, ToolboxUnit, LogUnit,
  GLVisWin2, StdVisualisations, MStrings, MPluginSystem4, MStringImpl,
  LCLAdvFunc, PresetUtil3_Path, ValueStorages, VisPresetList3, Buffers,
  KeyboardVisualisations, ValueStoragePreset, TVSPClient, GlobalTVSP, TVSPConst,
  Mide, PerformanceTestUnit, TagParamType, ParamTypeImpl, InfoMsgs;

type

  TMuviBalloonCategory = (mbcGeneral,mbcImportant,mbcUserInterface,mbcBeat,mbcNetwork,mbcInvalid);

  { TMainForm }

  TMainForm = class(TForm)
    AutoSyncCB: TCheckBox;
    Beat600CB: TCheckBox;
    BeatRelationAMB: TAlphaMorphBitmap;
    AnalyseSettingsBtn: TButton;
    AudioControlCenterBtn: TButton;
    LineWidthEdit: TFloatSpinEdit;
    DragMainPresetPMI: TMenuItem;
    MainThreadTimer: TTimer;
    EditArbitraryPresetPMI: TMenuItem;
    LogPMI: TMenuItem;
    ShowInfoPMI: TMenuItem;
    StatisticsPMI: TMenuItem;
    PerformanceTestMI: TMenuItem;
    MideMI: TMenuItem;
    UniqueStringPMI: TMenuItem;
    Preset2TB: TToolButton;
    ToolboxTB: TToolButton;
    ImportTB: TToolButton;
    WABtn: TBitBtn;
    BufferBtn: TBitBtn;
    BufferSelectBtn: TBitBtn;
    AdvancedPMI: TPopupMenu;
    AdvancedTB: TToolButton;
    HelpPMI: TPopupMenu;
    HelpTB: TToolButton;
    AboutMI: TMenuItem;
    DefaultStyle: TStyleControlStyle;
    NetworkDividerPanel: TPanel;
    SettingsMI: TMenuItem;
    SettingsTB: TToolButton;
    WAFPSEdit: TFloatSpinEdit;
    GraphicFPSLbl: TLabel;
    MasterVolumeLbl: TLabel;
    MasterVolumeOffBtn: TButton;
    MasterVolumeTB: TTrackBar;
    NewGLWinMI: TMenuItem;
    MPFLbl: TLabel;
    NewGLWinBtn: TButton;
    SyncModeLbl: TLabel;
    EnableBPMMI: TMenuItem;
    EnableSyncMI: TMenuItem;
    N8: TMenuItem;
    DebugToolsTB: TToolButton;
    DebugToolsPMI: TPopupMenu;
    BeatMonitorMI: TMenuItem;
    ShowDiagBtn: TButton;
    AddDiagBtn: TButton;
    DiagSelectCB: TComboBox;
    DiagGB: TStyleBox;
    SyncModeCB: TComboBox;
    AudioTB: TToolBar;
    ToolButton1: TToolButton;
    WAEnabledCB: TCheckBox;
    VPBLbl: TLabel;
    GraphicBGActivityLbl: TLabel;
    GraphicBGActivityImg: TImage;
    GraphicBGActivityValLbl: TLabel;
    VisProgressLbl: TLabel;
    VisProgressPB: TProgressBar;
    VisProgressPanel: TPanel;
    ParamPB: TBufferedPaintBox;
    ParamGB: TStyleBox;
    PluginMI: TMenuItem;
    N7: TMenuItem;
    ResolutionXLbl: TLabel;
    ResolutionYSE: TSpinEdit;
    ParamSB: TScrollBar;
    UserEdit: TEdit;
    BPMSyncMI: TMenuItem;
    PortLbl: TLabel;
    PWEdit: TEdit;
    UserLbl: TLabel;
    PWLbl: TLabel;
    PortEdit: TSpinEdit;
    SubbandEditMI: TMenuItem;
    N6: TMenuItem;
    SyncTapBtn: TButton;
    WAKeyCB: TComboBox;
    BPMAnalysisFramesLbl: TLabel;
    BeatPMI: TPopupMenu;
    BPMFacDblMI: TMenuItem;
    BPMFacHalfMI: TMenuItem;
    BPMSyncTapMI: TMenuItem;
    BPMNormMI: TMenuItem;
    BalloonHintsGB: TStyleBox;
    BeatIB: TIndexBar;
    BPMLbl: TLabel;
    DriverCB: TComboBox;
    DriverLbl: TLabel;
    SyncBtn: TBitBtn;
    SyncImageList: TImageList;
    PlugImageList: TImageList;
    KeyPreMI: TMenuItem;
    NetworkBHCB: TCheckBox;
    N5: TMenuItem;
    BeatBHCB: TCheckBox;
    PluginSB: TScrollBox;
    GraphicPriorityCB: TComboBox;
    ImageList: TImageList;
    DisconnectBtn: TButton;
    DisconnectMI: TMenuItem;
    NetCommanderMI: TMenuItem;
    ConnectMI: TMenuItem;
    NewWinMI: TMenuItem;
    N3: TMenuItem;
    NetworkPMI: TPopupMenu;
    PresetMI: TMenuItem;
    KeyboardMI: TMenuItem;
    OeffnenMI: TMenuItem;
    AppendMI: TMenuItem;
    N2: TMenuItem;
    CloseMI: TMenuItem;
    FilePMI: TPopupMenu;
    AppendTB: TToolButton;
    PresetTB: TToolButton;
    KeyboardTB: TToolButton;
    NetCommanderTB: TToolButton;
    BeatTimer: TTimer;
    BPMTB: TToolBar;
    BPMFacDblTB: TToolButton;
    BPMFacHalfTB: TToolButton;
    BeatTB: TToolButton;
    KeyPreTB: TToolButton;
    BeatDividerTB: TToolButton;
    VisualisationPMI: TPopupMenu;
    ToolToolBar: TToolBar;
    OpenTB: TToolButton;
    MenuTB: TToolBar;
    FileTB: TToolButton;
    VisualisationTB: TToolButton;
    NetworkTB: TToolButton;
    ResolutionXSE: TSpinEdit;
    ResolutionLbl: TLabel;
    GraphicPriorityLbl: TLabel;
    GraphicGB: TStyleBox;
    NewWinBtn: TButton;
    VisWinGB: TStyleBox;
    ConnectBtn: TButton;
    IPEdit: TEdit;
    IPLbl: TLabel;
    NetworkGB: TStyleBox;
    OpenDialog: TOpenDialog;
    WAHC: THeaderControl;
    DeviceCB: TComboBox;
    AudioGB: TStyleBox;
    DeviceLbl: TLabel;
    TrayIcon: TTrayIcon;
    WABufferCB: TComboBox;
    WALB: TListBox;
    WAPanel: TPanel;
    procedure AboutMIClick(Sender: TObject);
    procedure AnalyseSettingsBtnClick(Sender: TObject);
    procedure AudioControlBtnClick(Sender: TObject);
    procedure Beat600CBChange(Sender: TObject);
    procedure BeatBHCBChange(Sender: TObject);
    procedure BeatMonitorMIClick(Sender: TObject);
    procedure DragMainPresetPMIClick(Sender: TObject);
    procedure ImportTBClick(Sender: TObject);
    procedure LineWidthEditChange(Sender: TObject);
    procedure LogPMIClick(Sender: TObject);
    procedure MainThreadTimerTimer(Sender: TObject);
    procedure EditArbitraryPresetPMIClick(Sender: TObject);
    procedure PerformanceTestMIClick(Sender: TObject);
    procedure MideMIClick(Sender: TObject);
    procedure Preset2TBClick(Sender: TObject);
    procedure SettingsMIClick(Sender: TObject);
    procedure BeatTimerTimer(Sender: TObject);
    procedure BPMFacDblMIClick(Sender: TObject);
    procedure BPMFacHalfMIClick(Sender: TObject);
    procedure BPMFacNormalMIClick(Sender: TObject);
    procedure DriverCBChange(Sender: TObject);
    procedure CloseMIClick(Sender: TObject);
    procedure DisconnectMIClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure GraphicPriorityCBChange(Sender: TObject);
    procedure KeyPreMIClick(Sender: TObject);
    procedure MasterVolumeOffBtnClick(Sender: TObject);
    procedure MenuTBClick(Sender: TObject);
    procedure MenuTBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure NetCommanderMIClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ConnectMIClick(Sender: TObject);
    procedure KeyboardMIClick(Sender: TObject);
    procedure AppendMIClick(Sender: TObject);
    procedure NetworkBHCBChange(Sender: TObject);
    procedure NewGLWinMIClick(Sender: TObject);
    procedure OpenMIClick(Sender: TObject);
    procedure ParamPBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ParamPBMouseLeave(Sender: TObject);
    procedure ParamPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ParamPBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ParamPBResize(Sender: TObject);
    procedure ParamSBChange(Sender: TObject);
    procedure PluginMIClick(Sender: TObject);
    procedure PresetMIClick(Sender: TObject);
    procedure DeviceCBChange(Sender: TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewWinMIClick(Sender: TObject);
    procedure ResolutionSEChange(Sender: TObject);
    procedure ShowInfoPMIClick(Sender: TObject);
    procedure StatisticsPMIClick(Sender: TObject);
    procedure SubbandEditMIClick(Sender: TObject);
    procedure SyncMIClick(Sender: TObject);
    procedure SyncTapMIClick(Sender: TObject);
    procedure MasterVolumeTBChange(Sender: TObject);
    procedure ToolboxTBClick(Sender: TObject);
    procedure UniqueStringPMIClick(Sender: TObject);
    procedure WABtnClick(Sender: TObject);
    procedure WABtnChange(Sender: TObject);
    procedure WABtnKeyPress(Sender: TObject; var Key: char);
    //procedure WAFPSCBChange(Sender: TObject);
    procedure WAHCSectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure WALocalVisWinFPSEditChange(Sender: TObject);
    procedure WAGLVisWinFPSEditChange(Sender: TObject);
    procedure WAKeyCBChange(Sender: TObject);
    procedure WABufferCBChange(Sender: TObject);
    procedure WAEnabledCBChange(Sender: TObject);
    procedure WALBDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure WALBSelectionChange(Sender: TObject; User: boolean);
    procedure NetConnected;
    procedure NetFinishRequest(const ARequest: string; AState: TNetState);
    procedure FParamPainterPaint(Sender: TObject; Param: TPPParam);
    function FParamPainterRequestBG(Sender: TObject; Param: TPPParam): boolean;
    procedure PresetLoaderStarted(Sender: TObject);
    procedure PresetLoaderFinished(Sender: TObject);
    procedure BufferBtnClick(Sender: TObject);
    procedure BufferSelectBtnClick(Sender: TObject);
    procedure AutoSyncCBChange(Sender: TObject);
    procedure EnableSyncMIClick(Sender: TObject);
    procedure EnableBPMMIClick(Sender: TObject);
    procedure SyncModeCBChange(Sender: TObject);
    procedure ShowDiagBtnClick(Sender: TObject);
    procedure AddDiagBtnClick(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  private
    //TODO: for debugging only, remove it
    FStarted             : Boolean;

    FMainPreset          : IPPreset;
    FBGColor             : IPColor;
    FHighlightColor      : IPColor;
    FFontColor           : IPColor;
    FFontHighlightColor  : IPColor;
    FFontInHighlightColor: IPColor;
    procedure GetSystemColors;
    procedure FreeConfig;
  public
    PluginSystem: TMPluginSystem3;
    SpectrumData: TAdvancedBeatSpectrumData;
    Net         : TMuviNet;
    VisKeyboards: TVisKeyboards;
    VisPresets  : TPresets;
    VisWins     : TVisWins;
    PresetLoader: TPresetLoader;
    procedure VisWinClosed(AObject: TArrayListedObject; Index: Integer);
    procedure ShowBalloon(const AHint: string; const ACategory: TMuviBalloonCategory; AFlags: TBalloonFlags = bfNone; const ATitle: string = 'Muvi'; const ATimeout: integer = 3000);
    procedure ShowNetBalloon(const ATitle, AHint: string; AFlags: TBalloonFlags; const ATimeout: Integer);
    procedure DrawBeat;
    procedure DrawFrameInfo;
    procedure DrawTests;
    procedure PluginAreaRemoved(AObject: TArrayListedObject; Index: Integer);
    procedure InputManagerAddDriver(Sender: TObject);

    property Started: Boolean read FStarted;
  private
    ProgPath       : string;
    LastBPMFound   : Boolean;
    LastSyncTogg   : Boolean;
    SyncShowTime   : Integer;
    FStringManager : TStringManager;
    FShownCategorys: set of TMuviBalloonCategory;
    FNewPluginTop  : Integer;
    FPluginAreas   : TObjectArray;
    FOpenMenuTB    : TToolButton;
    BufferManager  : TVPBufferManager;
    FParamPainter  : TParamPainter;
    FSelWin        : TVisWin;
    FTrueIcon      : TBitmap;
    FFalseIcon     : TBitmap;
    FInputManager  : TInputManager;
    FSettings      : TSettingsManager;
    procedure Vised;
    procedure SetWACB;
    procedure SetWAKeyCB;
    function AddGUIArea(const AHeight: IEInt): IEGUIArea;
    procedure UpdateParams;
    procedure DrawParams;
    procedure SelWinChangePreset(Sender: TVisWin);
    procedure GetParamVal(Param: Pointer; AType: TVisParamType; out Value);
    procedure SetParamVal(Param: Pointer; AType: TVisParamType; const Value);
    procedure CallDiagProc(DiagProc: TDiagShowProc);
    function GetTakeFirstHintText(X, Y: dsi): string;
    procedure ChangeTakeFirstDiagBam(var ABam: TBam);
    procedure InitSettings; inline;
  end;

var
  MainForm: TMainForm;

const
  //Öffnen/Speichern
  MultiLoadFilter = 'AlleDateien (*.*)|*.*|' +
    'Bekannte Dateitypen (*.mvp;*.mkl;*.mcf;*.muf;*.mvl;*.vll;*.d'
    + 'll;*.mfl;*.ini;*.txt)|*.mvp;*.mkl;*.mcf;*.muf;*.mvl;*.vll;*.'
    + 'dll;*.mfl;*.ini;*.txt|Presets und Tastaturbelegung (*.mvp;*.'
    + 'mkl;*.mcf;*.muf)|*.mvp;*.mkl;*.mcf;*.muf|Visualisierungen (*'
    + '.mvl;*.vll;*.dll)|*.mvl;*.vll;*.dll|Muvi Dateilisten (*.mfl;'
    + '*.ini;*.txt)|*.mfl;*.ini;*.txt|Einzelne Visualisierungen (*.'
    + 'vll;*.dll)|*.vll;*.dll|Muvi Visualisierungspresets (*.mvp)|*'
    + '.mvp|Muvi Tastaturbelegungen (*.mkl)|*.mkl|Muvi Visualisieru'
    + 'ngslisten (*.mvl)|*.mvl|Muvi Visualisierungen (*.vll)|*.vll|'
    + 'Muvi Dateiliste (*.mfl)|*.mfl|Muvi Containerdatei (*.mcf)|*.'
    + 'mcf|Preset oder Tastaturbelegung (*.muf)|*.muf|Anwendungserw'
    + 'eiterungen (*.dll)|*.dll|Konfigurationseinstellungen (*.ini)'
    + '|*.ini|Textdokumente (*.txt)|*.txt';
  IniFileName        ='Muvi.ini';
  SettingFileName    ='Muvi.bcf';
  //Design
  BeatSpaceX         =10;
  BeatSpaceY         =3;
  DiagHeight         =100;
  SyncTestCount      =50;
  ParamXOffset       =150;
  NotParamWidth      =ParamXOffset+20;
  ParamHeight        =25;
  ParamYOffset       =3;
  ParamYOffset2      =2*ParamYOffset;
  CompleteParamHeight=ParamHeight+ParamYOffset2;
  ParamTextOffset    =5;
  MaxGraphicActivity =5;
  //WALB
  WALB_ComboTop    = 4;  //(WALB.ItemHeight{30}-21) div 2
  WALB_CBTop       = 8;  //(WALB.ItemHeight{30}-13) div 2
  WALB_ComboLeft   = 83; //21+31+31
  WALB_PicTop      = 5;  //(WALB.ItemHeight{30}-ImageList.Height{20}) div 2
  WALB_ComboOffset = 1;  //Anzahl Pixel, um die die Comboboxen nach links verschoben sind
  WALB_ComboSizeRed= 2;  //Anzahl Pixel, um die die Comboboxen kleiner als die Headersection sind

  //Color Scheme
  {clsBG             = $414C48;
  clsHighlight      = $818C88;
  clsFont           = $A1ACA8;
  clsFontHighlight  = $0000FF;
  clsFontInHighlight= $414C48;}
  //DefaultScheme
  {clsBG             = clBtnFace;
  clsHighlight      = clMenuHighlight;
  clsFont           = clWindowText;
  clsFontHighlight  = clRed;
  clsFontInHighlight= clWindow;}

procedure SetColorScheme(AContext: Pointer; Sender, SenderData: IInterface); cdecl;

implementation

{$R *.lfm}

{%REGION TMainForm}

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose:=(VisWins.Count=0) or (AdvMsg('Close all vis windows and exit?',TdMFrage)=mrYes);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MainThreadTimer.Enabled:=false;
  ToolBoxForm.Clear;

  FSettings.SaveToFile(ProgPath+SettingFileName);
  OnKeyDown:=nil;
  OnKeyPress:=nil;
  OnKeyUp:=nil;
  BeatTimer.Enabled:=false;
  WALB.Items.Clear;
  SetGlobalVisedEvent(nil);
  VisWins.Destroy;
  PluginSystem.SavePluginLibrarys(ProgPath+IniFileName);
  //to prevent late destruction of presets... (don't know why)
  VisualisationSystem.Clear;

  PluginSystem.Destroy;
  FPluginAreas.Destroy;
  TrayIcon.Hide;
  Net.Destroy;
  PresetLoader.Destroy;
  FParamPainter.Destroy;
  FreeConfig;
  {$IFDEF DAX}
  FInputManager.Destroy;
  AdvSpectrumData:=nil;
  SpectrumData.Destroy;
  {$ENDIF}
  VisPresets.Destroy;
  BufferManager.Destroy;
  FStringManager.Destroy;
  FFalseIcon.Destroy;
  FTrueIcon.Destroy;
  FSettings.Destroy;
  VisualisationSystemDone;

  Buffers.Done;
  MStringImpl.Done;
  GlobalTVSP.Done;
end;

procedure TMainForm.DeviceCBChange(Sender: TObject);
begin
  FInputManager.Machine:=DeviceCB.ItemIndex;
end;

procedure TMainForm.PresetMIClick(Sender: TObject);
begin
  PresetForm.EditPresets(VisPresets);
  SetWACB;
end;

procedure TMainForm.BufferBtnClick(Sender: TObject);
begin
  if WALB.ItemIndex>=0 then VisWins[WALB.ItemIndex].Compositions.LoadAll;
end;

procedure TMainForm.BufferSelectBtnClick(Sender: TObject);
begin
  if WALB.ItemIndex>=0 then LoadSelectForm.SelectAndLoad(VisWins[WALB.ItemIndex].Compositions);
end;

procedure TMainForm.AppendMIClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    if not LoadResultMessage(PluginSystem.MultiLoad(OpenDialog.FileName, True),OpenDialog.FileName) then begin
      PresetForm.UpdateVis;
      SetWACB;
      SetWAKeyCB;
    end;
  end;
end;

procedure TMainForm.NetworkBHCBChange(Sender: TObject);
begin
  if NetworkBHCB.Checked
    then Include(FShownCategorys, mbcNetwork)
    else Exclude(FShownCategorys, mbcNetwork);
end;

procedure TMainForm.NewGLWinMIClick(Sender: TObject);
var
  AVisWin: TVisWin;
begin
  AVisWin:=TGLVisWin.Create('Unnamed',0,0,Classes.Point(ResolutionXSE.Value,ResolutionYSE.Value),SpectrumData,VisKeyboards,VisPresets,PresetLoader);
  VisWins.AddItem(AVisWin);
  WALB.Items.Add('');

  Net.SendBlockDataUpdate([Net.VisWinBlockID]);
end;

procedure TMainForm.OpenMIClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    if not LoadResultMessage(PluginSystem.MultiLoad(OpenDialog.FileName),OpenDialog.FileName) then begin
      PresetForm.UpdateVis;
      SetWACB;
      SetWAKeyCB;
    end;
  end;
end;

procedure TMainForm.ParamPBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FParamPainter.MouseDown;
end;

procedure TMainForm.ParamPBMouseLeave(Sender: TObject);
begin
  FParamPainter.MouseLeave;
end;

procedure TMainForm.ParamPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  FParamPainter.MouseMove(X, Y);
end;

procedure TMainForm.ParamPBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FParamPainter.MouseUp;
end;

procedure TMainForm.ParamPBResize(Sender: TObject);
begin
  ParamSB.PageSize:=ParamPB.Height;
  FParamPainter.AutoRepaint:=False;
  FParamPainter.Width:=ParamPB.Width-NotParamWidth;
  DrawParams;
  FParamPainter.AutoRepaint:=True;
  ParamSB.Visible:=(ParamSB.Max-ParamSB.PageSize>0);
end;

procedure TMainForm.ParamSBChange(Sender: TObject);
begin
  DrawParams;
end;

procedure TMainForm.PluginMIClick(Sender: TObject);
begin
  PluginForm.ShowPlugins(PluginSystem);
end;

procedure TMainForm.NetCommanderMIClick(Sender: TObject);
begin
  Net.ShowCommander;
end;

procedure TMainForm.CloseMIClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AudioControlBtnClick(Sender: TObject);
begin
  FInputManager.ShowControlPanel;
end;

procedure TMainForm.Beat600CBChange(Sender: TObject);
begin
  SpectrumData.UseBeat600:=Beat600CB.Checked;
end;

procedure TMainForm.AnalyseSettingsBtnClick(Sender: TObject);
begin
  AnalyseSettingForm.SetAnalyseSettings(SpectrumData);
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.BeatBHCBChange(Sender: TObject);
begin
  if BeatBHCB.Checked
    then Include(FShownCategorys,mbcBeat)
    else Exclude(FShownCategorys,mbcBeat);
end;

procedure TMainForm.BeatMonitorMIClick(Sender: TObject);
begin
  BeatMonForm.Show;
end;

procedure TMainForm.DragMainPresetPMIClick(Sender: TObject);
begin
  DragValue(PresetUtil.Presets[VIDCONFIG].Inputs[ParamID(MainPresetName, vPreset)]);
end;

procedure TMainForm.ImportTBClick(Sender: TObject);
begin
  ImportUnit.ImportOldPresets(VisPresets);
end;

procedure TMainForm.LineWidthEditChange(Sender: TObject);
begin
  if FSelWin<>nil
    then if FSelWin is TGLVisWin
      then TGLVisWin(FSelWin).LineWidth:=LineWidthEdit.Value;
end;

procedure TMainForm.LogPMIClick(Sender: TObject);
begin
  LogForm.Show;
  Log('---Log opened---');
end;

procedure TMainForm.MainThreadTimerTimer(Sender: TObject);
begin
  VisualisationSystem.ExecuteMainThread;
end;

procedure TMainForm.EditArbitraryPresetPMIClick(Sender: TObject);
var
  AID         : TGUID;
  APresetParam: IPPreset;
begin
  if TryStringToGUID(
    Dialogs.InputBox('Edit arbitrary preset', 'Enter preset ID', '{00000000-0000-0000-0000-000000000000}'),
    AID) then begin
      APresetParam:=ValueStorages.CreatePresetValueStorage(AID);
      PresetEdit3Form.EditParam(NewParamPath(APresetParam.ID), APresetParam);
    end;
end;

procedure TMainForm.PerformanceTestMIClick(Sender: TObject);
begin
  PerformanceTestForm.Show;
end;

procedure TMainForm.MideMIClick(Sender: TObject);
begin
  MideForm.Show;
end;

procedure TMainForm.Preset2TBClick(Sender: TObject);
var
  AID   : TPParamID;
begin
  AID:=ParamID(MAINPRESETNAME, vPreset);
  PresetEdit3Form.EditParam(NewParamPath(AID), Config[AID]);
end;

procedure TMainForm.SettingsMIClick(Sender: TObject);
begin
  AdjustForm.EditSettings;
end;

procedure TMainForm.BeatTimerTimer(Sender: TObject);
begin
  if SpectrumData.Locked then exit;
  DrawBeat;
  BeatMonForm.DrawFrame();
  DrawFrameInfo;
  DrawTests;
end;

procedure TMainForm.BPMFacDblMIClick(Sender: TObject);
begin
  SpectrumData.BPMFac:=SpectrumData.BPMFac*2;
end;

procedure TMainForm.BPMFacHalfMIClick(Sender: TObject);
begin
  SpectrumData.BPMFac:=SpectrumData.BPMFac/2;
end;

procedure TMainForm.BPMFacNormalMIClick(Sender: TObject);
begin
  SpectrumData.BPMFac:=1;
end;

procedure TMainForm.DriverCBChange(Sender: TObject);
var
  I: Integer;
begin
  FInputManager.Driver:=DriverCB.ItemIndex;
  AudioControlCenterBtn.Visible:=fifControlPanel in FInputManager.Features;
  DeviceCB.Items.Clear;
  DeviceCB.Enabled:=(FInputManager.MachineCount>0);
  if DeviceCB.Enabled then begin
    for I:=0 to FInputManager.MachineCount-1
      do DeviceCB.Items.Add(FInputManager.MachineNames[I]);
    DeviceCB.ItemIndex:=FInputManager.Machine;
  end;
end;

procedure TMainForm.DisconnectMIClick(Sender: TObject);
begin
  Net.Disconnect;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  I         : Integer;
  ASucceeded: Boolean;
  AErrorStr : string;
begin
  ASucceeded:=True;
  AErrorStr:='';
  for I:=0 to Length(FileNames)-1 do
    if not lplSucceeded(PluginSystem.MultiLoad(FileNames[I],True).Success) then begin
      ASucceeded:=False;
      AErrorStr+=FileNames[I]+#$D#$A;
    end;
  if not ASucceeded
    then AdvMsg('Folgende Dateien konnten nicht geladen werden:'+#$D#$A+AErrorStr,TdMWarnung);
  SetWACB;
  SetWAKeyCB;
end;

procedure TMainForm.GraphicPriorityCBChange(Sender: TObject);
begin
  SetGlobalVisPriority(ThreadPrioritys[GraphicPriorityCB.ItemIndex]);
end;

procedure TMainForm.KeyPreMIClick(Sender: TObject);
begin
  KeyPreForm.GenerateAndShow(VisKeyboards,VisPresets);
end;

procedure TMainForm.MasterVolumeOffBtnClick(Sender: TObject);
begin
  SpectrumData.MasterVolume:=1.0;
  MasterVolumeTB.Position:=0;
end;

procedure TMainForm.MenuTBClick(Sender: TObject);
var
  APos: TPoint;
begin
  if FOpenMenuTB=Sender then exit;
  with TToolButton(Sender) do
  begin
    APos.X:=0;
    APos.Y:=Height;
    APos:=ClientToScreen(APos);
    FOpenMenuTB:=TToolButton(Sender);
    DropdownMenu.Popup(APos.X,APos.Y);
    FOpenMenuTB.Down:=false;
    FOpenMenuTB:=nil;
  end;
end;

procedure TMainForm.MenuTBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  //ggf. den Menübutten wechseln (alle bisherigen versuche scheiterten...)
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  VisWins.KeyDown(Chr(Key));
  KeyboardVisualisations.KeyPressed(Chr(Key));
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  //für spätere Verwendeung
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  VisWins.KeyUp;
end;

procedure TMainForm.ConnectMIClick(Sender: TObject);
begin
  Net.IPAddress:=IPEdit.Text;
  Net.Port:=PortEdit.Value;
  Net.Connect;
end;

procedure TMainForm.KeyboardMIClick(Sender: TObject);
begin
  KeyboardForm.EditKeyboards(VisKeyboards,VisPresets);
  SetWAKeyCB;
end;

procedure TMainForm.NetConnected;
begin
  Net.Auth(UserEdit.Text,PWEdit.Text);
end;

procedure TMainForm.NetFinishRequest(const ARequest: string; AState: TNetState);
begin
  if ARequest=srvAuth then begin
    if StateOK(AState) then begin
      Net.SendBlockDataUpdate;
      ShowBalloon('Anmelden erfolgreich',mbcNetwork,bfInfo);
    end else ShowBalloon('Anmelden nicht erfolgreich', mbcNetwork, bfWarning);
  end;
end;

procedure TMainForm.GetSystemColors;
begin
  FMainPreset:=IPPreset(Config[ParamID(MAINPRESETNAME, vPreset)]);
  FBGColor:=IPColor(Config[ParamID(BGCOLORNAME, vColor)]);
  FHighlightColor:=IPColor(Config[ParamID(HIGHLIGHTCOLORNAME, vColor)]);
  FFontColor:=IPColor(Config[ParamID(FONTCOLORNAME, vColor)]);
  FFontHighlightColor:=IPColor(Config[ParamID(FONTHIGHLIGHTCOLORNAME, vColor)]);
  FFontInHighlightColor:=IPColor(Config[ParamID(FONTINHIGHLIGHTCOLORNAME, vColor)]);
end;

procedure TMainForm.FreeConfig;
begin
  FMainPreset:=nil;
  FBGColor:=nil;
  FHighlightColor:=nil;
  FFontColor:=nil;
  FFontHighlightColor:=nil;
  FFontInHighlightColor:=nil;

  Config:=nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FStarted:=false;

  //connect to tvsp server
  GlobalTVSP.Init(TTVSPClient.Create(Self, 'localhost', DEFAULTPORT));
  MStringImpl.Init;
  Buffers.Init;

  InitVisualisationSystem;
  //register param types
  CallParamType.Register;
  IntegerParamType.Register;
  FloatParamType.Register;
  //ShortStringParamType.Register;
  ColorParamType.Register;
  BooleanParamType.Register;
  BufferParamType.Register;
  StringParamType.Register;
  PresetParamType.Register;
  PointerParamType.Register;
  TagParamType.Register;
  //create configuration visualisation
  ValueStoragePreset.Register;
  Configuration.Register;
  GetSystemColors;
  VisPresetList3.Register;
  //register param edits
  StdParamEdits.Register;
  PresetParamEdit.Register;
  PointerParamEdit.Register;
  //register additionl visualisations
  GLVisWin2.Register;
  StdVisualisations.Register;
  KeyboardVisualisations.Register;

  FNewPluginTop:=0;
  VisWins:=TVisWins.Create;
  VisWins.OnClosed:=@VisWinClosed;
  FStringManager:=TStringManager.Create;
  BufferManager:=TVPBufferManager.Create;
  FShownCategorys:=[mbcGeneral..mbcInvalid]-[mbcBeat];
  LastSyncTogg:=false;
  LastBPMFound:=false;

  SpectrumData:=TAdvancedBeatSpectrumData.Create(BufferManager);
  SpectrumData.SetCounts(512,2048,512,100,1000,1000,400,100,220500,20,2.0);
  AdvSpectrumData:=SpectrumData;

  VisPresets:=TPresets.Create;

  OpenDialog.Filter:=MultiLoadFilter;
  Net:=TMuviNet.Create(Self,FStringManager,VisWins,VisPresets);
  Net.OnConnected:=@NetConnected;
  Net.OnFinishRequest:=@NetFinishRequest;

  FInputManager:=TInputManager.Create(SpectrumData);
  FInputManager.AddDriver('ASIO',@NewAsioInput,AID_ASIO);

  FOpenMenuTB:=nil;
  {$IFDEF UPDATESCREEN}
  BeatTimer.Enabled:=true;
  {$ENDIF}
  FPluginAreas:=TObjectArray.Create;
  FPluginAreas.OnClosed:=@PluginAreaRemoved;
  PluginSystem:=TMPluginSystem4.Create(Net,FStringManager,VisWins,VisPresets,VisKeyboards,@AddGUIArea,BufferManager);
  PluginSystem.RegisterSourceType(PicSrcType,@NewPicSource);

  SetGlobalVisedEvent(@Vised);

  FParamPainter:=TParamPainter.Create(ParamPB, ParamPB.Buffer.Canvas,@GetParamVal,@SetParamVal,ParamPB.Width-NotParamWidth,ParamHeight);
  with FParamPainter do begin
    OnPaint:=@FParamPainterPaint;
    OnRequestBackground:=@FParamPainterRequestBG;
    Images:=PlugImageList;
    BoolBoxUI:=8;
    BoolBoxUHI:=9;
    BoolBoxDI:=10;
    BoolBoxDHI:=11;
    CallBoxUI:=10;
    CallBoxUHI:=11;
    CallBoxDI:=6;
  end;

  PresetLoader:=TPresetLoader.Create;
  PresetLoader.OnStarted:=@PresetLoaderStarted;
  PresetLoader.OnFinished:=@PresetLoaderFinished;

  //Ereignisse, die sonst durch zu frühes ausführen zu Fehlern führen würden
  ResolutionXSE.OnChange:=@ResolutionSEChange;
  ResolutionYSE.OnChange:=@ResolutionSEChange;
  //SelectPresetForm.OnChangePreset:=@WACBChange;

  MainThreadTimer.Enabled:=true;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  VisualisationSystem.ExecuteMainThread;
  Done:=true;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  I      : Integer;
  TempStr: string;
begin
  Application.AddOnIdleHandler(@ApplicationIdle, false);
  SelectPresetForm.Presets:=VisPresets;
  SelectPresetForm.OnChangePreset:=@WABtnChange;
  FTrueIcon:=TBitmap.Create;
  ImageList.GetBitmap(7,FTrueIcon);
  FFalseIcon:=TBitmap.Create;
  ImageList.GetBitmap(4,FFalseIcon);
  BeatMonForm.InitMonitor(SpectrumData);
  SetWACB;
  SetWAKeyCB;
  ProgPath:=ExtractFilePath(Application.ExeName);
  PluginSystem.MultiLoad(ProgPath+IniFileName);
  I:=1;
  TempStr:=ParamStr(1);
  while TempStr<>'' do begin
    PluginSystem.MultiLoad(TempStr,true);
    Inc(I);
    TempStr:=ParamStr(I);
  end;
  if Length(VisKeyboards)=0 then begin
    SetLength(VisKeyboards,1);
    InitKeyboard(VisKeyboards[0],'Tastaturbelegung 1');
  end;
  if VisPresets.Count=0
    then VisPresets.Add.Name:='Preset 1';
  with GraphicBGActivityImg do begin
    Picture.Bitmap.Width:=Width;
    Picture.Bitmap.Height:=Height;
  end;
  DrawParams;

  for I:=0 to FInputManager.DriverCount-1
    do DriverCB.Items.Add(FInputManager.DriverNames[I]);
  FInputManager.OnAddDriver:=@InputManagerAddDriver;

  FSettings:=TSettingsManager.Create;
  InitSettings;
  FSettings.LoadFromFile(ProgPath+SettingFileName);
  //set config notifictions
  Config[ParamID(BGCOLORNAME, vColor)].AddListener(@SetColorScheme, Self, VisualisationUtil.MainThread);
  Config[ParamID(HIGHLIGHTCOLORNAME, vColor)].AddListener(@SetColorScheme, Self, VisualisationUtil.MainThread);
  Config[ParamID(FONTCOLORNAME, vColor)].AddListener(@SetColorScheme, Self, VisualisationUtil.MainThread);
  Config[ParamID(FONTHIGHLIGHTCOLORNAME, vColor)].AddListener(@SetColorScheme, Self, VisualisationUtil.MainThread);
  Config[ParamID(FONTINHIGHLIGHTCOLORNAME, vColor)].AddListener(@SetColorScheme, Self, VisualisationUtil.MainThread);

  SetColorScheme(Self, nil, nil);

  InitDragging;
  ToolboxForm.UpdateAll;

  FStarted:=true;
end;

procedure TMainForm.InputManagerAddDriver(Sender: TObject);
begin
  DriverCB.Items.Add(FInputManager.DriverNames[FInputManager.DriverCount-1]);
end;

function TMainForm.AddGUIArea(const AHeight: IEInt): IEGUIArea;
var
  ANewArea: TEGUIArea;
begin
  ANewArea:=TEGUIArea.Create(Self);
  PluginSB.InsertControl(ANewArea.FObj);
  FPluginAreas.AddItem(ANewArea);
  ANewArea.FObj.Top:=FNewPluginTop;
  ANewArea.FObj.Height:=AHeight;
  FNewPluginTop+=AHeight;
  Result:=ANewArea;
end;

procedure TMainForm.PluginAreaRemoved(AObject: TArrayListedObject; Index: Integer);
var
  I       : Integer;
  AOldArea: TEGUIArea;
begin
  AOldArea:=TEGUIArea(AObject);
  FNewPluginTop:=AOldArea.FObj.Top;
  PluginSB.RemoveControl(AOldArea.FObj);
  for I:=Index to FPluginAreas.Count-1 do begin
    with TEGUIArea(FPluginAreas[I]) do begin
      FObj.Top:=FNewPluginTop;
      FNewPluginTop+=FObj.Height;
    end;
  end;
end;

procedure TMainForm.VisWinClosed(AObject: TArrayListedObject; Index: Integer);
begin
  WALB.Items.Delete(Index);
  WALBSelectionChange(nil,false);
  Net.SendBlockDataUpdate([Net.VisWinBlockID]);
end;

procedure TMainForm.NewWinMIClick(Sender: TObject);
var
  AVisWin: TVisWin;
begin
  AVisWin:=TLocalVisWin.Create('Unnamed',0,0,Classes.Point(ResolutionXSE.Value,ResolutionYSE.Value),SpectrumData,VisKeyboards,VisPresets,PresetLoader);
  VisWins.AddItem(AVisWin);
  WALB.Items.Add('');

  Net.SendBlockDataUpdate([Net.VisWinBlockID]);
end;

procedure TMainForm.ResolutionSEChange(Sender: TObject);
var
  I: Integer;
begin
  for I:=0 to VisWins.Count-1
    do VisWins[I].Resolution:=MuviPoint(ResolutionXSE.Value,ResolutionYSE.Value);
end;

procedure TMainForm.ShowInfoPMIClick(Sender: TObject);
begin
  TShortInfoMsg.Create('Test', 0);
end;

procedure TMainForm.StatisticsPMIClick(Sender: TObject);
begin
  ShowMessage(IntToStr(ParamTypeImpl.ParamCount));
end;

procedure TMainForm.SubbandEditMIClick(Sender: TObject);
var
  TempSubbands: TCSubbandSizes;
begin
  TempSubbands:=SpectrumData.BandFrequencys;
  if SubbandEditForm.EditSubbands(TempSubbands,SpectrumData)
    then SpectrumData.BandFrequencys:=TempSubbands;
end;

procedure TMainForm.SyncMIClick(Sender: TObject);
begin
  //Syncronisieren...
end;

procedure TMainForm.SyncTapMIClick(Sender: TObject);
begin
  SpectrumData.BPMTap;
end;

procedure TMainForm.MasterVolumeTBChange(Sender: TObject);
begin
  SpectrumData.MasterVolume:=Power(2.0,(-MasterVolumeTB.Position)/10.0);
end;

procedure TMainForm.ToolboxTBClick(Sender: TObject);
begin
  ToolBoxForm.Show;
end;

procedure TMainForm.UniqueStringPMIClick(Sender: TObject);
begin
  DragPlugForm.DragValue(CreateIntegerValueStorage(1337));
end;

procedure TMainForm.WABtnClick(Sender: TObject);
begin
  SelectPresetForm.Popup(WABtn);
end;

procedure TMainForm.WABtnChange(Sender: TObject);
begin
  WABtn.Caption:=VisPresets[SelectPresetForm.PresetIndex].Name;
  VisWins[WALB.ItemIndex].Preset:=SelectPresetForm.PresetIndex;//WACB.ItemIndex;
  Net.SendBlockDataUpdate([Net.PresetBlockID]);
end;

procedure TMainForm.WABtnKeyPress(Sender: TObject; var Key: char);
begin
  SelectPresetForm.FilterEdit.Filter:=Key;
  SelectPresetForm.Popup(WABtn);
end;

procedure TMainForm.SetWACB;
{var
  I,SelIndex: Integer;}
begin
  {WACB.OnChange:=nil;
  SelIndex:=WACB.ItemIndex;
  WACB.Items.Clear;
  for I:=0 to VisPresets.Count-1
    do WACB.Items.Add(VisPresets[I].Name);
  WACB.ItemIndex:=SelIndex;
  WACB.OnChange:=@WACBChange;}
  SelectPresetForm.UpdatePresets;
  Net.SendBlockDataUpdate([Net.PresetBlockID]);
end;

procedure TMainForm.SetWAKeyCB;
var
  I,SelIndex: Integer;
begin
  WAKeyCB.OnChange:=nil;
  SelIndex:=WAKeyCB.ItemIndex;
  WAKeyCB.Items.Clear;
  for I:=0 to Length(VisKeyboards)-1
    do WAKeyCB.Items.Add(VisKeyboards[I].Name);
  WAKeyCB.ItemIndex:=SelIndex;
  WAKeyCB.OnChange:=@WAKeyCBChange;
end;

procedure TMainForm.WALBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  TempRect: TRect;
  TextY   : Integer;
  TempWin : TVisWin;

  procedure NextSection(ASection: integer);
  begin
    TempRect.Left:=TempRect.Right;
    TempRect.Right+=WAHC.Sections[ASection].Width;
  end;

  procedure WriteText(const S: string);
  begin
    WALB.Canvas.TextRect(TempRect,TempRect.Left+5,TextY,S);
  end;

begin
  with WALB.Canvas do begin
    TempWin:=VisWins[Index];
    if odSelected in State then begin
      Brush.Color:=Color32ToColor(IPColor(Config[ParamID(HIGHLIGHTCOLORNAME, vColor)]).Value);
      Pen.Color:=Brush.Color;
      Font.Color:=Color32ToColor(IPColor(Config[ParamID(FONTINHIGHLIGHTCOLORNAME, vColor)]).Value);
    end else begin
      Brush.Color:=Color32ToColor(IPColor(Config[ParamID(BGCOLORNAME, vColor)]).Value);
      Pen.Color:=Brush.Color;
      Font.Color:=Color32ToColor(IPColor(Config[ParamID(FONTCOLORNAME, vColor)]).Value);
    end;
    TextY:=ARect.Top+((ARect.Bottom-ARect.Top-Canvas.TextHeight('Wg')) div 2);
    FillRect(ARect);
    TempRect:=ARect;
    TempRect.Right:=TempRect.Left+WALB_ComboLeft;

    if not (odSelected in State) then begin
      {if (TempWin is TLocalVisWin)
        then if TLocalVisWin(TempWin).Enabled
          then Draw(TempRect.Left,TempRect.Top+WALB_PicTop,FTrueIcon)
          else Draw(TempRect.Left,TempRect.Top+WALB_PicTop,FFalseIcon)
        else Draw(TempRect.Left,TempRect.Top+WALB_PicTop,FFalseIcon);}
      if TempWin.Enabled
        then Draw(TempRect.Left,TempRect.Top+WALB_PicTop,FTrueIcon)
        else Draw(TempRect.Left,TempRect.Top+WALB_PicTop,FFalseIcon);
      NextSection(3);
      WriteText(TempWin.Name);
      NextSection(4);
      WriteText(VisPresets[TempWin.Preset].Name);
      NextSection(5);
      WriteText(VisKeyboards[TempWin.Keyboard].Name);
      NextSection(6);
      WriteText(PresetBufferModeToStr(TLocalVisWin(TempWin).Compositions.Mode));
      NextSection(7);
      WriteText(TempWin.ClassName);
      NextSection(8);
      if (TempWin is TGLVisWin)
        then WriteText(FloatToStrF(TGLVisWin(TempWin).Thread.FPS,ffFixed,7,2))
        else if (TempWin is TLocalVisWin)
          then WriteText(FloatToStrF(GetGlobalVisFPS,ffFixed,7,2))
          else WriteText('---');
    end else begin
      NextSection(3);
      WriteText(TempWin.Name);
      NextSection(4);
      NextSection(5);
      NextSection(6);
      NextSection(7);
      WriteText(TempWin.ClassName);
      NextSection(8);
      if not ((TempWin is TGlVisWin) or (TempWin is TLocalVisWin))
        then WriteText('---');
    end;
  end;
end;

procedure TMainForm.WALBSelectionChange(Sender: TObject; User: boolean);
var
  ATop: Integer;
begin
  if FSelWin<>nil then FSelWin.OnChangePreset:=nil;
  WAEnabledCB.OnChange:=nil;
  WAFPSEdit.OnChange:=nil;
  if WALB.ItemIndex>=0 then begin
    //...
    FSelWin:=VisWins[WALB.ItemIndex];
    ATop:=(WALB.ItemIndex-WALB.TopIndex)*WALB.ItemHeight;
    //WAEnabledCB
    (***, weil nicht jedes TVisWin ein TLocalVisWin sein muss*)
    WAEnabledCB.Enabled:=true;//(FSelWin.ClassType=TLocalVisWin);
    WAEnabledCB.Top:=ATop+WALB_CBTop;
    if WAEnabledCB.Enabled
      then WAEnabledCB.Checked:=FSelWin.Enabled
      else WAEnabledCB.Checked:=True;
    WAEnabledCB.Visible:=true;
    //WACB
    {WACB.Left:=WALB_ComboLeft+WAHC.Sections[3].Width+WALB_ComboOffset;
    WACB.Width:=WAHC.Sections[4].Width-WALB_ComboSizeRed;
    WACB.Top:=ATop+WALB_ComboTop;
    WACB.ItemIndex:=FSelWin.Preset;
    WACB.Visible:=true;}
    //WACB
    WABtn.Left:=WALB_ComboLeft+WAHC.Sections[3].Width+WALB_ComboOffset;
    WABtn.Width:=WAHC.Sections[4].Width-WALB_ComboSizeRed;
    WABtn.Top:=ATop+WALB_ComboTop;
    //WABtn.ItemIndex:=FSelWin.Preset;
    WABtn.Caption:=VisPresets[FSelWin.Preset].Name;
    WABtn.Visible:=true;
    //WAKeyCB
    WAKeyCB.Left:=WABtn.Left+WAHC.Sections[4].Width+WALB_ComboOffset;
    WAKeyCB.Width:=WAHC.Sections[5].Width-WALB_ComboSizeRed;
    WAKeyCB.Top:=ATop+WALB_ComboTop;
    WAKeyCB.ItemIndex:=FSelWin.Keyboard;
    WAKeyCB.Visible:=true;
    //WABufferCB
    WABufferCB.Left:=WAKeyCB.Left+WAHC.Sections[5].Width+WALB_ComboOffset;
    WABufferCB.Width:=WAHC.Sections[6].Width-WALB_ComboSizeRed;
    WABufferCB.Top:=ATop+WALB_ComboTop;
    WABufferCB.ItemIndex:=Ord(TLocalVisWin(FSelWin).Compositions.Mode);
    WABufferCB.Visible:=true;
    //BufferBtn
    BufferBtn.Enabled:=WAEnabledCB.Enabled;
    BufferBtn.Top:=ATop;
    BufferBtn.Visible:=true;
    //BufferSelectBtn
    BufferSelectBtn.Enabled:=WAEnabledCB.Enabled;
    BufferSelectBtn.Top:=ATop;
    BufferSelectBtn.Visible:=true;
    //WAFPSEdit
    if FSelWin is TGLVisWin then begin
      WAFPSEdit.OnChange:=@WAGLVisWinFPSEditChange;
      WAFPSEdit.Value:=TGLVisWin(FSelWin).Thread.FPS;
      WAFPSEdit.Visible:=true;
    end else if FSelWin is TLocalVisWin then begin
      WAFPSEdit.OnChange:=@WALocalVisWinFPSEditChange;
      WAFPSEdit.Value:=GetGlobalVisFPS;
      WAFPSEdit.Visible:=true;
    end else WAFPSEdit.Visible:=false;
    if WAFPSEdit.Visible then begin
      WAFPSEdit.Left:=WABufferCB.Left+WAHC.Sections[6].Width+WAHC.Sections[7].Width+WALB_ComboOffset;
      WAFPSEdit.Width:=WAHC.Sections[8].Width-WALB_ComboSizeRed;
      WAFPSEdit.Top:=ATop+WALB_ComboTop;
    end;

    if FSelWin is TGLVisWin then begin
      LineWidthEdit.OnChange:=nil;
      LineWidthEdit.Value:=TGLVisWin(FSelWin).LineWidth;
      LineWidthEdit.OnChange:=@LineWidthEditChange;
    end;
    //...
    FSelWin.OnChangePreset:=@SelWinChangePreset;
  end else begin
    WABtn.Visible:=false;
    WAKeyCB.Visible:=false;
    WABufferCB.Visible:=false;
    WAEnabledCB.Visible:=false;
    WAFPSEdit.Visible:=false;
    BufferBtn.Visible:=false;
    BufferSelectBtn.Visible:=false;
    FSelWin:=nil;
  end;
  WAEnabledCB.OnChange:=@WAEnabledCBChange;
  UpdateParams;
end;

procedure TMainForm.WAHCSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  WALB.Repaint;
end;

procedure TMainForm.WALocalVisWinFPSEditChange(Sender: TObject);
begin
  SetGlobalVisFPS(TFloatSpinEdit(Sender).Value);
end;

procedure TMainForm.WAGLVisWinFPSEditChange(Sender: TObject);
begin
  TGLVisWin(VisWins[WALB.ItemIndex]).Thread.FPS:=TFloatSpinEdit(Sender).Value;
end;

procedure TMainForm.WAKeyCBChange(Sender: TObject);
begin
  VisWins[WALB.ItemIndex].Keyboard := WAKeyCB.ItemIndex;
end;

procedure TMainForm.WABufferCBChange(Sender: TObject);
begin
  VisWins[WALB.ItemIndex].Compositions.Mode:=TPresetBufferMode(WABufferCB.ItemIndex);
end;

procedure TMainForm.WAEnabledCBChange(Sender: TObject);
begin
  VisWins[WALB.ItemIndex].Enabled:=WAEnabledCB.Checked;
end;

procedure TMainForm.ShowBalloon(const AHint: string; const ACategory: TMuviBalloonCategory; AFlags: TBalloonFlags = bfNone; const ATitle: string = 'Muvi'; const ATimeout: Integer = 3000);
begin
  if ACategory in FShownCategorys then with TrayIcon do begin
    BalloonTitle:=ATitle;
    BalloonHint:=AHint;
    BalloonFlags:=AFlags;
    BalloonTimeout:=ATimeout;
    ShowBalloonHint;
  end;
end;

procedure TMainForm.ShowNetBalloon(const ATitle, AHint: string;
  AFlags: TBalloonFlags; const ATimeout: Integer);
begin
  ShowBalloon(AHint,mbcNetwork,AFlags,ATitle,ATimeout);
end;

procedure TMainForm.DrawBeat;
const
  SaveBufferOverrunStr = 'Schutzpufferüberlauf!' + #$D#$A +
    'Ein Schutzpufferüberlauf tritt normalerweise bei einer Fehlkalkulation der benötigten Rechenzeit auf. Er beeinträchtigt nicht die Stabilität von Muvi, Verursacht aber asynchrone Beats.' + #$D#$A + 'Um solche Schutzpufferüberläufe zu vermeiden, schließen sie ein paar nicht benötigte Programme oder stellen sie die Schtuzpuffergröße unter Analyseeinstellungen -> Schutzpuffergröße mindestens auf ';
begin
  BeatIB.Selected:=SpectrumData.Beat.Pos;
  if SpectrumData.Beat.Free
    then BPMLbl.Font.Color:=Color32ToColor(IPColor(Config[ParamID(FONTCOLORNAME, vColor)]).Value)
    else BPMLbl.Font.Color:=Color32ToColor(IPColor(Config[ParamID(FONTHIGHLIGHTCOLORNAME, vColor)]).Value);
  BeatRelationAMB.Alpha:=$FF-Round(sqr(Abs(SpectrumData.FSyncRelation))*4*$FF);

  case SpectrumData.BPMFound of
    bpmFalse   : BPMLbl.Caption:='No Beat';
    bpmTrue    : BPMLbl.Caption:=FloatToStrF(SpectrumData.BPM,ffFixed,7,2)+' BPM';
    bpmScanning: BPMLbl.Caption:=FloatToStrF(SpectrumData.BPM,ffFixed,7,2)+' BPM';
  end;
  with SpectrumData.Beat do begin
    if SyncTogg<>LastSyncTogg then begin
      LastSyncTogg:=SyncTogg;
      SyncImageList.GetBitmap(1,SyncBtn.Glyph);
      SyncShowTime:=0;
    end;
    if SyncShowTime<5
      then Inc(SyncShowTime)
      else SyncImageList.GetBitmap(0, SyncBtn.Glyph);
  end;
  if SpectrumData.BPMThread<>nil
    then BPMAnalysisFramesLbl.Caption:=IntToStr(SpectrumData.BPMThread.FramesNeeded)+' Frame(s)'+' ['+FloatToStrF(SpectrumData.BPMThread.ActiveFPS,ffFixed,7,2)+' FPS]'+', Sync: '+IntToStr(SpectrumData.SyncThread.FramesNeeded)+' ['+FloatToStrF(SpectrumData.SyncThread.ActiveFPS,ffFixed,7,2)+' FPS]';
  if SpectrumData.SyncThread<>nil
    then if SpectrumData.SyncThread.FramesNeeded>SpectrumData.SaveSyncCount
      then ShowBalloon(SaveBufferOverrunStr+IntToStr(SpectrumData.SyncThread.FramesNeeded),mbcBeat,bfError);
  if LastBPMFound then begin
    if SpectrumData.BPMFound=bpmFalse then begin
      Self.ShowBalloon('Rythm Lost',mbcBeat,bfInfo);
      LastBPMFound:=false;
    end;
  end else begin
    if SpectrumData.BPMFound=bpmTrue then begin
      Self.ShowBalloon('Rythm Found: '+FloatToStrF(SpectrumData.BPM,ffFixed,7,2)+' BPM',mbcBeat,bfInfo);
      LastBPMFound:=true;
    end;
  end;
end;

procedure TMainForm.PresetLoaderStarted(Sender: TObject);
begin
  VisProgressPanel.Visible:=true;
end;

procedure TMainForm.PresetLoaderFinished(Sender: TObject);
begin
  VisProgressPanel.Visible:=false;
end;

procedure TMainForm.AutoSyncCBChange(Sender: TObject);
begin
  SpectrumData.AutoSync:=AutoSyncCB.Checked;
end;

procedure TMainForm.EnableSyncMIClick(Sender: TObject);
begin
  SpectrumData.SyncEnabled:=EnableSyncMI.Checked;
end;

procedure TMainForm.EnableBPMMIClick(Sender: TObject);
begin
  SpectrumData.BPMEnabled:=EnableBPMMI.Checked;
end;

procedure TMainForm.SyncModeCBChange(Sender: TObject);
begin
  SpectrumData.SyncMode:=TBeatSyncMode(SyncModeCB.ItemIndex);
end;

procedure TMainForm.DrawFrameInfo;
var
  ARectWidth         : Integer;
  ATaskFac           : Real;
  AColor             : TColor;
  ACaption           : string;
  AFontColor         : TColor;
  AFontHighlightColor: TColor;
begin
  AFontColor:=Color32ToColor(IPColor(Config[ParamID(FONTCOLORNAME, vColor)]).Value);
  AFontHighlightColor:=Color32ToColor(IPColor(Config[ParamID(FONTHIGHLIGHTCOLORNAME, vColor)]).Value);

  with GraphicBGActivityImg.Picture.Bitmap do
  begin
    if GetGlobalVisTaskCount<=MaxGraphicActivity
      then ATaskFac:=GetGlobalVisTaskCount/MaxGraphicActivity
      else ATaskFac:=1.0;
    AColor:=Round($1FF*ATaskFac);
    if AColor>$FF
      then AColor:=$00FFFF-(AColor-$100)*$100
      else AColor:=$00FF00+AColor;
    Canvas.Brush.Color:=clBtnFace;
    Canvas.Pen.Color:= clBtnFace;
    Canvas.Rectangle(0,0,Width,Height);
    Canvas.Brush.Color:=AColor;
    Canvas.Pen.Color:=AColor;
    ARectWidth:=Round(Width*ATaskFac);
    Canvas.Rectangle(0,0,ARectWidth,Height);
  end;
  GraphicBGActivityValLbl.Caption:=IntToStr(GetGlobalVisTaskCount);

  if FSelWin=nil then begin
    GraphicFPSLbl.Caption:='Framerate: <no window slected>';
    GraphicFPSLbl.Font.Color:=AFontColor;
  end else if FSelWin.Enabled then begin
    if FSelWin is TGLVisWin then with TGLVisWin(FSelWin).Thread do begin
      GraphicFPSLbl.Caption:='Framerate: '+FloatToStrF(ActiveFPS,ffFixed,7,2) + ' FPS';
      if Slow
        then GraphicFPSLbl.Font.Color:=AFontHighlightColor
        else GraphicFPSLbl.Font.Color:=AFontColor;
    end else if FSelWin is TLocalVisWin then begin
      GraphicFPSLbl.Caption:='Framerate: '+FloatToStrF(GetGlobalVisActiveFPS,ffFixed,7,2) + ' FPS';
      if GlobalVisIsSlow
        then GraphicFPSLbl.Font.Color:=AFontHighlightColor
        else GraphicFPSLbl.Font.Color:=AFontColor;
    end else begin
      GraphicFPSLbl.Caption:='Framerate: <unknown>';
      GraphicFPSLbl.Font.Color:=AFontColor;
    end;
  end else begin
    GraphicFPSLbl.Caption:='Framerate: <Visualisation disabled>';
    GraphicFPSLbl.Font.Color:=AFontColor;
  end;

  if VisProgressPanel.Visible then
  begin
    VisProgressPB.Max := PresetLoader.MaxProgress;
    VisProgressPB.Position := PresetLoader.Progress;
    if PresetLoader.MaxProgress > 0 then
      ACaption := 'Buffering ('+FloatToStrF(
        (PresetLoader.Progress/PresetLoader.MaxProgress)*100.0,ffFixed,7,2) + '%)'
    else
      ACaption := 'Buffering';
    if PresetLoader.ActivePreset <> nil
      then VisProgressLbl.Caption:=ACaption+': '+PresetLoader.ActivePreset.Name
      else VisProgressLbl.Caption:=ACaption;
  end;
end;

procedure TMainForm.ShowDiagBtnClick(Sender: TObject);
begin
  CallDiagProc(@DiagDrawForm.ShowWithDiags);
end;

procedure TMainForm.AddDiagBtnClick(Sender: TObject);
begin
  CallDiagProc(@DiagDrawForm.AddDiags);
end;

procedure TMainForm.CallDiagProc(DiagProc: TDiagShowProc);

  procedure TestTakeFirstHistoryIndex;
  var
    ABam: TBam;
  begin
    with SpectrumData.TakeFirstHistory do begin
      ABam:=ToItem([2, 7]);
      ShowMessage('original: (2,7) ergebnis:('+IntToStr(IndexOf(ABam,0))+','+IntToStr(IndexOf(ABam,1))+')');
    end;
  end;

begin
  case DiagSelectCB.ItemIndex of
    0: DiagProc([DiagDrawData(SpectrumData.FSyncHistory,SpectrumData.FSyncHistoryPos,1.0,-1.0,clBlack,$CCCCCC,true)]);
    1: DiagProc([DiagDrawData(SpectrumData.FRSyncHistory,SpectrumData.FSyncHistoryPos,1.0,-1.0,clRed,$CCCCCC,true)]);
    2: DiagProc([DiagDrawData(SpectrumData.FSyncHistory,SpectrumData.FSyncHistoryPos,1.0,-1.0,clBlack,$CCCCCC,True),DiagDrawData(SpectrumData.FRSyncHistory,SpectrumData.FSyncHistoryPos,1.0,-1.0,clRed,$CCCCCC,true)]);
    3: DiagProc([DiagDrawData(SpectrumData.FMPFHistory,SpectrumData.FMPFHistoryPos,10000.0,-10000.0,clBlue,$CCCCCC,true,false)]);
    4: DiagProc([DiagDrawData(SpectrumData.FMPFHistoryDif,SpectrumData.FMPFHistoryPos, 10000.0,-10000.0,clGreen,$CCCCCC,true,false)]);
    5: DiagProc([DiagDrawData(SpectrumData.FMPTHistory,SpectrumData.FMPFHistoryPos,100.0,-100.0,clMaroon,$CCCCCC,true,false)]);
    6: DiagProc([DiagDrawData(SpectrumData.FMPTHistoryDif,SpectrumData.FMPFHistoryPos,100.0,-100.0,clNavy,$CCCCCC,true,false)]);
    7: DiagProc([DiagDrawDataSimple(SpectrumData.FFFTAverage,0,0.002,-0.002,clOlive,$CCCCCC,true,false)]);
    8: DiagProc([DiagDrawData(SpectrumData.FMPRHistory,SpectrumData.FMPFHistoryPos,0.002,-0.002,clPurple,$CCCCCC,true,false)]);
    9: DiagProc([DiagDrawData(SpectrumData.FMPRFHistory,SpectrumData.FMPFHistoryPos,10000.0,-10000.0,clPurple,$CCCCCC,true,false)]);
    10: DiagProc([DiagDrawData(SpectrumData.FMPRFHistoryDif,SpectrumData.FMPFHistoryPos,10000.0,-10000.0,clPurple,$CCCCCC,true,false)]);
    11: DiagProc([DiagDrawData(SpectrumData.FMPRTHistory,SpectrumData.FMPFHistoryPos,100.0,-100.0,clPurple,$CCCCCC,true,false)]);
    12: DiagProc([DiagDrawData(SpectrumData.FMPRTHistoryDif,SpectrumData.FMPFHistoryPos,100.0,-100.0,clPurple,$CCCCCC,true,false)]);
    13: DiagProc([DiagDrawData(SpectrumData.FMPRFHistoryW,SpectrumData.FMPFHistoryPos,600.0,-600.0,clPurple,$CCCCCC,true,false)]);
    14: DiagProc([DiagDrawData(SpectrumData.FMPRTHistoryW,SpectrumData.FMPFHistoryPos,100.0,-100.0,clPurple,$CCCCCC,true,false)]);
    15: DiagProc([DiagDrawData(SpectrumData.FMPRRHistory,SpectrumData.FMPFHistoryPos,10.0,0.0,clTeal,$CCCCCC,true,false)]);
    16: DiagProc([DiagDrawData(SpectrumData.FMPRRFHistory,SpectrumData.FMPFHistoryPos,10000.0,-10000.0,clTeal,$CCCCCC,true,false)]);
    17: DiagProc([DiagDrawData(SpectrumData.FMPRRFHistoryDif,SpectrumData.FMPFHistoryPos,10000.0,-10000.0,clTeal,$CCCCCC,true,false)]);
    18: DiagProc([DiagDrawData(SpectrumData.FMPRRTHistory,SpectrumData.FMPFHistoryPos,100.0,-100.0,clTeal,$CCCCCC,true,false)]);
    19: DiagProc([DiagDrawData(SpectrumData.FMPRRTHistoryDif,SpectrumData.FMPFHistoryPos,100.0,-100.0,clTeal,$CCCCCC,true,false)]);
    20: DiagProc([DiagDrawData(SpectrumData.FMPRRFHistoryW,SpectrumData.FMPFHistoryPos,100000.0,-100000.0,clTeal,$CCCCCC,true,false)]);
    21: DiagProc([DiagDrawData(SpectrumData.FMPRRTHistoryW,SpectrumData.FMPFHistoryPos,10000.0,-10000.0,clTeal,$CCCCCC,true,false)]);
    22: PicDiagForm.ShowWithDiag(SpectrumData.TakeFirstHistory,EMPTYBAM,0.0001,0.0,[$FF000000,$FF0000FF,$FFFF0000,$FFFFFF00,$FFFFFFFF],[doMax],@ChangeTakeFirstDiagBam,$FFFFFFFF,$FF000000,$FF777777,@GetTakeFirstHintText);
    23: TestTakeFirstHistoryIndex;
    24: ShowMessage('Dieser test ist veraltet');
    25: PlugPicTestForm.Show;
  end;
end;

procedure TMainForm.InitSettings; inline;
begin
  //FSettings.AddDiagBtn()
  FSettings.Complete;
end;

function TMainForm.GetTakeFirstHintText(X, Y: dsi): string;
begin
  Result:=FloatToStrF(SpectrumData.TakeFirstIndexToBPM(Y),ffFixed,7,2)+' BPM';
end;

procedure TMainForm.ChangeTakeFirstDiagBam(var ABam: TBam);
begin
  ABam:=SpectrumData.TakeFirstHistoryPos;
  SpectrumData.TakeFirstHistory.RotateNext(ABam,0);
end;

procedure TMainForm.DrawTests;
var
  AMPF: Real;
begin
  AMPF:=SpectrumData.LevelFrequency(SpectrumData.MPF);
  MPFLbl.Caption:=FloatToStrF(AMPF,ffFixed,7,2)+' Hz ('+ToneToStr(FrequencyToTone(AMPF))+')';
  VPBLbl.Caption:='VPB: '+FloatToStrF(SpectrumData.VPB/SpectrumData.Beat.BeatLength,ffFixed,7,4);
end;

procedure TMainForm.Vised;
begin
  SpectrumData.Recognize;
end;

procedure TMainForm.SelWinChangePreset(Sender: TVisWin);
begin
  UpdateParams;
end;

procedure TMainForm.GetParamVal(Param: Pointer; AType: TVisParamType; out Value);
begin
  GetVCOutput(PVCOutput(Param)^,Value);
end;

procedure TMainForm.SetParamVal(Param: Pointer; AType: TVisParamType; const Value);
begin
  SetVCOutput(PVCOutput(Param)^,Value);
end;

procedure TMainForm.FParamPainterPaint(Sender: TObject; Param: TPPParam);
begin
  ParamPB.Repaint;
end;

function TMainForm.FParamPainterRequestBG(Sender: TObject; Param: TPPParam): boolean;
begin
  with ParamPB.Buffer.Canvas do begin
    Pen.Color:=ParamPB.Color;
    Brush.Color:=Pen.Color;
    Rectangle(Param.DrawPos);
  end;
  Result:=true;
end;

procedure TMainForm.UpdateParams;
var
  I: Integer;
begin
  FParamPainter.OnPaint:=nil;
  FParamPainter.Clear;
  if FSelWin=nil then begin
    ParamSB.Max:=0;
    FParamPainter.OnPaint:=@FParamPainterPaint;
    exit;
  end;
  with TLocalVisWin(FSelWin) do begin
    for I:=0 to ParamCount-1 do FParamPainter.Add(ParamPtr(I),ParamNames[I],ParamTypes[I],ParamXOffset,(I*CompleteParamHeight)+ParamYOffset);
    ParamSB.Max:=ParamCount*CompleteParamHeight+ParamYOffset2;
  end;
  FParamPainter.OnPaint:=@FParamPainterPaint;
  DrawParams;
end;

procedure TMainForm.DrawParams;
var
  I,ATH: Integer;
begin
  ParamPB.ClearBuffer;
  if FSelWin=nil then exit;
  with TLocalVisWin(FSelWin) do with ParamPB.Buffer do begin
    ATH:=(ParamHeight-Canvas.TextHeight('Wg')) div 2;
    for I:=0 to ParamCount-1 do Canvas.TextOut(ParamTextOffset,I*CompleteParamHeight+ATH,ParamNames[I]+':');
    FParamPainter.Paint(Rect(0, ParamSB.Position,ParamPB.Width,ParamSB.Position+ParamPB.Height));
  end;
  ParamPB.Repaint;
end;

{%ENDREGION}
{%REGION Notifications}

procedure SetColorScheme(AContext: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TMainForm(AContext) do begin
    Color:=Color32ToColor(FBGColor.Value);
    AlphaBlendValue:=AlphaComponent(FBGColor.Value);
    AlphaBlend:=AlphaBlendValue<255;
    BeatIB.Color:=Color32ToColor(FBGColor.Value);
    ParamPB.Color:=Color32ToColor(FBGColor.Value);
    DrawParams;
    NetworkDividerPanel.Color:=Color32ToColor(FHighlightColor.Value);
    Font.Color:=Color32ToColor(FFontColor.Value);
  end;
end;

{%ENDREGION}

end.

