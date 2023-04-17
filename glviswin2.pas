unit GLVisWin2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SpectrumData, MCanvasUnit, GLCanvasUnit, StdTags, ValueStorages,
  ExtCtrls, AdvFunc, LCLIntf, Menus, AdvThreads, VisEventImpl, SyncObjs,
  MInterfacedObject, OpenGLContext, VisType2, PresetType, StdParamTypes,
  ParamTypes, MStrings, CanvasType, Math, SimpleVis, VisAddInput, AdvParamType,
  VisualisationUtils;

type
  TGLVisWin2         = class;

  TSizeParams        = record
    Left       : IPInteger;
    Top        : IPInteger;
    Width      : IPInteger;
    Height     : IPInteger;
    Fullscreen : IPBoolean;
    Monitor    : IPInteger;
    IsChanging : Boolean;
  end;

  { TGLVisForm2 }

  TGLVisForm2        = class(TForm)
    CloseBtn: TButton;
    HeaderEdit: TEdit;
    LeftBottomPanel: TPanel;
    MaximizeBtn: TButton;
    HeaderImage: TImage;
    HeaderLbl: TLabel;
    LeftPanel: TPanel;
    BottomPanel: TPanel;
    GLControl: TOpenGLControl;
    SetSizePMI: TMenuItem;
    MainPopupMenu: TPopupMenu;
    RightBottomPanel: TPanel;
    RenderTimer: TTimer;
    TopPanel: TPanel;
    RightPanel: TPanel;
    Timer: TTimer;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLControlMakeCurrent(Sender: TObject; var Allow: boolean);
    procedure GLControlResize(Sender: TObject);
    procedure HeaderEditExit(Sender: TObject);
    procedure HeaderEditKeyPress(Sender: TObject; var Key: char);
    procedure HeaderImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HeaderLblClick(Sender: TObject);
    procedure HeaderLblDblClick(Sender: TObject);
    procedure HeaderLblMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderLblMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MaximizeBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
    procedure SetSizePMIClick(Sender: TObject);
    procedure VisMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VisMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure VisMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure TimerTimer(Sender: TObject);
  private
    FExitPos          : Integer;
    FCanClose         : Boolean;
    FVis              : TGLVisWin2;
    FMouseIsDown      : Boolean;
    FSizeParams       : TSizeParams;
    procedure SetFullScreen(AIsFullscreen: Boolean);
    procedure WndInit(AVis: TGLVisWin2; ASizeParams: TSizeParams);
    procedure FreeSizeParams;
    procedure ResizeForm(AResizeMode: Byte);
  public
    procedure ShowControls;
    procedure HideControls;
  end;

  TGLVisWin2         = class (TVisualisationEvents)
  private
    FWnd                  : TGLVisForm2;
    FCanvas               : TGLCanvas;
    FResolution           : TPoint;
    FName                 : IPString;
    FPreset               : IPPreset;
    FVisible              : IPBoolean;
    FFrameRate            : IPFloat;
    FPresetEnvironment    : IPVisualisationEnvironment;
    FPresetThread         : IPThread;
    FPresetThreadPrototype: IPThreadPrototype;
    FVisualisation        : IPVisualisation;
    FInputToCall          : IPCall;
    FVisualisationLock    : TCriticalSection;
    procedure GLControlPaintNothing(Sender: TObject);
    procedure VisFormDoFormClose;
    procedure DoSetName(AName: string);
    procedure DoSetPreset(APreset: TVPreset);
  protected
    function GetName: string;
    procedure SetName(AName: string);
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
    procedure Resume; cdecl; override;
    procedure Suspend; cdecl; override;
    function GetLineWidth: Real;
    procedure SetLineWidth(Value: Real);
    procedure FCanvasFrameStarted;

    property PresetEnvironment: IPVisualisationEnvironment read FPresetEnvironment;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    procedure BringToFront;
  published
    property LineWidth: Real read GetLineWidth write SetLineWidth;
    property Name: string read GetName write SetName;
  end;

const
  VIDGLVISWIN  : TGUID  = '{523D3BA0-D2FC-46CD-A478-CF9DC9CF1649}';
  ExitStr      : string = #17#21#9#20;
  GLVISWIN_CAPTIONPARAMNAME  = 'Name';
  GLVISWIN_PRESETPARAMNAME   = 'Preset';
  GLVISWIN_VISIBLEPARAMNAME  = 'Visible';
  GLVISWIN_FRAMERATEPARAMNAME= 'Frame Rate';

  GLVISWINLEFTNAME           = 'Left';
  GLVISWINTOPNAME            = 'Top';
  GLVISWINWIDTHNAME          = 'Width';
  GLVISWINHEIGHTNAME         = 'Height';
  GLVISWINFULLSCREENNAME     = 'Fullscreen';
  GLVISWINMONITORINDEXNAME   = 'Monitor';

procedure Register;

implementation

{$R *.lfm}

const
  HidePos = 10;
  vcMoveX  = 1;
  vcMoveY  = 2;
  vcResizeX= 4;
  vcResizeY= 8;

function GenerateVisWinName: string; forward;

procedure GLVisWinNameChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl; forward;
procedure GLVisWinPresetChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl; forward;
procedure GLVisWinVisibleChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl; forward;
procedure GLVisWinFrameRateChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl; forward;

{%REGION TGLVisWin2}

constructor TGLVisWin2.Create(APrototype: IPVisualisationPrototype);
var
  ASizeParams: TSizeParams;
begin
  inherited Create(APrototype, false);
  FVisualisationLock:=TCriticalSection.Create;

  Application.CreateForm(TGLVisForm2,FWnd);
  FCanvas:=TGLCanvas.Create(FWnd.GLControl);

  FPresetThreadPrototype:=PresetUtil.NewThreadPrototype;
  FPresetThread:=FPresetThreadPrototype.Thread;
  FPresetEnvironment:=Environment.CreateSubEnvironment(IPGL2DCanvas(FCanvas), FPresetThread);

  DoInitialize;

  FName:=StringInputs[GLVISWIN_CAPTIONPARAMNAME];
  FName.AddListener(@GLVisWinNameChanged, Self, VisualisationUtil.MainThread);
  FVisible:=BooleanInputs[GLVISWIN_VISIBLEPARAMNAME];
  FVisible.AddListener(@GLVisWinVisibleChanged, Self, VisualisationUtil.MainThread);
  FFrameRate:=FloatInputs[GLVISWIN_FRAMERATEPARAMNAME];
  FFrameRate.AddListener(@GLVisWinFrameRateChanged, Self, VisualisationUtil.MainThread);
  FPreset:=PresetInputs[GLVISWIN_PRESETPARAMNAME];
  DoSetPreset(FPreset.ExecutedValue);
  FPreset.AddListener(@GLVisWinPresetChanged, Self, VisualisationUtil.MainThread);
  with ASizeParams do begin
    Left:=IntegerInputs[GLVISWINLEFTNAME];
    Top:=IntegerInputs[GLVISWINTOPNAME];
    Width:=IntegerInputs[GLVISWINWIDTHNAME];
    Height:=IntegerInputs[GLVISWINHEIGHTNAME];
    Fullscreen:=BooleanInputs[GLVISWINFULLSCREENNAME];
    Monitor:=IntegerInputs[GLVISWINMONITORINDEXNAME];
  end;

  FCanvas.Reset(@FCanvasFrameStarted);
  with FWnd do begin
    WndInit(Self, ASizeParams);
    SetName(GenerateVisWinName);
    //check isNanbefore isZero because Nan is not comparable...
    if isNan(FFrameRate.Value) or isZero(FFrameRate.Value)
      then RenderTimer.Interval:=MaxInt
      else RenderTimer.Interval:=Round(1000.0 / FFrameRate.Value);
  end;
end;

destructor TGLVisWin2.Destroy;
begin
  //FVisThread wird in VisFormDoFormClose() freigegeben
  with FWnd do begin
    FCanClose:=true;
    Close;
    FreeSizeParams;
  end;
  FName.RemoveListener(@GLVisWinNameChanged, Self);
  FName:=nil;
  FPreset.RemoveListener(@GLVisWinPresetChanged, Self);
  FPreset:=nil;
  FFrameRate.RemoveListener(@GLVisWinFrameRateChanged, Self);
  FFrameRate:=nil;
  FVisible.RemoveListener(@GLVisWinVisibleChanged, Self);
  FVisible:=nil;
  FVisualisation:=nil;
  FPresetThreadPrototype:=nil;
  FPresetThread:=nil;
  FPresetEnvironment:=nil;
  FVisualisationLock.Destroy;
end;

procedure TGLVisWin2.FCanvasFrameStarted;
{var
  AInputToCall: IPCall;}
begin
  {FVisualisationLock.Enter;
  AInputToCall:=FInputToCall;
  FVisualisationLock.Leave;
  AInputToCall.&Set;
  FCanvas.FrameEnd;}
  FPresetThreadPrototype.Started;
  FInputToCall.&Set;
  FPresetThreadPrototype.Execute;
  FPresetThreadPrototype.Stopped;
  FCanvas.FrameEnd;
end;

procedure TGLVisWin2.BringToFront;
begin
  FWnd.BringToFront;
end;

procedure TGLVisWin2.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID = ParamID(GLVISWIN_PRESETPARAMNAME, vPreset)
    then IPPresetSettings(ASettings).Environment:=FPresetEnvironment;
end;

procedure TGLVisWin2.Resume; cdecl;
begin
  if FVisible.Get
    then FWnd.Show;
  FWnd.RenderTimer.Enabled:=true;
  //FVisThread.Resume;
end;

procedure TGLVisWin2.Suspend; cdecl;
begin
  //FVisThread.Suspend;
  FWnd.RenderTimer.Enabled:=false;
  if FVisible.Get
    then FWnd.Hide;
end;

function TGLVisWin2.GetLineWidth: Real;
begin
  Result:=FCanvas.LineWidth;
end;

procedure TGLVisWin2.SetLineWidth(Value: Real);
begin
  FCanvas.LineWidth:=Value;
end;

function TGLVisWin2.GetName: string;
begin
  Result:=FName.Value;
end;

procedure TGLVisWin2.SetName(AName: string);
begin
  FName.Value:=AName;
end;

procedure TGLVisWin2.DoSetName(AName: string);
begin
  with FWnd do begin
    Caption:=AName;
    HeaderLbl.Caption:=AName;
  end;
end;

procedure TGLVisWin2.DoSetPreset(APreset: TVPreset);
var
  AVisualisation: IPVisualisation;
begin
  AVisualisation:=PresetUtil[APreset];

  FVisualisationLock.Enter;
  FVisualisation:=AVisualisation;
  FInputToCall:=IPCall(AVisualisation.Inputs[ParamID(MAININPUTNAME, vCall)]);
  FVisualisationLock.Leave;

  AVisualisation:=nil;
end;

procedure TGLVisWin2.GLControlPaintNothing(Sender: TObject);
begin
  //FVisThread.FrameStarted;
end;

procedure TGLVisWin2.VisFormDoFormClose;
begin
  //FVisThread.Terminate;
  FWnd.GLControl.OnPaint:=@GLControlPaintNothing;
  //FVisThread.Destroy;
  FCanvas.Destroy;
  inherited Destroy;
end;

{%ENDREGION TGLVisWin2}
{%REGION TGLVisForm2 - Listeners}

procedure VisFormLeftChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AValue: TVInteger;
begin
  with TGLVisForm2(Context) do begin
    AValue:=IChangedInteger(SenderData).Value;
    if (not FSizeParams.IsChanging) and (Left<>AValue) then begin
      FSizeParams.IsChanging:=true;
      Left:=AValue;
      FSizeParams.IsChanging:=false;
    end;
  end;
end;

procedure VisFormTopChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AValue: TVInteger;
begin
  with TGLVisForm2(Context) do begin
    AValue:=IChangedInteger(SenderData).Value;
    if (not FSizeParams.IsChanging) and (Top<>AValue) then begin
      FSizeParams.IsChanging:=true;
      Top:=AValue;
      FSizeParams.IsChanging:=false;
    end;
  end;
end;

procedure VisFormWidthChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AValue: TVInteger;
begin
  with TGLVisForm2(Context) do begin
    AValue:=IChangedInteger(SenderData).Value;
    if (not FSizeParams.IsChanging) and (Width<>AValue) then begin
      FSizeParams.IsChanging:=true;
      Width:=AValue;
      FSizeParams.IsChanging:=false;
    end;
  end;
end;

procedure VisFormHeightChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AValue: TVInteger;
begin
  with TGLVisForm2(Context) do begin
    AValue:=IChangedInteger(SenderData).Value;
    if (not FSizeParams.IsChanging) and (Height<>AValue) then begin
      FSizeParams.IsChanging:=true;
      Height:=AValue;
      FSizeParams.IsChanging:=false;
    end;
  end;
end;

procedure VisFormFullscreenChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGLVisForm2(Context)
   do if not FSizeParams.IsChanging
     then SetFullScreen(IChangedBoolean(SenderData).Value)
end;

procedure VisFormMonitorChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AMonitorIndex: Integer;
  AMonitor     : TMonitor;
begin
  with TGLVisForm2(Context) do if not FSizeParams.IsChanging then begin
    AMonitorIndex:=IChangedInteger(SenderData).Value;
    if AMonitorIndex < 1
      then AMonitorIndex:=1;
    if AMonitorIndex > Screen.MonitorCount
      then AMonitorIndex:=Screen.MonitorCount;
    Dec(AMonitorIndex);

    AMonitor:=Screen.Monitors[AMonitorIndex];
    if Monitor<>AMonitor then begin
      FSizeParams.IsChanging:=true;
      SetBounds((Left - Monitor.Left) + AMonitor.Left, (Top - Monitor.Top) + AMonitor.Top, Width, Height);
      FSizeParams.IsChanging:=false;
    end;
  end;
end;

{%ENDREGION}
{%REGION TGLVisForm2}

procedure TGLVisForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=FCanClose;
end;

procedure TGLVisForm2.CloseBtnClick(Sender: TObject);
begin
  //FCanClose:=true;
  //FVis.FVisible><false;
  FVis.FVisible.&Set(false);
end;

procedure TGLVisForm2.RenderTimerTimer(Sender: TObject);
begin
  FVis.FCanvas.FrameStart;
end;

procedure TGLVisForm2.SetSizePMIClick(Sender: TObject);
var
  ASize  : TPoint;
begin
  ParseInts(Dialogs.InputBox('Set Size','Enter the size you want to set the window to (B x H)',IntToStr(Width)+' x '+IntToStr(Height)),[@ASize.X,@ASize.Y]);
  SetBounds(Left,Top,ASize.X,ASize.Y);
end;

procedure TGLVisForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //FVis.VisFormDoFormClose;
  CloseAction:=caHide;
  Screen.Cursor:=crDefault;
end;

procedure TGLVisForm2.FormChangeBounds(Sender: TObject);
begin
  if FSizeParams.IsChanging
    then exit;
  FSizeParams.IsChanging:=true;
  FSizeParams.Left.&Set(Left);
  FSizeParams.Top.&Set(Top);
  FSizeParams.Width.&Set(Width);
  FSizeParams.Height.&Set(Height);
  FSizeParams.Fullscreen.&Set(WindowState = wsMaximized);
  FSizeParams.Monitor.&Set(Monitor.MonitorNum+1);
  FSIzeParams.IsChanging:=false;
end;

procedure TGLVisForm2.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //TODO: implement this
end;

procedure TGLVisForm2.GLControlMakeCurrent(Sender: TObject; var Allow: boolean);
begin
  //Allow:=(ThisThread = FVis.FVisThread);
  //if Allow then ThreadMsg('',TdMInfo);
  Allow:=true;
end;

procedure TGLVisForm2.GLControlResize(Sender: TObject);
begin
  FVis.FCanvas.Resized;
end;

procedure TGLVisForm2.HeaderEditExit(Sender: TObject);
begin
  HeaderEdit.Cursor:=crArrow;
  HeaderEdit.Visible:=false;
  FVis.Name:=HeaderEdit.Text;

  Timer.Tag:=0;
  Timer.Enabled:=true;
end;

procedure TGLVisForm2.HeaderEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then HeaderEditExit(Sender);
end;

procedure TGLVisForm2.HeaderImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then exit;
  FMouseIsDown:=true;
  if Y<=5 then begin
    if X<=5
      then ResizeForm(15)
      else if X>HeaderImage.Width-5
        then ResizeForm(14)
        else ResizeForm(10);
  end else begin
    if X<=5
      then ResizeForm(5)
      else if X>HeaderImage.Width-5
        then ResizeForm(4)
        else ResizeForm(3);
  end;
end;

procedure TGLVisForm2.HeaderImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not FMouseIsDown then with HeaderImage do begin
    if Y<=5 then begin
      if X<=5
        then Cursor:=crSizeNWSE
        else if X>Width-5
          then Cursor:=crSizeNESW
          else Cursor:=crSizeNS;
    end else begin
      if (X<=5) or (X>Width-5)
        then Cursor:=crSizeWE
        else Cursor:=crDefault;
    end;
  end;
  ShowControls;
end;

procedure TGLVisForm2.HeaderLblClick(Sender: TObject);
begin

end;

procedure TGLVisForm2.HeaderLblDblClick(Sender: TObject);
begin
  Timer.Enabled:=false;
  Timer.Tag:=-1;

  HeaderEdit.Text:=Caption;
  HeaderEdit.Cursor:=crDefault;
  HeaderEdit.Visible:=true;
  ActiveControl:=HeaderEdit;
  HeaderEdit.SelectAll;
end;

procedure TGLVisForm2.HeaderLblMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then exit;
  FMouseIsDown:=true;
  if Y<=5
    then ResizeForm(10)
    else ResizeForm(3);
end;

procedure TGLVisForm2.HeaderLblMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseIsDown then with HeaderLbl do begin
    if Y<=5
      then Cursor:=crSizeNS
      else Cursor:=crDefault;
  end;
  ShowControls;
end;

procedure TGLVisForm2.SetFullScreen(AIsFullscreen: Boolean);
begin
  if FSizeParams.IsChanging or (AIsFullScreen xor (WindowState<>wsMaximized))
    then exit;
  FSizeParams.IsChanging:=true;
  if AIsFullScreen then begin
    WindowState:=wsMaximized;
    MaximizeBtn.Caption:='r';
  end else begin
    WindowState:=wsNormal;
    MaximizeBtn.Caption:='p';
  end;
  FSizeParams.IsChanging:=false;
end;

procedure TGLVisForm2.MaximizeBtnClick(Sender: TObject);
begin
  //toggle state
  SetFullScreen(WindowState=wsNormal);
end;

procedure TGLVisForm2.ResizeForm(AResizeMode: Byte);
var
  AMousePos,ALastMousePos: TPoint;
  AIncrement             : Integer;
begin
  GetCursorPos(ALastMousePos);
  Application.ProcessMessages;
  while FMouseIsDown do begin
    GetCursorPos(AMousePos);
    AIncrement:=AMousePos.X-ALastMousePos.X;
    if (AResizeMode and vcMoveX)>0 then begin
      Left:=Left+AIncrement;
      if (AResizeMode and vcResizeX)>0 then Width:=Width-AIncrement;
    end else if (AResizeMode and vcResizeX)>0 then Width:=Width+AIncrement;
    AIncrement:=AMousePos.Y-ALastMousePos.Y;
    if (AResizeMode and vcMoveY)>0 then begin
      Top:=Top+AIncrement;
      if (AResizeMode and vcResizeY)>0 then Height:=Height-AIncrement;
    end else if (AResizeMode and vcResizeY)>0 then Height:=Height+AIncrement;
    ALastMousePos:=AMousePos;
    Application.ProcessMessages;
  end;
end;

procedure TGLVisForm2.VisMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then exit;
  FMouseIsDown:=true;
  ResizeForm(TControl(Sender).Tag);
end;

procedure TGLVisForm2.VisMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowControls;
end;

procedure TGLVisForm2.VisMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown:=false;
end;

procedure TGLVisForm2.FormCreate(Sender: TObject);
begin
  FExitPos:=1;
  FMouseIsDown:=false;
  GLControl.DoubleBuffered:=true;
end;

procedure TGLVisForm2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //TODO: implement this
end;

procedure TGLVisForm2.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=ExitStr[FExitPos]
    then Inc(FExitPos)
    else if Key=ExitStr[1]
      then FExitPos:=2
      else FExitPos:=1;
  FCanClose:=(FExitPos>Length(ExitStr));
  if FCanClose then begin
    BorderStyle:=bsSingle;
    WindowState:=wsNormal;
    Close;
  end;
end;

procedure TGLVisForm2.ShowControls;
begin
  if Timer.Tag<0 then exit;
  Timer.Tag:=0;
  if Timer.Enabled then exit;
  Timer.Enabled:=true;
  Screen.Cursor:=crDefault;
  LeftPanel.Visible:=true;
  TopPanel.Visible:=true;
  RightPanel.Visible:=true;
  BottomPanel.Visible:=true;
  LeftBottomPanel.Visible:=true;
  RightBottomPanel.Visible:=true;
end;

procedure TGLVisForm2.HideControls;
var
  ACursorPos: TPoint;
begin
  Timer.Enabled:=false;
  GetCursorPos(ACursorPos);
  ACursorPos:=ScreenToClient(ACursorPos);
  if (ACursorPos.X>=0) and (ACursorPos.Y>=0) and (ACursorPos.X<=Width) and (ACursorPos.Y<=Height) and Active
    then Screen.Cursor:=crNone;
  LeftPanel.Visible:=false;
  TopPanel.Visible:=false;
  RightPanel.Visible:=false;
  BottomPanel.Visible:=false;
  LeftBottomPanel.Visible:=false;
  RightBottomPanel.Visible:=false;
end;

procedure TGLVisForm2.TimerTimer(Sender: TObject);
begin
  Timer.Tag:=Timer.Tag+1;
  if Timer.Tag>=HidePos then HideControls;
end;

procedure TGLVisForm2.WndInit(AVis: TGLVisWin2; ASizeParams: TSizeParams);
begin
  FSizeParams:=ASizeParams;
  with FSizeParams do begin
    {
    Left.AddListener(@VisFormLeftChanged, Self);
    Top.AddListener(@VisFormTopChanged, Self);
    Width.AddListener(@VisFormWidthChanged, Self);
    Height.AddListener(@VisFormHeightChanged, Self);
    Fullscreen.AddListener(@VisFormFullscreenChanged, Self);
    Monitor.AddListener(@VisFormMonitorChanged, Self);
    }
    IsChanging:=true;
    //SetBounds(Left.Get, Top.Get, Width.Get, Height.Get);
    //SetFullScreen(Fullscreen.Get);
    IsChanging:=false;
  end;
  FCanClose:=false;
  FVis:=AVis;
end;

procedure TGLVisForm2.FreeSizeParams;
begin
  with FSizeParams do begin
    {
    Left.RemoveListener(@VisFormLeftChanged, Self);
    Top.RemoveListener(@VisFormTopChanged, Self);
    Width.RemoveListener(@VisFormWidthChanged, Self);
    Height.RemoveListener(@VisFormHeightChanged, Self);
    Fullscreen.RemoveListener(@VisFormFullscreenChanged, Self);
    Monitor.RemoveListener(@VisFormMonitorChanged, Self);
    }
    Left:=nil;
    Top:=nil;
    Width:=nil;
    Height:=nil;
    Fullscreen:=nil;
    Monitor:=nil;
  end;
end;

{%ENDREGION TGLVisForm}
{%REGION Misc}

procedure GLVisWinNameChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGLVisWin2(Context)
    do DoSetName(IChangedString(SenderData).Value);
end;

procedure GLVisWinPresetChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGLVisWin2(Context)
   do DoSetPreset(IChangedPreset(SenderData).ExecutedValue);
end;

procedure GLVisWinVisibleChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGLVisWin2(Context) do begin
    if IChangedBoolean(SenderData).Value
      then FWnd.Show
      else FWnd.Hide;
  end;
end;

procedure GLVisWinFrameRateChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TGLVisWin2(Context) do begin
    if not isZero(IChangedFloat(SenderData).Value)
      then FWnd.RenderTimer.Interval:=Round(1000.0 / IChangedFloat(SenderData).Value)
      else FWnd.RenderTimer.Interval:=MaxInt;
  end;
end;

var
  NextVisWinIndex: Cardinal = 0;

function GenerateVisWinName: string;
var
  AIndex: Cardinal;
begin
  AIndex:=InterLockedIncrement(NextVisWinIndex);
  Result:='GLVisualisationWindow' + IntToStr(AIndex);
end;

procedure CreateGLVisWin(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGLVisWin2.Create(APrototype);
end;

procedure Register;
begin
  PresetUtil.RegisterVis(VIDGLVISWIN, @CreateGLVisWin);
  with CreatePreset('OpenGL Visualisation Window', VIDGLVISWIN) do begin
    AddTag(tagListed);
    AddTag('Output.Screen');
    AddInput(This, GLVISWIN_CAPTIONPARAMNAME, 'OpenGL Visualisation Window');
    AddInput(This, GLVISWIN_PRESETPARAMNAME, NULLPRESETID);
    AddInput(This, GLVISWIN_FRAMERATEPARAMNAME, 25.0);
    AddInput(This, GLVISWIN_VISIBLEPARAMNAME, true);
    {
    AddInput(GLVISWINLEFTNAME, 200);
    AddInput(GLVISWINTOPNAME, 200);
    AddInput(GLVISWINWIDTHNAME, 492);
    AddInput(GLVISWINHEIGHTNAME, 416);
    AddInput(GLVISWINFULLSCREENNAME, false);
    AddInput(GLVISWINMONITORINDEXNAME, 1);
    }
  end;
end;

{%ENDREGION}

end.

