unit GLVisUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, VisDrawUnit, SpectrumData, VisTypeUnit, MCanvasUnit, GLCanvasUnit,
  ExtCtrls, AdvFunc, LCLIntf, Menus, AdvThreads,
  MInterfacedObject, VisWinsUnit, VisWinType, VisType, OpenGLContext;

type
  TGLVisWin          = class;

  { TVisForm }

  { TGLVisForm }

  TGLVisForm         = class(TForm)
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
    TopPanel: TPanel;
    RightPanel: TPanel;
    Timer: TTimer;
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
    FVis              : TGLVisWin;
    FMouseIsDown      : Boolean;
    procedure WndInit(AVis: TGLVisWin);
    procedure ResizeForm(AResizeMode: Byte);
  public
    procedure SetResolution(const AResolution: TPoint);
    procedure ShowControls;
    procedure HideControls;
  end;

  TKeyExecProc       = procedure (AObj: TGLVisWin; const Value: TVisKeyValue);

  TGLVisWin          = class (TVisWin)
  private
    FVisThread    : TVisThread2;
    FWnd          : TGLVisForm;
    FCanvas       : TGLCanvas;
    FPresets      : TPresets;
    FKeyboards    : PVisKeyboards;
    FKeyboard     : PVisKeyboard;
    FDoBackChange : TKeyExecProc;
    FPresetIndex  : Integer;
    FKeyboardIndex: Integer;
    FBackChangeVal: TVisKeyValue;
    FResolution   : TPoint;
    FLoader       : TPresetLoader;
    procedure GLControlPaintNothing(Sender: TObject);
    procedure VisFormDoFormClose;
    function GetThread: TFPSThread;
  protected
    procedure SetName_Local(const AName: string);
    function GetName_Local: string;
    procedure SetPreset_Local(const APreset: Integer);
    procedure SetKeyboard_Local(const AKeyboard: Integer);
    //procedure SetWaitTime(const AWaitTime: Integer);
    procedure SetResolution_Local(const AResolution: TPoint);

    function GetPreset: MVIndex; stdcall; override;
    procedure SetPreset(const APreset: MVIndex); stdcall; override;
    function GetKeyboard: MVIndex; stdcall; override;
    procedure SetKeyboard(const AKeyboard: MVIndex); stdcall; override;
    function GetName: ShortString; stdcall; override;
    procedure SetName(const AName: ShortString); stdcall; override;
    function GetResolution: TMuviPoint; stdcall; override;
    procedure SetResolution(const AResolution: TMuviPoint); stdcall; override;
    function GetParamCount: Cardinal; stdcall; override;
    function GetParamType(Index: Cardinal): TVisParamType; stdcall; override;
    function GetParamName(Index: Cardinal): ShortString; stdcall; override;
    procedure SetEnabled(Value: Boolean); override;
    function GetLineWidth: Real;
    procedure SetLineWidth(Value: Real);
  public
    constructor Create(const AName: string; APreset,AKeyboard: Integer; AResolution: TPoint; ASpectrumData: ISpectrumData; var AKeyboards: TVisKeyboards; APresets: TPresets; ALoader: TPresetLoader);
    destructor Destroy; override;
    procedure BringToFront;
    procedure KeyDown(const AKey: Char); override;
    procedure KeyUp; override;
    function ParamPtr(Index: Cardinal): PVCOutput; override;
    procedure SetParam(Index: Cardinal; const Value); stdcall; override;
    procedure GetParam(Index: Cardinal; out Value); stdcall; override;

    property Resolution: TPoint read FResolution write SetResolution_Local;
  published
    property Keyboard: Integer read FKeyboardIndex write SetKeyboard_Local;
    property LineWidth: Real read GetLineWidth write SetLineWidth;
    property Name: string read GetName_Local write SetName_Local;
    property Preset: Integer read FPresetIndex write SetPreset_Local;
    property Thread: TFPSThread read GetThread;
  end;

const
  ExitStr      : string= #17#21#9#20;

implementation

{$R *.lfm}

const
  HidePos = 10;
  vcMoveX  = 1;
  vcMoveY  = 2;
  vcResizeX= 4;
  vcResizeY= 8;

{TLocalVisWin}

procedure TLocalVisWin_KeyDown_ktNothing(AObj: TGLVisWin; const Value: TVisKeyValue);
begin

end;

procedure TLocalVisWin_KeyDown_ktPreset(AObj: TGLVisWin; const Value: TVisKeyValue);
begin
  AObj.FBackChangeVal:=AObj.Preset;
  AObj.SetPreset(Value);
end;

procedure TLocalVisWin_KeyDown_ktKeyboard(AObj: TGLVisWin; const Value: TVisKeyValue);
begin
  AObj.FBackChangeVal:=AObj.Keyboard;
  AObj.SetKeyboard(Value);
end;

constructor TGLVisWin.Create(const AName: string; APreset,AKeyboard: Integer; AResolution: TPoint; ASpectrumData: ISpectrumData; var AKeyboards: TVisKeyboards; APresets: TPresets; ALoader: TPresetLoader);
begin
  inherited Create;
  Application.CreateForm(TGLVisForm,FWnd);
  FPresets:=APresets;
  FKeyboards:=@AKeyboards;
  FPresetIndex:=APreset;
  FKeyboardIndex:=AKeyboard;
  FKeyboard:=@AKeyboards[AKeyboard];
  FCanvas:=TGLCanvas.Create(FWnd.GLControl);
  FDoBackChange:=@TLocalVisWin_KeyDown_ktNothing;
  FLoader:=ALoader;
  with FWnd do begin
    WndInit(Self);
    FCompositions:=TVisCompositions.Create(APresets,FCanvas,ASpectrumData,@GetGlobalVisBGThread.Push,ALoader);
    Show;

    FVisThread:=TVisThread2.Create(FCanvas,ASpectrumData,FCompositions[0]{APresets[APreset]});
    SetName_Local(AName);
    SetResolution_Local(AResolution);
  end;
end;

destructor TGLVisWin.Destroy;
begin
  //FVisThread wird in VisFormDoFormClose() freigegeben
  with FWnd do begin
    FCanClose:=true;
    Close;
  end;
end;

procedure TGLVisWin.BringToFront;
begin
  FWnd.BringToFront;
end;

const
  LocalVisWin_KeyDown: array [TVisKeyType] of TKeyExecProc = (@TLocalVisWin_KeyDown_ktNothing,@TLocalVisWin_KeyDown_ktPreset,@TLocalVisWin_KeyDown_ktKeyboard);

procedure TGLVisWin.KeyDown(const AKey: Char);
begin
  with FKeyboard^.Keys[AKey] do begin
    LocalVisWin_KeyDown[KeyType](Self,Value);
    if BackChange then FDoBackChange:=LocalVisWin_KeyDown[KeyType];
  end;
end;

procedure TGLVisWin.KeyUp;
begin
  FDoBackChange(Self,FBackChangeVal);
  FDoBackChange:=@TLocalVisWin_KeyDown_ktNothing;
end;

procedure TGLVisWin.SetEnabled(Value: Boolean);
begin
  if Value=Enabled then exit;
  inherited SetEnabled(Value);
  if Value
    then FVisThread.Resume
    else FVisThread.Suspend;
end;

function TGLVisWin.GetLineWidth: Real;
begin
  Result:=FCanvas.LineWidth;
end;

procedure TGLVisWin.SetLineWidth(Value: Real);
begin
  FCanvas.LineWidth:=Value;
end;

procedure TGLVisWin.SetName_Local(const AName: string);
begin
  with FWnd do begin
    Caption:=AName;
    HeaderLbl.Caption:=Caption;
  end;
end;

function TGLVisWin.GetName_Local: string;
begin
  Result:=FWnd.Caption;
end;

procedure TGLVisWin.SetPreset_Local(const APreset: Integer);
begin
  if (not Enabled) or (APreset<0) or (APreset>=FPresets.Count) then exit;
  if FVisThread.SetVis(FCompositions[APreset]) then begin
    FPresetIndex:=APreset;
    if Assigned(OnChangePreset) then begin
      FVisThread.WaitForChange;
      OnChangePreset(Self);
    end;
  end;
end;

procedure TGLVisWin.SetKeyboard_Local(const AKeyboard: Integer);
begin
  if (not Enabled) or (AKeyboard<0) or (AKeyboard>=Length(FKeyboards^)) then exit;
  FKeyboard:=@FKeyboards^[AKeyboard];
  FKeyboardIndex:=AKeyboard;
end;

procedure TGLVisWin.SetResolution_Local(const AResolution: TPoint);
begin
  FVisThread.Suspend;
  FWnd.SetResolution(AResolution);
  FVisThread.Resume;
  FResolution:=AResolution;
end;

function TGLVisWin.GetPreset: MVIndex; stdcall;
begin
  Result:=FPresetIndex;
end;

procedure TGLVisWin.SetPreset(const APreset: MVIndex); stdcall;
begin
  SetPreset_Local(APreset);
end;

function TGLVisWin.GetName: ShortString; stdcall;
begin
  Result:=FWnd.Caption;
end;

function TGLVisWin.GetKeyboard: MVIndex; stdcall;
begin
  Result:=FKeyboardIndex;
end;

procedure TGLVisWin.SetKeyboard(const AKeyboard: MVIndex); stdcall;
begin
  SetKeyboard_Local(AKeyboard);
end;

procedure TGLVisWin.SetName(const AName: ShortString); stdcall;
begin
  with FWnd do begin
    Caption:=AName;
    HeaderLbl.Caption:=AName;
  end;
end;

function TGLVisWin.GetResolution: TMuviPoint; stdcall;
begin
  with Result do begin
    X:=FResolution.X;
    Y:=FResolution.Y;
  end;
end;

procedure TGLVisWin.SetResolution(const AResolution: TMuviPoint); stdcall;
begin
  SetResolution_Local(Point(AResolution.X,AResolution.Y));
end;

function TGLVisWin.GetParamCount: Cardinal; stdcall;
begin
  Result:=FVisThread.Composition.ParamCount;
end;

function TGLVisWin.GetParamType(Index: Cardinal): TVisParamType; stdcall;
begin
  Result:=FVisThread.Composition.ParamTypes[Index];
end;

function TGLVisWin.GetParamName(Index: Cardinal): ShortString; stdcall;
begin
  Result:=FVisThread.Composition.ParamNames[Index];
end;

function TGLVisWin.ParamPtr(Index: Cardinal): PVCOutput;
begin
  Result:=FVisThread.Composition.ParamPtr(Index);
end;

procedure TGLVisWin.SetParam(Index: Cardinal; const Value); stdcall;
begin
  FVisThread.Composition.SetParam(Index,Value);
end;

procedure TGLVisWin.GetParam(Index: Cardinal; out Value); stdcall;
begin
  FVisThread.Composition.GetParam(Index,Value);
end;

function TGLVisWin.GetThread: TFPSThread;
begin
  Result:=FVisThread;
end;

procedure TGLVisWin.GLControlPaintNothing(Sender: TObject);
begin
  FVisThread.FrameStarted;
end;

procedure TGLVisWin.VisFormDoFormClose;
begin
  FVisThread.Terminate;
  FWnd.GLControl.OnPaint:=@GLControlPaintNothing;
  FVisThread.Destroy;
  FCompositions.Destroy;
  FCanvas.Destroy;
  inherited Destroy;
end;

{TVisForm}

procedure TGLVisForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=FCanClose;
end;

procedure TGLVisForm.CloseBtnClick(Sender: TObject);
begin
  FCanClose:=true;
  Close;
end;

procedure TGLVisForm.SetSizePMIClick(Sender: TObject);
var
  ASize  : TPoint;
begin
  ParseInts(Dialogs.InputBox('Set Size','Enter the size you want to set the window to (B x H)',IntToStr(Width)+' x '+IntToStr(Height)),[@ASize.X,@ASize.Y]);
  SetBounds(Left,Top,ASize.X,ASize.Y);
end;

procedure TGLVisForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FVis.VisFormDoFormClose;
  CloseAction:=caFree;
  Screen.Cursor:=crDefault;
end;

procedure TGLVisForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FVis.KeyUp;
end;

procedure TGLVisForm.GLControlMakeCurrent(Sender: TObject; var Allow: boolean);
begin
  //Allow:=(ThisThread = FVis.FVisThread);
  //if Allow then ThreadMsg('',TdMInfo);
  Allow:=true;
end;

procedure TGLVisForm.GLControlResize(Sender: TObject);
begin
  FVis.FCanvas.Resized;
end;

procedure TGLVisForm.HeaderEditExit(Sender: TObject);
begin
  HeaderEdit.Cursor:=crArrow;
  HeaderEdit.Visible:=false;
  Caption:=HeaderEdit.Text;
  HeaderLbl.Caption:=Caption;

  Timer.Tag:=0;
  Timer.Enabled:=true;
end;

procedure TGLVisForm.HeaderEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then HeaderEditExit(Sender);
end;

procedure TGLVisForm.HeaderImageMouseDown(Sender: TObject;
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

procedure TGLVisForm.HeaderImageMouseMove(Sender: TObject; Shift: TShiftState;
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

procedure TGLVisForm.HeaderLblClick(Sender: TObject);
begin

end;

procedure TGLVisForm.HeaderLblDblClick(Sender: TObject);
begin
  Timer.Enabled:=false;
  Timer.Tag:=-1;

  HeaderEdit.Text:=Caption;
  HeaderEdit.Cursor:=crDefault;
  HeaderEdit.Visible:=true;
  ActiveControl:=HeaderEdit;
  HeaderEdit.SelectAll;
end;

procedure TGLVisForm.HeaderLblMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then exit;
  FMouseIsDown:=true;
  if Y<=5
    then ResizeForm(10)
    else ResizeForm(3);
end;

procedure TGLVisForm.HeaderLblMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseIsDown then with HeaderLbl do begin
    if Y<=5
      then Cursor:=crSizeNS
      else Cursor:=crDefault;
  end;
  ShowControls;
end;

procedure TGLVisForm.MaximizeBtnClick(Sender: TObject);
begin
  if WindowState=wsNormal then begin
    WindowState:=wsMaximized;
    MaximizeBtn.Caption:='r';
  end else begin
    WindowState:=wsNormal;
    MaximizeBtn.Caption:='p';
  end;
end;

procedure TGLVisForm.ResizeForm(AResizeMode: Byte);
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

procedure TGLVisForm.VisMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then exit;
  FMouseIsDown:=true;
  ResizeForm(TControl(Sender).Tag);
end;

procedure TGLVisForm.VisMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowControls;
end;

procedure TGLVisForm.VisMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown:=false;
end;

procedure TGLVisForm.FormCreate(Sender: TObject);
begin
  FExitPos:=1;
  FMouseIsDown:=false;
  GLControl.DoubleBuffered:=true;
end;

procedure TGLVisForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FVis.KeyDown(Chr(Key));
end;

procedure TGLVisForm.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TGLVisForm.SetResolution(const AResolution: TPoint);
begin

end;

procedure TGLVisForm.ShowControls;
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

procedure TGLVisForm.HideControls;
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

procedure TGLVisForm.TimerTimer(Sender: TObject);
begin
  Timer.Tag:=Timer.Tag+1;
  if Timer.Tag>=HidePos then HideControls;
end;

procedure TGLVisForm.WndInit(AVis: TGLVisWin);
begin
  {VisImage.BeginUpdate;
  VisImage.Bitmap.BeginUpdate;

  VisImage.Bitmap.SetSize(400,400);
  VisImage.Bitmap.Clear($FF000000);

  VisImage.Bitmap.EndUpdate;
  VisImage.Bitmap.Changed;
  VisImage.EndUpdate;
  VisImage.Changed;}
  FVis:=AVis;
  //FResizeMode:=0;
  FCanClose:=false;
end;

end.

