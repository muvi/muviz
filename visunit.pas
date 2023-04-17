unit VisUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, GR32_Image, VisDrawUnit, SpectrumData, GR32, VisTypeUnit,
  GR32_Layers, ExtCtrls, AdvFunc, LCLIntf, GR32CanvasUnit,
  MInterfacedObject, VisWinsUnit, VisWinType, VisType;

type
  TLocalVisWin= class;

  { TVisForm }

  TVisForm           = class(TForm)
    CloseBtn: TButton;
    HeaderEdit: TEdit;
    MaximizeBtn: TButton;
    HeaderImage: TImage;
    HeaderLbl: TLabel;
    LeftPanel: TPanel;
    BottomPanel: TPanel;
    RightPanel: TPanel;
    Timer: TTimer;
    VisImage: TImage32;
    procedure BottomPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HeaderEditExit(Sender: TObject);
    procedure HeaderEditKeyPress(Sender: TObject; var Key: char);
    procedure HeaderLblDblClick(Sender: TObject);
    procedure LeftPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MaximizeBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure RightPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VisMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VisMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure VisMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure TimerTimer(Sender: TObject);
    procedure VisImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure VisImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure VisImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    ExitPos           : Integer;
    FCanClose         : Boolean;
    FVis              : TLocalVisWin;
    MDPos             : TPoint;
    MouseIsDown       : Boolean;
    procedure WndInit;
    procedure Resized;
  public
    procedure SetResolution(const AResolution: TPoint);
    procedure ShowControls;
    procedure HideControls;
  end;

  TKeyExecProc       = procedure (AObj: TLocalVisWin; const Value: TVisKeyValue);

  TLocalVisWin       = class (TVisWin)
  private
    FVisThread    : TVisThread;
    FWnd          : TVisForm;
    FCanvas       : TGR32Canvas;
    FPresets      : TPresets;
    FKeyboards    : PVisKeyboards;
    FKeyboard     : PVisKeyboard;
    FDoBackChange : TKeyExecProc;
    FPresetIndex  : Integer;
    FKeyboardIndex: Integer;
    FBackChangeVal: TVisKeyValue;
    FResolution   : TPoint;
    FLoader       : TPresetLoader;
    procedure VisFormFormClose(Sender: TObject; var CloseAction: TCloseAction);
  protected
    procedure SetName_Local(const AName: string);
    function GetName_Local: string;
    procedure SetPreset_Local(const APreset: Integer);
    procedure SetKeyboard_Local(const AKeyboard: Integer);
    procedure SetWaitTime(const AWaitTime: Integer);
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
    property Name: string read GetName_Local write SetName_Local;
    property Preset: Integer read FPresetIndex write SetPreset_Local;
    property WaitTime: Integer write SetWaitTime;
  end;

const
  ExitStr      : string= #17#21#9#20;

implementation

{$R *.lfm}

const
  HidePos = 10;

{TLocalVisWin}

procedure TLocalVisWin_KeyDown_ktNothing(AObj: TLocalVisWin; const Value: TVisKeyValue);
begin

end;

procedure TLocalVisWin_KeyDown_ktPreset(AObj: TLocalVisWin; const Value: TVisKeyValue);
begin
  AObj.FBackChangeVal:=AObj.Preset;
  AObj.SetPreset(Value);
end;

procedure TLocalVisWin_KeyDown_ktKeyboard(AObj: TLocalVisWin; const Value: TVisKeyValue);
begin
  AObj.FBackChangeVal:=AObj.Keyboard;
  AObj.SetKeyboard(Value);
end;

constructor TLocalVisWin.Create(const AName: string; APreset,AKeyboard: Integer; AResolution: TPoint; ASpectrumData: ISpectrumData; var AKeyboards: TVisKeyboards; APresets: TPresets; ALoader: TPresetLoader);
begin
  inherited Create;
  Application.CreateForm(TVisForm,FWnd);
  FPresets:=APresets;
  FKeyboards:=@AKeyboards;
  FPresetIndex:=APreset;
  FKeyboardIndex:=AKeyboard;
  FKeyboard:=@AKeyboards[AKeyboard];
  FCanvas:=TGR32Canvas.Create(FWnd.VisImage.Bitmap);
  FDoBackChange:=@TLocalVisWin_KeyDown_ktNothing;
  FLoader:=ALoader;
  with FWnd do begin
    FVis:=Self;
    WndInit;
    OnClose:=@VisFormFormClose;
    FCompositions:=TVisCompositions.Create(APresets,FCanvas,ASpectrumData,@GetGlobalVisBGThread.Push,ALoader);
    FVisThread:=TVisThread.Create(FCanvas,ASpectrumData,FCompositions[0]{APresets[APreset]});
    MouseIsDown:=false;
    SetName_Local(AName);
    SetResolution_Local(AResolution);


    Show;
  end;
end;

destructor TLocalVisWin.Destroy;
begin
  //FVisThread wird in VisFormDoFormClose() freigegeben
  with FWnd do begin
    //OnClose:=@VisForm_Default_FormClose;
    FCanClose:=true;
    Close;
  end;
end;

procedure TLocalVisWin.BringToFront;
begin
  FWnd.BringToFront;
end;

const
  LocalVisWin_KeyDown: array [TVisKeyType] of TKeyExecProc = (@TLocalVisWin_KeyDown_ktNothing,@TLocalVisWin_KeyDown_ktPreset,@TLocalVisWin_KeyDown_ktKeyboard);

procedure TLocalVisWin.KeyDown(const AKey: Char);
begin
  with FKeyboard^.Keys[AKey] do begin
    LocalVisWin_KeyDown[KeyType](Self,Value);
    if BackChange then FDoBackChange:=LocalVisWin_KeyDown[KeyType];
  end;
end;

procedure TLocalVisWin.KeyUp;
begin
  FDoBackChange(Self,FBackChangeVal);
  FDoBackChange:=@TLocalVisWin_KeyDown_ktNothing;
end;

procedure TLocalVisWin.SetEnabled(Value: Boolean);
begin
  if Value=Enabled then exit;
  inherited SetEnabled(Value);
  if Value
    then FVisThread.Resume
    else FVisThread.Suspend;
end;

procedure TLocalVisWin.SetName_Local(const AName: string);
begin
  with FWnd do begin
    Caption:=AName;
    HeaderLbl.Caption:=Caption;
  end;
end;

function TLocalVisWin.GetName_Local: string;
begin
  Result:=FWnd.Caption;
end;

procedure TLocalVisWin.SetPreset_Local(const APreset: Integer);
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

procedure TLocalVisWin.SetKeyboard_Local(const AKeyboard: Integer);
begin
  if (not Enabled) or (AKeyboard<0) or (AKeyboard>=Length(FKeyboards^)) then exit;
  FKeyboard:=@FKeyboards^[AKeyboard];
  FKeyboardIndex:=AKeyboard;
end;

procedure TLocalVisWin.SetWaitTime(const AWaitTime: Integer);
begin
  FVisThread.FWaitTime:=AWaitTime;
end;

procedure TLocalVisWin.SetResolution_Local(const AResolution: TPoint);
begin
  FVisThread.Suspend;
  FWnd.SetResolution(AResolution);
  FVisThread.Resume;
  FResolution:=AResolution;
end;

function TLocalVisWin.GetPreset: MVIndex; stdcall;
begin
  Result:=FPresetIndex;
end;

procedure TLocalVisWin.SetPreset(const APreset: MVIndex); stdcall;
begin
  SetPreset_Local(APreset);
end;

function TLocalVisWin.GetName: ShortString; stdcall;
begin
  Result:=FWnd.Caption;
end;

function TLocalVisWin.GetKeyboard: MVIndex; stdcall;
begin
  Result:=FKeyboardIndex;
end;

procedure TLocalVisWin.SetKeyboard(const AKeyboard: MVIndex); stdcall;
begin
  SetKeyboard_Local(AKeyboard);
end;

procedure TLocalVisWin.SetName(const AName: ShortString); stdcall;
begin
  with FWnd do begin
    Caption:=AName;
    HeaderLbl.Caption:=AName;
  end;
end;

function TLocalVisWin.GetResolution: TMuviPoint; stdcall;
begin
  with Result do begin
    X:=FResolution.X;
    Y:=FResolution.Y;
  end;
end;

procedure TLocalVisWin.SetResolution(const AResolution: TMuviPoint); stdcall;
begin
  SetResolution_Local(Point(AResolution.X,AResolution.Y));
end;

function TLocalVisWin.GetParamCount: Cardinal; stdcall;
begin
  Result:=FVisThread.Composition.ParamCount;
end;

function TLocalVisWin.GetParamType(Index: Cardinal): TVisParamType; stdcall;
begin
  Result:=FVisThread.Composition.ParamTypes[Index];
end;

function TLocalVisWin.GetParamName(Index: Cardinal): ShortString; stdcall;
begin
  Result:=FVisThread.Composition.ParamNames[Index];
end;

function TLocalVisWin.ParamPtr(Index: Cardinal): PVCOutput;
begin
  Result:=FVisThread.Composition.ParamPtr(Index);
end;

procedure TLocalVisWin.SetParam(Index: Cardinal; const Value); stdcall;
begin
  FVisThread.Composition.SetParam(Index,Value);
end;

procedure TLocalVisWin.GetParam(Index: Cardinal; out Value); stdcall;
begin
  FVisThread.Composition.GetParam(Index,Value);
end;

procedure TLocalVisWin.VisFormFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FVisThread.Destroy;
  FCompositions.Destroy;
  FCanvas.Destroy;
  CloseAction:=caFree;
  Screen.Cursor:=crDefault;
  inherited Destroy;
end;

{TVisForm}

procedure TVisForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=FCanClose;
end;

procedure TVisForm.VisMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AMousePos: TPoint;
begin
  MouseIsDown:=true;
  GetCursorPos(MDPos);
  Application.ProcessMessages;
  while MouseIsDown do begin
    GetCursorPos(AMousePos);
    Left:=Left+AMousePos.X-MDPos.X;
    Top:=Top+AMousePos.Y-MDPos.Y;
    MDPos:=AMousePos;
    Application.ProcessMessages;
  end;
end;

procedure TVisForm.RightPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AMousePos: TPoint;
begin
  MouseIsDown:=true;
  GetCursorPos(MDPos);
  Application.ProcessMessages;
  while MouseIsDown do begin
    GetCursorPos(AMousePos);
    Width:=Width+AMousePos.X-MDPos.X;
    MDPos:=AMousePos;
    Application.ProcessMessages;
  end;
  Resized;
end;

procedure TVisForm.CloseBtnClick(Sender: TObject);
begin
  FCanClose:=true;
  Close;
end;

procedure TVisForm.BottomPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AMousePos: TPoint;
begin
  MouseIsDown:=true;
  GetCursorPos(MDPos);
  Application.ProcessMessages;
  while MouseIsDown do begin
    GetCursorPos(AMousePos);
    Height:=Height+AMousePos.Y-MDPos.Y;
    MDPos:=AMousePos;
    Application.ProcessMessages;
  end;
  Resized;
end;

procedure TVisForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FVis.KeyUp;
end;

procedure TVisForm.HeaderEditExit(Sender: TObject);
begin
  HeaderEdit.Cursor:=crArrow;
  HeaderEdit.Visible:=false;
  Caption:=HeaderEdit.Text;
  HeaderLbl.Caption:=Caption;

  Timer.Tag:=0;
  Timer.Enabled:=true;
end;

procedure TVisForm.HeaderEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then HeaderEditExit(Sender);
end;

procedure TVisForm.HeaderLblDblClick(Sender: TObject);
begin
  Timer.Enabled:=false;
  Timer.Tag:=-1;

  HeaderEdit.Text:=Caption;
  HeaderEdit.Cursor:=crDefault;
  HeaderEdit.Visible:=true;
  ActiveControl:=HeaderEdit;
  HeaderEdit.SelectAll;
end;

procedure TVisForm.LeftPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AMousePos: TPoint;
  Increment: Integer;
begin
  MouseIsDown:=true;
  GetCursorPos(MDPos);
  Application.ProcessMessages;
  while MouseIsDown do begin
    GetCursorPos(AMousePos);
    Increment:=AMousePos.X-MDPos.X;
    Width:=Width-Increment;
    Left:=Left+Increment;
    MDPos:=AMousePos;
    Application.ProcessMessages;
  end;
  Resized;
end;

procedure TVisForm.MaximizeBtnClick(Sender: TObject);
begin
  if WindowState=wsNormal then begin
    WindowState:=wsMaximized;
    MaximizeBtn.Caption:='r';
  end else begin
    WindowState:=wsNormal;
    MaximizeBtn.Caption:='p';
  end;
  Resized;
end;

procedure TVisForm.VisMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowControls;
end;

procedure TVisForm.VisMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown:=false;
end;

procedure TVisForm.FormCreate(Sender: TObject);
begin
  ExitPos:=1;
end;

procedure TVisForm.FormDestroy(Sender: TObject);
begin
end;

procedure TVisForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FVis.KeyDown(Chr(Key));
end;

procedure TVisForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=ExitStr[ExitPos]
    then Inc(ExitPos)
    else if Key=ExitStr[1]
      then ExitPos:=2
      else ExitPos:=1;
  FCanClose:=(ExitPos>Length(ExitStr));
  if FCanClose then begin
    BorderStyle:=bsSingle;
    WindowState:=wsNormal;
    Close;
  end;
end;

procedure TVisForm.SetResolution(const AResolution: TPoint);
begin
  VisImage.Height:=Height;
  VisImage.Width:=Width;
  VisImage.Bitmap.Width:=AResolution.X;
  VisImage.Bitmap.Height:=AResolution.Y;
end;

procedure TVisForm.ShowControls;
begin
  if Timer.Tag<0 then exit;
  Timer.Tag:=0;
  if Timer.Enabled then exit;
  HeaderLbl.Visible:=true;
  HeaderImage.Visible:=true;
  Timer.Enabled:=true;
  Screen.Cursor:=crDefault;
  LeftPanel.Visible:=true;
  RightPanel.Visible:=true;
  BottomPanel.Visible:=true;
  MaximizeBtn.Visible:=true;
  CloseBtn.Visible:=true;
end;

procedure TVisForm.HideControls;
var
  ACursorPos: TPoint;
begin
  Timer.Enabled:=false;
  HeaderLbl.Visible:=false;
  HeaderImage.Visible:=false;
  GetCursorPos(ACursorPos);
  ACursorPos:=ScreenToClient(ACursorPos);
  if (ACursorPos.X>=0) and (ACursorPos.Y>=0) and (ACursorPos.X<=Width) and (ACursorPos.Y<=Height) and Active
    then Screen.Cursor:=crNone;
  LeftPanel.Visible:=false;
  RightPanel.Visible:=false;
  BottomPanel.Visible:=false;
  MaximizeBtn.Visible:=false;
  CloseBtn.Visible:=false;
end;

procedure TVisForm.TimerTimer(Sender: TObject);
begin
  Timer.Tag:=Timer.Tag+1;
  if Timer.Tag>=HidePos then HideControls;
end;

procedure TVisForm.VisImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  VisMouseDown(Sender,Button,Shift,X,Y);
end;

procedure TVisForm.VisImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  VisMouseMove(Sender,Shift,X,Y);
end;

procedure TVisForm.VisImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  VisMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TVisForm.Resized;
begin

end;

procedure TVisForm.WndInit;
begin
  VisImage.BeginUpdate;
  VisImage.Bitmap.BeginUpdate;

  VisImage.Bitmap.SetSize(400,400);
  VisImage.Bitmap.Clear($FF000000);

  VisImage.Bitmap.EndUpdate;
  VisImage.Bitmap.Changed;
  VisImage.EndUpdate;
  VisImage.Changed;
  FCanClose:=false;
end;

end.

