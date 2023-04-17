unit AboutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, Math, PluginType, LCLIntf, LCLAdvFunc;

const
  MUVI_VERSION: MVVersion = (Version:0; MainVersion:3; SubVersion:1) {10158};

//Version Tags
//progress
{$DEFINE VERP_ALPHA}
{.$DEFINE VERP_BETA}
{.$DEFINE VERP_GAMMA}
//accessibility
{$DEFINE VERA_NIGHTLY}
{.$DEFINE VERA_RC}
{.$DEFINE VERA_RELEASE}
//quality
{.$DEFINE VERQ_TRIAL}
{.$DEFINE VERQ_LITE}
{$DEFINE VERQ_DEFAULT}
{.$DEFINE VERQ_FULL}
{.$DEFINE VERQ_PRO}

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseBevel2: TBevel;
    CloseBevel1: TBevel;
    Image: TImage;
    CloseLbl: TLabel;
    AlphaTimer: TTimer;
    Label1: TLabel;
    MainIconImage: TImage;
    ProcNameLbl: TLabel;
    WebLbl: TLabel;
    ThirdPartyDescLbl: TLabel;
    VersionLbl: TLabel;
    ReleaseDateLbl: TLabel;
    procedure AlphaTimerTimer(Sender: TObject);
    procedure CloseMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainIconImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMDOnMainIcon: Boolean;
    FLastMousePos: TPoint;
    FKillCount   : Integer;
  end;

var
  AboutForm: TAboutForm;

implementation

{ TAboutForm }

procedure TAboutForm.AlphaTimerTimer(Sender: TObject);
const
  AAlphaSteps: Integer = 70;
  AStep                = (2*Pi)/70.0;
  AAlphaReduce         = 30/2.0;
var
  ASinPos,AMaxSteps: Integer;
begin
  ASinPos:=(AlphaTimer.Tag+1) mod AAlphaSteps;
  AlphaBlendValue:=$FF-Round((Sin(ASinPos*AStep)+1)*AAlphaReduce);
  AlphaTimer.Tag:=ASinPos;

  if not FMDOnMainIcon then begin
    if FKillCount>=5 then begin
      AMaxSteps:=370 div (FKillCount+8);
      MainIconImage.Left:=(ASinPos mod AMaxSteps)*(FKillCount+8)+40;
      MainIconImage.Top:=(ASinPos div AMaxSteps)*(FKillCount+8)+40;
    end;
  end;
end;

procedure TAboutForm.CloseMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CloseBevel1.Style:=bsRaised;
  CloseBevel2.Style:=bsRaised;
  CloseLbl.Font.Color:=clWhite;
  ModalResult:=mrOK;
  Close;
end;

procedure TAboutForm.CloseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CloseBevel1.Style:=bsLowered;
  CloseBevel2.Style:=bsLowered;
  CloseLbl.Font.Color:=clSilver;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  S: ShortString;
begin
  S:=MUVI_VERSION;
  VersionLbl.Caption:='Version: '+S;
  //progress
  {$IFDEF VERP_ALPHA}
  VersionLbl.Caption:=VersionLbl.Caption+'α';
  {$ELSE}{$IFDEF VERP_BETA}
  VersionLbl.Caption:=VersionLbl.Caption+'β';
  {$ENDIF}{$ENDIF}
  //quality
  {$IFDEF VERQ_TRIAL}
  VersionLbl.Caption:=VersionLbl.Caption+' Trial';
  {$ELSE}{$IFDEF VERQ_LITE}
  VersionLbl.Caption:=VersionLbl.Caption+' Lite';
  {$ELSE}{$IFDEF VERQ_FULL}
  VersionLbl.Caption:=VersionLbl.Caption+' Full';
  {$ELSE}{$IFDEF VERQ_PRO}
  VersionLbl.Caption:=VersionLbl.Caption+' Professional';
  {$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  //accessibility
  {$IFDEF VERA_NIGHTLY}
  VersionLbl.Caption:=VersionLbl.Caption+' Nightly';
  {$ELSE}{$IFDEF VERA_RC}
  VersionLbl.Caption:=VersionLbl.Caption+' Release Candidate';
  {$ENDIF}{$ENDIF}

  ReleaseDateLbl.Caption:='Release Date: '+{$I %Date%}+' '+{$I %Time%};
  //Easter eggs
  FMDOnMainIcon:=false;
  FKillCount:=0;
end;

procedure TAboutForm.FormHide(Sender: TObject);
begin
  AlphaTimer.Enabled:=false;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  AlphaTimer.Enabled:=true;
end;

procedure TAboutForm.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbRight) and (not MainIconImage.Visible) then with MainIconImage do begin
    Left:=40;
    Top:=40;
    Width:=64;
    Height:=64;
    Visible:=true;
  end;
end;

procedure TAboutForm.MainIconImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then begin
    FMDOnMainIcon:=true;
    GetCursorPos(FLastMousePos);
  end;
end;

procedure TAboutForm.ImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ANewMousePos: TPoint;

  procedure AntiKill;
  var
    AMainIconPos: TPoint;
  begin
    if not MainIconImage.Visible then exit;
    AMainIconPos:=MainIconImage.ClientToScreen(Point(0,0));
    if (ANewMousePos.X>=AMainIconPos.X-5)
      and (ANewMousePos.Y>=AMainIconPos.Y-5)
      and (ANewMousePos.X<AMainIconPos.X+69)
      and (ANewMousePos.Y<AMainIconPos.Y+69) then begin
        ANewMousePos:=FLastMousePos;
        SetCursorPos(FLastMousePos.X,FLastMousePos.Y);
      end;
  end;

begin
  GetCursorPos(ANewMousePos);
  if FMDOnMainIcon then begin
    MainIconImage.Left:=MainIconImage.Left+ANewMousePos.X-FLastMousePos.X;
    MainIconImage.Top:=MainIconImage.Top+ANewMousePos.Y-FLastMousePos.Y;
  end;
  if FKillCount=3 then AntiKill;
  FLastMousePos:=ANewMousePos;
end;

procedure TAboutForm.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure VanishMainIcon;
  var
    I: Integer;
  begin
    for I:=0 to 31 do begin
      MainIconImage.Width:=MainIconImage.Width-2;
      MainIconImage.Left:=MainIconImage.Left+1;
      MainIconImage.Height:=MainIconImage.Height-2;
      MainIconImage.Top:=MainIconImage.Top+1;
      Application.ProcessMessages;
      Sleep(20);
    end;
    Inc(FKillCount);
    MainIconImage.Visible:=false;
    case FKillCount of
      3: AdvMsg('I don''t like you playing with my beautiful icon like a ball. That''s enough!!!',TdMWarnung);
      5: AdvMsg('Difficulty level 5: I bet you won''t get the icon now!',TdMInfo);
      else if (FKillCount>=10) and (FKillCount mod 2=0)
        then AdvMsg('you reached level '+IntToStr(FKillCount)+'.',TdMInfo);
    end;
  end;

var
  ADialogCenter: TPoint;
begin
  if FMDOnMainIcon then begin
    ImageMouseMove(Sender,Shift,X,Y);
    ADialogCenter:=Image.ClientToScreen(Point(257,168));
    if (FLastMousePos.X>=ADialogCenter.X-10)
      and (FLastMousePos.Y>=ADialogCenter.Y-10)
      and (FLastMousePos.X<ADialogCenter.X+10)
      and (FLastMousePos.Y<ADialogCenter.Y+10)
      then VanishMainIcon;
    FMDOnMainIcon:=false;
  end;
end;

initialization
  {$I aboutunit.lrs}

end.

