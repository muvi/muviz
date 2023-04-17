unit InfoMsgFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, ComCtrls,
  ExtCtrls, InfoMsg;

type

  { TInfoMsgFrm }

  TInfoMsgFrm = class(TFrame)
    CloseBtn: TButton;
    Image: TImage;
    ImageList: TImageList;
    MsgLbl: TLabel;
    ProgressBar: TProgressBar;
    Timer: TTimer;
    procedure CloseBtnClick(Sender: TObject);
    procedure InfoMsgDestroy(Sender: TObject);
    procedure InfoMsgProgress(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FMsg: TInfoMsg;
  public
    procedure AssignMsg(AMsg: TInfoMsg);
    procedure RemoveMsg;
  end;

const
  INFOMSGTIMEOUT = 75; //75 Frames = 3 seconds

implementation

procedure TInfoMsgFrm.AssignMsg(AMsg: TInfoMsg);
begin
  Timer.Enabled:=false;

  FMsg:=AMsg;
  with FMsg do begin
    OnDestroy:=@InfoMsgDestroy;
    OnProgress:=@InfoMsgProgress;
    if Fixed then begin
      if CloseBtn.Visible then begin
        CloseBtn.Visible:=false;
        MsgLbl.Width:=MsgLbl.Width + CloseBtn.Width;
      end;
    end else begin
      if not CloseBtn.Visible then begin
        CloseBtn.Visible:=true;
        MsgLbl.Width:=MsgLbl.Width - CloseBtn.Width;
      end;
    end;
    if HasProgress then begin
      MsgLbl.Height:=56;
      ProgressBar.Visible:=true;
      MsgLbl.Caption:=Msg + #$D#$A + ProgressString;
      ProgressBar.Position:=Progress;
      //Timer.Enabled:=true;
    end else begin
      MsgLbl.Height:=76;
      ProgressBar.Visible:=false;
      MsgLbl.Caption:=Msg;
    end;
    if HasTimeout then begin
      Timer.Tag:=0;
      Timer.Enabled:=true;
    end;
    Image.Picture.Bitmap.SetSize(ImageList.Width, ImageList.Height);
    ImageList.GetBitmap(MsgType, Image.Picture.Bitmap);
  end;
end;

procedure TInfoMsgFrm.InfoMsgDestroy(Sender: TObject);
begin
  RemoveMsg;
end;

procedure TInfoMsgFrm.CloseBtnClick(Sender: TObject);
begin
  Timer.Enabled:=false;
  FMsg.Destroy;
  FMsg:=nil;
end;

procedure TInfoMsgFrm.InfoMsgProgress(Sender: TObject);
begin
  with FMsg do begin
    MsgLbl.Caption:=Msg + #$D#$A + ProgressString;
    ProgressBar.Position:=Progress;
  end;
  Application.ProcessMessages;
end;

procedure TInfoMsgFrm.TimerTimer(Sender: TObject);
begin
  Assert(FMsg <> nil);
  with FMsg do begin
    {
    if HasProgress then begin
      MsgLbl.Caption:=Msg + #$D#$A + ProgressString;
      ProgressBar.Position:=Progress;
    end;
    }
    //if FMsg.HasTimeout then begin
    Assert(FMsg.HasTimeout);
    Timer.Tag:=Timer.Tag + 1;
    if Timer.Tag > INFOMSGTIMEOUT then begin
      Timer.Enabled:=false;
      FMsg.Destroy;
      FMsg:=nil;
    end;
    //end;
  end;
  Application.ProcessMessages;
end;

procedure TInfoMsgFrm.RemoveMsg;
begin
  Timer.Enabled:=false;
  FMsg:=nil;
end;

initialization
  {$I infomsgframe.lrs}
end.

