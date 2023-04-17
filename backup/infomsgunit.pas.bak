unit InfoMsgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  InfoMsgFrame, InfoMsg;

type

  { TInfoMsgForm }

  TInfoMsgForm = class(TForm)
    InfoMsgFrm1: TInfoMsgFrm;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InfoMsgQueueChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  InfoMsgForm: TInfoMsgForm;

implementation

{ TInfoMsgForm }

procedure TInfoMsgForm.FormCreate(Sender: TObject);
begin
  InfoMsgQueue.OnMsgsChanged:=@InfoMsgQueueChanged;
end;

procedure TInfoMsgForm.FormDestroy(Sender: TObject);
begin
  InfoMsgQueue.OnMsgsChanged:=nil;
end;

procedure TInfoMsgForm.InfoMsgQueueChanged(Sender: TObject);
var
  AEnumerator: Pointer;
  AMsg, AMsg1: TInfoMsg;
begin
  AMsg1:=nil;

  AEnumerator:=nil;
  AMsg:=InfoMsgQueue.IterateMsgs(AEnumerator);
  AMsg1:=AMsg;
  while AMsg <> nil
    do AMsg:=InfoMsgQueue.IterateMsgs(AEnumerator);

  if AMsg1 <> nil then begin
    InfoMsgFrm1.AssignMsg(AMsg1);
    if not Visible
      then Show;
  end else if Visible
    then Hide;
end;

initialization
  {$I infomsgunit.lrs}
end.

