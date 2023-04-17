unit LogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, VisType2;

type

  { TLogForm }

  TLogForm = class(TForm)
    LogLB: TListBox;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure AddMessage(AMsg: string);
  end;

var
  LogForm: TLogForm;

procedure Log(AMsg: string);

implementation

var
  LLoggingEnabled: Cardinal = Cardinal(false);

{%REGION TLogForm}

procedure TLogForm.FormShow(Sender: TObject);
begin
  InterLockedExchange(LLoggingEnabled, Cardinal(true));
end;

procedure TLogForm.FormHide(Sender: TObject);
begin
  InterLockedExchange(LLoggingEnabled, Cardinal(false));
end;

procedure TLogForm.AddMessage(AMsg: string);
begin
  LogLB.TopIndex:=LogLB.Items.Add(AMsg);
end;

{%ENDREGION}
{%REGION TLogMessage}

type
  TLogMessage = class
  strict private
    FMsg: string;
  public
    constructor Create(AMsg: string);
    property Msg: string read FMsg;
  end;

constructor TLogMessage.Create(AMsg: string);
begin
  inherited Create;
  FMsg:=AMsg;
end;

{%ENDREGION}
{%REGION Misc}

procedure LogEvent(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  Assert(Sender = nil);
  with TLogMessage(Context) do begin
    LogForm.AddMessage(Msg);
    Destroy;
  end;
end;

procedure Log(AMsg: string);
var
  ALoggingEnabled: Cardinal;
begin
  InterLockedExchange(ALoggingEnabled, LLoggingEnabled);
  if Boolean(ALoggingEnabled) then begin
    VisualisationUtil.MainThread.Push(@LogEvent, TLogMessage.Create(AMsg), nil, nil);
    //Application.ProcessMessages;
  end;
end;

{%ENDREGION}

initialization
  {$I logunit.lrs}
end.

