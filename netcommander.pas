unit NetCommander;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ClientNetType;

type

  TCommandProc = procedure (Command: string) of object; stdcall;

  { TNetForm }

  TNetForm     = class(TForm)
    CommandMemo: TMemo;
    CommandEdit: TEdit;
    procedure CommandEditKeyPress(Sender: TObject; var Key: char);
    procedure CommandMemoChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  public
    FCommandCount: Integer;
    OnCommand    : TCommandProc;
    procedure ClearCommander;
    procedure SetColors(const BG,FG: TColor);
  end;

const
  DefaultText = '---Muvi Network Commander---'+#$D#$A#$D#$A;

function ReturnCount(const S: string; const Start: Integer = 0): Integer;

implementation

{ TNetForm }

procedure TNetForm.CommandEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then with CommandMemo do begin
    if Text[Length(Text)-1]<>#$D
      then Text:=Text+#$D#$A;
    SelStart:=Length(Text);
    SelLength:=0;
    OnCommand(CommandEdit.Text);
    if Length(CommandEdit.Text)>0 then begin
      {if CommandEdit.Text[1]=CommandAdress
        then CommandEdit.Text:=Copy(CommandEdit.Text,1,Pos(CommandDivider,CommandEdit.Text))
        else CommandEdit.Text:='';}
      CommandEdit.SelStart:=Length(CommandEdit.Text);
      CommandEdit.SelLength:=0;
    end;
  end;
end;

procedure TNetForm.CommandMemoChange(Sender: TObject);
begin

end;

procedure TNetForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CommandMemo.Lines.Clear;
end;

procedure TNetForm.FormShow(Sender: TObject);
begin
  ClearCommander;
end;

procedure TNetForm.ClearCommander;
begin
  CommandMemo.Text:=DefaultText;
  CommandMemo.SelStart:=Length(CommandMemo.Text);
  CommandMemo.SelLength:=0;
end;

procedure TNetForm.SetColors(const BG,FG: TColor);
begin
  CommandMemo.Color:=BG;
  CommandMemo.Font.Color:=FG;
  CommandEdit.Color:=BG;
  CommandEdit.Font.Color:=FG;
end;

function ReturnCount(const S: string; const Start: Integer = 0): Integer;
var
  I: Integer;
begin
  Result:=0;
  for I:=Start to Length(S) do if S[I]=#13 then Inc(Result);
end;

initialization
  {$I netcommander.lrs}

end.

