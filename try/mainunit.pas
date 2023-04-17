unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AppThread;

type

  { TMainForm }

  TMainForm = class(TForm)
    RunBtn: TButton;
    TerminateBtn: TButton;
    procedure RunBtnClick(Sender: TObject);
    procedure TerminateBtnClick(Sender: TObject);
  private
    FApp: TApplication;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.RunBtnClick(Sender: TObject);
var
  AForm1: TMainForm;
begin
  {FApp:=TApplication.Create(nil);
  FApp.Initialize;
  FApp.CreateForm(TMainForm, AForm1);
  FApp.Run;}
  TApplicationThread.Create(Self);
end;

procedure TMainForm.TerminateBtnClick(Sender: TObject);
begin
  FApp.Terminate;
end;

end.

