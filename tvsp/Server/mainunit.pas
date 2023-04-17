unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lNetComponents, TVSPServer, TVSPConst, DiagnoseUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    DiagnoseBtn: TButton;
    procedure DiagnoseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: TTVSPServer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  AStr  : string;
  APort : Integer;
begin
  if Application.HasOption('p', 'port') then begin
    AStr:=Application.GetOptionValue('p', 'port');
    if (not TryStrToInt(AStr, APort))
      or (APort < 0) or (APort > 65535) then begin
      WriteLn('invalid port: ' + AStr);
      Application.Terminate;
      exit;
    end;
  end else APort:=DEFAULTPORT;
  FServer:=TTVSPServer.Create(Self, APort);
end;

procedure TMainForm.DiagnoseBtnClick(Sender: TObject);
begin
  DiagnoseForm.Show;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FServer.Destroy;
end;

end.

