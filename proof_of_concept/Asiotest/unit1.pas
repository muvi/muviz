unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AsioVis;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FAsioMan: TAsioManager;
  public

  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FAsioMan.Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FAsioMan.Init(nil);
  FAsioMan.Driver:=0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FAsioMan.Done;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAsioMan:=TAsioManager.Create(Self,Handle);
end;

end.

