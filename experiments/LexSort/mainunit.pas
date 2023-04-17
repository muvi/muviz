unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LexCompare, LexDebug;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    LexedLbl: TLabel;
    CompareLbl: TLabel;
    procedure Edit1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Edit1Change(Sender: TObject);
var
  ALexedString1, ALexedString2: TLexedString;
begin
  ALexedString1:=Lex(Edit1.Text);
  ALexedString2:=Lex(Edit2.Text);

  LexedLbl.Caption:=LexDebug.LexDebug(ALexedString1);

  case ALexedString1.Compare(ALexedString2) of
    -1: CompareLbl.Caption:='Oben < Unten';
    0 : CompareLbl.Caption:='Oben = Unten';
    1 : CompareLbl.Caption:='Oben > Unten';
    else CompareLbl.Caption:='???';
  end;

  ALexedString1.Destroy;
  ALexedString2.Destroy;
end;

end.

