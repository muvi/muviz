unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ALbl: TLabel;
    BLbl: TLabel;
    CalcBtn: TButton;
    CLbl: TLabel;
    ResultLbl: TLabel;
    AEdit: TSpinEdit;
    BEdit: TSpinEdit;
    CEdit: TSpinEdit;
    procedure CalcBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CalcBtnClick(Sender: TObject);
var
  A, B, C, Result: Byte;
begin
  A:=AEdit.Value;
  B:=BEdit.Value;
  C:=CEdit.Value;
  A:=A+B;
  C:=C+B;
  Result:=A xor C;
  ResultLbl.Caption:=IntToStr(Result);
end;

end.

