unit AppFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAppForm }

  TAppForm = class(TForm)
    TerminateBtn: TButton;
    procedure TerminateBtnClick(Sender: TObject);
  public
    FApp: TApplication;
  end;

implementation

{$R *.lfm}

{ TAppForm }

procedure TAppForm.TerminateBtnClick(Sender: TObject);
begin
  FApp.Terminate;
end;

end.

