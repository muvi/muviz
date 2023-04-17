unit PerformanceTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Spin, StdCtrls, VisType2, PresetType, LCLIntf, StdParamTypes, MStrings;

type

  { TPerformanceTestForm }

  TPerformanceTestForm = class(TForm)
    TestBtn: TButton;
    CountLbl: TLabel;
    TimeLbl: TLabel;
    CountEdit: TSpinEdit;
    procedure TestBtnClick(Sender: TObject);
  end;

var
  PerformanceTestForm: TPerformanceTestForm;

implementation

{ TPerformanceTestForm }

procedure TPerformanceTestForm.TestBtnClick(Sender: TObject);
var
  AParam: IPCall;
  ATime : Cardinal;
  I     : Integer;
begin
  AParam:=IPCall(PresetUtil[NULLPRESETID].Inputs[ParamID('testing', vCall)]);
  ATime:=GetTickCount;
  for I:=0 to CountEdit.Value-1
    do AParam.&Set;
  TimeLbl.Caption:='Needed Time: ' + IntToStr(GetTickCount - ATime);
end;

initialization
  {$I PerformanceTestUnit.lrs}

end.

