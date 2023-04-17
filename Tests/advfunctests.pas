unit AdvFuncTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, AdvFunc;

type

  TAdvFuncTest= class(TTestCase)
  published
    procedure TestSplitStrSimple;
    procedure TestSplitStrComplex;
  end;

implementation

procedure TAdvFuncTest.TestSplitStrSimple;
var
  AResult: TStringArray;
begin
  AdvFunc.SplitStr('.bla bla.blubb@.prhek.0.', '.', AResult);
  AssertEquals(6, Length(AResult));
  AssertEquals('', AResult[0]);
  AssertEquals('bla bla', AResult[1]);
  AssertEquals('blubb@', AResult[2]);
  AssertEquals('prhek', AResult[3]);
  AssertEquals('0', AResult[4]);
  AssertEquals('', AResult[5]);
end;

procedure TAdvFuncTest.TestSplitStrComplex;
var
  AResult: TStringArray;
begin
  AdvFunc.SplitStr('Das ist blubb. Mir gehts blubbblubb blubb @blubb', 'blubb', AResult);
  AssertEquals(6, Length(AResult));
  AssertEquals('Das ist ', AResult[0]);
  AssertEquals('. Mir gehts ', AResult[1]);
  AssertEquals('', AResult[2]);
  AssertEquals(' ', AResult[3]);
  AssertEquals(' @', AResult[4]);
  AssertEquals('', AResult[5]);
end;

initialization
  RegisterTest(TAdvFuncTest);
end.

