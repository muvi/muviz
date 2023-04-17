unit ArrayFuncTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, AdvFunc;

type

  TArrayFuncTest    = class(TTestCase)
  published
    procedure TestInsertInAscendingTwoNumbersCorrectOrder;
    procedure TestInsertInAscendingTwoNumbersIncorrectOrder;
    procedure TestInsertInDescendingTwoNumbersCorrectOrder;
    procedure TestInsertInDescendingTwoNumbersIncorrectOrder;
    procedure TestRemoveFromAscendingTwoNumbersLast;
    procedure TestRemoveFromAscendingTwoNumbersFirst;
    procedure TestRemoveFromDescendingTwoNumbersLast;
    procedure TestRemoveFromDescendingTwoNumbersFirst;
  end;

  TIntegerArrayFunc = specialize TGlobalArrayFunc<Integer>;

implementation

procedure TArrayFuncTest.TestInsertInAscendingTwoNumbersCorrectOrder;
var
  AArray: array of Integer;
begin
  TIntegerArrayFunc.InsertInAscending(AArray, 1);
  TIntegerArrayFunc.InsertInAscending(AArray, 2);
  AssertEquals(2, Length(AArray));
  AssertEquals(1, AArray[0]);
  AssertEquals(2, AArray[1]);
end;

procedure TArrayFuncTest.TestInsertInAscendingTwoNumbersIncorrectOrder;
var
  AArray: array of Integer;
begin
  TIntegerArrayFunc.InsertInAscending(AArray, 2);
  TIntegerArrayFunc.InsertInAscending(AArray, 1);
  AssertEquals(2, Length(AArray));
  AssertEquals(1, AArray[0]);
  AssertEquals(2, AArray[1]);
end;

procedure TArrayFuncTest.TestInsertInDescendingTwoNumbersCorrectOrder;
var
  AArray: array of Integer;
begin
  TIntegerArrayFunc.InsertInDescending(AArray, 2);
  TIntegerArrayFunc.InsertInDescending(AArray, 1);
  AssertEquals(2, Length(AArray));
  AssertEquals(2, AArray[0]);
  AssertEquals(1, AArray[1]);
end;

procedure TArrayFuncTest.TestInsertInDescendingTwoNumbersIncorrectOrder;
var
  AArray: array of Integer;
begin
  TIntegerArrayFunc.InsertInDescending(AArray, 1);
  TIntegerArrayFunc.InsertInDescending(AArray, 2);
  AssertEquals(2, Length(AArray));
  AssertEquals(2, AArray[0]);
  AssertEquals(1, AArray[1]);
end;

procedure TArrayFuncTest.TestRemoveFromAscendingTwoNumbersLast;
var
  AArray: array of Integer;
begin
  SetLength(AArray, 2);
  AArray[0]:=1;
  AArray[1]:=2;
  TIntegerArrayFunc.RemoveFromAscending(AArray, 2);
  AssertEquals(1, Length(AArray));
  AssertEquals(1, AArray[0]);
end;

procedure TArrayFuncTest.TestRemoveFromAscendingTwoNumbersFirst;
var
  AArray: array of Integer;
begin
  SetLength(AArray, 2);
  AArray[0]:=1;
  AArray[1]:=2;
  TIntegerArrayFunc.RemoveFromAscending(AArray, 1);
  AssertEquals(1, Length(AArray));
  AssertEquals(2, AArray[0]);
end;

procedure TArrayFuncTest.TestRemoveFromDescendingTwoNumbersLast;
var
  AArray: array of Integer;
begin
  SetLength(AArray, 2);
  AArray[0]:=2;
  AArray[1]:=1;
  TIntegerArrayFunc.RemoveFromDescending(AArray, 1);
  AssertEquals(1, Length(AArray));
  AssertEquals(2, AArray[0]);
end;

procedure TArrayFuncTest.TestRemoveFromDescendingTwoNumbersFirst;
var
  AArray: array of Integer;
begin
  SetLength(AArray, 2);
  AArray[0]:=2;
  AArray[1]:=1;
  TIntegerArrayFunc.RemoveFromDescending(AArray, 2);
  AssertEquals(1, Length(AArray));
  AssertEquals(1, AArray[0]);
end;

initialization
  RegisterTest(TArrayFuncTest);
end.

