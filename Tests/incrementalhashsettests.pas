unit IncrementalHashSetTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, IncrementalSet,
  IncrementalHashSet, ObjectClassBasic;

type

  TIncrementalSetTest     = class(TTestCase)
  private
    FSet              : TIncrementalSet;
    FTestObjects      : array [0..9] of TObjectItem;
    FTestObjectsBackup: array [0..9] of TObjectItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetIncrementalSet: TIncrementalSet; virtual; abstract;
  published
    procedure TestAdd10Times;
    procedure TestAdd3x10Times;
    procedure TestEnumerator3x10Times;
    procedure TestDestroy3x10Times;
    procedure TestAddResult;
  end;

  TIncrementalHashSetTest = class (TIncrementalSetTest)
  protected
    function GetIncrementalSet: TIncrementalSet; override;
  end;

implementation

{%REGION TIncrementalSetTest}

procedure TIncrementalSetTest.TestAdd10Times;
var
  I: Integer;
begin
  for I:=0 to 9
    do FSet.Add(FTestObjects[0]);
  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[0]));
end;

procedure TIncrementalSetTest.TestAdd3x10Times;
var
  I: Integer;
begin
  for I:=0 to 9 do begin
    FSet.Add(FTestObjects[0]);
    FSet.Add(FTestObjects[1]);
    FSet.Add(FTestObjects[2]);
  end;
  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[0]));
  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[2]));
  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[1]));
end;

procedure TIncrementalSetTest.TestEnumerator3x10Times;
var
  I      : Integer;
  AObject: TObject;
begin
  for I:=0 to 9 do begin
    FSet.Add(FTestObjects[0]);
    FSet.Add(FTestObjects[1]);
    FSet.Add(FTestObjects[2]);
  end;

  I:=0;
  for AObject in FSet do begin
    if AObject = FTestObjects[0]
      then FTestObjects[0]:=nil
      else if AObject = FTestObjects[1]
        then FTestObjects[1]:=nil
        else if AObject = FTestObjects[2]
          then FTestObjects[2]:=nil
          else Fail('unknown object');
    Inc(I);
  end;

  AssertEquals(3, I);

  FTestObjects[0]:=FTestObjectsBackup[0];
  FTestObjects[1]:=FTestObjectsBackup[1];
  FTestObjects[2]:=FTestObjectsBackup[2];

  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[0]));
  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[2]));

  I:=0;
  for AObject in FSet do begin
    AssertTrue('Objects not equal', FTestObjects[1] = AObject);
    FTestObjects[1]:=nil;
    Inc(I);
  end;

  AssertEquals(1, I);
end;

procedure TIncrementalSetTest.TestDestroy3x10Times;
var
  I      : Integer;
  AObject: TObject;
begin
  for I:=0 to 9 do begin
    FSet.Add(FTestObjects[0]);
    FSet.Add(FTestObjects[1]);
    FSet.Add(FTestObjects[2]);
  end;

  FTestObjectsBackup[1].Destroy;
  FTestObjectsBackup[1]:=TObjectItem.Create;

  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[0]));
  for I:=0 to 9
    do AssertTrue('failed in round ' + IntToStr(I), FSet.Remove(FTestObjects[2]));

  for AObject in FSet
    do Fail('set has to be empty');
end;


procedure TIncrementalSetTest.TestAddResult;
var
  I: Integer;
begin
  AssertTrue('true expected at object 0', FSet.Add(FTestObjects[0]));
  AssertTrue('true expected at object 1', FSet.Add(FTestObjects[1]));
  AssertTrue('true expected at object 2', FSet.Add(FTestObjects[2]));
  for I:=1 to 9 do begin
    AssertFalse('false expected at object 0 in round ' + IntToStr(I), FSet.Add(FTestObjects[0]));
    AssertFalse('false expected at object 1 in round ' + IntToStr(I), FSet.Add(FTestObjects[1]));
    AssertFalse('false expected at object 2 in round ' + IntToStr(I), FSet.Add(FTestObjects[2]));
  end;
end;

procedure TIncrementalSetTest.SetUp;
var
  I: Integer;
begin
  FSet:=GetIncrementalSet;
  for I:=0 to 9 do begin
    FTestObjectsBackup[I]:=TObjectItem.Create;
    FTestObjects[I]:=FTestObjectsBackup[I];
  end;
end;

procedure TIncrementalSetTest.TearDown;
var
  I: Integer;
begin
  for I:=0 to 9
    do FTestObjectsBackup[I].Destroy;
  FSet.Destroy;
end;

{%ENDREGION}
{%REGION TIncrementalHashSetTest}

function TIncrementalHashSetTest.GetIncrementalSet: TIncrementalSet;
begin
  Result:=TIncrementalHashSet.Create(false);
end;

{%ENDREGION}

initialization

  RegisterTest(TIncrementalHashSetTest);
end.

