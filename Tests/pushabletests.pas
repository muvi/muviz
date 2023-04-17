unit PushableTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, cslPushable;

type

  TPushableTest= class(TTestCase)
  strict private
    FTestObject: TPushable;
    FTestItems : array [0..29] of TObject;
    procedure Fill;
  protected
    function GetTestObject: TPushable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
    property TestObject: TPushable read FTestObject;
  published
    procedure TestPush30;
    procedure TestPop30;
  end;

implementation

procedure TPushableTest.Fill;
var
  I: Integer;
begin
  AssertTrue('TestObject has to be empty', FTestObject.Empty);
  for I:=0 to 29 do begin
    FTestObject.Push(FTestItems[I]);
    AssertEquals(I+1, FTestObject.Count);
  end;
end;

procedure TPushableTest.TestPush30;
begin
  Fill;
  while not FTestObject.Empty
    do FTestObject.Pop;
end;

procedure TPushableTest.TestPop30;
var
  I    : Integer;
  AItem: TObject;
begin
  Fill;
  for I:=0 to 29 do begin
    AItem:=FTestObject.Pop;
    AssertSame(FTestItems[I], AItem);
  end;
end;

procedure TPushableTest.SetUp;
var
  I: Integer;
begin
  for I:=0 to Length(FTestItems)-1
    do FTestItems[I]:=TObject.Create;
  FTestObject:=GetTestObject;
end;

procedure TPushableTest.TearDown;
var
  I: Integer;
begin
  FTestObject.Destroy;
  for I:=0 to Length(FTestItems)-1
    do FTestItems[I].Destroy;
end;

end.

