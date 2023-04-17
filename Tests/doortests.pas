unit DoorTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Doors, DoorUtils,
  DoorUnit;

type
  TDoorTest= class(TTestCase)
  protected
    FDoorways: array [0..9] of IDoorway;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEnter;
    procedure TestEnterTwice;
    procedure TestEnterDifferent;
    procedure TestEnterDifferentTwice;
    procedure TestEnterAll;
    procedure TestEnterAllDifferentVeryOften;
    procedure TestTooManyDoors;
    procedure TestManyDoors;
  end;

implementation

procedure TDoorTest.SetUp;
var
  I: Integer;
begin
  DoorManager.Reset;
  for I:=0 to Length(FDoorways)-1
    do FDoorways[I]:=DoorManager.NewDoorway;
end;

procedure TDoorTest.TearDown;
var
  I: Integer;
begin
  for I:=0 to Length(FDoorways)-1
    do FDoorways[I]:=nil;
end;

procedure TDoorTest.TestEnter;
begin
  AssertTrue('first entering not successfull', FDoorways[0].Enter(DoorManager.NewEntryCode));
end;

procedure TDoorTest.TestEnterTwice;
var
  AEntryCode: TEntryCode;
begin
  AEntryCode:=DoorManager.NewEntryCode;
  AssertTrue('first entering not successfull', FDoorways[0].Enter(AEntryCode));
  AssertFalse('second entering successfull', FDoorways[0].Enter(AEntryCode));
  AssertFalse('third entering successfull', FDoorways[0].Enter(AEntryCode));
end;

procedure TDoorTest.TestEnterDifferent;
var
  I         : Integer;
begin
  for I:=0 to 20000
    do AssertTrue('first entering not successfull in iteration ' + IntToStr(I), FDoorways[0].Enter(DoorManager.NewEntryCode));
end;

procedure TDoorTest.TestEnterDifferentTwice;
var
  I         : Integer;
  AEntryCode: TEntryCode;
begin
  for I:=0 to 20000 do begin
    AEntryCode:=DoorManager.NewEntryCode;
    AssertTrue('first entering not successfull in iteration ' + IntToStr(I), FDoorways[0].Enter(AEntryCode));
    AssertFalse('second entering successfull in iteration ' + IntToStr(I), FDoorways[0].Enter(AEntryCode));
    AssertFalse('third entering successfull in iteration ' + IntToStr(I), FDoorways[0].Enter(AEntryCode));
  end;
end;

procedure TDoorTest.TestEnterAll;
var
  I         : Integer;
  AEntryCode: TEntryCode;
begin
  AEntryCode:=DoorManager.NewEntryCode;
  for I:=0 to Length(FDoorways)-1 do begin
    AssertTrue('first entering not successfull in iteration ' + IntToStr(I), FDoorways[I].Enter(AEntryCode));
    AssertFalse('second entering successfull in iteration ' + IntToStr(I), FDoorways[I].Enter(AEntryCode));
    AssertFalse('third entering successfull in iteration ' + IntToStr(I), FDoorways[I].Enter(AEntryCode));
  end;
end;

procedure TDoorTest.TestEnterAllDifferentVeryOften;
var
  I, J      : Integer;
  AEntryCode: TEntryCode;
begin
  for I:=0 to 200000 do begin
    AEntryCode:=DoorManager.NewEntryCode;
    for J:=0 to Length(FDoorways)-1 do begin
      AssertTrue('first entering not successfull in iteration ' + IntToStr(J), FDoorways[J].Enter(AEntryCode));
      AssertFalse('second entering successfull in iteration ' + IntToStr(J), FDoorways[J].Enter(AEntryCode));
      AssertFalse('third entering successfull in iteration ' + IntToStr(J), FDoorways[J].Enter(AEntryCode));
    end;
  end;
end;

procedure TDoorTest.TestTooManyDoors;
var
  I           : Integer;
  FHoldedDoors: array [0..DoorCount-1] of TEntryCode;
begin
  for I:=0 to DoorCount-1
    do FHoldedDoors[I]:=DoorManager.NewEntryCode;
  try
    DoorManager.NewEntryCode;
    Fail('Got more entry codes than doors available');
  except
    on E: EOutOfDoors do ;
    on E: Exception do Fail('EOutOfDoors exception expected, but got ' + E.ClassName);
  end;
end;

procedure TDoorTest.TestManyDoors;
var
  I: Integer;
begin
  for I:=0 to DoorCount //NOT -1 !!!
    do DoorManager.NewEntryCode;
end;

initialization

  RegisterTest(TDoorTest);
end.

