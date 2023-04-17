unit InitializerList;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils;

type
  TInitializerDependencies = Cardinal;

const
  MAXINITIALIZERID = (SizeOf(Cardinal) * 8)-1;

type
  TInitializerID           = 0..MAXINITIALIZERID;
  TInitializerProc         = procedure;
  TInitializer             = class
  strict private
    FID          : TInitializerID;
    FInit        : TInitializerProc;
    FDone        : TInitializerProc;
    FDependencies: TInitializerDependencies;
  private
    procedure FullfillDependencies(ADependencies: TInitializerDependencies);
  public
    constructor Create(AID: TInitializerID; AInit, ADone: TInitializerProc; ADependencies: TInitializerDependencies);
    destructor Destroy;
    property Dependencies: TInitializerDependencies read FDependencies;
  end;

implementation

{%REGION TInitializerList}

type
  TInitializerList = class
  strict private
    FInitialized           : array of TInitializer;
    FWaiting               : array of TInitializer;
    FFullfilledDependencies: TInitializerDependencies;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AInitializer: TInitializer);
    procedure DependenciesFullfilled(AInitializer: TInitializer);
  end;

constructor TInitializerList.Create;
begin
  inherited Create;
  FFullfilledDependencies:=0;
end;

destructor TInitializerList.Destroy;
var
  I: Integer;
begin
  Assert(Length(FWaiting) = 0);
  for I:=Length(FInitialized)-1 downto 0
    do FInitialized[I].Destroy;
  SetLength(FInitialized, 0);
  inherited Destroy;
end;

procedure TInitializerList.Add(AInitializer: TInitializer);
var
  L: Integer;
begin
  L:=Length(FWaiting);
  SetLength(FWaiting, L+1);
  FWaiting[L]:=AInitializer;
  AInitializer.FullfillDependencies(FFullfilledDependencies);
end;

procedure TInitializerList.DependenciesFullfilled(AInitializer: TInitializer);
var
  I, L: Integer;
begin
  //add AInitializer to FInitialized
  I:=Length(FInitialized);
  SetLength(FInitialized, I+1);
  FInitialized[I]:=AInitializer;
  //update dependencies
  FFullfilledDependencies:=FFullfilledDependencies or AInitializer.Dependencies;
  //search for AInitializer
  Assert(Length(FWaiting) > 0);
  I:=0;
  while FWaiting[I] <> AInitializer do begin
    FWaiting[I].FullfillDependencies(AInitializer.Dependencies);
    Inc(I);
    Assert(I < Length(FWaiting));
  end;
  //
  Inc(I);
  L:=Length(FWaiting);
  while I < L do begin
    FWaiting[I-1]:=FWaiting[I];
    FWaiting[I-1].FullfillDependencies(AInitializer.Dependencies);
    Inc(I);
  end;
  SetLength(FWaiting, L-1);
end;

var
  LInitializerList: TInitializerList = nil;

{%ENDREGION}
{%REGION TInitializer}

constructor TInitializer.Create(AID: TInitializerID; AInit, ADone: TInitializerProc; ADependencies: TInitializerDependencies);
begin
  inherited Create;
  FID:=AID;
  FInit:=AInit;
  FDone:=ADone;
  FDependencies:=ADependencies;
  LInitializerList.Add(Self);
end;

destructor TInitializer.Destroy;
begin
  FDone;
  inherited Destroy;
end;

procedure TInitializer.FullfillDependencies(ADependencies: TInitializerDependencies);
begin
  FDependencies:=FDependencies and (not ADependencies);
  if FDependencies = 0 then begin
    FInit;
    LInitializerList.DependenciesFullfilled(Self);
  end;
end;

{%ENDREGION}
{%REGION Misc}

{%ENDREGION}

initialization
  LInitializerList:=TInitializerList.Create;
finalization
  LInitializerList.Destroy;
end.

