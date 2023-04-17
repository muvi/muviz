unit Delegates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDelegateParams = TObject;

  TDelegateMethod = class
  public
    procedure Execute(AParams: TDelegateParams); virtual; abstract;
  end;

  TDelegate       = class
  strict protected
    //unsorted array of notifications.
    //sorting will have no benefit because of reordering after removal
    FMethods: array of TDelegateMethod;
    procedure Clear; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AMethod: TDelegateMethod); virtual;
    function Remove(AMethod: TDelegateMethod): Boolean; inline;
    function RemoveWeak(AMethod: TDelegateMethod): Boolean; virtual;
    procedure Execute(AParams: TDelegateParams; AFreeParams: Boolean = true); virtual;
  end;

  TLockedDelegate = class (TDelegate)
  strict private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AMethod: TDelegateMethod); override;
    function RemoveWeak(AMethod: TDelegateMethod): Boolean; override;
    procedure Execute(AParams: TDelegateParams; AFreeParams: Boolean = true); override;
  end;

implementation

{%REGION TDelegate}

constructor TDelegate.Create;
begin
  inherited Create;
end;

destructor TDelegate.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDelegate.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(FMethods)-1
    do FMethods[I].Destroy;
  SetLength(FMethods, 0);
end;

procedure TDelegate.Add(AMethod: TDelegateMethod);
var
  L: Integer;
begin
  L:=Length(FMethods);
  SetLength(FMethods, L+1);
  FMethods[L]:=AMethod;
end;

function TDelegate.RemoveWeak(AMethod: TDelegateMethod): Boolean;
var
  I,L        : Integer;
  ATmp, ATmp2: TDelegateMethod;
begin
  L:=Length(FMethods)-1;
  I:=L;
  while I>=0 do begin
    //swap with last
    ATmp:=FMethods[I];
    FMethods[I]:=ATmp2;
    ATmp2:=ATmp;
    //check if ready
    if ATmp.Equals(AMethod) then begin
      SetLength(FMethods, L);
      Result:=true;
      ATmp.Destroy;
      exit;
    end;
    Dec(I);
  end;
  //if element not found:
  //reinsert first element at last position
  FMethods[L]:=ATmp2;
  Result:=false;
end;

function TDelegate.Remove(AMethod: TDelegateMethod): Boolean; inline;
begin
  Result:=RemoveWeak(AMethod);
  AMethod.Destroy;
end;

procedure TDelegate.Execute(AParams: TDelegateParams; AFreeParams: Boolean = true);
var
  I: Integer;
begin
  for I:=0 to Length(FMethods)-1
    do FMethods[I].Execute(AParams);
  if AFreeParams
    then AParams.Destroy;
end;

{%ENDREGION}
{%REGION TLockedDelegate}

constructor TLockedDelegate.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TLockedDelegate.Destroy;
begin
  FLock.Destroy;
  inherited Destroy;
end;

procedure TLockedDelegate.Add(AMethod: TDelegateMethod);
begin
  FLock.Beginwrite;
  inherited Add(AMethod);
  FLock.Endwrite;
end;

function TLockedDelegate.RemoveWeak(AMethod: TDelegateMethod): Boolean;
begin
  FLock.Beginwrite;
  Result:=inherited RemoveWeak(AMethod);
  FLock.Endwrite;
end;

procedure TLockedDelegate.Execute(AParams: TDelegateParams; AFreeParams: Boolean = true);
begin
  FLock.Beginread;
  inherited Execute(AParams, AFreeParams);
  FLock.Endread;
end;

{%ENDREGION}

end.

