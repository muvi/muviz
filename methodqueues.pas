unit MethodQueues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Delegates;

type
  EMethodQueueException   = class (Exception)
  end;

  TMethodQueue            = class (TDelegate)
  strict private
    //do not save the execution params because they might be nil by default
    FIsExecuting: Boolean;
  public
    constructor Create;
    function RemoveWeak(AMethod: TDelegateMethod): Boolean; override;
    procedure Execute(AParams: TDelegateParams; AFreeParams: Boolean = true); override;
  end;

  TMethodQueueHelper      = class helper for TDelegate
  public
    procedure AddAndExecute(AMethod: TDelegateMethod; AParams: TDelegateParams; AFreeParams: Boolean = true); inline;
  end;

  //a delegate method which executes a delegate
  TDelegateDelegateMethod = class (TDelegateMethod)
  strict private
    FDelegate: TDelegate;
    FParams  : TDelegateParams;
  public
    constructor Create(ADelegate: TDelegate; AParams: TDelegateParams);
    destructor Destroy; override;
    procedure Execute(AParams: TDelegateParams); override;
    function Equals(Obj: TObject): Boolean; override;
  end;

implementation

{%REGION TMethodQueue}

constructor TMethodQueue.Create;
begin
  inherited Create;
  FIsExecuting:=false;
end;

function TMethodQueue.RemoveWeak(AMethod: TDelegateMethod): Boolean;
begin
  raise EMethodQueueException.Create('Method queue element can not be removed');
end;

procedure TMethodQueue.Execute(AParams: TDelegateParams; AFreeParams: Boolean = true);
var
  I    : Integer;
  AItem: TDelegateMethod;
begin
  if FIsExecuting
    then exit;
  FIsExecuting:=true;

  I:=0;
  //do not use for loop; length might change during execution
  while I < Length(FMethods) do begin
    AItem:=FMethods[I];
    AItem.Execute(AParams);
    Inc(I);
  end;
  if AFreeParams
    then AParams.Destroy;
  Clear;

  FIsExecuting:=false;
end;

{%ENDREGION}
{%REGION TMethodQueueHelper}

procedure TMethodQueueHelper.AddAndExecute(AMethod: TDelegateMethod; AParams: TDelegateParams; AFreeParams: Boolean = true); inline;
begin
  Add(AMethod);
  Execute(AParams, AFreeParams);
end;

{%ENDREGION}
{%REGION TDelegateDelegateMethod}

constructor TDelegateDelegateMethod.Create(ADelegate: TDelegate; AParams: TDelegateParams);
begin
  inherited Create;
  FDelegate:=ADelegate;
  FParams:=AParams;
end;

destructor TDelegateDelegateMethod.Destroy;
begin
  FParams.Destroy;
  inherited Destroy;
end;

function TDelegateDelegateMethod.Equals(Obj: TObject): Boolean;
begin
  Result:=(Obj is TDelegateDelegateMethod)
    and (TDelegateDelegateMethod(Obj).FDelegate = FDelegate)
    and (TDelegateDelegateMethod(Obj).FParams = FParams);
end;

procedure TDelegateDelegateMethod.Execute(AParams: TDelegateParams);
begin
  Assert(AParams = nil);
  FDelegate.Execute(FParams, false);
end;

{%ENDREGION}

end.

