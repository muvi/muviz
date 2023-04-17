unit Queue;

{$mode objfpc}{$H+}

interface

uses
  cslPushable, Enumerators;

type
  TQueue = class (TPushable)
  strict private
    procedure Grow;
  private
    FItems: array of TObject;
    FCount: Integer;
    FStart: Integer;
  strict protected
    function GetCount: Integer; override;
  public
    constructor Create(AInitialSize: Integer = 16);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator; override;
    procedure Push(AObject: TObject); override;
    function Pop: TObject; override;
    function Get: TObject; override;
  end;

implementation

{%REGION TQueueEnumerator}

type
  TQueueEnumerator = class (TEnumerator)
  strict private
    FPosition: Integer;
    FOwner   : TQueue;
  strict protected
    function GetCurrent: TObject; override;
  public
    constructor Create(AOwner: TQueue);
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

constructor TQueueEnumerator.Create(AOwner: TQueue);
begin
  inherited Create;
  FOwner:=AOwner;
  FPosition:=-1;
end;

function TQueueEnumerator.GetCurrent: TObject;
begin
  with FOwner do begin
    Assert((FPosition >= 0) and (FPosition < FCount));
    Result:=FItems[FStart + FPosition];
  end;
end;

function TQueueEnumerator.MoveNext: Boolean;
begin
  Assert(FPosition < FOwner.FCount);
  Inc(FPosition);
  Result:=FPosition < FOwner.FCount;
end;

procedure TQueueEnumerator.Reset;
begin
  FPosition:=-1;
end;

{%ENDREGION}
{%REGION TQueue}

constructor TQueue.Create(AInitialSize: Integer = 16);
begin
  inherited Create;
  SetLength(FItems, AInitialSize);
  FStart:=0;
  FCount:=0;
end;

destructor TQueue.Destroy;
begin
  //user has to handle content destruction
  Assert(FCount = 0);
  inherited Destroy;
end;

function TQueue.GetCount: Integer;
begin
  Result:=FCount;
end;

function TQueue.GetEnumerator: TEnumerator;
begin
  Result:=TQueueEnumerator.Create(Self);
end;

procedure TQueue.Grow;
var
  I, L: Integer;
begin
  L:=Length(FItems);
  SetLength(FItems, L*2);
  //shift items
  for I:=0 to FStart-1
    do FItems[I+L]:=FItems[I];
end;

procedure TQueue.Push(AObject: TObject);
begin
  Assert(FCount <= Length(FItems));
  if FCount >= Length(FItems)
    then Grow;

  FItems[(FStart + FCount) mod Length(FItems)]:=AObject;
  Inc(FCount);
end;

function TQueue.Pop: TObject;
begin
  //Queue may not be empty
  Assert(FCount > 0);
  Result:=FItems[FStart];
  FStart:=(FStart + 1) mod Length(FItems);
  Dec(FCount);
end;

function TQueue.Get: TObject;
begin
  //Queue may not be empty
  Assert(FCount > 0);
  Result:=FItems[FStart];
end;

{%ENDREGION}

end.

