unit InfoMsg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, LinkedList, Enumerators;

type
  TInfoMsg      = class
  strict private
    FMsg       : string;
    FMsgType   : Word;
    FEnumerator: TEnumerator;
    FOnDestroy : TNotifyEvent;
    FOnProgress: TNotifyEvent;
  strict protected
    function IsFixed: Boolean; virtual; abstract;
    function IsHasProgress: Boolean; virtual; abstract;
    function IsHasTimeout: Boolean; virtual; abstract;
    function GetProgress: Word; virtual; abstract;
    function GetProgressString: string; virtual; abstract;
    procedure DoInit; virtual;
  public
    constructor Create(AMsg: string; AMsgType: Word);
    destructor Destroy; override;
    property Msg: string read FMsg;
    property MsgType: Word read FMsgType;
    property Progress: Word read GetProgress;
    property ProgressString: string read GetProgressString;
    property Fixed: Boolean read IsFixed;
    property HasProgress: Boolean read IsHasProgress;
    property HasTimeout: Boolean read IsHasTimeout;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TInfoMsgQueue = class
  strict private
    FMsgs         : TLinkedList;
    FLock         : TMultiReadExclusiveWriteSynchronizer;
    FOnMsgsChanged: TNotifyEvent;
  private
    function AddMsg(AMsg: TInfoMsg): TEnumerator;
    procedure RemoveMsg(AEnumerator: TEnumerator);
  public
    constructor Create;
    destructor Destroy; override;
    function IterateMsgs(var AIterator: Pointer): TInfoMsg;
    property OnMsgsChanged: TNotifyEvent read FOnMsgsChanged write FOnMsgsChanged;
  end;

function InfoMsgQueue: TInfoMsgQueue;

implementation

{%REGION Misc}

var
  LInfoMsgQueue: TInfoMsgQueue = nil;

function InfoMsgQueue: TInfoMsgQueue;
begin
  Assert(LInfoMsgQueue <> nil);
  Result:=LInfoMsgQueue;
end;

{%ENDREGION}
{%REGION TInfoMsg}

constructor TInfoMsg.Create(AMsg: string; AMsgType: Word);
begin
  inherited Create;
  FMsg:=AMsg;
  FMsgType:=AMsgType;
  DoInit;
  FEnumerator:=LInfoMsgQueue.AddMsg(Self);
end;

destructor TInfoMsg.Destroy;
begin
  LInfoMsgQueue.RemoveMsg(FEnumerator);
  if Assigned(FOnDestroy)
    then FOnDestroy(Self);
  inherited Destroy;
end;

procedure TInfoMsg.DoInit;
begin
  //do nothing
end;

{%ENDREGION}
{%REGION TInfoMsgQueue}

constructor TInfoMsgQueue.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FMsgs:=TLinkedList.Create;
end;

destructor TInfoMsgQueue.Destroy;
begin
  Assert(FMsgs.Count = 0);
  FMsgs.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

function TInfoMsgQueue.AddMsg(AMsg: TInfoMsg): TEnumerator;
begin
  Assert(AMsg <> nil);

  FLock.Beginwrite;
  Result:=FMsgs.AddAt(AMsg);
  FLock.Endwrite;

  Assert(Result <> nil);
  if Assigned(FOnMsgsChanged)
    then FOnMsgsChanged(Self);
end;

procedure TInfoMsgQueue.RemoveMsg(AEnumerator: TEnumerator);
begin
  Assert(AEnumerator <> nil);

  FLock.Beginwrite;
  AEnumerator.Remove;
  FLock.Endwrite;

  if Assigned(FOnMsgsChanged)
    then FOnMsgsChanged(Self);
end;

function TInfoMsgQueue.IterateMsgs(var AIterator: Pointer): TInfoMsg;
begin
  if AIterator = nil then begin
    FLock.Beginread;
    AIterator:=FMsgs.Enumerator;
  end;
  with TEnumerator(AIterator) do begin
    if not MoveNext then begin
      Result:=nil;
      AIterator:=nil;
      FLock.Endread;
    end else Result:=TInfoMsg(Current);
  end;
end;

{%ENDREGION}

initialization
  LInfoMsgQueue:=TInfoMsgQueue.Create;
finalization
  LInfoMsgQueue.Destroy;
end.

