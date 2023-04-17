unit ParamNotificationList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2;

type
  TNotification                = record
    Proc   : TPParamNotification;
    Context: Pointer;
    Thread : IPThread;
  end;

  {
  TNotificationExecutor        = class
  public
    procedure Execute(AProc: TPParamNotification; AContext: Pointer; AThread: IPThread); virtual; abstract;
  end;
  }

  TNotificationArray           = array of TNotification;

  TNotificationList            = class
  private
    //unsorted array of notifications.
    //sorting will have no benefit because of reordering after removal
    FNotifications: array of TNotification;
    FLock         : TMultiReadExclusiveWriteSynchronizer;
    //used for removal while notificating
    FIndex        : Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ANotification: TPParamNotification; AContext: Pointer; AThread: IPThread);
    function Remove(ANotification: TPParamNotification; AContext: Pointer): Boolean;
    procedure Notify(ASender, ASenderData: IInterface);
  end;

  {
  TDefaultNotificationExecutor = class (TNotificationExecutor)
  strict private
    FSender: IInterface;
  public
    constructor Create(ASender: TPParamNotificationSender);
    destructor Destroy; override;
    procedure Execute(AProc: TPParamNotification; AContext: Pointer; AThread: IPThread); override;
  end;
  }

operator = (m1, m2: TNotification): Boolean;

implementation

{%REGION TDefaultNotificationExecutor}

{
constructor TDefaultNotificationExecutor.Create(ASender: TPParamNotificationSender);
begin
  inherited Create;
  FSender:=ASender;
end;

destructor TDefaultNotificationExecutor.Destroy;
begin
  FSender:=nil;
  inherited Destroy;
end;

procedure TDefaultNotificationExecutor.Execute(AProc: TPParamNotification; AContext: Pointer; AThread: IPThread);
begin
  Assert(AThread <> nil);
  AThread.Push(AProc, AContext, FSender);
end;
}

{%ENDREGION}
//TODO: use delegates
{%REGION TNotificationList}

constructor TNotificationList.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TNotificationList.Destroy;
begin
  SetLength(FNotifications, 0);
  FLock.Destroy;
  inherited Destroy;
end;

procedure TNotificationList.Add(ANotification: TPParamNotification; AContext: Pointer; AThread: IPThread);
var
  ANewNotification: TNotification;
  L               : Integer;
begin
  with ANewNotification do begin
    Proc:=ANotification;
    Context:=AContext;
    Thread:=AThread;
  end;

  FLock.Beginwrite;
  L:=Length(FNotifications);
  SetLength(FNotifications, L+1);
  FNotifications[L]:=ANewNotification;
  FLock.Endwrite;
end;

function TNotificationList.Remove(ANotification: TPParamNotification; AContext: Pointer): Boolean;
var
  I,L                          : Integer;
  ATmp, ATmp2, AOldNotification: TNotification;
begin
  with AOldNotification do begin
    Proc:=ANotification;
    Context:=AContext;
  end;

  FLock.Beginwrite;
  L:=Length(FNotifications)-1;
  I:=L;
  while I>=0 do begin
    //swap with last
    ATmp:=FNotifications[I];
    FNotifications[I]:=ATmp2;
    ATmp2:=ATmp;
    //check if ready
    if ATmp=AOldNotification then begin
      with ATmp
        do if Thread <> nil
          then Thread.CancelAll(Proc, Context);
      SetLength(FNotifications, L);
      if FIndex >= I
        then Dec(FIndex);
      FLock.Endwrite;
      Result:=true;
      exit;
    end;
    Dec(I);
  end;
  //if element not found:
  //reinsert first element at last position
  FNotifications[L]:=ATmp2;
  FLock.Endwrite;
  Result:=false;
end;

procedure TNotificationList.Notify(ASender, ASenderData: IInterface);
var
  I            : Integer;
  ANotification: TNotification;
begin
  FLock.Beginread;
  I:=0;
  //do not save Length in L, it may change
  while I < Length(FNotifications) do begin
    ANotification:=FNotifications[I];

    FLock.Endread;

    with ANotification
      do if Thread <> nil
        then Thread.Push(Proc, Context, ASender, ASenderData)
        else Proc(Context, ASender, ASenderData);
    Inc(I);

    FLock.Beginread;
  end;
  {
  for I:=0 to Length(FNotifications)-1
    do with FNotifications[I]
      do Thread.Push(Proc, Context, ASender);
  }
  FLock.Endread;
  //AExecutor.Destroy;
end;

{%ENDREGION}
{%REGION Misc}

operator = (m1, m2: TNotification): Boolean;
begin
  //do not compare thread here
  Result:=(m1.Context = m2.Context) and (m1.Proc = m2.Proc);
end;

{%ENDREGION}

end.

