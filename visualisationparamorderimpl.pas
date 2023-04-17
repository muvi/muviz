unit VisualisationParamOrderImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisualisationParamOrder, ParamNotificationList, VisType2,
  StdParamTypes, PointerBoundParamOrder;

type
  TFixedVisualisationParamOrder    = class (TInterfacedObject, IVisualisationParamOrder)
  strict protected
    FOrderInversePriority: LongInt;
    function GetOrderInversePriority: LongInt; cdecl; virtual;
    procedure SetOrderInversePriority(APriority: LongInt); cdecl; virtual;
    procedure DoSetOrderInversePriority(APriority: LongInt); virtual;
  protected
    property OrderInversePriority: LongInt read GetOrderInversePriority write DoSetOrderInversePriority;
  public
    constructor Create(AOrderInversePriority: LongInt);
    procedure AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl; virtual;
    procedure RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl; virtual;
  end;

  TChangingVisualisationParamOrder = class (TFixedVisualisationParamOrder)
  strict private
    FNotifications: TNotificationList;
    FOwner        : IPParam;
  strict protected
    procedure SetOrderInversePriority(APriority: LongInt); cdecl; override;
    function GetOrderInversePriority: LongInt; cdecl; override;
    procedure DoSetOrderInversePriority(APriority: LongInt); override;
    property Owner: IPParam read FOwner;
  public
    constructor Create(AOwner: IPParam; AOrderInversePriority: LongInt);
    destructor Destroy; override;
    procedure AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl; override;
    procedure RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl; override;
  end;

  TPointerCreator                  = function: IPPointer of object;

  TPointerBoundParamOrder          = class (TChangingVisualisationParamOrder, IPointerBoundParamOrder)
  strict private
    FBoundTo             : IPPointer;
    FBoundToLock         : TMultiReadExclusiveWriteSynchronizer;
    FDesiredPointerInput : TPointerSide;
    FDesiredPointerOutput: TPointerSide;
    FUnboundValue        : LongInt;
    FPointerCreator      : TPointerCreator;
    function GetBoundTo: IPPointer;
    procedure SetBoundTo(AValue: IPPointer);
    procedure SetBoundTo(AValue: IPPointer; ADoLock: Boolean);
  private
    procedure UpdatePriority(ADoCheckUndesired: Boolean = true);
  strict protected
    procedure SetOrderInversePriority(APriority: LongInt); cdecl; override;
  public
    constructor Create(AOwner: IPParam; ADesiredPointerInput, ADesiredPointerOutput: TPointerSide; AUnboundValue: LongInt; APointerCreator: TPointerCreator);
    destructor Destroy; override;
    property BoundTo: IPPointer read GetBoundTo write SetBoundTo;
  end;

implementation

{%REGION TFixedVisualisationParamOrder}

constructor TFixedVisualisationParamOrder.Create(AOrderInversePriority: LongInt);
begin
  inherited Create;
  FOrderInversePriority:=AOrderInversePriority;
end;

function TFixedVisualisationParamOrder.GetOrderInversePriority: LongInt; cdecl;
begin
  Result:=FOrderInversePriority;
end;

procedure TFixedVisualisationParamOrder.SetOrderInversePriority(APriority: LongInt); cdecl;
begin
  //do nothing
end;

procedure TFixedVisualisationParamOrder.DoSetOrderInversePriority(APriority: LongInt);
begin
  FOrderInversePriority:=APriority;
end;

procedure TFixedVisualisationParamOrder.AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl;
begin
  //do nothing
end;

procedure TFixedVisualisationParamOrder.RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  //do nothing
end;

{%ENDREGION}
{%REGION TChangingVisualisationParamOrder}

constructor TChangingVisualisationParamOrder.Create(AOwner: IPParam; AOrderInversePriority: LongInt);
begin
  inherited Create(AOrderInversePriority);
  //no reference counting
  Move(AOwner, FOwner, SizeOf(FOwner));
  FNotifications:=TNotificationList.Create;
end;

destructor TChangingVisualisationParamOrder.Destroy;
begin
  FNotifications.Destroy;
  //no reference counting
  FillChar(FOwner, SizeOf(FOwner), 0);
  inherited Destroy;
end;

procedure TChangingVisualisationParamOrder.SetOrderInversePriority(APriority: LongInt); cdecl;
begin
  OrderInversePriority:=APriority;
end;

function TChangingVisualisationParamOrder.GetOrderInversePriority: LongInt; cdecl;
begin
  InterLockedExchange(Result, FOrderInversePriority);
end;

procedure TChangingVisualisationParamOrder.DoSetOrderInversePriority(APriority: LongInt);
begin
  InterLockedExchange(FOrderInversePriority, APriority);
  FNotifications.Notify(FOwner, nil);
end;

procedure TChangingVisualisationParamOrder.AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
begin
  FNotifications.Add(AListener, AContext, AThread);
end;

procedure TChangingVisualisationParamOrder.RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  FNotifications.Remove(AListener, AContext);
end;

{%ENDREGION}
{%REGION TPointerBoundParamOrder}

procedure PointerChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  Assert(Context <> nil);
  TPointerBoundParamOrder(Context).UpdatePriority;
end;

constructor TPointerBoundParamOrder.Create(AOwner: IPParam; ADesiredPointerInput, ADesiredPointerOutput: TPointerSide; AUnboundValue: LongInt; APointerCreator: TPointerCreator);
begin
  Assert(Assigned(APointerCreator));
  inherited Create(AOwner, AUnboundValue);
  FBoundToLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FBoundTo:=nil;
  FUnboundValue:=AUnboundValue;
  FPointerCreator:=APointerCreator;
  FDesiredPointerInput:=ADesiredPointerInput;
  FDesiredPointerOutput:=ADesiredPointerOutput;
end;

destructor TPointerBoundParamOrder.Destroy;
begin
  FBoundToLock.Beginwrite;
  if FBoundTo <> nil
    then FBoundTo.RemoveListener(@PointerChanged, Self);
  FBoundTo:=nil;
  FBoundToLock.Endwrite;
  FBoundToLock.Destroy;
  inherited Destroy;
end;

procedure TPointerBoundParamOrder.SetOrderInversePriority(APriority: LongInt); cdecl;
var
  ANewPointer: TVPointer;
  ABoundTo   : IPPointer;
begin
  //set new pointer
  with ANewPointer do begin
    Input:=FDesiredPointerInput;
    Output:=FDesiredPointerOutput;
    InversePriority:=APriority;
  end;
  //set FBoundTo
  FBoundToLock.Beginwrite;

  if FBoundTo = nil then begin
    ABoundTo:=FPointerCreator();
    ABoundTo.Value:=ANewPointer;
    //may not bind a pointer with "undesired" input and output
    SetBoundTo(ABoundTo, false);
  end else FBoundTo.Value:=ANewPointer;
  Assert(FBoundTo <> nil);
  //do not call inherited...
  //updated in PointerChanged

  FBoundToLock.Endwrite;
end;

procedure TPointerBoundParamOrder.UpdatePriority(ADoCheckUndesired: Boolean = true);
var
  AValue: TVPointer;
begin
  FBoundToLock.Beginwrite;

  Assert(FBoundTo <> nil);
  AValue:=FBoundTo.Value;
  if (not ADoCheckUndesired) or ((AValue.Input = FDesiredPointerInput) and (AValue.Output = FDesiredPointerOutput))
    then OrderInversePriority:=AValue.InversePriority
    else SetBoundTo(nil, false);

  FBoundToLock.Endwrite;
end;

function TPointerBoundParamOrder.GetBoundTo: IPPointer;
begin
  FBoundToLock.Beginread;
  Result:=FBoundTo;
  FBoundToLock.Endread;
end;

procedure TPointerBoundParamOrder.SetBoundTo(AValue: IPPointer; ADoLock: Boolean);
begin
  if ADoLock
    then FBoundToLock.Beginwrite;

  if FBoundTo <> AValue then begin
    if FBoundTo <> nil
      then FBoundTo.RemoveListener(@PointerChanged, Self);

    FBoundTo:=AValue;

    if FBoundTo <> nil then begin
      FBoundTo.AddListener(@PointerChanged, Self, nil);
      UpdatePriority(false);
    end else OrderInversePriority:=FUnboundValue;
  end;

  if ADoLock
    then FBoundToLock.Endwrite;
end;

procedure TPointerBoundParamOrder.SetBoundTo(AValue: IPPointer);
begin
  SetBoundTo(AValue, true);
end;

{%ENDREGION}

end.

