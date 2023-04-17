unit ParamSorter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, VisualisationParamOrder, AdvFunc;

type
  TVisualisationParamSorter     = class;

  TVisualisationParamSorterItem = class
  strict private
    FOwner         : TVisualisationParamSorter;
    FParam         : IPParam;
    procedure SetOrder(AOrder: Integer);
  private
    FIndex         : Integer;
    FOrder         : Integer;
    FOrderInterface: IVisualisationParamOrder;
    property Order: Integer read FOrder write SetOrder;
    property Owner: TVisualisationParamSorter read FOwner;
  public
    constructor Create(AOwner: TVisualisationParamSorter; AParam: IPParam);
    destructor Destroy;
    property Param: IPParam read FParam;
  end;

  TVisualisationParamSorterItems= array of TVisualisationParamSorterItem;

  TVisualisationParamSorter     = class
  strict private
    FVisualisation: IPVisualisation;
    FParams       : TVisualisationParamSorterItems;
    FThread       : IPThread;
    FOnChange     : TNotifyEvent;
    function GetCount: Integer; inline;
    function GetParam(AIndex: Integer): IPParam; inline;
  protected
    procedure OrderChanged(AItem: TVisualisationParamSorterItem);
    procedure Added(AItem: TVisualisationParamSorterItem);
    procedure Deleted(AItem: TVisualisationParamSorterItem);
    procedure UpdateIndex(AIndex: Integer); inline;
    property Thread: IPThread read FThread;
  public
    constructor Create(AVisualisation: IPVisualisation; AThread: IPThread);
    destructor Destroy;
    function OrderAtIndex(AIndex: Integer): Integer;
    procedure SetIndex(AParam: IPParam; AIndex: Integer);

    property Count: Integer read GetCount;
    property Params[AIndex: Integer]: IPParam read GetParam; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{%REGION Misc}

operator < (m1, m2: TVisualisationParamSorterItem): Boolean; inline;
var
  S1, S2: string;
begin
  if m1.Order = m2.Order then begin
    S1:=m1.Param.ID.Name.PasStr;
    S2:=m2.Param.ID.Name.PasStr;
    if S1 = S2
      then Result:=m1.Param.ID.&Type < m1.Param.ID.&Type
      else Result:=S1 < S2;
  end else Result:=m1.Order < m2.Order;
end;

function ParamSorterItemSmaller(const V1,V2: TVisualisationParamSorterItem): Boolean;
begin
  Result:=V1 < V2;
end;

type
  TParamSorterAdvFunc = specialize TAdvFunc<TVisualisationParamSorterItems, TVisualisationParamSorterItem>;

{%ENDREGION}
{%REGION TVisualisationParamSorterItem}

procedure OrderChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AContextClass: TVisualisationParamSorterItem;
begin
  Assert(Context <> nil);
  AContextClass:=TVisualisationParamSorterItem(Context);
  with AContextClass do begin
    Assert(Sender = Param);
    Assert(FOrderInterface <> nil);
    FOrder:=FOrderInterface.OrderInversePriority;
    Owner.OrderChanged(AContextClass);
  end;
end;

constructor TVisualisationParamSorterItem.Create(AOwner: TVisualisationParamSorter; AParam: IPParam);
var
  AOrderInterface: IInterface;
begin
  inherited Create;
  FOwner:=AOwner;
  FParam:=AParam;
  FIndex:=-1;

  AOrderInterface:=FParam.AttachedInterfaces[GUIDVISPARAMORDER];
  if AOrderInterface = nil then begin
    FOrder:=0;
    FOrderInterface:=nil
  end else begin
    FOrderInterface:=IVisualisationParamOrder(AOrderInterface);
    with FOrderInterface do begin
      FOrder:=OrderInversePriority;
      AddListener(@OrderChanged, Self, AOwner.Thread);
    end;
  end;
end;

destructor TVisualisationParamSorterItem.Destroy;
begin
  Assert(FParam <> nil);
  if FOrderInterface <> nil
    then FOrderInterface.RemoveListener(@OrderChanged, Self);
  FOrderInterface:=nil;
  FParam:=nil;
  inherited Destroy;
end;

procedure TVisualisationParamSorterItem.SetOrder(AOrder: Integer);
begin
  if FOrderInterface <> nil then begin
    FOrder:=AOrder;
    FOrderInterface.OrderInversePriority:=AOrder;
  end;
end;

{%ENDREGION}
{%REGION TVisualisationParamSorter}

procedure VisualisationGotInput(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AContextClass: TVisualisationParamSorter;
begin
  Assert(Context <> nil);
  Assert(Sender <> nil);
  AContextClass:=TVisualisationParamSorter(Context);

  AContextClass.Added(TVisualisationParamSorterItem.Create(AContextClass, IPParam(Sender)));
end;

constructor TVisualisationParamSorter.Create(AVisualisation: IPVisualisation; AThread: IPThread);
var
  I        : Integer;
  AIterator: Pointer;
  AParam   : IPParam;
begin
  Assert(AThread <> nil);
  Assert(AVisualisation <> nil);
  inherited Create;
  FThread:=AThread;
  FVisualisation:=AVisualisation;
  //copy from visualisation
  I:=0;
  AIterator:=nil;
  AParam:=AVisualisation.IterateInput(AIterator);
  while AParam <> nil do begin
    SetLength(FParams, I+1);
    FParams[I]:=TVisualisationParamSorterItem.Create(Self, AParam);
    Inc(I);
    AParam:=AVisualisation.IterateInput(AIterator);
  end;
  AVisualisation.AddInputGotListener(@VisualisationGotInput, Self, AThread);
  //sort
  TParamSorterAdvFunc.Quicksort(FParams, @ParamSorterItemSmaller, Length(FParams)-1);
  //set indices
  for I:=0 to Length(FParams)-1
    do FParams[I].FIndex:=I;
end;

destructor TVisualisationParamSorter.Destroy;
var
  I: Integer;
begin
  FVisualisation.RemoveInputGotListener(@VisualisationGotInput, Self);
  for I:=0 to Length(FParams)-1
    do FParams[I].Destroy;
  SetLength(FParams, 0);
  FVisualisation:=nil;
  FThread:=nil;
  inherited Destroy;
end;

procedure TVisualisationParamSorter.UpdateIndex(AIndex: Integer); inline;
begin
  FParams[AIndex].FIndex:=AIndex;
end;

procedure TVisualisationParamSorter.OrderChanged(AItem: TVisualisationParamSorterItem);
var
  I, L: Integer;
begin
  L:=Length(FParams);
  Assert(AItem.FIndex >= 0);
  Assert(AItem.FIndex < L);
  I:=AItem.FIndex;
  //move above
  if I >= 1 then while AItem < FParams[I-1] do begin
    Assert(FParams[I] = AItem);
    TParamSorterAdvFunc.Swap(FParams[I-1], FParams[I]);
    UpdateIndex(I);
    Dec(I);
    if I < 1
      then break;
  end;
  //move below
  if I < L-1 then while FParams[I+1] < AItem do begin
    Assert(FParams[I] = AItem);
    TParamSorterAdvFunc.Swap(FParams[I+1], FParams[I]);
    UpdateIndex(I);
    Inc(I);
    if I >= L-1
      then break;
  end;
  //set index
  Assert(FParams[I] = AItem);
  UpdateIndex(I);

  if Assigned(FOnChange)
    then FOnChange(Self);
end;

function TVisualisationParamSorter.OrderAtIndex(AIndex: Integer): Integer;
type
  TModifiedItem = record
    Item    : TVisualisationParamSorterItem;
    NewOrder: LongInt;
  end;
var
  I                      : Integer;
  ATopOrder, ABottomOrder: Int64;
  AModified              : array of TModifiedItem;

  function CheckedOrder(AIndex: Integer): Int64;
  begin
    if AIndex < 0
      then Result:=not MaxInt
      else if AIndex >= Length(FParams)
        then Result:=MaxInt
        else Result:=FParams[AIndex].Order;
  end;

  function CalcNewOrder: Integer;
  begin
    Result:=(ATopOrder + ABottomOrder) div Int64(2);
    Assert(Result >= ATopOrder);
    if Result = ATopOrder
      then Inc(Result);
  end;

begin
  Assert(AIndex >= 0);
  Assert(AIndex <= Length(FParams));

  //generate new orders
  ATopOrder:=CheckedOrder(AIndex - 1);
  ABottomOrder:=CheckedOrder(AIndex);

  Result:=CalcNewOrder;
  ATopOrder:=Result;

  I:=0;
  while (ATopOrder >= ABottomOrder) and (ATopOrder < MaxInt) do begin
    SetLength(AModified, I + 1);
    //generate order to insert ACurrentItem at NewIndex + I
    with AModified[I] do begin
      Item:=FParams[AIndex + I];
      ABottomOrder:=CheckedOrder(AIndex + I + 1);

      NewOrder:=CalcNewOrder;
      ATopOrder:=NewOrder;
    end;
    Inc(I);
  end;

  //set new orders
  for I:=0 to Length(AModified)-1
    do with AModified[I]
      do Item.Order:=NewOrder;

  //cleanup
  SetLength(AModified, 0);
end;

procedure TVisualisationParamSorter.SetIndex(AParam: IPParam; AIndex: Integer);
var
  AOrderInterface: IInterface;
begin
  AOrderInterface:=AParam.AttachedInterfaces[GUIDVISPARAMORDER];
  if AOrderInterface <> nil
    then IVisualisationParamOrder(AOrderInterface).OrderInversePriority:=OrderAtIndex(AIndex);
end;

{
procedure TVisualisationParamSorter.IndexChanged(AItem: TVisualisationParamSorterItem; ANewIndex: Integer);
type
  TModifiedItem = record
    Item    : TVisualisationParamSorterItem;
    NewOrder: LongInt;
  end;
var
  I, ATopOrder, ABottomOrder: Integer;
  AModified   : array of TModifiedItem;
  ACurrentItem: TVisualisationParamSorterItem;

  function CheckedOrder(AIndex: Integer): Int64;
  begin
    if AIndex < 0
      then Result:=not MaxInt
      else if AIndex >= Length(FParams)
        then Result:=MaxInt
        else Result:=FParams[AIndex].Order;
  end;

begin
  Assert(ANewIndex >= 0);
  Assert(ANewIndex < Length(FParams));

  //generate new orders
  I:=0;
  ATopOrder:=CheckedOrder(ANewIndex - 1);
  ACurrentItem:=AItem;
  repeat
    SetLength(AModified, I+1);
    //generate order to insert ACurrentItem at NewIndex + I
    with AModified[I] do begin
      Item:=ACurrentItem;
      ABottomOrder:=CheckedOrder(ANewIndex + I);

      NewOrder:=(ATopOrder + ABottomOrder) div Int64(2);
      Assert(NewOrder >= ATopOrder);
      if NewOrder = ATopOrder
        then Inc(NewOrder);
      ATopOrder:=NewOrder;
    end;

    if I < Length(FParams)
      then ACurrentItem:=FParams[I]
      else Assert((ATopOrder < ABottomOrder) or (ATopOrder = MaxInt));
    Inc(I);
  until (ATopOrder < ABottomOrder) or (ATopOrder = MaxInt);

  //set new orders
  for I:=0 to Length(AModified)-1
    do with AModified[I]
      do Item.Order:=NewOrder;

  //cleanup
  SetLength(AModified, 0);
end;
}

procedure TVisualisationParamSorter.Added(AItem: TVisualisationParamSorterItem);
var
  I, L: Integer;
begin
  L:=Length(FParams);
  SetLength(FParams, L+1);
  //search position
  I:=L;
  if I>0 then while AItem < FParams[I-1] do begin
    FParams[I]:=FParams[I-1];
    UpdateIndex(I);
    Dec(I);
    if I<=0
      then break;
  end;
  //insert item
  FParams[I]:=AItem;
  UpdateIndex(I);

  if Assigned(FOnChange)
    then FOnChange(Self);
end;

procedure TVisualisationParamSorter.Deleted(AItem: TVisualisationParamSorterItem);
begin
  //not implemented
  Assert(false);
end;

function TVisualisationParamSorter.GetCount: Integer; inline;
begin
  Result:=Length(FParams);
end;

function TVisualisationParamSorter.GetParam(AIndex: Integer): IPParam; inline;
begin
  Assert(AIndex < Length(FParams));
  Result:=FParams[AIndex].Param;
end;

{%ENDREGION}

end.

