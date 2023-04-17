unit ParamSorter2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, AdvFunc, LexCompare, MStrings, HashMap, MapKeys,
  StdParamTypes;

//does not really work, if you sort by a param which connects multiple times to the same other param

type
  TParamSorterKey                      = class
  strict private
    FID: TPParamID;
  public
    constructor Create(AID: TPParamID);
    function Equals(AOther: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property ID: TPParamID read FID;
  end;

  TVisualisationParamSorter            = class;
  TVisualisationParamSOrterPointerItem = class;

  TVisualisationParamSorterItem        = class
  strict private
    FParam         : IPParam;
    FIndex         : Integer;
    FOwner         : TVisualisationParamSorter;

    FOrder         : Integer;
    FOrdered       : Boolean;
    FOrderIssuedBy : TVisualisationParamSorterPointerItem;

    FLexedParamName: TLexedString;
  strict protected
    property Owner: TVisualisationParamSorter read FOwner;
  protected
    procedure SetConnection(ADoSort: Boolean = false); virtual;
    procedure SetOrder(AOrder: Integer; AIssuedBy: TVisualisationParamSorterPointerItem);
    procedure RemoveOrder;
    function Smaller(AThan: TVisualisationParamSorterItem): Boolean;
    property Index: Integer read FIndex write FIndex;
    property LexedParamName: TLexedString read FLexedParamName;
  public
    constructor Create(AOwner: TVisualisationParamSorter; AParam: IPParam);
    destructor Destroy; override;
    procedure RemoveBindings; virtual;
    property Order: Integer read FOrder;
    property Ordered: Boolean read FOrdered;
    property OrderIssuedBy: TVisualisationParamSorterPointerItem read FOrderIssuedBy;
    property Param: IPParam read FParam;
  end;

  TVisualisationParamSorterPointerItem = class (TVisualisationParamSorterItem)
  strict private
    FConnects: TVisualisationParamSorterItem;
    procedure RemoveConnection(ADoSort: Boolean = false);
  protected
    procedure SetConnection(ADoSort: Boolean = false); override;
  public
    constructor Create(AOwner: TVisualisationParamSorter; AParam: IPParam);
    destructor Destroy; override;
    //to remove the connection, if the whole list is being destroyed
    procedure RemoveBindings; override;
  end;

  TVisualisationParamSorterItems       = array of TVisualisationParamSorterItem;

  TVisualisationParamSorter            = class
  strict private
    FVisualisation    : IPVisualisation;
    FParams           : TVisualisationParamSorterItems;
    FParamMap         : THashMap;
    FConnectedCount   : Integer;
    FThread           : IPThread;
    FSortedBy         : IPParam;
    FOnChange         : TNotifyEvent;
    function GetCount: Integer; inline;
    function GetParam(AIndex: Integer): IPParam; inline;
    procedure SetSortedBy(ASortedBy: IPParam);
    procedure SortAll;
  strict protected
    procedure UpdateIndex(AIndex: Integer); inline;
    property ParamMap: THashMap read FParamMap;
    property Items: TVisualisationParamSorterItems read FParams;
    property Thread: IPThread read FThread;
    property Visualisation: IPVisualisation read FVisualisation;
  protected
    function GetParam(AID: TPParamID): TVisualisationParamSorterItem;
    procedure Added(AItem: TVisualisationParamSorterItem);
    procedure Deleted(AItem: TVisualisationParamSorterItem);
    procedure OrderChanged(AItem: TVisualisationParamSorterItem);
    procedure ConnectionsIncreased;
    procedure ConnectionsDecreased;
  public
    constructor Create(AVisualisation: IPVisualisation; AThread: IPThread; ASortedBy: IPParam = nil);
    destructor Destroy; override;

    property SortedBy: IPParam read FSortedBy write SetSortedBy;
    property ConnectedCount: Integer read FConnectedCount;
    property Count: Integer read GetCount;
    property Params[AIndex: Integer]: IPParam read GetParam; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{%REGION Misc}

operator < (m1, m2: TVisualisationParamSorterItem): Boolean; inline;
begin
  Result:=m1.Smaller(m2);
end;

function ParamSorterItemSmaller(const V1,V2: TVisualisationParamSorterItem): Boolean;
begin
  Result:=V1 < V2;
end;

function CreateItem(AOwner: TVisualisationParamSorter; AParam: IPParam): TVisualisationParamSorterItem;
begin
  if AParam.ID.&Type = TPParamType(vPointer)
    then Result:=TVisualisationParamSorterPointerItem.Create(AOwner, AParam)
    else Result:=TVisualisationParamSorterItem.Create(AOwner, AParam);
end;

type
  TParamSorterAdvFunc = specialize TAdvFunc<TVisualisationParamSorterItems, TVisualisationParamSorterItem>;

{%ENDREGION}
{%REGION TParamSorterKey}

constructor TParamSorterKey.Create(AID: TPParamID);
begin
  inherited Create;
  FID:=AID;
end;

function TParamSorterKey.Equals(AOther: TObject): Boolean;
begin
  Assert(AOther.InheritsFrom(TParamSorterKey));
  Result:=TParamSorterKey(AOther).ID = FID;
end;

function TParamSorterKey.GetHashCode: PtrInt;
begin
  Result:=GetStringHash(FID.Name) xor FID.&Type;
end;

{%ENDREGION}
{%REGION TVisualisationParamSorterItem}

constructor TVisualisationParamSorterItem.Create(AOwner: TVisualisationParamSorter; AParam: IPParam);
begin
  inherited Create;
  FParam:=AParam;
  FIndex:=-1;
  FOwner:=AOwner;
  FOrdered:=false;
  FOrderIssuedBy:=nil;
  FLexedParamName:=Lex(AParam.ID.Name);
end;

destructor TVisualisationParamSorterItem.Destroy;
begin
  Assert(FParam <> nil);
  FParam:=nil;
  FLexedParamName.Destroy;
  inherited Destroy;
end;

procedure TVisualisationParamSorterItem.RemoveBindings;
begin
  //do nothing
end;

procedure TVisualisationParamSorterItem.SetConnection(ADoSort: Boolean = false);
begin
  //do nothing, this is used to connect pointers
end;

procedure TVisualisationParamSorterItem.SetOrder(AOrder: Integer; AIssuedBy: TVisualisationParamSorterPointerItem);
begin
  FOrder:=AOrder;
  FOrdered:=true;
  FOrderIssuedBy:=AIssuedBy;
end;

procedure TVisualisationParamSorterItem.RemoveOrder;
begin
  FOrdered:=false;
  FOrderIssuedBy:=nil;
end;

function TVisualisationParamSorterItem.Smaller(AThan: TVisualisationParamSorterItem): Boolean;
var
  ACompareResult: SmallInt;
begin
  if Owner.SortedBy = Self.Param then begin
    Result:=true;
    exit;
  end else if Owner.SortedBy = AThan.Param then begin
    Result:=false;
    exit;
  end;

  if Ordered then begin
    if AThan.Ordered
      then Result:=Order < AThan.Order
      else Result:=true;
  end else begin
    if AThan.Ordered then Result:=false else begin
      ACompareResult:=FLexedParamName.Compare(AThan.LexedParamName);
      case ACompareResult of
        -1: Result:=true;
        0 : Result:=FParam.ID.&Type < AThan.Param.ID.&Type;
        1 : Result:=false;
        else Assert(false);
      end;
    end;
  end;
end;

{%ENDREGION}
{%REGION TVisualisationParamSorterPointerItem}

constructor TVisualisationParamSorterPointerItem.Create(AOwner: TVisualisationParamSorter; AParam: IPParam);
begin
  Assert(AParam.ID.&Type = TPParamType(vPointer));
  inherited Create(AOwner, AParam);
  FConnects:=nil;
end;

destructor TVisualisationParamSorterPointerItem.Destroy;
begin
  RemoveConnection;
  inherited Destroy;
end;

procedure TVisualisationParamSorterPointerItem.RemoveConnection(ADoSort: Boolean = false);
begin
  if FConnects <> nil then begin
    FConnects.RemoveOrder;
    Owner.ConnectionsDecreased;
    if ADoSort
      then Owner.OrderChanged(FConnects);
  end;
end;

procedure TVisualisationParamSorterPointerItem.SetConnection(ADoSort: Boolean = false);
begin
  RemoveConnection(ADoSort);
  with IPPointer(Param).Value do begin
    if (Output.Preset.Name = '') and (Owner.SortedBy <> nil) and (Output.Param = Owner.SortedBy.ID) then begin
      if Input.Preset.Name = ''
        then FConnects:=Owner.GetParam(Input.Param)
        else FConnects:=Owner.GetParam(Input.Preset);

      FConnects.SetOrder(InversePriority, Self);
      Owner.ConnectionsIncreased;

      if ADoSort
        then Owner.OrderChanged(FConnects);
    end else FConnects:=nil;
  end;
end;

procedure TVisualisationParamSorterPointerItem.RemoveBindings;
begin
  FConnects:=nil;
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

  AContextClass.Added(CreateItem(AContextClass, IPParam(Sender)));
end;

constructor TVisualisationParamSorter.Create(AVisualisation: IPVisualisation; AThread: IPThread; ASortedBy: IPParam = nil);
var
  I        : Integer;
  AIterator: Pointer;
  AParam   : IPParam;
  AItem    : TVisualisationParamSorterItem;
begin
  Assert(AThread <> nil);
  Assert(AVisualisation <> nil);
  inherited Create;
  FSortedBy:=ASortedBy;
  FThread:=AThread;
  FVisualisation:=AVisualisation;
  //create param map
  FParamMap:=THashMap.Create;
  //copy from visualisation
  FConnectedCount:=0;
  I:=0;
  AIterator:=nil;
  AParam:=AVisualisation.IterateInput(AIterator);
  while AParam <> nil do begin
    SetLength(FParams, I+1);
    AItem:=CreateItem(Self, AParam);
    FParams[I]:=AItem;
    FParamMap.Add(TParamSorterKey.Create(AItem.Param.ID), AItem);
    Inc(I);
    AParam:=AVisualisation.IterateInput(AIterator);
  end;
  AVisualisation.AddInputGotListener(@VisualisationGotInput, Self, AThread);

  SortAll;
end;

destructor TVisualisationParamSorter.Destroy;
var
  I: Integer;
begin
  FVisualisation.RemoveInputGotListener(@VisualisationGotInput, Self);
  FParamMap.Destroy;
  FParamMap:=nil;
  for I:=0 to Length(FParams)-1 do with FParams[I] do begin
    RemoveBindings;
    Destroy;
  end;
  SetLength(FParams, 0);
  FVisualisation:=nil;
  FThread:=nil;
  inherited Destroy;
end;

procedure TVisualisationParamSorter.SortAll;
var
  I: Integer;
begin
  //initialize pointers
  for I:=0 to Length(FParams)-1
    do FParams[I].SetConnection;
  //sort
  TParamSorterAdvFunc.Quicksort(FParams, @ParamSorterItemSmaller, Length(FParams)-1);
  //set indices
  for I:=0 to Length(FParams)-1
    do FParams[I].Index:=I;
end;

procedure TVisualisationParamSorter.UpdateIndex(AIndex: Integer); inline;
begin
  FParams[AIndex].Index:=AIndex;
end;

procedure TVisualisationParamSorter.OrderChanged(AItem: TVisualisationParamSorterItem);
var
  I, L: Integer;
begin
  L:=Length(FParams);
  Assert(AItem.Index >= 0);
  Assert(AItem.Index < L);
  I:=AItem.Index;
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

procedure TVisualisationParamSorter.ConnectionsIncreased;
begin
  Inc(FConnectedCount);
end;

procedure TVisualisationParamSorter.ConnectionsDecreased;
begin
  Dec(FConnectedCount);
end;

{
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
}

{
procedure TVisualisationParamSorter.SetIndex(AParam: IPParam; AIndex: Integer);
var
  AOrderInterface: IInterface;
begin
  AOrderInterface:=AParam.AttachedInterfaces[GUIDVISPARAMORDER];
  if AOrderInterface <> nil
    then IVisualisationParamOrder(AOrderInterface).OrderInversePriority:=OrderAtIndex(AIndex);
end;
}

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

function TVisualisationParamSorter.GetParam(AID: TPParamID): TVisualisationParamSorterItem;
var
  AKey: TParamSorterKey;
  AItem: TObject;
begin
  AKey:=TParamSorterKey.Create(AID);

  AItem:=FParamMap[AKey];
  Assert(AItem <> nil);
  Result:=TVisualisationParamSorterItem(AItem);

  AKey.Destroy;
end;

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

  //add to hashmap
  FParamMap.Add(TParamSorterKey.Create(AItem.Param.ID), AItem);
  //sort
  AItem.SetConnection(true);

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

procedure TVisualisationParamSorter.SetSortedBy(ASortedBy: IPParam);
begin
  FSortedBy:=ASortedBy;
  SortAll;
  if Assigned(FOnChange)
    then FOnChange(Self);
end;

{%ENDREGION}

end.

