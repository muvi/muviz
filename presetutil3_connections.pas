unit PresetUtil3_Connections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic, PresetUtil3_Wires, VisType2, ParamType2,
  GraphX32, ObjectClasses, AdvCoord, Delegates, MethodQueues,
  PresetUtil3_UpdatingWire, PresetUtil3_Path;

type
  TConnection            = class;
  TConnectionEvent       = procedure (Sender: TObject; AConnection: TConnection) of object;

  TConnector             = class
  strict private
    FOnConnectionOwned   : TDelegate;
    FOnConnectionDisowned: TDelegate;
    FOnConnectionAdded   : TDelegate;
    FConnections         : TObjectList;
    //to avoid disowned executed before every owned event is executed.
    //happens if someone propagates a connection in an owned event.
    FCurrentEvents       : TDelegate;
    procedure SetPosition(AValue: TRealPoint);
    procedure ExecuteEvent(AEvent: TDelegate; AConnection: TConnection); inline;
    //used in the destructor
    procedure Clear; inline;
  private
    procedure Owned(AConnection: TConnection); inline;
    procedure Disowned(AConnection: TConnection); inline;
    procedure Added(AConnection: TConnection); inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddConnection(AConnection: TConnection);
    procedure AddConnectionOwnedHandler(AHandler: TConnectionEvent);
    procedure RemoveConnectionOwnedHandler(AHandler: TConnectionEvent);
    procedure AddConnectionDisownedHandler(AHandler: TConnectionEvent);
    procedure RemoveConnectionDisownedHandler(AHandler: TConnectionEvent);
    procedure AddConnectionAddedHandler(AHandler: TConnectionEvent);
    procedure RemoveConnectionAddedHandler(AHandler: TConnectionEvent);
    property Connections: TObjectList read FConnections;
    property Position: TRealPoint write SetPosition;
  end;

  TConnection            = class (TAbstractConnection)
  strict private
    FDestination: ISidedParamPath;
    FConnectors : array of TConnector;
    //indicates, how far the connections has reached its destination
    FOwningLevel: Integer;
    FUpdater    : TUpdaterSide;
    function GetType: TPParamType;
    function GetNextRoutingID: TPParamID;
    function GetIsLeft: Boolean; inline;
    procedure DoBeforeConstruction(ADestination: ISidedParamPath); inline;
    procedure DoAfterConstruction; inline;
  private
    //events called by TConnector
    procedure AddConnector(AConnector: TConnector);
    procedure RemoveConnector(AConnector: TConnector);
  strict protected
    function GetColor: TColor32; override;
    function GetHasColor: Boolean; override;
  public
    constructor Create(AWire: TPWContainer; ADestination: ISidedParamPath);
    constructor Create(AReplaces: TAbstractConnection; ADestination: ISidedParamPath);
    destructor Destroy; override;
    function OwnedBy(AConnector: TConnector): Boolean;
    function IsDestination(AConnector: TConnector): Boolean;
    property Destination: ISidedParamPath read FDestination;
    property IsLeft: Boolean read GetIsLeft;
    property NextRoutingID: TPParamID read GetNextRoutingID;
    property &Type: TPParamType read GetType;
    property Updater: TUpdaterSide read FUpdater;
  end;

implementation

{%REGION TConnectionEventDelegateParams}

type
  TConnectionEventDelegateParams = class (TDelegateParams)
  strict private
    FSender    : TObject;
    FConnection: TConnection;
  public
    constructor Create(ASender: TObject; AConnection: TConnection);
    property Connection: TConnection read FConnection;
    property Sender: TObject read FSender;
  end;

constructor TConnectionEventDelegateParams.Create(ASender: TObject; AConnection: TConnection);
begin
  inherited Create;
  FSender:=ASender;
  FConnection:=AConnection;
end;

{%ENDREGION}
{%REGION TConnectionEventDelegateMethod}

type
  TConnectionEventDelegateMethod = class (TDelegateMethod)
  strict private
    FMethod: TConnectionEvent;
  protected
    procedure Execute(AParams: TDelegateParams); override;
  public
    constructor Create(AMethod: TConnectionEvent);
    function Equals(Obj: TObject): Boolean; override;
  end;

constructor TConnectionEventDelegateMethod.Create(AMethod: TConnectionEvent);
begin
  inherited Create;
  FMethod:=AMethod;
end;

function TConnectionEventDelegateMethod.Equals(Obj: TObject): Boolean;
begin
  Result:=(Obj is TConnectionEventDelegateMethod)
      and (TConnectionEventDelegateMethod(Obj).FMethod = FMethod)
end;

procedure TConnectionEventDelegateMethod.Execute(AParams: TDelegateParams);
begin
  with TConnectionEventDelegateParams(AParams)
    do FMethod(Sender, Connection);
end;

{%ENDREGION}
{%REGION TConnection}

constructor TConnection.Create(AWire: TPWContainer; ADestination: ISidedParamPath);
begin
  DoBeforeConstruction(ADestination);
  inherited Create(AWire);
  DoAfterConstruction;
end;

constructor TConnection.Create(AReplaces: TAbstractConnection; ADestination: ISidedParamPath);
begin
  DoBeforeConstruction(ADestination);
  inherited Create(AReplaces);
  DoAfterConstruction;
end;

procedure TConnection.DoBeforeConstruction(ADestination: ISidedParamPath); inline;
begin
  //has to be done before inherited construction because the construction uses
  //the GetType method
  Assert(ADestination <> nil);
  Assert(ADestination.Count > 0);
  FDestination:=ADestination;
end;

procedure TConnection.DoAfterConstruction; inline;
begin
  if Self = Wire.FirstConnection
    then FUpdater:=TPWContainer(Wire).Updater.FirstSide
    else FUpdater:=TPWContainer(Wire).Updater.LastSide;
  if Destination <> nil
    then SetLength(FConnectors, Destination.Count)
    //one place for adding the open connector
    else SetLength(FConnectors, 1);
  FOwningLevel:=0;
end;

destructor TConnection.Destroy;
begin
  //disown only the last one, because the othe ones where not owned...
  if FOwningLevel > 0
    then FConnectors[FOwningLevel-1].Disowned(Self);
  SetLength(FConnectors, 0);
  FDestination:=nil;
  inherited Destroy;
end;

function TConnection.GetType: TPParamType;
begin
  Result:=FDestination[FDestination.Count-1].&Type;
end;

function TConnection.GetColor: TColor32;
begin
  Result:=ParamTypeUtil[&Type].Color;
end;

function TConnection.GetHasColor: Boolean;
begin
  Result:=not FDestination.Equals(OpenPath);
end;

function TConnection.GetNextRoutingID: TPParamID;
begin
  Assert(FDestination <> nil);
  Assert(FOwningLevel < Length(FConnectors));
  Result:=FDestination[FOwningLevel];
end;

function TConnection.GetIsLeft: Boolean; inline;
begin
  Result:=FDestination.IsLeft;
end;

function TConnection.OwnedBy(AConnector: TConnector): Boolean;
begin
  //no one should be able to call this method if he is not added to the
  //connector list
  Assert(FOwningLevel > 0);
  Result:=FConnectors[FOwningLevel-1] = AConnector;
end;

function TConnection.IsDestination(AConnector: TConnector): Boolean;
begin
  Result:=(FOwningLevel = Length(FConnectors)) and OwnedBy(AConnector);
end;

procedure TConnection.AddConnector(AConnector: TConnector);
begin
  Assert(FOwningLevel < Length(FConnectors));

  //do this before setting the new connector to produce correct values in
  //isDestination()
  if FOwningLevel > 0
    then FConnectors[FOwningLevel-1].Disowned(Self);

  FConnectors[FOwningLevel]:=AConnector;
  Inc(FOwningLevel);
  AConnector.Added(Self);
  AConnector.Owned(Self);
end;

procedure TConnection.RemoveConnector(AConnector: TConnector);
begin
  Dec(FOwningLevel);
  Assert(FConnectors[FOwningLevel] = AConnector);
  if FOwningLevel > 0
    then FConnectors[FOwningLevel-1].Owned(Self)
    else Destroy;
end;

{%ENDREGION}
{%REGION TConnector}

constructor TConnector.Create;
begin
  inherited Create;
  FConnections:=TObjectList.Create;
  FOnConnectionOwned:=TDelegate.Create;
  FOnConnectionDisowned:=TDelegate.Create;
  FOnConnectionAdded:=TDelegate.Create;
  FCurrentEvents:=TMethodQueue.Create;
end;

destructor TConnector.Destroy;
begin
  Clear;
  FConnections.Destroy;
  FCurrentEvents.Destroy;
  FOnConnectionOwned.Destroy;
  FOnConnectionDisowned.Destroy;
  FOnConnectionAdded.Destroy;
  inherited Destroy;
end;

procedure TConnector.Clear;
var
  AItem: TObjectListItem;
begin
  AItem:=FConnections.First;
  while AItem<>nil do begin
    TConnection(AItem.Content).RemoveConnector(Self);
    AItem:=AItem.Next;
  end;
  FConnections.Clear;
end;

procedure TConnector.ExecuteEvent(AEvent: TDelegate; AConnection: TConnection); inline;
begin
  //AEvent.Execute(TConnectionEventDelegateParams.Create(Self, AConnection));
  FCurrentEvents.AddAndExecute(
    TDelegateDelegateMethod.Create(
      AEvent,
      TConnectionEventDelegateParams.Create(Self, AConnection)),
    nil,
    false);
end;

procedure TConnector.Owned(AConnection: TConnection); inline;
begin
  ExecuteEvent(FOnConnectionOwned, AConnection);
end;

procedure TConnector.Disowned(AConnection: TConnection); inline;
begin
  ExecuteEvent(FOnConnectionDisowned, AConnection);
end;

procedure TConnector.Added(AConnection: TConnection); inline;
begin
  ExecuteEvent(FOnConnectionAdded, AConnection);
end;

procedure TConnector.AddConnectionOwnedHandler(AHandler: TConnectionEvent);
begin
  FOnConnectionOwned.Add(TConnectionEventDelegateMethod.Create(AHandler));
end;

procedure TConnector.RemoveConnectionOwnedHandler(AHandler: TConnectionEvent);
var
  AResult: Boolean;
begin
  AResult:=FOnConnectionOwned.Remove(TConnectionEventDelegateMethod.Create(AHandler));
  Assert(AResult);
end;

procedure TConnector.AddConnectionDisownedHandler(AHandler: TConnectionEvent);
begin
  FOnConnectionDisowned.Add(TConnectionEventDelegateMethod.Create(AHandler));
end;

procedure TConnector.RemoveConnectionDisownedHandler(AHandler: TConnectionEvent);
var
  AResult: Boolean;
begin
  AResult:=FOnConnectionDisowned.Remove(TConnectionEventDelegateMethod.Create(AHandler));
  Assert(AResult);
end;

procedure TConnector.AddConnectionAddedHandler(AHandler: TConnectionEvent);
begin
  FOnConnectionAdded.Add(TConnectionEventDelegateMethod.Create(AHandler));
end;

procedure TConnector.RemoveConnectionAddedHandler(AHandler: TConnectionEvent);
var
  AResult: Boolean;
begin
  AResult:=FOnConnectionAdded.Remove(TConnectionEventDelegateMethod.Create(AHandler));
  Assert(AResult);
end;

procedure TConnector.AddConnection(AConnection: TConnection);
begin
  FConnections.Add(AConnection);
  AConnection.AddConnector(Self);
  //AddConnector calls Owned and Added...
end;

procedure TConnector.SetPosition(AValue: TRealPoint);
var
  AItem: TObjectListItem;
begin
  AItem:=FConnections.First;
  while AItem<>nil do begin
    TConnection(AItem.Content).Position:=AValue;
    AItem:=AItem.Next;
  end;
end;

{%ENDREGION}

end.

