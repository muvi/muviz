unit PresetUtil3_ConnectionRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, PresetUtil3_Connections, ObjectClasses,
  AdvCoord, PresetUtil3_Path, VisType2;

type
  TConnectionRouter    = class (TSlaveControl)
  strict private
    FParentRouter: TConnectionRouter;
    FPath        : IParamPath;
  strict protected
    function GetSubRouter(AID: TPParamID): TConnectionRouter; virtual; abstract;
  public
    constructor Create(AOwner: TSlaveControl; AParentRouter: TConnectionRouter; APath: IParamPath; AMaster: TMasterControl = nil);
    destructor Destroy; override;
    procedure RouteConnection(AConnection: TConnection); virtual; abstract;
    property ParentRouter: TConnectionRouter read FParentRouter;
    property Path: IParamPath read FPath;
    property SubRouter[AID: TPParamID]: TConnectionRouter read GetSubRouter;
  end;

  TRoutableConnections = class
  strict private
    FConnector  : TConnector;
    FConnections: TObjectList;
    procedure ConnectorConnectionAdded(Sender: TObject; AConnection: TConnection);
  public
    constructor Create(AConnector: TConnector);
    destructor Destroy; override;
    property Connections: TObjectList read FConnections;
    property Connector: TConnector read FConnector;
  end;

  TRoutingControlHelper= class helper for TSlaveControl
    procedure SetConnectionPosition(AConnection: TConnection; APosition: TRealPoint); inline;
  end;

implementation

{%REGION TConnectionRouter}

constructor TConnectionRouter.Create(AOwner: TSlaveControl; AParentRouter: TConnectionRouter; APath: IParamPath; AMaster: TMasterControl = nil);
begin
  inherited Create(AOwner, AMaster);
  FParentRouter:=AParentRouter;
  FPath:=APath;
end;

destructor TConnectionRouter.Destroy;
begin
  FPath:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRoutableConnections}

constructor TRoutableConnections.Create(AConnector: TConnector);
begin
  inherited Create;
  FConnections:=TObjectList.Create;
  FConnector:=AConnector;
  FConnector.AddConnectionAddedHandler(@ConnectorConnectionAdded);
end;

destructor TRoutableConnections.Destroy;
begin
  FConnector.RemoveConnectionAddedHandler(@ConnectorConnectionAdded);
  FConnections.Destroy;
  inherited Destroy;
end;

procedure TRoutableConnections.ConnectorConnectionAdded(Sender: TObject; AConnection: TConnection);
begin
  //if it is the destination, the plugs will handle it...
  if not AConnection.IsDestination(FConnector)
    then FConnections.Add(AConnection);
end;

{%ENDREGION}
{%REGION TRoutingControlHelper}

procedure TRoutingControlHelper.SetConnectionPosition(AConnection: TConnection; APosition: TRealPoint); inline;
begin
  AConnection.Position:=AbsoluteRect.TopLeft + APosition + Master.ScrollPos;
end;

{%ENDREGION}

end.

