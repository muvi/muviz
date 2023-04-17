unit PresetUtil3_ConnectedWireManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetUtil3_Wires, PresetUtil3_Connections, AdvCoord,
  PresetUtil3_UpdatingWire, PresetUtil3_Path, VisType2, StdParamTypes, MStrings,
  MStringImpl;

type
  TConnectedWireManager = class (TBasicWireManager)
  strict private
    FOpenConnector: TConnector;
  public
    constructor Create(AConfig: TWireConfiguration);
    destructor Destroy; override;
    function NewWire(AUpdater: TWireUpdater): TUpdatablePWContainer;
    function TryRouteOpen(AConnection: TConnection): Boolean;
    property OpenConnector: TConnector read FOpenConnector;
  end;

  TWireManager          = TConnectedWireManager;

implementation

{%REGION TConnectedWireManager}

constructor TConnectedWireManager.Create(AConfig: TWireConfiguration);
begin
  inherited Create(AConfig);
  FOpenConnector:=TConnector.Create;
end;

destructor TConnectedWireManager.Destroy;
begin
  FOpenConnector.Destroy;
  inherited Destroy;
end;

function TConnectedWireManager.NewWire(AUpdater: TWireUpdater): TUpdatablePWContainer;
begin
  Result:=TUpdatablePWContainer.Create(Config, AUpdater);
  AddWire(Result);
end;

function TConnectedWireManager.TryRouteOpen(AConnection: TConnection): Boolean;
begin
  Assert(AConnection <> nil);
  Result:=AConnection.Destination.Equals(OpenPath);
  if Result
    then OpenConnector.AddConnection(AConnection);
end;

{%ENDREGION}

end.

