unit PresetUtil3_WireSlot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, PresetUtil3_Connections, CanvasGraphX32,
  ObjectClasses, PresetUtil3_ConnectionRouter, AdvCoord;

type
  TWireSlot = class (TSlaveControl)
  strict private
    FConnections         : TRoutableConnections;
    FOwnedConnectionCount: Integer;
    FHandlersRemoved     : Boolean;
    procedure ConnectorConnectionOwned(Sender: TObject; AConnection: TConnection);
    procedure ConnectorConnectionDisowned(Sender: TObject; AConnection: TConnection);
    //realigns a connection.
    //checks if the connection is owned.
    procedure Realign(AConnection: TConnection); inline;
    //returns a random wire position
    function RandomPixelOffset(AConnection: TObject): PtrUInt;
  strict protected
    procedure DoPaint; override;
    procedure DoAbsolutePositionChanged; override;
    procedure Repaint; override;
    function IsClickThrough: Boolean; override;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections);
    destructor Destroy; override;
    //use in destructors only
    procedure RemoveHandlers;
  end;

const
  WIRESLOTCOLOR       = $777777;
  WIRESLOTXDIST       = 5;
  WIRESLOTYDIST       = 2;

implementation

{%REGION TWireSlot}

constructor TWireSlot.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections);
begin
  inherited Create(AOwner);
  FHandlersRemoved:=false;
  FOwnedConnectionCount:=0;
  FConnections:=AConnections;
  FConnections.Connector.AddConnectionOwnedHandler(@ConnectorConnectionOwned);
  FConnections.Connector.AddConnectionDisownedHandler(@ConnectorConnectionDisowned);
end;

destructor TWireSlot.Destroy;
begin
  if not FHandlersRemoved
    then RemoveHandlers;
  inherited Destroy;
end;

procedure TWireSlot.RemoveHandlers;
begin
  Assert(not FHandlersRemoved);
  FHandlersRemoved:=true;
  FConnections.Connector.RemoveConnectionOwnedHandler(@ConnectorConnectionOwned);
  FConnections.Connector.RemoveConnectionDisownedHandler(@ConnectorConnectionDisowned);
end;

procedure TWireSlot.DoPaint;
begin
  if FOwnedConnectionCount > 0 then begin
    Canvas.Brush.Color:=WIRESLOTCOLOR;
    Canvas.Pen.Color:=WIRESLOTCOLOR;
    DrawCutRect(Canvas, AbsoluteRect.Left, AbsoluteRect.Top, AbsoluteRect.Right, AbsoluteRect.Bottom, 2, 2);
  end;
end;

function TWireSlot.IsClickThrough: Boolean;
begin
  Result:=FOwnedConnectionCount <= 0;
end;

procedure TWireSlot.DoAbsolutePositionChanged;
var
  AItem: TObjectListItem;
begin
  AItem:=FConnections.Connections.First;
  while AItem<>nil do begin
    Realign(TConnection(AItem.Content));
    AItem:=AItem.Next;
  end;
end;

procedure TWireSlot.Repaint;
begin
  Master.Repaint(Owner);
end;

procedure TWireSlot.Realign(AConnection: TConnection); inline;
begin
  if AConnection.OwnedBy(FConnections.Connector) then begin
    if AConnection.IsLeft
      then SetConnectionPosition(AConnection, RealPoint(WIRESLOTXDIST + RandomPixelOffset(AConnection), Height - WIRESLOTYDIST))
      else SetConnectionPosition(AConnection, RealPoint(Width - WIRESLOTXDIST - RandomPixelOffset(AConnection), Height - WIRESLOTYDIST));
  end;
end;

procedure TWireSlot.ConnectorConnectionOwned(Sender: TObject; AConnection: TConnection);
begin
  //if it is the destination, the plugs will handle it...
  if not AConnection.IsDestination(FConnections.Connector) then begin
    Realign(AConnection);
    Inc(FOwnedConnectionCount);
    Assert(FOwnedConnectionCount > 0);
    if FOwnedConnectionCount = 1
      then Repaint;
  end;
end;

procedure TWireSlot.ConnectorConnectionDisowned(Sender: TObject; AConnection: TConnection);
begin
  //if it is the destination, the plugs will handle it...
  if not AConnection.IsDestination(FConnections.Connector) then begin
    Dec(FOwnedConnectionCount);
    Assert(FOwnedConnectionCount >= 0);
    if FOwnedConnectionCount = 0
      then Repaint;
  end;
end;

function TWireSlot.RandomPixelOffset(AConnection: TObject): PtrUInt;
begin
  Result:=((PtrUInt(AConnection) mod 4507) * 257) mod ((Width - WIRESLOTXDIST) div 2);
end;

{%ENDREGION}

end.

