unit PresetUtil3_WiredPlug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, PresetUtil3_Wires, GraphX32,
  PresetUtil3_Connections, ObjectClasses;

type
  TWiredPlug             = class (TSlaveControl)
  strict private
    FConnector  : TConnector;
    FConnections: TObjectList;
  strict protected
    function GetPlugColor: TColor32; virtual;
    function GetConnected: Boolean;
    procedure ConnectionDeleted(Sender: TObject); virtual; abstract;
    procedure ConnectionOwned(Sender: TObject; AConnection: TConnection); virtual; abstract;
    property Connections: TObjectList read FConnections;
    property Connector: TConnector read FConnector;
  public
    constructor Create(AOwner: TSlaveControl; AConnector: TConnector); reintroduce;
    destructor Destroy; override;

    property Owner;
    property PlugColor: TColor32 read GetPlugColor;
  end;

implementation

{%REGION TWiredPlug}

constructor TWiredPlug.Create(AOwner: TSlaveControl; AConnector: TConnector);
begin
  inherited Create(AOwner);
  FConnections:=TObjectList.Create;
  FConnections.OnDeleteObject:=@ConnectionDeleted;
  FConnector:=AConnector;
  FConnector.AddConnectionOwnedHandler(@ConnectionOwned);
end;

destructor TWiredPlug.Destroy;
begin
  FConnector.RemoveConnectionOwnedHandler(@ConnectionOwned);
  FConnections.Destroy;
  inherited Destroy;
end;

function TWiredPlug.GetPlugColor: TColor32;
begin
  Result:=UNDEFINEDCOLOR;
end;

function TWiredPlug.GetConnected: Boolean;
begin
  Result:=not FConnections.Empty;
end;

{procedure TWiredPlug.DoAbsolutePositionChanged;
var
  AItem: TObjectListItem;
begin
  AItem:=FConnections.First;
  while AItem<>nil do begin
    TConnection(AItem.Content).Position:=AbsoluteRect.TopLeft;
    AItem:=AItem.Next;
  end;
end;}

{%ENDREGION}

end.

