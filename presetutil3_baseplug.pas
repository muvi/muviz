unit PresetUtil3_BasePlug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetUtil3_Wires, GraphX32, PresetUtil3_Configuration,
  VisType2, AdvCoord, Controls, SlaveControl, ObjectClasses, ParamType2,
  Graphics, CanvasGraphX32, PresetUtil3_WiredPlug, PresetUtil3_Connections,
  PresetUtil3_ConnectionRouter, PresetUtil3_ConnectedWireManager;

type
  TVPVPlug               = class (TWiredPlug)
  strict private
    FConfig           : TPresetEditorConfiguration;
    FExtHighlightCount: Integer;
    FWireManager      : TWireManager;
    FDrawPlugImage    : Boolean;
    function GetExtHighlight: Boolean;
    procedure SetExtHighlight(Value: Boolean);
  strict protected
    procedure Connect(AConnection: TConnection); virtual; abstract;
    procedure Disconnect(AConnection: TConnection); virtual; abstract;
    procedure AddWire; virtual; abstract;

    procedure RealignConnection(AConnection: TConnection); inline;
    procedure RealignConnections; inline;
    procedure ConnectionDeleted(Sender: TObject); override;
    procedure ConnectionOwned(Sender: TObject; AConnection: TConnection); override;
    property WireManager: TWireManager read FWireManager;
  protected
    function GetIsOutput: Boolean; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetPlugColor: TColor32; override;
    function GetPlugType: TPParamType; virtual;
    procedure DoAbsolutePositionChanged; override;
    procedure DoAlign; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoPaint; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure HighlightConnected(AHighlighted: Boolean); virtual; abstract;

    property Config: TPresetEditorConfiguration read FConfig;
  public
    constructor Create(AOwner: TSlaveControl; AConnector: TConnector; AConfig: TPresetEditorConfiguration; AWireManager: TWireManager); reintroduce;
    destructor Destroy; override;

    property Connected: Boolean read GetConnected;
    property DrawPlugImage: Boolean read FDrawPlugImage write FDrawPlugImage;
    property ExtHighlighted: Boolean read GetExtHighlight write SetExtHighlight;
    property IsOutput: Boolean read GetIsOutput;
    property PlugType: TPParamType read GetPlugType;
  end;

const
  vUndefined = High(TPParamType);

implementation

{%REGION TVPVPlug}

constructor TVPVPlug.Create(AOwner: TSlaveControl; AConnector: TConnector; AConfig: TPresetEditorConfiguration; AWireManager: TWireManager);
begin
  inherited Create(AOwner, AConnector);
  FWireManager:=AWireManager;
  FConfig:=AConfig;
  FExtHighlightCount:=0;
  FDrawPlugImage:=false;
end;

destructor TVPVPlug.Destroy;
begin
  inherited Destroy;
end;

procedure TVPVPlug.DoAbsolutePositionChanged;
begin
  RealignConnections;
end;

procedure TVPVPlug.DoAlign;
begin
  RealignConnections;
end;

procedure TVPVPlug.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  if FDrawPlugImage then with Config.Pictures.OutputBmp do begin
    ANewBounds.Width:=Width + 4;
    if ANewBounds.Height < Height + 4
      then ANewBounds.Height:=Height + 4;
  end else begin
    ANewBounds.Width:=15;
  end;
end;

procedure TVPVPlug.DoPaint;
var
  AColor: TColor;
begin
  Canvas.Pen.Width:=1;
  //set color
  if FDrawPlugImage then begin
    if Highlighted
      then AColor:=Color32ToColor(BetaBlend(PlugColor, $FF999999, $50))
      else AColor:=Color32ToColor(BetaBlend(PlugColor, $FF999999, $20));
  end else begin
    if Highlighted
      then AColor:=Color32ToColor(BetaBlend(PlugColor, $FF999999, $E0))
      else AColor:=Color32ToColor(BetaBlend(PlugColor, $FF999999, $80));
  end;
  Canvas.Pen.Color:=AColor;
  Canvas.Brush.Color:=AColor;
  if IsOutput
    then DrawPartlyCutRect(Canvas, AbsoluteRect.Left, AbsoluteRect.Top, AbsoluteRect.Right, AbsoluteRect.Bottom, 4, 4, [reTopRight, reBottomRight])
    else DrawPartlyCutRect(Canvas, AbsoluteRect.Left, AbsoluteRect.Top, AbsoluteRect.Right, AbsoluteRect.Bottom, 4, 4, [reTopLeft, reBottomLeft]);

  if FDrawPlugImage then begin
    Config.Pictures.LoadImage(Connected,Highlighted,ExtHighlighted,PlugType);
    Canvas.Draw(
      AbsoluteRect.Left + ((Width - Config.Pictures.OutputBmp.Width) div 2),
      AbsoluteRect.Top + ((Height - Config.Pictures.OutputBmp.Height) div 2),
      Config.Pictures.OutputBmp);
  end;
end;

procedure TVPVPlug.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  AItem: TObjectListItem;
begin
  if WireManager.OpenConnector.Connections.Empty then begin
    if Connections.Count = 1
      then Disconnect(TConnection(Connections.First.Content))
      else AddWire;
  end else begin
    //connect all wires
    AItem:=WireManager.OpenConnector.Connections.First;
    while AItem <> nil do begin
      Connect(TConnection(AItem.Content));
      AItem:=AItem.Next;
    end;
  end;
end;

procedure TVPVPlug.DoMouseEnter;
begin
  //HighlightConnected(true);
  inherited DoMouseEnter;
end;

procedure TVPVPlug.DoMouseLeave;
begin
  //HighlightConnected(false);
  inherited DoMouseLeave;
end;

function TVPVPlug.GetExtHighlight: Boolean;
begin
  //TODO: repair this
  Result:=false;
  //Result:=(FExtHighlightCount>0);
end;

procedure TVPVPlug.SetExtHighlight(Value: Boolean);
{
var
  AOldExtHighlighted: Boolean;
  }
begin
  //TODO: repair this

  {
  //TODO: evtl. Fehler, wenn ein Kabel entfernt wird?!?
  AOldExtHighlighted:=ExtHighlighted;
  if Value
    then Inc(FExtHighlightCount)
    else Dec(FExtHighlightCount);
  if AOldExtHighlighted xor ExtHighlighted
    then Repaint;
  }
end;

function TVPVPlug.GetPlugType: TPParamType;
begin
  Result:=vUndefined;
end;

function TVPVPlug.GetPlugColor: TColor32;
begin
  Result:=ParamTypeUtil[PlugType].Color;
end;

procedure TVPVPlug.RealignConnection(AConnection: TConnection); inline;
begin
  SetConnectionPosition(AConnection, RealPoint(Width/2.0, Height/2.0));
end;

procedure TVPVPlug.RealignConnections; inline;
var
  AItem: TObjectListItem;
begin
  AItem:=Connections.First;
  while AItem<>nil do begin
    RealignConnection(TConnection(AItem.Content));
    AItem:=AItem.Next;
  end;
end;

procedure TVPVPlug.ConnectionDeleted(Sender: TObject);
begin
  Repaint;
end;

procedure TVPVPlug.ConnectionOwned(Sender: TObject; AConnection: TConnection);
begin
  //do not align connections which should be propagated
  if (AConnection.IsDestination(Connector)) and (AConnection.IsLeft <> IsOutput) then begin
    Connections.Add(AConnection);
    RealignConnection(AConnection);
    Repaint;
  end;
end;

{%ENDREGION}

end.

