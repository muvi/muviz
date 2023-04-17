unit PresetUtil3_Plugs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, VisType2, AdvCoord, Controls, MStrings,
  PhysicsWire, Graphics, GraphX32, ParamType2, PresetUtil3_Connections,
  PresetUtil3_Configuration, PresetUtil3_Wires, ObjectCallbackContainer,
  PresetUtil3_BasePlug, ObjectArray, PresetUtil3_WiredPlug, PointerParamEdit,
  PresetUtil3_ConnectedWireManager, PresetUtil3_UpdatingWire,
  PresetUtil3_Path, DebugTools, PresetUtil3_ConnectionRouter,
  PresetUtil3_WiredMaster;

type
  TVPVParamPlug          = class (TVPVPlug)
  strict private
    FPath          : ISidedParamPath;
    FRouter        : TConnectionRouter;
    function GetID: TPParamID; inline;
  strict protected
    procedure Connect(AConnection: TConnection); override;
    procedure Disconnect(AConnection: TConnection); override;
    procedure AddWire; override;
    property ID: TPParamID read GetID;
    property Path: ISidedParamPath read FPath;
  protected
    function GetPlugType: TPParamType; override;
    procedure HighlightConnected(AHighlighted: Boolean); override;

  public
    constructor Create(AOwner: TSlaveControl; AConnector: TConnector; AConfig: TPresetEditorConfiguration; AWireManager: TWireManager; APath: IParamPath; ARouter: TConnectionRouter);
    destructor Destroy; override;
  end;

  TVPVInputPlug          = class (TVPVParamPlug)
  protected
    function GetIsOutput: Boolean; override;
  end;

  TVPVOutputPlug         = class (TVPVParamPlug)
  protected
    function GetIsOutput: Boolean; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
  end;

implementation

{%REGION TVPVParamPlug}

constructor TVPVParamPlug.Create(AOwner: TSlaveControl; AConnector: TConnector; AConfig: TPresetEditorConfiguration; AWireManager: TWireManager; APath: IParamPath; ARouter: TConnectionRouter);
begin
  inherited Create(AOwner, AConnector, AConfig, AWireManager);
  Assert(APath <> nil);
  Assert(ARouter <> nil);
  FPath:=NewParamPath(not IsOutput, APath);
  FRouter:=ARouter;
end;

destructor TVPVParamPlug.Destroy;
begin
  FPath:=nil;
  inherited Destroy;
end;

procedure TVPVParamPlug.Connect(AConnection: TConnection);
begin
  AConnection.Updater.Path:=Path;
end;

procedure TVPVParamPlug.Disconnect(AConnection: TConnection);
begin
  AConnection.Updater.Path:=OpenSidedPath;
end;

procedure TVPVParamPlug.AddWire;
begin
  NewUpdater(TWiredMaster(Master), Path, FRouter);
end;

procedure TVPVParamPlug.HighlightConnected(AHighlighted: Boolean);
//var
//  I: Integer;
begin
  {
  for I:=0 to FWires.Count-1
    do TVPVPlug(TPWContainer(FWires[I]).OtherEnd[Self]).ExtHighlighted:=AHighlighted;
  }
end;

{function TVPVParamPlug.GetName: string;
begin
  Result:=FID.Name.PasStr;
end;}

function TVPVParamPlug.GetID: TPParamID; inline;
begin
  Result:=FPath[FPath.Count-1];
end;

function TVPVParamPlug.GetPlugType: TPParamType;
begin
  Result:=ID.&Type;
end;

{%ENDREGION}
{%REGION TVPVInputPlug}

function TVPVInputPlug.GetIsOutput: Boolean;
begin
  Result:=false;
end;

{%ENDREGION}
{%REGION TVPVOutputPlug}

function TVPVOutputPlug.GetIsOutput: Boolean;
begin
  Result:=true;
end;

procedure TVPVOutputPlug.DoSetBounds(var ANewBounds: TBoundsRect);
var
  AOldWidth: Integer;
begin
  AOldWidth:=ANewBounds.Width;
  inherited DoSetBounds(ANewBounds);
  ANewBounds.Left:=ANewBounds.Left-(ANewBounds.Width-AOldWidth);
end;

{%ENDREGION}

end.

