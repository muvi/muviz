unit PresetUtil3_Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PhysicsWire, Graphics, Controls, VisType2, AdvFunc,
  ParamType2, csl, HashMap, GraphX32, PresetUtil3_Wires, MiscSlaveControls,
  ParamTypeKey, PlugImages;

type
  TPresetEditorConfiguration     = class (TWireConfiguration)
  strict private
    //configuration
    FPictures          : TPlugImages;
    FWireSettings      : TPhysicsWireSettings;
    //used to draw wires
    FPixelSize         : Real;
    FWireKnotCount     : Integer;
    FWireVisibilityMode: TWireVisibilityMode;
  protected
    function GetPixelSize: Real; override;
    function GetWireKnotCount: Integer; override;
    function GetWireOffset: TPoint; override;
    function GetWireSettings: TPhysicsWireSettings; override;
    function GetWireVisibilityMode: TWireVisibilityMode; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Pictures: TPlugImages read FPictures;
    property PixelSize: Real read FPixelSize write FPixelSize;
    property WireSettings: TPhysicsWireSettings read FWireSettings;
    property WireKnotCount: Integer read FWireKnotCount write FWireKnotCount;
    property WireVisibilityMode: TWireVisibilityMode read FWireVisibilityMode write FWireVisibilityMode;
  end;

const
  //VIEWPRESETHEADERSPACE          = 10;
  VIEWPRESETITEMSPACE            = 6;
  VIEWPRESETINPUTTEXTWIDTH       = 100;
  VIEWPRESETOUTPUTTEXTWIDTH      = 100;
  VIEWPRESETKNOBLEFTRIGHT        = 6;
  VIEWPRESETKNOBTEXTSPACE        = 3;
  VIEWPRESETTEXTEDITSPACE        = 3;
  //VIEWPRESETHEADERITEMDIFFERENCE = ViewPresetHeaderSpace-ViewPresetItemSpace;

implementation

{%REGION TPresetEditorConfiguration}

constructor TPresetEditorConfiguration.Create;
begin
  inherited Create;
  FPictures:=TPlugImages.Create;
  FWireSettings:=TPhysicsWireSettings.Create;
  FPixelSize:=1.0;
  FWireKnotCount:=50;
end;

destructor TPresetEditorConfiguration.Destroy;
begin
  FPictures.Destroy;
  FWireSettings.Destroy;
  inherited Destroy;
end;

function TPresetEditorConfiguration.GetPixelSize: Real;
begin
  Result:=FPixelSize;
end;

function TPresetEditorConfiguration.GetWireKnotCount: Integer;
begin
  Result:=FWireKnotCount;
end;

function TPresetEditorConfiguration.GetWireOffset: TPoint;
begin
  Result:=FPictures.ImageCenter;
end;

function TPresetEditorConfiguration.GetWireSettings: TPhysicsWireSettings;
begin
  Result:=FWireSettings;
end;

function TPresetEditorConfiguration.GetWireVisibilityMode: TWireVisibilityMode;
begin
  Result:=FWireVisibilityMode;
end;

{%ENDREGION}

end.

