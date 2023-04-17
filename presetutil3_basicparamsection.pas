unit PresetUtil3_BasicParamSection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, VisType2, ParamPainter2, AdvCoord,
  PresetUtil3_Connections;

type
  TBasicParamSection    = class (TSlaveControl)
  strict protected
    function GetExpectedHeight: Integer; virtual; abstract;
    function GetMinWidth: Integer; virtual; abstract;
    function IsRoutable: Boolean; virtual;

    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    function GetTextWidth(const S: string): Integer;
  public
    procedure ContextCreated; virtual;
    property ExpectedHeight: Integer read GetExpectedHeight;
    property MinWidth: Integer read GetMinWidth;
    property Routable: Boolean read IsRoutable;
  end;

  TRoutableParamSection = class (TBasicParamSection)
  strict protected
    function GetConnector: TConnector; virtual; abstract;
    function GetEdit: TPPParam; virtual; abstract;
    function IsRoutable: Boolean; override;
    function GetID: TPParamID; virtual; abstract;
  public
    property Connector: TConnector read GetConnector;
    property Edit: TPPParam read GetEdit;
    property ID: TPParamID read GetID;
  end;

const
  PARAMNAMESIZE     = 12;
  PARAMTOPSPACE     = 2;
  KNOBSPACE         = 2;
  SEPERATORNAMESIZE = 16;

implementation

{%REGION TBasicParamSection}

procedure TBasicParamSection.ContextCreated;
begin
  //do nothing
end;

function TBasicParamSection.IsRoutable: Boolean;
begin
  Result:=false;
end;

procedure TBasicParamSection.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  inherited DoSetBounds(ANewBounds);
  ANewBounds.Height:=GetExpectedHeight;
end;

function TBasicParamSection.GetTextWidth(const S: string): Integer;
begin
  Result:=Canvas.TextWidth(S);
end;

{%ENDREGION}
{%REGION TRoutableParamSection}

function TRoutableParamSection.IsRoutable: Boolean;
begin
  Result:=true;
end;

{%ENDREGION}

end.

