unit PresetUtil3_DummyPlugs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetUtil3_BasePlug, DebugTools;

type
  TVPVDummyPlug          = class (TVPVPlug)
  protected
    //function GetConnected: Boolean; override;
  end;

  TVPVDummyOutputPlug    = class (TVPVDummyPlug)
  protected
    function GetIsOutput: Boolean; override;
    function GetName: string; override;
  protected
    procedure HighlightConnected(AHighlighted: Boolean); override;
  end;

  TVPVDummyInputPlug     = class (TVPVDummyPlug)
  protected
    function GetIsOutput: Boolean; override;
    function GetName: string; override;
  protected
    procedure HighlightConnected(AHighlighted: Boolean); override;
  end;

implementation

{%REGION TVPVDummyPlug}

{
function TVPVDummyPlug.GetConnected: Boolean;
begin
  Result:=false;
end;
}

{%ENDREGION}
{%REGION TVPVDummyOutputPlug}

function TVPVDummyOutputPlug.GetIsOutput: Boolean;
begin
  Result:=true;
end;

function TVPVDummyOutputPlug.GetName: string;
begin
  Result:='Outputs';
end;

procedure TVPVDummyOutputPlug.HighlightConnected(AHighlighted: Boolean);
begin
  raise ENotImplemented.Create(TMethod(@HighlightConnected));
  //TODO: Implement this
end;

{%ENDREGION}
{%REGION TVPVDummyInputPlug}

function TVPVDummyInputPlug.GetIsOutput: Boolean;
begin
  Result:=false;
end;

function TVPVDummyInputPlug.GetName: string;
begin
  Result:='Inputs';
end;

procedure TVPVDummyInputPlug.HighlightConnected(AHighlighted: Boolean);
begin
  raise ENotImplemented.Create(TMethod(@HighlightConnected));
  //TODO: Implement this
end;

{%ENDREGION}

end.

