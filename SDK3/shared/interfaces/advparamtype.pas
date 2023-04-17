unit AdvParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamType2, VisType2, StdParamTypes;

type
  IPPresetSettings  = interface (IPParamSettings)
    procedure SetEnvironment(AEnvironment: IPVisualisationEnvironment); cdecl;
    property Environment: IPVisualisationEnvironment write SetEnvironment;
  end;

  IPIntegerSettings = interface (IPParamSettings)
    procedure SetBounds(AMin, AMax: TVInteger); cdecl;
  end;

  IPFloatSettings   = interface (IPParamSettings)
    procedure SetBounds(AMin, AMax, ANan: TVFloat); cdecl;
  end;

implementation

end.

