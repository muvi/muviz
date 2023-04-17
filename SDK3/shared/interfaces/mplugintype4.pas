unit MPluginType4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, MStrings, ParamType2, PresetType, SpectrumData,
  VisType2, StdParamTypes, TVSPSources;

type
  IMPluginSystem4  = interface (IMPluginSystem)
    ['{DFC9EA7F-4310-4D36-993B-C4775AC0DB57}']
    function GetEmptyString: IString; cdecl;
    function GetEmptyBuffer: IVFloatBuffer; cdecl;
    function GetParamTypeUtil: IPParamTypeUtil; cdecl;
    function GetPresetUtil: IPPresetUtil; cdecl;
    function GetSpectrumData: ISpectrumData; cdecl;
    function GetNoCanvas: IPCanvas; cdecl;
    function GetSources: ITVSPSources; cdecl;
  end;

var
  PluginSystem: IMPluginSystem4;

implementation

end.

