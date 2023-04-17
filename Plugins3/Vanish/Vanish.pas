library Vanish;

{$mode objfpc}{$H+}
{$DEFINE NOGRAPHICS}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, TVSPSourceUtil, TVSPSources, VisEventImpl, GUIDop,
  AdvGlFunc, SimpleVis, VisAddInput, VisualisationUtils, MainUnit, VanishUnit,
  GraphX32;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '2.0.0';
  LIB_NAME                    = 'Vanish2';
  LIB_DESCRIPTION             = 'Different Transitions';

{$I libmain.inc}

end.

