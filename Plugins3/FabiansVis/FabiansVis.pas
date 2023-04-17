library FabiansVis;

{$mode objfpc}{$H+}
{$DEFINE NOGRAPHICS}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, TVSPSourceUtil, TVSPSources, VisEventImpl, GUIDop,
  AdvGlFunc, VisualisationUtils, GraphX32, MainUnit, FabiansVisUnit;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '2.0.0';
  LIB_NAME                    = 'Fabians visualisations';
  LIB_DESCRIPTION             = 'Contains Basic Gobos';

{$I libmain.inc}

end.

