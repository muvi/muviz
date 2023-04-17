library Geometry;

{$mode objfpc}{$H+}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, TVSPSourceUtil, TVSPSources, VisEventImpl, GUIDop,
  AdvGlFunc, SimpleVis, VisualisationUtils, GraphX32, AdvFunc, AdvCoord,
  GeometryUnit, MainUnit;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '2.0.0';
  LIB_NAME                    = 'Geometry2';
  LIB_DESCRIPTION             = 'Geometric Elements & Rain';

{$I libmain.inc}

end.

