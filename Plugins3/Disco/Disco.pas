library Disco;

{$mode objfpc}{$H+}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, TVSPSourceUtil, TVSPSources, VisEventImpl, GUIDop,
  AdvGlFunc, SimpleVis, VisAddInput, VisualisationUtils, MainUnit, DiscoUnit,
  GraphX32;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '2.0.0';
  LIB_NAME                    = 'Disco2';
  LIB_DESCRIPTION             = 'Lasers & Strobes';

{$I libmain.inc}

end.

