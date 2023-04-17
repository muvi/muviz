library DMX;

{$mode objfpc}{$H+}
{$DEFINE NOGRAPHICS}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, AdvParamType, TVSPSourceUtil, TVSPSources, VisEventImpl,
  GUIDop, AdvGlFunc, SimpleVis, VisAddInput, ParamTypes, VisualisationUtils,
  MainUnit, DMXUnit, OpenDMX, GraphX32;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '1.0.0';
  LIB_NAME                    = 'DMX';
  LIB_DESCRIPTION             = 'DMX settings and Open DMX implementation';

{$I libmain.inc}

end.

