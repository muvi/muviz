library Trigger2000;

{$mode objfpc}{$H+}
{$DEFINE NOGRAPHICS}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, TVSPSourceUtil, TVSPSources, VisEventImpl, GUIDop,
  AdvGlFunc, GraphX32, AdvFunc, MainUnit, Trigger2000Unit, SimpleVis, ParamOp,
  ParamTypes, VisualisationUtils;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '2.0.0';
  LIB_NAME                    = 'Trigger2000_2';
  LIB_DESCRIPTION             = 'Advanced Triggers';

{$I libmain.inc}

end.

