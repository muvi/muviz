library PicVis;

{$mode objfpc}{$H+}
{$DEFINE NOGRAPHICS}

uses
  Classes, PluginType, MPluginType4, MStrings, MTypes, TagType, VPBuffers,
  ImportType, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes,
  CanvasType, StdTags, TVSPSourceUtil, TVSPSources, VisEventImpl, GUIDop,
  AdvGlFunc, SimpleVis, VisAddInput, VisualisationUtils, MainUnit, PicVisUnit,
  GraphX32, Forms, Interfaces;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '2.0.0';
  LIB_NAME                    = 'PicVis2';
  LIB_DESCRIPTION             = 'Picture Visualisation for Muvi';

{$I libmain.inc}

end.

