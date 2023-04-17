library Example;

{$mode objfpc}{$H+}

uses
  Classes, MainUnit, PluginType, MPluginType4, MStrings, MTypes, TagType,
  VPBuffers, ParamType2, PresetType, VisType2, SpectrumData, StdParamTypes;

const
  REQUIRED_VERSION            = '0.4.0';
  LIB_VERSION                 = '1.0.0';
  LIB_NAME                    = 'Example Muvi Plugin';
  LIB_DESCRIPTION             = 'Muvi Plugin SDK';

{$I libmain.inc}

end.

