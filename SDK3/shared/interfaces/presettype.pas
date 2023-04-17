unit PresetType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, ParamType2, TagType, MStrings;

type
  IITaggedPresets           = interface
    function Get(Index: LongInt): IPVisualisation; cdecl;
    function GetCount: LongInt; cdecl;
    property Count: LongInt read GetCount;
    property Items[Index: LongInt]: IPVisualisation read Get; default;
  end;

  IPPresetUtil              = interface (IPVisualisationUtil)
    function GetPreset(AID: TPPresetID): IPVisualisation; cdecl;
    function GetPreset(AName: IString): IPVisualisation; cdecl;
    function GetPresetTags: ITags; cdecl;
    function GetPresetsWithTag(ATag: IString): IITaggedPresets; cdecl;

    property Presets[AID: TPPresetID]: IPVisualisation read GetPreset; default;
    //warning: inefficient and may return nil
    property PresetsByName[AName: IString]: IPVisualisation read GetPreset;
    property PresetTags: ITags read GetPresetTags;
    property PresetsWithTag[ATag: IString]: IITaggedPresets read GetPresetsWithTag;
  end;

var
  //the global unique preset util
  PresetUtil: IPPresetUtil = nil;

implementation

end.

