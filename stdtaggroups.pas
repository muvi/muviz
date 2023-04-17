unit StdTagGroups;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TagUnit;

var
  PresetTags, ParamTags: TTagGroup;

implementation

initialization
  PresetTags:=TTagGroup.Create;
  ParamTags:=TTagGroup.Create;
finalization
  PresetTags.Destroy;
  ParamTags.Destroy;
end.

