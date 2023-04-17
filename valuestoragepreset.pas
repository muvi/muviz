unit ValueStoragePreset;

{$mode objfpc}{$H+}

interface

uses
  ValueStorages, StdTags, PresetType, VisType2, MStrings, VisAddInput;

procedure Register;

implementation

procedure Register;
begin
  with PresetUtil[PIDTEMPSTORAGE] do begin
    AddTag(TAGSYSTEM);
    AddTag(TAGHIDDEN);
    AddTag(TAGPREDEFINED);
    AddTag(TAGNONVISUAL);
    //add system inputs
    AddInput(This, NAMEINPUTNAME, 'Temporary Storage Preset');
  end;
end;

end.

