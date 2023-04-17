unit StdTags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  //presets
  TAGLISTED            = 'listed';
  TAGNORMALPRESET      = 'preset';
  TAGPREDEFINED        = 'predefined';
  TAGSYSTEM            = 'system';
  TAGNONVISUAL         = 'nonvisual';
  TAGVISUAL            = 'visual';
  TAGHIDDEN            = 'hidden';
  TAGIMPORTED          = 'Imported';
  TAGSUBPRESET         = 'Subpreset';
  TAGDEPRECATED        = 'Deprecated';
  TAGIMPORTEDSUBPRESET = 'Imported.Subpreset';
  TAGSAVE              = 'save';

  {
  //params
  TAGCCOLOR     = 'ccolor';
  TAGAUTOCALL   = 'autocall';
  }

function Secret(ATag: string): Boolean;

implementation

function Secret(ATag: string): Boolean;
begin
  Result:=false;
end;

end.

