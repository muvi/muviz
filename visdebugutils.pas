unit VisDebugUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2;

type
  IDebugParam = interface
    ['{51EBA60A-38C9-4E45-8853-4A35BBA22E09}']
    function GetPrototype: IPParamPrototype; experimental;
  end;

  IDebugAttachObject = interface
    ['{2F4FEADF-FB7C-49E4-B2AF-21FE3B6B0B4C}']
    function GetAttachedParams: string; experimental;
  end;

implementation

end.

