unit DoorUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Doors;

type
  IDoorway     = interface
    function Enter(AEntryCode: TEntryCode): Boolean; cdecl;
  end;

  IDoorManager = interface
    function NewDoorway: IDoorway; cdecl;
    function NewEntryCode: TEntryCode; cdecl;
  end;

var
  DoorManager: IDoorManager;

implementation

end.

