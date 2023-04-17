unit VisWinType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType;

type
  MVIndex    = LongWord;
  TMuviPoint = packed record
    X,Y: LongInt;
  end;
  IVisWin    = interface (IMInterface)
    ['{F4B84A68-ECD9-46F7-A044-CFA2EC41DA95}']
    function GetPreset: MVIndex; stdcall;
    procedure SetPreset(const APreset: MVIndex); stdcall;
    function GetName: ShortString; stdcall;
    procedure SetName(const ACaption: ShortString); stdcall;
    function GetResolution: TMuviPoint; stdcall;
    procedure SetResolution(const AResolution: TMuviPoint); stdcall;
    function GetKeyboard: MVIndex; stdcall;
    procedure SetKeyboard(const AKeyboard: MVIndex); stdcall;

    property Name: ShortString read GetName write SetName;
    property Preset: MVIndex read GetPreset write SetPreset;
    property Resolution: TMuviPoint read GetResolution write SetResolution;
    property Keyboard: MVIndex read GetKeyboard write SetKeyboard;
 end;

function MuviPoint(const AX,AY: Integer): TMuviPoint;

implementation

{Allgemein}

function MuviPoint(const AX,AY: Integer): TMuviPoint;
begin
  with Result do begin
    X:=AX;
    Y:=AY;
  end;
end;

end.

