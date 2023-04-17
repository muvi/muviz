unit MPlugin; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MInterfacedObject, PluginType;

type
  TMPlugin = class (TMInterfacedObject, IMPlugin)
  protected
    function GetID: MVPluginID; stdcall; virtual; abstract;
    function GetName: ShortString; stdcall; virtual;
    procedure Init; stdcall; virtual;
    procedure Done; stdcall; virtual;
  end;

implementation

{TMPlugin}

function TMPlugin.GetName: ShortString; stdcall;
begin
  Result:='Unnamed';
end;

procedure TMPlugin.Init; stdcall;
begin
  //do nothing
end;

procedure TMPlugin.Done; stdcall;
begin
  //do nothing
end;

end.

