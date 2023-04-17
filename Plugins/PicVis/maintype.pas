unit MainType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, MPluginType;

procedure InitPlugin(APluginSystem: IMPluginSystem); stdcall;
procedure PluginDone; stdcall;

var
  PluginSystem: IMPluginSystem3_1;

implementation

procedure InitPlugin(APluginSystem: IMPluginSystem); stdcall;
begin
  PluginSystem:=IMPluginSystem3_1(APluginSystem.Future('0.3.1'));
end;

procedure PluginDone; stdcall;
begin
  PluginSystem:=nil;
end;

end.

