unit MainType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, MPluginType;

procedure InitPlugin(APluginSystem: IMPluginSystem); stdcall;
procedure PluginDone; stdcall;

var
  PluginSystem: IMPluginSystem2;

implementation

procedure InitPlugin(APluginSystem: IMPluginSystem); stdcall;
begin
  PluginSystem:=IMPluginSystem2(APluginSystem.Future('0.2.0'));
end;

procedure PluginDone; stdcall;
begin
  PluginSystem:=nil;
end;

end.

