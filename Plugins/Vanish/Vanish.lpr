library Vanish;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}{$R Vanish.rc}{$ENDIF}

uses
  Classes, PluginType, MainType, MainUnit, MPluginType, SpectrumData, VPBuffers,
  DllStr, VisType, ClientNetType, VisWinType, StdDllInterfaces, VanishUnit;

function LibGetData(APluginSystem: IMPluginSystem): TPluginLibInfo; stdcall;
begin
  with Result do begin
    Name:='Vanish';
    Description:='Different Transitions';
    Version:='1.0.0';
    Supported:=(APluginSystem.Future('0.2.0')<>nil);
  end;
end;

procedure InitPlugin(APluginSystem: IMPluginSystem); stdcall;
begin
  MainType.InitPlugin(APluginSystem);
  Init_Plugin;
end;

procedure PluginDone; stdcall;
begin
  Plugin_Done;
  MainType.PluginDone;
end;

exports
  LibGetData, InitPlugin, PluginDone;
end.

