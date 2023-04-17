library PicVis;

{$mode objfpc}{$H+}

uses
  Classes, PluginType, MainType, MainUnit, MPluginType, SpectrumData, VPBuffers,
  DllStr, VisType, ClientNetType, VisWinType, StdDllInterfaces, PictureType,
  PictureVisualisations;

function LibGetData(APluginSystem: IMPluginSystem): TPluginLibInfo; stdcall;
begin
  with Result do begin
    Name:='PicVis';
    Description:='Picture Visualisation for Muvi';
    Version:='0.1.0';
    Supported:=(APluginSystem.Future('0.3.1')<>nil);
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

