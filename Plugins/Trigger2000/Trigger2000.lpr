library Trigger2000;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}{$R Trigger2000.rc}{$ENDIF}

uses
  Classes, MainType, MainUnit, MPluginType, VPBuffers,
  DllStr, VisType, ClientNetType, VisWinType, StdDllInterfaces,
  AdvancedTriggers, SpectrumData, PluginType, GUIDop;

function LibGetData(APluginSystem: IMPluginSystem): TPluginLibInfo; stdcall;
begin
  with Result do begin
    Name:='Trigger 2000';
    Description:='Advanced Triggers';
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

