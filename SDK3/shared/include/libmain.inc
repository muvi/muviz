function LibGetData(APluginSystem: IMPluginSystem): TPluginLibInfo; stdcall;
begin
  with Result do begin
    Name:=LIB_NAME;
    Description:=LIB_DESCRIPTION;
    Version:=LIB_VERSION;
    Supported:=(APluginSystem.Future(REQUIRED_VERSION)<>nil);
  end;
end;

procedure InitPlugin(APluginSystem: IMPluginSystem); stdcall;
begin
  //register units
  PluginSystem:=IMPluginSystem4(APluginSystem.Future(REQUIRED_VERSION));
  EmptyString:=PluginSystem.GetEmptyString;
  EmptyBuffer:=PluginSystem.GetEmptyBuffer;
  ParamTypeUtil:=PluginSystem.GetParamTypeUtil;
  PresetUtil:=PluginSystem.GetPresetUtil;
  VisualisationUtil:=PresetUtil;
  NoCanvas:=PluginSystem.GetNoCanvas;
  AdvSpectrumData:=ISpectrumData3(PluginSystem.GetSpectrumData.Future('0.5.0'));
  SourceUtil:=PluginSystem.GetSources;

  Init_Plugin;
end;

procedure PluginDone; stdcall;
begin
  Plugin_Done;
  //unregister units
  SourceUtil:=nil;
  AdvSpectrumData:=nil;
  NoCanvas:=nil;
  VisualisationUtil:=nil;
  PresetUtil:=nil;
  ParamTypeUtil:=nil;
  EmptyBuffer:=nil;
  EmptyString:=nil;
  PluginSystem:=nil;
end;

exports
  LibGetData, InitPlugin, PluginDone;

