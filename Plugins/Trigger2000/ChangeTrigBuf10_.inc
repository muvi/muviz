procedure ChangeTrigBuf10_(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace; BufferIndex: Byte); inline;
var
  AParams          : TTrigBuf10Params absolute Params;
  AWSRec           : TTrigBuf10WS absolute Workspace;
  I,AUsed,ARepeat  : Integer;
begin
  AUsed:=AParams.Used;
  ARepeat:=AParams.DoRepeat;
  if AUsed<0 then AUsed:=0;
  if AUsed>10 then AUsed:=10;
  if ARepeat<0 then ARepeat:=0;
  for I:=0 to ARepeat-1 do PluginSystem.BufferManager.SetBufferItem(AWSRec,(I*AParams.Used)+BufferIndex,AParams.Values[BufferIndex]);
end;

procedure ChangeTrigBuf101(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,0);
end;

procedure ChangeTrigBuf102(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,1);
end;

procedure ChangeTrigBuf103(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,2);
end;

procedure ChangeTrigBuf104(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,3);
end;

procedure ChangeTrigBuf105(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,4);
end;

procedure ChangeTrigBuf106(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,5);
end;

procedure ChangeTrigBuf107(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,6);
end;

procedure ChangeTrigBuf108(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,7);
end;

procedure ChangeTrigBuf109(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,8);
end;

procedure ChangeTrigBuf1010(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  ChangeTrigBuf10_(Dest,Source,Visualisation,Params,Workspace,9);
end;

