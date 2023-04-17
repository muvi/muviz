unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, VanishUnit, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  with PluginSystem do begin
    RegisterVis('Vanish','__VANISH',@VisVanish,[],[],[],vpDefault,[],[]);
    RegisterVis('Rotate','__ROTATE',@VisRotate,['Geschwindigkeit'],[vReal],[],RotateIV,[],[]);
    RegisterVis('Move','____MOVE',@VisMove,['X','Y'],[vInteger,vInteger],[],MoveIV,[],[]);
    RegisterVis('Zoom','____ZOOM',@VisZoom,['X','Y'],[vReal,vReal],[],ZoomIV,[],[]);
    RegisterVis('Crazy Scale','CRAZYSCAL',@VisCrazyScale,['X','Y'],[vReal,vReal],[],ZoomIV,[],[]);
  end;
  //Das Plugin hier initialisieren
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

