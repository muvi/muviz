unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, PictureVisualisations, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  with PluginSystem do begin
    RegisterVis('Picture Drawer','PicDraw ',@DrawPicture,['Pfad'],[vString],[@ChangePicDraw],PicDrawIV,[],[],SizeOf(TPicDrawWS),@InitPicDraw,@FreePicDraw);
    RegisterVisC('Canvas','CANVAS01',@DrawCanvas,['Breite','Höhe'],[vReal,vReal],[],CanvasIV,[],[],['Canvas'],SizeOf(TCanvasWS),@InitCanvas,@FreeCanvas);
  end;
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

