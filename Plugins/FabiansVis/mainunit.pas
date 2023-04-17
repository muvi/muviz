unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, UVisGobo, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  PluginSystem.RegisterVis('VisGobo','GoboVIsD', @procVisGobo, ['Modus','GoboGröße','GoboAnzahl', 'Mehrfarbige Gobos','Grenzwert','Streckfaktor'], [VInteger,VInteger,VInteger,VBoolean,vReal,vReal],[nil,nil,@ChangeGoboParams],GoboIV,[],[],SizeOf(TWSRec),@InitGobo,@FreeGobo);
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

