unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

