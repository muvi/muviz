unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DMXUnit;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  DMXUnit.Register;
  //Das Plugin hier initialisieren
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

