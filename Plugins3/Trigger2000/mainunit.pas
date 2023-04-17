unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Trigger2000Unit;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  Trigger2000Unit.Register;
  //Das Plugin hier initialisieren
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

