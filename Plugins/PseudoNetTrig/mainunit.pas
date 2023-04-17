unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

const
  NetTrigIV: ShortString = '<Unbenannt>';

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  with PluginSystem do begin
    RegisterVis('Network Trigger (Integer)','!NT#Int ',nil,['Name'],[vString],[],NetTrigIV,['Output'],[oInteger]);
    RegisterVis('Network Trigger (Real)','!NT#Real',nil,['Name'],[vString],[],NetTrigIV,['Output'],[oReal]);
    RegisterVis('Network Trigger (String)','!NT#Str ',nil,['Name'],[vString],[],NetTrigIV,['Output'],[oString]);
    RegisterVis('Network Trigger (Color)','!NT#Clr ',nil,['Name'],[vString],[],NetTrigIV,['Output'],[oColor]);
    RegisterVis('Network Trigger (Boolean)','!NT#Bool',nil,['Name'],[vString],[],NetTrigIV,['Output'],[oBoolean]);
    RegisterVis('Network Trigger (Call)','!NT#Call',nil,['Name'],[vString],[],NetTrigIV,['Output'],[oCall]);
  end;
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

