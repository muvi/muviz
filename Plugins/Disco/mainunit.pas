unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, DiscoUnit, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  with PluginSystem do begin
    RegisterVis('Strobe',' STROBE ', @DrawStrobe, ['Blinkzeit','Dunkelzeit'], [VInteger,VInteger],[],StrobeIV,[],[],Sizeof(TStrobeWS),@InitStrobe);
    RegisterVis('Rythm Strobe','RSTROBE ', @DrawRStrobe, ['Grenzwert','Streckfaktor'], [VReal,VReal],[],RStrobeIV,[],[]);
    RegisterVis('Lines',' LINES  ', @DrawLines, ['Linienlänge','Abstand zum Mittelpunkt','Grenzwert'],[VReal,VReal,VReal],[],LinesIV,[],[],SizeOf(TLinesWS),@InitLines);
    RegisterVis('Rythm Circle','CRCIRCLE', @DrawRCircle, ['Radius','Geschwindigkeit'], [VReal,VReal],[],CircleIV,[],[],SizeOf(TCircleWS),@InitCircle);
    //RegisterVis(VisType('Laser',' LASER  ', @DrawLaser, ['Linienlänge','Abstand zum Mittelpunkt'], [VReal,VReal],LinesIV,nil{ChangeLinesParams},SizeOf(TLinesWS),@InitLaser));
    //RegisterVis(VisType('Off Laser','OFFLASER', @DrawLaser, ['Linienlänge','Abstand zum Mittelpunkt'], [VReal,VReal],LinesIV,nil{ChangeLinesParams},SizeOf(TLinesWS),@InitOffLaser));
  end;
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

