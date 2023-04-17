unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, GeometryUnit, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  with PluginSystem do begin
    RegisterVis('N-Eck','nANGLE__', @DrawNAngle, ['Farbe','<Unbenutzt>','<Unbenutzt>','Radius','Eckenanzahl','X-Position','Y-Position','Drehung','Sichtbar'], [VReal,VInteger,VReal,VReal,VReal,vBoolean],[],NAngleIV,[],[],0);
    RegisterVis('Trigger Circle','CIRCLE__', @DrawTrigCircle, ['Farbe 1','Farbe 2','<Unbenutzt>','Mindestradius','X-Position','Y-Position','Drehung','Streckfaktor','Daten','Sichtbar'], [VReal,VReal,VReal,VReal,VReal,VBuffer,vBoolean],[],TrigCircleIV,[],[],0);
    RegisterVis('Smoothed Trigger Circle','S~CIRCLE', @DrawTrigSmoothCircle, ['Farbe 1','Farbe 2','<Unbenutzt>','Mindestradius','X-Position','Y-Position','Drehung','Streckfaktor','Daten','Sichtbar'], [VReal,VReal,VReal,VReal,VReal,VBuffer,vBoolean],[],TrigCircleIV,[],[],0);
    RegisterVis('Rain','__RAIN__', @DrawTrigRain, ['Farbe 1','Farbe 2','<Unbenutzt>','Regenlänge','Regenabstand','Geschwindikeit','Drehung','Streckfaktor','Daten','Sichtbar'], [VReal,VReal,VReal,VReal,VReal,VBuffer,vBoolean],[],TrigRainIV,[],[],SizeOf(TRainWS),@InitTrigRain);
    RegisterVis('Trigger Partikel','TrigPart', @DrawTrigPart, ['Farbe Oben Links','Farbe Oben Rechts','Farbe Unten Links','Farbe Unten Rechts','X-Werte','Y-Werte','X-Streckfaktor','Y-Streckfaktor','X-Offset','Y-Offset','Partikelgröße','Sichtbar'], [vColor,vBuffer,vBuffer,vReal,vReal,vReal,vReal,vReal,vBoolean],[],TrigPartIV,[],[]);
    RegisterVis('Pro Lightning','ProLgthn', @DrawProLightning, ['Linienfarbe','Hintergrundfarbe','Drehung','Drehungsdiffusität','Breite'], [vColor,vColor,vReal,vReal,vReal],[],ProLightIV,[],[],SizeOf(TProLightWS),@InitProLightning);
    RegisterVis('Smoothy Line','SmoothLn', @DrawSmoothyLine, ['Farbe','Position','Ausbreitung','Vertikal'], [vColor,vReal,vReal,vBoolean],[nil,nil,@ChangeSmoothyLineWidth,@ChangeSmoothyLineOrientation],SmoothyLineIV,[],[],SizeOf(TSmoothyLineWS),@ChangeSmoothyLineOrientation);
    //RegisterVis('Lightning 2.0','LIGHT2.0',@DrawVisLightning,['Grenzwert'],[vReal],[],LightningIV,[],[]);
    RegisterVis('Smiley','Smiley:)', @DrawSmiley, ['Größe','Größe Linkes Auge','Größe Rechtes Auge','Mundwinkel','Mundöffnung','Farbe','Farbe Linkes Auge','Farbe Rechtes Auge','Mundfarbe','Daten','Streckfaktor','Daten Linkes Auge','Streckfaktor Linkes Auge','Daten Rechtes Auge','Streckfaktor Rechtes Auge','Munddaten','Mundstreckung','Mundöffnungsdaten','Mundöffnungsstreckung'],[vReal,vReal,vReal,vReal,vReal,vColor,vColor,vColor,vColor,vBuffer,vReal,vBuffer,vReal,vBuffer,vReal,vBuffer,vReal,vBuffer,vReal],[@ChangeSmileyBodySize,nil,nil,nil,nil,nil,nil,nil,nil,@ChangeSmileyData,nil,@ChangeSmileyLeftEyeData,nil,@ChangeSmileyRightEyeData,nil,@ChangeSmileyMouthData,nil,@ChangeSmileyMouthOpenData],SmileyIV,[],[],SizeOf(TSmileyWS),@InitSmiley,@FreeSmiley);
  end;
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

