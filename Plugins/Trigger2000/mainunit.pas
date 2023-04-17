unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType,
  DllStr, MainType, AdvancedTriggers, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  with PluginSystem do begin
    RegisterVis('BPM','!BPMTrig',@TrigBPM,[],[],[],vpDefault,['BPM'],[oReal]);
    RegisterVis('BPM gefunden','!BPMFoundTrig',@TrigBPMFound,[],[],[],vpDefault,['BPM gefunden'],[oBoolean]);
    RegisterVis('Beatposition','!BeatPos',@TrigBeatPos,[],[],[],vpDefault,['Beatposition'],[oInteger]);
    RegisterVis('BeatMax','!BeatMax',@TrigBeatMax,[],[],[],vpDefault,['BeatMax'],[oInteger]);
    RegisterVis('General Beat Trigger','!BeatTrg',@TrigBeat,[],[],[],vpDefault,['Beat'],[oBoolean]);
    RegisterVis('Beat Stop Caller','!BeatCal',@TrigBeatStopCall,[],[],[],vpDefault,['Anschluss'],[oCall],SizeOf(TBSCWS),@InitBeatStopCall);
    RegisterVis('Frequenzy Beat Trigger','!FS-Beat',@TrigBeatFS,['Frequenzband'],[vInteger],[],FSBeatIV,['Beat'],[oBoolean]);
    RegisterVis('Wavedaten','!BUFWAVE',@TrigWData,[],[],[],vpDefault,['Wavedaten'],[oBuffer]);
    RegisterVis('FFT','!BUF_FFT',@TrigFFTData,[],[],[],vpDefault,['FFT'],[oBuffer]);
    RegisterVis('Buffer 10','!BUF__10',nil,['Verwendet','Wiederholen','Wert 1','Wert 2','Wert 3','Wert 4','Wert 5','Wert 6','Wert 7','Wert 8','Wert 9','Wert 10'],[vInteger,vInteger,vReal,vReal,vReal,vReal,vReal,vReal,vReal,vReal,vReal,vReal],[@ChangeTrigBuf10,@ChangeTrigBuf10,@ChangeTrigBuf101,@ChangeTrigBuf102,@ChangeTrigBuf103,@ChangeTrigBuf104,@ChangeTrigBuf105,@ChangeTrigBuf106,@ChangeTrigBuf107,@ChangeTrigBuf108,@ChangeTrigBuf109,@ChangeTrigBuf1010],TrigBuf10IV,['Puffer'],[oBuffer],SizeOf(TTrigBuf10WS),@InitTrigBuf10,@FreeTrigBuf10);
    RegisterVis('Multi Bufferer','!MultiBf',nil,['Input'],[vBuffer],[@ChangeMultiBuf],MultiBufIV,['Ausgang 1','Ausgang 2','Ausgang 3','Ausgang 4'],[oBuffer,oBuffer,oBuffer,oBuffer]);
    RegisterVis('Buffer Cutter','!BuffCut',nil,['Anfang','Ende','Input'],[vInteger,vInteger,vBuffer],[@ChangeBufCut,@ChangeBufCut,@ChangeBufCut],BufCutIV,['Output'],[oBuffer]);
    RegisterVis('Buffer Cutter (Relative)','!BufCutR',nil,['Anfang','Ende','Input'],[vReal,vReal,vBuffer],[@ChangeBufCutR,@ChangeBufCutR,@ChangeBufCutR],BufCutRIV,['Output'],[oBuffer]);
    RegisterVis('Buffer Conector','!BufConn',@VisBufCon,['Buffer 1','Buffer 2','Buffer 3','Buffer 4','Buffer 5'],[vBuffer,vBuffer,vBuffer,vBuffer,vBuffer],[@ChangeBufCon,@ChangeBufCon,@ChangeBufCon,@ChangeBufCon,@ChangeBufCon],BufConIV,['Output'],[oBuffer],SizeOf(TBufConWS),@InitBufCon,@FreeBufCon);
    RegisterVis('Linear Buffer','!Lin_Buf',nil,['<Unbenutzt>','<Unbenutzt>','<Unbenutzt>','Start','Ende','Größe'],[vReal,vReal,vInteger],[@ChangeLinBufValues,@ChangeLinBufValues,@ChangeLinBuf],LinBufIV,['Output'],[oBuffer],SizeOf(TLinBufWS),@InitLinBuf,@FreeLinBuf);
    RegisterVis('Buffer Getter','!GetBuf_',@VisBufGet,['<Unbenutzt>','<Unbenutzt>','<Unbenutzt>','Puffer','Index'],[vBuffer,vInteger],[],BufGetIV,['Output'],[oReal]);
    RegisterVis('Buffer Getter (Relative)','!GetBufR',@VisBufGetR,['<Unbenutzt>','<Unbenutzt>','<Unbenutzt>','Puffer','Position'],[vBuffer,vReal],[],BufGetRIV,['Output'],[oReal]);
    RegisterVis('Buffer Calculation','!BufCalc',@VisBufCalc,['Buffer','Calculation Input'],[vBuffer,vReal],[@ChangeBufCalc],BufCalcIV,['Buffer','Calculation Output','Next Output'],[oBuffer,oReal,oCall],SizeOf(TBufCalcWS),@InitBufCalc,@FreeBufCalc);
    RegisterVis('Weichspüler','!Smoothr',@TrigSmooth,['<Unbenutzt>','<Unbenutzt>','<Unbenutzt>','Input','Faktor'],[vReal,vReal],[],TrigSmoothIV,['Output'],[oReal],SizeOf(vpReal),@InitTrigSmooth);

    RegisterVis('Beatposition (Real)','!BeatPoF',@TrigBeatPosReal,[],[],[],vpDefault,['Beatposition'],[oReal]);
    RegisterVis('BSG','!BSG    ',@TrigBeatSpeed,['Max per Beat'],[vReal],[],BSGIV,['Speed'],[oReal],SizeOf(TBSGWS),@InitTrigBeatSpeed);

    RegisterVis('Now','!NOW    ',@TrigNow,[],[],[],vpDefault,['Second', 'Minute', 'Hour', 'Day', 'Month', 'Year'],[oInteger, oInteger, oInteger, oInteger, oInteger, oInteger]);
  end;
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

