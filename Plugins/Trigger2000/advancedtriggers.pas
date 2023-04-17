unit AdvancedTriggers; 

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisType, VPBuffers, MainType, PluginType, Math, SysUtils;

type
  TBSCWS            = vpBool;
  TBeatFSParams     = vpInt;

  TTrigBuf10Params  = packed record
    Used,DoRepeat: vpInt;
    Values       : array [0..9] of vpReal;
  end;

  TMultiBufParams   = vpRealBuffer;
  TBufCutParams     = packed record
    Start,Stop: vpInt;
    Buffer    : vpRealBuffer;
  end;
  TBufCutRParams    = packed record
    Start,Stop: vpReal;
    Buffer    : vpRealBuffer;
  end;
  TBufConParams     = packed array [0..4] of vpRealBuffer;
  TTrigBuf10WS      = vpRealBuffer;
  TBufConWS         = vpRealBuffer;
  TLinBufParams     = packed record
    Start,Stop: vpReal;
    Count     : vpInt;
  end;
  TLinBufWS         = vpRealBuffer;
  TBufGetParams     = packed record
    Buffer  : vpBuffer;
    Index   : vpInt;
  end;
  TBufGetRParams    = packed record
    Buffer  : vpBuffer;
    Position: vpReal;
  end;
  TTrigSmoothParams = packed record
    Input,Fac: vpReal;
  end;
  TBufCalcParams    = packed record
    Buffer: vpRealBuffer;
    Input : vpReal;
  end;
  TBufCalcWS        = record
    Buffer     : vpRealBuffer;
    ActiveIndex: Integer;
  end;
  TBSGParams        = vpReal;
  TBSGWS            = record
    LastBeatPos: Real;
    //NegBeat    : Real;
  end;

const
  FSBeatIV    : TBeatFSParams = 0;
  TrigBuf10IV : TTrigBuf10Params = (Used: 10; DoRepeat: 1; Values: (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));
  MultiBufIV  : vpRealBuffer = (P1:0;P2:0;P3:0;P4:0);
  BufCutIV    : TBufCutParams = (Start: 0; Stop: 100000; Buffer: (P1:0;P2:0;P3:0;P4:0));
  BufCutRIV   : TBufCutRParams= (Start: 0.0; Stop: 100.0; Buffer: (P1:0;P2:0;P3:0;P4:0));
  BufConIV    : TBufConParams = ((P1:0;P2:0;P3:0;P4:0),(P1:0;P2:0;P3:0;P4:0),(P1:0;P2:0;P3:0;P4:0),(P1:0;P2:0;P3:0;P4:0),(P1:0;P2:0;P3:0;P4:0));
  LinBufIV    : TLinBufParams = (Start: 0.0; Stop: 100.0; Count: 100);
  BufGetIV    : TBufGetParams = (Buffer: (P1:0;P2:0;P3:0;P4:0); Index: 0);
  BufGetRIV   : TBufGetRParams= (Buffer: (P1:0;P2:0;P3:0;P4:0); Position: 0.0);
  TrigSmoothIV: TTrigSmoothParams = (Input: 0.0; Fac: 2.0);
  BufCalcIV   : TBufCalcParams = (Buffer: (P1:0;P2:0;P3:0;P4:0); Input: 0.0);
  BSGIV       : TBSGParams = 1.0;

var
  VPEMPTYOUTPUT: vpCall = ();

procedure TrigBPM(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBPMFound(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeatPos(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeatPosReal(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeatMax(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeat(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitBeatStopCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeatStopCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeatFS(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure TrigWData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigFFTData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
//procedure TrigDoNothing(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitTrigBuf10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeTrigBuf10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure ChangeTrigBuf101(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf102(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf103(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf104(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf105(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf106(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf107(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf108(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf109(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeTrigBuf1010(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure ChangeMultiBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeBufCut(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeBufCutR(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitLinBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeLinBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeLinBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeLinBufValues(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure VisBufGet(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisBufGetR(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure TrigSmooth(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitTrigSmooth(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitTrigBeatSpeed(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBeatSpeed(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigNow(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

implementation

procedure TrigBPM(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: vpReal;
begin
  AResult:=Source.BPM;
  Visualisation.SetOutput(0,AResult);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AResult);
end;

procedure TrigBPMFound(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: vpBool;
begin
  AResult:=(Source.BPMFound=bpmTrue);
  Visualisation.SetOutput(0,AResult);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AResult);
end;

procedure TrigBeatPos(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Visualisation.SetOutput(0,Source.Beat.Pos);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,Source.Beat.Pos);
end;

procedure TrigBeatPosReal(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: vpReal;
begin
  AResult:=ISpectrumData2(Source.Future('0.4.0')).BeatPos;
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigBeatMax(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Visualisation.SetOutput(0,Source.Beat.Max);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,Source.Beat.Max);
end;

procedure TrigBeat(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: vpBool;
begin
  AResult:=not Source.Beat.Free;
  Visualisation.SetOutput(0,AResult);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AResult);
end;

procedure InitBeatStopCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TBSCWS absolute Workspace;
begin
  AWSRec:=false;
end;

procedure TrigBeatStopCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: vpBool;
  AWSRec : TBSCWS absolute Workspace;
begin
  AResult:=Source.Beat.Togg{ (Source.BPMFound=bpmTrue)};
  if AResult<>AWSRec then begin
    Visualisation.SetOutput(0,vpDefault);
    //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,0);
    AWSRec:=AResult;
  end;
end;

procedure TrigBeatFS(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: vpBool;
  AParams: TBeatFSParams absolute Params;
begin
  AResult:=Source.BeatAt[AParams];
  Visualisation.SetOutput(0,AResult);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AResult);
end;

{procedure TrigDoNothing(var bmp: TBitmap32; const Source: ISpectrumData; const Visualisation: TVisualisation; const Params; var Workspace); stdcall;
begin

end;}

procedure TrigWData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: VPRealBuffer;
begin
  AResult:=Source.GetBuffer(WaveDataBI,0);//PluginSystem.BufferManager.ToVPRealBuffer(Source.WaveDataCount,Source.GetBuffer(WaveDataBI,0));
  Visualisation.SetOutput(0,AResult);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AResult);
end;

procedure TrigFFTData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AResult: VPRealBuffer;
begin
  AResult:=Source.GetLevelBuffer(0);
  Visualisation.SetOutput(0,AResult);
end;

procedure InitTrigBuf10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams          : TTrigBuf10Params absolute Params;
  AWSRec           : TTrigBuf10WS absolute Workspace;
  I,J,AUsed,ARepeat: Integer;
begin
  AUsed:=AParams.Used;
  ARepeat:=AParams.DoRepeat;
  if AUsed<0 then AUsed:=0;
  if AUsed>10 then AUsed:=10;
  if ARepeat<0 then ARepeat:=0;
  AWSRec:=PluginSystem.BufferManager.NewBuffer(AParams.Used*AParams.DoRepeat);
  for I:=0 to ARepeat-1
    do for J:=0 to AUsed-1
      do PluginSystem.BufferManager.SetBufferItem(AWSRec,(I*AParams.Used)+J,AParams.Values[J]);
  Visualisation.SetOutput(0,AWSRec);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AWSRec);
end;

{$I ChangeTrigBuf10_.inc}

procedure ChangeTrigBuf10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  FreeTrigBuf10(Dest,Source,Visualisation,Params,Workspace);
  InitTrigBuf10(Dest,Source,Visualisation,Params,Workspace);
end;

procedure FreeTrigBuf10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec            : TTrigBuf10WS absolute Workspace;
begin
  PluginSystem.BufferManager.DisposeBuffer(AWSRec);
end;

procedure ChangeMultiBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TMultiBufParams absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
  Visualisation.SetOutput(1,AParams);
  Visualisation.SetOutput(2,AParams);
  Visualisation.SetOutput(3,AParams);
  {with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AParams);
  with Visualisation.VisOutputs[1] do DoSet(bmp,Source,Data,AParams);
  with Visualisation.VisOutputs[2] do DoSet(bmp,Source,Data,AParams);
  with Visualisation.VisOutputs[3] do DoSet(bmp,Source,Data,AParams);}
end;

{type
  vpRealBufferElem= MVFloat;
  vpRealBuffer2   = array [0..0] of vpRealBufferElem;
  pvpRealBuffer2  = ^vpRealBuffer2;
  vpRealBuffer_   = packed record
    Size: LongWord;
    R1  : LongWord;
    R2  : Int64;
    Dest: Pointer;
    R3  : LongWord;
    R4  : UInt64;
  end;

const
  vpBufElemSize = SizeOf(vpRealBufferElem);}

procedure ChangeBufCut(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams     : TBufCutParams absolute Params;
  ABuffer     : vpRealBuffer;
  //ABuffer2    : vpRealBuffer_ absolute ABuffer;
  AStart,ACount: Cardinal;
begin
  if AParams.Start>=0
    then AStart:=AParams.Start
    else AStart:=0;
  ACount:=AParams.Stop-AStart;
  if ACount<0 then ACount:=0;
  ABuffer:=PluginSystem.BufferManager.Cut(AParams.Buffer,AStart,ACount);
  Visualisation.SetOutput(0,ABuffer);
  {with PluginSystem.BufferManager do begin
  ABuffer:=AParams.Buffer;
  if AParams.Start>=//ABuffer2.Size
    then AStart:=ABuffer2.Size-1
    else if AParams.Start<0
      then AStart:=0
      else AStart:=AParams.Start;
  if AParams.Stop>=ABuffer2.Size
    then AStop:=ABuffer2.Size-1
    else if AParams.Stop<0
      then AStop:=0
      else AStop:=AParams.Stop;
  if AStart<=AStop then begin
    ABuffer2.Dest+=AStart*vpBufElemSize;
    ABuffer2.Size:=AStop-AStart;
  end else ABuffer2.Size:=0;  }
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ABuffer);
end;

procedure ChangeBufCutR(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams     : TBufCutRParams absolute Params;
  AParams2    : TBufCutParams;
  BufferSize  : Real;
  {ABuffer     : vpRealBuffer;
  ABuffer2    : vpRealBuffer_ absolute ABuffer;
  AStart,AStop: Cardinal;
  ASizeFac    : Real; }
begin
  AParams2.Buffer:=AParams.Buffer;
  BufferSize:=PluginSystem.BufferManager.SizeOfBuffer(AParams.Buffer);
  AParams2.Start:=Round((AParams.Start/100.0)*BufferSize);
  AParams2.Stop:=Round((AParams.Stop/100.0)*BufferSize);
  ChangeBufCut(Dest,Source,Visualisation,AParams2,Workspace)
  {GetVisParams(Visualisation,AParams);
  ABuffer:=AParams.Buffer;
  ASizeFac:=ABuffer2.Size/100.0;
  if AParams.Start>=100.0
    then AStart:=ABuffer2.Size-1
    else if AParams.Start<0.0
      then AStart:=0
      else AStart:=Round(AParams.Start*ASizeFac);
  if AParams.Stop>=100.0
    then AStop:=ABuffer2.Size-1
    else if AParams.Stop<0.0
      then AStop:=0
      else AStop:=Round(AParams.Stop*ASizeFac);
  if AStart<=AStop then begin
    ABuffer2.Dest+=AStart*vpBufElemSize;
    ABuffer2.Size:=AStop-AStart;
  end else ABuffer2.Size:=0;
  with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ABuffer);}
end;

procedure InitBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec     : TBufConWS absolute Workspace;
begin
  AWSRec:=VPEMPTYBUFFER;
end;

const
  vpBufElemSize = SizeOf(vpRealBufferItem);

procedure VisBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams    : TBufConParams absolute Params;
  AWSRec     : TBufConWS absolute Workspace;
  I,ASize    : Integer;
  ABufferData: Pointer;
begin
  ABufferData:=PluginSystem.BufferManager.GetBufferData(AWSRec);
  for I:=0 to 4 do begin
    ASize:=PluginSystem.BufferManager.SizeOfBuffer(AParams[I])*vpBufElemSize;
    Move(PluginSystem.BufferManager.GetBufferData(AParams[I])^,ABufferData^,ASize);
    ABufferData+=ASize;
  end;
end;

procedure ChangeBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams    : TBufConParams absolute Params;
  AWSRec     : TBufConWS absolute Workspace;
  I,ASize    : Integer;
  ABufferData: Pointer;
begin
  PluginSystem.BufferManager.DisposeBuffer(AWSRec);

  ASize:=0;
  for I:=0 to 4 do ASize+=PluginSystem.BufferManager.SizeOfBuffer(AParams[I]);
  AWSRec:=PluginSystem.BufferManager.NewBuffer(ASize);
  ABufferData:=PluginSystem.BufferManager.GetBufferData(AWSRec);
  for I:=0 to 4 do begin
    ASize:=PluginSystem.BufferManager.SizeOfBuffer(AParams[I])*vpBufElemSize;
    Move(PluginSystem.BufferManager.GetBufferData(AParams[I])^,ABufferData^,ASize);
    ABufferData+=ASize;
  end;
  Visualisation.SetOutput(0,AWSRec);
end;

procedure FreeBufCon(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TBufConWS absolute Workspace;
begin
  PluginSystem.BufferManager.DisposeBuffer(AWSRec);
end;

procedure InitLinBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TLinBufParams absolute Params;
  AWSRec : TLinBufWS absolute Workspace;
begin
  AWSRec:=PluginSystem.BufferManager.NewBuffer(AParams.Count);
  ChangeLinBufValues(Dest,Source,Visualisation,Params,Workspace);
  Visualisation.SetOutput(0,AWSRec);
end;

procedure FreeLinBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TLinBufWS absolute Workspace;
begin
  PluginSystem.BufferManager.DisposeBuffer(AWSRec);
end;

procedure ChangeLinBuf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  FreeLinBuf(Dest,Source,Visualisation,Params,Workspace);
  InitLinBuf(Dest,Source,Visualisation,Params,Workspace);
end;

procedure ChangeLinBufValues(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TLinBufParams absolute Params;
  AWSRec    : TLinBufWS absolute Workspace;
  AVal,AStep: Real;
  I         : Cardinal;
begin
  AStep:=(AParams.Stop-AParams.Start)/AParams.Count;
  AVal:=AParams.Start;
  for I:=0 to AParams.Count-1 do begin
    PluginSystem.BufferManager.SetBufferItem(AWSRec,I,AVal);
    AVal+=AStep;
  end;
end;

procedure VisBufGet(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TBufGetParams absolute Params;
  ATemp  : vpReal;
begin
  if (AParams.Index>=0) and (AParams.Index<PluginSystem.BufferManager.SizeOfBuffer(AParams.Buffer))
    then ATemp:=PluginSystem.BufferManager.GetBufferItem(AParams.Buffer,AParams.Index)
    else ATemp:=0.0;
  Visualisation.SetOutput(0,ATemp);
end;

procedure VisBufGetR(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TBufGetRParams absolute Params;
  ASize  : Integer;
  ATemp  : vpReal;
begin
  ASize:=PluginSystem.BufferManager.SizeOfBuffer(AParams.Buffer);
  if (AParams.Position>=0) and (AParams.Position<=100.0) and (ASize>0)
    then ATemp:=PluginSystem.BufferManager.GetBufferItem(AParams.Buffer,Round(ASize*(AParams.Position/100.0)))
    else ATemp:=0.0;
  Visualisation.SetOutput(0,ATemp);
end;

procedure TrigSmooth(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TTrigSmoothParams absolute Params;
  AWSRec : vpReal absolute Workspace;
begin
  AWSRec:=((AParams.Input*AParams.Fac)+(AWSRec*(1.0/AParams.Fac)))/2.0;
  Visualisation.SetOutput(0,AWSRec);
end;

procedure InitTrigSmooth(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TTrigSmoothParams absolute Params;
  AWSRec : vpReal absolute Workspace;
begin
  AWSRec:=AParams.Input;
end;

procedure InitBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec     : TBufCalcWS absolute Workspace;
begin
  with AWSRec do begin
    Buffer:=VPEMPTYBUFFER;
    ActiveIndex:=-1;
  end;
end;

procedure VisBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams       : TBufCalcParams absolute Params;
  AWSRec        : TBufCalcWS absolute Workspace;
  I             : Integer;
  ASrcBufferData: pvpRealBufferItem;
  ADstBufferData: pvpRealBufferItem;
  ABufferItem   : vpReal;
begin
  with PluginSystem.BufferManager do begin
    ASrcBufferData:=GetBufferData(AParams.Buffer);
    ADstBufferData:=GetBufferData(AWSRec.Buffer);
    for I:=0 to SizeOfBuffer(AParams.Buffer)-1 do begin
      AWSRec.ActiveIndex:=I;
      //vpRealBufferItem<>vpReal, also hier konvertieren
      ABufferItem:=ASrcBufferData^;
      Visualisation.SetOutput(1,ABufferItem);
      Visualisation.SetOutput(2,VPEMPTYOUTPUT);
      ADstBufferData^:=AParams.Input;
      Inc(ASrcBufferData);
      Inc(ADstBufferData);
    end;
  end;
  AWSRec.ActiveIndex:=-1;
  Visualisation.SetOutput(0,AWSRec.Buffer);
end;

procedure ChangeBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams    : TBufCalcParams absolute Params;
  AWSRec     : TBufCalcWS absolute Workspace;
begin
  with PluginSystem.BufferManager do begin
    DisposeBuffer(AWSRec.Buffer);
    AWSRec.Buffer:=NewBuffer(SizeOfBuffer(AParams.Buffer));
  end;
end;

{procedure ChangeBufCalcInput(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams    : TBufCalcParams absolute Params;
  AWSRec     : TBufCalcWS absolute Workspace;
begin
  if AWSRec.ActiveIndex>=0
    then PluginSystem.BufferManager.SetBufferItem(AWSRec.Buffer,AWSRec.ActiveIndex,AParams.Input);
end;}

procedure FreeBufCalc(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TBufCalcWS absolute Workspace;
begin
  PluginSystem.BufferManager.DisposeBuffer(AWSRec.Buffer);
end;

procedure InitTrigBeatSpeed(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TBSGWS absolute Workspace;
begin
  with AWSRec do begin
    LastBeatPos:=NaN;
    //NegBeat:=0.0;
  end;
end;

procedure TrigBeatSpeed(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams    : TBSGParams absolute Params;
  AWSRec     : TBSGWS absolute Workspace;
  AResult    : vpReal;
  ANewBeatPos: Real;
begin
  with AWSRec do begin
    ANewBeatPos:=ISpectrumData2(Source.Future('0.4.0')).BeatPos;
    if isNan(LastBeatPos) then begin
      AResult:=0.0;
    end else begin
      AResult:=ANewBeatPos-LastBeatPos;
      if AResult<0.0 then AResult+=Source.Beat.Max;
      //ACHTUNG: NegBeat wird IMMER negativ sein
      {if AResult>Source.Beat.Max/2.0 then begin
        NegBeat+=AResult-Source.Beat.Max;
        AResult:=0.0;
      end;
      AResult+=NegBeat;
      if AResult<0.0 then begin
        NegBeat:=AResult;
        AResult:=0.0;
      end else NegBeat:=0.0; }
      AResult*=AParams;
    end;
    LastBeatPos:=ANewBeatPos;
    Visualisation.SetOutput(0,AResult);
  end;
end;

procedure TrigNow(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  ANow                                  : TDateTime;
  A1, A2, A3, A4                        : Word;
  Second, Minute, Hour, Day, Month, Year: vpInt;
begin
  ANow:=Now;
  DecodeTime(ANow, A1, A2, A3, A4);
  Second:=A3;
  Minute:=A2;
  Hour:=A1;
  DecodeDate(ANow, A1, A2, A3);
  Day:=A3;
  Month:=A2;
  Year:=A1;
  Visualisation.SetOutput(0, Second);
  Visualisation.SetOutput(1, Minute);
  Visualisation.SetOutput(2, Hour);
  Visualisation.SetOutput(3, Day);
  Visualisation.SetOutput(4, Month);
  Visualisation.SetOutput(5, Year);
end;

end.

