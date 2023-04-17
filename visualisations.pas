unit Visualisations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GR32, SpectrumData, GR32_Blend, VisDrawUnit,
  VisType, VisTypeUnit;

procedure DrawVisWaveAlt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawVisLightning(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawSimpleBassCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
//procedure DoNothing(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawBugTest(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ClearBmp(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawFFTData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawPeak(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

implementation

var
  //bowArray       : array[0..256] of Double; // holds scaled cosine-values precalculated for the bent WaveVis
  rot1,rot2      : Double;

procedure DrawVisWaveAlt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  I,YPos1,YPos2,r,g,b,c1     : Integer;
  XSpace,YFac,AYOffset,BowFac: Double;
  OldPos,NewPos              : TPoint;
  (*i, m, k       : Integer;
  r, g, b       : Integer;
  l1, l2, r1, r2: Double;
  c1, c2        : Integer;*)
begin
  XSpace:=Dest.Width/Source.LongWaveDataCount;
  YPos1:=Round(Dest.Height) div 6;
  YPos2:=Round(Dest.Height)-YPos1;
  OldPos.Y:=0;
  OldPos.X:=0;
  YFac:=Dest.Height/6;
  BowFac:=pi/Source.LongWaveDataCount;
  I:=0;
  repeat
    NewPos.X:=Round(I*XSpace);
    NewPos.Y:=Round(Source.WaveDataLong[I,0]*YFac);
    AYOffset:=sin(I*BowFac)*(Dest.Height/6);

    c1 := abs(Round(Source.WaveDataLong[I,0]*255{32767}){ div 128}); // same as l2, but absolute and *4
    //c2 := abs(Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i]*32767) div 128); // same as r2, but absolute and *4

    r := RedComponent(Visualisation.C1)   + c1;
    g := GreenComponent(Visualisation.C1) + c1;
    b := BlueComponent(Visualisation.C1)  + c1;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;

    Dest.Line(OldPos.X,OldPos.Y+YPos1+AYOffset,NewPos.X,NewPos.Y+YPos1+AYOffset,Color32(r,g,b));
    Dest.Line(OldPos.X,OldPos.Y+YPos2-AYOffset,NewPos.X,NewPos.Y+YPos2-AYOffset,Color32(r,g,b));
    OldPos:=NewPos;
    Inc(I,8);
  until I>=Source.LongWaveDataCount;
  //for I:=0 to Source.LongWaveDataCount-1;
  (*//WaveDataLong := Source.getwavedatalong;
  i := 0;
  m := 0;
  k := 0;
  l1 := 100+bowArray[0];
  r1 := 300-bowArray[0];
  repeat
    // [sample] + [yOffset] + [bow (to get the line "round")]
    l2 := (Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+8]*32767) div 512) + 100 + bowArray[k];
    r2 := (Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+9]*32767) div 512) + 300 - bowArray[k];

    c1 := abs(Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+8]*32767) div 128); // same as l2, but absolute and *4
    c2 := abs(Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+9]*32767) div 128); // same as r2, but absolute and *4

    r := RedComponent(Visualisation.C1)   + c1;
    g := GreenComponent(Visualisation.C1) + c1;
    b := BlueComponent(Visualisation.C1)  + c1;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;
    bmp.LineFS(m, l1, m+2, l2, Color32(r,g,b));

    r := RedComponent(Visualisation.C1)   + c2;
    g := GreenComponent(Visualisation.C1) + c2;
    b := BlueComponent(Visualisation.C1)  + c2;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;
    bmp.LineFS(m, r1, m+2, r2, Color32(r,g,b));
    inc(i, 8);
    inc(m, 2);
    inc(k);
    l1 := l2;
    r1 := r2;
  until i >= {Length(WaveDataLong)}{Source.SampleCount*3}Source.LongWaveDataCount{LongWaveDataCount}-8{Source.FreqCount}{+2}{9};*)
end;

(*procedure DrawVisWaveAlt(var bmp: TBitmap32; const Source: ISpectrumData; const Visualisation: TVisualisation; const Params; var Workspace); stdcall;
var
  i, m, k       : Integer;
  r, g, b       : Integer;
  l1, l2, r1, r2: Double;
  c1, c2        : Integer;
begin
  //WaveDataLong := Source.getwavedatalong;
  i := 0;
  m := 0;
  k := 0;
  l1 := 100+bowArray[0];
  r1 := 300-bowArray[0];
  repeat
    // [sample] + [yOffset] + [bow (to get the line "round")]
    l2 := (Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+8]*32767) div 512) + 100 + bowArray[k];
    r2 := (Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+9]*32767) div 512) + 300 - bowArray[k];

    c1 := abs(Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+8]*32767) div 128); // same as l2, but absolute and *4
    c2 := abs(Round(Source.WaveDataLongC{LongC}{WaveDataLong}[i+9]*32767) div 128); // same as r2, but absolute and *4

    r := RedComponent(Visualisation.C1)   + c1;
    g := GreenComponent(Visualisation.C1) + c1;
    b := BlueComponent(Visualisation.C1)  + c1;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;
    bmp.LineFS(m, l1, m+2, l2, Color32(r,g,b));

    r := RedComponent(Visualisation.C1)   + c2;
    g := GreenComponent(Visualisation.C1) + c2;
    b := BlueComponent(Visualisation.C1)  + c2;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;
    bmp.LineFS(m, r1, m+2, r2, Color32(r,g,b));
    inc(i, 8);
    inc(m, 2);
    inc(k);
    l1 := l2;
    r1 := r2;
  until i >= {Length(WaveDataLong)}{Source.SampleCount*3}Source.LongWaveDataCount{LongWaveDataCount}-8{Source.FreqCount}{+2}{9};
end;*)

procedure DrawSimpleBassCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  Middle     : TPoint;
  BassValue,I: Integer;
  BassValue2 : Double;
begin
  Middle.X:=Round(Dest.Width) div 2;
  Middle.Y:=Round(Dest.Height) div 2;
  BassValue2:=0;
  for I:=0 to 1 do BassValue2+=Source.Levels[I,0];
  BassValue:=Trunc(BassValue2*200);
  //bmp.PenColor:=$FF0000FF;
  Dest.FillRect(Middle.X-BassValue,Middle.Y-BassValue,Middle.X+BassValue,Middle.Y+BassValue,Visualisation.C1{Lighten(color,-40)});
end;

procedure DrawVisLightning(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  Cx, Cy,
  Dx, Dy,
  Fx, Fy,
  d, w,
  x1, x2, y1, y2, vl,
  CosR1, SinR1,
  CosR2, SinR2: Double;
  i           : Integer;
  AParams     : vpReal absolute Params;
const
  pi2: Double = 6.283; // 2*pi
begin
  //GetVisParams(Visualisation,AParams);
  if Source.Peak[0]{LeftPeak}+Source.Peak[1]{RightPeak} > AParams then
  begin
    //WaveData := Source.getwavedata;
    Cx := Dest.Width/2;
    Cy := Dest.Height/2;
    if random(10) = 0 then rot1 := random*pi2;
    if random(10) = 0 then rot2 := random*pi2;
    CosR1 := cos(rot1)*10;
    SinR1 := sin(rot1)*10;
    CosR2 := cos(rot2)*10;
    SinR2 := sin(rot2)*10;

    // Draw Channel 1
    Dx := Cx+CosR1;
    Dy := Cy+SinR1;
    x1 := Cx;
    y1 := Cy;
    i  := 0;
    repeat
      Fx := Dx-Cx;
      Fy := Dy-Cy;
      vl := sqrt(sqr(Fx)+sqr(Fy));
      d  := vl{ / 128};
      w  := (Source.WaveData[i,0]{ / 128})*d;
      x2 := Dx+(Fy / vl)*w;
      y2 := Dy-(Fx / vl)*w;
      Dest.Line(x1, y1, x2, y2, Lighten(Visualisation.C1, -i*3));
      x1 := x2;
      y1 := y2;
      Dx := Dx + CosR1;
      Dy := Dy + SinR1;
      inc(i, 4);
    until i >= 80;

    // Draw Channel 2
    Dx := Cx+CosR2;
    Dy := Cy+SinR2;
    x1 := Cx;
    y1 := Cy;
    i  := 1;
    repeat
      Fx := Dx-Cx;
      Fy := Dy-Cy;
      vl := sqrt(sqr(Fx)+sqr(Fy));
      d  := vl{ / 128};
      w  := (Source.WaveData[i,0]{ / 128})*d;
      x2 := Dx+(Fy / vl)*w;
      y2 := Dy-(Fx / vl)*w;
      Dest.Line(x1, y1, x2, y2, Lighten(Visualisation.C1, -i*3));
      x1 := x2;
      y1 := y2;
      Dx := Dx + CosR2;
      Dy := Dy + SinR2;
      inc(i, 4);
    until i >= 80;
  end;
end;

{procedure DrawVisWaveAlt(var bmp: TBitmap32; const Source: TBassplayer; color: TColor32);
var
  i, m, k       : Integer;
  r, g, b       : Integer;
  l1, l2, r1, r2: Double;
  c1, c2        : Integer;
begin
  WaveDataLong := Source.getwavedatalong;
  i := 0;
  m := 0;
  k := 0;
  l1 := 100+bowArray[0];
  r1 := 300-bowArray[0];
  repeat
    // [sample] + [yOffset] + [bow (to get the line "round")]
    l2 := (WaveDataLong[i+8] div 512) + 100 + bowArray[k];
    r2 := (WaveDataLong[i+9] div 512) + 300 - bowArray[k];

    c1 := abs(WaveDataLong[i+8] div 128); // same as l2, but absolute and *4
    c2 := abs(WaveDataLong[i+9] div 128); // same as r2, but absolute and *4

    r := RedComponent(color)   + c1;
    g := GreenComponent(color) + c1;
    b := BlueComponent(color)  + c1;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;
    bmp.LineFS(m, l1, m+2, l2, Color32(r,g,b));

    r := RedComponent(color)   + c2;
    g := GreenComponent(color) + c2;
    b := BlueComponent(color)  + c2;
    if r > 255 then r := 255;
    if g > 255 then g := 255;
    if b > 255 then b := 255;
    bmp.LineFS(m, r1, m+2, r2, Color32(r,g,b));
    inc(i, 8);
    inc(m, 2);
    inc(k);
    l1 := l2;
    r1 := r2;
  until i >= Length(WaveDataLong)-9;
end;}

{procedure DoNothing(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin

end;}

procedure DrawBugTest(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Dest.Line(random(100),0,100,100,Visualisation.C2);
end;

procedure ClearBmp(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Dest.Clear(Visualisation.C3);
end;

type TDrawFFTDataParam= vpReal;

procedure DrawFFTData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  I,IH     : Integer;
  AParam   : TDrawFFTDataParam absolute Params;
  DataWidth: Real;
begin
  DataWidth:=Dest.Width/Source.FreqCount;
  IH:=Round(Dest.Height);
  //GetVisParams(Visualisation,AParam);
  for I:=0 to Source.FreqCount-1
    do Dest.FillRect(Round(I*DataWidth),IH-Round(Source.Levels[I,0]*IH*AParam),Round((I+1)*DataWidth),IH,Visualisation.C1);
end;

procedure DrawPeak(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParam: vpReal absolute Params;
  IH    : Integer;
begin
  IH:=Round(Dest.Height);
  //GetVisParams(Visualisation,AParam);
  Dest.FillRect(0,IH-Round(Source.Peak[0]*IH*AParam),Dest.Width,IH,Visualisation.C2);
end;

{procedure Init_Local;
var
  I,M: Integer;
begin
  M:=0;
  for I:=0 to 256 do
  begin
    bowArray[I]:=(cos((200-m)/100) * 50);
    Inc(M,2);
  end;
end;}

type
  TTestParams   = packed record
    P1: TColor32;
    P2: Integer;
    P3: Real;
    P4: ShortString;
  end;

  TNewTestParams= packed record
    P1: TColor32;
    P2: Integer;
    P3: Real;
    P4: ShortString;
    P5: Boolean;
  end;

const
  TestIV     : TTestParams = (P1:$FF00FF00; P2:20; P3:0.5; P4:'Allgemeiner Parametertest');
  NewTestIV  : TNewTestParams = (P1:$FF00FF00; P2:20; P3:0.5; P4:'Erweiterter Parametertest'; P5:false);
  _EmptyInit : Pointer = nil;
  LightningIV: vpReal = 0.1;
  TestID     : UInt64 = $FA64732BCD9F0AE3;
  NewTestID  : UInt64 = $FA64732BCD9F0AE4;
  LazTestID  : UInt64 = $76A8C93D23E216FA;
  FFTIV      : vpReal = 20.0;
  PeakIV     : vpReal = 20.0;

initialization
  //Init_Local;
  TVisualisation.CreateAndRegisterVis('Wave','WAVEVIS ',@DrawVisWaveAlt,[],[],[],_EmptyInit,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Lightning','LIGHTNIN',@DrawVisLightning,['Grenzwert'],[vReal],[],LightningIV,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Bassrechteck','BASSRECT',@DrawSimpleBassCircle,[],[],[],_EmptyInit,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Test',TestID,nil,['Farbe','Ganze Zahl','Gleitkommawert','Text'],[vColor,vInteger,vReal,vString],[],TestIV,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Neuer Test',NewTestID,nil,['Farbe','Ganze Zahl','Gleitkommawert','Text','Wahrheitswert'],[vColor,vInteger,vReal,vString,vBoolean],[],NewTestIV,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Clear Layer','________',@ClearBmp,[],[],[],_EmptyInit,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Lazarus Bugtest',LazTestID,@DrawBugTest,[],[],[],_EmptyInit,[],[],[]);
  TVisualisation.CreateAndRegisterVis('FFT Daten','FFT DATA',@DrawFFTData,['Streckfaktor'],[vReal],[],FFTIV,[],[],[]);
  TVisualisation.CreateAndRegisterVis('Lautstärke','  PEAK  ',@DrawPeak,['Streckfaktor'],[vReal],[],PeakIV,[],[],[]);
  //CreateAndRegisterVis('',TVisID(0),@DoNothing,[],[],[],nil,[],[]);
end.

