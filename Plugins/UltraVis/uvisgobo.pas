unit UVisGobo;

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData, MainType, GraphX32;

procedure procVisGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeGoboParams(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure DrawVox(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawWaveCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

type
  TParamsRec = packed record
    C4         : vpColor;
    GoboSize   : vpInt;
    GoboCount  : vpInt;
    Limit      : vpReal;
    Scale      : vpReal;
    Radius     : vpReal;
    SinkSpeed  : vpReal;
  end;

  TVoxParams        = packed record
    Scale: vpReal;
  end;

  TWaveCircleParams = packed record
    Radius,Intensity: vpReal;
  end;

  TPhiR             = packed record
    R,Phi: vpReal;
  end;

  TRealPoint        = packed record
    X,Y: vpReal;
  end;

  {TPhiRArray = array of TPhiR;
  PPhiRArray = ^TPhiRArray;}
  TIPhiRArray = array [0..0] of TPhiR;
  TPhiRArray  = ^TIPhiRArray;
  TIVIntArray = array [0..0] of Integer;
  TVIntArray  = ^TIVIntArray;

  TSpreadGoboWSRec= record
    ALength    : Cardinal;
    GoboPos    : TPhiRArray;
    oldGoboPos : TPhiRArray;
    GoboLevels : TVIntArray;
    GoboLevels2: TVIntArray;
  end;

const
  PhiRSize                       = SizeOf(TPhiR);
  IntSize                        = SizeOf(Integer);

  SpreadGoboIV: TParamsRec       = (C4: $0000FF00; GoboSize: 3; GoboCount: 20; Limit: Real(0.015); Scale: Real(1000.0); Radius: 100; SinkSpeed: 1.0);
  VoxIV: TVoxParams              = (Scale: 10.0);
  WaveCircleIV: TWaveCircleParams= (Radius: 70.0; Intensity: 20.0);

implementation

procedure ChangeGoboParams(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TParamsRec absolute Params;
  AWSRec  : TSpreadGoboWSRec absolute Workspace;
  I       : Integer;
  PhiFac  : Real;
  FreqArea: Integer;
begin
  with AWSRec do begin
    with PluginSystem do begin
      FreeMemory(GoboPos,ALength*PhiRSize);
      FreeMemory(oldGoboPos,ALength*PhiRSize);
      FreeMemory(GoboLevels,ALength*IntSize);
      FreeMemory(GoboLevels2,ALength*IntSize);
      ALength:=AParams.GoboCount;
      GetMemory(GoboPos,ALength*PhiRSize);
      GetMemory(oldGoboPos,ALength*PhiRSize);
      GetMemory(GoboLevels,ALength*IntSize);
      GetMemory(GoboLevels2,ALength*IntSize);
    end;
    PhiFac:=(2*Pi)/AParams.GoboCount;
    FreqArea:=512 div 8;
    for I:=0 to AParams.GoboCount-1 do begin
      with GoboPos^[I] do begin
        Phi:=PhiFac*I;
        R:=0.0;
      end;
      with oldGoboPos^[I] do begin
        Phi:=GoboPos^[I].Phi;
        R:=0.0;
      end;
      GoboLevels^[I]:=random(FreqArea);
      GoboLevels2^[I]:=random(FreqArea)+(FreqArea*2);
    end;
  end;
end;

procedure InitGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TSpreadGoboWSRec absolute Workspace;
begin
  with AWSRec do with PluginSystem do begin
    ALength:=0;
    GetMemory(GoboPos,0);
    GetMemory(oldGoboPos,0);
    GetMemory(GoboLevels,0);
    GetMemory(GoboLevels2,0);
  end;
  ChangeGoboParams(Dest,Source,Visualisation,Params,Workspace);
  randomize;
end;

procedure FreeGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TSpreadGoboWSRec absolute Workspace;
begin
  with AWSRec do with PluginSystem do begin
    FreeMemory(GoboPos,ALength*PhiRSize);
    FreeMemory(oldGoboPos,ALength*PhiRSize);
    FreeMemory(GoboLevels,ALength*IntSize);
    FreeMemory(GoboLevels2,ALength*IntSize);
  end;
end;

procedure procVisGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams             : TParamsRec absolute Params;
  AWSRec              : TSpreadGoboWSRec absolute Workspace;
  Bass, Mitten, Hoehen: Real;
  i                   : Integer;
  tphi,tr             : Real;
  tx,ty               : Integer;
  ALimit              : Real;
  AC1,AC2             : vpColor;
  ARadius             : Real;
begin
  Bass:=0;
  Mitten:=0;
  Hoehen:=0;
  for i:=0 to 3 do  Bass += source.Levels[i,0];
  for i:=Source.FreqCount div 8 to Source.FreqCount div 4 do Mitten += source.Levels[i,0];
  for i:=Source.FreqCount div 8 * 3 to Source.FreqCount div 2 do Hoehen+=source.Levels[i,0];
  if Dest.Height>Dest.Width
    then ARadius:=Dest.Width*(AParams.Radius/200)
    else ARadius:=Dest.Height*(AParams.Radius/200);

  with AWSRec do for i:=0 to AParams.GoboCount-1 do begin
    //Schredderwert
    ALimit:=AParams.Limit-(Source.Levels[GoboLevels2^[i],0]*10);

    if Source.Levels[GoboLevels^[i],0]>ALimit then begin
      tr:=((Source.Levels[GoboLevels^[i],0]-ALimit)*AParams.Scale);
    end else begin
      tr:=-AWSRec.GoboPos^[i].R*(ALimit-Source.Levels[GoboLevels^[i],0])*AParams.SinkSpeed;
    end;

    with GoboPos^[i] do begin
      R+=tr;
      if R>ARadius then begin
        R:=frac(R/ARadius)*ARadius;
        Phi:=Random*2*Pi;
      end;
      tx:=Round((R*Sin(Phi))+(Dest.Width/2));
      ty:=Round((R*Cos(Phi))+(Dest.Height/2));
      AC1:=BetaBlend(Visualisation.C3,Visualisation.C1,Round(($FF/ARadius)*R));
      AC2:=BetaBlend(AParams.C4,Visualisation.C2,Round(($FF/ARadius)*R));
    end;
    AWSRec.oldGoboPos^[i].R:=AWSRec.GoboPos^[i].R;
    AWSRec.oldGoboPos^[i].Phi:=AWSRec.GoboPos^[i].Phi;
    Dest.FillRect(tx,ty,tx+AParams.GoboSize,ty+AParams.GoboSize,BetaBlend(AC1,AC2,Round(($FF/(Source.FreqCount div 8))*GoboLevels^[i]))  {Visualisation.C1+(Round(($FF/(Source.FreqCount div 8))*GoboLevels^[i])*$100)});
  end;
end;

procedure DrawVox(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams                  : TVoxParams absolute Params;
  I,J,IH,DC,IH2,AFreqHeight: Integer;
  DataWidth                : Real;
begin
  DataWidth:=Dest.Width/Source.FreqCount;
  IH:=Round(Dest.Height);
  IH2:=IH div 2;
  DC:=Source.FreqCount div 2;
  J:=0;
  for I:=DC to Source.FreqCount-1 do begin
    AFreqHeight:=Round(Source.Levels[I,0]*IH*AParams.Scale);
    Dest.FillRect(Round(J*DataWidth),IH2-AFreqHeight,Round((J+1)*DataWidth),IH2+AFreqHeight,Visualisation.C1);
    Inc(J);
  end;
  for I:=0 to DC-1 do begin
    AFreqHeight:=Round(Source.Levels[I,0]*IH*AParams.Scale);
    Dest.FillRect(Round(J*DataWidth),IH2-AFreqHeight,Round((J+1)*DataWidth),IH2+AFreqHeight,Visualisation.C1);
    Inc(J);
  end;
end;

procedure DrawWaveCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams                      : TWaveCircleParams absolute Params;
  APhiStep,ar,rr               : Real;
  I,IH2,IW2                    : Integer;
  LastPoint,NewPoint,FirstPoint: TRealPoint;

  function GetALineColor(const AValue: Real): vpColor;
  var
    c1,r,g,b: Integer;
  begin
    c1 := abs(Round(AValue*300));
    if c1>255 then c1:=255;
    c1:=255-c1;
    Result:=BetaBlend(Visualisation.C1,Visualisation.C2,c1);
  end;

begin
  APhiStep:=(2*Pi)/Source.WaveDataCount;
  I:=1;
  IH2:=Round(Dest.Height/2);
  IW2:=Round(Dest.Width/2);
  if Dest.Height<Dest.Width
    then rr:=(AParams.Radius/200)*Dest.Height
    else rr:=(AParams.Radius/200)*Dest.Width;
  ar:=rr+(AParams.Intensity*Source.WaveData[0,0]);
  FirstPoint.X:=ar*sin(0)+IW2;
  FirstPoint.Y:=ar*cos(0)+IH2;
  LastPoint:=FirstPoint;
  while I<Source.WaveDataCount do begin

    ar:=rr+(AParams.Intensity*Source.WaveData[I,0]);
    NewPoint.X:=ar*sin(I*APhiStep)+IW2;
    NewPoint.Y:=ar*cos(I*APhiStep)+IH2;

    Dest.Line(LastPoint.X,LastPoint.Y,NewPoint.X,NewPoint.Y,GetALineColor(Source.WaveData[I,0]));
    LastPoint:=NewPoint;
    Inc(I,1);
  end;
  Dest.Line(LastPoint.X,LastPoint.Y,FirstPoint.X,FirstPoint.Y,GetALineColor(Source.WaveData[0,0]));
end;

end.

