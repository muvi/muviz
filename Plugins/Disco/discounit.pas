unit DiscoUnit;

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData, GraphX32, AdvCoord, Classes;

procedure DrawStrobe(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawRStrobe(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawLines(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawRCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitStrobe(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitLines(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

type
  TStrobeParams = packed record
    BlinkFrames,BlackFrames: vpInt;
  end;

  TRStrobeParams= packed record
    Limit,Scale: vpReal;
  end;

  TLinesParams  = packed record
    LineLength,Dist,Limit: vpReal;
  end;

  TCircleParams = packed record
    R,Speed: vpReal;
  end;

  TStrobeWS     = Cardinal;

  TLinesWS      = TPhiR;
  TCircleWS     = Real;

const
  StrobeIV : TStrobeParams = (BlinkFrames: 2; BlackFrames: 2);
  RStrobeIV: TRStrobeParams= (Limit: Real(2.0); Scale: Real(20.0));
  LinesIV  : TLinesParams  = (LineLength: Real(30000.0); Dist: Real(1000.0); Limit: Real(0.0005));
  CircleIV : TCircleParams = (R: Real(100.0); Speed: Real(30000.0));

implementation

procedure InitStrobe(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TStrobeWS absolute Workspace;
begin
  AWSRec:=0;
end;

procedure DrawStrobe(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams             : TStrobeParams absolute Params;
  AWSRec              : TStrobeWS absolute Workspace;
begin
  with AParams do begin
    if AWSRec<BlinkFrames
      then Dest.FillRect(0,0,Dest.Width,Dest.Height,Visualisation.C1)
      else Dest.FillRect(0,0,Dest.Width,Dest.Height,Visualisation.C2);
    Inc(AWSRec);
    AWSRec:=AWSRec mod (BlinkFrames+BlackFrames);
  end;
end;

procedure DrawRStrobe(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams             : TRStrobeParams absolute Params;
begin
  with AParams
    do Dest.FillRect(0,0,Dest.Width,Dest.Height,BetaBlend(Visualisation.C1,Visualisation.C2,Round((Source.Peak[0]-Limit)*Scale*$FF)));
end;

procedure InitLines(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TLinesWS absolute Workspace;
begin
  with AWSRec do begin
    R:=100;
    Phi:=0;
  end;
end;

procedure DrawLines(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TLinesParams absolute Params;
  AWSRec : TLinesWS absolute Workspace;
  ACenter: TPoint;
  NewPos2: TPoint;
  OldPos2: TPoint;
  OldR   : Real;
begin
  //init
  if Source.Levels[0,0]<AParams.Limit then exit;
  ACenter:=Point(Round(Dest.Width/2),Round(Dest.Height/2));
  OldPos2:=AdvCoord.Point(RealPoint(AWSRec,ACenter));
  //draw
  with AWSRec do begin
    OldR:=R;
    R:=Source.WaveData[0,0]*AParams.Dist+1;
    Phi:=Phi+RToPhi(R,OldR,(Source.Levels[0,0]+Source.Levels[1,0]+Source.Levels[2,0]+Source.Levels[3,0])*AParams.LineLength);
  end;
  //done
  NewPos2:=AdvCoord.Point(RealPoint(AWSRec,ACenter));
  Dest.Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,Visualisation.C1);
end;

procedure InitCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TCircleWS absolute Workspace;
begin
  AWSRec:=0;
end;

procedure DrawRCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TCircleParams absolute Params;
  AWSRec : TCircleWS absolute Workspace;
  ACenter: TPoint;
  NewPos2: TPoint;
  OldPos2: TPoint;
begin
  //init
  ACenter:=Point(Round(Dest.Width/2),Round(Dest.Height/2));
  OldPos2:=AdvCoord.Point(RealPoint(PhiR(AParams.R,AWSRec),ACenter));
  //draw
  with AParams
     do AWSRec:=AWSRec+RToPhi(R,R,(Source.Levels[0,0]+Source.Levels[1,0]+Source.Levels[2,0]+Source.Levels[3,0])*Speed);
  //done
  NewPos2:=AdvCoord.Point(RealPoint(PhiR(AParams.R,AWSRec),ACenter));
  Dest.Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,Visualisation.C1);
end;

end.

