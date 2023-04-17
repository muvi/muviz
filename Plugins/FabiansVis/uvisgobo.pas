unit UVisGobo;

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData, MainType, GraphX32;

procedure procVisGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeGoboParams(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

type
  TParamsRec = packed record
    Modus      : vpInt;
    GoboGroesse: vpInt;
    GoboAnzahl : vpInt;
    MultiColor : vpBool;
    Limit      : vpReal;
    Scale      : vpReal;
  end;

  TPhiR      = packed record
    R,Phi: Real
  end;

  {TPhiRArray = array of TPhiR;
  PPhiRArray = ^TPhiRArray;}
  TIPhiRArray = packed array [0..0] of TPhiR;
  TPhiRArray  = ^TIPhiRArray;
  TIVIntArray = packed array [0..0] of Integer;
  TVIntArray  = ^TIVIntArray;
  {TIntArray  = array of Integer;
  PIntArray  = ^TIntArray;}

  TWSRec = record
    ALength    : Cardinal;
    GoboPos    : TPhiRArray;
    oldGoboPos : TPhiRArray;
    GoboLevels : TVIntArray;
    GoboLevels2: TVIntArray;
  end;

const
  PhiRSize           = SizeOf(TPhiR);
  IntSize            = SizeOf(Integer);

  GoboIV: TParamsRec = (Modus: 0; GoboGroesse: 20; GoboAnzahl: 5; MultiColor: true; Limit: vpReal(0.015); Scale: vpReal(1000.0));

implementation

procedure ChangeGoboParams(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TParamsRec absolute Params;
  AWSRec  : TWSRec absolute Workspace;
  I       : Integer;
  PhiFac  : Real;
  FreqArea: Integer;
begin
  with AWSRec do with PluginSystem do begin
    FreeMemory(GoboPos,ALength*PhiRSize);
    FreeMemory(oldGoboPos,ALength*PhiRSize);
    FreeMemory(GoboLevels,ALength*IntSize);
    FreeMemory(GoboLevels2,ALength*IntSize);
    ALength:=AParams.GoboAnzahl;
    GetMemory(GoboPos,ALength*PhiRSize);
    GetMemory(oldGoboPos,ALength*PhiRSize);
    GetMemory(GoboLevels,ALength*IntSize);
    GetMemory(GoboLevels2,ALength*IntSize);

    {VisSetLength(GoboPos,AParams.GoboAnzahl,ALength,PhiRSize);
    VisSetLength(oldGoboPos,AParams.GoboAnzahl,ALength,PhiRSize);
    VisSetLength(GoboLevels,AParams.GoboAnzahl,ALength,IntSize);
    VisSetLength(GoboLevels2,AParams.GoboAnzahl,ALength,IntSize);}
    {SetLength(GoboPos^,AParams.GoboAnzahl);
    SetLength(oldGoboPos^,AParams.GoboAnzahl);
    SetLength(GoboLevels^,AParams.GoboAnzahl);
    SetLength(GoboLevels2^,AParams.GoboAnzahl);}
    {if AParams.GoboAnzahl<=0 then begin
      Dest.FillRect(0,0,100,100,$FF00FFFF);
      exit;
    end;}

    if AParams.GoboAnzahl>0
      then PhiFac:=(2*Pi)/AParams.GoboAnzahl
      else exit;
    FreqArea:=512 div 8;
    for I:=0 to AParams.GoboAnzahl-1 do begin
      with GoboPos^[I] do begin
        Phi:=PhiFac*I;
        R:=0.0;
      end;
      //PiCut(GoboPos^[I].Phi);
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
  AWSRec: TWSRec absolute Workspace;
begin
  with AWSRec do with PluginSystem do begin
    ALength:=0;
    GetMemory(GoboPos,0);
    GetMemory(oldGoboPos,0);
    GetMemory(GoboLevels,0);
    GetMemory(GoboLevels2,0);
    {GetMem(GoboPos,SizeOf(TPhiRArray));
    GetMem(oldGoboPos,SizeOf(TPhiRArray));
    GetMem(GoboLevels,SizeOf(TIntArray));
    GetMem(GoboLevels2,SizeOf(TIntArray));}
  end;
  randomize;
  ChangeGoboParams(Dest,Source,Visualisation,Params,Workspace);
end;

procedure FreeGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TWSRec absolute Workspace;
begin
  with AWSRec do with PluginSystem do begin
    FreeMemory(GoboPos,ALength*PhiRSize);
    FreeMemory(oldGoboPos,ALength*PhiRSize);
    FreeMemory(GoboLevels,ALength*IntSize);
    FreeMemory(GoboLevels2,ALength*IntSize);
    {VisSetLength(GoboPos,0,ALength,PhiRSize);
    VisSetLength(oldGoboPos,0,ALength,PhiRSize);
    VisSetLength(GoboLevels,0,ALength,IntSize);
    VisSetLength(GoboLevels2,0,ALength,IntSize); }
    //ALength:=0;
    {SetLength(GoboPos^,0);
    SetLength(oldGoboPos^,0);
    SetLength(GoboLevels^,0);
    SetLength(GoboLevels2^,0);
    FreeMem(GoboPos,SizeOf(TPhiRArray));
    FreeMem(oldGoboPos,SizeOf(TPhiRArray));
    FreeMem(GoboLevels,SizeOf(TIntArray));
    FreeMem(GoboLevels2,SizeOf(TIntArray));}
  end;
end;

procedure procVisGobo(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams             : TParamsRec absolute Params;
  AWSRec              : TWSRec absolute Workspace;
  i                   : Integer;
  tphi,tr             : Real;
  tx,ty               : Integer;
  ALimit              : Real;
begin
  if Source.Levels[Round((Source.FreqCount/16)*3),0]>Source.Levels[Round((Source.FreqCount/16)*3)+1,0] then begin
    tphi:=Source.Levels[Round((Source.FreqCount/16)*3),0]*10;
  end else begin
    tphi:=-Source.Levels[Round((Source.FreqCount/16)*3)+1,0]*10;
  end;
  with AWSRec do for i:=0 to AWSRec.ALength{AParams.GoboAnzahl}-1 do begin
    //Schredderwert
    ALimit:=AParams.Limit-(Source.Levels[GoboLevels2^[i]{+(Source.FreqCount div 8)},0]*10);

    if Source.Levels[GoboLevels^[i],0]{Bass}>ALimit then begin
      tr:=((Source.Levels[GoboLevels^[i],0]{Bass}-ALimit)*AParams.Scale);
    end else begin
      tr:=-AWSRec.GoboPos^[i].R*(ALimit-Source.Levels[GoboLevels^[i],0]{Bass});
    end;

    with GoboPos^[i] do begin
      R+=tr;
      Phi+=tphi;
      tx:=Round((R*Sin(Phi))+(Dest.Width/2));
      ty:=Round((R*Cos(Phi))+(Dest.Height/2));
    end;
    AWSRec.oldGoboPos^[i].R:=AWSRec.GoboPos^[i].R;
    AWSRec.oldGoboPos^[i].Phi:=AWSRec.GoboPos^[i].Phi;
    Dest.FillRect(tx,ty,tx+AParams.GoboGroesse,ty+AParams.GoboGroesse,BetaBlend(Visualisation.C1,Visualisation.C2,Round(($FF/(Source.FreqCount div 8))*GoboLevels^[i]))  {Visualisation.C1+(Round(($FF/(Source.FreqCount div 8))*GoboLevels^[i])*$100)});
  end;
end;

end.

