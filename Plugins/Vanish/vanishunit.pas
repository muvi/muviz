unit VanishUnit;

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData;

procedure VisVanish(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisRotate(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisZoom(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisCrazyScale(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure VisMove(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
{procedure ChangeGoboParams(const Visualisation: TVisualisation; var Workspace);
procedure InitGobo(const Visualisation: TVisualisation; var Workspace);
procedure FreeGobo(const Visualisation: TVisualisation; var Workspace);}

type
  TVisRotateParams = packed record
    Speed: vpReal;
  end;

  TVisZoomParams   = packed record
    X,Y: vpReal;
  end;

  TVisMoveParams   = packed record
    X,Y: vpInt;
  end;

  {TParamsRec = packed record
    Modus      : Integer;
    GoboGroesse: Integer;
    GoboAnzahl : Integer;
    MultiColor : Boolean;
    Limit      : Real;
    Scale      : Real;
  end;

  TPhiR      = record
    R,Phi: Real
  end;

  TPhiRArray = array of TPhiR;
  PPhiRArray = ^TPhiRArray;
  TIntArray  = array of Integer;
  PIntArray  = ^TIntArray;

  TWSRec = record
    GoboPos    : PPhiRArray;
    oldGoboPos : PPhiRArray;
    GoboLevels : PIntArray;
    GoboLevels2: PIntArray;
  end; }

const
  //GoboIV: TParamsRec = (Modus: 0; GoboGroesse: 20; GoboAnzahl: 5; MultiColor: true; Limit: Real(0.015); Scale: Real(1000.0));
  RotateIV: TVisRotateParams = (Speed: 2.0);
  MoveIV  : TVisMoveParams   = (X: 0; Y: 2);
  ZoomIV  : TVisZoomParams   = (X: 2; Y: 2);

implementation

{procedure ChangeGoboParams(const Visualisation: TVisualisation; var Workspace);
var
  //AParams : TParamsRec;
  //AWSRec  : TWSRec absolute Workspace;
  I       : Integer;
  PhiFac  : Real;
  FreqArea: Integer;
begin
  GetVisParams(Visualisation,AParams);
  with AWSRec do begin
    SetLength(GoboPos^,AParams.GoboAnzahl);
    SetLength(oldGoboPos^,AParams.GoboAnzahl);
    SetLength(GoboLevels^,AParams.GoboAnzahl);
    SetLength(GoboLevels2^,AParams.GoboAnzahl);
    PhiFac:=(2*Pi)/AParams.GoboAnzahl;
    FreqArea:=512 div 8;
    for I:=0 to AParams.GoboAnzahl-1 do begin
      GoboPos^[I].Phi:=PhiFac*I;
      //PiCut(GoboPos^[I].Phi);
      oldGoboPos^[I].Phi:=GoboPos^[I].Phi;
      GoboLevels^[I]:=random(FreqArea);
      GoboLevels2^[I]:=random(FreqArea)+(FreqArea*2);
    end;
  end;
end;

procedure InitGobo(const Visualisation: TVisualisation; var Workspace);
var
  AWSRec: TWSRec absolute Workspace;
begin
  with AWSRec do begin
    GetMem(GoboPos,SizeOf(TPhiRArray));
    GetMem(oldGoboPos,SizeOf(TPhiRArray));
    GetMem(GoboLevels,SizeOf(TIntArray));
    GetMem(GoboLevels2,SizeOf(TIntArray));
  end;
  ChangeGoboParams(Visualisation,Workspace);
  randomize;
end;

procedure FreeGobo(const Visualisation: TVisualisation; var Workspace);
var
  AWSRec : TWSRec absolute Workspace;
begin
  with AWSRec do begin
    SetLength(GoboPos^,0);
    SetLength(oldGoboPos^,0);
    SetLength(GoboLevels^,0);
    SetLength(GoboLevels2^,0);
    FreeMem(GoboPos,SizeOf(TPhiRArray));
    FreeMem(oldGoboPos,SizeOf(TPhiRArray));
    FreeMem(GoboLevels,SizeOf(TIntArray));
    FreeMem(GoboLevels2,SizeOf(TIntArray));
  end;
end;}

procedure VisVanish(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
{var
  AParams             : TParamsRec;
  AWSRec              : TWSRec absolute Workspace;
  Bass, Mitten, Hoehen: Real;
  i                   : Integer;
  tphi,tr             : Real;
  tx,ty               : Integer;
  ALimit              : Real;}
//var
  //bmp2: TBitmap32;
  //Trans: TAffineTransformation;
begin
  //Trans:=TAffineTransformation.Create;
  {bmp2:=TBitmap32.Create;
  bmp2.BeginUpdate;
  bmp2.Width:=bmp.Width;
  bmp2.Height:=bmp.Height;
  bmp2.Clear(Visualisation.C3 and $FFFFFF);
  bmp2.MasterAlpha:=Visualisation.C3 shr 24;
  bmp2.EndUpdate;
  bmp2.Changed;}

  //bmp.BeginUpdate;
  //bmp.Draw(0,0,bmp2);
  Dest.Vanish(Visualisation.C3);
  //bmp.FillRectTS(0,0,bmp.Width,bmp.Height,Visualisation.C3);
  //bmp.EndUpdate;
  //bmp.Changed;
  //bmp2.Destroy;
  //Trans.Destroy;
  //bmp.FillRectTS(0,0,100,100,$FFFFFFFF{Visualisation.C1});
  (*GetVisParams(Visualisation, AParams);
  Bass:=0;
  Mitten:=0;
  Hoehen:=0;
  for i:=0 to 3 do  Bass += source.Levels[i];
  for i:=source.FreqCount div 8 to source.FreqCount div 4 do Mitten += source.Levels[i];
  for i:=source.FreqCount div 8 * 3 to source.FreqCount div 2 do Hoehen+=source.Levels[i];
  //setlength(AWSRec.GoboPos,AParams.GoboAnzahl);
  //setlength(AWSRec.oldGoboPos,AParams.GoboAnzahl);
  if Source.Levels[Round((Source.FreqCount/16)*3)]>Source.Levels[Round((Source.FreqCount/16)*3)+1] then begin
    tphi:=Source.Levels[Round((Source.FreqCount/16)*3)]*10;
  end else begin
    tphi:=-Source.Levels[Round((Source.FreqCount/16)*3)+1]*10;
  end;
  with AWSRec do for i:=0 to AParams.GoboAnzahl-1 do begin
    //InputBox('','','X: '+IntToStr(AWSRec.GoboPos[i].X)+'; Y: '+IntToStr(AWSRec.GoboPos[i].Y));
    //ShowMessage();
    {tx:= AWSRec.GoboPos^[i].x - AWSRec.oldGoboPos^[i].x;
    ty:= AWSRec.GoboPos^[i].y - AWSRec.oldGoboPos^[i].y;
    tx/= 1.1;
    ty/= 1.1;
    tx+= Bass*2000;
    ty+= Bass*2000;
    if AWSRec.GoboPos^[i].x<0 then tx+=abs(tx)/2 else tx-=abs(tx)/2;
    if AWSRec.GoboPos^[i].y<0 then ty+=abs(ty)/2 else ty-=abs(ty)/2;
    tx*=Bass*20;
    ty*=Bass*20;
    AWSRec.oldGoboPos^[i].x:=AWSRec.GoboPos^[i].x;
    AWSRec.oldGoboPos^[i].y:=AWSRec.GoboPos^[i].y;
    AWSRec.GoboPos^[i].x:=round(tx);
    AWSRec.GoboPos^[i].y:=round(ty);
    }
    //tr:= AWSRec.GoboPos^[i].R - AWSRec.oldGoboPos^[i].R;
    //tphi:= AWSRec.GoboPos^[i].Phi - AWSRec.oldGoboPos^[i].Phi;

    //tr:=GoboPos^[i].R;
    //tphi:=GoboPos^[i].Phi;
    //tr/= 1.1;
    //tphi/= 1.1;
    //tx+= Bass*2000;
    //ty+= Bass*2000;

    //Schredderwert
    ALimit:=AParams.Limit-(Source.Levels[GoboLevels2^[i]{+(Source.FreqCount div 8)}]*10);

    if Source.Levels[GoboLevels^[i]]{Bass}>ALimit then begin
      tr:=((Source.Levels[GoboLevels^[i]]{Bass}-ALimit)*AParams.Scale);
    end else begin
      tr:=-AWSRec.GoboPos^[i].R*(ALimit-Source.Levels[GoboLevels^[i]]{Bass});
    end;

    {if Source.Levels[Round((Source.FreqCount/16)*3)]>Source.Levels[Round((Source.FreqCount/16)*4)] then begin
      tphi:=Source.Levels[Round((Source.FreqCount/16)*3)]*10;
    end else begin
      tphi:=-Source.Levels[Round((Source.FreqCount/16)*3)]*10;
    end;}
    //tphi:=0.3;
    //if tphi>pi then tphi-=2*pi;
    //if tphi<-pi then tphi+=2*pi;

    //tr:=-AWSRec.GoboPos^[i].R*(0.02-Bass);
    //if AWSRec.GoboPos.R<0 then tr:=0;
    //tr-=(tr/2){+Source.Levels[Round((Source.FreqCount/AParams.GoboAnzahl)*i)]};
    //if AWSRec.GoboPos^[i].phi<0{bmp.width  div 2} then tx+=abs(tx)/2 else tx-=abs(tx{-(bmp.width/2)})/2;
    //if AWSRec.GoboPos^[i].r<0{bmp.height div 2} then tr+=abs(tr)/1.2; //else tr-=abs(tr{-(bmp.height/2)})/2;
    //tr*=Bass*20;
    //if tr<0 then tr*=0.5;
    //tx*=Bass*20;
    //ty*=Bass*20;

    //GoboPos^[i].R:=tr;
    //GoboPos^[i].Phi:=tphi;

    with GoboPos^[i] do begin
      R+=tr;
      Phi+=tphi;
      //PiCut(Phi);
      //if phi>pi then phi-=2*pi;
      //if phi<-pi then phi+=2*pi;
      tx:=Round((R*Sin(Phi))+(bmp.Width/2));
      ty:=Round((R*Cos(Phi))+(bmp.Height/2));
    end;
    AWSRec.oldGoboPos^[i].R:=AWSRec.GoboPos^[i].R;
    AWSRec.oldGoboPos^[i].Phi:=AWSRec.GoboPos^[i].Phi;
    bmp.FillRectTS(tx,ty,tx+AParams.GoboGroesse,ty+AParams.GoboGroesse,BetaBlend(Visualisation.C1,Visualisation.C2,Round(($FF/(Source.FreqCount div 8))*GoboLevels^[i]))  {Visualisation.C1+(Round(($FF/(Source.FreqCount div 8))*GoboLevels^[i])*$100)});
    //bmp.FillRectTS(R*Cos(AWSRec.GoboPos^[i].x),AWSRec.GoboPos^[i].y,AWSRec.GoboPos^[i].x+AParams.GoboGroesse,AWSRec.GoboPos^[i].y+AParams.GoboGroesse,$FFFFFFFF{Visualisation.C1});
    //bmp.FillRectTS(0,0,100,100,$FFFFFFFF{Visualisation.C1});

  end;*)
  (*bmp.FillRectTS(AWSRec.GoboPos[0].x,AWSRec.GoboPos[0].y,AWSRec.GoboPos[0].x+AParams.GoboGroesse,AWSRec.GoboPos[0].y+AParams.GoboGroesse,$FFFFFFFF{Visualisation.C1});
  bmp.FillRectTS(0,0,100,100,$FFFFFFFF{Visualisation.C1});*)
end;

procedure VisRotate(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
{var
  AParams     : TVisRotateParams;
  Trans       : TAffineTransformation;
  bmp2,bmp3   : TBitmap32;
  Offset,XS,YS: Integer;}
  //Trans0: TProjectiveTransformation;
var
  AParams: TVisRotateParams absolute Params;
begin
  Dest.Rotate(AParams.Speed,Visualisation.C3);
  {GetVisParams(Visualisation,AParams);
  Trans:=TAffineTransformation.Create;
  //Trans.
  bmp2:=TBitmap32.Create;
  bmp3:=TBitmap32.Create;
  bmp2.BeginUpdate;
  //bmp2.Resampler.Destroy;
  bmp3.BeginUpdate;
  //bmp2.Resampler:=TKernelResampler.Create;
  TLinearResampler.Create(bmp2);
  //TKernelResampler.Create(bmp2);
  //TKernelResampler(bmp2.Resampler).Kernel:=THammingKernel.Create;
  Offset:=Trunc(sqrt(sqr(bmp.Width)+sqr(bmp.Height)))+1;
  XS:=(Offset-bmp.Width) div 2;
  YS:=(Offset-bmp.Height) div 2;
  bmp2.Width:=Offset{bmp.Width+60};
  bmp2.Height:=Offset;
  bmp3.Width:=Offset;
  bmp3.Height:=Offset;
  bmp2.Clear(Visualisation.C3);
  bmp2.Draw(XS,YS,bmp);
  bmp2.EndUpdate;
  bmp2.Changed;
  //bmp.BeginUpdate;
  //TKernelResampler.Create(bmp);
  //TKernelResampler(bmp.Resampler).Kernel:=THammingKernel.Create;
  //bmp.dr
  //bmp.Clear(Visualisation.C3);
  //bmp.FillRectTS(0,0,bmp.Width,bmp.Height,Visualisation.C3);
  Trans.Clear;
  //Trans.
  Trans.SrcRect:=FloatRect(XS,YS,XS-1+bmp.Width,YS-1+bmp.Height);
  Trans.Rotate(Offset div 2,Offset div 2,AParams.Speed);
  //bmp.Clear(Visualisation.C3);
  Transform(bmp3,bmp2,Trans);
  bmp3.EndUpdate;
  bmp3.Changed;
  bmp.Draw(-XS,-YS,bmp3);
  //bmp.EndUpdate;
  //bmp.Changed;
  Trans.Destroy;
  bmp2.Destroy;
  bmp3.Destroy;}
end;

procedure VisCrazyScale(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
{var
  AParams     : TVisZoomParams;
  Trans       : TAffineTransformation;
  bmp2,bmp3   : TBitmap32;}
  //Offset,XS,YS: Integer;
var
  AParams: TVisZoomParams absolute Params;
begin
  Dest.Zoom(AParams.X,AParams.Y,Visualisation.C3);
 {GetVisParams(Visualisation,AParams);
  Trans:=TAffineTransformation.Create;
  bmp2:=TBitmap32.Create;
  bmp3:=TBitmap32.Create;
  bmp2.BeginUpdate;
  bmp3.BeginUpdate;
  TLinearResampler.Create(bmp2);
  //Offset:=Trunc(sqrt(sqr(bmp.Width)+sqr(bmp.Height)))+1;
  //XS:=(Offset-bmp.Width) div 2;
  //YS:=(Offset-bmp.Height) div 2;
  bmp2.Width:=bmp.Width;
  bmp2.Height:=bmp.Height;
  bmp3.Width:=bmp.Width;
  bmp3.Height:=bmp.Height;
  bmp2.Clear(Visualisation.C3);
  bmp2.Draw(0,0,bmp);
  bmp2.EndUpdate;
  bmp2.Changed;
  Trans.Clear;
  Trans.SrcRect:=FloatRect(0,0,bmp.Width,bmp.Height);
  Trans.Scale(AParams.X,AParams.Y);
  //.Rotate(Offset div 2,Offset div 2,AParams.Speed);
  Transform(bmp3,bmp2,Trans);
  bmp3.EndUpdate;
  bmp3.Changed;
  bmp.Draw(0,0,bmp3);
  Trans.Destroy;
  bmp2.Destroy;
  bmp3.Destroy;}
end;

procedure VisZoom(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
{var
  AParams     : TVisZoomParams;
  Trans       : TAffineTransformation;
  bmp2,bmp3   : TBitmap32;
  //Offset,XS,YS: Integer;
  XS,YS,NW,NH  : Integer; }
var
  AParams: TVisZoomParams absolute Params;
begin
  Dest.Zoom(AParams.X,AParams.Y,Visualisation.C3);
  {GetVisParams(Visualisation,AParams);
  Trans:=TAffineTransformation.Create;
  bmp2:=TBitmap32.Create;
  bmp3:=TBitmap32.Create;
  bmp2.BeginUpdate;
  bmp3.BeginUpdate;
  TLinearResampler.Create(bmp2);
  //Offset:=Trunc(sqrt(sqr(bmp.Width)+sqr(bmp.Height)))+1;
  //XS:=(Offset-bmp.Width) div 2;
  //YS:=(Offset-bmp.Height) div 2;
  //XS:=bmp
  XS:=Round(((AParams.X*bmp.Width)-bmp.Width)/2);
  YS:=Round(((AParams.Y*bmp.Height)-bmp.Height)/2);

  NW:=Round(bmp.Width*AParams.X);
  NH:=Round(bmp.Height*AParams.Y);
  bmp2.Width:=bmp.Width;
  bmp2.Height:=bmp.Height;
  bmp3.Width:=NW;
  bmp3.Height:=NH;
  bmp2.Clear(Visualisation.C3);
  bmp2.Draw(0,0,bmp);
  bmp2.EndUpdate;
  bmp2.Changed;
  Trans.Clear;
  Trans.SrcRect:=FloatRect(0,0,bmp.Width,bmp.Height);
  Trans.Scale(AParams.X,AParams.Y);
  //.Rotate(Offset div 2,Offset div 2,AParams.Speed);
  Transform(bmp3,bmp2,Trans);
  bmp3.EndUpdate;
  bmp3.Changed;
  bmp.Draw(-XS,-YS,bmp3);
  Trans.Destroy;
  bmp2.Destroy;
  bmp3.Destroy;}
end;

procedure VisMove(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
{var
  AParams: TVisMoveParams;
  //Trans  : TAffineTransformation;
  bmp2   : TBitmap32;
  //Trans0: TProjectiveTransformation;}
var
  AParams: TVisMoveParams absolute Params;
begin
  Dest.Move(AParams.X,AParams.Y);
  {GetVisParams(Visualisation,AParams);
  //Trans:=TAffineTransformation.Create;
  //Trans.
  bmp2:=TBitmap32.Create;
  bmp2.BeginUpdate;
  //bmp2.Resampler.Destroy;
  //bmp2.Resampler:=TKernelResampler.Create;
  //TLinearResampler.Create(bmp2);
  //TKernelResampler.Create(bmp2);
  //TKernelResampler(bmp2.Resampler).Kernel:=THammingKernel.Create;
  bmp2.Width:=bmp.Width;
  bmp2.Height:=bmp.Height;
  bmp2.Draw(0,0,bmp);
  bmp2.EndUpdate;
  bmp2.Changed;
  //bmp.Clear(Visualisation.C3);
  bmp.Draw(AParams.X,AParams.Y,bmp2);
  //bmp.BeginUpdate;
  //TKernelResampler.Create(bmp);
  //TKernelResampler(bmp.Resampler).Kernel:=THammingKernel.Create;
  //bmp.dr
  //Trans.Clear;
  //Trans.
  //Trans.SrcRect:=FloatRect(0,0,bmp.Width-1,bmp.Height-1);
  //Trans. Rotate(bmp.Width div 2,bmp.Height div 2,AParams.Speed);
  //Transform(bmp,bmp2,Trans);
  //bmp.EndUpdate;
  //bmp.Changed;
  //Trans.Destroy;
  bmp2.Destroy;}
end;

end.

