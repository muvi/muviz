unit GraphX32;

{$mode objfpc}{$H+}

interface

uses
  VisType;

type
  TXBGR = packed record
    R,G,B,X: Byte;
  end;

  TARGB = packed record
    B,G,R,A: Byte;
  end;

function ColorToColor32(const Color: CvColor; const Alpha: Byte = $FF): CvColor;
function Color32ToColor(const Color: CvColor): CvColor;
function BlendAlpha({const }Color,Blend: CvColor): CvColor;
function BetaBlend(const Color1,Color2: CvColor; const Beta: Byte): CvColor;
procedure PiCut(var X: Real);

implementation

function ColorToColor32(const Color: CvColor; const Alpha: Byte = $FF): CvColor;
begin
  Result:=(Color and $0000FF00) or (Alpha shl 24) or ((Color and $00FF0000) shr 16) or ((Color and $000000FF) shl 16);
end;

function Color32ToColor(const Color: CvColor): CvColor;
begin
  Result:=(Color and $0000FF00) or ((Color and $00FF0000) shr 16) or ((Color and $000000FF) shl 16);
end;

function BlendAlpha({const }Color,Blend: CvColor): CvColor;
var
  Color2 : TARGB absolute Color;
  Result2: TARGB absolute Result;
  Blend2 : TARGB absolute Blend;
begin
  Result2.R:=(Blend2.R*Blend2.A+Color2.R*($FF-Blend2.A)) div $FF;
  Result2.G:=(Blend2.G*Blend2.A+Color2.G*($FF-Blend2.A)) div $FF;
  Result2.B:=(Blend2.B*Blend2.A+Color2.B*($FF-Blend2.A)) div $FF;
  Result2.A:=Color2.A;
end;

function BetaBlend(const Color1,Color2: CvColor; const Beta: Byte): CvColor;
var
  AColor1: TARGB absolute Color1;
  AColor2: TARGB absolute Color2;
  Result2: TARGB absolute Result;
begin
  Result2.R:=(AColor1.R*Beta+AColor2.R*($FF-Beta)) div $FF;
  Result2.G:=(AColor1.G*Beta+AColor2.G*($FF-Beta)) div $FF;
  Result2.B:=(AColor1.B*Beta+AColor2.B*($FF-Beta)) div $FF;
  Result2.A:=(AColor1.A*Beta+AColor2.A*($FF-Beta)) div $FF;
end;

procedure PiCut(var X: Real);
begin
  if X>Pi
    then X-=(Round(X/Pi)+1)*Pi
    else if X<-Pi
      then X+=(-Round(X/Pi)+1)*Pi
end;

end.

