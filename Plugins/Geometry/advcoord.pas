unit AdvCoord;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math;

type
  TPhiR         = record
    R,Phi: Real;
  end;

  TRealPoint    = record
    X,Y: Real;
  end;

  TRR           = record
    RP1,RP2: Real;
  end;

const
  ZeroCenter: TRealPoint = (X:Real(0.0);Y:Real(0.0));

function RealPoint(const X,Y: Real): TRealPoint; overload;
function RealPoint(const P: TPoint): TRealPoint; overload;
function RealPoint(const PR: TPhiR; const Center: TRealPoint): TRealPoint; overload;
function RealPoint(const PR: TPhiR; const Center: TPoint): TRealPoint; overload;
function PhiR(const R,Phi: Real): TPhiR; overload;
function PhiR(const P: TRealPoint; const Center: TRealPoint): TPhiR; overload;
function PhiR(const P: TRealPoint; const Center: TPoint): TPhiR; overload;
function PhiR(const P: TPoint; const Center: TRealPoint): TPhiR; overload;
function PhiR(const P: TPoint; const Center: TPoint): TPhiR; overload;
function Point(const P: TRealPoint): TPoint; overload;

function CutPoint(const RP1,RP2: Real; const P1,P2: TRealPoint): TRealPoint; overload;
function RR(const PR,P1,P2: TRealPoint): TRR;
function arccos2(const Y,X: Real): Real;
function RToPhi(const R1,R2,BowLength: Real): Real;

implementation

function RealPoint(const X,Y: Real): TRealPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

function RealPoint(const P: TPoint): TRealPoint;
begin
  Result.X:=P.X;
  Result.Y:=P.Y;
end;

function RealPoint(const PR: TPhiR; const Center: TRealPoint): TRealPoint;
begin
  with PR do begin
    Result.X:=(R*Sin(Phi))+Center.X;
    Result.Y:=(R*Cos(Phi))+Center.Y;
  end;
end;

function RealPoint(const PR: TPhiR; const Center: TPoint): TRealPoint;
begin
  with PR do begin
    Result.X:=(R*Sin(Phi))+Center.X;
    Result.Y:=(R*Cos(Phi))+Center.Y;
  end;
end;

function PhiR(const R,Phi: Real): TPhiR;
begin
  Result.R:=R;
  Result.Phi:=Phi;
end;

function PhiR(const P: TRealPoint; const Center: TRealPoint): TPhiR;
var
  PX,PY: Real;
begin
  PX:=P.X-Center.X;
  PY:=P.Y-Center.Y;
  Result.R:=sqrt(sqr(PX)+sqr(PY));
  Result.Phi:=ArcTan2(PY,PX);
end;

function PhiR(const P: TRealPoint; const Center: TPoint): TPhiR;
var
  PX,PY: Real;
begin
  PX:=P.X-Center.X;
  PY:=P.Y-Center.Y;
  Result.R:=sqrt(sqr(PX)+sqr(PY));
  Result.Phi:=ArcTan2(PY,PX);
end;

function PhiR(const P: TPoint; const Center: TRealPoint): TPhiR;
var
  PX,PY: Real;
begin
  PX:=P.X-Center.X;
  PY:=P.Y-Center.Y;
  Result.R:=sqrt(sqr(PX)+sqr(PY));
  Result.Phi:=ArcTan2(PY,PX);
end;

function PhiR(const P: TPoint; const Center: TPoint): TPhiR;
var
  PX,PY: Real;
begin
  PX:=P.X-Center.X;
  PY:=P.Y-Center.Y;
  Result.R:=sqrt(sqr(PX)+sqr(PY));
  Result.Phi:=ArcTan2(PY,PX);
end;

function Point(const P: TRealPoint): TPoint;
begin
  Result.X:=Round(P.X);
  Result.Y:=Round(P.Y);
end;

function arccos2(const Y,X: Real): Real;
begin
  Result:=arccos(Y/X);                     if Y<0 then Result:=-Result;
  if X<0 then Result+=Pi/2;
end;

function CutPoint(const RP1,RP2: Real; const P1,P2: TRealPoint): TRealPoint;
var
  {hyp: Real;
  Phi: Real;      }
  a,h,sqrb,sqrc,beta,gamma: Real;
begin
  if IsZero(RP1) then begin
    Result:=P1;
    exit;
  end;
  if (IsZero(P2.X-P1.X) and IsZero(P2.Y-P1.Y)) then begin
    Result.X:=P1.X;
    Result.Y:=P1.Y+RP1;
    exit;
  end;
  a:=sqrt(sqr(P2.X-P1.X)+sqr(P2.Y-P1.Y));
  if RP1+RP2<a then begin
    Result:=P1;
    exit;
  end;
  sqrb:=sqr(RP2);
  sqrc:=sqr(RP1);
  h:=sqrt(sqrb-sqr((sqr(a)+sqrb-sqrc)/(2*a)));
  beta:=(Pi/2)-arccos2(h,RP1);
  //gamma:=Arctan2(P2.Y-P1.Y,P2.X-P1.X);
  {if beta>=0
    then beta:=beta+gamma
    else beta:=-beta+gamma;}
  gamma:=0{Arctan2(P2.Y-P1.Y,P2.X-P1.X)};
  Result.X:=(RP1*sin(beta{+Pi*(3/4)}+gamma))+P1.X;
  Result.Y:=(RP1*cos(beta{+Pi*(3/4)}+gamma))+P1.Y;
  {hyp:=sqrt(sqr(P2.X-P1.X)+sqr(P2.Y-P1.Y));
  Phi:=ArcCos(RR.RP1)}
end;

function RR(const PR,P1,P2: TRealPoint): TRR;
begin

end;

function RToPhi(const R1,R2,BowLength: Real): Real;
var
  BowLength2: Real;
begin
  BowLength2:=(R1/R2)*BowLength;
  Result:=Sin((BowLength2/2)/R1)*2;
end;

end.

