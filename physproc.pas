unit PhysProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvCoord, Math;

function WeightForce(m,g: Real): Real; inline;
function SpringForce(D,dl: Real): Real; inline; overload;
function SpringForce(const FP,P: TRealPoint; D,SpringLength: Real): TRealPoint; inline; overload;
function ForceToAcceleration(const AForce: TRealPoint; AWeight: Real): TRealPoint; inline;

implementation

function WeightForce(m,g: Real): Real; inline;
begin
  Result:=m*g;
end;

function SpringForce(D,dl: Real): Real; inline;
begin
  Result:=(-D)*dl
end;

function SpringForce(const FP,P: TRealPoint; D,SpringLength: Real): TRealPoint; inline;
var
  ADist,AForce{,ADirection}: Real;
  //ADirection2            : TRealPoint;
begin
  ADist:=Distance(FP,P);
  if IsZero(ADist) then begin
    Result:=ZEROCENTER;
    exit;
  end;
  AForce:=D*(ADist-SpringLength);

  {ADirection2:=FP-P;
  ADirection:=Arctan2(ADirection2.Y,ADirection2.X);

  with Result do begin
    X:=AForce*Cos(ADirection);
    Y:=AForce*Sin(ADirection);
  end; }
  Result:=(FP-P)*(AForce/ADist);

  {ADirection:=(APrevKnot-AKnot.Position)*(1/ADist);
  Result:=ADirection*AForceP;}
end;

function ForceToAcceleration(const AForce: TRealPoint; AWeight: Real): TRealPoint;
begin
  Result:=AForce/AWeight;
end;

end.

