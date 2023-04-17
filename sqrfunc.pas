unit SqrFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvCoord, Math;

type
  TSqrFunc = record
    a,b,c: Real;
  end;

function SqrFunc(const aa,bb,cc: Real): TSqrFunc; overload;
function SqrFunc(const P1,P2: TRealPoint; const aa: Real): TSqrFunc; overload;
function SqrFunc(const P1,P2: TPoint; const aa: Real): TSqrFunc; overload;
function ExecFunc(const AFunc: TSqrFunc; const X: Real): Real; overload;

implementation

function SqrFunc(const aa,bb,cc: Real): TSqrFunc;
begin
  with Result do begin
    a:=aa;
    b:=bb;
    c:=cc;
  end;
end;

function ExecFunc(const AFunc: TSqrFunc; const X: Real): Real;
begin
  with AFunc do Result:=a*sqr(X)+b*x+c;
end;

function SqrFunc(const P1,P2: TRealPoint; const aa: Real): TSqrFunc;
begin
  with Result do begin
    a:=aa;
    b:=((P2.Y-P1.Y)/(P2.X-P1.X))-(a*(P1.X+P2.X));
    c:=P1.Y-a*sqr(P1.X)-b*P1.X;
  end;
end;

function SqrFunc(const P1,P2: TPoint; const aa: Real): TSqrFunc; overload;
begin
  Result:=SqrFunc(RealPoint(P1),RealPoint(P2),aa);
end;

end.

