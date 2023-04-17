unit ParamOp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, MStrings;

operator + (m1: IPFloat; m2: TVFloat): TVFloat;
operator + (m1: TVFloat; m2: IPFloat): TVFloat;
operator + (m1: IPFloat; m2: IPFloat): TVFloat;
operator * (m1: IPFloat; m2: TVFloat): TVFloat;
operator * (m1: TVFloat; m2: IPFloat): TVFloat;
operator * (m1: IPFloat; m2: IPFloat): TVFloat;
operator - (m1: IPFloat; m2: TVFloat): TVFloat;
operator - (m1: TVFloat; m2: IPFloat): TVFloat;
operator - (m1: IPFloat; m2: IPFloat): TVFloat;
operator / (m1: IPFloat; m2: TVFloat): TVFloat;
operator / (m1: TVFloat; m2: IPFloat): TVFloat;
operator / (m1: IPFloat; m2: IPFloat): TVFloat;
operator + (m1: IPInteger; m2: TVInteger): TVInteger;
operator + (m1: TVInteger; m2: IPInteger): TVInteger;
operator + (m1: IPInteger; m2: IPInteger): TVInteger;
operator * (m1: IPInteger; m2: TVInteger): TVInteger;
operator * (m1: TVInteger; m2: IPInteger): TVInteger;
operator * (m1: IPInteger; m2: IPInteger): TVInteger;
operator - (m1: IPInteger; m2: TVInteger): TVInteger;
operator - (m1: TVInteger; m2: IPInteger): TVInteger;
operator - (m1: IPInteger; m2: IPInteger): TVInteger;
operator + (m1: IPString; m2: TVString): TVString;
operator + (m1: TVString; m2: IPString): TVString;
operator + (m1: IPString; m2: IPString): TVString;
operator < (m1: IPFloat; m2: TVFloat): Boolean;
operator < (m1: TVFloat; m2: IPFloat): Boolean;
operator < (m1: IPFloat; m2: IPFloat): Boolean;
operator < (m1: IPInteger; m2: TVInteger): Boolean;
operator < (m1: TVInteger; m2: IPInteger): Boolean;
operator < (m1: IPInteger; m2: IPInteger): Boolean;
operator > (m1: IPFloat; m2: TVFloat): Boolean;
operator > (m1: TVFloat; m2: IPFloat): Boolean;
operator > (m1: IPFloat; m2: IPFloat): Boolean;
operator > (m1: IPInteger; m2: TVInteger): Boolean;
operator > (m1: TVInteger; m2: IPInteger): Boolean;
operator > (m1: IPInteger; m2: IPInteger): Boolean;
operator <= (m1: IPFloat; m2: TVFloat): Boolean;
operator <= (m1: TVFloat; m2: IPFloat): Boolean;
operator <= (m1: IPFloat; m2: IPFloat): Boolean;
operator <= (m1: IPInteger; m2: TVInteger): Boolean;
operator <= (m1: TVInteger; m2: IPInteger): Boolean;
operator <= (m1: IPInteger; m2: IPInteger): Boolean;
operator >= (m1: IPFloat; m2: TVFloat): Boolean;
operator >= (m1: TVFloat; m2: IPFloat): Boolean;
operator >= (m1: IPFloat; m2: IPFloat): Boolean;
operator >= (m1: IPInteger; m2: TVInteger): Boolean;
operator >= (m1: TVInteger; m2: IPInteger): Boolean;
operator >= (m1: IPInteger; m2: IPInteger): Boolean;

implementation

operator + (m1: IPFloat; m2: TVFloat): TVFloat;
begin
  Result:=m1.Get + m2;
end;

operator + (m1: TVFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1 + m2.Get;
end;

operator + (m1: IPFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1.Get + m2.Get;
end;

operator * (m1: IPFloat; m2: TVFloat): TVFloat;
begin
  Result:=m1.Get * m2;
end;

operator * (m1: TVFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1 * m2.Get;
end;

operator * (m1: IPFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1.Get * m2.Get;
end;

operator - (m1: IPFloat; m2: TVFloat): TVFloat;
begin
  Result:=m1.Get - m2;
end;

operator - (m1: TVFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1 - m2.Get;
end;

operator - (m1: IPFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1.Get - m2.Get;
end;

operator / (m1: IPFloat; m2: TVFloat): TVFloat;
begin
  Result:=m1.Get / m2;
end;

operator / (m1: TVFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1 / m2.Get;
end;

operator / (m1: IPFloat; m2: IPFloat): TVFloat;
begin
  Result:=m1.Get / m2.Get;
end;

operator + (m1: IPInteger; m2: TVInteger): TVInteger;
begin
  Result:=m1.Get + m2;
end;

operator + (m1: TVInteger; m2: IPInteger): TVInteger;
begin
  Result:=m1 + m2.Get;
end;

operator + (m1: IPInteger; m2: IPInteger): TVInteger;
begin
  Result:=m1.Get + m2.Get;
end;

operator * (m1: IPInteger; m2: TVInteger): TVInteger;
begin
  Result:=m1.Get * m2;
end;

operator * (m1: TVInteger; m2: IPInteger): TVInteger;
begin
  Result:=m1 * m2.Get;
end;

operator * (m1: IPInteger; m2: IPInteger): TVInteger;
begin
  Result:=m1.Get * m2.Get;
end;

operator - (m1: IPInteger; m2: TVInteger): TVInteger;
begin
  Result:=m1.Get - m2;
end;

operator - (m1: TVInteger; m2: IPInteger): TVInteger;
begin
  Result:=m1 - m2.Get;
end;

operator - (m1: IPInteger; m2: IPInteger): TVInteger;
begin
  Result:=m1.Get - m2.Get;
end;

operator + (m1: IPString; m2: TVString): TVString;
begin
  Result:=m1.Get + m2;
end;

operator + (m1: TVString; m2: IPString): TVString;
begin
  Result:=m1 + m2.Get;
end;

operator + (m1: IPString; m2: IPString): TVString;
begin
  Result:=m1.Get + m2.Get;
end;

operator < (m1: IPFloat; m2: TVFloat): Boolean;
begin
  Result:=m1.Value < m2;
end;

operator < (m1: TVFloat; m2: IPFloat): Boolean;
begin
  Result:=m1 < m2.Value;
end;

operator < (m1: IPFloat; m2: IPFloat): Boolean;
begin
  Result:=m1.Value < m2.Value;
end;

operator < (m1: IPInteger; m2: TVInteger): Boolean;
begin
  Result:=m1.Value < m2;
end;

operator < (m1: TVInteger; m2: IPInteger): Boolean;
begin
  Result:=m1 < m2.Value;
end;

operator < (m1: IPInteger; m2: IPInteger): Boolean;
begin
  Result:=m1.Value < m2.Value;
end;

operator > (m1: IPFloat; m2: TVFloat): Boolean;
begin
  Result:=m1.Value > m2;
end;

operator > (m1: TVFloat; m2: IPFloat): Boolean;
begin
  Result:=m1 > m2.Value;
end;

operator > (m1: IPFloat; m2: IPFloat): Boolean;
begin
  Result:=m1.Value > m2.Value;
end;

operator > (m1: IPInteger; m2: TVInteger): Boolean;
begin
  Result:=m1.Value > m2;
end;

operator > (m1: TVInteger; m2: IPInteger): Boolean;
begin
  Result:=m1 > m2.Value;
end;

operator > (m1: IPInteger; m2: IPInteger): Boolean;
begin
  Result:=m1.Value > m2.Value;
end;

operator <= (m1: IPFloat; m2: TVFloat): Boolean;
begin
  Result:=m1.Value <= m2;
end;

operator <= (m1: TVFloat; m2: IPFloat): Boolean;
begin
  Result:=m1 <= m2.Value;
end;

operator <= (m1: IPFloat; m2: IPFloat): Boolean;
begin
  Result:=m1.Value <= m2.Value;
end;

operator <= (m1: IPInteger; m2: TVInteger): Boolean;
begin
  Result:=m1.Value <= m2;
end;

operator <= (m1: TVInteger; m2: IPInteger): Boolean;
begin
  Result:=m1 <= m2.Value;
end;

operator <= (m1: IPInteger; m2: IPInteger): Boolean;
begin
  Result:=m1.Value <= m2.Value;
end;

operator >= (m1: IPFloat; m2: TVFloat): Boolean;
begin
  Result:=m1.Value >= m2;
end;

operator >= (m1: TVFloat; m2: IPFloat): Boolean;
begin
  Result:=m1 >= m2.Value;
end;

operator >= (m1: IPFloat; m2: IPFloat): Boolean;
begin
  Result:=m1.Value >= m2.Value;
end;

operator >= (m1: IPInteger; m2: TVInteger): Boolean;
begin
  Result:=m1.Value >= m2;
end;

operator >= (m1: TVInteger; m2: IPInteger): Boolean;
begin
  Result:=m1 >= m2.Value;
end;

operator >= (m1: IPInteger; m2: IPInteger): Boolean;
begin
  Result:=m1.Value >= m2.Value;
end;

end.

