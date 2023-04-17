unit GUIDop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

operator < (const m1,m2: TGUID): Boolean;
operator <= (const m1,m2: TGUID): Boolean;
operator > (const m1,m2: TGUID): Boolean;
operator >= (const m1,m2: TGUID): Boolean;
operator = (const m1,m2: TGUID): Boolean;

const
  ZEROGUID: TGUID = '{EDC80759-D14A-4893-83E2-C8C3369B00DB}';

implementation

{%REGION Operators}

type
  TCGUID= packed record
    hi,low: UInt64;
  end;

operator < (const m1,m2: TGUID): Boolean;
var
  m1a: TCGUID absolute m1;
  m2a: TCGUID absolute m2;
begin
  if (m1a.hi<m2a.hi)
    then Result:=true
    else if (m1a.hi=m2a.hi)
      then Result:=(m1a.low<m2a.low);
end;

operator <= (const m1,m2: TGUID): Boolean;
var
  m1a: TCGUID absolute m1;
  m2a: TCGUID absolute m2;
begin
  if (m1a.hi<m2a.hi)
    then Result:=true
    else if (m1a.hi=m2a.hi)
      then Result:=(m1a.low<=m2a.low);
end;

operator > (const m1,m2: TGUID): Boolean;
var
  m1a: TCGUID absolute m1;
  m2a: TCGUID absolute m2;
begin
  if (m1a.hi>m2a.hi)
    then Result:=true
    else if (m1a.hi=m2a.hi)
      then Result:=(m1a.low>m2a.low);
end;

operator >= (const m1,m2: TGUID): Boolean;
var
  m1a: TCGUID absolute m1;
  m2a: TCGUID absolute m2;
begin
  if (m1a.hi>m2a.hi)
    then Result:=true
    else if (m1a.hi=m2a.hi)
      then Result:=(m1a.low>=m2a.low);
end;

operator = (const m1,m2: TGUID): Boolean;
var
  m1a: TCGUID absolute m1;
  m2a: TCGUID absolute m2;
begin
  Result:=((m1a.hi=m2a.hi) and (m1a.low=m2a.low));
end;

{%ENDREGION}

end.

