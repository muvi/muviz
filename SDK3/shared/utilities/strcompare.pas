(*
  implements a utilities for a relation between strings, where a string can both
  be greater or less if the length is increased.
*)
unit StrCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(*
  Result:
    negative: S1 < S2
    0       : S1 = S2
    positive: S1 > S2
*)
function StrCompI(S1, S2: string): ShortInt;

implementation

function StrCompI(S1, S2: string): ShortInt;
var
  S1Index, S2Index: Integer;
begin
  S1Index:=1;
  S2Index:=1;


  for I:=1 to L do begin

  end;
end;

end.

