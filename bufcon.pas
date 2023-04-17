unit BufCon;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

procedure Int32ToSingle(const Source; var Dest; Count: Cardinal);

implementation

procedure Int32ToSingle(const Source; var Dest; Count: Cardinal);
var
  I  : Integer;
  Src: array [0..0] of LongInt absolute Source;
  Dst: array [0..0] of Single absolute Dest;
begin
  for I:=0 to Count-1 do Dst[I]:=Src[I]/MaxLongInt;
end;

end.

