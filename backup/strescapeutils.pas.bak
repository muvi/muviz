unit StrEscapeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCharacters = set of Char;

function EscapeStr(AStr: string; AAllowedChars: TCharacters): string;

const
  NUMBERSANDLETTERS = ['0'..'9', 'A'..'Z', 'a'..'z'];
  ADVANCEDCHARS     = ['0'..'9', 'A'..'Z', 'a'..'z', ',', '.', ';', ' ', '!', '?'];

implementation

function EscapeStr(AStr: string; AAllowedChars: TCharacters): string;
var
  I: Integer;
begin
  Result:='';
  for I:=1 to Length(AStr) do begin
    if AStr[I] in AAllowedChars
      then Result += AStr[I]
      else Result += '#' + IntToStr(Ord(AStr[I]));
  end;
end;

end.

