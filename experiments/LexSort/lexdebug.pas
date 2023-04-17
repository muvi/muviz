unit LexDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LexCompare, csl;

function LexDebug(AStr: TLexedString): string;

implementation

function LexDebug(AStr: TLexedString): string;
var
  ATokenObject: TObject;
  AToken      : TToken;
begin
  Result:='';
  for ATokenObject in AStr do begin
    Assert(ATokenObject <> nil);
    Assert(ATokenObject.InheritsFrom(TToken));

    AToken:=TToken(ATokenObject);
    case AToken.ID of
      tiInteger: begin
          Assert(AToken.InheritsFrom(TIntegerToken));
          Result += '[' + IntToStr(TIntegerToken(AToken).Value) + ']';
        end;
      tiString : begin
          Assert(AToken.InheritsFrom(TStringToken));
          Result += '<' + TStringToken(AToken).Value + '>';
        end
      else Assert(false);
    end;
  end;
end;

end.

