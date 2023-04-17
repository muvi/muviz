unit UniqueStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GloballyUniqueString: string;
function GloballyUniqueEnglishText: string;

implementation

type
  TGloballyUniqueString = record
  case Boolean of
    true : (GUID : TGUID);
    //not SizeOf(TGUID)-1 do it this way to have an empty buffer for the last 4 dummy bits
    false: (Bytes: array [0..SizeOf(TGUID)] of Byte);
  end;

const
  UIDStrAlphabetLength  = 64;
  UIDStrAlphabet       : array [0..UIDStrAlphabetLength-1] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '_',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z', '-'
    );

  // (SizeOf(TGUID) = 16) * (BitSizeOf(Byte) = 8) / (BitSizeOf(UIDSTrAlphabetLength))
  //   = 21.33
  UIDStrLength = 22;

function GloballyUniqueString: string;
var
  AID  : TGloballyUniqueString;
  AStr : array [0..UIDStrLength - 1] of Char;
  I    : Integer;

  function GetChar(ABitPos: Integer): Char;
  var
    ATempBytePos, ATempBitPos: Integer;
  begin
    ATempBytePos:=ABitPos div 8;
    ATempBitPos:=ABitPos mod 8;
    Result:=UIDStrAlphabet[
        ((AID.Bytes[ATempBytePos] shr ATempBitPos)
        or (AID.Bytes[ATempBytePos + 1] shl (8 - ATempBitPos)) )
        and 63];
  end;

begin
  CreateGUID(AID.GUID);
  for I:=0 to UIDStrLength-1
    do AStr[I]:=GetChar(I*6);
  Result:=AStr;
end;

type
  TWordPosition = (wmFront, wmMiddle, wmEnd);

function WordPositionToConsonantPosition(AWordPosition: TWordPosition; AStartsWithVocale, AHasSpace: Boolean): TWordPosition;
begin
  if AHasSpace then begin
    if AStartsWithVocale
      then Result:=wmFront
      else Result:=wmEnd;
  end else begin
    if AStartsWithVocale then begin
      if AWordPosition = wmFront
        then Result:=wmMiddle
        else Result:=AWordPosition;
    end else begin
      if AWordPosition = wmEnd
        then Result:=wmMiddle
        else Result:=AWordPosition;
    end;
  end;
end;

const
  UIDVocaleAlphabet   : array [0..7] of string =
    ('a', 'e', 'i', 'o', 'u', 'ai', 'y', 'oo');
  UIDConsonantAlphabet: array [TWordPosition] of array [0..31] of string =
    (('b', 'c', 'd', 'f', 'g', 'h', 'j', 'k',
    'l', 'm', 'n', 'p', 'qu', 'r', 's', 't',
    'v', 'w', 'x', 'z', 'st', 'cl', 'th', 'br',
    'fl', 'gr', 'kn', 'pl', 'wr', 'str', 'br', 'cr'),
    ('b', 'd', 'f', 'g', 'k', 'l', 'm', 'n',
    'p', 'qu', 'rr', 's', 't', 'v', 'x', 'z',
    'st', 'pb', 'str', 'br', 'gh', 'nfl', 'mbl', 'dg',
    'ns', 'ld', 'bl', 'rk', 'pl', 'ck', 'nd', 'rc'),
    ('b', 'd', 'f', 'g', 'k', 'l', 'm', 'n',
    'p', 'r', 's', 't', 'v', 'w', 'x', 'z',
    'st', 'ck', 'nd', 'ld', 'gh', 'rk', 'ght', 'sk',
    'rd', 'tch', 'ng', 'mp', 'nt', 'sh', 'rt', 'ngs'));


function ByteToUniqueWord(AByte: Byte; APosition: TWordPosition; AStartsWithVocale, AHasSpace: Boolean): string;
begin
  if AStartsWithVocale then begin
    Result:=UIDVocaleAlphabet[AByte and 7];
    if AHasSpace
      then Result += ' ';
    Result += UIDConsonantAlphabet[WordPositionToCOnsonantPosition(APosition, AStartsWithVocale, AHasSpace)][AByte shr 3];
  end else begin
    Result:=UIDConsonantAlphabet[WordPositionToConsonantPosition(APosition, AStartsWithVocale, AHasSpace)][AByte and 31];
    if AHasSpace
      then Result += ' ';
    Result += UIDVocaleAlphabet[AByte shr 5];
  end;
end;

function GloballyUniqueEnglishText: string;
var
  AID: TGloballyUniqueString;
begin
  CreateGUID(AID.GUID);
  Result:=ByteToUniqueWord(AID.Bytes[0], wmFront, true, false)
    + ByteToUniqueWord(AID.Bytes[1], wmMiddle, true, false)
    + ByteToUniqueWord(AID.Bytes[2], wmMiddle, true, false)
    + ByteToUniqueWord(AID.Bytes[3], wmMiddle, true, true)
    + ByteToUniqueWord(AID.Bytes[4], wmEnd, true, false) + ' '
    + ByteToUniqueWord(AID.Bytes[5], wmEnd, true, false) + ' '
    + ByteToUniqueWord(AID.Bytes[6], wmFront, true, false)
    + ByteToUniqueWord(AID.Bytes[7], wmMiddle, true, false)
    + ByteToUniqueWord(AID.Bytes[8], wmMiddle, true, false)
    + ByteToUniqueWord(AID.Bytes[9], wmEnd, true, false) + ' '
    + ByteToUniqueWord(AID.Bytes[10], wmFront, true, false)
    + ByteToUniqueWord(AID.Bytes[11], wmMiddle, true, false)
    + ByteToUniqueWord(AID.Bytes[12], wmEnd, true, false) + ' '
    + ByteToUniqueWord(AID.Bytes[13], wmFront, true, false)
    + ByteToUniqueWord(AID.Bytes[14], wmMiddle, true, false)
    + ByteToUniqueWord(AID.Bytes[15], wmEnd, true, false)
end;

end.

