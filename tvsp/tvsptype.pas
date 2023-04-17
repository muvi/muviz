unit TVSPType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, TVSPSources;

type
  TTVSPMsgID         = LongWord;
  TTVSPIndex         = LongWord;
  TTVSPString        = {array [0..X] of} Char; //dynamic size (determined by size of the hole msg, no 0-determination)
  TTVSPPermission    = SmallInt;

const
  TVSPMAXPARAMMEMSIZE   = 255 + 255 * SizeOf(TTVSPSrcID);
  TVSPMAXPARAMVALUESIZE = 255;
  TVSPMAXSOURCES        = 255;

function TVSPParamValueSize(AType: TPParamType): Cardinal;
function TVSPParamMemSize(AType: TPParamType): Cardinal;
function TVSPParamSourceCount(AType: TPParamType): Cardinal;
function FromTVSPString(constref AStr: TTVSPString; ASize: Cardinal): string;
procedure ToTVSPString(const AStr: string; var AOutput: TTVSPString);

implementation

type
  TPParamTypeTVSPProperties = packed record
    TypePart1  : LongWord;
    TypePart2  : Word;
    SourceCount: Byte;
    ValueSize  : Byte;
  end;

function TVSPParamValueSize(AType: TPParamType): Cardinal;
begin
  Result:=TPParamTypeTVSPProperties(AType).ValueSize;
end;

function TVSPParamMemSize(AType: TPParamType): Cardinal;
begin
  with TPParamTypeTVSPProperties(AType)
    do Result:=ValueSize + SourceCount * SizeOf(TTVSPSrcID);
end;

function TVSPParamSourceCount(AType: TPParamType): Cardinal;
begin
  Result:=TPParamTypeTVSPProperties(AType).SourceCount;
end;

function FromTVSPString(constref AStr: TTVSPString; ASize: Cardinal): string;
var
  I   : Integer;
  APos: ^TTVSPString;
begin
  Result:='';
  APos:=@AStr;
  for I:=0 to ASize-1 do begin
    Result += APos^;
    Inc(APos);
  end;
end;

procedure ToTVSPString(const AStr: string; var AOutput: TTVSPString);
var
  I   : Integer;
  APos: ^TTVSPString;
begin
  APos:=@AOutput;
  for I:=1 to Length(AStr) do begin
    APos^:=AStr[I];
    Inc(APos);
  end;
end;

end.

