(*
  Contains functions related to parameter types
*)

unit ParamFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, AdvFunc, Math, ParamType2,
  VisType2, MStrings;

function FloatBufferToString(ABuffer: TVBuffer): string; inline;
function StringToFloatBuffer(S: string): TVBuffer; inline;
function StrToPointer(S: string): TVPointer; inline;
function PointerToStr(APtr: TVPointer): string; //inline;
procedure CharsToPtr(Ptr: Pointer; S: string); inline;
function CharsFromPtr(Ptr: Pointer; Count: Integer): string; inline;
function ParamIDSize(AID: TPParamID): Cardinal;
procedure ParamIDToPtr(Ptr: Pointer; AID: TPParamID);
function ParamIDFromPtr(Ptr: Pointer): TPParamID;
function FloatToColor(X: TVFloat): TVColor;
function FloatToInteger(X: TVFloat): TVInteger;

const
  TRUECHARS    : set of Char = ['J','T','W','Y','S','j','t','w','y','s'];
  PRESETDIVIDER: Char = '.';

implementation

const
  BufferItemKomma      : Char = ',';
var
  //modified in initialization
  BufferItemNumbers    : set of Char = ['0','1','2','3','4','5','6','7','8','9','-','e','E'];
  BufferItemIgnoreChars: set of Char = ['.',',','_'];
  BufferItemSeperators : set of Char = [#0..#255];

{%REGION General}

function FloatBufferToString(ABuffer: TVBuffer): string; inline;
var
  I: Integer;
begin
  if ABuffer.Size<1 then begin
    Result:='';
    exit;
  end;
  Result:=FloatToStrF(ABuffer[0],ffFixed,7,2);
  for I:=1 to ABuffer.Size-1 do Result+=' ;'+FloatToStrF(ABuffer[I],ffFixed,7,2);
end;

function CountBufferFromString(S: string): Integer; inline;
var
  I               : Integer;
  PrevWasSeperator: Boolean;
begin
  PrevWasSeperator:=true;
  Result:=0;
  for I:=1 to Length(S) do begin
    if (S[I] in BufferItemNumbers) and PrevWasSeperator then begin
      Inc(Result);
      PrevWasSeperator:=false
    end;
    if S[I] in BufferItemSeperators
      then PrevWasSeperator:=true;
  end;
end;

procedure InsertFromString(S: string; ABuffer: TVBuffer); inline;
var
  I,AIndex        : Integer;
  PrevWasSeperator: Boolean;
  AItem           : string;
begin
  PrevWasSeperator:=true;
  AItem:='';
  AIndex:=0;
  for I:=1 to Length(S) do begin
    if S[I] in BufferItemNumbers then begin
      AItem+=S[I];
      PrevWasSeperator:=false
    end;
    if S[I] in BufferItemSeperators then begin
      if not PrevWasSeperator
        then ABuffer[AIndex]:=StrToFloatE(AItem, NaN);
      AItem:='';
      Inc(AIndex);
      PrevWasSeperator:=true;
    end;
  end;
  //add the last item
  if not PrevWasSeperator
    then ABuffer[AIndex]:=StrToFloatE(AItem, NaN);
end;

function StringToFloatBuffer(S: string): TVBuffer; inline;
var
  AItemCount: LongWord;
begin
  AItemCount:=CountBufferFromString(S);
  Result:=EmptyBuffer.Resize(AItemCount);
  InsertFromString(S, Result);
end;

procedure CharsToPtr(Ptr: Pointer; S: string); inline;
var
  I   : Integer;
  APtr: ^Char;
begin
  APtr:=Ptr;
  for I:=1 to Length(S) do begin
    APtr^:=S[I];
    Inc(APtr);
  end;
end;

function CharsFromPtr(Ptr: Pointer; Count: Integer): string; inline;
var
  I   : Integer;
  APtr: ^Char;
begin
  APtr:=Ptr;
  Result:='';
  for I:=0 to Count-1 do begin
    Result+=APtr^;
    Inc(APtr);
  end;
end;

function StrToPointer(S: string): TVPointer; inline;
begin
  Result:=DEFAULTPOINTER
end;

function PointerToStr(APtr: TVPointer): string; //inline;
begin
  if APtr.Output.Preset.Name.Equals('')
    then Result:=APtr.Output.Param.Name
    else Result:=APtr.Output.Preset.Name+PRESETDIVIDER+APtr.Output.Param.Name;
  if APtr.Input.Preset.Name.Equals('')
    then Result += '->' + APtr.Input.Param.Name
    else Result += '->' + APtr.Input.Preset.Name+PRESETDIVIDER+APtr.Input.Param.Name;
  Result += ' [' + IntToStr(APtr.InversePriority) + ']';
end;

function ParamIDSize(AID: TPParamID): Cardinal;
begin
  Result:=SizeOf(TPParamType)+SizeOf(LongWord)+AID.Name.Length;
end;

procedure ParamIDToPtr(Ptr: Pointer; AID: TPParamID);
var
  AType  : ^TPParamType;
  ALength: ^LongWord;
  AStr   : PChar;
begin
  AType:=Ptr;
  AType^:=AID.&Type;
  ALength:=Ptr+SizeOf(TPParamType);
  ALength^:=AID.Name.Length;
  AStr:=Ptr+SizeOf(TPParamType)+SizeOf(LongWord);
  CharsToPtr(AID.Name, AStr);
end;

function ParamIDFromPtr(Ptr: Pointer): TPParamID;
var
  AType  : ^TPParamType;
  ALength: ^LongWord;
begin
  AType:=Ptr;
  Result.&Type:=AType^;
  ALength:=Ptr+SizeOf(TPParamType);
  Result.Name:=PChar(CharsFromPtr(Ptr+SizeOf(TPParamType)+SizeOf(LongWord), ALength^));
end;

function FloatToColor(X: TVFloat): TVColor;
begin
  if isNan(X) or (X < 0.0)
    then Result:=$00000000
    else if X > $FFFFFFFF
      then Result:=$FFFFFFFF
      else Result:=Round(X);
end;

function FloatToInteger(X: TVFloat): TVInteger;
begin
  if isNan(X)
    then Result:=0
    else if X > MaxInt
      then Result:=MaxInt
      else if X <= not MaxInt
        then Result:=not MaxInt
        else Result:=Round(X);
end;

{%ENDREGION}

initialization
  BufferItemNumbers+=[BufferItemKomma];
  BufferItemIgnoreChars-=[BufferItemKomma];
  BufferItemSeperators-=BufferItemNumbers+BufferItemIgnoreChars;
end.

