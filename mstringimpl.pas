unit MStringImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MStrings, Dialogs, TVSPSources, TVSPSourceUtil;

type
  TMString = class (TInterfacedObject, IString)
  private
    FSource: ITVSPSource;
    function GetPasStr: string; cdecl;
    function GetCStr: PChar; cdecl;
    function GetChar(Index: LongInt): Char; cdecl;
    function GetLength: LongInt; cdecl;
    function GetTVSPSource: ITVSPSource; cdecl;
    function Concat(S: IString): IString; cdecl;
    function FromPasStr(S: string): IString; cdecl;
    function FromCStr(S: PChar): IString; cdecl;
    function FromTVSPSource(ASource: ITVSPSource): IString; cdecl;
    function Equals(Other: IString): Boolean; cdecl; reintroduce;
  public
    constructor Create(ASource: ITVSPSource);
    destructor Destroy; override;
  end;

procedure Init;
procedure Done;

implementation

{%REGION TMString}

constructor TMString.Create(ASource: ITVSPSource);
begin
  Assert(ASource <> nil);
  inherited Create;
  FSource:=ASource;
end;

destructor TMString.Destroy;
begin
  FSource:=nil;
  inherited Destroy;
end;

function TMString.GetPasStr: string; cdecl;
var
  I, L: LongWord;
begin
  L:=FSource.Size;
  if L > 0 then begin
    SetLength(Result, L);
    for I:=0 to L-1
      do FSource.Get(I, 1, Result[I+1]);
  end else Result:='';
end;

function TMString.GetCStr: PChar; cdecl;
begin
  Result:=StrAlloc(FSource.Size);
  FSource.Get(0, FSource.Size, Result^);
end;

function TMString.GetChar(Index: LongInt): Char; cdecl;
begin
  Assert((Index > 0) and (Index <= FSource.Size));
  FSource.Get(Index-1, 1, Result);
end;

function TMString.GetLength: LongInt; cdecl;
begin
  Result:=FSource.Size;
end;

function TMString.GetTVSPSource: ITVSPSource; cdecl;
begin
  Result:=FSource;
end;

function TMString.Concat(S: IString): IString; cdecl;
begin
  //TODO: optimize
  Result:=FromPasStr(GetPasStr + S.PasStr);
end;

function TMString.FromPasStr(S: string): IString; cdecl;
var
  ANewSource: ITVSPSource;
  ANewGUID  : TGUID;
  I         : Integer;
begin
  if Length(S) > 0 then begin
    CreateGUID(ANewGUID);
    ANewSource:=SourceUtil[TVSPSourceID(Length(S), ANewGUID)];
    for I:=0 to ANewSource.Size-1
      do ANewSource.&Set(I, 1, S[I+1]);
    Result:=TMString.Create(ANewSource);
  end else Result:=EmptyString;
end;

function TMString.FromCStr(S: PChar): IString; cdecl;
var
  ANewSource: ITVSPSource;
  ANewGUID  : TGUID;
  L         : Integer;
begin
  CreateGUID(ANewGUID);
  L:=Length(S);
  ANewSource:=SourceUtil.Sources[TVSPSourceID(L, ANewGUID)];
  ANewSource.&Set(0, L, S^);
  Result:=TMString.Create(ANewSource);
end;

function TMString.FromTVSPSource(ASource: ITVSPSource): IString; cdecl;
begin
  Result:=TMString.Create(ASource);
end;

function TMString.Equals(Other: IString): Boolean; cdecl;
var
  I: Integer;
begin
  Assert(Other<>nil);
  Assert(FSource <> nil);
  Result:=FSource.Size = Other.Length;
  if Result
    then for I:=1 to FSource.Size
      do if GetChar(I) <> Other.Chars[I]
        then begin
          Result:=false;
          exit;
        end;
end;

{%ENDREGION}

procedure Init;
begin
  EmptyString:=TMString.Create(SourceUtil.Sources[ZEROSOURCEID]);
end;

procedure Done;
begin
  EmptyString:=nil;
end;

end.

