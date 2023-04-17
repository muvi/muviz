unit MStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPSources;

type
  IString    = interface
    function GetPasStr: string; cdecl;
    function GetCStr: PChar; cdecl;
    function GetChar(Index: LongInt): Char; cdecl;
    function GetLength: LongInt; cdecl;
    function GetTVSPSource: ITVSPSource; cdecl;
    function Concat(S: IString): IString; cdecl;
    function FromPasStr(S: string): IString; cdecl;
    function FromCStr(S: PChar): IString; cdecl;
    function Equals(Other: IString): Boolean; cdecl;
    function FromTVSPSource(ASource: ITVSPSource): IString; cdecl;

    property Chars[Index: LongInt]: Char read GetChar; default;
    //Returns a new C str. Do not forget to dispose after use.
    property CStr: PChar read GetCStr;
    property Length: LongInt read GetLength;
    property PasStr: string read GetPasStr;
    property TVSPSource: ITVSPSource read GetTVSPSource;
  end;

//operator = (S1, S2: IString): Boolean;
operator := (S: string): IString;
operator := (S: IString): string;
operator := (S: ShortString): IString;
operator := (S: IString): ShortString;
operator := (S: PChar): IString;
operator := (S: IString): PChar;
operator + (S1, S2: IString): IString;
operator = (S1: IString; S2: string): Boolean;

var
  EmptyString: IString;

implementation

{%REGION IString - operators}

operator = (S1: IString; S2: string): Boolean;
begin
  Result:=S1.GetPasStr = S2;
end;

operator := (S: string): IString;
begin
  Result:=EmptyString.FromPasStr(S);
end;

operator := (S: IString): string;
begin
  Result:=S.PasStr;
end;

operator := (S: ShortString): IString;
begin
  Result:=EmptyString.FromPasStr(S);
end;

operator := (S: IString): ShortString;
begin
  Result:=S.PasStr;
end;

operator := (S: PChar): IString;
begin
  Result:=EmptyString.FromCStr(S);
end;

operator := (S: IString): PChar;
begin
  Result:=S.CStr;
end;

operator + (S1, S2: IString): IString;
begin
  Result:=S1.Concat(S2);
end;

{%ENDREGION}

end.

