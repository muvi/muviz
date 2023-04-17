unit PluginType; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  MVFloat       = Single;
  MVBigFloat    = Double;
  MVBigFloat2   = Extended;
  MVInt         = LongWord;
  MVSInt        = LongInt;
  MVTime        = UInt64;
  MVSet         = type Word;
  MVSetItem     = type MVSet;
  MVPluginID    = array [0..31] of Char;
  MVVersion     = packed record
    Version,MainVersion: Byte;
    SubVersion         : Word;
  end;

  MVRect        = packed record
    Left,Top,Right,Bottom: MVSInt;
  end;

  TPluginLibInfo= packed record
    Name       : ShortString;
    Description: ShortString;
    Version    : MVVersion;
    Supported  : Boolean;
  end;

  IMInterface   = interface
    ['{78C02B09-125B-4786-9EF2-80CC9FE9DC35}']
    function GetVersion: MVVersion; stdcall;
    function Future(const Version: MVVersion): IMInterface; stdcall;
  end;

  IMPlugin      = interface (IMInterface)
    ['{CFE10EB4-5359-4406-9CF8-1A5FE070B22C}']
    function GetID: MVPluginID; stdcall;
    function GetName: ShortString; stdcall;
    procedure Init; stdcall;
    procedure Done; stdcall;
  end;

  IMPluginSystem= interface (IMInterface)
    ['{E65398DC-3387-42B5-A533-8A2E8A91A02B}']
    function GetPlugin(const ID: MVPluginID): IMPlugin; stdcall;
    procedure InstallPlugin(Plugin: IMPlugin); stdcall;
  end;

const
  ZeroVersion: MVVersion = (Version:0;MainVersion:0;SubVersion:0);

operator <= (const m1,m2: MVVersion): Boolean;
operator >= (const m1,m2: MVVersion): Boolean;
operator < (const m1,m2: MVVersion): Boolean;
operator > (const m1,m2: MVVersion): Boolean;
operator = (const m1,m2: MVVersion): Boolean;
operator := (const m: MVVersion) r: ShortString;
operator := (const m: ShortString) r: MVVersion;
operator := (const m: TRect) r: MVRect;
operator := (const m: MVRect) r: TRect;

implementation

operator <= (const m1,m2: MVVersion): Boolean;
var
  Am1: LongWord absolute m1;
  Am2: LongWord absolute m2;
begin
  Result:=Am1<=Am2;
end;

operator >= (const m1,m2: MVVersion): Boolean;
var
  Am1: LongWord absolute m1;
  Am2: LongWord absolute m2;
begin
  Result:=Am1>=Am2;
end;

operator < (const m1,m2: MVVersion): Boolean;
var
  Am1: LongWord absolute m1;
  Am2: LongWord absolute m2;
begin
  Result:=Am1<Am2;
end;

operator > (const m1,m2: MVVersion): Boolean;
var
  Am1: LongWord absolute m1;
  Am2: LongWord absolute m2;
begin
  Result:=Am1>Am2;
end;

operator = (const m1,m2: MVVersion): Boolean;
var
  Am1: LongWord absolute m1;
  Am2: LongWord absolute m2;
begin
  Result:=Am1=Am2;
end;

operator := (const m: MVVersion) r: ShortString;
begin
  Result:=IntToStr(m.Version)+'.'+IntToStr(m.MainVersion)+'.'+IntToStr(m.SubVersion);
end;

operator := (const m: ShortString) r: MVVersion;
var
  m2          : ShortString;
  mPos,TempInt: Integer;
begin
  mPos:=Pos('.',m);
  if TryStrToInt(Copy(m,1,mPos-1),TempInt)
    then r.Version:=TempInt
    else r.Version:=0;
  m2:=m;
  Delete(m2,1,mPos);
  mPos:=Pos('.',m2);
  if TryStrToInt(Copy(m2,1,mPos-1),TempInt)
    then r.MainVersion:=TempInt
    else r.MainVersion:=0;
  Delete(m2,1,mPos);
  if TryStrToInt(m2,TempInt)
    then r.SubVersion:=TempInt
    else r.SubVersion:=0;
end;

operator := (const m: TRect) r: MVRect;
begin
  with Result do begin
    Left:=m.Left;
    Top:=m.Top;
    Right:=m.Right;
    Bottom:=m.Bottom;
  end;
end;

operator := (const m: MVRect) r: TRect;
begin
  with Result do begin
    Left:=m.Left;
    Top:=m.Top;
    Right:=m.Right;
    Bottom:=m.Bottom;
  end;
end;

end.

