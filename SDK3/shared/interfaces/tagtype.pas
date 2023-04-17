unit TagType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MStrings;

type
  ITags                     = interface
    function Get(Index: LongInt): IString; cdecl;
    function GetCount: LongInt; cdecl;
    property Count: LongInt read GetCount;
    property Items[Index: LongInt]: IString read Get; default;
  end;

implementation

end.

