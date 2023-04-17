unit DllStr; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  SInt           = LongInt;
  DString        = LongInt;
  DLongString    = array [0..1048575] of Char;
  IStringManager = interface
    ['{CB4C902A-9389-4226-BC57-2B1991899706}']
    function NewString(const S: ShortString): DString; stdcall; overload;
    function NewString(const S: DString): DString; stdcall; overload;
    procedure DisposeStr(const S: DString); stdcall;
    function SCopy(const S: DString; const Index,Count: SInt): DString; stdcall;
    procedure SCopyTo(const S,Dest: DString; const Index,Count: SInt); stdcall;
    procedure SCopyAndAdd(const S,Dest: DString; const Index,Count: SInt); stdcall;
    procedure SDelete(const S: DString; const Index,Count: SInt); stdcall;
    function SPos(const SubStr: ShortString; const S: DString): SInt; stdcall; overload;
    function SPos(const SubStr: DString; const S: DString): SInt; stdcall; overload;
    function SGet(const S: DString; const Index,Count: SInt): ShortString; stdcall;
    procedure SSet(const S: DString; const X: ShortString); stdcall; overload;
    procedure SSet(const S: DString; const X: DString); stdcall; overload;
    procedure SAdd(const S: DString; const X: ShortString); stdcall; overload;
    procedure SAdd(const S: DString; const X: DString); stdcall; overload;
    procedure SAdd(const S: DString; const X: DLongString); stdcall; overload;
    procedure SAdd(const S: DString; const X: DLongString; const Size: SInt); stdcall; overload;
    procedure SAddBefore(const S: DString; const X: ShortString); stdcall; overload;
    procedure SAddBefore(const S: DString; const X: DString); stdcall; overload;
    function SLength(const S: DString): SInt; stdcall;
    function SGetChar(const S: DString; const Index: SInt): Char; stdcall;
    function SSameAs(const S1: DString; const S2: ShortString): Boolean; stdcall; overload;
    function SSameAs(const S1,S2: DString): Boolean; stdcall; overload;
    procedure SLoadFromFile(var S: DString; const FileName: ShortString); stdcall;
    procedure SSaveToFile(var S: DString; const FileName: ShortString); stdcall;
  end;

const
  InvalidDString = -1;

implementation

end.

