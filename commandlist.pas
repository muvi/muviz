unit CommandList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ICommandList = interface
    ['{8720FC58-4AF9-4357-A4C8-F9E0F4A4DB21}']
    procedure AddCommand(const ACommand: ShortString; const AData); stdcall;
    procedure RemoveCommand(const ACommand: ShortString); stdcall;
    procedure OverrideCommand(const ACommand: ShortString; const AData; out OldData); stdcall;
    function GetCommand(const ACommand: ShortString): Pointer; overload; stdcall;
    procedure GetCommand(const ACommand: ShortString; out AData); overload; stdcall;
    function GetCount: Integer; stdcall;
    function GetItem(const Index: Integer): ShortString; stdcall;
    function CommandExists(const Command: ShortString): Boolean; stdcall;
    property Items[Index: Integer]: ShortString read GetItem;
    property Commands[ACommand: ShortString]: Pointer read GetCommand; default;
    property Count: Integer read GetCount;
  end;

implementation

end.

