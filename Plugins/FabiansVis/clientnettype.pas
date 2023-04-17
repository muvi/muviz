unit ClientNetType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DllStr, PluginType;

const
  CommandDivider    = ' ';
  CommandIntegrator = '"';

type
  INetClient     = interface;

  TNetReaction   = procedure (ANet: INetClient; AParams: DString) of object; stdcall;
  NetInt         = LongInt;
  NetFloat       = Double;

  INetCommandList= interface
    ['{F680F0DF-588E-4423-92A1-D7021494623B}']
    procedure AddCommand(const ACommand: ShortString; const AData: TNetReaction); stdcall;
    procedure OverrideCommand(const ACommand: ShortString; const AData: TNetReaction; out OldData: TNetReaction); stdcall;
    procedure GetCommand(const ACommand: ShortString; out AData: TNetReaction); stdcall;
    function GetCount: Integer; stdcall;
    function GetItem(const Index: Integer): ShortString; stdcall;
    function CommandExists(const Command: ShortString): Boolean; stdcall;
    property Items[Index: Integer]: ShortString read GetItem;
    property Count: Integer read GetCount;
  end;

  INetClient     = interface (IMInterface)
    ['{379BB55A-CA54-400B-B889-69C0C4FD5BB2}']
    function GetActive: Boolean; stdcall;
    procedure SetActive(const Value: Boolean); stdcall;
    function GetChatOnly: Boolean; stdcall;
    procedure SetChatOnly(const Value: Boolean); stdcall;
    function GetPort: Word; stdcall;
    procedure SetPort(const Value: Word); stdcall;
    function GetIPAddress: ShortString; stdcall;
    procedure SetIPAddress(const AIPAddress: ShortString); stdcall;
    function GetCommands: INetCommandList; stdcall;

    procedure Connect; stdcall;
    procedure Disconnect; stdcall;

    procedure ProcessCommand(const ACommand: DString); stdcall;
    procedure ReturnCommand(const Command: DString); stdcall;

    function NextParam(var Params: DString): ShortString; stdcall;
    procedure Send(const Msg: DString); stdcall;

    property Active: Boolean read GetActive write SetActive;
    property Commands: INetCommandList read GetCommands;
    property Port: Word read GetPort write SetPort;
    property IPAddress: ShortString read GetIPAddress write SetIPAddress;
  end;

implementation

end.

