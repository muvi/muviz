unit AuthNetType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClientNetType, PluginType, DllStr;

type
  TNetState                 = type Word;

  TNetEventData             = packed record
    Str   : DString;
    Obj   : IMInterface;
    Index : LongInt;
    Ptr   : Pointer;
  end;

  TNetEvent                 = procedure (const AData: TNetEventData) of object; stdcall;

  INetCallbacks             = interface
    ['{995CFDCC-1181-49C1-80B0-C032C0BC3108}']
    function AddCallback(const AName: ShortString): LongInt; stdcall;
    function CallbackID(const AName: ShortString): LongInt; stdcall;
    procedure Add(const ID: LongInt; const AProc: TNetEvent); stdcall;
    procedure Remove(const ID: LongInt; const AProc: TNetEvent); stdcall;
    procedure Call(const ID: LongInt; const AData: TNetEventData); stdcall;
  end;

  IAuthClient       = interface (INetClient)
    ['{92BDE64B-B8D6-4B02-9454-B2FAAB2F2A70}']
    function GetCallbacks: INetCallbacks; stdcall;
    function GetRequest: ShortString; stdcall;
    function GetState: TNetState; stdcall;

    procedure Auth(const AUsername,APW: ShortString); stdcall;
    procedure ReAuth; stdcall;
    procedure SIDRequest; stdcall;
    procedure SendRequest(const Request: ShortString; const Params: DString); stdcall;
    procedure FinishRequest(AState: TNetState); stdcall;

    property Callbacks: INetCallbacks read GetCallbacks;
    property Request: ShortString read GetRequest;
    property State: TNetState read GetState;
  end;

const
  ansIdle           = 0;
  ansNotReady       = 1000;
  ansOK             = 2000;
  ansReserved       = 3000;
  ansClientError    = 4000;
  ansServerError    = 5000;
  ansUserError      = 6000;

  ansWaiting        = 1001;
  ansCanceled       = 6001;
  ansNotAllowed     = 4500;

  cidConnected      = 0;
  cidDisconnected   = 1;
  cidRequestFinished= 2;

function NetEventData(AStr: DString = InvalidDString; AObject: IMInterface = nil; AIndex: LongInt = -1; APtr: Pointer = nil): TNetEventData;

implementation

function NetEventData(AStr: DString = InvalidDString; AObject: IMInterface = nil; AIndex: LongInt = -1; APtr: Pointer = nil): TNetEventData;
begin
  with Result do begin
    Str:=AStr;
    Obj:=AObject;
    Index:=AIndex;
    Ptr:=APtr;
  end;
end;

end.

