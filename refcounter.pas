unit RefCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  RefCount         = interface (IInterface)

  end;

  TRefCounter      = class (RefCount)
  strict private
    FRefCount: Integer;
    FOnZero  : TNotifyEvent;
    function GetCounter: RefCount;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
  public
    constructor Create;
    property Count: Integer read FRefCount;
    property Counter: RefCount read GetCounter;
    property OnZero: TNotifyEvent read FOnZero write FOnZero;
  end;

implementation

{%REGION TRefCounter}

constructor TRefCounter.Create;
begin
  inherited Create;
  FRefCount:=0;
end;

function TRefCounter.GetCounter: RefCount;
begin
  Result:=RefCount(Self);
end;

function TRefCounter.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=S_OK;
end;

function TRefCounter._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=interlockedincrement(FRefCount);
end;

function TRefCounter._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=interlockeddecrement(FRefCount);
  if (Result=0) and Assigned(FOnZero)
    then FOnZero(Self);
end;

{%ENDREGION}

end.

