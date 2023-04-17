unit MInterfacedObject;

{$mode objfpc}{$H+}
{.$DEFINE IDEBUG}

interface

uses
  Classes, SysUtils, PluginType {$IFDEF IDEBUG},Dialogs{$ENDIF};

type
  TMInterfacedObject = class (TObject, IMInterface)
  {$IFDEF IDEBUG}
  private
    FRefCount: Cardinal;
  {$ENDIF}
  protected
    function QueryInterface(const iid: tguid; out obj) : LongInt; stdcall; virtual;
    function _AddRef: LongInt; stdcall; virtual;
    function _Release: LongInt; stdcall; virtual;
    function GetVersion: MVVersion; stdcall; virtual;
    function Future(const Version: MVVersion): IMInterface; stdcall; virtual;
  {$IFDEF IDEBUG}
  public
    constructor Create;
    destructor Destroy; override;
  {$ENDIF}
  end;

implementation

{TMInterfacedObject}

{$IFDEF IDEBUG}
constructor TMInterfacedObject.Create;
begin
  inherited Create;
  FRefCount:=0;
end;

destructor TMInterfacedObject.Destroy;
begin
  ShowMessage('Klasse: "'+ClassName+'"; Referenzen: '+IntToStr(FRefCount));
  inherited Destroy;
end;
{$ENDIF}

function TMInterfacedObject.GetVersion: MVVersion; stdcall;
begin
  Result:=ZeroVersion;
end;

function TMInterfacedObject.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=ZeroVersion
    then Result:=IMInterface(Self)
    else Result:=nil;
end;

function TMInterfacedObject.QueryInterface(const iid: tguid; out obj) : LongInt; stdcall;
begin
  Result:=0;
end;

function TMInterfacedObject._AddRef: LongInt; stdcall;
begin
  {$IFDEF IDEBUG} Inc(FRefCount); {$ENDIF}
  Result:=1;
end;

function TMInterfacedObject._Release: LongInt; stdcall;
begin
  {$IFDEF IDEBUG} Dec(FRefCount); {$ENDIF}
  Result:=1;
end;

end.

