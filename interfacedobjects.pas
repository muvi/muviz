unit InterfacedObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNoRefInterfacedObject       = class (IInterface)
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  //WARNING: has BUGS with a probability of 100% (tested)!!!
  {TTriggerableInterfacedObject = class (TInterfacedObject, IInterface)
  private
    FDeleted: Boolean;
  protected
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; reintroduce;
    function IsDeleted: Boolean; cdecl;
  public
    constructor Create;
    destructor Destroy; override;
    //indicates, that the object is no longer needed. It will be destroyed,
    //if there are no more interface references
    procedure MayDestroy; cdecl;
    property Deleted: Boolean read FDeleted;
  end;}

implementation

{%REGION TNoRefInterfacedObject}

function TNoRefInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
   if GetInterface(iid,obj)
     then Result:=S_OK
     else Result:=longint(E_NOINTERFACE);
end;

function TNoRefInterfacedObject._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=0;
end;

function TNoRefInterfacedObject._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=0;
end;

{%ENDREGION}
{%REGION TTriggerableInterfacedObject}

{constructor TTriggerableInterfacedObject.Create;
begin
  inherited Create;
  FDeleted:=false;
end;

destructor TTriggerableInterfacedObject.Destroy;
begin
  //for debugging purpose
  inherited Destroy;
end;

function TTriggerableInterfacedObject._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  //Taken from objpas.inc, line 995
  //modified to destroy only if deleted
  _Release:=interlockeddecrement(frefcount);
  if (_Release=0) and FDeleted then
    Self.Destroy;
end;

procedure TTriggerableInterfacedObject.MayDestroy; cdecl;
begin
  //is it necessary to lock this?
  FDeleted:=true;
  if FRefCount<1
    then Destroy;
end;

function TTriggerableInterfacedObject.IsDeleted: Boolean; cdecl;
begin
  Result:=FDeleted;
end;}

{%ENDREGION}

end.

