unit Enumerators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDummyInterfacedObject = class (IUnknown)
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  IEnumeratorObject      = specialize IEnumerator<TObject>;
  IEnumerableObject      = specialize IEnumerable<TObject>;

  TEnumerator            = class (TDummyInterfacedObject, IEnumeratorObject, IEnumerableObject)
  strict private
    function GetEnumeratorAsInterface: IEnumeratorObject;
    function IEnumerableObject.GetEnumerator = GetEnumeratorAsInterface;
  strict protected
    function GetEnumerator: TEnumerator;
    function GetCurrent: TObject; virtual; abstract;
  public
    function MoveNext: Boolean; virtual; abstract;
    function Remove: TObject; virtual;
    procedure Delete; virtual;
    procedure Reset; virtual; abstract;
    property Current: TObject read GetCurrent;
  end;

implementation

{%REGION TDummyInterfacedObject}

function TDummyInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TDummyInterfacedObject._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=1;
end;

function TDummyInterfacedObject._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=1;
end;

{%ENDREGION}
{%REGION TEnumerator}

function TEnumerator.GetEnumerator: TEnumerator;
begin
  Result:=Self;
end;

function TEnumerator.GetEnumeratorAsInterface: IEnumeratorObject;
begin
  Result:=Self;
end;

function TEnumerator.Remove: TObject;
begin
  raise EInvalidOperation.Create('Remove operation is not supported by '+ClassName);
end;

procedure TEnumerator.Delete;
begin
  raise EInvalidOperation.Create('Delete operation is not supported by '+ClassName);
end;

{%ENDREGION}

end.

