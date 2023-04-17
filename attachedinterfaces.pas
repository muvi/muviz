unit AttachedInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TInterfaces = class
  private
    FInterfaces: array of IInterface;
    FLock      : IReadWriteSync;
    function GetItem(AID: TGUID): IInterface;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AInterface: IInterface); inline;
    function Query(constref iid : TGuid; out obj): LongInt;
    property Items[AID: TGUID]: IInterface read GetItem; default;
  end;

implementation

{%REGION TInterfaces}

constructor TInterfaces.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TInterfaces.Destroy;
var
  I: Integer;
begin
  for I:=0 to Length(FInterfaces)-1
    do FInterfaces[I]:=nil;
  SetLength(FInterfaces, 0);
  FLock:=nil;
  inherited Destroy;
end;

procedure TInterfaces.Add(AInterface: IInterface); inline;
var
  L: Integer;
begin
  Assert(AInterface<>nil);

  FLock.BeginWrite;

  L:=Length(FInterfaces);
  SetLength(FInterfaces, L+1);
  FInterfaces[L]:=AInterface;

  FLock.EndWrite;
end;

function TInterfaces.Query(constref iid : TGuid; out obj): LongInt;
var
  I: Integer;
begin
  FLock.BeginRead;

  for I:=0 to Length(FInterfaces)-1 do begin
    if Supports(FInterfaces[I], iid, obj) then begin
      Result:=S_OK;
      FLock.EndRead;
      exit;
    end;
  end;
  Result:=LongInt(E_NOINTERFACE);

  FLock.EndRead;
end;

function TInterfaces.GetItem(AID: TGUID): IInterface;
var
  I: Integer;
begin
  FLock.BeginRead;

  for I:=0 to Length(FInterfaces)-1 do begin
    Result:=FInterfaces[I];
    if Supports(Result, AID) then begin
      FLock.EndRead;
      exit;
    end;
  end;
  Result:=nil;

  FLock.EndRead;
end;

{%ENDREGION}

end.

