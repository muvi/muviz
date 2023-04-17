unit TVSPClientIndexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPType;

type
  TTVSPClientIndexer = class;

  TTVSPIndexedObject = class
  strict private
    FLock   : TMultiReadExclusiveWriteSynchronizer;
    FIndex  : TTVSPIndex;
    FIndexer: TTVSPClientIndexer;
    function GetIndexed: Boolean;
  protected
    property Indexer: TTVSPClientIndexer read FIndexer;
    procedure RemoveIndex; virtual;
    procedure InitIndex(AIndexer: TTVSPClientIndexer; AIndex: TTVSPIndex); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Index: TTVSPIndex read FIndex;
    property Indexed: Boolean read GetIndexed;
  end;

  TTVSPClientIndexer = class
  strict private
    FIndex    : array of TTVSPIndexedObject;
    FIndexLock: TMultiReadExclusiveWriteSynchronizer;
    function GetItem(AIndex: TTVSPIndex): TTVSPIndexedObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddIndexed(AIndexed: TTVSPIndexedObject; AIndex: TTVSPIndex);
    procedure RemoveIndexed(AIndexed: TTVSPIndexedObject);
    property Items[AIndex: TTVSPIndex]: TTVSPIndexedObject read GetItem; default;
  end;

implementation

{%REGION TTVSPIndexedObject}

constructor TTVSPIndexedObject.Create;
begin
  inherited Create;
  FLock:=TMultiReadEXclusiveWriteSynchronizer.Create;
  FIndexer:=nil;
end;

destructor TTVSPIndexedObject.Destroy;
begin
  Assert(not Indexed);
  FLock.Destroy;
  inherited Destroy;
end;

procedure TTVSPIndexedObject.InitIndex(AIndexer: TTVSPClientIndexer; AIndex: TTVSPIndex);
begin
  FLock.Beginwrite;

  Assert(FIndexer = nil);
  FIndexer:=AIndexer;
  FIndex:=AIndex;

  FLock.Endwrite;
end;

procedure TTVSPIndexedObject.RemoveIndex;
begin
  FLock.Beginwrite;

  Assert(FIndexer <> nil);
  FIndexer:=nil;

  FLock.Endwrite;
end;

function TTVSPIndexedObject.GetIndexed: Boolean;
begin
  FLock.Beginread;
  Result:=FIndexer <> nil;
  FLock.Endread;
end;

{%ENDREGION}
{%REGION TTVSPClientIndexer}

constructor TTVSPClientIndexer.Create;
begin
  inherited Create;
  FIndexLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TTVSPClientIndexer.Destroy;
var
  I: Integer;
begin
  for I:=0 to Length(FIndex)-1
    do if FIndex[I] <> nil
      then FIndex[I].RemoveIndex;
  SetLength(FIndex, 0);
  FIndexLock.Destroy;
  inherited Destroy;
end;

procedure TTVSPClientIndexer.AddIndexed(AIndexed: TTVSPIndexedObject; AIndex: TTVSPIndex);
begin
  FIndexLock.Beginwrite;

  AIndexed.InitIndex(Self, AIndex);
  if Length(FIndex) <= AIndex
    then SetLength(FIndex, AIndex+1);
  FIndex[AIndex]:=AIndexed;

  FIndexLock.Endwrite;
end;

procedure TTVSPClientIndexer.RemoveIndexed(AIndexed: TTVSPIndexedObject);
begin
  FIndexLock.Beginwrite;

  Assert(AIndexed.Index < Length(FIndex));
  FIndex[AIndexed.Index]:=nil;
  AIndexed.RemoveIndex;

  FIndexLock.Endwrite;
end;

function TTVSPClientIndexer.GetItem(AIndex: TTVSPIndex): TTVSPIndexedObject;
begin
  FIndexLock.Beginread;

  if Length(FIndex) > AIndex
    then Result:=FIndex[AIndex]
    else Result:=nil;

  FIndexLock.Endread;
end;

{%ENDREGION}

end.

