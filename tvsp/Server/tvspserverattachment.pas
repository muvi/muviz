unit TVSPServerAttachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractNet, ObjectClassBasic, TVSPType;

type
  TKnownIndexBlock      = LongWord;

  TTVSPServerAttachment = class (TObjectItem)
  strict private
    FNet             : TANet;
    FSocket          : TASocket;
    FKnownIndicesLock: TMultiReadExclusiveWriteSynchronizer;
    FKnownIndices    : array of TKnownIndexBlock;
  public
    constructor Create(ANet: TANet; ASocket: TASocket);
    destructor Destroy; override;
    procedure Send(AMsg: Pointer; ASize: Cardinal);
    function KnowsIndex(AIndex: TTVSPIndex): Boolean;
    procedure LearnIndex(AIndex: TTVSPIndex);
  end;

const
  KNOWNINDEXBLOCK_BITSIZE = SizeOf(TKnownIndexBlock) * 8;

implementation

{%REGION TTVSPServerAttachment}

constructor TTVSPServerAttachment.Create(ANet: TANet; ASocket: TASocket);
begin
  Assert(ASocket <> nil);
  inherited Create;
  FNet:=ANet;
  FSocket:=ASocket;
  FKnownIndicesLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TTVSPServerAttachment.Destroy;
begin
  FKnownIndicesLock.Destroy;
  SetLength(FKnownIndices, 0);
  inherited Destroy;
end;

procedure TTVSPServerAttachment.Send(AMsg: Pointer; ASize: Cardinal);
begin
  FNet.Send(FSocket, AMsg, ASize);
end;

function TTVSPServerAttachment.KnowsIndex(AIndex: TTVSPIndex): Boolean;
var
  ABlock: Integer;
begin
  FKnownIndicesLock.Beginread;

  ABlock:=AIndex div KNOWNINDEXBLOCK_BITSIZE;
  Result:=(Length(FKnownIndices) > ABlock)
      and Boolean((FKnownIndices[ABlock] shr (AIndex mod KNOWNINDEXBLOCK_BITSIZE)) and 1);

  FKnownIndicesLock.Endread;
end;

procedure TTVSPServerAttachment.LearnIndex(AIndex: TTVSPIndex);
var
  ABlock, L: Integer;
begin
  FKnownIndicesLock.Beginwrite;

  ABlock:=AIndex div KNOWNINDEXBLOCK_BITSIZE;
  L:=Length(FKnownIndices);
  //set size
  if L <= ABlock then begin
    SetLength(FKnownIndices, ABlock+1);
    while L<=ABlock do begin
      FKnownIndices[L]:=0;
      Inc(L);
    end;
  end;
  //learn index
  FKnownIndices[ABlock]:=FKnownIndices[ABlock] or (LongWord(1) shl (AIndex mod KNOWNINDEXBLOCK_BITSIZE));

  FKnownIndicesLock.Endwrite;
end;

{%ENDREGION}

end.

