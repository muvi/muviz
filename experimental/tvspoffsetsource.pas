unit TVSPOffsetSource;

{$mode objfpc}{$H+}

interface

uses
  TVSPSources;

type
  TTVSPOffsetSource = class (TInterfacedObject, ITVSPSource)
  strict private
    FOriginal: ITVSPSource;
    FOffset  : LongWord;
    FPartSize: LongWord;
    function GetID: TTVSPSrcID; cdecl;
  public
    constructor Create(AOriginal: ITVSPSource; AOffset, APartSize: LongWord);
    destructor Destroy; override;
    procedure &Set(AStart, ACount: LongWord; const Src); cdecl;
    procedure Get(AStart, ACount: LongWord; out Dest); cdecl;
    function Cut(AStart, ACount: LongWord): ITVSPSource; cdecl;
    procedure _ValAddRef; cdecl;
    procedure _ValRelease; cdecl;
    property ID: TTVSPSrcID read GetID;
  end;

implementation

{%REGION TTVSPOffsetSource}

constructor TTVSPOffsetSource.Create(AOriginal: ITVSPSource; AOffset, APartSize: LongWord);
begin
  Assert(AOriginal <> nil);
  Assert(AOffset <= AOriginal.ID.PartSize);
  Assert(APartSize <= AOriginal.ID.PartSize - AOffset);
  inherited Create;
  FOriginal:=AOriginal;
  FOffset:=AOffset;
  FPartSize:=APartSize;
end;

destructor TTVSPOffsetSource.Destroy;
begin
  FOriginal:=nil;
  inherited Destroy;
end;

function TTVSPOffsetSource.GetID: TTVSPSrcID; cdecl;
begin
  Result:=FOriginal.ID;
  with Result do begin
    PartOffset:=FOffset;
    PartSize:=FPartSize;
  end;
end;

procedure TTVSPOffsetSource.&Set(AStart, ACount: LongWord; const Src); cdecl;
begin
  Assert(AStart <= FPartSize);
  Assert(ACount <= FPartSize - AStart);
  FOriginal.&Set(AStart + FOffset, ACount, Src);
end;

procedure TTVSPOffsetSource.Get(AStart, ACount: LongWord; out Dest); cdecl;
begin
  Assert(AStart <= FPartSize);
  Assert(ACount <= FPartSize - AStart);
  FOriginal.Get(AStart + FOffset, ACount, Dest);
end;

function TTVSPOffsetSource.Cut(AStart, ACount: LongWord): ITVSPSource; cdecl;
begin
  Assert(AStart <= FPartSize);
  Assert(ACount <= FPartSize - AStart);
  Result:=TTVSPOffsetSource.Create(FOriginal, FOffset + AStart, ACount);
end;

procedure TTVSPOffsetSource._ValAddRef; cdecl;
begin
  FOriginal._ValAddRef;
end;

procedure TTVSPOffsetSource._ValRelease; cdecl;
begin
  FOriginal._ValRelease;
end;

{%ENDREGION}

end.

