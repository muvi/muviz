unit TVSPSources;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, GuidOp;

type
  TTVSPSrcID         = packed record
    ID        : TGUID;
    Size      : LongWord;
    //offset of the part you want to have from the source.
    //Set PartOffset = 0 and PartSize = Size if you need the whole source.
    //PartOffset: LongWord;
    //PartSize  : LongWord;
  end;

  ITVSPSource   = interface
    function GetID: TTVSPSrcID; cdecl;
    function GetSize: LongWord; cdecl;
    procedure &Set(AStart, ACount: LongWord; const Src); cdecl;
    procedure Get(AStart, ACount: LongWord; out Dest); cdecl;
    //function Cut(AStart, ACount: LongWord): ITVSPSource; cdecl;
    procedure _ValAddRef; cdecl;
    procedure _ValRelease; cdecl;
    property ID: TTVSPSrcID read GetID;
    property Size: LongWord read GetSize;
  end;

  ITVSPSources  = interface
    function GetSource(AID: TTVSPSrcID): ITVSPSource; cdecl;
    function GetSourceOrNil(AID: TTVSPSrcID): ITVSPSource; cdecl; deprecated;
    property Sources[AID: TTVSPSrcID]: ITVSPSource read GetSource; default;
    property SourcesOrNil[AID: TTVSPSrcID]: ITVSPSource read GetSourceOrNil; deprecated;
  end;

const
  ZEROSOURCEID: TTVSPSrcID = (ID: '{00000000-0000-0000-0000-000000000000}'; Size: 0{; PartOffset: 0; PartSize: 0});

operator = (m1, m2: TTVSPSrcID): Boolean;
//function TVSPSourceID(ASize: LongWord; AID: TGUID; APartOffset, APartSize: LongWord): TTVSPSrcID;
function TVSPSourceID(ASize: LongWord; AID: TGUID): TTVSPSrcID;
function TVSPSourceID(ASize: LongWord): TTVSPSrcID;

implementation

operator = (m1, m2: TTVSPSrcID): Boolean;
begin
  Result:=(m1.Size = m2.Size) and (m1.ID = m2.ID);
end;

{
function TVSPSourceID(ASize: LongWord; AID: TGUID; APartOffset, APartSize: LongWord): TTVSPSrcID;
begin
  Result.Size:=ASize;
  Result.ID:=AID;
  Result.PartOffset:=APartOffset;
  Result.PartSize:=APartSize;
end;
}

function TVSPSourceID(ASize: LongWord; AID: TGUID): TTVSPSrcID;
begin
  Result.Size:=ASize;
  Result.ID:=AID;
  //Result.PartOffset:=0;
  //Result.PartSize:=ASize;
end;

function TVSPSourceID(ASize: LongWord): TTVSPSrcID;
begin
  Result.Size:=ASize;
  CreateGUID(Result.ID);
  //Result.PartOffset:=0;
  //Result.PartSize:=ASize;
end;

end.

