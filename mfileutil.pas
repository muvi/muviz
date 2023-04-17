unit MFileUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvType;

const
  MuviFileID            = 'Muvi ';
  MuviFileIDSize        = Length(MuviFileID);
  GenerelFileIDSize     = 10;
  GenerelMuviFileIDSize = MuviFileIDSize+GenerelFileIDSize;
  CommentChar           = ';';
  IniBrackets           = '[';


type
  TMuviFileID       = array [1..GenerelMuviFileIDSize] of Char;

function ReadFileID(Stream: TStream): TMuviFileID;

implementation

function ReadFileID(Stream: TStream): TMuviFileID;
begin
  if Stream.Size<GenerelMuviFileIDSize then begin
    Result:='';
    exit;
  end;
  Stream.Read(Result,GenerelMuviFileIDSize);
end;

end.

