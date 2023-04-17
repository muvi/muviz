unit GlobalTVSP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPClient, TVSPSourceUtil;

function TVSP: TTVSPClient; inline;
procedure Init(ATVSP: TTVSPClient); inline;
procedure Done; inline;

implementation

var
  LocalTVSP: TTVSPClient = nil;

function TVSP: TTVSPClient; inline;
begin
  Result:=LocalTVSP;
  Assert(Result <> nil);
end;

procedure Init(ATVSP: TTVSPClient); inline;
begin
  Assert(LocalTVSP = nil);
  LocalTVSP:=ATVSP;
  SourceUtil:=ATVSP.Sources;
end;

procedure Done; inline;
begin
  SourceUtil:=nil;
  LocalTVSP.Destroy;
end;

end.

