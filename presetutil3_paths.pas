unit PresetUtil3_Paths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetUtil3_Path;

type
  TPaths          = class
  strict private
    FItems: array of IParamPath;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(APath: IParamPath);
    procedure Remove(APath: IParamPath);
    function Contains(APath: IParamPath): Boolean;
  end;

implementation

{%REGION TPaths}

constructor TPaths.Create;
begin
  inherited Create;
end;

destructor TPaths.Destroy;
var
  I: Integer;
begin
  for I:=0 to Length(FItems)-1
    do FItems[I]:=nil;
  SetLength(FItems, 0);
  inherited Destroy;
end;

procedure TPaths.Add(APath: IParamPath);
var
  L: Integer;
begin
  Assert(APath<>nil);
  L:=Length(FItems);
  SetLength(FItems, L+1);
  FItems[L]:=APath;
end;

procedure TPaths.Remove(APath: IParamPath);
var
  I, L         : Integer;
  ATemp, ATemp2: IParamPath;
begin
  Assert(APath<>nil);
  ATemp:=nil;
  L:=Length(FItems);
  for I:=L-1 downto 0 do begin
    ATemp2:=FItems[I];
    FItems[I]:=ATemp;
    ATemp:=ATemp2;
    if ATemp.Equals(APath) then begin
      SetLength(FItems, L-1);
      exit;
    end;
  end;
  Assert(false);
end;

function TPaths.Contains(APath: IParamPath): Boolean;
var
  I: Integer;
begin
  Assert(APath<>nil);
  for I:=0 to Length(FItems)-1 do begin
    Result:=FItems[I].Equals(APath);
    if Result then exit;
  end;
  //necessary if the paths are empty
  Result:=false;
end;

{%ENDREGION}

end.

