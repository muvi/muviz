unit ParamTypeKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, HashMap, VisType2;

type
  TPParamTypeKey        = class
  private
    FKey: TPParamType;
  public
    constructor Create(AKey: TPParamType);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property Key: TPParamType read FKey;
  end;

implementation

{%REGION TPParamTypeKey}

constructor TPParamTypeKey.Create(AKey: TPParamType);
begin
  inherited Create;
  FKey:=AKey;
end;

function TPParamTypeKey.Equals(AObject: TObject): Boolean;
begin
  Result:=AObject.InheritsFrom(TPParamTypeKey) and (TPParamTypeKey(AObject).Key=FKey);
end;

function TPParamTypeKey.GetHashCode: PtrInt;
begin
  //cut the higher digits....
  Result:=FKey;
end;

{%ENDREGION}

end.

