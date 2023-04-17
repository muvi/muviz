unit ProgressHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TProgressHandler = class
  strict private
    FMax: Int64;
  public
    property Max: Int64 read FMax write FMax;
    procedure SetProgress(AProgress: Int64; AProgressString: string); virtual; abstract;
    constructor Create;
  end;

implementation

{%REGION TProgressHandler}

constructor TProgressHandler.Create;
begin
  inherited Create;
  FMax:=65533;
end;

{%ENDREGION}

end.

