unit CubicalSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCSDimension = Byte;

  TCSLowLevel  = record
    Count  : TCSDimension;
    Content: bitpacked array [TCSDimension] of Boolean;
  end;

  TCSHighLevel = record
    Count  : TCSDimension;
    Content: packed array [TCSDimension] of Pointer;
  end;

  TCubicalSet = class
  strict private
    FContent: TCSHighLevel;
    function GetContains(AObject: TObject): Boolean;
    procedure SetContains(AObject: TObject; AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Contains[AObject: TObject]: Boolean read GetContains write SetContains;
  end;

implementation

{%REGION TCubicalSet}

constructor TCubicalSet.Create;
begin
  inherited Create;
  FillChar(FContent, SizeOf(FContent), 0);
end;

destructor TCubicalSet.Destroy;
var
  I, I1, I2, I3, I4 {$IFDEF CPU64}, I5, I6, I7, I8 {$ENDIF}: Integer;
begin
  for I:=Low(TCSDimension) to High(TCSDimension) do begin

  end;
  inherited Destroy;
end;

function TCubicalSet.GetContains(AObject: TObject): Boolean;
procedure SetContains(AObject: TObject; AValue: Boolean);

{%ENDREGION}

end.

