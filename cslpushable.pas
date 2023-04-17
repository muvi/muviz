unit cslPushable;

{$mode objfpc}{$H+}

interface

uses
  Enumerators;

type
  TPushable = class
  strict private
    function GetEmpty: Boolean; inline;
  strict protected
    function GetCount: Integer; virtual; abstract;
  public
    function GetEnumerator: TEnumerator; virtual; abstract;
    procedure Push(AObject: TObject); virtual; abstract;
    function Pop: TObject; virtual; abstract;
    function Get: TObject; virtual; abstract;
    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property Enumerator: TEnumerator read GetEnumerator;
  end;

implementation

{%REGION TPushable}

function TPushable.GetEmpty: Boolean; inline;
begin
  Assert(Count >= 0);
  Result:=Count = 0;
end;

{%ENDREGION}

end.

