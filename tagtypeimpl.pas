unit TagTypeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TagUnit, TagType, MStrings;

type
  TITags = class (TInterfacedObject, ITags)
  private
    FTags    : TTagArray;
    FTagGroup: TTagGroup;
    function Get(Index: LongInt): IString; cdecl;
    function GetCount: LongInt; cdecl;
  public
    constructor Create(ATags: TTagArray; ATagGroup: TTagGroup);
    destructor Destroy;
  end;

  TITaggedObjects = class (TInterfacedObject)
  private
    FObjects: TObjectArray;
  protected
    function GetObject(Index: LongInt): TObject; inline;
    function GetCount: LongInt; cdecl;
  public
    constructor Create(AObjects: TObjectArray);
    destructor Destroy;
  end;

implementation

{%REGION TITags}

constructor TITags.Create(ATags: TTagArray; ATagGroup: TTagGroup);
begin
  inherited Create;
  FTags:=ATags;
  FTagGroup:=ATagGroup;
end;

destructor TITags.Destroy;
begin
  SetLength(FTags, 0);
  inherited Destroy;
end;

function TITags.Get(Index: LongInt): IString; cdecl;
begin
  Result:=FTagGroup.Strings[FTags[Index]];
end;

function TITags.GetCount: LongInt; cdecl;
begin
  Result:=Length(FTags);
end;

{%ENDREGION}
{%REGION TITaggedObjects}

constructor TITaggedObjects.Create(AObjects: TObjectArray);
begin
  inherited Create;
  FObjects:=AObjects;
end;

destructor TITaggedObjects.Destroy;
begin
  SetLength(FObjects, 0);
  inherited Destroy;
end;

function TITaggedObjects.GetObject(Index: LongInt): TObject; inline;
begin
  Result:=FObjects[Index];
end;

function TITaggedObjects.GetCount: LongInt; cdecl;
begin
  Result:=Length(FObjects);
end;

{%ENDREGION}

end.

