unit ParamTypeUtilImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamType2, csl, HashMap, Enumerators, VisType2,
  ParamTypeKey;

type
  TPParamTypeEnumerator = class (TInterfacedObject, IPParamTypeIterator)
  private
    FEnumerator: TEnumerator;
    function GetCurrent: IPParamType; cdecl;
  public
    constructor Create(AEnumerator: TEnumerator);
    destructor Destroy; override;
    function MoveNext: Boolean; cdecl;
    property Current: IPParamType read GetCurrent;
  end;

  TPParamContainer      = class
  private
    FContent: IPParamType;
  public
    constructor Create(AContent: IPParamType);
    destructor Destroy; override;
    property Content: IPParamType read FContent;
  end;

  TPParamTypeUtil       = class (TInterfacedObject, IPParamTypeUtil)
  private
    FMap: TMap;
    function GetType(ATypeID: TPParamType): IPParamType; cdecl;
    function AddType(AUtil: IPParamType): Boolean; cdecl;
  protected
    function GetIterator: IPParamTypeIterator; cdecl;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: TPParamTypeEnumerator;
    //TODO: implement enumerators in hashmap!!!
    property Enumerator: TPParamTypeEnumerator read GetEnumerator;
    property Types[ATypeID: TPParamType]: IPParamType read GetType; default;
  end;

implementation

{%REGION TPParamContainer}

constructor TPParamContainer.Create(AContent: IPParamType);
begin
  inherited Create;
  FContent:=AContent;
end;

destructor TPParamContainer.Destroy;
begin
  FContent:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TPParamTypeEnumerator}

constructor TPParamTypeEnumerator.Create(AEnumerator: TEnumerator);
begin
  inherited Create;
  FEnumerator:=AEnumerator;
end;

destructor TPParamTypeEnumerator.Destroy;
begin
  FEnumerator.Destroy;
  inherited Destroy;
end;

function TPParamTypeEnumerator.GetCurrent: IPParamType; cdecl;
begin
  Result:=TPParamContainer(FEnumerator.Current).Content;
end;

function TPParamTypeEnumerator.MoveNext: Boolean; cdecl;
begin
  Result:=FEnumerator.MoveNext;
end;

{%ENDREGION}
{%REGION TPParamTypeUtil}

constructor TPParamTypeUtil.Create;
begin
  inherited Create;
  FMap:=THashMap.Create;
  ParamTypeUtil:=Self;
end;

destructor TPParamTypeUtil.Destroy;
begin
  FMap.Clean;
  FMap.Destroy;
  inherited Destroy;
end;

function TPParamTypeUtil.GetType(ATypeID: TPParamType): IPParamType; cdecl;
var
  AKey: TObject;
  AVal: TObject;
begin
  AKey:=TPParamTypeKey.Create(ATypeID);
  AVal:=FMap[AKey];
  AKey.Destroy;
  if AVal=nil then begin
    Result:=nil;
    Assert(false);
    exit;
  end;
  Result:=TPParamContainer(AVal).Content;
end;

function TPParamTypeUtil.AddType(AUtil: IPParamType): Boolean; cdecl;
var
  AKey: TPParamTypeKey;
begin
  if (AUtil=nil) then begin
    Result:=false;
    exit;
  end;
  AKey:=TPParamTypeKey.Create(AUtil.&Type);
  Result:=not FMap.Contains(AKey);
  if Result
    then FMap.Add(AKey, TPParamContainer.Create(AUtil))
    else AKey.Destroy;
end;

function TPParamTypeUtil.GetEnumerator: TPParamTypeEnumerator;
begin
  Result:=TPParamTypeEnumerator.Create(FMap.Enumerator);
end;

function TPParamTypeUtil.GetIterator: IPParamTypeIterator; cdecl;
begin
  Result:=TPParamTypeEnumerator.Create(FMap.Enumerator);
end;

{%ENDREGION}

end.

