unit PresetUtil3_Path;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, RefCounter, MStrings, StdParamTypes;

type
  IParamPath      = interface
    function GetObj: TObject;
    function GetItem(AIndex: Integer): TPParamID;
    function GetCount: Integer;
    function Equals(APath: IParamPath): Boolean;
    function ToString: string;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TPParamID read GetItem; default;
  end;

  ISidedParamPath = interface (IParamPath)
    function GetIsLeft: Boolean;
    property IsLeft: Boolean read GetIsLeft;
  end;

function NewParamPath(AItems: array of TPParamID): IParamPath;
function NewParamPath(AIsLeft: Boolean; AItems: array of TPParamID): ISidedParamPath;
function NewParamPath(AIsLeft: Boolean; APath: IParamPath): ISidedParamPath;
function OpenPath: IParamPath; inline;
function OpenSidedPath: ISidedParamPath; inline;
function EqualCount(APath1, APath2: IParamPath): Integer;

operator + (m1, m2: IParamPath): IParamPath;
operator + (m1: TPParamID; m2: IParamPath): IParamPath;
operator + (m1: IParamPath; m2: TPParamID): IParamPath;
operator - (m1, m2: IParamPath): IParamPath;
operator - (m1: IParamPath; m2: Cardinal): IParamPath;
operator / (m1, m2: IParamPath): IParamPath;

implementation

{%REGION TParamPath}

type
  TParamPath      = class (TInterfacedObject, IParamPath)
  strict private
    function GetItem(AIndex: Integer): TPParamID;
    function GetCount: Integer;
    function GetObj: TObject;
  private
    FItems: array of TPParamID;
  public
    constructor Create(AItems: array of TPParamID);
    constructor CreateAdd(AItems1, AItems2: array of TPParamID);
    constructor CreateRemoveFront(AItems: array of TPParamID; ARemoveCount: Cardinal);
    constructor CreateRemoveBack(AItems: array of TPParamID; ARemoveCount: Cardinal);
    constructor CreateEqualPart(APath1, APath2: TParamPath);
    destructor Destroy; override;
    function Equals(APath: IParamPath): Boolean;
    function Equals(Obj: TObject): Boolean; override;
    function ToString: string; override;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TPParamID read GetItem; default;
  end;

constructor TParamPath.Create(AItems: array of TPParamID);
var
  I, L: Integer;
begin
  inherited Create;
  L:=Length(AItems);
  SetLength(FItems, L);
  for I:=0 to L-1
    do FItems[I]:=AItems[I];
end;

constructor TParamPath.CreateAdd(AItems1, AItems2: array of TPParamID);
var
  I, L1, L2: Integer;
begin
  inherited Create;
  L1:=Length(AItems1);
  L2:=Length(AItems2);
  SetLength(FItems, L1+L2);
  for I:=0 to L1-1
    do FItems[I]:=AItems1[I];
  for I:=0 to L2-1
    do FItems[I+L1]:=AItems2[I];
end;

constructor TParamPath.CreateRemoveFront(AItems: array of TPParamID; ARemoveCount: Cardinal);
var
  I, L: Integer;
begin
  inherited Create;
  L:=Length(AItems);
  SetLength(FItems, L-ARemoveCount);
  for I:=ARemoveCount to L-1
    do FItems[I-ARemoveCount]:=AItems[I];
end;

constructor TParamPath.CreateRemoveBack(AItems: array of TPParamID; ARemoveCount: Cardinal);
var
  I, L: Integer;
begin
  inherited Create;
  L:=Length(AItems)-ARemoveCount;
  SetLength(FItems, L);
  for I:=0 to L-1
    do FItems[I]:=AItems[I];
end;

constructor TParamPath.CreateEqualPart(APath1, APath2: TParamPath);
var
  I, AEqualCount: Integer;
begin
  inherited Create;
  AEqualCount:=EqualCount(APath1, APath2);
  SetLength(FItems, AEqualCount);
  for I:=0 to AEqualCount-1
    do FItems[I]:=APath1[I];
end;

destructor TParamPath.Destroy;
begin
  SetLength(FItems, 0);
  inherited Destroy;
end;

function TParamPath.Equals(APath: IParamPath): Boolean;
begin
  Result:=Equals(APath.GetObj);
end;

function TParamPath.Equals(Obj: TObject): Boolean;
var
  I     : Integer;
  AOther: TParamPath;
begin
  Result:=Obj.InheritsFrom(TParamPath)
    and (Length(FItems) = Length(TParamPath(Obj).FItems));
  if Result then begin
    AOther:=TParamPath(Obj);
    for I:=0 to Length(FItems)-1 do begin
      if AOther.FItems[I]<>FItems[I] then begin
        Result:=false;
        exit;
      end;
    end;
  end;
end;

function TParamPath.ToString: string;
var
  I: Integer;
begin
  if Length(FItems) = 0 then begin
    Result:='';
    exit;
  end;
  Result:=FItems[0].Name;
  for I:=1 to Length(FItems)-1
    do Result += SUBPRESETDIVIDER + FItems[I].Name;
end;

function TParamPath.GetItem(AIndex: Integer): TPParamID;
begin
  Result:=FItems[AIndex];
end;

function TParamPath.GetCount: Integer;
begin
  Result:=Length(FItems);
end;

function TParamPath.GetObj: TObject;
begin
  Result:=Self;
end;

{%ENDREGION}
{%REGION TSidedParamPath}

type
  TSidedParamPath = class (TParamPath, ISidedParamPath)
  strict private
    FIsLeft        : Boolean;
    function GetIsLeft: Boolean;
  public
    constructor Create(AItems: array of TPParamID; AIsLeft: Boolean);
    constructor Create(APath: IParamPath; AIsLeft: Boolean);
    function Equals(Obj: TObject): Boolean; override;
    property IsLeft: Boolean read FIsLeft;
  end;

constructor TSidedParamPath.Create(AItems: array of TPParamID; AIsLeft: Boolean);
begin
  inherited Create(AItems);
  FIsLeft:=AIsLeft;
end;

constructor TSidedParamPath.Create(APath: IParamPath; AIsLeft: Boolean);
begin
  inherited Create(TParamPath(APath.GetObj).FItems);
  FIsLeft:=AIsLeft;
end;

function TSidedParamPath.Equals(Obj: TObject): Boolean;
begin
  Result:=inherited Equals(Obj);
  if Result and Obj.InheritsFrom(TSidedParamPath)
    then Result:=FIsLeft = TSidedParamPath(Obj).FIsLeft;
end;

function TSidedParamPath.GetIsLeft: Boolean;
begin
  Result:=FIsLeft;
end;

{%ENDREGION}
{%REGION Misc}

function NewParamPath(AItems: array of TPParamID): IParamPath;
begin
  Result:=TParamPath.Create(AItems);
end;

function NewParamPath(AIsLeft: Boolean; AItems: array of TPParamID): ISidedParamPath;
begin
  Result:=TSidedParamPath.Create(AItems, AIsLeft);
end;

function NewParamPath(AIsLeft: Boolean; APath: IParamPath): ISidedParamPath;
begin
  Result:=TSidedParamPath.Create(APath, AIsLeft);
end;

var
  LOpenPath: IParamPath;

function OpenPath: IParamPath; inline;
begin
  //do not do this in initialization because the MStrings are not intialized.
  if LOpenPath = nil
    then LOpenPath:=NewParamPath(ParamID('', vCall));
  Result:=LOpenPath
end;

function OpenSidedPath: ISidedParamPath; inline;
begin
  Result:=NewParamPath(true, OpenPath);
end;

function EqualCount(APath1, APath2: IParamPath): Integer;
var
  L: Integer;
begin
  L:=APath1.Count;
  if APath2.Count < L
    then L:=APath2.Count;
  Result:=0;
  while (Result<L) and (APath1[Result] = APath2[Result])
    do Inc(Result);
end;

{%ENDREGION}
{%REGION operators}

operator + (m1, m2: IParamPath): IParamPath;
begin
  Result:=TParamPath.CreateAdd(TParamPath(m1.GetObj).FItems, TParamPath(m2.GetObj).FItems);
end;

operator + (m1: TPParamID; m2: IParamPath): IParamPath;
begin
  Result:=TParamPath.CreateAdd(m1, TParamPath(m2.GetObj).FItems);
end;

operator + (m1: IParamPath; m2: TPParamID): IParamPath;
begin
  Result:=TParamPath.CreateAdd(TParamPath(m1.GetObj).FItems, m2);
end;

operator - (m1, m2: IParamPath): IParamPath;
begin
  Result:=TParamPath.CreateRemoveFront(TParamPath(m1.GetObj).FItems, TParamPath(m2.GetObj).Count);
end;

operator - (m1: IParamPath; m2: Cardinal): IParamPath;
begin
  Assert(m1 <> nil);
  Result:=TParamPath.CreateRemoveBack(TParamPath(m1.GetObj).FItems, m2);
end;

operator / (m1, m2: IParamPath): IParamPath;
begin
  Result:=TParamPath.CreateEqualPart(TParamPath(m1.GetObj), TParamPath(m2.GetObj));
end;

{%ENDREGION}

finalization
  LOpenPath:=nil;
end.

