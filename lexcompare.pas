unit LexCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, LinkedList, Enumerators;

type
  TTokenIdentifier = (tiEmpty, tiInteger, tiString);

  TToken           = class
  strict private
    FID: TTokenIdentifier;
  protected
    //is only called, if the tokens have an identical token id
    function ValueSmaller(AThan: TToken): Boolean; virtual; abstract;
    function ValueEquals(AOther: TToken): Boolean; virtual; abstract;
  public
    constructor Create(AID: TTokenIdentifier);
    function Smaller(AThan: TToken): Boolean;
    function Equals(AOther: TObject): Boolean; override;
    property ID: TTokenIdentifier read FID;
  end;

  TLexedString     = class (TLinkedList)
  public
    destructor Destroy; override;
    //Compare(X) < 0 => this < X
    function Compare(ATo: TLexedString): SmallInt;
  end;

  TStringToken     = class (TToken)
  strict private
    FValue: string;
  protected
    function ValueSmaller(AThan: TToken): Boolean; override;
    function ValueEquals(AOther: TToken): Boolean; override;
  public
    constructor Create(AValue: string);
    property Value: string read FValue;
  end;

  TIntegerToken    = class (TToken)
  strict private
    FValue: Integer;
  protected
    function ValueSmaller(AThan: TToken): Boolean; override;
    function ValueEquals(AOther: TToken): Boolean; override;
  public
    constructor Create(AValue: Integer);
    property Value: Integer read FValue;
  end;

const
  NUMBERS      = ['0'..'9'];
  TOKENIDORDER: array [TTokenIdentifier] of Integer = (0, 1, 2);

operator < (m1, m2: TToken): Boolean;
operator < (m1, m2: TLexedString): Boolean;
function Lex(AStr: string): TLexedString;

implementation

{%REGION Misc}

operator < (m1, m2: TToken): Boolean;
begin
  Assert(m1 <> nil);
  Assert(m2 <> nil);
  Result:=m1.Smaller(m2);
end;

operator < (m1, m2: TLexedString): Boolean;
begin
  Assert(m1 <> nil);
  Assert(m2 <> nil);
  Result:=m1.Compare(m2) < 0;
end;

//if the integer is too long, one should better create a string token
function TryCreateIntegerToken(AStr: string): TToken;
var
  AInt: Integer;
begin
  if TryStrToInt(AStr, AInt)
    then Result:=TIntegerToken.Create(AInt)
    else Result:=TStringToken.Create(AStr);
end;

function Lex(AStr: string): TLexedString;
var
  I              : Integer;
  C              : Char;
  ACurrentTokenID: TTokenIdentifier;
  ACurrentToken  : string;

  procedure FinishToken;
  var
    AToken: TToken;
  begin
    case ACurrentTokenID of
      tiInteger: AToken:=TryCreateIntegerToken(ACurrentToken);
      tiString : AToken:=TStringToken.Create(ACurrentToken);
      else exit;
    end;
    Result.Add(AToken);
  end;

  procedure AddToToken(ATokenID: TTokenIdentifier);
  begin
    if ACurrentTokenID = ATokenID then ACurrentToken += C else begin
      FinishToken;
      ACurrentTokenID:=ATokenID;
      ACurrentToken:=C;
    end;
  end;

begin
  Result:=TLexedString.Create;
  ACurrentToken:='';
  ACurrentTokenID:=tiEmpty;

  for I:=1 to Length(AStr) do begin
    C:=AStr[I];
    case C of
      '0'..'9': AddToToken(tiInteger);
      '-'     : if (I < Length(AStr)) and (AStr[I+1] in NUMBERS)
        then AddToToken(tiInteger)
        else AddToToken(tiString);
      else AddToToken(tiString);
    end;
  end;
  FinishToken;
end;

{%ENDREGION}
{%REGION TToken}

constructor TToken.Create(AID: TTokenIdentifier);
begin
  inherited Create;
  FID:=AID;
end;

function TToken.Smaller(AThan: TToken): Boolean;
var
  AOrder, AOtherOrder: Integer;
begin
  Assert(AThan <> nil);
  Assert(AThan.InheritsFrom(TToken));
  AOrder:=TOKENIDORDER[FID];
  AOtherOrder:=TOKENIDORDER[TToken(AThan).ID];
  if AOrder = AOtherOrder
    then Result:=ValueSmaller(AThan)
    else Result:=AOrder < AOtherOrder;
end;

function TToken.Equals(AOther: TObject): Boolean;
begin
  Assert(AOther <> nil);
  Assert(AOther.InheritsFrom(TToken));
  if FID = TToken(AOther).ID
    then Result:=ValueEquals(TToken(AOther))
    else Result:=false;
end;

{%ENDREGION}
{%REGION TLexedString}

destructor TLexedString.Destroy;
begin
  Clean;
  inherited Destroy;
end;

function TLexedString.Compare(ATo: TLexedString): Smallint;
var
  AEnumerator                    : TEnumerator;
  ATokenObject, AOtherTokenObject: TObject;
  AToken, AOtherToken            : TToken;
begin
  Assert(ATo <> nil);

  AEnumerator:=ATo.Enumerator;
  for ATokenObject in Self do begin
    //if the other string is shorter than this
    if not AEnumerator.MoveNext then begin
      AEnumerator.Destroy;
      Result:=1;
      exit;
    end;

    AOtherTokenObject:=AEnumerator.Current;
    AToken:=TToken(ATokenObject);
    AOtherToken:=TToken(AOtherTokenObject);
    Assert(AToken <> nil);
    Assert(AOtherToken <> nil);
    Assert(AToken.InheritsFrom(TToken));
    Assert(AOtherToken.InheritsFrom(TToken));

    if AToken < AOtherToken then begin
      Result:=-1;
      AEnumerator.Destroy;
      exit;
    end else if not AToken.Equals(AOtherToken) then begin
      Result:=1;
      AEnumerator.Destroy;
      exit;
    end;
  end;
  //if this string is shorter or equal to the other
  if AEnumerator.MoveNext
    then Result:=-1
    else Result:=0;
  AEnumerator.Destroy;
end;

{%ENDREGION}
{%REGION TStringToken}

constructor TStringToken.Create(AValue: string);
begin
  inherited Create(tiString);
  FValue:=AValue;
end;

function TStringToken.ValueSmaller(AThan: TToken): Boolean;
begin
  Assert(AThan <> nil);
  Assert(AThan.InheritsFrom(TStringToken));
  Assert(AThan.ID = tiString);
  Result:=FValue < TStringToken(AThan).Value;
end;

function TStringToken.ValueEquals(AOther: TToken): Boolean;
begin
  Assert(AOther <> nil);
  Assert(AOther.InheritsFrom(TStringToken));
  Assert(AOther.ID = tiString);
  Result:=FValue = TStringToken(AOther).Value;
end;

{%ENDREGION}
{%REGION TIntegerToken}

constructor TIntegerToken.Create(AValue: Integer);
begin
  inherited Create(tiInteger);
  FValue:=AValue;
end;

function TIntegerToken.ValueSmaller(AThan: TToken): Boolean;
begin
  Assert(AThan <> nil);
  Assert(AThan.InheritsFrom(TIntegerToken));
  Assert(AThan.ID = tiInteger);
  Result:=FValue < TIntegerToken(AThan).Value;
end;

function TIntegerToken.ValueEquals(AOther: TToken): Boolean;
begin
  Assert(AOther <> nil);
  Assert(AOther.InheritsFrom(TIntegerToken));
  Assert(AOther.ID = tiInteger);
  Result:=FValue = TIntegerToken(AOther).Value;
end;

{%ENDREGION}

end.

