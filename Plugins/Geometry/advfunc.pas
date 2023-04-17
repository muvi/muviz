unit AdvFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TSearchFunc    = function (const Index: Integer; var FindData): Boolean;
  TSearch2Func   = function (const Index: Integer; var FindData): Boolean of object;
  TGetTextWidth  = function (const S: string): Integer of object;
  TRealDivMod    = record
    ADiv: Integer;
    AMod: Real;
  end;

const
  HexBasis = 16;
  BinBasis = 2;
  DecBasis = 10;
  OctBasis = 8;

  ThreadPrioritys: array [0..6] of TThreadPriority = (tpTimeCritical,tpHighest,tpHigher,tpNormal,tpLower,tpLowest,tpIdle);

procedure SaveText(const Text: string; S: TStream);
procedure LoadText(var Text: string; S: TStream);
function StringBigger(const S,S0: string): Boolean;
function StringSmaller(const S,S0: string): Boolean;
function BinarySearch(ABigger,ASmaller,AEqual: TSearchFunc; var FindData; const AEnd: Integer; const AStart: Integer = 0; const NoResult: Integer = -1): Integer; overload;
function BinarySearch(ABigger,ASmaller,AEqual: TSearch2Func; var FindData; const AEnd: Integer; const AStart: Integer = 0; const NoResult: Integer = -1): Integer; overload;
procedure Swap(var X1,X2; const Size: Cardinal);
function StrToIntB(const S: string; const Basis: Byte; ErrorResult: Int64 = -1): Int64;
function RealMod(const V1,V2: Real): Real; inline;
function RealDivMod(const V1,V2: Real): TRealDivMod; inline;
function StrShorten(const S: string; AGetTextWidth: TGetTextWidth; MaxSize: Integer): string;
function IntMax(V: array of Integer): Integer;
function Max(V: array of Real): Real;
function IntMin(V: array of Integer): Integer;
function Min(V: array of Real): Real;
function PhiCut(APhi: Real): Real;
function RoundUp(X: Real): Integer;
function StrToIntE(const S: string; ErrorResult: Integer = -1): Integer;
function StrToFloatE(const S: string; ErrorResult: Real): Real;

const
  KeyDescriptions: array [Char] of string=
    ('#0',
    'Linke Maustaste',
    'Rechte Maustaste',
    'Abbrechen',
    'Mittlere Maustaste',
    'X1 Maustaste',
    'X2 Maustaste',
    '#7',
    'Rück',
    'Tab',
    'Linefeed',
    '#11',
    'Löschen',
    'Eingabe',
    '#14',
    '#15',
    'Umschalt',
    'Strg',
    'Alt',
    'Pause',
    'Feststell',
    'IME Kana/Hangul',
    '#16',
    'IME Junja',
    'IME Final',
    'IME Hanja/Kanji',
    '#26',
    'Esc',
    'Konvertieren',
    'Nicht Konvertieren',
    'Akzeptieren',
    'IME Modus wechseln',
    'Leertaste',
    'Bild Auf',
    'Bild Ab',
    'Ende',
    'Pos1',
    'Links',
    'Nach Oben',
    'Rechts',
    'Nach Unten',
    'Auswählen',
    'Drucken',
    'Ausführen',
    'Druck',
    'Einfg',
    'Entf',
    'Hilfe',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '#58',
    '#59',
    '#60',
    '#61',
    '#62',
    '#63',
    '#64',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    'Windowstaste Links',
    'Windowstaste Rechts',
    'Popupmenütaste',
    '#94',
    'Standby',
    '0 (Zehnertastatur)',
    '1 (Zehnertastatur)',
    '2 (Zehnertastatur)',
    '3 (Zehnertastatur)',
    '4 (Zehnertastatur)',
    '5 (Zehnertastatur)',
    '6 (Zehnertastatur)',
    '7 (Zehnertastatur)',
    '8 (Zehnertastatur)',
    '9 (Zehnertastatur)',
    '* (Zehnertastatur)',
    '+ (Zehnertastatur)',
    'Trennen',
    '- (Zehnertastatur)',
    ', (Zehnertastatur)',
    '/ (Zehnertastatur)',
    'F1',
    'F2',
    'F3',
    'F4',
    'F5',
    'F6',
    'F7',
    'F8',
    'F9',
    'F10',
    'F11',
    'F12',
    'F13',
    'F14',
    'F15',
    'F16',
    'F17',
    'F18',
    'F19',
    'F20',
    'F21',
    'F22',
    'F23',
    'F24',
    '#136',
    '#137',
    '#138',
    '#139',
    '#140',
    '#141',
    '#142',
    '#143',
    'Num',
    'Rollen',
    '#146',
    '#147',
    '#148',
    '#149',
    '#150',
    '#151',
    '#152',
    '#153',
    '#154',
    '#155',
    '#156',
    '#157',
    '#158',
    '#159',
    'Umschalt Links',
    'Umschalt Rechts',
    'Strg Links',
    'Strg Rechts',
    'Alt Links',
    'Alt Rechts',
    'Zurück',
    'Vorwärts',
    'Aktualisieren',
    'Anhalten',
    'Suchen',
    'Favoriten',
    'Standardseite',
    'Ton Aus',
    'Leiser',
    'Lauter',
    'Vorspulen',
    'Zurückspulen',
    'Stop',
    'Wiedergabe',
    'E-Mail',
    'Stück Auswählen',
    'Anwendung 1',
    'Anwendung 2',
    '#184',
    '#185',
    'Ü',
    '+',
    ',',
    '-',
    '.',
    '#',
    'Ö',
    '#193',
    '#194',
    '#195',
    '#196',
    '#197',
    '#198',
    '#199',
    '#200',
    '#201',
    '#202',
    '#203',
    '#204',
    '#205',
    '#206',
    '#207',
    '#208',
    '#209',
    '#210',
    '#211',
    '#212',
    '#213',
    '#214',
    '#215',
    '#216',
    '#217',
    '#218',
    'ß',
    '^',
    '´',
    'Ä',
    '#223',
    '#224',
    '#225',
    '<',
    '#227',
    '#228',
    'Prozess',
    '#230',
    'Keine Tastatureingabe',
    '#232',
    '#233',
    '#234',
    '#235',
    '#236',
    '#237',
    '#238',
    '#239',
    '#240',
    '#241',
    '#242',
    '#243',
    '#244',
    '#245',
    'Attn',
    'CrSel',
    'ExSel',
    'Lösche EOF',
    'Abspielen',
    'Zoom',
    'Unbenannt',
    'PA1',
    'Löschen',
    '#255');

implementation                                 

procedure SaveText(const Text: string; S: TStream);
var
  I: Integer;
  B: Char;
begin
  for I:=1 to Length(Text) do S.Write(Text[I],1);
  B:=#0;
  S.Write(B,1);
end;

procedure LoadText(var Text: string; S: TStream);
var B: Char;
begin
  Text:='';
  S.Read(B,1);
  while B<>#0 do begin
    Text:=Text+B;
    S.Read(B,1);
  end;
end;

function StringBigger(const S,S0: string): Boolean;
var
  I,L: Integer;
begin
  L:=Length(S);
  I:=Length(S0);
  Result:=(I>L);
  if not Result then L:=I;
  if L=0 then exit;
  I:=1;
  while S[I]=S0[I] do begin
    if I=L then exit;
    Inc(I);
  end;
  Result:=Ord(S0[I])>Ord(S[I]);
end;

function StringSmaller(const S,S0: string): Boolean;
var
  I,L: Integer;
begin
  L:=Length(S);
  I:=Length(S0);
  Result:=(I<L);
  if Result then L:=I;
  if L=0 then exit;
  I:=1;
  while S[I]=S0[I] do begin
    if I=L then exit;
    Inc(I);
  end;
  Result:=Ord(S0[I])<Ord(S[I]);
end;

function BinarySearch(ABigger,ASmaller,AEqual: TSearchFunc; var FindData; const AEnd: Integer; const AStart: Integer = 0; const NoResult: Integer = -1): Integer;
var
  First,Last: Cardinal;
begin
  //binäre Suche
  First:=AStart;
  Last:=AEnd;
  Result:=(First+Last) div 2;
  while Last-First>1 do begin
    if ASmaller(Result,FindData)
      then Last:=Result
      else if ABigger(Result,FindData)
        then First:=Result
        else exit;
    Result:=(First+Last) div 2;
  end;

  {if ABigger(First) then begin
    if ASmaller(Last)
      then Result:=NoResult
      else Result:=Last;
  end else Result:=First;}

  if AEqual(First,FindData)
    then Result:=First
    else if AEqual(Last,FindData)
      then Result:=Last
      else Result:=NoResult;
end;

function BinarySearch(ABigger,ASmaller,AEqual: TSearch2Func; var FindData; const AEnd: Integer; const AStart: Integer = 0; const NoResult: Integer = -1): Integer;
var
  First,Last: Cardinal;
begin
  //binäre Suche
  First:=AStart;
  Last:=AEnd;
  Result:=(First+Last) div 2;
  while Last-First>1 do begin
    if ASmaller(Result,FindData)
      then Last:=Result
      else if ABigger(Result,FindData)
        then First:=Result
        else exit;
    Result:=(First+Last) div 2;
  end;

  {if ABigger(First) then begin
    if ASmaller(Last)
      then Result:=NoResult
      else Result:=Last;
  end else Result:=First;}

  if AEqual(First,FindData)
    then Result:=First
    else if AEqual(Last,FindData)
      then Result:=Last
      else Result:=NoResult;
end;

procedure Swap(var X1,X2; const Size: Cardinal);
var
  Temp: Pointer;
begin
  GetMem(Temp,Size);
  Move(X1,Temp^,Size);
  Move(X2,X1,Size);
  Move(Temp^,X2,Size);
  FreeMem(Temp,Size);
end;

function StrToIntB(const S: string; const Basis: Byte; ErrorResult: Int64 = -1): Int64;

  function GetVal(C: Char): Byte;
  const
    Ord0 = Ord('0');
    Ord9 = Ord('9');
    OrdA = Ord('A');
    OrdZ = Ord('Z');
    Ord_A= Ord('a');
    Ord_Z= Ord('z');
  var
    C2: Byte absolute C;
  begin
    if (C2>=Ord0) and (C2<=Ord9)
      then Result:=C2-Ord0
      else if (C2>=OrdA) and (C2<=OrdZ)
        then Result:=C2-OrdA+10
        else if (C2>=Ord_A) and (C2<=Ord_Z)
          then Result:=C2-Ord_A+10
          else Result:=255;
  end;

var
  I,Mul: Integer;
  AVal : Byte;
begin
  Mul:=1;
  Result:=0;
  for I:=Length(S) downto 1 do begin
    AVal:=GetVal(S[I]);
    if AVal>=Basis then begin
      Result:=ErrorResult;
      exit;
    end;
    Result+=AVal*Mul;
    Mul*=Basis;
  end;
end;

function RealMod(const V1,V2: Real): Real; inline;
begin
  Result:=Frac(V1/V2)*V2;
end;

function RealDivMod(const V1,V2: Real): TRealDivMod; inline;
var
  ADiv: Real;
begin
  ADiv:=V1/V2;
  Result.ADiv:=Trunc(ADiv);
  Result.AMod:=Frac(ADiv)*V2;
end;

function StrShorten(const S: string; AGetTextWidth: TGetTextWidth; MaxSize: Integer): string;
var
  AL,HL,LL: Integer;
  Fits    : Boolean;
begin
  if AGetTextWidth(S)<=MaxSize then begin
    Result:=S;
    exit;
  end;
  Result:=S[1]+'...';
  if AGetTextWidth(Result)>MaxSize then exit;

  LL:=0;
  HL:=Length(S);
  Fits:=false;

  while (not Fits) or (HL-LL>1) do begin
    AL:=(HL+LL) div 2;
    Result:=Copy(S,1,AL)+'...';
    Fits:=(AGetTextWidth(Result)<=MaxSize);
    if Fits
      then LL:=AL
      else HL:=AL;
  end;
end;

function IntMax(V: array of Integer): Integer;
var
  I: Integer;
begin
  Result:=V[0];
  for I:=1 to Length(V)-1 do if V[I]>Result then Result:=V[I];
end;

function Max(V: array of Real): Real;
var
  I: Integer;
begin
  Result:=V[0];
  for I:=1 to Length(V)-1 do if V[I]>Result then Result:=V[I];
end;

function IntMin(V: array of Integer): Integer;
var
  I: Integer;
begin
  Result:=V[0];
  for I:=1 to Length(V)-1 do if V[I]<Result then Result:=V[I];
end;

function Min(V: array of Real): Real;
var
  I: Integer;
begin
  Result:=V[0];
  for I:=1 to Length(V)-1 do if V[I]<Result then Result:=V[I];
end;

function PhiCut(APhi: Real): Real;
begin
  if APhi<0
    then Result:=2*pi+RealMod(APhi,2*pi)
    else Result:=RealMod(APhi,2*pi);
end;

function RoundUp(X: Real): Integer;
begin
  if IsZero(Frac(X))
    then Result:=Trunc(X)
    else Result:=Trunc(X)+1;
end;

function StrToIntE(const S: string; ErrorResult: Integer = -1): Integer;
begin
  if not TryStrToInt(S,Result) then Result:=ErrorResult;
end;

function StrToFloatE(const S: string; ErrorResult: Real): Real;
begin
  if not TryStrToFloat(S,Result) then Result:=ErrorResult;
end;

end.

