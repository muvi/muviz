unit DllStrUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DllStr;

type
  TStringManager  = class (TObject,IStringManager)
  private
    function GetStrings(const Index: Integer): string;
    procedure SetStrings(const Index: Integer; const Value: string);
  protected
    function QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  public
    FStrings: array of string;
    constructor Create;
    destructor Destroy; override;
    function NewString(const S: ShortString): DString; stdcall; overload;
    function NewString(const S: DString): DString; stdcall; overload;
    function NewString_(const S: AnsiString): DString; stdcall;
    procedure DisposeStr(const S: DString); stdcall;
    function SCopy(const S: DString; const Index,Count: SInt): DString; stdcall;
    procedure SCopyTo(const S,Dest: DString; const Index,Count: SInt); stdcall;
    procedure SCopyAndAdd(const S,Dest: DString; const Index,Count: SInt); stdcall;
    procedure SDelete(const S: DString; const Index,Count: SInt); stdcall;
    function SPos(const SubStr: ShortString; const S: DString): SInt; stdcall; overload;
    function SPos(const SubStr: DString; const S: DString): SInt; stdcall; overload;
    function SGet(const S: DString; const Index,Count: SInt): ShortString; stdcall;
    procedure SSet(const S: DString; const X: ShortString); stdcall; overload;
    procedure SSet(const S: DString; const X: DString); stdcall; overload;
    procedure SAdd(const S: DString; const X: ShortString); stdcall; overload;
    procedure SAdd(const S: DString; const X: DString); stdcall; overload;
    procedure SAdd(const S: DString; const X: DLongString); stdcall; overload;
    procedure SAdd(const S: DString; const X: DLongString; const Size: SInt); stdcall; overload;
    procedure SAddBefore(const S: DString; const X: ShortString); stdcall; overload;
    procedure SAddBefore(const S: DString; const X: DString); stdcall; overload;
    function SLength(const S: DString): SInt; stdcall;
    function SGetChar(const S: DString; const Index: SInt): Char; stdcall;
    function SSameAs(const S1: DString; const S2: ShortString): Boolean; stdcall; overload;
    function SSameAs(const S1,S2: DString): Boolean; stdcall; overload;
    procedure SLoadFromFile(var S: DString; const FileName: ShortString); stdcall;
    procedure SSaveToFile(var S: DString; const FileName: ShortString); stdcall;
    property Strings[Index: Integer]: string read GetStrings write SetStrings; default;
  end;

implementation

{TStringManager}

constructor TStringManager.Create;
begin
  inherited Create;
end;

destructor TStringManager.Destroy;
begin
  inherited Destroy;
  SetLength(FStrings,0);
end;

function TStringManager.GetStrings(const Index: Integer): string;
begin
  Result:=FStrings[Index];
end;

procedure TStringManager.SetStrings(const Index: Integer; const Value: string);
begin
  FStrings[Index]:=Value;
end;

function TStringManager.NewString(const S: ShortString): DString; stdcall;
begin
  Result:=Length(FStrings);
  SetLength(FStrings,Result+1);
  FStrings[Result]:=S;
end;

function TStringManager.NewString(const S: DString): DString; stdcall;
begin
  Result:=Length(FStrings);
  SetLength(FStrings,Result+1);
  FStrings[Result]:=FStrings[S];
end;

function TStringManager.NewString_(const S: AnsiString): DString; stdcall;
begin
  Result:=Length(FStrings);
  SetLength(FStrings,Result+1);
  FStrings[Result]:=S;
end;

procedure TStringManager.DisposeStr(const S: DString); stdcall;
var
  I,L: Integer;
begin
  L:=Length(FStrings)-1;
  for I:=S to L-1 do FStrings[I]:=FStrings[I+1];
  FStrings[L]:='';
  SetLength(FStrings,L);
end;

function TStringManager.SCopy(const S: DString; const Index,Count: SInt): DString; stdcall;
begin
  Result:=NewString(Copy(FStrings[S],Index,Count));
end;

procedure TStringManager.SCopyTo(const S,Dest: DString; const Index,Count: SInt); stdcall;
begin
  FStrings[Dest]:=Copy(FStrings[S],Index,Count);
end;

procedure TStringManager.SCopyAndAdd(const S,Dest: DString; const Index,Count: SInt); stdcall;
begin
  FStrings[Dest]+=Copy(FStrings[S],Index,Count);
end;

procedure TStringManager.SDelete(const S: DString; const Index,Count: SInt); stdcall;
begin
  Delete(FStrings[S],Index,Count);
end;

function TStringManager.SPos(const SubStr: ShortString; const S: DString): SInt; stdcall; overload;
begin
  Result:=Pos(SubStr,FStrings[S]);
end;

function TStringManager.SPos(const SubStr: DString; const S: DString): SInt; stdcall; overload;
begin
  Result:=Pos(FStrings[SubStr],FStrings[S]);
end;

function TStringManager.SGet(const S: DString; const Index,Count: SInt): ShortString; stdcall;
begin
  Result:=Copy(FStrings[S],Index,Count);
end;

procedure TStringManager.SSet(const S: DString; const X: ShortString); stdcall;
begin
  FStrings[S]:=X;
end;

procedure TStringManager.SSet(const S: DString; const X: DString); stdcall;
begin
  FStrings[S]:=FStrings[X];
end;

procedure TStringManager.SAdd(const S: DString; const X: ShortString); stdcall;
begin
  FStrings[S]+=X;
end;

procedure TStringManager.SAdd(const S: DString; const X: DString); stdcall;
begin
  FStrings[S]+=FStrings[X];
end;

procedure TStringManager.SAdd(const S: DString; const X: DLongString); stdcall; overload;
begin
  FStrings[S]+=X;
end;

procedure TStringManager.SAdd(const S: DString; const X: DLongString; const Size: SInt); stdcall; overload;
var
  I: Integer;
  //A: array of Char;
begin
  //SetLength(A,Size);
  //A:=X;
  //FStrings[S]+=A;
  for I:=0 to Size-1 do FStrings[S]+=X[I];
  //FStrings[S]+=Copy(X,1,Size);
end;

procedure TStringManager.SAddBefore(const S: DString; const X: ShortString); stdcall;
begin
  FStrings[S]:=X+FStrings[S];
end;

procedure TStringManager.SAddBefore(const S: DString; const X: DString); stdcall;
begin
  FStrings[S]:=FStrings[X]+FStrings[S];
end;

function TStringManager.SLength(const S: DString): SInt; stdcall;
begin
  Result:=Length(FStrings[S]);
end;

function TStringManager.SGetChar(const S: DString; const Index: SInt): Char; stdcall;
begin
  Result:=FStrings[S][Index];
end;

function TStringManager.SSameAs(const S1: DString; const S2: ShortString): Boolean; stdcall; overload;
begin
  Result:=(FStrings[S1]=S2);
end;

function TStringManager.SSameAs(const S1,S2: DString): Boolean; stdcall; overload;
begin
  Result:=(FStrings[S1]=FStrings[S2]);
end;

type
  TRWString = array [1..1048576] of Char;
const
  RWSize = SizeOf(TRWString);

procedure TStringManager.SLoadFromFile(var S: DString; const FileName: ShortString); stdcall;
var
  ABuffer: TRWString;
  FS     : TStream;
  ASize,I: Integer;
begin
  FStrings[S]:='';
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  while FS.Position<FS.Size-RWSize do begin
    FS.Read(ABuffer,RWSize);
    FStrings[S]+=ABuffer;
  end;
  ASize:=FS.Size-FS.Position;
  FS.Read(ABuffer,ASize);
  for I:=1 to ASize do FStrings[S]+=ABuffer[I];
  FS.Destroy;
end;

procedure TStringManager.SSaveToFile(var S: DString; const FileName: ShortString); stdcall;
var
  FS: TFileStream;
  I : Integer;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  for I:=1 to Length(FStrings[S]) do FS.Write(FStrings[S][I],1);
  FS.Destroy;
end;

{TStringManager - Standardinterfacemethoden}

function TStringManager.QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
begin
  Result:=0;
end;

function TStringManager._AddRef : longint;stdcall;
begin
  Result:=1;
end;

function TStringManager._Release : longint;stdcall;
begin
  Result:=1;
end;

{Allgemein}

{function NewString(const S: ShortString): IString; stdcall;
var
  ANewStr: TStringManager;
begin
  ANewStr:=TStringManager.Create(S);
  Result:=ANewStr;
end;

procedure DisposeString(var S: IString);
begin
  TStringManager(S).Destroy;
end; }

end.

