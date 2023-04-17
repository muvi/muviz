unit AdvClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynamicBase;

type
  TLList                = class
  protected
    FFirst      : Pointer;
    FLast       : Pointer;
    FPosition   : Pointer;
    FOnCanBefore: TSortFunc;
    FSize       : dsi;
    FCount      : dsi;
    function InitItem(const AData): Pointer;
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure ToFirst;
    procedure ToLast;
    procedure Next;
    procedure Prev;
    procedure InsertBefore(const AData);
    procedure InsertBehind(const AData);
    procedure InsertFirst(const AData);
    procedure InsertLast(const AData);
    procedure InsertSorted(const AData);
    procedure Delete;
    procedure DeleteI;
    procedure Get(out AData);
    procedure Clear;
    function Empty: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    function ValidPos: Boolean;

    property Count: dsi read FCount;
    property OnCanBefore: TSortFunc read FOnCanBefore write FOnCanBefore;
  end;

  TLQueue               = class
  protected
    FFirst      : Pointer;
    FLast       : Pointer;
    FSize       : dsi;
    FCount      : dsi;
    function InitItem(const AData): Pointer;
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure Push(const AData);
    procedure Pop(out AData);
    procedure Delete;
    procedure Get(out AData);
    procedure Clear;
    function Empty: Boolean;
    property Count: dsi read FCount;
  end;

  TLVector              = class
  protected
    FData    : Pointer;
    FSize    : dsi;
    FLength  : dsi;
    FPosition: Pointer;
    function IndexPtr(const Index: dsi): Pointer; inline;
    procedure SetLength(const ALength: dsi);
    function GetItemI(const Index: dsi): Pointer;
    procedure SetItemI(const Index: dsi; const Value: Pointer);
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure GetItem(const Index: dsi; out AData); overload;
    procedure SetItem(const Index: dsi; const AData); overload;

    procedure ToFirst;
    procedure ToLast;
    procedure Next;
    procedure Prev;
    procedure GetItem(out AData); overload;
    procedure SetItem(const AData); overload;
    function Empty: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;

    property Items[const Index: dsi]: Pointer read GetItemI write SetItemI;
  published
    property Length: dsi read FLength write SetLength;
  end;

  TLCircleVector        = class
  protected
    FData     : Pointer;
    FSize     : dsi;
    FLength   : dsi;
    FPosition : Pointer;
    FCirclePos: dsi;
    function IndexPtr(const Index: dsi): Pointer; inline;
    procedure SetLength(const ALength: dsi);
    function GetItemI(const Index: dsi): Pointer;
    procedure SetItemI(const Index: dsi; const Value: Pointer);
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure GetItem(const Index: dsi; out AData); overload;
    procedure SetItem(const Index: dsi; const AData); overload;

    procedure ToFirst;
    procedure ToLast;
    procedure Next;
    procedure Prev;
    procedure GetItem(out AData); overload;
    procedure SetItem(const AData); overload;
    function Empty: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    procedure Circulate;

    property Items[const Index: dsi]: Pointer read GetItemI write SetItemI;
  published
    property Length: dsi read FLength write SetLength;
  end;

  TLCFVector            = class
  protected
    FData       : Pointer;
    FSize       : dsi;
    FInnerSize  : dsi;
    FLength     : dsi;
    FOuterLength: dsi;
    FInnerLength: dsi;
    FPosition   : Pointer;
    FCirclePos  : dsi;
    FInnerPos   : dsi;
    FOuterPos   : dsi;
    function IndexPtr(const Index: dsi): Pointer; inline; overload;
    function IndexPtr(const OuterIndex,InnerIndex: dsi): Pointer; inline; overload;
    function GetItemI(const OuterIndex,InnerIndex: dsi): Pointer;
    procedure SetItemI(const OuterIndex,InnerIndex: dsi; const Value: Pointer);
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure GetItem(const OuterIndex,InnerIndex: dsi; out AData); overload;
    procedure SetItem(const OuterIndex,InnerIndex: dsi; const AData); overload;

    procedure ToFirst;
    procedure ToLast;
    procedure NextInner;
    procedure PrevInner;
    procedure NextOuter;
    procedure PrevOuter;
    procedure GetItem(out AData); overload;
    procedure SetItem(const AData); overload;
    function Empty: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    procedure Circulate;
    procedure SetLength(const AOuterLength,AInnerLength: dsi);

    property Items[const OuterIndex,InnerIndex: dsi]: Pointer read GetItemI write SetItemI;
  published
    property InnerLength: dsi read FInnerLength;
    property OuterLength: dsi read FOuterLength;
  end;

implementation

{TLList}

type
  TLListItem    = record
    Next,Prev,Data: Pointer;
  end;
  PLListItem    = ^TLListItem;

constructor TLList.Create(const ASize: dsi);
begin
  inherited Create;
  FSize:=ASize;
  FFirst:=nil;
  FLast:=nil;
  FPosition:=nil;
end;

destructor TLList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLList.InitItem(const AData): Pointer;
var
  AResult: PLListItem absolute Result;
begin
  GetMem(Result,SizeOf(TLListItem));
  GetMem(AResult^.Data,FSize);
  Move(AData,AResult^.Data^,FSize);
end;

procedure TLList.ToFirst;
begin
  FPosition:=FFirst;
end;

procedure TLList.ToLast;
begin
  FPosition:=FLast;
end;

procedure TLList.Next;
begin
  FPosition:=PLListItem(FPosition)^.Next;
end;

procedure TLList.Prev;
begin
  FPosition:=PLListItem(FPosition)^.Prev;
end;

procedure TLList.InsertBefore(const AData);
var
  ANew     : PLListItem;
  APosition: PLListItem;// absolute FPosition;
begin
  ANew:=InitItem(AData);
  APosition:=FPosition;
  with ANew^ do begin
    Prev:=APosition^.Prev;
    Next:=APosition;
    if APosition^.Prev<>nil
      then PLListItem(APosition^.Prev)^.Next:=ANew
      else FFirst:=ANew;
    APosition^.Prev:=ANew;
  end;
  Inc(FCount);
end;

procedure TLList.InsertBehind(const AData);
var
  ANew     : PLListItem;
  APosition: PLListItem;// absolute FPosition;
begin
  ANew:=InitItem(AData);
  APosition:=FPosition;
  with ANew^ do begin
    Next:=APosition^.Next;
    Prev:=APosition;
    if APosition^.Next<>nil
      then PLListItem(APosition^.Next)^.Prev:=ANew
      else FLast:=ANew;
    APosition^.Next:=ANew;
  end;
  Inc(FCount);
end;

procedure TLList.InsertFirst(const AData);
var
  ANew: PLListItem;
begin
  ANew:=InitItem(AData);
  with ANew^ do begin
    Prev:=nil;
    Next:=FFirst;
    if FFirst<>nil
      then PLListItem(FFirst)^.Prev:=ANew
      else FLast:=ANew;
    FFirst:=ANew;
  end;
  Inc(FCount);
end;

procedure TLList.InsertLast(const AData);
var
  ANew: PLListItem;
begin
  ANew:=InitItem(AData);
  with ANew^ do begin
    Next:=nil;
    Prev:=FLast;
    if FLast<>nil
      then PLListItem(FLast)^.Next:=ANew
      else FFirst:=ANew;
    FLast:=ANew;
  end;
  Inc(FCount);
end;

procedure TLList.InsertSorted(const AData);
var
  APosition,APos2: PLListItem;
begin
  if FFirst=nil then begin
    InsertLast(AData);
    exit;
  end;
  APosition:=FFirst;
  while not FOnCanBefore(AData,APosition^.Data) do begin
    APosition:=APosition^.Next;
    if APosition=nil then begin
      InsertLast(AData);
      exit;
    end;
  end;
  APos2:=FPosition;
  FPosition:=APosition;
  InsertBefore(AData);
  FPosition:=APos2;
end;

procedure TLList.Delete;
var
  APosition: PLListItem;
begin
  APosition:=FPosition;
  FreeMem(APosition^.Data,FSize);
  FPosition:=APosition^.Next;
  if APosition^.Prev<>nil
    then PLListItem(APosition^.Prev)^.Next:=APosition^.Next
    else FFirst:=APosition^.Next;
  if APosition^.Next<>nil
    then PLListItem(APosition^.Next)^.Prev:=APosition^.Prev
    else FLast:=APosition^.Prev;
  FreeMem(APosition,SizeOf(TLListItem));
  Dec(FCount);
end;

procedure TLList.DeleteI;
var
  APosition: PLListItem;
begin
  APosition:=FPosition;
  FreeMem(APosition^.Data,FSize);
  FPosition:=APosition^.Prev;
  if APosition^.Prev<>nil
    then PLListItem(APosition^.Prev)^.Next:=APosition^.Next
    else FFirst:=APosition^.Next;
  if APosition^.Next<>nil
    then PLListItem(APosition^.Next)^.Prev:=APosition^.Prev
    else FLast:=APosition^.Prev;
  FreeMem(APosition,SizeOf(TLListItem));
  Dec(FCount);
end;

procedure TLList.Get(out AData);
//var
  //APosition: PLListItem absolute FPosition;
begin
  Move(PLListItem(FPosition)^.Data^,AData,FSize);
end;

procedure TLList.Clear;
var
  APos,AOldPos: PLListItem;
begin
  APos:=FFirst;
  while APos<>nil do begin
    FreeMem(APos^.Data,FSize);
    AOldPos:=APos;
    APos:=APos^.Next;
    FreeMem(AOldPos,SizeOf(TLListItem));
  end;
  FFirst:=nil;
  FLast:=nil;
  FPosition:=nil;
  FCount:=0;
end;

function TLList.Empty: Boolean;
begin
  Result:=(FFirst=nil);
end;

function TLList.IsFirst: Boolean;
begin
  Result:=(FPosition=FFirst);
end;

function TLList.IsLast: Boolean;
begin
  Result:=(FPosition=FLast);
end;

function TLList.ValidPos: Boolean;
begin
  Result:=(FPosition<>nil);
end;

{TLCue}

type
  TLCueItem     = record
    Prev,Data: Pointer;
  end;
  PLCueItem     = ^TLCueItem;

constructor TLQueue.Create(const ASize: dsi);
begin
  inherited Create;
  FSize:=ASize;
  FFirst:=nil;
  FLast:=nil;
end;

destructor TLQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLQueue.InitItem(const AData): Pointer;
var
  AResult: PLCueItem absolute Result;
begin
  GetMem(Result,SizeOf(TLCueItem));
  GetMem(AResult^.Data,FSize);
  Move(AData,AResult^.Data^,FSize);
end;

procedure TLQueue.Push(const AData);
var
  ANew: PLCueItem;
begin
  ANew:=InitItem(AData);
  with ANew^ do begin
    Prev:=nil;
    if FFirst<>nil
      then PLCueItem(FFirst)^.Prev:=ANew
      else FLast:=ANew;
    FFirst:=ANew;
  end;
  Inc(FCount);
end;

procedure TLQueue.Delete;
var
  APosition: PLCueItem;
begin
  APosition:=FLast;
  FreeMem(APosition^.Data,FSize);
  FLast:=APosition^.Prev;
  if FLast=nil then FFirst:=nil;
  FreeMem(APosition,SizeOf(TLCueItem));
  Dec(FCount);
end;

procedure TLQueue.Get(out AData);
//var
  //ALast: PLCueItem absolute FLast;
begin
  Move(PLCueItem(FLast)^.Data^,AData,FSize);
end;

procedure TLQueue.Pop(out AData);
begin
  Get(AData);
  Delete;
end;

procedure TLQueue.Clear;
var
  APos,AOldPos: PLCueItem;
begin
  APos:=FLast;
  while APos<>nil do begin
    FreeMem(APos^.Data,FSize);
    AOldPos:=APos;
    APos:=APos^.Prev;
    FreeMem(AOldPos,SizeOf(TLCueItem));
  end;
  FFirst:=nil;
  FLast:=nil;
  FCount:=0;
end;

function TLQueue.Empty: Boolean;
begin
  Result:=(FFirst=nil);
end;

{TLVector}

constructor TLVector.Create(const ASize: dsi = SizeOf(TObject));
begin
  inherited Create;
  FSize:=ASize;
  FLength:=0;
end;

destructor TLVector.Destroy;
begin
  if FLength>0 then FreeMem(FData,FSize*FLength);
  inherited Destroy;
end;

function TLVector.IndexPtr(const Index: dsi): Pointer; inline;
begin
  Result:=FData+(Index*FSize);
end;

procedure TLVector.SetLength(const ALength: dsi);
begin
  if FLength>0 then FreeMem(FData,FSize*FLength);
  GetMem(FData,FSize*ALength);
  FLength:=ALength;
end;

function TLVector.GetItemI(const Index: dsi): Pointer;
begin
  GetMem(Result,FSize);
  Move(IndexPtr(Index)^,Result^,FSize);
end;

procedure TLVector.SetItemI(const Index: dsi; const Value: Pointer);
begin
  Move(Value^,IndexPtr(Index)^,FSize);
end;

procedure TLVector.GetItem(const Index: dsi; out AData);
begin
  Move(IndexPtr(Index)^,AData,FSize);
end;

procedure TLVector.SetItem(const Index: dsi; const AData);
begin
  Move(AData,IndexPtr(Index)^,FSize);
end;

procedure TLVector.ToFirst;
begin
  FPosition:=FData; //IndexPtr(0)
end;

procedure TLVector.ToLast;
begin
  FPosition:=IndexPtr(FLength-1);
end;

procedure TLVector.Next;
begin
  FPosition+=FSize;
end;

procedure TLVector.Prev;
begin
  FPosition-=FSize;
end;

procedure TLVector.GetItem(out AData);
begin
  Move(FPosition^,AData,FSize);
end;

procedure TLVector.SetItem(const AData);
begin
  Move(AData,FPosition^,FSize);
end;

function TLVector.Empty: Boolean;
begin
  Result:=(FLength=0);
end;

function TLVector.IsFirst: Boolean;
begin
  Result:=(FPosition=FData); //IndexPtr(0)
end;

function TLVector.IsLast: Boolean;
begin
  Result:=(FPosition=IndexPtr(FLength-1));
end;

{TLCircleVector}

constructor TLCircleVector.Create(const ASize: dsi = SizeOf(TObject));
begin
  inherited Create;
  FSize:=ASize;
  FLength:=0;
  FCirclePos:=0;
end;

destructor TLCircleVector.Destroy;
begin
  if FLength>0 then FreeMem(FData,FSize*FLength);
  inherited Destroy;
end;

function TLCircleVector.IndexPtr(const Index: dsi): Pointer; inline;
begin
  Result:=FData+(((Index+FCirclePos) mod FLength)*FSize);
end;

procedure TLCircleVector.SetLength(const ALength: dsi);
begin
  if FLength>0 then FreeMem(FData,FSize*FLength);
  GetMem(FData,FSize*ALength);
  FLength:=ALength;
end;

function TLCircleVector.GetItemI(const Index: dsi): Pointer;
begin
  GetMem(Result,FSize);
  Move(IndexPtr(Index)^,Result^,FSize);
end;

procedure TLCircleVector.SetItemI(const Index: dsi; const Value: Pointer);
begin
  Move(Value^,IndexPtr(Index)^,FSize);
end;

procedure TLCircleVector.GetItem(const Index: dsi; out AData);
begin
  Move(IndexPtr(Index)^,AData,FSize);
end;

procedure TLCircleVector.SetItem(const Index: dsi; const AData);
begin
  Move(AData,IndexPtr(Index)^,FSize);
end;

procedure TLCircleVector.ToFirst;
begin
  FPosition:=IndexPtr(0);
end;

procedure TLCircleVector.ToLast;
begin
  FPosition:=IndexPtr(FLength-1);
end;

procedure TLCircleVector.Next;
begin
  FPosition+=FSize;
  if FPosition=FData+(FLength*FSize)
    then FPosition:=FData;
end;

procedure TLCircleVector.Prev;
begin
  if FPosition<>FData
    then FPosition-=FSize
    else FPosition:=FData+((FLength-1)*FSize);
end;

procedure TLCircleVector.GetItem(out AData);
begin
  Move(FPosition^,AData,FSize);
end;

procedure TLCircleVector.SetItem(const AData);
begin
  Move(AData,FPosition^,FSize);
end;

function TLCircleVector.Empty: Boolean;
begin
  Result:=(FLength=0);
end;

function TLCircleVector.IsFirst: Boolean;
begin
  Result:=(FPosition=IndexPtr(0));
end;

function TLCircleVector.IsLast: Boolean;
begin
  Result:=(FPosition=IndexPtr(FLength-1));
end;

procedure TLCircleVector.Circulate;
begin
  FCirclePos:=(FCirclePos+1) mod FLength;
end;

{TLCircleVector}

constructor TLCFVector.Create(const ASize: dsi = SizeOf(TObject));
begin
  inherited Create;
  FSize:=ASize;
  FLength:=0;
  FCirclePos:=0;
  FInnerLength:=0;
  FInnerSize:=0;
  FOuterLength:=0;
end;

destructor TLCFVector.Destroy;
begin
  if FLength>0 then FreeMem(FData,FSize*FLength);
  inherited Destroy;
end;

function TLCFVector.IndexPtr(const Index: dsi): Pointer; inline;
begin
  Result:=FData+(((Index+(FCirclePos*FInnerLength)) mod FLength)*FSize);
end;

function TLCFVector.IndexPtr(const OuterIndex,InnerIndex: dsi): Pointer; inline;
begin
  Result:=IndexPtr((OuterIndex*FInnerLength)+InnerIndex);
end;

procedure TLCFVector.SetLength(const AOuterLength,AInnerLength: dsi);
begin
  if FLength>0 then FreeMem(FData,FSize*FLength);
  FInnerLength:=AInnerLength;
  FOuterLength:=AOuterLength;
  FLength:=FInnerLength*FOuterLength;
  FInnerSize:=FInnerLength*FSize;
  GetMem(FData,FSize*FLength);
end;

function TLCFVector.GetItemI(const OuterIndex,InnerIndex: dsi): Pointer;
begin
  GetMem(Result,FSize);
  Move(IndexPtr(OuterIndex,InnerIndex)^,Result^,FSize);
end;

procedure TLCFVector.SetItemI(const OuterIndex,InnerIndex: dsi; const Value: Pointer);
begin
  Move(Value^,IndexPtr(OuterIndex,InnerIndex)^,FSize);
end;

procedure TLCFVector.GetItem(const OuterIndex,InnerIndex: dsi; out AData);
begin
  Move(IndexPtr(OuterIndex,InnerIndex)^,AData,FSize);
end;

procedure TLCFVector.SetItem(const OuterIndex,InnerIndex: dsi; const AData);
begin
  Move(AData,IndexPtr(OuterIndex,InnerIndex)^,FSize);
end;

procedure TLCFVector.ToFirst;
begin
  FPosition:=IndexPtr(0);
  FInnerPos:=0;
  FOuterPos:=0;
end;

procedure TLCFVector.ToLast;
begin
  FPosition:=IndexPtr(FLength-1);
  FInnerPos:=FInnerLength-1;
  FOuterPos:=FOuterLength-1;
end;

procedure TLCFVector.NextInner;
begin
  FPosition+=FSize;
  {if FPosition>=FData+(FLength*FSize)
    then FPosition:=FData;}
  Inc(FInnerPos);
end;

procedure TLCFVector.PrevInner;
begin
  FPosition-=FSize;
  Dec(FInnerPos);
  {if FPosition>FData
    then FPosition-=FSize
    else FPosition:=FData+((FLength-1)*FSize);}
end;

procedure TLCFVector.NextOuter;
begin
  Inc(FOuterPos);
  FPosition:=IndexPtr(FOuterPos,FInnerPos);
end;

procedure TLCFVector.PrevOuter;
begin
  if FOuterPos=0
    then FOuterPos:=FOuterLength-1
    else Dec(FOuterPos);
  //if FOuterPos<0 then FOuterPos:=FOuterLength+FOuterPos; <- geht nicht, weil dsi vorzeichenlos ist
  FPosition:=IndexPtr(FOuterPos,FInnerPos);
end;

procedure TLCFVector.GetItem(out AData);
begin
  Move(FPosition^,AData,FSize);
end;

procedure TLCFVector.SetItem(const AData);
begin
  Move(AData,FPosition^,FSize);
end;

function TLCFVector.Empty: Boolean;
begin
  Result:=(FLength=0);
end;

function TLCFVector.IsFirst: Boolean;
begin
  Result:=(FPosition=IndexPtr(0));
end;

function TLCFVector.IsLast: Boolean;
begin
  Result:=(FPosition=IndexPtr(FLength-1));
end;

procedure TLCFVector.Circulate;
begin
  FCirclePos:=(FCirclePos+1) mod FOuterLength;
end;

end.

