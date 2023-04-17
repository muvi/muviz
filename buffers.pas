unit Buffers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, TVSPSources, TVSPSourceUtil;

type
  TVBufferItem     = Single;
  TVBufferContent  = array [0..0] of TVBufferItem;
  PVBufferContent  = ^TVBufferContent;

  TVFloatBufferImpl= class (TInterfacedObject, IVFloatBuffer)
  private
    FBuffer          : ITVSPSource;
    function GetItem(Index: LongWord): TVFloat; cdecl;
    procedure SetItem(Index: LongWord; Value: TVFloat); cdecl;
    function GetSize: LongWord; cdecl;
  public
    constructor Create(ABuffer: ITVSPSource);
    constructor Create(AItems: array of TVBufferItem);
    destructor Destroy; override;
    //Buffer size is not modifiable, thus Cut and Resize return a new buffer
    //be carefull using this method: the created buffer immediately becomes undefined
    //when releasing the original buffer.
    //to avoid this use Cut(<Size>, <Pos>).Resize(<Size>)
    function Cut(Size: LongWord; Pos: LongWord = 0): IVFloatBuffer; cdecl;
    //Warning: new item values are undefined and have to be set by developer
    //Resize(Size) creates a copy of the buffer
    function Resize(NewSize: LongWord): IVFloatBuffer; cdecl;
    function FromSource(Source: ITVSPSource): IVFloatBuffer; cdecl;
    function GetSource: ITVSPSource; cdecl;

    property Items[Index: LongWord]: TVFloat read GetItem write SetItem; default;
    property Size: LongWord read GetSize;
  end;

procedure Init;
procedure Done;

implementation

{%REGION TVFloatBuffer}

constructor TVFloatBufferImpl.Create(ABuffer: ITVSPSource);
begin
  Assert(ABuffer <> nil);
  inherited Create;
  FBuffer:=ABuffer;
end;

constructor TVFloatBufferImpl.Create(AItems: array of TVBufferItem);
var
  I: Integer;
begin
  inherited Create;
  FBuffer:=SourceUtil[TVSPSourceID(SizeOf(TVBufferItem) * Length(AItems))];
  for I:=0 to Length(AItems)-1
    do FBuffer.&Set(I * SizeOf(TVBufferItem), SizeOf(TVBufferItem), AItems[I]);
end;

destructor TVFloatBufferImpl.Destroy;
begin
  FBuffer:=nil;
  inherited Destroy;
end;

function TVFloatBufferImpl.GetItem(Index: LongWord): TVFloat; cdecl;
var
  AResult: TVBufferItem;
begin
  Assert(Index < Size);
  FBuffer.Get(Index * SizeOf(TVBufferItem), SizeOf(TVBufferItem), AResult);
  Result:=AResult;
end;

procedure TVFloatBufferImpl.SetItem(Index: LongWord; Value: TVFloat); cdecl;
var
  AValue: TVBufferItem;
begin
  Assert(Index < Size);
  AValue:=Value;
  FBuffer.&Set(Index * SizeOf(TVBufferItem), SizeOf(TVBufferItem), AValue);
end;

function TVFloatBufferImpl.GetSize: LongWord; cdecl;
begin
  Assert(FBuffer.Size mod SizeOf(TVBufferItem) = 0);
  Result:=FBuffer.Size div SizeOf(TVBufferItem);
end;

function TVFloatBufferImpl.Cut(Size: LongWord; Pos: LongWord = 0): IVFloatBuffer; cdecl;
begin
  //TODO
  Assert(false);
  //Result:=TVFloatBufferImpl.Create(FBuffer.Cut(Pos * SizeOf(TVBufferItem), Size * SizeOf(TVBufferItem)));
end;

function TVFloatBufferImpl.Resize(NewSize: LongWord): IVFloatBuffer; cdecl;
var
  ANewBuf                : TVFloatBufferImpl;
  AByteSize, ANewByteSize: LongWord;

  procedure DoCopy(ASize: LongWord);
  var
    ACopyMem: Pointer;
  begin
    if ASize > 0 then begin
      GetMem(ACopyMem, ASize);
      FBuffer.Get(0, ASize, ACopyMem^);
      ANewBuf.FBuffer.&Set(0, ASize, ACopyMem^);
      FreeMem(ACopyMem, ASize);
    end;
  end;

begin
  AByteSize:=FBuffer.Size;
  ANewByteSize:=NewSize * SizeOf(TVBufferItem);
  ANewBuf:=TVFloatBufferImpl.Create(SourceUtil[TVSPSourceID(ANewByteSize)]);

  if ANewByteSize>=AByteSize
    then DoCopy(AByteSize)
    else DoCopy(ANewByteSize);

  Result:=IVFloatBuffer(ANewBuf);
end;

function TVFloatBufferImpl.FromSource(Source: ITVSPSource): IVFloatBuffer; cdecl;
begin
  Result:=TVFloatBufferImpl.Create(Source);
end;

function TVFloatBufferImpl.GetSource: ITVSPSource; cdecl;
begin
  Result:=FBuffer;
end;

{%ENDREGION}

procedure Init;
begin
  EmptyBuffer:=TVFloatBufferImpl.Create(SourceUtil[ZEROSOURCEID]);
end;

procedure Done;
begin
  EmptyBuffer:=nil;
end;

end.

