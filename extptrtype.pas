unit ExtPtrType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType;

const MaxPtrArray = 536870910;

type
  TSD2Bits            = type Pointer;

  TSD2BufElem        = MVFloat;
  PSD2BufElem        = ^TSD2BufElem;
  TSD2BufferFormat   = array [0..MaxPtrArray] of TSD2BufElem;
  PSD2BufferFormat   = ^TSD2BufferFormat;

  TSD2IntBufferFormat= array [0..MaxPtrArray] of MVInt;
  PSD2IntBufferFormat= ^TSD2IntBufferFormat;
  TSD2IntBufElem     = MVInt;
  PSD2IntBufElem     = ^TSD2IntBufElem;

  TSD2BitBufElem     = Cardinal;
  PSD2BitBufElem     = ^TSD2BitBufElem;
  TSD2BitBufFormat   = array [0..MaxPtrArray] of TSD2BitBufElem;
  PSD2BitBufFormat   = ^TSD2BitBufFormat;

procedure ZeroMVFloat(const Buffer: Pointer; const ACount: Cardinal; const AValue: MVFloat = 0.0); inline; deprecated;

function SD2Bits_Get(const Bits: PSD2BitBufFormat; const Index: MVInt): Boolean; inline; deprecated;
procedure SD2Bits_Set(const Bits: PSD2BitBufFormat; const Index: MVInt); inline; deprecated;
procedure SD2Bits_Reset(const Bits: PSD2BitBufFormat; const Size: MVInt); inline; deprecated;
function SD2Bits_Resize(var Bits: TSD2Bits; const NewSize: MVInt; var ElementSize,OldSize: MVInt; const BufferCount: MVInt = 1): MVInt; inline; deprecated;
function SD2Bits_Compare(var Bits1,Bits2: PSD2BitBufFormat; const Count,Size: MVInt): MVInt; {inline;} deprecated;
procedure SD2Bits_Copy(const Source,Dest: PSD2BitBufFormat; const Size: MVInt); inline; deprecated;
function SD2Bits_Evaluate(var Bits: PSD2BitBufFormat; const Count,Size: MVInt): MVInt; inline; deprecated;

const
  SD2BufElemSize        = SizeOf(TSD2BufElem);
  SD2BitComponentSize   = SizeOf(TSD2BitBufElem);
  SD2BitComponentSizeB  = SD2BitComponentSize*8;
  SD2IntBufElemSize     = SizeOf(TSD2IntBufElem);

implementation

procedure ZeroMVFloat(const Buffer: Pointer; const ACount: Cardinal; const AValue: MVFloat = 0.0); inline;
var
  I   : Integer;
  ABuf: PSD2BufferFormat absolute Buffer;
begin
  for I:=0 to ACount-1 do ABuf^[I]:=AValue;
end;

function SD2Bits_Get(const Bits: PSD2BitBufFormat; const Index: MVInt): Boolean; inline;
begin
  Result:=Boolean((Bits^[Index div SD2BitComponentSizeB] shr (Index mod SD2BitComponentSizeB)) and 1);
end;

procedure SD2Bits_Set(const Bits: PSD2BitBufFormat; const Index: MVInt); inline;
var
  ABuf: PSD2BitBufElem;
begin
  ABuf:=@Bits^[Index div SD2BitComponentSizeB];
  ABuf^:=ABuf^ or (1 shl (Index mod SD2BitComponentSizeB));
end;

procedure SD2Bits_Reset(const Bits: PSD2BitBufFormat; const Size: MVInt); inline;
var
  I: Integer;
begin
  for I:=0 to Size-1 do Bits^[I]:=0;
end;

procedure SD2Bits_Copy(const Source,Dest: PSD2BitBufFormat; const Size: MVInt); inline;
var
  I: Integer;
begin
  for I:=0 to Size-1 do Dest^[I]:=Source^[I];
end;

function SD2Bits_Resize(var Bits: TSD2Bits; const NewSize: MVInt; var ElementSize,OldSize: MVInt; const BufferCount: MVInt = 1): MVInt; inline;
begin
  FreeMem(Bits,OldSize);
  Result:=NewSize div SD2BitComponentSizeB;
  if NewSize mod SD2BitComponentSizeB>0 then Inc(Result);
  ElementSize:=Result*SD2BitComponentSize;
  OldSize:=ElementSize*BufferCount;
  GetMem(Bits,OldSize);
end;

function SD2Bits_Compare(var Bits1,Bits2: PSD2BitBufFormat; const Count,Size: MVInt): MVInt; //inline;
var
  I,J,OverSize: Integer;
  Comb        : TSD2BitBufElem;
begin
  Result:=0;
  for I:=0 to Count-2 do begin
    Comb:=Bits1^[I] and Bits2^[I];
    for J:=0 to SD2BitComponentSizeB-1 do Result+=(Comb shr J) and 1;
  end;
  OverSize:=Size-((Count-1)*SD2BitComponentSizeB);
  Comb:=Bits1^[Count-1] and Bits2^[Count-1];
  for J:=0 to OverSize-1 do Result+=(Comb shr J) and 1;
end;

function SD2Bits_Evaluate(var Bits: PSD2BitBufFormat; const Count,Size: MVInt): MVInt; inline;
var
  I,J,OverSize: Integer;
begin
  Result:=0;
  for I:=0 to Count-2 do begin
    for J:=0 to SD2BitComponentSizeB-1 do Result+=(Bits^[I] shr J) and 1;
  end;
  OverSize:=Size-((Count-1)*SD2BitComponentSizeB);
  for J:=0 to OverSize-1 do Result+=(Bits^[Count-1] shr J) and 1;
end;

end.

