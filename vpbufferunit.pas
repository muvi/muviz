unit VPBufferUnit;

{$mode objfpc}{$H+}

interface

uses
  VPBuffers, MInterfacedObject, VisType, PluginType;

const
  vpRealBufferMax  = 536870910;

type
  vpRealBuffer2    = array [0..vpRealBufferMax] of vpRealBufferItem;
  pvpRealBuffer2   = ^vpRealBuffer2;
  vpRealBuffer_    = packed record
    Size: LongWord;
    R1  : LongWord;
    R2  : Int64;
    Dest: pvpRealBuffer2;
    R3  : LongWord;
    R4  : UInt64;
  end;
  pvpRealBuffer_   = ^vpRealBuffer_;
  pvpRealBuffer    = ^vpRealBuffer;

  TVPBufferManager = class (TMInterfacedObject, IVPBufferManager)
  private
    function GetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord): vpRealBufferItem; stdcall;
    procedure SetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord; const Value: vpRealBufferItem); stdcall;
    function GetAddressOfItem(const ABuffer: vpRealBuffer; const Index: LongWord): pvpRealBufferItem; stdcall;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    constructor Create;
    function NewBuffer(const ASize: LongWord): vpRealBuffer; stdcall;
    procedure DisposeBuffer(var ABuffer: vpRealBuffer); stdcall;
    function ToVPRealBuffer(const ASize: LongWord; const Data: Pointer): vpRealBuffer; stdcall;
    function GetBufferData(const ABuffer: vpRealBuffer): Pointer; stdcall;
    function SizeOfBuffer(const ABuffer: vpRealBuffer): LongWord; stdcall;
    procedure ResizeBuffer(var ABuffer: vpRealBuffer; const NewSize: LongWord); stdcall;
    function Cut(const Source: vpRealBuffer; Pos,Count: LongWord): vpRealBuffer; stdcall;
    function Connect(const Sources: array of vpRealBuffer): vpRealBuffer; stdcall;

    property AddressOfItem[const ABuffer: vpRealBuffer; const Index: LongWord]: pvpRealBufferItem read GetAddressOfItem;
    //property Data[const ABuffer: vpRealBuffer]: Pointer;
    property Items[const ABuffer: vpRealBuffer; const Index: LongWord]: vpRealBufferItem read GetBufferItem write SetBufferItem;
    property Size[const ABuffer: vpRealBuffer]: LongWord read SizeOfBuffer;
  end;

  {TVPBufferManager = class (TMInterfacedObject, IVPBufferManager)
  private
    FBuffers: TUIDArray;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    constructor Create;
    destructor Destroy;
    function NewBuffer: IVPBuffer; stdcall;
    function GetBuffer(const ABuffer: vpRealBuffer): IVPBuffer; stdcall;
  end;

  TVPBuffer        = class (TMInterfacedObject, IVPBuffer)
  private
    FData     : Pointer;
    FSize     : Cardinal;
    FDataOwned: Boolean;
    FRefCount : Cardinal;
    function GetItem(Index: MVIndex): vpReal; stdcall;
    procedure SetItem(Index: MVIndex; const Value: vpReal); stdcall;
  protected
    function QueryInterface(const iid: tguid; out obj) : LongInt; stdcall; override;
    function _AddRef: LongInt; stdcall; override;
    function _Release: LongInt; stdcall; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    procedure FreeContent; inline;
  public
    constructor Create;
    destructor Destroy;
    property Items[Index: MVIndex] read GetItem write SetItem;
  end;}

function RealBufferMiddle(const ABuffer: vpRealBuffer): vpRealBufferItem;

var
  BufferManager: TVPBufferManager;

implementation

{TVPBufferManager}

constructor TVPBufferManager.Create;
begin
  inherited Create;
  BufferManager:=Self;
end;

const
  Local_BM_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TVPBufferManager.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_BM_Version;
end;

function TVPBufferManager.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_BM_Version
    then Result:=IMInterface(IVPBufferManager(Self))
    else Result:=inherited Future(Version);
end;

function TVPBufferManager.NewBuffer(const ASize: LongWord): vpRealBuffer; stdcall;
var
  Result2: vpRealBuffer_ absolute Result;
begin
  GetMem(Result2.Dest,SizeOf(vpRealBufferItem)*ASize);
  Result2.Size:=ASize;
end;

procedure TVPBufferManager.DisposeBuffer(var ABuffer: vpRealBuffer); stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  FreeMem(ABuffer2.Dest,SizeOf(vpRealBufferItem)*ABuffer2.Size);
  ABuffer2.Size:=0;
end;

function TVPBufferManager.ToVPRealBuffer(const ASize: LongWord; const Data: Pointer): vpRealBuffer; stdcall;
var
  Result2: vpRealBuffer_ absolute Result;
begin
  with Result2 do begin
    Dest:=Data;
    Size:=ASize;
  end;
end;

function TVPBufferManager.GetBufferData(const ABuffer: vpRealBuffer): Pointer; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=ABuffer2.Dest;
end;

function TVPBufferManager.SizeOfBuffer(const ABuffer: vpRealBuffer): LongWord; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=ABuffer2.Size;
end;

procedure TVPBufferManager.ResizeBuffer(var ABuffer: vpRealBuffer; const NewSize: LongWord); stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  FreeMem(ABuffer2.Dest,SizeOf(vpReal)*ABuffer2.Size);
  GetMem(ABuffer2.Dest,SizeOf(vpReal)*NewSize);
  ABuffer2.Size:=NewSize;
end;

function TVPBufferManager.Cut(const Source: vpRealBuffer; Pos,Count: LongWord): vpRealBuffer; stdcall;
var
  ASource: vpRealBuffer_ absolute Source;
  AResult: vpRealBuffer_ absolute Result;
  APos   : Cardinal;
begin
  if Pos<=ASource.Size
    then APos:=Pos
    else APos:=ASource.Size;
  if APos+Count<=ASource.Size
    then AResult.Size:=Count
    else AResult.Size:=ASource.Size-APos;
  AResult.Dest:=ASource.Dest+(APos*SizeOf(vpRealBufferItem));
end;

function TVPBufferManager.Connect(const Sources: array of vpRealBuffer): vpRealBuffer; stdcall;
var
  I      : Integer;
  ABuf   : pvpRealBuffer;
  ABuf2  : pvpRealBuffer_ absolute ABuf;
  AResult: vpRealBuffer_ absolute Result;
  ADest  : Pointer;
begin
  AResult.Size:=0;
  for I:=0 to Length(Sources)-1 do begin
    ABuf:=@Sources[I];
    AResult.Size+=ABuf2^.Size;
  end;
  GetMem(AResult.Dest,SizeOf(vpRealBufferItem)*AResult.Size);
  ADest:=AResult.Dest;
  for I:=0 to Length(Sources)-1 do begin
    ABuf:=@Sources[I];
    Move(ABuf2^.Dest^,ADest^,ABuf2^.Size*SizeOf(vpRealBufferItem));
    ADest+=ABuf2^.Size*SizeOf(vpRealBufferItem);
  end;
end;

function TVPBufferManager.GetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord): vpRealBufferItem; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=ABuffer2.Dest^[Index];
end;

procedure TVPBufferManager.SetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord; const Value: vpRealBufferItem); stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  ABuffer2.Dest^[Index]:=Value;
end;

function TVPBufferManager.GetAddressOfItem(const ABuffer: vpRealBuffer; const Index: LongWord): pvpRealBufferItem; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=@ABuffer2.Dest^[Index];
end;

{constructor TVPBufferManager.Create;
begin
  inherited Create;
  FBuffers:=TUIDArray.Create;
end;

destructor TVPBufferManager.Destroy;
begin
  FBuffers.Destroy;
  inherited Destroy;
end;

const
  Local_BM_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TVPBufferManager.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_BM_Version;
end;

function TVPBufferManager.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_BM_Version
    then Result:=IMInterface(IVPBufferManager(Self))
    else Result:=inherited Future(Version);
end;

function TVPBufferManager.NewBuffer: IVPBuffer; stdcall;
var
  ABuffer: TVPBuffer;
begin
  ABuffer:=TVPBuffer.Create();
  FBuffers.AddItem();
end;

function TVPBufferManager.GetBuffer(const ABuffer: vpRealBuffer): IVPBuffer; stdcall;}

{Allgemein}

function RealBufferMiddle(const ABuffer: vpRealBuffer): vpRealBufferItem;
var
  I    : Integer;
  ABuf2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=0.0;
  for I:=0 to ABuf2.Size-1 do Result+=ABuf2.Dest^[I];
  Result/=ABuf2.Size;
end;

end.

