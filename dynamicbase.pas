unit DynamicBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MInterfacedObject;

type
  TSortFunc             = function (const ANew,AExisting): Boolean of object; stdcall;
  dsi                   = Cardinal;
  TPrereserveItem       = type Pointer;

  TAdvancedMemory       = class
  {strict }private
    FSize     : dsi;
    FConnected: TAdvancedMemory;
  {strict }protected
    procedure SetSize(const ASize: dsi = SizeOf(TObject)); virtual;
    property Connected: TAdvancedMemory read FConnected;
  protected
    procedure ConnectTo(const AMemory: TAdvancedMemory);
    procedure Disconnect;
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure Unuse(const AItem: TPrereserveItem); virtual; abstract;
    function Use: TPrereserveItem; virtual; abstract;
  published
    property Size: dsi read FSize write SetSize;
  end;

  TManagedMemory        = class of TAdvancedMemory;

  TPrereservedMemory    = class (TAdvancedMemory)
  private
    FCount      : dsi;
    FMem        : Pointer;
    FMemSize    : dsi;
    FFirstUnused: Pointer;
    procedure SetCount(const ACount: dsi);
  protected
    procedure SetSize(const ASize: dsi = SizeOf(TObject)); override;
  public
    constructor Create(const ASize: dsi = SizeOf(TObject)); override;
    constructor CreateC(const ACount: dsi; const ASize: dsi = SizeOf(TObject)); virtual;
    destructor Destroy; override;
    procedure Unuse(const AItem: TPrereserveItem); override;
    function Use: TPrereserveItem; override;
    procedure Reset;
    function Full: Boolean;
  published
    property Count: dsi read FCount write SetCount;
  end;

  TDynamicMemory        = class (TAdvancedMemory)
  public
    procedure Unuse(const AItem: TPrereserveItem); override;
    function Use: TPrereserveItem; override;
  end;

  TADObject             = class (TMInterfacedObject)
    FTopOfDestruction: Pointer;
  protected
    procedure AddDestroyObject(AObject: TObject);
  public
    constructor Create(AOwner: TADObject = nil); virtual; reintroduce;
    destructor Destroy; override;
  end;

  TDefaultMemory        = TDynamicMemory;
  TDynamicStructure     = class;
  TDynamicStructureClass= class of TDynamicStructure;

  TDynamicStructure     = class (TADObject)
  private
    FSize  : dsi;
    FMemory: TAdvancedMemory;
  protected
    procedure ConnectTo(AStructure: TDynamicStructure);
    function CreateConnected(const AClass: TDynamicStructureClass; const ASize: dsi = SizeOf(TObject)): TDynamicStructure;
  public
    constructor Create(const ASize: dsi = SizeOf(TObject); AOwner: TADObject = nil; const AMemoryManager: TManagedMemory = nil); virtual; reintroduce;
    destructor Destroy; override;
    property Memory: TAdvancedMemory read FMemory;
  published
    property Size: dsi read FSize;
  end;

  EDynamicStructureException = class (Exception)

  end;

implementation

{TAdvancedMemory}

constructor TAdvancedMemory.Create(const ASize: dsi = SizeOf(TObject));
begin
  inherited Create;
  FSize:=ASize;
  FConnected:=nil;
end;

destructor TAdvancedMemory.Destroy;
begin
  inherited Destroy;
end;

procedure TAdvancedMemory.SetSize(const ASize: dsi = SizeOf(TObject));
begin
  FSize:=ASize;
end;

procedure TAdvancedMemory.ConnectTo(const AMemory: TAdvancedMemory);
begin
  if AMemory.ClassType<>ClassType then raise EDynamicStructureException.Create('Buffers can only be Connected if they have the same classtype');
  FConnected:=AMemory;
end;

procedure TAdvancedMemory.Disconnect;
begin
  FConnected:=nil;
end;

{TPrereservedMemory}

type
  TPrereserveItemData = record
    Next,Prev: Pointer;  //Prev ist nur nach dem Einfügen in FFirstUsed definiert
  end;
  PPrereserveItemData = ^TPrereserveItemData;

const
  PrereserveItemDataSize = SizeOf(TPrereserveItemData);

constructor TPrereservedMemory.Create(const ASize: dsi = SizeOf(TObject));
begin
  inherited Create(ASize);
  FCount:=0;
  FMem:=nil;
  FMemSize:=0;
  FFirstUnused:=nil;
end;

constructor TPrereservedMemory.CreateC(const ACount: dsi; const ASize: dsi = SizeOf(TObject));
begin
  inherited Create(ASize);
  FCount:=ACount;
  FMemSize:=ACount*(ASize+PrereserveItemDataSize);
  GetMem(FMem,FMemSize);
  Reset;
end;

destructor TPrereservedMemory.Destroy;
begin
  if FMemSize>0 then FreeMem(FMem);
  inherited Destroy;
end;

procedure TPrereservedMemory.SetCount(const ACount: dsi);
begin
  if FMemSize>0 then FreeMem(FMem,FMemSize);
  FCount:=ACount;
  FMemSize:=FCount*(Size+PrereserveItemDataSize);
  GetMem(FMem,FMemSize);
  Reset;
  if Connected<>nil then TPrereservedMemory(Connected).SetCount(ACount);
end;

procedure TPrereservedMemory.SetSize(const ASize: dsi = SizeOf(TObject));
begin
  inherited SetSize(ASize);
  if FMemSize>0 then FreeMem(FMem,FMemSize);
  FMemSize:=FCount*(Size+PrereserveItemDataSize);
  GetMem(FMem,FMemSize);
  Reset;
end;

procedure TPrereservedMemory.Unuse(const AItem: TPrereserveItem);
var
  AItem2    : Pointer;
  AItem2Data: PPrereserveItemData absolute AItem2;
begin
  AItem2:=AItem-PrereserveItemDataSize;
  AItem2Data^.Next:=FFirstUnused;
  FFirstUnused:=AItem2;
end;

function TPrereservedMemory.Use: TPrereserveItem;
//var
  //AItemData   : PPrereserveItemData absolute FFirstUnused;
begin
  Result:=FFirstUnused+PrereserveItemDataSize;
  FFirstUnused:=PPrereserveItemData(FFirstUnused)^.Next;
end;

procedure TPrereservedMemory.Reset;
var
  I           : Integer;
  AItemPointer: Pointer;
  AItem       : PPrereserveItemData absolute AItemPointer;
begin
  FFirstUnused:=nil;
  AItemPointer:=FMem;
  for I:=0 to FCount-1 do begin
    AItem^.Next:=FFirstUnused;
    FFirstUnused:=AItemPointer;
    AItemPointer+=Size+PrereserveItemDataSize;
  end;
end;

function TPrereservedMemory.Full: Boolean;
begin
  Result:=(FFirstUnused=nil);
end;

{TDynamicMemory}

procedure TDynamicMemory.Unuse(const AItem: TPrereserveItem);
begin
  FreeMem(AItem,Size);
end;

function TDynamicMemory.Use: TPrereserveItem;
begin
  GetMem(Result,Size);
end;

{TADObject}

type
  TADObjectContainer    = record
    Next   : Pointer;
    Content: TObject;
  end;
  PADObjectContainer    = ^TADObjectContainer;

const
  TADObjectContainerSize= SizeOf(TADObjectContainer);

constructor TADObject.Create(AOwner: TADObject = nil);
begin
  inherited Create;
  FTopOfDestruction:=nil;
  if AOwner<>nil then AOwner.AddDestroyObject(Self);
end;

destructor TADObject.Destroy;
var
  ACPointer,AOld: Pointer;
  AContainer    : PADObjectContainer absolute ACPointer;
begin
  ACPointer:=FTopOfDestruction;
  while ACPointer<>nil do begin
    AContainer^.Content.Destroy;
    AOld:=ACPointer;
    ACPointer:=AContainer^.Next;
    FreeMem(AOld,TADObjectContainerSize);
  end;
  inherited Destroy;
end;

procedure TADObject.AddDestroyObject(AObject: TObject);
var
  ACPointer : Pointer;
  AContainer: PADObjectContainer absolute ACPointer;
begin
  GetMem(ACPointer,TADObjectContainerSize);
  with AContainer^ do begin
    Next:=FTopOfDestruction;
    Content:=AObject;
  end;
  FTopOfDestruction:=ACPointer;
end;

{TDynamicStructure}

constructor TDynamicStructure.Create(const ASize: dsi = SizeOf(TObject); AOwner: TADObject = nil; const AMemoryManager: TManagedMemory = nil);
begin
  inherited Create(AOwner);
  FSize:=ASize;
  if AMemoryManager=nil
    then FMemory:=TDefaultMemory.Create(FSize)
    else FMemory:=AMemoryManager.Create(FSize);
end;

destructor TDynamicStructure.Destroy;
begin
  FMemory.Destroy;
  inherited Destroy;
end;

procedure TDynamicStructure.ConnectTo(AStructure: TDynamicStructure);
begin
  FMemory.ConnectTo(AStructure.Memory);
end;

function TDynamicStructure.CreateConnected(const AClass: TDynamicStructureClass; const ASize: dsi = SizeOf(TObject)): TDynamicStructure;
begin
  Result:=AClass.Create(ASize,Self,TManagedMemory(FMemory.ClassType));
  FMemory.ConnectTo(Result.Memory);
end;

end.

