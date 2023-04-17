unit EqualityCounterList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynamicBase;

type
  TEqualityCounterList = class (TDynamicStructure)
  private
    FFirst: Pointer;
    FLast : Pointer;
    FPos  : Pointer;
    FSize : dsi;
  protected
  public
    constructor Create(const ASize: dsi = SizeOf(TObject); AOwner: TADObject = nil; const AMemoryManager: TManagedMemory = nil); override;
    destructor Destroy; override;
    procedure Insert(const AItem);
    {procedure Delete;
    procedure DeleteComplete;
    procedure ToFirst;
    procedure ToLast;
    procedure Next;
    procedure Prev;
    procedure Get(out AItem);
    procedure Clear;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    function IsEmpty: Boolean; }
  end;

implementation

{TEqualityCounterList}

type
  TECLData = record
    Next,Prev{,Higher,Lower,}: Pointer;
    Count                  : dsi;
  end;

const
  ECLDataSize = SizeOf(TECLData);

constructor TEqualityCounterList.Create(const ASize: dsi = SizeOf(TObject); AOwner: TADObject = nil; const AMemoryManager: TManagedMemory = nil);
begin
  inherited Create(ASize+ECLDataSize,AOwner,AMemoryManager);
  FSize:=ASize;
  FFirst:=nil;
  FLast:=nil;
  FPos:=nil;
end;

destructor TEqualityCounterList.Destroy;
begin
  //Clear;
  inherited Destroy;
end;

procedure TEqualityCounterList.Insert(const AItem);
begin

end;

end.

