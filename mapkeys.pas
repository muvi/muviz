unit MapKeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvFunc, GUIDop;

type
  TGUIDKey                        = class
  private
    FKey: TGUID;
  public
    constructor Create(AKey: TGUID);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property Key: TGUID read FKey;
  end;

  TStringKey                      = class (TInterfacedObject)
  private
    FKey: string;
  public
    constructor Create(AKey: string);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property Key: string read FKey;
  end;

  TIntegerKey                     = class
  private
    FKey: Integer;
  public
    constructor Create(AKey: Integer);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property Key: Integer read FKey;
  end;

function GetStringHash(AStr: string): PtrInt;
function GetPCharHash(AStr: PChar): PtrInt;
function GetGUIDHash(AGUID: TGUID): PtrInt;

implementation

{%REGION TGUIDKey}

constructor TGUIDKey.Create(AKey: TGUID);
begin
  inherited Create;
  FKey:=AKey;
end;

function TGUIDKey.Equals(AObject: TObject): Boolean;
begin
  Result:=AObject.InheritsFrom(TGUIDKey) and (TGUIDKey(AObject).Key=FKey);
end;

function TGUIDKey.GetHashCode: PtrInt;
begin
  Result:=GetGUIDHash(FKey);
end;

{%ENDREGION}
{%REGION TStringKey}

constructor TStringKey.Create(AKey: string);
begin
  inherited Create;
  FKey:=AKey;
end;

function TStringKey.Equals(AObject: TObject): Boolean;
begin
  Result:=AObject.InheritsFrom(TStringKey) and (TStringKey(AObject).Key=FKey);
end;

function TStringKey.GetHashCode: PtrInt;
begin
  Result:=GetStringHash(FKey);
end;

{%ENDREGION}
{%REGION TIntegerKey}

constructor TIntegerKey.Create(AKey: Integer);
begin
  inherited Create;
  FKey:=AKey;
end;

function TIntegerKey.Equals(AObject: TObject): Boolean;
begin
  Result:=AObject.InheritsFrom(TIntegerKey) and (TIntegerKey(AObject).Key=FKey);
end;

function TIntegerKey.GetHashCode: PtrInt;
begin
  Result:=FKey;
end;

{%ENDREGION}
{%REGIOn Misc}

function GetStringHash(AStr: string): PtrInt;
var
  I     : Integer;
  AShift: Cardinal;
const
  PtrIntBitSize = SizeOf(PtrInt)*8;
begin
  Result:=0;
  AShift:=0;
  for I:=1 to Length(AStr) do begin
    PtrUInt(Result):=PtrUInt(Result) xor (Ord(AStr[I]) shl AShift);
    AShift:=(AShift+8) mod PtrIntBitSize;
  end;
end;

function GetPCharHash(AStr: PChar): PtrInt;
var
  I, AShift: Integer;
const
  PtrIntBitSize = SizeOf(PtrInt)*8;
begin
  Result:=0;
  AShift:=0;
  I:=0;
  while AStr[I]<>#0 do begin
    Result:=Result xor (Ord(AStr[I]) shl AShift);
    AShift:=(AShift+8) mod PtrIntBitSize;
    Inc(I);
  end;
end;

function GetGUIDHash(AGUID: TGUID): PtrInt;
begin
  PtrUInt(Result):=AGUID.D1
    xor (AGUID.D2 or (AGUID.D3 shl 16))
    xor (AGUID.D4[0]
      or (AGUID.D4[1] shl 8)
      or (AGUID.D4[2] shl 16)
      or (AGUID.D4[3] shl 24))
    xor (AGUID.D4[4]
      or (AGUID.D4[5] shl 8)
      or (AGUID.D4[6] shl 16)
      or (AGUID.D4[7] shl 24));
end;

{%ENDREGION}

end.

