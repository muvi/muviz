unit StdParamTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, Math, MStrings, TVSPSources, Doors,
  StdPermissions;

const
  vCall    = $0000000000000000 {0 Byte Value, 0 Sources};
  vInteger = $0400000000000001 {4 Byte Value, 0 Sources};
  vFloat   = $0800000000000002 {8 Byte Value, 0 Sources};
  vColor   = $0400000000000004 {4 Byte Value, 0 Sources};
  vBoolean = $0100000000000005 {1 Byte Value, 0 Sources};
  vBuffer  = $0001000000000006 {0 Byte Value, 1 Source};
  vString  = $0001000000000007 {0 Byte Value, 1 Source};
  vPreset  = $1000000000000008 {16 Byte Value, 0 Sources};
  vPointer = $2404000000000009 {36 Byte Value, 4 Sources};

type
  TVCall              = packed record end;
  TVInteger           = LongInt;
  TVFloat             = Double;
  TVColor             = type LongWord;
  TVBoolean           = Boolean;

  IVFloatBuffer       = interface
    function GetItem(Index: LongWord): TVFloat; cdecl;
    procedure SetItem(Index: LongWord; Value: TVFloat); cdecl;
    function GetSize: LongWord; cdecl;
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
    property Source: ITVSPSource read GetSource;
  end;

  TVBuffer            = IVFloatBuffer;
  TVString            = IString;
  TVPreset            = TPPresetID;
  TVLimitedInteger    = TVInteger;
  TVLimitedFloat      = TVFloat;
  TPointerSide        = packed record
    Preset: TPParamID;
    Param : TPParamID;
  end;

  TVPointer           = packed record
    Output         : TPointerSide;
    Input          : TPointerSide;
    InversePriority: LongInt;
  end;

  IPCall              = interface (IPParam)
    procedure &Set; cdecl;
    procedure &Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
  end;

  IPInteger           = interface (IPParam)
    procedure &Set(Value: TVInteger); cdecl;
    procedure &Set(Value: TVInteger; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVInteger; cdecl;
    function GetMin: TVInteger; cdecl;
    function GetMax: TVInteger; cdecl;
    property Max: TVInteger read GetMax;
    property Min: TVInteger read GetMin;
    property Value: TVInteger read Get write &Set;
  end;

  IPFloat            = interface (IPParam)
    procedure &Set(Value: TVFloat); cdecl;
    procedure &Set(Value: TVFloat; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVFloat; cdecl;
    function GetMin: TVFloat; cdecl;
    function GetMax: TVFloat; cdecl;
    property Max: TVFloat read GetMax;
    property Min: TVFloat read GetMin;
    property Value: TVFloat read Get write &Set;
  end;

  IPColor             = interface (IPParam)
    procedure &Set(Value: TVColor); cdecl;
    procedure &Set(Value: TVColor; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVColor; cdecl;
    property Value: TVColor read Get write &Set;
  end;

  IPBoolean           = interface (IPParam)
    procedure &Set(Value: TVBoolean); cdecl;
    procedure &Set(Value: TVBoolean; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVBoolean; cdecl;
    property Value: TVBoolean read Get write &Set;
  end;

  IPBuffer            = interface (IPParam)
    procedure &Set(Value: TVBuffer); cdecl;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVBuffer; cdecl;
    property Value: TVBuffer read Get write &Set;
  end;

  IPString            = interface (IPParam)
    procedure &Set(Value: TVString); cdecl;
    procedure &Set(Value: TVString; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVString; cdecl;
    property Value: TVString read Get write &Set;
  end;

  IPPreset            = interface (IPParam)
    procedure AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
    procedure RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
    procedure &Set(Value: TVPreset); cdecl;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVPreset; cdecl;
    function GetExecutedValue: TVPreset; cdecl;
    function IsForceAlive: Boolean; cdecl;
    property ForceAlive: Boolean read isForceAlive;
    property Value: TVPreset read Get write &Set;
    property ExecutedValue: TVPreset read GetExecutedValue;
  end;

  IPPointer           = interface (IPParam)
    procedure &Set(Value: TVPointer); cdecl;
    procedure &Set(Value: TVPointer; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVPointer; cdecl;
    property Value: TVPointer read Get write &Set;
  end;

  IChangedCall        = IChangedValue;

  IChangedInteger     = interface (IChangedValue)
    function Get: TVInteger; cdecl;
    property Value: TVInteger read Get;
  end;

  IChangedFloat       = interface (IChangedValue)
    function Get: TVFloat; cdecl;
    property Value: TVFloat read Get;
  end;

  IChangedColor       = interface (IChangedValue)
    function Get: TVColor; cdecl;
    property Value: TVColor read Get;
  end;

  IChangedBoolean     = interface (IChangedValue)
    function Get: TVBoolean; cdecl;
    property Value: TVBoolean read Get;
  end;

  IChangedBuffer      = interface (IChangedValue)
    function Get: TVBuffer; cdecl;
    property Value: TVBuffer read Get;
  end;

  IChangedString      = interface (IChangedValue)
    function Get: TVString; cdecl;
    property Value: TVString read Get;
  end;

  IChangedPreset      = interface (IChangedValue)
    function GetExecutedValue: TVPreset; cdecl;
    function Get: TVPreset; cdecl;
    property ExecutedValue: TVPreset read GetExecutedValue;
    property Value: TVPreset read Get;
  end;

  IChangedPointer     = interface (IChangedValue)
    function Get: TVPointer; cdecl;
    property Value: TVPointer read Get;
  end;

var
  DEFAULTCALL       : TVCall        = ();
  DEFAULTINTEGER    : TVInteger     = 0;
  DEFAULTFLOAT      : TVFloat       = Nan;
  DEFAULTCOLOR      : TVColor       = $00000000;
  DEFAULTBOOLEAN    : TVBoolean     = false;
  DEFAULTPRESET     : TVPreset      = '{00000000-0000-0000-0000-000000000000}';

function DEFAULTSTRING: TVString; inline;
function DEFAULTBUFFER: TVBuffer; inline;
function DEFAULTPOINTER: TVPointer; inline;

//---usefull simplicity operators, helpers, types, functions, ...---

function EMPTYPOINTERSIDE: TPointerSide; inline;

operator = (const m1: TVPointer; const m2: TVPointer): Boolean; inline;
operator = (const m1: TPointerSide; const m2: TPointerSide): Boolean; inline;

operator := (m2: IPInteger): TVInteger;
operator := (m2: IPFloat): TVFloat;
operator := (m2: IPColor): TVColor;
operator := (m2: IPBoolean): TVBoolean;
operator := (m2: IPBuffer): TVBuffer;
operator := (m2: IPString): TVString;
operator := (m2: IPPointer): TVPointer;
//operator := (m2: IPPreset): TVPreset;

operator >< (m1: IPInteger; m2: TVInteger): TVInteger;
operator >< (m1: IPFloat; m2: TVFloat): TVFloat;
operator >< (m1: IPColor; m2: TVColor): TVColor;
operator >< (m1: IPBoolean; m2: TVBoolean): TVBoolean;
operator >< (m1: IPBuffer; m2: TVBuffer): TVBuffer;
operator >< (m1: IPString; m2: TVString): TVString;
operator >< (m1: IPPointer; m2: TVPointer): TVPointer;
operator >< (m1: IPPreset; m2: TVPreset): TVPreset;

type
  TVPointerConnectionMode = (ptrNone, ptrInput, ptrOutput, ptrNormal, ptrBridge);

function ConnectionMode(const APtr: TVPointer): TVPointerConnectionMode;
function Connected(const APtr: TVPointer): Boolean;
function Connected(const ASide: TPointerSide): Boolean; inline;
function PointerSide(APreset, AParam: TPParamID): TPointerSide; inline;

var
  EmptyBuffer: IVFloatBuffer = nil;

implementation

{%REGION default value generators}

function DEFAULTSTRING: TVString; inline;
begin
  Result:=EmptyString;
end;

function DEFAULTBUFFER: TVBuffer; inline;
begin
  Result:=EmptyBuffer;
end;

function EMPTYPOINTERSIDE: TPointerSide; inline;
begin
  with Result do begin
    Preset.Name:=EmptyString;
    Preset.&Type:=vPreset;
    Param.Name:=EmptyString;
    Param.&Type:=vCall;
  end;
end;

function DEFAULTPOINTER: TVPointer; inline;
begin
  with Result do begin
    Output:=EMPTYPOINTERSIDE;
    Input:=EMPTYPOINTERSIDE;
    InversePriority:=0;
  end;
end;

{%ENDREGION}
{%REGION operators}

operator = (const m1: TPointerSide; const m2: TPointerSide): Boolean; inline;
begin
  Result:=(m1.Param = m2.Param)
      and (m1.Preset = m2.Preset);
end;

operator = (const m1: TVPointer; const m2: TVPointer): Boolean; inline;
begin
  Result:=(m1.Input = m2.Input)
      and (m1.Output = m2.Output)
      //to make shure that the param is attached again, if the order is changed
      and (m1.InversePriority = m2.InversePriority);
end;

operator := (m2: IPInteger): TVInteger;
begin
  Result:=m2.Get;
end;

operator := (m2: IPFloat): TVFloat;
begin
  Result:=m2.Get;
end;

operator := (m2: IPColor): TVColor;
begin
  Result:=m2.Get;
end;

operator := (m2: IPBoolean): TVBoolean;
begin
  Result:=m2.Get;
end;

operator := (m2: IPBuffer): TVBuffer;
begin
  Result:=m2.Get;
end;

operator := (m2: IPString): TVString;
begin
  Result:=m2.Get;
end;

operator := (m2: IPPointer): TVPointer;
begin
  Result:=m2.Get;
end;

{operator := (m2: IPPreset): TVPreset;
begin
  Result:=m2.Get;
end;}

operator >< (m1: IPInteger; m2: TVInteger): TVInteger;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPFloat; m2: TVFloat): TVFloat;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPColor; m2: TVColor): TVColor;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPBoolean; m2: TVBoolean): TVBoolean;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPBuffer; m2: TVBuffer): TVBuffer;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPString; m2: TVString): TVString;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPPointer; m2: TVPointer): TVPointer;
begin
  m1.&Set(m2);
  Result:=m2;
end;

operator >< (m1: IPPreset; m2: TVPreset): TVPreset;
begin
  m1.&Set(m2);
  Result:=m2;
end;

{%ENDREGION}
{%REGION Misc}

function ConnectionMode(const APtr: TVPointer): TVPointerConnectionMode;
begin
  if (APtr.Input.Param.Name='') or (APtr.Output.Param.Name='') then begin
    Result:=ptrNone;
    exit;
  end;
  if APtr.Input.Preset.Name='' then begin
    if APtr.Output.Preset.Name=''
      then Result:=ptrBridge
      else Result:=ptrOutput;
  end else begin
    if APtr.Output.Preset.Name=''
      then Result:=ptrInput
      else Result:=ptrNormal;
  end;
end;

function Connected(const ASide: TPointerSide): Boolean; inline;
begin
  Result:=ASide.Param.Name <> '';
end;

function Connected(const APtr: TVPointer): Boolean;
begin
  Result:=Connected(APtr.Input) and Connected(APtr.Output);
end;

function PointerSide(APreset, AParam: TPParamID): TPointerSide; inline;
begin
  Result.Preset:=APreset;
  Result.Param:=AParam;
end;

{%ENDREGION}

end.

