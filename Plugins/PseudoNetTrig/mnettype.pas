unit MNetType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AuthNetType, DllStr, PluginType, VisWinType;

//type
  {IMNetClient = interface (IAuthClient)
    ['{22DFE83A-8F10-4A18-9D84-6DCEAF4ECC2A}']
  end;}

{const
  rutMuvi = 2;}

type
  TMuviBlockID      = 0..63;
  TMuviBlockIDs     = packed set of TMuviBlockID;
  IMuviClient       = interface;
  IMuviNetData      = interface;
  TMuviBlockParser  = procedure (ANet: IMuviClient; ABlock: DString; AMuvi: IMuviNetData; const ABlockID: TMuviBlockID) of object; stdcall;
  TMuviBlockSender  = function (ANet: IMuviClient; const ABlockID: TMuviBlockID): DString of object; stdcall;

  TMuviBlockListItem= packed record
    Parser: TMuviBlockParser;
    Sender: TMuviBlockSender;
  end;

  IBlockParserList  = interface
    ['{50DCCF49-AEF8-4A69-BC90-3D7364C4BCE8}']
    procedure AddParser(const ABlockName: ShortString; const AParser: TMuviBlockParser; const ASender: TMuviBlockSender); stdcall;
    function GetParser(const ABlockName: ShortString): TMuviBlockListItem; stdcall; overload;
    function GetParser(const ABlockName: ShortString; out AID: TMuviBlockID): TMuviBlockListItem; stdcall; overload;
    function GetParserID(const ABlockName: ShortString): TMuviBlockID;
    procedure RemoveParser(const AParser: ShortString); stdcall;
  end;

  IMuviVisWin       = interface (IMInterface)
    ['{08767D24-852A-4F1F-8DA3-655DA5E2CB54}']
    function GetCaption: ShortString; stdcall;
    function GetPreset: LongWord; stdcall;
    function GetResolution: TMuviPoint; stdcall;
    function GetVisWinID: LongWord; stdcall;
    procedure SetCaption(const ACaption: ShortString); stdcall;
    procedure SetPreset(const APreset: LongWord); stdcall;
    procedure SetResolution(const ASolution: TMuviPoint); stdcall;

    function ChangePreset(const APreset: LongWord): Boolean; stdcall;

    property Caption: ShortString read GetCaption write SetCaption;
    property Preset: LongWord read GetPreset write SetPreset;
    property Resolution: TMuviPoint read GetResolution write SetResolution;
    property VisWinID: LongWord read GetVisWinID;
  end;

  IMuviNetData      = interface (IMInterface)
    ['{DFD4A3A2-1EDB-4BDC-89A6-2A1D334D8405}']
    function GetID: LongWord; stdcall;
    function GetPresetCount: LongInt; stdcall;
    function GetPreset(const AIndex: LongInt): ShortString; stdcall;
    function GetVisWinCount: LongInt; stdcall;
    function GetVisWin(const AIndex: LongInt): IMuviVisWin; stdcall;
    procedure SetPresetCount(const ACount: LongInt); stdcall;
    procedure SetPreset(const AIndex: LongInt; const AValue: ShortString); stdcall;

    procedure GetMuviData(const ID: Int64; out AData); stdcall;
    procedure SetMuviData(const ID: Int64; const AData); stdcall;
    function MuviDataPtr(const ID: Int64): Pointer; stdcall;

    function SupportsBlock(const BlockID: TMuviBlockID): Boolean; stdcall;

    property ID: LongWord read GetID;
    property PresetCount: LongInt read GetPresetCount write SetPresetCount;
    property Presets[const AIndex: LongInt]: ShortString read GetPreset write SetPreset;
    property VisWinCount: LongInt read GetVisWinCount;
    property VisWins[const AIndex: LongInt]: IMuviVisWin read GetVisWin;
  end;

  TMuviDataIterator = LongInt;

  IMuviClient       = interface (IAuthClient)
    ['{EF775C41-E7E5-4F8F-9BC2-C7BFAB938210}']
    function GetBlockParsers: IBlockParserList; stdcall;
    function GetMuviNetData(const AID: LongInt): IMuviNetData; stdcall;
    function GetcidBlockIDsChanged: LongInt; stdcall;
    function GetcidMuviNetDataAdded: LongInt; stdcall;
    function GetcidMuviNetDataRemoved: LongInt; stdcall;
    function GetPresetBlockID: TMuviBlockID; stdcall;
    function GetVisWinBlockID: TMuviBlockID; stdcall;

    function AddMuviData(const ASize: LongInt; const InitialValues): Int64; stdcall;
    function IterFirstMuvi: TMuviDataIterator; stdcall;
    function IterNextMuvi(var Iterator: TMuviDataIterator): IMuviNetData; stdcall;
    procedure SendBlockDataUpdate(const UpdatedBlocks: TMuviBlockIDs); stdcall; overload;
    procedure SendBlockDataUpdate; stdcall; overload;

    property BlockParsers: IBlockParserList read GetBlockParsers;
    property cidBlockIDsChanged: LongInt read GetcidBlockIDsChanged;
    property cidMuviNetDataAdded: LongInt read GetcidMuviNetDataAdded;
    property cidMuviNetDataRemoved: LongInt read GetcidMuviNetDataRemoved;
    property Muvis[const AID: LongInt]: IMuviNetData read GetMuviNetData;
    property PresetBlockID: TMuviBlockID read GetPresetBlockID;
    property VisWinBlockID: TMuviBlockID read GetVisWinBlockID;
  end;

const
  bpPresets         = 'presets';
  bpVisWins         = 'viswins';

  urSetPreset       = 'viswins_setpreset';

implementation

end.

