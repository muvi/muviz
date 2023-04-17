unit VisDrawType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisTypeUnit, GR32;

const
  MuviFileID            = 'Muvi ';
  MuviFileIDSize        = Length(MuviFileID);
  GenerelFileIDSize     = 10;
  KeyboardOFileID       = 'Key Layout';
  KeyboardFileID        = MuviFileID+KeyboardOFileID;
  KeyboardFileIDSize    = MuviFileIDSize+GenerelFileIDSize;
  GenerelMuviFileIDSize = MuviFileIDSize+GenerelFileIDSize;

type
  TVisWinType     = (vwLocal,vwNetwork);
  TVisWinEvent    = procedure of object;
  TVisKeyType     = (ktNothing,ktPreset,ktKeyboard);
  TVisKeyValue    = Integer;

  TVisKey         = record
    KeyType   : TVisKeyType;
    BackChange: Boolean;
    Value     : TVisKeyValue;
  end;

  TVisKeys        = array [Char] of TVisKey;

  TVisKeyboard    = record
    Name: string;
    Keys: TVisKeys;
  end;

  TVisKeyboards   = array of TVisKeyboard;
  PVisKeyboards   = ^TVisKeyboards;

  TVisID         = type UInt64;
  TVisIDStr      = array [1..8] of Char;

  TSVisParamDesc = record
    Name  :   ShortString;
    AType :  TVisParamType;
    Change: TVisProc;
  end;

  TSVisOutputDesc= record
    Name:  ShortString;
    AType: TVisOutputType;
  end;

  TVisOutputDesc = array of TSVisOutputDesc;
  TVisParamDesc  = array of TSVisParamDesc;

  TVisType       = record
    Name         : ShortString;
    VisParamSize : Cardinal;
    VisParamDesc : TVisParamDesc;
    VisProc      : TVisProc;
    WorkspaceSize: Cardinal;
    VisID        : TVisID;
    InitWorkspace: TVisProc;
    FreeWorkspace: TVisProc;
    FirstAction  : TVisProc;
    //ChangeParams : TChangeParams;
    InitialValues: Pointer;
    OutputDesc   : TVisOutputDesc;
    C1Desc       : ShortString;
    C2Desc       : ShortString;
    C3Desc       : ShortString;
  end;

  TTriggerInfo      = packed record
    Layer,Param: Integer;
  end;

  TVisInputPos      = type Pointer;

  TPresetOutputInfo = record
    Layer,Param: Integer;
    Offset     : TVisInputPos;
  end;

  TPresetVisOutputs = array of TPresetOutputInfo;

  TPresetVis        = record
    VisID     : Cardinal;
    C1,C2,C3  : TColor32;
    VisParams : TVisParams;
    VisOutputs: TPresetVisOutputs;
    DesignPos : TPoint;
  end;

  TVisParam         = type Pointer;
  TPresetLayers     = array of TPresetVis;
  TVisLayers        = array of TVisualisation;
  //TPresetTriggers  = array of TVisualisation;

  TVisComposition   = record
    //Name           : string;
    Layers,Triggers: TVisLayers;
    //WSize          : Cardinal;
  end;

  TPreset           = record
    Name           : string;
    Layers,Triggers: TPresetLayers;
    WSize          : Cardinal;
  end;

  TPresets          = array of TPreset;
  PPresets          = ^TPresets;
  PPreset           = ^TPreset;
  TVisSetFunc       = procedure (const APreset: Integer) of object;
  TVisGetFunc       = function: Integer of object;
  TWinRemoveProc    = procedure (const Index: Integer) of object;
  TSolutionProc     = procedure (const AHeight: Integer) of object;
  PVisualisation    = ^TVisualisation;
  PPresetVis        = ^TPresetVis;
  //PTrigger          = ^TTrigger;

  TGenerellVisOutput= record
    Dest: TVisParam;
    Trig: TTriggerInfo;
  end;

  TMuviFileID     = array [1..GenerelMuviFileIDSize] of Char;

  {TExecutedObject   = class
  protected
    procedure DoExecute; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;}

implementation

end.

