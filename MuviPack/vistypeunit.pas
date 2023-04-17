unit VisTypeUnit;

{$mode objfpc}{$H+}

{.$DEFINE USE_DVM}
{$DEFINE USE_NET}
{.$DEFINE SAVE_NET}
{$DEFINE USE_VPL}
{$DEFINE USE_STR}
{.$DEFINE SAVE_STR}
{.$DEFINE USE_BUF}

interface

uses
  Classes, SysUtils, GR32, SpectrumData {$IFDEF USE_DVM}, DynamicVisMem {$ENDIF}
  {$IFDEF USE_NET}, NetType {$ENDIF}{$IFDEF USE_VPL}, StdDllInterfaces{$ENDIF}
  {$IFDEF USE_STR}, DllStr{$ENDIF} {.$IFDEF USE_BUF}, VPBuffers{.$ENDIF};

const
  SDKVersion     = 'VisLibrary v1.0 Win32';

type
  TVisParamType  = (vInteger = 0, vReal = 1, vString = 2, vColor = 3, vBoolean = 4, vBuffer = 5);
  TVisOutputType = (oCall = 0, oInteger = 1, oReal = 2, oString = 3, oColor = 4, oBoolean = 5, oBuffer = 6);
  TVisParams     = type Pointer;
  TVisWorkspace  = type Pointer;

  vpInt          = LongInt;
  vpReal         = Double;
  vpString       = ShortString;
  vpColor        = TColor32;
  vpBool         = Boolean;

  TVisOutputData = packed record
    D,V,P: Pointer;
  end;

  TVisOutputSet  = procedure (var bmp: TBitmap32; const Source: ISpectrumData; var Data: TVisOutputData; const Value); stdcall;
  TVisOutputGet  = procedure (const Data: TVisOutputData; var Dest); stdcall; //not supported yet

  TVisOutput     = record
    Data : TVisOutputData;
    DoSet: TVisOutputSet;
    DoGet: TVisOutputGet;
  end;

  TVisOutputs    = array of TVisOutput;

  TVisualisation = record
    VisID     : LongWord;
    C1, C2, C3: TColor32;
    VisParams : TVisParams;
    Workspace : TVisWorkspace;
    VisOutputs: TVisOutputs;
  end;

  TVisProc         = procedure(var bmp: TBitmap32; const Source: ISpectrumData; const Visualisation: TVisualisation; const Params; var Workspace); stdcall;
  TGetVisParams    = procedure(const Visualisation: TVisualisation; out Params);
  TRegisterVis     = procedure(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
  TRegisterVis2    = procedure(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil); stdcall;
  {$IFDEF USE_NET}
  TGetNetFunc      = function: INetwork; stdcall;
  {$ENDIF}
  {$IFDEF USE_VPL}
  TGUIAddFunc      = function (const AHeight: IEInt): IEGUIArea; stdcall;
  TGUIRemoveFunc   = procedure (var AArea: IEGUIArea); stdcall;
  TIControlAdd     = function (const ControlType: IEControlType): IEControl; stdcall;
  TIControlRemove  = procedure (var AControl: IEControl); stdcall;
  {$ENDIF}
  {$IFDEF USE_STR}
  TGetStrManager   = function : IStringManager; stdcall;
  {$ENDIF}
  {.$IFDEF USE_BUF}
  TGetBufferManager= function : IVPBufferManager; stdcall;
  {.$ENDIF}

var
  RegisterVis     : TRegisterVis;
  RegisterVis2    : TRegisterVis2;
  GetVisParams    : TGetVisParams;
  {$IFDEF USE_NET}
  {$IFDEF SAVE_NET}
  Network         : INetwork;
  {$ELSE}
  GetNet          : TGetNetFunc;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_VPL}
  SPAddGUI        : TGUIAddFunc;
  SPRemoveGUI     : TGUIRemoveFunc;
  SPNewIControl   : TIControlAdd;
  SpRemoveIControl: TIControlRemove;
  {$ENDIF}
  {$IFDEF USE_STR}
  {$IFDEF SAVE_STR}
  StrMan          : IStringManager;
  {$ELSE}
  GetStrManager   : TGetStrManager;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_BUF}
  BufferManager   : IVPBufferManager;
  {$ENDIF}

function AssignProcs(const Procs: array of Pointer): ShortString; stdcall;

implementation

function AssignProcs(const Procs: array of Pointer): ShortString; stdcall;
{$IFDEF SAVE_NET}
var
  GetNet: TGetNetFunc;
{$ENDIF}
{$IFDEF SAVE_STR}
var
  GetStrManager: TGetStrManager;
{$ENDIF}
{$IFDEF USE_BUF}
var
  GetBufferManager: TGetBufferManager;
{$ENDIF}
begin
  Result:=SDKVersion;
  RegisterVis:=TRegisterVis(Procs[0]);
  GetVisParams:=TGetVisParams(Procs[1]);
  {$IFDEF USE_DVM}
  SetMemFuncs(TMemProc(Procs[2]), TMemProc(Procs[3]), TSetLengthProc(Procs[4]));
  {$ENDIF}
  {$IFDEF USE_NET}
  GetNet:=TGetNetFunc(Procs[5]);
  {$IFDEF SAVE_NET}
  Network:=GetNet();
  {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_VPL}
  SPAddGUI:=TGUIAddFunc(Procs[6]);
  SPRemoveGUI:=TGUIRemoveFunc(Procs[7]);
  SPNewIControl:=TIControlAdd(Procs[9]);
  SPRemoveIControl:=TIControlRemove(Procs[10]);
  {$ENDIF}
  {$IFDEF USE_STR}
  GetStrManager:=TGetStrManager(Procs[8]);
  {$IFDEF SAVE_STR}
  StrMan:=GetStrManager();
  {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_BUF}
  GetBufferManager:=TGetBufferManager(Procs[11]);
  BufferManager:=GetBufferManager();
  {$ENDIF}
  RegisterVis2:=TRegisterVis2(Procs[12]);
end;

end.

