unit MPluginType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, SpectrumData, DllStr, VisType, MNetType,
  StdDllInterfaces, VPBuffers;

type
  IMPluginSystem2 = interface (IMPluginSystem)
    ['{79C3B46F-1032-4459-9C42-C084C232B55E}']
    function InMVSet(ASet: MVSet; AItem: MVSetItem): Boolean; stdcall;
    procedure IncludeMVSet(var ASet: MVSet; AItems: MVSet); stdcall;
    procedure ExcludeMVSet(var ASet: MVSet; AItems: MVSet); stdcall;
    function SupportsParam(AParamType: TVisParamType): Boolean; stdcall;
    function SupportsOutput(AOutputType: TVisOutputType): Boolean; stdcall;

    function GetStringManager: IStringManager; stdcall;
    function GetNet: IMuviClient; stdcall;
    function GetBufferManager: IVPBufferManager; stdcall;

    procedure RegisterVis(const AName: ShortString; const ID; const AVisProc: TVisProc; const AParamNames: array of ShortString; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of ShortString; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
    function AddGUIArea(const AHeight: IEInt): IEGUIArea; stdcall;
    procedure GetMemory(var P: Pointer; const Size: LongWord); stdcall;
    procedure FreeMemory(var P: Pointer; const Size: LongWord); stdcall;

    property BufferManager: IVPBufferManager read GetBufferManager;
    property StringManager: IStringManager read GetStringManager;
    property Net: IMuviClient read GetNet;
  end;

implementation

end.

