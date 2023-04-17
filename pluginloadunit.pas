unit PluginLoadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, PluginType, AdvFunc;

const
  LibraryExt          = '.dll';
  MuviLibraryExt      = '.vll';

type
  TPluginLoadResult   = record
    PluginInfo: TPluginLibInfo;
    LoadData  : TLibHandle;
  end;
  TInstallCheck       = function (const APluginName: string): Boolean;

const
  InvalidLoadedLibData: TLibHandle = 0;

function LoadPluginLib(const AFileName: string; const APluginSystem: IMPluginSystem): TPluginLoadResult;
function InitPluginLib(const AFileName: string; const APluginSystem: IMPluginSystem; const ALoadData: TLibHandle): Boolean;
procedure FreePluginLib(const AFileName: string; const ALoadData: TLibHandle);

implementation

const
  LibGetDataName = 'LibGetData';
  InitPluginName = 'InitPlugin';
  PluginDoneName = 'PluginDone';

type
  TLibGetData    = function (APluginSystem: IMPluginSystem): TPluginLibInfo; stdcall;
  TInitPlugin    = procedure (APluginSystem: IMPluginSystem); stdcall;
  TPluginDone    = procedure; stdcall;

function LoadPluginLib(const AFileName: string; const APluginSystem: IMPluginSystem): TPluginLoadResult;
var
  ALibGetData: TLibGetData;
begin
  Result.LoadData:=LoadLibrary(PAnsiChar(AFileName));
  Result.PluginInfo.Supported:=(Result.LoadData<>0);
  if not Result.PluginInfo.Supported then begin
    FreeLibrary(Result.LoadData);
    exit;
  end;
  ALibGetData:=TLibGetData(GetProcAddress(Result.LoadData,LibGetDataName));
  if ALibGetData=nil then begin
    FreeLibrary(Result.LoadData);
    Result.PluginInfo.Supported:=false;
    exit;
  end;
  Result.PluginInfo:=ALibGetData(APluginSystem);
  if not Result.PluginInfo.Supported then begin
    FreeLibrary(Result.LoadData);
    exit;
  end;
end;

function InitPluginLib(const AFileName: string; const APluginSystem: IMPluginSystem; const ALoadData: TLibHandle): Boolean;
var
  AInitPlugin: TInitPlugin;
begin
  AInitPlugin:=TInitPlugin(GetProcAddress(ALoadData,InitPluginName));
  if AInitPlugin=nil then begin
    Result:=false;
    FreeLibrary(ALoadData);
    exit;
  end;
  Result:=true;
  AInitPlugin(APluginSystem);
end;

procedure FreePluginLib(const AFileName: string; const ALoadData: TLibHandle);
var
  APluginDone: TPluginDone;
begin
  APluginDone:=TPluginDone(GetProcAddress(ALoadData,PluginDoneName));
  if APluginDone<>nil then APluginDone;
  FreeLibrary(ALoadData);
end;

end.

