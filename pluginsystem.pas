unit PluginSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, CommandListImpl, PluginLoadUnit,
  MInterfacedObject, AdvType, Dialogs, dynlibs;

type
  TPluginLibData          = record
    Name       : ShortString;
    Version    : MVVersion;
    Description: ShortString;
    FileName   : ShortString;
  end;

  TPluginSystem           = class (TMInterfacedObject,IMPluginSystem)
  private
    FPlugins    : TCommandList;
    FAutoInit   : Boolean;
    FLibrarys   : TCommandList;
  protected
    {function QueryInterface(const iid: tguid; out obj) : LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;}
    procedure UnloadPlugins;
    function GetPluginLibData(const Index: Integer): TPluginLibData;
    function GetPluginLibCount: Integer;
    procedure DoFree; virtual;
  public
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
    function GetPlugin(const ID: MVPluginID): IMPlugin; stdcall;
    procedure InstallPlugin(Plugin: IMPlugin); stdcall;
    procedure InitPlugins;
    procedure UninstallPlugins;
    function LoadPluginLibrary(const FileName: string): TLoadFileResult;
    function LoadPluginLibraryE(const FileName: string): TLoadFileResult;
    function LoadPluginLibrarys(const FileName: string): TLoadFileResult;
    procedure SavePluginLibrarys(const FileName: string);
    property PluginLibCount: Integer read GetPluginLibCount;
    property PluginLibData[const Index: Integer]: TPluginLibData read GetPluginLibData;
  published
    property AutoInit: Boolean read FAutoInit write FAutoInit;
  end;

const
  lplInitFailed       = 240;
  lplNoValidEntry     = 241;

implementation

uses AdvFunc;

{TPluginSystem}

type
  TInternalPluginLibInfo = record
    FileName   : ShortString;
    Version    : MVVersion;
    Description: ShortString;
    LoadData   : TLibHandle;
  end;

  PInternalPluginLibInfo = ^TInternalPluginLibInfo;

const
  LibListHeader          = '[Muvi Plugins]';
  CommentChar            = ';';

constructor TPluginSystem.Create;
const
  InvalidPlugin: IMPlugin= nil;
var
  AInvalidInternalPluginLibInfo: TInternalPluginLibInfo;
begin
  inherited Create;
  FPlugins:=TCommandList.Create(SizeOf(IMPlugin),InvalidPlugin);
  FAutoInit:=false;
  with AInvalidInternalPluginLibInfo do begin
    FileName:='';
    with Version do begin
      Version:=0;
      MainVersion:=0;
      SubVersion:=0;
    end;
    Description:='Invalid Library';
    LoadData:=InvalidLoadedLibData;
  end;
  FLibrarys:=TCommandList.Create(SizeOf(TInternalPluginLibInfo),AInvalidInternalPluginLibInfo);
end;

destructor TPluginSystem.Destroy;
begin
  UninstallPlugins;
  UnloadPlugins;
  FPlugins.Destroy;
  FLibrarys.Destroy;
  DoFree;
  inherited Destroy;
end;

procedure TPluginSystem.DoFree;
begin
  //do nothing
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TPluginSystem.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TPluginSystem.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IMPluginSystem(Self))
    else Result:=inherited Future(Version);
end;

function TPluginSystem.GetPlugin(const ID: MVPluginID): IMPlugin; stdcall;
begin
  FPlugins.GetCommand(ID,Result);
end;

procedure TPluginSystem.InstallPlugin(Plugin: IMPlugin); stdcall;
begin
  FPlugins.AddCommand(Plugin.GetID,Plugin);
  if FAutoInit then Plugin.Init;
end;

procedure TPluginSystem.InitPlugins;
var
  I      : Integer;
  APlugin: IMPlugin;
begin
  for I:=1 to FPlugins.Count-1 do begin;
    FPlugins.GetData(I,APlugin);
    APlugin.Init;
  end;
  FAutoInit:=true;
end;

procedure TPluginSystem.UninstallPlugins;
var
  I      : Integer;
  APlugin: IMPlugin;
begin
  for I:=1 to FPlugins.Count-1 do begin;
    FPlugins.GetData(I,APlugin);
    APlugin.Done;
    APlugin:=nil;
  end;
  FPlugins.ClearCommands;
end;

function TPluginSystem.LoadPluginLibrary(const FileName: string): TLoadFileResult;
var
  AResult: TPluginLoadResult;
  AInfo  : TInternalPluginLibInfo;
begin
  AResult:=PluginLoadUnit.LoadPluginLib(FileName,Self);
  if not AResult.PluginInfo.Supported then begin
    Result:=lplNotSupported;
    exit;
  end;
  if FLibrarys.CommandExists(AResult.PluginInfo.Name) then begin
    Result:=lplAlreadyOpen;
    exit;
  end;
  if not PluginLoadUnit.InitPluginLib(FileName,Self,AResult.LoadData) then begin
    Result:=lplInitFailed;
    exit;
  end;
  Result:=lplOK;
  AInfo.Description:=AResult.PluginInfo.Description;
  AInfo.FileName:=FileName;
  AInfo.LoadData:=AResult.LoadData;
  AInfo.Version:=AResult.PluginInfo.Version;
  FLibrarys.AddCommand(AResult.PluginInfo.Name,AInfo);
end;

function TPluginSystem.LoadPluginLibraryE(const FileName: string): TLoadFileResult;
var
  AExt: string;
begin
  AExt:=LowerCase(ExtractFileExt(FileName));
  if (AExt=LibraryExt) or (AExt=MuviLibraryExt)
    then Result:=LoadPluginLibrary(FileName)
    else Result:=lplUnknownExtension;
end;

function TPluginSystem.LoadPluginLibrarys(const FileName: string): TLoadFileResult;

  procedure LoadLibList;
  var
    F      : System.Text;
    TempStr: string;
  begin
    AssignFile(F,FileName);
    Reset(F);
    ReadLn(F,TempStr);
    if TempStr<>LibListHeader then begin
      Result:=lplInvalidHeader;
      exit;
    end;
    Result:=lplNoValidEntry;
    repeat
      ReadLn(F,TempStr);
      if TempStr<>'' then if TempStr[1]<>CommentChar then begin
        if not lplSucceeded(Result)
          then Result:=LoadPluginLibrary(TempStr)
          else LoadPluginLibrary(TempStr);
      end;
    until EOF(F);
    CloseFile(F);
  end;

var
  AExt: string;
begin
  AExt:=LowerCase(ExtractFileExt(FileName));
  if (AExt=LibraryExt) or (AExt=MuviLibraryExt)
    then Result:=LoadPluginLibrary(FileName)
    else LoadLibList;
end;

procedure TPluginSystem.SavePluginLibrarys(const FileName: string);
var
  F: Text;
  I: Integer;
begin
  AssignFile(F,FileName);
  Rewrite(F);
  WriteLn(F,LibListHeader);
  for I:=1 to FLibrarys.Count-1
    do WriteLn(F,PInternalPluginLibInfo(FLibrarys.Data[I])^.FileName);
  CloseFile(F);
end;

procedure TPluginSystem.UnloadPlugins;
var
  I    : Integer;
  AInfo: PInternalPluginLibInfo;
begin
  for I:=1 to FLibrarys.Count-1 do begin
    AInfo:=FLibrarys.Data[I];
    PluginLoadUnit.FreePluginLib(AInfo^.FileName,AInfo^.LoadData);
  end;
  FLibrarys.ClearCommands;
end;

function TPluginSystem.GetPluginLibData(const Index: Integer): TPluginLibData;
var
  AInfo: PInternalPluginLibInfo;
begin
  AInfo:=FLibrarys.Data[Index];
  Result.Version:=AInfo^.Version;
  Result.FileName:=AInfo^.FileName;
  Result.Description:=AInfo^.Description;
  Result.Name:=FLibrarys.Items[Index];
end;

function TPluginSystem.GetPluginLibCount: Integer;
begin
  Result:=FLibrarys.Count;
end;

end.

