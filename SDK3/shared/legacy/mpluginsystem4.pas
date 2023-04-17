unit MPluginSystem4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPluginSystem, MPluginType4, VisType2, MStrings,
  ParamType2, PresetType, SpectrumData, PluginType, StdParamTypes,
  TVSPSources, TVSPSourceUtil;

type
  TMPluginSystem4 = class (TMPluginSystem3, IMPluginSystem4)
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    function GetEmptyString: IString; cdecl;
    function GetEmptyBuffer: IVFloatBuffer; cdecl;
    function GetParamTypeUtil: IPParamTypeUtil; cdecl;
    function GetPresetUtil: IPPresetUtil; cdecl;
    function GetSpectrumData: ISpectrumData; cdecl;
    function GetNoCanvas: IPCanvas; cdecl;
    function GetSources: ITVSPSources; cdecl;
  end;

implementation

{%REGION TMPluginSystem4}

const
  Local_Version4: MVVersion = (Version:0;MainVersion:4;SubVersion:0);

function TMPluginSystem4.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version4;
end;

function TMPluginSystem4.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version4
    then Result:=IMInterface(IMPluginSystem4(Self))
    else Result:=inherited Future(Version);
end;

function TMPluginSystem4.GetEmptyString: IString; cdecl;
begin
  Result:=EmptyString;
end;

function TMPluginSystem4.GetEmptyBuffer: IVFloatBuffer; cdecl;
begin
  Result:=EmptyBuffer;
end;

function TMPluginSystem4.GetParamTypeUtil: IPParamTypeUtil; cdecl;
begin
  Result:=ParamTypeUtil;
end;

function TMPluginSystem4.GetPresetUtil: IPPresetUtil; cdecl;
begin
  Result:=PresetUtil;
end;

function TMPluginSystem4.GetSpectrumData: ISpectrumData; cdecl;
begin
  Result:=AdvSpectrumData;
end;

function TMPluginSystem4.GetNoCanvas: IPCanvas; cdecl;
begin
  Result:=NoCanvas;
end;

function TMPluginSystem4.GetSources: ITVSPSources; cdecl;
begin
  Result:=SourceUtil;
end;

{%ENDREGION}

end.

