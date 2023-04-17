unit FreqAna3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VPBuffers, StdParamTypes, FreqAna2, SpectrumData,
  VPBufferUnit, PluginType;

type
  TAdvancedBeatSpectrumData = class (TBeatSpectrumData, ISpectrumData3)
  protected
    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
  public
    function GetBuffer2(ABufferIndex, AChannel: MVInt): IVFloatBuffer; stdcall;
    function GetLevelBuffer2(AChannel: MVInt): IVFloatBuffer; stdcall;
  end;

function OldBufferToNewBuffer(ABuffer: vpBuffer): IVFloatBuffer;

implementation

{%REGION TAdvancedBeatSpectrumData}

const
  Local_Version: MVVersion = (Version:0;MainVersion:5;SubVersion:0);

function TAdvancedBeatSpectrumData.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TAdvancedBeatSpectrumData.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(ISpectrumData3(Self))
    else Result:=inherited Future(Version);
end;

function TAdvancedBeatSpectrumData.GetBuffer2(ABufferIndex,AChannel: MVInt): IVFloatBuffer; stdcall;
begin
  Result:=OldBufferToNewBuffer(GetBuffer(ABufferIndex, AChannel));
end;

function TAdvancedBeatSpectrumData.GetLevelBuffer2(AChannel: MVInt): IVFloatBuffer; stdcall;
begin
  Result:=OldBufferToNewBuffer(GetLevelBuffer(AChannel));
end;

{%ENDREGION}
{%REGION Misc}

function OldBufferToNewBuffer(ABuffer: vpBuffer): IVFloatBuffer;
var
  I, L      : Integer;
begin
  L:=BufferManager.Size[ABuffer];
  Result:=EmptyBuffer.Resize(L);
  for I:=0 to L-1
    do Result[I]:=BufferManager.Items[ABuffer, I];
end;

{%ENDREGION}

end.

