unit FAPParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, VisType2, MStrings;

type
  IPForceAlivePreset = interface (IPPreset)
    procedure BufferAll(ATag: IString); cdecl;
    procedure BufferAll; cdecl;
    procedure ReleaseAll; cdecl;
    procedure Buffer(AID: TPPresetID); cdecl;
    procedure Release(AID: TPPresetID); cdecl;
  end;

implementation

end.

