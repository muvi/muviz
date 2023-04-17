unit VisualisationMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamType2, ParamTypeUtilImpl, PresetUtilImpl, PresetType,
  VisType2, ThreadImpl, VisualisationEnvironmentUnit;

type
  TVisualisationSystem = class
  private
    FParamTypeUtil      : TPParamTypeUtil;
    FPresetUtil         : TPPresetUtil;
  public
    constructor Create;
    destructor Destroy;
    procedure Clear;
    procedure ExecuteMainThread; inline;
  end;

var
  VisualisationSystem: TVisualisationSystem = nil;

procedure InitVisualisationSystem;
procedure VisualisationSystemDone;

implementation

{%REGION TVisualisationSystem}

constructor TVisualisationSystem.Create;
begin
  Assert(VisualisationSystem=nil);
  inherited Create;
  FParamTypeUtil:=TPParamTypeUtil.Create;
  FPresetUtil:=TPPresetUtil.Create;
  VisualisationSystem:=Self;
end;

destructor TVisualisationSystem.Destroy;
begin
  //do not destroy the utils, because they are interfaced objects. They are
  //destroyed automatically. Doing it will crash.

  //FPresetUtil.Destroy;
  //FParamTypeUtil.Destroy;
  inherited Destroy;
end;

procedure TVisualisationSystem.ExecuteMainThread; inline;
begin
  FPresetUtil.ExecuteMainThread;
end;

procedure TVisualisationSystem.Clear;
begin
  FPresetUtil.Presets.Clear;
end;

{%ENDREGION}
{%REGION Misc}

procedure InitVisualisationSystem;
begin
  TVisualisationSystem.Create;
end;

procedure VisualisationSystemDone;
begin
  VisualisationSystem.Destroy;
end;

{%ENDREGION}

end.

