unit VisualisationEnvironmentUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, DebugTools, PresetType, HashMap, PresetUtilImpl,
  MapKeys, csl, PresetTypeUnit, MExceptions, StdTags, TagUnit,
  MStrings, CanvasType, StdTagGroups;

type
  TPNoCanvas                = class (TInterfacedObject, IPCanvas)
  protected
    function GetType: TPCanvasType; cdecl; virtual;
  public
    property &Type: TPCanvasType read GetType;
  end;

  TVisualisationEnvironment = class (TInterfacedObject, IPVisualisationEnvironment)
  private
    FThread        : IPThread;
    FCanvas        : IPCanvas;
  protected
    function GetThread: IPThread; cdecl;
    function GetCanvas: IPCanvas; cdecl;
  public
    constructor Create(AThread: IPThread; ACanvas: IPCanvas = nil);
    destructor Destroy; override;
    function CreateSubEnvironment(ACanvas: IPCanvas = nil; AThread: IPThread = nil): IPVisualisationEnvironment; cdecl;
    property Canvas: IPCanvas read FCanvas;
    property Thread: IPThread read FThread;
  end;

implementation

{%REGION TPNoCanvas}

function TPNoCanvas.GetType: TPCanvasType; cdecl;
begin
  Result:=cNoCanvas;
end;

{%ENDREGION}
{%REGION TVisualisationEnvironment}

constructor TVisualisationEnvironment.Create(AThread: IPThread; ACanvas: IPCanvas = nil);
begin
  inherited Create;
  Assert(AThread<>nil);
  Assert(ACanvas<>nil);
  FThread:=AThread;
  FCanvas:=ACanvas;
end;

destructor TVisualisationEnvironment.Destroy;
begin
  FThread:=nil;
  FCanvas:=nil;
  inherited Destroy;
end;

function TVisualisationEnvironment.CreateSubEnvironment(ACanvas: IPCanvas = nil; AThread: IPThread = nil): IPVisualisationEnvironment; cdecl;
var
  ANewCanvas: IPCanvas;
begin
  if ACanvas=nil
    then ANewCanvas:=Canvas
    else ANewCanvas:=ACanvas;
  if AThread=nil
    then Result:=TVisualisationEnvironment.Create(Thread, ANewCanvas)
    else Result:=TVisualisationEnvironment.Create(AThread, ANewCanvas);
end;

function TVisualisationEnvironment.GetThread: IPThread; cdecl;
begin
  Result:=FThread;
end;

function TVisualisationEnvironment.GetCanvas: IPCanvas; cdecl;
begin
  Result:=FCanvas;
end;

{%ENDREGION}
{%REGION Misc}

function DoCreateMainEnvironment(AThread: IPThread): IPVisualisationEnvironment;
begin
  Result:=TVisualisationEnvironment.Create(AThread, NoCanvas)
end;

{%ENDREGION}

initialization
  NoCanvas:=TPNoCanvas.Create;
  CreateMainEnvironment:=@DoCreateMainEnvironment;
finalization
  NoCanvas:=nil;
end.

