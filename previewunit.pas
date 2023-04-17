unit PreviewUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType, VisTypeUnit, PseudoSpectrumData, VPBufferUnit,
  MCanvasUnit;

type
  TPreviewEvent        = procedure of object;
  TPresetPreviewDrawer = class
  strict private
    FPreset       : TPreset;
    FNewPreset    : TPreset;
    FSpectrumData : TPseudoSpectrumData;
    FBufferManager: TVPBufferManager;
    FDest         : TMCanvas;
    FOwnsDest     : Boolean;
    FOnReady      : TPreviewEvent;
    FOnStart      : TPreviewEvent;
    FPresetThread : TThread;
    procedure EmulateThreadPush(AVis: TVisualisation; AProc: TVisProc; AFinished: TVisProc = nil);
  private
    function DoGeneratePreview: Boolean;
  public
    constructor Create(ADest: TMCanvas; AOwnsDest: Boolean = false);
    destructor Destroy; override;
    procedure GeneratePreview(APreset: TPreset);
    property OnReady: TPreviewEvent read FOnReady write FOnReady;
    property OnStart: TPreviewEvent read FOnStart write FOnStart;
  end;


implementation

{TPresetPreviewThread}

type
  TPresetPreviewThread = class (TThread)
  private
    FOwner: TPresetPreviewDrawer;
    FReady: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPresetPreviewDrawer); virtual;
    destructor Destroy; override;
  end;

constructor TPresetPreviewThread.Create(AOwner: TPresetPreviewDrawer);
begin
  inherited Create(true);
  FOwner:=AOwner;
  FreeOnTerminate:=false;
  FReady:=false;
end;

destructor TPresetPreviewThread.Destroy;
begin
  Terminate;
  if Suspended then Resume;
  while not FReady do ;
  inherited Destroy;
end;

procedure TPresetPreviewThread.Execute;
begin
  while not Terminated
    do if FOwner.DoGeneratePreview
      then Suspend;
  FReady:=true;
end;

{TPresetPreviewDrawer}

constructor TPresetPreviewDrawer.Create(ADest: TMCanvas; AOwnsDest: Boolean = false);
begin
  inherited Create;
  FPreset:=nil;
  FNewPreset:=nil;
  FDest:=ADest;
  FOwnsDest:=AOwnsDest;
  FBufferManager:=TVPBufferManager.Create;
  FSpectrumData:=TPseudoSpectrumData.Create(FBufferManager);
  FPresetThread:=TPresetPreviewThread.Create(Self);
end;

destructor TPresetPreviewDrawer.Destroy;
begin
  FPresetThread.Destroy;
  FSpectrumData.Destroy;
  FBufferManager.Destroy;
  if FOwnsDest then FDest.Destroy;
  inherited Destroy;
end;

procedure TPresetPreviewDrawer.EmulateThreadPush(AVis: TVisualisation; AProc: TVisProc; AFinished: TVisProc = nil);
begin
  AVis.CallVisProc(AProc);
  if Assigned(AFinished) then AVis.CallVisProc(AFinished);
end;

function TPresetPreviewDrawer.DoGeneratePreview: Boolean;
var
  I            : Integer;
  FComposition : TVisComposition;
const
  AVisCount = 10;
label
  AExit;
begin
  FPreset:=FNewPreset;
  with TPresetPreviewThread(FPresetThread) do begin
    if Assigned(FOnStart) then FOnStart;
    FComposition:=TVisComposition.Create(FPreset,FDest,FSpectrumData,@EmulateThreadPush);
    if (FPreset<>FNewPreset) or Terminated then goto AExit;
    for I:=0 to AVisCount-1 do begin
      FComposition.Visualize;
      if (FPreset<>FNewPreset) or Terminated then goto AExit;
    end;
    AExit:
    FComposition.Destroy;
    Result:=((FPreset=FNewPreset) and (not Terminated));
    if Result then if Assigned(FOnReady) then FOnReady;
  end;
end;

procedure TPresetPreviewDrawer.GeneratePreview(APreset: TPreset);
begin
  FNewPreset:=APreset;
  with FPresetThread do if Suspended then Resume;
end;

end.

