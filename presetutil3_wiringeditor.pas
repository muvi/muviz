unit PresetUtil3_WiringEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetUtil3_Scrolling, ExtCtrls;

type
  TWiringPresetEditor = class (TScrollingPresetEditor)
  strict private
    FRenderSteps  : Integer;
    FWireTimer    : TTimer;
    FOnEndRender  : TNotifyEvent;
    FOnRender     : TNotifyEvent;
    FOnStartRender: TNotifyEvent;
    procedure PresetEditorStartRender(Sender: TObject);
    procedure PresetEditorEndRender(Sender: TObject);
    procedure WireTimerTimer(Sender: TObject);
    function GetRenderInterval: Integer;
    procedure SetRenderInterval(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property RenderInterval: Integer read GetRenderInterval write SetRenderInterval;
    property RenderSteps: Integer read FRenderSteps write FRenderSteps;
    property OnEndRender: TNotifyEvent read FOnEndRender write FOnEndRender;
    property OnRender: TNotifyEvent read FOnRender write FOnRender;
    property OnStartRender: TNotifyEvent read FOnStartRender write FOnStartRender;
  end;

implementation

{%REGION TWiringPresetEditor}

constructor TWiringPresetEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderSteps:=3;
  FWireTimer:=TTimer.Create(Self);
  //do not use InsertComponent(FWIreTimer)
  //that will result in bugs
  with FWireTimer do begin
    Enabled:=false;
    OnTimer:=@WireTimerTimer;
    Interval:=40;
  end;
  with PresetEditor do begin
    OnStartRender:=@PresetEditorStartRender;
    OnEndRender:=@PresetEditorEndRender;
  end;
end;

destructor TWiringPresetEditor.Destroy;
begin
  with PresetEditor do begin
    OnStartRender:=nil;
    OnEndRender:=nil;
  end;
  FWireTimer.Destroy;
  inherited Destroy;
end;

function TWiringPresetEditor.GetRenderInterval: Integer;
begin
  Result:=FWireTimer.Interval;
end;

procedure TWiringPresetEditor.SetRenderInterval(AValue: Integer);
begin
  FWireTimer.Interval:=AValue;
end;

procedure TWiringPresetEditor.PresetEditorStartRender(Sender: TObject);
begin
  FWireTimer.Enabled:=true;
  if Assigned(FOnStartRender)
    then FOnStartRender(Self);
end;

procedure TWiringPresetEditor.PresetEditorEndRender(Sender: TObject);
begin
  FWireTimer.Enabled:=false;
  if Assigned(FOnEndRender)
    then FOnEndRender(Self);
end;

procedure TWiringPresetEditor.WireTimerTimer(Sender: TObject);
begin
  PresetEditor.Rewire(FWireTimer.Interval/1000, FRenderSteps);
  if Assigned(FOnRender)
    then FOnRender(Self);
end;

{%ENDREGION}

end.

