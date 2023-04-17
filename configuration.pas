unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, StdTags, VisEventImpl, MStrings, AdvParamType,
  StdParamTypes, ParamTypes, PresetType, VisAddInput, VisTypeImpl, SimpleVis,
  VisualisationUtils;

type
  TConfig = class (TVisualisationEvents)
  private
    FCurrentVisualisation          : IPVisualisation;
    FMainPreset                    : IPPreset;
    //colors
    FBGColor                       : IPColor;
    FHighlightColor                : IPColor;
    FFontColor                     : IPColor;
    FFontHighlightColor            : IPColor;
    FFontInHighlightColor          : IPColor;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    property CurrentVisualisation: IPVisualisation read FCurrentVisualisation;

    //property BGColor: IPColor read FBGColor;
    //property HighlightColor: IPColor read FHighlightColor;
    //property FontColor: IPColor read FFontColor;
    //property FontHighlightColor: IPColor read FFontHighlightColor;
    //property FontInHighlightColor: IPColor read FFontInHighlightColor;
    //property MainPreset: IPPreset read FMainPreset;
  end;

procedure Register;

var
  Config: IPVisualisation;
const
  VIDCONFIG: TGUID         = '{FF5EC525-5DE9-49F3-8EBB-415FDDF73363}';
  //PIDCONFIG: TGUID         = '{ADC888FF-B420-4537-BF53-2CEF1E59EFC8}';

  MAINPRESETNAME           = 'Main Preset';
  BGCOLORNAME              = 'Background Color';
  HIGHLIGHTCOLORNAME       = 'Highlight Color';
  FONTCOLORNAME            = 'Font Color';
  FONTHIGHLIGHTCOLORNAME   = 'Highlighted Font Color';
  FONTINHIGHLIGHTCOLORNAME = 'Font in Highlight Color';

implementation

const
  //Color Scheme
  clsBG             = $FF484C41;
  clsHighlight      = $FF888C81;
  clsFont           = $FFA8ACA1;
  clsFontHighlight  = $FFFF0000;
  clsFontInHighlight= $FF484C41;
  //DefaultScheme
  {clsBG             = clBtnFace;
  clsHighlight      = clMenuHighlight;
  clsFont           = clWindowText;
  clsFontHighlight  = clRed;
  clsFontInHighlight= clWindow;}

{%REGION TConfig}

procedure MainPresetChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TConfig(Context) do begin
    CurrentVisualisation.Suspend;
    FCurrentVisualisation:=PresetUtil[FMainPreset.ExecutedValue];
    CurrentVisualisation.Resume;
  end;
end;

constructor TConfig.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMainPreset:=PresetInputs[MainPresetName];
  FCurrentVisualisation:=PresetUtil[DEFAULTPRESET];
  FMainPreset.AddListener(@MainPresetChanged, Self, VisualisationUtil.MainThread);
  with VisualisationUtil do begin
    FBGColor:=ColorInputs[BGColorName];
    FBGColor.Value:=clsBG;
    FHighlightColor:=ColorInputs[HighlightColorName];
    FHighlightColor.Value:=clsHighlight;
    FFontColor:=ColorInputs[FontColorName];
    FFontColor.Value:=clsFont;
    FFontHighlightColor:=ColorInputs[FontHighlightColorName];
    FFontHighlightColor.Value:=clsFontHighlight;
    FFontInHighlightColor:=ColorInputs[FontInHighlightColorName];
    FFontInHighlightColor.Value:=clsFontInHighlight;
  end;
  FCurrentVisualisation.Resume;
end;

destructor TConfig.Destroy;
begin
  FMainPreset.RemoveListener(@MainPresetChanged, Self);
  FCurrentVisualisation.Suspend;
  FMainPreset:=nil;
  FBGColor:=nil;
  FHighlightColor:=nil;
  FFontColor:=nil;
  FFontHighlightColor:=nil;
  FFontInHighlightColor:=nil;
  FCurrentVisualisation:=nil;
  inherited Destroy;
end;

procedure TConfig.GetInput(ASettings: IPParamSettings); cdecl;
begin
  {
  if ASettings.Param.ID = ParamID(BGCOLORNAME, vColor) then begin
    ASettings.Thread:=VisualisationUtil.MainThread;
    exit;
  end;
  if ASettings.Param.ID = ParamID(HIGHLIGHTCOLORNAME, vColor) then begin
    ASettings.Thread:=VisualisationUtil.MainThread;
    exit;
  end;
  if ASettings.Param.ID = ParamID(FONTCOLORNAME, vColor) then begin
    ASettings.Thread:=VisualisationUtil.MainThread;
    exit;
  end;
  if ASettings.Param.ID = ParamID(FONTHIGHLIGHTCOLORNAME, vColor) then begin
    ASettings.Thread:=VisualisationUtil.MainThread;
    exit;
  end;
  if ASettings.Param.ID = ParamID(FONTINHIGHLIGHTCOLORNAME, vColor) then begin
    ASettings.Thread:=VisualisationUtil.MainThread;
    exit;
  end;
  }
  Assert(Prototype.Environment <> nil);
  Assert(Prototype.Thread <> nil);
  if ASettings.Param.ID = ParamID(MAINPRESETNAME, vPreset) then begin
    IPPresetSettings(ASettings).Environment:=Prototype.Environment;
    exit;
  end;
end;

{%ENDREGION}
{%REGION Misc}

procedure CreateConfig(APrototype: IPVisualisationPrototype); cdecl;
begin
  TConfig.Create(APrototype);
end;

procedure Register;
begin
  PresetUtil.RegisterVis(VIDCONFIG, @CreateConfig);
  Config:=CreatePreset('Configuration', VIDCONFIG);
  with Config do begin
    AddInput(This, MainPresetName, DEFAULTPRESET);
    AddInput(This, BGColorName, clsBG);
    AddInput(This, HighlightColorName, clsHighlight);
    AddInput(This, FontColorName, clsFont);
    AddInput(This, FontHighlightColorName, clsFontHighlight);
    AddInput(This, FontInHighlightColorName, clsFontInHighlight);
    AddInput(This, LOADEDINPUTNAME, true);
    AddTag(TAGSYSTEM);
    AddTag(TAGHIDDEN);
    AddTag(TAGPREDEFINED);
    AddTag(TAGNONVISUAL);
  end;
end;

{%ENDREGION}

finalization
  Config:=nil;
end.

