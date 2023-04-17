unit DMXDevice;

{$mode objfpc}

interface

uses
  Classes, SysUtils, VisType2, ParamOp, StdParamTypes, MStrings, GUIDop;

type
  TDMXDevice = class
  strict private
    FPreset            : IPPreset;
    FVis               : IPVisualisation;
    FSize              : IPInteger;
    FChannels          : array of IPInteger;
    FIndex             : Integer;
    FEnvironment       : IPVisualisationEnvironment;
    FOnNeedsDestruction: TNotifyEvent;
    function GetValue(AChannel: Integer): Byte;
    procedure SetValue(AChannel: Integer; AValue: Byte);
  private
    procedure UpdatePreset;
    procedure UpdateSize; inline;
    procedure UpdateChannels;
  public
    constructor Create(APreset: IPPreset; AEnvironment: IPVisualisationEnvironment; AIndex: Integer; AOnNeedsDestruction: TNotifyEvent);
    destructor Destroy; override;
    property Index: Integer read FIndex write FIndex;
    property Values[AChannel: Integer]: Byte read GetValue write SetValue; default;
  end;

implementation

{%REGION TDMXDevice}

procedure PresetChanged(Context: Pointer; Sender: TPParamNotificationSender); cdecl;
begin
  with TDMXDevice(Context)
    do UpdatePreset;
end;

procedure SizeChanged(Context: Pointer; Sender: TPParamNotificationSender); cdecl;
begin
  with TDMXDevice(Context)
    do UpdateChannels;
end;

constructor TDMXDevice.Create(APreset: IPPreset; AEnvironment: IPVisualisationEnvironment; AIndex: Integer; AOnNeedsDestruction: TNotifyEvent);
begin
  inherited Create;
  FEnvironment:=AEnvironment;
  FIndex:=AIndex;
  FOnNeedsDestruction:=AOnNeedsDestruction;
  FPreset:=APreset;
  FVis:=AEnvironment[APreset.Get];
  FSize:=nil;
  FPreset.AddListener(@PresetChanged, Self);
  UpdateSize;
end;

destructor TDMXDevice.Destroy;
var
  I: Integer;
begin
  FSize.RemoveListener(@SizeChanged, Self);
  FPreset.RemoveListener(@PresetChanged, Self);
  for I:=0 to Length(FChannels)-1
    do FChannels[I]:=nil;
  SetLength(FChannels, 0);
  FSize:=nil;
  FPreset:=nil;
  FEnvironment:=nil;
  inherited Destroy;
end;

procedure TDMXDevice.UpdatePreset;
var
  ANewPreset: TVPreset;
begin
  ANewPreset:=FPreset.Get;
  //if ANewPreset<>NULLPRESETID then begin
    FVis:=FEnvironment[ANewPreset];
    UpdateSize;
  //end else FOnNeedsDestruction(Self);
end;

procedure TDMXDevice.UpdateSize; inline;
begin
  if FSize<>nil
    then FSize.RemoveListener(@SizeChanged, Self);
  FSize:=IPInteger(FVis.Inputs[ParamID('Size', vInteger)]);
  FSize.AddListener(@SizeChanged, Self);
  UpdateChannels;
end;

procedure TDMXDevice.UpdateChannels;
var
  ANewSize, AOldSize, I: Integer;
begin
  ANewSize:=FSize;
  AOldSize:=Length(FChannels);
  SetLength(FChannels, ANewSize);
  for I:=AOldSize to ANewSize-1
    do FChannels[I]:=IPInteger(FVis.Inputs[ParamID(IntToStr(I), vInteger)]);
end;

function TDMXDevice.GetValue(AChannel: Integer): Byte;
begin
  if (AChannel >= 0) and (AChannel < Length(FChannels))
    then Result:=FChannels[AChannel].Get
    else Result:=0;
end;

procedure TDMXDevice.SetValue(AChannel: Integer; AValue: Byte);
begin
  if (AChannel >= 0) and (AChannel < Length(FChannels))
    then FChannels[AChannel].&Set(AValue);
end;

{%ENDREGION}

end.

