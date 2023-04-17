unit DMXUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, ParamTypes,
  SimpleVis, ParamType2, SysUtils, OpenDMX, VisAddInput, VisualisationUtils,
  AdvParamType, StdPermissions;

type
  TDMXMainInterface= class (TInterfacedObject)
  private
    FVisualisation: IPVisualisation;
  public
    constructor Create(AParam: IPPreset);
    destructor Destroy; override;
  end;

  TDMXMainVis      = class (TVisualisationEvents)
  protected
     procedure GetInput(ASettings: IPParamSettings); cdecl; override;
     procedure GotInput(Param: IPParam); cdecl; override;
  end;

  TDMXDeviceChannel= class
  strict private
    FDeviceParam: IPInteger;
    FDMXParam   : IPInteger;
  public
    constructor Create(ADeviceParam, ADMXParam: IPInteger; AOwnerID: TGUID);
    destructor Destroy;
  end;

  TDMXDevice       = class (TVisualisationEvents)
  strict private
    FInterface   : IPString;
    FStartAddress: IPInteger;
    FChannelCount: IPInteger;
    FChannels    : array of TDMXDeviceChannel;
    FInterfaceVis: IPVisualisation;
    procedure ClearChannels;
  private
    procedure UpdateInterface;
    procedure UpdateChannels;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TOpenDMXInterface= class;

  TOpenDMXChannel  = class
  private
    FOwner       : TOpenDMXInterface;
    FChannelIndex: Integer;
    FParam       : IPInteger;
  public
    constructor Create(AOwner: TOpenDMXInterface; AIndex: Integer);
    destructor Destroy; override;
  end;

  TOpenDMXInterface= class (TVisualisationEvents)
  strict private
    FOpenDMX   : TOpenDMX;
    FChannels  : array [0..511] of TOpenDMXChannel;
  private
    {
    FDevice    : IPInteger;
    FDeviceName: IPString;
    }
    property OpenDMX: TOpenDMX read FOpenDMX;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    procedure Resume; cdecl; override;
    procedure Suspend; cdecl; override;
  end;

const
  VIDDMXMAIN         : TGUID = '{EB82A060-78F3-4B9F-A089-ECACE6CC2494}';
  VIDDMXDEVICE       : TGUID = '{DBCC30CC-940D-4DF8-BF9C-A3DFADDCA863}';
  VIDOPENDMXINTERFACE: TGUID = '{567DA78A-955C-4613-98FA-7569CF1FA435}';

  {
  PIDDMXMAIN         : TGUID = '{3DB3CB10-5136-46C8-AC45-300FB65FA6F1}';
  PIDDMXDEVICE       : TGUID = '{F4BBA63B-2B72-4C01-9082-489FDAFEFDCB}';
  PIDOPENDMXINTERFACE: TGUID = '{EC527B26-EF4A-4656-AFB5-DCC2C6248F72}';
  }

  DMXDEVICESTARTADDRESSNAME = 'Address';
  DMXDEVICECHANNELCOUNTNAME = 'Channels';
  DMXDEVICEINTERFACENAME    = 'Device';

  DMXCHANNELPREFIX          = 'Channel ';

procedure Register;

implementation

{%REGION TDMXMainInterface}

procedure DMXMainInterfacePresetChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TDMXMainInterface(Context) do begin
    FVisualisation.Suspend;
    FVisualisation:=PresetUtil[IPPreset(Sender).ExecutedValue];
    FVisualisation.Resume;
  end;
end;

constructor TDMXMainInterface.Create(AParam: IPPreset);
begin
  inherited Create;
  AParam.AttachInterface(Self);
  AParam.AddListener(@DMXMainInterfacePresetChanged, Self, VisualisationUtil.MainThread);
  FVisualisation:=PresetUtil[AParam.ExecutedValue];
  FVisualisation.Resume;
end;

destructor TDMXMainInterface.Destroy;
begin
  //not necessary to remove the listener here...
  //this destructor is called if the parameter is destroyed
  FVisualisation.Suspend;
  FVisualisation:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TDMXMainVis}

procedure CreateDMXMainVis(APrototype: IPVisualisationPrototype); cdecl;
begin
  TDMXMainVis.Create(APrototype);
end;

procedure TDMXMainVis.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID.&Type = TPParamType(vPreset) then begin
    //ASettings.Thread:=VisualisationUtil.MainThread;
    IPPresetSettings(ASettings).Environment:=VisualisationUtil.MainEnvironment;
  end else inherited GetInput(ASettings);
end;

procedure TDMXMainVis.GotInput(Param: IPParam); cdecl;
begin
  if Param.ID.&Type = TPParamType(vPreset)
    then TDMXMainInterface.Create(IPPreset(Param));
end;

{%ENDREGION}
{%REGION TDMXDeviceChannel}

constructor TDMXDeviceChannel.Create(ADeviceParam, ADMXParam: IPInteger; AOwnerID: TGUID);
begin
  inherited Create;
  FDeviceParam:=ADeviceParam;
  FDMXParam:=ADMXParam;
  ADeviceParam.Attach(ADMXParam, TPEXECUTE, AOwnerID, ADeviceParam.LowestInversePriority+1);
end;

destructor TDMXDeviceChannel.Destroy;
begin
  FDeviceParam.Detach(FDMXParam);
  FDMXParam:=nil;
  FDeviceParam:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TDMXDevice}

procedure CreateDMXDevice(APrototype: IPVisualisationPrototype); cdecl;
begin
  TDMXDevice.Create(APrototype);
end;

procedure DMXDeviceInterfaceChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TDMXDevice(Context).UpdateInterface;
end;

procedure DMXDeviceStartAddressChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TDMXDevice(Context).UpdateChannels;
end;

procedure DMXDeviceChannelCountChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TDMXDevice(Context).UpdateChannels;
end;

constructor TDMXDevice.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FInterface:=StringInputs[DMXDEVICEINTERFACENAME];
  FInterface.AddListener(@DMXDeviceInterfaceChanged, Self, Environment.Thread);
  FStartAddress:=IntegerInputs[DMXDEVICESTARTADDRESSNAME];
  FStartAddress.AddListener(@DMXDeviceStartAddressChanged, Self, Environment.Thread);
  FChannelCount:=IntegerInputs[DMXDEVICECHANNELCOUNTNAME];
  FChannelCount.AddListener(@DMXDeviceChannelCountChanged, Self, Environment.Thread);
  FInterfaceVis:=nil;
end;

destructor TDMXDevice.Destroy;
begin
  ClearChannels;
  FInterfaceVis:=nil;
  FInterface.RemoveListener(@DMXDeviceInterfaceChanged, Self);
  FInterface:=nil;
  FStartAddress.RemoveListener(@DMXDeviceStartAddressChanged, Self);
  FStartAddress:=nil;
  FChannelCount.RemoveListener(@DMXDeviceChannelCountChanged, Self);
  FChannelCount:=nil;
  inherited Destroy;
end;

procedure TDMXDevice.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID = ParamID(DMXDEVICESTARTADDRESSNAME, vInteger)
    then IPIntegerSettings(ASettings).SetBounds(1, MaxInt)
    else if ASettings.Param.ID = ParamID(DMXDEVICECHANNELCOUNTNAME, vInteger)
      then IPIntegerSettings(ASettings).SetBounds(0, MaxInt)
      else if (Pos(DMXCHANNELPREFIX, ASettings.Param.ID.Name.PasStr) = 1) and (ASettings.Param.ID.&Type = TPParamType(vInteger))
        then IPIntegerSettings(ASettings).SetBounds(0, 255)
        else inherited GetInput(ASettings);
end;

procedure TDMXDevice.ClearChannels;
var
  I: Integer;
begin
  for I:=0 to Length(FChannels)-1
    do FChannels[I].Destroy;
  SetLength(FChannels, 0);
end;

procedure TDMXDevice.UpdateInterface;
var
  AInterface: string;
  AParam    : IPParam;
begin
  AInterface:=FInterface.Value;
  if AInterface<>'' then begin
    AParam:=PresetUtil[VIDDMXMAIN][ParamID(AInterface, vPreset)];
    if AParam<>nil
      then FInterfaceVis:=PresetUtil[IPPreset(AParam).ExecutedValue]
      else FInterfaceVis:=nil;
  end else FInterfaceVis:=nil;
  UpdateChannels;
end;

procedure TDMXDevice.UpdateChannels;
var
  I, AChannelCount, AStartAddress: Integer;
begin
  ClearChannels;
  if FInterfaceVis<>nil then begin
    AStartAddress:=FStartAddress;
    AChannelCount:=FChannelCount;
    SetLength(FChannels, AChannelCount);
    for I:=0 to AChannelCount-1 do begin
      FChannels[I]:=TDMXDeviceChannel.Create(
        IntegerInputs[DMXCHANNELPREFIX + IntToStr(I+1)],
        IPInteger(FInterfaceVis[ParamID(DMXCHANNELPREFIX + IntToStr(I + AStartAddress), vInteger)]),
        Prototype.ID);
    end;
  end;
end;

{%ENDREGION}
{%REGION TOpenDMXChannel}

procedure DMXChannelChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TOpenDMXChannel(Context) do begin
    if FOwner.OpenDMX<>nil
      then FOwner.OpenDMX[FChannelIndex]:=FParam.Get;
  end;
end;

constructor TOpenDMXChannel.Create(AOwner: TOpenDMXInterface; AIndex: Integer);
begin
  inherited Create;
  FChannelIndex:=AIndex + 1;
  FOwner:=AOwner;
  FParam:=AOwner.IntegerInputs[DMXCHANNELPREFIX + IntToStr(FChannelIndex)];
  FParam.AddListener(@DMXChannelChanged, Self, VisualisationUtil.MainThread);
end;

destructor TOpenDMXChannel.Destroy;
begin
  FParam.RemoveListener(@DMXChannelChanged, Self);
  FParam:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TOpenDMXInterface}

procedure CreateOpenDMXInterface(APrototype: IPVisualisationPrototype); cdecl;
begin
  TOpenDMXInterface.Create(APrototype);
end;

constructor TOpenDMXInterface.Create(APrototype: IPVisualisationPrototype);
var
  I: Integer;
begin
  inherited Create(APrototype, false);
  FOpenDMX:=nil;
  for I:=0 to 511
    do FChannels[I]:=TOpenDMXChannel.Create(Self, I);
  //initialize later, so FOpenDMX does not become nil
  DoInitialize;
end;

destructor TOpenDMXInterface.Destroy;
var
  I: Integer;
begin
  //Assert(FOpenDMX = nil);
  for I:=0 to 511
    do FChannels[I].Destroy;
  if FOpenDMX<>nil
    then FOpenDMX.Terminate;
  inherited Destroy;
end;

procedure TOpenDMXInterface.Resume; cdecl;
begin
  inherited Resume;
  FOpenDMX:=TOpenDMX.Create;
end;

procedure TOpenDMXInterface.Suspend; cdecl;
begin
  FOpenDMX.Terminate;
  FOpenDMX:=nil;
  inherited Suspend;
end;

procedure TOpenDMXInterface.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if (ASettings.Param.ID.&Type = TPParamType(vInteger)) and (Pos(DMXCHANNELPREFIX, ASettings.Param.ID.Name.PasStr) = 1)
    then IPIntegerSettings(ASettings).SetBounds(0, 255)
    else inherited GetInput(ASettings);
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
var
  AMainPreset: IPVisualisation;
  I          : Integer;
begin
  with PresetUtil do begin
    RegisterVis(VIDDMXMAIN, @CreateDMXMainVis);
    AMainPreset:=CreatePreset('DMX settings', VIDDMXMAIN);
    with AMainPreset do begin
      AddTag(TAGLISTED);
      AddTag('DMX.Settings');
      AddTag(TAGPREDEFINED);
    end;
    RegisterVis(VIDDMXDEVICE, @CreateDMXDevice);
    with CreatePreset('DMX Device', VIDDMXDEVICE) do begin
      AddTag(TAGLISTED);
      AddTag('DMX');
      AddTag(TAGPREDEFINED);
      AddInput(This, DMXDEVICEINTERFACENAME, 'Device 1');
      AddInput(This, DMXDEVICESTARTADDRESSNAME, 1);
      AddInput(This, DMXDEVICECHANNELCOUNTNAME, 3);
      AddInput(This, DMXCHANNELPREFIX + '1', 0);
      AddInput(This, DMXCHANNELPREFIX + '2', 0);
      AddInput(This, DMXCHANNELPREFIX + '3', 0);
    end;
    RegisterVis(VIDOPENDMXINTERFACE, @CreateOpenDMXInterface);
    with CreatePreset('Enttec Open USB DMX', VIDOPENDMXINTERFACE) do begin
      AddTag(TAGLISTED);
      AddTag('DMX.Interfaces');
      AddTag(TAGPREDEFINED);
      for I:=0 to 511
        do AddInput(This, 'Channel ' + IntToStr(I+1), 0);
    end;
  end;
end;

{%ENDREGION}

end.

