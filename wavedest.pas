unit WaveDest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpectrumData, PluginType;

type
  IWaveDest = Interface (IMInterface)
    ['{CA1BB963-4850-45B6-96E4-23B129E6796E}']
    function GetLocked: Boolean; stdcall;
    procedure Analyse; stdcall; overload;
    procedure Analyse(const ASampleCount: MVInt); stdcall; overload;
    function GetBuffer(const AChannel: MVInt): Pointer; stdcall;

    property Channels[const AChannel: MVInt]: Pointer read GetBuffer;
    property Locked: Boolean read GetLocked;
  end;

  TfaiFeature         = (fifControlPanel = 0,fifSetChannels = 1,fifSetSampleRate = 2,fifSetSampleSize = 3);
  TfaiFeatures        = set of TfaiFeature;

  TMDriverData        = packed record
    Name: MVSStr;
    ID  : TGUID;
  end;

  IFreqAnaInput2      = interface
    ['{C49219B0-AFD4-45BB-BDD9-68592138D6B8}']
    function GetFeatures: TfaiFeatures; stdcall;
    function GetDriverCount: MVInt; stdcall;
    function GetDriver(Index: MVInt): TMDriverData; stdcall;
    function GetMaxChannels: MVInt; stdcall;
    function GetSupportedBufferSizeCount: MVInt; stdcall;
    function GetSupportedBufferSize(Index: MVInt): MVInt; stdcall;
    function GetPreferedBufferSize: MVInt; stdcall;
    function GetSupportedSampleRateCount: MVInt; stdcall;
    function GetSupportedSampleRate(Index: MVInt): MVInt; stdcall;
    function GetPreferedSampleRate: MVInt; stdcall;

    function SetDriver(ADriver: MVInt): Boolean; stdcall;
    function Run: Boolean; stdcall;
    procedure Stop; stdcall;
    procedure CloseDriver; stdcall;

    function SetBufferSize(ABufferSize: MVInt): Boolean; stdcall;
    function SetChannels(AChannels: MVInt): Boolean; stdcall;
    function SetSampleRate(ASampleRate: MVInt): Boolean; stdcall;
    function ShowControlPanel: Boolean; stdcall;

    property DriverCount: MVInt read GetDriverCount;
    property Driver[const Index: MVInt]: TMDriverData read GetDriver;
    property Features: TfaiFeatures read GetFeatures;
    property MaxChannels: MVInt read GetMaxChannels;
    property SupportedBufferSizeCount: MVInt read GetSupportedBufferSizeCount;
    property SupportedBufferSizes[Index: MVInt]: MVInt read GetSupportedBufferSize;
    property SupportedSampleRateCount: MVInt read GetSupportedSampleRateCount;
    property SupportedSampleRates[Index: MVInt]: MVInt read GetSupportedSampleRate;
  end;

  TFreqAnaInput2      = class (IFreqAnaInput2)
  private
    FRefCount: Cardinal;
    FWaveDest: IWaveDest;
  protected
    function QueryInterface(constref iid: tguid; out obj) : LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;

    function GetFeatures: TfaiFeatures; stdcall; virtual; abstract;
    function GetDriverCount: MVInt; stdcall; virtual; abstract;
    function GetDriver(Index: MVInt): TMDriverData; stdcall; virtual; abstract;
    function GetMaxChannels: MVInt; stdcall; virtual; abstract;
    function GetSupportedBufferSizeCount: MVInt; stdcall; virtual; abstract;
    function GetSupportedBufferSize(Index: MVInt): MVInt; stdcall; virtual; abstract;
    function GetPreferedBufferSize: MVInt; stdcall; virtual; abstract;
    function GetSupportedSampleRateCount: MVInt; stdcall; virtual; abstract;
    function GetSupportedSampleRate(Index: MVInt): MVInt; stdcall; virtual; abstract;
    function GetPreferedSampleRate: MVInt; stdcall; virtual; abstract;

    function SetDriver(ADriver: MVInt): Boolean; stdcall; virtual; abstract;
    function Run: Boolean; stdcall; virtual; abstract;
    procedure Stop; stdcall; virtual; abstract;
    procedure CloseDriver; stdcall; virtual; abstract;

    function SetBufferSize(ABufferSize: MVInt): Boolean; stdcall; virtual; abstract;
    function SetChannels(AChannels: MVInt): Boolean; stdcall; virtual; abstract;
    function SetSampleRate(ASampleRate: MVInt): Boolean; stdcall; virtual; abstract;
    function ShowControlPanel: Boolean; stdcall; virtual; abstract;

    property WaveDest: IWaveDest read FWaveDest;
  public
    constructor Create(AWaveDest: IWaveDest); virtual;
    destructor Destroy; override;
  end;

  TNewInput           = function (AWaveDest: IWaveDest): IFreqAnaInput2; stdcall;

implementation

{TFreqAnaInput2}

constructor TFreqAnaInput2.Create(AWaveDest: IWaveDest);
begin
  inherited Create;
  FWaveDest:=AWaveDest;
  FRefCount:=0;
end;

destructor TFreqAnaInput2.Destroy;
begin
  FWaveDest:=nil;
  inherited Destroy;
end;

function TFreqAnaInput2.QueryInterface(constref iid: tguid; out obj) : LongInt; stdcall;
begin
  Result:=S_OK;
end;

function TFreqAnaInput2._AddRef: LongInt; stdcall;
begin
  Inc(FRefCount);
  Result:=FRefCount;
end;

function TFreqAnaInput2._Release: LongInt; stdcall;
begin
  Dec(FRefCount);
  Result:=FRefCount;
  if FRefCount=0 then Destroy;
end;

end.

