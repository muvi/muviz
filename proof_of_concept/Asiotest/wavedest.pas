unit WaveDest;

{$mode objfpc}{$H+}
{.$DEFINE IDEBUG}

interface

uses
  Classes, SysUtils, {SpectrumData,} PluginType, MInterfacedObject
  {$IFDEF IDEBUG}, Dialogs{$ENDIF};

type
  IWaveDest = Interface (IMInterface)
    ['{CA1BB963-4850-45B6-96E4-23B129E6796E}']
    procedure Analyse; stdcall; overload;
    procedure Analyse(const ASampleCount: MVInt); stdcall; overload;
    function Init(const ASampleRate,AChannels,ASampleCount: MVInt): Boolean; stdcall;
    function GetBuffer(const AChannel: MVInt): Pointer; stdcall;

    property Channels[const AChannel: MVInt]: Pointer read GetBuffer;
  end;

  TfaiFeature         = (fifControlPanel = 0,fifSetChannels = 1,fifSetSampleRate = 2,fifSetSampleSize = 3);
  TfaiFeatures        = set of TfaiFeature;

  IFreqAnaInput       = interface (IMInterface)
    ['{01F29392-9DCE-473D-8660-58ACD960607B}']
    function GetFeatures: TfaiFeatures; stdcall;
    function GetDriverCount: MVInt; stdcall;
    function GetDriverNames(const Index: MVInt): ShortString; stdcall;
    function GetRunning: Boolean; stdcall;
    function GetDriverIndex: MVInt; stdcall;
    procedure Init(AWaveDest: IWaveDest); stdcall;
    procedure Done; stdcall;
    procedure Run; stdcall;
    procedure Stop; stdcall;
    procedure ShowControlPanel; stdcall;
    procedure SetCounts(const ASampleRate,AChannels,ASampleCount: MVInt); stdcall;
    procedure SetDriverIndex(const Value: MVInt); stdcall;

    property Driver: MVInt read GetDriverIndex write SetDriverIndex;
    property DriverCount: MVInt read GetDriverCount;
    property DriverNames[const Index: MVInt]: ShortString read GetDriverNames;
    property Features: TfaiFeatures read GetFeatures;
    property Running: Boolean read GetRunning;
  end;

  TFreqAnaInput       = class (TMInterfacedObject, IFreqAnaInput)
  private
    FWaveDest: IWaveDest;
    FIsInited: Boolean;
    FRunning : Boolean;
    {$IFDEF IDEBUG} FRefCount: Integer; {$ENDIF}
  protected
    {$IFDEF IDEBUG}
    function _AddRef: LongInt; stdcall; override;
    function _Release: LongInt; stdcall; override;
    {$ENDIF}
    procedure AfterDestroy; virtual;
    function GetFeatures: TfaiFeatures; stdcall; virtual;
    function GetDriverCount: MVInt; stdcall; virtual; abstract;
    function GetDriverNames(const Index: MVInt): ShortString; stdcall; virtual; abstract;
    function GetRunning: Boolean; stdcall; virtual;
    function GetDriverIndex: MVInt; stdcall; virtual; abstract;
    procedure SetCounts(const ASampleRate,AChannels,ASampleCount: MVInt); stdcall; virtual;
    procedure SetDriverIndex(const Value: MVInt); stdcall; virtual; abstract;

    property WaveDest: IWaveDest read FWaveDest;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Init(AWaveDest: IWaveDest); stdcall; virtual;
    procedure Done; stdcall; virtual;
    procedure Run; stdcall; virtual;
    procedure Stop; stdcall; virtual;
    procedure ShowControlPanel; stdcall; virtual;
    property DriverNames[const Index: MVInt]: ShortString read GetDriverNames;
  published
    property Driver: MVInt read GetDriverIndex write SetDriverIndex;
    property DriverCount: MVInt read GetDriverCount;
    property Features: TfaiFeatures read GetFeatures;
    property Running: Boolean read GetRunning;
  end;

implementation

{TFreqAnaInput}

constructor TFreqAnaInput.Create(AOwner: TComponent);
begin
  inherited Create;
  {$IFDEF IDEBUG}
  FRefCount:=0;
  FIsInited:=false;
  FRunning:=false;
  {$ENDIF}
end;

destructor TFreqAnaInput.Destroy;
begin
  if FIsInited then Done;
  if FWaveDest<>nil then FWaveDest:=nil;
  AfterDestroy;
  {$IFDEF IDEBUG}
  ShowMessage('TFreqAna RefCount: '+IntToStr(FRefCount));
  {$ENDIF}
  inherited Destroy;
end;

procedure TFreqAnaInput.AfterDestroy;
begin
  //do nothing
end;

{function TFreqAnaInput.QueryInterface(const iid: tguid; out obj): LongInt; stdcall;
begin
  Result:=0;
end;

function TFreqAnaInput._AddRef: LongInt; stdcall;
begin
  Result:=1;
end;

function TFreqAnaInput._Release : LongInt; stdcall;
begin
  Result:=1;
end;}

function TFreqAnaInput.GetFeatures: TfaiFeatures; stdcall;
begin
  Result:=[];
end;

function TFreqAnaInput.GetRunning: Boolean; stdcall;
begin
  Result:=FRunning;
end;

procedure TFreqAnaInput.Init(AWaveDest: IWaveDest); stdcall;
begin
  FWaveDest:=AWaveDest;
  FIsInited:=true;
end;

procedure TFreqAnaInput.Done; stdcall;
begin
  if FRunning then Stop;
  FIsInited:=false;
  FWaveDest:=nil;
end;

procedure TFreqAnaInput.Run; stdcall;
begin
  FRunning:=true;
end;

procedure TFreqAnaInput.Stop; stdcall;
begin
  FRunning:=false;
end;

procedure TFreqAnaInput.ShowControlPanel; stdcall;
begin
  //do nothing
end;

procedure TFreqAnaInput.SetCounts(const ASampleRate,AChannels,ASampleCount: MVInt); stdcall;
begin
  //do nothing
end;

{$IFDEF IDEBUG}
function TFreqAnaInput._AddRef: LongInt; stdcall;
begin
  Inc(FRefCount);
  Result:=1;
end;

function TFreqAnaInput._Release: LongInt; stdcall;
begin
  Dec(FRefCount);
  Result:=1;
end;
{$ENDIF}

end.

