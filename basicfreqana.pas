unit BasicFreqAna;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SpectrumData, PluginType, Dialogs, WaveDest,
  ExtPtrType, DynamicBase, VPBuffers;

type
  TSD2WData           = type Pointer;
  TSD2IntData         = type Pointer;

  TGetLevels          = function (const Index,Channel: MVInt): MVFloat of object;
  TGetLevelsC         = function (const Index: MVInt): MVFloat of object;
  TBasicSpectrumData  = class;
  TSDFilter           = procedure (var ABuffer; const ASize: LongWord) of object; stdcall;

  TBasicSpectrumData  = class (TADObject, IWaveDest, IBasicSpectrumData)
  private
    //Puffer
    FWBufferCounts           : array of Cardinal;
    FLongestWBufferCount     : MVInt;
    FInputBuffer             : TSD2WData;
    //Puffergroeßen
    FSingleInputBufferSize   : MVInt;
    FInputBufferChannelOffset: MVInt;
    FInputBufferChannelSize  : MVInt;
    FInputBufferSize         : MVInt;
    //Indizes
    FAInputBufferIndex       : MVInt;
    FAInputBufferIndexPart   : MVInt;
    //Aufnahmeeingenschaften
    FInputBufferCount          : MVInt;
    FInputBufferSampleCount    : MVInt;
    FChannels                  : MVInt;
    FSampleRate                : MVInt;
    FSyncTime                  : MVFloat;
    FSyncTimerIncrement        : Real;
    FCompleteSyncTimerIncrement: Real;

    FLocked                    : Cardinal;
    FRecognized                : Boolean;
    FMasterVolume              : MVFloat;
    FInputStarted              : Boolean;
    //Events
    FOnReceivedFrame           : TNotifyEvent;
    //intern
    FAInputBuffers: array of TSD2WData;
    FLInputBuffers: array of TSD2WData;

    FBufferManager: IVPBufferManager;
  protected
    FABuffers     : array of array of TSD2WData;
  private
    FDoSetMasterVolume: TSDFilter;
    function GetChannels: MVInt; stdcall;
    function GetSampleRate: MVInt; stdcall;
    function GetLocked: Boolean; stdcall;
    procedure SetMasterVolume(const Value: MVFloat);
    function GetMasterVolume: MVFloat;

    procedure F_Empty(var ABuffer; const ASize: LongWord); stdcall;
    procedure F_DoVolumize(var ABuffer; const ASize: LongWord); stdcall;
    procedure DoFilter(var ABuffer; const ASize: LongWord); inline;
  public
    function Init(const ASampleRate,AChannels,ASampleCount: MVInt): Boolean; stdcall;
    procedure Run; virtual;
    procedure Stop; virtual;
    procedure Analyse; stdcall; overload;
    procedure Analyse(const ASampleCount: MVInt); stdcall; overload;
    function GetBuffer(const AChannel: MVInt): Pointer; stdcall; overload;
    function GetLastBuffer(const AChannel: MVInt): Pointer; stdcall;
    function GetBufferPtr(const ABufferIndex,AChannel: MVInt): Pointer; stdcall; overload;
    function GetBuffer(const ABufferIndex,AChannel: MVInt): vpRealBuffer; stdcall; overload;
    procedure Lock; stdcall;
    procedure Unlock; stdcall;
    procedure Recognize; stdcall;

    property Recognized: Boolean read FRecognized write FRecognized;
  protected
    function GetWData(const ABufferIndex,AChannel,AIndex: MVInt): MVFloat;
    procedure DoAnalyse; virtual;
    procedure DoInit; virtual;
    procedure BeforeInit; virtual;
    procedure Launched(var ACounts: array of MVInt); virtual;

    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
    function WaveDest_GetVersion: MVVersion; virtual; stdcall;
    function WaveDest_Future(const Version: MVVersion): IMInterface; virtual; stdcall;

    function IWaveDest.GetVersion = WaveDest_GetVersion;
    function IWaveDest.Future = WaveDest_Future;

    property BufferManager: IVPBufferManager read FBufferManager;
    property InputStarted: Boolean read FInputStarted;
  public
    constructor Create(ABufferManager: IVPBufferManager); virtual; reintroduce;
    destructor Destroy; override;
    procedure SetCounts(const ACounts: array of MVInt);
  published
    property Channels: MVInt read FChannels;
    property InputBufferSampleCount: Cardinal read FInputBufferSampleCount;
    property Locked: Boolean read GetLocked;
    property MasterVolume: MVFloat read GetMasterVolume write SetMasterVolume;
    property SampleRate: MVInt read FSampleRate;
    property SyncTime: MVFloat read FSyncTime;
    property OnReceivedFrame: TNotifyEvent read FOnReceivedFrame write FOnReceivedFrame;
  end;

function ssqr(const X: ValReal): ValReal; inline;
function cbe(const X: ValReal): ValReal; inline;
function ssqrt(const X: ValReal): ValReal; inline;

implementation

constructor TBasicSpectrumData.Create(ABufferManager: IVPBufferManager);
begin
  inherited Create;
  FBufferManager:=ABufferManager;
  FDoSetMasterVolume:=@F_Empty;
  FMasterVolume:=1.0;
  FLocked:=0;
  FRecognized:=true;
  FSyncTime:=0.0;
  FSyncTimerIncrement:=0.0;
  FCompleteSyncTimerIncrement:=0.0;
  FLongestWBufferCount:=0;
  FInputBufferChannelSize:=0;
  FInputBufferSize:=0;
  FInputBufferChannelOffset:=0;
  FSingleInputBufferSize:=0;

  FAInputBufferIndex:=0;
  FAInputBufferIndexPart:=0;
  //Aufnahmeeingenschaften
  FInputBufferSampleCount:=1; //1 Sample, damit nicht durch 0 dividiert wird und der Puffer nicht leer ist
  FChannels:=1;
  FSampleRate:=44100;
end;

destructor TBasicSpectrumData.Destroy;
begin
  Lock;
  if FInputBufferSize>0 then FreeMem(FInputBuffer,FInputBufferSize);
  SetLength(FAInputBuffers,0);
  SetLength(FLInputBuffers,0);
  inherited Destroy;
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TBasicSpectrumData.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TBasicSpectrumData.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IBasicSpectrumData(Self))
    else Result:=inherited Future(Version);
end;

const
  Local_WaveDest_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TBasicSpectrumData.WaveDest_GetVersion: MVVersion; stdcall;
begin
  Result:=Local_WaveDest_Version;
end;

function TBasicSpectrumData.WaveDest_Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_WaveDest_Version
    then Result:=IMInterface(IWaveDest(Self))
    else Result:=nil;
end;

procedure TBasicSpectrumData.DoAnalyse;
begin
  //bleibt leer
end;

procedure TBasicSpectrumData.Analyse(const ASampleCount: MVInt); stdcall;
var
  I,J                 : Integer;
  IB1,IB2,AInputBuffer: TSD2WData;
begin
  if Locked then exit;

  for I:=0 to FChannels-1 do DoFilter(FAInputBuffers[I]^,ASampleCount);

  IB1:=FAInputBuffers[0];
  IB2:=IB1-FInputBufferChannelOffset;
  for I:=0 to FChannels-1 do begin
    Move(IB1^,IB2^,ASampleCount);
    IB1+=FInputBufferChannelSize;
    IB2+=FInputBufferChannelSize;
  end;

  Inc(FAInputBufferIndexPart,ASampleCount);
  if FAInputBufferIndexPart>=FInputBufferSampleCount then begin
    FAInputBufferIndex:=succ(FAInputBufferIndex) mod FInputBufferCount;
    FAInputBufferIndexPart:=0;
  end;

  for I:=0 to FChannels-1 do begin
    AInputBuffer:=FInputBuffer+(I*FInputBufferChannelSize)+FInputBufferChannelOffset+(FAInputBufferIndex*FSingleInputBufferSize)+(FAInputBufferIndexPart*SD2BufElemSize);
    FLInputBuffers[I]:=FAInputBuffers[I];
    FAInputBuffers[I]:=AInputBuffer;
    for J:=0 to Length(FABuffers)-1
      do FABuffers[J][I]:=AInputBuffer-SD2BufElemSize-(FWBufferCounts[J]*SD2BufElemSize);
  end;
  FSyncTime+=FSyncTimerIncrement*ASampleCount;
  DoAnalyse;
  if Assigned(FOnReceivedFrame) then FOnReceivedFrame(Self);
end;

procedure TBasicSpectrumData.Analyse; stdcall;
var
  I,J                 : Integer;
  IB1,IB2,AInputBuffer: TSD2WData;
begin
  if Locked then exit;

  for I:=0 to FChannels-1 do DoFilter(FAInputBuffers[I]^,FInputBufferSampleCount);

  IB1:=FAInputBuffers[0];
  IB2:=IB1-FInputBufferChannelOffset;
  for I:=0 to FChannels-1 do begin
    Move(IB1^,IB2^,FSingleInputBufferSize);
    IB1+=FInputBufferChannelSize;
    IB2+=FInputBufferChannelSize;
  end;

  FAInputBufferIndex:=succ(FAInputBufferIndex) mod FInputBufferCount;
  for I:=0 to FChannels-1 do begin
    AInputBuffer:=FInputBuffer+(I*FInputBufferChannelSize)+FInputBufferChannelOffset+(FAInputBufferIndex*FSingleInputBufferSize);
    FLInputBuffers[I]:=FAInputBuffers[I];
    FAInputBuffers[I]:=AInputBuffer;
    for J:=0 to Length(FABuffers)-1
      do FABuffers[J][I]:=AInputBuffer-SD2BufElemSize-(FWBufferCounts[J]*SD2BufElemSize);
  end;
  FSyncTime+=FCompleteSyncTimerIncrement;
  DoAnalyse;
  if Assigned(FOnReceivedFrame) then FOnReceivedFrame(Self);
  FRecognized:=false;
end;

procedure TBasicSpectrumData.DoFilter(var ABuffer; const ASize: LongWord); inline;
begin
  FDoSetMasterVolume(ABuffer,ASize);
end;

procedure TBasicSpectrumData.F_Empty(var ABuffer; const ASize: LongWord); stdcall;
begin

end;

procedure TBasicSpectrumData.F_DoVolumize(var ABuffer; const ASize: LongWord); stdcall;
var
  I       : Integer;
  ABuffer2: TSD2BufferFormat absolute ABuffer;
begin
  for I:=0 to ASize-1 do ABuffer2[I]*=FMasterVolume;
end;

procedure TBasicSpectrumData.BeforeInit;
begin
  Lock;
  FInputStarted:=false;
end;

procedure TBasicSpectrumData.Launched(var ACounts: array of MVInt);
begin
  //do nothing;
end;

function TBasicSpectrumData.Init(const ASampleRate,AChannels,ASampleCount: MVInt): Boolean; stdcall;
begin
  Result:=((ASampleRate>0) and (AChannels>0) and (ASampleCount>0));
  if not Result then exit;
  BeforeInit;
  FChannels:=AChannels;
  FSampleRate:=ASampleRate;
  FInputBufferSampleCount:=ASampleCount;
  FSyncTimerIncrement:=1/ASampleRate;
  FCompleteSyncTimerIncrement:=FSyncTimerIncrement*ASampleCount;
  SetCounts(FWBufferCounts);
end;

procedure TBasicSpectrumData.Run;
begin
  //do nothing
end;

procedure TBasicSpectrumData.Stop;
begin
  //do nothing
end;

function TBasicSpectrumData.GetBuffer(const AChannel: MVInt): Pointer; stdcall;
begin
  Result:=FAInputBuffers[AChannel];
end;

function TBasicSpectrumData.GetLastBuffer(const AChannel: MVInt): Pointer; stdcall;
begin
  Result:=FLInputBuffers[AChannel];
end;

function TBasicSpectrumData.GetBufferPtr(const ABufferIndex,AChannel: MVInt): Pointer; stdcall;
begin
  Result:=FABuffers[ABufferIndex][AChannel];
end;

function TBasicSpectrumData.GetBuffer(const ABufferIndex,AChannel: MVInt): vpRealBuffer; stdcall;
begin
  Result:=BufferManager.ToVPRealBuffer(FWBufferCounts[ABufferIndex],GetBufferPtr(ABufferIndex,AChannel));
end;

function TBasicSpectrumData.GetWData(const ABufferIndex,AChannel,AIndex: MVInt): MVFloat;
var
  ABuf : TSD2WData;
  ABuf2: PSD2BufferFormat absolute ABuf;
begin
  ABuf:=FABuffers[ABufferIndex][AChannel];
  Result:=ABuf2^[AIndex];
end;

procedure TBasicSpectrumData.DoInit;
begin
  //Do nothing
end;

procedure TBasicSpectrumData.SetCounts(const ACounts: array of MVInt);
var
  I,J,L       : Integer;
  AInputBuffer: TSD2WData;
begin
  if not Locked then BeforeInit;
  FRecognized:=true;
  if FInputBufferSize>0 then FreeMem(FInputBuffer,FInputBufferSize);
  L:=Length(ACounts);
  SetLength(FWBufferCounts,L);
  for I:=0 to L-1 do begin
    FWBufferCounts[I]:=ACounts[I];
  end;
  Launched(FWBufferCounts);
  FLongestWBufferCount:=1;
  for I:=0 to L-1 do begin
    if FWBufferCounts[I]>FLongestWBufferCount then FLongestWBufferCount:=FWBufferCounts[I];
  end;
  FInputBufferCount:=FLongestWBufferCount div FInputBufferSampleCount;
  if FLongestWBufferCount mod FInputBufferSampleCount>0 then Inc(FInputBufferCount);
  FSingleInputBufferSize:=FInputBufferSampleCount*SD2BufElemSize;
  FInputBufferChannelOffset:=FInputBufferCount*FSingleInputBufferSize;
  FInputBufferChannelSize:=FInputBufferChannelOffset*2;
  FInputBufferSize:=FInputBufferChannelSize*FChannels;

  GetMem(FInputBuffer,FInputBufferSize);
  ZeroMVFloat(FInputBuffer,FInputBufferSampleCount*FInputBufferCount*2*FChannels);

  SetLength(FAInputBuffers,FChannels);
  SetLength(FLInputBuffers,FChannels);
  SetLength(FABuffers,L);
  for J:=0 to L-1 do SetLength(FABuffers[J],FChannels);
  for I:=0 to FChannels-1 do begin
    AInputBuffer:=FInputBuffer+(I*FInputBufferChannelSize)+FInputBufferChannelOffset;
    FAInputBuffers[I]:=AInputBuffer;
    FLInputBuffers[I]:=AInputBuffer-FSingleInputBufferSize;
    for J:=0 to L-1 do FABuffers[J][I]:=AInputBuffer-SD2BufElemSize-(ACounts[J]*SD2BufElemSize);
  end;

  FAInputBufferIndex:=0;
  FAInputBufferIndexPart:=0;

  DoInit;
  FInputStarted:=true;
  Unlock;
end;

function TBasicSpectrumData.GetChannels: MVInt; stdcall;
begin
  Result:=FChannels;
end;

function TBasicSpectrumData.GetSampleRate: MVInt; stdcall;
begin
  Result:=FSampleRate;
end;

procedure TBasicSpectrumData.Lock; stdcall;
begin
  Inc(FLocked);
end;

procedure TBasicSpectrumData.Unlock; stdcall;
begin
  Dec(FLocked);
end;

function TBasicSpectrumData.GetLocked: Boolean; stdcall;
begin
  Result:=(FLocked>0);
end;

procedure TBasicSpectrumData.SetMasterVolume(const Value: MVFloat);
begin
  FMasterVolume:=Value;
  if IsZero(FMasterVolume-1.0)
    then FDoSetMasterVolume:=@F_Empty
    else FDoSetMasterVolume:=@F_DoVolumize;
end;

function TBasicSpectrumData.GetMasterVolume: MVFloat;
begin
  Result:=FMasterVolume;
end;

procedure TBasicSpectrumData.Recognize; stdcall;
begin
  FRecognized:=true;
end;

{Allgemein}

function ssqr(const X: ValReal): ValReal; inline;
begin
  if X>=0
    then Result:=sqr(X)
    else Result:=-sqr(X);
end;

function cbe(const X: ValReal): ValReal; inline;
begin
  Result:=sqr(X)*X;
end;

function ssqrt(const X: ValReal): ValReal; inline;
begin
  if X>=0
    then Result:=sqrt(X)
    else Result:=-sqrt(-X);
end;

end.

