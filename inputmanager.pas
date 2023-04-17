unit InputManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WaveDest, AdvFunc, BasicFreqAna, GUIDop;

type
  TInputDesc    = record
    _Constructor: TNewInput;
    Name        : string;
    ID          : PGUID;
    IDSize      : Cardinal;
  end;

  PInputDesc    = ^TInputDesc;

  TDriverID     = record
    ID    : TGUID;
    Driver: PInputDesc;
  end;

  TInputManager = class
  private
    //Allgemein
    FSpectrumData    : TBasicSpectrumData;
    FDrivers         : array of TInputDesc;
    FDriverIDs       : array of TDriverID;
    //Treiberdaten
    FInput           : IFreqAnaInput2;
    FDriver          : Integer;
    //Gerätedaten
    FMachine         : Integer;
    FMachineCount    : Cardinal;
    FFeatures        : TfaiFeatures;
    //Zusatndsdaten
    FIsRunning       : Boolean;
    //Interaktion
    FOnAddDriver     : TNotifyEvent;
    procedure SetDriver(Value: Integer); overload;
    procedure SetDriver(Value: TGUID); overload;
    function GetDriverCount: Integer;
    function GetDriverName(Index: Integer): string;
    procedure SetMachine(Value: Integer);
    function GetMachineCount: Integer;
    function GetMachineName(Index: Integer): string;
    //Vergleichsfunktionen
    function DriverIDBigger(const Index: Integer; var FindData): Boolean;
    function DriverIDSmaller(const Index: Integer; var FindData): Boolean;
    function DriverIDEqual(const Index: Integer; var FindData): Boolean;
  protected
    function LoadDriver: Boolean;
    function LoadMachine: Boolean;
    function Run: Boolean;
    procedure CloseDriver;
    procedure CloseMachine;
    procedure Stop;
  public
    constructor Create(ASpectrumData: TBasicSpectrumData);
    destructor Destroy; override;
    procedure AddDriver(AName: string; AConstructor: TNewInput; AID: TGUID);
    procedure ShowControlPanel;
    property Driver: Integer read FDriver write SetDriver;
    property DriverCount: Integer read GetDriverCount;
    property DriverNames[Index: Integer]: string read GetDriverName;
    property Features: TfaiFeatures read FFeatures;
    property Machine: Integer read FMachine write SetMachine;
    property MachineCount: Integer read GetMachineCount;
    property MachineNames[Index: Integer]: string read GetMachineName;

    property OnAddDriver: TNotifyEvent read FOnAddDriver write FOnAddDriver;
  end;

implementation

{TInputManager}

constructor TInputManager.Create(ASpectrumData: TBasicSpectrumData);
begin
  inherited Create;
  FSpectrumData:=ASpectrumData;
  FDriver:=-1;
  FMachine:=-1;
  FInput:=nil;
  FIsRunning:=false;
  FFeatures:=[];
  FOnAddDriver:=nil;
end;

destructor TInputManager.Destroy;
begin
  SetLength(FDriverIDs,0);
  SetLength(FDrivers,0);
  if FInput<>nil then begin
    if FMachine>=0 then begin
      Stop;
      FInput.CloseDriver;
    end;
    FInput:=nil;
  end;
  inherited Destroy;
end;

procedure TInputManager.AddDriver(AName: string; AConstructor: TNewInput; AID: TGUID);
var
  I,L: Integer;
begin
  L:=Length(FDriverIDs);
  SetLength(FDriverIDs,L+1);
  SetLength(FDrivers,L+1);
  I:=L;
  while I>0 do begin
    if FDriverIDs[I].ID>AID
      then FDriverIDs[I+1]:=FDriverIDs[I]
      else break;
  end;
  with FDriverIDs[I] do begin
    ID:=AID;
    Driver:=@FDrivers[L];
  end;
  with FDrivers[L] do begin
    _Constructor:=AConstructor;
    Name:=AName;
    ID:=@FDriverIDs[I].ID;
    IDSize:=0;
  end;
  if Assigned(FOnAddDriver) then FOnAddDriver(Self);
end;

{TInputManager - Treiber Laden}

//vorher:
//- Es ist kein Treiber geladen
//- der zu ladende Treiberindex steht in FDriver
//nachher:
//- der Treiber ist geladen, falls es möglich war
//- Treiberinformationen sind gesetzt
function TInputManager.LoadDriver: Boolean;
begin
  if (FDriver>=0) and (FDriver<Length(FDrivers)) then begin
    FInput:=FDrivers[FDriver]._Constructor(FSpectrumData);
    Result:=(FInput<>nil);
    if Result then begin
      //get input info
      FFeatures:=FInput.Features;
      FMachineCount:=FInput.GetDriverCount;
    end else begin
      FInput:=nil;
      FDriver:=-1;
      //set empty input info
      FFeatures:=[];
      FMachineCount:=0;
    end;
  end else begin
    Result:=false;
    FInput:=nil;
    FDriver:=-1;
    //set empty input info
    FFeatures:=[];
    FMachineCount:=0;
  end;
end;

//vorher:
//- Es ist kein Gerät geladen
//- der zu ladende Geräteindex steht in FDevice
//nachher:
//- das Gerät ist geladen, falls es möglich war
//- Geräteinformationen sind gesetzt
function TInputManager.LoadMachine: Boolean;
begin
  //note: FInput=nil -> FMachineCount=0
  if (FMachine>=0) and (FMachine<FMachineCount) then begin
    Result:=FInput.SetDriver(FMachine);
    if Result then begin
      //set device info
    end else begin
        FMachine:=-1;
      //set empty device info
    end;
  end else begin
    Result:=false;
    FMachine:=-1;
    //set empty device info
  end;
end;

//vorher:
//- Der Treiber läuft nicht
//nachher:
//- der Treiber läuft, falls es möglich war
function TInputManager.Run: Boolean;
begin
  if FMachine<0 then exit;
  Result:=FInput.SetChannels(1);
  if Result then begin
    FSpectrumData.Init(FInput.GetPreferedSampleRate,1,FInput.GetPreferedBufferSize);
    Result:=FInput.Run;
    if Result then FSpectrumData.Run;
  end;
  FIsRunning:=Result;
end;

//vorher:
//- Es ist evtl. ein Treiber geladen
//- Es ist evtl. ein Gerät geladen
//- Der Treiber läuft evtl.
//nachher:
//- Es ist kein Treiber geladen
procedure TInputManager.CloseDriver;
begin
  if FInput=nil then exit;
  CloseMachine;
  FInput:=nil;
end;

//vorher:
//- Es ist evtl. ein Treiber geladen
//- Es ist evtl. ein Gerät geladen
//- Der Treiber läuft evtl.
//nachher:
//- Es ist kein Gerät geladen
procedure TInputManager.CloseMachine;
begin
  //note: FInput=nil -> FMachine=-1
  if (FMachine<0) then exit;
  Stop;
  FInput.CloseDriver;
end;

//vorher:
//- Es ist evtl. ein Treiber geladen
//- Es ist evtl. ein Gerät geladen
//- Der Treiber läuft evtl.
//nachher:
//- Es läuft kein Treiber
procedure TInputManager.Stop;
begin
  if not FIsRunning then exit;
  FSpectrumData.Stop;
  FInput.Stop;
  FIsRunning:=false;
end;

{TInputManager - Misc}

procedure TInputManager.ShowControlPanel;
begin
  if fifControlPanel in FFeatures then FInput.ShowControlPanel;
end;

procedure TInputManager.SetDriver(Value: Integer);
begin
  if Value=FDriver then exit;
  CloseDriver;
  FDriver:=Value;
  FMachine:=0;
  LoadDriver;
  LoadMachine;
  Run;
end;

procedure TInputManager.SetDriver(Value: TGUID);
begin
  SetDriver(BinarySearch(@DriverIDBigger,@DriverIDSmaller,@DriverIDEqual,Value,Length(FDriverIDs)-1));
end;

function TInputManager.GetDriverCount: Integer;
begin
  Result:=Length(FDrivers);
end;

function TInputManager.GetDriverName(Index: Integer): string;
begin
  if (Index>=0) and (Index<Length(FDrivers))
    then Result:=FDrivers[Index].Name;
end;

procedure TInputManager.SetMachine(Value: Integer);
begin
  if Value=FMachine then exit;
  CloseMachine;
  FMachine:=Value;
  LoadMachine;
  Run;
end;

function TInputManager.GetMachineCount: Integer;
begin
  if FInput<>nil
    then Result:=FInput.DriverCount
    else Result:=0;
end;

function TInputManager.GetMachineName(Index: Integer): string;
begin
  if FInput<>nil
    then Result:=FInput.Driver[Index].Name
    else Result:='';
end;

{TInputManager - Vergleichsmethoden}

function TInputManager.DriverIDBigger(const Index: Integer; var FindData): Boolean;
var
  AFindData: TGUID absolute FindData;
begin
  Result:=(FDriverIDs[Index].ID<AFindData);
end;

function TInputManager.DriverIDSmaller(const Index: Integer; var FindData): Boolean;
var
  AFindData: TGUID absolute FindData;
begin
  Result:=(FDriverIDs[Index].ID>AFindData);
end;

function TInputManager.DriverIDEqual(const Index: Integer; var FindData): Boolean;
var
  AFindData: TGUID absolute FindData;
begin
  Result:=(FDriverIDs[Index].ID=AFindData);
end;

end.

