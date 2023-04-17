unit AuthNetUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClientNetCom, AuthNetType, PluginType, ClientNetType,
  DllStr, DllStrUnit, NetEventDelegates;

const
  SIDLength       = 40;

type
  TSID            = packed array [0..SIDLength-1] of Char;
  TNetRequestEvent= procedure (const ARequest: string; AState: TNetState) of object;

  TAuthClient     = class (TNetClient, IAuthClient)
  private
    FState            : TNetState;
    FLastRequest      : ShortString;
    FSID              : TSID;
    FNetEventCallbacks: TNetEventCallbacks;
    FOnFinishRequest  : TNetRequestEvent;
    //function GetState: TNetState; stdcall;
    //procedure SetState(const AState: TNetState); stdcall;
    function GetCallbacks: INetCallbacks; stdcall;
    function GetRequest: ShortString; stdcall;
    function GetState: TNetState; stdcall;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    procedure Connected; override;
    procedure Disconnected; override;
    procedure SendRequest(const Request: ShortString; const Params: string); inline; overload;
    procedure SendRequest(const Request: ShortString; const Params: DString); stdcall; overload;
    procedure FinishRequest(AState: TNetState); stdcall;

    procedure nvi_Ok(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Error(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_RightError(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_ReceiveSID(ANet: INetClient; AParams: DString); stdcall;

    //property State: TNetState read FState write FState;
    property Callbacks: INetCallbacks read GetCallbacks;
    property Request: ShortString read FLastRequest;
    property State: TNetState read FState;
  public
    constructor Create(AOwner: TComponent; const AStringManager: TStringManager = nil); override;
    destructor Destroy; override;

    procedure Auth(const AUsername,APW: ShortString); stdcall;
    procedure ReAuth; stdcall;
    procedure SIDRequest; stdcall;

    property Active;
    property IPAddress;
    property Port;
    property RequestState: TNetState read FState;

    property OnFinishRequest: TNetRequestEvent read FOnFinishRequest write FOnFinishRequest;
  end;

const
  nviOK            = '!ok';
  nviError         = '!err';
  nviRightError    = '!rme';
  nviReceiveSID    = 'rcsid';

  SidUserStr       = '/SID/';

  srvAuth          = 'auth';
  srvSID           = 'sid';

function StateOK(AState: TNetState): Boolean;

implementation

{TAuthClient}

constructor TAuthClient.Create(AOwner: TComponent; const AStringManager: TStringManager = nil);
begin
  inherited Create(AOwner,AStringManager);
  FSID:='';
  FState:=ansIdle;
  FNetEventCallbacks:=TNetEventCallbacks.Create;
  with FNetEventCallbacks do begin
    AddCallback('connected'); //0
    AddCallback('disconnected'); //1
    AddCallback('request_finished'); //2
  end;
  with Commands do begin
    AddCommand(nviOK,@nvi_OK);
    AddCommand(nviError,@nvi_Error);
    AddCommand(nviRightError,@nvi_RightError);
    AddCommand(nviReceiveSID,@nvi_ReceiveSID);
  end;
end;

destructor TAuthClient.Destroy;
begin
  FNetEventCallbacks.Destroy;
  inherited Destroy;
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TAuthClient.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TAuthClient.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IAuthClient(Self))
    else Result:=inherited Future(Version);
end;

procedure TAuthClient.Connected;
begin
  FNetEventCallbacks.Call(cidConnected,NetEventData);
end;

procedure TAuthClient.Disconnected;
begin
  FNetEventCallbacks.Call(cidDisconnected,NetEventData);
end;

procedure TAuthClient.SendRequest(const Request: ShortString; const Params: string); inline;
begin
  FinishRequest(ansCanceled);
  FState:=ansWaiting;
  FLastRequest:=Request;
  Send(Request+CommandDivider+Params);
end;

procedure TAuthClient.SendRequest(const Request: ShortString; const Params: DString); stdcall;
begin
  SendRequest(Request,StringManager[Params]);
end;

procedure TAuthClient.FinishRequest(AState: TNetState); stdcall;
begin
  if (FState<ansNotReady) or (FState>=ansOK) then exit;
  FState:=AState;
  FNetEventCallbacks.Call(cidRequestFinished,NetEventData);
  if Assigned(FOnFinishRequest) then FOnFinishRequest(FLastRequest,AState);
  FState:=ansIdle;
end;

function TAuthClient.GetCallbacks: INetCallbacks; stdcall;
begin
  Result:=FNetEventCallbacks;
end;

function TAuthClient.GetRequest: ShortString; stdcall;
begin
  Result:=FLastRequest;
end;

function TAuthClient.GetState: TNetState; stdcall;
begin
  Result:=FState;
end;

procedure TAuthClient.Auth(const AUsername,APW: ShortString); stdcall;
begin
  SendRequest(srvAuth,CommandIntegrator+AUsername+CommandIntegrator+CommandDivider+CommandIntegrator+APW+CommandIntegrator);
  {Send(srvAuth+CommandDivider+CommandIntegrator+AUsername+CommandIntegrator+CommandDivider+CommandIntegrator+APW+CommandIntegrator);
  FState:=ansWaiting;
  while FState=ansWaiting do Sleep(10);
  Result:=FState;
  FState:=ansIdle;}
end;

procedure TAuthClient.ReAuth; stdcall;
begin
  SendRequest(srvAuth,SidUserStr+CommandDivider+FSID);
  //Send(srvAuth+CommandDivider+SidUserStr+CommandDivider+FSID);
  //FState:=ansWaiting;
  //while FState=ansWaiting do Sleep(10);
  //Result:=(FState=ansOK);
  //FState:=ansIdle;
end;

procedure TAuthClient.SIDRequest; stdcall;
begin
  Send(srvSID);
end;

procedure TAuthClient.nvi_Ok(ANet: INetClient; AParams: DString); stdcall;
begin
  FinishRequest(ansOK);
end;

procedure TAuthClient.nvi_Error(ANet: INetClient; AParams: DString); stdcall;
begin
  FinishRequest(ansClientError);
end;

procedure TAuthClient.nvi_RightError(ANet: INetClient; AParams: DString); stdcall;
begin
  FinishRequest(ansNotAllowed);
end;

procedure TAuthClient.nvi_ReceiveSID(ANet: INetClient; AParams: DString); stdcall;
begin
  FSID:=NextParam(AParams);
end;

{Allgemein}

function StateOK(AState: TNetState): Boolean;
begin
  Result:=((AState>=ansOK) and (AState<ansReserved));
end;

end.

