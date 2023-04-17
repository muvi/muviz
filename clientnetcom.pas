unit ClientNetCom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, NetCommander, Forms, lNet, lCommon,
  lNetComponents, AdvFunc, ExtCtrls, ClientNetType, DllStrUnit, DllStr,
  CommandListImpl2, CommandList, PluginType, MInterfacedObject;

const
  ProgName = 'Muvi Server';

  //nviInvalidCommand = '';
  nviHelp           = 'help';
  nviMessage        = 'message';
  nviAsk            = 'ask';
  nviChat           = 'chat';
  //nviClear          = 'clr';
  //nviGetName        = 'whoami';
  nviSetColor       = 'color';
  nviExit           = 'exit';
  nviBalloon        = 'balloon';
  nviCopyright      = '(C)';

type
  TNetClient          = class;

  TNetCommandList     = class (TCommandList, INetCommandList)
  public
    constructor Create({const ElemSize: Integer; }const InvalidData: TNetReaction); virtual; reintroduce;
    procedure AddCommand(const ACommand: ShortString; const AData: TNetReaction); stdcall;
    procedure OverrideCommand(const ACommand: ShortString; const AData: TNetReaction; out OldData: TNetReaction); stdcall;
    procedure GetCommand(const ACommand: ShortString; out AData: TNetReaction); stdcall;
  end;

  TConnectedEvent     = procedure of object;

  TNetClient          = class (TMInterfacedObject, INetClient)
  private
    FStringManager: TStringManager;

    FOwner        : TComponent;
    FNetCommander : TNetForm;
    FCommands     : TNetCommandList;
    FActive       : Boolean;
    FChatOnly     : Boolean;
    FDestroyStrMan: Boolean;
    FOnConnected  : TConnectedEvent;
    function GetActive: Boolean; stdcall;
    procedure SetActive(const Value: Boolean); stdcall;
    function GetChatOnly: Boolean; stdcall;
    procedure SetChatOnly(const Value: Boolean); stdcall;
    function GetPort: Word; stdcall;
    procedure SetPort(const Value: Word); stdcall;
    function GetIPAddress: ShortString; stdcall;
    procedure SetIPAddress(const AIPAddress: ShortString); stdcall;
    function GetCommands: INetCommandList; overload; stdcall;
    function GetCommands2: TNetCommandList; overload; stdcall;

    procedure DoExec(Command: string);
    procedure React(const Command: string; Params: string);
    procedure ReturnEndCommand;

    procedure FTCPError(const msg: string; aSocket: TLSocket);
    procedure FTCPConnect(aSocket: TLSocket);
    procedure FTCPReceive(aSocket: TLSocket);
    procedure FTCPDisconnect(aSocket: TLSocket);
    procedure FTCPCanSend(aSocket: TLSocket);

    procedure nvi_InvalidCommand(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Help(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Message(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Chat(ANet: INetClient; AParams: DString); stdcall;
    //procedure nvi_Clear(ANet: INetClient; AParams: DString); stdcall;
    //procedure nvi_GetName(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_SetColor(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Exit(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Balloon(ANet: INetClient; AParams: DString); stdcall;
    procedure nvi_Copyright(ANet: INetClient; AParams: DString); stdcall;
  protected
    FTCP         : TLTCPComponent;
    procedure Connected; virtual;
    procedure Disconnected; virtual;
    procedure NetError(const Msg: string); virtual;
    procedure ShowBalloon(const ATitle,AHint: string; AFlags: TBalloonFlags; const ATimeout: Integer); virtual;

    function NextParam(var Params: DString): ShortString; overload; stdcall;
    procedure Send(const Msg: DString); stdcall; overload;

    //function GetNewestInterface: INetClient; virtual; stdcall;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;

    {procedure AddCommand(const ACommand: ShortString; const AReaction: TNetReaction); stdcall;
    procedure RemoveCommand(const ACommand: ShortString); stdcall;
    function OverrideCommand(const ACommand: ShortString; const AReaction: TNetReaction): TNetReaction; stdcall;
    function GetCommand(const Index: Integer): ShortString; stdcall;
    procedure ClearCommands;}

    property Active: Boolean read FActive write SetActive;
    property ChatOnly: Boolean read FChatOnly write FChatOnly;
    property Commands: TNetCommandList read GetCommands2;
    property IPAddress: ShortString read GetIPAddress write SetIPAddress;
    property Port: Word read GetPort write SetPort;
  public
    constructor Create(AOwner: TComponent; const AStringManager: TStringManager = nil); virtual;
    destructor Destroy; override;

    {function GetLogLength: Integer; stdcall;
    function GetLogEntry(const Index: Integer): string; stdcall; overload;
    function GetLogEntry_D(const Index: Integer): DString; stdcall; overload;
    procedure ClearCommandLog; stdcall;}
    procedure Connect; stdcall;
    procedure Disconnect; stdcall;

    procedure Send(const Msg: string); overload;

    procedure ReceiveCommand(ACommand: string); stdcall;
    procedure ProcessCommand(ACommand: string); stdcall; overload;
    procedure ProcessCommand(const ACommand: DString); stdcall; overload;
    procedure ReturnCommand(const Command: string); stdcall; overload;
    procedure ReturnCommand(const Command: DString); stdcall; overload;

    {procedure INetClient.ProcessCommand = ProcessCommand_D;
    procedure INetClient.GetLogEntry = GetLogEntry_D; }

    procedure ShowCommander; stdcall;
    property StringManager: TStringManager read FStringManager;
    property OnConnected: TConnectedEvent read FOnConnected write FOnConnected;
  end;

const
  CommandLineCount = 200;

implementation

function Net_NextParam(var Params: AnsiString): ShortString; stdcall; forward;

{TNetCommandList}

constructor TNetCommandList.Create({const ElemSize: Integer; }const InvalidData: TNetReaction);
begin
  inherited Create({ElemSize,}SizeOf(TNetReaction),InvalidData);
end;

procedure TNetCommandList.AddCommand(const ACommand: ShortString; const AData: TNetReaction); stdcall;
begin
  inherited AddCommand(ACommand,AData);
end;

procedure TNetCommandList.OverrideCommand(const ACommand: ShortString; const AData: TNetReaction; out OldData: TNetReaction); stdcall;
begin
  inherited OverrideCommand(ACommand,AData,OldData);
end;

procedure TNetCommandList.GetCommand(const ACommand: ShortString; out AData: TNetReaction); stdcall;
begin
  inherited GetCommand(ACommand,AData);
end;

{TNetworkCom}

constructor TNetClient.Create(AOwner: TComponent; const AStringManager: TStringManager = nil);
begin
  inherited Create;
  FOwner:=AOwner;
  if AStringManager<>nil then begin
    FStringManager:=AStringManager;
    FDestroyStrMan:=false;
  end else begin
    FStringManager:=TStringManager.Create;
    FDestroyStrMan:=true;
  end;
  FActive:=false;
  FChatOnly:=true;
  FCommands:=TNetCommandList.Create({SizeOf(TNetReaction),}@nvi_InvalidCommand);
  FTCP:=TLTCPComponent.Create(AOwner);
  FTCP.OnReceive:=@FTCPReceive;
  FTCP.OnError:=@FTCPError;
  FTCP.OnDisconnect:=@FTCPDisconnect;
  FTCP.OnConnect:=@FTCPConnect;
  FTCP.OnCanSend:=@FTCPCanSend;

  FTCP.ReuseAddress:=true;
  FTCP.Timeout:=10000;
  FTCP.SocketNet:=LAF_INET;

  //FCommandStart:=0;
  Application.CreateForm(TNetForm,FNetCommander);
  FNetCommander.OnCommand:=@ProcessCommand;

  with FCommands do begin
    AddCommand(nviHelp,@nvi_Help);
    AddCommand(nviMessage,@nvi_Message);
    AddCommand(nviChat,@nvi_Chat);
    //AddCommand(nviClear,@nvi_Clear);
    //AddCommand(nviGetName,@nvi_GetName);
    AddCommand(nviSetColor,@nvi_SetColor);
    AddCommand(nviExit,@nvi_Exit);
    AddCommand(nviBalloon,@nvi_Balloon);
    AddCommand(nviCopyright,@nvi_Copyright);
  end;
end;

destructor TNetClient.Destroy;
begin
  if FActive then Disconnect;
  //if FKillTimer<>nil then FKillTimer.Destroy;
  FTCP.Destroy;
  FCommands.Destroy;
  if FDestroyStrMan then FStringManager.Destroy;
  inherited Destroy;
end;

procedure TNetClient.Connected;
begin
  //bleibt leer, um überschrieben zu werden
end;

procedure TNetClient.Disconnected;
begin
  //bleibt leer, um überschrieben zu werden
end;

procedure TNetClient.NetError(const Msg: string);
begin
  //bleibt leer, um überschrieben zu werden
end;

procedure TNetClient.ShowBalloon(const ATitle,AHint: string; AFlags: TBalloonFlags; const ATimeout: Integer);
begin
  //bleibt leer, um überschrieben zu werden
end;

function TNetClient.NextParam(var Params: DString): ShortString; overload; stdcall;
begin
  Result:=Net_NextParam(FStringManager.FStrings[Params]);
end;

procedure TNetClient.Send(const Msg: string);
begin
  FTCP.SendMessage(Msg);
end;

procedure TNetClient.Send(const Msg: DString); stdcall;
begin
  Send(FStringManager.FStrings[Msg]);
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TNetClient.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TNetClient.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(INetClient(Self))
    else Result:=inherited Future(Version);
end;

{function TNetworkCom.NextParam(var Params: AnsiString): ShortString; overload;
begin
  Result:=NetNextParam(Params);
end;}

procedure TNetClient.FTCPCanSend(aSocket: TLSocket);
begin

end;

procedure TNetClient.FTCPConnect(aSocket: TLSocket);
begin
  //ShowBalloon(ProgName,'Die Verbindung wurde hergestellt',bfInfo,2000);
  ReturnCommand('Connected');
  //FTCP.SendMessage(nviAuthentificate+CommandDivider+'Muvi',FTCP.Socks[0]);
  FActive:=true;
  Connected;
  if Assigned(FOnConnected) then FOnConnected;
end;

procedure TNetClient.FTCPError(const msg: string; aSocket: TLSocket);
begin
  ReturnCommand(msg);
  NetError(msg);
end;

{procedure TNetClient.FTCPAccept(aSocket: TLSocket);
begin
  ReturnCommand('Connection Accepted');
  Connected;
end;}

procedure TNetClient.FTCPReceive(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s)>0 then ReceiveCommand(s);
end;

procedure TNetClient.FTCPDisconnect(aSocket: TLSocket);
begin
  ReturnCommand('Connection lost');
  FActive:=false;
  Disconnected;
end;

procedure TNetClient.DoExec(Command: string);
begin
  if not FChatOnly then ReturnCommand('"'+Command+'"');
  React(Net_NextParam(Command),Command);
end;

procedure TNetClient.ReceiveCommand(ACommand: string); stdcall;
begin
  if Length(ACommand)=0 then exit;
  DoExec(ACommand);
end;

procedure TNetClient.ProcessCommand(ACommand: string); stdcall;
begin
  ReceiveCommand(ACommand);
end;

procedure TNetClient.ProcessCommand(const ACommand: DString); stdcall;
begin
  ReceiveCommand(FStringManager[ACommand]);
end;

procedure TNetClient.React(const Command: string; Params: string);
var
  AReaction: TNetReaction;
  AParams  : DString;
begin
  FCommands.GetCommand(Command,AReaction);
  with FStringManager do begin
    AParams:=NewString_(Params);
    AReaction(Self,AParams);
    DisposeStr(AParams);
  end;
end;

procedure TNetClient.Connect; stdcall;
begin
  if FActive then exit;
  FTCP.Connect(FTCP.Host,FTCP.Port);
end;

procedure TNetClient.Disconnect; stdcall;
begin
  FTCP.Disconnect;
  FTCPDisconnect(nil);
end;

function TNetClient.GetActive: Boolean; stdcall;
begin
  Result:=FActive;
end;

procedure TNetClient.SetActive(const Value: Boolean); stdcall;
begin
  if Value then Connect else Disconnect;
end;

{function TNetClient.GetAdress: Integer; stdcall;
begin
  Result:=FAdress;
end;}

function TNetClient.GetChatOnly: Boolean; stdcall;
begin
  Result:=FChatOnly;
end;

procedure TNetClient.SetChatOnly(const Value: Boolean); stdcall;
begin
  FChatOnly:=Value;
end;

{function TNetClient.GetIP: ShortString; stdcall;
begin
  Result:=FIP;
end;}

{procedure TNetClient.SetIP(const Value: ShortString); stdcall;
begin
  FIP:=Value;
end;}

{function TNetClient.GetIsServer: Boolean; stdcall;
begin
  Result:=FIsServer;
end;

procedure TNetClient.SetIsServer(const Value: Boolean); stdcall;
begin
  FIsServer:=Value;
end;}

{function TNetClient.GetOwnName: ShortString; stdcall;
begin
  Result:=SockName(FAdress);
end;}

function TNetClient.GetPort: Word; stdcall;
begin
  Result:=FTCP.Port;
end;

procedure TNetClient.SetPort(const Value: Word); stdcall;
begin
  FTCP.Port:=Value;
end;

function TNetClient.GetIPAddress: ShortString; stdcall;
begin
  Result:=FTCP.Host;
end;

procedure TNetClient.SetIPAddress(const AIPAddress: ShortString); stdcall;
begin
  FTCP.Host:=AIPAddress;
end;

function TNetClient.GetCommands: INetCommandList; stdcall;
begin
  Result:=FCommands;
end;

function TNetClient.GetCommands2: TNetCommandList; stdcall;
begin
  Result:=FCommands;
end;

{function TNetClient.GetLogLength: Integer; stdcall;
begin
  Result:=Length(FCommandLog);
end;

function TNetClient.GetLogEntry(const Index: Integer): string; stdcall;
begin
  Result:=FCommandLog[(FCommandStart+Index) mod Length(FCommandLog)];
end;

function TNetClient.GetLogEntry_D(const Index: Integer): DString; stdcall;
begin
  Result:=FStringManager.NewString(FCommandLog[(FCommandStart+1+Index) mod Length(FCommandLog)]);
end;}

{function TNetClient.GetLastSender: TLSocket;
begin
  //if Length(FLastSenders)<1 then Dialogs.ShowMessage('hilfe');
  Result:=FLastSenders[Length(FLastSenders)-1];
end;}

{procedure TNetClient.ClearCommandLog; stdcall;
begin
  FNetCommander.ClearCommander;
  FCommandStart:=0;
  SetLength(FCommandLog,0);
end;}

procedure TNetClient.ReturnCommand(const Command: string); stdcall;
var
  I: Integer;
begin
  {I:=Length(FCommandLog);
  if I<CommandLineCount then begin
    Inc(FCommandStart);
    SetLength(FCommandLog,I+1);
    FCommandLog[I]:=Command;
  end else begin
    FCommandStart:=(FCommandStart+1) mod I;
    FCommandLog[FCommandStart]:=Command;
  end;}
  if FNetCommander.Visible then with FNetCommander do begin
    for I:=0 to CommandMemo.Lines.Count-CommandLineCount do CommandMemo.Lines.Delete(0);
    CommandMemo.Lines.Add('  - '+Command);
  end;
end;

procedure TNetClient.ReturnCommand(const Command: DString); stdcall;
begin
  ReturnCommand(FStringManager[Command]);
end;

procedure TNetClient.ReturnEndCommand;
begin
  if FNetCommander.Visible then with FNetCommander.CommandMemo do begin
    Text:=Text+#$D#$A;
    SelStart:=Length(Text);
    SelLength:=0;
  end;
end;

procedure TNetClient.ShowCommander; stdcall;
var
  I: Integer;
begin
  with FNetCommander do begin
    Show;
    //CommandMemo.Lines.Clear;
    //for I:=0 to Length(FCommandLog)-1 do CommandMemo.Lines.Add(GetLogEntry(I));
  end;
end;

{TNetworkVisInstaller - Commands}

procedure TNetClient.nvi_InvalidCommand(ANet: INetClient; AParams: DString); stdcall;
begin
  {if FChatOnly then begin
    if FSTringManager[AParams]=''
      then ReturnCommand('"'+FLastCommand+'"')
      else ReturnCommand('"'+FLastCommand+CommandDivider+FStringManager[AParams]+'"');
  end;}
end;

{procedure TNetClient.nvi_Connected(ANet: INetwork; AParams: DString; const ASender: Integer); stdcall;
begin
  //FMuviIndex:=SrvSockIndex(Net_NextParam(AParams));
  //FMuviCount:=StrToInt(Net_NextParam(AParams));
  FAdress:=SockIndex(NextParam(AParams));
  Connected;
end;}

{procedure TNetClient.nvi_Authentificate(ANet: INetwork; AParams: DString; const ASender: Integer); stdcall;
begin
  SendToSender(nviConnected+CommandDivider+GetSenderName+CommandAdder);
  ShowBalloon(ProgName,'Die Verbindung mit Muvi an der Adresse '+LastSender.PeerAddress+' wurde hergestellt',bfInfo,2000);
end;}

procedure TNetClient.nvi_Help(ANet: INetClient; AParams: DString); stdcall;
var
  I       : Integer;
  ASendStr: string;
begin
  ASendStr:='HELP:';
  for I:=1 to FCommands.Count-1 do ASendStr+=#$D#$A+FCommands.Items[I];
  Send(ASendStr);
end;

procedure TNetClient.nvi_Message(ANet: INetClient; AParams: DString); stdcall;
begin
  ShowMessage(FStringManager[AParams]);
end;

procedure TNetClient.nvi_Chat(ANet: INetClient; AParams: DString); stdcall;
begin
  FChatOnly:=(FStringManager[AParams]<>'0');
end;

{procedure TNetClient.nvi_Clear(ANet: INetClient; AParams: DString); stdcall;
begin
  ClearCommandLog;
end;}

{procedure TNetClient.nvi_GetName(ANet: INetClient; AParams: DString); stdcall;
begin
  Send('youare'+CommandDivider+UserToStr(ASender),ASender);
end;}

procedure TNetClient.nvi_SetColor(ANet: INetClient; AParams: DString); stdcall;
var
  BG,FG: Integer;
begin
  BG:=StrToIntB(NextParam(AParams),16);
  FG:=StrToIntB(NextParam(AParams),16);
  FNetCommander.SetColors(BG,FG);
end;

procedure TNetClient.nvi_Exit(ANet: INetClient; AParams: DString); stdcall;
begin
  if NextParam(AParams)='noexit'
    then ShowCommander
    else FNetCommander.Close;
end;

{procedure TNetworkCom.nvi_Sleep(ANet: INetwork; AParams: DString; const ASender: Integer); stdcall;
var
  SleepTime: Integer;
begin
  if TryStrToInt(NextParam(AParams),SleepTime) then Sleep(SleepTime);
end;}

procedure TNetClient.nvi_Balloon(ANet: INetClient; AParams: DString); stdcall;
begin
  ShowBalloon(ProgName,FStringManager[AParams],bfNone,2000);
end;

procedure TNetClient.nvi_Copyright(ANet: INetClient; AParams: DString); stdcall;
begin
  Send('Muvi (c) 2010-2011 by Christian Baldus');
end;

{Allgemein}

{function Net_NextParam(var Params: string): string; stdcall;
var
  P: Integer;
begin
  if Length(Params)>0 then if Params[1]=CommandIntegrator then begin
    Delete(Params,1,1);
    P:=Pos(CommandIntegrator+CommandDivider,Params);
    if P>0 then begin
      Result:=Copy(Params,1,P-1);
      Delete(Params,1,P+1);
    end else begin
      Result:=Copy(Params,1,Length(Params)-1);
      Params:='';
    end;
    exit;
  end;
  P:=Pos(CommandDivider,Params);
  if P>0 then begin
    Result:=Copy(Params,1,P-1);
    Delete(Params,1,P);
  end else begin
    Result:=Params;
    Params:='';
  end;
end;  }

{function SocketIP(ASocket: TNetSocket): ShortString;
begin
  Result:=TLSocket(ASocket).PeerAddress;
end;

function SocketPort(ASocket: TNetSocket): Word;
begin
  Result:=TLSocket(ASocket).LocalPort;
end;

procedure DisconnectSocket(ASocket: TNetSocket);
begin
  TLSocket(ASocket).Disconnect;
end;}

{Allgemein}

function Net_NextParam(var Params: AnsiString): ShortString; stdcall;
var
  P: Integer;
begin
  if Length(Params)>0 then if Params[1]=CommandIntegrator then begin
    Delete(Params,1,1);
    P:=Pos(CommandIntegrator+CommandDivider,Params);
    if P>0 then begin
      Result:=Copy(Params,1,P-1);
      Delete(Params,1,P+1);
    end else begin
      Result:=Copy(Params,1,Length(Params)-1);
      Params:='';
    end;
    exit;
  end;
  P:=Pos(CommandDivider,Params);
  if P>0 then begin
    Result:=Copy(Params,1,P-1);
    Delete(Params,1,P);
  end else begin
    Result:=Params;
    Params:='';
  end;
end;

end.

