unit InfoMsgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InfoMsg, SyncObjs, ProgressHandler;

type
  TShortInfoMsg    = class (TInfoMsg)
  strict protected
    function IsFixed: Boolean; override;
    function IsHasProgress: Boolean; override;
    function IsHasTimeout: Boolean; override;
  end;

  TProgressInfoMsg = class (TInfoMsg)
  strict private
    FLock          : TCriticalSection;
    FProgress      : Word;
    FProgressString: string;
  strict protected
    function IsFixed: Boolean; override;
    function IsHasProgress: Boolean; override;
    function IsHasTimeout: Boolean; override;
    function GetProgress: Word; override;
    function GetProgressString: string; override;
    procedure DoInit; override;
  public
    destructor Destroy; override;
    procedure SetProgress(AProgress: Word; AProgressString: string);
    function GetProgressHandler: TProgressHandler;
  end;

implementation

{%REGION TShortInfoMsg}

function TShortInfoMsg.IsFixed: Boolean;
begin
  Result:=false;
end;

function TShortInfoMsg.IsHasProgress: Boolean;
begin
  Result:=false;
end;

function TShortInfoMsg.IsHasTimeout: Boolean;
begin
  Result:=true;
end;

{%ENDREGION}
{%REGION TInfoProgressHandler}

type
  TInfoProgressHandler = class (TProgressHandler)
  strict private
    FOwner: TProgressInfoMsg;
  strict protected
    procedure SetProgress(AProgress: Int64; AProgressString: string); override;
  public
    constructor Create(AOwner: TProgressInfoMsg);
  end;

constructor TInfoProgressHandler.Create(AOwner: TProgressInfoMsg);
begin
  inherited Create;
  FOwner:=AOwner;
end;

procedure TInfoProgressHandler.SetProgress(AProgress: Int64; AProgressString: string);
begin
  FOwner.SetProgress((AProgress * 65535) div Max, AProgressString);
end;

{%ENDREGION}
{%REGION TProgressInfoMsg}

procedure TProgressInfoMsg.DoInit;
begin
  FLock:=TCriticalSection.Create;
  FProgress:=0;
  FProgressString:='';
end;

destructor TProgressInfoMsg.Destroy;
begin
  FLock.Destroy;
  inherited Destroy;
end;

procedure TProgressInfoMsg.SetProgress(AProgress: Word; AProgressString: string);
begin
  FLock.Enter;

  FProgress:=AProgress;
  FProgressString:=AProgressString;

  FLock.Leave;

  if Assigned(OnProgress)
    then OnProgress(Self);
end;

function TProgressInfoMsg.IsFixed: Boolean;
begin
  Result:=true;
end;

function TProgressInfoMsg.IsHasProgress: Boolean;
begin
  Result:=true;
end;

function TProgressInfoMsg.IsHasTimeout: Boolean;
begin
  Result:=false;
end;

function TProgressInfoMsg.GetProgress: Word;
begin
  FLock.Enter;
  Result:=FProgress;
  FLock.Leave;
end;

function TProgressInfoMsg.GetProgressString: string;
begin
  FLock.Enter;
  Result:=FProgressString;
  FLock.Leave;
end;

function TProgressInfoMsg.GetProgressHandler: TProgressHandler;
begin
  Result:=TInfoProgressHandler.Create(Self);
end;

{%ENDREGION}

end.

