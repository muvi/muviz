unit AbstractNet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TASocket                 = TObject;
  TAReceiveEvent           = procedure (ASocket: TASocket; AMsg: Pointer; ASize: Cardinal) of object;
  TACreateAttachmentEvent  = function (ASocket: TASocket): TObject of object;

  TANet                    = class
  strict private
    FOnReceive         : TAReceiveEvent;
    FOnCreateAttachment: TACreateAttachmentEvent;
    procedure SetOnReceive(AOnReceive: TAReceiveEvent);
  strict protected
    procedure Connect; virtual;
    function GetAttachment(ASocket: TASocket): TObject; virtual; abstract;
    function SocketAdded(ASocket: TASocket): TObject;
  public
    procedure Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal); virtual; abstract;
    property Attachments[ASocket: TASocket]: TObject read GetAttachment; default;
    property OnCreateAttachment: TACreateAttachmentEvent read FOnCreateAttachment write FOnCreateAttachment;
    property OnReceive: TAReceiveEvent read FOnReceive write SetOnReceive;
  end;

implementation

{%REGION TANet}

procedure TANet.SetOnReceive(AOnReceive: TAReceiveEvent);
begin
  Assert(Assigned(AOnReceive));
  if Assigned(FOnReceive) then begin
    FOnReceive:=AOnReceive;
  end else begin
    FOnReceive:=AOnReceive;
    Connect;
  end;
end;

procedure TANet.Connect;
begin
  //do nothing;
end;

function TANet.SocketAdded(ASocket: TASocket): TObject;
begin
  if Assigned(FOnCreateAttachment)
    then Result:=FOnCreateAttachment(ASocket)
    else Result:=nil;
end;

{%ENDREGION}

end.

