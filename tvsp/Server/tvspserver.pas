unit TVSPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TCPServer, TVSPGroups, TVSPServerSources, AbstractNet,
  TVSPConst, TVSPType, TVSPMsg, VisType2, TVSPServerAttachment, GUIDop,
  TVSPServerGroups, TVSPServerError, TVSPAbstractServerValue,
  TVSPServerPermissions, TVSPPermissions, TVSPParamGroupMap;

type
  TPermissiveReceiver   = procedure (ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission) of object;

  TTVSPServer           = class
  strict private
    FNet    : TANet;
  strict protected
    function CreateAttachment(ASocket: TASocket): TObject;
    procedure Receive(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    procedure ServerAssert(ACorrect: Boolean; AError: TTVSPServerError = teUNKNOWN);
    procedure HandleError(ASocket: TASocket; AException: Exception);
    //reactions
    procedure ReceiveList(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveSubscribe(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveVal(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission);
    procedure ReceiveValIndexed(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission);
    procedure ReceiveSrc(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission);
    procedure ReceiveGrant(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveSubscribeVal(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveDeleteVal(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveDeleteValIndexed(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);

    procedure ReceivePermission(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; AForwardTo: TPermissiveReceiver);
  public
    constructor Create(AOwner: TComponent; APort: Word);
    destructor Destroy; override;
  end;

implementation

{%REGION TTVSPServer}
{%REGION General}

constructor TTVSPServer.Create(AOwner: TComponent; APort: Word);
begin
  inherited Create;
  FNet:=TTCPServer.Create(AOwner, APort);
  FNet.OnReceive:=@Receive;
  FNet.OnCreateAttachment:=@CreateAttachment;
end;

destructor TTVSPServer.Destroy;
begin
  FNet.Destroy;
  inherited Destroy;
end;

function TTVSPServer.CreateAttachment(ASocket: TASocket): TObject;
begin
  Result:=TTVSPServerAttachment.Create(FNet, ASocket);
end;

procedure TTVSPServer.Receive(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AMsgType: ^TTVSPMsgID;
begin
  try
    ServerAssert(ASize>=SizeOf(TTVSPMsgID), teNOTYPE);
    AMsgType:=AMsg;
    case AMsgType^ of
      tcLIST               : ReceiveList(ASocket, AMsg, ASize);
      tcSUBSCRIBE          : ReceiveSubscribe(ASocket, AMsg, ASize);
      tcVAL                : ReceiveVal(ASocket, AMsg, ASize, nil);
      tcVALINDEXED         : ReceiveValIndexed(ASocket, AMsg, ASize, nil);
      tcSRC                : ReceiveSrc(ASocket, AMsg, ASize, nil);
      tcGRANT              : ReceiveGrant(ASocket, AMsg, ASize);
      tcVALPERMITTED       : ReceivePermission(ASocket, AMsg, ASize, @ReceiveVal);
      tcVALINDEXEDPERMITTED: ReceivePermission(ASocket, AMsg, ASize, @ReceiveValIndexed);
      tcSRCPERMITTED       : ReceivePermission(ASocket, AMsg, ASize, @ReceiveSrc);
      tcSUBSCRIBEVAL       : ReceiveSubscribeVal(ASocket, AMsg, ASize);
      tcDELETEVAL          : ReceiveDeleteVal(ASocket, AMsg, ASize);
      tcDELETEVALINDEXED   : ReceiveDeleteValIndexed(ASocket, AMsg, ASize);
      else raise ETVSPServerError.Create(teUNKNOWNTYPE);
    end;
  except
    on E: Exception
      do HandleError(ASocket, E);
  end;
end;

procedure TTVSPServer.ServerAssert(ACorrect: Boolean; AError: TTVSPServerError = teUNKNOWN);
begin
  if not ACorrect
    then raise ETVSPServerError.Create(AError);
end;

procedure TTVSPServer.HandleError(ASocket: TASocket; AException: Exception);
var
  ASendMsg : ^TSMError;
  ASendSize: Cardinal;
begin
  //send
  ASendSize:=SizeOf(TTVSPMsgID) + Length(AException.Message);
  GetMem(ASendMsg, ASendSize);
  with ASendMsg^ do begin
    MsgID:=tcERROR;
    ToTVSPString(AException.Message, Msg);
  end;
  FNet.Send(ASocket, ASendMsg, ASendSize);
  FreeMem(ASendMsg);
end;

{%ENDREGION}
{%REGION Receivers}

procedure TTVSPServer.ReceiveList(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  APosition         : TObject;
  ACurrent          : TTVSPBasicGroup;
  ASendMsg          : ^TSMListRet;
  ASendSize         : Cardinal;
  AGroup            : PTVSPListEntry;
begin
  ServerAssert(ASize = SizeOf(TCMList), teWRONGSIZE);
  APosition:=Groups.StartIterate;
  //create msg
  ASendSize:=SizeOf(TTVSPMsgID) + Groups.Count(APosition)*SizeOf(TTVSPListEntry);
  GetMem(ASendMsg, ASendSize);
  ASendMsg^.MsgID:=tcLISTRET;
  //fill msg

  ACurrent:=Groups.Iterate(APosition);
  AGroup:=@ASendMsg^.Groups;
  while ACurrent <> nil do begin
    AGroup^:=ACurrent.Key;
    ACurrent:=Groups.Iterate(APosition);
    Assert((ACurrent = nil) or (ACurrent.Key <> AGroup^));
    Inc(AGroup);
  end;
  //send
  FNet.Send(ASocket, ASendMsg, ASendSize);
  FreeMem(ASendMsg, ASendSize);
end;

procedure TTVSPServer.ReceiveSubscribe(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: ^TCMSubscribe;
  AGroup      : TTVSPServerGroup;
begin
  ServerAssert(ASize = SizeOf(TCMSubscribe), teWRONGSIZE);
  AReceivedMsg:=AMsg;
  AGroup:=Groups[AReceivedMsg^.GroupID] as TTVSPServerGroup;
  Assert(AGroup <> nil);
  if not AGroup.Subscribe(TTVSPServerAttachment(FNet[ASocket]))
    then raise ETVSPServerError.Create(teDOUBLESUBSCRIPTION, GUIDToString(AGroup.Key));
end;

procedure TTVSPServer.ReceiveValIndexed(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission);
var
  AReceivedMsg: ^TCMValIndexed;
  AValue      : TTVSPBasicValue;
begin
  ServerAssert(ASize >= TCMVALINDEXED_HEADERSIZE, teWRONGSIZE);
  AReceivedMsg:=AMsg;

  AValue:=Indexer[AReceivedMsg^.Index];
  ServerAssert(AValue <> nil, teINVALIDINDEX);
  ServerAssert(ASize = TCMVALINDEXED_HEADERSIZE + AValue.MemSize, teWRONGSIZE);

  TTVSPServerValue(AValue).&Set(TTVSPServerAttachment(FNet[ASocket]), AReceivedMsg^.Value, APermission);
end;

procedure TTVSPServer.ReceiveVal(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission);
var
  AReceivedMsg: ^TCMVal;
  AValue      : TTVSPBasicValue;
  ANameLength : Integer;
  AValueSize  : Integer;
begin
  ServerAssert(ASize > TCMVAL_HEADERSIZE, teWRONGSIZE);
  AReceivedMsg:=AMsg;

  with AReceivedMsg^.ValueID do begin
    AValueSize:=TVSPParamMemSize(&Type);
    ANameLength:=ASize - TCMVAL_HEADERSIZE - AValueSize;
    ServerAssert(ANameLength >= 0, teWRONGSIZE);
    AValue:=Groups.Items[GroupID][FromTVSPString(Name, ANameLength), &Type];
    //this has to be true. Otherwise the ANameLength should be negative
    Assert(ASize = TCMVAL_HEADERSIZE + ANameLength + AValue.MemSize);
  end;
  //no server assert, because this has to be true
  Assert(AValueSize = AValue.MemSize);

  TTVSPServerValue(AValue).&Set(TTVSPServerAttachment(FNet[ASocket]), (AMsg + ASize - AValueSize)^, APermission);
end;

procedure TTVSPServer.ReceiveSrc(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; APermission: PCMPermission);
var
  AReceivedMsg: ^TCMSrc;
  ASrc        : TTVSPServerSource;
begin
  ServerAssert(APermission = nil, teNOTIMPLEMENTED);

  ServerAssert(ASize >= TCMSRC_HEADERSIZE, teWRONGSIZE);
  AReceivedMsg:=AMsg;
  ServerAssert(ASize = TCMSRC_HEADERSIZE + AReceivedMsg^.Count, teWRONGSIZE);

  with AReceivedMsg^ do begin
    ASrc:=TTVSPServerSource(Sources.AddedItems[SrcID.Size, SrcID.ID]);
    ASrc.&Set(TTVSPServerAttachment(FNet[ASocket]), Start, Count, Value);
  end;
end;

procedure TTVSPServer.ReceiveGrant(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: PCMGrant;
begin
  ServerAssert(ASize = SizeOf(TCMGrant));
  AReceivedMsg:=AMsg;
  with AReceivedMsg^
    do Permissions.Grant(GroupID, Permission, TTVSPServerAttachment(FNet[ASocket]));
end;

procedure TTVSPServer.ReceiveSubscribeVal(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: PCMSubscribeVal;
begin
  ServerAssert(ASize >= TCMSUBSCRIBEVAL_HEADERSIZE, teWRONGSIZE);
  AReceivedMsg:=AMsg;
  with AReceivedMsg^
    do ParamGroupMap[FromTVSPString(Name, ASize - TCMSUBSCRIBEVAL_HEADERSIZE), &Type].Subscribe(TTVSPServerAttachment(FNet[ASocket]));
end;

procedure TTVSPServer.ReceiveDeleteVal(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: PCMDeleteVal;
  AValue      : TTVSPBasicValue;
begin
  ServerAssert(ASize >= TCMDELETEVAL_HEADERSIZE, teWRONGSIZE);

  AReceivedMsg:=AMsg;
  with AReceivedMsg^.ValueID do begin
    AValue:=Groups.ItemsOrNil[GroupID][FromTVSPString(Name, ASize - TCMDELETEVAL_HEADERSIZE), &Type];
    ServerAssert(AValue <> nil, teINVALIDPARAM);
    TTVSPServerValue(AValue).Delete(TTVSPServerAttachment(FNet[ASocket]));
  end;
end;

procedure TTVSPServer.ReceiveDeleteValIndexed(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: PCMDeleteValIndexed;
  AValue      : TTVSPIndexedValue;
begin
  ServerAssert(ASize = SizeOf(TCMDeleteVal), teWRONGSIZE);

  AReceivedMsg:=AMsg;
  AValue:=Indexer[AReceivedMsg^.Index];
  ServerAssert(AValue <> nil, teINVALIDINDEX);
  TTVSPServerValue(AValue).Delete(TTVSPServerAttachment(FNet[ASocket]));
end;

procedure TTVSPServer.ReceivePermission(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal; AForwardTo: TPermissiveReceiver);
var
  APermission: PCMPermission;
begin
  ServerAssert(ASize >= SizeOf(TCMPermission) + SizeOf(TTVSPMsgID), teWRONGSIZE);
  APermission:=AMsg + ASize - SizeOf(TCMPermission);
  ServerAssert(HasPermission(Permissions[APermission^.Group, APermission^.Permission], TTVSPServerAttachment(FNet[ASocket])), teNOTGRANTED);
  AForwardTo(ASocket, AMsg, ASize - SizeOf(TCMPermission), APermission);
end;

{%ENDREGION}
{%ENDREGION}

initialization
  TVSPParamGroupMap.Init;
  TVSPServerSources.Init;
  TVSPServerGroups.Init;
  TVSPGroups.Init;
finalization
  TVSPGroups.Done;
  TVSPServerGroups.Done;
  TVSPServerSources.Done;
  TVSPParamGroupMap.Done;
end.

