unit TVSPClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPMsg, TCPClient, AbstractNet, TVSPType, TVSPConst,
  SyncObjs, TVSPIndexedValues, TVSPClientIndexer, VisType2, TVSPClientSources,
  TVSPSources;

type
  TTVSPClientError      = (teUNKNOWN, teNOTYPE, teUNKNOWNTYPE, teWRONGSIZE,
      teSERVERERROR);
  TTVSPReceiveListEvent = procedure (AList: array of TTVSPListEntry) of object;
  TTVSPCreateIndexEvent = function (AGroup: TGUID; AType: TPParamType; AName: string): TTVSPIndexedValue of object;
  TTVSPSubscribeEvent   = procedure (AGroup, AParent: TGUID) of object;

  ETVSPClientError      = class (Exception)
    constructor Create(AError: TTVSPClientError; AMsg: string = ''); reintroduce;
  end;

  TTVSPClient           = class
  strict private
    FNet            : TANet;
    FSources        : TTVSPClientSources;
    FSourceInterface: ITVSPSources;
    FValueIndex     : TTVSPClientIndexer;
    FOnReceiveList  : TTVSPReceiveListEvent;
    FOnCreateIndex  : TTVSPCreateIndexEvent;
  strict protected
    procedure Receive(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);

    procedure ClientAssert(ACorrect: Boolean; AError: TTVSPClientError = teUNKNOWN); inline;

    procedure ReceiveError(AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveListRet(AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveValIndexed(AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveValIndex(AMsg: Pointer; ASize: Cardinal);
    procedure ReceiveSrc(AMsg: Pointer; ASize: Cardinal);
  public
    constructor Create(AOwner: TComponent; AAddress: string; APort: Word);
    destructor Destroy; override;
    procedure UpdateList;
    procedure Subscribe(AGroup: TGUID);
    //call only if you create an index unrequested, no need to use in OnCreateIndex
    procedure RegisterIndex(AIndex: TTVSPIndexedValue);
    property Sources: TTVSPClientSources read FSources;
    property OnCreateIndex: TTVSPCreateIndexEvent read FOnCreateIndex write FOnCreateIndex;
    property OnReceiveList: TTVSPReceiveListEvent read FOnReceiveList write FOnReceiveList;
  end;

implementation

{%REGION ETVSPClientError}

constructor ETVSPClientError.Create(AError: TTVSPClientError; AMsg: string = '');
begin
  //determine message
  case AError of
    teNOTYPE, teUNKNOWNTYPE: AMsg:='Unknown message type.';
    teWRONGSIZE            : AMsg:='Invalid message size.';
    teSERVERERROR          : ;
    else                     AMsg:='Unknown message error.';
  end;
  inherited Create(AMsg);
end;

{%ENDREGION}
{%REGION TTVSPClient}
{%REGION General}

constructor TTVSPClient.Create(AOwner: TComponent; AAddress: string; APort: Word);
begin
  inherited Create;
  FValueIndex:=TTVSPClientIndexer.Create;
  FNet:=TTCPClient.Create(AOwner, AAddress, APort);
  FNet.OnReceive:=@Receive;
  FSources:=TTVSPClientSources.Create(FNet);
  FSourceInterface:=ITVSPSources(FSources);
end;

destructor TTVSPClient.Destroy;
begin
  FSourceInterface:=nil;
  FNet.Destroy;
  FValueIndex.Destroy;
  inherited Destroy;
end;

procedure TTVSPClient.Receive(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
var
  AMsgType: ^TTVSPMsgID;
begin
  ClientAssert(ASize>=SizeOf(AMsgType), teNOTYPE);
  AMsgType:=AMsg;
  case AMsgType^ of
    tcERROR      : ReceiveError(AMsg, ASize);
    tcLISTRET    : ReceiveListRet(AMsg, ASize);
    tcVALINDEXED : ReceiveValIndexed(AMsg, ASize);
    tcVALINDEX   : ReceiveValIndex(AMsg, ASize);
    tcSRC        : ReceiveSrc(AMsg, ASize);
    //tcSRCINDEXRET: ReceiveSrcIndexRet(AMsg, ASize);
    else raise ETVSPClientError.Create(teUNKNOWNTYPE);
  end;
end;

procedure TTVSPClient.ClientAssert(ACorrect: Boolean; AError: TTVSPClientError = teUNKNOWN); inline;
begin
  if not ACorrect
    then raise ETVSPClientError.Create(AError);
end;

{%ENDREGION}
{%REGION Receivers}

procedure TTVSPClient.ReceiveError(AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: ^TSMError;
begin
  AReceivedMsg:=AMsg;
  raise ETVSPClientError.Create(teServerError, 'Server Error: ' + FromTVSPString(AReceivedMsg^.Msg, ASize - SizeOf(TTVSPMsgID)));
end;

procedure TTVSPClient.ReceiveListRet(AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: ^TSMListRet;
  I, ACount   : Integer;
  AList       : array of TTVSPListEntry;
  AGroup      : PTVSPListEntry;
begin
  Assert(ASize>=SizeOf(TTVSPMsgID));
  Assert((ASize - SizeOf(TTVSPMsgID)) mod SizeOf(TTVSPListEntry) = 0);
  ACount:=(ASize - SizeOf(TTVSPMsgID)) div SizeOf(TTVSPListEntry);

  AReceivedMsg:=AMsg;
  SetLength(AList, ACount);
  AGroup:=@AReceivedMsg^.Groups;
  for I:=0 to ACount-1 do begin
    AList[I]:=AGroup^;
    Inc(AGroup);
  end;

  Assert(Assigned(FOnReceiveList));
  FOnReceiveList(AList);
end;

procedure TTVSPClient.ReceiveValIndexed(AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: ^TSMValIndexed;
  AItem       : TTVSPIndexedValue;
begin
  Assert(ASize >= TSMVALINDEXED_HEADERSIZE);
  AReceivedMsg:=AMsg;

  Assert(FValueIndex[AReceivedMsg^.Index] <> nil);
  AItem:=TTVSPIndexedValue(FValueIndex[AReceivedMsg^.Index]);
  Assert(ASize = TSMVALINDEXED_HEADERSIZE + AItem.MemSize);

  AItem.ReceiveChange(AMsg + TSMVALINDEXED_HEADERSIZE);
end;

procedure TTVSPClient.ReceiveValIndex(AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: ^TSMValIndex;
  ANewIndex   : TTVSPIndexedValue;
begin
  Assert(Assigned(FOnCreateIndex));

  AReceivedMsg:=AMsg;
  with AReceivedMsg^ do begin
    ANewIndex:=FOnCreateIndex(
        ValueID.GroupID,
        ValueID.&Type,
        FromTVSPString(ValueID.Name, ASize - TSMVALINDEX_HEADERSIZE));
    ANewIndex.InitNet(FNet);
    FValueIndex.AddIndexed(ANewIndex, Index);
  end;
end;

procedure TTVSPClient.ReceiveSrc(AMsg: Pointer; ASize: Cardinal);
var
  AReceivedMsg: ^TSMSrc;
begin
  AReceivedMsg:=AMsg;
  with AReceivedMsg^
    do TTVSPClientSource(Sources.NetItems[SrcID.Size, SrcID.ID]).NetSet(Start, Count, Value);
  //ShowMessage('Received source: ' + GUIDToString(AReceivedMsg^.SrcID.ID));
end;

{%ENDREGION}
{%REGION User methods}

procedure TTVSPClient.UpdateList;
var
  AMsg: TCMList;
begin
  Assert(Assigned(FOnReceiveList));
  AMsg.MsgID:=tcLIST;
  FNet.Send(nil, @AMsg, SizeOf(AMsg));
end;

procedure TTVSPClient.Subscribe(AGroup: TGUID);
var
  AMsg: TCMSubscribe;
begin
  AMsg:=MakeMsgSubscribe(AGroup);
  FNet.Send(nil, @AMsg, SizeOf(AMsg));
end;

procedure TTVSPClient.RegisterIndex(AIndex: TTVSPIndexedValue);
begin
  AIndex.InitNet(FNet);
end;

{%ENDREGION}
{%ENDREGION}

end.

