unit TVSPAbstractServerValue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPGroups, VisType2, TVSPServerAttachment, TVSPMsg,
  TVSPPermissions, TVSPServerPermissions, TVSPParamGroupMap;

type
  TTVSPServerValue = class (TTVSPParamGroupedValue)
  strict private
    procedure SendIndexIfNecessary(AAttachment: TTVSPServerAttachment; var AMsg: PSMValIndex; var ASize: Cardinal);
    procedure Send(AAttachment: TTVSPServerAttachment; AValMsg: PSMValIndexed; AValMsgSize: Cardinal; var AIndexMsg: PSMValIndex; var AIndexMsgSize: Cardinal);
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); virtual; abstract;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); virtual; abstract;
  public
    procedure &Set(ASender: TTVSPServerAttachment; const Src; APermission: PCMPermission);
    procedure Send(AAttachment: TTVSPServerAttachment; APassive: Boolean);
    procedure Delete(ASender: TTVSPServerAttachment);
  end;

  TTVSPLockedValue = class (TTVSPServerValue)
  strict private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  strict protected
    property Lock: TMultiReadExclusiveWriteSynchronizer read FLock;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    destructor Destroy; override;
  end;

implementation

uses
  TVSPServerGroups;

{%REGION TTVSPServerValue}

procedure TTVSPServerValue.&Set(ASender: TTVSPServerAttachment; const Src; APermission: PCMPermission);
var
  AIterator        : TObject;
  AAttachment      : TTVSPServerAttachment;

  AMsg             : PSMValIndexed;
  AMsgSize         : Cardinal;

  AIndexMsg        : PSMValIndex;
  AIndexMsgSize    : Cardinal;

  APermissionValue : TTVSPPermissionValue;
begin
  DoSetValue(ASender, Src);
  AMsgSize:=MakeMsgValIndexed(Index, Src, MemSize, false, AMsg);
  AIndexMsg:=nil;

  if APermission <> nil
    then with APermission^
      do APermissionValue:=Permissions[Group, Permission];

  //publish change
  AIterator:=nil;
  AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
  while AAttachment <> nil do begin
    if (AAttachment <> ASender) and ((APermission = nil) or (not RequestedPermission(APermissionValue, AAttachment)))
      then Send(AAttachment, AMsg, AMsgSize, AIndexMsg, AIndexMsgSize);
    AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
  end;
  //send the index to the sender if necessary
  SendIndexIfNecessary(ASender, AIndexMsg, AIndexMsgSize);

  FreeMem(AMsg, AMsgSize);
  if AIndexMsg<>nil
    then FreeMem(AIndexMsg, AIndexMsgSize);
end;

procedure TTVSPServerValue.SendIndexIfNecessary(AAttachment: TTVSPServerAttachment; var AMsg: PSMValIndex; var ASize: Cardinal);
begin
  if not AAttachment.KnowsIndex(Index) then begin
    //create the msg only once
    if AMsg = nil
      then ASize:=MakeMsgValIndex(Index, Owner.Key, &Type, Key, AMsg);
    AAttachment.Send(AMsg, ASize);
    AAttachment.LearnIndex(Index);
  end;
end;

procedure TTVSPServerValue.Send(AAttachment: TTVSPServerAttachment; AValMsg: PSMValIndexed; AValMsgSize: Cardinal; var AIndexMsg: PSMValIndex; var AIndexMsgSize: Cardinal);
begin
  //send indices if necessary
  SendIndexIfNecessary(AAttachment, AIndexMsg, AIndexMsgSize);
  //publish the change
  AAttachment.Send(AValMsg, AValMsgSize);
end;

procedure TTVSPServerValue.Send(AAttachment: TTVSPServerAttachment; APassive: Boolean);
var
  AValMsg      : PSMValIndexed;
  AValMsgSize  : Cardinal;

  AIndexMsg    : PSMValIndex;
  AIndexMsgSize: Cardinal;
begin
  AValMsgSize:=PrepareMsgValIndexed(Index, MemSize, APassive, AValMsg);
  DoGetValue(AAttachment, AValMsg^.Value);
  AIndexMsg:=nil;

  Send(AAttachment, AValMsg, AValMsgSize, AIndexMsg, AIndexMsgSize);

  FreeMem(AValMsg, AValMsgSize);
  if AIndexMsg<>nil
    then FreeMem(AIndexMsg, AIndexMsgSize);
end;

procedure TTVSPServerValue.Delete(ASender: TTVSPServerAttachment);
var
  AMsg       : TSMDeleteValIndexed;
  AAttachment: TTVSPServerAttachment;
  AIterator  : TObject;
begin
  AMsg:=MakeMsgDeleteValIndexed(Index);

  AIterator:=nil;
  AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
  while AAttachment <> nil do begin
    if AAttachment <> ASender then begin
      Assert(AAttachment.KnowsIndex(Index));
      AAttachment.Send(@AMsg, SizeOf(AMsg));
    end;
    AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
  end;

  inherited Delete;
end;

{%ENDREGION}
{%REGION TTVSPLockedValue}

constructor TTVSPLockedValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AOwner, AName, AType);
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TTVSPLockedValue.Destroy;
begin
  FLock.Destroy;
  inherited Destroy;
end;

{%ENDREGION}

end.

