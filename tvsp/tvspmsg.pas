unit TVSPMsg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPType, VisType2, TVSPConst, TVSPSources;

type
  TTVSPValueID        = packed record
    GroupID : TGUID;
    &Type   : TPParamType;
    Name    : TTVSPString; //dynamic size
  end;

  TTVSPListEntry      = TGUID;

  PTVSPListEntry      = ^TTVSPListEntry;

  //server messages

  TSMError           = packed record
    MsgID: TTVSPMsgID;
    Msg  : TTVSPString; //dynamic size
  end;

  TSMListRet         = packed record
    MsgID : TTVSPMsgID;
    Groups: {array [0..X] of} TTVSPListEntry; //dynamic size
  end;

  TSMValIndexed      = packed record
    MsgID : TTVSPMsgID;
    Index : TTVSPIndex;
    Value : Byte; //dynmic size
  end;

  PSMValIndexed      = ^TSMValIndexed;

  TSMValIndex        = packed record
    MsgID  : TTVSPMsgID;
    Index  : TTVSPIndex;
    ValueID: TTVSPValueID;
  end;

  PSMValIndex        = ^TSMValIndex;

  TSMSrc             = packed record
    MsgID  : TTVSPMsgID;
    SrcID  : TTVSPSrcID;
    Start  : UInt64;
    Count  : LongWord;
    Value  : Byte; //dynamic size
  end;

  PSMSrc             = ^TSMSrc;

  TSMGrant           = packed record
    MsgID     : TTVSPMsgID;
    GroupID   : TGUID;
    Permission: TTVSPPermission;
  end;

  PSMGrant           = ^TSMGrant;

  TSMValGroup        = packed record
    MsgID  : TTVSPMsgID;
    GroupID: TGUID;
    &Type  : TPParamType;
    Name   : TTVSPString;
  end;

  PSMValGroup        = ^TSMValGroup;

  TSMDeleteValIndexed= packed record
    MsgID : TTVSPMsgID;
    Index : TTVSPIndex;
  end;

  PSMDeleteValIndexed= ^TSMDeleteValIndexed;

  //client messages

  TCMList            = packed record
    MsgID: TTVSPMsgID;
  end;

  TCMSubscribe       = packed record
    MsgID   : TTVSPMsgID;
    GroupID : TGUID;
  end;

  TCMValIndexed      = TSMValIndexed;

  TCMVal             = packed record
    MsgID    : TTVSPMsgID;
    ValueID  : TTVSPValueID //dynamicSize
    //Value    : Byte; //dynmic size
  end;

  TCMSrc             = TSMSrc;
  PCMSrc             = PSMSrc;
  TCMGrant           = TSMGrant;
  PCMGrant           = PSMGrant;

  TCMPermission      = packed record
    Permission: TTVSPPermission;
    Group     : TGUID;
  end;

  PCMPermission      = ^TCMPermission;

  TCMSubscribeVal    = packed record
    MsgID: TTVSPMsgID;
    &Type: TPParamType;
    Name : TTVSPString;
  end;

  PCMSubscribeVal    = ^TCMSubscribeVal;

  TCMDeleteVal       = packed record
    MsgID    : TTVSPMsgID;
    ValueID  : TTVSPValueID; //dynamicSize
  end;

  PCMDeleteVal       = ^TCMDeleteVal;
  TCMDeleteValIndexed= TSMDeleteValIndexed;
  PCMDeleteValIndexed= PSMDeleteValIndexed;

const
  TSMVALINDEXED_HEADERSIZE     = SizeOf(TSMValIndexed) - SizeOf(TSMValIndexed.Value);
  TSMVALINDEX_HEADERSIZE       = SizeOf(TSMValIndex) - SizeOf(TSMValIndex.ValueID.Name);
  TSMSRC_HEADERSIZE            = SizeOf(TSMSrc) - SizeOf(TSMSrc.Value);
  TCMVALGROUP_HEADERSIZE       = SizeOf(TSMValGroup) - SizeOf(TSMValGroup.Name);

  TCMVALINDEXED_HEADERSIZE     = SizeOf(TCMValIndexed) - SizeOf(TCMValIndexed.Value);
  TCMVAL_HEADERSIZE            = SizeOf(TCMVal) - SizeOf(TCMVal.ValueID.Name);
  TCMSRC_HEADERSIZE            = SizeOf(TCMSrc) - SizeOf(TCMSrc.Value);
  TCMSUBSCRIBEVAL_HEADERSIZE   = SizeOf(TCMSubscribeVal) - SizeOf(TCMSubscribeVal.Name);
  TCMDELETEVAL_HEADERSIZE      = SizeOf(TCMDeleteVal) - SizeOf(TCMDeleteVal.ValueID.Name);

function PrepareMsgValIndexed(AIndex: TTVSPIndex; AValueSize: Integer; APassive: Boolean; out AMsg: PSMValIndexed): Cardinal;
function MakeMsgValIndexed(AIndex: TTVSPIndex; const AValue; AValueSize: Integer; APassive: Boolean; out AMsg: PSMValIndexed): Cardinal;
function MakeMsgValIndex(AIndex: Cardinal; AGroupID: TGUID; AValueType: TPParamType; AValueName: string; out AMsg: PSMValIndex): Cardinal;
function PrepareMsgSrc(ASrcID: TGUID; ASrcSize, AStart, ACount: Cardinal; out AMsg: PSMSrc): Cardinal;
function MakeMsgSrc(ASrcID: TGUID; ASrcSize, AStart, ACount: Cardinal; const AValue; out AMsg: PSMSrc): Cardinal;
function MakeMsgSubscribe(AGroupID: TGUID): TCMSubscribe;
function MakeMsgGrant(AGroup: TGUID; APermission: TTVSPPermission): TCMGrant;
function MakeMsgSubscribeVal(AType: TPParamType; AName: string; out AMsg: PCMSubscribeVal): Cardinal;
function MakeMsgValGroup(AMsgID: TTVSPMsgID; AGroup: TGUID; AType: TPParamType; AName: string; out AMsg: PSMValGroup): Cardinal;
function MakeMsgDeleteVal(AGroup: TGUID; AType: TPParamType; AName: string; out AMsg: PCMDeleteVal): Cardinal;
function MakeMsgDeleteValIndexed(AIndex: TTVSPIndex): TCMDeleteValIndexed;

implementation

{%REGION msg creators}

function PrepareMsgValIndexed(AIndex: TTVSPIndex; AValueSize: Integer; APassive: Boolean; out AMsg: PSMValIndexed): Cardinal;
begin
  Result:=TSMVALINDEXED_HEADERSIZE + AValueSize;
  GetMem(AMsg, Result);
  with AMsg^ do begin
    if APassive
      then MsgID:=tcVALINDEXEDPASSIVE
      else MsgID:=tcVALINDEXED;
    Index:=AIndex;
  end;
end;

function MakeMsgValIndexed(AIndex: TTVSPIndex; const AValue; AValueSize: Integer; APassive: Boolean; out AMsg: PSMValIndexed): Cardinal;
begin
  Result:=PrepareMsgValIndexed(AIndex, AValueSize, APassive, AMsg);
  Move(AValue, AMsg^.Value, AValueSize);
end;

function MakeMsgValIndex(AIndex: Cardinal; AGroupID: TGUID; AValueType: TPParamType; AValueName: string; out AMsg: PSMValIndex): Cardinal;
begin
  Result:=TSMVALINDEX_HEADERSIZE + Length(AValueName);
  GetMem(AMsg, Result);

  with AMsg^ do begin
    MsgID:=tcVALINDEX;
    Index:=AIndex;
    ValueID.GroupID:=AGroupID;
    ValueID.&Type:=AValueType;
    ToTVSPString(AValueName, ValueID.Name);
  end;
end;

function PrepareMsgSrc(ASrcID: TGUID; ASrcSize, AStart, ACount: Cardinal; out AMsg: PSMSrc): Cardinal;
begin
  Result:=TSMSRC_HEADERSIZE + ACount;
  GetMem(AMsg, Result);

  with AMsg^ do begin
    MsgID:=tcSRC;
    SrcID.ID:=ASrcID;
    SrcID.Size:=ASrcSize;
    Start:=AStart;
    Count:=ACount;
  end;
end;

function MakeMsgSrc(ASrcID: TGUID; ASrcSize, AStart, ACount: Cardinal; const AValue; out AMsg: PSMSrc): Cardinal;
begin
  Result:=PrepareMsgSrc(ASrcID, ASrcSize, AStart, ACount, AMsg);
  Move(AValue, AMsg^.Value, ACount);
end;

function MakeMsgSubscribe(AGroupID: TGUID): TCMSubscribe;
begin
  with Result do begin
    MsgID:=tcSUBSCRIBE;
    GroupID:=AGroupID;
  end;
end;

function MakeMsgGrant(AGroup: TGUID; APermission: TTVSPPermission): TCMGrant;
begin
  with Result do begin
    MsgID:=tcGRANT;
    GroupID:=AGroup;
    Permission:=APermission;
  end;
end;

function MakeMsgSubscribeVal(AType: TPParamType; AName: string; out AMsg: PCMSubscribeVal): Cardinal;
begin
  Result:=TCMSUBSCRIBEVAL_HEADERSIZE + Length(AName);
  GetMem(AMsg, Result);
  with AMsg^ do begin
    MsgID:=tcSUBSCRIBEVAL;
    &Type:=AType;
    ToTVSPString(AName, Name);
  end;
end;

function MakeMsgValGroup(AMsgID: TTVSPMsgID; AGroup: TGUID; AType: TPParamType; AName: string; out AMsg: PSMValGroup): Cardinal;
begin
  Result:=TCMVALGROUP_HEADERSIZE + Length(AName);
  GetMem(AMsg, Result);
  with AMsg^ do begin
    MsgID:=AMsgID;
    GroupID:=AGroup;
    &Type:=AType;
    ToTVSPString(AName, Name);
  end;
end;

function MakeMsgDeleteVal(AGroup: TGUID; AType: TPParamType; AName: string; out AMsg: PCMDeleteVal): Cardinal;
begin
  Result:=TCMDELETEVAL_HEADERSIZE + Length(AName);
  GetMem(AMsg, Result);
  with AMsg^ do begin
    MsgID:=tcDELETEVAL;
    ValueID.GroupID:=AGroup;
    ValueID.&Type:=AType;
    ToTVSPString(AName, ValueID.Name);
  end;
end;

function MakeMsgDeleteValIndexed(AIndex: TTVSPIndex): TCMDeleteValIndexed;
begin
  with Result do begin
    MsgID:=tcDELETEVALINDEXED;
    Index:=AIndex;
  end;
end;

{%ENDREGION}

end.

