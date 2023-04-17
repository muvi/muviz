unit TVSPValues;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, TVSPGroups, TVSPType, TVSPAbstractServerValue, VisType2,
  TVSPServerSources, TVSPServerError, TVSPServerAttachment, TVSPSources;

type
  TTVSP1ByteValue  = class (TTVSPServerValue)
  strict private
    FValue: Cardinal;
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
  end;

  TTVSP4ByteValue  = class (TTVSPServerValue)
  strict private
    FValue: Cardinal;
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
  end;

  TTVSP8ByteValue  = class (TTVSPLockedValue)
  strict private
    FValue: Int64;
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
  end;

  TTVSP16ByteValue = class (TTVSPLockedValue)
  strict private
    FValue: TGUID;
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
  end;

  TTVSPSourceValue = class (TTVSPLockedValue)
  strict private
    FValue: TTVSPSrcID;
    function CheckSource(ASrcID: TTVSPSrcID): TTVSPServerSource; inline;
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
  end;

implementation

uses
  TVSPServerGroups;

{%REGION TTVSP1ByteValue}

procedure TTVSP1ByteValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
var
  ANewVal: Cardinal;
begin
  InterLockedExchange(ANewVal, FValue);
  Byte(Dest):=ANewVal;
end;

procedure TTVSP1ByteValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
begin
  InterLockedExchange(FValue, Byte(Src));
end;

{%ENDREGION}
{%REGION TTVSP4ByteValue}

procedure TTVSP4ByteValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
var
  ANewVal: Cardinal;
begin
  InterLockedExchange(ANewVal, FValue);
  LongWord(Dest):=ANewVal;
end;

procedure TTVSP4ByteValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
begin
  InterLockedExchange(FValue, LongWord(Src));
end;

{%ENDREGION}
{%REGION TTVSP8ByteValue}

procedure TTVSP8ByteValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
begin
  Lock.Beginread;
  Int64(Dest):=FValue;
  Lock.Endread;
end;

procedure TTVSP8ByteValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
begin
  Lock.Beginwrite;
  FValue:=Int64(Src);
  Lock.Endwrite;
end;

{%ENDREGION}
{%REGION TTVSP16ByteValue}

procedure TTVSP16ByteValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
begin
  Lock.Beginread;
  TGUID(Dest):=FValue;
  Lock.Endread;
end;

procedure TTVSP16ByteValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
begin
  Lock.Beginwrite;
  FValue:=TGUID(Src);
  Lock.Endwrite;
end;

{%ENDREGION}
{%REGION TTVSPSourceValue}

constructor TTVSPSourceValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AOwner, AName, AType);
  FValue:=ZEROSOURCEID;
end;

procedure TTVSPSourceValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
var
  ASource: TTVSPServerSource;
begin
  Lock.Beginread;
  TTVSPSrcID(Dest):=FValue;
  Lock.Endread;

  with TTVSPSrcID(Dest)
    do ASource:=Sources[Size, ID] as TTVSPServerSource;
  Assert(ASource <> nil);
  ASource.Subscribe(ASender);
end;

procedure TTVSPSourceValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
var
  AOldValue             : TTVSPSrcID;
  AOldSource, ANewSource: TTVSPServerSource;
  AIterator             : TObject;
  AAttachment           : TTVSPServerAttachment;
begin
  Lock.Beginwrite;
  AOldValue:=FValue;
  FValue:=TTVSPSrcID(Src);
  Lock.Endwrite;

  //get sources
  {
  AOldSource:=Sources[ZEROSOURCEID.Size, ZEROSOURCEID.ID] as TTVSPServerSource;
  Assert(AOldSource <> nil);
  }
  AOldSource:=Sources[AOldValue.Size, AOldValue.ID] as TTVSPServerSource;
  Assert(AOldSource <> nil);
  ANewSource:=CheckSource(TTVSPSrcID(Src));

  //resubscribe
  AIterator:=nil;
  AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
  while AAttachment <> nil do begin
    AOldSource.Unsubscribe(AAttachment);
    ANewSource.Subscribe(AAttachment, ASender <> AAttachment);
    AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
  end;
end;

function TTVSPSourceValue.CheckSource(ASrcID: TTVSPSrcID): TTVSPServerSource; inline;
begin
  Result:=Sources[ASrcID.Size, ASrcID.ID] as TTVSPServerSource;
  if Result = nil
    then raise ETVSPServerError.Create(teINVALIDSOURCE);
end;

{%ENDREGION}

end.

