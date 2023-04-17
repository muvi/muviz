unit TVSPValues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPGroups, TVSPAbstractServerValue, VisType2,
  TVSPServerSources, TVSPServerError, TVSPServerAttachment, TVSPSources;

type
  PTVSPSrcID       = ^TTVSPSrcID;
  {
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
  }

  TTVSPBigValue    = class (TTVSPLockedValue)
  strict private
    FValue: Pointer;
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
    property Value: Pointer read FValue;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    destructor Destroy; override;
  end;

  TTVSPSourceValue = class (TTVSPBigValue)
  strict private
    function CheckSource(ASrcID: TTVSPSrcID): Boolean; inline;
    procedure Resubscribe(AOldID, ANewID: PTVSPSrcID; ASender: TTVSPServerAttachment);
    procedure Unsubscribe(AID: PTVSPSrcID);
  strict protected
    procedure DoGetValue(ASender: TTVSPServerAttachment; out Dest); override;
    procedure DoSetValue(ASender: TTVSPServerAttachment; const Src); override;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    destructor Destroy; override;
  end;

implementation

uses
  TVSPServerGroups;

{
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
}
{%REGION TTVSPBigValue}

constructor TTVSPBigValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AOwner, AName, AType);
  GetMem(FValue, MemSize);
end;

destructor TTVSPBigValue.Destroy;
begin
  FreeMem(FValue, MemSize);
  inherited Destroy;
end;

procedure TTVSPBigValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
begin
  Lock.Beginread;
  Move(FValue^, Dest, MemSize);
  Lock.Endread;
end;

procedure TTVSPBigValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
begin
  Lock.Beginwrite;
  Move(Src, FValue^, MemSize);
  Lock.Endwrite;
end;

{%ENDREGION}
{%REGION TTVSPSourceValue}

constructor TTVSPSourceValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
var
  I       : Integer;
  ASources: ^TTVSPSrcID;
begin
  inherited Create(AOwner, AName, AType);
  Assert(SourceCount > 0);
  ASources:=Value;
  for I:=0 to SourceCount-1 do begin
    ASources^:=ZEROSOURCEID;
    Inc(ASources);
  end;
end;

destructor TTVSPSourceValue.Destroy;
begin
  Unsubscribe(Value);
  inherited Destroy;
end;

procedure TTVSPSourceValue.DoGetValue(ASender: TTVSPServerAttachment; out Dest);
{
var
  I        : Integer;
  ASource  : TTVSPServerSource;
  ASourceID: ^TTVSPSrcID;
  }
begin
  inherited DoGetValue(ASender, Dest);
  {
  ASourceID:=@Dest;
  for I:=0 to SourceCount-1 do begin
    with ASourceID^
      do ASource:=Sources[Size, ID] as TTVSPServerSource;
    Assert(ASource <> nil);
    ASource.Subscribe(ASender);

    Inc(ASourceID);
  end;
  }
end;

procedure TTVSPSourceValue.DoSetValue(ASender: TTVSPServerAttachment; const Src);
var
  AOldSrcIDs, AOldID, ANewID: PTVSPSrcID;
  AOldSrcIDsSize, I         : Cardinal;
begin
  //check
  ANewID:=@Src;
  for I:=0 to SourceCount-1 do begin
    if not CheckSource(ANewID^)
      then raise ETVSPServerError.Create(teINVALIDSOURCE);
    Inc(ANewID);
  end;

  //update
  AOldSrcIDsSize:=SourceCount * SizeOf(TTVSPSrcID);
  GetMem(AOldSrcIDs, AOldSrcIDsSize);

  Lock.Beginwrite;
  Move(Value^, AOldSrcIDs^, AOldSrcIDsSize);
  Move(Src, Value^, MemSize);
  Lock.Endwrite;

  //resubscribe
  Resubscribe(AOldSrcIDs, @Src, ASender);

  FreeMem(AOldSrcIDs, AOldSrcIDsSize);
end;

procedure TTVSPSourceValue.Resubscribe(AOldID, ANewID: PTVSPSrcID; ASender: TTVSPServerAttachment);
var
  I                     : Cardinal;
  AOldSource, ANewSource: TTVSPServerSource;
  AIterator             : TObject;
  AAttachment           : TTVSPServerAttachment;
begin
  Assert(ASender <> nil);
  Assert(AOldID <> nil);
  Assert(ANewID <> nil);
  for I:=0 to SourceCount-1 do begin
    if AOldID^ <> ANewID^ then begin
      //get sources
      AOldSource:=Sources[AOldID^.Size, AOldID^.ID] as TTVSPServerSource;
      Assert(AOldSource <> nil);
      ANewSource:=Sources[ANewID^.Size, ANewID^.ID] as TTVSPServerSource;
      Assert(ANewSource <> nil);

      //resubscribe
      AIterator:=nil;
      AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
      while AAttachment <> nil do begin
        ANewSource.Subscribe(AAttachment, ASender <> AAttachment);
        AOldSource.Unsubscribe(AAttachment);
        AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
      end;
    end;
    Inc(AOldID);
    Inc(ANewID);
  end;
end;

procedure TTVSPSourceValue.Unsubscribe(AID: PTVSPSrcID);
var
  I          : Cardinal;
  ASource    : TTVSPServerSource;
  AIterator  : TObject;
  AAttachment: TTVSPServerAttachment;
begin
  for I:=0 to SourceCount-1 do begin
    //get sources
    ASource:=Sources[AID^.Size, AID^.ID] as TTVSPServerSource;

    //unsubscribe
    AIterator:=nil;
    AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);

    Assert((ASource <> nil) or (AAttachment = nil));

    while AAttachment <> nil do begin
      ASource.Unsubscribe(AAttachment);
      AAttachment:=TTVSPServerGroup(Owner).IterateSubscribers(AIterator);
    end;
    Inc(AID);
  end;
end;

function TTVSPSourceValue.CheckSource(ASrcID: TTVSPSrcID): Boolean; inline;
begin
  Result:=Sources[ASrcID.Size, ASrcID.ID] <> nil;
end;

{%ENDREGION}

end.

