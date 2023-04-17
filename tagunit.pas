unit TagUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvFunc, RefCounter, UIDArray;

(*
--- TAGS ---
TTags: Tags für ein Objekt
TTagSet: Sammlung von getaggten Objekten
TTag: Repräsentiert einen speziellen Tag. Ist zuweisungskompatibel zu string.
      NUR als Ergebnis von (TTagGroup)[new tag] verwenden.
      mehrere TTag's sind auch untereinander Zuweisungskompatibel. NewTag muss
      dann nicht verwendet werden.
      TTagGroup.Tags[str] erzeugt zu str einen neuen Tag
TTagGroup: Repräsentiert eine Menge von benutzbaren Tags. Die Tags in einem
           TagSet müssen aus genau einer TTagGroup stammen.
*)

type

  TTagGroup     = class;

  TTag          = packed object
  strict private
    FID      : TUID;
    FTagGroup: TTagGroup;
    FRefCount: RefCount;
  private
    procedure Init(AID: TUID; ATagGroup: TTagGroup; ARefCount: RefCount); inline;
    property ID: TUID read FID;
    property TagGroup: TTagGroup read FTagGroup;
  end;

  TTagArray     = array of TTag;
  TObjectArray  = array of TObject;
  TTags         = class;

  TTagSet       = class
  strict private
    FTaggedObjects: array of array of TTags;
    FTags         : array of TTag;
    FLock         : TMultiReadExclusiveWriteSynchronizer;
    function GetTagCount: Integer; inline;
    function GetTag(AIndex: Integer): TTag; inline;
    function GetTaggedObjectCount(ATag: TTag): Integer; inline;
    function GetTaggedObjectCountUnlocked(ATag: TTag): Integer; inline;
    function GetTaggedObject(ATag: TTag; AIndex: Integer): TObject; inline;
  private
    //executed, if ATag is added to ATags
    procedure Added(ATag: TTag; ATags: TTags); inline;
    //executed, if ATag is removed from ATags
    procedure Removed(ATag: TTag; ATags: TTags); inline;
  protected
    property Lock: TMultiReadExclusiveWriteSynchronizer read FLock;
  public
    constructor Create;
    destructor Destroy; override;
    //returns an array of all tags. Use this for iterating in multi-thread apps.
    function toArray: TTagArray;
    //returns an array of all objects with the given tag.
    //Use this for iterating in multi-thread apps.
    function toArray(ATag: TTag): TObjectArray;
    property TagCount: Integer read GetTagCount;
    property Tags[AIndex: Integer]: TTag read GetTag; default;
    property TaggedObjectCount[ATag: TTag]: Integer read GetTaggedObjectCount;
    property TaggedObjects[ATag: TTag; AIndex: Integer]: TObject read GetTaggedObject;
  end;

  TTags         = class
  strict private
    FTagSet: TTagSet;
    FObject: TObject;
    FTags  : array of TTag;
    FOrder : Integer;
    function GetTagCount: Integer; inline;
    function GetTag(AIndex: Integer): TTag; inline;
    function GetLock: TMultiReadExclusiveWriteSynchronizer; inline;
  protected
    property Lock: TMultiReadExclusiveWriteSynchronizer read GetLock;
    function DoHas(ATag: TTag): Boolean;
    procedure DoAdd(ATag: TTag);
    procedure DoRemove(ATag: TTag);
  public
    //Order: Reihenfolge in der dieses Objekt gefunden wird
    constructor Create(ATagSet: TTagSet; AObject: TObject; AOrder: Integer = 0);
    destructor Destroy; override;
    //überprüft, ob dieses Objekt einen bestimmten Tag hat.
    //Wird von AddTag und RemoveTag NICHT automatisch gemacht.
    function Has(ATag: TTag): Boolean; inline;
    //Adds a tag. Do never add a tag twice. If you are not shure, use Has()
    procedure Add(ATag: TTag); inline;
    //Removes a tag. Do never remove a non existing tag. If you are not shure, use Has()
    procedure Remove(ATag: TTag); inline;
    //Adds a tag if it does not exist.
    procedure AddSecure(ATag: TTag); inline;
    //Removes a tag if it exists.
    procedure RemoveSecure(ATag: TTag); inline;
    //returns an array of all tags. Use this for iterating in multi-thread apps.
    function ToArray: TTagArray;
    property &Object: TObject read FObject write FObject;
    property Order: Integer read FOrder;
    property TagCount: Integer read GetTagCount;
    property Tags[AIndex: Integer]: TTag read GetTag; default;
  end;

  TTagContainer = class (TRefCounter)
  strict private
    FStr  : string;
    FTagID: TUID;
  private
    procedure SetID(AID: TUID); inline;
  protected
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; override;
  public
    constructor Create(const AStr: string);
    property Str: string read FStr;
    property TagID: TUID read FTagID;
  end;

  TTagGroup     = class
  strict private
    FTags      : TUIDArray;
    FSortedTags: array of TTagContainer;
    FLock      : TMultiReadExclusiveWriteSynchronizer;
    function GetString(ATag: TTag): string; inline;
    function GetTag(AStr: string): TTag; inline;
    procedure ContainerRefZero(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property Strings[ATag: TTag]: string read GetString;
    property Tags[AStr: string]: TTag read GetTag; default;
  end;

operator < (m1,m2: TTags): Boolean; inline;
operator < (m1,m2: TTag): Boolean; inline;
operator = (m1,m2: TTag): Boolean; inline;
operator < (m1,m2: TTagContainer): Boolean; inline;
operator + (m1: TTags; m2: TTag): TTags; inline;
operator - (m1: TTags; m2: TTag): TTags; inline;
operator := (m1: TTag): string;


implementation

{$I advfunc.inc}

type
  TTagsFunc                = specialize TArrayFunc<TTags>;
  TTagFunc                 = specialize TArrayFunc<TTag>;
  TTagOperatorFunc         = specialize TOperatorFunc<TTagArray,TTag>;
  TTagContainerArray       = array of TTagContainer;
  TTagContainerFunc        = specialize TArrayFunc<TTagContainer>;
  TTagContainerOperatorFunc= specialize TOperatorFunc<TTagContainerArray,TTagContainer>;

{%REGION TTag}

procedure TTag.Init(AID: TUID; ATagGroup: TTagGroup; ARefCount: RefCount); inline;
begin
  FID:=AID;
  FTAGGroup:=ATagGroup;
  FRefCount:=ARefCount;
end;

{%ENDREGION}
{%REGION TTagSet}

constructor TTagSet.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TTagSet.Destroy;
var
  I: Integer;
begin
  FLock.Beginwrite;
  for I:=0 to Length(FTaggedObjects)-1
    do SetLength(FTaggedObjects[I],0);
  SetLength(FTaggedObjects,0);
  SetLength(FTags,0);
  FLock.Endwrite;
  FLock.Destroy;
  inherited Destroy;
end;

function TTagSet.GetTagCount: Integer; inline;
begin
  FLock.Beginread;
  Result:=Length(FTags);
  FLock.Endread;
end;

function TTagSet.GetTag(AIndex: Integer): TTag; inline;
begin
  FLock.Beginread;
  Result:=FTags[AIndex];
  FLock.Endread;
end;

function TTagSet.GetTaggedObjectCountUnlocked(ATag: TTag): Integer; inline;
begin
  if Length(FTaggedObjects)>ATag.ID
    then Result:=Length(FTaggedObjects[ATag.ID])
    else Result:=0;
end;

function TTagSet.GetTaggedObjectCount(ATag: TTag): Integer; inline;
begin
  FLock.Beginread;
  Result:=GetTaggedObjectCountUnlocked(ATag);
  FLock.Endread;
end;

function TTagSet.GetTaggedObject(ATag: TTag; AIndex: Integer): TObject; inline;
begin
  FLock.Beginread;
  Result:=FTaggedObjects[ATag.ID][AIndex].&Object;
  FLock.Endread;
end;

procedure TTagSet.Added(ATag: TTag; ATags: TTags); inline;
begin
  //do not lock here! this is called from locked methods only.
  if GetTaggedObjectCountUnlocked(ATag)=0 then begin
    //expand
    if Length(FTaggedObjects)<=ATag.ID
      then SetLength(FTaggedObjects, ATag.ID+1);
    TTagFunc.InsertInAscending(FTags,ATag);
  end;
  TTagsFunc.InsertInAscending(FTaggedObjects[ATag.ID],ATags);
end;

procedure TTagSet.Removed(ATag: TTag; ATags: TTags); inline;
var
  I: Integer;
begin
  //do not lock here! this is called from locked methods only.
  TTagsFunc.RemoveFromAscending(FTaggedObjects[ATag.ID],ATags);
  if GetTaggedObjectCountUnlocked(ATag)=0 then begin
    TTagFunc.RemoveFromAscending(FTags,ATag);
    //shrink
    if ATag.ID=Length(FTaggedObjects) then begin
      I:=ATag.ID-1;
      while (I>=0) and (Length(FTaggedObjects[I])=0)
        do Dec(I);
      SetLength(FTaggedObjects, I+1);
    end;
  end;
end;

function TTagSet.toArray: TTagArray;
var
  I: Integer;
begin
  Lock.Beginread;
  //copy to avoid threading issues
  SetLength(Result, Length(FTags));
  for I:=0 to Length(FTags)-1
    do Result[I]:=FTags[I];
  Lock.Endread;
end;

function TTagSet.toArray(ATag: TTag): TObjectArray;
var
  I,L: Integer;
begin
  Lock.Beginread;
  //copy to avoid threading issues
  L:=GetTaggedObjectCountUnlocked(ATag);
  SetLength(Result, L);
  for I:=0 to L-1
    do Result[I]:=FTaggedObjects[ATag.ID][I].&Object;
  Lock.Endread;
end;

{%ENDREGION}
{%REGION TTags}

constructor TTags.Create(ATagSet: TTagSet; AObject: TObject; AOrder: Integer = 0);
begin
  inherited Create;
  FTagSet:=ATagSet;
  FObject:=AObject;
  FOrder:=AOrder;
end;

destructor TTags.Destroy;
begin
  SetLength(FTags,0);
  inherited Destroy;
end;

function TTags.GetLock: TMultiReadExclusiveWriteSynchronizer; inline;
begin
  Result:=FTagSet.Lock;
end;

function TTags.GetTagCount: Integer; inline;
begin
  Lock.Beginread;
  Result:=Length(FTags);
  Lock.Endread;
end;

function TTags.GetTag(AIndex: Integer): TTag; inline;
begin
  Lock.Beginread;
  Result:=FTags[AIndex];
  Lock.Endread;
end;

function TTags.DoHas(ATag: TTag): Boolean;
begin
  //otherwise the BinarySearch attempts to read at index -1
  if (Length(FTags)=0) then begin
    Result:=false;
    exit;
  end;
  Result:=(TTagOperatorFunc.BinarySearch(ATag,FTags,Length(FTags)-1) >= 0);
end;

procedure TTags.DoAdd(ATag: TTag);
begin
  TTagFunc.InsertInAscending(FTags,ATag);
  FTagSet.Added(ATag,Self);
end;

procedure TTags.DoRemove(ATag: TTag);
begin
  TTagFunc.RemoveFromAscending(FTags,ATag);
  FTagSet.Removed(ATag,Self);
end;

function TTags.Has(ATag: TTag): Boolean; inline;
begin
  Lock.Beginread;
  Result:=DoHas(ATag);
  Lock.Endread;
end;

procedure TTags.Add(ATag: TTag); inline;
begin
  Lock.Beginwrite;
  DoAdd(ATag);
  Lock.Endwrite;
end;

procedure TTags.Remove(ATag: TTag); inline;
begin
  Lock.Beginwrite;
  DoRemove(ATag);
  Lock.Endwrite;
end;

procedure TTags.AddSecure(ATag: TTag); inline;
begin
  Lock.Beginwrite;
  if not DoHas(ATag)
    then DoAdd(ATag);
  Lock.Endwrite;
end;

procedure TTags.RemoveSecure(ATag: TTag); inline;
begin
  Lock.Beginwrite;
  if DoHas(ATag)
    then DoRemove(ATag);
  Lock.Endwrite;
end;

function TTags.ToArray: TTagArray;
var
  I: Integer;
begin
  Lock.Beginread;
  //copy to avoid threading issues
  SetLength(Result, Length(FTags));
  for I:=0 to Length(FTags)-1
    do Result[I]:=FTags[I];

  Lock.Endread;
end;

{%ENDREGION}
{%REGION TTagContainer}

constructor TTagContainer.Create(const AStr: string);
begin
  inherited Create;
  FStr:=AStr;
end;

function TTagContainer._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=inherited _Release;
  if Count<1 then Destroy;
end;

procedure TTagContainer.SetID(AID: TUID); inline;
begin
  FTagID:=AID;
end;

{%ENDREGION}
{%REGION TTagGroup}

constructor TTagGroup.Create;
begin
  inherited Create;
  SetLength(FSortedTags, 0);
  FTags:=TUIDArray.Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TTagGroup.Destroy;
var
  I: Integer;
begin
  FLock.Beginwrite;
  for I:=0 to Length(FSortedTags)-1
    do FSortedTags[I].OnZero:=nil;
  SetLength(FSortedTags,0);
  FTags.SoftClear;
  FTags.Destroy;
  FLock.Endwrite;
  FLock.Destroy;
  inherited Destroy;
end;

function TTagGroup.GetString(ATag: TTag): string; inline;
begin
  FLock.Beginread;
  Result:=TTagContainer(FTags[ATag.ID]).Str;
  FLock.Endread;
end;

function TTagGroup.GetTag(AStr: string): TTag; inline;
var
  AContainer: TTagContainer;
  AIndex,L  : Integer;
begin
  //just a simple implementation
  //TODO: maybe implement try reading later
  FLock.Beginwrite;
  L:=Length(FSortedTags);
  //otherwise BinarySearch will access the element index -1
  if L>=1 then begin
    //temporären Container erzeugen
    AContainer:=TTagContainer.Create(AStr);
    AIndex:=TTagContainerOperatorFunc.BinarySearch(AContainer,FSortedTags,L-1);
    AContainer.Destroy;
  end else AIndex:=-1;
  if AIndex>=0 then AContainer:=FSortedTags[AIndex] else begin
    //neue Tag-Sorte zu diesem Tag erstellen
    AContainer:=TTagContainer.Create(AStr);
    AContainer.SetID(FTags.AddItem(AContainer));
    TTagContainerFunc.InsertInAscending(FSortedTags,AContainer);
    AContainer.OnZero:=@ContainerRefZero;
  end;
  Result.Init(AContainer.TagID,Self,AContainer.Counter);
  FLock.Endwrite;
end;

//löscht eine Tag-Sorte
procedure TTagGroup.ContainerRefZero(Sender: TObject);
var
  AContainer: TTagContainer;
begin
  AContainer:=TTagContainer(Sender);

  FLock.Beginwrite;
  FTags.RemoveItem(AContainer.TagID);
  TTagContainerFunc.RemoveFromAscending(FSortedTags,AContainer);
  FLock.Endwrite;
end;

{%ENDREGION}
{%REGION Operators}

operator < (m1,m2: TTags): Boolean; inline;
begin
  Result:=(m1.Order<m2.Order);
end;

operator < (m1,m2: TTag): Boolean; inline;
begin
  Result:=m1.TagGroup.Strings[m1] < m2.TagGroup.Strings[m2];
end;

operator = (m1,m2: TTag): Boolean; inline;
begin
  Result:=(m1.ID=m2.ID) and (m1.TagGroup = m2.TagGroup);
end;

operator < (m1,m2: TTagContainer): Boolean; inline;
begin
  Result:=(m1.Str<m2.Str);
end;

operator + (m1: TTags; m2: TTag): TTags; inline;
begin
  m1.AddSecure(m2);
  Result:=m1;
end;

operator - (m1: TTags; m2: TTag): TTags; inline;
begin
  m1.RemoveSecure(m2);
  Result:=m1;
end;

operator := (m1: TTag): string;
begin
  Result:=m1.TagGroup.Strings[m1];
end;

{%ENDREGION}


end.

