unit TagParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  TagParamTypeIntf, CallParamConverters, MStrings, TVSPSources, Doors,
  StdPermissions, CallParamType, MapKeys, HashMap, LinkedList, Enumerators;

type
  TVChangedTag           = TVChangedCall;

  TVTagValueStorage      = class (TPParamValueStorage, IPTag)
  protected
    procedure DoSet(AValue: TPChangedValue); override;
  public
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    //function Clone(AThread: IPThread = nil): IPParam; cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    procedure &Set; cdecl;
    procedure &Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property Param: IPParam read GetParam;
  end;

  TVTagTypeBase          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVTagType              = class;

  TVTagMapEntry          = class (TStringKey, IPTagEntry)
  strict private
    FOwner : TVTagType;
    FValues: TLinkedList;
    FLock  : TMultiReadExclusiveWriteSynchronizer;
    function GetTag: IString; cdecl;
  private
    function Add(AValueStorage: TVTagValueStorage): TEnumerator;
    procedure Remove(AValueStorage: TVTagValueStorage; var AEnumerator: TEnumerator);
  public
    constructor Create(AOwner: TVTagType; ATag: string);
    destructor Destroy; override;
    function IterateValues(var AIterator: Pointer): IPTag; cdecl;
  end;

  TVTagType              = class (TVTagTypeBase, IPTagParamType)
  strict private
    FTagMap: THashMap;
    FLock  : TMultiReadExclusiveWriteSynchronizer;
    function GetTag(ATag: IString): IPTagEntry; cdecl;
  private
    function Add(AValueStorage: TVTagValueStorage): TEnumerator;
    procedure Remove(AValueStorage: TVTagValueStorage; var AEnumerator: TEnumerator);
    procedure EntryDeleted(AEntry: TVTagMapEntry);
  public
    constructor Create;
    destructor Destroy; override;
    function IterateTags(var AIterator: Pointer): IPTagEntry; cdecl;
    property Tags[ATag: IString]: IPTagEntry read GetTag; default;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#137+'r'+#137+'r'+#137+'r'+#137+'r'+#137+'r'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#0#0#0+''+#137+'r'+#0+''+#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#0#0#137+'r'+#0#0#137+'r'+#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#0#0#0+''+#137+'r'+#0+''+#0#0#0#0#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#137+'r'+#0+'H'+#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#137+'r'+#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+'H'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVTagValueStorage}

procedure TVTagValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = TPParamType(vTag));
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedTag.Create(true), APermission, APermissionOwner, AEntryCode);
end;

procedure TVTagValueStorage.ToStream(out ADest); cdecl;
begin
  //do nothing
end;

procedure TVTagValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  RegisterChange(TVChangedTag.Create(true), APermission, APermissionOwner, AEntryCode);
end;

procedure TVTagValueStorage.&Set; cdecl;
begin
  &Set(TPNOLIMIT, NULLVISID, nil);
end;

procedure TVTagValueStorage.&Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedTag.Create(true), APermission, APermissionOwner, AEntryCode);
end;

function TVTagValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPTag(Self);
end;

procedure TVTagValueStorage.DoSet(AValue: TPChangedValue);
begin
  inherited DoSet(AValue);
end;

procedure TVTagValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(APermission, APermissionOwner, AEntryCode);
end;

{%ENDREGION}
{%REGION TVTagTypeBase}

function TVTagTypeBase.GetColor: TMColor; cdecl;
begin
  Result:=$FF006C
end;

function TVTagTypeBase.GetName: IString; cdecl;
begin
  Result:='Tag';
end;

function TVTagTypeBase.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVTagTypeBase.GetType: TPParamType; cdecl;
begin
  Result:=vTag;
end;

function TVTagTypeBase.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVTagValueStorage.Create(APrototype);
end;

function TVTagTypeBase.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall, vTag    : Result:=ADest;
    vInteger       : Result:=IPTag(TVIntegerToCall.Create(IPInteger(ADest)));
    vFloat         : Result:=IPTag(TVFloatToCall.Create(IPFloat(ADest)));
    vColor         : Result:=IPTag(TVColorToCall.Create(IPColor(ADest)));
    vBoolean       : Result:=IPTag(TVBooleanToCall.Create(IPBoolean(ADest)));
    vBuffer        : Result:=IPTag(TVBufferToCall.Create(IPBuffer(ADest)));
    vString        : Result:=IPTag(TVStringToCall.Create(IPString(ADest)));
    vPreset        : Result:=IPTag(TVPresetToCall.Create(IPPreset(ADest)));
    vPointer       : Result:=IPTag(TVPointerToCall.Create(IPPointer(ADest)));
    else             Result:=IPTag(TVDummyCallConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TVTagMapEntry}

constructor TVTagMapEntry.Create(AOwner: TVTagType; ATag: string);
begin
  inherited Create(ATag);
  FOwner:=AOwner;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FValues:=TLinkedList.Create;
end;

destructor TVTagMapEntry.Destroy;
begin
  Assert(FValues.Count = 0);
  FValues.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

function TVTagMapEntry.GetTag: IString; cdecl;
begin
  Result:=Key;
end;

function TVTagMapEntry.Add(AValueStorage: TVTagValueStorage): TEnumerator;
begin
  Assert(AValueStorage <> nil);

  FLock.Beginwrite;
  Result:=FValues.AddAt(AValueStorage);
  FLock.Endwrite;
end;

procedure TVTagMapEntry.Remove(AValueStorage: TVTagValueStorage; var AEnumerator: TEnumerator);
var
  ADeleteNow: Boolean;
begin
  Assert(AValueStorage <> nil);
  Assert(AEnumerator <> nil);

  FLock.Beginwrite;
  AEnumerator.Remove;
  ADeleteNow:=FValues.Count = 0;
  FLock.Endwrite;

  AEnumerator.Destroy;
  AEnumerator:=nil;

  if ADeleteNow
    then FOwner.EntryDeleted(Self);
end;

function TVTagMapEntry.IterateValues(var AIterator: Pointer): IPTag; cdecl;
var
  AEnumerator: TEnumerator;
begin
  if AIterator = nil then begin
    FLock.Beginread;
    AEnumerator:=FValues.Enumerator;
    AIterator:=AEnumerator;
  end else AEnumerator:=TEnumerator(AIterator);
  if AEnumerator.MoveNext then begin
    Result:=TVTagValueStorage(AEnumerator.Current);
  end else begin
    Result:=nil;
    AEnumerator.Destroy;
    AIterator:=nil;
    FLock.Endread;
  end;
end;

{%ENDREGION}
{%REGION TVTagType}

constructor TVTagType.Create;
begin
  inherited Create;
  FTagMap:=THashMap.Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TVTagType.Destroy;
begin
  FLock.Destroy;
  Assert(FTagMap.Count = 0);
  FTagMap.Destroy;
  inherited Destroy;
end;

function TVTagType.Add(AValueStorage: TVTagValueStorage): TEnumerator;
var
  AKey : TStringKey;
  AItem: TObject;
begin
  Assert(AValueStorage <> nil);

  AKey:=TStringKey.Create(AValueStorage.ID.Name);

  FLock.Beginwrite;

  AItem:=FTagMap.Items[AKey];
  if AItem = nil then begin
    AItem:=TVTagMapEntry.Create(Self, AValueStorage.ID.Name);
    FTagMap.Add(AItem, AItem);
    Result:=TVTagMapEntry(AItem).Add(AValueStorage);
  end else Result:=TVTagMapEntry(AItem).Add(AValueStorage);

  FLock.Endwrite;

  AKey.Destroy;
end;

procedure TVTagType.Remove(AValueStorage: TVTagValueStorage; var AEnumerator: TEnumerator);
var
  AKey : TStringKey;
  AItem: TObject;
begin
  Assert(AValueStorage <> nil);
  Assert(AEnumerator <> nil);

  AKey:=TStringKey.Create(AValueStorage.ID.Name);

  FLock.Beginwrite;

  AItem:=FTagMap.Items[AKey];
  Assert(AItem <> nil);
  TVTagMapEntry(AItem).Remove(AValueStorage, AEnumerator);

  FLock.Endwrite;

  AKey.Destroy;
end;

procedure TVTagType.EntryDeleted(AEntry: TVTagMapEntry);
var
  AResult: Boolean;
begin
  //Do not Lock here! This is called via Remove -> Entry.Remove -> EntryDeleted
  Assert(AEntry <> nil);

  AResult:=FTagMap.Delete(AEntry);
  Assert(AResult);
end;

function TVTagType.GetTag(ATag: IString): IPTagEntry; cdecl;
var
  AKey : TStringKey;
begin
  AKey:=TStringKey.Create(ATag);

  FLock.Beginread;
  Result:=FTagMap[AKey] as TVTagMapEntry;
  FLock.Endread;

  AKey.Destroy;
end;

function TVTagType.IterateTags(var AIterator: Pointer): IPTagEntry; cdecl;
var
  AEnumerator: TEnumerator;
begin
  if AIterator = nil then begin
    FLock.Beginread;
    AEnumerator:=FTagMap.Enumerator;
    AIterator:=AEnumerator;
  end else AEnumerator:=TEnumerator(AIterator);
  if AEnumerator.MoveNext then begin
    Result:=TVTagMapEntry(AEnumerator.Current);
  end else begin
    Result:=nil;
    AEnumerator.Destroy;
    AIterator:=nil;
    FLock.Endread;
  end;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPTagParamType(TVTagType.Create));
end;

{%ENDREGION}

end.

