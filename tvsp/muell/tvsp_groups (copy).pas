unit tvsp_Groups;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, HashMap, SyncObjs, MapKeys;

type
  TTVSPValue  = class (TStringKey)
  strict private
    FValue: Pointer;
    FSize : Cardinal;
    FLock : TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
  end;

  TTVSPGroup  = class (TGUIDKey)
  strict private
    FMap : TMap;
    FLock: TCriticalSection;
    function GetItem(AName: string): TTVSPValue;
  public
    constructor Create(AID: TGUID);
    destructor Destroy; override;
    property Items[AName: string]: TTVSPValue read GetItem; default;
  end;

  TTVSPGroups = class
  strict private
    FMap : TMap;
    FLock: TCriticalSection;
    function GetItem(AID: TGUID): TTVSPGroup;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[AID: TGUID]: TTVSPGroup read GetItem; default;
  end;

implementation

{%REGION TTVSPValue}

constructor TTVSPValue.Create(AName: string);
begin
  inherited Create(AName);
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FSize:=0;
  GetMem(FValue, 0);
end;

destructor TTVSPValue.Destroy;
begin
  FreeMem(FValue, FSize);
  FLock.Destroy;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTVSPGroup}

constructor TTVSPGroup.Create(AID: TGUID);
begin
  inherited Create(AID);
  FLock:=TCriticalSection.Create;
  FMap:=THashMap.Create;
end;

destructor TTVSPGroup.Destroy;
begin
  FMap.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

function TTVSPGroup.GetItem(AName: string): TTVSPValue;
var
  AKey: TStringKey;
begin
  AKey:=TStringKey.Create(AName);

  FLock.Enter;

  Result:=TTVSPValue(FMap[AKey]);
  if Result=nil then begin
    Result:=TTVSPValue.Create(AName);
    FMap.Add(Result, Result);
  end;

  FLock.Leave;
  AKey.Destroy;
end;

{%ENDREGION}
{%REGION TTVSPGroups}

constructor TTVSPGroups.Create;
begin
  inherited Create;
  FLock:=TCriticalSection.Create;
  FMap:=THashMap.Create;
end;

destructor TTVSPGroups.Destroy;
begin
  FMap.Clean;
  FMap.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

function TTVSPGroups.GetItem(AID: TGUID): TTVSPGroup;
var
  AKey : TGUIDKey;
begin
  AKey:=TGUIDKey.Create(AID);

  FLock.Enter;

  Result:=TTVSPGroup(FMap.Items[AKey]);
  if Result=nil then begin
    Result:=TTVSPGroup.Create(AID);
    FMap.Add(Result, Result);
  end;

  FLock.Leave;
  AKey.Destroy;
end;

{%ENDREGION}

end.

