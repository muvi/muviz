unit BufferParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, BufferParamConverters, Buffers, MStrings, TVSPSources,
  TVSPSourceUtil, Doors, StdPermissions;

type
  TVChangedBuffer       = class (specialize TPCustomChangedValue<TVBuffer>, IChangedBuffer)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVBufferValueStorage  = class (TPParamValueStorage, IPBuffer)
  private
    FValue: TVBuffer;
    FLock : TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure DoSet(AValue: TPChangedValue); override;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    function Get: TVBuffer; cdecl;
    procedure &Set(Value: TVBuffer); cdecl;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property Param: IPParam read GetParam;
    property Value: TVBuffer read Get write &Set;
  end;

  TVBufferType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyBufferConverter= class (TVBufferParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVBuffer; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#255#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0#155#0;

{%REGION TVChangedBuffer}

function TVChangedBuffer.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedBuffer(Self);
end;

{%ENDREGION}
{%REGION TVBufferValueStorage}

constructor TVBufferValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  //FValue<>nil - in every case
  FValue:=DEFAULTBUFFER;
  FValue.Source._ValAddRef;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TVBufferValueStorage.Destroy;
begin
  Assert(FValue <> nil);
  FValue.Source._ValRelease;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TVBufferValueStorage.DoSet(AValue: TPChangedValue);
begin
  FLock.Beginwrite;
  FValue.Source._ValRelease;
  FValue:=TVChangedBuffer(AValue).Value;
  FValue.Source._ValAddRef;
  FLock.Endwrite;

  inherited DoSet(AValue);
end;

procedure TVBufferValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
var
  ANewBuffer: TVBuffer;
begin
  Assert(ASrc.ID.&Type = TPParamType(vBuffer));
  if Prototype.CheckEntryCode(AEntryCode) then begin
    ANewBuffer:=IPBuffer(ASrc).Get;
    Assert(ANewBuffer<>nil);
    RegisterChange(TVChangedBuffer.CreateDefault(ANewBuffer, ANewBuffer.Size = 0), APermission, APermissionGroup, AEntryCode);
  end;
end;

procedure TVBufferValueStorage.ToStream(out ADest); cdecl;
begin
  FLock.Beginread;
  TTVSPSrcID(ADest):=FValue.Source.ID;
  FLock.Endread;
end;

procedure TVBufferValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
var
  AValue: TVBuffer;
begin
  AValue:=EmptyBuffer.FromSource(SourceUtil[TTVSPSrcID(ASrc)]);
  RegisterChange(TVChangedBuffer.CreateDefault(AValue, AValue.Size = 0), APermission, APermissionGroup, AEntryCode);
end;

function TVBufferValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPBuffer(Self);
end;

function TVBufferValueStorage.Get: TVBuffer; cdecl;
begin
  FLock.Beginread;
  Result:=FValue;
  FLock.Endread;
end;

procedure TVBufferValueStorage.&Set(Value: TVBuffer); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVBufferValueStorage.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  Assert(Value<>nil);
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedBuffer.CreateDefault(Value, Value.Size = 0), APermission, APermissionGroup, AEntryCode);
end;

procedure TVBufferValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTBUFFER, APermission, APermissionGroup, AEntryCode);
end;

{%ENDREGION}
{%REGION TVBufferType}

function TVBufferType.GetColor: TMColor; cdecl;
begin
  Result:=$FFFF00FF
end;

function TVBufferType.GetName: IString; cdecl;
begin
  Result:='Buffer';
end;

function TVBufferType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVBufferType.GetType: TPParamType; cdecl;
begin
  Result:=vBuffer;
end;

function TVBufferType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVBufferValueStorage.Create(APrototype);
end;

function TVBufferType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall          : Result:=IPBuffer(TVCallToBuffer.Create(IPCall(ADest)));
    vInteger       : Result:=IPBuffer(TVIntegerToBuffer.Create(IPInteger(ADest)));
    vFloat         : Result:=IPBuffer(TVFloatToBuffer.Create(IPFloat(ADest)));
    vColor         : Result:=IPBuffer(TVColorToBuffer.Create(IPColor(ADest)));
    vBoolean       : Result:=IPBuffer(TVBooleanToBuffer.Create(IPBoolean(ADest)));
    vBuffer        : Result:=ADest;
    vString        : Result:=IPBuffer(TVStringToBuffer.Create(IPString(ADest)));
    vPreset        : Result:=IPBuffer(TVPresetToBuffer.Create(IPPreset(ADest)));
    vPointer       : Result:=IPBuffer(TVPointerToBuffer.Create(IPPointer(ADest)));
    else             Result:=IPBuffer(TVDummyBufferConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyBufferConverter}

constructor TVDummyBufferConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyBufferConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyBufferConverter.&Set(Value: TVBuffer; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyBufferConverter.Get: TVBuffer; cdecl;
begin
  Result:=EmptyBuffer;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVBufferType.Create));
end;

{%ENDREGION}

end.

