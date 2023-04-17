unit CallParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, CallParamConverters, MStrings, TVSPSources, Doors,
  StdPermissions;

type
  TVChangedCall          = class (TPChangedValue, IChangedCall)
  public
    function GetIChangedValue: IChangedValue; override;
  end;

  TVCallValueStorage     = class (TPParamValueStorage, IPCall)
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

  TVCallType             = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyCallConverter   = class (TVCallParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#32#0+'6'+#0+'6'+#0+'6'+#0+'6'+#0+'.'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+')'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+';'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0+')'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+';'+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#10#0+'@'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'E'+#0#23#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#7#0+'>'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'D'+#0#18#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#5#0+';'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'B'+#0#14#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#3#0+'8'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'F'+#0+'@'+#0#10#0#0#0#0#0#0#0#0#0#0#0#0#0#11#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#24#0#16#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedCall}

function TVChangedCall.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedCall(Self);
end;

{%ENDREGION}
{%REGION TVCallValueStorage}

procedure TVCallValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = ID.&Type);
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedCall.Create(true), APermission, APermissionOwner, AEntryCode);
end;

procedure TVCallValueStorage.ToStream(out ADest); cdecl;
begin
  //do nothing
end;

procedure TVCallValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  RegisterChange(TVChangedCall.Create(true), APermission, APermissionOwner, AEntryCode);
end;

procedure TVCallValueStorage.&Set; cdecl;
begin
  &Set(TPNOLIMIT, NULLVISID, nil);
end;

procedure TVCallValueStorage.&Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedCall.Create(true), APermission, APermissionOwner, AEntryCode);
end;

function TVCallValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPCall(Self);
end;

procedure TVCallValueStorage.DoSet(AValue: TPChangedValue);
begin
  inherited DoSet(AValue);
end;

procedure TVCallValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(APermission, APermissionOwner, AEntryCode);
end;

{%ENDREGION}
{%REGION TVCallType}

function TVCallType.GetColor: TMColor; cdecl;
begin
  Result:=$FF414141
end;

function TVCallType.GetName: IString; cdecl;
begin
  Result:='Call';
end;

function TVCallType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVCallType.GetType: TPParamType; cdecl;
begin
  Result:=vCall;
end;

function TVCallType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=TVCallValueStorage.Create(APrototype);
end;

function TVCallType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall          : Result:=ADest;
    vInteger       : Result:=IPCall(TVIntegerToCall.Create(IPInteger(ADest)));
    vFloat         : Result:=IPCall(TVFloatToCall.Create(IPFloat(ADest)));
    vColor         : Result:=IPCall(TVColorToCall.Create(IPColor(ADest)));
    vBoolean       : Result:=IPCall(TVBooleanToCall.Create(IPBoolean(ADest)));
    vBuffer        : Result:=IPCall(TVBufferToCall.Create(IPBuffer(ADest)));
    vString        : Result:=IPCall(TVStringToCall.Create(IPString(ADest)));
    vPreset        : Result:=IPCall(TVPresetToCall.Create(IPPreset(ADest)));
    vPointer       : Result:=IPCall(TVPointerToCall.Create(IPPointer(ADest)));
    else             Result:=IPCall(TVDummyCallConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyCallConverter}

constructor TVDummyCallConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyCallConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyCallConverter.&Set(APermission: TTVSPClientPermission; APermissionOwner: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionOwner, AEntryCode);
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVCallType.Create));
end;

{%ENDREGION}

end.

