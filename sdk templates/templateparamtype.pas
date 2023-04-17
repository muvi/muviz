unit TemplateParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, TemplateParamConverters;

type
  TVTemplateValueStorage  = class (TPParamValueStorage)
  private
    FValue: TVTemplate;
  protected
    function GetType: TPParamType; cdecl; override;
  public
    //sets AParam to the stored value
    procedure SetTo(ADest: IPParam); cdecl; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam); cdecl; override;
    function Clone: IPParamValueStorage; cdecl; override;
    function ToStream: TPParamValueStream; cdecl; override;
    procedure FromStream(AStream: TPParamValueStream); cdecl; override;

    property &Type: TPParamType read GetType;
  end;

  TVTemplateType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: PChar; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage: IPParamValueStorage; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyTemplateConverter= class (TVTemplateParam)
  private
     FConvertFrom: IPParam;
     FStorage    : IPParamValueStorage;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = 'Insert picture here';

{%REGION TVTemplateValueStorage}

function TVTemplateValueStorage.GetType: TPParamType; cdecl;
begin
  Result:=vTemplate;
end;

procedure TVTemplateValueStorage.SetTo(ADest: IPParam); cdecl;
begin
  IPTemplate(ADest).&Set(FValue);
end;

procedure TVTemplateValueStorage.GetFrom(ASrc: IPParam); cdecl;
begin
  FValue:=IPTemplate(ASrc).Get;
end;

function TVTemplateValueStorage.Clone: IPParamValueStorage; cdecl;
var
  ANewStorage: TVTemplateValueStorage;
begin
  ANewStorage:=TVTemplateValueStorage.Create;
  ANewStorage.FValue:=FValue;
  Result:=IPParamValueStorage(ANewStorage);
end;

function TVTemplateValueStorage.ToStream: TPParamValueStream; cdecl;
var
  APtr: ^TVTemplate;
begin
  Result.Size:=SizeOf(FValue);
  GetMem(Result.Value, Result.Size);
  APtr:=Result.Value;
  APtr^:=FValue;
end;

procedure TVTemplateValueStorage.FromStream(AStream: TPParamValueStream); cdecl;
var
  APtr: ^TVTemplate;
begin
  APtr:=AStream.Value;
  FValue:=APtr^;
end;

{%ENDREGION}
{%REGION TVTemplateType}

function TVTemplateType.GetColor: TMColor; cdecl;
begin
  //TODO: insert color here
  Result:=$FF000000
end;

function TVTemplateType.GetName: PChar; cdecl;
begin
  Result:='Template';
end;

function TVTemplateType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVTemplateType.GetType: TPParamType; cdecl;
begin
  Result:=vTemplate;
end;

function TVTemplateType.CreateValueStorage: IPParamValueStorage; cdecl;
begin
  Result:=IPParamValueStorage(TVTemplateValueStorage.Create);
end;

function TVTemplateType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.&Type of
    //TODO: insert at right pos
    //vTemplate    : Result:=ADest;
    vCall        : Result:=IPTemplate(TVCallToTemplate.Create(IPCall(ADest)));
    vInteger     : Result:=IPTemplate(TVIntegerToTemplate.Create(IPInteger(ADest)));
    vReal        : Result:=IPTemplate(TVRealToTemplate.Create(IPReal(ADest)));
    vShortString : Result:=IPTemplate(TVShortStringToTemplate.Create(IPShortString(ADest)));
    vColor       : Result:=IPTemplate(TVColorToTemplate.Create(IPColor(ADest)));
    vBoolean     : Result:=IPTemplate(TVBooleanToTemplate.Create(IPBoolean(ADest)));
    vBuffer      : Result:=IPTemplate(TVBufferToTemplate.Create(IPBuffer(ADest)));
    vString      : Result:=IPTemplate(TVStringToTemplate.Create(IPString(ADest)));
    vPreset      : Result:=IPTemplate(TVPresetToTemplate.Create(IPPreset(ADest)));
    vPointer     : Result:=IPTemplate(TVPointerToTemplate.Create(IPPointer(ADest)));
    else           Result:=IPTemplate(TVDummyTemplateConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyTemplateConverter}

constructor TVDummyTemplateConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
  FStorage:=ParamTypeUtil[AConvertFrom.&Type].CreateValueStorage;
end;

destructor TVDummyTemplateConverter.Destroy;
begin
  FConvertFrom:=nil;
  FStorage:=nil;
end;

procedure TVDummyTemplateConverter.&Set(Value: TVTemplate); cdecl;
begin
  FStorage.GetFrom(FConvertFrom);
  FStorage.SetTo(FConvertFrom);
end;

function TVDummyTemplateConverter.Get: TVTemplate; cdecl;
begin
  Result:=DEFAULTTEMPLATE;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  ParamTypeUtil.AddType(IPParamType(TVTemplateType));
end;

{%ENDREGION}

end.

