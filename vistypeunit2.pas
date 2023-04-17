unit VisTypeUnit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TagUnit, UIDArray, VisType2, csl, HashMap, MapKeys,
  StdParamTypes;

type
  TPType            = class
  strict private
    FID           : TPVisID;
    FConstructor  : TPVisConstructor;
  public
    constructor Create(AID: TPVisID; AConstructor: TPVisConstructor);
    //function Instantiate(AEnvironment: IPVisualisationEnvironment): IPVisualisation; virtual; abstract;
    property &Constructor: TPVisConstructor read FConstructor;
    property ID: TPVisID read FID;
  end;

  TPDefaultType     = TPType;

  TPTypes           = class
  strict private
    FTypes      : TMap;
    FNullVis    : TPType;
    function GetVisualisation(AID: TPVisID): TPType;
    function GetVisualisationOrNil(AID: TPVisID): TPType;
  public
    constructor Create;
    destructor Destroy; override;
    //procedure Register(AID: TPVisID; AConstructor: TPVisConstructor);
    procedure Register(AType: TPType);
    property NullVis: TPType read FNullVis;
    property Visualisations[AID: TPVisID]: TPType read GetVisualisation; default;
    property VisualisationsOrNil[AID: TPVisID]: TPType read GetVisualisationOrNil;
  end;

implementation

procedure CreateNullVis(APrototype: IPVisualisationPrototype); cdecl; forward;

{%REGION TPType}

constructor TPType.Create(AID: TPVisID; AConstructor: TPVisConstructor);
begin
  inherited Create;
  FID:=AID;
  FConstructor:=AConstructor;
end;

{
function TPDefaultType.Instantiate(AEnvironment: IPVisualisationEnvironment): IPVisualisation;
begin
  Result:=CreateVisualisation(FConstructor, AEnvironment);
end;
}

{%ENDREGION}
{%REGION TPTypes}

constructor TPTypes.Create;
begin
  inherited Create;
  FTypes:=THashMap.Create;
  //null visualisation
  FNullVis:=TPDefaultType.Create(NULLVISID, @CreateNullVis);
end;

destructor TPTypes.Destroy;
begin
  //do not destroy FNullVis here. This is done in Clean below.
  FTypes.Clean;
  FTypes.Destroy;
  inherited Destroy;
end;

function TPTypes.GetVisualisation(AID: TPVisID): TPType;
var
  AKey : TGUIDKey;
  AItem: TObject;
begin
  AKey:=TGUIDKey.Create(AID);
  AItem:=FTypes.Items[AKey];
  if AItem=nil
    then Result:=FNullVis
    else Result:=TPType(AItem);
  AKey.Destroy;
end;

function TPTypes.GetVisualisationOrNil(AID: TPVisID): TPType;
var
  AKey : TGUIDKey;
  AItem: TObject;
begin
  AKey:=TGUIDKey.Create(AID);
  AItem:=FTypes.Items[AKey];
  Result:=AItem as TPType;
  AKey.Destroy;
end;

procedure TPTypes.Register(AType: TPType);
begin
  FTypes.Add(TGUIDKey.Create(AType.ID),AType);
end;

{%ENDREGION}
{%REGION Misc}

procedure CreateNullVis(APrototype: IPVisualisationPrototype); cdecl;
begin
  //do nothing
end;

{%ENDREGION}

end.

