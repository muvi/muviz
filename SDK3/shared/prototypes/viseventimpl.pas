unit VisEventImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2;

type
  TVisualisationEvents   = class (TInterfacedObject, IPVisualisationEvents)
  private
    FPrototype: IPVisualisationPrototype;
    function GetEnvironment: IPVisualisationEnvironment;
  protected
    //CAUTION: not thread-synchronized
    procedure GetInput(ASettings: IPParamSettings); cdecl; virtual;
    //thread-synchronized
    procedure GotInput(Param: IPParam); cdecl; virtual;
    procedure Suspend; cdecl; virtual;
    procedure Resume; cdecl; virtual;
    //call after you are prepared for getting GetInput events
    procedure DoInitialize;
    property Environment: IPVisualisationEnvironment read GetEnvironment;
    property Prototype: IPVisualisationPrototype read FPrototype;
  public
    constructor Create(APrototype: IPVisualisationPrototype; AAutoInitialize: Boolean = true);
    destructor Destroy; override;
  end;

implementation

{%REGION TVisualisationEvents}

constructor TVisualisationEvents.Create(APrototype: IPVisualisationPrototype; AAutoInitialize: Boolean = true);
begin
  inherited Create;
  FPrototype:=APrototype;
  if AAutoInitialize
    then DoInitialize;
end;

destructor TVisualisationEvents.Destroy;
begin
  FPrototype:=nil;
  inherited Destroy;
end;

procedure TVisualisationEvents.DoInitialize;
begin
  FPrototype.SetEvents(Self);
end;

procedure TVisualisationEvents.GetInput(ASettings: IPParamSettings); cdecl;
begin
  //do nothing
end;

procedure TVisualisationEvents.GotInput(Param: IPParam); cdecl;
begin
  //do nothing
end;

procedure TVisualisationEvents.Suspend; cdecl;
begin
  //do nothing
end;

procedure TVisualisationEvents.Resume; cdecl;
begin
  //do nothing
end;

function TVisualisationEvents.GetEnvironment: IPVisualisationEnvironment;
begin
  Result:=Prototype.Environment;
end;

{%ENDREGION}

end.

