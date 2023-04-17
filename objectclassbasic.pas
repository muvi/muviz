unit ObjectClassBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MInterfacedObject;

type
  TObjectItem     = class;

  TObjectContainer= class
  private
    FNextContainer  : TObjectContainer;
    FPrevContainer  : TObjectContainer;
    FObject         : TObjectItem;
    FOwnsObject     : Boolean;
    FObjectDestroyed: Boolean;
    procedure RemoveFromObject;
    procedure AddToObject;
    procedure SetObject(Value: TObjectItem);
  protected
    procedure ObjectDestroyed; virtual;

    property &Object: TObjectItem read FObject write FObject;
    property OwnsObject: Boolean read FOwnsObject;
  public
    constructor Create(AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
    destructor Destroy; override;
    property Content: TObjectItem read FObject write SetObject;
  end;

  TObjectItem     = class (TMInterfacedObject)
  private
    FContainer: TObjectContainer;
  protected
    procedure RemoveFromLists; virtual;
    //executed after removing this Item from all lists
    procedure DoFree; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{%REGION TObjectItem}

constructor TObjectItem.Create;
begin
  inherited Create;
  FContainer:=nil;
end;

destructor TObjectItem.Destroy;
begin
  RemoveFromLists;
  DoFree;
  inherited Destroy;
end;

procedure TObjectItem.DoFree;
begin
  //do nothing
end;

procedure TObjectItem.RemoveFromLists;
var
  AContainer,AContainer2: TObjectContainer;
begin
  AContainer:=FContainer;
  while AContainer<>nil do begin
    AContainer2:=AContainer;
    AContainer:=AContainer.FNextContainer;
    AContainer2.ObjectDestroyed;
  end;
end;

{%ENDREGION}
{%REGION TObjectContainer}

constructor TObjectContainer.Create(AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
begin
  inherited Create;
  FObjectDestroyed:=false;
  FObject:=AObject;
  if AObject<>nil then AddToObject;
  FOwnsObject:=AOwnsObjects;
end;

destructor TObjectContainer.Destroy;
begin
  if (FObject<>nil) and (not FObjectDestroyed) then begin
    RemoveFromObject;
    if FOwnsObject then FObject.Destroy;
  end;
  inherited Destroy;
end;

procedure TObjectContainer.RemoveFromObject;
begin
  if FPrevContainer<>nil
    then FPrevContainer.FNextContainer:=FNextContainer
    else FObject.FContainer:=FNextContainer;
  if FNextContainer<>nil
    then FNextContainer.FPrevContainer:=FPrevContainer;
end;

procedure TObjectContainer.AddToObject;
begin
  FPrevContainer:=nil;
  FNextContainer:=FObject.FContainer;
  if FNextContainer<>nil then FNextContainer.FPrevContainer:=Self;
  FObject.FContainer:=Self;
end;

procedure TObjectContainer.ObjectDestroyed;
begin
  FObjectDestroyed:=true;
  Destroy;
end;

procedure TObjectContainer.SetObject(Value: TObjectItem);
begin
  if FObject<>nil then begin
    RemoveFromObject;
    if FOwnsObject then FObject.Destroy;
  end;
  FObject:=Value;
  if Value<>nil then AddToObject;
end;

{%ENDREGION}

end.

