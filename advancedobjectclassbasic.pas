unit AdvancedObjectClassBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic;

type
  TAdvancedObjectContainer           = class;

  TAdvancedObjectStructure           = class
  protected
    procedure AutomaticDeletion; virtual; abstract;
    procedure BeforeAutomaticDeletion(Sender: TAdvancedObjectContainer); virtual; abstract;
  end;

  TAdvancedObjectContainer           = class (TObjectContainer)
  strict private
    FOwner: TAdvancedObjectStructure;
  protected
    procedure ObjectDestroyed; override;
    property Owner: TAdvancedObjectStructure read FOwner;
  public
    constructor Create(AOwner: TAdvancedObjectStructure; AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
  end;

  TAutomaticNotifyingObjectStructure = class (TAdvancedObjectStructure)
  protected
    FOnBeforeAutomaticDeletion: TNotifyEvent;
    FOnAutomaticDeletion      : TNotifyEvent;
    procedure AutomaticDeletion; override;
    procedure BeforeAutomaticDeletion(Sender: TAdvancedObjectContainer); override;
  public
    property OnBeforeAutomaticDeletion: TNotifyEvent read FOnBeforeAutomaticDeletion write FOnBeforeAutomaticDeletion;
    property OnAutomaticDeletion: TNotifyEvent read FOnAutomaticDeletion write FOnAutomaticDeletion;
  end;

implementation

{%REGION TAdvancedObjectContainer}

constructor TAdvancedObjectContainer.Create(AOwner: TAdvancedObjectStructure; AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
begin
  Assert(AOwner <> nil);
  inherited Create(AObject, AOwnsObjects);
  FOwner:=AOwner;
end;

procedure TAdvancedObjectContainer.ObjectDestroyed;
var
  AOwner: TAdvancedObjectStructure;
begin
  FOwner.BeforeAutomaticDeletion(Self);
  AOwner:=FOwner;
  inherited ObjectDestroyed;
  AOwner.AutomaticDeletion;
end;

{%ENDREGION}
{%REGION TAutomaticNotifyingObjectStructure}

procedure TAutomaticNotifyingObjectStructure.AutomaticDeletion;
begin
  if Assigned(FOnAutomaticDeletion)
    then FOnAutomaticDeletion(Self);
end;

procedure TAutomaticNotifyingObjectStructure.BeforeAutomaticDeletion(Sender: TAdvancedObjectContainer);
begin
  if Assigned(FOnBeforeAutomaticDeletion)
    then FOnBeforeAutomaticDeletion(Self);
end;

{%ENDREGION}

end.

