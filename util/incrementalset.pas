unit IncrementalSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Enumerators, ObjectClassBasic, AdvancedObjectClassBasic;

type
  TIncrementalNotifyEvent      = procedure (Sender: TObject; ACount: Cardinal) of object;
  TIncrementalSetRemovalResult = record
    Existed, ExistsNow: Boolean;
  end;

  TIncrementalSet              = class (TAdvancedObjectStructure)
  strict private
    FOnBeforeAutomaticDeletion: TIncrementalNotifyEvent;
    FOnAutomaticDeletion      : TNotifyEvent;
  protected
    procedure AutomaticDeletion; override;
  public
    //returns if the item is new
    function Add(AItem: TObjectItem): Boolean; virtual; abstract;
    //returns if the item existed and if it exists after the removal
    function Remove(AItem: TObjectItem): TIncrementalSetRemovalResult; virtual; abstract;
    function GetEnumerator: TEnumerator; virtual; abstract;
    property Enumerator: TEnumerator read GetEnumerator;
    property OnAutomaticDeletion: TNotifyEvent read FOnAutomaticDeletion write FOnAutomaticDeletion;
    property OnBeforeAutomaticDeletion: TIncrementalNotifyEvent read FOnBeforeAutomaticDeletion write FOnBeforeAutomaticDeletion;
  end;

implementation

{%REGION TIncrementalSet}

procedure TIncrementalSet.AutomaticDeletion;
begin
  if Assigned(FOnAutomaticDeletion)
    then FOnAutomaticDeletion(Self);
end;

{%ENDREGION}

end.

