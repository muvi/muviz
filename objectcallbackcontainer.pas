unit ObjectCallbackContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic;

type
  TObjectCallbackContainer = class (TObjectContainer)
  private
    FOnDestroy: TNotifyEvent;
  protected
    procedure ObjectDestroyed; override;
  public
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

implementation

{%REGION TObjectCallbackContainer}

procedure TObjectCallbackContainer.ObjectDestroyed;
begin
  //do not destroy this container
  &Object:=nil;
  if Assigned(FOnDestroy)
    then FOnDestroy(Self);
end;

{%ENDREGION}

end.

