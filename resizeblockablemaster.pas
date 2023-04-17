unit ResizeBlockableMaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl;

type
  TResizeBlockableMaster = class (TMasterControl)
  strict private
    FResizingEnabled: Boolean;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent);
    //needed for some subcontrols which may call resize when they are hidden
    property ResizingEnabled: Boolean read FResizingEnabled write FResizingEnabled;
  end;

implementation

{%REGION TResizeBlockableMaster}

constructor TResizeBlockableMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResizingEnabled:=true;
end;

procedure TResizeBlockableMaster.Resize;
begin
  if FResizingEnabled
    then inherited Resize;
end;

{%ENDREGION}

end.

