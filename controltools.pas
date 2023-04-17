unit ControlTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils;

type
  TParentChangerList = class
  private
    FControls: array of TControl;
    FParentRemoved: boolean;
    function GetControlCount: integer; inline;
    function GetControl(AIndex: integer): TControl; inline;
    procedure DoRemoveParent(AControls: array of TControl);
  public
    constructor Create;
    constructor Create(AControls: array of TControl);
    destructor Destroy; override;
    procedure ChangeParent(ANewParent: TWinControl); inline;
    procedure RemoveParent; inline;
    procedure Add(AControl: TControl); inline;
    procedure Add(AControls: array of TControl); inline;
    procedure Clear; inline;
    property ControlCount: integer read GetControlCount;
    property Controls[AIndex: integer]: TControl read GetControl;
    property ParentRemoved: boolean read FParentRemoved;
  end;

implementation

{TParentChangerList}

constructor TParentChangerList.Create;
begin
  inherited Create;
  FParentRemoved := False;
end;

constructor TParentChangerList.Create(AControls: array of TControl);
var
  I, L: integer;
begin
  inherited Create;
  FParentRemoved := False;
  L := Length(AControls);
  SetLength(FControls, L);
  for I := 0 to L - 1 do
    FControls[I] := AControls[I];
end;

destructor TParentChangerList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TParentChangerList.ChangeParent(ANewParent: TWinControl); inline;
var
  I: integer;
  AControl: TControl;
begin
  for I := 0 to Length(FControls) - 1 do
  begin
    AControl := FControls[I];
    if AControl.Parent <> ANewParent then
    begin
      ;
      if not FParentRemoved then
        AControl.Parent.RemoveControl(AControl);
      ANewParent.InsertControl(AControl);
    end;
  end;
  FParentRemoved := False;
end;

procedure TParentChangerList.RemoveParent;
begin
  DoRemoveParent(FControls);
  FParentRemoved := True;
end;

procedure TParentChangerList.Add(AControl: TControl); inline;
var
  L: integer;
begin
  L := Length(FControls);
  SetLength(FControls, L + 1);
  FControls[L] := AControl;
  if FParentRemoved then
    AControl.Parent.RemoveControl(AControl);
end;

procedure TParentChangerList.Add(AControls: array of TControl); inline;
var
  I, L1, L2: integer;
begin
  L1 := Length(FControls);
  L2 := Length(AControls);
  SetLength(FControls, L1 + L2);
  for I := 0 to L2 - 1 do
    FControls[I + L1] := AControls[I];
  if FParentRemoved then
    DoRemoveParent(AControls);
end;

procedure TParentChangerList.Clear; inline;
begin
  SetLength(FControls, 0);
  FParentRemoved := False;
end;

procedure TParentChangerList.DoRemoveParent(AControls: array of TControl);
var
  I: integer;
  AControl: TControl;
begin
  for I := 0 to Length(AControls) - 1 do
  begin
    AControl := AControls[I];
    AControl.Parent.RemoveControl(AControl);
  end;
  FParentRemoved := True;
end;

function TParentChangerList.GetControlCount: integer; inline;
begin
  Result := Length(FControls);
end;

function TParentChangerList.GetControl(AIndex: integer): TControl; inline;
begin
  Result := FControls[AIndex];
end;

end.
