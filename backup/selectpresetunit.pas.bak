unit SelectPresetUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, VisTypeUnit, LCLType, LCLIntf;

type

  { TSelectPresetForm }

  TSelectPresetForm = class(TForm)
    FilterEdit: TTreeFilterEdit;
    TreeView: TTreeView;
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FPresets       : TPresets;
    FOnChangePreset: TNotifyEvent;
    FPresetIndex   : Integer;
    procedure SetPresets(Value: TPresets);
  public
    procedure Popup(AtControl: TControl);
    procedure UpdatePresets;
    property PresetIndex: Integer read FPresetIndex;
    property Presets: TPresets read FPresets write SetPresets;
    property OnChangePreset: TNotifyEvent read FOnChangePreset write FOnChangePreset;
  end;

var
  SelectPresetForm: TSelectPresetForm;

implementation

procedure TSelectPresetForm.Popup(AtControl: TControl);
var
  APos: TPoint;
begin
  FPresetIndex:=-1;
  APos:=Point(0,0);
  APos:=AtControl.ClientToScreen(APos);
  if AtControl.Width>=200 then begin
    Left:=APos.x;
    Width:=AtControl.Width;
  end else begin
    Left:=APos.X-((200-AtControl.Width) div 2);
    Width:=200;
  end;
  Top:=APos.Y+AtControl.Height;
  TreeView.Selected:=nil;
  ActiveControl:=FilterEdit;
  FilterEdit.SelStart:=Length(FilterEdit.Filter);
  FilterEdit.SelLength:=0;
  Show;
  //Result:=(ShowModal=mrOK);
  //if Result and Assigned(FOnChangePreset)
    //then FOnChangePreset(Self);
end;

procedure TSelectPresetForm.UpdatePresets;
var
  I          : Integer;
  AParentNode: TTreeNode;
begin
  TreeView.Selected:=nil;
  TreeView.Items.Clear;
  AParentNode:=TreeView.Items.Add(nil,'Unsortierte Presets');
  for I:=0 to FPresets.Count-1 do begin
    TreeView.Items.AddChild(AParentNode,FPresets[I].Name);
  end;
  AParentNode.Expanded:=true;
end;

procedure TSelectPresetForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=27 {Escape} then Hide;
end;

procedure TSelectPresetForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ASelNode: TTreeNode;
begin
  if Key = VK_RETURN then begin
    ASelNode:=TreeView.Selected;
    if ASelNode<>nil then begin
      FPresetIndex:=ASelNode.Index;
      if Assigned(FOnChangePreset) then FOnChangePreset(Self);
      Hide;
    end;
  end;
end;

procedure TSelectPresetForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AMousePos: TPoint;
  ANode    : TTreeNode;
begin
  GetCursorPos(AMousePos);
  AMousePos:=TreeView.ScreenToClient(AMousePos);
  ANode:=TreeView.GetNodeAt(AMousePos.X, AMousePos.Y);
  if ANode<>nil then begin
    FPresetIndex:=ANode.Index;
    if Assigned(FOnChangePreset) then FOnChangePreset(Self);
    Hide;
  end;
end;

procedure TSelectPresetForm.FormDeactivate(Sender: TObject);
begin
  Hide;
end;

procedure TSelectPresetForm.FormHide(Sender: TObject);
begin
  FilterEdit.Filter:='';
  ActiveControl:=nil;
end;

procedure TSelectPresetForm.SetPresets(Value: TPresets);
begin
  FPresets:=Value;
  UpdatePresets;
end;

initialization
  {$I selectpresetunit.lrs}
end.

