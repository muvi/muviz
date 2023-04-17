unit KeyPreUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, VisDrawUnit, AdvFunc, VisTypeUnit;

type

  TKeyNodeAddProc= procedure (const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode) of object;

  { TKeyPreForm }

  TKeyPreForm    = class(TForm)
    ImageList: TImageList;
    KeyPreTV: TTreeView;
    KeyPreTVFilterEdit: TTreeFilterEdit;
    procedure FormCreate(Sender: TObject);
  private
    AddKeyNode: array [TVisKeyType] of TKeyNodeAddProc;
    FKeyboards: TVisKeyboards;
    FPresets  : TPresets;
    procedure AddKeyNode_ktNothing(const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode);
    procedure AddKeyNode_ktPreset(const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode);
    procedure AddKeyNode_ktKeyboard(const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode);
  public
    procedure GenerateAndShow(const AKeyboards: TVisKeyboards; const APresets: TPresets);
    procedure Generate;
  end;

var
  KeyPreForm: TKeyPreForm;

implementation

procedure TKeyPreForm.GenerateAndShow(const AKeyboards: TVisKeyboards; const APresets: TPresets);
begin
  FKeyboards:=AKeyboards;
  FPresets:=APresets;
  Generate;
  Show;
end;

procedure TKeyPreForm.FormCreate(Sender: TObject);
begin
  AddKeyNode[ktNothing]:=@AddKeyNode_ktNothing;
  AddKeyNode[ktPreset]:=@AddKeyNode_ktPreset;
  AddKeyNode[ktKeyboard]:=@AddKeyNode_ktKeyboard;
end;

procedure TKeyPreForm.AddKeyNode_ktNothing(const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode);
begin

end;

procedure TKeyPreForm.AddKeyNode_ktPreset(const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode);
var
  ANodeVal: string;
  ANode   : TTreeNode;
begin
  if AKey.Value<FPresets.Count
    then ANodeVal:=FPresets[AKey.Value].Name
    else ANodeVal:='<Preset '+IntToStr(AKey.Value)+'>';
  {if AKey.BackChange
    then ANodeVal+=' // P|B'
    else ANodeVal+=' // P';}
  ANodeVal:='['+KeyDescriptions[AKeyChar]+'] '+ANodeVal;
  ANode:=KeyPreTV.Items.AddChild(AParentNode,ANodeVal);
  if AKey.BackChange
    then ANode.ImageIndex:=0
    else ANode.ImageIndex:=1;
end;

procedure TKeyPreForm.AddKeyNode_ktKeyboard(const AKeyChar: Char; const AKey: TVisKey; const AParentNode: TTreeNode);
var
  ANodeVal: string;
  ANode   : TTreeNode;
begin
  if AKey.Value<Length(FKeyboards)
    then ANodeVal:=FKeyboards[AKey.Value].Name
    else ANodeVal:='<Tastaturbelegung '+IntToStr(AKey.Value)+'>';
  {if AKey.BackChange
    then ANodeVal+=' // K|B'
    else ANodeVal+=' // K';}
  ANodeVal:='['+KeyDescriptions[AKeyChar]+'] '+ANodeVal;
  ANode:=KeyPreTV.Items.AddChild(AParentNode,ANodeVal);
  ANode.ImageIndex:=2;
end;

procedure TKeyPreForm.Generate;
var
  I          : Integer;
  J          : Char;
  AParentNode: TTreeNode;
  AKey       : ^TVisKey;
begin
  KeyPreTV.Items.Clear;
  for I:=0 to Length(FKeyboards)-1 do with FKeyboards[I] do begin
    AParentNode:=KeyPreTV.Items.Add(nil,Name);
    AParentNode.ImageIndex:=3;
    for J:=#0 to #255 do begin
      AKey:=@Keys[J];
      AddKeyNode[AKey^.KeyType](J,AKey^,AParentNode);
    end;
  end;
end;

initialization
  {$I keypreunit.lrs}

end.

