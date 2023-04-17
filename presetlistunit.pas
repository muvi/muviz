unit PresetListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, LResources, Forms, Controls,
  ComCtrls, AdvFunc, PresetType, TagType, EditBtn, VisType2, StdTags, LCLIntf,
  LCLType, MStrings, GUIDop, StdParamTypes;

type

  TPresetChosenEvent = procedure (Sender: TObject; APreset: TPPresetID) of object;

  { TPresetListFrame2 }

  TPresetListFrame2  = class(TFrame)
    PresetTV: TTreeView;
    FilterEdit: TTreeFilterEdit;
    procedure FilterEditChange(Sender: TObject);
    function FilterEditFilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure PresetTVDeletion(Sender: TObject; Node: TTreeNode);
    procedure FilterEditFiltered(Sender: TObject; var Done: Boolean);
    procedure PresetTVKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure PresetTVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FOnPresetChosen: TPresetChosenEvent;
    function GetIsPreset(ANode: TTreeNode): Boolean;
    function GetPresetID(ANode: TTreeNode): TPPresetID;
    function FirstVisibleLeaf(AFirstNode: TTreeNode): TTreeNode;
  public
    procedure UpdatePresets;
    procedure Clear;
    property IsPreset[ANode: TTreeNode]: Boolean read GetIsPreset;
    property PresetID[ANode: TTreeNode]: TPPresetID read GetPresetID;
    property OnPresetChosen: TPresetChosenEvent read FOnPresetChosen write FOnPresetChosen;
  end;

implementation

{%REGION TPresetListItemData}

type
  TPresetListItemData = class
  private
    FPresetID: TPPresetID;
    FVisible : Boolean;
    FNode    : TTreeNode;
  public
    constructor Create(APresetID: TPPresetID; AVisible: Boolean = true);
    property Node: TTreeNode read FNode write FNode;
    property PresetID: TPPresetID read FPresetID;
    property Visible: Boolean read FVisible;
  end;

constructor TPresetListItemData.Create(APresetID: TPPresetID; AVisible: Boolean = true);
begin
  inherited Create;
  FPresetID:=APresetID;
  FVisible:=AVisible;
end;

{%ENDREGION}
{%REGION TPresetTagArray}

type
  TPresetTagArrayItem = record
    Name        : TStringArray;
    CompleteName: string;
    Node        : TTreeNode;
  end;
  TPresetTagArray     = array of TPresetTagArrayItem;
  TPresetTagArrayFunc = specialize TAdvFunc<TPresetTagArray, TPresetTagArrayItem>;

//returns the level index, where the first difference occures
function PresetTagArrayItemEqualityLevel(const V1, V2: TPresetTagArrayItem): Integer;
var
  I, ALength: Integer;
begin
  if Length(V1.Name)<Length(V2.Name)
    then ALength:=Length(V1.Name)
    else ALength:=Length(V2.Name);
  for I:=0 to ALength-1 do begin
    if V1.Name[I]<>V2.Name[I] then begin
      Result:=I;
      exit;
    end;
  end;
  Result:=ALength;
end;

function PresetTagArrayItemSmaller(const V1, V2: TPresetTagArrayItem): Boolean;
var
  AEqualityLevel: Integer;
begin
  AEqualityLevel:=PresetTagArrayItemEqualityLevel(V1, V2);
  if AEqualityLevel=Length(V2.Name) then begin
    Result:=false;
    exit;
  end;
  if AEqualityLevel=Length(V1.Name) then begin
    Result:=true;
    exit;
  end;
  Result:=(V1.Name[AEqualityLevel] < V2.Name[AEqualityLevel]);
end;

{%ENDREGION}
{%REGION TPresetListFrame2}

procedure TPresetListFrame2.PresetTVDeletion(Sender: TObject; Node: TTreeNode);
begin
  {if Node.Data<>nil
    then TPresetListItemData(Node.Data).Destroy;
  Node.Data:=nil;}
end;

function TPresetListFrame2.FilterEditFilterItem(Item: TObject; out Done: Boolean): Boolean;
var
  AGUID : TGUID;
  AItem2: TPresetListItemData;
begin
  if Item=nil then begin
    Result:=false;
    Done:=true;
    exit;
  end;
  AItem2:=TPresetListItemData(Item);
  if TryStringToGUID(FilterEdit.Filter, AGUID) then begin
    Result:=(AItem2.PresetID=AGUID);
    Done:=false;
  end else begin
    Result:=false;
    Done:=not AItem2.Visible;
  end;
end;

procedure TPresetListFrame2.PresetTVKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and Assigned(FOnPresetChosen) and IsPreset[PresetTV.Selected]
    then FOnPresetChosen(Self, PresetID[PresetTV.Selected]);
end;

procedure TPresetListFrame2.PresetTVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AMousePos: TPoint;
  ANode    : TTreeNode;
begin
  if Assigned(FOnPresetChosen) then begin
    GetCursorPos(AMousePos);
    AMousePos:=PresetTV.ScreenToClient(AMousePos);
    ANode:=PresetTV.GetNodeAt(AMousePos.X, AMousePos.Y);
    if IsPreset[ANode]
      then FOnPresetChosen(Self, PresetID[ANode]);
  end;
end;

function TPresetListFrame2.FirstVisibleLeaf(AFirstNode: TTreeNode): TTreeNode;
var
  ANode: TTreeNode;
begin
  Result:=nil;
  ANode:=PresetTV.Items.GetFirstVisibleNode;
  while ANode<>nil do begin
    Result:=ANode;
    ANode:=ANode.GetFirstVisibleChild;
  end;
end;

procedure TPresetListFrame2.FilterEditFiltered(Sender: TObject; var Done: Boolean);
begin
  //change selection
  PresetTV.Selected:=FirstVisibleLeaf(PresetTV.Items.GetFirstNode);
  //remove this method
  Application.RemoveOnIdleHandler(@FilterEditFiltered);
end;

procedure TPresetListFrame2.FilterEditChange(Sender: TObject);
begin
  //TODO: really dirty workaround...
  //use something like OnFilteringEnd later if implemented
  if FilterEdit.Filter<>''
    then Application.AddOnIdleHandler(@FilterEditFiltered, false);
end;

procedure TPresetListFrame2.UpdatePresets;

  procedure DoAddItem(AParent: TTreeNode; AEqualityLevel: Integer; var AItem: TPresetTagArrayItem);
  var
    K        : Integer;
    APresets : IITaggedPresets;
    APreset  : IPVisualisation;
    AItemData: TPresetListItemData;
  begin
    //add all necessary parent tags and the tag itself
    for K:=AEqualityLevel to Length(AItem.Name)-1 do begin
      AParent:=PresetTV.Items.AddChild(AParent, AItem.Name[K]);
    end;
    AItem.Node:=AParent;
    //add all presets with this tag
    APresets:=PresetUtil.PresetsWithTag[AItem.CompleteName];
    for K:=0 to APresets.Count-1 do begin
      APreset:=APresets[K];
      AItemData:=TPresetListItemData.Create(APreset.ID, APreset.HasTag(TAGLISTED) and (not APreset.IsLoaded));
      AItemData.Node:=PresetTV.Items.AddChildObject(AParent, IPString(APreset.Inputs[ParamID(NAMEINPUTNAME, vString)]).Value, AItemData);
    end;
  end;

var
  I, J, AEquality: Integer;
  AAllTags      : ITags;
  AItems        : TPresetTagArray;
  AParentNode   : TTreeNode;
begin
  //clear
  Clear;
  FilterEdit.Filter:=' ';
  AAllTags:=PresetUtil.PresetTags;
  if AAllTags.Count=0
    then exit;
  //build tag list
  SetLength(AItems, AAllTags.Count);
  for I:=0 to Length(AItems)-1 do begin
    AItems[I].CompleteName:=AAllTags[I];
    SplitStr(AItems[I].CompleteName, '.', AItems[I].Name);
  end;
  TPresetTagArrayFunc.Quicksort(AItems, @PresetTagArrayItemSmaller, Length(AItems)-1);
  //build nodes
  DoAddItem(nil, 0, AItems[0]);
  for I:=1 to Length(AItems)-1 do begin
    AEquality:=PresetTagArrayItemEqualityLevel(AItems[I-1], AItems[I]);
    AParentNode:=AItems[I-1].Node;
    for J:=AEquality to Length(AItems[I-1].Name)-1
      do AParentNode:=AParentNode.Parent;
    DoAddItem(AParentNode, AEquality, AItems[I]);
  end;
  //cleanup
  SetLength(AItems, 0);
  //use filter
  FilterEdit.Filter:='';
end;

procedure TPresetListFrame2.Clear;
begin
  PresetTV.Items.Clear;
end;

function TPresetListFrame2.GetIsPreset(ANode: TTreeNode): Boolean;
begin
  Result:=(ANode<>nil);
  if Result
    then Result:=(ANode.Data<>nil);
end;

function TPresetListFrame2.GetPresetID(ANode: TTreeNode): TPPresetID;
begin
  Result:=TPresetListItemData(ANode.Data).PresetID;
end;

{%ENDREGION}

initialization
  {$I presetlistunit.lrs}

end.

