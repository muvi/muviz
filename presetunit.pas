unit PresetUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, VisTypeUnit, Buttons, Menus, ComCtrls, VisType,
  LCLAdvFunc, GR32_Image, SpectrumData, PresetList, AdvType, PluginSystem,
  EditPreset, VisDrawUnit, PreviewUnit, GR32CanvasUnit, EditPresetNew,
  ImportUnit;

type

  { TPresetForm }

  TPresetForm = class(TForm)
    AppendBtn: TBitBtn;
    ConvertIDBtn: TButton;
    EditPresetBtn: TBitBtn;
    EditPreset2Btn: TBitBtn;
    PresetPreviewImg: TImage32;
    PresetPreviewLbl: TLabel;
    PresetPreviewBevel: TBevel;
    DestCanvasCB: TComboBox;
    DestCamvasLbl: TLabel;
    PresetNameLbl: TLabel;
    NewMI: TMenuItem;
    ChangeTriggerBtn: TBitBtn;
    LayerTB: TToggleBox;
    OverviewPanel: TPanel;
    PresetDataPanel: TPanel;
    PresetLayerPanel: TPanel;
    PresetLB: TListBox;
    Splitter: TSplitter;
    PresetTV: TTreeView;
    PresetFilterEdit: TTreeFilterEdit;
    TriggerTB: TToggleBox;
    DuplicateMI: TMenuItem;
    MoveDownBtn: TBitBtn;
    MoveUpBtn: TBitBtn;
    DeletePresetMI: TMenuItem;
    RenamePresetMI: TMenuItem;
    OpenDialog: TOpenDialog;
    PresetPopupMenu: TPopupMenu;
    SaveBtn: TBitBtn;
    OpenBtn: TBitBtn;
    PresetBevel: TBevel;
    AddLayerBtn: TButton;
    DeleteLayerBtn: TButton;
    PresetPanel: TPanel;
    SaveDialog: TSaveDialog;
    VisualisationCB: TComboBox;
    procedure AddLayerBtnClick(Sender: TObject);
    procedure AppendBtnClick(Sender: TObject);
    procedure ChangeTriggerBtnClick(Sender: TObject);
    procedure ConvertIDBtnClick(Sender: TObject);
    procedure EditPreset2BtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    //procedure IconBtnClick(Sender: TObject);
    procedure LayerTBChange(Sender: TObject);
    procedure NewMIClick(Sender: TObject);
    procedure PresetBevelChangeBounds(Sender: TObject);
    //procedure ListBtnClick(Sender: TObject);
    procedure PresetTVEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure PresetTVEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure PresetTVSelectionChanged(Sender: TObject);
    procedure TriggerBtnClick(Sender: TObject);
    procedure DeleteLayerBtnClick(Sender: TObject);
    procedure DuplicateMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DeletePresetMIClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure LayerLBSelectionChange(Sender: TObject; User: boolean);
    procedure RenamePresetMIClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure TriggerTBChange(Sender: TObject);
    procedure VisualisationCBChange(Sender: TObject);
    procedure DestCanvasCBChange(Sender: TObject);
    procedure PreviewReady;
    procedure PreviewStart;
  private
    FPreset                : TPreset;
    FPresetLayers          : ^TPresetLayers;
    FSelectedIndex         : Integer;
    FDestCanvasPossibilitys: array of TPresetOutputInfo;
    FPreviewDrawer         : TPresetPreviewDrawer;
    procedure ShowNoLayer;
    procedure ShowPresets;
    procedure ShowPreset;
    procedure ShowVisParams(const {Preset,}Layer: Integer);
    {procedure ShowTrigger;
    procedure ShowTrigger(const Index: Integer);
    procedure ShowTrigParams(const Preset,Layer: Integer);}
    //procedure DoShowParams(var Visualisation: TPresetVis);
    procedure SetView(const ATriggers: Boolean);
    procedure HideShowTriggers;
    procedure UpdateDestCanvas;
    procedure FindDestCanvas;
  public
    VisPresets: TPresets;
    procedure UpdateVis;
    procedure EditPresets(APresets: TPresets);
  end;

var
  PresetForm: TPresetForm;

function LoadResultMessage(const AResult: TMuviLoadResult; const AFileName: string): Boolean;

implementation

{$R *.lfm}

{ TPresetForm }

procedure TPresetForm.EditPresets(APresets: TPresets);
var
  I: Integer;
begin
  VisPresets:=APresets;
  VisualisationCB.Items.Clear;
  for I:=0 to TVisualisation.VisCount-1 do VisualisationCB.Items.Add(TVisualisation.Visualisations(I).Name);
  VisualisationCB.ItemIndex:=0;

  FPreviewDrawer:=TPresetPreviewDrawer.Create(TGR32Canvas.Create(PresetPreviewImg.Bitmap),true);
  FPreviewDrawer.OnReady:=@PreviewReady;
  FPreviewDrawer.OnStart:=@PreviewStart;

  ShowPresets;
  //Show;
  ShowModal;
end;

procedure TPresetForm.ShowPresets;
var
  I,APageIndex: Integer;
  AParentNode : TTreeNode;
begin
  PresetTV.Selected:=nil;
  PresetTV.Items.Clear;
  AParentNode:=PresetTV.Items.Add(nil,'Unsortierte Presets');
  for I:=0 to VisPresets.Count-1 do begin
    PresetTV.Items.AddChild(AParentNode,VisPresets[I].Name);
  end;
  AParentNode.Expanded:=true;
  FSelectedIndex:=0;  (***,wenn Kategorien eingeführt werden*)

  SetView(false);
  TriggerTB.Checked:=false;
  LayerTB.Checked:=true;
  FPreset:=VisPresets[FSelectedIndex];
  FPresetLayers:=@FPreset.Layers;
  ShowPreset;
end;

procedure TPresetForm.PreviewStart;
begin
  PresetPreviewImg.Bitmap.Clear($FF000000);
end;

procedure TPresetForm.ShowPreset;
var
  I: Integer;
begin
  PresetPreviewLbl.Visible:=true;
  PresetPreviewImg.Visible:=false;

  PresetLB.Items.Clear;
  for I:=0 to Length(FPresetLayers^)-1
    do PresetLB.Items.Add(TVisualisation.Visualisations(FPresetLayers^[I].VisID).Name);
  UpdateDestCanvas;
  ShowNoLayer;

  PresetNameLbl.Caption:=FPreset.Name;
  PresetPreviewImg.Bitmap.BeginUpdate;
  FPreviewDrawer.GeneratePreview(FPreset);
end;

procedure TPresetForm.ShowVisParams(const {Preset,}Layer: Integer);
{var
  ParamCount: Integer;}
begin
  //VisualisationCS.SubControlCount:=0;
  VisualisationCB.Enabled:=(Layer>=0);
  if not VisualisationCB.Enabled then exit;
  with FPresetLayers^[Layer] do VisualisationCB.ItemIndex:=VisID;
  //DoShowParams(FPresetLayers^{VisPresets^[Preset]}{.Layers}[Layer]);
  DestCanvasCB.Enabled:=true;
  FindDestCanvas;
  {with VisPresets^[Preset].Layers[Layer] do begin
    VisualisationCB.ItemIndex:=VisID;
    with Visualisations(VisID) do begin
      ParamCount:=Length(VisParamDesc);
      VisualisationCS.SubControlCount:=ParamCount;
    end;
    C1DVPF.AssignParam(C1);
    C2DVPF.AssignParam(C2);
    C3DVPF.AssignParam(C3);
  end;}
end;

{procedure TPresetForm.DoShowParams(var Visualisation: TPresetVis{const Preset,Layer: Integer});
var
  I         : Integer;
  AParam    : TPresetVisParam;
  AFrame    : TBasicVisParamFrame;
  AParamName: string;
  AParamType: TVisParamType;
begin
  with Visualisation do begin
    VisualisationCB.ItemIndex:=VisID;
    with TVisualisation.Visualisations(VisID) do begin
      for I:=0 to Length(VisParamDesc)-1 do begin
        AParam:=TPresetVisParam.Create(FPresetLayers^[PresetLB.ItemIndex],I,AParamName,AParamType);
        AFrame:=CreateVisParamFrame(AParamType,VisualisationCS);
        VisualisationCS.AddSubControl(AFrame);
        with AFrame do begin
          ShowWithParam(AParam,AParamName);
          Width:=C1DVPF.Width;
        end;
      end;

      C1DVPF.AssignParam(C1,C1Desc);
      C2DVPF.AssignParam(C2,C2Desc);
      C3DVPF.AssignParam(C3,C3Desc);
    end;
  end;
end;}

procedure TPresetForm.SetView(const ATriggers: Boolean);
begin
  //MoveDownBtn.Enabled:=not ATriggers;
  //MoveUpBtn.Enabled:=MoveDownBtn.Enabled;
  if ATriggers then begin
    AddLayerbtn.Caption:='Trigger Hinzufügen';
    DeleteLayerBtn.Caption:='Trigger Löschen';
  end else begin
    AddLayerbtn.Caption:='Layer Hinzufügen';
    DeleteLayerBtn.Caption:='Layer Löschen';
  end;
end;

procedure TPresetForm.ShowNoLayer;
begin
  //VisualisationCS.SubControlCount:=0;
  VisualisationCB.Enabled:=false;
  //C1DVPF.Disable;
  //C2DVPF.Disable;
  //C3DVPF.Disable;
  DestCanvasCb.Enabled:=false;
end;

procedure TPresetForm.AddLayerBtnClick(Sender: TObject);
var
  PresetIndex: Integer;
begin
  PresetIndex:=Length(FPresetLayers^);
  SetLength(FPresetLayers^,PresetIndex+1);
  FPresetLayers^[PresetIndex].Produce(VisualisationCB.ItemIndex);
  //FPresetLayers^[PresetIndex]:=CreateVis(VisualisationCB.ItemIndex);
  PresetLB.Items.Add(TVisualisation.Visualisations(FPresetLayers^[PresetIndex].VisID).Name);
  PresetLB.ItemIndex:=PresetIndex;
  ShowVisParams({PresetNotebook.PageIndex,}PresetIndex);
  UpdateDestCanvas;
end;

procedure TPresetForm.AppendBtnClick(Sender: TObject);
begin
  OpenDialog.Title:='Anhängen';
  if OpenDialog.Execute then begin
    if LoadResultMessage(VisPresets.LoadFromFile(OpenDialog.FileName,true),OpenDialog.FileName)
      then exit;
    ShowPresets;
  end;
end;

procedure TPresetForm.ChangeTriggerBtnClick(Sender: TObject);
var
  L: Integer;
begin
  if PresetLB.ItemIndex<0 then exit;
  if LayerTB.Checked then begin
    L:=Length(FPreset.Triggers);
    SetLength(FPreset.Triggers,L+1);
    FPreset.Swap(PresetLB.ItemIndex,not L);
    //ChangeLayers(FPreset^,PresetLB.ItemIndex,not L);
  end else begin
    L:=Length(FPreset.Layers);
    SetLength(FPreset.Layers,L+1);
    FPreset.Swap(not PresetLB.ItemIndex,L);
    //ChangeLayers(FPreset^,not PresetLB.ItemIndex,L);
  end;
  DeleteLayerBtnClick(Sender);
  UpdateDestCanvas;
end;

procedure TPresetForm.ConvertIDBtnClick(Sender: TObject);
var
  AIDStr: string;
begin
  if PresetLB.ItemIndex>=0 then begin
    if LayerTB.Checked then begin
      AIDStr:=GUIDToString(
        ImportUnit.ConvertVisID(
          TVisualisation.Visualisations(
            VisPresets[FSelectedIndex].Layers[PresetLB.ItemIndex].VisID
          ).VisID
        )
      )
    end else begin
      AIDStr:=GUIDToString(
        ImportUnit.ConvertVisID(
          TVisualisation.Visualisations(
            VisPresets[FSelectedIndex].Triggers[PresetLB.ItemIndex].VisID
          ).VisID
        )
      )
    end;
    Dialogs.InputQuery('Neue ID', 'Die neue ID dieser Visualisierung lautet: ', AIDStr);
  end;
end;

procedure TPresetForm.EditPreset2BtnClick(Sender: TObject);
begin
  NewPresetEditForm.EditPreset(VisPresets[FSelectedIndex]);
end;

procedure TPresetForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FPreviewDrawer.Destroy;
end;

procedure TPresetForm.FormDestroy(Sender: TObject);
begin
  //PLF.ListDone;
  SetLength(FDestCanvasPossibilitys,0);
end;

procedure TPresetForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  I         : Integer;
  ASucceeded: Boolean;
  AErrorStr : string;
begin
  ASucceeded:=true;
  AErrorStr:='';
  for I:=0 to Length(FileNames)-1 do if not lplSucceeded(VisPresets.LoadFromFile(FileNames[I],true).Success) then begin
    ASucceeded:=false;
    AErrorStr+=FileNames[I]+#$D#$A;
  end;
  if not ASucceeded then AdvMsg('Folgende Dateien konnten nicht geladen werden:'+#$D#$A+AErrorStr,TdMWarnung);
  ShowPresets;
end;

procedure TPresetForm.PreviewReady;
begin
  PresetPreviewLbl.Visible:=false;
  PresetPreviewImg.Visible:=true;
  PresetPreviewImg.Bitmap.EndUpdate;
  PresetPreviewImg.Bitmap.Changed;
end;

procedure TPresetForm.LayerTBChange(Sender: TObject);
begin
  TriggerTB.Checked:=not LayerTB.Checked;
  HideShowTriggers;
end;

procedure TPresetForm.NewMIClick(Sender: TObject);
var
  AName: string;
begin
  FSelectedIndex:=VisPresets.Count;
  AName:='Preset '+IntToStr(FSelectedIndex+1);
  VisPresets.Add.Name:=AName;
  //SetLength(VisPresets,FSelectedIndex);
  //VisPresets[FSelectedIndex].Name:=AName;
  PresetTV.Items.AddChild(PresetTV.Items[0],AName).EditText; (***,wenn Kategorien eingeführt werden*)
  {PresetNotebook.Pages.Strings[PresetIndex]:=AName;
  PresetNotebook.Pages.Add('+');}
end;

procedure TPresetForm.PresetBevelChangeBounds(Sender: TObject);
begin

end;

{procedure TPresetForm.ListBtnClick(Sender: TObject);
begin
  PLF.PresetLV.ViewStyle:=vsReport;
end;}

{procedure TPresetForm.PresetLVSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Item<>nil then begin
    PresetNotebook.PageIndex:=Item.Index;
    PresetNotebookPageChanged(Sender);
  end;
end;}

procedure TPresetForm.PresetTVEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  if PresetTV.Selected=nil then exit;
  if PresetTV.Selected.Parent=nil then exit;
  VisPresets[FSelectedIndex].Name:=S;
end;

procedure TPresetForm.PresetTVEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  if PresetTV.Selected=nil then begin
    AllowEdit:=false;
    exit;
  end;
  AllowEdit:=(PresetTV.Selected.Parent<>nil);
end;

procedure TPresetForm.PresetTVSelectionChanged(Sender: TObject);
begin
  if PresetTV.Selected=nil then exit;
  if PresetTV.Selected.Parent=nil then exit;

  FSelectedIndex:=PresetTV.Selected.Index; (***, wenn Kategorien eingeführt werden...*)

  SetView(false);
  TriggerTB.Checked:=false;
  LayerTB.Checked:=true;
  FPreset:=VisPresets[FSelectedIndex];
  FPresetLayers:=@FPreset.Layers;
  ShowPreset;
end;

var
  ObsoleteWarningResult: TModalResult = mrRetry;

procedure TPresetForm.TriggerBtnClick(Sender: TObject);
var
  AMsgResult: TModalResult;
begin
  AMsgResult:=ObsoleteWarningResult;
  while AMsgResult=mrRetry
    do AMsgResult:=AdvMsg('really use obsolete trash?',TdMCrazy);
  case AMsgResult of
    mrYes,mrOK,mrIgnore: ; //do nothing
    mrYesToAll         : ObsoleteWarningResult:=mrYes;
    mrNoToAll          : begin
        ObsoleteWarningResult:=mrNo;
        exit;
      end;
    else exit;
  end;

  PresetEditForm.EditPreset(VisPresets[FSelectedIndex]);
end;

procedure TPresetForm.DeleteLayerBtnClick(Sender: TObject);
var
  ALayerIndex: Integer;
begin
  if PresetLB.ItemIndex<0 then exit;
  ALayerIndex:=PresetLB.ItemIndex;
  if LayerTB.Checked
    then FPreset.Delete(ALayerIndex)
    else FPreset.Delete(not ALayerIndex);
  {if LayerTB.Checked
    then RemoveLayer(FPreset^{VisPresets^[PresetNotebook.PageIndex]},ALayerIndex)
    else RemoveLayer(FPreset^{VisPresets^[PresetNotebook.PageIndex]},not ALayerIndex);}
  //RemoveLayer(VisPresets^)
  PresetLB.Items.Delete(ALayerIndex);
  if ALayerIndex>=PresetLB.Items.Count then begin
    if PresetLB.Items.Count>0 then PresetLB.ItemIndex:=PresetLB.Items.Count-1 else begin
      ShowNoLayer;
      exit;
    end;
  end else PresetLB.ItemIndex:=ALayerIndex;
  ShowVisParams({PresetNotebook.PageIndex,}PresetLB.ItemIndex);
  UpdateDestCanvas;
end;

procedure TPresetForm.DuplicateMIClick(Sender: TObject);
var
  AOldPreset: TPreset;
  AName     : string;
begin
  //OldPresetIndex:=FSelectedIndex;
  AOldPreset:=VisPresets[FSelectedIndex];
  FSelectedIndex:=VisPresets.Count;
  FPreset:=VisPresets.Add;
  AOldPreset.CopyTo(FPreset);
  FPreset.Name:=AOldPreset.Name;

  //AName:=VisPresets[OldPresetIndex].Name {'Preset '+IntToStr(PresetIndex+1)};
  //if not InputQuery('Neues Preset','Geben Sie den Namen des neuen Presets ein',AName) then exit;
  //PresetIndex:=VisPresets.Count+1;
  //SetLength(VisPresets^,PresetIndex);
  //Dec(PresetIndex);
  //VisPresets^[OldPresetIndex].CopyTo(VisPresets^[PresetIndex]);
  //CopyPreset(VisPresets^[OldPresetIndex],VisPresets^[PresetIndex]);
  //VisPresets^[PresetIndex].Name:=AName;

  PresetTV.Items.AddChild(PresetTV.Items[0],FPreset.Name).EditText; (***,wenn Kategorien eingeführt werden*)
  //PresetNotebook.Pages.Strings[PresetIndex]:=AName;
  //PresetNotebook.Pages.Add('+');
  //FSelectedIndex:=PresetIndex;

  SetView(false);
  TriggerTB.Checked:=false;
  LayerTB.Checked:=true;
  //FPreset:=@VisPresets^[PresetIndex];
  FPresetLayers:=@FPreset.Layers;
  ShowPreset{(PresetIndex)};
end;

procedure TPresetForm.UpdateVis;
var
  I: Integer;
begin
  VisualisationCB.Items.Clear;
  for I:=0 to TVisualisation.VisCount-1 do VisualisationCB.Items.Add(TVisualisation.Visualisations(I).Name);
end;

procedure TPresetForm.FormCreate(Sender: TObject);
begin
  VisualisationCB.ItemIndex:=0;
  //PLF.InitList;
  PresetPreviewImg.Bitmap.Width:=PresetPreviewImg.Width;
  PresetPreviewImg.Bitmap.Height:=PresetPreviewImg.Height;
end;

procedure TPresetForm.DeletePresetMIClick(Sender: TObject);
begin
  if AdvMsg('Möchten Sie das Preset "'+VisPresets[FSelectedIndex].Name+'" wirklich dauerhaft löschen?',TdMFrage)=mrYes then begin
    //PresetNotebook.OnPageChanged:=nil;
    if VisPresets.Count>1 then begin
      VisPresets.Remove(FSelectedIndex);
      PresetTV.Items[0].Items[FSelectedIndex].Delete; (***,wenn Kategorien eingeführt werden*)
      //PresetNotebook.Pages.Delete(FSelectedIndex);
      if FSelectedIndex>=VisPresets.Count
        then FSelectedIndex:=VisPresets.Count-1;
    end else begin
      //ClearLayers(VisPresets^[0]);
      VisPresets[0].Clear;
      VisPresets[0].Name:='Preset 1';
      PresetTV.Items[0].Items[0].Text:='Preset 1'; (***,wenn Kategorien eingeführt werden*)
      //PresetNotebook.Pages.Strings[FSelectedIndex]:='Preset 1';
    end;
    SetView(false);
    TriggerTB.Checked:=false;
    LayerTB.Checked:=true;
    FPreset:=VisPresets[FSelectedIndex];
    FPresetLayers:=@FPreset.Layers;
    ShowPreset;
    //PresetNotebook.OnPageChanged:=@PresetNotebookPageChanged;
  end;
end;

procedure TPresetForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then case Key of
    46: DeletePresetMIClick(nil);
    79: if ssShift in Shift
      then AppendBtn.Click
      else OpenBtn.Click;
    82: RenamePresetMIClick(nil);
    83: SaveBtn.Click;
    68: DuplicateMI.Click;
  end;
end;

procedure TPresetForm.MoveDownBtnClick(Sender: TObject);
var
  AIndex: Integer;
begin
  if PresetLB.ItemIndex>=PresetLB.Count-1 then exit;
  //with VisPresets^[PresetNotebook.PageIndex] do begin
  if LayerTB.Checked then begin
    AIndex:=PresetLB.ItemIndex;
    FPreset.Swap(AIndex,AIndex+1);
    //ChangeLayers(FPreset^,AIndex,AIndex+1)
  end else begin
    AIndex:=not PresetLB.ItemIndex;
    FPreset.Swap(AIndex,AIndex-1);
    //ChangeLayers(FPreset^,AIndex,AIndex-1);
  end;
  //ChangeLayers(FPreset^{Layers},PresetLB.ItemIndex,PresetLB.ItemIndex+1);
  PresetLB.Items.Strings[PresetLB.ItemIndex]:=TVisualisation.Visualisations(FPresetLayers^{Layers}[PresetLB.ItemIndex].VisID).Name;
  PresetLB.Items.Strings[PresetLB.ItemIndex+1]:=TVisualisation.Visualisations(FPresetLayers^{Layers}[PresetLB.ItemIndex+1].VisID).Name;
  //end;
  PresetLB.ItemIndex:=PresetLB.ItemIndex+1;
  UpdateDestCanvas;
end;

procedure TPresetForm.MoveUpBtnClick(Sender: TObject);
var
  AIndex: Integer;
begin
  if PresetLB.ItemIndex<1 then exit;
  //with VisPresets^[PresetNotebook.PageIndex] do begin
  if LayerTB.Checked then begin
    AIndex:=PresetLB.ItemIndex;
    FPreset.Swap(AIndex,AIndex-1);
    //ChangeLayers(FPreset^,AIndex,AIndex-1)
  end else begin
    AIndex:=not PresetLB.ItemIndex;
    FPreset.Swap(AIndex,AIndex+1);
    //ChangeLayers(FPreset^,AIndex,AIndex+1);
  end;
  PresetLB.Items.Strings[PresetLB.ItemIndex]:=TVisualisation.Visualisations(FPresetLayers^{Layers}[PresetLB.ItemIndex].VisID).Name;
  PresetLB.Items.Strings[PresetLB.ItemIndex-1]:=TVisualisation.Visualisations(FPresetLayers^{Layers}[PresetLB.ItemIndex-1].VisID).Name;
  //end;
  PresetLB.ItemIndex:=PresetLB.ItemIndex-1;
  UpdateDestCanvas;
end;

procedure TPresetForm.OpenBtnClick(Sender: TObject);
begin
  OpenDialog.Title:='Öffnen';
  if OpenDialog.Execute then begin
    if LoadResultMessage(VisPresets.LoadFromFile(OpenDialog.FileName),OpenDialog.FileName) then exit;
    ShowPresets;
  end;
end;

procedure TPresetForm.LayerLBSelectionChange(Sender: TObject; User: boolean);
begin
  ShowVisParams({PresetNotebook.PageIndex,}PresetLB.ItemIndex);
end;

{procedure TPresetForm.PresetNotebookPageChanged(Sender: TObject);
var
  PresetIndex: Integer;
  AName      : string;
begin
  if PresetNotebook.PageIndex=PresetNotebook.PageCount-1 then begin
    PresetIndex:=Length(VisPresets^)+1;
    AName:='Preset '+IntToStr(PresetIndex);
    if not InputQuery('Neues Preset','Geben Sie den Namen des neuen Presets ein',AName) then begin
      PresetNotebook.PageIndex:=PresetNotebook.PageIndex-1;
      exit;
    end;
    SetLength(VisPresets^,PresetIndex);
    Dec(PresetIndex);
    VisPresets^[PresetIndex].Name:=AName;
    PresetNotebook.Pages.Strings[PresetIndex]:=AName;
    PresetNotebook.Pages.Add('+');
    //PLF.UpdateList;
  end;
  SetView(false);
  TriggerTB.Checked:=false;
  LayerTB.Checked:=true;
  FPreset:=@VisPresets^[PresetNotebook.PageIndex];
  FPresetLayers:=@FPreset^.Layers;
  ShowPreset{(PresetNotebook.PageIndex)};
end;}

procedure TPresetForm.RenamePresetMIClick(Sender: TObject);
{var
  AName: ^string;
  ANode: TTreeNode;}
begin
  {AName:=@VisPresets^[FSelectedIndex].Name;
  AName^:=InputBox('Preset Umbenennen','Geben Sie den neuen Namen des Presets ein',AName^);
  //PresetNotebook.Pages.Strings[PresetNotebook.PageIndex]:=AName^;
  ANode:=PresetTV.Items[0].Items[FSelectedIndex];  (***,wenn Kategorien eingeführt werden*)
  ANode.Text:=AName^;}
  PresetTV.Items[0].Items[FSelectedIndex].EditText;
end;

procedure TPresetForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute
    then VisPresets.SaveToFile(SaveDialog.FileName);
end;

procedure TPresetForm.TriggerTBChange(Sender: TObject);
begin
  LayerTB.Checked:=not TriggerTB.Checked;
  HideShowTriggers;
end;

procedure TPresetForm.HideShowTriggers;
begin
  SetView(TriggerTB.Checked);
  if TriggerTB.Checked
    then FPresetLayers:=@FPreset.Triggers
    else FPresetLayers:=@FPreset.Layers;
  ShowPreset;
end;

procedure TPresetForm.UpdateDestCanvas;
var
  I,J,AP     : Integer;
  AVisType   : PVisType;

  procedure InsertPos(const AName: string; const ADest: TPresetOutputInfo);
  begin
    Inc(AP);
    if Length(FDestCanvasPossibilitys)<=AP then SetLength(FDestCanvasPossibilitys,AP+10);
    FDestCanvasPossibilitys[AP]:=ADest;
    DestCanvasCB.Items.Add(AName);
  end;

begin
  AP:=-1;
  DestCanvasCB.Items.Clear;
  InsertPos('DEFAULTCANVAS',poiDEFAULTCANVAS);
  with FPreset do for I:=0 to Length(Layers)-1 do with Layers[I] do begin
    AVisType:=TVisualisation.VisPtr(VisID);
    for J:=0 to Length(AVisType^.CanvasDesc)-1
      do InsertPos('Layers['+IntToStr(I)+']:'+AVisType^.Name+'.'+AVisType^.CanvasDesc[J],PresetOutputInfo(I,J));
  end;
  with FPreset do for I:=0 to Length(Triggers)-1 do with Triggers[I] do begin
    AVisType:=TVisualisation.VisPtr(VisID);
    for J:=0 to Length(AVisType^.CanvasDesc)-1
      do InsertPos('Triggers['+IntToStr(I)+']:'+AVisType^.Name+'.'+AVisType^.CanvasDesc[J],PresetOutputInfo(not I,J));
  end;
  SetLength(FDestCanvasPossibilitys,AP+1);
  if PresetLB.ItemIndex>=0 then FindDestCanvas;
end;

procedure TPresetForm.FindDestCanvas;
var
  I          : Integer;
  ADestCanvas: TPresetOutputInfo;
begin
  DestCanvasCB.OnChange:=nil;
  ADestCanvas:=FPresetLayers^[PresetLB.ItemIndex].DestCanvas;
  for I:=0 to Length(FDestCanvasPossibilitys)-1
    do if (ADestCanvas.Layer=FDestCanvasPossibilitys[I].Layer) and (ADestCanvas.Param=FDestCanvasPossibilitys[I].Param)
      then begin
        DestCanvasCB.ItemIndex:=I;
        DestCanvasCB.OnChange:=@DestCanvasCBChange;
        exit;
      end;
  DestCanvasCB.ItemIndex:=-1;
  DestCanvasCB.OnChange:=@DestCanvasCBChange;
end;

procedure TPresetForm.VisualisationCBChange(Sender: TObject);
var
  AVis: ^TPresetVis;
begin
  AVis:=@FPresetLayers^{VisPresets^[PresetNotebook.PageIndex].Layers}[PresetLB.ItemIndex];
  //DestroyVis(AVis^);
  AVis^.Destroy;
  //AVis^:=CreateVis(VisualisationCB.ItemIndex);
  AVis^.Produce(VisualisationCB.ItemIndex);
  PresetLB.Items.Strings[PresetLB.ItemIndex]:=VisualisationCB.Items.Strings[VisualisationCB.ItemIndex];
  ShowVisParams({PresetNotebook.PageIndex,}PresetLB.ItemIndex);
end;

procedure TPresetForm.DestCanvasCBChange(Sender: TObject);
begin
  FPresetLayers^[PresetLB.ItemIndex].DestCanvas:=FDestCanvasPossibilitys[DestCanvasCB.ItemIndex];
end;

{Allgemein}

function LoadResultMessage(const AResult: TMuviLoadResult; const AFileName: string): Boolean;
begin
  case AResult.Success of
    lplInitFailed                       : begin
        Result:=true;
        AdvMsg('Das Plugin "'+AFileName+'" konnte nicht geöffnet werden!',TdMFError);
      end;
    lplNotSupported                     : begin
        Result:=true;
        AdvMsg('Das Plugin "'+AFileName+'" wird nicht unterstützt!',TdMWarnung);
      end;
    lplInvalidHeader,lplUnknownExtension: begin
        Result:=true;
        AdvMsg('Die Datei "'+AFileName+'" ist keine gültige Muvi-Visualisierungspresetdatei!',TdMFError);
      end;
    lplOK                               : Result:=false;
    lplPluginMissing                    : begin
        Result:=true;
        AdvMsg('Zum Öffnen einer oder mehrerer Presets aus der Datei "'+AFileName+'" ist mindestens noch folgendes Plugin notwendig: '+TVisIDStr(AResult.FailInfo),TdMWarnung);
      end;
    lplAlreadyOpen                      : begin
        Result:=true;
        AdvMsg('Die Datei "'+AFileName+'" wurde bereits installiert.',TdMInfo);
      end;
    lplFileNotFound                     : begin
        Result:=true;
        AdvMsg('Die Datei "'+AFileName+'" wurde nicht gefunden.',TdMFError);
      end;
    lplNoValidEntry                     : begin
        Result:=true;
        AdvMsg('Die Datei "'+AFileName+'" enthält keinen gültigen Eintrag.',TdMInfo);
      end;
    else begin
        Result:=true;
        AdvMsg('Beim Öffnen der Datei "'+AFileName+'" ist ein unbekannter Fehler aufgetreten ('+IntToStr(AResult.Success)+').',TdMFError);
      end;
  end;
end;

end.

