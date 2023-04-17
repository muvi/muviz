unit ToolboxUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, DragPlugUnit, ParamType2, VisType2, PresetType,
  StdTags, PresetListUnit, EditBtn, StdParamTypes, MStrings, ValueStorages;

const
  RecentCount = 50;

type

  { TToolboxForm }

  TToolboxForm = class(TForm)
    DefaultValueTS: TTabSheet;
    FavoritesTS: TTabSheet;
    DefaultValueLV: TListView;
    PlugImageList: TImageList;
    PresetLF: TPresetListFrame2;
    RecentTS: TTabSheet;
    ToolboxPC: TPageControl;
    PresetOverviewTS: TTabSheet;
    procedure DefaultValueLVEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure DefaultValueLVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PresetLFPresetChosen(Sender: TObject; APreset: TPPresetID);
  private
    FRecent    : array [0..RecentCount-1] of IPParam;
    FRecentPos : Integer;
    FParamTypes: array of TPParamType;
  public
    procedure UpdateDefaultValues;
    procedure UpdatePresets;
    procedure UpdateAll;
    procedure Clear;
    procedure TakeValue(AValue: IPParam);
  end;

var
  ToolboxForm: TToolboxForm;

implementation

{%REGION TToolboxForm}

procedure TToolboxForm.DefaultValueLVEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit:=false;
end;

procedure TToolboxForm.DefaultValueLVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AType : TPParamType;
  AValue: IPParam;
begin
  if DefaultValueLV.Selected=nil
    then exit;
  AType:=FParamTypes[DefaultValueLV.Selected.Index];
  //initialized with default value
  AValue:=CreateValueStorage(AType);
  //AValue.Param.GetFrom(GetDummy(AType, VisualisationUtil.MainThread));
  TakeValue(AValue);
end;

procedure TToolboxForm.PresetLFPresetChosen(Sender: TObject; APreset: TPPresetID);
var
  AValue: IPParam;
begin
  AValue:=CreateValueStorage(vPreset);
  IPPreset(AValue).Value:=APreset;
  TakeValue(AValue);
end;

procedure TToolboxForm.FormCreate(Sender: TObject);
begin
  FRecentPos:=0;
  PresetLF.OnPresetChosen:=@PresetLFPresetChosen;
end;

procedure TToolboxForm.FormShow(Sender: TObject);
begin
  UpdatePresets;
end;

procedure TToolboxForm.Clear;
begin
  //do not do this in FormDestroy (will crash) or FormClose
  //(will delete everything which may be usefull again)
  PresetLF.Clear;
end;

procedure TToolboxForm.UpdateAll;
begin
  UpdateDefaultValues;
  UpdatePresets;
end;

procedure TToolboxForm.UpdateDefaultValues;
var
  I, AImageIndex: Integer;
  AIterator     : IPParamTypeIterator;
  AParamType    : IPParamType;
begin
  DefaultValueLV.Items.Clear;
  PlugImageList.Clear;
  SetLength(FParamTypes, 0);

  I:=0;
  AIterator:=ParamTypeUtil.Iterator;
  while AIterator.MoveNext do begin
    AParamType:=AIterator.GetCurrent;
    DragPlugForm.PlugImages.LoadImage(true, false, false, AParamType.&Type);
    AImageIndex:=PlugImageList.Add(DragPlugForm.PlugImages.OutputBmp, nil);
    with DefaultValueLV.Items.Add do begin
      Caption:=AParamType.Name;
      ImageIndex:=AImageIndex;
    end;
    SetLength(FParamTypes, I+1);
    FParamTypes[I]:=AParamType.&Type;
    Inc(I);
  end;
end;

procedure TToolboxForm.UpdatePresets;
begin
  PresetLF.UpdatePresets;
end;

procedure TToolboxForm.TakeValue(AValue: IPParam);
begin
  FRecentPos:=(FRecentPos+1) mod RecentCount;
  FRecent[FRecentPos]:=AValue;
  DragValue(AValue);
end;

{%ENDREGION}

initialization
  {$I toolboxunit.lrs}

end.

