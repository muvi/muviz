unit PresetParamEdit;

{$mode objfpc}{$H+}

interface

uses
  MiscSlaveControls, ParamPainter2, VisType2, PresetUtil3_ViewPreset, MStrings,
  SlaveControl, PresetUtil3_Connections, PresetUtil3_ConnectionRouter, Controls,
  Classes, AdvCoord, PresetUtil3_Path, ControlPlacer, Graphics, PresetType,
  SelectPresetUnit2, StdParamTypes, GUIDop, ObjectClasses, PresetTagUnit,
  Dialogs, LCLAdvFunc, SysUtils, VisualisationUtils, GlobalVisualisationAliveMap;

type
  TPPPPreset             = class (TPPUParam)
  private
    FPresetID          : TPPresetID;
    FForceUpdate       : Boolean;
    FViewPreset        : TViewPreset;
    FExpanderBtn       : TSwitchButton;
    FOldExpanded       : Boolean;
    FEditTagsBtn       : TSlaveButton;
    FAddPresetBtn      : TSlaveButton;
    FSavePresetBtn     : TSlaveButton;
    procedure DoSetExpanded(AStore: Boolean = true);
    procedure SetExpanded(Value: Boolean);
    function GetExpanded: Boolean;
    procedure ExpanderBtnClick(Sender: TSlaveControl);
    procedure EditTagsBtnClick(Sender: TSlaveControl);
    procedure AddPresetBtnClick(Sender: TSlaveControl);
    procedure SavePresetBtnClick(Sender: TSlaveControl);
    procedure PresetSelected(Sender: TObject);
    procedure ConnectorConnectionOwned(Sender: TObject; AConnection: TConnection);
    procedure RouteConnections; inline;
    function DoUpdateIfNecessary: Boolean;
  strict protected
    function GetSubRouter: TConnectionRouter; override;
    function GetMinWidth: Integer; override;
  protected
    procedure DoPaint; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoAlign; override;
    procedure DoUpdate; override;
    procedure ViewPresetBoundsChanged(Sender: TSlaveControl);
    procedure DestroyViewPreset;
    procedure CreateViewPreset;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
    destructor Destroy; override;
    procedure UpdateAll; override;

    property Expanded: Boolean read GetExpanded write SetExpanded;
  end;

procedure Register;

implementation

uses
  PresetUtil3;

{%REGION TPPPPreset}

constructor TPPPPreset.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  FForceUpdate:=true;
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
  FOldExpanded:=false;
  FExpanderBtn:=TSwitchButton.Create(Self, Painter.Config.Pictures.ExpanderBtnImages);
  FExpanderBtn.OnClick:=@ExpanderBtnClick;
  FEditTagsBtn:=TSlaveButton.Create(Self, Painter.Config.Pictures.EditTagsImages);
  FEditTagsBtn.OnClick:=@EditTagsBtnClick;
  FAddPresetBtn:=TSlaveButton.Create(Self, Painter.Config.Pictures.AddPresetImages);
  FAddPresetBtn.OnClick:=@AddPresetBtnClick;
  FSavePresetBtn:=TSlaveButton.Create(Self, Painter.Config.Pictures.SavePresetImages);
  FSavePresetBtn.OnClick:=@SavePresetBtnClick;
  //do not create the FViewPreset here. This might result in endless recursion
  FViewPreset:=nil;

  Connections.Connector.AddConnectionOwnedHandler(@ConnectorConnectionOwned);

  FExpanderBtn.Switched:=TPresetEditor(Master).ExpandedParams.Contains(Path);
  DoSetExpanded(false);
end;

destructor TPPPPreset.Destroy;
begin
  Connections.Connector.RemoveConnectionOwnedHandler(@ConnectorConnectionOwned);
  FExpanderBtn.Destroy;
  if FViewPreset<>nil
    then FViewPreset.Destroy;
  inherited Destroy;
end;

function TPPPPreset.GetMinWidth: Integer;
begin
  //do not wrap anything if there is a preset
  Result:=-1;
end;

procedure TPPPPreset.ConnectorConnectionOwned(Sender: TObject; AConnection: TConnection);
begin
  //if it is the destination, the plugs will handle it...
  if (not AConnection.IsDestination(Connections.Connector)) and (FViewPreset<>nil)
    then FViewPreset.RouteConnection(AConnection);
end;

function TPPPPreset.GetExpanded: Boolean;
begin
  Result:=FExpanderBtn.Switched;
end;

procedure TPPPPreset.DoPaint;
begin
  inherited DoPaint;
  FillBG;
  case State of
    ppDefault  : Canvas.Font.Color:=clBlack;
    ppMouseOver: Canvas.Font.Color:=clWhite;
    ppDown     : Canvas.Font.Color:=clGray;
  end;
  Canvas.TextOut(AbsoluteRect.Left, AbsoluteRect.Top, IPString(PresetUtil[FPresetID].Inputs[ParamID(NAMEINPUTNAME, vString)]).Value);
  Canvas.Font.Color:=clBlack;
end;

procedure TPPPPreset.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  AScreenCoord: TPoint;
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if State=ppDown then begin
    if (ssRight in Shift) and (ssShift in Shift) and (ssCtrl in Shift) then begin
      Dialogs.InputBox('Info', 'The selected visualisation''s ID is:', GUIDToString(FPresetID));
    end else begin
      AScreenCoord.X:=AbsoluteRect.Left;
      AScreenCoord.Y:=AbsoluteRect.Top+DEFAULTPARAMHEIGHT;
      AScreenCoord:=Master.ClientToScreen(AScreenCoord);
      Assert(SelectPresetForm2 <> nil);
      SelectPresetForm2.SelectPreset(AScreenCoord.X, AScreenCoord.Y, @PresetSelected);
    end;
  end;
end;

procedure TPPPPreset.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  if FViewPreset<>nil
    then ANewBounds.Height:=DEFAULTPARAMHEIGHT + FViewPreset.Height
    else ANewBounds.Height:=DEFAULTPARAMHEIGHT;
  ClickRect:=Rect(0, 0, ANewBounds.Width - FExpanderBtn.Width - 5, DEFAULTPARAMHEIGHT);
end;

procedure TPPPPreset.DoAlign;
begin
  FExpanderBtn.Top:=0;
  FExpanderBtn.Left:=Width - FExpanderBtn.Width;
  FAddPresetBtn.Top:=0;
  FAddPresetBtn.Left:=FExpanderBtn.Left - FAddPresetBtn.Width - 5;
  FSavePresetBtn.Top:=0;
  FSavePresetBtn.Left:=FAddPresetBtn.Left - FSavePresetBtn.Width;
  FEditTagsBtn.Top:=0;
  FEditTagsBtn.Left:=FSavePresetBtn.Left - FEditTagsBtn.Width;
  if FViewPreset<>nil
    then FViewPreset.SetBounds(0, DEFAULTPARAMHEIGHT, Width, 0);
end;

function TPPPPreset.DoUpdateIfNecessary: Boolean;
var
  AName       : string;
  AOldPresetID: TPPresetID;
begin
  AOldPresetID:=FPresetID;
  FPresetID:=IPPreset(Param).ExecutedValue;
  Result:=(AOldPresetID<>FPresetID) or FForceUpdate;
  if Result then begin
    FForceUpdate:=false;
    AName:=IPString(PresetUtil[FPresetID].Inputs[ParamID(NAMEINPUTNAME, vString)]).Value;
    //ClickRect:=Rect(0, 0, Canvas.TextWidth(AName), Canvas.TextHeight(AName));
    Master.RIN_StartEvent;
    if FViewPreset<>nil then begin
      DestroyViewPreset;
      CreateViewPreset;
      //to call DoSetBounds
      Height:=0;
      DoAlign;
    end;
    Repaint;
    Master.RIN;
  end;
end;

procedure TPPPPreset.DoUpdate;
begin
  DoUpdateIfNecessary;
end;

procedure TPPPPreset.UpdateAll;
begin
  if not DoUpdateIfNecessary then
    if FViewPreset <> nil
      then FViewPreset.UpdateAllParams;
end;

procedure TPPPPreset.DoSetExpanded(AStore: Boolean = true);
var
  AIndex: Integer;
begin
  if FOldExpanded=FExpanderBtn.Switched
    then exit;
  if FExpanderBtn.Switched then begin
    CreateViewPreset;
    if AStore
      then TPresetEditor(Master).ExpandedParams.Add(Path);
  end else begin
    DestroyViewPreset;
    if AStore
      then TPresetEditor(Master).ExpandedParams.Remove(Path);
  end;
  SetBounds(Left, Top, Width, Height);
  FOldExpanded:=FExpanderBtn.Switched;
end;

procedure TPPPPreset.RouteConnections; inline;
var
  AItem: TObjectListItem;
begin
  AItem:=Connections.Connections.First;
  while AItem<>nil do begin
    FViewPreset.RouteConnection(TConnection(AItem.Content));
    AItem:=AItem.Next;
  end;
end;

procedure TPPPPreset.DestroyViewPreset;
var
  AViewPreset: TViewPreset;
begin
  AViewPreset:=FViewPreset;
  //set FViewPreset to nil before destruction to avoid routing
  FViewPreset:=nil;
  AViewPreset.Destroy;
end;

procedure TPPPPreset.CreateViewPreset;
begin
  FViewPreset:=TViewPreset.Create(Self, ParentRouter, Painter, Param, Path);
  FViewPreset.OnBoundsChanged:=@ViewPresetBoundsChanged;
  RouteConnections;
end;

procedure TPPPPreset.SetExpanded(Value: Boolean);
begin
  FExpanderBtn.Switched:=Value;
  DoSetExpanded;
end;

procedure TPPPPreset.ExpanderBtnClick(Sender: TSlaveControl);
begin
  DoSetExpanded;
end;

procedure TPPPPreset.EditTagsBtnClick(Sender: TSlaveControl);
var
  APos          : TPoint;
  AVisualisation: IPVisualisation;
begin
  with IPPreset(Param) do begin
    AVisualisation:=PresetUtil[FPresetID];
    APos:=Point(AbsoluteRect.Left + FEditTagsBtn.Left, AbsoluteRect.Top + DEFAULTPARAMHEIGHT);
    APos:=Master.ClientToScreen(APos);
    Assert(PresetTagEditForm <> nil);
    PresetTagEditForm.EditTags(APos.X, APos.Y, AVisualisation);
  end;
end;

procedure TPPPPreset.AddPresetBtnClick(Sender: TSlaveControl);
var
  ANewPresetID, AOldPresetID: TPPresetID;
begin
  ANewPresetID:=MakeTemplate(FPresetID).ID;
  IPPreset(PresetUtil[FPresetID][ParamID(PARENTINPUTNAME, vPreset)]).Value:=ANewPresetID;
  GlobalVisualisationAliveMap.SetBufferedVersion(Param.Owner.ID, Param.ID.Name, ANewPresetID, FPresetID);
  with IPPreset(Param) do begin
    AOldPresetID:=Value;
    Value:=ANewPresetID;
    GlobalVisualisationAliveMap.SetBufferedVersion(Param.Owner.ID, Param.ID.Name, AOldPresetID, NULLVISID);
  end;
end;

procedure TPPPPreset.SavePresetBtnClick(Sender: TSlaveControl);
begin
  //with IPPreset(Param) do begin
    if LCLAdvFunc.AdvMsg('Are you sure?', TdMFrage) = mrYes
      then SaveToParent(PresetUtil[FPresetID]);
  //end;
end;

procedure TPPPPreset.PresetSelected(Sender: TObject);
begin
  if SelectPresetForm2.Success
    then IPPreset(Param).Value:=SelectPresetForm2.Selected;
end;

procedure TPPPPreset.ViewPresetBoundsChanged(Sender: TSlaveControl);
begin
  //just to call DoSetBounds...
  Height:=0;
end;

function TPPPPreset.GetSubRouter: TConnectionRouter;
begin
  if FViewPreset <> nil
    then Result:=FViewPreset
    else Result:=nil;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  TParamPainter.AddTypeEdit(vPreset, TPPPPreset);
end;

{%EDNREGION}

end.

