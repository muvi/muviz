unit PresetUtil3_ViewPreset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, PresetType, VisType2, PresetUtil3_Plugs,
  ParamType2, PresetUtil3_DummyPlugs, ParamPainter2, StdParamEdits,
  MiscSlaveControls, Controls, AdvCoord, StdParamTypes, MExceptions,
  PresetUtil3_Configuration, PresetUtil3_Wires, DebugTools, Graphics, AdvFunc,
  DropButton, DragPlugUnit, PresetUtilities, Dialogs, MStrings, ParamSorter2,
  PresetUtil3_ParamSection, PresetUtil3_Connections, PresetUtil3_Path,
  PresetUtil3_ConnectionRouter, PresetUtil3_PointingRouter,
  PresetUtil3_BasicParamSection, PresetUtil3_SeperatorSection;

type
  TViewPresetSortable = class (TSortable)
  strict private
    FParamSorter: TVisualisationParamSorter;
  strict protected
    function GetSortedBy: IPParam; override;
    procedure SetSortedBy(AValue: IPParam); override;
  public
    constructor Create(AParamSorter: TVisualisationParamSorter);
  end;

  TViewPreset         = class (TPointingRouter)
  strict private
    //other stuff
    FParam            : IPPreset;
    FPreset           : IPVisualisation;
    FPainter          : TParamPainter;
    FParamSections    : array of TBasicParamSection;
    FDropButtons      : array of TDropButton;
    FExpectedHeight   : Integer;
    FParams           : TVisualisationParamSorter;
    FSortable         : TViewPresetSortable;
    function GetParamSectionMinWidth(AIndex: Integer): Integer; inline;
    procedure CalcExpectedHeight(AWidth: Integer);
    procedure CalcExpectedHeight;
    function InputCanDrop(Sender: TDropButton): Boolean;
    procedure InputDropped(Sender: TSlaveControl);
    procedure ParamsChanged(Sender: TObject);
  strict protected
    procedure DoPaint; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoAlign; override;
    procedure ParamSectionBoundsChanged(Sender: TSlaveControl);
    procedure GrabParams;
    procedure ClearParams;
    function GetSubRouter(AID: TPParamID): TConnectionRouter; override;
  public
    constructor Create(AOwner: TSlaveControl; AParentRouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath); reintroduce;
    destructor Destroy; override;
    procedure UpdateAllParams;
    class function MayCreate(AParam: IPParam; APresetUtil: IPPresetUtil): Boolean;
    procedure RouteConnection(AConnection: TConnection); override;
    function HighestInversePriority(AID: TPParamID): LongInt; override;
    function AddParam(AName: IString; AValue: IPParam; AIndex: Integer = -1; AAdditionalInformation: TObject = nil): TPPParam; override;
    procedure RemoveParam(AParam: TParamSection);
    procedure RemoveParam(AParam: TPPParam); override;
    //property ShowInvisible: Boolean read FShowInvisible write SetShowInvisible;
  end;

const
  ConnectedSeperatorName    = 'Connected';
  NotConnectedSeperatorName = 'Not Connected';

implementation

{%REGION TViewPresetSortable}

constructor TViewPresetSortable.Create(AParamSorter: TVisualisationParamSorter);
begin
  inherited Create;
  FParamSorter:=AParamSorter;
end;

function TViewPresetSortable.GetSortedBy: IPParam;
begin
  Result:=FParamSorter.SortedBy;
end;

procedure TViewPresetSortable.SetSortedBy(AValue: IPParam);
begin
  FParamSorter.SortedBy:=AValue;
end;

{%ENDREGION}
{%REGION TViewPreset}

constructor TViewPreset.Create(AOwner: TSlaveControl; AParentRouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath);
begin
  inherited Create(AOwner, AParentRouter, APath);
  FExpectedHeight:=0;
  if AParam.ID.&Type<>TPParamType(vPreset)
    then raise EIncompatibleParamType.Create('Preset-parameter (' + IntToStr(Integer(vPreset shr 32)) + '.' + IntToStr(Integer(vPreset)) + ') expected, but got ' + IntToStr(Integer(AParam.ID.&Type shr UInt64(32))) + '.' + IntToStr(Integer(AParam.ID.&Type)) + '.');
  FParam:=IPPreset(AParam);
  FPreset:=PresetUtil[FParam.ExecutedValue];
  FPainter:=APainter;
  FParams:=TVisualisationParamSorter.Create(FPreset, VisualisationUtil.MainThread);
  FSortable:=TViewPresetSortable.Create(FParams);
  FParams.OnChange:=@ParamsChanged;
  GrabParams;
end;

destructor TViewPreset.Destroy;
begin
  FParam:=nil;
  FPreset:=nil;
  FSortable.Destroy;
  FParams.Destroy;
  //do not call clear params. This is done automatoically by the properties of
  //TSlaveControl
  inherited Destroy;
end;

class function TViewPreset.MayCreate(AParam: IPParam; APresetUtil: IPPresetUtil): Boolean;
begin
  Result:=(AParam.ID.&Type = TPParamType(vPreset));
end;

procedure TViewPreset.UpdateAllParams;
var
  I: Integer;
begin
  for I:=0 to Length(FParamSections)-1 do begin
    if FParamSections[I].InheritsFrom(TParamSection)
      then TParamSection(FParamSections[I]).UpdateEdit;
  end;
end;

procedure TViewPreset.GrabParams;
var
  I, J: Integer;
begin
  if FParams.SortedBy = nil then begin;
    SetLength(FParamSections, FParams.Count);
    for I:=0 to FParams.Count-1
      do FParamSections[I]:=TParamSection.Create(Self, Self, FPainter, Path + FParams[I].ID, FParams[I], FSortable);
  end else begin
    SetLength(FParamSections, FParams.Count + 2);
    J:=0;
    for I:=0 to FParams.Count-1 do begin
      if I = 1 then begin
        FParamSections[J]:=TSeperatorSection.Create(Self, ConnectedSeperatorName);
        Inc(J);
      end;
      if I = FParams.ConnectedCount + 1 then begin
        FParamSections[J]:=TSeperatorSection.Create(Self, NotConnectedSeperatorName);
        Inc(J);
      end;
      FParamSections[J]:=TParamSection.Create(Self, Self, FPainter, Path + FParams[I].ID, FParams[I], FSortable);
      Inc(J);
    end;
  end;

  //there has to be one drop button more than inputs
  SetLength(FDropButtons, Length(FParamSections)+1);
  for I:=0 to Length(FDropButtons)-1 do begin
    FDropButtons[I]:=TDropButton.Create(Self);
    with FDropButtons[I] do begin
      Tag:=I;
      OnCanDrop:=@InputCanDrop;
      OnDropped:=@InputDropped;
    end;
  end;

  //do before updating, otherwise wires won't be at the right positions
  DoAlign;

  //first update of param sections
  //do this only after EVERY parameter is created because of pointers
  for I:=0 to Length(FParamSections)-1
    do FParamSections[I].ContextCreated;

  CalcExpectedHeight;
end;

procedure TViewPreset.ClearParams;
var
  I      : Integer;
begin
  for I:=0 to Length(FParamSections)-1
    do FParamSections[I].Destroy;
  SetLength(FParamSections, 0);

  for I:=0 to Length(FDropButtons)-1
    do FDropButtons[I].Destroy;
  SetLength(FDropButtons, 0);
end;

procedure TViewPreset.ParamsChanged(Sender: TObject);
begin
  Master.RIN_StartEvent;

  ClearParams;
  GrabParams;

  Repaint;
  Master.RIN;
end;

procedure TViewPreset.CalcExpectedHeight(AWidth: Integer);
var
  I, AMinWidth, ANextMinWidth, AInnerWidth, AHeight, AExpectedWidth: Integer;

  procedure UpdateHeight; inline;
  var
    AExpectedHeight: Integer;
  begin
    AExpectedHeight:=FParamSections[I].ExpectedHeight + VIEWPRESETITEMSPACE;
    if AHeight < AExpectedHeight
      then AHeight:=AExpectedHeight;
  end;

begin
  AInnerWidth:=AWidth - 2*VIEWPRESETKNOBLEFTRIGHT;
  if AInnerWidth < 0
    then AInnerWidth:=0;
  FExpectedHeight:=VIEWPRESETITEMSPACE*3; //top + std + bottom
  ANextMinWidth:=GetParamSectionMinWidth(0);
  I:=0;
  while I < Length(FParamSections) do begin
    AExpectedWidth:=0;
    AHeight:=0;
    AMinWidth:=ANextMinWidth;
    ANextMinWidth:=GetParamSectionMinWidth(I+1);
    while (AMinWidth >= 0) and (ANextMinWidth >= 0) and (AExpectedWidth + AMinWidth + ANextMinWidth < AInnerWidth) do begin
      AExpectedWidth += AMinWidth;
      UpdateHeight;
      Inc(I);
      AMinWidth:=ANextMinWidth;
      ANextMinWidth:=GetParamSectionMinWidth(I+1);
    end;

    UpdateHeight;
    FExpectedHeight+=AHeight;
    Inc(I);
  end;
end;

procedure TViewPreset.CalcExpectedHeight;
begin
  Master.RIN_StartEvent;

  CalcExpectedHeight(Width);
  if Height <> FExpectedHeight
    then Height:=FExpectedHeight
    else DoAlign;

  Master.RIN;
end;

procedure TViewPreset.DoPaint;
begin
  Canvas.Pen.Color:=clSilver;
  Canvas.Brush.Color:=clGray;
  Canvas.Font.Color:=clBlack;
  Canvas.Font.Height:=0;
  Canvas.RoundRect(AbsoluteRect,10,10);
end;

procedure TViewPreset.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  inherited DoSetBounds(ANewBounds);
  if ANewBounds.Width <> Width
    then CalcExpectedHeight(ANewBounds.Width);
  ANewBounds.Height:=FExpectedHeight;
end;


function TViewPreset.GetParamSectionMinWidth(AIndex: Integer): Integer; inline;
begin
  if AIndex < Length(FParamSections) then begin
    Result:=FParamSections[AIndex].MinWidth;
    if Result>=0
      then Result += VIEWPRESETITEMSPACE;
  end else Result:=-1;
end;

procedure TViewPreset.DoAlign;
var
  I, L, ATop, ALeft, AMinWidth, ANextMinWidth, ANewTop, AInnerWidth, ANewLeft: Integer;
  //needs to have no sign

  procedure DoSetParamSectionBounds(AWidth: Integer);
  var
    AHeight: Integer;
  begin
    with FParamSections[I] do begin
      OnBoundsChanged:=nil;
      SetBounds(ALeft, ATop, AWidth, 0);
      OnBoundsChanged:=@ParamSectionBoundsChanged;
      ALeft += AMinWidth;
      //BoundsRect.Bottom = Top + Height + 1
      AHeight:=BoundsRect.Bottom + VIEWPRESETITEMSPACE + 1;
      if ANewTop < AHeight
        then ANewTop:=AHeight;
    end;
  end;

  procedure CalcNewLeft;
  begin
    AMinWidth:=ANextMinWidth;
    ANextMinWidth:=GetParamSectionMinWidth(I+1);
    if (AMinWidth >= 0) and (ANextMinWidth >= 0)
      then ANewLeft:=ALeft + AMinWidth + ANextMinWidth
      else ANewLeft:=MaxInt;
  end;

begin
  AInnerWidth:=Width - 2*VIEWPRESETKNOBLEFTRIGHT;
  if AInnerWidth < 0
    then AInnerWidth:=0;
  L:=Length(FParamSections);
  FDropButtons[0].SetBounds(VIEWPRESETKNOBLEFTRIGHT, VIEWPRESETITEMSPACE, Width - 2*VIEWPRESETKNOBLEFTRIGHT, VIEWPRESETITEMSPACE);

  if Length(FParamSections) > 0 then begin
    ANewTop:=VIEWPRESETITEMSPACE*2;
    ANextMinWidth:=GetParamSectionMinWidth(0);
    I:=0;
    while I < L do begin
      ATop:=ANewTop;
      ALeft:=VIEWPRESETKNOBLEFTRIGHT;

      CalcNewLeft;
      //getMinWidth returns MaxInt if I is too high, so no check is necessary here
      while ANewLeft < AInnerWidth do begin
        DoSetParamSectionBounds(AMinWidth - VIEWPRESETITEMSPACE);
        FDropButtons[I+1].SetBounds(ALeft - VIEWPRESETITEMSPACE, ATop, VIEWPRESETITEMSPACE, FParamSections[I].Height);
        Inc(I);
        CalcNewLeft;
      end;

      DoSetParamSectionBounds(Width - VIEWPRESETKNOBLEFTRIGHT - ALeft);
      FDropButtons[I+1].SetBounds(VIEWPRESETKNOBLEFTRIGHT, ANewTop - VIEWPRESETITEMSPACE, AInnerWidth, VIEWPRESETITEMSPACE);
      Inc(I);
    end;
  end;
end;

procedure TViewPreset.ParamSectionBoundsChanged(Sender: TSlaveControl);
begin
  CalcExpectedHeight;
  Master.Repaint(Owner);
end;

function TViewPreset.InputCanDrop(Sender: TDropButton): Boolean;
begin
  Result:=DraggingValue;
end;

procedure TViewPreset.InputDropped(Sender: TSlaveControl);
var
  AIndex  : Integer;
  ANewName: string;
begin
  ANewName:=Dialogs.InputBox('New Input', 'How to call the new input?', '');
  if ANewName<>'' then begin
    //add the preset input
    AIndex:=TDropButton(Sender).Tag;
    AddParam(ANewName, DraggedValue, AIndex);
  end;
  StopDraggingValue;
end;

function TViewPreset.AddParam(AName: IString; AValue: IPParam; AIndex: Integer = -1; AAdditionalInformation: TObject = nil): TPPParam;
var
  L            : Integer;

  procedure OpenSectionSpace; inline;
  var
    I: Integer;
  begin
    SetLength(FParamSections, L+1);
    for I:=L downto AIndex+1
      do FParamSections[I]:=FParamSections[I-1];
  end;

var
  AParamSection: TParamSection;
  APresetParam : IPParam;
begin
  Master.RIN_StartEvent;

  {
  //add drop button
  L:=Length(FDropButtons);
  SetLength(FDropButtons, L+1);

  FDropButtons[L]:=TDropButton.Create(Self);
  with FDropButtons[L] do begin
    Tag:=L;
    OnCanDrop:=@InputCanDrop;
    OnDropped:=@InputDropped;
  end;

  //add param
  L:=Length(FParamSections);
  if AIndex < 0
    then AIndex:=L;
  }

  APresetParam:=PresetUtilities.InsertInput(AValue, AName, FPreset, FParams);

  {
  //add param section
  //to avoid crash in some threading situations
  if AIndex >= L
    then AIndex:=L;
  OpenSectionSpace;

  AParamSection:=TParamSection.Create(Self, Self, FPainter, Path + APresetParam.ID, APresetParam, FSortable, AAdditionalInformation);
  FParamSections[AIndex]:=AParamSection;

  DoAlign;

  AParamSection.ContextCreated;
  Result:=AParamSection.Edit;

  CalcExpectedHeight;
  }

  Master.RIN;
end;

procedure TViewPreset.RemoveParam(AParam: TParamSection);
var
  L : Integer;

  procedure FindAndRemove;
  var
    I: Integer;
  begin
    L:=Length(FParamSections);
    Assert(L > 0);
    I:=0;
    while FParamSections[I]<>AParam do begin
      Inc(I);
      Assert(I < L);
    end;
    while I <= L-2 do begin
      FParamSections[I]:=FParamSections[I+1];
      Inc(I);
    end;
    SetLength(FParamSections, L-1);
  end;

var
  APresetParam: IPParam;
begin
  Master.RIN_StartEvent;

  FindAndRemove;
  L:=Length(FDropButtons);
  FDropButtons[L-1].Destroy;
  SetLength(FDropButtons, L-1);
  //save to securely remove the param after destroying everything visible
  APresetParam:=AParam.PresetParam;
  AParam.Destroy;

  PresetUtilities.RemoveInput(APresetParam, FPreset);

  //recalculate bounds
  CalcExpectedHeight;

  Master.RIN;
end;

procedure TViewPreset.RemoveParam(AParam: TPPParam);
begin
  RemoveParam(TParamSection(AParam.Owner));
end;

procedure TViewPreset.RouteConnection(AConnection: TConnection);
var
  I  : Integer;
  AID: TPParamID;
begin
  //if the wire end is open
  if FPainter.WireManager.TryRouteOpen(AConnection)
    then exit;
  AID:=AConnection.NextRoutingID;
  for I:=0 to Length(FParamSections)-1 do begin
    if (FParamSections[I].Routable) and (TRoutableParamSection(FParamSections[I]).ID = AID) then begin
      TRoutableParamSection(FParamSections[I]).Connector.AddConnection(AConnection);
      exit;
    end;
  end;
end;

function TViewPreset.GetSubRouter(AID: TPParamID): TConnectionRouter;
var
  I: Integer;
begin
  for I:=0 to Length(FParamSections)-1 do begin
    if (FParamSections[I].Routable) and (TRoutableParamSection(FParamSections[I]).ID = AID) then begin
      Result:=TRoutableParamSection(FParamSections[I]).Edit.SubRouter;
      exit;
    end;
  end;
  Result:=nil;
end;

function TViewPreset.HighestInversePriority(AID: TPParamID): LongInt;
var
  I: Integer;
begin
  for I:=0 to Length(FParamSections)-1 do begin
    if (FParamSections[I].Routable) and (TRoutableParamSection(FParamSections[I]).ID = AID) then begin
      Result:=TRoutableParamSection(FParamSections[I]).Edit.Param.HighestInversePriority;
      exit;
    end;
  end;
end;

{%ENDREGION}

end.

