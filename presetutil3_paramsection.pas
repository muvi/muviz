unit PresetUtil3_ParamSection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, PresetUtil3_Plugs, PresetUtil3_Configuration,
  ParamPainter2, VisType2, PresetType, AdvCoord, Graphics, CanvasGraphX32,
  AdvFunc, MStrings, PresetUtil3_Connections, PresetUtil3_ConnectionRouter,
  PresetUtil3_WireSlot, PresetUtil3_Path, StrEscapeUtils, VisDebugUtils,
  Controls, Dialogs, PresetUtil3_BasicParamSection;

type
  TSortable         = class
  strict protected
    function GetSortedBy: IPParam; virtual; abstract;
    procedure SetSortedBy(AValue: IPParam); virtual; abstract;
  public
    property SortedBy: IPParam read GetSortedBy write SetSortedBy;
  end;

  TParamSection     = class (TRoutableParamSection)
  private
    FInputPlug  : TVPVInputPlug;
    FOutputPlug : TVPVOutputPlug;
    FEdit       : TPPParam;
    FPresetParam: IPParam;
    FConnector  : TConnector;
    FConnections: TRoutableConnections;
    FWireSlot   : TWireSlot;
    FSortable   : TSortable;
    procedure SetFont; inline;
  strict protected
    function GetExpectedHeight: Integer; override;
    function GetEdit: TPPParam; override;
    function GetConnector: TConnector; override;
    function GetID: TPParamID; override;
    function GetMinWidth: Integer; override;

    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoAlign; override;
    procedure DoPaint; override;

    procedure EditBoundsChanged(Sender: TSlaveControl);
  public
    constructor Create(AOwner: TSlaveControl; ARouter: TConnectionRouter; APainter: TParamPainter; APath: IParamPath; APresetParam: IPParam; ASortable: TSortable; AAdditionalInformation: TObject = nil); reintroduce;
    destructor Destroy; override;
    procedure UpdateEdit;
    //the constructor does NOT do this automatically
    procedure ContextCreated; override;
    property PresetParam: IPParam read FPresetParam;
  end;

implementation

const
  WIRESLOTWIDTH   = 100;
  WIRESLOTHEIGHT  = 6;

{%REGION TParamSection}

constructor TParamSection.Create(AOwner: TSlaveControl; ARouter: TConnectionRouter; APainter: TParamPainter; APath: IParamPath; APresetParam: IPParam; ASortable: TSortable; AAdditionalInformation: TObject = nil);
begin
  Assert(APresetParam<>nil);
  inherited Create(AOwner);
  FPresetParam:=APresetParam;
  FConnector:=TConnector.Create;
  FConnections:=TRoutableConnections.Create(FConnector);
  FSortable:=ASortable;

  FEdit:=APainter.NewParam(Self, FConnections, ARouter, APresetParam, APath, nil, AAdditionalInformation);
  FInputPlug:=TVPVInputPlug.Create(Self, FConnector, APainter.Config, APainter.WireManager, APath, ARouter);
  FOutputPlug:=TVPVOutputPlug.Create(Self, FConnector, APainter.Config, APainter.WireManager, APath, ARouter);
  //create after edit, because the edit may propagate the connection
  FWireSlot:=TWireSlot.Create(Self, FConnections);
end;

destructor TParamSection.Destroy;
begin
  FWireSlot.RemoveHandlers;
  //destroy the edits seperate and not autmatically to avoid a lot of bugs...
  FEdit.Destroy;
  FInputPlug.Destroy;
  FOutputPlug.Destroy;
  FWireSlot.Destroy;
  FConnections.Destroy;
  FConnector.Destroy;
  FPresetParam:=nil;
  inherited Destroy;
end;

procedure TParamSection.UpdateEdit;
begin
  Assert(FEdit <> nil);
  FEdit.UpdateAll;
end;

procedure TParamSection.ContextCreated;
begin
  FEdit.ContextCreated;
end;

function TParamSection.GetExpectedHeight: Integer;
begin
  Result:=FEdit.Height + 3*PARAMTOPSPACE + PARAMNAMESIZE;
end;

function TParamSection.GetID: TPParamID;
begin
  Result:=FPresetParam.ID;
end;

function TParamSection.GetEdit: TPPParam;
begin
  Result:=FEdit;
end;

function TParamSection.GetConnector: TConnector;
begin
  Result:=FConnector;
end;

procedure TParamSection.EditBoundsChanged(Sender: TSlaveControl);
begin
  Height:=GetExpectedHeight;
  Master.Repaint(Owner);
end;

function TParamSection.GetMinWidth: Integer;
var
  AFontWidth: Integer;
begin
  Result:=FEdit.MinWidth;
  if Result >= 0 then begin
    SetFont;
    AFontWidth:=Canvas.TextWidth(FPresetParam.ID.Name);
    Canvas.Font.Height:=0;
    if Result < AFontWidth
      then Result:=AFontWidth;
    Result += FInputPlug.Width + 2*KNOBSPACE + FOutputPlug.Width;
  end;
end;

procedure TParamSection.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  ADebugParam       : IDebugParam;
  ADebugAttachObject: IDebugAttachObject;
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if (Button = mbRight) and (ssShift in Shift) then begin
    FPresetParam.QueryInterface(IDebugParam, ADebugParam);
    ADebugParam.GetPrototype.QueryInterface(IDebugAttachObject, ADebugAttachObject);
    ShowMessage(ADebugAttachObject.GetAttachedParams);
  end;
  if (FSortable <> nil)
    and (Button = mbLeft)
    //and (ssCtrl in Shift)
    and (X > FInputPlug.Width)
    and (X < Width - FOutputPlug.Width)
    and (Y < 2 * PARAMTOPSPACE + PARAMNAMESIZE)
    then begin
      if FSortable.SortedBy <> PresetParam
        then FSortable.SortedBy:=PresetParam
        else FSortable.SortedBy:=nil;
    end;
end;

procedure TParamSection.DoAlign;
begin
  inherited DoAlign;
  //FInputPlug
  FInputPlug.SetBounds(0, 0, 0, Height);
  //FEdit
  FEdit.OnBoundsChanged:=nil;
  FEdit.SetBounds(
    FInputPlug.Width + KNOBSPACE,
    2 * PARAMTOPSPACE + PARAMNAMESIZE,
    Width - FInputPlug.Width - FOutputPlug.Width - 2*KNOBSPACE,
    0);
  FEdit.OnBoundsChanged:=@EditBoundsChanged;
  //FOutputPlug
  FOutputPlug.SetBounds(Width, 0, 0, Height);
  //FWireSlot
  FWireSlot.SetBounds(
    (Width - WIRESLOTWIDTH) div 2,
    PARAMTOPSPACE + (PARAMNAMESIZE - WIRESLOTHEIGHT) div 2,
    WIRESLOTWIDTH,
    WIRESLOTHEIGHT);
end;

procedure TParamSection.DoPaint;
var
  AStr: string;
begin
  Canvas.Pen.Width:=1;
  Canvas.Pen.Color:=PARAM_BGCOLOR;
  Canvas.Brush.Color:=PARAM_BGCOLOR;
  DrawCutRect(Canvas, AbsoluteRect.Left, AbsoluteRect.Top, AbsoluteRect.Right, AbsoluteRect.Bottom, 4, 4);
  //draw param name
  Canvas.Font.Color:=clBlack;
  SetFont;
  if FSortable.SortedBy = PresetParam
    then AStr:='>> ' + FPresetParam.ID.Name
    else AStr:=FPresetParam.ID.Name;
  AStr:=StrShorten({EscapeStr(}AStr{, ADVANCEDCHARS)}, @GetTextWidth, FEdit.Width);
  Canvas.TextOut(
    AbsoluteRect.Left + FEdit.Left,
    ABsoluteRect.Top + PARAMTOPSPACE,
    AStr);
  Canvas.Font.Height:=0;
end;

procedure TParamSection.SetFont; inline;
begin
  Canvas.Font.Height:=PARAMNAMESIZE;
end;

{%ENDREGION}

end.

