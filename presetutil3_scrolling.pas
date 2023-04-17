unit PresetUtil3_Scrolling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, PresetUtil3, StdCtrls, Forms, DragPlugUnit;

type
  TScrollingPresetEditor = class (TCustomControl)
  strict private
    FPresetEditor  : TPresetEditor;
    FScrollBar     : TScrollBar;
    FScrollLeftover: Integer; //used for mouse wheel event
    FFirstPaintTime: Boolean; //used to set the ScrollIncrement to the TextHeight
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure PresetEditorResize(Sender: TObject);
    function GetScrollIncrement: Integer;
    procedure SetScrollIncrement(AValue: Integer);
    procedure PresetEditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PresetEditor: TPresetEditor read FPresetEditor;
    property ScrollIncrement: Integer read GetScrollIncrement write SetScrollIncrement;
  end;

const
  //WheelDelta change caused by one wheel click
  //constant set according to microsofts WM_MouseWheel definition
  WHEEL_DELTA = 120;

implementation

{%REGION TScrollingPresetEditor}

constructor TScrollingPresetEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FScrollBar
  FScrollBar:=TScrollBar.Create(Self);
  with FScrollBar do begin
    Kind:=sbVertical;
    Align:=alRight;
    TabStop:=false;
    OnScroll:=@ScrollBarScroll;
  end;
  InsertControl(FScrollBar);
  //FPresetEditor
  FPresetEditor:=TPresetEditor.Create(Self);
  InsertControl(FPresetEditor);
  with FPresetEditor do begin
    Align:=alClient;
    OnMaxScrollPosChanged:=@PresetEditorResize;
    OnResize:=@PresetEditorResize;
    OnMouseWheel:=@PresetEditorMouseWheel;
  end;
  FScrollLeftover:=0;
  FFirstPaintTime:=true;

  DragPlugUnit.AddDestControl(FPresetEditor);
  DragPlugUnit.AddDestControl(FScrollBar);
  DragPlugUnit.AddDestControl(Self);
end;

destructor TScrollingPresetEditor.Destroy;
begin
  RemoveControl(FPresetEditor);
  RemoveControl(FScrollBar);
  FPresetEditor.Destroy;
  FScrollBar.Destroy;
  inherited Destroy;
end;

function TScrollingPresetEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  AScrollsize: Int64;
begin
  AScrollSize:=((WheelDelta*Int64(ScrollIncrement)) + FScrollLeftover);
  FPresetEditor.ScrollPos:=Point(
      FPresetEditor.ScrollPos.X,
      FPresetEditor.ScrollPos.Y - (AScrollSize div WHEEL_DELTA));
  FScrollLeftover:=AScrollSize mod WHEEL_DELTA;
  FScrollBar.Position:=FPresetEditor.ScrollPos.Y;
  Result:=true;
end;

procedure TScrollingPresetEditor.PresetEditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TScrollingPresetEditor.Paint;
begin
  inherited Paint;
  if FFirstPaintTime then begin
    //will raise an exception if executed in the constructor
    //because of undefined canvas, so do it here
    ScrollIncrement:=Canvas.TextHeight('Wg') * 4;
    FFirstPaintTime:=false;
  end;
end;

procedure TScrollingPresetEditor.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FPresetEditor.ScrollPos:=Point(FPresetEditor.ScrollPos.X, ScrollPos);
end;

procedure TScrollingPresetEditor.PresetEditorResize(Sender: TObject);
var
  AMaxScrollPos: Integer;
begin
  AMaxScrollPos:=FPresetEditor.MaxScrollPos.Y;
  FScrollBar.Max:=AMaxScrollPos+FPresetEditor.Height;
  FScrollBar.PageSize:=FPresetEditor.Height;
  FScrollBar.LargeChange:=FPresetEditor.Height;
  if FPresetEditor.ScrollPos.Y>FScrollBar.Max then FPresetEditor.ScrollPos:=Point(FPresetEditor.ScrollPos.X, FScrollBar.Max);
  FScrollBar.Visible:=(AMaxScrollPos>0);
end;

function TScrollingPresetEditor.GetScrollIncrement: Integer;
begin
  Result:=FScrollBar.SmallChange;
end;

procedure TScrollingPresetEditor.SetScrollIncrement(AValue: Integer);
begin
  FScrollBar.SmallChange:=AValue;
end;

{%ENDREGION}

end.

