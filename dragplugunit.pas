unit DragPlugUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, VisType2, LCLIntf, PlugImages;

type

  { TDragPlugForm }

  TDragPlugForm = class(TForm)
    ImageList: TImageList;
    Image: TImage;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FPlugImages: TPlugImages;
    FValue     : IPParam;
    function GetPlugImages: TPlugImages;
  public
    procedure DragValue(AValue: IPParam);
    procedure StopDraggingValue;
    procedure Init;
    property PlugImages: TPlugImages read GetPlugImages;
  end;

var
  DragPlugForm: TDragPlugForm = nil;

procedure DragValue(AValue: IPParam);
function StopDraggingValue: IPParam;
procedure InitDragging;
function DraggingValue: Boolean;
function DraggedValue: IPParam;
procedure AddDestControl(AControl: TControl);

implementation

var
  DestControls: array of TControl;

{%REGION TDragPlugForm}

procedure TDragPlugForm.FormDeactivate(Sender: TObject);
var
  I            : Integer;
  AMouseControl: TControl;
begin
  AMouseControl:=Application.MouseControl;
  for I:=0 to Length(DestControls)-1
    do if DestControls[I]=AMouseControl
      then exit;
  StopDraggingValue;
end;

procedure TDragPlugForm.FormCreate(Sender: TObject);
var
  AShape: TBitmap;
begin
  FPlugImages:=nil;
  AShape:=TBitmap.Create;
  AShape.Transparent:=false;
  ImageList.GetBitmap(10, AShape);
  //this is really needed (no idea why...)
  Self.Handle;
  SetShape(AShape);
  AShape.Destroy;
end;

procedure TDragPlugForm.FormDestroy(Sender: TObject);
begin
  if FPlugImages<>nil
    then FPlugImages.Destroy;
end;

procedure TDragPlugForm.TimerTimer(Sender: TObject);
var
  AMousePos: TPoint;
begin
  GetCursorPos(AMousePos);
  Left:=AMousePos.X;
  Top:=AMousePos.Y+1;
end;

procedure TDragPlugForm.Init;
begin
  if FPlugImages=nil then begin
    FPlugImages:=TPlugImages.Create;
    with FPlugImages do begin
      Images:=ImageList;
      PlugedImageIndex:=0;
      UnplugedImageIndex:=2;
      HighlightedPlugedImageIndex:=1;
      HighlightedUnplugedImageIndex:=3;
      ExtHighlightedPlugedImageIndex:=4;
      ExtHighlightedUnplugedImageIndex:=5;
      PlugedUndefinedImageIndex:=6;
      UnplugedUndefinedImageIndex:=8;
      HighlightedPlugedUndefinedImageIndex:=7;
      HighlightedUnplugedUndefinedImageIndex:=9;
      ExtHighlightedPlugedUndefinedImageIndex:=7;
      ExtHighlightedUnplugedUndefinedImageIndex:=9;
      InitPlugBmps;
    end;
  end;
end;

procedure TDragPlugForm.DragValue(AValue: IPParam);
begin
  //start dragging
  FValue:=AValue;
  TimerTimer(nil);
  PlugImages.LoadImage(true, false, false, AValue.ID.&Type);
  Image.Picture.Assign(PlugImages.OutputBmp);
  Show;
  Timer.Enabled:=true;
end;

procedure TDragPlugForm.StopDraggingValue;
begin
  Timer.Enabled:=false;
  FValue:=nil;
  Hide;
end;

function TDragPlugForm.GetPlugImages: TPlugImages;
begin
  Assert(FPlugImages <> nil);
  Result:=FPlugImages;
end;

{%ENDREGION}
{%REGION Misc}

procedure DragValue(AValue: IPParam);
begin
  Assert(DragPlugForm <> nil);
  DragPlugForm.DragValue(AValue);
end;

function StopDraggingValue: IPParam;
begin
  Assert(DragPlugForm <> nil);
  Result:=DragPlugForm.FValue;
  DragPlugForm.StopDraggingValue;
end;

procedure InitDragging;
begin
  Assert(DragPlugForm <> nil);
  DragPlugForm.Init;
end;

function DraggingValue: Boolean;
begin
  Result:=(DragPlugForm <> nil) and (DragPlugForm.FValue<>nil);
end;

function DraggedValue: IPParam;
begin
  if DragPlugForm <> nil
    then Result:=DragPlugForm.FValue
    else Result:=nil;
end;

procedure AddDestControl(AControl: TControl);
var
  L: Integer;
begin
  L:=Length(DestControls);
  SetLength(DestControls, L+1);
  DestControls[L]:=AControl;
end;

{%ENDREGION}

initialization
  {$I dragplugunit.lrs}
finalization
  SetLength(DestControls, 0);
end.

