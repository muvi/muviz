unit AdvGraphCtrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Controls, ObjectClassBasic;

const
  clDefaultParamHint = $383838;

type
  TGraphicsHint   = class
  private
    FVisible  : Boolean;
    FEditable : Boolean;
    FMouseOver: Boolean;
    FEditing  : Boolean;
    FPos,FSize: TPoint;
    FS        : ^string;
    FCheckRect: TRect;

    FCanvas   : TCanvas;
    FOwner    : TCustomControl;
    FHintColor: TColor;
    FEdit     : TEdit;

    FOnHide   : TNotifyEvent;
    FOnDraw   : TNotifyEvent;
  protected
    procedure FEditChange(Sender: TObject); virtual;
    procedure FEditEnter(Sender: TObject); virtual;
    procedure FEditExit(Sender: TObject); virtual;
    procedure FEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
  public
    constructor Create(AOwner: TCustomControl; ACanvas: TCanvas);
    destructor Destroy;
    procedure DrawHint(X,Y: Integer; const AHintSize: TPoint; var AStr: string; const AAddRect: TRect; AEditable: Boolean = false); overload;
    procedure DrawHint(X,Y: Integer; var AStr: string; const AAddRect: TRect; AEditable: Boolean = false); overload;
    function HintSize(const AStr: string): TPoint;
    procedure FinallyDrawHint;
    procedure ClearHint;
    procedure EndEdit;
    function CheckMouseOver(X,Y: Integer): Boolean; //gibt zurück, ob die Maus auf dem Hinweis steht
  published
    property Color: TColor read FHintColor write FHintColor default clDefaultParamHint;
    property Editing: Boolean read FEditing;
    property Visible: Boolean read FVisible;

    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

  TKCControl      = class (TObjectItem)
  private
    FVisible  : Boolean;
    FCanvas   : TCanvas;
    FDrawPos  : TRect;
    FAutoPaint: Boolean;
    function GetLeft: Integer;
    procedure SetLeft(Value: Integer);
    function GetTop: Integer;
    procedure SetTop(Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
  protected
    property Canvas: TCanvas read FCanvas;
    property DrawPos: TRect read FDrawPos;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    procedure Paint; virtual; abstract;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
  public
    constructor Create(ACanvas: TCanvas; ALeft,ATop,AWidth,AHeight: Integer; AAutoPaint: Boolean = true);
    destructor Destroy; override;
    procedure Repaint;
    property AutoPaint: Boolean read FAutoPaint write FAutoPaint;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Visible: Boolean read FVisible write FVisible;
  end;

implementation

{TGraphicsHint}

constructor TGraphicsHint.Create(AOwner: TCustomControl; ACanvas: TCanvas);
begin
  inherited Create;
  FOwner:=AOwner;
  FCanvas:=ACanvas;
  FMouseOver:=false;
  FVisible:=false;
  FHintColor:=clDefaultParamHint;
  FEdit:=TEdit.Create(FOwner);
  FEdit.Visible:=false;
  FEdit.AutoSize:=false;
  FEdit.OnEnter:=@FEditEnter;
  FEdit.OnExit:=@FEditExit;
  FEdit.OnKeyDown:=@FEditKeyDown;
  FOwner.InsertControl(FEdit);
  FEditing:=false;
end;

destructor TGraphicsHint.Destroy;
begin
  FOwner.RemoveControl(FEdit);
  FEdit.OnChange:=nil;
  FEdit.Destroy;
  FS:=nil;
  inherited Destroy;
end;

procedure TGraphicsHint.DrawHint(X,Y: Integer; var AStr: string; const AAddRect: TRect; AEditable: Boolean = false);
begin
  DrawHint(X,Y,HintSize(AStr),AStr,AAddRect,AEditable);
end;

procedure TGraphicsHint.DrawHint(X,Y: Integer; const AHintSize: TPoint; var AStr: string; const AAddRect: TRect; AEditable: Boolean = false);
begin
  if FEditing then EndEdit;
  FVisible:=true;
  FPos.X:=X;
  FPos.Y:=Y;
  FSize:=AHintSize;
  FS:=@AStr;
  FCheckRect:=Classes.Rect(FPos.X-AAddRect.Left,FPos.Y-AAddRect.Top,FPos.X+FSize.X+AAddRect.Right,FPos.Y+FSize.Y+AAddRect.Bottom);
  FEditable:=AEditable;

  FEdit.OnChange:=nil;
  FEdit.Text:=AStr;
  FEdit.Left:=X+5;
  FEdit.Top:=Y+2;
  FEdit.Width:=AHintSize.X-10;
  FEdit.Height:=AHintSize.Y-4;
  FEdit.OnChange:=@FEditChange;
  if Assigned(FOnDraw) then FOnDraw(Self);
end;

function TGraphicsHint.HintSize(const AStr: string): TPoint;
var
  AOldStyle: TFontStyles;
begin
  AOldStyle:=FCanvas.Font.Style;
  FCanvas.Font.Style:=[fsBold,fsItalic];
  with Result do begin
    X:=FCanvas.TextWidth(AStr)+20;
    Y:=FCanvas.TextHeight('Wg')+10;
  end;
  FCanvas.Font.Style:=AOldStyle;
end;

procedure TGraphicsHint.FinallyDrawHint;
begin
  if not FVisible then exit;
  FCanvas.Pen.Color:=FHintColor;
  FCanvas.Pen.Style:=psSolid;
  FCanvas.Brush.Color:=FHintColor;
  FCanvas.Font.Color:=clWhite;
  FCanvas.Font.Style:=[fsBold,fsItalic];
  FCanvas.RoundRect(FPos.X,FPos.Y,FPos.X+FSize.X,FPos.Y+FSize.Y,10,10);
  FCanvas.TextOut(FPos.X+10,FPos.Y+5,FS^);
end;

procedure TGraphicsHint.ClearHint;
begin
  if (not FMouseOver) and (not FEditing) then FVisible:=false;
end;

procedure TGraphicsHint.EndEdit;
begin
  FEditing:=false;
  if not FMouseOver then begin
    FEdit.Visible:=false;
    FVisible:=false;
    if Assigned(FOnHide) then FOnHide(Self);
  end;
end;

function TGraphicsHint.CheckMouseOver(X,Y: Integer): Boolean;
begin
  Result:=(FVisible and (X>=FCheckRect.Left) and (X<=FCheckRect.Right) and (Y>=FCheckRect.Top) and (Y<=FCheckRect.Bottom));
  FMouseOver:=Result;
  if FEditing then exit;
  if Result
    then FEdit.Visible:=(X>=FPos.X) and (X<=FPos.X+FSize.X) and (Y>=FPos.Y) and (Y<=FPos.Y+FSize.Y)
    else FEdit.Visible:=false;
  if not FMouseOver then if Assigned(FOnHide) then begin
    FVisible:=false;
    FOnHide(Self);
  end;
end;

procedure TGraphicsHint.FEditChange(Sender: TObject);
begin
  FS^:=FEdit.Text;
end;

procedure TGraphicsHint.FEditEnter(Sender: TObject);
begin
  FEditing:=true;
end;

procedure TGraphicsHint.FEditExit(Sender: TObject);
begin
  EndEdit;
end;

procedure TGraphicsHint.FEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=13 then EndEdit;
end;

{TKCControl}

constructor TKCControl.Create(ACanvas: TCanvas; ALeft,ATop,AWidth,AHeight: Integer; AAutoPaint: Boolean = true);
begin
  inherited Create;
  FCanvas:=ACanvas;
  FVisible:=true;
  FAutoPaint:=AAutoPaint;
  SetBounds(ALeft,ATop,AWidth,AHeight);
end;

destructor TKCControl.Destroy;
begin
  inherited Destroy;
end;

procedure TKCControl.Repaint;
begin
  if not FVisible then exit;
  Paint;
end;

procedure TKCControl.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
begin
  with FDrawPos do begin
    Left:=ALeft;
    Top:=ATop;
    Right:=ALeft+AWidth;
    Bottom:=ATop+AHeight;
  end;
  if FAutoPaint then Repaint;
end;

function TKCControl.GetLeft: Integer;
begin
  Result:=FDrawPos.Left;
end;

procedure TKCControl.SetLeft(Value: Integer);
begin
  with FDrawPos do begin
    Right+=Value-Left;
    Left:=Value;
  end;
  if FAutoPaint then Repaint;
end;

function TKCControl.GetTop: Integer;
begin
  Result:=FDrawPos.Top;
end;

procedure TKCControl.SetTop(Value: Integer);
begin
  with FDrawPos do begin
    Bottom+=Value-Top;
    Top:=Value;
  end;
  if FAutoPaint then Repaint;
end;

function TKCControl.GetWidth: Integer;
begin
  with FDrawPos do Result:=Right-Left;
end;

procedure TKCControl.SetWidth(Value: Integer);
begin
  with FDrawPos do Right:=Left+Value;
  if FAutoPaint then Repaint;
end;

function TKCControl.GetHeight: Integer;
begin
  with FDrawPos do Result:=Bottom-Top;
end;

procedure TKCControl.SetHeight(Value: Integer);
begin
  with FDrawPos do Bottom:=Top+Value;
  if FAutoPaint then Repaint;
end;

end.

