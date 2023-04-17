unit ParamPainter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, VisType, ObjectClasses, ObjectClassBasic,
  AdvCoord, Controls, StdCtrls, Dialogs, SplitImageButtons, GraphX32, AdvFunc,
  Spin, Math, ColorChooseDlg, CanvasGraphX32;

type
  TGetParamVal  = procedure (Param: Pointer; AType: TVisParamType; out Value) of object;
  TSetParamVal  = procedure (Param: Pointer; AType: TVisParamType; const Value) of object;
  TPPParamState = (ppDefault,ppMouseOver,ppDown);
  TParamPainter = class;
  TPPParam      = class;
  TParamEvent   = procedure (Sender: TObject; Param: TPPParam) of object;
  TParamFunc    = function (Sender: TObject; Param: TPPParam): Boolean of object;

  TPPParam      = class (TObjectItem)
  private
    FParam     : Pointer;
    FParamName : string;
    FState     : TPPParamState;
    FPos       : TRect;
    FDrawPos   : TRect;
    FOwner     : TParamPainter;
    FType      : TVisParamType;
    FVisible   : Boolean;
    FClickPos  : TRect;
    function GetHeight: Integer;
    function GetLeft: Integer;
    procedure SetLeft(Value: Integer);
    function GetTop: Integer;
    procedure SetTop(Value: Integer);
    function GetWidth: Integer;
    function GetCanvas: TCanvas;
  protected
    procedure SetVal(var Value);
    procedure GetVal(out Value);
    procedure DoPaint; virtual;
    procedure DoMouseMove(X,Y: Integer); virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseLeave; virtual;
    procedure EditChanged; virtual; abstract;
    function IsEditing: Boolean;
    function IsMouseOver: Boolean;
    function WasEditing: Boolean;

    property Canvas: TCanvas read GetCanvas;
    property ClickPos: TRect read FClickPos write FClickPos;
    property Owner: TParamPainter read FOwner;
  public
    constructor Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop{,AWidth,AHeight}: Integer); virtual;
    procedure Paint(const AViewRect: TRect);
    procedure MouseMove(X,Y: Integer);
    procedure MouseDown;
    procedure MouseUp;
    procedure MouseLeave;
    procedure Resize; virtual;
    procedure SetWH(ALeft,ATop: Integer);

    property DrawPos: TRect read FDrawPos;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property Param: Pointer read FParam;
    property ParamName: string read FParamName;
    property Position: TRect read FPos;
    property State: TPPParamState read FState;
    property Top: Integer read GetTop write SetTop;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read GetWidth;
  end;

  TParamPainter = class
  private
    FAutoRepaint        : Boolean;
    FItems              : TObjectList;
    FGetProc            : TGetParamVal;
    FSetProc            : TSetParamVal;
    FMouseOverItem      : TPPParam;
    FCanvas             : TCanvas;
    FOwner              : TCustomControl;
    FViewRect           : TRect;
    FTempDrawBitmap     : TBitmap;
    FImages             : TImageList;
    FBoolBoxUI          : Integer;
    FBoolBoxUHI         : Integer;
    FBoolBoxDI          : Integer;
    FBoolBoxDHI         : Integer;
    FCallBoxUI          : Integer;
    FCallBoxUHI         : Integer;
    FCallBoxDI          : Integer;
    FWidth              : Integer;
    FHeight             : Integer;
    FCBUIBmp            : TBitmap;
    FCBUHIBmp           : TBitmap;
    FCBDIBmp            : TBitmap;
    FAutoReget          : Boolean;

    FEditingItem        : TPPParam;
    FLastEditingItem    : TPPParam;
    FAEdit              : TCustomEdit;
    FEdit               : TEdit;
    FSpinEdit           : TSpinEdit;
    FFloatSpinEdit      : TFloatSpinEdit;
    FColorDlg           : TChooseColorDialog;

    FOnPaint            : TParamEvent;
    FOnRequestBackground: TParamFunc;
    procedure SetImages(Value: TImageList);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetCallBoxUI(Value: Integer);
    procedure SetCallBoxUHI(Value: Integer);
    procedure SetCallBoxDI(Value: Integer);
    function GetCount: Integer;
    procedure AddEdit(AEdit: TCustomEdit); inline;
  protected
    procedure FEditChange(Sender: TObject); virtual;
    procedure FEditEnter(Sender: TObject); virtual;
    procedure FEditExit(Sender: TObject); virtual;
    procedure FEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure StartEdit(AEdit: TCustomEdit; AItem: TPPParam);
    procedure EndEdit;
    function Editing: Boolean;

    function RequestBackground(Param: TPPParam): Boolean;
    procedure Painted(Param: TPPParam);
    procedure PreDrawButton(ImgIndex: Integer; Btn: TBitmap);
    procedure PreDrawButton_(ImgIndex: Integer; Btn,t1,t2,t3: TBitmap);
    procedure ResizeAllBtns;
    procedure LoadImage(Index: Integer);
    property Canvas: TCanvas read FCanvas;
    property CBDI: TBitmap read FCBDIBmp;
    property CBUI: TBitmap read FCBUIBmp;
    property CBUHI: TBitmap read FCBUHIBmp;
    property ColorDlg: TChooseColorDialog read FColorDlg;
    property Owner: TCustomControl read FOwner;
    property TempDrawBitmap: TBitmap read FTempDrawBitmap;
    property ValueEdit: TEdit read FEdit;
    property ValueSpinEdit: TSpinEdit read FSpinEdit;
    property ValueFloatSpinEdit: TFloatSpinEdit read FFloatSpinEdit;
  public
    constructor Create(AOwner: TCustomControl; ACanvas: TCanvas; AGet: TGetParamVal; ASet: TSetParamVal; AWidth,AHeight: Integer; AAutoRepaint: Boolean = true);
    destructor Destroy; override;
    function Add(AParam: Pointer; const AParamName: string; AAType: TVisParamType; ALeft,ATop: Integer): TPPParam;
    procedure Clear;
    procedure EndMouseOver;
    procedure Paint(const AViewRect: TRect);
    function MouseMove(X,Y: Integer): Boolean;
    procedure MouseDown;
    procedure MouseUp;
    procedure MouseLeave;
    procedure SetWH(AWidth,AHeight: Integer);
    procedure SetViewRect(const ARect: TRect);
  published
    property AutoReget: Boolean read FAutoReget write FAutoReget;
    property AutoRepaint: Boolean read FAutoRepaint write FAutoRepaint;
    property BoolBoxUI: Integer read FBoolBoxUI write FBoolBoxUI;
    property BoolBoxUHI: Integer read FBoolBoxUHI write FBoolBoxUHI;
    property BoolBoxDI: Integer read FBoolBoxDI write FBoolBoxDI;
    property BoolBoxDHI: Integer read FBoolBoxDHI write FBoolBoxDHI;
    property CallBoxUI: Integer read FCallBoxUI write SetCallBoxUI;
    property CallBoxUHI: Integer read FCallBoxUHI write SetCallBoxUHI;
    property CallBoxDI: Integer read FCallBoxDI write SetCallBoxDI;
    property Count: Integer read GetCount;
    property GetProc: TGetParamVal read FGetProc write FGetProc;
    property Height: Integer read FHeight write SetHeight;
    property Images: TImageList read FImages write SetImages;
    property SetProc: TSetParamVal read FSetProc write FSetProc;
    property Width: Integer read FWidth write SetWidth;

    property OnPaint: TParamEvent read FOnPaint write FOnPaint;
    property OnRequestBackground: TParamFunc read FOnRequestBackground write FOnRequestBackground;
  end;

  TPPParamClass = class of TPPParam;

  TPPPCall      = class (TPPParam)
  protected
    procedure DoPaint; override;
    procedure DoMouseDown; override;
  end;

  TPPPBool      = class (TPPParam)
  private
    FDown: vpBool;
  protected
    procedure DoPaint; override;
    procedure DoMouseDown; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer); override;
  end;

  TPPPColor     = class (TPPParam)
  private
    FSelColor: vpColor;
  protected
    procedure DoPaint; override;
    procedure DoMouseDown; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer); override;
  end;

  TPPPString    = class (TPPParam)
  private
    FStr     : vpString;
    FShownStr: string;
    function GetStrWidth(const S: string): Integer;
    procedure SetShownStr;
  protected
    procedure DoPaint; override;
    procedure DoMouseMove(X,Y: Integer); override;
    procedure DoMouseLeave; override;
    procedure EditChanged; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer); override;
  end;

  TPPPInt       = class (TPPParam)
  private
    FInt: vpInt;
  protected
    procedure DoPaint; override;
    procedure DoMouseMove(X,Y: Integer); override;
    procedure DoMouseLeave; override;
    procedure EditChanged; override;
  public
    constructor Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer); override;
  end;

  TPPPReal      = class (TPPParam)
  private
    FReal: vpReal;
  protected
    procedure DoPaint; override;
    procedure DoMouseMove(X,Y: Integer); override;
    procedure DoMouseLeave; override;
    procedure EditChanged; override;
  public
    constructor Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer); override;
  end;

implementation

{TParamPainter}

constructor TParamPainter.Create(AOwner: TCustomControl; ACanvas: TCanvas; AGet: TGetParamVal; ASet: TSetParamVal; AWidth,AHeight: Integer; AAutoRepaint: Boolean = true);
begin
  inherited Create;
  FItems:=TObjectList.Create(true);
  FAutoRepaint:=AAutoRepaint;
  FAutoReget:=false;
  FGetProc:=AGet;
  FSetProc:=ASet;
  FMouseOverItem:=nil;
  FOwner:=AOwner;
  FCanvas:=ACanvas;
  FTempDrawBitmap:=TBitmap.Create;
  FCBUIBmp:=TBitmap.Create;
  FCBUHIBmp:=TBitmap.Create;
  FCBDIBmp:=TBitmap.Create;
  FBoolBoxUI:=-1;
  FBoolBoxUHI:=-1;
  FBoolBoxDI:=-1;
  FBoolBoxDHI:=-1;
  FCallBoxUI:=-1;
  FCallBoxUHI:=-1;
  FCallBoxDI:=-1;
  SetWH(AWidth,AHeight);

  FEdit:=TEdit.Create(FOwner);
  FEdit.MaxLength:=255;
  AddEdit(FEdit);

  FSpinEdit:=TSpinEdit.Create(FOwner);
  FSpinEdit.MaxValue:=MaxInt;
  FSpinEdit.MinValue:=-MaxInt;
  AddEdit(FSpinEdit);

  FFloatSpinEdit:=TFloatSpinEdit.Create(FOwner);
  FFloatSpinEdit.MaxValue:=Infinity;
  FFloatSpinEdit.MinValue:=NegInfinity;
  FFloatSpinEdit.DecimalPlaces:=4;
  AddEdit(FFloatSpinEdit);

  FColorDlg:=TChooseColorDialog.Create(AOwner);
  FColorDlg.AutoHide:=true;

  FEditingItem:=nil;
  FLastEditingItem:=nil;
  FAEdit:=nil;
end;

destructor TParamPainter.Destroy;
begin
  FColorDlg.Destroy;
  FOwner.RemoveControl(FFloatSpinEdit);
  FFloatSpinEdit.Destroy;
  FOwner.RemoveControl(FSpinEdit);
  FSpinEdit.Destroy;
  FOwner.RemoveControl(FEdit);
  FEdit.Destroy;

  FCBUIBmp.Destroy;
  FCBUHIBmp.Destroy;
  FCBDIBmp.Destroy;
  FItems.Destroy;
  FTempDrawBitmap.Destroy;
  inherited Destroy;
end;

procedure TParamPainter.AddEdit(AEdit: TCustomEdit); inline;
begin
  AEdit.Visible:=false;
  AEdit.AutoSize:=false;
  AEdit.OnEnter:=@FEditEnter;
  AEdit.OnKeyDown:=@FEditKeyDown;
  FOwner.InsertControl(AEdit);
end;

procedure TParamPainter.LoadImage(Index: Integer);
begin
  if FImages<>nil then FImages.GetBitmap(Index,FTempDrawBitmap);
end;

const
  ParamClasses: array [oCall..oBuffer] of TPPParamClass = (TPPPCall,TPPPInt,TPPPReal,TPPPString,TPPPColor,TPPPBool,TPPParam);

function TParamPainter.Add(AParam: Pointer; const AParamName: string; AAType: TVisParamType; ALeft,ATop: Integer): TPPParam;
begin
  Result:=ParamClasses[AAType].Create(Self,AParam,AParamName,AAType,ALeft,ATop);
  FItems.Add(Result);
end;

procedure TParamPainter.Clear;
begin
  EndMouseOver;
  FItems.Clear;
end;

procedure TParamPainter.EndMouseOver;
begin
  FMouseOverItem:=nil;
  EndEdit;
end;

procedure TParamPainter.Paint(const AViewRect: TRect);
var
  AItem: TObjectListItem;
begin
  FViewRect:=AViewRect;
  if FItems.Empty then exit;
  AItem:=FItems.Last;
  while AItem<>nil do begin
    TPPParam(AItem.Content).Paint(AViewRect);
    AItem:=AItem.Prev;
  end;
end;

procedure TParamPainter.StartEdit(AEdit: TCustomEdit; AItem: TPPParam);
begin
  FAEdit:=AEdit;
  FAEdit.Left:=AItem.DrawPos.Left;
  FAEdit.Top:=AItem.DrawPos.Top;
  FAEdit.Width:=AItem.DrawPos.Right-AItem.DrawPos.Left;
  FAEdit.Height:=AItem.DrawPos.Bottom-AItem.DrawPos.Top;
  FAEdit.OnChange:=@FEditChange;
  FAEdit.OnExit:=@FEditExit;
  FAEdit.Visible:=true;
  FLastEditingItem:=AItem;
end;

procedure TParamPainter.EndEdit;
begin
  if FAEdit<>nil then begin
    FAEdit.OnExit:=nil;
    FAEdit.OnChange:=nil;
    FAEdit.Visible:=false;
    FEditingItem:=nil;
    FAEdit:=nil;
    FLastEditingItem:=nil;
  end;
end;

function TParamPainter.Editing: Boolean;
begin
  Result:=(FEditingItem<>nil);
end;

procedure TParamPainter.FEditChange(Sender: TObject);
begin
  FEditingItem.EditChanged;
end;

procedure TParamPainter.FEditEnter(Sender: TObject);
begin
  FEditingItem:=FMouseOverItem;
end;

procedure TParamPainter.FEditExit(Sender: TObject);
begin
  EndEdit;
end;

procedure TParamPainter.FEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=13) and (FMouseOverItem<>FEditingItem) then EndEdit;
end;

function TParamPainter.MouseMove(X,Y: Integer): Boolean;
var
  AItem : TObjectListItem;
  AMItem: TPPParam;
begin
  if FItems.Empty then exit;
  AItem:=FItems.Last;
  while AItem<>nil do begin
    AMItem:=TPPParam(AItem.Content);
    if InRect(Classes.Point(X,Y),AMItem.FClickPos) then begin
      if (FMouseOverItem<>AMItem) and (FMouseOverItem<>nil) then FMouseOverItem.MouseLeave;
      FMouseOverItem:=AMItem;
      FMouseOverItem.MouseMove(X-FMouseOverItem.FPos.Left,Y-FMouseOverItem.FPos.Top);
      Result:=true;
      exit;
    end;
    AItem:=AItem.Prev;
  end;
  if FMouseOverItem<>nil then FMouseOverItem.MouseLeave;
  FMouseOverItem:=nil;
  Result:=false;
end;

procedure TParamPainter.MouseDown;
begin
  if FMouseOverItem<>nil then FMouseOverItem.MouseDown;
end;

procedure TParamPainter.MouseUp;
begin
  if FMouseOverItem<>nil then FMouseOverItem.MouseUp;
end;

procedure TParamPainter.MouseLeave;
begin
  {if FMouseOverItem<>nil then begin
    FMouseOverItem.MouseLeave;
    //FMouseOverItem:=nil;
  end;}
end;

procedure TParamPainter.SetImages(Value: TImageList);
begin
  FImages:=Value;
  if Value<>nil
    then FTempDrawBitmap.SetSize(Value.Width,Value.Height)
    else FTempDrawBitmap.SetSize(10,10);
end;

procedure TParamPainter.SetHeight(Value: Integer);
var
  AItem: TObjectListItem;
begin
  if FHeight=Value then exit;
  FHeight:=Value;
  if FItems.Empty then exit;
  AItem:=FItems.First;
  while AItem<>nil do begin
    with TPPParam(AItem.Content) do begin
      with FPos do Bottom:=Top+Value;
      Resize;
    end;
    AItem:=AItem.Next;
  end;
  ResizeAllBtns;
end;

procedure TParamPainter.SetWidth(Value: Integer);
var
  AItem: TObjectListItem;
begin
  if FWidth=Value then exit;
  FWidth:=Value;
  if FItems.Empty then exit;
  AItem:=FItems.First;
  while AItem<>nil do begin
    with TPPParam(AItem.Content) do begin
      with FPos do Right:=Left+Value;
      Resize;
    end;
    AItem:=AItem.Next;
  end;
  ResizeAllBtns;
end;

function TParamPainter.RequestBackground(Param: TPPParam): Boolean;
begin
  if FAutoRepaint then begin
    if Assigned(FOnRequestBackground)
      then Result:=FOnRequestBackground(Self,Param)
      else Result:=true;
  end else Result:=true;
end;

procedure TParamPainter.Painted(Param: TPPParam);
begin
  if Assigned(FOnPaint) then FOnPaint(Self,Param);
end;

procedure TParamPainter.PreDrawButton_(ImgIndex: Integer; Btn,t1,t2,t3: TBitmap);
begin
  if ImgIndex>=0 then begin
    LoadImage(ImgIndex);
    SplitImageH(FTempDrawBitmap,t1,t2,t3);
    SIBtnImgH(Btn,t1,t2,t3,FWidth);
  end else with Btn do begin
    SetSize(FWidth,FHeight);
    with Canvas do begin
      Brush.Color:=clBtnFace;
      Pen.Color:=clBtnText;
      Rectangle(0,0,Width-1,Height-1);
    end;
  end;
end;

procedure TParamPainter.PreDrawButton(ImgIndex: Integer; Btn: TBitmap);
var
  t1,t2,t3: TBitmap;
begin
  t1:=TBitmap.Create;
  t2:=TBitmap.Create;
  t3:=TBitmap.Create;
  PreDrawButton_(ImgIndex,Btn,t1,t2,t3);
  t1.Destroy;
  t2.Destroy;
  t3.Destroy;
end;

procedure TParamPainter.ResizeAllBtns;
var
  t1,t2,t3: TBitmap;
begin
  t1:=TBitmap.Create;
  t2:=TBitmap.Create;
  t3:=TBitmap.Create;
  PreDrawButton_(FCallBoxUHI,FCBUHIBmp,t1,t2,t3);
  PreDrawButton_(FCallBoxUI,FCBUIBmp,t1,t2,t3);
  PreDrawButton_(FCallBoxDI,FCBDIBmp,t1,t2,t3);
  t1.Destroy;
  t2.Destroy;
  t3.Destroy;
end;

procedure TParamPainter.SetCallBoxUI(Value: Integer);
begin
  FCallBoxUI:=Value;
  PreDrawButton(Value,FCBUIBmp);
end;

procedure TParamPainter.SetCallBoxUHI(Value: Integer);
begin
  FCallBoxUHI:=Value;
  PreDrawButton(Value,FCBUHIBmp);
end;

procedure TParamPainter.SetCallBoxDI(Value: Integer);
begin
  FCallBoxDI:=Value;
  PreDrawButton(Value,FCBDIBmp);
end;

function TParamPainter.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

procedure TParamPainter.SetWH(AWidth,AHeight: Integer);
var
  AItem: TObjectListItem;
begin
  if (FWidth=AWidth) and (FHeight=AHeight) then exit;
  FWidth:=AWidth;
  FHeight:=AHeight;
  if FItems.Empty then exit;
  AItem:=FItems.First;
  while AItem<>nil do begin
    with TPPParam(AItem.Content) do begin
      with FPos do begin
        Right:=Left+AWidth;
        Bottom:=Top+AHeight;
      end;
      Resize;
    end;
    AItem:=AItem.Next;
  end;
  ResizeAllBtns;
end;

procedure TParamPainter.SetViewRect(const ARect: TRect);
begin
  FViewRect:=ARect;
end;

{TPPParam}

constructor TPPParam.Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer);
begin
  inherited Create;
  FOwner:=AOwner;
  FParam:=AParam;
  FParamName:=AParamName;
  FState:=ppDefault;
  FType:=AType;
  with FPos do begin
    Left:=ALeft;
    Top:=ATop;
    Right:=ALeft+AOwner.Width;
    Bottom:=ATop+AOwner.Height;
  end;
  FVisible:=true;
  Resize;
end;

procedure TPPParam.SetVal(var Value);
begin
  FOwner.SetProc(FParam,FType,Value);
  if FOwner.FAutoReget then GetVal(Value);
  if FOwner.AutoRepaint then Paint(FOwner.FViewRect);
end;

procedure TPPParam.GetVal(out Value);
begin
  FOwner.GetProc(FParam,FType,Value);
end;

procedure TPPParam.DoPaint;
begin
  //do nothing
end;

procedure TPPParam.DoMouseMove(X,Y: Integer);
begin
  //do nothing
end;

procedure TPPParam.DoMouseDown;
begin
  //do nothing
end;

procedure TPPParam.DoMouseUp;
begin
  //do nothing
end;

procedure TPPParam.DoMouseLeave;
begin
  //do nothing
end;

procedure TPPParam.Paint(const AViewRect: TRect);
begin
  if not FVisible then exit;
  if not InView(FPos,AViewRect) then exit;
  FDrawPos:=ToViewRect(FPos,AViewRect);
  DoPaint;
  FOwner.Painted(Self);
end;

function TPPParam.IsEditing: Boolean;
begin
  Result:=(FOwner.FEditingItem=Self);
end;

function TPPParam.IsMouseOver: Boolean;
begin
  Result:=(FOwner.FMouseOverItem=Self);
end;

function TPPParam.WasEditing: Boolean;
begin
  Result:=(FOwner.FLastEditingItem=Self);
end;

procedure TPPParam.MouseMove(X,Y: Integer);
begin
  if not FVisible then exit;
  if FState=ppDefault then begin
    FState:=ppMouseOver;
    if FOwner.AutoRepaint then Paint(FOwner.FViewRect);
  end;
  DoMouseMove(X,Y);
end;

procedure TPPParam.MouseDown;
begin
  if not FVisible then exit;
  FState:=ppDown;
  if FOwner.AutoRepaint then Paint(FOwner.FViewRect);
  DoMouseDown;
end;

procedure TPPParam.MouseUp;
begin
  if not FVisible then exit;
  FState:=ppMouseOver;
  if FOwner.AutoRepaint then Paint(FOwner.FViewRect);
  DoMouseUp;
end;

procedure TPPParam.MouseLeave;
begin
  if not FVisible then exit;
  FState:=ppDefault;
  if FOwner.AutoRepaint then Paint(FOwner.FViewRect);
  DoMouseLeave;
end;

procedure TPPParam.Resize;
begin
  FClickPos:=FPos;
  if FOwner.AutoRepaint then Paint(FOwner.FViewRect);
end;

function TPPParam.GetHeight: Integer;
begin
  Result:=FOwner.FHeight;
end;

function TPPParam.GetLeft: Integer;
begin
  Result:=FPos.Left;
end;

procedure TPPParam.SetLeft(Value: Integer);
begin
  FPos.Right:=(Value+(FPos.Right-FPos.Left));
  FPos.Left:=Value;
  Resize;
  if FOwner.FAutoRepaint then Paint(FOwner.FViewRect);
end;

function TPPParam.GetTop: Integer;
begin
  Result:=FPos.Top;
end;

procedure TPPParam.SetTop(Value: Integer);
begin
  FPos.Bottom:=(Value+(FPos.Bottom-FPos.Top));
  FPos.Top:=Value;
  Resize;
  if FOwner.FAutoRepaint then Paint(FOwner.FViewRect);
end;

function TPPParam.GetWidth: Integer;
begin
  Result:=FOwner.FWidth;
end;

function TPPParam.GetCanvas: TCanvas;
begin
  Result:=FOwner.FCanvas;
end;

procedure TPPParam.SetWH(ALeft,ATop: Integer);
begin
  with FPos do begin
    Left:=ALeft;
    Top:=ATop;
    Right:=ALeft+FOwner.FWidth;
    Bottom:=ATop+FOwner.FHeight;
  end;
  Resize;
end;

{TPPPCall}

procedure TPPPCall.DoPaint;
var
  ABmp: TBitmap;
begin
  inherited DoPaint;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;
  case State of
    ppDefault  : ABmp:=FOwner.CBUI;//Canvas.Brush.Color:=clBtnFace;
    ppMouseOver: ABmp:=FOwner.CBUHI;//Canvas.Brush.Color:=cl3DLight;
    ppDown     : ABmp:=FOwner.CBDI;//Canvas.Brush.Color:=clBtnShadow;
  end;
  //Canvas.Rectangle(DrawPos);
  Canvas.Draw(DrawPos.Left,DrawPos.Top+((Height-ABmp.Height) div 2),ABmp);
end;

procedure TPPPCall.DoMouseDown;
begin
  inherited DoMouseDown;
  SetVal(vpDefault);
end;

{TPPPBool}

constructor TPPPBool.Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer);
begin
  inherited Create(AOwner,AParam,AParamName,AType,ALeft,ATop);
  GetVal(FDown);
end;

procedure TPPPBool.Resize;
begin
  with ClickPos do begin
    Left:=Position.Left;
    Top:=Position.Top;
    Right:=Position.Left+FOwner.TempDrawBitmap.Width;
    Bottom:=Position.Bottom;
  end;
end;

procedure TPPPBool.DoPaint;
begin
  inherited DoPaint;
  if FDown then begin
    if State=ppDefault
      then FOwner.LoadImage(FOwner.BoolBoxDI)
      else FOwner.LoadImage(FOwner.BoolBoxDHI);
  end else begin
    if State=ppDefault
      then FOwner.LoadImage(FOwner.BoolBoxUI)
      else FOwner.LoadImage(FOwner.BoolBoxUHI);
  end;
  Canvas.Draw(DrawPos.Left,DrawPos.Top+((DrawPos.Bottom-DrawPos.Top-FOwner.TempDrawBitmap.Height) div 2),FOwner.TempDrawBitmap)
end;

procedure TPPPBool.DoMouseDown;
begin
  inherited DoMouseDown;
  FDown:=not FDown;
  SetVal(FDown);
end;

{TPPPColor}

constructor TPPPColor.Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer);
begin
  inherited Create(AOwner,AParam,AParamName,AType,ALeft,ATop);
  GetVal(FSelColor);
end;

procedure TPPPColor.Resize;
begin
  with ClickPos do begin
    Left:=Position.Left;
    Top:=Position.Top;
    Right:=Position.Left+(Position.Bottom-Position.Top)*3;
    Bottom:=Position.Bottom;
  end;
end;

procedure TPPPColor.DoPaint;
var
  ARect: TRect;
begin
  inherited DoPaint;
  Canvas.Pen.Width:=1;
  case State of
    ppDefault  : Canvas.Pen.Color:=$777777;
    ppMouseOver: Canvas.Pen.Color:=$555555;
    ppDown     : Canvas.Pen.Color:=$222222;
  end;
  ARect:=Classes.Rect(DrawPos.Left,DrawPos.Top,DrawPos.Left+((DrawPos.Bottom-DrawPos.Top)*3),DrawPos.Bottom);
  Canvas.Rectangle(ARect.Left,ARect.Top,ARect.Right-1,ARect.Bottom-1);
  Canvas.Rectangle(ARect.Left+1,ARect.Top+1,ARect.Right-2,ARect.Bottom-2);
  ARect:=Classes.Rect(ARect.Left+2,ARect.Top+2,ARect.Right-3,ARect.Bottom-3);
  DrawAlphaColorArea(Canvas,ARect,FSelColor,clWhite,clBlack,8,8);
end;

procedure TPPPColor.DoMouseDown;
var
  ASelColor: TColor32;
begin
  inherited DoMouseDown;
  with FOwner.FColorDlg do begin
    Color32:=FSelColor;
    if Execute then begin
      ASelColor:=Color32;
      FSelColor:=ASelColor;
      SetVal(ASelColor);
    end;
  end;
end;

{TPPPString}

constructor TPPPString.Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer);
begin
  inherited Create(AOwner,AParam,AParamName,AType,ALeft,ATop);
  GetVal(FStr);
  SetShownStr;
end;

procedure TPPPString.Resize;
begin
  inherited Resize;
  SetShownStr;
end;

procedure TPPPString.SetShownStr;
begin
  Canvas.Font.Style:=[];
  FShownStr:=StrShorten(FStr,@GetStrWidth,Position.Right-Position.Left);
end;

function TPPPString.GetStrWidth(const S: string): Integer;
begin
  Result:=Canvas.TextWidth(S);
end;

procedure TPPPString.DoPaint;
var
  AY: Integer;
begin
  if not FOwner.RequestBackground(Self) then exit;
  inherited DoPaint;
  Canvas.Brush.Style:=bsClear;
  AY:=((DrawPos.Bottom-DrawPos.Top)-Canvas.TextHeight('Wg')) div 2;
  Canvas.TextOut(DrawPos.Left,DrawPos.Top+AY,FShownStr);
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPString.DoMouseMove(X,Y: Integer);
begin
  inherited DoMouseMove(X,Y);
  with Owner do if not Editing then begin
    ValueEdit.Text:=FStr;
    StartEdit(ValueEdit,Self);
  end;
end;

procedure TPPPString.DoMouseLeave;
begin
  inherited DoMouseLeave;
  if (not IsEditing) and WasEditing then FOwner.EndEdit;
end;

procedure TPPPString.EditChanged;
begin
  FStr:=Owner.ValueEdit.Text;
  SetShownStr;
  SetVal(FStr);
end;

{TPPPInt}

constructor TPPPInt.Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer);
begin
  inherited Create(AOwner,AParam,AParamName,AType,ALeft,ATop);
  GetVal(FInt);
end;

procedure TPPPInt.DoPaint;
var
  AY: Integer;
begin
  if not FOwner.RequestBackground(Self) then exit;
  inherited DoPaint;
  Canvas.Brush.Style:=bsClear;
  AY:=((DrawPos.Bottom-DrawPos.Top)-Canvas.TextHeight('Wg')) div 2;
  Canvas.TextOut(DrawPos.Left,DrawPos.Top+AY,IntToStr(FInt));
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPInt.DoMouseMove(X,Y: Integer);
begin
  inherited DoMouseMove(X,Y);
  with Owner do if not Editing then begin
    ValueSpinEdit.Value:=FInt;
    StartEdit(ValueSpinEdit,Self);
  end;
end;

procedure TPPPInt.DoMouseLeave;
begin
  inherited DoMouseLeave;
  if (not IsEditing) and WasEditing then FOwner.EndEdit;
end;

procedure TPPPInt.EditChanged;
begin
  FInt:=Owner.ValueSpinEdit.Value;
  SetVal(FInt);
end;

{TPPPReal}

constructor TPPPReal.Create(AOwner: TParamPainter; AParam: Pointer; const AParamName: string; AType: TVisParamType; ALeft,ATop: Integer);
begin
  inherited Create(AOwner,AParam,AParamName,AType,ALeft,ATop);
  GetVal(FReal);
end;

procedure TPPPReal.DoPaint;
var
  AY: Integer;
begin
  if not FOwner.RequestBackground(Self) then exit;
  inherited DoPaint;
  Canvas.Brush.Style:=bsClear;
  AY:=((DrawPos.Bottom-DrawPos.Top)-Canvas.TextHeight('Wg')) div 2;
  Canvas.TextOut(DrawPos.Left,DrawPos.Top+AY,FloatToStrF(FReal,ffFixed,7,4));
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPReal.DoMouseMove(X,Y: Integer);
begin
  inherited DoMouseMove(X,Y);
  with Owner do if not Editing then begin
    ValueFloatSpinEdit.Value:=FReal;
    StartEdit(ValueFloatSpinEdit,Self);
  end;
end;

procedure TPPPReal.DoMouseLeave;
begin
  inherited DoMouseLeave;
  if (not IsEditing) and WasEditing then FOwner.EndEdit;
end;

procedure TPPPReal.EditChanged;
begin
  FReal:=Owner.ValueFloatSpinEdit.Value;
  SetVal(FReal);
end;

end.

