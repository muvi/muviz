unit StdParamEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamPainter2, Graphics, SlaveControl, VisType2, AdvCoord,
  StdParamTypes, Controls, ControlPlacer, GraphX32, AdvFunc, Dialogs, MStrings,
  CanvasGraphX32, PresetUtil3_ConnectionRouter, PresetUtil3_Connections,
  PresetUtil3_Path, SlaveRotaryFader;

type
  TPPPCall        = class (TPPParam)
  private
    FButtonImg: TBitmap;
  strict protected
    function GetMinWidth: Integer; override;
  protected
    procedure DoPaint; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoUpdate; override;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
    destructor Destroy; override;
  end;

  TPPPBoolean     = class (TPPUParam)
  private
    FDown: TVBoolean;
  strict protected
    function GetMinWidth: Integer; override;
  protected
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
    procedure DoPaint; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoUpdate; override;
  end;

  TPPPColor       = class (TPPUParam)
  private
    FSelColor: TVColor;
    procedure ColorDialogExecuted(Sender: TObject; Success: Boolean);
  strict protected
    function GetMinWidth: Integer; override;
  protected
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
    procedure DoPaint; override;
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoUpdate; override;
  end;

  TPPPBaseString  = class (TPPUParam)
  private
    FStr     : string;
    FShownStr: string;
    function GetStrWidth(const S: string): Integer;
    procedure SetShownStr(AWidth: Integer);
  strict protected
    function GetMinWidth: Integer; override;
  protected
    procedure DoPaint; override;
    procedure DoSetState(ANewState: TPlacingControlState); override;
    procedure UsingStarted; override;
    procedure ControlChanged; override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure DoUpdate; override;

    procedure DoSetVal(Value: string); virtual; abstract;
    function DoGetVal: string; virtual; abstract;
    procedure DoInitEdit; virtual; abstract;
  end;

  TPPPString      = class (TPPPBaseString)
  protected
    procedure DoSetVal(Value: string); override;
    function DoGetVal: string; override;
    procedure DoInitEdit; override;
  end;

  TPPPRotaryParam = class (TPPParam)
  strict private
    FRotaryFader: TSlaveRotaryFader;
    function GetRotaryValue: Real;
    procedure SetRotaryValue(AValue: Real);
  strict protected
    function GetMinWidth: Integer; override;
    procedure RotaryFaderFade(Sender: TSLaveControl); virtual; abstract;
    procedure DoAlign; override;
    function CheckUseRotaryFader(out AMin, AMax: Real): Boolean; virtual; abstract;
    //sets the rotary value without calling RotaryFaderFade
    property RotaryValue: Real read GetRotaryValue write SetRotaryValue;
  protected
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
  end;

  TPPPInteger     = class (TPPPRotaryParam)
  private
    FMin: TVInteger;
    FMax: TVInteger;
    FInt: TVInteger;
    procedure SetValue(ANewVal: TVInteger);
    function CheckConstraint(AConstraint: TVInteger): Boolean; inline;
  strict protected
    function GetMinWidth: Integer; override;
    function CheckUseRotaryFader(out AMin, AMax: Real): Boolean; override;
    procedure RotaryFaderFade(Sender: TSLaveControl); override;
  protected
    procedure DoPaint; override;
    procedure DoSetState(ANewState: TPlacingControlState); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure UsingStarted; override;
    procedure ControlChanged; override;
    procedure DoUpdate; override;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
  end;

  TPPPFloat       = class (TPPUParam)
  private
    FMin  : TVFloat;
    FMax  : TVFloat;
    FFloat: TVFloat;
  strict protected
    function GetMinWidth: Integer; override;
  protected
    procedure DoPaint; override;
    procedure DoSetState(ANewState: TPlacingControlState); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure UsingStarted; override;
    procedure ControlChanged; override;
    procedure DoUpdate; override;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
  end;

procedure Register;

implementation

{%REGION TPPPCall}

constructor TPPPCall.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
  FButtonImg:=TBitmap.Create;
end;

destructor TPPPCall.Destroy;
begin
  FButtonImg.Destroy;;
  inherited Destroy;
end;

function TPPPCall.GetMinWidth: Integer;
begin
  Result:=30;
end;

procedure TPPPCall.DoPaint;
begin
  inherited DoPaint;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;
  Painter.Images.LoadImage(State, ClickRect.Right-ClickRect.Left, FButtonImg);
  Canvas.Draw(AbsoluteRect.Left + ClickRect.Left, AbsoluteRect.Top + ClickRect.Top, FButtonImg);
end;

procedure TPPPCall.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  IPCall(Param).&Set;
end;

procedure TPPPCall.DoUpdate;
begin
  //do nothing
end;

{%ENDREGION}
{%REGION TPPPBoolean}

constructor TPPPBoolean.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
  ClickRect:=Rect(0, 0, Painter.Images.OutputBmp.Width, Painter.Images.OutputBmp.Height);
end;

function TPPPBoolean.GetMinWidth: Integer;
begin
  Result:=Painter.Images.OutputBmp.Width;
end;

procedure TPPPBoolean.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Height:=DEFAULTPARAMHEIGHT;
  //do nothing, do not set the ClickRect, too
end;

procedure TPPPBoolean.DoPaint;
begin
  inherited DoPaint;
  with Painter.Images do begin
    if FDown then begin
      if State=ppDefault
        then LoadImage(BoolBoxDI)
        else LoadImage(BoolBoxDHI);
    end else begin
      if State=ppDefault
        then LoadImage(BoolBoxUI)
        else LoadImage(BoolBoxUHI);
    end;
    Canvas.Draw(AbsoluteRect.Left, AbsoluteRect.Top+((Height-OutputBmp.Height) div 2), OutputBmp);
  end;
end;

procedure TPPPBoolean.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if State=ppDown then begin
    FDown:=not FDown;
    IPBoolean(Param).&Set(FDown);
  end;
end;

procedure TPPPBoolean.DoUpdate;
begin
  FDown:=IPBoolean(Param).Get;
  Repaint;
end;

{%ENDREGION}
{%REGION TPPPColor}

constructor TPPPColor.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
  ClickRect:=Rect(0, 0, DEFAULTPARAMHEIGHT*5, DEFAULTPARAMHEIGHT-1);
end;

function TPPPColor.GetMinWidth: Integer;
begin
  Result:=DEFAULTPARAMHEIGHT*5;
end;

procedure TPPPColor.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Height:=DEFAULTPARAMHEIGHT;
end;

procedure TPPPColor.DoPaint;
var
  ARect: TRect;
begin
  inherited DoPaint;
  case State of
    ppDefault  : Canvas.Pen.Color:=$777777;
    ppMouseOver: Canvas.Pen.Color:=$555555;
    ppDown     : Canvas.Pen.Color:=$222222;
  end;
  Canvas.Pen.JoinStyle:=pjsMiter;
  Canvas.Pen.Width:=1;
  ARect:=ClickRect+AbsoluteRect.TopLeft;
  DrawRect(Canvas, ARect);
  DrawRect(Canvas, ARect.Left+1, ARect.Top+1, ARect.Right-1, ARect.Bottom-1);
  //only -1 because the canvas rectangle method does not include the bottom/right pixels
  ARect:=Classes.Rect(ARect.Left+2, ARect.Top+2, ARect.Right-1, ARect.Bottom-1);
  DrawAlphaColorArea(Canvas,ARect,FSelColor,clWhite,clBlack,8,8);
end;

procedure TPPPColor.ColorDialogExecuted(Sender: TObject; Success: Boolean);
var
  ASelColor: TColor32;
begin
  if Success then with Painter.ColorDlg do begin
    ASelColor:=Color32;
    FSelColor:=ASelColor;
    IPColor(Param).&Set(ASelColor);
  end;
end;

procedure TPPPColor.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if State=ppDown then with Painter.ColorDlg do begin
    Color32:=FSelColor;
    OnExecuted:=@ColorDialogExecuted;
    ExecuteNonBlocking;
  end;
end;

procedure TPPPColor.DoUpdate;
begin
  FSelColor:=IPColor(Param).Get;
  Repaint;
end;

{%ENDREGION}
{%REGION TPPPBaseString}

function TPPPBaseString.GetMinWidth: Integer;
begin
  Result:=100;
end;

procedure TPPPBaseString.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Height:=Painter.ValueEdit.Height;
  ClickRect:=Rect(0, 0, ANewBounds.Width, ANewBounds.Height);
  SetShownStr(ANewBounds.Width);
end;

procedure TPPPBaseString.SetShownStr(AWidth: Integer);
begin
  Canvas.Font.Style:=[];
  FShownStr:=StrShorten(FStr, @GetStrWidth, AWidth);
end;

function TPPPBaseString.GetStrWidth(const S: string): Integer;
begin
  Result:=Canvas.TextWidth(S);
end;

procedure TPPPBaseString.DoPaint;
var
  AY: Integer;
begin
  inherited DoPaint;
  FillBG;
  AY:=(Height-Canvas.TextHeight('Wg')) div 2;
  Canvas.TextOut(AbsoluteRect.Left, AbsoluteRect.Top+AY, FShownStr);
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPBaseString.DoSetState(ANewState: TPlacingControlState);
begin
  inherited DoSetState(ANewState);
  with Painter do begin
    case ANewState of
      ppMouseOver: StartUse(ValueEdit, Self);
      ppDown     : StartUse(ValueEdit, Self, true);
    end;
  end;
end;

procedure TPPPBaseString.UsingStarted;
begin
  DoInitEdit;
  Painter.ValueEdit.Text:=FStr;
end;

procedure TPPPBaseString.ControlChanged;
begin
  FStr:=Painter.ValueEdit.Text;
  SetShownStr(Width);
  DoSetVal(FStr);
end;

procedure TPPPBaseString.DoUpdate;
begin
  FStr:=DoGetVal;
  SetShownStr(Width);
  Repaint;
end;

{%ENDREGION}
{%REGION TPPPString}

procedure TPPPString.DoSetVal(Value: string);
begin
  IPString(Param).&Set(Value);
end;

function TPPPString.DoGetVal: string;
begin
  Result:=IPString(Param).Get;
end;

procedure TPPPString.DoInitEdit;
begin
  Painter.ValueEdit.MaxLength:=0;
end;

{%ENDREGION}
{%REGION TPPPRotaryParam}

constructor TPPPRotaryParam.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
var
  AMin, AMax: Real;
begin
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
  if CheckUseRotaryFader(AMin, AMax) then begin
    FRotaryFader:=TSlaveRotaryFader.Create(Self, Painter.Config.Pictures.RotaryImages);
    with FRotaryFader do begin
      OnFade:=@RotaryFaderFade;
      Min:=AMin;
      Max:=AMax;
    end;
  end else FRotaryFader:=nil;
  DoUpdate;
end;

procedure TPPPRotaryParam.DoAlign;
begin
  inherited DoAlign;
  if FRotaryFader<>nil
    then FRotaryFader.SetBounds(0, 0, 0, 0);
end;

procedure TPPPRotaryParam.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  if FRotaryFader<>nil
    then ClickRect:=Rect(FRotaryFader.Width + 2, 0, ANewBounds.Width, ANewBounds.Height)
    else ClickRect:=Rect(0, 0, ANewBounds.Width, ANewBounds.Height);
end;

function TPPPRotaryParam.GetRotaryValue: Real;
begin
  //do not use the rotary fader to store something, just to update the param
  Assert(FRotaryFader<>nil);
  Result:=FRotaryFader.Value;
end;

procedure TPPPRotaryParam.SetRotaryValue(AValue: Real);
begin
  if FRotaryFader<>nil then begin
    FRotaryFader.OnFade:=nil;
    FRotaryFader.Value:=AValue;
    FRotaryFader.OnFade:=@RotaryFaderFade;
  end;
end;

function TPPPRotaryParam.GetMinWidth: Integer;
begin
  if FRotaryFader<>nil
    then Result:=FRotaryFader.Width
    else Result:=0;
end;

{%ENDREGION}
{%REGION TPPPInteger}

constructor TPPPInteger.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  Assert(AParam.ID.&Type = TPParamType(vInteger));
  with IPInteger(AParam) do begin
    FMin:=Min;
    FMax:=Max;
  end;
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
end;

function TPPPInteger.CheckUseRotaryFader(out AMin, AMax: Real): Boolean;
begin
  Result:=CheckConstraint(FMin) and CheckConstraint(FMax);
  AMin:=FMin;
  AMax:=FMax;
end;

function TPPPInteger.CheckConstraint(AConstraint: TVInteger): Boolean; inline;
begin
  Result:=(AConstraint > -100001) and (AConstraint < 100001);
end;

function TPPPInteger.GetMinWidth: Integer;
begin
  Result:=60 + inherited GetMinWidth;
end;

procedure TPPPInteger.DoPaint;
var
  AY: Integer;
begin
  inherited DoPaint;
  FillBG;
  AY:=((AbsoluteRect.Bottom-AbsoluteRect.Top)-Canvas.TextHeight('Wg')) div 2;
  Canvas.TextOut(AbsoluteRect.Left + ClickRect.Left, AbsoluteRect.Top+AY, IntToStr(FInt));
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPInteger.DoSetState(ANewState: TPlacingControlState);
begin
  inherited DoSetState(ANewState);
  with Painter do begin
    case ANewState of
      ppMouseOver: StartUse(ValueSpinEdit, Self);
      ppDown     : StartUse(ValueSpinEdit, Self, true);
    end;
  end;
end;

procedure TPPPInteger.UsingStarted;
begin
  with Painter.ValueSpinEdit do begin
    MinValue:=FMin;
    MaxValue:=FMax;
    Value:=FInt;
  end;
end;

procedure TPPPInteger.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Height:=Painter.ValueSpinEdit.Height;
  //ClickRect:=Rect(0, 0, ANewBounds.Width, ANewBounds.Height);
  inherited DoSetBounds(ANewBounds);
end;

procedure TPPPInteger.SetValue(ANewVal: TVInteger);
begin
  FInt:=ANewVal;
  IPInteger(Param).&Set(FInt);
end;

procedure TPPPInteger.RotaryFaderFade(Sender: TSLaveControl);
begin
  SetValue(Round(RotaryValue));
end;

procedure TPPPInteger.ControlChanged;
begin
  SetValue(Painter.ValueSpinEdit.Value);
end;

procedure TPPPInteger.DoUpdate;
begin
  FInt:=IPInteger(Param).Get;
  RotaryValue:=FInt;
  Repaint;
end;

{%ENDREGION}
{%REGION TPPPFloat}

constructor TPPPFloat.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  Assert(AParam.ID.&Type = TPParamType(vFloat));
  with IPFloat(AParam) do begin
    FMin:=Min;
    FMax:=Max;
  end;
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
end;

function TPPPFLoat.GetMinWidth: Integer;
begin
  Result:=80;
end;

procedure TPPPFloat.DoPaint;
var
  AY: Integer;
begin
  inherited DoPaint;
  FillBG;
  AY:=(Height-Canvas.TextHeight('Wg')) div 2;
  Canvas.TextOut(AbsoluteRect.Left, AbsoluteRect.Top+AY, FloatToStrF(FFloat,ffFixed,7,4));
  Canvas.Brush.Style:=bsSolid;
end;

procedure TPPPFloat.DoSetState(ANewState: TPlacingControlState);
begin
  inherited DoSetState(ANewState);
  with Painter do begin
    case ANewState of
      ppMouseOver: StartUse(ValueFloatSpinEdit, Self);
      ppDown     : StartUse(ValueFloatSpinEdit, Self, true);
    end;
  end;
end;

procedure TPPPFloat.UsingStarted;
begin
  with Painter.ValueFloatSpinEdit do begin
    MinValue:=FMin;
    MaxValue:=FMax;
    Value:=FFloat;
  end;
end;

procedure TPPPFloat.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  ANewBounds.Height:=Painter.ValueFloatSpinEdit.Height;
  ClickRect:=Rect(0, 0, ANewBounds.Width, ANewBounds.Height);
end;

procedure TPPPFloat.ControlChanged;
begin
  FFloat:=Painter.ValueFloatSpinEdit.Value;
  IPFloat(Param).&Set(FFloat);
end;

procedure TPPPFloat.DoUpdate;
begin
  FFloat:=IPFloat(Param).Get;
  Repaint;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  TParamPainter.AddTypeEdit(vCall, TPPPCall);
  TParamPainter.AddTypeEdit(vBoolean, TPPPBoolean);
  TParamPainter.AddTypeEdit(vColor, TPPPColor);
  //TParamPainter.AddTypeEdit(vShortString, TPPPShortString);
  TParamPainter.AddTypeEdit(vString, TPPPString);
  TParamPainter.AddTypeEdit(vInteger, TPPPInteger);
  TParamPainter.AddTypeEdit(vFloat, TPPPFloat);
end;

{%ENDREGION}

end.

