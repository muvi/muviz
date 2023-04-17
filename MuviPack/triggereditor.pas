unit TriggerEditor; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VisDrawType, VisTypeUnit, VisDrawUnit,
  AdvFunc;

type
  TTriggerSelInfo= record
    Layer,Param: Integer;
    Output     : Boolean;
  end;

  TTriggerEdit = class (TGraphicControl)
  private
    FLastLayer      : TTriggerInfo;
    FSelLayer       : TTriggerInfo;
    FPreset         : ^TPreset;
    FSelIndex       : Integer;
    FParamFontHeight: Integer;
    FLastMouseOver  : TTriggerSelInfo;
    FMouseClick     : TTriggerSelInfo;
    FSelection      : TTriggerSelInfo;
    FLastMouse      : TPoint;
    FBuffer         : TBitmap;

    procedure DrawTriggers;
    function LayerPos(const Trig: TTriggerInfo): TRect;
    function LayerAt(const X,Y: Integer): TTriggerInfo;
    procedure Connect(const TI1,TI2: TTriggerInfo);
    procedure CreateCon(var OutVis,InVis: TPresetVis; const OutIndex,InIndex,InLayer: Integer);
    procedure NextTrigInfo(const Layers: TPresetLayers; var TrigInfo: TTriggerInfo);
  protected
    function GetVisParamType(const Vis: TVisType; const Index: Integer): TVisParamType;
    function ParamIsInput(const ParamType: TVisParamType): Boolean; overload;
    function ParamIsInput(const Vis: TVisType; const Index: Integer): Boolean;

    procedure DrawTrigger(const Trigger: TPresetVis);
    function ElemPos(const Elem: TTriggerSelInfo): TRect;
    function ElemAt(const P: TPoint): TTriggerSelInfo;
  private
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure DblClick; override;
    procedure TripleClick; override;
    procedure QuadClick; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetPreset(var APreset: TPreset);
  published
    property Align;
    property Anchors;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

implementation

{TTriggerEdit}

const
  TrigSize   = 200;
  TrigXDist  = 10;
  PlugColors      : array [TVisParamType] of TColor = ($FF0000,$00FF00,$0000FF,$FFFF00,$00FFFF,$FF00FF{,$AA0000,$00AA00,$0000AA,$AAAA00,$00AAAA,$000000});
  //MovePlugColors  : array [TVisParamType] of TColor = ($FF3333,$33FF33,$3333FF,$FFFF33,$33FFFF,$FF33FF{,$AA3333,$33AA33,$3333AA,$AAAA33,$33AAAA,$333333});
  OutputColors    : array [TVisOutputTYpe] of TColor = ($999999,$FF0000,$00FF00,$0000FF,$FFFF00,$00FFFF,$FF00FF);
  //MoveOutputColors: array [TVisOutputTYpe] of TColor = ($EEEEEE,$FF3333,$33FF33,$3333FF,$FFFF33,$33FFFF,$FF33FF);

  InputVisTypes : set of TVisParamType = [vInteger,vReal,vString,vColor,vBoolean];
  //OutputVisTypes: set of TVisParamType = [{vIntegerOutput,vRealOutput,vStringOutput,vColorOutput,vBooleanOutput,vCall}];

constructor TTriggerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreset:=nil;
  FParamFontHeight:=FBuffer.Canvas.TextHeight('Wg');
  FBuffer:=TBitmap.Create;
end;

destructor TTriggerEdit.Destroy;
begin
  FPreset:=nil;
  FBuffer.Destroy;
  inherited Destroy;
end;

procedure TTriggerEdit.SetPreset(var APreset: TPreset);
begin
  //FPreset:=@APreset;
  FPreset:=@APreset;
  FSelLayer.Param:=-2;
  FLastMouseOver.Param:=-3;
  FMouseClick.Param:=-3;
  FSelection.Param:=-3;
  FSelIndex:=0;
  {with TriggerImage do begin
    Picture.Bitmap.Width:=Width;
    Picture.Bitmap.Height:=Height;
  end;}
  //ParamFontHeight:=TriggerImage.Picture.Bitmap.Canvas.TextHeight('Wg');
  DrawTriggers;
  //Result:=ShowModal;
end;

procedure TTriggerEdit.Paint;
begin
  inherited Paint;
  DrawTriggers;
end;

procedure TTriggerEdit.Click;
begin
  inherited Click;
end;

procedure TTriggerEdit.DblClick;
begin
  inherited DblClick;
end;

procedure TTriggerEdit.TripleClick;
begin
  inherited TripleClick;
end;

procedure TTriggerEdit.QuadClick;
begin
  inherited QuadClick;
end;

procedure TTriggerEdit.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  FLastMouse:=Point(X,Y);
  FMouseClick:=FLastMouseOver;
  if (FSelection.Param>=0) and FSelection.Output and ((not FLastMouseOver.Output) or (FLastMouseOver.Param<0)) then begin
    VisDrawUnit.SetVisOutput(FPreset^,FSelection.Layer,FSelection.Param,FLastMouseOver.Layer,FLastMouseOver.Param);
    DrawTriggers;
    FSelection.Param:=-2;
  end else FSelection:=FLastMouseOver;
  inherited MouseDown(Button,Shift,X,Y);
end;

procedure TTriggerEdit.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if FMouseClick.Param>=-2 then begin
    if FMouseClick.Layer>=0 then with FPreset^.Layers[FMouseClick.Layer] do begin
      DesignPos.X+=X-FLastMouse.X;
      DesignPos.Y+=Y-FLastMouse.Y;
    end else with FPreset^.Triggers[not FMouseClick.Layer] do begin
      DesignPos.X+=X-FLastMouse.X;
      DesignPos.Y+=Y-FLastMouse.Y;
    end;
    DrawTriggers;
  end else if FLastMouseOver.Param>=-1 then with FBuffer.Canvas do begin
    Pen.Mode:=pmNot;
    Rectangle(ElemPos(FLastMouseOver));
    Pen.Mode:=pmCopy;
  end;
  FLastMouseOver:=ElemAt(Point(X,Y));
  if FLastMouseOver.Param>=-1 then with FBuffer.Canvas do begin
    Pen.Mode:=pmNot;
    Rectangle(ElemPos(FLastMouseOver));
    Pen.Mode:=pmCopy;
  end;
  FLastMouse:=Point(X,Y);
  inherited MouseMove(Shift,X,Y);
end;

procedure TTriggerEdit.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  FMouseClick.Param:=-3;
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure TTriggerEdit.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TTriggerEdit.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TTriggerEdit.Resize;
begin
  if FPreset<>nil then begin
    FBuffer.SetSize(Width,Height);
    DrawTriggers;
  end;
  inherited Resize;
end;

{function TTriggerEdit.EditTrigger(var Preset: TPreset): TModalResult;
begin
  FPreset:=@Preset;
  SelLayer.Param:=-2;
  LastMouseOver.Param:=-3;
  MouseClick.Param:=-3;
  Selection.Param:=-3;
  ASelIndex:=0;
  with TriggerImage do begin
    Picture.Bitmap.Width:=Width;
    Picture.Bitmap.Height:=Height;
  end;
  ParamFontHeight:=TriggerImage.Picture.Bitmap.Canvas.TextHeight('Wg');
  DrawTriggers;
  Result:=ShowModal;
end;}

const
  ParamWidth     = 200;
  OutputWidth    = 200;
  MachineWidth   = ParamWidth+OutputWidth;

procedure TTriggerEdit.DrawTrigger(const Trigger: TPresetVis);
var
  I,ParamCount,OutputCount,AH: Integer;
  ARect                      : TRect;
begin
  with Visualisations(Trigger.VisID) do begin
    ParamCount:=Length(VisParamDesc);
    OutputCount:=Length(OutputDesc);
    if ParamCount>=OutputCount
      then AH:=(ParamCount+4)*FParamFontHeight
      else AH:=(OutputCount+1)*FParamFontHeight;
    ARect.Left:=Trigger.DesignPos.X;
    ARect.Right:=Trigger.DesignPos.X+ParamWidth+OutputWidth;
    ARect.Top:=Trigger.DesignPos.Y;
    ARect.Bottom:=Trigger.DesignPos.Y+AH;
    with FBuffer.Canvas do begin
      Pen.Color:=clActiveBorder;
      Brush.Color:=clBtnFace;
      Rectangle(ARect);
      ARect.Bottom:=Trigger.DesignPos.Y+FParamFontHeight;
      Brush.Color:=$000000;
      Pen.Color:=Brush.Color;
      Font.Color:=$FFFFFF;
      Rectangle(ARect);
      TextOut(ARect.Left+3,ARect.Top,Name);
      Font.Color:=$000000;

      //ARect.Top:=ARect.Bottom;
      ARect.Left:=Trigger.DesignPos.X;
      //ARect.Bottom:=ARect.Top+ParamFontHeight;
      ARect.Right:=Trigger.DesignPos.X+ParamWidth;

      Brush.Color:=PlugColors[vColor];
      Pen.Color:=Brush.Color;

      ARect.Top:=ARect.Bottom;
      ARect.Bottom+=FParamFontHeight;
      Rectangle(ARect);
      TextOut(ARect.Left+3,ARect.Top,C1Desc);

      ARect.Top:=ARect.Bottom;
      ARect.Bottom+=FParamFontHeight;
      Rectangle(ARect);
      TextOut(ARect.Left+3,ARect.Top,C2Desc);

      ARect.Top:=ARect.Bottom;
      ARect.Bottom+=FParamFontHeight;
      Rectangle(ARect);
      TextOut(ARect.Left+3,ARect.Top,C3Desc);

      for I:=0 to ParamCount-1 do with VisParamDesc[I] do begin
        ARect.Top:=ARect.Bottom;
        ARect.Bottom+=FParamFontHeight;
        Brush.Color:=PlugColors[AType];
        Pen.Color:=Brush.Color;
        Rectangle(ARect);
        TextOut(ARect.Left+3,ARect.Top,Name);
      end;

      //ARect.Top:=P.Y+ParamFontHeight;
      ARect.Left:=Trigger.DesignPos.X+ParamWidth;
      ARect.Bottom:=Trigger.DesignPos.Y+FParamFontHeight;
      ARect.Right:=ARect.Left+OutputWidth;
      for I:=0 to OutputCount-1 do with OutputDesc[I] do begin
        ARect.Top:=ARect.Bottom;
        ARect.Bottom+=FParamFontHeight;
        Brush.Color:=OutputColors[AType];
        Pen.Color:=Brush.Color;
        Rectangle(ARect);
        TextOut(ARect.Left+3,ARect.Top,Name);
      end;
    end;
  end;
end;

function TTriggerEdit.ElemPos(const Elem: TTriggerSelInfo): TRect;
begin
  with Elem do begin
    if Layer>=0 then with FPreset^.Layers[Layer].DesignPos do begin
      Result.Left:=X;
      Result.Top:=Y;
    end else with FPreset^.Triggers[not Layer].DesignPos do begin
      Result.Left:=X;
      Result.Top:=Y;
    end;
    if Param=-1 then begin
      Result.Right:=Result.Left+ParamWidth+OutputWidth;
      Result.Bottom:=Result.Top+FParamFontHeight;
      exit;
    end;
    if Output then begin
      Result.Left+=ParamWidth;
      Result.Right:=Result.Left+OutputWidth;
    end else Result.Right:=Result.Left+ParamWidth;
      Result.Top+=(Param+1)*FParamFontHeight;
      Result.Bottom:=Result.Top+FParamFontHeight;
  end;
end;

function TTriggerEdit.ElemAt(const P: TPoint): TTriggerSelInfo;

  function ElemAt_GetHeight(const Vis: TVisType): Integer;
  var
    ACount: Integer;
  begin
    Result:=Length(Vis.VisParamDesc)+4; //4=1+3 +1 wegen Kopfzeile +3 wegen Standardparametern
    ACount:=Length(Vis.OutputDesc)+1; //+1 wegen Kopfzeile
    if ACount>Result then Result:=ACount;
    Result*=FParamFontHeight;
  end;

  procedure ElemAt_DoSearch(var Layers: TPresetLayers{; var Pos: TPositions});
  var
    AX,AY: Integer;
  begin
    Result.Layer:=Length(Layers)-1;
    if Result.Layer<0 then exit;
    with Layers[Result.Layer].DesignPos do begin
      AX:=X;
      AY:=Y;
    end;
    while (P.X<AX) or (P.X>AX+MachineWidth) or (P.Y<AY) or (P.Y>AY+ElemAt_GetHeight(Visualisations(Layers[Result.Layer].VisID))) do begin
      Dec(Result.Layer);
      if Result.Layer<0 then exit;
      with Layers[Result.Layer].DesignPos do begin
        AX:=X;
        AY:=Y;
      end;
    end;
    Result.Output:=(P.X>AX+ParamWidth);
    Result.Param:=((P.Y-AY) div FParamFontHeight)-1;
    if Result.Output then begin
      if Result.Param>=Length(Visualisations(Layers[Result.Layer].VisID).OutputDesc)
        then Result.Param:=-2;
    end else begin
      if Result.Param>=Length(Visualisations(Layers[Result.Layer].VisID).VisParamDesc)+3
        then Result.Param:=-2;
    end;
  end;

begin
  ElemAt_DoSearch(FPreset^.Triggers{,TPos});
  if Result.Layer<0 then begin
    ElemAt_DoSearch(FPreset^.Layers{,LPos});
    if Result.Layer<0 then Result.Param:=-3;
  end else Result.Layer:=not Result.Layer;
end;



function TTriggerEdit.GetVisParamType(const Vis: TVisType; const Index: Integer): TVisParamType;
begin
  if Index<3
    then Result:=vColor
    else Result:=Vis.VisParamDesc[Index-3].AType
end;

function TTriggerEdit.ParamIsInput(const ParamType: TVisParamType): Boolean;
begin
  Result:=(ParamType in InputVisTypes);
end;

function TTriggerEdit.ParamIsInput(const Vis: TVisType; const Index: Integer): Boolean;
begin
  if Index<3
    then Result:=true
    else Result:=((Vis.VisParamDesc[Index-3].AType) in InputVisTypes);
end;

procedure TTriggerEdit.CreateCon(var OutVis,InVis: TPresetVis; const OutIndex,InIndex,InLayer: Integer);
var
  NewOutput: TGenerellVisOutput;
begin
  {if Visualisations(Outvis.VisID).VisParamDesc[OutIndex].AType=vCall then begin
    with NewCall do begin
      Trig:=InLayer;
    end;
    SetVisParam(OutVis,OutIndex,NewCall);
  end else} begin
    with NewOutput.Trig do begin
      Layer:=InLayer;
      Param:=InIndex;
    end;
    SetVisParam(OutVis,OutIndex,NewOutput);
  end;
end;

procedure TTriggerEdit.Connect(const TI1,TI2: TTriggerInfo);

  //function GetTriggerObject(const TI: TTriggerInfo):

  procedure GetCon(const TI: TTriggerInfo; var con: PPresetVis);
  begin
    if TI.Layer>=0
      then con:=@FPreset^.Layers[TI.Layer]
      else con:=@FPreset^.Triggers[-TI.Layer-1];
  end;

var
  con1,con2,conin,conout  : PPresetVis;
  OutIndex,InIndex,InLayer: Integer;
begin
  //Meldung('LastLayer: '+IntToStr(TI1.Layer)+'; LastParam: '+IntToStr(TI1.Param)+'; SelLayer: '+IntToStr(TI2.Layer)+'; SelParam: '+IntToStr(TI2.Param),TdmInfo);
  if (TI1.Param<-1) or (TI2.Param<-1) then exit;
  GetCon(TI1,con1);
  GetCon(TI2,con2);
  if ParamIsInput(Visualisations(con1^.VisID),TI1.Param) then begin
    if ParamIsInput(Visualisations(con2^.VisID),TI2.Param) then begin
      Meldung('Ein Eingang kann nicht mit einem Eingang verbunden werden',TdMWarnung);
      exit;
    end;
    conin:=con1;
    conout:=con2;
    OutIndex:=TI2.Param;
    InIndex:=TI1.Param;
    InLayer:=TI1.Layer;
  end else begin
    if not ParamIsInput(Visualisations(con2^.VisID),TI2.Param) then begin
      Meldung('Ein Ausgang kann nicht mit einem Ausgang verbunden werden',TdMWarnung);
      exit;
    end;
    conout:=con1;
    conin:=con2;
    OutIndex:=TI1.Param;
    InIndex:=TI2.Param;
    InLayer:=TI2.Layer;
  end;
  (*if ParamIsInput(Visualisations(con2^.VisID),TI2.Param){Visualisations(con2^.VisID).VisParamDesc[TI2.Param].AType in InputVisTypes} then begin
    if conin=nil then conin:=con2 else begin
      Meldung('Ein Eingang kann nicht mit einem Eingang verbunden werden',TdMWarnung);
      exit;
    end;
  end else begin
    if conout=nil then conout:=con2 else begin
      Meldung('Ein Ausgang kann nicht mit einem Ausgang verbunden werden',TdMWarnung);
      exit;
    end;
  end;*)
  CreateCon(conout^,conin^,OutIndex,InIndex,InLayer);
  DrawTriggers;
end;

{procedure TTriggerEditor.FormResize(Sender: TObject);
begin
  if FPreset<>nil then begin
    with TriggerImage do begin
      Picture.Bitmap.Width:=Width;
      Picture.Bitmap.Height:=Height;
    end;
    DrawTriggers;
  end;
end;}

procedure TTriggerEdit.NextTrigInfo(const Layers: TPresetLayers; var TrigInfo: TTriggerInfo);
begin
  if TrigInfo.Param<Length(Visualisations(Layers[TrigInfo.Layer].VisID).VisParamDesc)+2 then Inc(TrigInfo.Param) else begin
    Inc(TrigInfo.Layer);
    TrigInfo.Param:=-1;
  end;
end;

function TTriggerEdit.LayerPos(const Trig: TTriggerInfo): TRect;
var
  I        : Integer;
  ATrigInfo: TTriggerInfo;
begin
  ATrigInfo.Layer:=0;
  ATrigInfo.Param:=-1;
  I:=0;
  if Trig.Layer<0 then begin
    Result.Left:=TrigXDist;
    Result.Right:=TrigXDist+TrigSize;
    while (ATrigInfo.Param<>Trig.Param) or (ATrigInfo.Layer<>-Trig.Layer-1) do begin
      NextTrigInfo(FPreset^.Triggers,ATrigInfo);
      Inc(I);
    end;
  end else begin
    Result.Right:=FBuffer.Width-TrigXDist;
    Result.Left:=Result.Right-TrigSize;
    while (ATrigInfo.Param<>Trig.Param) or (ATrigInfo.Layer<>Trig.Layer) do begin
      NextTrigInfo(FPreset^.Layers,ATrigInfo);
      Inc(I);
    end;
  end;
  Result.Top:=I*FParamFontHeight;
  Result.Bottom:=Result.Top+FParamFontHeight;
end;

function TTriggerEdit.LayerAt(const X,Y: Integer): TTriggerInfo;
var
  AW,Top2: Integer;

  procedure DoSearch(const Layers: TPresetLayers);
  var
    L: Integer;
  begin
    L:=Length(Layers);
    while Top2<Y do begin
      if Result.Layer>=L then begin
        Result.Param:=-2;
        exit;
      end;
      NextTrigInfo(Layers,Result);
      Top2+=FParamFontHeight;
    end;
    if Result.Layer>=L then Result.Param:=-2;
  end;

begin
  AW:=FBuffer.Width;
  //AH:=TriggerImage.Height;
  Top2:=FParamFontHeight;
  Result.Param:=-1;
  Result.Layer:=0;
  if (X>=TrigXDist) and (X<=TrigXDist+TrigSize) then begin
    DoSearch(FPreset^.Triggers);
    Result.Layer:=-Result.Layer-1;
  end else begin
    if (X<=AW-TrigXDist) and (X>=AW-TrigXDist-TrigSize) then begin
      DoSearch(FPreset^.Layers);
    end else Result.Param:=-2;
  end;
end;

procedure TTriggerEdit.DrawTriggers;
var
  L     : Integer;
  ARect2: TRect;

  procedure DrawPlug(const PlugRect: TRect; const PlugDesc: string; const PlugType: TVisParamType);
  begin
    with FBuffer.Canvas do begin
      Brush.Color:=PlugColors[PlugType];
      Pen.Color:=Brush.Color;
      Rectangle(PlugRect);
      TextOut(PlugRect.Left+3,PlugRect.Top,PlugDesc);
    end;
  end;

  procedure DrawNameField(const PlugRect: TRect; const PlugDesc: string);
  begin
    with FBuffer.Canvas do begin
      Brush.Color:=$000000;
      Pen.Color:=Brush.Color;
      Font.Color:=$FFFFFF;
      Rectangle(PlugRect);
      TextOut(PlugRect.Left+3,PlugRect.Top,PlugDesc);
    end;
  end;

  procedure DrawLayers(const Layers: TPresetLayers);
  var
    I,J: Integer;
  begin
    FLastLayer.Param:=-2;
    with FBuffer.Canvas do begin
      ARect2.Top:=0;
      ARect2.Bottom:=FParamFontHeight;
      for I:=0 to Length(Layers)-1 do begin
        with Layers[I] do begin
          with Visualisations(VisID) do begin
            //Font.Color:=$FFFFFF;
            DrawNameField(ARect2,Name);
            Font.Color:=$000000;
            ARect2.Top:=ARect2.Bottom;
            ARect2.Bottom+=FParamFontHeight;

            L:=Length(VisParamDesc);

            DrawPlug(ARect2,C1Desc,vColor);
            ARect2.Top:=ARect2.Bottom;
            ARect2.Bottom+=FParamFontHeight;

            DrawPlug(ARect2,C2Desc,vColor);
            ARect2.Top:=ARect2.Bottom;
            ARect2.Bottom+=FParamFontHeight;

            DrawPlug(ARect2,C3Desc,vColor);
            ARect2.Top:=ARect2.Bottom;
            ARect2.Bottom+=FParamFontHeight;

            for J:=0 to L-1 do begin
              with VisParamDesc[J] do DrawPlug(ARect2,Name,AType);
              ARect2.Top:=ARect2.Bottom;
              ARect2.Bottom+=FParamFontHeight;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure DoDrawConnections(var ALayer: TPresetVis; const ALayerIndex: Integer);
  var
    J          : Integer;
    Rect1,Rect2: TRect;
    TempSelInfo: TTriggerSelInfo;
  begin
    for J:=0 to Length(ALayer.VisOutputs)-1 do with ALayer.VisOutputs[J] do if Param>=-1 then begin
      TempSelInfo.Param:=J;
      TempSelInfo.Layer:=ALayerIndex;
      TempSelInfo.Output:=true;
      Rect1:=ElemPos(TempSelInfo);
      TempSelInfo.Param:=Param;
      TempSelInfo.Layer:=Layer;
      TempSelInfo.Output:=false;
      Rect2:=ElemPos(TempSelInfo);
      FBuffer.Canvas.Line(Rect1.Right,(Rect1.Top+Rect1.Bottom) div 2,Rect2.Left,(Rect2.Top+Rect2.Bottom) div 2);
    end;
  end;

var
  I: Integer;
begin
  with FBuffer do with FPreset^ do begin
    Canvas.Brush.Color:=$FFFFFF;
    Canvas.Pen.Color:=$FFFFFF;
    Canvas.Rectangle(0,0,Width,Height);
    for I:=0 to Length(Layers)-1 do DrawTrigger({LPos[I],}Layers[I]);
    for I:=0 to Length(Triggers)-1 do DrawTrigger({TPos[I],}Triggers[I]);
    Canvas.Pen.Color:=clBlack;
    for I:=0 to Length(Layers)-1 do DoDrawConnections(Layers[I],I);
    for I:=0 to Length(Triggers)-1 do DoDrawConnections(Triggers[I],not I);
  end;
end;

{Allgemein}

procedure Register;
begin
  RegisterComponents('Muvi',[TTriggerEdit]);
end;

end.

