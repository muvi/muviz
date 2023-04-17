unit PresetUtil3_Wires;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, AdvCoord, ObjectClassBasic,
  ObjectClasses, PhysicsWire, GraphX32, Graphics;

type
  TWireVisibilityMode    = (wvmAll, wvmOne, wvmBoth);

  TWireConfiguration     = class
  protected
    function GetPixelSize: Real; virtual; abstract;
    function GetWireOffset: TPoint; virtual; abstract;
    function GetWireSettings: TPhysicsWireSettings; virtual; abstract;
    function GetWireKnotCount: Integer; virtual; abstract;
    function GetWireVisibilityMode: TWireVisibilityMode; virtual; abstract;
  public
    property PixelSize: Real read GetPixelSize;
    property WireKnotCount: Integer read GetWireKnotCount;
    property WireOffset: TPoint read GetWireOffset;
    property WireSettings: TPhysicsWireSettings read GetWireSettings;
    property WireVisibilityMode: TWireVisibilityMode read GetWireVisibilityMode;
  end;

  TBasicWireManager      = class;
  TBasicPWContainer      = class;
  TWireInitializationMode= set of (wimFirst, wimLast);

  TAbstractConnection    = class (TObjectItem)
  strict private
    FWire: TBasicPWContainer;
    function GetPosition: TRealPoint;
    procedure SetPosition(AValue: TRealPoint);
  private
    procedure WireRemoved; inline;
  strict protected
    function GetColor: TColor32; virtual;
    function GetHasColor: Boolean; virtual;
    property Wire: TBasicPWContainer read FWire;
  public
    constructor Create(AReplaces: TAbstractConnection);
    constructor Create(AWire: TBasicPWContainer);
    destructor Destroy; override;
    property Color: TColor32 read GetColor;
    property HasColor: Boolean read GetHasColor;
    property Position: TRealPoint read GetPosition write SetPosition;
  end;

  TBasicPWContainer      = class (TObjectItem)
  strict private
    FConfig                : TWireConfiguration;
    FWire                  : TPhysicsWire;
    FFirstConnection       : TAbstractConnection;
    FLastConnection        : TAbstractConnection;
    FFirstConnectionColor  : TColor32;
    FLastConnectionColor   : TColor32;
    FOnChanged             : TNotifyEvent;
    FInitializationMode    : TWireInitializationMode;
    procedure SetFirstConnection(Value: TAbstractConnection); inline;
    procedure SetLastConnection(Value: TAbstractConnection); inline;
    function GetFirstKnot: TRealPoint; inline;
    procedure SetFirstKnot(Value: TRealPoint); inline;
    function GetLastKnot: TRealPoint; inline;
    procedure SetLastKnot(Value: TRealPoint); inline;
    function GetKnotAt(AConnection: TAbstractConnection): TRealPoint; inline;
    procedure SetKnotAt(AConnection: TAbstractConnection; Value: TRealPoint); inline;
    function GetOtherEnd(AConnection: TAbstractConnection): TAbstractConnection; inline;

    function HasColor(AConnection: TAbstractConnection): Boolean; inline;
    procedure DetermineColor; inline;
    property FirstKnot: TRealPoint read GetFirstKnot write SetFirstKnot;
    property LastKnot: TRealPoint read GetLastKnot write SetLastKnot;
  protected
    property Config: TWireConfiguration read FConfig;

    procedure Disconnect(AConnection: TAbstractConnection); inline;
    procedure Connect(AConnection: TAbstractConnection; AReplaces: TAbstractConnection); inline;
    //is executed after removing this control from lists
    procedure DoFree; override;

    property KnotAt[AConnection: TAbstractConnection]: TRealPoint read GetKnotAt write SetKnotAt;
    property OtherEnd[APlug: TAbstractConnection]: TAbstractConnection read GetOtherEnd;
  public
    constructor Create(AConfig: TWireConfiguration);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; ARect: TRect);

    property FirstConnection: TAbstractConnection read FFirstConnection;
    property LastConnection: TAbstractConnection read FLastConnection;
    property Wire: TPhysicsWire read FWire;
    //in preset context: used to start the rendering process
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TBasicWireManager      = class
  strict private
    FWires                        : TObjectList;
    FMovingCount                  : Cardinal;
    FConfig                       : TWireConfiguration;
    //events
    FOnWireAdded                  : TNotifyEvent;
    FOnWireChanged                : TNotifyEvent;
  strict protected
    procedure AddWire(AWire: TBasicPWContainer);
  public
    constructor Create(AConfig: TWireConfiguration);
    destructor Destroy; override;
    procedure Rewire(const t: Real = 1.0; const Steps: Cardinal = 1);

    property Config: TWireConfiguration read FConfig;
    property MovingCount: Cardinal read FMovingCount;  //the number of wires in non-stable state
    property Wires: TObjectList read FWires;  //do not modify!
    //events
    property OnWireAdded: TNotifyEvent read FOnWireAdded write FOnWireAdded;
    property OnWireChanged: TNotifyEvent read FOnWireChanged write FOnWireChanged;
  end;

const
  UNDEFINEDCOLOR   : TColor32    = $FF888888;

implementation

const
  INNERCABLELIGHTEN: Byte        = 24;  //for drawing

procedure DrawWire(Canvas: TCanvas; ARect: TRect; AWire: TPhysicsWire; AFirstColor,ALastColor: TColor32; APixelSize: Real; AVisibilityMode: TWireVisibilityMode); forward;

{%REGION TAbstractConnection}

constructor TAbstractConnection.Create(AReplaces: TAbstractConnection);
begin
  inherited Create;
  Assert(AReplaces<>nil);
  FWire:=AReplaces.Wire;
  FWire.Connect(Self, AReplaces);
end;

constructor TAbstractConnection.Create(AWire: TBasicPWContainer);
begin
  inherited Create;
  Assert(AWire<>nil);
  FWire:=AWire;
  AWire.Connect(Self, nil);
end;

destructor TAbstractConnection.Destroy;
begin
  if FWire<>nil
    then FWire.Disconnect(Self);
  inherited Destroy;
end;

function TAbstractConnection.GetColor: TColor32;
begin
  Result:=UNDEFINEDCOLOR;
end;

function TAbstractConnection.GetHasColor: Boolean;
begin
  Result:=true;
end;

function TAbstractConnection.GetPosition: TRealPoint;
begin
  Result:=FWire.KnotAt[Self];
end;

procedure TAbstractConnection.SetPosition(AValue: TRealPoint);
begin
  FWire.KnotAt[Self]:=AValue;
end;

procedure TAbstractConnection.WireRemoved; inline;
begin
  FWire:=nil;
  Destroy;
end;

{%ENDREGION}
{%REGION TBasicPWContainer}

constructor TBasicPWContainer.Create(AConfig: TWireConfiguration);
begin
  inherited Create;
  FConfig:=AConfig;
  FFirstConnection:=nil;
  FLastConnection:=nil;
  FWire:=TPhysicsWire.Create(ZEROCENTER,ZEROCENTER,FConfig.WireSettings);
  FFirstConnectionColor:=UNDEFINEDCOLOR;
  FLastConnectionColor:=UNDEFINEDCOLOR;
  FInitializationMode:=[];
end;

destructor TBasicPWContainer.Destroy;
begin
  FWire.Destroy;
  inherited Destroy;
end;

procedure TBasicPWContainer.DoFree;
begin
  if FFirstConnection<>nil
    then FFirstConnection.WireRemoved;
  if FLastConnection<>nil
    then FLastConnection.WireRemoved;
end;

procedure TBasicPWContainer.Disconnect(AConnection: TAbstractConnection); inline;
begin
  if FFirstConnection=AConnection
    then SetFirstConnection(nil)
    else begin
      Assert(FLastConnection = AConnection);
      SetLastConnection(nil);
    end
end;

procedure TBasicPWContainer.Connect(AConnection: TAbstractConnection; AReplaces: TAbstractConnection); inline;
begin
  if FFirstConnection=AReplaces
    then SetFirstConnection(AConnection)
    else begin
      Assert(FLastConnection=AReplaces);
      SetLastConnection(AConnection);
    end;
  if AReplaces<>nil
    then AReplaces.WireRemoved;
end;

procedure TBasicPWContainer.Draw(Canvas: TCanvas; ARect: TRect);
begin
  if FInitializationMode = [wimFirst, wimLast]
    then DrawWire(Canvas,ARect,FWire,FLastConnectionColor,FFirstConnectionColor,FConfig.PixelSize, FConfig.WireVisibilityMode);
end;

function TBasicPWContainer.HasColor(AConnection: TAbstractConnection): Boolean; inline;
begin
  Result:=(AConnection <> nil) and AConnection.HasColor;
end;

procedure TBasicPWContainer.DetermineColor; inline;
begin
  if HasColor(FLastConnection) then begin
    FLastConnectionColor:=FLastConnection.Color;
    if HasColor(FFirstConnection)
      then FFirstConnectionColor:=FFirstConnection.Color
      else FFirstConnectionColor:=FLastConnectionColor;
  end else begin
    if HasColor(FFirstConnection)
      then FFirstConnectionColor:=FFirstConnection.Color
      else FFirstConnectionColor:=UNDEFINEDCOLOR;
    FLastConnectionColor:=FFirstConnectionColor;
  end;
end;

procedure TBasicPWContainer.SetFirstConnection(Value: TAbstractConnection);
begin
  FFirstConnection:=Value;
  if (Value = nil) and (FLastConnection = nil) then begin
    Destroy;
    exit;
  end;
  DetermineColor;
end;

procedure TBasicPWContainer.SetLastConnection(Value: TAbstractConnection);
begin
  FLastConnection:=Value;
  if (Value = nil) and (FFirstConnection = nil) then begin
    Destroy;
    exit;
  end;
  DetermineColor;
end;

function TBasicPWContainer.GetFirstKnot: TRealPoint;
begin
  Result:=FWire.FirstKnot/Config.PixelSize;
end;

procedure TBasicPWContainer.SetFirstKnot(Value: TRealPoint);
var
  ANewVal: TRealPoint;
begin
  //do not do something if the position has not changed.
  //This happens often (ie. because of scrolling).
  ANewVal:=Value*Config.PixelSize;
  if ANewVal<>FWire.FirstKnot then begin
    FWire.FirstKnot:=ANewVal;
    if Assigned(FOnChanged)
      then FOnChanged(Self);
  end;
  if FInitializationMode = [wimLast]
    then FWire.InitKnots;
  Include(FInitializationMode, wimFirst);
end;

function TBasicPWContainer.GetLastKnot: TRealPoint;
begin
  Result:=FWire.LastKnot/Config.PixelSize;
end;

procedure TBasicPWContainer.SetLastKnot(Value: TRealPoint);
var
  ANewVal: TRealPoint;
begin
  //do not do something if the position has not changed.
  //This happens often (ie. because of scrolling).
  ANewVal:=Value*Config.PixelSize;
  if ANewVal<>FWire.LastKnot then begin
    FWire.LastKnot:=ANewVal;
    if Assigned(FOnChanged)
      then FOnChanged(Self);
  end;
  if FInitializationMode = [wimFirst]
    then FWire.InitKnots;
  Include(FInitializationMode, wimLast);
end;

function TBasicPWContainer.GetKnotAt(AConnection: TAbstractConnection): TRealPoint; inline;
begin
  if FFirstConnection=AConnection
    then Result:=LastKnot
    else Result:=FirstKnot;
end;

procedure TBasicPWContainer.SetKnotAt(AConnection: TAbstractConnection; Value: TRealPoint); inline;
begin
  if FFirstConnection=AConnection
    then LastKnot:=Value
    else FirstKnot:=Value;
end;

function TBasicPWContainer.GetOtherEnd(AConnection: TAbstractConnection): TAbstractConnection; inline;
begin
  if FFirstConnection=AConnection
    then Result:=FLastConnection
    else Result:=FFirstConnection;
end;

{%ENDREGION}
{%REGION TBasicWireManager}

constructor TBasicWireManager.Create(AConfig: TWireConfiguration);
begin
  inherited Create;
  FConfig:=AConfig;
  FMovingCount:=0;
  FWires:=TObjectList.Create(true);
end;

destructor TBasicWireManager.Destroy;
begin
  FWires.Destroy;
  inherited Destroy;
end;

procedure TBasicWireManager.Rewire(const t: Real = 1.0; const Steps: Cardinal = 1);
var
  at   : Real;
  I    : Integer;
  AItem: TObjectListItem;
begin
  at:=t/Steps;
  FMovingCount:=FWires.Count;

  AItem:=FWires.First;
  while AItem<>nil do begin
    with TBasicPWContainer(AItem.Content).Wire do begin
      if InRest
        then Dec(FMovingCount)
        else for I:=1 to Steps do Calc(at);
    end;
    AItem:=AItem.Next;
  end;
end;

procedure TBasicWireManager.AddWire(AWire: TBasicPWContainer);
begin
  if Assigned(FOnWireChanged)
    then AWire.OnChanged:=FOnWireChanged;
  FWires.Add(AWire);
  Inc(FMovingCount);
  if Assigned(FOnWireAdded)
    then FOnWireAdded(Self);
  AWire.Wire.KnotCount:=Config.WireKnotCount;
end;

{%ENDREGION}
{%REGION Misc}

procedure DrawWire(Canvas: TCanvas; ARect: TRect; AWire: TPhysicsWire; AFirstColor,ALastColor: TColor32; APixelSize: Real; AVisibilityMode: TWireVisibilityMode);
var
  AP      : TPoint;
  AClrStep: Single;

  procedure DoDrawWire(AFirstColor2,ALastColor2: TColor32); inline;
  var
    I: Integer;
  begin
    Canvas.Pen.Color:=Color32ToColor(AFirstColor2);
    Canvas.MoveTo(Round(AWire.FirstKnot.X/APixelSize)-ARect.Left,Round(AWire.FirstKnot.Y/APixelSize)-ARect.Top);
    for I:=0 to AWire.KnotCount-1 do begin
      AP:=Classes.Point(Round(AWire.Knots[I].X/APixelSize)-ARect.Left,Round(AWire.Knots[I].Y/APixelSize)-ARect.Top);
      Canvas.LineTo(AP);
      Canvas.Pen.Color:=Color32ToColor(BlendAlpha(Round(sqr(I)*AClrStep),AFirstColor2,ALastColor2));
    end;
    Canvas.LineTo(Round(AWire.LastKnot.X/APixelSize)-ARect.Left,Round(AWire.LastKnot.Y/APixelSize)-ARect.Top);
  end;

  function PointOutOfRect(APoint: TRealPoint): Boolean;
  begin
    Result:=not InRect(Point(APoint / APixelSize), ARect);
  end;

begin
  if not InView(Rect(AWire.ViewRect/APixelSize),ARect) then exit;
  //check VisibilityMode
  case AVisibilityMode of
    wvmOne : if PointOutOfRect(AWire.FirstKnot)
        and PointOutOfRect(AWire.LastKnot)
          then exit;
    wvmBoth: if PointOutOfRect(AWire.FirstKnot)
        or PointOutOfRect(AWire.LastKnot)
          then exit;
    //wvmAll: do nothing;
  end;
  //draw wire
  AClrStep:=$FF/sqr(AWire.KnotCount-1);
  Canvas.Pen.Width:=3;
  DoDrawWire(AFirstColor,ALastColor);
  Canvas.Pen.Width:=1;
  DoDrawWire(Lighten(AFirstColor,INNERCABLELIGHTEN),Lighten(ALastColor,INNERCABLELIGHTEN))
end;

{%ENDREGION}

end.

