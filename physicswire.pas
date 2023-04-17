unit PhysicsWire;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvCoord, PhysProc, ObjectClassBasic, Math;

type
  TWireKnot           = record
    Position,Speed: TRealPoint;
  end;

  TPhysicsWireSettings= class
  private
    FWeight           : Real;
    FGravity          : Real;
    FD                : Real; //Federkonstante
    FKnotDistance     : Real;
    FSpeedConservation: Real;
    FWeightForce      : Real;
    FRestForce        : Real; //Maximale Kraft in N, die als Ruhe betrachtet wird und nicht mehr berechnet werden muss
    FBox              : TRealRect; //Bereich, den das Kabel nicht verlassen darf
    procedure SetWeight(Value: Real);
    procedure SetGravity(Value: Real);
    function GetFriction: Real;
    procedure SetFriction(const Value: Real);
  public
    constructor Create(const AD: Real = 300.0; const AWeight: Real = 4.0; const AKnotDistance: Real = 0.5; const AFriction: Real = 0.1; const AGravity: Real = 9.81; const ARestForce: Real = 1.0);
    destructor Destroy; override;
    procedure SetNoBox;

    property Box: TRealRect read FBox write FBox;
    property D: Real read FD write FD;
    property RestForce: Real read FRestForce write FRestForce;
    property Friction: Real read GetFriction write SetFriction;
    property Gravity: Real read FGravity write SetGravity;
    property KnotDistance: Real read FKnotDistance write FKnotDistance;
    property Weight: Real read FWeight write SetWeight;
    property WeightForce: Real read FWeightForce;
  end;

  TPhysicsWire        = class (TObjectItem)
  private
    FKnots            : array of TWireKnot;
    FWireSettings     : TPhysicsWireSettings;
    FOwnsSettings     : Boolean;
    FFirstKnot        : TRealPoint;
    FLastKnot         : TRealPoint;
    FFLViewRect       : TRealRect;
    FViewRect         : TRealRect;
    FMaxForce         : Real;
    FInRest           : Boolean;
    function GetKnotCount: Cardinal; inline;
    procedure SetKnotCount(Value: Cardinal);
    function GetKnot(Index: Cardinal): TRealPoint;
    procedure SetFirstKnot(const Value: TRealPoint);
    procedure SetLastKnot(const Value: TRealPoint);
    procedure SetFLViewRect;
  protected
    procedure SetKnotSpeed(var AKnot: TWireKnot; const PrevKnot,NextKnot: TRealPoint);
  public
    constructor Create(const AFirstKnot,ALastKnot: TRealPoint; const AWireSettings: TPhysicsWireSettings = nil; const AKnotCount: Cardinal = 1);
    destructor Destroy; override;
    procedure Calc(const t: Real = 1.0);
    procedure RestWakeUp;
    //places all knots on a straight line
    procedure InitKnots;
    property FirstKnot: TRealPoint read FFirstKnot write SetFirstKnot;
    property InRest: Boolean read FInRest;
    property KnotCount: Cardinal read GetKnotCount write SetKnotCount;
    property Knots[Index: Cardinal]: TRealPoint read GetKnot;
    property LastKnot: TRealPoint read FLastKnot write SetLastKnot;
    property MaxForce: Real read FMaxForce;
    property ViewRect: TRealRect read FViewRect;
    property WireSettings: TPhysicsWireSettings read FWireSettings;
  end;

implementation

{TPhysicsWireSettings}

constructor TPhysicsWireSettings.Create(const AD: Real = 300.0; const AWeight: Real = 4.0; const AKnotDistance: Real = 0.5; const AFriction: Real = 0.1; const AGravity: Real = 9.81; const ARestForce: Real = 1.0);
begin
  inherited Create;
  FD:=AD;
  FWeight:=AWeight;
  SetGravity(AGravity);
  FKnotDistance:=AKnotDistance;
  FSpeedConservation:=1-AFriction;
  FRestForce:=ARestForce;
  SetNoBox;
end;

destructor TPhysicsWireSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TPhysicsWireSettings.SetWeight(Value: Real);
begin
  FWeight:=Value;
  FWeightForce:=FWeight*FGravity;
end;

procedure TPhysicsWireSettings.SetGravity(Value: Real);
begin
  FGravity:=Value;
  FWeightForce:=FWeight*FGravity;
end;

function TPhysicsWireSettings.GetFriction: Real;
begin
  Result:=1-FSpeedConservation;
end;

procedure TPhysicsWireSettings.SetFriction(const Value: Real);
begin
  FSpeedConservation:=1-Value;
end;

procedure TPhysicsWireSettings.SetNoBox;
begin
  FBox:=RealRect(-Infinity,-Infinity,Infinity,Infinity);
end;

{TPhysicsWire}

constructor TPhysicsWire.Create(const AFirstKnot,ALastKnot: TRealPoint; const AWireSettings: TPhysicsWireSettings = nil; const AKnotCount: Cardinal = 1);
begin
  inherited Create;
  FFirstKnot:=AFirstKnot;
  SetLastKnot(ALastKnot);
  FViewRect:=FFLViewRect;
  FMaxForce:=0.0;
  FInRest:=false;
  if AKnotCount>0
    then SetKnotCount(AKnotCount)
    else SetKnotCount(1);
  FOwnsSettings:=(AWireSettings=nil);
  if FOwnsSettings
    then FWireSettings:=TPhysicsWireSettings.Create
    else FWireSettings:=AWireSettings;
end;

destructor TPhysicsWire.Destroy;
begin
  if FOwnsSettings then FWireSettings.Destroy;
  SetLength(FKnots,0);
  inherited Destroy;
end;

procedure TPhysicsWire.SetFLViewRect;
begin
  with FFLViewRect do begin
    if FFirstKnot.X<FLastKnot.X then begin
      Left:=FFirstKnot.X;
      Right:=FLastKnot.X;
    end else begin
      Left:=FLastKnot.X;
      Right:=FFirstKnot.X;
    end;
    if FFirstKnot.Y<FLastKnot.Y then begin
      Top:=FFirstKnot.Y;
      Bottom:=FLastKnot.Y;
    end else begin
      Top:=FLastKnot.Y;
      Bottom:=FFirstKnot.Y;
    end;
  end;
end;

procedure TPhysicsWire.SetKnotSpeed(var AKnot: TWireKnot; const PrevKnot,NextKnot: TRealPoint);
var
  AForce: TRealPoint;
  RForce: Real;
begin
  with FWireSettings do begin
    AForce:=SpringForce(PrevKnot,AKnot.Position,FD,FKnotDistance)+SpringForce(NextKnot,AKnot.Position,FD,FKnotDistance);
    AForce.Y+=FWeightForce;
    AKnot.Speed+=ForceToAcceleration(AForce,FWeight);
    AKnot.Speed*=FSpeedConservation;

    RForce:=sqrt(sqr(AForce.X)+sqr(AForce.Y));
    if RForce>FMaxForce then FMaxForce:=RForce;
  end;
end;

procedure TPhysicsWire.Calc(const t: Real = 1.0);
var
  I,L      : Integer;
begin
  FViewRect:=FFLViewRect;
  FMaxForce:=0.0;

  L:=Length(FKnots);
  if L=1 then SetKnotSpeed(FKnots[0],FFirstKnot,FLastKnot) else begin
    SetKnotSpeed(FKnots[0],FFirstKnot,FKnots[1].Position);
    for I:=1 to L-2
      do SetKnotSpeed(FKnots[I],FKnots[I-1].Position,FKnots[I+1].Position);
    SetKnotSpeed(FKnots[L-1],FKnots[L-2].Position,FLastKnot);
  end;

  with FWireSettings do for I:=0 to L-1 do with FKnots[I] do begin
    Position+=(Speed*t);
    //Check if in FWireSettings.Box
      if Position.X<FBox.Left
        then Position.X:=FBox.Left
        else if Position.X>FBox.Right
          then Position.X:=FBox.Right;
      if Position.Y<FBox.Top
        then Position.Y:=FBox.Top
        else if Position.Y>FBox.Bottom
          then Position.Y:=FBox.Bottom;
    //Check ViewRect
    if Position.X<FViewRect.Left
      then FViewRect.Left:=Position.X
      else if Position.X>FViewRect.Right
        then FViewRect.Right:=Position.X;
    if Position.Y<FViewRect.Top
      then FViewRect.Top:=Position.Y
      else if Position.Y>FViewRect.Bottom
        then FViewRect.Bottom:=Position.Y;
  end;
  if FMaxForce<=FWireSettings.FRestForce then FInRest:=true;
end;

procedure TPhysicsWire.RestWakeUp;
begin
  FInRest:=false;
end;

procedure TPhysicsWire.InitKnots;
var
  I     : Integer;
  AFLVec: TRealPoint;
begin
  FInRest:=false;
  AFLVec:=(FLastKnot-FFirstKnot)/(KnotCount+1);
  for I:=0 to KnotCount-1 do with FKnots[I] do begin
    Speed:=ZeroCenter;
    Position:=FFirstKnot+(AFLVec*(I+1));
  end;
  FViewRect:=FFLViewRect;
end;

function TPhysicsWire.GetKnotCount: Cardinal; inline;
begin
  Result:=Length(FKnots);
end;

procedure TPhysicsWire.SetKnotCount(Value: Cardinal);
begin
  if Value<=0 then exit;
  SetLength(FKnots,Value);
  InitKnots;
end;

procedure TPhysicsWire.SetFirstKnot(const Value: TRealPoint);
begin
  FInRest:=false;
  if IsZero(Value.X-FLastKnot.X) and IsZero(Value.Y-FLastKnot.Y)
    then FFirstKnot:=RealPoint(Value.X+1e-10,Value.Y)
    else FFirstKnot:=Value;
  SetFLViewRect;
end;

procedure TPhysicsWire.SetLastKnot(const Value: TRealPoint);
begin
  FInRest:=false;
  if IsZero(Value.X-FFirstKnot.X) and IsZero(Value.Y-FirstKnot.Y)
    then FLastKnot:=RealPoint(Value.X-1e-10,Value.Y)
    else FLastKnot:=Value;
  SetFLViewRect;
end;

function TPhysicsWire.GetKnot(Index: Cardinal): TRealPoint;
begin
  Result:=FKnots[Index].Position;
end;

end.

