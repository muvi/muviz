unit TouchControlBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  ETouch             = class (Exception)
  end;

  TTouchPoint        = class
  strict private
    FPosition    : TPoint;
    FSourceHandle: THandle;
    FID          : Cardinal;
    FIsPalm      : Boolean;
    FSize        : TPoint;
  private
    procedure Update(APosition: TPoint; AIsPalm: Boolean; ASize: TPoint);
    property ID: Cardinal read FID;
  public
    constructor Create(AID: Cardinal; APosition: TPoint; AIsPalm: Boolean; ASize: TPoint; ASourceHandle: THandle);
    destructor Destroy; override;
    property Position: TPoint read FPosition;
    property SourceHandle: THandle read FSourceHandle;
    property IsPalm: Boolean read FIsPalm;
    property Size: TPoint read FSize;
  end;

  TBasicTouchControl = class (TCustomControl)
  strict private
    FTouchPoints      : array of TTouchPoint;
    FPrimaryTouchPoint: TTouchPoint;
    function GetTouchPointCount: Integer; inline;
    function GetTouchPoint(AIndex: Integer): TTouchPoint; inline;
  protected
    function AddOrUpdateTouchPoint(AID: Cardinal; APosition: TPoint; AIsPalm: Boolean; ASize: TPoint; ASourceHandle: THandle; AIsPrimary: Boolean): TTouchPoint;
    function RemoveTouchPoint(AID: Cardinal): TTouchPoint;
    procedure TouchDown(APoint: TTouchPoint; var AHandled: Boolean); virtual;
    procedure TouchUp(APoint: TTouchPoint; var AHandled: Boolean); virtual;
    procedure TouchMove(APoint: TTouchPoint; var AHandled: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property PrimaryTouchPoint: TTouchPoint read FPrimaryTouchPoint;
    property TouchPointCount: Integer read GetTouchPointCount;
    property TouchPoints[AIndex: Integer]: TTouchPoint read GetTouchPoint;
  end;

implementation

{%REGION TTouchPoint}

constructor TTouchPoint.Create(AID: Cardinal; APosition: TPoint; AIsPalm: Boolean; ASize: TPoint; ASourceHandle: THandle);
begin
  inherited Create;
  FPosition:=APosition;
  FIsPalm:=AIsPalm;
  FSize:=ASize;
  FID:=AID;
  FSourceHandle:=ASourceHandle;
end;

destructor TTouchPoint.Destroy;
begin
  inherited Destroy;
end;

procedure TTouchPoint.Update(APosition: TPoint; AIsPalm: Boolean; ASize: TPoint);
begin
  FPosition:=APosition;
  FIsPalm:=AIsPalm;
  FSize:=ASize;
end;

{%ENDREGION}
{%REGION TBasicTouchControl}

constructor TBasicTouchControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrimaryTouchPoint:=nil;
end;

destructor TBasicTouchControl.Destroy;
begin
  inherited Destroy;
end;

function TBasicTouchControl.GetTouchPointCount: Integer; inline;
begin
  Result:=Length(FTouchPoints);
end;

function TBasicTouchControl.GetTouchPoint(AIndex: Integer): TTouchPoint; inline;
begin
  Result:=FTouchPoints[AIndex];
end;

function TBasicTouchControl.AddOrUpdateTouchPoint(AID: Cardinal; APosition: TPoint; AIsPalm: Boolean; ASize: TPoint; ASourceHandle: THandle; AIsPrimary: Boolean): TTouchPoint;

  procedure DoFind; inline;
  var
    I: Integer;
  begin
    Result:=nil;
    for I:=0 to Length(FTouchPoints)-1 do begin
      if FTouchPoints[I].ID = AID then begin
        //update
        Result:=FTouchPoints[I];
        Result.Update(APosition, AIsPalm, ASize);
        exit;
      end;
    end;
    //add
    I:=Length(FTouchPoints);
    SetLength(FTouchPoints, I+1);
    Result:=TTouchPoint.Create(AID, APosition, AIsPalm, ASize, ASourceHandle);
    FTouchPoints[I]:=Result;
  end;

begin
  DoFind;
  if AIsPrimary
    then FPrimaryTouchPoint:=Result;
end;

function TBasicTouchControl.RemoveTouchPoint(AID: Cardinal): TTouchPoint;
var
  I, L: Integer;
begin
  L:=Length(FtouchPoints);
  for I:=0 to L-1 do begin
    if FTouchPoints[I].ID = AID then begin
      Result:=FTouchPoints[I];
      FTouchPoints[I]:=FTouchPoints[L-1];
      SetLength(FTouchPoints, L-1);
      if FPrimaryTouchPoint = Result
        then FPrimaryTouchPoint:=nil;
      exit;
    end;
  end;
  raise ETouch.Create('Could not find the touch point with ID ' + IntToStr(AID));
end;

procedure TBasicTouchControl.TouchDown(APoint: TTouchPoint; var AHandled: Boolean);
begin
  //do bothing
end;

procedure TBasicTouchControl.TouchUp(APoint: TTouchPoint; var AHandled: Boolean);
begin
  //do bothing
end;

procedure TBasicTouchControl.TouchMove(APoint: TTouchPoint; var AHandled: Boolean);
begin
  //do bothing
end;

{%ENDREGION}

end.

