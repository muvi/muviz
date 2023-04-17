unit MyRectangleDesigner; //sample component for Touch/Gesture handling
// fev/2013 Omar Reis

interface

uses
  SysUtils, Classes, Controls,Graphics,Types,Messages,Windows,
  D7GesturesHeader;

type
  // sample graphic rectangle object, using float coordinates
  TRealRect = Class
  public
    Left, Top, Right, Bottom: Single;
    Color:TColor;
    constructor Create;
    procedure   RandomizeRect;   //randomize rect coordinates
    function    AsTRect:TRect;   //convert to TRect for drawing
  end;

  // TMyRectangleDesigner - sample component for Gesture/Touch handling
  TMyRectangleDesigner = class(TCustomControl)
  private
    fRectList:TList;         //rectangle list
    fSelectedRect:TRealRect; //rectangle being manipulated ( drawn w/ fat line )
    // private Gesture state variables (add these to your code )
    f_ptFirst:TPoint;        //saved point
    f_dwArguments:Int64;     //saved T/G dw arguments

    // private Touch/Gesture procedures
    procedure ProcessGestureMove(dx, dy: integer);
    procedure ProcessGestureZoom(const k, ZoomCenterX, ZoomCenterY: Single);
    procedure ProcessGestureRotate(const angle, RotateCenterX, RotateCenterY:Single);
    // /Gesture

    procedure CreateRandomRects;
    procedure DrawBall(x, y: integer);
  protected
    procedure Paint; override;
    // touch/gesture notifications
    procedure WMTouch( var Msg: TMessage);                 message WM_TOUCH;
    procedure WMGestureNotify( var Msg: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMGesture( var Msg: TMessage);               message WM_GESTURE;
    procedure WMTabletFlick( var Msg: TMessage);           message WM_TABLET_FLICK;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ClearRectangles;
    Procedure   SelectNext; //select the next rectangle
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TMyRectangleDesigner]);
end;

{ TRealRect }

constructor TRealRect.Create;
begin
  inherited;
  case Random(8) of  //randomize color
   0: Color:=clBlue;
   1: Color:=clGreen;
   2: Color:=clRed;
   3: Color:=clNavy;
   4: Color:=clOlive;
   5: Color:=clTeal;
   6: Color:=clYellow;
   7: Color:=clWhite;
  end;
end;

procedure TRealRect.RandomizeRect;
begin
  Left    := Random(400);
  Top     := Random(300);
  Right   := Left + 50+ Random(400);
  Bottom  := Top  + 50+ Random(300);
end;

function TRealRect.AsTRect: TRect;
begin
  Result := Classes.Rect( Round(Left), Round(Top), Round(Right), Round(Bottom) );
end;

{ TMyRectangleDesigner }

constructor TMyRectangleDesigner.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csDoubleClicks, csOpaque]; //csOpaque adicionado Mai09: Om:

  fRectList:=TList.Create;
  fSelectedRect:=nil;

  CreateRandomRects; //populate
end;

destructor TMyRectangleDesigner.Destroy;
begin
  ClearRectangles;
  fRectList.Free;
  inherited;
end;

procedure TMyRectangleDesigner.ClearRectangles;
var i:Integer;
begin
  for i:=0 to fRectList.Count-1 do
    TRealRect(fRectList.Items[i]).Free;
  fRectList.Clear;
end;

procedure TMyRectangleDesigner.CreateRandomRects;
var i,n:Integer; aRRect:TRealRect;
begin
  Randomize;
  ClearRectangles;
  n:=10+Random(10);
  for i:= 1 to n do
    begin
      aRRect := TRealRect.Create;
      aRRect.RandomizeRect;
      fRectList.Add(aRRect);
    end;
  fSelectedRect := TRealRect(fRectList.Items[0]);
end;

procedure TMyRectangleDesigner.Paint;
var i:Integer; aRRect:TRealRect; R:TRect;
begin
  //clear background
  Canvas.Brush.Style :=bsSolid;
  Canvas.Brush.Color := Self.Color;
  Canvas.Pen.Style := psClear;
  Canvas.Rectangle(0,0,Width,Height);
  //prepare canvas to paint rects
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Width   := 3;

  for i:=0 to fRectList.Count-1 do //draw rectangles
    begin
      aRRect := TRealRect(fRectList.Items[i]);
      if Assigned(fSelectedRect) and (fSelectedRect=aRRect) then Canvas.Pen.Width := 2 //selected --> draws fat lines
        else Canvas.Pen.Width := 1;
      Canvas.Pen.Color   := aRRect.Color;
      R := aRRect.AsTRect;
      Canvas.Rectangle(R); //draw
    end;
end;

procedure TMyRectangleDesigner.SelectNext;
var ix:Integer;
begin
  if Assigned(fSelectedRect) then
    begin
      ix := fRectList.IndexOf(fSelectedRect);
      if ix<0 then ix:=0;
    end
    else ix:=0;

  inc(ix);
  if (ix>=fRectList.Count) then ix:=0;

  if (ix<fRectList.Count) then
    begin
      fSelectedRect := TRealRect( fRectList.Items[ix] );  //change selected rect
      Invalidate;
    end;
end;

procedure TMyRectangleDesigner.WMGestureNotify(var Msg: TWMGestureNotify);
begin
  //what is this for??
  Msg.Result := DefWindowProc( Handle, Msg.Msg, Msg.Unused, Longint(Msg.NotifyStruct));
end;

procedure TMyRectangleDesigner.DrawBall(x,y:integer);
var R:TRect;
begin
  R := Classes.Rect(X-5,Y-5,X+5,Y+5);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Width   := 1;
  Canvas.Pen.Color   := clBlack;
  Canvas.Ellipse(R);
end;

procedure TMyRectangleDesigner.WMGesture(var Msg: TMessage);
var gi:TGestureInfo;  bResult,bHandled:BOOL; dwError:DWORD; L:Integer;
    ptZoomCenterX,ptZoomCenterY,K,angle:Single; _ptSecond:TPoint;
begin
  bHandled:= False;
  if bTouchGestureAPIPresent then
    begin
      L:=SizeOf(gi);
      FillChar(gi,L,#0);
      gi.cbSize := L;
      bResult := GetGestureInfoFn(Msg.lParam , gi); // retrieve gesture info with lParam 

      if bResult then
        begin
          bHandled:=True;
          case gi.dwID of
            GID_BEGIN: ;
            GID_END:   ;
            GID_PAN:  begin
                        if gi.dwFlags=GF_Begin then
                          begin
                            f_ptFirst.x := gi.ptsLocation.x;
 	                    f_ptFirst.y := gi.ptsLocation.y;
 	                    f_ptFirst   := ScreenToClient(f_ptFirst);
                          end
                          else begin // read the second point (middle between fingers in the position)
 	                    _ptSecond.x := gi.ptsLocation.x;
 	                    _ptSecond.y := gi.ptsLocation.y;
 	                    _ptSecond   := ScreenToClient(_ptSecond);
 	                    ProcessGestureMove(_ptSecond.x-f_ptFirst.x,_ptSecond.y-f_ptFirst.y);
 	                    f_ptFirst := _ptSecond;      //save
                          end;
                      end;
            GID_ZOOM: begin
                        if gi.dwFlags=GF_Begin then
                          begin
                            f_ptFirst.x   := gi.ptsLocation.x;
 	                    f_ptFirst.y   := gi.ptsLocation.y;
 	                    f_ptFirst     := ScreenToClient(f_ptFirst);
                            f_dwArguments := gi.ullArguments;
                          end
                          else begin
 	                    _ptSecond.x := gi.ptsLocation.x;
 	                    _ptSecond.y := gi.ptsLocation.y;
 	                    _ptSecond   := ScreenToClient(_ptSecond);
                            ptZoomCenterX := (f_ptFirst.x + _ptSecond.x)/2;
                            ptZoomCenterY := (f_ptFirst.y + _ptSecond.y)/2;
	                    k := gi.ullArguments/f_dwArguments;
	                    ProcessGestureZoom(k,ptZoomCenterX,ptZoomCenterY);
 	                    f_ptFirst     := _ptSecond;
                            f_dwArguments := gi.ullArguments;
                          end;
                      end;
            GID_ROTATE: begin
                            if gi.dwFlags=GF_BEGIN then f_dwArguments := 0
                            else begin
                              f_ptFirst.x := gi.ptsLocation.x;
                              f_ptFirst.y := gi.ptsLocation.y;
                              f_ptFirst   := ScreenToClient(f_ptFirst);
                              angle := GID_ROTATE_ANGLE_FROM_ARGUMENT(LODWORD(gi.ullArguments))-GID_ROTATE_ANGLE_FROM_ARGUMENT(f_dwArguments);
                              ProcessGestureRotate( angle,f_ptFirst.x,f_ptFirst.y);  //angle in radians
                              f_dwArguments := LODWORD(gi.ullArguments);            //save previous
                            end;
                        end;
            GID_TWOFINGERTAP: ;   // 2 finger tap
            GID_PRESSANDTAP:  ;   // press n tap
          else bHandled:= False;
          end;
        end
        else begin //GetGestureInfo failed
          dwError := GetLastError;
          // Handle dwError ??
        end;

      CloseGestureInfoHandleFn(Msg.lParam);
    end;
  if bHandled then Msg.Result := 0
    else Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TMyRectangleDesigner.WMTabletFlick(var Msg: TMessage);
begin
  Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TMyRectangleDesigner.WMTouch(var Msg: TMessage);

  function TouchPointToPoint(const TouchPoint: TTouchInput): TPoint;
  begin
    Result := Classes.Point(TouchPoint.X div 100, TouchPoint.Y div 100);
    PhysicalToLogicalPointFn(Handle, Result);
    Result := ScreenToClient(Result);
  end ;

var
  TouchInputs: array of TTouchInput;
  counter: Integer;
  Handled: Boolean;
  Point: TPoint;
  R:TRect;
begin
  Handled := False;
  if not bTouchGestureAPIPresent then Exit;

  SetLength(TouchInputs, Msg.WParam);
  GetTouchInputInfoFn(Msg.LParam, Msg.WParam, @TouchInputs[0], SizeOf(TTouchInput));

  try
    for counter := 1 to Length (TouchInputs) do
    begin
      Point := TouchPointToPoint(TouchInputs [counter-1]);
      R := Classes.Rect(Point.X-5,Point.Y-5,Point.X+5,Point.Y+5);
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style   := psSolid;
      Canvas.Pen.Width   := 1;
      Canvas.Pen.Color   := clBlack;
      Canvas.Ellipse(R);
    end;
    Handled := True;
  finally
    if Handled then CloseTouchInputHandleFn(Msg.LParam)
      else inherited ;
  end;
end;

procedure TMyRectangleDesigner.ProcessGestureMove(dx, dy: integer);
begin
  if ((dx<>0) or (dy<>0)) and Assigned(fSelectedRect) then
    begin
      fSelectedRect.Left   := fSelectedRect.Left  + dx;
      fSelectedRect.Right  := fSelectedRect.Right + dx;
      fSelectedRect.Top    := fSelectedRect.Top   + dy;
      fSelectedRect.Bottom := fSelectedRect.Bottom+ dy;
      Invalidate;
    end;
end;

procedure TMyRectangleDesigner.ProcessGestureZoom(const k, ZoomCenterX,ZoomCenterY: Single);
var dx1,dx2,dy1,dy2:Single;
begin
  if (k<>1.0) and Assigned(fSelectedRect) then
    begin
      dx1 := (fSelectedRect.Left  - ZoomCenterX)*k;
      dx2 := (fSelectedRect.Right - ZoomCenterX)*k;
      dy1 := (fSelectedRect.Top   - ZoomCenterY)*k;
      dy2 := (fSelectedRect.Bottom- ZoomCenterY)*k;

      fSelectedRect.Left   := ZoomCenterX + dx1;
      fSelectedRect.Right  := ZoomCenterX + dx2;
      fSelectedRect.Top    := ZoomCenterY + dy1;
      fSelectedRect.Bottom := ZoomCenterY + dy2;

      Invalidate;
    end;
end;

procedure TMyRectangleDesigner.ProcessGestureRotate( const angle, RotateCenterX, RotateCenterY:Single ); //angle in radians

  Procedure _RotatePoint(var x,y:Single);
  var newX,newY,sinA,cosA:Single;
  begin
    sinA:=sin(angle);
    cosA:=cos(angle);
    newX := RotateCenterX + (x-RotateCenterX)*cosA - (y-RotateCenterY)*sinA;
    newY := RotateCenterY + (x-RotateCenterX)*sinA + (y-RotateCenterY)*cosA;
    x:=newX; y:=newY;
  end;

begin
  if (angle<>0) and Assigned(fSelectedRect) then
    begin
      _RotatePoint(fSelectedRect.Left,  fSelectedRect.Top   );      //????
      _RotatePoint(fSelectedRect.Right, fSelectedRect.Bottom);
      Invalidate;
    end;
end;

end.
