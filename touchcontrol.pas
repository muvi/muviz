unit TouchControl;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  TouchEvent, Windows,
  {$ENDIF}
  Classes, SysUtils, LMessages, TouchControlBase, Controls;

type
  {$IFDEF WINDOWS}
  TTouchControl = class (TBasicTouchControl)
  private
    FPrevWndProc: WNDPROC;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    function WMGestureNotify(wParam: WPARAM; lParam: LPARAM): LRESULT;
    function WMGesture(wParam: WPARAM; lParam: LPARAM): LRESULT;
    function WMTouch(wParam: WPARAM; lParam: LPARAM): LRESULT;
    function WMTabletFlick(wParam: WPARAM; lParam: LPARAM): LRESULT;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;
  {$ELSE}
  //don't handle any touching
  TTouchControl = TBasicTouchControl;
  {$ENDIF}

implementation

{%REGION TTouchControl}
{$IFDEF WINDOWS}

function WndCallback(AHwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  //only Touch controls use this callback...
  with TTouchControl(FindControl(AHwnd)) do begin
    case uMsg of
      WM_GESTURENOTIFY: Result:=WMGestureNotify(wParam, lParam);
      WM_GESTURE      : Result:=WMGesture(wParam, lParam);
      WM_TOUCH        : Result:=WMTouch(wParam, lParam);
      WM_TABLET_FLICK : Result:=WMTabletFlick(wParam, lParam);
      else Result:=CallWindowProc(FPrevWndProc, AHwnd, uMsg, WParam, LParam);
    end;
  end;
end;

constructor TTouchControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TTouchControl.Destroy;
begin
  inherited Destroy;
end;

procedure TTouchControl.CreateHandle;
begin
  inherited CreateHandle;
  //hook the wnd proc to receive custom events
  FPrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(@WndCallback)));
  //RegisterTouchWindow(Handle, 0);
end;

procedure TTouchControl.DestroyHandle;
begin
  //UnRegisterTouchWindow(Handle);
  //set the original wnd proc
  FPrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(FPrevWndProc)));
  inherited DestroyHandle;
end;

function TTouchControl.WMGestureNotify(wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result:=DefWindowProc(Handle, WM_GESTURENOTIFY, wParam, lParam);
end;

function TTouchControl.WMGesture(wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result:=DefWindowProc(Handle, WM_GESTURE, wParam, lParam);
end;

type
  TTouchEvent     = procedure (APoint: TTouchPoint; var AHandled: Boolean) of object;
  TTouchEventInfo = record
    ATouchPoint: TTouchPoint;
    ADestroy   : Boolean;
    AMethod    : TTouchEvent;
  end;

function TTouchControl.WMTouch(wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  AHandled                    : Boolean;
  ATouchPointCount, I         : Integer;
  ATouchPoints, ATouchPointMem: PTOUCHINPUT;
  AEventInfo                  : array of TTouchEventInfo;

  function DoAdd: TTouchPoint;
  var
    ASize: TPoint;
  begin
    with ATouchPoints^ do begin
      if Boolean(TOUCHINPUTMASKF_CONTACTAREA and dwMask)
        then ASize:=Point(cxContact, cyContact)
        else ASize:=Point(-1, -1);
      Result:=AddOrUpdateTouchPoint(dwID,
        Point(x, y),
        Boolean(dwFlags and TOUCHEVENTF_PALM),
        ASize,
        hSource,
        Boolean(dwFlags and TOUCHEVENTF_PRIMARY));
    end;
  end;

begin
  AHandled:=false;

  ATouchPointCount:=LOWORD(wParam);
  if ATouchPointCount > 0 then begin
    ATouchPointMem:=GetMem(ATouchPointCount * SizeOf(TOUCHINPUT));
    if not GetTouchInputInfo(lParam, ATouchPointCount, ATouchPointMem, SizeOf(TOUCHINPUT))
      then raise ETouch.Create('Could not get touch input info.');

    //get touch in info
    ATouchPoints:=ATouchPointMem;
    SetLength(AEventInfo, ATouchPointCount);
    for I:=0 to ATouchPointCount-1 do begin
      with ATouchPoints^ do with AEventInfo[I] do begin
        if Boolean(dwFlags and TOUCHEVENTF_UP) then begin
          ATouchPoint:=RemoveTouchPoint(dwID);
          ADestroy:=true;
          AMethod:=@TouchUp;
        end else if Boolean(dwFlags and TOUCHEVENTF_MOVE) then begin
          ATouchPoint:=DoAdd;
          ADestroy:=false;
          AMethod:=@TouchMove;
        end else if Boolean(dwFlags and TOUCHEVENTF_DOWN) then begin
          ATouchPoint:=DoAdd;
          ADestroy:=false;
          AMethod:=@TouchDown;
        end else begin
          ATouchPoint:=DoAdd;
          ADestroy:=false;
          AMethod:=nil;
        end;
      end;
      Inc(ATouchPoints);
    end;
    //do not free ATouchPoints here, because it is incremented...
    FreeMem(ATouchPointMem);
  end;

  //call events
  for I:=0 to ATouchPointCount-1 do with AEventInfo[I] do begin
    if Assigned(AMethod)
      then AMethod(ATouchPoint, AHandled);
    if ADestroy
      then ATouchPoint.Destroy;
  end;
  SetLength(AEventInfo, 0);

  if AHandled then begin
    if not CloseTouchInputHandle(lParam)
      then raise ETouch.Create('Could not close the touch input handle');
    Result:=S_OK;
  end else Result:=CallWindowProc(FPrevWndProc, Handle, WM_TOUCH, WParam, LParam);
                 //DefWindowProc(Handle, WM_TOUCH, wParam, lParam);
end;

function TTouchControl.WMTabletFlick(wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result:=DefWindowProc(Handle, WM_TABLET_FLICK, wParam, lParam);
end;

{$ENDIF}
{%ENDREGION}

end.

