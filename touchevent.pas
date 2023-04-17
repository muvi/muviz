unit TouchEvent;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, LMessages;

const
  WM_GESTURENOTIFY  = $011A;
  WM_GESTURE        = $0119;
  WM_TOUCH          = $0240;

  WM_TABLET_DEFBASE = $02C0;
  WM_TABLET_FLICK   = WM_TABLET_DEFBASE + 11;

  TOUCHEVENTF_MOVE                = $0001;
  TOUCHEVENTF_DOWN                = $0002;
  TOUCHEVENTF_UP                  = $0004;
  TOUCHEVENTF_INRANGE             = $0008;
  TOUCHEVENTF_PRIMARY             = $0010;
  TOUCHEVENTF_NOCOALESCE          = $0020;
  TOUCHEVENTF_PALM                = $0080;

  TOUCHINPUTMASKF_CONTACTAREA     = $0004;
  TOUCHINPUTMASKF_EXTRAINFO       = $0002;
  TOUCHINPUTMASKF_TIMERFROMSYSTEM = $0001;

type
  HTOUCHINPUT          = HANDLE;

  TLMTouch             = record
    Msg            : UINT;
    MsgFiller      : TDWordFiller;
    TouchPointCount: HALFPARAM;
    Reserved       : HALFPARAM;
    WParamFiller   : TDWordFiller;
    Handle         : HTOUCHINPUT;
    Result         : LRESULT;
  end;

  TOUCHINPUT           = packed record
    x          : LONG;
    y          : LONG;
    hSource    : HANDLE;
    dwID       : DWORD;
    dwFlags    : DWORD;
    dwMask     : DWORD;
    dwTime     : DWORD;
    dwExtraInfo: ULONG_PTR;
    cxContact  : DWORD;
    cyContact  : DWORD;
  end;

  TTouchInput          = TOUCHINPUT;

  TGestureNotifyStruct = record
    cbSize: UINT; // size, in bytes, of this structure
    dwFlags: DWORD; // unused
    hwndTarget: HWND; // handle to window targeted by the gesture
    ptsLocation: TSmallPoint; // starting location
    dwInstanceID: DWORD; // internally used
  end;

  PGestureNotifyStruct = ^TGestureNotifyStruct;

  TWMGestureNotify     = packed record
    Msg         : UINT;
    MsgFiller   : TDWordFiller;
    Unused      : WPARAM;
    NotifyStruct: PGestureNotifyStruct;
    Result      : LRESULT;
  end;

  PTOUCHINPUT          = ^TOUCHINPUT;

function CloseTouchInputHandle(hTouchInput: HTOUCHINPUT): BOOL; stdcall; external user32 name 'CloseTouchInputHandle';
function PhysicalToLogicalPoint(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall; external user32 name 'PhysicalToLogicalPoint';
function GetTouchInputInfo(hTouchInput: HTOUCHINPUT; cInputs: UINT; pInputs: PTOUCHINPUT; cbSize: Integer): BOOL; stdcall; external user32 name 'GetTouchInputInfo';
function RegisterTouchWindow(hwnd: HWND; ulFlags: Cardinal): BOOL; stdcall; external user32 name 'RegisterTouchWindow';
function UnregisterTouchWindow(hwnd: HWND): BOOL; stdcall; external user32 name 'UnregisterTouchWindow';

implementation

end.

