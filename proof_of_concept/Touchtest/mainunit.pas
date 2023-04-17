unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, TouchControl,
  TouchEvent, LMessages, Windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    //procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  protected
    {
    procedure WMTOUCH(var Msg: TLMessage); message WM_TOUCH;
    procedure WMGestureNotify(var Msg: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMGesture(var Msg: TLMessage); message WM_GESTURE;
    procedure WMTabletFlick(var Msg: TLMessage); message WM_TABLET_FLICK;
    }
  private
    FTouchControl: TTouchControl;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTouchControl:=TTouchCOntrol.Create(Self);
  InsertControl(FTouchControl);
  with FTouchControl do begin
    Align:=alClient;
  end;
end;


{
procedure TForm1.WMGestureNotify(var Msg: TWMGestureNotify);
begin
  Msg.Result := DefWindowProc(Form1.Handle, Msg.Msg, Msg.Unused, Longint(Msg.NotifyStruct));
end;

procedure TForm1.WMGesture(var Msg: TLMessage);
begin
  Msg.Result := DefWindowProc(Form1.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TForm1.WMTabletFlick(var Msg: TLMessage);
begin
  Msg.Result := DefWindowProc(Form1.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TForm1.WMTOUCH(var Msg: TLMessage);
  function TouchPointToPoint(const TouchPoint: TTouchInput): TPoint;
  begin
    Result := Classes.Point(TouchPoint.X div 100, TouchPoint.Y div 100);
    PhysicalToLogicalPoint(Form1.Handle, Result);
  end;

var
  TouchInputs: array of TTouchInput;
  counter: Integer;
  Handled: Boolean;
  Point: TPoint;
begin
  Handled := False;
  SetLength(TouchInputs, Msg.WParam);
  GetTouchInputInfo(Msg.LParam, Msg.WParam, @TouchInputs[0], SizeOf(TTouchInput));

  try
    for counter := 1 to Length (TouchInputs) do begin
      Point := TouchPointToPoint(TouchInputs [counter-1]);
      // ...
    end;

    Handled := True;
  finally
    if Handled then
      CloseTouchInputHandle(Msg.LParam)
    else
      inherited;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterTouchWindow ( Form1.Handle, 0 );
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UnRegisterTouchWindow ( Form1.Handle );
end;
}

end.

