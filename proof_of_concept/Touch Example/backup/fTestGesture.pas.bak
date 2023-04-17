unit fTestGesture; // Test of gesture enabled Delphi 7 component
// fev/2013 by Omar Reis
// MyRectangleDesigner.pas implements a sample graphic component that
// allows u to manipulate rectangles. You can move, scale and rotate
// rectangles, using one or two fingers gestures. Requires Windows 7/8
// and a multi-touch PC.

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  D7GesturesHeader,
  Dialogs, StdCtrls,
  MyRectangleDesigner;

type
  TForm1 = class(TForm)
    cbTouchEnabled: TCheckBox;
    btnNextRect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure cbTouchEnabledClick(Sender: TObject);
    procedure btnNextRectClick(Sender: TObject);
  protected
  private
  public
    MyRectDesigner: TMyRectangleDesigner;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadTouchGestureAPI; //if any
  if bMultiTouchHardware then Caption:=Caption+'- T/G hardware present'
    else Caption:=Caption+'- no T/G hardware ';

  if bTouchGestureAPIPresent then
    begin //RegisterTouchWindowFn( Form1.Handle, 0 );
      Caption:=Caption+'- O/S support';
    end
    else Caption:=Caption+'- no OS support';

  //create
  MyRectDesigner := TMyRectangleDesigner.Create(nil);
  MyRectDesigner.Top    := 300;
  MyRectDesigner.Left   := 300;
  MyRectDesigner.Width  := 600;
  MyRectDesigner.Height := 500;

  MyRectDesigner.Parent:=Self;
  MyRectDesigner.Align := alClient;
  MyRectDesigner.SendToBack;

  EnableAllGestures(MyRectDesigner.Handle);   //all gestures  (rotation etc)
end;

procedure TForm1.cbTouchEnabledClick(Sender: TObject);
begin
  if bTouchGestureAPIPresent then
    begin
      if cbTouchEnabled.Checked then RegisterTouchWindowFn( MyRectDesigner.Handle, 0 )
        else UnRegisterTouchWindowFn ( MyRectDesigner.Handle );
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if cbTouchEnabled.Checked and bTouchGestureAPIPresent then
    UnRegisterTouchWindowFn ( Form1.Handle );
end;

procedure TForm1.btnNextRectClick(Sender: TObject);
begin
  MyRectDesigner.SelectNext; //select next rectangle
end;

end.
