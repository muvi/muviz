unit ColorChooseDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ColorChooseUnit, GraphX32, Graphics, Controls, Forms;

type
  TExecutedEvent     = procedure (Sender: TObject; Success: Boolean) of object;

  TChooseColorDialog = class (TComponent)
  private
    FDlg       : TChooseColorForm;
    FOnExecuted: TExecutedEvent;
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetColor32: TColor32;
    procedure SetColor32(Value: TColor32);
    function GetAlpha: Byte;
    procedure SetAlpha(Value: Byte);
    function GetLastColor32(Index: Byte): TColor32;
    function GetLastColor(Index: Byte): TColor;
    function GetLastAlpha(Index: Byte): Byte;
    function GetAutoHide: Boolean;
    procedure SetAutoHide(Value: Boolean);
  protected
    procedure DlgExecuted(Success: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure ExecuteNonBlocking;
    property Color32: TColor32 read GetColor32 write SetColor32;
    property LastAlphas[Index: Byte]: Byte read GetLastAlpha;
    property LastColors[Index: Byte]: TColor read GetLastColor;
    property LastColors32[Index: Byte]: TColor32 read GetLastColor32;
  published
    property Alpha: Byte read GetAlpha write SetAlpha;
    property AutoHide: Boolean read GetAutoHide write SetAutoHide;
    property Color: TColor read GetColor write SetColor;
    property OnExecuted: TExecutedEvent read FOnExecuted write FOnExecuted;
  end;

implementation

{%REGION TChooseColorDialog}

constructor TChooseColorDialog.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  Application.CreateForm(TChooseColorForm,FDlg);
  with FDlg do begin
    for I:=0 to 7 do FLastColors[I]:=$00000000;
    FFirstLastColor:=7;
    FAutoHide:=false;
    FOnExecuted:=@DlgExecuted;
  end;
end;

destructor TChooseColorDialog.Destroy;
begin
  inherited Destroy;
end;

function TChooseColorDialog.Execute: Boolean;
begin
  with FDlg do begin
    if FAutoHide then begin
      DlgSucceeded:=dlgNone;
      Show;
      while DlgSucceeded=dlgNone do Application.ProcessMessages;
      Result:=(DlgSucceeded=dlgTrue);
    end else Result:=(ShowModal=mrOK);
  end;
end;

procedure TChooseColorDialog.ExecuteNonBlocking;
begin
  with FDlg do begin
    DlgSucceeded:=dlgNone;
    Show;
  end;
end;

procedure TChooseColorDialog.DlgExecuted(Success: Boolean);
begin
  if Success
    then with FDlg do AddLastColor(AlphaBar.SelColor32);
  if Assigned(FOnExecuted)
    then FOnExecuted(Self, Success);
end;

function TChooseColorDialog.GetColor: TColor;
begin
  Result:=FDlg.HSVColorChooser.SelColor;
end;

procedure TChooseColorDialog.SetColor(Value: TColor);
begin
  FDlg.AlphaBar.SelColor:=Value;
  FDlg.AlphaBarChange(nil);
end;

function TChooseColorDialog.GetColor32: TColor32;
begin
  Result:=FDlg.HSVColorChooser.SelColor32;
end;

procedure TChooseColorDialog.SetColor32(Value: TColor32);
begin
  FDlg.AlphaBar.SelColor32:=Value;
  FDlg.AlphaBarChange(nil);
end;

function TChooseColorDialog.GetAlpha: Byte;
begin
  Result:=FDlg.AlphaBar.SelAlpha;
end;

procedure TChooseColorDialog.SetAlpha(Value: Byte);
begin
  FDlg.AlphaBar.SelAlpha:=Value;
  FDlg.AlphaBarChange(nil);
end;

function TChooseColorDialog.GetLastColor32(Index: Byte): TColor32;
begin
  if Index<=7
    then Result:=FDlg.FLastColors[(FDlg.FFirstLastColor+8-Index) mod 8]
    else Result:=$00000000;
end;

function TChooseColorDialog.GetLastColor(Index: Byte): TColor;
begin
  if Index<=7
    then Result:=Color32ToColor(FDlg.FLastColors[(FDlg.FFirstLastColor+8-Index) mod 8])
    else Result:=$000000;
end;

function TChooseColorDialog.GetLastAlpha(Index: Byte): Byte;
begin
  if Index<=7
    then Result:=AlphaComponent(FDlg.FLastColors[(FDlg.FFirstLastColor+8-Index) mod 8])
    else Result:=$00;
end;

function TChooseColorDialog.GetAutoHide: Boolean;
begin
  Result:=FDlg.FAutoHide;
end;

procedure TChooseColorDialog.SetAutoHide(Value: Boolean);
begin
  with FDlg do begin
    FAutoHide:=Value;
    if Value
      then OnDeactivate:=@FormDeactivate
      else OnDeactivate:=nil;
  end;
end;

{%ENDREGION}

end.

