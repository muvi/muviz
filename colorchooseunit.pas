unit ColorChooseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ChooseColor, AdvFunc, AdvGui, GraphX32,
  CanvasGraphX32;

type
  TDlgSuccess      = (dlgNone,dlgTrue,dlgFalse);
  TDlgExecutedEvent= procedure (Success: Boolean) of object;

  { TChooseColorForm }

  TChooseColorForm = class(TForm)
    AlphaBar: TAlphaBar;
    ColorBevel: TBevel;
    ColorPB: TBufferedPaintBox;
    CancelBtn: TButton;
    GridColorChooser: TGridColorChooser;
    ExpandImageList: TImageList;
    Label1: TLabel;
    OKBtn: TButton;
    AlphaEdit: TEdit;
    ExpandBtn: TBitBtn;
    ColorNameEdit: TEdit;
    HSVColorChooser: THSVColorChooser;
    procedure AlphaBarChange(Sender: TObject);
    procedure AlphaEditChange(Sender: TObject);
    procedure AlphaEditExit(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ExpandBtnClick(Sender: TObject);
    procedure ColorNameEditChange(Sender: TObject);
    procedure ColorNameEditExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridColorChooserChanged(Sender: TObject);
    procedure HSVColorChooserChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    procedure ShowColor;
    procedure ShowSuggestions;
    procedure DoClose; inline;
  public
    FLastColors    : array [0..7] of TColor32;
    FFirstLastColor: Byte;
    FAutoHide      : Boolean;
    FOnExecuted    : TDlgExecutedEvent;
    DlgSucceeded   : TDlgSuccess;
    procedure AddLastColor(AColor: TColor32);
  end;

implementation

{ TChooseColorForm }

procedure TChooseColorForm.HSVColorChooserChange(Sender: TObject);
begin
  AlphaBar.SelColor32:=HSVColorChooser.SelColor32;
  GridColorChooser.SelColor32:=HSVColorChooser.SelColor32;
  ShowColor;
end;

procedure TChooseColorForm.OKBtnClick(Sender: TObject);
begin
  if FAutoHide then begin
    Hide;
    DoClose;
    DlgSucceeded:=dlgTrue;
    if Assigned(FOnExecuted)
      then FOnExecuted(true);
  end;
end;

procedure TChooseColorForm.AlphaBarChange(Sender: TObject);
begin
  HSVColorChooser.SelColor32:=AlphaBar.SelColor32;
  GridColorChooser.SelColor32:=AlphaBar.SelColor32;
  ShowColor;
end;

procedure TChooseColorForm.AlphaEditChange(Sender: TObject);
var
  AAlphaVal: Int64;
begin
  AAlphaVal:=StrToIntB(AlphaEdit.Text,16);
  if AAlphaVal>=0 then begin
    AlphaBar.SelAlpha:=AAlphaVal;
    HSVColorChooser.SelColor32:=AlphaBar.SelColor32;
    GridColorChooser.SelColor32:=AlphaBar.SelColor32;
    ShowColor;
  end;
end;

procedure TChooseColorForm.AlphaEditExit(Sender: TObject);
begin
  AlphaEdit.OnChange:=nil;
  AlphaEdit.Text:=IntToHex(AlphaBar.SelAlpha,2);
  AlphaEdit.OnChange:=@AlphaEditChange;
end;

procedure TChooseColorForm.CancelBtnClick(Sender: TObject);
begin
  if FAutoHide then begin
    Hide;
    DoClose;
    DlgSucceeded:=dlgFalse;
    if Assigned(FOnExecuted)
      then FOnExecuted(false);
  end;
end;

procedure TChooseColorForm.ExpandBtnClick(Sender: TObject);
begin
  if Tag=0 then begin
    HSVColorChooser.Size:=1.0;
    Width:=Width+96;
    Height:=Height+96;
    ExpandImageList.GetBitmap(1,ExpandBtn.Glyph);
    GridColorChooser.Count:=40;
    Tag:=1;
  end else begin
    HSVColorChooser.Size:=0.7;
    Width:=Width-96;
    Height:=Height-96;
    ExpandImageList.GetBitmap(0,ExpandBtn.Glyph);
    GridColorChooser.Count:=20;
    Tag:=0;
  end;
  OKBtn.SetFocus;
  ShowSuggestions;
end;

procedure TChooseColorForm.ColorNameEditChange(Sender: TObject);
var
  AColorVal: Int64;
begin
  AColorVal:=StrToIntB(ColorNameEdit.Text,16);
  if AColorVal>=0 then begin
    AlphaBar.SelColor32:=(AlphaBar.SelColor32 and $FF000000) or AColorVal;
    HSVColorChooser.SelColor32:=AlphaBar.SelColor32;
    GridColorChooser.SelColor32:=AlphaBar.SelColor32;
    ShowColor;
  end;
end;

procedure TChooseColorForm.ColorNameEditExit(Sender: TObject);
begin
  ColorNameEdit.OnChange:=nil;
  ColorNameEdit.Text:=IntToHex(AlphaBar.SelColor32 and $FFFFFF,6);
  ColorNameEdit.OnChange:=@ColorNameEditChange;
end;

procedure TChooseColorForm.DoClose; inline;
begin
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
end;

procedure TChooseColorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  DoClose;
  DlgSucceeded:=dlgFalse;
  if Assigned(FOnExecuted)
    then FOnExecuted(false);
end;

procedure TChooseColorForm.FormDeactivate(Sender: TObject);
begin
  if (DlgSucceeded=dlgNone) and FAutoHide then begin
    Hide;
    DoClose;
    DlgSucceeded:=dlgTrue;
    if Assigned(FOnExecuted)
      then FOnExecuted(true);
  end;
end;

procedure TChooseColorForm.FormShow(Sender: TObject);
begin
  Position:=poDesigned;
  Application.AddOnDeactivateHandler(@FormDeactivate);
end;

procedure TChooseColorForm.GridColorChooserChanged(Sender: TObject);
begin
  AlphaBar.SelColor32:=GridColorChooser.SelColor32;
  HSVColorChooser.SelColor32:=GridColorChooser.SelColor32;
  ShowColor;
end;

procedure TChooseColorForm.AddLastColor(AColor: TColor32);
var
  I: Integer;
begin
  for I:=0 to 7 do if FLastColors[I]=AColor then exit;
  FFirstLastColor:=(FFirstLastColor+1) mod 8;
  FLastColors[FFirstLastColor]:=AColor;
end;

procedure TChooseColorForm.ShowSuggestions;
const
  HC3 = (2*pi)/3;
  HC6 = pi/3;
  HC12= pi/6;
var
  AHSV: TAHSV;

  procedure _ShowDefaultSuggestions(HSugg,SVSugg,LastSugg: Integer);
  begin
    AHSV:=HSVColorChooser.SelHSV;
    with AHSV do H:=PhiCut((Round(H/HC12)-1)*HC12);
    GridColorChooser.Colors32[HSugg]:=AHSVToARGB(AHSV);
    with AHSV do H:=PhiCut(H+HC12);
    GridColorChooser.Colors32[HSugg+1]:=AHSVToARGB(AHSV);
    with AHSV do H:=PhiCut(H+HC12);
    GridColorChooser.Colors32[HSugg+2]:=AHSVToARGB(AHSV);
    AHSV:=HSVColorChooser.SelHSV;
    with AHSV do begin
      S:=$7F;
      V:=$FF;
    end;
    GridColorChooser.Colors32[SVSugg]:=AHSVToARGB(AHSV);
    with AHSV do begin
      S:=$FF;
      V:=$7F;
    end;
    GridColorChooser.Colors32[SVSugg+1]:=AHSVToARGB(AHSV);
    with AHSV do begin
      S:=$7F;
      V:=$7F;
    end;
    GridColorChooser.Colors32[SVSugg+2]:=AHSVToARGB(AHSV);
    with AHSV do begin
      S:=$FF;
      V:=$FF;
    end;
    GridColorChooser.Colors32[SVSugg+3]:=AHSVToARGB(AHSV);
    GridColorChooser.Colors32[LastSugg]:=FLastColors[FFirstLastColor];
    GridColorChooser.Colors32[LastSugg+1]:=FLastColors[(FFirstLastColor+7) mod 8];
    GridColorChooser.Colors32[LastSugg+2]:=FLastColors[(FFirstLastColor+6) mod 8];
    GridColorChooser.Colors32[LastSugg+3]:=FLastColors[(FFirstLastColor+5) mod 8];
  end;

begin
  GridColorChooser.StartUpdate;
  if Tag=0 then begin
    _ShowDefaultSuggestions(9,12,16);
    GridColorChooser.Colors32[0]:=$00FF0000 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[1]:=$00FFFF00 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[2]:=$0000FF00 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[3]:=$0000FFFF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[4]:=$000000FF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[5]:=$00FF00FF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[6]:=$00000000 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[7]:=$007F7F7F or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[8]:=$00FFFFFF or (GridColorChooser.SelColor32 and $FF000000);
  end else begin
    _ShowDefaultSuggestions(17,20,32);
    GridColorChooser.Colors32[0]:=$00FF0000 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[1]:=$00FF7F00 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[2]:=$00FFFF00 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[3]:=$007FFF00 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[4]:=$0000FF00 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[5]:=$0000FF7F or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[6]:=$0000FFFF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[7]:=$00007FFF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[8]:=$000000FF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[9]:=$007F00FF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[10]:=$00FF00FF or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[11]:=$00FF007F or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[12]:=$00000000 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[13]:=$003F3F3F or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[14]:=$007F7F7F or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[15]:=$00C0C0C0 or (GridColorChooser.SelColor32 and $FF000000);
    GridColorChooser.Colors32[16]:=$00FFFFFF or (GridColorChooser.SelColor32 and $FF000000);

    with AHSV do begin
      S:=$3F;
      V:=$3F;
    end;
    GridColorChooser.Colors32[24]:=AHSVToARGB(AHSV);
    with AHSV do begin
      S:=$C0;
      V:=$C0;
    end;
    GridColorChooser.Colors32[25]:=AHSVToARGB(AHSV);
    with AHSV do begin
      S:=$3F;
      V:=$C0;
    end;
    GridColorChooser.Colors32[26]:=AHSVToARGB(AHSV);

    GridColorChooser.Colors32[27]:=(GridColorChooser.SelColor32 and $00FFFFFF);
    GridColorChooser.Colors32[28]:=(GridColorChooser.SelColor32 and $00FFFFFF) or $3F000000;
    GridColorChooser.Colors32[29]:=(GridColorChooser.SelColor32 and $00FFFFFF) or $7F000000;
    GridColorChooser.Colors32[30]:=(GridColorChooser.SelColor32 and $00FFFFFF) or $C0000000;
    GridColorChooser.Colors32[31]:=(GridColorChooser.SelColor32 and $00FFFFFF) or $FF000000;

    GridColorChooser.Colors32[36]:=FLastColors[(FFirstLastColor+4) mod 8];
    GridColorChooser.Colors32[37]:=FLastColors[(FFirstLastColor+3) mod 8];
    GridColorChooser.Colors32[38]:=FLastColors[(FFirstLastColor+2) mod 8];
    GridColorChooser.Colors32[39]:=FLastColors[(FFirstLastColor+1) mod 8];
  end;
  GridColorChooser.EndUpdate;
end;

procedure TChooseColorForm.ShowColor;
var
  AHSV: TAHSV;
begin
  if not ColorNameEdit.Focused then ColorNameEditExit(nil);
  if not AlphaEdit.Focused then AlphaEditExit(nil);
  AHSV:=HSVColorChooser.SelHSV;
  //Label1.Caption:='A='+IntToStr(AHSV.A)+'; H='+FloatToStrF((AHSV.H/pi)*180.0,ffFixed,7,2)+'°; S='+IntToStr(AHSV.S)+'; V='+IntToStr(AHSV.V)+';';
  with ColorPB.Buffer
    do DrawAlphaColorArea(Canvas,Rect(0,0,Width,Height),AlphaBar.SelColor32,clWhite,clBlack,12,12);
  ColorPB.Repaint;
  ShowSuggestions;
end;

initialization
  {$I colorchooseunit.lrs}

end.

