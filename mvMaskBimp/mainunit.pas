unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, ChooseColor, GraphX32;

type

  { TMainForm }

  TMainForm = class(TForm)
    LoadBtn: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    SaveBtn: TButton;
    ModifyBtn: TButton;
    Image: TImage;
    SavePictureDialog: TSavePictureDialog;
    procedure LoadBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  TPlugModifierPixel     = packed record
    //Gray, Alpha, Result Alpha (=Deckkraft des Ergebnisbildes)
    G,A,RA,X: Byte;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadBtnClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute
    then Image.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
end;

procedure TMainForm.ModifyBtnClick(Sender: TObject);
var
  I,J              : Integer;
  AColor           : TColor;
  AColor2          : TColor;
  ARGB             : TXBGR absolute AColor;
  AMod             : TPlugModifierPixel absolute AColor2;
  ATransparentColor: TColor;
  GB               : Byte;
begin
  with Image.Picture.Bitmap do begin
    ATransparentColor:=Canvas.Pixels[0,0];
    for I:=0 to Width-1 do for J:=0 to Height-1 do begin
      AColor:=Canvas.Pixels[I,J];
      if AColor<>ATransparentColor then begin
        GB:=(ARGB.G+ARGB.B) div 2;
        AMod.X:=0;
        AMod.RA:=$FF;
        AMod.A:=ARGB.R-GB;
        if AMod.A<$FF
          then AMod.G:=Round((GB*$FF)/($FF-AMod.A))
          else AMod.G:=0;
        Canvas.Pixels[I,J]:=AColor2;
      end else Canvas.Pixels[I,J]:=$00000000;
    end;
  end;
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
begin
  if SavePictureDialog.Execute
    then Image.Picture.Bitmap.SaveToFile(SavePictureDialog.FileName);
end;

end.

