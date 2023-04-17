unit PictureType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType, SourceType, PluginType;

type
  MVFontName      = ShortString;
  MVFontStyle     = MVSet;

  IMPicture       = interface (IMSource)
    ['{1017C675-0AAD-4260-A7CF-E94AAE6A7917}']
    function GetCanvas: IMCanvas; stdcall;
    procedure LoadFromFile(FileName: ShortString); stdcall;
    procedure SaveToFile(FileName: ShortString); stdcall;

    property Canvas: IMCanvas read GetCanvas;
  end;

  IMPicture1_1    = interface (IMPicture)
    ['{589B78F3-194F-46CC-9460-43B27B93FAB3}']
    procedure SetSize(AWidth,AHeight: CvFloat); stdcall;
  end;

  IMCanvas2       = interface (IMCanvas)
    ['{A4CDABD1-0F04-4AB4-87C7-3CAC4AAAB356}']
    procedure Draw(X,Y: MVSInt; Pic: IMPicture); stdcall;
    procedure StretchDraw(X1,Y1,X2,Y2: MVSInt; Pic: IMPicture); stdcall;
    procedure RectDraw(const SrcRect,DestRect: MVRect; Pic: IMPicture); stdcall;
  end;

  IMSourcedCanvas = interface (IMCanvas2)
    ['{8C90AE89-730E-4E07-8489-8CFA1379111B}']
    function GetSource(ID: MVSourceID): IMSource; stdcall;
    procedure DeleteSource(AID: MVSourceID); stdcall;
  end;

  IMCanvas3       = interface (IMCanvas2)
    ['{7B2643E9-DE23-4AA5-990B-DAC45CE8B181}']
    //Schriftgröße in Punkt
    procedure TextOut(X,Y: CvFloat; S: ShortString; Font: MVFontName; Size: MVFloat; Style: MVFontStyle);
    //procedure TextOutD(X,Y: CvFloat; S: DString; Font: MVFontName; Size: MVFloat; Style: MVFontStyle);
    //Schriftgröße in Pixeln
    procedure TextOutP(X,Y: CvFloat; S: ShortString; Font: MVFontName; Size: MVFloat; Style: MVFontStyle);
    //... TextOutO mit Orientation... textouts mit Zeilenumbruch?!
  end;

const
  PicSrcType: MVSourceStr = 'PICTURE ';
  mvfsBold     = 1;
  mvfsItalic   = 2;
  mvfsUnderline= 4;
  mvfsStrikeOut= 8;

implementation

end.

