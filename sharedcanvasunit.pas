unit SharedCanvasUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GR32, GL;

type
  TPictureContent = record
    Bitmap : TBitmap32;
    TexID  : GLUInt;
    Changed: Boolean;
  end;

  PPictureContent = ^TPictureContent;

implementation

end.

