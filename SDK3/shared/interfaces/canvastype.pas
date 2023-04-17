unit CanvasType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, GL;

const
  cNoCanvas  : TGUID = '{00000000-0000-0000-0000-000000000000}';
  cGL2DCanvas: TGUID = '{8DF8372D-4922-4AE5-AE15-F238393D3C5C}';

type
  TGLCoord     = 0..3;
  TGLMatrix    = array [TGLCoord] of array [TGLCoord] of GLdouble;
  PGLMatrix    = ^TGLMatrix;

  IPGL2DCanvas = interface (IPCanvas)
    ['{8DF8372D-4922-4AE5-AE15-F238393D3C5C}']
    function GetWidth: Double; cdecl;
    function GetHeight: Double; cdecl;
    function GetLastScreenTransformationMatrix: PGLMatrix; cdecl;

    property Height: Double read GetHeight;
    property LastScreenTransformationMatrix: PGLMatrix read GetLastScreenTransformationMatrix;
    property Width: Double read GetWidth;
  end;

implementation

end.

