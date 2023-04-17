unit VisType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, SpectrumData;

const
  vUnknown = 0;
  vInteger = 1;
  vReal    = 2;
  vString  = 3;
  vColor   = 4;
  vBoolean = 5;
  vBuffer  = 6;

  oCall    = 0;
  oInteger = 1;
  oReal    = 2;
  oString  = 3;
  oColor   = 4;
  oBoolean = 5;
  oBuffer  = 6;

type
  MVIndex    = LongWord;

  vpInt          = LongInt;
  vpReal         = Double;
  vpString       = ShortString;
  vpColor        = LongWord;
  vpBool         = Boolean;

  TVisParamType  = type Word;
  TVisOutputType = type Word;

  CvFloat         = Single;
  CvColor         = vpColor;

  IMCanvas        = interface (IMInterface)
    ['{31DD04AA-3EC5-45D2-89E2-508D8DE7CC24}']
    function GetHeight: CvFloat; stdcall;
    function GetWidth: CvFloat; stdcall;

    procedure Line(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall;
    procedure FillRect(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall;
    procedure Vanish(const Color: CvColor); stdcall;
    procedure Rotate(const Phi: CvFloat; const Color: CvColor); stdcall;
    procedure Zoom(const X,Y: CvFloat; const Color: CvColor); stdcall;
    procedure Move(const X,Y: CvFloat); stdcall;
    procedure Clear(const Color: CvColor); stdcall;

    property Height: CvFloat read GetHeight;
    property Width: CvFloat read GetWidth;
  end;

  IVisualisation  = interface (IMInterface)
    ['{9FD8D967-491E-477E-A00E-8D547D153A66}']
    function GetC1: vpColor; stdcall; deprecated;
    function GetC2: vpColor; stdcall; deprecated;
    function GetC3: vpColor; stdcall; deprecated;

    procedure SetOutput(const Index: LongWord; const Value); stdcall;

    property C1: vpColor read GetC1; deprecated;
    property C2: vpColor read GetC2; deprecated;
    property C3: vpColor read GetC3; deprecated;
  end;

  TVisProc        = procedure(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

implementation

end.

