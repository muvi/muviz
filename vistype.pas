unit VisType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, SpectrumData;

const
  vCall    = 0;
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
  MVIndex          = LongWord;

  vpCall           = packed record end;
  vpInt            = LongInt;
  vpReal           = Double;
  vpString         = ShortString;
  vpColor          = LongWord;
  vpBool           = Boolean;

  TVisParamType    = type Word;
  TVisOutputType   = type Word;

  CvFloat          = Single;
  CvColor          = vpColor;

  IMCanvas         = interface (IMInterface)
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

  IVisualisation   = interface (IMInterface)
    ['{9FD8D967-491E-477E-A00E-8D547D153A66}']
    function GetC1: vpColor; stdcall; deprecated;
    function GetC2: vpColor; stdcall; deprecated;
    function GetC3: vpColor; stdcall; deprecated;

    procedure SetOutput(const Index: LongWord; const Value); stdcall;

    property C1: vpColor read GetC1; deprecated;
    property C2: vpColor read GetC2; deprecated;
    property C3: vpColor read GetC3; deprecated;
  end;

  TVisProc         = procedure(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

  IVisualisation2  = interface (IVisualisation)
    ['{7FB21CAD-C54E-4C62-86EF-FF0F0F849954}']
    procedure ThreadPush(AProc: TVisProc; AFinished: TVisProc = nil); stdcall;
  end;

  IVisualisation3  = interface (IVisualisation)
    ['{9254DEEF-7A2D-4798-9017-5B0D7E5B9077}']
    procedure SetCanvas(Index: MVIndex; Canvas: IMCanvas); stdcall;
  end;

  //noch nicht implementiert!!!
  IVisualisation3_1= interface (IMInterface)
    ['{069B4014-CF30-43AC-B3A8-65E14B3273A9}']
    procedure SetOutput(const Index: LongWord; const Value); stdcall;
    procedure ThreadPush(AProc: TVisProc; AFinished: TVisProc = nil); stdcall;
    procedure SetCanvas(Index: MVIndex; Canvas: IMCanvas); stdcall;
  end;

const
  vpDefault: UInt64 = 0;

implementation

end.

