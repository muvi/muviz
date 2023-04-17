unit ParamType2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, MTypes, PluginType, MStrings;

const
  ParamPicWidth       = 20;
  ParamPicHeight      = 20;

type
  TPParamPicPixel     = packed record
    S,V: ShortInt;
  end;

  TPParamPic          = packed array [0..ParamPicWidth-1,0..ParamPicHeight-1] of TPParamPicPixel;
  TPParamConverter    = function (ADest: IPParam): IPParam; cdecl;

  IPParamType         = interface (IMInterface)
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
    function Convert(ADest: IPParam): IPParam; cdecl;
    function GetColor: TMColor; cdecl;
    function GetName: IString; cdecl;
    function GetPicture: TPParamPic; cdecl;
    function GetType: TPParamType; cdecl;
    //not implemented yet
    procedure AddConverter(AConvertFrom: TPParamType; AConverter: TPParamConverter); cdecl;

    property Color: TMColor read GetColor;
    property Name: IString read GetName;
    property Picture: TPParamPic read GetPicture;
    property &Type: TPParamType read GetType;
  end;

  IPParamTypeIterator = interface
    function GetCurrent: IPParamType; cdecl;
    function MoveNext: Boolean; cdecl;
  end;

  IPParamTypeUtil     = interface
    function GetType(ATypeID: TPParamType): IPParamType; cdecl;
    function AddType(AUtil: IPParamType): Boolean; cdecl;
    function GetIterator: IPParamTypeIterator; cdecl;

    property Types[ATypeID: TPParamType]: IPParamType read GetType; default;
    property Iterator: IPParamTypeIterator read GetIterator;
  end;

var
  ParamTypeUtil: IPParamTypeUtil = nil;

implementation

end.

