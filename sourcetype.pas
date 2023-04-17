unit SourceType; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType;

type
  MVSourceStr   = array [0..7] of Char;
  MVSourceID    = Int64;

  IMSource      = interface (IMInterface)
    ['{EC8503E3-F1DF-4FCA-A23B-57FE66C7E095}']
    function GetSrcID: MVSourceID; stdcall;
    procedure SetSrcID(AID: MVSourceID); stdcall;
    function GetSrcFormat: MVSourceStr; stdcall;
    function GetSrcType: MVSourceStr; stdcall;
    //used for internal purpose only
    function IDP: Pointer; stdcall;

    property SrcID: MVSourceID read GetSrcID write SetSrcID;
    property SrcFormat: MVSourceStr read GetSrcFormat;
    property SrcType: MVSourceStr read GetSrcType;
  end;

  TNewSource    = function: IMSource; stdcall;

implementation

end.

