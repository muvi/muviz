unit ParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ParamPicWidth         = 20;
  ParamPicHeight        = 20;
  ParamPicValueStrLength= 800;

type
  TVisOutputData         = packed record
    D,V,P: Pointer;
  end;

  TVisOutputSet          = procedure (const Data: TVisOutputData; const Value); stdcall;
  TVisOutputGet          = procedure (const Data: TVisOutputData; out Value); stdcall;

  TParamPicPixel         = packed record
    S,V: ShortInt;
  end;

  TParamPic              = packed array [0..ParamPicWidth-1,0..ParamPicHeight-1] of TParamPicPixel;
  TParamName             = packed array [0..127] of Char;
  TParamPicValueStr      = array [0..ParamPicValueStrLength-1] of Char;

operator := (m: TParamPicValueStr) r: TParamPic;
operator := (m: string) r: TParamPic;

implementation

operator := (m: TParamPicValueStr) r: TParamPic;
begin
  Move(m,r,800);
end;

operator := (m: string) r: TParamPic;
var
  AStr: TParamPicValueStr;
begin
  AStr:=m;
  Move(AStr,r,800);
end;

end.

