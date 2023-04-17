unit AdvType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLoadFileResult   = type Byte;

const
  lplOK               = 0;
  lplFailed           = 100;
  lplFOKBOE           = 200; //Format OK But Other Error
  lplUnknown          = 255;

  lplUnknownExtension = 101;
  lplInvalidHeader    = 104;
  lplFileNotFound     = 105;

  lplAlreadyOpen      = 201;
  lplNotSupported     = 202;

function lplSucceeded(AResult: TLoadFileResult): Boolean;
function lplIsFokboe(AResult: TLoadFileResult): Boolean;
function lplFormatError(AResult: TLoadFileResult): Boolean;

implementation

{Allgemein}

function lplSucceeded(AResult: TLoadFileResult): Boolean;
begin
  Result:=(AResult<lplFailed);
end;

function lplIsFokboe(AResult: TLoadFileResult): Boolean;
begin
  Result:=(AResult>=lplFOKBOE);
end;

function lplFormatError(AResult: TLoadFileResult): Boolean;
begin
  Result:=((AResult>=lplFailed) and (AResult<lplFOKBOE));
end;

end.

