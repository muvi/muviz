unit MExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EIncompatibleParamType = class (Exception)
  end;

  EImportException       = class (Exception)
  end;

  EInvalidListener                   = class (Exception)
  end;

  EInternalException     = class (Exception)
  protected
    function ProblemCodeToStr(AProblemCode: Cardinal): string; virtual;
    function GetProblemStr(AProblemCode: Cardinal; ALocationStr: string): string;
  public
    constructor Create(AProblemCode: Cardinal);
    constructor Create(AProblemCode: Cardinal; AMethod: TMethod);
    constructor Create(AProblemCode: Cardinal; ALocation: string);
  end;

const
  EIUNKNOWN                = 0;
  EIVISSETTWICE            = 1;
  EIPARAMETERTYPESNOTEQUAL = 2;
  EIWRONGTHREAD            = 3;
  EIILLEGALWIREEQUALITYMODE= 4;

  EIMAXPROBLEM             = 4;

const
  InternalProblemNames: array [0..EIMAXPROBLEM] of string = (
    'Unknown Problem',
    'The visualisation was set twice',
    'The parameter types are not equal',
    'Thread method called from an illegal thread',
    'Incorrect connection path'
    );

implementation

{%REGION EInternalException}

function EInternalException.ProblemCodeToStr(AProblemCode: Cardinal): string;
begin
  if AProblemCode <= EIMAXPROBLEM
    then Result:=InternalProblemNames[AProblemCode]
    else Result:='';
end;

function EInternalException.GetProblemStr(AProblemCode: Cardinal; ALocationStr: string): string;
var
  AProblemStr: string;
begin
  Result:='An internal problem occured';
  if ALocationStr<>''
    then Result+=' at ' + ALocationStr;
  AProblemStr:=ProblemCodeToStr(AProblemCode);
  if AProblemStr<>''
    then AProblemStr:=' (' + AProblemStr + ')';
  Result+='. Problem Code: ' + IntToStr(AProblemCode) + AProblemStr + '. This is a bug.';
end;

constructor EInternalException.Create(AProblemCode: Cardinal);
begin
  inherited Create(GetProblemStr(AProblemCode, ''));
end;

constructor EInternalException.Create(AProblemCode: Cardinal; AMethod: TMethod);
var
  AObject: TObject;
begin
  AObject:=TObject(AMethod.Data);
  inherited Create(GetProblemStr(AProblemCode, AObject.UnitName + '.' + AObject.ClassName + '.' + AObject.MethodName(AMethod.Code)));
end;

constructor EInternalException.Create(AProblemCode: Cardinal; ALocation: string);
begin
  inherited Create(GetProblemStr(AProblemCode, ALocation));
end;

{%ENDREGION}

end.

