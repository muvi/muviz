unit DebugTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  ENotImplementedHelper = class helper for ENotImplemented
  public
    constructor Create(AMethod: TMethod);
    constructor Create(AMethod: string; AUnit: string);
  end;

implementation

{%REGION ENotImplementedHelper}

constructor ENotImplementedHelper.Create(AMethod: TMethod);
var
  AObject   : TObject;
begin
  AObject:=TObject(AMethod.Data);
  inherited Create('Method "' + AObject.ClassName + '.' + AObject.MethodName(AMethod.Code) + '" in Unit "' + AObject.UnitName + '" not implemented.');
end;

constructor ENotImplementedHelper.Create(AMethod: string; AUnit: string);
begin
  inherited Create('Method "' + AMethod + '" in Unit "' + AUnit + '" not implemented.');
end;

{%ENDREGION}

end.

