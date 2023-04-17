unit Param_CopyConverted;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, ParamType2, StdParamTypes, StdPermissions;

procedure CopyParamConverted(ASrc, ADest: IPParam);

implementation

procedure CopyParamConverted(ASrc, ADest: IPParam);
var
  AConverter: IPParam;
begin
  if ASrc.ID.&Type <> ADest.ID.&Type then begin
    //ADest.GetFrom(ParamTypeUtil[ADest.ID.&Type].Convert(ASrc))
  end else ADest.GetFrom(ASrc, TPNOLIMIT, NULLVISID);
  if ADest.ID.&Type = vColor
    then IPColor(ADest).Value:=$FFFF0000;
end;

end.

