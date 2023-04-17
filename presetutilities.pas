unit PresetUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresetType, VisType2, ParamType2, MStrings, ParamSorter2,
  StdPermissions;

function InsertInput(AValue: IPParam; AName: IString; AVisualisation: IPVisualisation; AParamSorter: TVisualisationParamSorter): IPParam;
procedure RemoveInput(AInput: IPParam; AVisualisation: IPVisualisation);

implementation

function InsertInput(AValue: IPParam; AName: IString; AVisualisation: IPVisualisation; AParamSorter: TVisualisationParamSorter): IPParam;
begin
  Assert(AValue<>nil);
  Assert(AName<>nil);
  Assert(AVisualisation <> nil);
  Result:=AVisualisation.Inputs[ParamID(AName, AValue.ID.&Type)];
  Result.GetFrom(AValue, TPNOLIMIT, NULLVISID);
  //AParamSorter.SetIndex(Result, AIndex);
end;

procedure RemoveInput(AInput: IPParam; AVisualisation: IPVisualisation);
begin
  AInput.MakeDefault(TPNOLIMIT, NULLVISID);
end;

end.

