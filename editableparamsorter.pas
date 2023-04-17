unit EditableParamSorter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamSorter2, StdParamTypes, VisType2, UniqueStrings,
  MStrings;

type
  TEditableParamSorter = class (TVisualisationParamSorter)
  strict protected
    //makes shure that there is a gap between the previous
    function AssureConnectionSpace(AIndex: Integer): Integer;
    //creates a pointer to connect SortedBy and the new Param
    procedure ConnectWithPointer(AID: TPParamID; AIndex: Integer);
    //function AddParam(AID: TPParamID): IPParam;
  public
    function InsertParam(AID: TPParamID; AIndex: Integer): IPParam;
    {
    function InsertParam(AID: TPParamID): IPParam;
    procedure MoveParam(ACurrentIndex, ANewIndex: Integer);
    procedure DeleteParam(AIndex: Integer);
    }
  end;

implementation

{%REGION TEditableParamSorter}

function TEditableParamSorter.AssureConnectionSpace(AIndex: Integer): Integer;
var
  I             : Integer;
  AItem1, AItem2: TVisualisationParamSorterItem;
begin
  Assert(AIndex >= 1);
  Assert(AIndex <= ConnectedCount+1);

  if AIndex = 1 then begin
    Result:=Items[AIndex].Order - 1;
  end else begin
    Assert(AIndex > 1);

    AItem1:=Items[AIndex-1];
    Result:=AItem1.Order + 1;
    AItem2:=Items[AIndex];
    //look for items to change
    I:=AIndex;
    while AItem2.Order - AItem1.Order <= 1 do begin
      Assert(AItem2.Order - AItem1.Order >= 0);
      Inc(I);
      if I > ConnectedCount
        then break;
      AItem1:=AItem2;
      AItem2:=Items[I];
    end;
    //increment priority
    Dec(I);
    while I >= AIndex do begin
      Assert(Items[I].OrderIssuedBy <> nil);
      Assert(Items[I].OrderIssuedBy.Param.ID.&Type = TPParamType(vPreset));
      with IPPointer(Items[I].OrderIssuedBy.Param).Value
        do InversePriority:=InversePriority + 1;
      Dec(I);
    end;
  end;
end;

procedure TEditableParamSorter.ConnectWithPointer(AID: TPParamID; AIndex: Integer);
var
  APointer: IPParam;
  AValue  : TVPointer;
begin
  Assert(AIndex >= 1);
  Assert(AIndex <= ConnectedCount+1);
  Assert(SortedBy <> nil);

  APointer:=Visualisation[ParamID(GloballyUniqueEnglishText, vPointer)];

  with AValue do begin
    Output.Preset.Name:='';
    Output.Param:=SortedBy.ID;
    InversePriority:=AssureConnectionSpace(AIndex);
    Input.Preset.Name:='';
    Input.Param:=AID;
  end;

  IPPointer(APointer).&Set(AValue);
end;

function TEditableParamSorter.InsertParam(AID: TPParamID; AIndex: Integer): IPParam;
begin
  Result:=Visualisation[AID];
  if (SortedBy <> nil) and (AIndex >= 1) and (AIndex <= ConnectedCount+1)
    then ConnectWithPointer(AID, AIndex);
end;

{
function TEditableParamSorter.InsertParam(AID: TPParamID): IPParam;
procedure TEditableParamSorter.MoveParam(ACurrentIndex, ANewIndex: Integer);
procedure TEditableParamSorter.DeleteParam(AIndex: Integer);
}

{%ENDREGION}

end.

