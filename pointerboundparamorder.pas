unit PointerBoundParamOrder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisualisationParamOrder, StdParamTypes;

type
  IPointerBoundParamOrder = interface (IVisualisationParamOrder)
    function GetBoundTo: IPPointer;
    procedure SetBoundTo(AValue: IPPointer);
    property BoundTo: IPPointer read GetBoundTo write SetBoundTo;
  end;

implementation

end.

