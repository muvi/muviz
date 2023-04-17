unit VisualisationParamOrder;

{$mode objfpc}{$H+}

interface

uses
  VisType2;

const
  GUIDVISPARAMORDER: TGUID = '{715A8D07-9E43-4DF5-8C5E-54127FE8091A}';

type
  IVisualisationParamOrder = interface
    ['{715A8D07-9E43-4DF5-8C5E-54127FE8091A}']
    function GetOrderInversePriority: LongInt; cdecl;
    procedure SetOrderInversePriority(APriority: LongInt); cdecl;
    procedure AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl;
    procedure RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;

    property OrderInversePriority: LongInt read GetOrderInversePriority write SetOrderInversePriority;
  end;

implementation

end.

