unit PresetUtil3_WiredMaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, PresetUtil3_ConnectedWireManager,
  ResizeBlockableMaster;

type
  TWiredMaster = class (TResizeBlockableMaster)
  strict protected
    function GetWireManager: TWireManager; virtual; abstract;
  public
    property WireManager: TWireManager read GetWireManager;
  end;

implementation

end.

