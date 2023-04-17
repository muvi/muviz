unit AdjustUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Buttons, ExtCtrls;

type

  { TAdjustForm }

  TAdjustForm = class(TForm)
    OKBtn: TBitBtn;
    PageControl: TPageControl;
    DividerPanel: TPanel;
    AudioTS: TTabSheet;
    GraphicsTS: TTabSheet;
    NetworkingTS: TTabSheet;
    HintsTS: TTabSheet;
    DebuggingTS: TTabSheet;
    OtherTS: TTabSheet;
  public
    function EditSettings(ASettings: Integer = -1): Boolean;
  end;

var
  AdjustForm: TAdjustForm;

const
  AUDIOSETTINGS   = 0;
  GRAPHICSETTINGS = 1;
  NETWORKSETTINGS = 2;
  HINTSETTINGS    = 3;
  OTHERSETTINGS   = 4;
  DEBUGSETTINGS   = 5;

implementation

function TAdjustForm.EditSettings(ASettings: Integer = -1): Boolean;
begin
  if ASettings>=0
    then PageControl.ActivePageIndex:=ASettings;
  ShowModal;
  Result:=true;
end;

initialization
  {$I adjustunit.lrs}

end.

