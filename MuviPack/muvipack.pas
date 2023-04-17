{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit MuviPack; 

interface

uses
    VisTypeUnit, TriggerEditor, VisDrawType, VisDrawUnit, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('TriggerEditor', @TriggerEditor.Register); 
end; 

initialization
  RegisterPackage('MuviPack', @Register); 
end.
