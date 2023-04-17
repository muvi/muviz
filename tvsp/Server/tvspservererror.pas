unit TVSPServerError;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTVSPServerError      = (teNOTIMPLEMENTED, teUNKNOWN, teNOTYPE, teUNKNOWNTYPE, teWRONGSIZE,
    teDOUBLESUBSCRIPTION, teINVALIDINDEX, teINVALIDSOURCE, teINVALIDTYPE,
    teALREADYGRANTED, teNOTGRANTED, teINVALIDPARAM);

  ETVSPServerError      = class (Exception)
    constructor Create(AError: TTVSPServerError; AInfo: string = ''); reintroduce;
  end;

implementation

{%REGION ETVSPServerError}

constructor ETVSPServerError.Create(AError: TTVSPServerError; AInfo: string = '');
var
  AMsg: string;
begin
  //determine message
  case AError of
    teNOTIMPLEMENTED       : AMsg:='Feature not implemented: ' + AInfo;
    teNOTYPE, teUNKNOWNTYPE: AMsg:='Unknown message type.';
    teWRONGSIZE            : AMsg:='Invalid message size.';
    teDOUBLESUBSCRIPTION   : AMsg:='Already subscribed to group ' + AInfo;
    teINVALIDINDEX         : AMsg:='Index does not exist.';
    teINVALIDSOURCE        : AMsg:='Source not found';
    teINVALIDTYPE          : AMsg:='Invalid param type.';
    teALREADYGRANTED       : AMsg:='permission ' + AInfo + ' was already granted.';
    teNOTGRANTED           : AMsg:='permission ' + AInfo + ' was not granted.';
    else                     AMsg:='Unknown message error.';
  end;
  inherited Create(AMsg);
end;

{%ENDREGION}

end.

