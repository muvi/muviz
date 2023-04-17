unit TVSPSizeConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2;

const
  TVSP1BYTE  : TPParamType = $2000000000000000;
  TVSP4BYTE  : TPParamType = $4000000000000000;
  TVSP8BYTE  : TPParamType = $6000000000000000;
  TVSP16BYTE : TPParamType = $8000000000000000;
  TVSPDYNAMIC: TPParamType = $A000000000000000;

implementation

end.

