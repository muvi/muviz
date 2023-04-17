unit ArtNetConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


const
  UDPBUFFERSIZE         = 65535;

  PORTDIRECTEDBROADCAST = $1936;

  ARTNETID      : array [0..7] of Char = 'Art-Net'+#0;
  ARTNETPROTVER : Word = 14;

  talkArtPollReplyOnConditionChange = $2;
  talkDiagnostics                   = $4;
  talkDiagnosticsUnicast            = $8;

  OpPoll             = $2000;
  OpPollReply        = $2100;
  OpDiagData         = $2300;
  OpCommand          = $2400;
  OpOutput           = $5000;
  OpDMX              = opOutput;
  OpNzs              = $5100;
  OpSync             = $5200;
  OpAddress          = $6000;
  OpInput            = $7000;
  OpTodRequest       = $8000;
  OpTodData          = $8100;
  OpTodControl       = $8200;
  OpRdm              = $8300;
  OpRdmSub           = $8400;
  OpVideoSetup       = $A010;
  OpVideoPalette     = $A020;
  OpVideoData        = $A040;
  OpMacMaster        = $F000;
  OpMacSlave         = $F100;
  OpFirmwareMaster   = $F200;
  OpFirmwareReply    = $F300;
  OpFileTnMaster     = $F400;
  OpFileFnMaster     = $F500;
  OpFileFnReply      = $F600;
  OpIpProg           = $F800;
  OpIpProgReply      = $F900;
  OpMedia            = $9000;
  OpMediaPatch       = $9100;
  OpMediaControl     = $9200;
  OpMediaContrlReply = $9300;
  OpTimeCode         = $9700;
  OpTimeSync         = $9800;
  OpTrigger          = $9900;
  OpDirectory        = $9A00;
  OpDirectoryReply   = $9B00;

  StNode             = $00;
  StController       = $01;
  StMedia            = $02;
  StRoute            = $03;
  StBackup           = $04;
  StConfig           = $05;
  StVisual           = $06;

implementation

end.

