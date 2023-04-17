program TVSPTestClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetvisual, VisType2, StdParamTypes, TVSPSources, GUIDop, MainUnit,
  TVSPType, TVSPMsg, TVSPConst, AbstractNet, TCPNet, TVSPBasicSources, csl,
  HashMap, MapKeys, AdvFunc, TCPClient, TVSPClient, TVSPClientIndexer,
  TVSPIndexedValues, TestValues, TVSPClientSources, TCPSendQueue;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

