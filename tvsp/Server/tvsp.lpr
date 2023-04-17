program tvsp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetvisual, csl, HashMap, Enumerators, ObjectClassBasic, ObjectClasses,
  OrderedObjectList, GUIDop, VisType2, AdvFunc, TCPServer, MainUnit, TVSPServer,
  TVSPGroups, TVSPType, TVSPMsg, TVSPConst, AbstractNet, TCPNet,
  TVSPServerAttachment, TVSPSources, TVSPBasicSources, TCPSendQueue,
  TVSPServerGroups, TVSPValues, TVSPAbstractServerValue, TVSPServerError,
  TVSPServerSources, IncrementalSet, IncrementalHashSet, TCPReceiveBuffer,
  TCPSocketProperties, TCPType, TVSPPermissions, TVSPServerPermissions, 
  TVSPParamGroupMap, LockedObjects, DiagnoseUnit, AdvancedObjectClassBasic;

{$R *.res}

begin
  //Heaptrc.SetHeapTraceOutput('tvsp.trc');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDiagnoseForm, DiagnoseForm);
  Application.Run;
end.

