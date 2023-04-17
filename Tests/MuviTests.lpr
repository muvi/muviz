program MuviTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, HashMapTests, HashMap, MapKeys, csl,
  TagUnit, TagTests, AdvFuncTests, ArrayFuncTests, ListTests, LinkedList, Doors,
  DoorUnit, MethodQueues, Delegates, ObjectClassBasic, Queue, cslPushable,
  TVSPPermissions, TVSPType, IncrementalHashSet, IncrementalSet, advguipack,
  GUIDop, PluginType, DoorTests, MethodQueueTests, IncrementalHashSetTests,
  PushableTests, QueueTests, TVSPPermissionTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

