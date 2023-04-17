program Muvi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MStringImpl
  { you can add units after this }, MainUnit, VisUnit, VisDrawUnit,
  Visualisations, CXControls, AdvGuiPack, AdvCoord, AdvFunc, GraphX32,
  PresetUnit, NetCommander, SQLDBLaz, lazcontrols, lazopenglcontext, lnetvisual,
  KeyboardUnit, SelectKey, BeatMon, FramePlugin, KeyPreUnit, DllStr, DllStrUnit,
  StdDllCtrls, StdDllInterfaces, StdDllType, BasicFreqAna, WaveDest, FFTFreqAna,
  FreqAna2, ExtPtrType, AnalyseSettings, PluginSystem, CommandList,
  CommandListImpl, VPBuffers, SpectrumData, PluginType, UltraSort,
  PseudoSpectrumData, PresetList, AdvClasses, SubbandEditUnit,
  GenericMemoryArrays, EqualityCounterList, DynamicBase,
  DynamicErrorDebugHelper, MPlugin, MInterfacedObject, PluginLoadUnit,
  MPluginSystem, MPluginType, ClientNetCom, ClientNetType, CommandListImpl2,
  MNetType, MNetUnit, AuthNetType, AuthNetUnit, NetEventDelegates, UIDArray,
  VisWinType, VisTypeUnit, VisType, VPBufferUnit, MCanvasUnit, GR32CanvasUnit,
  VisWinsUnit, ArrayListedObjects, AdvType, PluginListUnit, EditPreset,
  PresetUtil, SqrFunc, PhysicsWire, PhysProc, ObjectClassBasic, ObjectClasses,
  AdvGraphCtrl, ParamPainter, ColorChooseUnit, ColorChooseDlg, Asio, AsioList,
  BufCon, firm, PictureType, PictureTypeUnit, MInterfacedSource, SourceType,
  SrcUIDArray, ThreadOptions, LoadSelectUnit, MPFFreqAna, DiagDrawUnit,
  AdvMPFFreqAna, PicDiagUnit, GenericMemoryArraysXD, InputManager, AsioVis2,
  AdvThreads, PreviewUnit, PresetUtil2, EditPresetNew, ParamType,
  PlugPicTestUnit, GLVisUnit, GLCanvasUnit, AboutUnit, SettingUnit, MFileUtil,
  AdjustUnit, ControlTools, PresetTypeUnit, SelectPresetUnit, SharedCanvasUnit,
  VisTypeUnit2, TagUnit, RefCounter, VisTypeImpl, csl, Enumerators, LinkedList,
  HashMap, MapKeys, ParamTypeImpl, MTypes, CallParamType, StdParamTypes,
  StdParamTypeImpl, IntegerParamType, CallParamConverters,
  IntegerParamConverters, Buffers, FloatParamType, FloatParamConverters,
  ParamFunc, ColorParamType, ColorParamConverters, BooleanParamType,
  BooleanParamConverters, BufferParamType, BufferParamConverters,
  StringParamType, StringParamConverters, PresetParamType,
  PresetParamConverters, PointerParamType, PointerParamConverters, PresetUtil3,
  SlaveControl, PresetUtil3_ViewPreset, PresetUtil3_Plugs, MiscSlaveControls,
  PresetUtil3_Configuration, ParamTypeUtilImpl, PresetUtil3_Wires,
  ObjectCallbackContainer, PresetUtil3_BasePlug, PresetUtil3_DummyPlugs,
  DebugTools, ParamPainter2, ImageConfig, StdParamEdits, ParamTypeKey,
  ControlPlacer, PresetParamEdit, MExceptions, EditPreset3, VisType2,
  PresetType, ParamType2, StdTags, VisualisationMain, PresetUtilImpl,
  ImportUnit, configuration, VisualisationEnvironmentUnit, InterfacedObjects,
  ParamNotificationList, AdvParamType, ToolboxUnit, PlugImages, DragPlugUnit,
  TagType, tagTypeImpl, PresetListUnit, SelectPresetUnit2, ParamMap,
  VisEventImpl, GLVisWin2, StdVisualisations, AdvGlFunc, ThreadImpl,
  DefaultThread, PresetUtilities, dropbutton, MStrings, CanvasType, GUIDop,
  MPluginType4, ImportType, MPluginSystem4, CanvasGraphX32, LCLAdvFunc, ParamOp,
  SimpleVis, AttachedInterfaces, DoorUnit, Doors, AttachedParams, DoorUtils,
  ObjectArray, PresetUtil3_ParamSection, PresetUtil3_Connections,
  PresetUtil3_WiredPlug, Delegates, PresetUtil3_ConnectionRouter,
  PresetUtil3_Scrolling, PresetUtil3_WiringEditor, PresetUtil3_WireSlot,
  MethodQueues, PresetUtil3_ConnectedWireManager, PresetUtil3_UpdatingWire,
  PresetUtil3_Path, PresetUtil3_Paths, PresetUtil3_PointingRouter,
  PresetUtil3_WiredMaster, UniqueStrings, PointerParamEdit,
  ResizeBlockableMaster, FreqAna3, SlaveRotaryFader, TouchEvent, TouchControl,
  TouchControlBase, StdTagGroups, VisAddInput, PresetTagUnit, FAPParamType,
  KeyboardVisualisations, ValueStorages, ValueStoragePreset,
  VisualisationParamOrder, TVSPSources, cslPushable, Queue,
  VisPresetList3, VisualisationParamOrderImpl, PointerBoundParamOrder,
  TVSPClient, TVSPIndexedValues, TVSPClientIndexer, TVSPClientSources,
  TCPClient, TVSPMsg, AbstractNet, TVSPType, TVSPConst, GlobalTVSP,
  TVSPSourceUtil, GlobalVisualisationAliveMap, SimpleNotificationList,
  visualisationutils, LogUnit, EditPresetSettings, VisFiles,
  Param_CopyConverted, Mide, pascalscript, PerformanceTestUnit, StrEscapeUtils,
  StdPermissions, TagParamType, TagParamTypeIntf, InfoMsg, InfoMsgs,
  InfoMsgTypes, InfoMsgFrame, InfoMsgUnit, ProgressHandler, ParamSorter2,
  LexCompare, VisDebugUtils, PresetUtil3_BasicParamSection, 
PresetUtil3_SeperatorSection, EditableParamSorter;

{.$IFDEF WINDOWS}{.$R Muvi.rc}{.$ENDIF}

{$R *.res}

begin
  //heaptrc.SetHeapTraceOutput('muvi.trc');
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPresetForm, PresetForm);
  Application.CreateForm(TKeyboardForm, KeyboardForm);
  Application.CreateForm(TBeatMonForm, BeatMonForm);
  Application.CreateForm(TKeyPreForm, KeyPreForm);
  Application.CreateForm(TAnalyseSettingForm, AnalyseSettingForm);
  Application.CreateForm(TSubbandEditForm, SubbandEditForm);
  Application.CreateForm(TErrorDebugForm, ErrorDebugForm);
  Application.CreateForm(TPluginForm, PluginForm);
  Application.CreateForm(TPresetEditForm, PresetEditForm);
  Application.CreateForm(TLoadSelectForm, LoadSelectForm);
  Application.CreateForm(TDiagDrawForm, DiagDrawForm);
  Application.CreateForm(TPicDiagForm, PicDiagForm);
  Application.CreateForm(TPresetEdit3Form, PresetEdit3Form);
  Application.CreateForm(TPlugPicTestForm, PlugPicTestForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TAdjustForm, AdjustForm);
  Application.CreateForm(TSelectPresetForm, SelectPresetForm);
  Application.CreateForm(TNewPresetEditForm, NewPresetEditForm);
  Application.CreateForm(TToolboxForm, ToolboxForm);
  Application.CreateForm(TDragPlugForm, DragPlugForm);
  Application.CreateForm(TSelectPresetForm2, SelectPresetForm2);
  Application.CreateForm(TPresetTagEditForm, PresetTagEditForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TPresetEditSettingsForm, PresetEditSettingsForm);
  Application.CreateForm(TMideForm, MideForm);
  Application.CreateForm(TPerformanceTestForm, PerformanceTestForm);
  Application.CreateForm(TInfoMsgForm, InfoMsgForm);
  Application.Run;
end.

