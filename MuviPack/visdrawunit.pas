unit VisDrawUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GR32, {Simple}SpectrumData, Forms, AdvFunc, VisTypeUnit,
  VisLoadUnit, GR32_Transforms, GR32_Resamplers, VisDrawType, Dialogs,
  DynamicVisMem, VPBuffers, PluginType, ExtPtrType, AdvClasses;

const
  ANDColor          = $00FFFFFF;
  ANDAlpha          = $FF000000;
  SHLAlpha          = 24;
  VIS_ID_SIZE       = 8;
  PresetOFileID     = 'VisPresets';
  Preset2OFileID    = 'Presets v2';
  PresetFileID      = MuviFileID+PresetOFileID;
  Preset2FileID     = MuviFileID+Preset2OFileID;
  PresetFileIDSize  = MuviFileIDSize+GenerelFileIDSize;
  LibListHeader     = '[Muvi Visualisations]';
  CommentChar       = ';';
  IniBrackets       = '[';

type

  TVisedEvent       = procedure of object;

  TVisThread        = class {(TExecutedObject)}{(TThread)}
  private
    {FBGColor      : TColor32;
    FColor        : TColor32;
    FBuffer       : TBitmap32;}
    //FWorkspaceSize: Cardinal;
    //FWorkspace    : Pointer;
    ChangePreset  : procedure of object;
    FNewPreset    : PPreset;
    Suspended     : Boolean;
//    procedure InitPreset;
//    procedure FreePreset;
    procedure DoChangePreset;
    procedure DoNotChangePreset;
    procedure DoChangeParams;
    procedure DoSuspend;
  protected
    FPreset           : TVisComposition;
//    procedure Execute; {override;}
    procedure DoExecute;
  public
    FSpectrumData  : ISpectrumData;
    FBitmap        : TBitmap32;
    FTransformation: TAffineTransformation;
    FWaitTime      : Integer;
    constructor Create(ABmp: TBitmap32; ASpectrumData: ISpectrumData; var APreset: TPreset){(BGCol, VisCol: TColor32; Path: String)};
    destructor Destroy; override;
    procedure SetPreset(var APreset: TPreset);
    procedure Suspend;
    procedure Resume;
    //procedure Terminate;
    //procedure WaitFor;
  end;

  TMuviSuccessCode  = (scFail = 0, scSuccess = 1, scPluginMissing = 2, scPluginExists = 3, scFileNotFound = 4);
  TMuviLoadResult   = record
    Success : TMuviSuccessCode;
    FailInfo: TVisID;
  end;

  vpRealBufferElem= MVFloat;
  vpRealBuffer2   = array [0..MaxPtrArray] of vpRealBufferElem;
  pvpRealBuffer2  = ^vpRealBuffer2;
  vpRealBuffer_   = packed record
    Size: LongWord;
    R1  : LongWord;
    R2  : Int64;
    Dest: pvpRealBuffer2;
    R3  : LongWord;
    R4  : UInt64;
  end;

  TVPBufferManager = class (TObject, IVPBufferManager)
  private
    function GetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord): Double; stdcall;
    procedure SetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord; const Value: Double); stdcall;
  protected
    function QueryInterface(const iid : tguid; out obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;

    function GetVersion: MVVersion; stdcall;
    function Future(const Version: MVVersion): IMInterface; stdcall;
  public
    function NewBuffer(const ASize: LongWord): vpRealBuffer; stdcall;
    procedure DisposeBuffer(var ABuffer: vpRealBuffer); stdcall;
    function ToVPRealBuffer(const ASize: LongWord; const Data: Pointer): vpRealBuffer; stdcall;
    function GetBufferData(const ABuffer: vpRealBuffer): Pointer; stdcall;
    function SizeOfBuffer(const ABuffer: vpRealBuffer): LongWord; stdcall;
    procedure ResizeBuffer(var ABuffer: vpRealBuffer; const NewSize: LongWord); stdcall;

    property BufferItems[const ABuffer: vpRealBuffer; const Index: LongWord]: Double read GetBufferItem write SetBufferItem;
  end;

procedure RenderPresetPreview(const APreset: TPreset; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);

procedure VTDestroyPreset(var Dest: TVisComposition; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
//procedure VTDestroyPresetLayers(var Dest: TVisLayers; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
procedure VTCopyPreset(const Source: TPreset; var Dest: TVisComposition; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
//procedure VTCopyPresetLayers(const Source: TPresetLayers; var Dest: TVisLayers);
//procedure VTInitPresetLayers(var Dest: TVisLayers; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);

procedure Visualize(var bmp: TBitmap32; const Source: ISpectrumData; const Workspace: Pointer; const Visualisation: TVisualisation); inline; overload;
procedure Visualize(var bmp: TBitmap32; const Source: ISpectrumData; const Workspace: Pointer; const Preset: TVisComposition); inline; overload;
{procedure TrigCall(var bmp: TBitmap32; const Source: ISpectrumData; const Trigger: TVisOutput);
procedure TrigCall_(var bmp: TBitmap32; const Source: ISpectrumData; const Trigger: TVisualisation);}

function MultiLoad(const FileName: string; var APresets: TPresets; var AKeyboards: TVisKeyboards; Append: Boolean = false): TMuviLoadResult;

procedure SavePresetsToStream(const Presets: TPresets; Stream: TStream);
function LoadPresetsFromStream(var Presets: TPresets; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TMuviLoadResult;
procedure SavePresetsToFile(const Presets: TPresets; const FileName: string);
function LoadPresetsFromFile(var Presets: TPresets; const FileName: string; const Append: Boolean = false): TMuviLoadResult;

procedure SavePresetsToStream2(const Presets: TPresets; Stream: TStream);
function LoadPresetsFromStream2(var Presets: TPresets; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TMuviLoadResult;
procedure SavePresetsToFile2(const Presets: TPresets; const FileName: string);
function LoadPresetsFromFile2(var Presets: TPresets; const FileName: string; const Append: Boolean = false): TMuviLoadResult;

function LoadPresetsFromStreamU(var Presets: TPresets; Stream: TStream; const Append: Boolean = false): TMuviLoadResult;
function LoadPresetsFromFileU(var Presets: TPresets; const FileName: string; const Append: Boolean = false): TMuviLoadResult;

procedure ClearLayers(var Preset: TPreset);
procedure RemovePreset(var Presets: TPresets; const Index: Integer);
procedure ClearPresets(var Presets: TPresets);
procedure RemoveLayer_(var Layers: TPresetLayers; const Index: Integer);
procedure RemoveLayer(var APreset: TPreset; const Index: Integer);
procedure ChangeLayers_(var Layers: TPresetLayers; const Index1,Index2: Integer);
procedure ChangeLayers(var APreset: TPreset; const Index1,Index2: Integer);
procedure CopyPreset(const Src: TPreset; var Dest: TPreset);
procedure CopyLayer(const Src: TPresetVis; out Dest: TPresetVis);

function FindID(const VisID): Cardinal;
function CreateVis(const Index: Cardinal): TPresetVis;
function CreateVis_ID(const VisID): TPresetVis;
procedure DestroyVis(var Visualisation: TPresetVis);

{procedure SetVisParams(var Visualisation: TVisualisation; const Params); overload;
procedure SetVisParam(var Visualisation: TVisualisation; const ParamIndex: Cardinal; const Params); overload;
procedure SetVisParam(const Visualisation: TVisualisation; const Dest: TVisParam; const Source; const Size: Cardinal); overload;
procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
procedure VisParamsChanged(const Visualisation: TVisualisation); overload;}
procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;

procedure GetVisParams(const Visualisation: TVisualisation; out Params); overload;
procedure GetVisParam(const Visualisation: TVisualisation; const ParamIndex: Cardinal; out Params); overload;
procedure GetVisParam(const Source: TVisParam; out Dest; const Size: Cardinal); overload;
function ParamPos(const Visualisation: TVisualisation; const Index: Integer): TVisParam; overload;

procedure SetVisParams(var Visualisation: TPresetVis; const Params); overload;
procedure SetVisParam(var Visualisation: TPresetVis; const ParamIndex: Cardinal; const Params); overload;
procedure SetVisParam(const Visualisation: TPresetVis; const Dest: TVisParam; const Source; const Size: Cardinal); overload;
//procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
//procedure VisParamsChanged(const Visualisation: TPresetVis); overload;
procedure GetVisParams(const Visualisation: TPresetVis; out Params); overload;
procedure GetVisParam(const Visualisation: TPresetVis; const ParamIndex: Cardinal; out Params); overload;
//procedure GetVisParam(const Source: TVisParam; out Dest; const Size: Cardinal); overload;
function ParamPos(const Visualisation: TPresetVis; const Index: Integer): TVisParam; overload;

{procedure SetVisOutput(const Visualisation: TVisualisation; const Value; const Size,Index: Integer); overload;
procedure SetVisOutput2(const Visualisation: TVisualisation; const Value; const Index: Integer);}
function VisOutputOffset(const Visualisation: TPresetVis; const Index: Integer): TVisInputPos;
//function InOutFits(const Preset: TPreset; const OutLayer,Output,InLayer,Param: Integer): Boolean;
procedure SetVisOutput(var Preset: TPreset; const AOutLayer,AOutput,AInLayer,AParam: Integer); overload;

function RealBufferMiddle(const ABuffer: vpRealBuffer): vpReal;

procedure InitKeyboard(var AKeyboard: TVisKeyboard; const AName: string);
procedure RemoveKeyboard(var AKeyboards: TVisKeyboards; const Index: Integer);
function ReadFileID(Stream: TStream): TMuviFileID;
procedure SaveKeyboardsToStream(const Keyboards: TVisKeyboards; Stream: TStream);
function LoadKeyboardsFromStream(var Keyboards: TVisKeyboards; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): Boolean;
procedure SaveKeyboardsToFile(const Keyboards: TVisKeyboards; const FileName: string);
function LoadKeyboardsFromFile(var Keyboards: TVisKeyboards; const FileName: string; const Append: Boolean = false): Boolean;

//operator + (const m1,m2: TVisParamType) Result: TVisParamType;

//procedure SetVisOutput(const Preset: TPreset; var SetVis: TVisualisation; const Param: Integer); overload;

{
function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AVisParamDesc: TVisParamDesc; const AInitialValues; const AVisParamSize: Cardinal; const AWorkspaceSize: Cardinal; const ChangeParamsProc: TWorkspaceproc = nil; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AVisParamDesc: TVisParamDesc; const AVisOutputDesc: TVisOutputDesc; const AInitialValues; const AVisParamSize: Cardinal; const AWorkspaceSize: Cardinal; const ChangeParamsProc: TWorkspaceproc = nil; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AVisParamDesc: TVisParamDesc; const AInitialValues; const ChangeParamsProc: TWorkspaceproc = nil; const AWorkspaceSize: Cardinal = 0; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const ANames: array of string; const ATypes: array of TVisParamType; const AInitialValues; const AVisParamSize: Cardinal; const AWorkspaceSize: Cardinal; const ChangeParamsProc: TWorkspaceproc = nil; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
function VisType(const AName: string; const ID; const AVisProc: TVisProc; const ANames: array of string; const ATypes: array of TVisParamType; const AInitialValues; const ChangeParamsProc: TWorkspaceproc = nil; const AWorkspaceSize: Cardinal = 0; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType;
function VisType2(const AName: string; const ID; const AVisProc: TVisProc; const ANames: array of string; const ATypes: array of TVisParamType; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const ChangeParamsProc: TWorkspaceProc = nil; const AWorkspaceSize: Cardinal = 0; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType;
}

function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AParamDesc: TVisParamDesc; const AInitialValues; const AVisParamSize: Cardinal; const AOutputDesc: TVisOutputDesc; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil; const AC1Desc: ShortString = '1. Linienfarbe'; const AC2Desc: ShortString = '2. Linienfarbe'; const AC3Desc: ShortString = 'Hintergrundfarbe'): TVisType;
function VisType(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil): TVisType;

function LoadVis(const FileName: string): Boolean;
procedure SaveVis(const FileName: string);

function RegisterVis(const AVisType: TVisType): Integer;
procedure CreateAndRegisterVis(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
procedure CreateAndRegisterVis2(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil); stdcall;
procedure UnRegisterVis(const ID: Cardinal);
function RegVisCount: Integer;
function Visualisations(const Index: Integer): TVisType;

{function RegisterTrig(const AVisType: TVisType): Integer;
procedure UnRegisterTrig(const ID: Cardinal);
function RegTrigCount: Integer;
function Triggers(const Index: Integer): TVisType;}

procedure SetGlobalWaitTime(Value: Integer);
function GetGlobalWaitTime: Integer;
property GlobalWaitTime: Integer read GetGlobalWaitTime write SetGlobalWaitTime;
function GetGlobalVisPriority: TThreadPriority;
procedure SetGlobalVisPriority(const Value: TThreadPriority);
procedure SetGlobalVisedEvent(const Value: TVisedEvent);
property GlobalVisPriority: TThreadPriority read GetGlobalVisPriority write SetGlobalVisPriority;

procedure DVMGetMem(var P: Pointer; const Size: Cardinal);
procedure DVMFreeMem(var P: Pointer; const Size: Cardinal);
procedure DVMSetLength(var Arr: TVisArray; const NewLength,OldLength,ElemSize: Cardinal);

procedure UnloadPlugins;

implementation

const
  NilStart = TVisInputPos(0);

type
  TVisTypeList = array of TVisType;

var
  FRegisteredVis : TVisTypeList;
  //FRegisteredTrig: TVisTypeList;

{TGlobalVisThread}

type
  //TVisTProc        = procedure of object;
  //TVisTProcs        = array of TVisProc;
  TVisTData        = record
    Vis : TVisThread;
    Next: Pointer;
  end;
  PVisTData        = ^TVisTData;
  TGVTActionProc   = procedure of object;

  TGlobalVisThread = class (TThread)
  private
    FVisProcs      : PVisTData;
    FWaitTime      : Integer;
    FAThread       : TVisThread;
    FAction        : TGVTActionProc;
    FActionExecuted: Boolean;
    FOnVised       : TVisedEvent;
    procedure SetOnVised(const Value: TVisedEvent);
    procedure VisedDoNothing;
  protected
    procedure Execute; override;
    procedure DoAdd;
    procedure DoRemove;
    procedure DoNothing;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const VisThread: TVisThread);
    procedure Remove(const VisThread: TVisThread);
    procedure Clear;
  published
    property OnVised: TVisedEvent read FOnVised write SetOnVised;
  end;

const
  VisTDataSize = SizeOf(TVisTData);

constructor TGlobalVisThread.Create;
begin
  inherited Create(true);
  FOnVised:=@VisedDoNothing;
  FVisProcs:=nil;
  FWaitTime:=25;
  FreeOnTerminate:=true;
  FAction:=@DoNothing;
  Resume;
end;

destructor TGlobalVisThread.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGlobalVisThread.Execute;
var
  NextData: PVisTData;
begin
  while not Terminated do begin
    NextData:=FVisProcs;
    while NextData<>nil do with NextData^ do begin
      //Proc;
      Vis.DoExecute;
      NextData:=Next;
    end;
    //hier ggf. Visualisierungsprozeduren hinzufügen/löschen
    FAction;
    FOnVised;
    Sleep(FWaitTime);
  end;
end;

procedure TGlobalVisThread.VisedDoNothing;
begin
  //do nothing
end;

procedure TGlobalVisThread.SetOnVised(const Value: TVisedEvent);
begin
  if Value<>nil
    then FOnVised:=Value
    else FOnVised:=@VisedDoNothing;
end;

procedure TGlobalVisThread.Add(const VisThread: TVisThread);
begin
  FAThread:=VisThread;
  FActionExecuted:=false;
  FAction:=@DoAdd;
  while not FActionExecuted do Application.ProcessMessages;
end;

procedure TGlobalVisThread.Remove(const VisThread: TVisThread);
begin
  FAThread:=VisThread;
  FActionExecuted:=false;
  FAction:=@DoRemove;
  while not FActionExecuted do Application.ProcessMessages;
end;

procedure TGlobalVisThread.DoAdd;
var
  NewData: PVisTData;
begin
  GetMem(NewData,VisTDataSize);
  with NewData^ do begin
    Vis:=FAThread;
    Next:=FVisProcs;
  end;
  FVisProcs:=NewData;

  FActionExecuted:=true;
  FAction:=@DoNothing;
end;

{function CountVisProcs(ANextData: PVisTData): Integer;
begin
  Result:=0;
  while ANextData<>nil do begin
    ANextData:=ANextData^.Next;
    Inc(Result);
  end;
end;}

procedure TGlobalVisThread.DoRemove;
var
  NextData   : PVisTData;
  PtrNextData: ^PVisTData;
  //C1,C2: Integer;
begin
  //C1:=CountVisProcs(FVisProcs);

  NextData:=FVisProcs;
  PtrNextData:=@FVisProcs;
  while NextData^.Vis<>FAThread do begin
    with NextData^ do begin
      PtrNextData:=@Next;
      NextData:=Next;
    end;
  end;
  PtrNextData^:=NextData^.Next;
  FreeMem(NextData{PtrNextData^},VisTDataSize);

  FActionExecuted:=true;
  FAction:=@DoNothing;

  //C2:=CountVisProcs(FVisProcs);
  //Meldung('Anfang: '+IntToStr(C1)+'Ende: '+IntToStr(C2),TdMInfo);
end;

procedure TGlobalVisThread.DoNothing;
begin

end;

procedure TGlobalVisThread.Clear;
var
  NextData,NextData2: PVisTData;
begin
  NextData:=FVisProcs;
  while NextData<>nil do begin
    NextData2:=NextData;
    NextData:=NextData^.Next;
    FreeMem(NextData2,VisTDataSize);
  end;
  FVisProcs:=nil;
end;

var
  GlobalVisThread: TGlobalVisThread;

{TVisThread}

constructor TVisThread.Create(ABmp: TBitmap32; ASpectrumData: ISpectrumData; var APreset: TPreset){(BGCol, VisCol: TColor32; Path: String)};
begin
  inherited Create{(true)};
  FWaitTime:=25;
  {FBuffer:=TBitmap32.Create;
  FBuffer.Width:=300;
  FBuffer.Height:=300;}
  {FTransformation:=TAffineTransformation.Create;
  FTransformation.Clear;}
  FBitmap:=ABmp;
  FSpectrumData:=ASpectrumData;

  VTCopyPreset(APreset,FPreset,FBitmap,FSpectrumData);
  //FPreset:=@APreset;
  //FWorkspaceSize:=0;
  //InitPreset;

  ChangePreset:=@DoNotChangePreset;
  FBitmap.DrawMode:=dmBlend;
  FBitmap.CombineMode:=cmMerge;
  //Resume;
  GlobalVisThread.Add(Self{@DoExecute});
end;

destructor TVisThread.Destroy;
begin
  Suspend;
  //Terminate;
  VTDestroyPreset(FPreset,FBitmap,FSpectrumData);
  FSpectrumData:=nil;
  //FreePreset;
  //FBuffer.Destroy;
  //FTransformation.Destroy;
  inherited Destroy;
  //whiel
end;

procedure TVisThread.Suspend;
begin
  //Suspended:=false;
  //ChangePreset:=@DoSuspend;
  //while not Suspended do Application.ProcessMessages;
  GlobalVisThread.Remove(Self{@DoExecute});
end;

procedure TVisThread.Resume;
begin
  GlobalVisThread.Add(Self{@DoExecute});
end;

{procedure TVisThread.Terminate;
begin
  //GlobalVisThread.Remove(@DoExecute);
end;

procedure TVisThread.WaitFor;
begin

end;}

procedure TVisThread.DoSuspend;
begin
  GlobalVisThread.Remove(Self{@DoExecute});
  Suspended:=true;
  ChangePreset:=@DoNotChangePreset;
end;

(*procedure TVisThread.InitPreset;
var
  I         : Integer;
  AWorkspace: Pointer;
  ALayer    : ^TVisualisation;
begin
  with FPreset^ do begin
    FWorkspaceSize:=0;
    for I:=0 to Length(Layers)-1 do FWorkspaceSize+=FRegisteredVis[Layers[I].VisID].WorkspaceSize;

    GetMem(FWorkspace,FWorkspaceSize);
    //ShowMessage(IntToStr(FWorkspaceSize));
    AWorkspace:=FWorkspace;
    for I:=0 to Length(Layers)-1 do begin
      ALayer:=@Layers[I];
      with FRegisteredVis[ALayer^.VisID] do begin
        {if Assigned(InitWorkspace) then }InitWorkspace(ALayer^,AWorkspace^);
        {if Assigned(ChangeParams) then }ChangeParams(ALayer^,AWorkspace^);
        AWorkspace:=AWorkspace+WorkspaceSize;
      end;
    end;
  end;
end;

procedure TVisThread.FreePreset;
var
  I         : Integer;
  AWorkspace: Pointer;
  ALayer    : ^TVisualisation;
begin
  AWorkspace:=FWorkspace;
  with FPreset^ do begin
    for I:=0 to Length(Layers)-1 do begin
      ALayer:=@Layers[I];
      with FRegisteredVis[ALayer^.VisID] do begin
        {if Assigned(FreeWorkspace) then }FreeWorkspace(ALayer^,AWorkspace^);
        AWorkspace:=AWorkspace+WorkspaceSize;
      end;
    end;
  end;
  FWorkspaceSize:=0;
  FreeMem(FWorkspace,FWorkspaceSize);
  FWorkspace:=nil;
end;*)

procedure TVisThread.DoChangePreset;
begin
  {FreePreset;
  FPreset:=FNewPreset;
  InitPreset;}
  VTDestroyPreset(FPreset,FBitmap,FSpectrumData);
  VTCopyPreset(FNewPreset^,FPreset,FBitmap,FSpectrumData);
  ChangePreset:=@DoNotChangePreset;
end;

procedure TVisThread.DoNotChangePreset;
begin

end;

procedure TVisThread.DoChangeParams;
begin
  //FRegisteredVis[FPreset^.]
end;

procedure TVisThread.SetPreset(var APreset: TPreset);
begin
  {Suspend;
  FreePreset;
  FPreset:=@APreset;
  InitPreset;
  Resume;}
  FNewPreset:=@APreset;
  ChangePreset:=@DoChangePreset;
end;

procedure TVisThread.DoExecute;
begin
  FBitmap.BeginUpdate;
  Visualize(FBitmap,FSpectrumData,nil{FWorkspace},FPreset{^});
  FBitmap.EndUpdate;
  FBitmap.Changed;
  //hier ggf. Preset wechseln
  ChangePreset;
end;

//procedure TVisThread.Execute;
//begin
 // while {(not Terminated) and} (not Application.Terminated) do begin
    //FBuffer.BeginUpdate;
    {FBuffer.Width:=FBitmap.Width;
    FBuffer.Height:=FBitmap.Height;}
    (*FBitmap.BeginUpdate;
    {FBuffer.BeginUpdate;
    FBuffer.DrawMode:=dmBlend;
    FBuffer.CombineMode:=cmMerge;}
    //if Suspended then ShowMessage('tschüss');
    Visualize(FBitmap,FSpectrumIndicator,FWorkspace,FPreset^);
    {FBuffer.EndUpdate;
    FBuffer.Changed;}
    //FBitmap.WrapMode:=wmMirror;
    //FBitmap.Clear($FF000000);
    //FBitmap.Draw(0,0,FBuffer);
    FBitmap.EndUpdate;
    FBitmap.Changed;
    //hier ggf. Preset wechseln
    ChangePreset;*)
    //DoExecute;
    //Sleep(FWaitTime);
  //end;
//end;

{TVPBufferManager}

function TVPBufferManager.QueryInterface(const iid : tguid; out obj) : longint; stdcall;
begin
  Result:=0;
end;

function TVPBufferManager._AddRef : longint;stdcall;
begin
  Result:=0;
end;

function TVPBufferManager._Release : longint;stdcall;
begin
  Result:=0;
end;

function TVPBufferManager.GetVersion: MVVersion; stdcall;
begin
  with Result do begin
    Version:=1;
    MainVersion:=0;
    SubVersion:=0;
  end;
end;

function TVPBufferManager.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  Result:=IMInterface(IVPBufferManager(Self));
end;

function TVPBufferManager.NewBuffer(const ASize: LongWord): vpRealBuffer; stdcall;
var
  Result2: vpRealBuffer_ absolute Result;
begin
  GetMem(Result2.Dest,SizeOf(vpRealBufferElem)*ASize);
  Result2.Size:=ASize;
end;

procedure TVPBufferManager.DisposeBuffer(var ABuffer: vpRealBuffer); stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  FreeMem(ABuffer2.Dest,SizeOf(vpRealBufferElem)*ABuffer2.Size);
  ABuffer2.Size:=0;
end;

function TVPBufferManager.ToVPRealBuffer(const ASize: LongWord; const Data: Pointer): vpRealBuffer; stdcall;
var
  Result2: vpRealBuffer_ absolute Result;
begin
  with Result2 do begin
    Dest:=Data;
    Size:=ASize;
  end;
end;

function TVPBufferManager.GetBufferData(const ABuffer: vpRealBuffer): Pointer; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=ABuffer2.Dest;
end;

function TVPBufferManager.SizeOfBuffer(const ABuffer: vpRealBuffer): LongWord; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=ABuffer2.Size;
end;

procedure TVPBufferManager.ResizeBuffer(var ABuffer: vpRealBuffer; const NewSize: LongWord); stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  FreeMem(ABuffer2.Dest,SizeOf(vpReal)*ABuffer2.Size);
  GetMem(ABuffer2.Dest,SizeOf(vpReal)*NewSize);
  ABuffer2.Size:=NewSize;
end;

function TVPBufferManager.GetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord): Double; stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  Result:=ABuffer2.Dest^[Index];
end;

procedure TVPBufferManager.SetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord; const Value: Double); stdcall;
var
  ABuffer2: vpRealBuffer_ absolute ABuffer;
begin
  ABuffer2.Dest^[Index]:=Value;
end;

{Allgemein}

function GetC1Pointer(var Visualisation: TVisualisation): Pointer;
begin
  Result:=@Visualisation.C1;
end;

function GetC2Pointer(var Visualisation: TVisualisation): Pointer;
begin
  Result:=@Visualisation.C2;
end;

function GetC3Pointer(var Visualisation: TVisualisation): Pointer;
begin
  Result:=@Visualisation.C3;
end;


{procedure _DoSetVisOutput__(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput__Call(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput__Integer(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput__Real(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput__String(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput__Color(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput__Boolean(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput_CallChanged_Integer(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput_CallChanged_Real(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput_CallChanged_String(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput_CallChanged_Color(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;
procedure _DoSetVisOutput_CallChanged_Boolean(var bmp: TBitmap32; const Source: ISpectrumData; const Data: TVisOutputData; const Value); stdcall; forward;}


type
  TGetDefaultColorPointer = function (var Visualisation: TVisualisation): Pointer;

const
  GetDefaultColorPointer: array [0..2] of TGetDefaultColorPointer = (@GetC1Pointer,@GetC2Pointer,@GetC3Pointer);

{$I visconvert.inc}

procedure {TVisThread.}VTDestroyPresetLayers(var Dest: TVisLayers; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
var
  I    : Integer;
begin
  for I:=0 to Length(Dest)-1 do begin
    with Dest[I] do with FRegisteredVis[VisID] do begin
      FreeWorkspace(ABitmap,ASpectrumData,Dest[I],VisParams^,Workspace^);
      FreeMem(VisParams,VisParamSize);
      //Move(ASource^.Params,ADest^.Params,VisParamSize);
      SetLength(VisOutputs,0);
      (***)
      FreeMem(Workspace,WorkspaceSize);
    end;
  end;
end;

procedure {TVisThread.}VTDestroyPreset(var Dest: TVisComposition; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
begin
  //Dest.Name:='';
  VTDestroyPresetLayers(Dest.Layers,ABitmap,ASpectrumData);
  VTDestroyPresetLayers(Dest.Triggers,ABitmap,ASpectrumData);
end;

procedure {TVisThread.}VTCopyPresetLayers(const Source: TPresetLayers; var Dest: TVisLayers);
var
  I,L    : Integer;
  ASource: PPresetVis;
  ADest  : PVisualisation;
begin
  L:=Length(Source);
  SetLength(Dest,L);
  for I:=0 to L-1 do begin
    ASource:=@Source[I];
    ADest:=@Dest[I];
    ADest^.VisID:=ASource^.VisID;
    ADest^.C1:=ASource^.C1;
    ADest^.C2:=ASource^.C2;
    ADest^.C3:=ASource^.C3;
    with FRegisteredVis[ASource^.VisID] do begin
      GetMem(ADest^.VisParams,VisParamSize);
      Move(ASource^.VisParams^,ADest^.VisParams^,VisParamSize);
      SetLength(ADest^.VisOutputs,Length(ASource^.VisOutputs));
      (***)
      GetMem(ADest^.Workspace,WorkspaceSize);
      //InitWorkspace(FBitmap,FSpectrumData,ADest^,ADest^.VisParams,ADest^.Workspace^);
    end;
  end;
  //Outputs zuweisen
  {for I:=0 to L-1 do begin
    ASource:=@Source[I];
    ADest:=@Dest[I];
    AssignOutputs(ASource^,ADest^);
  end;}
end;

procedure {TVisThread.}VTInitPresetLayers(var Dest: TVisLayers; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
var
  I    : Integer;
  ADest: PVisualisation;
begin
  for I:=0 to Length(Dest)-1 do begin
    ADest:=@Dest[I];
    with FRegisteredVis[ADest^.VisID]
      do InitWorkspace(ABitmap,ASpectrumData,ADest^,ADest^.VisParams,ADest^.Workspace^);
  end;
end;

procedure MatchOutputs(const Source: TPreset; var Dest: TVisComposition);

  {procedure OutputPos(var V1: TVisParams; var V2: TVisInputPos; var Result: TVisOutput);
  var
    _V1    : Pointer absolute V1;
    _V2    : Pointer absolute V2;
    _Result: Pointer absolute Result;
  begin
    _Result:=V1;
    _Result+=_V2;
//    _Result:=_V1+_V2;
  end;}

  procedure DoMatch(const ASource: TPresetVis; var ADest: TVisualisation);
  var
    J          : Integer;
    AInputLayer: PVisualisation;
    AOutputDesc: ^TVisOutputDesc;
  {const
    OutputTrans: array [TVisOutputType] of string = ('oCall','oInteger','oReal','oString','oColor','oBoolean');
    ParamTrans : array [TVisParamType] of string = ('vInteger','vReal','vString','vColor','vBoolean');}
  begin
    with ASource do begin
      AOutputDesc:=@FRegisteredVis[ASource.VisID].OutputDesc;
      for J:=0 to Length(VisOutputs)-1 do with VisOutputs[J] do begin
        if Param<-1 then begin
          ADest.VisOutputs[J].DoSet:=@_DoSetVisOutput__;
          continue;
        end;
        if Layer>=0
          then AInputLayer:=@Dest.Layers[Layer]
          else AInputLayer:=@Dest.Triggers[not Layer];
        with ADest.VisOutputs[J] do begin
          if Param>=3 then begin
            {ADest.VisOutputs[J]}Data.D:=AInputLayer^.VisParams+Cardinal(Offset);
            with FRegisteredVis[AInputLayer^.VisID].VisParamDesc[Param-3] do begin
              Data.P:=Change;
              DoSet:=_DoSetVisOutput_Converters_[Data.P<>nil][AType][AOutputDesc^[J].AType];
            end;
          end else if Param>=0
            then begin
              {ADest.VisOutputs[J]}Data.D:=GetDefaultColorPointer[Param](AInputLayer^);
              //DoSet:=@_DoSetVisOutput__Color;
              DoSet:=_DoSetVisOutput_Converters_[false][vColor][AOutputDesc^[J].AType];
            end else begin
              Data.P:=FRegisteredVis[AInputLayer^.VisID].VisProc;
              DoSet:=@_DoSetVisOutput__Call;
              //ADest.VisOutputs[J]:=AInputLayer;
            end;
          Data.V:=AInputLayer;
        end;
      end;
    end;
  end;
var
  I: Integer;
begin
  for I:=0 to Length(Dest.Layers)-1 do DoMatch(Source.Layers[I],Dest.Layers[I]);
  for I:=0 to Length(Dest.Triggers)-1 do DoMatch(Source.Triggers[I],Dest.Triggers[I]);
end;

{procedure PreInitOutputs(var Dest: TVisLayers);
var
  I,J: Integer;
begin
  for I:=0 to Length(Dest)-1 do with Dest[I] do begin
    for J:=0 to Length(VisOutputs)-1 do VisOutputs[J].
  end;
end;}

procedure {TVisThread.}VTCopyPreset(const Source: TPreset; var Dest: TVisComposition; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
begin
  //Dest.Name:=Source.Name;
  VTCopyPresetLayers(Source.Layers,Dest.Layers);
  VTCopyPresetLayers(Source.Triggers,Dest.Triggers);
  MatchOutputs(Source,Dest);
  VTInitPresetLayers(Dest.Layers,ABitmap,ASpectrumData);
  VTInitPresetLayers(Dest.Triggers,ABitmap,ASpectrumData);
end;

procedure RenderPresetPreview(const APreset: TPreset; ABitmap: TBitmap32; ASpectrumData: ISpectrumData);
var
  APreset2: TVisComposition;
begin
  VTCopyPreset(APreset,APreset2,ABitmap,ASpectrumData);
  ABitmap.BeginUpdate;
  ABitmap.Clear($FF000000);
  Visualize(ABitmap,ASpectrumData,nil,APreset2);
  ABitmap.EndUpdate;
  ABitmap.Changed;
  VTDestroyPreset(APreset2,ABitmap,ASpectrumData);
end;

{Allgemein - TPresets}

{function CalcWorkspaceSize(const APreset: TPreset): Cardinal;
var
  I: Integer;
begin

end;}

const
  {ParamTypeSize : array [TVisParamType] of Cardinal = (SizeOf(Integer),SizeOf(Real),SizeOf(ShortString),SizeOf(TColor32),SizeOf(Boolean));
  OutputTypeSize: array [TVisOutputType] of Cardinal = (0,SizeOf(Integer),SizeOf(Real),SizeOf(ShortString),SizeOf(TColor32),SizeOf(Boolean));}
  ParamTypeSize : array [TVisParamType] of Cardinal = (SizeOf(vpInt),SizeOf(vpReal),SizeOf(vpString),SizeOf(vpColor),SizeOf(vpBool),SizeOf(vpBuffer));
  //OutputTypeSize: array [TVisOutputType] of Cardinal = (0,SizeOf(vpInt),SizeOf(vpReal),SizeOf(vpString),SizeOf(vpColor),SizeOf(vpBool),SizeOf(vpBuffer));

function CreateVis_NoInit(const Index: Cardinal): TPresetVis; forward;
function CreateVis_ID_NoInit(const VisID): TPresetVis; forward;

procedure Visualize(var bmp: TBitmap32; const Source: ISpectrumData; const Workspace: Pointer; const Visualisation: TVisualisation);
begin
  FRegisteredVis[Visualisation.VisID].VisProc(bmp,Source,Visualisation,Visualisation.VisParams^,Visualisation.Workspace^);
end;

var
  _Vising: Boolean = false;

procedure Visualize(var bmp: TBitmap32; const Source: ISpectrumData; const Workspace: Pointer; const Preset: TVisComposition);
var
  I         : Integer;
  ALayer    : ^TVisualisation;
  //AWorkspace: Pointer;
begin
  //AWorkspace:=Workspace;
  while _Vising do ;
  _Vising:=true;
  for I:=0 to Length(Preset.Layers)-1 do begin
    ALayer:=@Preset.Layers[I];
    with FRegisteredVis[ALayer^.VisID] do begin
      VisProc(bmp,Source,ALayer^,ALayer^.VisParams^,ALayer^.Workspace^);
      //AWorkspace:=WorkspaceSize+AWorkspace;
    end;
  end;
  _Vising:=false;
end;

{procedure TrigCall_(var bmp: TBitmap32; const Source: ISpectrumData; const Trigger: TVisualisation);
begin
  FRegisteredVis[Trigger.VisID].VisProc(bmp,Source,Trigger,Trigger.VisParams^,Trigger.Workspace^);
end;

procedure TrigCall(var bmp: TBitmap32; const Source: ISpectrumData; const Trigger: TVisOutput);
var
  ATrig: PVisualisation absolute Trigger;
begin
  if Trigger<>nil then FRegisteredVis[ATrig^.VisID].VisProc(bmp,Source,ATrig^,ATrig^.VisParams^,ATrig^.Workspace^);
end;}

{Allgemein -TPresets - Load&Save v1}

procedure SavePresetsToStream(const Presets: TPresets; Stream: TStream);
var
  I,J,L: Integer;
  AType: ^TVisType;
begin
  Stream.Write(PresetFileID,PresetFileIDSize);
  L:=Length(Presets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with Presets[I] do begin
    SaveText(Name,Stream);
    L:=Length(Layers);
    Stream.Write(L,SizeOf(L));
    for J:=0 to L-1 do with Layers[J] do begin
      AType:=@FRegisteredVis[VisID];
      Stream.Write(AType^.VisID,VIS_ID_SIZE);
      Stream.Write(C1,SizeOf(C1));
      Stream.Write(C2,SizeOf(C2));
      Stream.Write(C3,SizeOf(C3));
      Stream.Write(VisParams^,AType^.VisParamSize);
    end;
  end;
end;

function LoadPresetsFromStream(var Presets: TPresets; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TMuviLoadResult;
var
  I,J,L,Offset: Integer;
  AVisID      : TVisID;
  AVis        : ^TPresetVis;
begin
  if ReadHeader then begin
    Result.Success:=TMuviSuccessCode(ReadFileID(Stream)=PresetFileID);
    if Result.Success<>scSuccess then exit;
  end else Result.Success:=scSuccess;
  if Append then begin
    Offset:=Length(Presets);
  end else begin
    Offset:=0;
    ClearPresets(Presets);
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(Presets,L);
  for I:=Offset to L-1 do with Presets[I] do begin
    LoadText(Name,Stream);
    Stream.Read(L,SizeOf(L));
    SetLength(Layers,L);
    WSize:=0;
    for J:=0 to L-1 do begin
      Stream.Read(AVisID,VIS_ID_SIZE);
      AVis:=@Layers[J];
      AVis^:=CreateVis_ID_NoInit(AVisID);
      with AVis^ do begin
        if VisID=0 then begin
          Result.Success:=scPluginMissing;
          Result.FailInfo:=AVisID;
          exit;
        end;
        Stream.Read(C1,SizeOf(C1));
        Stream.Read(C2,SizeOf(C2));
        Stream.Read(C3,SizeOf(C3));
        with FRegisteredVis[VisID] do begin
          Stream.Read(VisParams^,VisParamSize);
          WSize+=WorkspaceSize;
        end;
      end;
    end;
  end;
end;

procedure SavePresetsToFile(const Presets: TPresets; const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SavePresetsToStream(Presets,FS);
  FS.Free;
end;

function LoadPresetsFromFile(var Presets: TPresets; const FileName: string; const Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadPresetsFromStream(Presets,FS,Append);
  FS.Free;
end;

{Allgemein -TPresets - Load&Save v2}

procedure SavePresetsToStream2(const Presets: TPresets; Stream: TStream);
var
  L: Integer;

  procedure SavePresetLayers(var ALayers: TPresetLayers);
  var
    J    : Integer;
    AType: ^TVisType;
  begin
    L:=Length(ALayers);
    Stream.Write(L,SizeOf(L));
    for J:=0 to L-1 do with ALayers[J] do begin
      AType:=@FRegisteredVis[VisID];
      Stream.Write(AType^.VisID,VIS_ID_SIZE);
      Stream.Write(C1,SizeOf(C1));
      Stream.Write(C2,SizeOf(C2));
      Stream.Write(C3,SizeOf(C3));
      Stream.Write(VisParams^,AType^.VisParamSize);
    end;
  end;

  procedure SaveOutputs(var ALayers: TPresetLayers);
  var
    J,K   : Integer;
  begin
    for J:=0 to Length(ALayers)-1 do with ALayers[J] do begin
      for K:=0 to Length(FRegisteredVis[VisID].OutputDesc)-1 do with VisOutputs[K] do begin
        Stream.Write(Layer,SizeOf(Layer));
        Stream.Write(Param,SizeOf(Param));
      end;
      Stream.Write(DesignPos,SizeOf(DesignPos));
    end;
  end;

var
  I: Integer;
begin
  Stream.Write(Preset2FileID,PresetFileIDSize);
  L:=Length(Presets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with Presets[I] do begin
    SaveText(Name,Stream);
    SavePresetLayers(Layers);
    SavePresetLayers(Triggers);
    SaveOutputs(Layers);
    SaveOutputs(Triggers);
  end;
end;

function LoadPresetsFromStream2(var Presets: TPresets; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TMuviLoadResult;
var
  L: Integer;

  procedure LoadPresetLayers(var ALayers: TPresetLayers);
  var
    J           : Integer;
    AVisID      : TVisID;
    AVis        : ^TPresetVis;
  begin
    Stream.Read(L,SizeOf(L));
    SetLength(ALayers,L);
    //WSize:=0;
    for J:=0 to L-1 do begin
      Stream.Read(AVisID,VIS_ID_SIZE);
      AVis:=@ALayers[J];
      AVis^:=CreateVis_ID_NoInit(AVisID);
      with AVis^ do begin
        if VisID=0 then begin
          Result.Success:=scPluginMissing;
          Result.FailInfo:=AVisID;
          exit;
        end;
        Stream.Read(C1,SizeOf(C1));
        Stream.Read(C2,SizeOf(C2));
        Stream.Read(C3,SizeOf(C3));
        with FRegisteredVis[VisID] do begin
          Stream.Read(VisParams^,VisParamSize);
          //WSize+=WorkspaceSize;
        end;
      end;
    end;
  end;

  procedure LoadPresetOutputs(var ALayers: TPresetLayers; var APreset: TPreset);
  var
    J,K: Integer;
  begin
    for J:=0 to Length(ALayers)-1 do with ALayers[J] do begin
      L:=Length(FRegisteredVis[VisID].OutputDesc);
      SetLength(VisOutputs,L);
      for K:=0 to L-1 do with VisOutputs[K] do begin
        Stream.Read(Layer,SizeOf(Layer));
        Stream.Read(Param,SizeOf(Param));
        if Param>=3 then begin
          if Layer>=0
            then Offset:=VisOutputOffset(APreset.Layers[Layer],Param-3)
            else Offset:=VisOutputOffset(APreset.Triggers[not Layer],Param-3)
        end;
      end;
      Stream.Read(DesignPos,SizeOf(DesignPos));
    end;
  end;

var
  I,Offset: Integer;
  APPreset: ^TPreset;
begin
  if ReadHeader then begin
    Result.Success:=TMuviSuccessCode(ReadFileID(Stream)=Preset2FileID);
    if Result.Success<>scSuccess then exit;
  end else Result.Success:=scSuccess;
  if Append then begin
    Offset:=Length(Presets);
  end else begin
    Offset:=0;
    ClearPresets(Presets);
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(Presets,L);
  for I:=Offset to L-1 do begin
    APPreset:=@Presets[I];
    LoadText(APPreset^.Name,Stream);
    LoadPresetLayers(APPreset^.Layers);
    if Result.Success<>scSuccess then exit;
    LoadPresetLayers(APPreset^.Triggers);
    if Result.Success<>scSuccess then exit;
    LoadPresetOutputs(APPreset^.Layers,APPreset^);
    LoadPresetOutputs(APPreset^.Triggers,APPreset^);
  end;
end;

procedure SavePresetsToFile2(const Presets: TPresets; const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SavePresetsToStream2(Presets,FS);
  FS.Free;
end;

function LoadPresetsFromFile2(var Presets: TPresets; const FileName: string; const Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadPresetsFromStream2(Presets,FS,Append);
  FS.Free;
end;

{Allgemein -TPresets - Load&Save universal}

{procedure SavePresetsToStream(const Presets: TPresets; Stream: TStream);
var
  I,J,L: Integer;
  AType: ^TVisType;
begin
  Stream.Write(PresetFileID,PresetFileIDSize);
  L:=Length(Presets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with Presets[I] do begin
    SaveText(Name,Stream);
    L:=Length(Layers);
    Stream.Write(L,SizeOf(L));
    for J:=0 to L-1 do with Layers[J] do begin
      AType:=@FRegisteredVis[VisID];
      Stream.Write(AType^.VisID,VIS_ID_SIZE);
      Stream.Write(C1,SizeOf(C1));
      Stream.Write(C2,SizeOf(C2));
      Stream.Write(C3,SizeOf(C3));
      Stream.Write(VisParams^,AType^.VisParamSize);
    end;
  end;
end;}

function LoadPresetsFromStreamU(var Presets: TPresets; Stream: TStream; const Append: Boolean = false): TMuviLoadResult;
{var
  I,J,L,Offset: Integer;
  AVisID      : TVisID;
  AVis        : ^TPresetVis;}
var
  AFID: TMuviFileID;
begin
  AFID:=ReadFileID(Stream);
  if AFID=PresetFileID
    then Result:=LoadPresetsFromStream(Presets,Stream,Append,false)
    else if AFID=Preset2FileID
      then Result:=LoadPresetsFromStream2(Presets,Stream,Append,false)
      else Result.Success:=scFail;
  {if ReadHeader then begin
    Result:=(ReadFileID(Stream)=PresetFileID);
    if not Result then exit;
  end else Result:=true;}
  {if Append then begin
    Offset:=Length(Presets);
  end else begin
    Offset:=0;
    ClearPresets(Presets);
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(Presets,L);
  for I:=Offset to L-1 do with Presets[I] do begin
    LoadText(Name,Stream);
    Stream.Read(L,SizeOf(L));
    SetLength(Layers,L);
    WSize:=0;
    for J:=0 to L-1 do begin
      Stream.Read(AVisID,VIS_ID_SIZE);
      AVis:=@Layers[J];
      AVis^:=CreateVis_ID_NoInit(AVisID);
      with AVis^ do begin
        Stream.Read(C1,SizeOf(C1));
        Stream.Read(C2,SizeOf(C2));
        Stream.Read(C3,SizeOf(C3));
        with FRegisteredVis[VisID] do begin
          Stream.Read(VisParams^,VisParamSize);
          WSize+=WorkspaceSize;
        end;
      end;
    end;
  end;}
end;

{procedure SavePresetsToFile(const Presets: TPresets; const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SavePresetsToStream(Presets,FS);
  FS.Free;
end;}

function LoadPresetsFromFileU(var Presets: TPresets; const FileName: string; const Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadPresetsFromStreamU(Presets,FS,Append);
  FS.Free;
end;


procedure ClearLayers(var Preset: TPreset);
var
  I: Integer;
begin
  with Preset do begin
    for I:=0 to Length(Layers)-1 do DestroyVis(Layers[I]);
    for I:=0 to Length(Triggers)-1 do DestroyVis(Triggers[I]);
    SetLength(Layers,0);
    SetLength(Triggers,0);
    WSize:=0;
  end;
end;

procedure ClearPresets(var Presets: TPresets);
var
  I      : Integer;
  APreset: PPreset;
begin
  for I:=0 to Length(Presets)-1 do begin
    APreset:=@Presets[I];
    //with Presets[I] do begin
    ClearLayers(APreset^);
    APreset^.Name:='';
    //end;
  end;
  SetLength(Presets,0);
end;

procedure RemovePreset(var Presets: TPresets; const Index: Integer);
var
 I      : Integer;
 APreset: PPreset;
begin
  APreset:=@Presets[Index];
  //with Presets[Index] do begin
  ClearLayers(APreset^);
  APreset^.Name:='';
  //end;
  for I:=Index to Length(Presets)-2 do Presets[I]:=Presets[I+1];
  SetLength(Presets,Length(Presets)-1);
end;

procedure RemoveLayer_(var Layers: TPresetLayers; const Index: Integer);
var
  I     : Integer;
  ALayer: ^TPresetVis;
begin
  //with Preset do begin
  ALayer:=@Layers[Index];
  //WSize-=FRegisteredVis[ALayer^.VisID].WorkspaceSize;
  DestroyVis(ALayer^);
  for I:=Index to Length(Layers)-2 do Layers[I]:=Layers[I+1];
  SetLength(Layers,Length(Layers)-1);

  {for I:=0 to Length(Layers)-1 do with Layers[I] do begin
    for J:=0 to Length(VisOutputs)-1 do with VisOutputs[J] do begin
      if Layer>=0 then begin
        if Layer>Index
            then Dec(Layer)
            else if Layer=Index
              then Param:=-2;
      end else begin
        if (not Layer)>Index
          then Inc(Layer)
          else if (not Layer)=Index
            then Param:=-2;
      end;
    end;
  end;}
  //end;
end;

procedure RemoveLayer(var APreset: TPreset; const Index: Integer);

  procedure _Reconnect_Layers_Changed(var ALayers: TPresetLayers);
  var
    I,J: Integer;
  begin
    for I:=0 to Length(ALayers)-1 do with ALayers[I] do begin
      for J:=0 to Length(VisOutputs)-1 do with VisOutputs[J] do begin
        if Layer>Index
          then Dec(Layer)
          else if Layer=Index
            then Param:=-2;
      end;
    end;
  end;

  procedure _Reconnect_Triggers_Changed(var ALayers: TPresetLayers);
  var
    I,J: Integer;
  begin
    for I:=0 to Length(ALayers)-1 do with ALayers[I] do begin
      for J:=0 to Length(VisOutputs)-1 do with VisOutputs[J] do begin
        if Layer<Index
          then Inc(Layer)
          else if Layer=Index
            then Param:=-2;
      end;
    end;
  end;

begin
  if Index>=0 then begin
    RemoveLayer_(APreset.Layers,Index);
    _Reconnect_Layers_Changed(APreset.Layers);
    _Reconnect_Layers_Changed(APreset.Triggers);
  end else begin
    RemoveLayer_(APreset.Triggers,not Index);
    _Reconnect_Triggers_Changed(APreset.Layers);
    _Reconnect_Triggers_Changed(APreset.Triggers);
  end;

  {for I:=0 to Length(APreset.Layers)-1 do with Layers[I] do begin
    for J:=0 to Length(VisOutputs)-1 do with VisOutputs[J] do begin
      if Layer>=0 then begin
        if Layer>Index
            then Dec(Layer)
            else if Layer=Index
              then Param:=-2;
      end else begin
        if (not Layer)>Index
          then Inc(Layer)
          else if (not Layer)=Index
            then Param:=-2;
      end;
    end;
  end;}
end;

procedure ChangeLayers_(var Layers: TPresetLayers; const Index1,Index2: Integer);
var
  TempLayer: TPresetVis;
begin
  TempLayer:=Layers[Index1];
  Layers[Index1]:=Layers[Index2];
  Layers[Index2]:=TempLayer;
end;

procedure ChangeLayers(var APreset: TPreset; const Index1,Index2: Integer);

  procedure _Reconnect_(var ALayers: TPresetLayers);
  var
    I,J: Integer;
  begin
    for I:=0 to Length(ALayers)-1 do with ALayers[I] do begin
      for J:=0 to Length(VisOutputs)-1 do with VisOutputs[J] do begin
        if Layer=Index1
          then Layer:=Index2
          else if Layer=Index2
            then Layer:=Index1;
      end;
    end;
  end;

var
  TempLayer      : TPresetVis;
  ALayer1,ALayer2: PPresetVis;
begin
  if Index1>=0
    then ALayer1:=@APreset.Layers[Index1]
    else ALayer1:=@APreset.Triggers[not Index1];
  if Index2>=0
    then ALayer2:=@APreset.Layers[Index2]
    else ALayer2:=@APreset.Triggers[not Index2];

  TempLayer:=ALayer1^;
  ALayer1^:=ALayer2^;
  ALayer2^:=TempLayer;

  _Reconnect_(APreset.Layers);
  _Reconnect_(APreset.Triggers);
end;

procedure CopyPreset(const Src: TPreset; var Dest: TPreset);
var
  I,L           : Integer;
  //SrcVis,DestVis: PPresetVis;
begin
  L:=Length(Src.Layers);
  SetLength(Dest.Layers,L);
  for I:=0 to L-1 do CopyLayer(Src.Layers[I],Dest.Layers[I]);
  L:=Length(Src.Triggers);
  SetLength(Dest.Triggers,L);
  for I:=0 to L-1 do CopyLayer(Src.Triggers[I],Dest.Triggers[I]);
    {SrcVis:=@Src.Layers[I];
    DestVis:=@Dest.Layers[I];
    DestVis^:=CreateVis(SrcVis^.VisID);
    DestVis^.C1:=SrcVis^.C1;
    DestVis^.C2:=SrcVis^.C2;
    DestVis^.C3:=SrcVis^.C3;
    DestVis^.DesignPos:=SrcVis^.DesignPos;
    Move(SrcVis^.VisParams^,DestVis^.VisParams^,FRegisteredVis[SrcVis^.VisID].VisParamSize);}
  //end;
end;

procedure CopyLayer(const Src: TPresetVis; out Dest: TPresetVis);
var
  I,L: Integer;
begin
  Dest:=CreateVis(Src.VisID);
  Dest.C1:=Src.C1;
  Dest.C2:=Src.C2;
  Dest.C3:=Src.C3;
  Dest.DesignPos:=Src.DesignPos;
  L:=Length(Src.VisOutputs);
  SetLength(Dest.VisOutputs,L);
  for I:=0 to L-1 do begin
    Dest.VisOutputs[I]:=Src.VisOutputs[I];
  end;
  Move(Src.VisParams^,Dest.VisParams^,FRegisteredVis[Src.VisID].VisParamSize);
end;

{Allgemein - TVisType - DLLs}

var
  VisLibNames: array of string;

const
  LoadVis_False = 0;
  LoadVis_True  = 1;
  LoadVis_Exists= 2;

function GetVPBufferManager: IVPBufferManager; stdcall; forward;

function DoLoadVis(FileName: string): Byte;
var
  I,L    : Integer;
  Result2: Boolean absolute Result;
begin
  L:=Length(VisLibNames);
  for I:=0 to L-1 do if VisLibNames[I]=FileName then begin
    Result:=LoadVis_Exists;
    exit;
  end;
  Result2:=VisLoadUnit.LoadVis(FileName,{@RegisterVis,}@DVMGetMem,@DVMFreeMem,@DVMSetLength,@GetVPBufferManager);
  if Result2 then begin
    SetLength(VisLibNames,L+1);
    VisLibNames[L]:=FileName;
  end;
end;

function LoadMuviStream(const FileName: string; var APresets: TPresets; var AKeyboards: TVisKeyboards; Append: Boolean = false): TMuviLoadResult;
var
  FS     : TFileStream;
  AFileID: TMuviFileID;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  AFileID:=ReadFileID(FS);
  if AFileID=Preset2FileID
    then Result:=LoadPresetsFromStream2(APresets,FS,Append,false)
    else if AFileID=KeyboardFileID
      then Result.Success:=TMuviSuccessCode(LoadKeyboardsFromStream(AKeyboards,FS,Append,false))
      else if AFileID=PresetFileID
        then Result:=LoadPresetsFromStream(APresets,FS,Append,false)
        else Result.Success:=scFail;
  FS.Free;
end;

function MultiLoadList(const FileName: string; var APresets: TPresets; var AKeyboards: TVisKeyboards; Append: Boolean = false): TMuviLoadResult;
var
  F        : System.Text;
  TempStr  : string;
begin
  AssignFile(F,FileName);
  Reset(F);
  ReadLn(F,TempStr);
  Result.Success:=scFail;
  repeat
    ReadLn(F,TempStr);
    if TempStr<>'' then if (TempStr[1]<>CommentChar) and (TempStr[1]<>IniBrackets) then begin
      if Result.Success<>scSuccess
        then Result:=MultiLoad(TempStr,APresets,AKeyboards,Append)
        else MultiLoad(TempStr,APresets,AKeyboards,Append);
    end;
  until EOF(F);
  CloseFile(F);
  if Result.Success<>scSuccess then Result.Success:=scFail;
end;

function TryLoadVis(const FileName: string): Byte;
var
  AExt   : string;
  Result2: Boolean absolute Result;
begin
  AExt:=LowerCase(ExtractFileExt(FileName));
  Result2:=((AExt=LibraryExt) or (AExt=MuviLibraryExt));
  if Result2 then Result:=DoLoadVis(FileName);
end;

function MultiLoad(const FileName: string; var APresets: TPresets; var AKeyboards: TVisKeyboards; Append: Boolean = false): TMuviLoadResult;
begin
  if not FileExists(FileName) then begin
    Result.Success:=scFileNotFound;
    exit;
  end;
  case TryLoadVis(FileName) of
    LoadVis_false : begin
        Result:=LoadMuviStream(FileName,APresets,AKeyboards,Append);
        if Result.Success=scFail
          then Result:=MultiLoadList(FileName,APresets,AKeyboards,Append);
      end;
    LoadVis_true  : Result.Success:=scSuccess;
    LoadVis_Exists: Result.Success:=scPluginExists;
  end;
end;

function LoadVis(const FileName: string): Boolean; overload;

  procedure LoadLibList;
  var
    F      : System.Text;
    TempStr: string;
  begin
    AssignFile(F,FileName);
    Reset(F);
    ReadLn(F,TempStr);
    if TempStr<>LibListHeader then begin
      Result:=false;
      exit;
    end;
    repeat
      ReadLn(F,TempStr);
      if TempStr<>''
        then if TempStr[1]<>CommentChar
          then DoLoadVis(TempStr);
    until EOF(F);
    CloseFile(F);
  end;

var
  AExt: string;
begin
  AExt:=LowerCase(ExtractFileExt(FileName));
  if (AExt=LibraryExt) or (AExt=MuviLibraryExt)
    then Result:=(DoLoadVis(FileName)=LoadVis_True)
    else LoadLibList;
end;

procedure SaveVis(const FileName: string);
var
  F: Text;
  I: Integer;
begin
  AssignFile(F,FileName);
  Rewrite(F);
  WriteLn(F,LibListHeader);
  for I:=0 to Length(VisLibNames)-1
    do WriteLn(F,VisLibNames[I]);
  CloseFile(F);
end;

procedure WS_DoNothing(var bmp: TBitmap32; const Source: ISpectrumData; const Visualisation: TVisualisation; const Params; var Workspace); stdcall;
begin
  //do nothing
end;

function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AParamDesc: TVisParamDesc; const AInitialValues; const AVisParamSize: Cardinal; const AOutputDesc: TVisOutputDesc; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil; const AC1Desc: ShortString = '1. Linienfarbe'; const AC2Desc: ShortString = '2. Linienfarbe'; const AC3Desc: ShortString = 'Hintergrundfarbe'): TVisType;
var
  AVisID: TVisID absolute ID;
begin
  with Result do begin
    Name:=AName;
    if AVisProc<>nil
      then VisProc:=AVisProc
      else VisProc:=@WS_DoNothing;
    VisParamDesc:=AParamDesc;
    OutputDesc:=AOutputDesc;
    VisParamSize:=AVisParamSize;
    WorkspaceSize:=AWorkspaceSize;
    C1Desc:=AC1Desc;
    C2Desc:=AC2Desc;
    C3Desc:=AC3Desc;
    VisID:=AVisID;
    if AInitWorkspace<>nil
      then InitWorkspace:=AInitWorkspace
      else InitWorkspace:=@WS_DoNothing;
    if AFreeWorkspace<>nil
      then FreeWorkspace:=AFreeWorkspace
      else FreeWorkspace:=@WS_DoNothing;
    if AFirstAction<>nil
      then FirstAction:=AFirstAction
      else FirstAction:=@WS_DoNothing;
    GetMem(InitialValues,AVisParamSize);
    Move(AInitialValues,InitialValues^,AVisParamSize);
  end;
end;

function VisType(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil): TVisType;
var
  I,L,AVisParamSize,PseudoParamCount: Integer;
  AVisParamDesc                     : TVisParamDesc;
  AVisOutputDesc                    : TVisOutputDesc;
  AC1Desc,AC2Desc,AC3Desc           : ShortString;
begin
  SetLength(AVisParamDesc,Length(AParamTypes));
  PseudoParamCount:=Length(AParamNames)-Length(AParamTypes);
  AVisParamSize:=0;
  for I:=0 to Length(AParamTypes)-1 do with AVisParamDesc[I] do begin
    Name:=AParamNames[I+PseudoParamCount];
    AType:=AParamTypes[I];
    AVisParamSize+=ParamTypeSize[AType];
  end;
  if PseudoParamCount>0 then begin
    AC1Desc:=AParamNames[0];
    if PseudoParamCount>1 then begin
      AC2Desc:=AParamNames[1];
      if PseudoParamCount>2
        then AC3Desc:=AParamNames[2]
        else AC3Desc:='Hintergrundfarbe';
    end else begin
      AC2Desc:='2. Linienfarbe';
      AC3Desc:='Hintergrundfarbe';
    end;
  end else begin
    AC1Desc:='1. Linienfarbe';
    AC2Desc:='2. Linienfarbe';
    AC3Desc:='Hintergrundfarbe';
  end;
  L:=Length(AChangeParams);
  for I:=0 to L-1 do begin
    //TempChangeParams:=AChangeParams[I];
    AVisParamDesc[I].Change:=AChangeParams[I];
    {if TempChangeParams<>nil
      then AVisParamDesc[I].Change:=TempChangeParams
      else AVisParamDesc[I].Change:=@WS_DoNothing;}
  end;
  for I:=L to Length(AParamTypes)-1 do AVisParamDesc[I].Change:=nil{@WS_DoNothing};

  SetLength(AVisOutputDesc,Length(AOutputTypes));
  for I:=0 to Length(AOutputTypes)-1 do with AVisOutputDesc[I] do begin
    Name:=AOutputNames[I];
    AType:=AOutputTypes[I];
  end;
  Result:=VisType_(AName,ID,AVisProc,AVisParamDesc,AInitialValues,AVisParamSize,AVisOutputDesc,AWorkspaceSize,AInitWorkspace,AFreeWorkspace,AFirstAction,AC1Desc,AC2Desc,AC3Desc);
end;

{function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AVisParamDesc: TVisParamDesc; const AInitialValues; const AVisParamSize: Cardinal; const AWorkspaceSize: Cardinal; const ChangeParamsProc: TWorkspaceproc = nil; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
var
  AVisID: TVisID absolute ID;
begin
  with Result do begin
    Name:=AName;
    VisProc:=AVisProc;
    VisParamDesc:=AVisParamDesc;
    VisParamSize:=AVisParamSize;
    WorkspaceSize:=AWorkspaceSize;
    VisID:=AVisID;
    if InitWorkspaceProc<>nil
      then InitWorkspace:=InitWorkspaceProc
      else InitWorkspace:=@WS_DoNothing;
    if FreeWorkspaceProc<>nil
      then FreeWorkspace:=FreeWorkspaceProc
      else FreeWorkspace:=@WS_DoNothing;
    if ChangeParamsProc<>nil
      then ChangeParams:=ChangeParamsProc
      else ChangeParams:=@WS_DoNothing;
    GetMem(InitialValues,AVisParamSize);
    Move(AInitialValues,InitialValues^,AVisParamSize);
  end;
end;}

{function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AVisParamDesc: TVisParamDesc; const AVisOutputDesc: TVisOutputDesc; const AInitialValues; const AVisParamSize: Cardinal; const AWorkspaceSize: Cardinal; const ChangeParamsProc: TWorkspaceproc = nil; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
var
  AVisID: TVisID absolute ID;
begin
  with Result do begin
    Name:=AName;
    VisProc:=AVisProc;
    VisParamDesc:=AVisParamDesc;
    VisParamSize:=AVisParamSize;
    WorkspaceSize:=AWorkspaceSize;
    VisID:=AVisID;
    if InitWorkspaceProc<>nil
      then InitWorkspace:=InitWorkspaceProc
      else InitWorkspace:=@WS_DoNothing;
    if FreeWorkspaceProc<>nil
      then FreeWorkspace:=FreeWorkspaceProc
      else FreeWorkspace:=@WS_DoNothing;
    if ChangeParamsProc<>nil
      then ChangeParams:=ChangeParamsProc
      else ChangeParams:=@WS_DoNothing;
    GetMem(InitialValues,AVisParamSize);
    Move(AInitialValues,InitialValues^,AVisParamSize);
    OutputDesc:=AVisOutputDesc;
  end;
end;

function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const AVisParamDesc: TVisParamDesc; const AInitialValues; const ChangeParamsProc: TWorkspaceproc = nil; const AWorkspaceSize: Cardinal = 0; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
var
  AVisParamSize: Cardinal;
  I            : Integer;
begin
  AVisParamSize:=0;
  for I:=0 to Length(AVisParamDesc)-1 do AVisParamSize+=ParamTypeSize[AVisParamDesc[I].AType];
  Result:=VisType_(AName,ID,AVisProc,AVisParamDesc,AInitialValues,AVisParamSize,AWorkspaceSize,ChangeParamsProc,InitWorkspaceProc,FreeWorkspaceProc);
end;

function VisType_(const AName: string; const ID; const AVisProc: TVisProc; const ANames: array of string; const ATypes: array of TVisParamType; const AInitialValues; const AVisParamSize: Cardinal; const AWorkspaceSize: Cardinal; const ChangeParamsProc: TWorkspaceproc = nil; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
var
  I            : Integer;
  AVisParamDesc: TVisParamDesc;
begin
  SetLength(AVisParamDesc,Length(ATypes));
  for I:=0 to Length(ATypes)-1 do with AVisParamDesc[I] do begin
    Name:=ANames[I];
    AType:=ATypes[I];
  end;
  Result:=VisType_(AName,ID,AVisProc,AVisParamDesc,AInitialValues,AVisParamSize,AWorkspaceSize,ChangeParamsProc,InitWorkspaceProc,FreeWorkspaceProc);
end;

function VisType(const AName: string; const ID; const AVisProc: TVisProc; const ANames: array of string; const ATypes: array of TVisParamType; const AInitialValues; const ChangeParamsProc: TWorkspaceproc = nil; const AWorkspaceSize: Cardinal = 0; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType; overload;
var
  I            : Integer;
  AVisParamDesc: TVisParamDesc;
begin
  SetLength(AVisParamDesc,Length(ATypes));
  for I:=0 to Length(ATypes)-1 do with AVisParamDesc[I] do begin
    Name:=ANames[I];
    AType:=ATypes[I];
  end;
  Result:=VisType_(AName,ID,AVisProc,AVisParamDesc,AInitialValues,ChangeParamsProc,AWorkspaceSize,InitWorkspaceProc,FreeWorkspaceProc);
end;}

{function VisType2(const AName: string; const ID; const AVisProc: TVisProc; const ANames: array of string; const ATypes: array of TVisParamType; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const ChangeParamsProc: TWorkspaceProc = nil; const AWorkspaceSize: Cardinal = 0; const InitWorkspaceProc: TWorkspaceProc = nil; const FreeWorkspaceProc: TWorkspaceProc = nil): TVisType;
var
  I,AVisParamSize: Integer;
  AVisParamDesc  : TVisParamDesc;
  AVisOutputDesc : TVisOutputDesc;
begin
  SetLength(AVisParamDesc,Length(ATypes));
  AVisParamSize:=0;
  for I:=0 to Length(ATypes)-1 do with AVisParamDesc[I] do begin
    Name:=ANames[I];
    AType:=ATypes[I];
    AVisParamSize+=ParamTypeSize[AType];
  end;
  //for I:=0 to Length(AVisParamDesc)-1 do AVisParamSize+=ParamTypeSize[AVisParamDesc[I].AType];
  SetLength(AVisOutputDesc,Length(AOutputTypes));
  for I:=0 to Length(AOutputTypes)-1 do with AVisOutputDesc[I] do begin
    Name:=AOutputNames[I];
    AType:=AOutputTypes[I];
  end;
  Result:=VisType_(AName,ID,AVisProc,AVisParamDesc,AVisOutputDesc,AInitialValues,AVisParamSize,AWorkspaceSize,ChangeParamsProc,InitWorkspaceProc,FreeWorkspaceProc);
end;}

{Allgemein - TVisType - Registrieren}

function RegisterVisTypeListElement(const AVisType: TVisType; var AList: TVisTypeList): Integer;
begin
  Result:=Length(AList);
  SetLength(AList,Result+1);
  if Result>0 then while AList[Result-1].VisID>AVisType.VisID do begin
    AList[Result]:=AList[Result-1];
    Dec(Result);
    if Result=0 then break;
  end;
  AList[Result]:=AVisType;
end;

procedure DestroyVisTypeListElement(const Index: Integer; var AList: TVisTypeList);
begin
  with AList[Index] do FreeMem(InitialValues,VisParamSize);
end;

procedure UnRegisterVisTypeListElement(const ID: Cardinal; var AList: TVisTypeList);
var
  L,I: Integer;
begin
  DestroyVisTypeListElement(ID,AList);
  L:=Length(AList);
  for I:=ID to L-2 do AList[I]:=AList[I+1];
  SetLength(AList,L-1);
end;

{procedure DestroyVisTypeList(var AList: TVisTypeList);
var
  I: Integer;
begin
  for I:=0 to Length(AList)-1 do DestroyVisType(I,AList);
  SetLength(AList,0);
end;}

procedure ClearVisTypeList(var AList: TVisTypeList);
var
  I: Integer;
begin
  for I:=0 to Length(AList)-1 do DestroyVisTypeListElement(I,AList);
  SetLength(AList,0);
end;

{Allgemein - TVisType - Registrieren - Visualisierungen}

function RegisterVis(const AVisType: TVisType): Integer;
begin
  Result:=RegisterVisTypeListElement(AVisType,FRegisteredVis);
  {Result:=Length(FRegisteredVis);
  SetLength(FRegisteredVis,Result+1);
  if Result>0 then while FRegisteredVis[Result-1].VisID>AVisType.VisID do begin
    FRegisteredVis[Result]:=FRegisteredVis[Result-1];
    Dec(Result);
    if Result=0 then break;
  end;
  FRegisteredVis[Result]:=AVisType;}
end;

procedure CreateAndRegisterVis(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil); stdcall;
begin
  RegisterVis(VisType(AName,ID,AVisProc,AParamNames,AParamTypes,AChangeParams,AInitialValues,AOutputNames,AOutputTypes,AWorkspaceSize,AInitWorkspace,AFreeWorkspace));
end;

procedure CreateAndRegisterVis2(const AName: string; const ID; const AVisProc: TVisProc; const AParamNames: array of string; const AParamTypes: array of TVisParamType; const AChangeParams: array of TVisProc; const AInitialValues; const AOutputNames: array of string; const AOutputTypes: array of TVisOutputType; const AWorkspaceSize: Cardinal = 0; const AInitWorkspace: TVisProc = nil; const AFreeWorkspace: TVisProc = nil; const AFirstAction: TVisProc = nil); stdcall;
begin
  RegisterVis(VisType(AName,ID,AVisProc,AParamNames,AParamTypes,AChangeParams,AInitialValues,AOutputNames,AOutputTypes,AWorkspaceSize,AInitWorkspace,AFreeWorkspace,AFirstAction));
end;

procedure UnRegisterVis(const ID: Cardinal);
//var
//  L,I: Integer;
begin
  UnRegisterVisTypeListElement(ID,FRegisteredVis);
  {DestroyVisType(ID);
  L:=Length(FRegisteredVis);
  for I:=ID to L-2 do FRegisteredVis[I]:=FRegisteredVis[I+1];
  SetLength(FRegisteredVis,L-1);}
end;

procedure DestroyVisType(const Index: Integer);
begin
  DestroyVisTypeListElement(Index,FRegisteredVis);
  //with FRegisteredVis[Index] do FreeMem(InitialValues,VisParamSize);
end;

{procedure DestroyVisTypes;
var
  I: Integer;
begin
  for I:=0 to Length(FRegisteredVis)-1 do DestroyVisType(I);
end;}

procedure ClearVis;
begin
  ClearVisTypeList(FRegisteredVis);
  {DestroyVisTypes;
  SetLength(FRegisteredVis,0);}
end;

function RegVisCount: Integer;
begin
  Result:=Length(FRegisteredVis);
end;

function Visualisations(const Index: Integer): TVisType;
begin
  Result:=FRegisteredVis[Index];
end;

{Allgemein - TVisType - Registrieren - Triggerelemente}

{function RegisterTrig(const AVisType: TVisType): Integer;
begin
  RegisterVisTypeListElement(AVisType,FRegisteredTrig);
end;

procedure UnRegisterTrig(const ID: Cardinal);
begin
  UnRegisterVisTypeListElement(ID,FRegisteredTrig);
end;

procedure DestroyTrigType(const Index: Integer);
begin
  DestroyVisTypeListElement(Index,FRegisteredTrig);
end;

procedure ClearTrig;
begin
  ClearVisTypeList(FRegisteredTrig);
end;

function RegTrigCount: Integer;
begin
  Result:=Length(FRegisteredTrig);
end;

function Triggers(const Index: Integer): TVisType;
begin
  Result:=FRegisteredTrig[Index];
end;}

{Allgemein - TVisualisation}

function FindID(const VisID): Cardinal;
var
  VisID2    : TVisID absolute VisID;
  AVisID    : TVisID;
  First,Last: Cardinal;
begin
  //binäre Suche
  First:=0;
  Last:=Length(FRegisteredVis)-1;
  Result:=Last div 2;
  while Last-First>1 do begin
    AVisID:=FRegisteredVis[Result].VisID;
    if AVisID>VisID2
      then Last:=Result
      else if AVisID<VisID2
        then First:=Result
        else exit;
    Result:=(First+Last) div 2;
  end;
  if FRegisteredVis[First].VisID=VisID2
    then Result:=First
    else if FRegisteredVis[Last].VisID=VisID2
      then Result:=Last
      else Result:=0;
  //lineare Version
  {for First:=0 to Length(FRegisteredVis)-1 do begin
    if FRegisteredVis[First].VisID=VisID2 then begin
      Result:=First;
      exit;
    end;
  end;
  Result:=0}
end;

function CreateVis_NoInit(const Index: Cardinal): TPresetVis;
begin
  with Result do begin
    VisID:=Index;
    with FRegisteredVis[Index] do begin
      GetMem(VisParams,VisParamSize);
      SetLength(VisOutputs,Length(OutputDesc));
      //GetMem(Workspace,WorkspaceSize);
    end;
  end;
end;

function CreateVis_ID_NoInit(const VisID): TPresetVis;
begin
  Result:=CreateVis_NoInit(FindID(VisID));
end;

function CreateVis(const Index: Cardinal): TPresetVis;
var
  I: Integer;
begin
  Result:=CreateVis_NoInit(Index);
  with Result do begin
    C1:=$FFFF0000;
    C2:=$FF00FF00;
    C3:=$FF0000FF;
    with FRegisteredVis[Index] do begin
      Move(InitialValues^,VisParams^,VisParamSize);
      //if Assigned(InitWorkspace) then InitWorkspace(Result,Workspace^);
    end;
    DesignPos:=Point(0,0);
    for I:=0 to Length(VisOutputs)-1 do VisOutputs[I].Param:=-2;
  end;
end;

function CreateVis_ID(const VisID): TPresetVis;
begin
  Result:=CreateVis(FindID(VisID));
end;

procedure DestroyVis(var Visualisation: TPresetVis);
begin
  with Visualisation do begin
    with FRegisteredVis[VisID] do begin
      //if Assigned(FreeWorkspace) then FreeWorkspace(Visualisation,Workspace^);
      FreeMem(VisParams,VisParamSize);
      SetLength(VisOutputs,0);
      //FreeMem(Workspace,WorkspaceSize);
    end;
    VisParams:=nil;
  end;
end;


{procedure SetVisParams(var Visualisation: TVisualisation; const Params);
begin
  with FRegisteredVis[Visualisation.VisID] do begin
    Move(Params,Visualisation.VisParams^,VisParamSize);
    //if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);
  end;
end;

procedure SetVisParam(var Visualisation: TVisualisation; const ParamIndex: Cardinal; const Params); overload;
begin
  with FRegisteredVis[Visualisation.VisID] do begin
    Move(Params,ParamPos(Visualisation,ParamIndex)^,VisParamSize);
    VisParamDesc[ParamIndex].Change();
    //if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);
  end;
end;
}

{procedure SetVisParam(const Visualisation: TVisualisation; const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
  with FRegisteredVis[Visualisation.VisID]
    do if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);
end;}

{
procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
end;

procedure VisParamsChanged(var Bmp: TBitmap32; const Source: ISpectrumData; const Visualisation: TVisualisation);
begin
  with FRegisteredVis[Visualisation.VisID]
    do if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);
end;
}

{
procedure SetVisParams(var Visualisation: TVisualisation; const ParamIndex: Integer; const Params);
begin
  with FRegisteredVis[Visualisation.VisID]
    do Move(Params,ParamPos(Visualisation,ParamIndex)^,VisParamSize);
end;

procedure SetVisParams(const Dest: TVisParam; const Source; const Size: Cardinal);
begin
  Move(Source,Dest^,Size);
end;

procedure VisParamChanged(var bmp: TBitmap32; const Source: ISpectrumData; const Visualisation: TVisualisation; const ParamIndex: Cardinal);
begin
  FRegisteredVis[Visualisation.VisID].VisParamDesc[ParamIndex].Change(bmp,Source,Visualisation,Visualisation.VisParams^,Visualisation.Workspace^);
end;
}

procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
end;

procedure GetVisParams(const Visualisation: TVisualisation; out Params);
begin
  Move(Visualisation.VisParams^,Params,FRegisteredVis[Visualisation.VisID].VisParamSize);
end;

procedure GetVisParam(const Visualisation: TVisualisation; const ParamIndex: Cardinal; out Params);
begin
  Move(ParamPos(Visualisation,ParamIndex)^,Params,FRegisteredVis[Visualisation.VisID].VisParamSize);
end;

procedure GetVisParam(const Source: TVisParam; out Dest; const Size: Cardinal); overload;
begin
  Move(Source^,Dest,Size);
end;

function ParamPos(const Visualisation: TVisualisation; const Index: Integer): TVisParam;

  procedure DoGetParamPos(var Params);
    var
      I: Integer;
  begin
    Result:=@Params;
    with FRegisteredVis[Visualisation.VisID]
      do for I:=0 to Index-1
        do Result+=ParamTypeSize[VisParamDesc[I].AType];
  end;

begin
  DoGetParamPos(Visualisation.VisParams^);
end;

function ParamPos(const Visualisation: TVisualisation; const Index,LastIndex: Integer; const LastParam: TVisParam): TVisParam;
var
  I: Integer;
begin
  Result:=LastParam;
  with FRegisteredVis[Visualisation.VisID]
    do for I:=LastIndex to Index-1
      do Result+=ParamTypeSize[VisParamDesc[I].AType];
end;


procedure SetVisParams(var Visualisation: TPresetVis; const Params);
begin
  with FRegisteredVis[Visualisation.VisID] do begin
    Move(Params,Visualisation.VisParams^,VisParamSize);
    //if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);
  end;
end;

procedure SetVisParam(var Visualisation: TPresetVis; const ParamIndex: Cardinal; const Params); overload;
begin
  with FRegisteredVis[Visualisation.VisID] do begin
    Move(Params,ParamPos(Visualisation,ParamIndex)^,VisParamSize);
    //if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);
  end;
end;

procedure SetVisParam(const Visualisation: TPresetVis; const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
  {with FRegisteredVis[Visualisation.VisID]
    do if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);}
end;

{procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
end; }

(*procedure VisParamsChanged(const Visualisation: TPresetVis);
begin
  {with FRegisteredVis[Visualisation.VisID]
    do if Assigned(ChangeParams) then ChangeParams(Visualisation,Visualisation.Workspace^);}
end;*)

procedure GetVisParams(const Visualisation: TPresetVis; out Params);
begin
  Move(Visualisation.VisParams^,Params,FRegisteredVis[Visualisation.VisID].VisParamSize);
end;

procedure GetVisParam(const Visualisation: TPresetVis; const ParamIndex: Cardinal; out Params);
begin
  Move(ParamPos(Visualisation,ParamIndex)^,Params,FRegisteredVis[Visualisation.VisID].VisParamSize);
end;

{procedure GetVisParam(const Source: TVisParam; out Dest; const Size: Cardinal); overload;
begin
  Move(Source^,Dest,Size);
end;}

function ParamPos(const Visualisation: TPresetVis; const Index: Integer): TVisParam;

  procedure DoGetParamPos(var Params);
    var
      I: Integer;
  begin
    Result:=@Params;
    with FRegisteredVis[Visualisation.VisID]
      do for I:=0 to Index-1
        do Result+=ParamTypeSize[VisParamDesc[I].AType];
  end;

begin
  DoGetParamPos(Visualisation.VisParams^);
end;

function ParamPos(const Visualisation: TPresetVis; const Index,LastIndex: Integer; const LastParam: TVisParam): TVisParam;
var
  I: Integer;
begin
  Result:=LastParam;
  with FRegisteredVis[Visualisation.VisID]
    do for I:=LastIndex to Index-1
      do Result+=ParamTypeSize[VisParamDesc[I].AType];
end;


{procedure SetVisOutput(const Visualisation: TVisualisation; const Value; const Size,Index: Integer);
begin
  Move(Value,Visualisation.VisOutputs[Index]^,Size);
end;

procedure SetVisOutput2(const Visualisation: TVisualisation; const Value; const Index: Integer);
var
  AOutput: TVisOutput;
begin
  AOutput:=Visualisation.VisOutputs[Index];
  if AOutput<>nil then Move(Value,AOutput^,OutputTypeSize[FRegisteredVis[Visualisation.VisID].OutputDesc[Index].AType]);
  //FRegisteredVis[]
end;}

function VisOutputOffset(const Visualisation: TPresetVis; const Index: Integer): TVisInputPos;

  procedure DoGetParamPos(var Params);
  var
    I: Integer;
  begin
    Result:=NilStart;
    with FRegisteredVis[Visualisation.VisID]
      do for I:=0 to Index-1
        do Result+=ParamTypeSize[VisParamDesc[I].AType];
  end;

begin
  DoGetParamPos(Visualisation.VisParams^);
end;

{function InOutFits(const Preset: TPreset; const OutLayer,Output,InLayer,Param: Integer): Boolean;
begin

end;}

procedure SetVisOutput(var Preset: TPreset; const AOutLayer,AOutput,AInLayer,AParam: Integer); overload;
var
  InLayer,OutLayer: PPresetVis;
begin
  if AOutLayer>=0
    then OutLayer:=@Preset.Layers[AOutLayer]
    else OutLayer:=@Preset.Triggers[not AOutLayer];
  if AInLayer>=0
    then InLayer:=@Preset.Layers[AInLayer]
    else InLayer:=@Preset.Triggers[not AInLayer];
  with OutLayer^.VisOutputs[AOutput] do begin
    Layer:=AInLayer;
    Param:=AParam;
    if AParam>=3 then Offset:=VisOutputOffset(InLayer^,AParam-3);
    //ShowMessage('Output '+IntToStr(AOutLayer)+';'+IntToStr(AOutput)+' Connected to Input '+IntToStr(AInLayer)+';'+IntToStr(AParam)+' Offset: '+IntToStr(Integer(Offset)));
  end;
end;

procedure InitKeyboard(var AKeyboard: TVisKeyboard; const AName: string);
var
  I: Char;
begin
  AKeyboard.Name:=AName;
  for I:=#0 to #255 do with AKeyboard.Keys[I] do begin
    KeyType:=ktNothing;
    Value:=0;
    BackChange:=false;
  end;
end;

procedure RemoveKeyboard(var AKeyboards: TVisKeyboards; const Index: Integer);
var
  I,L: Integer;
begin
  L:=Pred(Length(AKeyboards));
  for I:=Index to Pred(L)
    do AKeyboards[I]:=AKeyboards[succ(I)];
  SetLength(AKeyboards,L);
end;

function ReadFileID(Stream: TStream): TMuviFileID;
begin
  if Stream.Size<GenerelMuviFileIDSize then begin
    Result:='';
    exit;
  end;
  Stream.Read(Result,GenerelMuviFileIDSize);
end;

procedure SaveKeyboardsToStream(const Keyboards: TVisKeyboards; Stream: TStream);
var
  I,L: Integer;
begin
  Stream.Write(KeyboardFileID,KeyboardFileIDSize);
  L:=Length(Keyboards);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with Keyboards[I] do begin
    SaveText(Name,Stream);
    Stream.Write(Keys,SizeOf(Keys));
  end;
end;

function LoadKeyboardsFromStream(var Keyboards: TVisKeyboards; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): Boolean;
var
  I,L,Offset: Integer;
  //AFileID   : array [1..KeyboardFileIDSize-1] of Char;
begin
  if ReadHeader then begin
    {if Stream.Size<KeyboardFileIDSize then begin
      Result:=false;
      exit;
    end;
    Stream.Read(AFileID,KeyboardFileIDSize);
    Result:=(AFileID=KeyboardFileID);}
    Result:=(ReadFileID(Stream)=KeyboardFileID);
    if not Result then exit;
  end else Result:=true;
  if Append then begin
    Offset:=Length(Keyboards);
  end else begin
    Offset:=0;
    SetLength(Keyboards,0);
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(Keyboards,L);
  for I:=Offset to L-1 do with Keyboards[I-Offset] do begin
    LoadText(Name,Stream);
    Stream.Read(Keys,SizeOf(Keys));
  end;
end;

procedure SaveKeyboardsToFile(const Keyboards: TVisKeyboards; const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SaveKeyboardsToStream(Keyboards,FS);
  FS.Free;
end;

function LoadKeyboardsFromFile(var Keyboards: TVisKeyboards; const FileName: string; const Append: Boolean = false): Boolean;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadKeyboardsFromStream(Keyboards,FS,Append);
  FS.Free;
end;


procedure SetGlobalWaitTime(Value: Integer);
begin
  GlobalVisThread.FWaitTime:=Value;
end;

function GetGlobalWaitTime: Integer;
begin
  Result:=GlobalVisThread.FWaitTime;
end;

function GetGlobalVisPriority: TThreadPriority;
begin
  Result:=GlobalVisThread.Priority;
end;

procedure SetGlobalVisPriority(const Value: TThreadPriority);
begin
  GlobalVisThread.Priority:=Value;
end;

procedure SetGlobalVisedEvent(const Value: TVisedEvent);
begin
  GlobalVisThread.OnVised:=Value;
end;

procedure DVMGetMem(var P: Pointer; const Size: Cardinal);
begin
  GetMem(P,Size);
end;

procedure DVMFreeMem(var P: Pointer; const Size: Cardinal);
begin
  FreeMem(P,Size);
end;

type
  TArrByteArray = array [0..MaxPtrArray] of Byte;
  PArrByteArray = ^TArrByteArray;

procedure DVMSetLength(var Arr: TVisArray; const NewLength,OldLength,ElemSize: Cardinal);
var
  NewArr         : TVisArray;
  OldSize,NewSize: Cardinal;

  procedure ZeroNewArr(const Start: Cardinal);
  var
    I   : Cardinal;
    Arr2: PArrByteArray absolute NewArr;
  begin
    for I:=Start to NewSize-1 do Arr2^[I]:=0;
  end;

begin
  if NewLength=OldLength then exit;
  NewSize:=NewLength*ElemSize;
  if NewLength>0
    then GetMem(NewArr,NewSize)
    else NewArr:=nil;
  if OldLength>0 then begin
    OldSize:=OldLength*ElemSize;
    if NewLength>OldLength then begin
      Move(Arr^,NewArr^,OldSize);
      ZeroNewArr(OldSize);
    end else Move(Arr^,NewArr^,NewSize);
    FreeMem(Arr,OldSize);
  end else ZeroNewArr(0);
  Arr:=NewArr;
end;

var
  LVPBufferManager: TVPBufferManager;

function GetVPBufferManager: IVPBufferManager; stdcall;
begin
  Result:=IVPBufferManager(LVPBufferManager);
end;

procedure UnloadPlugins;
begin
  FreeLibrarys;
end;


function RealBufferMiddle(const ABuffer: vpRealBuffer): vpReal;
var
  I       : Integer;
  ABuffer2: vpRealBuffer_ absolute ABuffer;
  AValPos : Pointer;
  AVal    : ^vpRealBufferElem absolute AValPos;
begin
  Result:=0.0;
  if ABuffer2.Size<1 then exit;
  AValPos:=ABuffer2.Dest;
  for I:=0 to ABuffer2.Size-1 do begin
    Result+=AVal^;
    AValPos+=SizeOf(vpRealBufferElem);
  end;
  Result/=ABuffer2.Size;
end;

{procedure FreeGlobalVis;
begin
  GlobalVisThread.Terminate;
  //GlobalVisThread.WaitFor;
end;}

//var
//  DefaultVisPath: string;

initialization
  LVPBufferManager:=TVPBufferManager.Create;
  AssignProcs([@CreateAndRegisterVis,@GetVisParams,@DVMGetMem,@DVMFreeMem,@DVMSetLength,nil,nil,nil,nil,nil,nil,@GetVPBufferManager,@CreateAndRegisterVis2]);
  //AssignProcs(@VisType,@GetVisParams);
  //AdvancedAssignProcs([@TrigCall_,@VisType2,@SetVisOutput,@TrigCall,@SetVisOutput2]);
  GlobalVisThread:=TGlobalVisThread.Create;
  //DefaultVisPath:=ExtractFilePath(Application.ExeName)+DefaultVisFileName;
  //if FileExists(DefaultVisPath) then LoadVis(DefaultVisPath);
finalization
  //SaveVis(DefaultVisPath);
  GlobalVisThread.Terminate;
  ClearVis;
  LVPBufferManager.Destroy;
  //ClearTrig;
end.

