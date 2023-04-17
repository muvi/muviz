unit VisDrawUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpectrumData, Forms, AdvFunc, VisType, Dialogs,
  VisTypeUnit, MCanvasUnit, AdvType, VPBuffers, PluginType, ExtPtrType,
  AdvClasses, AdvThreads, LCLIntf, MFileUtil;

const
  ANDColor          = $00FFFFFF;
  ANDAlpha          = $FF000000;
  SHLAlpha          = 24;

type
  TVisThread        = class (TObject)
  private
    ChangeVis       : TEmptyEvent;
    FNewVis         : TVisComposition;
    FChangeSucceeded: Boolean;
    FSuspendCount   : Integer;
    procedure DoChangeVis;
    procedure DoNotChangeVis;
    function GetSuspended: Boolean;
  protected
    FComposition   : TVisComposition;
    procedure DoExecute;
    //DoDoExecute nie direkt aufrufen!
    //stattdessen: FCanvas.FrameStart(@DoDoExecute)
    procedure DoDoExecute;
  public
    FSpectrumData  : ISpectrumData;
    FCanvas        : TMCanvas;
    FWaitTime      : Integer;
    constructor Create(ACanvas: TMCanvas; ASpectrumData: ISpectrumData; AVis: TVisComposition);
    destructor Destroy; override;
    function SetVis(AVis: TVisComposition): Boolean;
    procedure Suspend;
    procedure Resume;
    procedure WaitForChange;

    property Composition: TVisComposition read FComposition;
    property Suspended: Boolean read GetSuspended;
  end;

  TVisThread2       = class (TFPSThread)
  private
    ChangeVis       : TEmptyEvent;
    FNewVis         : TVisComposition;
    FChangeSucceeded: Boolean;
    FComposition    : TVisComposition;
    FSpectrumData   : ISpectrumData;
    FCanvas         : TMCanvas;
    FStarting       : Boolean;
    procedure DoChangeVis;
    procedure DoNotChangeVis;
  protected
    procedure DoExecute; override;
    function GetTime: LongWord; override;
  public
    constructor Create(ACanvas: TMCanvas; ASpectrumData: ISpectrumData; AVis: TVisComposition);
    destructor Destroy; override;
    function SetVis(AVis: TVisComposition): Boolean;
    procedure WaitForChange;
    //bestätigt, dass Canvas.FrameStart() erfolgreich ausgeführt wurde
    //wird normalerweise vom Canvas aufgerufen
    procedure FrameStarted;

    property Composition: TVisComposition read FComposition;
  end;

function LoadMuviStream(const FileName: string; APresets: TPresets; var AKeyboards: TVisKeyboards; Append: Boolean = false): TMuviLoadResult;

procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
procedure GetVisParam(const Source: TVisParam; out Dest; const Size: Cardinal); overload;

procedure SetVisParams(var Visualisation: TPresetVis; const Params); overload;
procedure SetVisParam(var Visualisation: TPresetVis; const ParamIndex: Cardinal; const Params); overload;
procedure SetVisParam(const Visualisation: TPresetVis; const Dest: TVisParam; const Source; const Size: Cardinal); overload;
procedure GetVisParams(const Visualisation: TPresetVis; out Params); overload;
procedure GetVisParam(const Visualisation: TPresetVis; const ParamIndex: Cardinal; out Params); overload;
function ParamPos(const Visualisation: TPresetVis; const Index: Integer): TVisParam; overload;
function NextParamPos(const Visualisation: TPresetVis; Index: Integer; LastParam: TVisParam): TVisParam;
function NextParamPosCCC(const Visualisation: TPresetVis; Index: Integer; LastParam: TVisParam): TVisParam;

function VisOutputOffset(const Visualisation: TPresetVis; const Index: Integer): TVisInputPos;
procedure SetVisOutput(var Preset: TPreset; AOutLayer,AOutput,AInLayer,AParam: Integer); overload;
procedure SetVisOutput(var Preset: TPreset; const AOutput,AParam: TPresetOutputInfo); overload;

procedure InitKeyboard(var AKeyboard: TVisKeyboard; const AName: string);
procedure RemoveKeyboard(var AKeyboards: TVisKeyboards; const Index: Integer);
procedure SaveKeyboardsToStream(const Keyboards: TVisKeyboards; Stream: TStream);
function LoadKeyboardsFromStream(var Keyboards: TVisKeyboards; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TLoadFileResult;
procedure SaveKeyboardsToFile(const Keyboards: TVisKeyboards; const FileName: string);
function LoadKeyboardsFromFile(var Keyboards: TVisKeyboards; const FileName: string; const Append: Boolean = false): TLoadFileResult;

function GlobalVisIsSlow: Boolean;
function GetGlobalVisFPS: Double;
procedure SetGlobalVisFPS(Value: Double);
function GetGlobalVisActiveFPS: Double;
function GetGlobalVisPriority: TThreadPriority;
procedure SetGlobalVisPriority(Value: TThreadPriority);
procedure SetGlobalVisedEvent(Value: TEmptyEvent);
function GetGlobalVisTaskCount: Cardinal;
function GetGlobalVisBGThread: TVisBGThread;

const
  ParamNamePrefix  = 'Parameter ';
  OutputNamePrefix = 'Output ';
  //OPNamePrefix[IsOutput]
  OPNamePrefix: array [Boolean] of string = (ParamNamePrefix,OutputNamePrefix);

implementation

const
  NilStart = TVisInputPos(0);

{TGlobalVisThread}

type
  TVisTData        = record
    Vis : TVisThread;
    Next: Pointer;
  end;
  PVisTData        = ^TVisTData;

  TGlobalVisThread = class (TFPSThread)
  private
    FVisProcs      : PVisTData;
    FAThread       : TVisThread;
    FAction        : TEmptyEvent;
    FActionExecuted: Boolean;
    FOnVised       : TEmptyEvent;
    FBGThread      : TVisBGThread;
    FFinished      : Boolean;
    procedure SetOnVised(const Value: TEmptyEvent);
    procedure VisedDoNothing;
  protected
    procedure DoExecute; override;
    function GetTime: LongWord; override;
    procedure DoAdd;
    procedure DoRemove;
    procedure DoNothing;
  public
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    procedure Add(const VisThread: TVisThread);
    procedure Remove(const VisThread: TVisThread);
    procedure Clear;

    property BGThread: TVisBGThread read FBGThread;
  published
    property OnVised: TEmptyEvent read FOnVised write SetOnVised;
  end;

const
  VisTDataSize = SizeOf(TVisTData);

constructor TGlobalVisThread.Create;
begin
  inherited Create;
  FOnVised:=@VisedDoNothing;
  FVisProcs:=nil;
  FreeOnTerminate:=false;
  FAction:=@DoNothing;
  FBGThread:=TVisBGThread.Create;
  FFinished:=false;
  Resume;
end;

destructor TGlobalVisThread.Destroy;
begin
  Terminate;
  WaitForFinish;
  Clear;
  FBGThread.Destroy;
  inherited Destroy;
end;

procedure TGlobalVisThread.DoExecute;
var
  NextData: PVisTData;
begin
  BGThread.SyncFinish;
  NextData:=FVisProcs;
  while NextData<>nil do with NextData^ do begin
    Vis.DoExecute;
    NextData:=Next;
  end;
  //hier ggf. Visualisierungsprozeduren hinzufügen/löschen
  FAction;
  FOnVised;
end;

function TGlobalVisThread.GetTime: LongWord;
begin
  Result:=GetTickCount;
end;

procedure TGlobalVisThread.VisedDoNothing;
begin
  //do nothing
end;

procedure TGlobalVisThread.SetOnVised(const Value: TEmptyEvent);
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

procedure TGlobalVisThread.DoRemove;
var
  NextData   : PVisTData;
  PtrNextData: ^PVisTData;
begin
  NextData:=FVisProcs;
  PtrNextData:=@FVisProcs;
  while NextData^.Vis<>FAThread do begin
    with NextData^ do begin
      PtrNextData:=@Next;
      NextData:=Next;
    end;
  end;
  PtrNextData^:=NextData^.Next;
  FreeMem(NextData,VisTDataSize);

  FActionExecuted:=true;
  FAction:=@DoNothing;
end;

procedure TGlobalVisThread.DoNothing;
begin
  //do nothing
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

constructor TVisThread.Create(ACanvas: TMCanvas; ASpectrumData: ISpectrumData; AVis: TVisComposition{var APreset: TPreset});
begin
  inherited Create;
  FWaitTime:=25;
  FCanvas:=ACanvas;
  FSpectrumData:=ASpectrumData;
  FChangeSucceeded:=true;

  FComposition:=AVis;
  ChangeVis:=@DoNotChangeVis;
  FCanvas.Reset(@DoDoExecute);
  FSuspendCount:=0;
  GlobalVisThread.Add(Self);
end;

destructor TVisThread.Destroy;
begin
  if FSuspendCount<=0 then GlobalVisThread.Remove(Self);
  //TVisComposition.Free gibt das Preset nur dann frei, wenn es nicht mehr
  //Element z.B. einer TVisCompositions Liste ist
  FComposition.Free;
  FSpectrumData:=nil;
  inherited Destroy;
end;

procedure TVisThread.Suspend;
begin
  if FSuspendCount=0
    then GlobalVisThread.Remove(Self);
  Inc(FSuspendCount);
end;

procedure TVisThread.Resume;
begin
  Dec(FSuspendCount);
  if FSuspendCount=0
    then GlobalVisThread.Add(Self);
end;

function TVisThread.GetSuspended: Boolean;
begin
  Result:=(FSuspendCount>0);
end;

procedure TVisThread.WaitForChange;
begin
  while not FChangeSucceeded do Application.ProcessMessages;
end;

procedure TVisThread.DoChangeVis;
begin
  //TVisComposition.Free gibt das Preset nur dann frei, wenn es nicht mehr
  //Element z.B. einer TVisCompositions Liste ist
  FComposition.Free;
  FComposition:=FNewVis;
  ChangeVis:=@DoNotChangeVis;
  FChangeSucceeded:=true;
end;

procedure TVisThread.DoNotChangeVis;
begin
  //do nothing
end;

function TVisThread.SetVis(AVis: TVisComposition): Boolean;
begin
  //beseitigt auch Fehler beim setzen des gleichen Presets direkt nach Wechsel
  //von Buffer Mode Reuse auf Manual...
  //behindert das neu setzen von Presets nicht, weil in dem Fall AVis neu
  //generiert worden wäre und eine andere Addresse hätte
  if AVis=FComposition then exit;
  Result:=(FChangeSucceeded);
  if not Result then exit;
  FChangeSucceeded:=false;
  FNewVis:=AVis;
  ChangeVis:=@DoChangeVis;
end;

procedure TVisThread.DoExecute;
begin
  FCanvas.FrameStart;
end;

procedure TVisThread.DoDoExecute;
begin
  FComposition.Visualize;
  FCanvas.FrameEnd;
  //hier ggf. Preset wechseln
  ChangeVis;
end;

{TVisThread2}

constructor TVisThread2.Create(ACanvas: TMCanvas; ASpectrumData: ISpectrumData; AVis: TVisComposition);
begin
  inherited Create;
  FCanvas:=ACanvas;
  FSpectrumData:=ASpectrumData;
  FComposition:=AVis;
  FStarting:=false;
  FChangeSucceeded:=true;
  ChangeVis:=@DoNotChangeVis;
  FCanvas.Reset(@FrameStarted);
  FPS:=30.0;
  Start;
end;

destructor TVisThread2.Destroy;
begin
  Terminate;
  WaitForFinish;
  FComposition.Free;
  FSpectrumData:=nil;
  inherited Destroy;
end;

procedure TVisThread2.DoChangeVis;
begin
  //TVisComposition.Free gibt das Preset nur dann frei, wenn es nicht mehr
  //Element z.B. einer TVisCompositions Liste ist
  FComposition.Free;
  FComposition:=FNewVis;
  ChangeVis:=@DoNotChangeVis;
  FChangeSucceeded:=true;
end;

procedure TVisThread2.DoNotChangeVis;
begin
  //do nothing
end;

procedure TVisThread2.DoExecute;
begin
  {if FStarting then begin
    Meldung('',TdMInfo);
    exit;
  end;}
  FStarting:=true;
  FCanvas.FrameStart;
  //while FStarting and (not Terminated) do ;
  //  Suspend;
end;

function TVisThread2.GetTime: LongWord;
begin
  Result:=GetTickCount;
end;

procedure TVisThread2.FrameStarted;
begin
  if not Terminated then begin
    FComposition.Visualize;
    FCanvas.FrameEnd;
    //hier ggf. Preset wechseln
    ChangeVis;
  end;
  FStarting:=false;
  //Resume;
end;

function TVisThread2.SetVis(AVis: TVisComposition): Boolean;
begin
  //beseitigt auch Fehler beim setzen des gleichen Presets direkt nach Wechsel
  //von Buffer Mode Reuse auf Manual...
  //behindert das neu setzen von Presets nicht, weil in dem Fall AVis neu
  //generiert worden wäre und eine andere Addresse hätte
  if AVis=FComposition then exit;
  Result:=(FChangeSucceeded);
  if not Result then exit;
  FChangeSucceeded:=false;
  FNewVis:=AVis;
  ChangeVis:=@DoChangeVis;
end;

procedure TVisThread2.WaitForChange;
begin
  while not FChangeSucceeded do Application.ProcessMessages;
end;

{Allgemein - TVisType - DLLs}

function LoadMuviStream(const FileName: string; APresets: TPresets; var AKeyboards: TVisKeyboards; Append: Boolean = false): TMuviLoadResult;
var
  FS     : TFileStream;
  AFileID: TMuviFileID;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  AFileID:=ReadFileID(FS);

  if AFileID=Preset4FileID
    then Result:=APresets.LoadFromStream4(FS,Append,false)
    else if AFileID=Preset3FileID
      then Result:=APresets.LoadFromStream3(FS,Append,false)
      else if AFileID=Preset2FileID
        then Result:=APresets.LoadFromStream2(FS,Append,false)
        else if AFileID=KeyboardFileID
          then Result.Success:=LoadKeyboardsFromStream(AKeyboards,FS,Append,false)
          else if AFileID=Preset1FileID
            then Result:=APresets.LoadFromStream1(FS,Append,false)
            else Result.Success:=lplInvalidHeader;
  FS.Free;
end;

{Allgemein - TVisualisation}

procedure SetVisParam(const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
end;

procedure GetVisParam(const Source: TVisParam; out Dest; const Size: Cardinal); overload;
begin
  Move(Source^,Dest,Size);
end;

procedure SetVisParams(var Visualisation: TPresetVis; const Params);
begin
  with TVisualisation.Visualisations(Visualisation.VisID) do begin
    Move(Params,Visualisation.VisParams^,VisParamSize);
  end;
end;

procedure SetVisParam(var Visualisation: TPresetVis; const ParamIndex: Cardinal; const Params); overload;
begin
  with TVisualisation.Visualisations(Visualisation.VisID) do begin
    Move(Params,ParamPos(Visualisation,ParamIndex)^,VisParamSize);
  end;
end;

procedure SetVisParam(const Visualisation: TPresetVis; const Dest: TVisParam; const Source; const Size: Cardinal); overload;
begin
  Move(Source,Dest^,Size);
end;

procedure GetVisParams(const Visualisation: TPresetVis; out Params);
begin
  Move(Visualisation.VisParams^,Params,TVisualisation.Visualisations(Visualisation.VisID).VisParamSize);
end;

procedure GetVisParam(const Visualisation: TPresetVis; const ParamIndex: Cardinal; out Params);
begin
  Move(ParamPos(Visualisation,ParamIndex)^,Params,TVisualisation.Visualisations(Visualisation.VisID).VisParamSize);
end;

function ParamPos(const Visualisation: TPresetVis; const Index: Integer): TVisParam;

  procedure DoGetParamPos(var Params);
    var
      I: Integer;
  begin
    Result:=@Params;
    with TVisualisation.Visualisations(Visualisation.VisID)
      do for I:=0 to Index-1
        do Result+=VisParamTypes.SizeOfParam(VisParamDesc[I].AType);
  end;

begin
  DoGetParamPos(Visualisation.VisParams^);
end;

function ParamPos(const Visualisation: TPresetVis; const Index,LastIndex: Integer; const LastParam: TVisParam): TVisParam;
var
  I: Integer;
begin
  Result:=LastParam;
  with TVisualisation.Visualisations(Visualisation.VisID)
    do for I:=LastIndex to Index-1
      do Result+=VisParamTypes.SizeOfParam(VisParamDesc[I].AType);
end;

function NextParamPos(const Visualisation: TPresetVis; Index: Integer; LastParam: TVisParam): TVisParam;
begin
  Result:=LastParam+VisParamTypes.SizeOfParam(TVisualisation.Visualisations(Visualisation.VisID).VisParamDesc[Index].AType);
end;

function NextParamPosCCC(const Visualisation: TPresetVis; Index: Integer; LastParam: TVisParam): TVisParam;
begin
  if Index>3
    then Result:=LastParam+VisParamTypes.SizeOfParam(TVisualisation.Visualisations(Visualisation.VisID).VisParamDesc[Index-4].AType)
    else case Index of
      0: Result:=@Visualisation.C1;
      1: Result:=@Visualisation.C2;
      2: Result:=@Visualisation.C3;
      3: Result:=Visualisation.VisParams;
    end;
end;

function VisOutputOffset(const Visualisation: TPresetVis; const Index: Integer): TVisInputPos;

  procedure DoGetParamPos(var Params);
  var
    I: Integer;
  begin
    Result:=NilStart;
    with TVisualisation.Visualisations(Visualisation.VisID)
      do for I:=0 to Index-1
        do Result+=VisParamTypes.SizeOfParam(VisParamDesc[I].AType);
  end;

begin
  DoGetParamPos(Visualisation.VisParams^);
end;

procedure SetVisOutput(var Preset: TPreset; AOutLayer,AOutput,AInLayer,AParam: Integer); overload;
var
  OutLayer: PPresetVis;
begin
  if AOutLayer>=0
    then OutLayer:=@Preset.Layers[AOutLayer]
    else OutLayer:=@Preset.Triggers[not AOutLayer];
  with OutLayer^.VisOutputs[AOutput] do begin
    Layer:=AInLayer;
    Param:=AParam;
  end;
end;

procedure SetVisOutput(var Preset: TPreset; const AOutput,AParam: TPresetOutputInfo); overload;
begin
  SetVisOutput(Preset,AOutput.Layer,AOutput.Param,AParam.Layer,AParam.Param);
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

function LoadKeyboardsFromStream(var Keyboards: TVisKeyboards; Stream: TStream; const Append: Boolean = false; const ReadHeader: Boolean = true): TLoadFileResult;
var
  I,L,Offset: Integer;
begin
  if ReadHeader then begin
    if ReadFileID(Stream)=KeyboardFileID then Result:=lplOK else begin
      Result:=lplInvalidHeader;
      exit;
    end;
  end else Result:=lplOK;
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
    Name:=LoadText(Stream);
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

function LoadKeyboardsFromFile(var Keyboards: TVisKeyboards; const FileName: string; const Append: Boolean = false): TLoadFileResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadKeyboardsFromStream(Keyboards,FS,Append);
  FS.Free;
end;

function GlobalVisIsSlow: Boolean;
begin
  Result:=GlobalVisThread.Slow;
end;

function GetGlobalVisFPS: Double;
begin
  Result:=GlobalVisThread.FPS;
end;

procedure SetGlobalVisFPS(Value: Double);
begin
  GlobalVisThread.FPS:=Value;
end;

function GetGlobalVisActiveFPS: Double;
begin
  Result:=GlobalVisThread.ActiveFPS;
end;

function GetGlobalVisPriority: TThreadPriority;
begin
  Result:=GlobalVisThread.Priority;
end;

procedure SetGlobalVisPriority(Value: TThreadPriority);
begin
  GlobalVisThread.Priority:=Value;
end;

procedure SetGlobalVisedEvent(Value: TEmptyEvent);
begin
  GlobalVisThread.OnVised:=Value;
end;

function GetGlobalVisTaskCount: Cardinal;
begin
  Result:=GlobalVisThread.BGThread.TaskCount;
end;

function GetGlobalVisBGThread: TVisBGThread;
begin
  Result:=GlobalVisThread.BGThread;
end;

initialization
  GlobalVisThread:=TGlobalVisThread.Create;
finalization
  {GlobalVisThread.Terminate;
  GlobalVisThread.WaitFor;}
  GlobalVisThread.Destroy;
end.

