unit PointerParamEdit;

{$mode objfpc}{$H+}

interface

uses
  ParamPainter2, SysUtils, StdParamTypes, PresetUtil3_UpdatingWire,
  SlaveControl, PresetUtil3_ConnectionRouter, VisType2, PresetUtil3_Path,
  PresetUtil3_WiredMaster, ParamType2, PresetUtil3_PointingRouter, MStrings,
  PresetUtil3_Wires, PresetUtil3_Connections, UniqueStrings, ParamTypes,
  Graphics, ParamFunc, AdvFunc, ValueStorages;

type
  TWireEqualityMode      = Byte;
  EIllegalPointerPath    = class (Exception)
  end;

  TPPPPointer            = class (TPPParam)
  strict private
    FPointer       : TVPointer;
    FUpdater       : TWireUpdater;
    FOwnsUpdater   : Boolean;
    procedure DoUpdate(APointer: TVPointer);
    function GetTextWidth(const S: string): Integer;
  private
    procedure SetValue(AValue: TVPointer); inline;
    procedure DisownUpdater;
    procedure Updated; inline;
  strict protected
    procedure ContextCreated; override;
    function GetMinWidth: Integer; override;
  protected
    procedure DoUpdate; override;
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil); override;
    destructor Destroy; override;
  end;

procedure Register;
function PointerInputToPath(const APtr: TVPointer): ISidedParamPath;
function PointerOutputToPath(const APtr: TVPointer): ISidedParamPath;
function PathToPointer(APath1, APath2: ISidedParamPath; AInversePriority: LongInt): TVPointer;
function NewUpdater(AMaster: TWiredMaster; APath: ISidedParamPath; ARouter: TConnectionRouter): TWireUpdater;

implementation


{%REGION Misc}

function NotOpen(AFirstSide, ALastSide: IParamPath): Boolean; inline;
begin
  Assert((AFirstSide <> nil) and (ALastSide <> nil));
  Result:=(not AFirstSide.Equals(OpenPath)) and (not ALastSide.Equals(OpenPath));
end;

function PathIsInput(APath, AOtherPath: ISidedParamPath): Boolean;
begin
  Result:=(APath.Count < AOtherPath.Count) xor APath.IsLeft;
end;

{%ENDREGION}
{%REGION TPointerUpdater}

type
  TEditRecreationMode = (ermCreate, ermDestroy, ermUpdate);
  TEditRecreationModes= set of TEditRecreationMode;

  TPointerUpdater     = class (TWireUpdater)
  strict private
    FFirstSideIsOutput: Boolean;
    FEdit             : TPPPPointer;
    FPointerOffset    : IParamPath;
    FMaster           : TWiredMaster;
    FWire             : TPWContainer;
    FRouter           : TPointingRouter;
    procedure SetPointerValue(AValue: TVPointer);
    function GetPointerValue: TVPointer;
    function GetConnectionDestination(APath: ISidedParamPath): ISidedParamPath;
    function GetInputConnectionDestination: ISidedParamPath; inline;
    function GetOutputConnectionDestination: ISidedParamPath; inline;
    function QualifyPath(APath: ISidedParamPath): ISidedParamPath; inline;
    function PointerInputToQualifiedPath(APointer: TVPointer): ISidedParamPath;
    function PointerOutputToQualifiedPath(APointer: TVPointer): ISidedParamPath;

    function Connected: Boolean; inline;
    function EditRecreationMode: TEditRecreationModes;
    function TryGetPointerOffset(AFirstSide, ALastSide: IParamPath): Boolean;
    procedure UpdateRouter;
    procedure UpdateInputSide;
    procedure DoMakeNewConnection(ADestination: ISidedParamPath; AReplaces: TAbstractConnection);
    procedure DoMakeNewConnection(ADestination: ISidedParamPath);

    procedure UpdateWiring;
    procedure UpdateEdit;

    function GetInputSide: TUpdaterSide; inline;
    function GetOutputSide: TUpdaterSide; inline;
    property InputSide: TUpdaterSide read GetInputSide;
    property OutputSide: TUpdaterSide read GetOutputSide;

    property Master: TWiredMaster read FMaster;
    //gets connection destinations of connections to generate
    property InputConnectionDestination: ISidedParamPath read GetInputConnectionDestination;
    property OutputConnectionDestination: ISidedParamPath read GetOutputConnectionDestination;
  strict protected
    function CheckLegalPathCombo(AFirstSide, ALastSide: ISidedParamPath): Boolean; override;
    procedure FirstSideChanged; override;
    procedure LastSideChanged; override;
  public
    constructor Create(AMaster: TWiredMaster; AEdit: TPPPPointer);
    constructor Create(AMaster: TWiredMaster; APath: ISidedParamPath; ARouter: TConnectionRouter);
    destructor Destroy; override;
    procedure Delete; override;
    property PointerOffset: IParamPath read FPointerOffset;
    property PointerValue: TVPointer read GetPointerValue write SetPointerValue;
  end;

constructor TPointerUpdater.Create(AMaster: TWiredMaster; AEdit: TPPPPointer);
begin
  Assert(AEdit <> nil);
  inherited Create;
  FMaster:=AMaster;
  FEdit:=AEdit;
  FWire:=nil;
  FPointerOffset:=AEdit.Path-1;
  if AEdit.ParentRouter <> nil
    then FRouter:=TPointingRouter(AEdit.ParentRouter)
    //happens if the main parameter is a pointer
    else FRouter:=nil;
end;

constructor TPointerUpdater.Create(AMaster: TWiredMaster; APath: ISidedParamPath; ARouter: TConnectionRouter);
begin
  Assert(APath <> nil);
  Assert(ARouter <> nil);
  inherited Create;
  FMaster:=AMaster;
  FEdit:=nil;
  FWire:=nil;
  FRouter:=TPointingRouter(ARouter);
  FPointerOffset:=ARouter.Path;

  DoEvents:=false;
  FirstSide.Path:=APath;
  LastSide.Path:=OpenSidedPath;
  DoEvents:=true;
  UpdateWiring;
end;

destructor TPointerUpdater.Destroy;
begin
  //do not destroy the wire here. This is done by the connections
  inherited Destroy;
end;

procedure TPointerUpdater.UpdateWiring;
var
  AFirstDestination, ALastDestination: ISidedParamPath;
begin
  Master.RIN_StartEvent;
  if Connected then begin
    AFirstDestination:=GetConnectionDestination(FirstSide.Path);
    ALastDestination:=GetConnectionDestination(LastSide.Path);

    if FWire<>nil then begin
      if not TConnection(FWire.FirstConnection).Destination.Equals(AFirstDestination)
        then DoMakeNewConnection(AFirstDestination, FWire.FirstConnection);
      if not TConnection(FWire.LastConnection).Destination.Equals(ALastDestination)
        then DoMakeNewConnection(ALastDestination, FWire.LastConnection);
    end else begin
      FWire:=Master.WireManager.NewWire(Self);
      DoMakeNewConnection(AFirstDestination);
      DoMakeNewConnection(ALastDestination);
      UpdateInputSide;
    end;

    AFirstDestination:=nil;
    ALastDestination:=nil;
  end else begin
    if FWire<>nil then begin
      FWire.Destroy;
      FWire:=nil;
    end;
  end;
  Master.RIN;
end;

procedure TPointerUpdater.UpdateEdit;
var
  ARecreationMode: TEditRecreationModes;
  AValue         : IPParam;
begin
  //FPointer offset was defined before in CheckLegalPathCombo
  ARecreationMode:=EditRecreationMode;
  if ARecreationMode = [] then UpdateWiring else begin
    if ermDestroy in ARecreationMode then begin
      Assert(not (ermUpdate in ARecreationMode));
      FEdit.DisownUpdater;
      FRouter.RemoveParam(FEdit);
      FEdit:=nil;
      //if there is nothing else to do, nothing will do the wiring
      if ARecreationMode = [ermDestroy]
        then UpdateWiring;
    end;
    if ermCreate in ARecreationMode then begin
      Assert(FEdit = nil);
      Assert(not (ermUpdate in ARecreationMode));
      UpdateRouter;
      UpdateInputSide;
      AValue:=CreateValueStorage(vPointer);
      IPPointer(AValue).&Set(PointerValue);
      //initialize the edit
      FEdit:=TPPPPointer(FRouter.AddParam(GloballyUniqueEnglishText, AValue, -1, Self));
    end;
    if ermUpdate in ARecreationMode
      then FEdit.Updated;
  end;
end;

procedure TPointerUpdater.FirstSideChanged;
begin
  UpdateEdit;
end;

procedure TPointerUpdater.LastSideChanged;
begin
  UpdateEdit;
end;

function TPointerUpdater.CheckLegalPathCombo(AFirstSide, ALastSide: ISidedParamPath): Boolean;
begin
  Result:=(not NotOpen(AFirstSide, ALastSide))
      or (TryGetPointerOffset(AFirstSide, ALastSide)
        and (PathIsInput(AFirstSide, ALastSide) xor PathIsInput(ALastSide, AFirstSide)));
end;

procedure TPointerUpdater.Delete;
begin
  DoEvents:=false;
  FirstSide.Path:=OpenSidedPath;
  LastSide.Path:=FirstSide.Path;
  DoEvents:=true;
  UpdateEdit;
  Destroy;
end;

procedure TPointerUpdater.SetPointerValue(AValue: TVPointer);
begin
  DoEvents:=false;

  InputSide.Path:=PointerInputToQualifiedPath(AValue);
  OutputSide.Path:=PointerOutputToQualifiedPath(AValue);

  DoEvents:=true;
  UpdateWiring;
end;

function TPointerUpdater.GetPointerValue: TVPointer;
var
  AOutputConnectionDestination: ISidedParamPath;
  AInversePriority            : LongInt;
  ASubRouter                  : TConnectionRouter;
begin
  Assert(FRouter <> nil);

  //get inverse priority
  AOutputConnectionDestination:=OutputConnectionDestination;
  case AOutputConnectionDestination.Count of
    1: AInversePriority:=FRouter.HighestInversePriority(AOutputConnectionDestination[0]);
    2: begin
        ASubRouter:=FRouter.SubRouter[AOutputConnectionDestination[0]];
        Assert(ASubRouter <> nil);
        Assert(ASubRouter is TPointingRouter);
        AInversePriority:=TPointingRouter(ASubRouter).HighestInversePriority(AOutputConnectionDestination[1]);
      end
    //OutputConnectionDestination should never return a path with more than 2 items
    else Assert(false);
  end;

  Result:=PathToPointer(InputConnectionDestination, AOutputConnectionDestination, AInversePriority + 1);
end;

function TPointerUpdater.GetInputSide: TUpdaterSide; inline;
begin
  if FFirstSideIsOutput
    then Result:=LastSide
    else Result:=FirstSide;
end;

function TPointerUpdater.GetOutputSide: TUpdaterSide; inline;
begin
  if FFirstSideIsOutput
    then Result:=FirstSide
    else Result:=LastSide;
end;

function TPointerUpdater.GetConnectionDestination(APath: ISidedParamPath): ISidedParamPath;
begin
  Assert(APath <> nil);
  if APath.Equals(OpenPath)
    then Result:=APath
    else Result:=NewParamPath(APath.IsLeft, APath - FPointerOffset);
end;

function TPointerUpdater.GetInputConnectionDestination: ISidedParamPath; inline;
begin
  Result:=GetConnectionDestination(InputSide.Path);
end;

function TPointerUpdater.GetOutputConnectionDestination: ISidedParamPath; inline;
begin
  Result:=GetConnectionDestination(OutputSide.Path);
end;

function TPointerUpdater.QualifyPath(APath: ISidedParamPath): ISidedParamPath; inline;
begin
  Result:=NewParamPath(APath.IsLeft, FPointerOffset + APath);
end;

function TPointerUpdater.PointerInputToQualifiedPath(APointer: TVPointer): ISidedParamPath;
begin
  if StdParamTypes.Connected(APointer)
    then Result:=QualifyPath(PointerInputToPath(APointer))
    else Result:=OpenSidedPath;
end;

function TPointerUpdater.PointerOutputToQualifiedPath(APointer: TVPointer): ISidedParamPath;
begin
  if StdParamTypes.Connected(APointer)
    then Result:=QualifyPath(PointerOutputToPath(APointer))
    else Result:=OpenSidedPath;
end;

function TPointerUpdater.Connected: Boolean; inline;
begin
  Assert((FirstSide.Path <> nil) and (LastSide.Path <> nil));
  //if only one side is open, it is CONNECTED to the open connector
  Result:=not (FirstSide.Path.Equals(OpenPath) and LastSide.Path.Equals(OpenPath));
end;

function TPointerUpdater.EditRecreationMode: TEditRecreationModes;
begin
  Assert(FPointerOffset <> nil);
  if Connected then begin
    if NotOpen(FirstSide.Path, LastSide.Path) then begin
      if FEdit <> nil then begin
        if not FPointerOffset.Equals(FEdit.Path - 1)
          then Result:=[ermCreate, ermDestroy]
          else Result:=[ermUpdate];
      end else Result:=[ermCreate];
    end else Result:=[];
  end else if FEdit<>nil
    then Result:=[ermDestroy]
    else Result:=[];
end;

function TPointerUpdater.TryGetPointerOffset(AFirstSide, ALastSide: IParamPath): Boolean; inline;
var
  APointerOffset                             : IParamPath;
  AFirstSideOffsetCount, ALastSideOffsetCount: Integer;
begin
  APointerOffset:=ALastSide / AFirstSide;
  AFirstSideOffsetCount:=AFirstSide.Count - APointerOffset.Count;
  ALastSideOffsetCount:=ALastSide.Count - APointerOffset.Count;
  Result:=((AFirstSideOffsetCount <= 2) and (ALastSideOffsetCount <= 2))
    and (IntAbs(AFirstSideOffsetCount - ALastSideOffsetCount) <= 1);
  if Result then begin
    if (AFirstSideOffsetCount > 0) and (ALastSideOffsetCount > 0)
      then FPointerOffset:=APointerOffset
      else FPointerOffset:=APointerOffset - 1;
  end;
end;

procedure TPointerUpdater.UpdateRouter;
var
  AEqualCount, I, L: Integer;
  ARouter          : TConnectionRouter;
begin
  ARouter:=FRouter;

  AEqualCount:=EqualCount(FPointerOffset, ARouter.Path);
  L:=ARouter.Path.Count;
  for I:=AEqualCount to L-1 do begin
    ARouter:=ARouter.ParentRouter;
    Assert(ARouter <> nil);
  end;
  L:=FPointerOffset.Count;
  for I:=AEqualCount to L-1 do begin
    ARouter:=ARouter.SubRouter[FPointerOffset[I]];
    Assert(ARouter <> nil);
  end;

  FRouter:=TPointingRouter(ARouter);
end;

procedure TPointerUpdater.UpdateInputSide;
begin
  FFirstSideIsOutput:=PathIsInput(LastSide.Path, FirstSide.Path);
end;

procedure TPointerUpdater.DoMakeNewConnection(ADestination: ISidedParamPath; AReplaces: TAbstractConnection);
begin
  Assert(FRouter<>nil);
  FRouter.RouteConnection(TConnection.Create(AReplaces, ADestination));
end;

procedure TPointerUpdater.DoMakeNewConnection(ADestination: ISidedParamPath);
begin
  Assert(FRouter<>nil);
  FRouter.RouteConnection(TConnection.Create(FWire, ADestination));
end;

{%ENDREGION}
{%REGION TPPPPointer}

constructor TPPPPointer.Create(AOwner: TSlaveControl; AConnections: TRoutableConnections; ARouter: TConnectionRouter; APainter: TParamPainter; AParam: IPParam; APath: IParamPath; AMaster: TMasterControl = nil; AAdditionalInformation: TObject = nil);
begin
  inherited Create(AOwner, AConnections, ARouter, APainter, AParam, APath, AMaster, AAdditionalInformation);
  //no automatic updating in constructor, so do it here
  FOwnsUpdater:=true;
  if AAdditionalInformation <> nil then begin
    Assert(AAdditionalInformation is TPointerUpdater);
    FUpdater:=TPointerUpdater(AAdditionalInformation);
  end else FUpdater:=TPointerUpdater.Create(TWiredMaster(Master), Self);
end;

destructor TPPPPointer.Destroy;
begin
  if FOwnsUpdater
    then FUpdater.Destroy;
  inherited Destroy;
end;

function TPPPPointer.GetMinWidth: Integer;
begin
  Result:=100;
end;

procedure TPPPPointer.ContextCreated;
begin
  DoUpdate;
end;

procedure TPPPPointer.DoUpdate(APointer: TVPointer);
begin
  FPointer:=APointer;
  Master.RIN_StartEvent;
  TPointerUpdater(FUpdater).PointerValue:=APointer;
  Repaint;
  Master.RIN;
end;

procedure TPPPPointer.DoUpdate;
begin
  DoUpdate(IPPointer(Param).Get);
end;

procedure TPPPPointer.SetValue(AValue: TVPointer); inline;
begin
  IPPointer(Param).&Set(AValue);
end;

procedure TPPPPointer.DisownUpdater;
begin
  FOwnsUpdater:=false;
end;

procedure TPPPPointer.Updated; inline;
begin
  SetValue(TPointerUpdater(FUpdater).PointerValue);
end;

function TPPPPointer.GetTextWidth(const S: string): Integer;
begin
  Result:=Canvas.TextWidth(S);
end;

procedure TPPPPointer.DoPaint;
var
  AY: Integer;
begin
  inherited DoPaint;
  FillBG;
  AY:=(Height-Canvas.TextHeight('Wg')) div 2;
  Canvas.Font.Style:=[];
  Canvas.Font.Color:=clBlack;
  Canvas.TextOut(AbsoluteRect.Left,
                 AbsoluteRect.Top+AY,
                 StrShorten(PointerToStr(FPointer), @GetTextWidth, Width));
  Canvas.Brush.Style:=bsSolid;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  TParamPainter.AddTypeEdit(vPointer, TPPPPointer);
end;

function PointerInputToPath(const APtr: TVPointer): ISidedParamPath;
begin
  Assert(Connected(APtr));
  if APtr.Input.Preset.Name <> '' then begin
    Result:=NewParamPath(true,
        [APtr.Input.Preset,
        APtr.Input.Param]);
  end else begin
    //if it is a direct connection, do not change the isLeft to false
    Result:=NewParamPath(APtr.Output.Preset.Name = '',
        APtr.Input.Param);
  end;
end;

function PointerOutputToPath(const APtr: TVPointer): ISidedParamPath;
begin
  Assert(Connected(APtr));
  with Result do if APtr.Output.Preset.Name <> '' then begin
    Result:=NewParamPath(false,
        [APtr.Output.Preset,
        APtr.Output.Param]);
  end else begin
      //if it is a direct connection, do not change the isLeft to true
    Result:=NewParamPath(APtr.Input.Preset.Name <> '',
        APtr.Output.Param);
  end;
end;

function PathToPointerSide(APath: ISidedParamPath): TPointerSide;
begin
  Case APath.Count of
    1: with Result do begin
        Preset:=ParamID('', vPreset);
        Param:=APath[0];
      end;
    2: with Result do begin
        Preset:=APath[0];
        Param:=APath[1];
      end;
    else raise EIllegalPointerPath.Create('Path ' + APath.ToString + ' contains to many items.');
  end;
end;

function PathToPointer(APath1, APath2: ISidedParamPath; AInversePriority: LongInt): TVPointer;
begin
  if PathIsInput(APath1, APath2) then begin
    Assert(not PathIsInput(APath2, APath1));
    with Result do begin
      Input:=PathToPointerSide(APath1);
      Output:=PathToPointerSide(APath2);
    end;
  end else begin
    Assert(PathIsInput(APath2, APath1));
    with Result do begin
      Input:=PathToPointerSide(APath2);
      Output:=PathToPointerSide(APath1);
    end;
  end;
  Result.InversePriority:=AInversePriority;
end;

function NewUpdater(AMaster: TWiredMaster; APath: ISidedParamPath; ARouter: TConnectionRouter): TWireUpdater;
begin
  Result:=TPointerUpdater.Create(AMaster, APath, ARouter);
end;

{%ENDREGION}


end.

