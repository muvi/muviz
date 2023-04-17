(*
  a unit to convert old presets into new presets
*)
unit ImportUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisualisationMain, VisType2, ImportType, VisAddInput,
  VisType, VisTypeUnit, PresetType, StdTags, DebugTools, MStrings,
  StdParamTypes, MExceptions, ParamType2, UniqueStrings, VisPresetList3,
  Param_CopyConverted, csl, LinkedList;

procedure ImportOldPresets(APresets: VisTypeUnit.TPresets);
function ImportOldPreset(APreset: VisTypeUnit.TPreset): TPPresetID;
function ConvertVisID(AOldID: VisTypeUnit.TVisID): VisType2.TPVisID;

implementation

{%REGION TListedPointer}

type
  TListedPointer = class
  strict private
    FPtr          : TVPointer;
    FVisualisation: IPVisualisation;
    procedure DefinePointer;
    procedure SetOutput;
  public
    constructor Create(APtr: TVPointer; AVisualisation: IPVisualisation);
    //automatically calls SetOutput
    destructor Destroy; override;
  end;

constructor TListedPointer.Create(APtr: TVPointer; AVisualisation: IPVisualisation);
begin
  //assertions: AVisualisation
  Assert(AVisualisation <> nil);
  //assertions: output preset
  Assert(APtr.Output.Preset.&Type = vPreset);
  Assert(not APtr.Output.Preset.Name.Equals(''));
  //assertions: input preset
  Assert(APtr.Input.Preset.&Type = vPreset);
  Assert(not APtr.Input.Preset.Name.Equals(''));
  //assertions: output param
  Assert(not APtr.Output.Preset.Name.Equals(''));
  //assertions: input param
  Assert(not APtr.Input.Preset.Name.Equals(''));

  inherited Create;
  FVisualisation:=AVisualisation;
  FPtr:=APtr;
end;

destructor TListedPointer.Destroy;
begin
  //do this first!
  SetOutput;
  DefinePointer;
  FVisualisation:=nil;
  inherited Destroy;
end;

procedure TListedPointer.DefinePointer;
begin
  AddInput(FVisualisation, GloballyUniqueEnglishText, FPtr);
end;

//needed, because the old connections did not have output values
procedure TListedPointer.SetOutput;
var
  //AInputParam, AOutputParam  : IPParam;
  AInputPreset, AOutputPreset: IPVisualisation;
begin
  AOutputPreset:=PresetUtil[IPPreset(FVisualisation[FPtr.Output.Preset]).Value];
  AInputPreset:=PresetUtil[IPPreset(FVisualisation[FPtr.Input.Preset]).Value];
  CopyParamConverted(AInputPreset[FPtr.Input.Param], AOutputPreset[FPtr.Output.Param]);
end;

{%ENDREGION}
{%REGION conversion methods}

type
  TGUIDConversionRec = packed record
    Time: UInt64;
    ID  : VisTypeUnit.TVisID;
  end;

const
  CONVERSIONGUIDTEMPLATE: TGUID = '{2C75A06B-5A8D-471A-814E-4C08AF9A0C67}';

(*
  converts an old vis id into a new one
  $Param AOldID the old id
  $return the new id
*)
function ConvertVisID(AOldID: VisTypeUnit.TVisID): VisType2.TPVisID;
var
  AID: TGUIDConversionRec absolute Result;
begin
  if AOldID<>0 then begin
    Result:=CONVERSIONGUIDTEMPLATE;
    AID.ID:=AOldID;
  end else Result:=NULLVISID;
end;

const
  LAYERPREFIX    = 'Layer ';
  TRIGGERPREFIX  = 'Trigger ';
  POINTERPREFIX  = 'Pointer ';

(*
  converts an old param type descriptor into a new one
  VisTypeUnit.TVisOutputType -> VisType2.TPParamType
  $Param AOldType the old type descriptor
  $return the new type descriptor
*)
function OldTypeToNewType(AOldType: VisType.TVisOutputType): TPParamType;
begin
  case AOldType of
    VisType.vCall    : Result:=StdParamTypes.vCall;
    VisType.vInteger : Result:=StdParamTypes.vInteger;
    VisType.vReal    : Result:=StdParamTypes.vFloat;
    VisType.vString  : Result:=StdParamTypes.vString;
    VisType.vColor   : Result:=StdParamTypes.vColor;
    VisType.vBoolean : Result:=StdParamTypes.vBoolean;
    VisType.vBuffer  : Result:=StdParamTypes.vBuffer;
    else raise EImportException.Create('could not import param of type "' + VisTypeUnit.VisParamTypes.ParamTypePtr[AOldType]^.Name + '".');
  end;
end;

(*
  converts an old param value into a param representing a new value
  $Param AVisualisation the visualisation the param should be added to
  $Param AName the new params name
  $Param AOldType the old type descriptor
  $Param AValue the old value
*)
procedure AddOldInput(AVisualisation: IPVisualisation; AName: IString; AOldType: VisType.TVisOutputType; const AValue);
begin
  case AOldType of
    VisType.vCall    : AddInput(AVisualisation, AName);
    VisType.vInteger : AddInput(AVisualisation, AName, VisType.vpInt(AValue));
    VisType.vReal    : AddInput(AVisualisation, AName, VisType.vpReal(AValue));
    VisType.vString  : AddInput(AVisualisation, AName, VisType.vpString(AValue));
    VisType.vColor   : AddInput(AVisualisation, AName, VisType.vpColor(AValue));
    VisType.vBoolean : AddInput(AVisualisation, AName, VisType.vpBool(AValue));
    VisType.vBuffer  : AddInput(AVisualisation, AName, EmptyBuffer);
    else raise EImportException.Create('could not import param of type "' + VisTypeUnit.VisParamTypes.ParamTypePtr[AOldType]^.Name + '".');
  end;
end;

procedure AddOldPresetInputValue(ADest: IPVisualisation; AOldPreset: VisTypeUnit.TPreset; AOldInput: TNamedPresetOutput);
var
  AParamDesc: VisTypeUnit.TSVisParamDesc;
  AValue    : Pointer;
  AType     : PVisType;
begin
  with AOldPreset.Items[AOldInput.OutputInfo.Layer]^ do begin
    case AOldInput.OutputInfo.Param of
      //main input
      -1: AddInput(ADest, AOldInput.Name);
      //C1
      0 : AddInput(ADest, AOldInput.Name, TVColor(C1));
      //C2
      1 : AddInput(ADest, AOldInput.Name, TVColor(C2));
      //C3
      2 : AddInput(ADest, AOldInput.Name, TVColor(C3));
      else begin
        AType:=TVisualisation.VisPtr(VisID);
        AParamDesc:=AType^.VisParamDesc[AOldInput.OutputInfo.Param-3];
        AValue:=VisParams + PtrInt(AParamDesc.Offset);
        AddOldInput(ADest, AOldInput.Name, AParamDesc.AType, AValue^);
      end;
    end;
  end;
end;

(*
  Checks if there is an output with the same name and adds the postfix ' Input'
  if so.
  $Param ADesiredName the name to check
  $Param AOutputDesc the old output desc array
  $Return the new name
*)
function CheckName(const ADesiredName: string; const AOutputDesc: TVisOutputDesc): string;
var
  I: Integer;
begin
  for I:=0 to Length(AOutputDesc)-1
    do if AOutputDesc[I].Name = ADesiredName then begin
      Result:=ADesiredName + ' Input';
      exit;
    end;
  Result:=ADesiredName;
end;

(*
  Checks if there is an input with the same name and adds the postfix ' Output'
  if so.
  $Param ADesiredName the name to check
  $Param AInputDesc the old param desc array
  $Return the new name
*)
function CheckName(const ADesiredName: string; const AInputDesc: TVisParamDesc): string;
var
  I: Integer;
begin
  for I:=0 to Length(AInputDesc)-1
    do if AInputDesc[I].Name = ADesiredName then begin
      Result:=ADesiredName + ' Output';
      exit;
    end;
  Result:=ADesiredName;
end;

(*
  Creates a sub preset for another preset and returns a preset parameter definition containing its ID.
  Retrieves the sub presets param definitions from an old Layer.
  $Param ADest the preset the subpreset id should be added to (as parameter)
  $Param APresetName the parent presets name
  $Param AName the new presets name, for example Layer1
  $Param ALayer the old layer containing the param definitions used to create the sub preset
  $Param AAutoCall wether the layers main param has to be called automatically
  $Param AIndex the layers index
*)
procedure LayerParamDef(ADest: IPVisualisation; APresetName, AName: string; const ALayer: TPresetVis; AAutoCall: Boolean; AIndex: Integer);
var
  I           : Integer;
  AType       : PVisType;
  AValue      : Pointer;
  APresetID   : TPPresetID;
  ANewPreset  : IPVisualisation;
  AOutputDesc : VisTypeUnit.TSVisOutputDesc;
  AParamDesc  : VisTypeUnit.TSVisParamDesc;
  ACallPointer: TVPointer;
begin
  AType:=TVisualisation.VisPtr(ALayer.VisID);
  //create the preset
  CreateGUID(APresetID);
  ANewPreset:=PresetUtil[APresetID];
  with ANewPreset do begin
    AddTag(TAGIMPORTEDSUBPRESET);
    AddTag(TAGSUBPRESET);
    AddTag(TAGHIDDEN);
    //AddTag(TAGLISTED);
    AddTag(TAGSAVE);
    //create system parameters
    AddInput(This, NAMEINPUTNAME, APresetName + SUBPRESETDIVIDER + AName + ' (' + AType^.Name + ')');
    AddInput(This, PARENTINPUTNAME, ConvertVisID(AType^.VisID));
    //create basic parameters
    AddInput(This, MAININPUTNAME);
    //AddInput(This, AUTOCALLNAME, AAutoCall);
    AddInput(This, C1NAME, ALayer.C1);
    AddInput(This, C2NAME, ALayer.C2);
    AddInput(This, C3NAME, ALayer.C3);
    //import the other parameters
    for I:=0 to Length(AType^.VisParamDesc)-1 do begin
      AParamDesc:=AType^.VisParamDesc[I];
      AValue:=ALayer.VisParams + PtrInt(AParamDesc.Offset);
      AddOldInput(This, CheckName(AParamDesc.Name, AType^.OutputDesc), AParamDesc.AType, AValue^);
    end;
    //import outputs
    for I:=0 to Length(AType^.OutputDesc)-1 do begin
      AOutputDesc:=AType^.OutputDesc[I];
      AddInput(This, ParamID(CheckName(AOutputDesc.Name, AType^.VisParamDesc), OldTypeToNewType(AOutputDesc.AType)));
    end;
  end;
  AddInput(ADest, AName, ANewPreset.ID);
  if AAutoCall then begin
    with ACallPointer do begin
      Input.Preset:=ParamID(AName, vPreset);
      Input.Param:=ParamID(MAININPUTNAME, vCall);
      Output.Preset:=ParamID('', vPreset);
      Output.Param:=ParamID(MAININPUTNAME, vCall);
      InversePriority:=AIndex;
    end;
    AddInput(ADest, GloballyUniqueEnglishText, ACallPointer);
  end;
end;

(*
  returns the number of pointer typed parameters needed to import a preset
  $Param APreset the preset to import
  $return the number of pointer typed parameters needed to import a preset
*)
function CountParamPointers(APreset: VisTypeUnit.TPreset): Integer;
var
  I: Integer;
begin
  Result:=0;
  for I:=0 to Length(APreset.Layers)-2 do begin
    Result+=Length(APreset.Layers[I].VisOutputs);
  end;
  for I:=0 to Length(APreset.Triggers)-2 do begin
    Result+=Length(APreset.Triggers[I].VisOutputs);
  end;
  Result+=Length(APreset.Params);
  Result+=Length(APreset.Outputs);
end;

(*
  converts an old param descriptor into a new input ID
  TSVisParamDesc -> TPInputID
  $Param ADesc the old param descriptor
  $Param AOutputDesc the old output descriptions
  $return the new input id
*)
function VisParamDescToInputID(const ADesc: TSVisParamDesc; const AOutputDesc: TVisOutputDesc): TPParamID;
begin
  Result.Name:=CheckName(ADesc.Name, AOutputDesc);
  Result.&Type:=OldTypeToNewType(ADesc.AType);
end;

(*
  converts an old output descriptor into a new output ID
  TSVisOutputDesc -> TPOutputID
  $Param ADesc the old output descriptor
  $Param AParamDesc the old param descriptions
  $return the new output id
*)
function VisOutputDescToParamID(const ADesc: TSVisOutputDesc; const AParamDesc: TVisParamDesc): TPParamID;
begin
  Result.Name:=CheckName(ADesc.Name, AParamDesc);
  Result.&Type:=OldTypeToNewType(ADesc.AType);
end;

(*
  converts the contents of an old vis output to a new pointer side
  $Param AOutput the old presets output to convert
  $Param APreset the old preset
  $return the generated pointer side
*)
function VisOutputToParamID(AOutput: TPresetOutputInfo; APreset: VisTypeUnit.TPreset): TPointerSide;
var
  AType: PVisType;
begin
  //Param
  case AOutput.Param of
    -1: Result.Param:=ParamID(MAININPUTNAME, vCall);
    0 : Result.Param:=ParamID(C1NAME, vColor);
    1 : Result.Param:=ParamID(C2NAME, vColor);
    2 : Result.Param:=ParamID(C3NAME, vColor);
    else if AOutput.Param>=3 then begin
      AType:=VisTypeUnit.TVisualisation.VisPtr(APreset.Items[AOutput.Layer]^.VisID);
      Result.Param:=VisParamDescToInputID(AType^.ParamDesc[AOutput.Param], AType^.OutputDesc);
    end else {if AOutput.Param < -1} begin
      Result:=EMPTYPOINTERSIDE;
      //do not set the preset here
      exit;
    end;
  end;
  //Preset
  Result.Preset.&Type:=vPreset;
  if AOutput.Layer>=0
    then Result.Preset.Name:=LAYERPREFIX + IntToStr(AOutput.Layer+1)
    else Result.Preset.Name:=TRIGGERPREFIX + IntToStr((not AOutput.Layer)+1);
end;

(*
  converts the contents of an old vis output to a new pointer parameter value
  $Param AOutput the old presets output to convert
  $Param APreset the old preset
  $Param AOutputLayerName name of the layer the old output belongs to
  $Param AOutptuDesc description of the old output
  $Param AParamDesc the old param descriptions
  $return the generated pointer parameter value
*)
function VisOutputToPointer(AOutput: TPresetOutputInfo; APreset: VisTypeUnit.TPreset; AOutputLayerName: string; AOutputDesc: TSVisOutputDesc; const AParamDesc: TVisParamDesc): TVPointer;
begin
  //Input
  Result.Input:=VisOutputToParamID(AOutput, APreset);
  //Output
  if not Connected(Result.Input) then begin
    Result.Output:=EMPTYPOINTERSIDE;
    exit;
  end;
  Result.Output.Preset:=ParamID(AOutputLayerName, vPreset);
  Result.Output.Param:=VisOutputDescToParamID(AOutputDesc, AParamDesc);
end;

(*
  creates a pointer param definition for an output of the specified layer
  $Param ADest the preset the new pointer should be added to
  $Param ALayerName the name of the layer in the new preset, for example "Layer1"
  $Param ALayer the Layer whose output should be converted
  $Param AIndex the index of the output which should be converted
  $Param APreset the old preset where the old layer belongs to
  $Param APointerList list where all the pointers are collected, to set the output afterwards
*)
procedure PointerParamDef(ADest: IPVisualisation; ALayerName: string; constref ALayer: TPresetVis; AIndex: Integer; APreset: VisTypeUnit.TPreset; APointerList: TList);
var
  APtr       : TVPointer;
  AType      : PVisType;
begin
  AType:=VisTypeUnit.TVisualisation.VisPtr(ALayer.VisID);
  APtr:=VisOutputToPointer(ALayer.VisOutputs[AIndex], APreset, ALayerName, AType^.OutputDesc[AIndex], AType^.VisParamDesc);
  if APtr.Input.Param.Name<>''
    //add pointer to list (is inserted later)
    then APointerList.Add(TListedPointer.Create(APtr, ADest));
end;

(*
  imports an old layer or trigger
  $Param APreset the preset the layer belongs to
  $Param APrefix the prefix for the layer name, may be LAYERPREFIX or TRIGGERPREFIX
  $Param AIndex the layer index
  $Param ADest the preset the parameters (preset and pointers) should be added to
  $Param AIsLayer whether the "layer" is a real layer. if false, it is expected
                  to be a trigger
  $Param APointerList list where all the pointers are collected, to set the output afterwards
*)
procedure ImportOldLayer(APreset: VisTypeUnit.TPreset; APrefix: string; const AIndex: Integer; ADest: IPVisualisation; AIsLayer: Boolean; APointerList: TList);
var
  I     : Integer;
  AName : string;
  ALayer: PPresetVis;
begin
  AName:=APrefix + IntToStr(AIndex+1);
  if AIsLayer
    then ALayer:=@APreset.Layers[AIndex]
    else ALayer:=@APreset.Triggers[AIndex];

  LayerParamDef(ADest, APreset.Name, AName, ALayer^, AIsLayer, AIndex);
  for I:=0 to Length(ALayer^.VisOutputs)-1
    do PointerParamDef(ADest, AName, ALayer^, I, APreset, APointerList);
end;

(*
  imports preset parameter or output (the params which where originally shown on the gui)
  $Param AOldPreset the old preset param or output belongs to
  $Param ADest the preset the parameters should be added to
  $Param AOldParam the old param description
  $Param APresetPointerSide the pointer side which should have the preset set to '' afterwards
  $Param ALayerPointerSide the other pointer side
  $Param APointerValue the complete pointer containing APresetPointerSide and ALayerPointerSide

  $remark one may vary only the order of APresetPointerSide and ALAyerPointerSide, not the value
*)
procedure ImportOldPresetParamOrOutput(AOldPreset: VisTypeUnit.TPreset; ADest: IPVisualisation; AOldParam: TNamedPresetOutput; var APresetPointerSide, ALayerPointerSide: TPointerSide; var APointerValue: TVPointer; IsOutput: Boolean);
begin
  ALayerPointerSide:=VisOutputToParamID(AOldParam.OutputInfo, AOldPreset);
  with APresetPointerSide.Preset do begin
    Name:='';
    &Type:=vPreset;
  end;
  with APresetPointerSide do begin
    with Param do begin
      Name:=AOldParam.Name;
      //this is important, even if IsOutput = false, because it is needed in
      //APresetPointerSide
      &Type:=ALayerPointerSide.Param.&Type;
      if IsOutput
        then AddInput(ADest, Param)
        else AddOldPresetInputValue(ADest, AOldPreset, AOldParam);
      AddInput(ADest, {Name}GloballyUniqueEnglishText, APointerValue);
    end;
  end;
end;

(*
  imports preset parameter (the params which where originally shown on the gui)
  $Param AOldPreset the old preset param or output belongs to
  $Param ADest the preset the parameters should be added to
  $Param AIndex the input index
*)
procedure ImportOldPresetInput(AOldPreset: VisTypeUnit.TPreset; ADest: IPVisualisation; AIndex: Integer);
var
  APointerValue: TVPointer;
begin
  //do not initialize APointerValue
  ImportOldPresetParamOrOutput(AOldPreset, ADest, AOldPreset.Params[AIndex], APointerValue.Output, APointerValue.Input, APointerValue, false);
end;

(*
  imports preset output
  $Param AOldPreset the old preset param or output belongs to
  $Param ADest the preset the parameters should be added to
  $Param AIndex the output index
*)
procedure ImportOldPresetOutput(AOldPreset: VisTypeUnit.TPreset; ADest: IPVisualisation; AIndex: Integer);
var
  APointerValue: TVPointer;
begin
  //do not initialize APointerValue
  ImportOldPresetParamOrOutput(AOldPreset, ADest, AOldPreset.Outputs[AIndex], APointerValue.Input, APointerValue.Output, APointerValue, true);
end;

(*
  imports an old preset and returns its new id
  $Param APreset the old preset to import
  $return the new presets id
*)
function ImportOldPreset(APreset: VisTypeUnit.TPreset): TPPresetID;
var
  I           : Integer;
  ANewPreset  : IPVisualisation;
  APointerList: TList;
begin
  //create the new preset
  CreateGUID(Result);
  ANewPreset:=PresetUtil[Result];
  with ANewPreset do begin
    //add tags
    AddTag(TAGPREDEFINED);
    AddTag(TAGIMPORTED);
    AddTag(TAGVISUAL);
    AddTag(TAGLISTED);
    AddTag(TAGSAVE);
    //add system inputs
    AddInput(This, NAMEINPUTNAME, APreset.Name);
    AddInput(This, PARENTINPUTNAME, VIDPRESETLIST);
  end;
  APointerList:=TLinkedList.Create;
  //create presets from layers
  for I:=0 to Length(APreset.Layers)-1 do begin
    ImportOldLayer(APreset, LAYERPREFIX, I, ANewPreset, true, APointerList);
  end;
  //create presets from triggers
  for I:=0 to Length(APreset.Triggers)-1 do begin
    ImportOldLayer(APreset, TRIGGERPREFIX, I, ANewPreset, false, APointerList);
  end;
  //import inputs
  for I:=0 to Length(APreset.Params)-1 do begin
    ImportOldPresetInput(APreset, ANewPreset, I);
  end;
  //import outputs
  for I:=0 to Length(APreset.Outputs)-1 do begin
    ImportOldPresetOutput(APreset, ANewPreset, I);
  end;
  //set the outputs
  APointerList.Clean;
  APointerList.Destroy;
end;

(*
  imports all old presets from a specified presets set and stores it in the
  global new preset util
  $Param APresets the old preset set
*)
procedure ImportOldPresets(APresets: VisTypeUnit.TPresets);
var
  I: Integer;
begin
  for I:=0 to APresets.Count-1
    do ImportOldPreset(APresets[I]);
end;

{%ENDREGION}

end.

