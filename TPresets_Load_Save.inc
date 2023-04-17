{%REGION Load&Save v1}

procedure TPresets.SaveToStream1(Stream: TStream);
var
  I,J,L: Integer;
  AType: ^TVisType;
begin
  Stream.Write(Preset1FileID,PresetFileIDSize);
  L:=Length(FPresets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with FPresets[I] do begin
    SaveText(Name,Stream);
    L:=Length(Layers);
    Stream.Write(L,SizeOf(L));
    for J:=0 to L-1 do with Layers[J] do begin
      AType:=TVisualisation.VisPtr(VisID);
      Stream.Write(AType^.VisID,VIS_ID_SIZE);
      Stream.Write(C1,SizeOf(C1));
      Stream.Write(C2,SizeOf(C2));
      Stream.Write(C3,SizeOf(C3));
      Stream.Write(VisParams^,AType^.VisParamSize);
    end;
  end;
end;

function TPresets.LoadFromStream1(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;
var
  I,J,L,Offset: Integer;
  AVisID      : TVisID;
  AVis        : ^TPresetVis;
begin
  if ReadHeader then begin
    if ReadFileID(Stream)=Preset1FileID then Result.Success:=lplOK else begin
      Result.Success:=lplInvalidHeader;
      exit;
    end;
  end else Result.Success:=lplOK;
  if Append then begin
    Offset:=Length(FPresets);
  end else begin
    Offset:=0;
    Clear;
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(FPresets,L);
  for I:=Offset to L-1 do begin
    FPresets[I]:=TPreset.Create;
    with FPresets[I] do begin
      Name:=LoadText(Stream);
      Stream.Read(L,SizeOf(L));
      SetLength(Layers,L);
      WSize:=0;
      for J:=0 to L-1 do begin
        Stream.Read(AVisID,VIS_ID_SIZE);
        AVis:=@Layers[J];
        AVis^.Produce_ID(AVisID,false);
        //AVis^:=CreateVis_ID_NoInit(AVisID);
        with AVis^ do begin
          if VisID=0 then begin
            Result.Success:=lplPluginMissing;
            Result.FailInfo:=AVisID;
            //nicht geladene Presets löschen
            //(nötig, damit die Liste keine nicht
            //erstellten Elemente enthält)
            SetLength(FPresets,I+1);
            exit;
          end;
          Stream.Read(C1,SizeOf(C1));
          Stream.Read(C2,SizeOf(C2));
          Stream.Read(C3,SizeOf(C3));
          with TVisualisation.Visualisations(VisID) do begin
            Stream.Read(VisParams^,VisParamSize);
            WSize+=WorkspaceSize;
          end;
          DestCanvas:=poiDEFAULTCANVAS;
        end;
      end;
    end;
  end;
end;

procedure TPresets.SaveToFile1(const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SaveToStream1(FS);
  FS.Free;
end;

function TPresets.LoadFromFile1(const FileName: string; Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadFromStream1(FS,Append);
  FS.Free;
end;

{%ENDREGION}
{%REGION Load&Save v2}

procedure TPresets.SaveToStream2(Stream: TStream);
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
      AType:=TVisualisation.VisPtr(VisID);
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
      for K:=0 to Length(TVisualisation.Visualisations(VisID).OutputDesc)-1 do with VisOutputs[K] do begin
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
  L:=Length(FPresets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with FPresets[I] do begin
    SaveText(Name,Stream);
    SavePresetLayers(Layers);
    SavePresetLayers(Triggers);
    SaveOutputs(Layers);
    SaveOutputs(Triggers);
  end;
end;

function TPresets.LoadFromStream2(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;
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
    for J:=0 to L-1 do begin
      Stream.Read(AVisID,VIS_ID_SIZE);
      AVis:=@ALayers[J];
      AVis^.Produce_ID(AVisID,false);
      //AVis^:=CreateVis_ID_NoInit(AVisID);
      with AVis^ do begin
        if VisID=0 then begin
          Result.Success:=lplPluginMissing;
          Result.FailInfo:=AVisID;
          exit;
        end;
        Stream.Read(C1,SizeOf(C1));
        Stream.Read(C2,SizeOf(C2));
        Stream.Read(C3,SizeOf(C3));
        with TVisualisation.Visualisations(VisID) do begin
          Stream.Read(VisParams^,VisParamSize);
        end;
      end;
    end;
  end;

  procedure LoadPresetOutputs(var ALayers: TPresetLayers; APreset: TPreset);
  var
    J,K: Integer;
  begin
    for J:=0 to Length(ALayers)-1 do with ALayers[J] do begin
      L:=Length(TVisualisation.Visualisations(VisID).OutputDesc);
      SetLength(VisOutputs,L);
      for K:=0 to L-1 do with VisOutputs[K] do begin
        Stream.Read(Layer,SizeOf(Layer));
        Stream.Read(Param,SizeOf(Param));
      end;
      Stream.Read(DesignPos,SizeOf(DesignPos));
      DestCanvas:=poiDEFAULTCANVAS;
    end;
  end;

var
  I,Offset : Integer;
  APPreset : TPreset;
begin
  if ReadHeader then begin
    if ReadFileID(Stream)=Preset2FileID then Result.Success:=lplOK else begin
      Result.Success:=lplInvalidHeader;
      exit;
    end;
  end else Result.Success:=lplOK;
  if Append then begin
    Offset:=Length(FPresets);
  end else begin
    Offset:=0;
    Clear;
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(FPresets,L);
  for I:=Offset to L-1 do begin
    APPreset:=TPreset.Create;
    FPresets[I]:=APPreset;
    APPreset.Name:=LoadText(Stream);
    LoadPresetLayers(APPreset.Layers);
    if not lplSucceeded(Result.Success) then begin
      //nicht geladene Presets löschen
      //(nötig, damit die Liste keine nicht
      //erstellten Elemente enthält)
      SetLength(FPresets,I+1);
      exit;
    end;
    LoadPresetLayers(APPreset.Triggers);
    if not lplSucceeded(Result.Success) then begin
      //nicht geladene Presets löschen
      //(nötig, damit die Liste keine nicht
      //erstellten Elemente enthält)
      SetLength(FPresets,I+1);
      exit;
    end;
    LoadPresetOutputs(APPreset.Layers,APPreset);
    LoadPresetOutputs(APPreset.Triggers,APPreset);
  end;
end;

procedure TPresets.SaveToFile2(const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SaveToStream2(FS);
  FS.Free;
end;

function TPresets.LoadFromFile2(const FileName: string; Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadFromStream2(FS,Append);
  FS.Free;
end;

{%ENDREGION}
{%REGION Load&Save v3}

procedure TPresets.SaveToStream3(Stream: TStream);

  procedure SavePresetLayers(var ALayers: TPresetLayers);
  var
    J,L2          : Integer;
    AType         : ^TVisType;
    AVisIDModifier: Boolean;
  begin
    L2:=Length(ALayers);
    Stream.Write(L2,SizeOf(L2));
    for J:=0 to L2-1 do with ALayers[J] do begin
      AType:=TVisualisation.VisPtr(VisID);
      Stream.Write(AType^.VisID,VIS_ID_SIZE);
      AVisIDModifier:=(VisID<0);
      Stream.Write(AVisIDModifier,SizeOf(AVisIDModifier));
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
      for K:=0 to Length(TVisualisation.Visualisations(VisID).OutputDesc)-1 do with VisOutputs[K] do begin
        Stream.Write(Layer,SizeOf(Layer));
        Stream.Write(Param,SizeOf(Param));
      end;
      Stream.Write(DesignPos,SizeOf(DesignPos));
      Stream.Write(Expanded,SizeOf(Expanded));
    end;
  end;

  procedure SavePPO(var AParams: TNamedPresetOutputs);
  var
    J,L2: Integer;
  begin
    L2:=Length(AParams);
    Stream.Write(L2,SizeOf(L2));
    for J:=0 to L2-1 do with AParams[J] do begin
      Stream.Write(OutputInfo,SizeOf(OutputInfo));
      SaveText(Name,Stream);
    end;
  end;

var
  I,L: Integer;
begin
  Stream.Write(Preset3FileID,PresetFileIDSize);
  L:=Length(FPresets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with FPresets[I] do begin
    SaveText(Name,Stream);
    SavePresetLayers(Layers);
    SavePresetLayers(Triggers);
    SaveOutputs(Layers);
    SaveOutputs(Triggers);
    SavePPO(Params);
    SavePPO(Outputs);
  end;
end;

function TPresets.LoadFromStream3(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;

  procedure LoadPresetLayers(var ALayers: TPresetLayers);
  var
    J,L2          : Integer;
    AVisID        : TVisID;
    AVis          : ^TPresetVis;
    AVisIDModifier: Boolean;
  begin
    Stream.Read(L2,SizeOf(L2));
    SetLength(ALayers,L2);
    for J:=0 to L2-1 do begin
      Stream.Read(AVisID,VIS_ID_SIZE);
      Stream.Read(AVisIDModifier,SizeOf(AVisIDModifier));
      AVis:=@ALayers[J];
      AVis^.Produce_ID(AVisID,false);
      //AVis^:=CreateVis_ID_NoInit(AVisID);
      with AVis^ do begin
        if VisID=0 then begin
          Result.Success:=lplPluginMissing;
          Result.FailInfo:=AVisID;
          exit;
        end;
        Stream.Read(C1,SizeOf(C1));
        Stream.Read(C2,SizeOf(C2));
        Stream.Read(C3,SizeOf(C3));
        with TVisualisation.Visualisations(VisID) do begin
          Stream.Read(VisParams^,VisParamSize);
        end;
      end;
    end;
  end;

  procedure LoadPresetOutputs(var ALayers: TPresetLayers; var APreset: TPreset);
  var
    J,K,L2: Integer;
  begin
    for J:=0 to Length(ALayers)-1 do with ALayers[J] do begin
      L2:=Length(TVisualisation.Visualisations(VisID).OutputDesc);
      SetLength(VisOutputs,L2);
      for K:=0 to L2-1 do with VisOutputs[K] do begin
        Stream.Read(Layer,SizeOf(Layer));
        Stream.Read(Param,SizeOf(Param));
      end;
      Stream.Read(DesignPos,SizeOf(DesignPos));
      Stream.Read(Expanded,SizeOf(Expanded));
      DestCanvas:=poiDEFAULTCANVAS;
    end;
  end;

  procedure LoadPPO(var AParams: TNamedPresetOutputs);
  var
    J,L2: Integer;
  begin
    Stream.Read(L2,SizeOf(L2));
    SetLength(AParams,L2);
    for J:=0 to L2-1 do with AParams[J] do begin
      Stream.Read(OutputInfo,SizeOf(OutputInfo));
      Name:=LoadText(Stream);
    end;
  end;

var
  I,L,Offset: Integer;
  APPreset  : TPreset;
begin
  if ReadHeader then begin
    if ReadFileID(Stream)=Preset3FileID then Result.Success:=lplOK else begin
      Result.Success:=lplInvalidHeader;
      exit;
    end;
  end else Result.Success:=lplOK;
  if Append then begin
    Offset:=Length(FPresets);
  end else begin
    Offset:=0;
    Clear;
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(FPresets,L);
  for I:=Offset to L-1 do begin
    APPreset:=TPreset.Create;
    FPresets[I]:=APPreset;
    APPreset.Name:=LoadText(Stream);
    LoadPresetLayers(APPreset.Layers);
    if not lplSucceeded(Result.Success) then begin
      //nicht geladene Presets löschen
      //(nötig, damit die Liste keine nicht
      //erstellten Elemente enthält)
      SetLength(FPresets,I+1);
      exit;
    end;
    LoadPresetLayers(APPreset.Triggers);
    if not lplSucceeded(Result.Success) then begin
      //nicht geladene Presets löschen
      //(nötig, damit die Liste keine nicht
      //erstellten Elemente enthält)
      SetLength(FPresets,I+1);
      exit;
    end;
    LoadPresetOutputs(APPreset.Layers,APPreset);
    LoadPresetOutputs(APPreset.Triggers,APPreset);
    LoadPPO(APPreset.Params);
    LoadPPO(APPreset.Outputs);
  end;
end;

procedure TPresets.SaveToFile3(const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SaveToStream3(FS);
  FS.Free;
end;

function TPresets.LoadFromFile3(const FileName: string; Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadFromStream3(FS,Append);
  FS.Free;
end;

{%ENDREGION}
{%REGION Load&Save v4}

procedure TPresets.SaveToStream4(Stream: TStream);

  procedure SavePresetLayers(var ALayers: TPresetLayers);
  var
    J,L2          : Integer;
    AType         : ^TVisType;
    AVisIDModifier: Boolean;
  begin
    L2:=Length(ALayers);
    Stream.Write(L2,SizeOf(L2));
    for J:=0 to L2-1 do with ALayers[J] do begin
      AType:=TVisualisation.VisPtr(VisID);
      Stream.Write(AType^.VisID,VIS_ID_SIZE);
      AVisIDModifier:=(VisID<0);
      Stream.Write(AVisIDModifier,SizeOf(AVisIDModifier));
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
      for K:=0 to Length(TVisualisation.Visualisations(VisID).OutputDesc)-1 do with VisOutputs[K] do begin
        Stream.Write(Layer,SizeOf(Layer));
        Stream.Write(Param,SizeOf(Param));
      end;
      Stream.Write(DesignPos,SizeOf(DesignPos));
      Stream.Write(Expanded,SizeOf(Expanded));
      //Destination Canvas
      with DestCanvas do begin
        Stream.Write(Layer,SizeOf(Layer));
        Stream.Write(Param,SizeOf(Param));
      end;
    end;
  end;

  procedure SavePPO(var AParams: TNamedPresetOutputs);
  var
    J,L2: Integer;
  begin
    L2:=Length(AParams);
    Stream.Write(L2,SizeOf(L2));
    for J:=0 to L2-1 do with AParams[J] do begin
      Stream.Write(OutputInfo,SizeOf(OutputInfo));
      SaveText(Name,Stream);
    end;
  end;

var
  I,L: Integer;
begin
  Stream.Write(Preset4FileID,PresetFileIDSize);
  L:=Length(FPresets);
  Stream.Write(L,SizeOf(L));
  for I:=0 to L-1 do with FPresets[I] do begin
    SaveText(Name,Stream);
    SavePresetLayers(Layers);
    SavePresetLayers(Triggers);
    SaveOutputs(Layers);
    SaveOutputs(Triggers);
    SavePPO(Params);
    SavePPO(Outputs);
  end;
end;

function TPresets.LoadFromStream4(Stream: TStream; Append: Boolean = false; ReadHeader: Boolean = true): TMuviLoadResult;

  procedure LoadPresetLayers(var ALayers: TPresetLayers);
  var
    J,L2          : Integer;
    AVisID        : TVisID;
    AVis          : ^TPresetVis;
    AVisIDModifier: Boolean;
  begin
    Stream.Read(L2,SizeOf(L2));
    SetLength(ALayers,L2);
    for J:=0 to L2-1 do begin
      Stream.Read(AVisID,VIS_ID_SIZE);
      Stream.Read(AVisIDModifier,SizeOf(AVisIDModifier));
      AVis:=@ALayers[J];
      AVis^.Produce_ID(AVisID,false);
      //AVis^:=CreateVis_ID_NoInit(AVisID);
      with AVis^ do begin
        if VisID=0 then begin
          Result.Success:=lplPluginMissing;
          Result.FailInfo:=AVisID;
          exit;
        end;
        Stream.Read(C1,SizeOf(C1));
        Stream.Read(C2,SizeOf(C2));
        Stream.Read(C3,SizeOf(C3));
        with TVisualisation.Visualisations(VisID) do begin
          Stream.Read(VisParams^,VisParamSize);
        end;
      end;
    end;
  end;

  procedure LoadPresetOutputs(var ALayers: TPresetLayers; var APreset: TPreset);
  var
    J,K,L2: Integer;
  begin
    for J:=0 to Length(ALayers)-1 do with ALayers[J] do begin
      L2:=Length(TVisualisation.Visualisations(VisID).OutputDesc);
      SetLength(VisOutputs,L2);
      for K:=0 to L2-1 do with VisOutputs[K] do begin
        Stream.Read(Layer,SizeOf(Layer));
        Stream.Read(Param,SizeOf(Param));
      end;
      Stream.Read(DesignPos,SizeOf(DesignPos));
      Stream.Read(Expanded,SizeOf(Expanded));
      //Destination Canvas
      with DestCanvas do begin
        Stream.Read(Layer,SizeOf(Layer));
        Stream.Read(Param,SizeOf(Param));
      end;
    end;
  end;

  procedure LoadPPO(var AParams: TNamedPresetOutputs);
  var
    J,L2: Integer;
  begin
    Stream.Read(L2,SizeOf(L2));
    SetLength(AParams,L2);
    for J:=0 to L2-1 do with AParams[J] do begin
      Stream.Read(OutputInfo,SizeOf(OutputInfo));
      Name:=LoadText(Stream);
    end;
  end;

var
  I,L,Offset: Integer;
  APPreset  : TPreset;
begin
  if ReadHeader then begin
    if ReadFileID(Stream)=Preset4FileID then Result.Success:=lplOK else begin
      Result.Success:=lplInvalidHeader;
      exit;
    end;
  end else Result.Success:=lplOK;
  if Append then begin
    Offset:=Length(FPresets);
  end else begin
    Offset:=0;
    Clear;
  end;
  Stream.Read(L,SizeOf(L));
  L+=Offset;
  SetLength(FPresets,L);
  for I:=Offset to L-1 do begin
    APPreset:=TPreset.Create;
    FPresets[I]:=APPreset;
    APPreset.Name:=LoadText(Stream);
    LoadPresetLayers(APPreset.Layers);
    if not lplSucceeded(Result.Success) then begin
      //nicht geladene Presets löschen
      //(nötig, damit die Liste keine nicht
      //erstellten Elemente enthält)
      SetLength(FPresets,I+1);
      exit;
    end;
    LoadPresetLayers(APPreset.Triggers);
    if not lplSucceeded(Result.Success) then begin
      //nicht geladene Presets löschen
      //(nötig, damit die Liste keine nicht
      //erstellten Elemente enthält)
      SetLength(FPresets,I+1);
      exit;
    end;
    LoadPresetOutputs(APPreset.Layers,APPreset);
    LoadPresetOutputs(APPreset.Triggers,APPreset);
    LoadPPO(APPreset.Params);
    LoadPPO(APPreset.Outputs);
  end;
end;

procedure TPresets.SaveToFile4(const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SaveToStream4(FS);
  FS.Free;
end;

function TPresets.LoadFromFile4(const FileName: string; Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadFromStream4(FS,Append);
  FS.Free;
end;

{%ENDREGION}
{%REGION Load&Save universal}

function TPresets.LoadFromStream(Stream: TStream; Append: Boolean = false): TMuviLoadResult;
var
  AFID: TMuviFileID;
begin
  AFID:=ReadFileID(Stream);
  if AFID=Preset1FileID
    then Result:=LoadFromStream1(Stream,Append,false)
    else if AFID=Preset2FileID
      then Result:=LoadFromStream2(Stream,Append,false)
      else if AFID=Preset3FileID
        then Result:=LoadFromStream3(Stream,Append,false)
        else if AFID=Preset4FileID
          then Result:=LoadFromStream4(Stream,Append,false)
          else Result.Success:=lplInvalidHeader;
end;

procedure TPresets.SaveToStream(Stream: TStream); inline;
begin
  SaveToStream4(Stream);
end;

function TPresets.LoadFromFile(const FileName: string; Append: Boolean = false): TMuviLoadResult;
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadFromStream(FS,Append);
  FS.Free;
end;

procedure TPresets.SaveToFile(const FileName: string);
var
  FS: TStream;
begin
  FS:=TFileStream.Create(FileName,fmCreate or fmShareDenyNone);
  SaveToStream(FS);
  FS.Free;
end;

{%ENDREGION}
