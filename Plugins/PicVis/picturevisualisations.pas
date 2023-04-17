unit PictureVisualisations;

{$mode objfpc}

interface

uses
  Classes, SysUtils, PictureType, MainType, VisType, SpectrumData, PluginType,
  SourceType;

type
  TPicDrawParams = packed record
    Path: vpString;
  end;

  TPicDrawWS     = packed record
    PicID,NewPicID: MVSourceID;
  end;

  TCanvasParams  = packed record
    Width,Height: vpReal;
  end;

  TCanvasWS      = packed record
    PicID: MVSourceID;
  end;

function ReducePicSize(APic: IMPicture1_1; AWidth,AHeight: CvFloat): IMPicture1_1;
function MakePicFiting(APic: IMPicture1_1; ADest: IMCanvas): IMPicture1_1;

procedure InitPicDraw(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreePicDraw(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangePicDraw(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawPicture(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure PicDrawThreadedChange(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure PicDrawThreadFinish(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitCanvas(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeCanvas(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawCanvas(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

const
  PicDrawIV: TPicDrawParams = (Path:'');
  CanvasIV : TCanvasParams = (Width: 640; Height: 480);

implementation

{Picture Drawer}

function ReducePicSize(APic: IMPicture1_1; AWidth,AHeight: CvFloat): IMPicture1_1;
begin
  with PluginSystem do begin
    Result:=IMPicture1_1(IMPicture(NewSource(PicSrcType)).Future('0.1.1'));
    Result.SetSize(AWidth,AHeight);
    IMCanvas2(Result.Canvas.Future('0.2.0')).StretchDraw(0,0,Round(AWidth),Round(AHeight),APic);
    DeleteSource(APic.SrcID);
  end;
end;

function MakePicFiting(APic: IMPicture1_1; ADest: IMCanvas): IMPicture1_1;
begin
  if APic.Canvas.Width>ADest.Width then begin
    if APic.Canvas.Height>ADest.Height
      then Result:=ReducePicSize(APic,ADest.Height*(APic.Canvas.Width/APic.Canvas.Height),ADest.Height)
      else Result:=ReducePicSize(APic,ADest.Height*(APic.Canvas.Width/APic.Canvas.Height),ADest.Height);
  end else begin
    if APic.Canvas.Height>ADest.Height
      then Result:=ReducePicSize(APic,ADest.Width,ADest.Width*(APic.Canvas.Height/APic.Canvas.Width))
      else Result:=APic;
  end;
end;

procedure InitPicDraw(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TPicDrawParams absolute Params;
  AWorkspace: TPicDrawWS absolute Workspace;
  APic      : IMPicture1_1;
begin
  APic:=IMPicture1_1(IMPicture(PluginSystem.NewSource(PicSrcType)).Future('0.1.1'));
  if FileExists(AParams.Path) then begin
    APic.LoadFromFile(AParams.Path);
    AWorkspace.PicID:=MakePicFiting(APic,Dest).SrcID;
  end else AWorkspace.PicID:=APic.SrcID;
end;

procedure FreePicDraw(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWorkspace: TPicDrawWS absolute Workspace;
begin
  PluginSystem.DeleteSource(AWorkspace.PicID);
end;

procedure ChangePicDraw(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TPicDrawParams absolute Params;
  AWorkspace: TPicDrawWS absolute Workspace;
  APic      : IMPicture;
begin
  if FileExists(AParams.Path) then IVisualisation2(Visualisation.Future('0.2.0')).ThreadPush(@PicDrawThreadedChange,@PicDrawThreadFinish);
end;

procedure PicDrawThreadedChange(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TPicDrawParams absolute Params;
  AWorkspace: TPicDrawWS absolute Workspace;
  APic      : IMPicture1_1;
begin
  APic:=IMPicture1_1(IMPicture(PluginSystem.NewSource(PicSrcType)).Future('0.1.1'));
  APic.LoadFromFile(AParams.Path);
  AWorkspace.NewPicID:=MakePicFiting(APic,Dest).SrcID;
end;

procedure PicDrawThreadFinish(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWorkspace: TPicDrawWS absolute Workspace;
  APic      : IMPicture;
begin
  with AWorkspace do begin
    PluginSystem.DeleteSource(PicID);
    PicID:=NewPicID;
  end;
end;

procedure DrawPicture(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TPicDrawParams absolute Params;
  AWorkspace: TPicDrawWS absolute Workspace;
  APic      : IMPicture;
begin
  APic:=IMPicture(PluginSystem.Sources[AWorkspace.PicID]);
  IMCanvas2(Dest.Future('0.2.0')).StretchDraw(0,0,Round(Dest.Width),Round(Dest.Height),APic);
end;

{Canvas}

procedure InitCanvas(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TCanvasParams absolute Params;
  AWorkspace: TCanvasWS absolute Workspace;
  APic      : IMPicture1_1;
begin
  APic:=IMPicture1_1(IMPicture(PluginSystem.NewSource(PicSrcType)).Future('0.1.1'));
  AWorkspace.PicID:=APic.SrcID;
  APic.SetSize(AParams.Width,AParams.Height);
  IVisualisation3(Visualisation.Future('0.3.0')).SetCanvas(0,APic.Canvas);
end;

procedure FreeCanvas(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWorkspace: TCanvasWS absolute Workspace;
begin
  IVisualisation3(Visualisation.Future('0.3.0')).SetCanvas(0,nil);
  PluginSystem.DeleteSource(AWorkspace.PicID);
end;

procedure DrawCanvas(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TCanvasParams absolute Params;
  AWorkspace: TCanvasWS absolute Workspace;
  APic      : IMPicture;
begin
  APic:=IMPicture(PluginSystem.Sources[AWorkspace.PicID]);
  IMCanvas2(Dest.Future('0.2.0')).StretchDraw(0,0,Round(Dest.Width),Round(Dest.Height),APic);
end;

end.

