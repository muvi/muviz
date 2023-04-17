unit PresetList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, Controls,
  VisDrawUnit, GR32, CXCtrls, SpectrumData, PseudoSpectrumData, VisTypeUnit,
  Graphics, ExtCtrls, VPBuffers, VPBufferUnit;

type
  TPresetListFrame = class;

  TPresetListThread= class (TThread)
  private
    FDoRender: Boolean;
    FOwner   : TPresetListFrame;
  protected
    procedure Execute; override;
    procedure DoUpdateList;
  public
    constructor Create(AOwner: TPresetListFrame); virtual;
    destructor Destroy; override;
    procedure ReRender;
  end;

  { TPresetListFrame }

  TPresetListFrame = class(TFrame)
    EmptyPreviewIL: TImageList;
    LargePreviewIL: TImageList;
    PresetLV: TListView;
    procedure PresetLVResize(Sender: TObject);
  private
    FPresets      : TPresets;
    FBitmap       : TBitmap32;
    FBitmap2      : Graphics.TBitmap;
    FBufferManager: TVPBufferManager;
    FSpectrumData : TPseudoSpectrumData;
    //FPLT          : TPresetListThread;
    function GetSelected: Integer;
    procedure GetItemImage(const Index: Integer);
  public
    MaxRender: Integer;
    procedure AssignPresets(APresets: TPresets);
    procedure UpdateList;
    procedure UpdateItem(const Index: Integer);
    procedure InitList;
    procedure ListDone;
    property Selected: Integer read GetSelected;
  end;

implementation

{TPresetListThread}

constructor TPresetListThread.Create(AOwner: TPresetListFrame);
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  FOwner:=AOwner;
  FDoRender:=false;
end;

destructor TPresetListThread.Destroy;
begin
  inherited Destroy;
end;

procedure TPresetListThread.Execute;
begin
  while not Terminated do begin
    DoUpdateList;
    Suspend;
  end;
end;

procedure TPresetListThread.ReRender;
begin
  FDoRender:=true;
  Resume;
end;

procedure TPresetListThread.DoUpdateList;
var
  I       : Integer;
begin
  with FOwner do begin
    //PresetLV.Items.Clear;
    //LargePreviewIL.Clear;
    for I:=0 to FPresets.Count-1 do begin
      GetItemImage(I);
      LargePreviewIL.Replace(I,FBitmap2,nil);
      PresetLV.Update;
      with PresetLV.Items[I] do ImageIndex:=ImageIndex;
      //LargePreviewIL.Add(FBitmap2,nil);
      {ANewItem:=PresetLV.Items.Add;
      with ANewItem do begin
        Caption:=FPresets^[I].Name;
        ImageIndex:=I;
      end;}
    end;
  end;
end;

{TPresetListFrame}

procedure TPresetListFrame.InitList;
begin
  MaxRender:=5;
  FBitmap:=TBitmap32.Create;

  FBitmap.BeginUpdate;

  FBitmap.DrawMode:=dmBlend;
  FBitmap.CombineMode:=cmMerge;
  FBitmap.SetSize(LargePreviewIL.Width,LargePreviewIL.Height);

  FBitmap.EndUpdate;
  FBitmap.Changed;

  FBitmap2:=Graphics.TBitmap.Create;
  FBitmap2.Width:=LargePreviewIL.Width;
  FBitmap2.Height:=LargePreviewIL.Height;

  FBufferManager:=TVPBufferManager.Create;
  FSpectrumData:=TPseudoSpectrumData.Create(FBufferManager);
  //FPLT:=TPresetListThread.Create(Self);
  //ControlSet.ControlClass:=TToolBar;
  //ControlSet.SubControlCount:=10;
end;

procedure TPresetListFrame.ListDone;
begin
  //FPLT.Terminate;
  FSpectrumData.Destroy;
  FBufferManager.Destroy;
  FBitmap2.Destroy;
  FBitmap.Destroy;
end;

procedure TPresetListFrame.AssignPresets(APresets: TPresets);
begin
  FPresets:=APresets;
end;

procedure TPresetListFrame.UpdateList;
var
  I       : Integer;
  ANewItem: TListItem;
begin
  {PresetLV.Items.Clear;
  LargePreviewIL.Clear;
  for I:=0 to Length(FPresets^)-1 do begin
    LargePreviewIL.AddImages(EmptyPreviewIL);
    ANewItem:=PresetLV.Items.Add;
    with ANewItem do begin
      Caption:=FPresets^[I].Name;
      ImageIndex:=I;
    end;
  end;

  FPLT.ReRender; }
  PresetLV.Items.Clear;
  LargePreviewIL.Clear;
  for I:=0 to FPresets.Count-1 do begin
    //Application.ProcessMessages;
    //GetItemImage(I);
    //LargePreviewIL.Add(FBitmap2,nil);
    ANewItem:=PresetLV.Items.Add;
    with ANewItem do begin
      Caption:=FPresets[I].Name;
      ImageIndex:=-1;
    end;
  end;
end;

procedure TPresetListFrame.GetItemImage(const Index: Integer);
var
  ARect: TRect;
  I    : Integer;
begin
  with ARect do begin
    Left:=0;
    Top:=0;
    Right:=LargePreviewIL.Width;
    Bottom:=LargePreviewIL.Height;
  end;
  for I:=0 to MaxRender do {RenderPresetPreview(FPresets^[Index],FBitmap,FSpectrumData)};
  FBitmap2.Canvas.CopyRect(ARect,FBitmap.Canvas,ARect);
end;

procedure TPresetListFrame.UpdateItem(const Index: Integer);
begin
  GetItemImage(Index);
  LargePreviewIL.Replace(Index,FBitmap2,nil);
end;

procedure TPresetListFrame.PresetLVResize(Sender: TObject);
begin
  PresetLV.ViewStyle:=PresetLV.ViewStyle;
end;

function TPresetListFrame.GetSelected: Integer;
begin
  Result:=PresetLV.Selected.Index;
end;

initialization
  {$I presetlist.lrs}

end.

