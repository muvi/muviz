unit ImageConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TImageConfig = class
  strict private
    FTempDrawBitmap                           : TBitmap;
    //Misc
    FImages                                   : TImageList;
    FImageCenter                              : TPoint;
    procedure SetImages(Value: TImageList);
  protected
    function GetOutputBmp: TBitmap; virtual;
    property TempDrawBitmap: TBitmap read FTempDrawBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadImage(Index: Integer); virtual;

    property ImageCenter: TPoint read FImageCenter;
    property Images: TImageList read FImages write SetImages;
    //GetOutputBmp has to be used here, because it is overriden in
    //TPresetEditorImages
    property OutputBmp: TBitmap read GetOutputBmp;
  end;

implementation

{%REGION TImageConfig}

constructor TImageConfig.Create;
begin
  inherited Create;
  FTempDrawBitmap:=TBitmap.Create;
  SetImages(nil);
end;

destructor TImageConfig.Destroy;
begin
  FTempDrawBitmap.Destroy;
  inherited Destroy;
end;

procedure TImageConfig.SetImages(Value: TImageList);
begin
  FImages:=Value;
  if Value<>nil then begin
    FTempDrawBitmap.SetSize(Value.Width,Value.Height);
    FImageCenter:=Classes.Point(Value.Width div 2,Value.Height div 2);
  end else begin
    FTempDrawBitmap.SetSize(10,10);
    FImageCenter:=Classes.Point(5,5);
  end;
end;

procedure TImageConfig.LoadImage(Index: Integer);
begin
  if Index>=0 then FImages.GetBitmap(Index,FTempDrawBitmap) else begin
    with FTempDrawBitmap do begin
      Canvas.Pen.Color:=clGray;
      Canvas.Brush.Color:=clSilver;
      Canvas.Rectangle(0,0,Width,Height);
    end;
  end;
end;

function TImageConfig.GetOutputBmp: TBitmap;
begin
  Result:=FTempDrawBitmap;
end;

{%ENDREGION}

end.

