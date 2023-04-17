unit GR32CanvasUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType, MInterfacedObject, PluginType, MCanvasUnit, GR32,
  GR32_Transforms, GR32_Resamplers, PictureType, SharedCanvasUnit;

type
  TGR32Canvas = class (TMCanvas, IMCanvas)
  private
    FBitmap: TBitmap32;
  protected
    function GetHeight: CvFloat; stdcall; override;
    function GetWidth: CvFloat; stdcall; override;
  public
    constructor Create(ABmp: TBitmap32);
    destructor Destroy; override;

    procedure Reset(AFrameStarted: TEmptyEvent); override;
    procedure FrameStart; override;
    procedure FrameEnd; override;

    procedure Line(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall; override;
    procedure FillRect(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall; override;
    procedure Vanish(const Color: CvColor); stdcall; override;
    procedure Rotate(const Phi: CvFloat; const Color: CvColor); stdcall; override;
    procedure Zoom(const X,Y: CvFloat; const Color: CvColor); stdcall; override;
    procedure Move(const X,Y: CvFloat); stdcall; override;
    procedure Clear(const Color: CvColor); stdcall; override;
    procedure Draw(X,Y: MVSInt; Pic: IMPicture); stdcall; override;
    procedure StretchDraw(X1,Y1,X2,Y2: MVSInt; Pic: IMPicture); stdcall; override;
    procedure RectDraw(const SrcRect,DestRect: MVRect; Pic: IMPicture); stdcall; override;
  end;

implementation

{TGR32Canvas}

constructor TGR32Canvas.Create(ABmp: TBitmap32);
begin
  inherited Create;
  FBitmap:=ABmp;
  FBitmap.DrawMode:=dmBlend;
  FBitmap.CombineMode:=cmMerge;
end;

destructor TGR32Canvas.Destroy;
begin
  FBitmap:=nil;
  inherited Destroy;
end;

procedure TGR32Canvas.Reset(AFrameStarted: TEmptyEvent);
begin
  FBitmap.BeginUpdate;
  FBitmap.Clear($FF000000);
  FBitmap.EndUpdate;
  FBitmap.Changed;
  inherited Reset(AFrameStarted)
end;

procedure TGR32Canvas.FrameStart;
begin
  FBitmap.BeginUpdate;
  inherited FrameStart;
end;

procedure TGR32Canvas.FrameEnd;
begin
  FBitmap.EndUpdate;
  FBitmap.Changed;
end;

function TGR32Canvas.GetHeight: CvFloat; stdcall;
begin
  Result:=FBitmap.Height;
end;

function TGR32Canvas.GetWidth: CvFloat; stdcall;
begin
  Result:=FBitmap.Width;
end;

procedure TGR32Canvas.Line(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall;
begin
  FBitmap.LineFS(X1,Y1,X2,Y2,Color);
end;

procedure TGR32Canvas.FillRect(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall;
begin
  FBitmap.FillRectTS(Round(X1),Round(Y1),Round(X2),Round(Y2),Color);
end;

procedure TGR32Canvas.Vanish(const Color: CvColor); stdcall;
begin
  FBitmap.FillRectTS(0,0,FBitmap.Width,FBitmap.Height,Color);
end;

procedure TGR32Canvas.Rotate(const Phi: CvFloat; const Color: CvColor); stdcall;
var
  Trans       : TAffineTransformation;
  bmp2,bmp3   : TBitmap32;
  Offset,XS,YS: Integer;
  //Trans0: TProjectiveTransformation;
begin
  Trans:=TAffineTransformation.Create;
  //Trans.
  bmp2:=TBitmap32.Create;
  bmp3:=TBitmap32.Create;
  bmp2.BeginUpdate;
  //bmp2.Resampler.Destroy;
  bmp3.BeginUpdate;
  //bmp2.Resampler:=TKernelResampler.Create;
  TLinearResampler.Create(bmp2);
  //TKernelResampler.Create(bmp2);
  //TKernelResampler(bmp2.Resampler).Kernel:=THammingKernel.Create;
  Offset:=Trunc(sqrt(sqr(FBitmap.Width)+sqr(FBitmap.Height)))+1;
  XS:=(Offset-FBitmap.Width) div 2;
  YS:=(Offset-FBitmap.Height) div 2;
  bmp2.Width:=Offset{bmp.Width+60};
  bmp2.Height:=Offset;
  bmp3.Width:=Offset;
  bmp3.Height:=Offset;
  bmp2.Clear(Color);
  bmp2.Draw(XS,YS,FBitmap);
  bmp2.EndUpdate;
  bmp2.Changed;
  //bmp.BeginUpdate;
  //TKernelResampler.Create(bmp);
  //TKernelResampler(bmp.Resampler).Kernel:=THammingKernel.Create;
  //bmp.dr
  //bmp.Clear(Visualisation.C3);
  //bmp.FillRectTS(0,0,bmp.Width,bmp.Height,Visualisation.C3);
  Trans.Clear;
  //Trans.
  Trans.SrcRect:=FloatRect(XS,YS,XS-1+FBitmap.Width,YS-1+FBitmap.Height);
  Trans.Rotate(Offset div 2,Offset div 2,Phi);
  //bmp.Clear(Visualisation.C3);
  Transform(bmp3,bmp2,Trans);
  bmp3.EndUpdate;
  bmp3.Changed;
  FBitmap.Draw(-XS,-YS,bmp3);
  //bmp.EndUpdate;
  //bmp.Changed;
  Trans.Destroy;
  bmp2.Destroy;
  bmp3.Destroy;
end;

procedure TGR32Canvas.Zoom(const X,Y: CvFloat; const Color: CvColor); stdcall;
var
  Trans       : TAffineTransformation;
  bmp2,bmp3   : TBitmap32;
  //Offset,XS,YS: Integer;
  XS,YS,NW,NH  : Integer;
begin
  Trans:=TAffineTransformation.Create;
  bmp2:=TBitmap32.Create;
  bmp3:=TBitmap32.Create;
  bmp2.BeginUpdate;
  bmp3.BeginUpdate;
  TLinearResampler.Create(bmp2);
  //Offset:=Trunc(sqrt(sqr(bmp.Width)+sqr(bmp.Height)))+1;
  //XS:=(Offset-bmp.Width) div 2;
  //YS:=(Offset-bmp.Height) div 2;
  //XS:=bmp
  XS:=Round(((X*FBitmap.Width)-FBitmap.Width)/2);
  YS:=Round(((Y*FBitmap.Height)-FBitmap.Height)/2);

  NW:=Round(FBitmap.Width*X);
  NH:=Round(FBitmap.Height*Y);
  bmp2.Width:=FBitmap.Width;
  bmp2.Height:=FBitmap.Height;
  bmp3.Width:=NW;
  bmp3.Height:=NH;
  bmp2.Clear(Color);
  bmp2.Draw(0,0,FBitmap);
  bmp2.EndUpdate;
  bmp2.Changed;
  Trans.Clear;
  Trans.SrcRect:=FloatRect(0,0,FBitmap.Width,FBitmap.Height);
  Trans.Scale(X,Y);
  //.Rotate(Offset div 2,Offset div 2,AParams.Speed);
  Transform(bmp3,bmp2,Trans);
  bmp3.EndUpdate;
  bmp3.Changed;
  FBitmap.Draw(-XS,-YS,bmp3);
  Trans.Destroy;
  bmp2.Destroy;
  bmp3.Destroy;
end;

procedure TGR32Canvas.Move(const X,Y: CvFloat); stdcall;
var
  bmp2   : TBitmap32;
begin
  bmp2:=TBitmap32.Create;
  bmp2.BeginUpdate;
  bmp2.Width:=FBitmap.Width;
  bmp2.Height:=FBitmap.Height;
  bmp2.Draw(0,0,FBitmap);
  bmp2.EndUpdate;
  bmp2.Changed;
  FBitmap.Draw(Round(X),Round(Y),bmp2);
  bmp2.Destroy;
end;

procedure TGR32Canvas.Clear(const Color: CvColor); stdcall;
begin
  FBitmap.Clear(Color);
end;

procedure TGR32Canvas.Draw(X,Y: MVSInt; Pic: IMPicture); stdcall;
begin
  FBitmap.Draw(X,Y,PPictureContent(Pic.IDP)^.Bitmap);
end;

procedure TGR32Canvas.StretchDraw(X1,Y1,X2,Y2: MVSInt; Pic: IMPicture); stdcall;
var
  ABmp: TBitmap32;
begin
  ABmp:=PPictureContent(Pic.IDP)^.Bitmap;
  FBitmap.Draw(Rect(X1,Y1,X2,Y2),Rect(0,0,ABmp.Width,ABmp.Height),ABmp);
end;

procedure TGR32Canvas.RectDraw(const SrcRect,DestRect: MVRect; Pic: IMPicture); stdcall;
var
  ABmp: TBitmap32;
begin
  ABmp:=PPictureContent(Pic.IDP)^.Bitmap;
  FBitmap.Draw(DestRect,SrcRect,ABmp);
end;

end.

