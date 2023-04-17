unit PictureTypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PictureType, MInterfacedSource, GR32, PluginType,
  GR32CanvasUnit, VisType, Graphics, SourceType, IntfGraphics, GraphType,
  SharedCanvasUnit, GL, GLu, glext;

type
  TGR32Picture = class (TMInterfacedSource, IMPicture, IMPicture1_1)
  private
    FPic     : TPictureContent;
    //FBitmap  : TBitmap32;
    FCanvas  : TGR32Canvas;
    function GetCanvas: IMCanvas; stdcall;
  protected
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
    //used for internal purpose only
    function IDP: Pointer; stdcall; override;

    function GetSrcFormat: MVSourceStr; stdcall; override;
    function GetSrcType: MVSourceStr; stdcall; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: ShortString); stdcall;
    procedure SaveToFile(FileName: ShortString); stdcall;
    procedure SetSize(AWidth,AHeight: CvFloat); stdcall;

    property Canvas: IMCanvas read GetCanvas;
  end;

function NewPicSource: IMSource; stdcall;

implementation

{TGR32Picture}

constructor TGR32Picture.Create;
begin
  inherited Create;
  with FPic do begin
    Bitmap:=TBitmap32.Create;
    TexID:=0;
    //glGenTextures(1,@TexID);
    Changed:=true;
  end;
  FCanvas:=TGR32Canvas.Create(FPic.Bitmap);
end;

destructor TGR32Picture.Destroy;
begin
  FCanvas.Destroy;
  with FPic do begin
    Bitmap.Destroy;
  end;
  inherited Destroy;
end;

const
  Local_Version   : MVVersion = (Version:0;MainVersion:1;SubVersion:0);
  Local_Version1_1: MVVersion = (Version:0;MainVersion:1;SubVersion:1);

function TGR32Picture.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version1_1;
end;

function TGR32Picture.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version1_1
    then Result:=IMInterface(IMPicture1_1(Self))
    else if Version=Local_Version
      then Result:=IMInterface(IMPicture(Self))
      else Result:=inherited Future(Version);
end;

function TGR32Picture.GetSrcFormat: MVSourceStr; stdcall;
begin
  Result:='        ';
end;

function TGR32Picture.GetSrcType: MVSourceStr; stdcall;
begin
  Result:=PicSrcType;
end;

function TGR32Picture.GetCanvas: IMCanvas; stdcall;
begin
  FPic.Changed:=true;
  Result:=FCanvas;
end;

function TGR32Picture.IDP: Pointer; stdcall;
begin
  Result:=@FPic;
end;

procedure TGR32Picture.LoadFromFile(FileName: ShortString); stdcall;
{var
  APicture: Graphics.TPicture;}

  procedure LoadPNG;
  var
    AIntfImage        : TLazIntfImage;
    AReader           : TLazReaderPNG;
    I,J,AWSize        : Integer;
    //ABmpLine,AIntfLine: PColor32Array;
  begin
    AIntfImage:=TLazIntfImage.Create(0,0,[riqfRGB,riqfAlpha]);
    AReader:=TLazReaderPNG.Create;
    AIntfImage.LoadFromFile(FileName,AReader);

    FPic.Bitmap.SetSize(AIntfImage.Width,AIntfImage.Height);
    AWSize:=AIntfImage.Width*4;
    for I:=0 to AIntfImage.Height-1
      do Move(AIntfImage.GetDataLineStart(I)^,FPic.Bitmap.ScanLine[I]^,AWSize);
      {ABmpLine:=FBitmap.ScanLine[I];
      AIntfLine:=AIntfImage.GetDataLineStart(I);
      Move(AIntfLine^,ABmpLine^,);}
    //end;

    AReader.Destroy;
    AIntfImage.Destroy;
  end;

begin
  {APicture:=TPicture.Create;
  APicture.LoadFromFile(FileName);
  FBitmap.Assign(APicture);
  APicture.Destroy; }
  if LowerCase(ExtractFileExt(FileName))='.png'
    then LoadPNG
    else FPic.Bitmap.LoadFromFile(FileName);
  FPic.Changed:=true;



  {with FPic do begin
    if TexID=0 then glGenTextures(1,@TexID);
    glBindTexture(GL_TEXTURE_2D, TexID);
    if Changed then begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      //glTexParameteri(GL_TEXTURE_2D,GL_GENERATE_MIPMAP,GL_TRUE);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, NextPowerOfTwo(Round(Width)), NextPowerOfTwo(Round(Height)),0, GL_RGB, GL_UNSIGNED_BYTE, ATexMem);
      gluBuild2DMipmaps(GL_TEXTURE_2D,4,Bitmap.Width,Bitmap.Height,GL_BGRA,GL_UNSIGNED_BYTE,Bitmap.Bits);
      Changed:=false;
    end;
  end;}
end;

procedure TGR32Picture.SaveToFile(FileName: ShortString); stdcall;
begin
  FPic.Bitmap.SaveToFile(FileName);
end;

procedure TGR32Picture.SetSize(AWidth,AHeight: CvFloat); stdcall;
begin
  FPic.Bitmap.SetSize(Round(AWidth),Round(AHeight));
  FPic.Changed:=true;
end;

function NewPicSource: IMSource; stdcall;
begin
  Result:=IMSource(IMPicture(TGR32Picture.Create));
end;

end.

