unit MCanvasUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType, MInterfacedObject, PluginType, PictureType,
  SrcUIDArray, SourceType;

type
  TEmptyEvent= procedure of object;

  TMCanvas1  = class (TMInterfacedObject, IMCanvas)
  private
    FFrameStarted: TEmptyEvent;
  protected
    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;

    function GetHeight: CvFloat; stdcall; virtual; abstract;
    function GetWidth: CvFloat; stdcall; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(AFrameStarted: TEmptyEvent); virtual;
    procedure FrameStart; virtual;
    procedure FrameEnd; virtual; abstract;
    procedure Resized; virtual;

    procedure Line(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall; virtual; abstract;
    procedure FillRect(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall; virtual; abstract;
    procedure Vanish(const Color: CvColor); stdcall; virtual; abstract;
    procedure Rotate(const Phi: CvFloat; const Color: CvColor); stdcall; virtual; abstract;
    procedure Zoom(const X,Y: CvFloat; const Color: CvColor); stdcall; virtual; abstract;
    procedure Move(const X,Y: CvFloat); stdcall; virtual; abstract;
    procedure Clear(const Color: CvColor); stdcall; virtual; abstract;

    property Height: CvFloat read GetHeight;
    property Width: CvFloat read GetWidth;
  end;

  TMCanvas2  = class (TMCanvas1,IMCanvas2)
  private
  protected
    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
  public
    procedure Draw(X,Y: MVSInt; Pic: IMPicture); stdcall; virtual; abstract;
    procedure StretchDraw(X1,Y1,X2,Y2: MVSInt; Pic: IMPicture); stdcall; virtual; abstract;
    procedure RectDraw(const SrcRect,DestRect: MVRect; Pic: IMPicture); stdcall; virtual; abstract;
  end;

  TMCanvas   = class (TMCanvas2)

  end;

  TMSourcedCanvas = class (TMCanvas2, IMSourcedCanvas)
  private
    FSources: TSrcUIDArray;
  protected
    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;
    function GetSource(ID: MVSourceID): IMSource; stdcall;

    property _Sources: TSrcUIDArray read FSources;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DeleteSource(AID: MVSourceID); stdcall;

    property Sources[ID: MVSourceID]: IMSource read GetSource;
  end;

implementation

{TMCanvas1}

constructor TMCanvas1.Create;
begin
  inherited Create;
end;

destructor TMCanvas1.Destroy;
begin
  inherited Destroy;
end;

procedure TMCanvas1.Reset(AFrameStarted: TEmptyEvent);
begin
  FFrameStarted:=AFrameStarted;
  Resized;
end;

procedure TMCanvas1.FrameStart;
begin
  FFrameStarted();
end;

procedure TMCanvas1.Resized;
begin
  //do nothing
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TMCanvas1.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TMCanvas1.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IMCanvas(Self))
    else Result:=inherited Future(Version);
end;

{TMCanvas2}

const
  Local_Version2: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TMCanvas2.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version2;
end;

function TMCanvas2.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version2
    then Result:=IMInterface(IMCanvas2(Self))
    else Result:=inherited Future(Version);
end;

{TMSourcedCanvas}

const
  Local_Version3: MVVersion = (Version:0;MainVersion:3;SubVersion:0);

constructor TMSourcedCanvas.Create;
begin
  inherited Create;
  FSources:=TSrcUIDArray.Create;
end;

destructor TMSourcedCanvas.Destroy;
begin
  FSources.Destroy;
  inherited Destroy;
end;

function TMSourcedCanvas.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version3;
end;

function TMSourcedCanvas.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version3
    then Result:=IMInterface(IMSourcedCanvas(Self))
    else Result:=inherited Future(Version);
end;
function TMSourcedCanvas.GetSource(ID: MVSourceID): IMSource; stdcall;
begin
  if FSources.ValidID(ID)
    then Result:=FSources[ID]
    else Result:=nil;
end;

procedure TMSourcedCanvas.DeleteSource(AID: MVSourceID); stdcall;
begin
  FSources.DeleteItem(AID);
end;

end.

