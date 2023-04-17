unit GLContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, dglOpenGL;

type
  TGLCOptions       = set of (glcDoubleBuffered,glcStereo);

  TGLContext        = class
  private
  public
    constructor Create(AHandle: HWND; MajorVersion, MinorVersion: Integer; ForwardCompatible: Boolean = true; Options: TGLCOptions = [glcDoubleBuffered]; ColorBits: Integer = 32; DepthBits: Integer = 24; StencilBits: Integer = 0; AccumBits: Integer = 0; AuxBuffers: Integer = 0; Layer: Integer = 0; ASharedContext: TGLContext = nil); virtual;
    destructor Destroy; override;
    procedure Activate; virtual; abstract;
    //not necessary to deactivate a conext before activatimg another one
    procedure Deactivate; virtual; abstract;
    procedure SwapBuffers; virtual; abstract;
  end;

  EGLContext        = class (Exception)
  public
    constructor Create(AMsg: string);
  end;

function GLToBool(AGLBool: GLInt): Boolean; inline;
function BoolToGL(ABool: Boolean): GLInt; inline;

implementation

{%REGION TGLContext}

constructor TGLContext.Create(AHandle: HWND; MajorVersion, MinorVersion: Integer; ForwardCompatible: Boolean = true; Options: TGLCOptions = [glcDoubleBuffered]; ColorBits: Integer = 32; DepthBits: Integer = 24; StencilBits: Integer = 0; AccumBits: Integer = 0; AuxBuffers: Integer = 0; Layer: Integer = 0; ASharedContext: TGLContext = nil);
begin
  inherited Create;
end;

destructor TGLContext.Destroy;
begin
  inherited Destroy;
end;

{%ENDREGION}
{%REGION EGLContext}

constructor EGLContext.Create(AMsg: string);
begin
  inherited Create(AMsg);
end;

{%ENDREGION}
{%REGION General}

function GLToBool(AGLBool: GLInt): Boolean; inline;
begin
  Result:=Boolean(AGLBool);
end;

function BoolToGL(ABool: Boolean): GLInt; inline;
begin
  Result:=GLInt(ABool);
end;

{%ENDREGION}

end.

