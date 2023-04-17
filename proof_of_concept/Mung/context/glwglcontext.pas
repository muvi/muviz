unit GLWGLContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL;

type
  TGLWGLContext = class (TGLContext)
  private
    FDC: HWND;
    FRC: HGLRC;
  public
    constructor Create(AHandle: HWND; MajorVersion, MinorVersion: Integer; ForwardCompatible: Boolean = true; Options: TGLCOptions = [glcDoubleBuffered]; ColorBits: Integer = 32; DepthBits: Integer = 24; StencilBits: Integer = 0; AccumBits: Integer = 0; AuxBuffers: Integer = 0; Layer: Integer = 0; ASharedContext: TGLContext = nil); override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure SwapBuffers; override;
  end;

implementation

{%REGION TGLWGLContext}

constructor TGLWGLContext.Create(AHandle: HWND; MajorVersion, MinorVersion: Integer; ForwardCompatible: Boolean = true; Options: TGLCOptions = [glcDoubleBuffered]; ColorBits: Integer = 32; DepthBits: Integer = 24; StencilBits: Integer = 0; AccumBits: Integer = 0; AuxBuffers: Integer = 0; Layer: Integer = 0; ASharedContext: TGLContext = nil);
var
  AOptions: TRCOptions;
begin
  inherited Create(AHandle,MajorVersion,MinorVersion,ForwardCompatible,Options,ColorBits,DepthBits,StencilBits,AccumBits,AuxBuffers,Layer,ASharedContext);
  FDC:=GetDC(AHandle);
  AOptions:=[];
  if glcDoubleBuffered in Options then Include(AOptions,opDoubleBuffered);
  if glcStereo in Options then Include(AOptions,opStereo);
  FRC:=CreateRenderingContext(AHandle,AOptions,COlorBits,DepthBits,StencilBits,AccumBits,AuxBuffers,Layer);
  if FSharedContext<>nil then wglShareLists(FRC,TGLWGLContext(ASharedContext).FRC);
end;

destructor TGLWGLContext.Destroy;
begin
  DestroyRenderingContext(FRC);
  inherited Destroy;
end;

procedure TGLWGLContext.Activate;
begin
  ActivateRenderingContext(FDC,FRC);
end;

procedure TGLWGLContext.Deactivate;
begin
  DeactivateRenderingContext;
end;

procedure TGLWGLContext.SwapBuffers;
begin
  SwapBuffers();  (***)
end;

{%ENDREGION}

end.

