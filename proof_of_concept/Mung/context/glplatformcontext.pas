unit GLPlatformContext;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF LINUX}
  GLGLXContext,    //Linux
  {$ENDIF}
  {$IF Defined(WIN32) or Defined(WIN64)}
  GLWGLContext,    //Windows
  {$ENDIF}
  {$IFDEF DARWIN}
  GLCarbonContext, //MacOS
  {$ENDIF}
  Classes, SysUtils, GLContext;

type
  {$IFDEF LINUX}
  TGLCrossContext = TGLGLXContext;
  {$ENDIF}
  {$IF Defined(WIN32) or Defined(WIN64)}
  TGLCrossContext = TGLWGLContext;
  {$ENDIF}
  {$IFDEF DARWIN}
  TGLCrossContext = TGLCarbonContext;
  {$ENDIF}

implementation

end.

