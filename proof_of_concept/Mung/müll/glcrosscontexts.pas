unit glCrossContexts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

{.$I detectsys.inc}

type
  TGLCustomRenderContext = class
  private
  protected
  public
    constructor Create(ARenderDest: HWND); virtual; abstract;
    //destructor Destroy; override;
    procedure Activate; virtual; abstract;
    procedure Deactivate; virtual; abstract;
    procedure SwapBuffers; virtual; abstract;
  end;

  TGLLinuxContext = class (TGLCustomRenderContext)
  private
  public
    constructor Create(ARenderDest: HWND); override;
    destructor Destroy; override;
    procedure Activate
  end;


  TGLRenderContext = TGLLinuxContext;

implementation

{TGLLinuxContext}



end.

