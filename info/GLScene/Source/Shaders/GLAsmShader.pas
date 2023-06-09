//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLAsmShader<p>

    TGLAsmShader is a wrapper for all ARB shaders<p>
    This component is only a template and has to be replaced with a
    proper version by someone who uses ARB shaders more then me.


	<b>History : </b><font size=-1><ul>
      <li>04/06/10 - Yar - Replace OpenGL functons to OpenGLAdapter
      <li>28/07/09 - DaStr - Added GeomertyShader support (thanks YarUnderoaker)
      <li>24/07/09 - DaStr - TGLShader.DoInitialize() now passes rci
                              (BugTracker ID = 2826217)  
      <li>20/03/07 - DaStr - TGLCustomAsmShader now generates its own events
                             All outside stuff moved back to TGLPhongShader
      <li>22/02/07 - DaStr - Initial version (contributed to GLScene)



    Previous version history:
      v1.0    12 March     '2005  Creation
      v1.1    31 October   '2006  TGLCustomAsmShader.DoUnApply() Fix
                                  TGLAsmShader has more stuff in the published section
}
unit GLAsmShader;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  VectorGeometry, VectorTypes, GLTexture, OpenGL1x, GLContext, ARBProgram,
  GLCustomShader, GLRenderContextInfo;

type
  TGLCustomAsmShader = class;
  TGLAsmShaderEvent = procedure(Shader: TGLCustomAsmShader) of object;
  TGLAsmShaderUnUplyEvent = procedure(Shader: TGLCustomAsmShader; var ThereAreMorePasses: Boolean) of object;

  TGLAsmShaderParameter = class(TGLCustomShaderParameter)
  private
    { Private Declarations }
  protected
    { Protected Declarations }
{
    function GetAsVector1f: Single; override;
    function GetAsVector1i: Integer; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4f: TVector; override;
    function GetAsVector4i: TVector4i; override;

    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;

    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;

    procedure SetAsTexture1D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TGLTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); override;
}
  end;

  TGLCustomAsmShader = class(TGLCustomShader)
  private
    { Private Declarations }
    FVPHandle: Cardinal;
    FFPHandle: Cardinal;
    FGPHandle: Cardinal;

    FOnInitialize: TGLAsmShaderEvent;
    FOnApply: TGLAsmShaderEvent;
    FOnUnApply: TGLAsmShaderUnUplyEvent;
  protected
    { Protected Declarations }
    procedure ApplyShaderPrograms;
    procedure UnApplyShaderPrograms;
    procedure DestroyARBPrograms; virtual;

    function GetVPHandle: Cardinal; virtual;
    function GetFPHandle: Cardinal; virtual;
    function GetGPHandle: Cardinal; virtual;

    property OnApply: TGLAsmShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TGLAsmShaderUnUplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TGLAsmShaderEvent read FOnInitialize write FOnInitialize;

    procedure DoInitialize(var rci : TRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function ShaderSupported: Boolean; override;
  end;

  TGLAsmShader = class(TGLCustomAsmShader)
  published
    property FragmentProgram;
    property VertexProgram;
    property GeometryProgram;

    property OnApply;
    property OnUnApply;
    property OnInitialize;

    property ShaderStyle;
    property FailedInitAction;
  end;

implementation

{ TGLCustomAsmShader }

procedure TGLCustomAsmShader.DoFinalize;
begin
  inherited;
  DestroyARBPrograms;
end;


procedure TGLCustomAsmShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomAsmShader then
  begin
    // Nothing here ...yet
  end;
end;


constructor TGLCustomAsmShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


destructor TGLCustomAsmShader.Destroy;
begin
  DestroyARBPrograms;

  inherited Destroy;
end;

procedure TGLCustomAsmShader.DestroyARBPrograms;
begin
  if FVPHandle <> 0 then
  begin
    GL.DeletePrograms(1, @FVPHandle);
    FVPHandle := 0;
  end;

  if FFPHandle <> 0 then
  begin
    GL.DeletePrograms(1, @FFPHandle);
    FFPHandle := 0;
  end;

  if FGPHandle <> 0 then
  begin
    GL.DeletePrograms(1, @FGPHandle);
    FGPHandle := 0;
  end;
end;

procedure TGLCustomAsmShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  ApplyShaderPrograms();

  if Assigned(FOnApply) then
    FOnApply(Self);
end;


procedure TGLCustomAsmShader.DoInitialize(var rci : TRenderContextInfo; Sender : TObject);
var
  FailedText: string;
begin
  if not ShaderSupported then
  begin
    Enabled := False;
    HandleFailedInitialization;
  end
  else
  begin
    if VertexProgram.Enabled then
      try
        LoadARBProgram(GL_VERTEX_PROGRAM_ARB, VertexProgram.Code.Text, FVPHandle);
      except
        on E: Exception do
        begin
          FailedText := 'VertexProgram error: ' + #13  + E.Message;
          VertexProgram.Enabled := False;
        end;
      end;

    if FragmentProgram.Enabled then
      try
        LoadARBProgram(GL_FRAGMENT_PROGRAM_ARB, FragmentProgram.Code.Text, FFPHandle);
      except
        on E: Exception do
        begin
          FailedText := 'FragmentProgram error: ' + #13  + E.Message;
          FragmentProgram.Enabled := False;
        end;
      end;

    if GeometryProgram.Enabled then
      try
        LoadARBProgram(GL_GEOMETRY_PROGRAM_NV, GeometryProgram.Code.Text, FGPHandle);
      except
        on E: Exception do
        begin
          FailedText := 'GeometryProgram error: ' + #13  + E.Message;
          GeometryProgram.Enabled := False;
        end;
      end;

    Enabled := (FragmentProgram.Enabled or VertexProgram.Enabled or GeometryProgram.Enabled) and
               (FailedText = '');
    
    if Enabled then
    begin
      if Assigned(FOnInitialize) then
        FOnInitialize(Self)
    end
    else
      HandleFailedInitialization(FailedText);
  end;
end;

function TGLCustomAsmShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result)
  else
    Result := False;

  UnApplyShaderPrograms();
end;

function TGLCustomAsmShader.ShaderSupported: Boolean;
begin
  Result := (GL.ARB_vertex_program and GL.ARB_fragment_program);
end;

function TGLCustomAsmShader.GetFPHandle: Cardinal;
begin
  Result := FFPHandle;
end;

function TGLCustomAsmShader.GetVPHandle: Cardinal;
begin
  Result := FVPHandle;
end;

function TGLCustomAsmShader.GetGPHandle: Cardinal;
begin
  Result := FGPHandle;
end;

procedure TGLCustomAsmShader.ApplyShaderPrograms;
begin
  if VertexProgram.Enabled then begin
    GL.Enable(GL_VERTEX_PROGRAM_ARB);
    GL.BindProgram(GL_VERTEX_PROGRAM_ARB, FVPHandle);
  end;
  if FragmentProgram.Enabled then begin
    GL.Enable(GL_FRAGMENT_PROGRAM_ARB);
    GL.BindProgram(GL_FRAGMENT_PROGRAM_ARB, FFPHandle);
  end;
  if GeometryProgram.Enabled then begin
    GL.Enable(GL_GEOMETRY_PROGRAM_NV);
    GL.BindProgram(GL_GEOMETRY_PROGRAM_NV, FGPHandle);
  end;
end;

procedure TGLCustomAsmShader.UnApplyShaderPrograms;
begin
  if VertexProgram.Enabled   then GL.Disable(GL_VERTEX_PROGRAM_ARB);
  if FragmentProgram.Enabled then GL.Disable(GL_FRAGMENT_PROGRAM_ARB);
  if GeometryProgram.Enabled then GL.Disable(GL_GEOMETRY_PROGRAM_NV);
end;

initialization
  RegisterClasses([TGLCustomAsmShader, TGLAsmShader]);

end.
