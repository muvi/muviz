unit ShaderUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags,
  SimpleVis, VisAddInput, dglOpenGL, VisualisationUtils;

type
  TShader = class (TVisualisationEvents)
  private
    FShaderType: GLEnum;

    FCode      : IPString;
    FVersion   : IPString;
    FSuccess   : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype; AShaderType: GLEnum);
    destructor Destroy; override;
  end;

const
  VIDVERTEXSHADER    : TGUID = '{F02557E2-C0ED-43CE-A84E-6A29E562C5D4}';
  VIDFRAGMENTSHADER  : TGUID = '{F074CE11-248B-450B-B496-06301704A9D1}';

  {
  PIDVERTEXSHADER    : TGUID = '{4EAB6AA4-1E2E-4D3A-BA27-9CBE14827BB9}';
  PIDFRAGMENTSHADER  : TGUID = '{9AEBAAFF-21A3-4362-BCFD-2BC061B10D8A}';
  }

  SHADERCODENAME    = 'Code';
  SHADERVERSIONNAME = 'Version';
  SHADERSUCCESSNAME = 'Success';

procedure Register;

implementation

{%REGION TShader}

procedure ShaderCodeChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                       : IPGL2DCanvas;
  ATop, ALeft                  : Double;

  AProgramObject, AShaderObject: GLHandle;
  AShaderText                  : PGLChar;
  AShaderLength                : GLInt;
begin
  with TShader(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;

    FVersion.Value:=glGetString(GL_SHADING_LANGUAGE_VERSION);

    //AProgramObject:=glCreateProgram();
    //AShaderObject:=glCreateShader(FShaderType);

    //AShaderText:=FCode.Value;
    //AShaderLength:=FCode.Value.Length;
    //glShaderSource(AShaderObject, 1, @AShaderText, @AShaderLength);

    //glCompileShader(AShaderObject);
    {
    glAttachShader(AProgramObject, AShaderObject);
    glDeleteShader(AShaderObject);
    glLinkProgram(AProgramObject);
    }
  end;
end;

procedure CreateVertexShader(APrototype: IPVisualisationPrototype); cdecl;
begin
  TShader.Create(APrototype, GL_VERTEX_SHADER);
end;

procedure CreateFragmentShader(APrototype: IPVisualisationPrototype); cdecl;
begin
  TShader.Create(APrototype, GL_FRAGMENT_SHADER);
end;

constructor TShader.Create(APrototype: IPVisualisationPrototype; AShaderType: GLEnum);
begin
  inherited Create(APrototype);
  FShaderType:=AShaderType;

  FCode:=StringInputs[SHADERCODENAME];
  FCode.AddListener(@ShaderCodeChanged, Self, Environment.Thread);
  FVersion:=StringInputs[SHADERVERSIONNAME];
  FSuccess:=BooleanInputs[SHADERSUCCESSNAME];
end;

destructor TShader.Destroy;
begin
  FCode.RemoveListener(@ShaderCodeChanged, Self);
  FCode:=nil;
  FVersion:=nil;
  FSuccess:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDVERTEXSHADER, @CreateVertexShader);
    with CreatePreset('Vertex Shader', VIDVERTEXSHADER) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Shader');
      AddTag(TAGPREDEFINED);
      //AddInput(This, MAININPUTNAME);
      AddInput(This, SHADERCODENAME, ';');
      AddInput(This, SHADERVERSIONNAME, '');
      AddInput(This, SHADERSUCCESSNAME, false);
    end;
    RegisterVis(VIDFRAGMENTSHADER, @CreateFragmentShader);
    with CreatePreset('Fragment Shader', VIDFRAGMENTSHADER) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Shader');
      AddTag(TAGPREDEFINED);
      //AddInput(This, MAININPUTNAME);
      AddInput(This, SHADERCODENAME, ';');
      AddInput(This, SHADERVERSIONNAME, '');
      AddInput(This, SHADERSUCCESSNAME, false);
    end;
  end;
end;

{%ENDREGION}

initialization
  //dglOpenGL.InitOpenGL;
  //dglOpenGL.ReadExtensions;
  //dglOpenGL.ReadImplementationProperties;
end.

