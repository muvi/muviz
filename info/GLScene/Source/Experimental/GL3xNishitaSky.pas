//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xNishitaSky <p>

   GPU Crysis Atmospheric scattering.<p>
   There is two methods for update: <p>
    the first one just compute all nasty integral and <p>
    the other (FastUpdate) one use a precomputed texture to speed up thing <p>
    at the cost of loosing some quality when sun is near horizon.<p>

   <b>History : </b><font size=-1><ul>
      <li>08/03/10 - Yar - Creation
   </ul></font><p>
}

unit GL3xNishitaSky;

interface

{$I GLScene.inc}
{.$DEFINE NISHITA_SKY_DEBUG_MODE}

uses
  // VCL
  SysUtils, Classes,

  // GLScene
  GLScene, GLCadencer, OpenGL1x, VectorGeometry, VectorTypes, VectorGeometryEXT,
  GLState, GLContext, GLTexture, GLGraphics,
  GLTextureFormat, GLFBO,
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  GLSLog,
{$ENDIF}
  GL3xObjects, GLShadersManager, GLVBOManagers, GL3xMaterial,
  GLRenderContextInfo;

type

  TGLNishitaSkyChange = (nscConstants, nscRayleighMie, nscOpticalDepth,
    nscTime);
  TGLNishitaSkyChanges = set of TGLNishitaSkyChange;
  TGLNishitaSkyTexPrec = (nsp11bit, nsp16bit, nsp32bit);

  TNSConstantBlock = packed record
    WavelengthMie: TVector4fEXT;
    v3HG: TVector4fEXT;
    InvWavelength4: TVector4fEXT;
    v2dRayleighMieScaleHeight: TVector2fEXT;
    InvRayleighMieN: TVector2fEXT;
    InvRayleighMieNLessOne: TVector2fEXT;
    PI: Single;
    InnerRadius: Single;
    OuterRadius: Single;
    fScale: Single;
    KrESun: Single;
    KmESun: Single;
    Kr4PI: Single;
    Km4PI: Single;
    InvOpticalDepthN: Single;
    InvOpticalDepthNLessOne: Single;
    HalfTexelOpticalDepthN: Single;
    tNumSamples: Integer;
    iNumSamples: Integer;
    FakeConstForPadding: Single;
  end;

  TGL3xCustomNishitaSky = class(TGL3xBaseSceneObject)
  private
    { Private Declarations }
    ConstantBlock: TNSConstantBlock;
    FUBO: TGLUniformBufferHandle;
    FDomeDiv: Integer;
    FOpticalDepthN: Integer;
    FRayleighMieN: Integer;
    FMieTexture: TGLTexture;
    FRayleighTexture: TGLTexture;
    FOpticalDepthTexture: TGLTexture;
    OpticalDepthFBO: TGLFrameBuffer;
    RayleighMieFBO: TGLFrameBuffer;
    v3SunDir: TAffineVector;
    FOclock: Double;
    FFastUpdate: Boolean;
    FColorPrecision: TGLNishitaSkyTexPrec;
    FChanges: TGLNishitaSkyChanges;
    procedure SetOclock(Value: Double);
    function StoreOclock: Boolean;
    procedure SetDomeDivision(Value: Integer);
    procedure SetOpticalDepth(Value: Integer);
    procedure SetRayleighMie(Value: Integer);
    procedure SetColorPrecision(const Value: TGLNishitaSkyTexPrec);
    function GetMieScaleHeight: Single;
    function GetRayleighScaleHeight: Single;
    procedure SetMieScaleHeight(Value: Single);
    procedure SetRayleighScaleHeight(Value: Single);
    function StoreMieScaleHeight: Boolean;
    function StoreRayleighScaleHeight: Boolean;
  protected
    { Protected Declarations }
    procedure Initialize(StateCash: TGLStateCache);
    procedure MakeGPUOpticalDepth(StateCash: TGLStateCache);
    procedure MakeGPUMieRayleighBuffer(StateCash: TGLStateCache);
    procedure BuildBufferData(Sender: TGLBaseVBOManager); override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    property Oclock: Double read FOclock write SetOclock stored StoreOclock;
    property FastUpdate: Boolean read FFastUpdate write FFastUpdate
      default False;
    property DomeDivision: Integer read FDomeDiv write SetDomeDivision
      default 128;
    property OpticalDepthSize: Integer read FOpticalDepthN write SetOpticalDepth
      default 256;
    property RayleighMieSize: Integer read FRayleighMieN write SetRayleighMie
      default 256;
    property ColorPrecision: TGLNishitaSkyTexPrec read FColorPrecision write
      SetColorPrecision default nsp11bit;
    property MieScaleHeight: Single read GetMieScaleHeight write
      SetMieScaleHeight stored StoreMieScaleHeight;
    property RayleighScaleHeight: Single read GetRayleighScaleHeight write
      SetRayleighScaleHeight stored StoreRayleighScaleHeight;
  end;

  TGL3xNishitaSky = class(TGL3xCustomNishitaSky)
  published
    property Oclock;
    property FastUpdate;
    property DomeDivision;
    property OpticalDepthSize;
    property RayleighMieSize;
    property ColorPrecision;
    property MieScaleHeight;
    property RayleighScaleHeight;

    property BuiltProperties;
    property Position;
    property Direction;
    property PitchAngle;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property ObjectsSorting;
    property OnProgress;
  end;

implementation
{$IFDEF NISHITA_SKY_DEBUG_MODE}
uses
  GLFileDDS;
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders'}{$ENDIF}
const

  Header_fp: AnsiString =
    '#version 150' + #10#13 +
    'precision highp float;' + #10#13 +
    //    'layout(origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;' + #10#13 +
  'layout(std140) uniform ConstantBlock {' + #10#13 +
    ' vec3 WavelengthMie;' + #10#13 +
    ' vec3 v3HG;' + #10#13 +
    ' vec3 InvWavelength4;' + #10#13 +
    ' vec2 v2dRayleighMieScaleHeight;' + #10#13 +
    ' vec2 InvRayleighMieN;' + #10#13 +
    ' vec2 InvRayleighMieNLessOne;' + #10#13 +
    ' float PI;' + #10#13 +
    ' float InnerRadius;' + #10#13 +
    ' float OuterRadius;' + #10#13 +
    ' float fScale;' + #10#13 +
    ' float KrESun;' + #10#13 +
    ' float KmESun;' + #10#13 +
    ' float Kr4PI;' + #10#13 +
    ' float Km4PI;' + #10#13 +
    ' float InvOpticalDepthN;' + #10#13 +
    ' float InvOpticalDepthNLessOne;' + #10#13 +
    ' float HalfTexelOpticalDepthN;' + #10#13 +
    ' int tNumSamples;' + #10#13 +
    ' int iNumSamples;' + #10#13 +
    '};' + #10#13;

  Share_fp: AnsiString =
    'vec2 GetDensityRatio( float fHeight )' + #10#13 +
    '{' + #10#13 +
    '	float fAltitude = (fHeight - InnerRadius) * fScale;' + #10#13 +
    '	return exp( vec2(-fAltitude) / v2dRayleighMieScaleHeight.xy );' + #10#13 +
    '}' + #10#13 +
    'vec2 t( vec3 P, vec3 Px )' + #10#13 +
    '{' + #10#13 +
    '	vec2 OpticalDepth = vec2(0.0);' + #10#13 +
    '	vec3 v3Vector = Px - P;' + #10#13 +
    '	float fFar = length( v3Vector );' + #10#13 +
    '	vec3 v3Dir = v3Vector / fFar;' + #10#13 +
    '	float fSampleLength = fFar / float(tNumSamples);' + #10#13 +
    '	float fScaledLength = fSampleLength * fScale;' + #10#13 +
    '	vec3 v3SampleRay = v3Dir * fSampleLength;' + #10#13 +
    '	P += v3SampleRay * 0.5;' + #10#13 +
    '	for(int i = 0; i < tNumSamples; i++)' + #10#13 +
    '	{' + #10#13 +
    '		float fHeight = length( P );' + #10#13 +
    '		OpticalDepth += GetDensityRatio( fHeight );' + #10#13 +
    '		P += v3SampleRay;' + #10#13 +
    '	}' + #10#13 +
    '	OpticalDepth *= fScaledLength;' + #10#13 +
    '	return OpticalDepth;' + #10#13 +
    '}' + #10#13 +
    'float HitOuterSphere( vec3 O, vec3 Dir )' + #10#13 +
    '{' + #10#13 +
    '	vec3 L = -O;' + #10#13 +
    '	float B = dot( L, Dir );' + #10#13 +
    '	float C = dot( L, L );' + #10#13 +
    '	float D = C - B * B;' + #10#13 +
    '	float q = sqrt( OuterRadius * OuterRadius - D );' + #10#13 +
    '	float t = B;' + #10#13 +
    '	t += q;' + #10#13 +
    '	return t;' + #10#13 +
    '}' + #10#13;

  Render_vp: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'in vec2 TexCoord0;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'out vec3 vertex;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vertex = -Position;' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * vec4(Position, 1.0);' + #10#13 +
    '}';

  Update_vp: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	gl_Position = vec4(Position, 1.0);' + #10#13 +
    '}';

  Render_fp: AnsiString =
    'const float fExposure = -2.0;' + #10#13 +
    'vec3 HDR( vec3 LDR)' + #10#13 +
    '{' + #10#13 +
    '	return 1.0 - exp( fExposure * LDR );' + #10#13 +
    '}' + #10#13 +
    'float getMiePhase(float fCos, float fCos2)' + #10#13 +
    '{' + #10#13 +
    '	return v3HG.x * (1.0 + fCos2) / pow(v3HG.y - v3HG.z * fCos, 1.5);' + #10#13
    +
    '}' + #10#13 +
    'float getRayleighPhase(float fCos2)' + #10#13 +
    '{' + #10#13 +
    '	return 0.75 * (1.0 + fCos2);' + #10#13 +
    '}' + #10#13 +

  'in vec3 vertex;' + #10#13 +
    'in vec2 texcoord;' + #10#13 +
    'uniform sampler2D Mie;' + #10#13 +
    'uniform sampler2D Rayleigh;' + #10#13 +
    'uniform vec3 v3SunDir;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' float fCos = dot( v3SunDir, vertex ) / length( vertex );' + #10#13 +
    ' float fCos2 = fCos * fCos;' + #10#13 +
    ' vec3 v3RayleighSamples = texture(Rayleigh, texcoord).rgb;' + #10#13 +
    ' vec3 v3MieSamples = texture(Mie, texcoord).rgb;' + #10#13 +
    ' FragColor.rgb = getRayleighPhase(fCos2) * v3RayleighSamples.rgb +' + #10#13
    +
    '  getMiePhase(fCos, fCos2) * v3MieSamples.rgb;' + #10#13 +
    ' FragColor = vec4(HDR( FragColor.rgb ), 1.0);' + #10#13 +
    '}';

  Update_fp: AnsiString =
    'out vec4 Mie;' + #10#13 +
    'out vec4 RayLeigh;' + #10#13 +
    'uniform vec3 v3SunDir;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 Tex0 = (gl_FragCoord.xy - vec2(0.5)) * InvRayleighMieNLessOne.xy;' + #10#13
    +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 1e-3, 0.0 );' + #10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;' + #10#13 +
    ' float AngleXZ = PI * Tex0.y;' + #10#13 +
    ' vec3 v3Dir;' + #10#13 +
    ' v3Dir.x = sin( AngleY ) * cos( AngleXZ  );' + #10#13 +
    ' v3Dir.y = cos( AngleY );' + #10#13 +
    ' v3Dir.z = sin( AngleY ) * sin( AngleXZ  );' + #10#13 +
    ' v3Dir = normalize( v3Dir );' + #10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Dir );' + #10#13 +
    ' vec3 v3Ray = v3Dir;' + #10#13 +
    ' vec3 v3PointP = v3PointPv;' + #10#13 +
    ' float fSampleLength = fFarPvPa / iNumSamples;' + #10#13 +
    ' float fScaledLength = fSampleLength * fScale;' + #10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;' + #10#13 +
    ' v3PointP += v3SampleRay * 0.5;' + #10#13 +
    ' vec3 v3RayleighSum = vec3(0.0);' + #10#13 +
    ' vec3 v3MieSum = vec3(0.0);' + #10#13 +
    ' for( int k = 0; k < iNumSamples; k++ )' + #10#13 +
    ' {' + #10#13 +
    ' 	float PointPHeight = length( v3PointP );' + #10#13 +
    ' 	vec2 DensityRatio = GetDensityRatio( PointPHeight );' + #10#13 +
    ' 	DensityRatio *= fScaledLength;' + #10#13 +
    ' 	vec2 ViewerOpticalDepth = t( v3PointP, v3PointPv );' + #10#13 +
    ' 	float dFarPPc = HitOuterSphere( v3PointP, v3SunDir );' + #10#13 +
    ' 	vec2 SunOpticalDepth = t( v3PointP, v3PointP + v3SunDir * dFarPPc );' + #10#13
    +
    ' 	vec2 OpticalDepthP = SunOpticalDepth.xy + ViewerOpticalDepth.xy;' + #10#13
    +
    ' 	vec3 v3Attenuation = exp( - Kr4PI * InvWavelength4 * OpticalDepthP.x - Km4PI * OpticalDepthP.y );' + #10#13
    +
    ' 	v3RayleighSum += DensityRatio.x * v3Attenuation;' + #10#13 +
    ' 	v3MieSum += DensityRatio.y * v3Attenuation;' + #10#13 +
    ' 	v3PointP += v3SampleRay;' + #10#13 +
    ' }' + #10#13 +
    ' RayLeigh = vec4( v3RayleighSum * KrESun * InvWavelength4, 1.0 );' + #10#13
    +
    ' Mie = vec4( v3MieSum * KmESun * WavelengthMie, 1.0 );' + #10#13 +
    '}';

  UpdateFast_fp: AnsiString =
    'out vec4 Mie;' + #10#13 +
    'out vec4 RayLeigh;' + #10#13 +
    'uniform sampler2D OpticalDepth;' + #10#13 +
    'uniform vec3 v3SunDir;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 pos = gl_FragCoord.xy - vec2(0.5);' + #10#13 +
    ' vec2 Tex0 = pos * InvRayleighMieNLessOne.xy;' + #10#13 +
    ' float Tex1x = pos.x * InvRayleighMieN.x + HalfTexelOpticalDepthN;' + #10#13
    +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 0.001, 0.0 );' + #10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;' + #10#13 +
    ' float AngleXZ = PI * Tex0.y;' + #10#13 +

  ' vec3 v3Dir;' + #10#13 +
    ' v3Dir.x = sin( AngleY ) * cos( AngleXZ  );' + #10#13 +
    ' v3Dir.y = cos( AngleY );' + #10#13 +
    ' v3Dir.z = sin( AngleY ) * sin( AngleXZ  );' + #10#13 +
    ' v3Dir = normalize( v3Dir );' + #10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Dir );' + #10#13 +
    ' vec3 v3Ray = v3Dir;' + #10#13 +
    ' vec3 v3PointP = v3PointPv;' + #10#13 +
    ' float fSampleLength = fFarPvPa / iNumSamples;' + #10#13 +
    ' float fScaledLength = fSampleLength * fScale;' + #10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;' + #10#13 +
    ' v3PointP += v3SampleRay * 0.5;' + #10#13 +

  ' vec3 v3RayleighSum = vec3(0.0);' + #10#13 +
    ' vec3 v3MieSum = vec3(0.0);' + #10#13 +
    ' float SampleU = HalfTexelOpticalDepthN;' + #10#13 +
    ' for( int k = 0; k < iNumSamples; k++ )' + #10#13 +
    ' {' + #10#13 +
    ' 	float PointPHeight = length( v3PointP );' + #10#13 +
    ' 	vec2 DensityRatio = GetDensityRatio( PointPHeight );' + #10#13 +
    ' 	DensityRatio *= fScaledLength;' + #10#13 +
    ' 	vec2 ViewerOpticalDepth = texture( OpticalDepth, vec2( Tex1x, SampleU ) ).rg;' + #10#13
    +
    ' 	float fAngle = dot(v3PointP, v3SunDir) / length(v3PointP);' + #10#13 +
    ' 	float index = ((1.0 - fAngle) * 9.0/ 10.0) * 127.0/128.0;' + #10#13 +
    ' 	vec2 SunOpticalDepth = texture( OpticalDepth, vec2( index, SampleU ) ).ba;' + #10#13
    +
    ' 	vec2 OpticalDepthP = SunOpticalDepth.xy + ViewerOpticalDepth.xy;' + #10#13
    +
    ' 	vec3 v3Attenuation = exp( - Kr4PI * InvWavelength4 * OpticalDepthP.x - Km4PI * OpticalDepthP.y );' + #10#13
    +
    ' 	v3RayleighSum += DensityRatio.x * v3Attenuation;' + #10#13 +
    ' 	v3MieSum += DensityRatio.y * v3Attenuation;' + #10#13 +
    ' 	v3PointP += v3SampleRay;' + #10#13 +
    ' 	SampleU += 1.0 / float(iNumSamples);' + #10#13 +
    ' }' + #10#13 +
    ' RayLeigh = vec4( v3RayleighSum * KrESun * InvWavelength4, 1.0 );' + #10#13
    +
    ' Mie = vec4( v3MieSum * KmESun * WavelengthMie, 1.0 );' + #10#13 +
    '}';

  CreateOpticalDepth_fp: AnsiString =
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 Tex0 = (gl_FragCoord.xy - vec2(0.5)) * vec2( InvOpticalDepthNLessOne, InvOpticalDepthN );' + #10#13
    +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 0.001, 0.0 );' + #10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;' + #10#13 +

  ' vec3 v3Ray;' + #10#13 +
    ' v3Ray.x = sin( AngleY );' + #10#13 +
    ' v3Ray.y = cos( AngleY );' + #10#13 +
    ' v3Ray.z = 0.0;' + #10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Ray );' + #10#13 +
    ' vec3 v3PointP = v3PointPv;' + #10#13 +
    ' float fSampleLength = fFarPvPa / float(iNumSamples);' + #10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;' + #10#13 +
    ' v3PointP += v3SampleRay * 0.5;' + #10#13 +
    ' v3PointP += v3SampleRay * Tex0.y * iNumSamples;' + #10#13 +
    ' vec2 ViewerOpticalDepth = t( v3PointP, v3PointPv );' + #10#13 +
    ' vec2 SunOpticalDepth = t( v3PointP, v3Ray * fFarPvPa + v3PointPv );' + #10#13
    +
    ' FragColor = vec4( ViewerOpticalDepth, SunOpticalDepth.xy );' + #10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

const
  cTexPrec: array[TGLNishitaSkyTexPrec] of TGLInternalFormat =
    (tfR11F_G11F_B10F, tfRGBA_FLOAT16, tfRGBA_FLOAT32);

var
  uniformMie,
  uniformRayleigh,
  uniformOpticalDepth,
  uniformSunDir: TGLSLUniform;
  ublockConstants: TGLSLUniformBlock;
  RenderProgram: string;
  RenderVertexObject: string;
  RenderFragmentObject: string;

  UpdateProgram: string;
  UpdateVertexObject: string;
  UpdateFragmentObject: string;

  UpdateFastProgram: string;
  UpdateFastFragmentObject: string;

  CreateOpticalDepthProgram: string;
  CreateOpticalDepthFragmentObject: string;

  ProgramsLinked: Boolean = False;
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  SaveOnce: Boolean = true;
{$ENDIF}

constructor TGL3xCustomNishitaSky.Create(AOwner: TComponent);
const
  ESun: Extended = 20.0;
  Kr: Extended = 0.0025;
  Km: Extended = 0.0010;
  g: Extended = -0.995;
  g2: Extended = 0.995 * 0.995;

  procedure SetupTexture(tex: TGLTexture);
  begin
    with tex do
    begin
      ImageClassName := TGLBlankImage.ClassName;
      TextureWrap := twSeparate;
      TextureWrapS := twClampToEdge;
      TextureWrapR := twClampToEdge;
      MinFilter := miLinear;
      TextureFormatEx := cTexPrec[FColorPrecision];
      Disabled := false;
    end;
  end;

begin
  inherited;

  ConstantBlock.PI := PI;
  ConstantBlock.KrESun := Kr * ESun;
  ConstantBlock.KmESun := Km * ESun;
  ConstantBlock.Kr4PI := Kr * 4.0 * PI;
  ConstantBlock.Km4PI := Km * 4.0 * PI;
  ConstantBlock.InnerRadius := 6356.7523142;
  ConstantBlock.OuterRadius := ConstantBlock.InnerRadius * 1.0157313;
  // karman line
  ConstantBlock.fScale := 1.0 / (ConstantBlock.OuterRadius -
    ConstantBlock.InnerRadius);
  ConstantBlock.v2dRayleighMieScaleHeight := VectorMakeEXT(0.5, 0.1);
  ConstantBlock.InvWavelength4 := VectorMakeEXT(
    1.0 / power(0.650, 4),
    1.0 / power(0.570, 4),
    1.0 / power(0.475, 4),
    0);
  ConstantBlock.WavelengthMie := VectorMakeEXT(
    power(0.650, -0.84),
    power(0.570, -0.84),
    power(0.475, -0.84),
    0);
  ConstantBlock.v3HG := VectorMakeEXT(
    1.5 * (1.0 - g2) / (2.0 + g2),
    1.0 + g2,
    2.0 * g,
    0);

  ConstantBlock.tNumSamples := 50;
  ConstantBlock.iNumSamples := 20;
  FDomeDiv := 128;
  FOpticalDepthN := 256;
  FRayleighMieN := 256;

  ConstantBlock.InvOpticalDepthN := 1.0 / FOpticalDepthN;
  ConstantBlock.InvOpticalDepthNLessOne := 1.0 / (FOpticalDepthN - 1);
  ConstantBlock.HalfTexelOpticalDepthN := 0.5 / FOpticalDepthN;
  ConstantBlock.InvRayleighMieN := VectorMakeEXT(
    1.0 / FRayleighMieN,
    1.0 / (FRayleighMieN / 2));
  ConstantBlock.InvRayleighMieNLessOne := VectorMakeEXT(
    1.0 / (FRayleighMieN - 1),
    1.0 / (FRayleighMieN / 2 - 1));

  FChanges := [];
  SetOclock(12.0);

  FUBO := TGLUniformBufferHandle.Create;

  OpticalDepthFBO := TGLFrameBuffer.Create;
  RayleighMieFBO := TGLFrameBuffer.Create;
  FMieTexture := TGLTexture.Create(Self);
  FRayleighTexture := TGLTexture.Create(Self);
  FOpticalDepthTexture := TGLTexture.Create(Self);
  FColorPrecision := nsp11bit;
  SetupTexture(FMieTexture);
  SetupTexture(FRayleighTexture);
  SetupTexture(FOpticalDepthTexture);

  FFastUpdate := false;
end;

destructor TGL3xCustomNishitaSky.Destroy;
begin
  FUBO.Destroy;
  FMieTexture.Destroy;
  FRayleighTexture.Destroy;
  FOpticalDepthTexture.Destroy;
  OpticalDepthFBO.Destroy;
  RayleighMieFBO.Destroy;
  inherited;
end;

procedure TGL3xCustomNishitaSky.Initialize(StateCash: TGLStateCache);
const
  cDrawBuffers: array[0..1] of GLenum =
    (
    GL_COLOR_ATTACHMENT0,
    GL_COLOR_ATTACHMENT1
    );
var
  s: string;
begin
  // Initialize shaders
  if Length(RenderProgram) = 0 then
  begin

    with ShadersManager do
    begin
      BeginWork;
      // Register uniforms
      uniformMie := TGLSLUniform.RegisterUniform('Mie');
      uniformRayleigh := TGLSLUniform.RegisterUniform('Rayleigh');
      uniformOpticalDepth := TGLSLUniform.RegisterUniform('OpticalDepth');
      uniformSunDir := TGLSLUniform.RegisterUniform('v3SunDir');
      ublockConstants := TGLSLUniformBlock.RegisterUniformBlock('ConstantBlock');

      // Give name to new programs and objects
      RenderProgram := MakeUniqueProgramName('SkyRenderProgram');
      RenderVertexObject := MakeUniqueObjectName('SkyRenderVertexObject');
      RenderFragmentObject := MakeUniqueObjectName('SkyRenderFragmentObject');

      UpdateProgram := MakeUniqueProgramName('SkyUpdateProgram');
      UpdateVertexObject := MakeUniqueObjectName('SkyUpdateVertexObject');
      UpdateFragmentObject := MakeUniqueObjectName('SkyUpdateFragmentObject');

      UpdateFastProgram := MakeUniqueProgramName('SkyUpdateFastProgram');
      UpdateFastFragmentObject :=
        MakeUniqueObjectName('SkyUpdateFastFragmentObject');

      CreateOpticalDepthProgram :=
        MakeUniqueProgramName('SkyOpticalDepthProgram');
      CreateOpticalDepthFragmentObject :=
        MakeUniqueObjectName('SkyOpticalDepthFragmentObject');

      // Define programs
      DefineShaderProgram(RenderProgram);
      DefineShaderProgram(UpdateProgram);
      DefineShaderProgram(UpdateFastProgram);
      DefineShaderProgram(CreateOpticalDepthProgram);
      // Define objects
      DefineShaderObject(RenderVertexObject, Render_vp, [ptVertex]);
      DefineShaderObject(RenderFragmentObject, Header_fp + Render_fp,
        [ptFragment]);
      DefineShaderObject(UpdateVertexObject, Update_vp, [ptVertex]);
      DefineShaderObject(UpdateFragmentObject, Header_fp + Share_fp +
        Update_fp, [ptFragment]);
      DefineShaderObject(UpdateFastFragmentObject, Header_fp + Share_fp +
        UpdateFast_fp, [ptFragment]);
      DefineShaderObject(CreateOpticalDepthFragmentObject, Header_fp + Share_fp
        + CreateOpticalDepth_fp, [ptFragment]);
      // Attach objects
      AttachShaderObjectToProgram(RenderVertexObject, RenderProgram);
      AttachShaderObjectToProgram(RenderFragmentObject, RenderProgram);
      AttachShaderObjectToProgram(UpdateVertexObject, UpdateProgram);
      AttachShaderObjectToProgram(UpdateFragmentObject, UpdateProgram);
      AttachShaderObjectToProgram(UpdateVertexObject, UpdateFastProgram);
      AttachShaderObjectToProgram(UpdateFastFragmentObject,
        UpdateFastProgram);
      AttachShaderObjectToProgram(UpdateVertexObject,
        CreateOpticalDepthProgram);
      AttachShaderObjectToProgram(CreateOpticalDepthFragmentObject,
        CreateOpticalDepthProgram);
      // Link programs
      ProgramsLinked := LinkShaderProgram(RenderProgram);
      ProgramsLinked := ProgramsLinked and LinkShaderProgram(UpdateProgram);
      ProgramsLinked := ProgramsLinked and
        LinkShaderProgram(UpdateFastProgram);
      ProgramsLinked := ProgramsLinked and
        LinkShaderProgram(CreateOpticalDepthProgram);
      EndWork;
    end;
  end;
  // Initialize constant buffer
  if FUBO.Handle = 0 then
  begin
    with FUBO do
    begin
      AllocateHandle;
      BindBufferData(@ConstantBlock, SizeOf(TNSConstantBlock), GL_STATIC_READ);
      Unbind;
    end;
  end;
  // Initialize textures
  if not FOpticalDepthTexture.IsHandleAllocated and ProgramsLinked then
  begin
    TGLBlankImage(FOpticalDepthTexture.Image).Width := FOpticalDepthN;
    TGLBlankImage(FOpticalDepthTexture.Image).Height := FOpticalDepthN;
    FOpticalDepthTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    TGLBlankImage(FRayleighTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FRayleighTexture.Image).Height := FRayleighMieN div 2;
    TGLBlankImage(FMieTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FMieTexture.Image).Height := FRayleighMieN div 2;
    FRayleighTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    FMieTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    FMieTexture.AllocateHandle;
    FRayleighTexture.AllocateHandle;
    FOpticalDepthTexture.AllocateHandle;
    OpticalDepthFBO.Bind;
    OpticalDepthFBO.AttachTexture(0, FOpticalDepthTexture);
    Assert(OpticalDepthFBO.GetStringStatus(s) = fsComplete,
      'Framebuffer error: '+s);
    RayleighMieFBO.Bind;
    RayleighMieFBO.AttachTexture(0, FMieTexture);
    RayleighMieFBO.AttachTexture(1, FRayleighTexture);
    glDrawBuffers(2, @cDrawBuffers);
    Assert(OpticalDepthFBO.GetStringStatus(s) = fsComplete,
      'Framebuffer error: '+s);
    MakeGPUOpticalDepth(StateCash);
    Include(FChanges, nscTime);
  end;
  // Updates
  if nscConstants in FChanges then
  begin
    ConstantBlock.InvOpticalDepthN := 1.0 / FOpticalDepthN;
    ConstantBlock.InvOpticalDepthNLessOne := 1.0 / (FOpticalDepthN - 1);
    ConstantBlock.HalfTexelOpticalDepthN := 0.5 / FOpticalDepthN;
    ConstantBlock.InvRayleighMieN := VectorMakeEXT(
      1.0 / FRayleighMieN,
      1.0 / (FRayleighMieN / 2));
    ConstantBlock.InvRayleighMieNLessOne := VectorMakeEXT(
      1.0 / (FRayleighMieN - 1),
      1.0 / (FRayleighMieN / 2 - 1));
    FUBO.BindBufferData(@ConstantBlock, SizeOf(TNSConstantBlock),
      GL_STATIC_READ);
    Exclude(FChanges, nscConstants);
    Include(FChanges, nscTime);
  end;

  if nscRayleighMie in FChanges then
  begin
    TGLBlankImage(FRayleighTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FRayleighTexture.Image).Height := FRayleighMieN div 2;
    TGLBlankImage(FMieTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FMieTexture.Image).Height := FRayleighMieN div 2;
    FRayleighTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    FMieTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    Exclude(FChanges, nscRayleighMie);
    Include(FChanges, nscTime);
  end;

  if nscOpticalDepth in FChanges then
  begin
    TGLBlankImage(FOpticalDepthTexture.Image).Width := FOpticalDepthN;
    TGLBlankImage(FOpticalDepthTexture.Image).Height := FOpticalDepthN;
    FOpticalDepthTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    MakeGPUOpticalDepth(StateCash);
    Exclude(FChanges, nscOpticalDepth);
    Include(FChanges, nscTime);
  end;
end;

procedure TGL3xCustomNishitaSky.MakeGPUOpticalDepth(StateCash: TGLStateCache);
{$IFDEF NISHITA_SKY_DEBUG_MODE}
var
  debugImg: TGLDDSImage;
  uniformBlockSize: Integer;
  useCurrent: Boolean;
  castFormat: TGLInternalFormat;
{$ENDIF}
begin
  OpticalDepthFBO.Bind;

  with StateCash do
  begin
    ViewPort := Vector4iMake(0, 0, FOpticalDepthN, FOpticalDepthN);
    Disable(stBlend);
    Disable(stDepthTest);
    Disable(stCullFace);
    DepthWriteMask := False;
    SetDepthRange(0, 1);
  end;

  ShadersManager.UseProgram(CreateOpticalDepthProgram);

  FUBO.BindRange(ublockConstants.Location, 0, SizeOf(TNSConstantBlock));

  with DynamicVBOManager do
  begin
    BeginObject(nil);
    Attribute3f(attrPosition, -1, -1, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    EmitVertex;
    Attribute3f(attrPosition, -1, 1, 0);
    EmitVertex;
    Attribute3f(attrPosition, 1, -1, 0);
    EmitVertex;
    Attribute3f(attrPosition, 1, 1, 0);
    EmitVertex;
    EndPrimitives;
    EndObject;
  end;
  OpticalDepthFBO.Unbind;

{$IFDEF NISHITA_SKY_DEBUG_MODE}
  debugImg := TGLDDSImage.Create;
  if FColorPrecision = nsp11bit then
  begin
    useCurrent := false;
    castFormat := tfRGBA_FLOAT16;
  end
  else
  begin
    useCurrent := true;
    castFormat := tfRGBA8;
  end;
  debugImg.AssignFromTexture(FOpticalDepthTexture.RenderingContext,
    FOpticalDepthTexture.Handle,
    FOpticalDepthTexture.Image.NativeTextureTarget, useCurrent, castFormat);
  debugImg.SaveToFile('OpticalDepth.dds');
  debugImg.Free;
  GLSLogger.Log(Format('GPU Uniform block size = %d', [ublockConstants.DataSize]));
  uniformBlockSize := SizeOf(TNSConstantBlock);
  GLSLogger.Log(Format('CPU Uniform block size = %d', [uniformBlockSize]));
{$ENDIF}
end;

procedure TGL3xCustomNishitaSky.MakeGPUMieRayleighBuffer(
  StateCash: TGLStateCache);
{$IFDEF NISHITA_SKY_DEBUG_MODE}
var
  debugImg: TGLDDSImage;
  useCurrent: Boolean;
  castFormat: TGLInternalFormat;
{$ENDIF}
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if GL_GREMEDY_string_marker then
    glStringMarkerGREMEDY(24, 'MakeGPUMieRayleighBuffer');
{$ENDIF}
  RayleighMieFBO.Bind;

  with StateCash do
  begin
    ViewPort := Vector4iMake(0, 0, FRayleighMieN, FRayleighMieN div 2);
    Disable(stBlend);
    Disable(stDepthTest);
    Disable(stCullFace);
    DepthWriteMask := False;
  end;

  with ShadersManager do
  begin
    if FFastUpdate then
      UseProgram(UpdateFastProgram)
    else
      UseProgram(UpdateProgram);

      FUBO.BindRange(ublockConstants.Location, 0, SizeOf(TNSConstantBlock));
    Uniform3f(uniformSunDir, v3SunDir);

    if FFastUpdate then
      UniformSampler(uniformOpticalDepth, FOpticalDepthTexture.Handle, 0);
  end;

  with DynamicVBOManager do
  begin
    BeginObject(nil);
    Attribute3f(attrPosition, -1, -1, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    EmitVertex;
    Attribute3f(attrPosition, -1, 1, 0);
    EmitVertex;
    Attribute3f(attrPosition, 1, -1, 0);
    EmitVertex;
    Attribute3f(attrPosition, 1, 1, 0);
    EmitVertex;
    EndPrimitives;
    EndObject;
  end;
  RayleighMieFBO.Unbind;
  Exclude(FChanges, nscTime);

{$IFDEF NISHITA_SKY_DEBUG_MODE}
  if SaveOnce then
  begin
    debugImg := TGLDDSImage.Create;
    if FColorPrecision = nsp11bit then
    begin
      useCurrent := false;
      castFormat := tfRGBA_FLOAT16;
    end
    else
    begin
      useCurrent := true;
      castFormat := tfRGBA8;
    end;
    debugImg.AssignFromTexture(FMieTexture.RenderingContext, FMieTexture.Handle,
      FMieTexture.Image.NativeTextureTarget, useCurrent, castFormat);
    debugImg.SaveToFile('Mie.dds');
    debugImg.AssignFromTexture(FRayleighTexture.RenderingContext,
      FRayleighTexture.Handle,
      FRayleighTexture.Image.NativeTextureTarget, useCurrent, castFormat);
    debugImg.SaveToFile('Rayleigh.dds');
    debugImg.Free;
    SaveOnce := false;
  end;
{$ENDIF}
end;

procedure TGL3xCustomNishitaSky.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  M, VM: TMatrix;
  vp: TVector4i;
  storeFrameBuffer: TGLuint;
begin
  // Render self
  if GL_VERSION_3_2 and ARenderSelf then
  begin
    // Store states
    vp := ARci.GLStates.ViewPort;
    storeFrameBuffer := ARci.GLStates.DrawFrameBuffer;

    Initialize(ARci.GLStates);

    if ProgramsLinked then
    begin
      if nscTime in FChanges then
        MakeGPUMieRayleighBuffer(ARci.GLStates);
      // restore states
      ARci.GLStates.ViewPort := vp;
      ARci.GLStates.DrawFrameBuffer := storeFrameBuffer;
      with ShadersManager do
      begin
        UseProgram( RenderProgram );

        VM := TGLSceneBuffer(ARci.buffer).ViewMatrix;
        VM[3, 0] := 0;
        VM[3, 1] := 0;
        VM[3, 2] := 0;
        M := MatrixMultiply(TGLSceneBuffer(ARci.buffer).ModelMatrix, VM);
        M := MatrixMultiply(M, TGLSceneBuffer(ARci.buffer).ProjectionMatrix);
        UniformMat4f(uniformViewProjectionMatrix, M);

        UniformSampler(uniformMie, FMieTexture.Handle, 0);
        UniformSampler(uniformRayleigh, FRayleighTexture.Handle, 1);
        Uniform3f(uniformSunDir, v3SunDir);

        with ARci.GLStates do
        begin
          Disable(stBlend);
          Enable(stDepthTest);
          Enable(stCullFace);
          DepthWriteMask := False;
        end;

        FBuiltProperties.Manager.RenderClient(FBuiltProperties);

        if not ARci.GLStates.ForwardContext then
          UseFixedFunctionPipeline;

      end;
    end;
  end;

  // Render children
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGL3xCustomNishitaSky.BuildBufferData(Sender: TGLBaseVBOManager);
var
  V1, V2: TAffineVector;
  i, j: Integer;
  StepH, StepV: Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  vTexCoord, TexFactor, uTexCoord0, uTexCoord1: Single;
begin
  // common settings
  StepV := Pi / FDomeDiv;
  StepH := 2 * StepV;
  Phi := Pi / 2;
  Phi2 := Phi - StepH;
  TexFactor := 2 / (FDomeDiv + 2);

  with Sender do
  begin
    BeginObject(BuiltProperties);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute2f(attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    for j := 0 to FDomeDiv div 2 - 1 do
    begin
      Theta := 0;
      SinCos(Phi, SinP, CosP);
      SinCos(Phi2, SinP2, CosP2);
      V1[1] := SinP;
      V2[1] := SinP2;
      uTexCoord0 := (0.5 + j) * TexFactor;
      uTexCoord1 := (j + 1.5) * TexFactor;

      for i := 0 to FDomeDiv div 2 do
      begin
        SinCos(Theta, SinT, CosT);
        V1[0] := CosP * CosT;
        V2[0] := CosP2 * CosT;
        V1[2] := CosP * SinT;
        V2[2] := CosP2 * SinT;
        vTexCoord := (0.5 + i) * TexFactor;
        Attribute2f(attrTexCoord0, uTexCoord1, vTexCoord);
        Attribute3f(attrPosition, V2[0], V2[1], V2[2]);
        EmitVertex;
        Attribute2f(attrTexCoord0, uTexCoord0, vTexCoord);
        Attribute3f(attrPosition, V1[0], V1[1], V1[2]);
        EmitVertex;
        Theta := Theta - StepH;
      end;

      for i := 0 to FDomeDiv div 2 do
      begin
        SinCos(Theta, SinT, CosT);
        V1[0] := CosP * CosT;
        V2[0] := CosP2 * CosT;
        V1[2] := CosP * SinT;
        V2[2] := CosP2 * SinT;
        vTexCoord := 1.0 - (0.5 + i) * TexFactor;
        Attribute2f(attrTexCoord0, uTexCoord1, vTexCoord);
        Attribute3f(attrPosition, V2[0], V2[1], V2[2]);
        EmitVertex;
        Attribute2f(attrTexCoord0, uTexCoord0, vTexCoord);
        Attribute3f(attrPosition, V1[0], V1[1], V1[2]);
        EmitVertex;
        Theta := Theta - StepH;
      end;

      RestartStrip;
      Phi := Phi2;
      Phi2 := Phi2 - StepV;
    end;
    EndPrimitives;
    EndObject;
  end;
  inherited;
end;

procedure TGL3xCustomNishitaSky.Assign(Source: TPersistent);
var
  sky: TGL3xCustomNishitaSky;
begin
  inherited;
  if Source is TGL3xCustomNishitaSky then
  begin
    sky := TGL3xCustomNishitaSky(Source);
    ConstantBlock := sky.ConstantBlock;
    DomeDivision := sky.FDomeDiv;
    OpticalDepthSize := sky.FOpticalDepthN;
    RayleighMieSize := sky.FRayleighMieN;
    ColorPrecision := sky.FColorPrecision;
    MieScaleHeight := sky.GetMieScaleHeight;
    RayleighScaleHeight := sky.GetRayleighScaleHeight;
    Oclock := sky.FOclock;
    FFastUpdate := sky.FFastUpdate;
  end;
end;

procedure TGL3xCustomNishitaSky.SetOclock(Value: Double);
begin
  FOclock := Value;
  Value := frac((Value - 12) / 24);
  v3SunDir := AffineVectorMake(sin(2 * Pi * Value), cos(2 * Pi * Value), 0);
  Include(FChanges, nscTime);
  NotifyChange(Self);
end;

function TGL3xCustomNishitaSky.StoreOclock: Boolean;
begin
  Result := FOclock <> 12.0;
end;

procedure TGL3xCustomNishitaSky.SetDomeDivision(Value: Integer);
begin
  if Value < 16 then
    Value := 16;
  if Value <> FDomeDiv then
  begin
    fDomeDiv := Value;
    StructureChanged;
  end;
end;

procedure TGL3xCustomNishitaSky.SetOpticalDepth(Value: Integer);
begin
  if Value <> FOpticalDepthN then
  begin
    FOpticalDepthN := Value;
    Include(FChanges, nscConstants);
    Include(FChanges, nscOpticalDepth);
    NotifyChange(Self);
  end;
end;

procedure TGL3xCustomNishitaSky.SetRayleighMie(Value: Integer);
begin
  if Value < 32 then
    Value := 32
  else if Value > 2048 then
    Value := 2048;

  if Value <> FRayleighMieN then
  begin
    FRayleighMieN := Value;
    Include(FChanges, nscConstants);
    Include(FChanges, nscRayleighMie);
    NotifyChange(Self);
  end;
end;

procedure TGL3xCustomNishitaSky.SetColorPrecision(const Value:
  TGLNishitaSkyTexPrec);
begin
  if Value <> FColorPrecision then
  begin
    FColorPrecision := Value;
    Include(FChanges, nscRayleighMie);
    Include(FChanges, nscOpticalDepth);
    NotifyChange(Self);
  end;
end;

function TGL3xCustomNishitaSky.GetMieScaleHeight: Single;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight.Y;
end;

function TGL3xCustomNishitaSky.GetRayleighScaleHeight: Single;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight.X;
end;

procedure TGL3xCustomNishitaSky.SetMieScaleHeight(Value: Single);
begin
  if Value <> ConstantBlock.v2dRayleighMieScaleHeight.Y then
  begin
    ConstantBlock.v2dRayleighMieScaleHeight.Y := Value;
    Include(FChanges, nscConstants);
    NotifyChange(Self);
  end;
end;

procedure TGL3xCustomNishitaSky.SetRayleighScaleHeight(Value: Single);
begin
  if Value <> ConstantBlock.v2dRayleighMieScaleHeight.X then
  begin
    ConstantBlock.v2dRayleighMieScaleHeight.X := Value;
    Include(FChanges, nscConstants);
    NotifyChange(Self);
  end;
end;

function TGL3xCustomNishitaSky.StoreMieScaleHeight: Boolean;
const
  MSH: Single = 0.1;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight.Y <> MSH;
end;

function TGL3xCustomNishitaSky.StoreRayleighScaleHeight: Boolean;
const
  RSH: Single = 0.5;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight.X <> RSH;
end;

initialization

  RegisterClasses([TGL3xCustomNishitaSky, TGL3xNishitaSky]);

end.

