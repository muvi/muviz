//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileO3TC<p>

 <b>History : </b><font size=-1><ul>
        <li>31/05/10 - Yar - Fixes for Linux x64
        <li>08/05/10 - Yar - Removed check for residency in AssignFromTexture
        <li>22/04/10 - Yar - Fixes after GLState revision
        <li>27/01/10 - Yar   - Bugfix in BlockOffset with negative result
        <li>23/11/10 - DaStr - Added $I GLScene.inc
        <li>23/01/10 - Yar - Added to AssignFromTexture CurrentFormat parameter
                             Fixed cube map loading bug
        <li>20/01/10 - Yar - Creation
   </ul><p>
}
unit GLFileO3TC;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
  GLCrossPlatform,
  OpenGL1x,
  GLContext,
  GLGraphics,
  GLTextureFormat,
  ApplicationFileIO;

type

  TGLO3TCImage = class(TGLBaseImage)
  public
    class function Capabilities: TDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLuint;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); override;

    property Data: PGLPixel32Array read FData;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Depth: Integer read fDepth;
    property MipLevels: Integer read fMipLevels;
    property ColorFormat: GLenum read fColorFormat;
    property InternalFormat: TGLInternalFormat read fInternalFormat;
    property DataType: GLenum read fDataType;
    property ElementSize: Integer read fElementSize;
  end;

implementation

uses
  VectorGeometry;

const
  O3_TC_RGB_S3TC_DXT1 = 1;
  O3_TC_RGBA_S3TC_DXT5 = 4;
  O3_TC_ATI3DC_ATI2N = 16;

const
  O3_TC_CUBE_MAP = $0001;
  O3_TC_ARRAY = $0002;

type

  TO3TC_Header = record
    Useless: Cardinal;
    Magic: Cardinal; // Magic number: Must be O3TC.
    Size: Cardinal; // Must be filled with sizeof(TO3TC_Header).
    Version: Cardinal; // Version.
  end;

  TO3TC_ChunkHeader = record
    // Must be filled with sizeof(TO3TC_Chunk_Header):
    ChunkHeaderSize: Cardinal;
    // Reserved in JeGX's version 1.0
    Extension: Cardinal;
    // The size of the data chunk that follows this one.
    Size: Cardinal;
    // Reserved
    reserved2: Cardinal;
    // Pixel format:
    // - O3_TC_RGB_S3TC_DXT1 = 1
    // - O3_TC_RGBA_S3TC_DXT5 = 4
    // - O3_TC_ATI3DC_ATI2N = 16
    InternalPixelFormat: Cardinal;
    // Texture width.
    Width: Cardinal;
    // Texture height.
    Height: Cardinal;
    // Texture depth.
    Depth: Cardinal;
    // Number of mipmaps.
    NumMipmaps: Cardinal;
    // The texture name (optional).
    TextureName: array[0..127] of AnsiChar;
    // The texture id (optional).
    TextureId: Cardinal;
  end;

  // ------------------
  // ------------------ TGLO3TCImage ------------------
  // ------------------

  // LoadFromFile
  //

procedure TGLO3TCImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName := filename;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt('File %s not found.', [filename]);
end;

// SaveToFile
//

procedure TGLO3TCImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

// LoadFromStream
//

procedure TGLO3TCImage.LoadFromStream(stream: TStream);
type
  TFOURCC = array[0..3] of AnsiChar;
var
  Header: TO3TC_Header;
  ChunkHeader: TO3TC_ChunkHeader;
  w, h, d, bw, bh, level, face, faces: Integer;
  lData: PByte;
begin
  // Get the O3TC_Header.
  stream.Read(Header, Sizeof(TO3TC_Header));
  // Check for O3TC magic number...
  if TFOURCC(Header.Magic) <> 'O3TC' then
    raise EInvalidRasterFile.Create('Invalid O3TC file.');
  // Get the O3TC_Chunk_Header
  stream.Read(ChunkHeader, Sizeof(TO3TC_ChunkHeader));

  fWidth := ChunkHeader.Width;
  fHeight := ChunkHeader.Height;
  fDepth := ChunkHeader.Depth;
  // Get the number of mipmaps
  if ChunkHeader.NumMipmaps <> 0 then
    fMipLevels := ChunkHeader.NumMipmaps
  else
    fMipLevels := 1;

  if Header.Version > 1 then
  begin
    fCubeMap := (ChunkHeader.Extension and O3_TC_CUBE_MAP) <> 0;
    fTextureArray := (ChunkHeader.Extension and O3_TC_ARRAY) <> 0;
  end
  else
  begin
    fCubeMap := false;
    fTextureArray := false;
  end;

  // Set format properties
  case ChunkHeader.InternalPixelFormat of
    O3_TC_RGB_S3TC_DXT1:
      begin
        fColorFormat := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
        fInternalFormat := tfCOMPRESSED_RGB_S3TC_DXT1;
        fDataType := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
        fElementSize := 8;
      end;
    O3_TC_RGBA_S3TC_DXT5:
      begin
        fColorFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
        fInternalFormat := tfCOMPRESSED_RGBA_S3TC_DXT5;
        fDataType := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
        fElementSize := 16;
      end;
    O3_TC_ATI3DC_ATI2N:
      begin
        fColorFormat := GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI;
        fInternalFormat := tfCOMPRESSED_LUMINANCE_ALPHA_3DC;
        fDataType := GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI;
        fElementSize := 16;
      end;
  else
    raise EInvalidRasterFile.Create('Unsupported O3TC format.')
  end;

  if Integer(ChunkHeader.Size) <> DataSize then
    EInvalidRasterFile.Create('O3TC erroneous image data size.');

  ReallocMem(fData, ChunkHeader.Size);
  // Read raw data
  lData := PByte(fData);
  stream.Read(lData^, ChunkHeader.Size);

  fLevels.Clear;
  if fCubeMap then
    faces := 6
  else
    faces := 1;
  // Make the levels offset list
  for face := 0 to faces do
  begin
    w := Width;
    h := Height;
    d := Depth;
    if d = 0 then
      d := 1;
    for level := 0 to MipLevels - 1 do
    begin
      bw := (w + 3) div 4;
      bh := (h + 3) div 4;
      fLevels.Add(Pointer(PtrUInt(lData) - PtrUInt(fData)));
      Inc(lData, bw * bh * d * fElementSize);
      if w > 1 then
        w := w div 2
      else
        w := 1;
      if h > 1 then
        h := h div 2
      else
        h := 1;
      if d > 1 then
        d := d div 2
      else
        d := 1;
    end;
  end;
end;

// SaveFromStream
//

procedure TGLO3TCImage.SaveToStream(stream: TStream);
const
  Magic: array[0..3] of AnsiChar = 'O3TC';
var
  Header: TO3TC_Header;
  ChunkHeader: TO3TC_ChunkHeader;
begin
  if not ((fColorFormat = GL_COMPRESSED_RGB_S3TC_DXT1_EXT)
    or (fColorFormat = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT)
    or (fColorFormat = GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI)) then
    raise
      EInvalidRasterFile.Create('These image format do not match the O3TC format specification.');
  // Setup Header
  Header.Magic := Cardinal(Magic);
  Header.Size := SizeOf(TO3TC_Header) - SizeOf(Cardinal);
  ChunkHeader.Extension := 0;
  if not (fCubeMap or fTextureArray) then
  begin
    Header.Version := 1;
  end
  else
  begin
    Header.Version := 2;
    if fCubeMap then
      ChunkHeader.Extension := ChunkHeader.Extension or O3_TC_CUBE_MAP;
    if fTextureArray then
      ChunkHeader.Extension := ChunkHeader.Extension or O3_TC_ARRAY;
  end;
  ChunkHeader.ChunkHeaderSize := SizeOf(TO3TC_ChunkHeader);
  ChunkHeader.Width := fWidth;
  ChunkHeader.Height := fHeight;
  ChunkHeader.Depth := fDepth;
  ChunkHeader.NumMipmaps := fMipLevels;
  ChunkHeader.reserved2 := 1;
  FillChar(ChunkHeader.TextureName, 128, 0);
  ChunkHeader.TextureId := 0;
  case fColorFormat of
    GL_COMPRESSED_RGB_S3TC_DXT1_EXT: ChunkHeader.InternalPixelFormat :=
      O3_TC_RGB_S3TC_DXT1;
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: ChunkHeader.InternalPixelFormat :=
      O3_TC_RGBA_S3TC_DXT5;
    GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI: ChunkHeader.InternalPixelFormat :=
      O3_TC_ATI3DC_ATI2N;
  end;
  ChunkHeader.Size := DataSize;
  stream.Write(Header, Sizeof(TO3TC_Header));
  stream.Write(ChunkHeader, Sizeof(TO3TC_ChunkHeader));
  stream.Write(fData[0], ChunkHeader.Size);
end;

// AssignFromTexture
//

procedure TGLO3TCImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLuint;
  textureTarget: TGLTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat, texLod, optLod: Cardinal;
  level, faceCount, face: Integer;
  lData: PByte;
  residentFormat: TGLInternalFormat;
  bCompressed: Boolean;
  vtcBuffer, top, bottom: PByte;
  i, j, k: Integer;
  w, d, h, cw, ch: Integer;
  glTarget: TGLEnum;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (d and -4) then
      Result := fElementSize * (cw * ch * (d and -4) + x +
        cw * (y + ch * (z - 4 * ch)))
    else
      Result := fElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) + (z and
        3));
    if Result < 0 then
      Result := 0;
  end;

begin
  oldContext := CurrentGLContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;
  glTarget := DecodeGLTextureTarget(textureTarget);

  try
    textureContext.GLStates.TextureBinding[0, textureTarget] := textureHandle;
    fMipLevels := 0;
    GL.GetTexParameteriv(glTarget, GL_TEXTURE_MAX_LEVEL, @texLod);
    if glTarget = GL_TEXTURE_CUBE_MAP then
    begin
      fCubeMap := true;
      faceCount := 6;
      glTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
    end
    else
    begin
      fCubeMap := false;
      faceCount := 1;
    end;
    fTextureArray := (glTarget = GL_TEXTURE_1D_ARRAY)
      or (glTarget = GL_TEXTURE_2D_ARRAY)
      or (glTarget = GL_TEXTURE_CUBE_MAP_ARRAY);

    repeat
      // Check level existence
      GL.GetTexLevelParameteriv(glTarget, fMipLevels, GL_TEXTURE_INTERNAL_FORMAT,
        @texFormat);
      if texFormat = 1 then
        Break;
      Inc(fMipLevels);
      if fMipLevels = 1 then
      begin
        GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @fWidth);
        GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT, @fHeight);
        fDepth := 0;
        if (glTarget = GL_TEXTURE_3D)
          or (glTarget = GL_TEXTURE_2D_ARRAY)
          or (glTarget = GL_TEXTURE_CUBE_MAP_ARRAY) then
          glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_DEPTH, @fDepth);
        residentFormat := OpenGLFormatToInternalFormat(texFormat);
        if CurrentFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
        // Get optimal number or MipMap levels
        optLod := GetImageLodNumber(fWidth, fHeight, fDepth);
        if texLod > optLod then
          texLod := optLod;
        // Check for MipMap posibility
        if ((fInternalFormat >= tfFLOAT_R16)
          and (fInternalFormat <= tfFLOAT_RGBA32)) then
          texLod := 1;
      end;
    until fMipLevels = Integer(texLod);

    if fMipLevels > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(FData, DataSize);
      fLevels.Clear;
      lData := PByte(fData);
      bCompressed := IsCompressed;
      vtcBuffer := nil;
      w := fWidth;
      h := fHeight;
      d := fDepth;

      for face := 0 to faceCount - 1 do
      begin
        if fCubeMap then
          glTarget := face + GL_TEXTURE_CUBE_MAP_POSITIVE_X;
        for level := 0 to fMipLevels - 1 do
        begin
          fLevels.Add(Pointer(PtrUInt(lData) - PtrUInt(fData)));
          if bCompressed then
          begin

            if GL_NV_texture_compression_vtc and (d > 0) and not fTextureArray
              then
            begin
              if level = 0 then
                GetMem(vtcBuffer, LevelSize(0));
              GL.GetCompressedTexImage(glTarget, level, vtcBuffer);
              // Shufle blocks from VTC to S3TC
              cw := (w + 3) div 4;
              ch := (h + 3) div 4;
              top := lData;
              for k := 0 to d - 1 do
                for i := 0 to ch - 1 do
                  for j := 0 to cw - 1 do
                  begin
                    bottom := vtcBuffer;
                    Inc(bottom, blockOffset(j, i, k));
                    Move(bottom^, top^, fElementSize);
                    Inc(top, fElementSize);
                  end;
              if w > 1 then
                w := w div 2
              else
                w := 1;
              if h > 1 then
                h := h div 2
              else
                h := 1;
              if d > 1 then
                d := d div 2
              else
                d := 1;
            end
            else
              GL.GetCompressedTexImage(glTarget, level, lData);
          end
          else
            GL.GetTexImage(glTarget, level, fColorFormat, fDataType, lData);

          Inc(lData, LevelSize(level));
        end; // for level
      end; // for face
      if Assigned(vtcBuffer) then
        FreeMem(vtcBuffer);
      // Check memory corruption
      ReallocMem(FData, DataSize);

      if fMipLevels = 0 then
        fMipLevels := 1;
      CheckOpenGLError;
    end;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

// Capabilities
//

class function TGLO3TCImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('o3tc', 'oZone3D Texture Compression', TGLO3TCImage);

end.
