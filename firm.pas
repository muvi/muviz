unit firm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvFunc;

type
  TFirmFileType = byte;

  TFirmObject = class
  private
    FName: string;
  protected
    function GetOType: TFirmFileType; virtual; abstract;
  public
    class function CreateFromStream(Stream: TStream): TFirmObject;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveIndexToStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    property Name: string read FName write FName;
    property OType: TFirmFileType read GetOType;
  end;

  TFirmObjectClass = class of TFirmObject;

  TFirmContainer = class(TFirmObject)
  private
    FContent: array of TFirmObject;
  protected
    function GetOType: TFirmFileType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveIndexToStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TFirmFile = class(TFirmObject)
  private
    FOffset, FSize: UInt64;
  protected
    function GetOType: TFirmFileType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveIndexToStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TFirmFileAlias = class(TFirmObject)
  private
    FOffset, FSize: UInt64;
  protected
    function GetOType: TFirmFileType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    //procedure SaveIndexToStream(Stream: TStream); override;
    //procedure SaveToStream(Stream: TStream); override;
  end;

  TFirm = class
  private
    FStream: TStream;
    FByteOffset: UInt64;
    FContainers: TFirmContainer;
    FTempFilePath: string;
    //FHolded      : Boolean;
  protected
    procedure CopyToStream(Stream: TStream);
    procedure LoadIndex;
  public
    constructor Create(const ATempFilePath: string = 'Temp.tmp');
    destructor Destroy; override;

    procedure AssignStream(Stream: TStream);
    procedure Close;
    procedure CloseAndHold;
    procedure SaveToStream(Stream: TStream);

    property TempFilePath: string read FTempFilePath write FTempFilePath;
  end;

const
  FirmFile = 0;
  FirmFolder = 1;

implementation

const
  MemByteCount = 536870912; //512 MB
  FirmClasses: array [FirmFile..FirmFolder] of TFirmObjectClass =
    (TFirmFile, TFirmContainer);

{TFirm}

constructor TFirm.Create(const ATempFilePath: string = 'Temp.tmp');
begin
  inherited Create;
  FStream := nil;
  FTempFilePath := ATempFilePath;
end;

destructor TFirm.Destroy;
begin
  inherited Destroy;
end;

procedure TFirm.LoadIndex;
begin
  FContainers.LoadFromStream(FStream);
end;

procedure TFirm.AssignStream(Stream: TStream);
begin
  FStream := Stream;
  LoadIndex;
end;

procedure TFirm.Close;
begin
  FStream.Destroy;
  if FileExists(FTempFilePath) then
    DeleteFile(FTempFilePath);
end;

procedure TFirm.CopyToStream(Stream: TStream);
begin
  FStream.Position := FByteOffset;
  Stream.CopyFrom(FStream, FStream.Size - FByteOffset);
end;

procedure TFirm.CloseAndHold;
var
  AStream: TStream;
begin
  if FStream.Size - FByteOffset <= MemByteCount then
    AStream := TMemoryStream.Create
  else
    AStream := TFileStream.Create(FTempFilePath, fmCreate or fmShareDenyNone);
  CopyToStream(AStream);
  FStream.Destroy;
  FStream := AStream;
end;

procedure TFirm.SaveToStream(Stream: TStream);
begin
  FContainers.SaveIndexToStream(Stream);
end;

{TFirmObject}

class function TFirmObject.CreateFromStream(Stream: TStream): TFirmObject;
var
  AOType: TFirmFileType;
  AName: string;
begin
  AName:=LoadText(Stream);
  Stream.Read(AOType, SizeOf(AOType));
  Result := FirmClasses[AOType].Create;
  Result.FName := AName;
end;

{constructor TFirmObject.CreateFromStream(Stream: TStream);
begin
  inherited Create;
end;}

constructor TFirmObject.Create;
begin
  inherited Create;
end;

destructor TFirmObject.Destroy;
begin
  inherited Destroy;
end;

procedure TFirmObject.SaveIndexToStream(Stream: TStream);
var
  AOType: TFirmFileType;
begin
  SaveText(FName, Stream);
  AOType := OType;
  Stream.Write(AOType, SizeOf(AOType));
end;

{TFirmContainer}

function TFirmContainer.GetOType: TFirmFileType;
begin
  Result := FirmFolder;
end;

constructor TFirmContainer.Create;
begin
  inherited Create;
end;

destructor TFirmContainer.Destroy;
begin
  inherited Destroy;
end;

procedure TFirmContainer.LoadFromStream(Stream: TStream);
var
  AFileCount, I: longword;
begin
  Stream.Read(AFileCount, SizeOf(AFileCount));
  SetLength(FContent, AFileCount);
  for I := 0 to AFileCount - 1 do
    FContent[I] := TFirmObject.CreateFromStream(Stream);
end;

procedure TFirmContainer.SaveIndexToStream(Stream: TStream);
var
  AFileCount, I: longword;
begin
  inherited SaveIndexToStream(Stream);
  AFileCount := Length(FContent);
  Stream.Write(AFileCount, SizeOf(AFileCount));
  for I := 0 to AFileCount - 1 do
    FContent[I].SaveIndexToStream(Stream);
end;

procedure TFirmContainer.SaveToStream(Stream: TStream);
var
  I: longword;
begin
  for I := 0 to Length(FContent) - 1 do
    FContent[I].SaveToStream(Stream);
end;

{TFirmFile}

function TFirmFile.GetOType: TFirmFileType;
begin
  Result := FirmFile;
end;

constructor TFirmFile.Create;
begin
  inherited Create;
end;

destructor TFirmFile.Destroy;
begin
  inherited Destroy;
end;

procedure TFirmFile.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FOffset, SizeOf(FOffset));
  Stream.Read(FSize, SizeOf(FSize));
end;

procedure TFirmFile.SaveIndexToStream(Stream: TStream);
begin
  inherited SaveIndexToStream(Stream);
  Stream.Write(FSize, SizeOf(FSize)); //offset?!?!
  //Stream.Write(FSize,SizeOf(FSize));
end;

procedure TFirmFile.SaveToStream(Stream: TStream);
begin

end;

{TFirmFileAlias}

function TFirmFileAlias.GetOType: TFirmFileType;
begin
  Result := FirmFile;
end;

constructor TFirmFileAlias.Create;
begin
  inherited Create;
end;

destructor TFirmFileAlias.Destroy;
begin
  inherited Destroy;
end;

end.

