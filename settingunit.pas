unit SettingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MFileUtil, AdvType, AdvFunc;

type
  TSetting                = class
  private
    FName: string;
  strict protected
    function GetSize: LongWord; virtual;
  public
    constructor Create(AName: string);
    procedure SetToDefault; virtual;
    procedure LoadFromStream(Stream: TStream; ASize: Cardinal); virtual;
    procedure SaveToStream(Stream: TStream; ASize: Cardinal); virtual;
    property Size: LongWord read GetSize;
    property Name: string read FName;
  end;

  generic TDirectSetting<GType>  = class (TSetting)
  strict private
     type
       PGType = ^GType;
     var
       FDefault: GType;
       FDest   : PGType;
  strict protected
    function GetSize: LongWord; override;
  public
    constructor Create(AName: string; var ADest: GType);
    procedure SetToDefault; override;
    procedure LoadFromStream(Stream: TStream; ASize: Cardinal); override;
    procedure SaveToStream(Stream: TStream; ASize: Cardinal); override;
  end;

  generic TIndirectSetting<GType>= class (TSetting)
  private
    type
      TGetSetting = function : GType of object;
      TSetSetting = procedure (Value: GType) of object;

    var
      FDefault: GType;
      FGet    : TGetSetting;
      FSet    : TSetSetting;

    procedure SetGet(Value: TGetSetting);
    procedure SetSet(Value: TSetSetting);
  protected
    function GetSize: LongWord; override;
  public
    constructor Create(AName: string; AGet: TGetSetting; ASet: TSetSetting);
    constructor Create(AName: string; AGet: TGetSetting; ASet: TSetSetting; ADefault: GType);
    procedure SetToDefault; override;
    procedure LoadFromStream(Stream: TStream; ASize: Cardinal); override;
    procedure SaveToStream(Stream: TStream; ASize: Cardinal); override;

    property GetSetting: TGetSetting read FGet write SetGet;
    property SetSetting: TSetSetting read FSet write SetSet;
  end;

  TDirectStringSetting           = class (TSetting)
  strict private
     FDefault: string;
     FDest   : ^string;
  strict protected
    function GetSize: LongWord; override;
  public
    constructor Create(AName: string; var ADest: string);
    procedure SetToDefault; override;
    procedure LoadFromStream(Stream: TStream; ASize: Cardinal); override;
    procedure SaveToStream(Stream: TStream; ASize: Cardinal); override;
  end;

  TGetString                     = function : string of object;
  TSetString                     = procedure (const Value: string) of object;

  TIndirectStringSetting         = class (TSetting)
  strict private
    FDefault: string;
    FAStr   : string;
    FGet    : TGetString;
    FSet    : TSetString;
    procedure SetGet(Value: TGetString);
    procedure SetSet(Value: TSetString);
  strict protected
    function GetSize: LongWord; override;
  public
    constructor Create(AName: string; AGet: TGetString; ASet: TSetString);
    constructor Create(AName: string; AGet: TGetString; ASet: TSetString; const ADefault: string);
    procedure SetToDefault; override;
    procedure LoadFromStream(Stream: TStream; ASize: Cardinal); override;
    procedure SaveToStream(Stream: TStream; ASize: Cardinal); override;

    property GetSetting: TGetString read FGet write SetGet;
    property SetSetting: TSetString read FSet write SetSet;
  end;

  TSettings               = array of TSetting;

  TSettingsManager        = class
  strict private
    FSettings      : TSettings;
    FInvalidSetting: TSetting;
    function GetItem(const AName: string): TSetting;
  public
    constructor Create;
    destructor Destroy;
    procedure Add(ASetting: TSetting);
    procedure Complete;
    function LoadFromStream(Stream: TStream; ReadHeader: Boolean = true): TLoadFileResult;
    procedure SaveToStream(Stream: TStream);
    function LoadFromFile(AFileName: string): TLoadFileResult;
    procedure SaveToFile(AFileName: string);

    property InvalidSetting: TSetting read FInvalidSetting;
    property Items[AName: string]: TSetting read GetItem; default;
  end;

  TDirectIntSetting       = specialize TDirectSetting<LongInt>;
  TDirectUIntSetting      = specialize TDirectSetting<LongWord>;
  TIndirectIntSetting     = specialize TIndirectSetting<LongInt>;
  TIndirectUIntSetting    = specialize TIndirectSetting<LongWord>;

const
  SettingsOFileID = ' Settings ';
  SettingsFileID  = MuviFileID+SettingsOFileID;

operator < (m1,m2: TSetting) r: Boolean;

implementation

{TSetting}

constructor TSetting.Create(AName: string);
begin
  inherited Create;
  FName:=AName;
end;

function TSetting.GetSize: Cardinal;
begin
  Result:=0;
end;

procedure TSetting.SetToDefault;
begin
  //do nothing
end;

procedure TSetting.LoadFromStream(Stream: TStream; ASize: Cardinal);
begin
  Stream.Seek(ASize,soFromCurrent);
end;

procedure TSetting.SaveToStream(Stream: TStream; ASize: Cardinal);
begin
  //do nothing
end;

{TDirectSetting}

constructor TDirectSetting.Create(AName: string; var ADest: GType);
begin
  inherited Create(AName);
  FDefault:=ADest;
  FDest:=@ADest;
end;

function TDirectSetting.GetSize: LongWord;
begin
  Result:=SizeOf(GType);
end;

procedure TDirectSetting.SetToDefault;
begin
  FDest^:=FDefault;
end;

procedure TDirectSetting.LoadFromStream(Stream: TStream; ASize: Cardinal);
begin
  Stream.Read(FDest^,ASize);
end;

procedure TDirectSetting.SaveToStream(Stream: TStream; ASize: Cardinal);
begin
  Stream.Write(FDest^,ASize);
end;

{TIndirectSetting}

constructor TIndirectSetting.Create(AName: string; AGet: TGetSetting; ASet: TSetSetting);
begin
  inherited Create(AName);
  FGet:=AGet;
  FSet:=ASet;
  FDefault:=AGet();
end;

constructor TIndirectSetting.Create(AName: string; AGet: TGetSetting; ASet: TSetSetting; ADefault: GType);
begin
  inherited Create(AName);
  FGet:=AGet;
  FSet:=ASet;
  FDefault:=ADefault;
end;

procedure TIndirectSetting.SetGet(Value: TGetSetting);
begin
  if Value<>nil
    then FGet:=Value;
end;

procedure TIndirectSetting.SetSet(Value: TSetSetting);
begin
  if Value<>nil
    then FSet:=Value;
end;

function TIndirectSetting.GetSize: LongWord;
begin
  Result:=SizeOf(GType);
end;

procedure TIndirectSetting.SetToDefault;
begin
  FSet(FDefault);
end;

procedure TIndirectSetting.LoadFromStream(Stream: TStream; ASize: Cardinal);
var
  AVal: GType;
begin
  Stream.Read(AVal,ASize);
  FSet(AVal);
end;

procedure TIndirectSetting.SaveToStream(Stream: TStream; ASize: Cardinal);
var
  AVal: GType;
begin
  AVal:=FGet();
  Stream.Read(AVal,ASize);
end;

{TDirectStringSetting}

constructor TDirectStringSetting.Create(AName: string; var ADest: string);
begin
  inherited Create(AName);
  FDest:=@ADest;
  FDefault:=ADest;
end;

function TDirectStringSetting.GetSize: LongWord;
begin
  Result:=Length(FDest^);
end;

procedure TDirectStringSetting.SetToDefault;
begin
  FDest^:=FDefault;
end;

procedure TDirectStringSetting.LoadFromStream(Stream: TStream; ASize: Cardinal);
var
  I: Integer;
  C: Char;
begin
  FDest^:='';
  for I:=1 to ASize do begin
    Stream.Read(C,1);
    FDest^+=C;
  end;
end;

procedure TDirectStringSetting.SaveToStream(Stream: TStream; ASize: Cardinal);
var
  I: Integer;
begin
  for I:=1 to Length(FDest^)
    do Stream.Write(FDest^[I],1);
end;

{TIndirectStringSetting}

constructor TIndirectStringSetting.Create(AName: string; AGet: TGetString; ASet: TSetString);
begin
  inherited Create(AName);
  FGet:=AGet;
  FSet:=ASet;
  FDefault:=AGet();
end;

constructor TIndirectStringSetting.Create(AName: string; AGet: TGetString; ASet: TSetString; const ADefault: string);
begin
  inherited Create(AName);
  FGet:=AGet;
  FSet:=ASet;
  FDefault:=ADefault;
end;

procedure TIndirectStringSetting.SetGet(Value: TGetString);
begin
  if Value<>nil
    then FGet:=Value;
end;

procedure TIndirectStringSetting.SetSet(Value: TSetString);
begin
  if Value<>nil
    then FSet:=Value;
end;

function TIndirectStringSetting.GetSize: LongWord;
begin
  FAStr:=FGet();
  Result:=Length(FAStr);
end;

procedure TIndirectStringSetting.SetToDefault;
begin
  FSet(FDefault);
end;

procedure TIndirectStringSetting.LoadFromStream(Stream: TStream; ASize: Cardinal);
var
  I   : Integer;
  C   : Char;
  AStr: string;
begin
  AStr:='';
  for I:=1 to ASize do begin
    Stream.Read(C,1);
    AStr+=C;
  end;
  FSet(AStr);
end;

procedure TIndirectStringSetting.SaveToStream(Stream: TStream; ASize: Cardinal);
var
  I: Integer;
begin
  for I:=1 to ASize
    do Stream.Write(FAStr[I],1);
  FAStr:='';
end;

{TSettingsManager}

constructor TSettingsManager.Create;
begin
  inherited Create;
  FInvalidSetting:=TSetting.Create('');
end;

destructor TSettingsManager.Destroy;
var
  I: Integer;
begin
  for I:=0 to Length(FSettings)-1
    do FSettings[I].Destroy;
  Setlength(FSettings,0);
  FInvalidSetting.Destroy;
  inherited Destroy;
end;

procedure TSettingsManager.Add(ASetting: TSetting);
var
  L: Integer;
begin
  L:=Length(FSettings);
  SetLength(FSettings,L+1);
  FSettings[L]:=ASetting;
end;

{$I advfunc.inc}

type
  TSettingsFunc = specialize TOperatorFunc<TSettings,TSetting>;

procedure TSettingsManager.Complete;
begin
  TSettingsFunc.Quicksort(FSettings,Length(FSettings)-1);
end;

function TSettingsManager.GetItem(const AName: string): TSetting;
begin
  FInvalidSetting.FName:=AName;
  Result:=FInvalidSetting;
  TSettingsFunc.BinarySearchAndComplete(Result,FSettings,Length(FSettings)-1);
end;

function TSettingsManager.LoadFromStream(Stream: TStream; ReadHeader: Boolean = true): TLoadFileResult;
var
  I            : Integer;
  ASize        : LongWord;
  ASettingsName: string;
begin
  if ReadHeader then begin
    if ReadFileID(Stream)=SettingsFileID then Result:=lplOK else begin
      Result:=lplInvalidHeader;
      exit;
    end;
  end else Result:=lplOK;

  for I:=0 to Length(FSettings)-1 do begin
    ASettingsName:=LoadText(Stream);
    Stream.Read(ASize,SizeOf(LongWord));
    Items[ASettingsName].LoadFromStream(Stream,ASize);
  end;
end;

procedure TSettingsManager.SaveToStream(Stream: TStream);
var
  I            : Integer;
begin
  Stream.Write(SettingsFileID,GenerelMuviFileIDSize);
  for I:=0 to Length(FSettings)-1 do with FSettings[I] do begin
    SaveText(Name,Stream);
    Stream.Write(Size,SizeOf(LongWord));
    SaveToStream(Stream,Size);
  end;
end;

function TSettingsManager.LoadFromFile(AFileName: string): TLoadFileResult;
var
  FS: TFileStream;
begin
  if not FileExists(AFileName) then begin
    Result:=lplFileNotFound;
    exit;
  end;
  FS:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
  Result:=LoadFromStream(FS);
  FS.Destroy;
end;

procedure TSettingsManager.SaveToFile(AFileName: string);
var
  FS: TFileStream;
begin
  FS:=TFileStream.Create(AFileName,fmCreate or fmShareDenyNone);
  SaveToStream(FS);
  FS.Destroy;
end;

{Allgemein}

operator < (m1,m2: TSetting) r: Boolean;
begin
  Result:=(m1.Name<m2.Name);
end;

end.

