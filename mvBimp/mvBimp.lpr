program mvBimp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, IntfGraphics, SysUtils, Interfaces, Graphics, advguipack, GraphX32,
  ParamType, GraphType, AdvFunc;

type
  TMResConfig = record
    FirstStr  : array [Boolean] of string;
    LastStr   : array [Boolean] of string;
    ConnectStr: array [Boolean,Boolean] of string;
    AliasStr  : array [Char] of string;
  end;

const
  BasePixelPosX = 0;
  BasePixelPosY = 0;

{$R *.res}

procedure GetBaseVals(APic: TLazIntfImage; out ABaseS,ABaseV: Byte);
var
  AAHSV: TAHSV;
  ARGB : PRGB;
begin
  ARGB:=APic.GetDataLineStart(BasePixelPosY);
  Inc(ARGB,BasePixelPosX);
  AAHSV:=ARGBToAHSV(RGBToColor32(ARGB^));
  ABaseS:=AAHSV.S;
  ABaseV:=AAHSV.V;
end;

function AHSVToParamPicPixel(AAHSV: TAHSV; ABaseS,ABaseV: Byte): TParamPicPixel; inline;
begin
  with Result do begin
    S:=IntCut(Integer(AAHSV.S)-Integer(ABaseS),-128,127);
    V:=IntCut(Integer(AAHSV.V)-Integer(ABaseV),-128,127);
  end;
end;

function IntfImageToParamPicStr(APic: TLazIntfImage): TParamPicValueStr;
var
  I,J          : Integer;
  ARGB         : PRGB;
  AParamPic    : TParamPic absolute Result;
  ABaseS,ABaseV: Byte;
begin
  GetBaseVals(APic,ABaseS,ABaseV);
  for J:=0 to ParamPicHeight-1 do begin
    ARGB:=APic.GetDataLineStart(J);
    for I:=0 to ParamPicWidth-1 do begin
      AParamPic[I,J]:=AHSVToParamPicPixel(ARGBToAHSV(RGBToColor32(ARGB^)),ABaseS,ABaseV);
      Inc(ARGB);
    end;
  end;
end;

procedure ReadMResConfig(const AFileName: string; out AConfig: TMResConfig);
var
  F: TextFile;
  C: Char;
begin
  AssignFile(F,AFileName);
  Reset(F);
  with AConfig do begin
    ReadLn(F,FirstStr[false]);
    ReadLn(F,FirstStr[true]);
    ReadLn(F,LastStr[false]);
    ReadLn(F,LastStr[true]);
    ReadLn(F,ConnectStr[false,false]);
    ReadLn(F,ConnectStr[false,true]);
    ReadLn(F,ConnectStr[true,false]);
    ReadLn(F,ConnectStr[true,true]);
    for C:=#0 to #255 do ReadLn(F,AliasStr[C]);
  end;
  CloseFile(F);
end;

function ToMRessourceStr(const AStr: TParamPicValueStr; constref AConfig: TMResConfig): string;
var
  I                    : Integer;
  AAliasStr            : string;
  ALastEscaped,AEscaped: Boolean;
begin
  with AConfig do begin
    AAliasStr:=AliasStr[AStr[0]];
    ALastEscaped:=(AAliasStr='');
    if ALastEscaped then AAliasStr:=IntToStr(Ord(AStr[0]));
    Result:=FirstStr[ALastEscaped]+AAliasStr;
    for I:=1 to ParamPicValueStrLength-1 do begin
      AAliasStr:=AliasStr[AStr[I]];
      AEscaped:=(AAliasStr='');
      if AEscaped then AAliasStr:=IntToStr(Ord(AStr[I]));
      Result+=ConnectStr[ALastEscaped,AEscaped]+AAliasStr;
      ALastEscaped:=AEscaped;
    end;
    Result+=LastStr[ALastEscaped];
  end;
  //Result:='AString:='''+AStr+'''';
end;

procedure ExitStrOut(const AStr: string);
var
  Z: string;
begin
  WriteLn(AStr+#$D#$A+'-- Press any key to continue --');
  ReadLn(Z);
  halt;
end;

var
  IntfImage: TLazIntfImage;
  AConfig  : TMResConfig;
  F        : Text;
begin
  //read input values
  if not (FileExists(ParamStr(1)) and FileExists(ParamStr(2)))
    then ExitStrOut('Usage: mvBimp.exe pascal.mvb SourceBitmap.bmp Destination.txt');
  IntfImage:=TLazIntfImage.Create(0,0,[riqfRGB]);
  try
    IntfImage.LoadFromFile(ParamStr(2));
  except
    IntfImage.Destroy;
    ExitStrOut('Image Format not supported')
  end;
  if (IntfImage.Width<ParamPicWidth) or (IntfImage.Height<ParamPicHeight) then begin
    IntfImage.Destroy;
    ExitStrOut('Bitmap dimensions have to be '+IntToStr(ParamPicWidth)+'x'+IntToStr(ParamPicHeight));
  end;
  if (IntfImage.Width>ParamPicWidth) or (IntfImage.Height>ParamPicHeight)
    then WriteLn('Bitmap is to big. Only the '+IntToStr(ParamPicWidth)+'x'+IntToStr(ParamPicHeight)+' pixels at the top left corner are used');
  //create mresource string
  ReadMResConfig(ParamStr(1),AConfig);
  AssignFile(F,ParamStr(3));
  Rewrite(F);
  Write(F,ToMRessourceStr(IntfImageToParamPicStr(IntfImage),AConfig));
  CloseFile(F);
  //finish
  IntfImage.Destroy;
  WriteLn('MResource String created successfully')
end.

