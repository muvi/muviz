unit PlugImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PhysicsWire, Graphics, Controls, VisType2, AdvFunc,
  ParamType2, csl, HashMap, GraphX32,
  ImageConfig, MiscSlaveControls, ParamTypeKey;

type
  TPlugModifierPixel     = packed record
    //Gray, Alpha, Result Alpha (=Deckkraft des Ergebnisbildes)
    RA,A,G: Byte;
  end;

  TPlugModifier          = object
  strict private
    FModifier: array [0..ParamPicWidth-1,0..ParamPicHeight-1] of TPlugModifierPixel;
  public
    procedure ImportFromBitmap(ABmp: TBitmap);
    procedure ModifyPlugPic(const APic: TPParamPic; AColor: TColor32; Dest: TBitmap);
  end;

  //Usage: PlugStateArray[Plugged].Normal[Highlighted]...
  generic TPlugStateArray<AGType>= array [Boolean] of record
    Normal      : array [Boolean] of AGType;
    ExtHighlight: AGType;
  end;

  TBitmapPlugStateArray          = specialize TPlugStateArray<TBitmap>;
  TMods                          = specialize TPlugStateArray<TPlugModifier>;

  TPlugStateBmps                 = class
  private
    FBmps: TBitmapPlugStateArray;
    function GetNormal(Plugged, Highlighted: Boolean): TBitmap; inline;
    function GetExtHighlighted(Plugged: Boolean): TBitmap; inline;
  public
    constructor Create(AType: IPParamType; constref AMods: TMods);
    destructor Destroy; override;
    property Normal[Plugged, Highlighted: Boolean]: TBitmap read GetNormal;
    //Highlighted because of it's connection to another pin
    property ExtHighlighted[Plugged: Boolean]: TBitmap read GetExtHighlighted;
  end;

  TPlugImages                    = class (TImageConfig)
  strict private
    FImageDestBmp                             : TBitmap;
    FExpanderBtnImages                        : TSwitchButtonImages;
    FDropBtnImages                            : TSlaveButtonImages;
    FRotaryImages                             : TSlaveButtonImages;
    FEditTagsImages                           : TSlaveButtonImages;
    FAddPresetImages                          : TSlaveButtonImages;
    FSavePresetImages                         : TSlaveButtonImages;
    procedure AssignModsTo(var AMods: TMods);
  protected
    //fertige Bilder für die Plugs
    FPlugBmps                                 : TMap;
    //Image Indices
    FUnplugedImageIndex                       : Integer;
    FPlugedImageIndex                         : Integer;
    FHighlightedPlugedImageIndex              : Integer;
    FHighlightedUnplugedImageIndex            : Integer;
    FExtHighlightedPlugedImageIndex           : Integer;
    FExtHighlightedUnplugedImageIndex         : Integer;
    FUnplugedUndefinedImageIndex              : Integer;
    FPlugedUndefinedImageIndex                : Integer;
    FHighlightedPlugedUndefinedImageIndex     : Integer;
    FHighlightedUnplugedUndefinedImageIndex   : Integer;
    FExtHighlightedPlugedUndefinedImageIndex  : Integer;
    FExtHighlightedUnplugedUndefinedImageIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetOutputBmp: TBitmap; override;
    procedure InitPlugBmps;
    procedure FreePlugBmps;
    //loads the undefined pictures, if AType is unknown
    procedure LoadImage(Plugged,Highlighted,ExtHighlight: Boolean; AType: TPParamType); overload;
    procedure LoadImage(Index: Integer); override;

    property AddPresetImages: TSlaveButtonImages read FAddPresetImages;
    property DropBtnImages: TSlaveButtonImages read FDropBtnImages;
    property EditTagsImages: TSlaveButtonImages read FEditTagsImages;
    property ExpanderBtnImages: TSwitchButtonImages read FExpanderBtnImages;
    property ExtHighlightedPlugedImageIndex: Integer read FExtHighlightedPlugedImageIndex write FExtHighlightedPlugedImageIndex default -1;
    property ExtHighlightedPlugedUndefinedImageIndex: Integer read FExtHighlightedPlugedUndefinedImageIndex write FExtHighlightedPlugedUndefinedImageIndex;
    property ExtHighlightedUnplugedImageIndex: Integer read FExtHighlightedUnplugedImageIndex write FExtHighlightedUnplugedImageIndex default -1;
    property ExtHighlightedUnplugedUndefinedImageIndex: Integer read FExtHighlightedUnplugedUndefinedImageIndex write FExtHighlightedUnplugedUndefinedImageIndex;
    property HighlightedPlugedImageIndex: Integer read FHighlightedPlugedImageIndex write FHighlightedPlugedImageIndex default -1;
    property HighlightedPlugedUndefinedImageIndex: Integer read FHighlightedPlugedUndefinedImageIndex write FHighlightedPlugedUndefinedImageIndex;
    property HighlightedUnplugedImageIndex: Integer read FHighlightedUnplugedImageIndex write FHighlightedUnplugedImageIndex default -1;
    property HighlightedUnplugedUndefinedImageIndex: Integer read FHighlightedUnplugedUndefinedImageIndex write FHighlightedUnplugedUndefinedImageIndex;
    property PlugedImageIndex: Integer read FPlugedImageIndex write FPlugedImageIndex default -1;
    property PlugedUndefinedImageIndex: Integer read FPlugedUndefinedImageIndex write FPlugedUndefinedImageIndex default -1;
    property RotaryImages: TSlaveButtonImages read FRotaryImages;
    property SavePresetImages: TSlaveButtonImages read FSavePresetImages;
    property UnplugedImageIndex: Integer read FUnplugedImageIndex write FUnplugedImageIndex default -1;
    property UnplugedUndefinedImageIndex: Integer read FUnplugedUndefinedImageIndex write FUnplugedUndefinedImageIndex default -1;
  end;

const
  VIEWPRESETITEMSPACE            = 6;
  VIEWPRESETINPUTTEXTWIDTH       = 100;
  VIEWPRESETOUTPUTTEXTWIDTH      = 100;
  VIEWPRESETKNOBLEFTRIGHT        = 3;
  VIEWPRESETKNOBTEXTSPACE        = 3;
  VIEWPRESETTEXTEDITSPACE        = 3;

implementation

{%REGION TPlugImages}

constructor TPlugImages.Create;
begin
  inherited Create;
  FPlugBmps:=THashMap.Create;
  FExpanderBtnImages:=TSwitchButtonImages.Create(Self);
  FRotaryImages:=TSlaveButtonImages.Create(Self);
  FEditTagsImages:=TSlaveButtonImages.Create(Self);
  FAddPresetImages:=TSlaveButtonImages.Create(Self);
  FSavePresetImages:=TSlaveButtonImages.Create(Self);
  FDropBtnImages:=TSlaveButtonImages.Create(Self);
  FUnplugedImageIndex:=-1;
  FUnplugedUndefinedImageIndex:=-1;
  FPlugedImageIndex:=-1;
  FPlugedUndefinedImageIndex:=-1;
  FHighlightedUnplugedImageIndex:=-1;
  FHighlightedUnplugedUndefinedImageIndex:=-1;
  FExtHighlightedUnplugedImageIndex:=-1;
  FExtHighlightedUnplugedUndefinedImageIndex:=-1;
  FHighlightedPlugedImageIndex:=-1;
  FHighlightedPlugedUndefinedImageIndex:=-1;
  FExtHighlightedPlugedImageIndex:=-1;
  FExtHighlightedPlugedUndefinedImageIndex:=-1;
  FImageDestBmp:=TempDrawBitmap;
end;

destructor TPlugImages.Destroy;
begin
  FExpanderBtnImages.Destroy;
  FRotaryImages.Destroy;
  FEditTagsImages.Destroy;
  FAddPresetImages.Destroy;
  FSavePresetImages.Destroy;
  FDropBtnImages.Destroy;
  FreePlugBmps;
  FPlugBmps.Destroy;
  inherited Destroy;
end;

function TPlugImages.GetOutputBmp: TBitmap;
begin
  Result:=FImageDestBmp;
end;

procedure TPlugImages.InitPlugBmps;
var
  AMods     : TMods;
  AIterator : IPParamTypeIterator;
begin
  AssignModsTo(AMods);
  AIterator:=ParamTypeUtil.Iterator;
  while (AIterator.MoveNext)
  //for AParamType in FParamTypeUtil   //TODO: do not use the iterator, add a possibility to get all types to the util instead
    do FPlugBmps.Add(TPParamTypeKey.Create(AIterator.GetCurrent.&Type), TPlugStateBmps.Create(AIterator.GetCurrent, AMods));
end;

procedure TPlugImages.FreePlugBmps;
begin
  FPlugBmps.Clean;
end;

procedure TPlugImages.AssignModsTo(var AMods: TMods);
var
  ABmp: TBitmap;
begin
  ABmp:=TBitmap.Create;
  ABmp.PixelFormat:=pf24Bit;
  ABmp.SetSize(ParamPicWidth,ParamPicHeight);
  //Unplugged
  LoadImage(FUnplugedImageIndex);
  ABmp.Canvas.Draw(0,0,TempDrawBitmap);
  AMods[false].Normal[false].ImportFromBitmap(ABmp);
  //Unplugged and Highlighted
  LoadImage(FHighlightedUnplugedImageIndex);
  ABmp.Canvas.Draw(0,0,TempDrawBitmap);
  AMods[false].Normal[true].ImportFromBitmap(ABmp);
  //Highlighted, because the pin the wire is connected to is highlighted
  LoadImage(FExtHighlightedUnplugedImageIndex);
  ABmp.Canvas.Draw(0,0,TempDrawBitmap);
  AMods[false].ExtHighlight.ImportFromBitmap(ABmp);
  //Plugged
  LoadImage(FPlugedImageIndex);
  ABmp.Canvas.Draw(0,0,TempDrawBitmap);
  AMods[true].Normal[false].ImportFromBitmap(ABmp);
  //Plugged and Highlighted
  LoadImage(FHighlightedPlugedImageIndex);
  ABmp.Canvas.Draw(0,0,TempDrawBitmap);
  AMods[true].Normal[true].ImportFromBitmap(ABmp);
  //Plugged and Highlighted, because the pin the wire is connected to is highlighted
  LoadImage(FExtHighlightedPlugedImageIndex);
  ABmp.Canvas.Draw(0,0,TempDrawBitmap);
  AMods[true].ExtHighlight.ImportFromBitmap(ABmp);

  ABmp.Destroy;
end;

procedure TPlugImages.LoadImage(Index: Integer);
begin
  inherited LoadImage(Index);
  FImageDestBmp:=TempDrawBitmap;
end;

procedure TPlugImages.LoadImage(Plugged,Highlighted,ExtHighlight: Boolean; AType: TPParamType);
var
  AStateBmps: TPlugStateBmps;
  AKey      : TObject;
begin
  //get the plug bmps
  AKey:=TPParamTypeKey.Create(AType);
  AStateBmps:=TPlugStateBmps(FPlugBmps[AKey]);
  AKey.Destroy;
  //check if plug type exists
  if AStateBmps<>nil then begin
    if ExtHighlight
      then FImageDestBmp:=AStateBmps.ExtHighlighted[Plugged]
      else FImageDestBmp:=AStateBmps.Normal[Plugged,Highlighted];
  end else begin
    //get default plug bmps
    if Plugged then begin
      if ExtHighlight
        then LoadImage(FExtHighlightedPlugedUndefinedImageIndex)
        else if Highlighted
          then LoadImage(FHighlightedPlugedUndefinedImageIndex)
          else LoadImage(FPlugedUndefinedImageIndex);
    end else begin
      if ExtHighlight
        then LoadImage(FExtHighlightedUnplugedUndefinedImageIndex)
        else if Highlighted
          then LoadImage(FHighlightedUnplugedUndefinedImageIndex)
          else LoadImage(FUnplugedUndefinedImageIndex);
    end;
  end;
end;

{%ENDREGION}
{%REGION TPlugStateBmps}

constructor TPlugStateBmps.Create(AType: IPParamType; constref AMods: TMods);
begin
  inherited Create;
  with AType do begin
    with FBmps[false] do begin
      Normal[false]:=TBitmap.Create;
      Normal[true]:=TBitmap.Create;
      ExtHighlight:=TBitmap.Create;
      AMods[false].Normal[false].ModifyPlugPic(Picture,Color,Normal[false]);
      AMods[false].Normal[true].ModifyPlugPic(Picture,Color,Normal[true]);
      AMods[false].ExtHighlight.ModifyPlugPic(Picture,Color,ExtHighlight);
    end;
    with FBmps[true] do begin
      Normal[false]:=TBitmap.Create;
      Normal[true]:=TBitmap.Create;
      ExtHighlight:=TBitmap.Create;
      AMods[true].Normal[false].ModifyPlugPic(Picture,Color,Normal[false]);
      AMods[true].Normal[true].ModifyPlugPic(Picture,Color,Normal[true]);
      AMods[true].ExtHighlight.ModifyPlugPic(Picture,Color,ExtHighlight);
    end;
  end;
end;

destructor TPlugStateBmps.Destroy;
begin
  with FBmps[false] do begin
    Normal[false].Destroy;
    Normal[true].Destroy;
    ExtHighlight.Destroy;
  end;
  with FBmps[true] do begin
    Normal[false].Destroy;
    Normal[true].Destroy;
    ExtHighlight.Destroy;
  end;
  inherited Destroy;
end;

function TPlugStateBmps.GetNormal(Plugged, Highlighted: Boolean): TBitmap; inline;
begin
  Result:=FBmps[Plugged].Normal[Highlighted];
end;

function TPlugStateBmps.GetExtHighlighted(Plugged: Boolean): TBitmap; inline;
begin
  Result:=FBmps[Plugged].ExtHighlight;
end;

{%ENDREGION}
{%REGION TPlugModifier}

procedure TPlugModifier.ImportFromBitmap(ABmp: TBitmap);
begin
  ABmp.PixelFormat:=pf24Bit;
  ABmp.SetSize(ParamPicWidth,ParamPicHeight);
  ABmp.Canvas.Pixels[1,1]:=clBlack;
  Move(ABmp.RawImage.Data^,FModifier,SizeOf(FModifier));
end;

function GetSDefault(BaseS: Byte; S: ShortInt): Byte;
begin
  Result:=IntCut(Integer(BaseS)+S,0,$FF);
end;

function GetSBlack(BaseS: Byte; S: ShortInt): Byte;
begin
  Result:=0;
end;

procedure TPlugModifier.ModifyPlugPic(const APic: TPParamPic; AColor: TColor32; Dest: TBitmap);
const
  ATransparentColor = $963EE2; //andersrum ?!? (jetzt: BGR)
  ATransparentRGB: TBGR = (B:$96;G:$3E;R:$E2);
var
  I,J        : Integer;
  ARGB       : PBGR;
  AColorHSV  : TAHSV;
  AGetS      : function (BaseS: Byte; S: ShortInt): Byte;
begin
  Dest.SetSize(ParamPicWidth,ParamPicHeight);
  Dest.PixelFormat:=pf24Bit;
  Dest.Transparent:=true;
  Dest.TransparentColor:=ATransparentColor;
  AColorHSV:=ARGBToAHSV(AColor);
  if AColorHSV.S>0
    then AGetS:=@GetSDefault
    else AGetS:=@GetSBlack;
  ARGB:=Pointer(Dest.RawImage.Data);

  for J:=0 to ParamPicHeight-1 do begin
    for I:=0 to ParamPicWidth-1 do begin
      with FModifier[I,J] do if RA>=$7F
        then with APic[I,J]
          do ARGB^:=Color32ToBGR(AlphaBlendGray(A,G,AHSVToARGB(AHSV($FF,AColorHSV.H,AGetS(AColorHSV.S,S),IntCut(Integer(AColorHSV.V)+V,0,$FF)))))
        else ARGB^:=ATransparentRGB;
      Inc(ARGB);
    end;
  end;
end;

{%ENDREGION}

end.

