unit TemplateParamConverters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypeImpl, StdParamTypes, VisType2, AdvFunc;

type
  //TODO: remove the wrong class

  TVCallToTemplate        = class (TVTemplateParam)
  private
     FConvertFrom: IPCall;
  public
    constructor Create(AConvertFrom: IPCall);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVIntegerToTemplate     = class (TVTemplateParam)
  private
     FConvertFrom: IPInteger;
  public
    constructor Create(AConvertFrom: IPInteger);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVRealToTemplate        = class (TVTemplateParam)
  private
     FConvertFrom: IPReal;
  public
    constructor Create(AConvertFrom: IPReal);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVShortStringToTemplate = class (TVTemplateParam)
  private
     FConvertFrom: IPShortString;
  public
    constructor Create(AConvertFrom: IPShortString);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVColorToTemplate       = class (TVTemplateParam)
  private
     FConvertFrom: IPColor;
  public
    constructor Create(AConvertFrom: IPColor);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVBooleanToTemplate     = class (TVTemplateParam)
  private
     FConvertFrom: IPBoolean;
  public
    constructor Create(AConvertFrom: IPBoolean);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVBufferToTemplate      = class (TVTemplateParam)
  private
     FConvertFrom: IPBuffer;
  public
    constructor Create(AConvertFrom: IPBuffer);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVStringToTemplate      = class (TVTemplateParam)
  private
     FConvertFrom: IPString;
  public
    constructor Create(AConvertFrom: IPString);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVPresetToTemplate     = class (TVTemplateParam)
  private
     FConvertFrom: IPPreset;
  public
    constructor Create(AConvertFrom: IPPreset);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

  TVPointerToTemplate     = class (TVTemplateParam)
  private
     FConvertFrom: IPPointer;
  public
    constructor Create(AConvertFrom: IPPointer);
    destructor Destroy; override;
    procedure &Set(Value: TVTemplate); cdecl; override;
    function Get: TVTemplate; cdecl; override;
  end;

implementation

{%REGION TVCallToTemplate}

constructor TVCallToTemplate.Create(AConvertFrom: IPCall);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVCallToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVCallToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set;
end;

function TVCallToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=DEFAULTTEMPLATE;
end;

{%ENDREGION}
{%REGION TVIntegerToTemplate}

constructor TVIntegerToTemplate.Create(AConvertFrom: IPInteger);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVIntegerToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVIntegerToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(Value);
end;

function TVIntegerToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=FConvertFrom.Get;
end;

{%ENDREGION}
{%REGION TVRealToTemplate}

constructor TVRealToTemplate.Create(AConvertFrom: IPReal);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVRealToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVRealToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(Value);
end;

function TVRealToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=Round(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVShortStringToTemplate}

constructor TVShortStringToTemplate.Create(AConvertFrom: IPShortString);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVShortStringToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVShortStringToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(IntToStr(Value));
end;

function TVShortStringToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=StrToIntE(FConvertFrom.Get,DEFAULTTEMPLATE);
end;

{%ENDREGION}
{%REGION TVColorToTemplate}

constructor TVColorToTemplate.Create(AConvertFrom: IPColor);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVColorToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVColorToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(Value);
end;

function TVColorToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=FConvertFrom.Get;
end;

{%ENDREGION}
{%REGION TVBooleanToTemplate}

constructor TVBooleanToTemplate.Create(AConvertFrom: IPBoolean);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVBooleanToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBooleanToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(Value>0);
end;

function TVBooleanToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=Ord(FConvertFrom.Get);
end;

{%ENDREGION}
{%REGION TVBufferToTemplate}

constructor TVBufferToTemplate.Create(AConvertFrom: IPBuffer);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVBufferToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVBufferToTemplate.&Set(Value: TVTemplate); cdecl;
var
  ABuffer   : TVBuffer;
  AOldSize,I: Integer;
begin
  ABuffer:=FConvertFrom.Get;
  AOldSize:=ABuffer.Size;
  if AOldSize<>Value then begin
    ABuffer:=ABuffer.Resize(Value);
    for I:=AOldSize to Value-1
      do ABuffer[I]:=0.0;
  end;
  FConvertFrom.&Set(ABuffer);
end;

function TVBufferToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=FConvertFrom.Get.Size;
end;

{%ENDREGION}
{%REGION TVStringToTemplate}

constructor TVStringToTemplate.Create(AConvertFrom: IPString);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVStringToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVStringToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(PChar(IntToStr(Value)));
end;

function TVStringToTemplate.Get: TVTemplate; cdecl;
begin
  Result:=StrToIntE(FConvertFrom.Get,DEFAULTTEMPLATE);
end;

{%ENDREGION}
{%REGION TVPresetToTemplate}

constructor TVPresetToTemplate.Create(AConvertFrom: IPPreset);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVPresetToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPresetToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  FConvertFrom.&Set(DEFAULTPRESET);
end;

function TVPresetToTemplate.Get: TVTemplate; cdecl;
begin
  //NOTE: how to sensefully convert a preset into an int ?!?
  Result:=DEFAULTTEMPLATE;
end;

{%ENDREGION}
{%REGION TVPointerToTemplate}

constructor TVPointerToTemplate.Create(AConvertFrom: IPPointer);
begin
  inherited Create;
  FConvertFrom:=AConvertFrom;
end;

destructor TVPointerToTemplate.Destroy;
begin
  FConvertFrom:=nil;
  inherited Destroy;
end;

procedure TVPointerToTemplate.&Set(Value: TVTemplate); cdecl;
begin
  //TODO: maybe return the parameters order?
  FConvertFrom.&Set(DEFAULTPOINTER);
end;

function TVPointerToTemplate.Get: TVTemplate; cdecl;
begin
  //TODO: maybe return the parameters order?
  Result:=0;
end;

{%ENDREGION}

end.

