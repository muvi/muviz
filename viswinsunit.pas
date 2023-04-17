unit VisWinsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisWinType, PluginType, ArrayListedObjects, VisType,
  VisTypeUnit;

type
  TVisWins          = class;
  TVisWin           = class;
  TChangePresetEvent= procedure (Sender: TVisWin) of object;

  TVisWin           = class (TArrayListedObject, IVisWin)
  private
    FEnabled       : Boolean;
    FOnChangePreset: TChangePresetEvent;
  protected
    FCompositions  : TVisCompositions;

    function GetVersion: MVVersion; override; stdcall;
    function Future(const Version: MVVersion): IMInterface; override; stdcall;

    function GetPreset: MVIndex; stdcall; virtual; abstract;
    procedure SetPreset(const APreset: MVIndex); stdcall; virtual; abstract;
    function GetKeyboard: MVIndex; stdcall; virtual; abstract;
    procedure SetKeyboard(const AKeyboard: MVIndex); stdcall; virtual; abstract;
    function GetName: ShortString; stdcall; virtual; abstract;
    procedure SetName(const ACaption: ShortString); stdcall; virtual; abstract;
    function GetResolution: TMuviPoint; stdcall; virtual; abstract;
    procedure SetResolution(const AResolution: TMuviPoint); stdcall; virtual; abstract;
    function GetParamCount: Cardinal; stdcall; virtual; abstract;
    function GetParamType(Index: Cardinal): TVisParamType; stdcall; virtual; abstract;
    function GetParamName(Index: Cardinal): ShortString; stdcall; virtual; abstract;
    procedure SetEnabled(Value: Boolean); virtual;

    procedure KeyDown(const AKey: Char); virtual;
    procedure KeyUp; virtual;
  public
    constructor Create(AEnabled: Boolean = true);
    function ParamPtr(Index: Cardinal): PVCOutput; virtual; abstract;
    procedure SetParam(Index: Cardinal; const Value); stdcall; virtual; abstract;
    procedure GetParam(Index: Cardinal; out Value); stdcall; virtual; abstract;

    property Compositions: TVisCompositions read FCompositions;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: ShortString read GetName write SetName;
    property Preset: MVIndex read GetPreset write SetPreset;
    property Resolution: TMuviPoint read GetResolution write SetResolution;
    property Keyboard: MVIndex read GetKeyboard write SetKeyboard;

    property ParamCount: Cardinal read GetParamCount;
    property ParamNames[Index: Cardinal]: ShortString read GetParamName;
    property ParamTypes[Index: Cardinal]: TVisParamType read GetParamType;

    property OnChangePreset: TChangePresetEvent read FOnChangePreset write FOnChangePreset;
  end;

  TVisWins          = class (TObjectArray)
  private
    function GetVisWin(const AIndex: Integer): TVisWin;
  public
    procedure KeyDown(const AKey: Char);
    procedure KeyUp;

    property VisWins[const AIndex: Integer]: TVisWin read GetVisWin; default;
  end;

implementation

{TVisWins}

function TVisWins.GetVisWin(const AIndex: Integer): TVisWin;
begin
  Result:=TVisWin(Items[AIndex]);
end;

procedure TVisWins.KeyDown(const AKey: Char);
var
  I: Integer;
begin
  for I:=0 to Count-1 do TVisWin(Items[I]).KeyDown(AKey);
end;

procedure TVisWins.KeyUp;
var
  I: Integer;
begin
  for I:=0 to Count-1 do TVisWin(Items[I]).KeyUp;
end;

{TVisWin}

const
  Local_Version: MVVersion = (Version:1;MainVersion:0;SubVersion:0);

constructor TVisWin.Create(AEnabled: Boolean = true);
begin
  inherited Create;
  FEnabled:=AEnabled;
  FCompositions:=nil;
end;

function TVisWin.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TVisWin.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IVisWin(Self))
    else Result:=inherited Future(Version);
end;

procedure TVisWin.SetEnabled(Value: Boolean);
begin
  FEnabled:=Value;
end;

procedure TVisWin.KeyDown(const AKey: Char);
begin
  //do nothing
end;

procedure TVisWin.KeyUp;
begin
  //do nothing
end;

end.

