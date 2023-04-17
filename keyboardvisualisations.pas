unit KeyboardVisualisations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisEventImpl, AdvFunc, StdParamTypes, VisType2, SimpleVis,
  MStrings, PresetType, StdTags, VisAddInput, VisualisationUtils;

type
  TKeyboard  = class (TVisualisationEvents)
  strict private
    FKeys : array [Char] of IPCall;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    procedure KeyPressed(AKey: Char);
  end;

procedure Register;
procedure KeyPressed(AKey: Char);

const
  VIDKEYBOARD  : TGUID = '{844832FA-9C9D-4BB6-B8C5-6452A33615A6}';

  //PIDKEYBOARD  : TGUID = '{94158EB2-86EF-4849-B472-32CBCEE9E1B9}';

implementation

var
  Keyboards   : array of TKeyboard;
  KeyboardLock: TMultiReadExclusiveWriteSynchronizer;

procedure AddKeyboard(AKeyboard: TKeyboard); forward;
procedure RemoveKeyboard(AKeyboard: TKeyboard); forward;

{%REGION TKeyboard}

procedure CreateKeyboard(APrototype: IPVisualisationPrototype); cdecl;
begin
  TKeyboard.Create(APrototype);
end;

constructor TKeyboard.Create(APrototype: IPVisualisationPrototype);
var
  C: Char;
begin
  inherited Create(APrototype);
  for C:=Low(Char) to High(Char)
    do FKeys[C]:=CallInputs[KeyDescriptions[C]];
  AddKeyboard(Self);
end;

destructor TKeyboard.Destroy;
var
  C: Char;
begin
  RemoveKeyboard(Self);
  for C:=Low(Char) to High(Char)
    do FKeys[C]:=nil;
  inherited Destroy;
end;

procedure TKeyboard.KeyPressed(AKey: Char);
begin
  FKeys[AKey].&Set;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
var
  C: Char;
begin
  PresetUtil.RegisterVis(VIDKEYBOARD, @CreateKeyboard);
  with CreatePreset('Keyboard', VIDKEYBOARD) do begin
    AddTag(TAGLISTED);
    AddTag('Trigger.Keyboard');
    AddTag(TAGPREDEFINED);
    for C:='A' to 'Z'
      do AddInput(This, KeyDescriptions[C]);
  end;
end;

procedure KeyPressed(AKey: Char);
var
  I: Integer;
begin
  KeyboardLock.Beginread;

  for I:=0 to Length(Keyboards)-1
    do Keyboards[I].KeyPressed(AKey);

  KeyboardLock.Endread;
end;

procedure AddKeyboard(AKeyboard: TKeyboard);
var
  L: Integer;
begin
  KeyboardLock.Beginwrite;

  L:=Length(Keyboards);
  SetLength(Keyboards, L+1);
  Keyboards[L]:=AKeyboard;

  KeyboardLock.Endwrite;
end;

procedure RemoveKeyboard(AKeyboard: TKeyboard);
var
  I, L: Integer;
begin
  KeyboardLock.Beginwrite;

  L:=Length(Keyboards)-1;
  for I:=0 to L do begin
    if Keyboards[I] = AKeyboard then begin
      Keyboards[I]:=Keyboards[L];
      SetLength(Keyboards, L);

      KeyboardLock.Endwrite;
      exit;
    end;
  end;
  //keyboard MUST exist
  Assert(false);
end;

initialization
  KeyboardLock:=TMultiReadExclusiveWriteSynchronizer.Create;
finalization
  KeyboardLock.Destroy;
{%ENDREGION}

end.

