unit SelectKey;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, LResources, LCLType, FileUtil, ComCtrls, Buttons, LCLProc, Windows;

type
  TKeyboardKeys  = array of Char;

  { TKeySelectForm }

  TKeySelectForm = class(TForm)
    KeyLB: TListBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    KeyLbl: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure KeyLBDblClick(Sender: TObject);
    procedure KeyLBSelectionChange(Sender: TObject; User: boolean);
  public
    SelIndex: ^Integer;
  end;

const
  KeyDescriptions: array [Char] of string=
    ('#0',
    'Linke Maustaste',
    'Rechte Maustaste',
    'Abbrechen',
    'Mittlere Maustaste',
    'X1 Maustaste',
    'X2 Maustaste',
    '#7',
    'Rück',
    'Tab',
    'Linefeed',
    '#11',
    'Löschen',
    'Eingabe',
    '#14',
    '#15',
    'Umschalt',
    'Strg',
    'Alt',
    'Pause',
    'Feststell',
    'IME Kana/Hangul',
    '#16',
    'IME Junja',
    'IME Final',
    'IME Hanja/Kanji',
    '#26',
    'Esc',
    'Konvertieren',
    'Nicht Konvertieren',
    'Akzeptieren',
    'IME Modus wechseln',
    'Leertaste',
    'Bild Auf',
    'Bild Ab',
    'Ende',
    'Pos1',
    'Links',
    'Nach Oben',
    'Rechts',
    'Nach Unten',
    'Auswählen',
    'Drucken',
    'Ausführen',
    'Druck',
    'Einfg',
    'Entf',
    'Hilfe',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '#58',
    '#59',
    '#60',
    '#61',
    '#62',
    '#63',
    '#64',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    'Windowstaste Links',
    'Windowstaste Rechts',
    'Popupmenütaste',
    '#94',
    'Standby',
    '0 (Zehnertastatur)',
    '1 (Zehnertastatur)',
    '2 (Zehnertastatur)',
    '3 (Zehnertastatur)',
    '4 (Zehnertastatur)',
    '5 (Zehnertastatur)',
    '6 (Zehnertastatur)',
    '7 (Zehnertastatur)',
    '8 (Zehnertastatur)',
    '9 (Zehnertastatur)',
    '* (Zehnertastatur)',
    '+ (Zehnertastatur)',
    'Trennen',
    '- (Zehnertastatur)',
    ', (Zehnertastatur)',
    '/ (Zehnertastatur)',
    'F1',
    'F2',
    'F3',
    'F4',
    'F5',
    'F6',
    'F7',
    'F8',
    'F9',
    'F10',
    'F11',
    'F12',
    'F13',
    'F14',
    'F15',
    'F16',
    'F17',
    'F18',
    'F19',
    'F20',
    'F21',
    'F22',
    'F23',
    'F24',
    '#136',
    '#137',
    '#138',
    '#139',
    '#140',
    '#141',
    '#142',
    '#143',
    'Num',
    'Rollen',
    '#146',
    '#147',
    '#148',
    '#149',
    '#150',
    '#151',
    '#152',
    '#153',
    '#154',
    '#155',
    '#156',
    '#157',
    '#158',
    '#159',
    'Umschalt Links',
    'Umschalt Rechts',
    'Strg Links',
    'Strg Rechts',
    'Alt Links',
    'Alt Rechts',
    'Zurück',
    'Vorwärts',
    'Aktualisieren',
    'Anhalten',
    'Suchen',
    'Favoriten',
    'Standardseite',
    'Ton Aus',
    'Leiser',
    'Lauter',
    'Vorspulen',
    'Zurückspulen',
    'Stop',
    'Wiedergabe',
    'E-Mail',
    'Stück Auswählen',
    'Anwendung 1',
    'Anwendung 2',
    '#184',
    '#185',
    'Ü',
    '+',
    ',',
    '-',
    '.',
    '#',
    'Ö',
    '#193',
    '#194',
    '#195',
    '#196',
    '#197',
    '#198',
    '#199',
    '#200',
    '#201',
    '#202',
    '#203',
    '#204',
    '#205',
    '#206',
    '#207',
    '#208',
    '#209',
    '#210',
    '#211',
    '#212',
    '#213',
    '#214',
    '#215',
    '#216',
    '#217',
    '#218',
    'ß',
    '^',
    '´',
    'Ä',
    '#223',
    '#224',
    '#225',
    '<',
    '#227',
    '#228',
    'Prozess',
    '#230',
    'Keine Tastatureingabe',
    '#232',
    '#233',
    '#234',
    '#235',
    '#236',
    '#237',
    '#238',
    '#239',
    '#240',
    '#241',
    '#242',
    '#243',
    '#244',
    '#245',
    'Attn',
    'CrSel',
    'ExSel',
    'Lösche EOF',
    'Abspielen',
    'Zoom',
    'Unbenannt',
    'PA1',
    'Löschen',
    'Ungültige Eingabe');

procedure DoAttachThreadInput(const Attach,AttachTo: TThreadID; const DoAttach: Boolean = true);
procedure DoDetachThreadInput(const Detach,DetachFrom: TThreadID);
function GetKeyDescriptor(const Key: Char): string;
function GetKeyboardKey: Char;
function GetKeyboardKeys: TKeyboardKeys;
function GetKeyboardKeysNow: TKeyboardKeys;
procedure GetKeyboardKeysNow(var Keys: TKeyboardKeys);

implementation


{TKeySelectForm}

procedure DoAttachThreadInput(const Attach,AttachTo: TThreadID; const DoAttach: Boolean = true);
begin
  AttachThreadInput(Attach,AttachTo,DoAttach);
end;

procedure DoDetachThreadInput(const Detach,DetachFrom: TThreadID);
begin
  AttachThreadInput(Detach,DetachFrom,false);
end;

function GetKeyboardKeys: TKeyboardKeys;
var
  TempKeyState: TKeyboardState;
  NKey        : Byte absolute Result;
  I,L         : Byte;
begin
  GetKeyboardState(TempKeyState);
  SetLength(Result,1);
  NKey:=0;
  while (TempKeyState[NKey] shr 7) and 1=0 do begin
    Inc(NKey);
    if NKey=0 then begin
      Application.ProcessMessages;
      GetKeyboardState(TempKeyState);
    end;
  end;
  L:=1;
  if NKey<255
    then for I:=NKey+1 to 255
      do if Boolean((TempKeyState[I] shr 7) and 1)
        then begin
          SetLength(Result,L+1);
          Result[L]:=Chr(I);
          Inc(L);
        end;
end;

procedure GetKeyboardKeysNow(var Keys: TKeyboardKeys);
var
  TempKeyState: TKeyboardState;
  I,L         : Integer;
  //AKey        : Char absolute I;
begin
  SetLength(Keys,0);
  GetKeyboardState(TempKeyState);
  L:=0;
  for I:=0 to 255 do begin
    if Boolean((TempKeyState[I] shr 7) and 1=1) then begin
      SetLength(Keys,L+1);
      Keys[L]:=Chr(I);
      Inc(L);
      //ShowMessage('');
    end;

  end;
end;

function GetKeyboardKeysNow: TKeyboardKeys;
begin
  GetKeyboardKeysNow(Result);
end;

function GetKeyboardKey: Char;
var
  TempKeyState: TKeyboardState;
  NKey        : Byte absolute Result;
  KeySelector : TKeySelectForm;
  I,L         : Byte;
  ResultKeys  : array of Char;
  Selection   : Integer;
begin
  GetKeyboardState(TempKeyState);
  NKey:=0;
  while not Boolean((TempKeyState[NKey] shr 7) and 1) do begin
    Inc(NKey);
    if NKey=0 then begin
      Application.ProcessMessages;
      GetKeyboardState(TempKeyState);
    end;
  end;
  SetLength(ResultKeys,1);
  L:=1;
  ResultKeys[0]:=Result;
  if NKey<255
    then for I:=NKey+1 to 255
      do if Boolean((TempKeyState[I] shr 7) and 1)
        then begin
          SetLength(ResultKeys,L+1);
          ResultKeys[L]:=Chr(I);
          Inc(L);
        end;
  if L>1 then begin
    Application.CreateForm(TKeySelectForm,KeySelector);
    with KeySelector do begin
      for I:=0 to L-1 do KeyLB.Items.Add(KeyDescriptions[ResultKeys[I]]);
      SelIndex:=@Selection;
      if ShowModal=mrOK
        then Result:=ResultKeys[Selection]
        else Result:=#0;
    end;
    SetLength(ResultKeys,0);
  end;
end;

procedure TKeySelectForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TKeySelectForm.KeyLBDblClick(Sender: TObject);
begin
  OKBtn.Click;
end;

procedure TKeySelectForm.KeyLBSelectionChange(Sender: TObject; User: boolean);
begin
  OKBtn.Enabled:=(KeyLB.ItemIndex>=0);
  SelIndex^:=KeyLB.ItemIndex;
end;

{Allgemein}

function GetKeyDescriptor(const Key: Char): string;
begin
  Result:=KeyDescriptions[Key];
end;

initialization
  {$i selectkey.lrs}

end.
