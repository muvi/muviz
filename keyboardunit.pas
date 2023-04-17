unit KeyboardUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, DividerBevel, LResources,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, PresetUnit, LCLType,
  Menus, LCLAdvFunc, StyleControls, AdvType, SelectKey, Buttons, ComCtrls,
  VisDrawUnit, VisTypeUnit, ControlTools, AdvFunc;

type

  { TKeyboardForm }

  TKeyboardForm = class(TForm)
    AppendBtn: TBitBtn;
    ActionGB: TGroupBox;
    BackChangeCB: TCheckBox;
    KeyboardCB: TComboBox;
    KeyboardLB: TListBox;
    KeyboardLbl: TLabel;
    KeyboardRB: TRadioButton;
    NothingRB: TRadioButton;
    OpenBtn: TBitBtn;
    OpenDialog: TOpenDialog;
    KeyboardNotebook: TPageControl;
    PresetCB: TComboBox;
    PresetLbl: TLabel;
    PresetRB: TRadioButton;
    PressKeyBtn: TButton;
    RenameKeyboardMI: TMenuItem;
    DeleteKeyboardMI: TMenuItem;
    DuplicateMI: TMenuItem;
    KeyboardPopupMenu: TPopupMenu;
    SaveBtn: TBitBtn;
    SaveDialog: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ActionGBClick(Sender: TObject);
    procedure AppendBtnClick(Sender: TObject);
    procedure BackChangeCBChange(Sender: TObject);
    procedure DeleteKeyboardMIClick(Sender: TObject);
    procedure DuplicateMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure KeyboardCBChange(Sender: TObject);
    procedure KeyboardLBSelectionChange(Sender: TObject; User: boolean);
    procedure KeyboardNotebookPageChanged(Sender: TObject);
    procedure KeyboardRBChange(Sender: TObject);
    procedure KeyboardLBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure NothingRBChange(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure PresetCBEditingDone(Sender: TObject);
    procedure PresetRBChange(Sender: TObject);
    procedure PressKeyBtnClick(Sender: TObject);
    procedure RenameKeyboardMIClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    VisKeyboards : PVisKeyboards;
    VisPresets   : TPresets;
    AKey         : ^TVisKey;
    AKeyboard    : ^TVisKeyboard;
    FEditControls: TParentChangerList;
    procedure SetInternalEnabled(const EnablePreset,EnableKeyboard: Boolean);
    procedure ShowKeyboards;
    procedure ShowKeyboard(const Index: Integer);
    procedure ShowKey(const Key: Char);
    procedure AssignPresets;
    procedure SetCB(CB: TComboBox; const PreString: string; const Value: Integer);
    function GetCB(CB: TComboBox; const PreString: string): Integer;
    procedure SetControlSizes; inline;
  public
    procedure EditKeyboards(var AKeyboards: TVisKeyboards; APresets: TPresets);
  end;

var
  KeyboardForm: TKeyboardForm;

const
  PSPreset    = 'Preset';
  clUnusedText= $BBBBBB;

implementation

{ TKeyboardForm }

procedure TKeyboardForm.SetCB(CB: TComboBox; const PreString: string; const Value: Integer);
begin
  if Value<CB.Items.Count then CB.ItemIndex:=Value else begin
    CB.Text:=PreString+' '+IntToStr(Value+1);
  end;
end;

function TKeyboardForm.GetCB(CB: TComboBox; const PreString: string): Integer;
const
  Ord0 = Ord('0');
  Ord9 = Ord('9');
var
  I      : Integer;
  AOrd   : Byte;
  AChar  : Char;
  ANumber: string;
begin
  Result:=CB.ItemIndex;
  if Result>=0 then exit;
  I:=Length(CB.Text);
  if I=0 then begin
    Result:=0;
    CB.ItemIndex:=0;
    exit;
  end;
  AChar:=CB.Text[I];
  AOrd:=Ord(AChar);
  ANumber:='';
  while (AOrd>=Ord0) and (AOrd<=Ord9) do begin
    ANumber:=AChar+ANumber;
    if I=1 then break;
    Dec(I);
    AChar:=CB.Text[I];
    AOrd:=Ord(AChar);
  end;
  if ANumber='' then begin
    Result:=0;
    CB.ItemIndex:=0;
    exit;
  end;
  TryStrToInt(ANumber,Result);
  Result-=1;
  SetCB(CB,PreString,Result);
end;

procedure TKeyboardForm.ShowKeyboards;
var
  I,APageIndex: Integer;
begin
  KeyboardNotebook.OnChange:=nil;
  APageIndex:=KeyboardNotebook.PageIndex;
  //FEditControls.RemoveParent;
  FEditControls.ChangeParent(KeyboardNotebook.Pages[0]);

  //existierende überschreiben
  for I:=0 to IntMin(KeyboardNotebook.PageCount-1,Length(VisKeyboards^))-1 do begin
    KeyboardNotebook.Pages[I].Caption:=VisKeyboards^[I].Name;
    KeyboardCB.Items[I]:=VisKeyboards^[I].Name;
  end;
  //neue einfügen
  for I:=KeyboardNotebook.PageCount-1 to Length(VisKeyboards^)-1 do begin
    KeyboardNotebook.AddTabSheet;
    //AddTabSheet.Caption:=... funktioniert nicht wg dem '+'
    KeyboardNotebook.Pages[I].Caption:=VisKeyboards^[I].Name;
    KeyboardCB.Items.Add(VisKeyboards^[I].Name);
  end;
  //alte löschen
  for I:=KeyboardNotebook.PageCount-1 downto Length(VisKeyboards^)+1 do begin
    KeyboardNotebook.Pages[I].Destroy;
    KeyboardCB.Items.Delete(I);
  end;
  //'+' hinzufügen
  if KeyboardNotebook.PageCount>Length(VisKeyboards^)
    then KeyboardNotebook.Pages[KeyboardNotebook.PageCount-1].Caption:='+'
    else KeyboardNotebook.AddTabSheet.Caption:='+';
  //-1,-2 wg. +-Seite
  if APageIndex<KeyboardNotebook.PageCount-1
    then KeyboardNotebook.PageIndex:=APageIndex
    else KeyboardNotebook.PageIndex:=KeyboardNotebook.PageCount-2;

  KeyboardNotebook.OnChange:=@KeyboardNotebookPageChanged;
  ShowKeyboard(KeyboardNotebook.PageIndex);
end;

procedure TKeyboardForm.ShowKeyboard(const Index: Integer);
var
  ATabSheet: TTabSheet;
begin
  AKeyboard:=@VisKeyboards^[Index];
  KeyboardLB.Repaint;
  ATabSheet:=KeyboardNotebook.Pages[Index];
  FEditControls.ChangeParent(ATabSheet);
  if Visible then SetControlSizes;
  ShowKey(Chr(KeyboardLB.ItemIndex));
end;

procedure TKeyboardForm.ShowKey(const Key: Char);
begin
  NothingRB.OnChange:=nil;
  PresetRB.OnChange:=nil;
  KeyboardRB.OnChange:=nil;
  PresetCB.OnEditingDone:=nil;
  BackChangeCB.OnChange:=nil;
  KeyboardCB.OnChange:=nil;
  AKey:=@AKeyboard^.Keys[Key];
  with AKey^ do begin
    case KeyType of
      ktNothing : begin
          NothingRB.Checked:=true;
          PresetRB.Checked:=false;
          KeyboardRB.Checked:=false;
          SetInternalEnabled(false,false);
        end;
      ktPreset  : begin
          PresetRB.Checked:=true;
          NothingRB.Checked:=false;
          KeyboardRB.Checked:=false;
          SetInternalEnabled(true,false);
        end;
      ktKeyboard: begin
          KeyboardRB.Checked:=true;
          PresetRB.Checked:=false;
          NothingRB.Checked:=false;
          SetInternalEnabled(false,true);
        end;
    end;
  end;
  NothingRB.OnChange:=@NothingRBChange;
  PresetRB.OnChange:=@PresetRBChange;
  KeyboardRB.OnChange:=@KeyboardRBChange;
  PresetCB.OnEditingDone:=@PresetCBEditingDone;
  BackChangeCB.OnChange:=@BackChangeCbChange;
  KeyboardCB.OnChange:=@KeyboardCBChange;
end;

procedure TKeyboardForm.AssignPresets;
var
  I: Integer;
begin
  PresetCB.Items.CLear;
  for I:=0 to VisPresets.Count-1
    do PresetCB.Items.Add(VisPresets[I].Name);
end;

procedure TKeyboardForm.EditKeyboards(var AKeyboards: TVisKeyboards; APresets: TPresets);
begin
  VisKeyboards:=@AKeyboards;
  VisPresets:=APresets;
  AssignPresets;
  ShowKeyboards;
  ShowModal;
end;

procedure TKeyboardForm.SetInternalEnabled(const EnablePreset,EnableKeyboard: Boolean);
begin
  PresetLbl.Enabled:=EnablePreset;
  PresetCB.Enabled:=EnablePreset;
  BackChangeCB.Enabled:=EnablePreset;
  KeyboardLbl.Enabled:=EnableKeyboard;
  KeyboardCB.Enabled:=EnableKeyboard;
  PresetCB.OnEditingDone:=nil;
  BackChangeCB.OnChange:=nil;
  KeyboardCB.OnChange:=nil;
  if EnablePreset then begin
    SetCB(PresetCB,PSPreset,AKey^.Value);
    BackChangeCB.Checked:=AKey^.BackChange;
  end else begin
    PresetCB.ItemIndex:=-1;
    PresetCB.Text:='';
    BackChangeCB.Checked:=false;
  end;
  if EnableKeyboard
    then KeyboardCB.ItemIndex:=AKey^.Value
    else KeyboardCB.ItemIndex:=-1;
  PresetCB.OnEditingDone:=@PresetCBEditingDone;
  BackChangeCB.OnChange:=@BackChangeCBChange;
  KeyboardCB.OnChange:=@KeyboardCBChange;
end;

procedure TKeyboardForm.NothingRBChange(Sender: TObject);
begin
  if NothingRB.Checked then begin
    SetInternalEnabled(false,false);
    AKey^.KeyType:=ktNothing;
  end;
end;

procedure TKeyboardForm.OpenBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    LoadKeyboardsFromFile(VisKeyboards^,OpenDialog.FileName);
    ShowKeyboards;
  end;
end;

procedure TKeyboardForm.PresetCBEditingDone(Sender: TObject);
begin
  AKey^.Value:=GetCB(TComboBox(Sender),PSPreset);
end;

procedure TKeyboardForm.KeyboardRBChange(Sender: TObject);
begin
  if KeyboardRB.Checked then begin
    SetInternalEnabled(false,true);
    AKey^.KeyType:=ktKeyboard;
  end;
end;

procedure TKeyboardForm.KeyboardLBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with KeyboardLB.Canvas do begin
    if odSelected in State then begin
      Brush.Color:=clHighlight;
      if AKeyboard^.Keys[chr(Index)].KeyType=ktNothing
        then Font.Color:=clUnusedText
        else Font.Color:=clHighlightText;
    end else begin
      Brush.Color:=clWindow;
      if AKeyboard^.Keys[chr(Index)].KeyType=ktNothing
        then Font.Color:=clUnusedText
        else Font.Color:=clWindowText;
    end;
    Pen.Color:=Brush.Color;
    FillRect(ARect);
    TextOut(ARect.Left,ARect.Top,KeyboardLB.Items.Strings[Index])
  end;
end;

procedure TKeyboardForm.KeyboardNotebookPageChanged(Sender: TObject);
var
  KeyboardIndex: Integer;
  AName        : string;
begin
  if KeyboardNotebook.PageIndex=KeyboardNotebook.PageCount-1 then begin
    KeyboardIndex:=Length(VisKeyboards^)+1;
    AName:='Tastaturbelegung '+IntToStr(KeyboardIndex);
    if not InputQuery('Neue Tstaturbelegung','Geben Sie den Namen der neuen Tastaturbelegung ein',AName) then begin
      KeyboardNotebook.PageIndex:=KeyboardNotebook.PageIndex-1;
      exit;
    end;
    SetLength(VisKeyboards^,KeyboardIndex);
    Dec(KeyboardIndex);
    InitKeyboard(VisKeyboards^[KeyboardIndex],AName);
    KeyboardNotebook.Pages[KeyboardIndex].Caption:=AName;
    KeyboardNotebook.AddTabSheet.Caption:='+';
    KeyboardCB.Items.Add(AName);
  end;
  ShowKeyboard(KeyboardNotebook.PageIndex);
end;

procedure TKeyboardForm.KeyboardLBSelectionChange(Sender: TObject; User: boolean);
begin
  ShowKey(Chr(KeyboardLB.ItemIndex));
end;

procedure TKeyboardForm.BackChangeCBChange(Sender: TObject);
begin
  AKey^.BackChange:=BackChangeCB.Checked;
end;

procedure TKeyboardForm.AppendBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    LoadKeyboardsFromFile(VisKeyboards^,OpenDialog.FileName,true);
    ShowKeyboards;
  end;
end;

procedure TKeyboardForm.ActionGBClick(Sender: TObject);
begin

end;

procedure TKeyboardForm.DeleteKeyboardMIClick(Sender: TObject);
var
  APageIndex: Integer;
begin
  if AdvMsg('Möchten Sie die Tastaturbelegung "'+AKeyboard^.Name+'" wirklich dauerhaft löschen?',TdMFrage)=mrYes then begin
    KeyboardNotebook.OnChange:=nil;
    if Length(VisKeyboards^)>1 then begin
      RemoveKeyboard(VisKeyboards^,KeyboardNotebook.PageIndex);
      APageIndex:=KeyboardNotebook.PageIndex;
      if KeyboardNotebook.PageIndex>=KeyboardNotebook.PageCount-2 then begin
        KeyboardNotebook.PageIndex:=KeyboardNotebook.PageCount-3;
        FEditControls.ChangeParent(KeyboardNotebook.Pages[APageIndex-1]);
      end else FEditControls.ChangeParent(KeyboardNotebook.Pages[APageIndex+1]);
      KeyboardCB.Items.Delete(APageIndex);
      KeyboardNotebook.Pages[APageIndex].Destroy;
    end else begin
      InitKeyboard(AKeyboard^,'Tastaturbelegung 1');
      //ClearLayers(VisPresets[PresetNotebook.PageIndex].Layers);
      //VisKeyboard^[KeyboardNotebook.PageIndex].Name:='Tastaturbelegung 1';
      KeyboardNotebook.Pages[KeyboardNotebook.PageIndex].Caption:='Tastaturbelegung 1';
      KeyboardCB.Items.Strings[KeyboardNotebook.PageIndex]:='Tastaturbelegung 1';
    end;
    ShowKeyboard(KeyboardNotebook.PageIndex);
    KeyboardNotebook.OnChange:=@KeyboardNotebookPageChanged;
  end;
end;

procedure TKeyboardForm.DuplicateMIClick(Sender: TObject);
var
  KeyboardIndex,OldKeyboardIndex: Integer;
  AName                         : string;
begin
  OldKeyboardIndex:=KeyboardNotebook.PageIndex;
  AName:=VisKeyboards^[OldKeyboardIndex].Name {'Preset '+IntToStr(PresetIndex+1)};
  if not InputQuery('Neues Preset','Geben Sie den Namen der neuen Tastaturbelegung ein',AName) then begin
    //PresetNotebook.PageIndex:=PresetNotebook.PageIndex-1;
    exit;
  end;
  KeyboardIndex:=Length(VisKeyboards^)+1;
  SetLength(VisKeyboards^,KeyboardIndex);
  Dec(KeyboardIndex);
  Move(VisKeyboards^[OldKeyboardIndex],VisKeyboards^[KeyboardIndex],SizeOf(TVisKeyboard));
  //CopyPreset(VisKeyboards^[OldKeyboardIndex],VisKeyboards^[KeyboardIndex]);
  VisKeyboards^[KeyboardIndex].Name:=AName;
  KeyboardNotebook.Pages[KeyboardIndex].Caption:=AName;
  KeyboardNotebook.AddTabSheet.Caption:='+';
  KeyboardCB.Items.Add(AName);
  KeyboardNotebook.PageIndex:=KeyboardIndex;
  ShowKeyboard(KeyboardIndex);
end;

procedure TKeyboardForm.FormCreate(Sender: TObject);
begin
  FEditControls:=TParentChangerList.Create([PressKeyBtn,KeyboardLB,ActionGB]);
end;

procedure TKeyboardForm.FormDestroy(Sender: TObject);
begin
  FEditControls.Destroy;
end;

procedure TKeyboardForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  I         : Integer;
  ASucceeded: Boolean;
  AErrorStr : string;
begin
  ASucceeded:=true;
  AErrorStr:='';
  for I:=0 to Length(FileNames)-1 do if not lplSucceeded(LoadKeyboardsFromFile(VisKeyboards^,FileNames[I],true)) then begin
    ASucceeded:=false;
    AErrorStr+=FileNames[I]+#$D#$A;
  end;
  if not ASucceeded then AdvMsg('Folgende Dateien konnten nicht geladen werden:'+#$D#$A+AErrorStr,TdMWarnung);
  ShowKeyboards;
end;

procedure TKeyboardForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then case Key of
    46: DeleteKeyboardMI.Click;
    {79: if ssShift in Shift
      then AppendBtn.Click
      else OpenBtn.Click;}
    82: RenameKeyboardMI.Click;
    //83: SaveBtn.Click;
    68: DuplicateMI.Click;
  end;
end;

procedure TKeyboardForm.SetControlSizes; inline;
begin
  KeyboardLB.Height:=KeyboardNotebook.ActivePage.Height-52;
  ActionGB.Height:=KeyboardNotebook.ActivePage.Height-12;
  ActionGB.Width:=KeyboardNotebook.ActivePage.Width-198;
end;

procedure TKeyboardForm.FormShow(Sender: TObject);
begin
  //falls Keyboards schon angezeigt wurden, bevor
  //das Formular sichtbar war, Höhen erst jetzt setzen,
  //sonst werden sie 0
  SetControlSizes;
end;

procedure TKeyboardForm.KeyboardCBChange(Sender: TObject);
begin
  AKey^.Value:=KeyboardCB.ItemIndex;
end;

procedure TKeyboardForm.PresetRBChange(Sender: TObject);
begin
  if PresetRB.Checked then begin
    SetInternalEnabled(true,false);
    AKey^.KeyType:=ktPreset;
  end;
end;

procedure TKeyboardForm.PressKeyBtnClick(Sender: TObject);
var
  ASelKey: Char;
begin
  PressKeyBtn.Enabled:=false;
  ASelKey:=GetKeyboardKey;
  if ASelKey<>#0 then KeyboardLB.ItemIndex:=Ord(ASelKey);
  PressKeyBtn.Enabled:=true;
end;

procedure TKeyboardForm.RenameKeyboardMIClick(Sender: TObject);
var
  AName: ^string;
begin
  AName:=@VisKeyboards^[KeyboardNotebook.PageIndex].Name;
  AName^:=InputBox('Preset Umbenennen','Geben Sie den neuen Namen der Tastaturbelegung ein',AName^);
  KeyboardNotebook.Pages[KeyboardNotebook.PageIndex].Caption:=AName^;
end;

procedure TKeyboardForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute
    then SaveKeyboardsToFile(VisKeyboards^,SaveDialog.FileName);
end;

initialization
  {$I keyboardunit.lrs}

end.

