unit Mide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, uPSComponent, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TMideForm }

  TMideForm = class(TForm)
    ImageList: TImageList;
    ErrorLB: TListBox;
    PSScript: TPSScript;
    SynEdit: TSynEdit;
    SynFreePascalSyn: TSynFreePascalSyn;
    ToolBar1: TToolBar;
    CompileButton: TToolButton;
    RunButton: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure RunButtonClick(Sender: TObject);
  private
    //procedure DoStuff;
  end;

var
  MideForm: TMideForm;

implementation

{$R *.lfm}

{ TMideForm }

procedure DoStuff;
begin
  ShowMessage('it works');
end;

procedure TMideForm.CompileButtonClick(Sender: TObject);
var
  I: Integer;
begin
  PSScript.Script.Text:=SynEdit.Text;
  ErrorLB.Items.Clear;
  if PSScript.Compile
    then ErrorLB.Items.Add('compiling successfull');

  for I:=0 to PSScript.CompilerMessageCount-1 do begin
    ErrorLB.Items.Add(PSScript.CompilerMessages[i].MessageToString);
  end;
end;

procedure TMideForm.PSScriptCompile(Sender: TPSScript);
begin
  //PSScript.AddMethod(Self, @TMainForm.DoStuff, 'procedure DoStuff');
  PSScript.AddFunction(@DoStuff, 'procedure DoStuff');
end;

procedure TMideForm.RunButtonClick(Sender: TObject);
begin
  if PSScript.Execute
    then ShowMessage('terminated successfully')
    else ShowMessage('terminated unsuccessfully');
end;

procedure TMideForm.FormCreate(Sender: TObject);
begin

end;

procedure TMideForm.FormShow(Sender: TObject);
begin
end;

end.

