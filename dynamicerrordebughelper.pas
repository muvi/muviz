unit DynamicErrorDebugHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LMessages, ExtCtrls;

const
  LM_DynamicError = LM_User+348;

type

  { TErrorDebugForm }

  TErrorDebugForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    ErrorDescLbl: TLabel;
    FixCB: TComboBox;
    IgnoreBtn: TButton;
    FixBtn: TButton;
    FixLbl: TLabel;
    RaiseBtn: TButton;
    procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    {procedure BtnClick(Sender: TObject);
    procedure ErrorTimerTimer(Sender: TObject);}
    //procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    function ShowAndFixBug(ErrorObject: TObject; const ErrorMethod,ErrorDesc: string; const Fixes: array of string): Integer;
  end;

var
  ErrorDebugForm: TErrorDebugForm;
const
  PointerHexDigits = SizeOf(TObject)*2;

function ShowAndFixBug(ErrorObject: TObject; const ErrorMethod,ErrorDesc: string; const Fixes: array of string): Integer;

implementation

uses DynamicBase;

{EDynamicDebugException}

type
  TDDEFixes              = array of string;

  EDynamicDebugException = class (Exception)
  private
    FErrorObject: TObject;
    FErrorMethod: string;
    FFixes      : TDDEFixes;
  public
    constructor Create(AErrorObject: TObject; const AErrorMethod,AErrorDesc: string; const AFixes: array of string); virtual; reintroduce;
    property ErrorObject: TObject read FErrorObject;
    property ErrorMethod: string read FErrorMethod;
    property Fixes: TDDEFixes read FFixes;
  end;

constructor EDynamicDebugException.Create(AErrorObject: TObject; const AErrorMethod,AErrorDesc: string; const AFixes: array of string);
var
  I: Integer;
begin
  inherited Create(AErrorDesc);
  FErrorObject:=AErrorObject;
  FErrorMethod:=AErrorMethod;
  SetLength(FFixes,Length(AFixes));
  for I:=0 to Length(AFixes)-1
    do FFixes[I]:=AFixes[I];
end;

{TErrorDebugForm}

var
  AResult: Integer;

{var
  AErrorObject: TObject;
  AErrorMethod: string;
  AErrorDesc  : string;
  AFixes      : array of string;
  AResult     : Integer = -1;
  AModalResult: TModalResult;}

{procedure TErrorDebugForm.ErrorTimerTimer(Sender: TObject);
begin
  if AResult=-2 then begin
    AResult:=-3;
    AResult:=ShowAndFixBug(AErrorObject,AErrorMethod,AErrorDesc,AFixes);
  end;
end;}

{procedure TErrorDebugForm.BtnClick(Sender: TObject);
begin
  AModalResult:=TButton(Sender).ModalResult;
  Hide;
end;}

procedure TErrorDebugForm.ApplicationPropertiesException(Sender: TObject;
  E: Exception);
begin
  if E.InheritsFrom(EDynamicDebugException)
    then with EDynamicDebugException(E)
      do AResult:=ShowAndFixBug(ErrorObject,ErrorMethod,Message,Fixes);
end;

{procedure TErrorDebugForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AModalResult:=mrCancel;
end; }

function TErrorDebugForm.ShowAndFixBug(ErrorObject: TObject; const ErrorMethod,ErrorDesc: string; const Fixes: array of string): Integer;
var
  I: Integer;
begin
  FixCB.Items.Clear;
  FixLbl.Caption:='Verfügbare Fixes ('+IntToStr(Length(Fixes))+'):';
  if Length(Fixes)>0 then begin
    for I:=0 to Length(Fixes)-1 do FixCB.Items.Add(Fixes[I]);
    FixCB.ItemIndex:=0;
    FixBtn.Enabled:=true;
  end else FixBtn.Enabled:=false;
  ErrorDescLbl.Caption:='Es ist ein Fehler beim Benutzen des Objektes vom Typ "'+ErrorObject.ClassName+'" bei Adressse 0x'+IntToHex(PtrInt(ErrorObject),PointerHexDigits)+' in der Methode "'+ErrorMethod+'"aufgetreten .'+#$D#$A#$D#$A+'Fehlerbeschreibung:'+#$D#$A+ErrorDesc;

  {AModalResult:=mrNone;
  Show;
  while AModalResult=mrNone do Application.ProcessMessages;}

  case {AModalResult} ShowModal of
    mrIgnore        : begin
        Result:=-1;
        exit;
      end;
    mrAbort,mrCancel: begin
        Result:=-1;
        raise EDynamicStructureException.Create(ErrorDesc) at ErrorObject;
      end;
    mrRetry         : begin
        Result:=FixCB.ItemIndex;
        exit;
      end;
  end;
end;

{Allgemein}

function ShowAndFixBug(ErrorObject: TObject; const ErrorMethod,ErrorDesc: string; const Fixes: array of string): Integer;
{var
  I   : Integer;}
begin
  //Result:=ErrorDebugForm.ShowAndFixBug(ErrorObject,ErrorMethod,ErrorDesc,Fixes);
  raise EDynamicDebugException.Create(ErrorObject,ErrorMethod,ErrorDesc,Fixes);
  Result:=AResult;
  {AErrorObject:=ErrorObject;
  AErrorMethod:=ErrorMethod;
  AErrorDesc:=ErrorDesc;
  SetLength(AFixes,Length(Fixes));
  for I:=0 to Length(Fixes)-1 do AFixes[I]:=Fixes[I];
  AResult:=-2;
  while AResult<-1 do (*Sleep(20);  *)Application.ProcessMessages;
  Result:=AResult;  }
end;

initialization
  {$I dynamicerrordebughelper.lrs}

end.

