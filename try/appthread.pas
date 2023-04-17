unit AppThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AppFormUnit, Forms;

type
  TApplicationThread = class (TThread)
  private
    FApp    : TApplication;
    FAppForm: TAppForm;
    FOwner  : TComponent;
    procedure FAppException(Sender: TObject; E: Exception);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
  end;

implementation

{%REGION TApplicationThread}

constructor TApplicationThread.Create(AOwner: TComponent);
begin
  FOwner:=AOwner;
  inherited Create(false);
end;

destructor TApplicationThread.Destroy;
begin
  inherited Destroy;
end;

procedure TApplicationThread.FAppException(Sender: TObject; E: Exception);
begin
end;

procedure TApplicationThread.Execute;
begin
  self.Synchronize();
  FApp:=TApplication.Create(FOwner);
  FApp.OnException:=@FAppException;
  FApp.Initialize;
  FApp.CreateForm(TAppForm, FAppForm);
  FAppForm.FApp:=FApp;
  FApp.Run;
  FApp.Terminate;
end;

{%ENDREGION}

end.

