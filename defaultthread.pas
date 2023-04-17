unit DefaultThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2;

type                              //TODO: implement IInterface
  TDefaultThread = class (TThread{, IInterface})
  private
    FPrototype: IPThreadPrototype;
    FRefCount : Cardinal;
  protected
    property Prototype: IPThreadPrototype read FPrototype;
    procedure Execute; override;
  public
    constructor Create(APrototype: IPThreadPrototype);
    destructor Destroy;
  end;

implementation

{$REGION TDefaultThread}

//TODO: class completely buggy
constructor TDefaultThread.Create(APrototype: IPThreadPrototype);
begin
  inherited Create(false);
  FRefCount:=0;
  FPrototype:=APrototype;
  //APrototype.AttachInterface(Self);
end;

destructor TDefaultThread.Destroy;
begin
  FPrototype:=nil;
  inherited Destroy;
end;

procedure TDefaultThread.Execute;
begin
  Prototype.Started;
  while not Terminated do begin
    Prototype.Execute;
    //TODO: how to release this thread?
    Prototype.WaitForTask;
  end;
end;

{%ENDREGION}

end.

