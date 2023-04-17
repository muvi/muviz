program BralGen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Math;

type

  { TBralGenMain }

  TBralGenMain = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TBralGenMain }

procedure TBralGenMain.DoRun;
var
  ErrorMsg,FileName1,FileName2: String;
  MaxFreq,Gamma,AVal,X        : Real;
  I                           : Integer;
  F1,F2                       : System.Text;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  writeLn('hallo, wie geht es dir?');
  ReadLn(ErrorMsg);
  if Pos('gut',LowerCase(ErrorMsg))>0
    then writeLn('Sehr schoen! Jetzt zum eigentlichen Programm!')
    else writeLn('Mir geht es auch '+ErrorMsg+'.');
  sleep(500);
  WriteLn('Welche Höchstfrequenz soll ich verwenden?');
  ReadLn(ErrorMsg);
  if not TryStrToFloat(ErrorMsg,MaxFreq) then begin
    WriteLn('Du bist auch echt zu blöd n Fließkommawert einzugeben, oder was?');
    sleep(500);
    Terminate;
    exit;
  end;
  WriteLn('Welches gamma findest du gerade besonders sinnvoll?');
  ReadLn(ErrorMsg);
  if not TryStrToFloat(ErrorMsg,Gamma) then begin
    WriteLn('Du bist nicht wirklich zu doof um einen ganz normalen Fließkommawert einzugeben, oder?');
    sleep(500);
    Terminate;
    exit;
  end;

  WriteLn('Zieldateiname und Pfad (ohne Dateiendung bitte!)?');
  ReadLn(FileName1);
  FileName2:=FileName1+'_last.txt';
  FileName1+='_first.txt';
  AssignFile(F1,FileName1);
  AssignFile(F2,FileName2);
  Rewrite(F1);
  Rewrite(F2);

  WriteLn(#$D#$A+'Die Werte sind:'#$D#$A);

  AVal:=0.0;
  ErrorMsg:=FloatToStrF(AVal,ffFixed,7,4);
  WriteLn(ErrorMsg);
  for I:=1 to 32 do begin
    WriteLn(F1,ErrorMsg);
    AVal:=(power((I/32),Gamma))*MaxFreq;
    ErrorMsg:=FloatToStrF(AVal,ffFixed,7,4);
    WriteLn(ErrorMsg);
    WriteLn(F2,ErrorMsg);
  end;

  CloseFile(F1);
  CloseFile(F2);

  WriteLn(#$D#$A+'Findest du meine Ergebnisse gut?');
  ReadLn(ErrorMsg);
  if Pos('n',LowerCase(ErrorMsg))=1 then begin
    WriteLn('Ich bin schwer enttaeuscht :''(');
    sleep(1000);
    WriteLn('Niemand interessiert sich für son einsames Programm wie mich :''(');
    sleep(1000);
    WriteLn('So langsam werde ich agressiv!');
    sleep(1000);
    WriteLn('GRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR');
    sleep(500);
    WriteLn('Du bist auch doof!');
    sleep(500);
    ErrorMsg:='DO';
    for I:=0 to 50 do begin
      ErrorMsg+='O';
      WriteLn(ErrorMsg+'F');
      sleep((50-I)*20);
    end;
  end else WriteLn('Find ich gut! :D');
  Sleep(500);
  // stop program loop
  Terminate;
end;

constructor TBralGenMain.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBralGenMain.Destroy;
begin
  inherited Destroy;
end;

procedure TBralGenMain.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TBralGenMain;

{$IFDEF WINDOWS}{$R BralGen.rc}{$ENDIF}

begin
  Application:=TBralGenMain.Create(nil);
  Application.Title:='BralGen';
  Application.Run;
  Application.Free;
end.

