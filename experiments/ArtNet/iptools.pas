unit IPTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Dialogs;

type
  TIP4       = array [0..3] of Byte;

  EInvalidIP = class (Exception)
  end;


function ParseIPv4(AIP: string): TIP4;
function IPToStr(AIP: TIP4): string;
operator = (m1, m2: TIP4): Boolean;

var
  LocalIP: TIP4;

implementation

function GetLocalIP: string;
var
  AProcess      : TProcess;
  APos          : Integer;
  AProcessResult: string;
begin
  try
    AProcess:=TProcess.Create(nil);
    AProcess.CommandLine:='hostname -I';
    AProcess.Options:=[poUsePipes];
    AProcess.Execute;
    if AProcess.WaitOnExit and (AProcess.Output.NumBytesAvailable > 0) then begin
      SetLength(Result, AProcess.Output.NumBytesAvailable);
      AProcess.Output.ReadBuffer(Result[1], AProcess.Output.NumBytesAvailable);
    end;
    //only one ip
    APos:=Pos(' ', Result);
    if APos > 0
      then Delete(Result, APos, Length(Result) - APos + 1);

    AProcess.Free;
  except
    Result:='0.0.0.0';
    AProcess.Free;
  end;
end;

function ParseIPv4(AIP: string): TIP4;
var
  I, AIPPos, AIPVal: Integer;
  AStr             : string;
begin
  AStr:='';
  AIPPos:=0;
  for I:=1 to Length(AIP) do begin
    case AIP[I] of
      '0'..'9': AStr+=AIP[I];
      '.'     : begin
          AIPVal:=StrToInt(AStr);
          if (AIPPos > 2) or (AIPVal > 255)
            then raise EInvalidIP.Create('Invalid IP: ' + AIP);
          Result[AIPPos]:=AIPVal;
          Inc(AIPPos);
          AStr:='';
        end
      else raise EInvalidIP.Create('Invalid IP: ' + AIP);
    end;
  end;
  AIPVal:=StrToInt(AStr);
  if (AIPPos <> 3) or (AIPVal > 255)
    then raise EInvalidIP.Create('Invalid IP: ' + AIP);
  Result[3]:=AIPVal;
end;

operator = (m1, m2: TIP4): Boolean;
begin
  Result:=(m1[0] = m2[0])
      and (m1[1] = m2[1])
      and (m1[2] = m2[2])
      and (m1[3] = m2[3]);
end;

function IPToStr(AIP: TIP4): string;
begin
  Result:=IntToStr(AIP[0]) + '.'
      + IntToStr(AIP[1]) + '.'
      + IntToStr(AIP[2]) + '.'
      + IntToStr(AIP[3]);
end;

initialization
  {$IFDEF WINDOWS}
  LocalIP:=ParseIPv4('0.0.0.0');
  {$ELSE}
  LocalIP:=ParseIPv4(GetLocalIP);
  {$ENDIF}
end.

