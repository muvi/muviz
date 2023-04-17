unit QueueTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PushableTests, cslPushable, Queue, testregistry;

type
  TQueueTest = class (TPushableTest)
  protected
    function GetTestObject: TPushable; override;
  end;

implementation

{%REGION TQueueTest}

function TQueueTest.GetTestObject: TPushable;
begin
  Result:=TQueue.Create;
end;

{%ENDREGION}

initialization
  RegisterTest(TQueueTest);
end.

