unit TVSPCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTVSPCommand = class
  public
    procedure Execute; virtual; abstract;
  end;

implementation

end.

