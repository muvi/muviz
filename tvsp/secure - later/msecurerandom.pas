unit MSecureRandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMSRSeed       = array[0..1] of UInt64;

  TMSecureRandom = class
  public
    constructor Create(ASeed: TMSRSeed);
    destructor Destroy; override;
    function Next: Byte; virtual; abstract;
  end;

implementation

end.

