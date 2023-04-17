unit MSecureNet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractNet, MSecureRandom;

type
  TMSecureNet = class (TANet)
  strict private
    FParentNet     : TANet;
    //sending
    FSendSequenceNr: UInt64;
    FSendGen       : TMSecureRandom;
    //receiving
    FReceiveGen    : TMSecureRandom;
  protected
    procedure Receive(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
    property ParentNet: TANet read FParentNet;
  public
    constructor Create(AParentNet: TANet; ASendGen, AReceiveGen: TMSecureRandom);
    destructor Destroy; override;
    procedure Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal); override;
  end;

implementation

{%REGION TMSecureNet}

constructor TMSecureNet.Create(AParentNet: TANet; ASendGen, AReceiveGen: TMSecureRandom);
begin
  inherited Create;
  FParentNet:=AParentNet;
  FSendGen:=ASendGen;
  FReceiveGen:=AReceiveGen;
  FSendSequenceNr:=0;
end;

destructor TMSecureNet.Destroy;
begin
  FReceiveGen.Destroy;
  FSendGen.Destroy;
  inherited Destroy;
end;

procedure TMSecureNet.Send(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);
begin

end;

procedure TMSecureNet.Receive(ASocket: TASocket; AMsg: Pointer; ASize: Cardinal);

{%ENDREGION}

end.

