unit MInterfacedSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, MInterfacedObject, SourceType,
  ObjectClassBasic;

type
  TMInterfacedSource = class (TObjectItem)
  private
    FRefCount: Cardinal;
    FID      : MVSourceID;
  protected
    function _AddRef: LongInt; stdcall; override;
    function _Release: LongInt; stdcall; override;
    function GetSrcID: MVSourceID; stdcall;
    //sets this objects Source ID. Never call directly.
    procedure SetSrcID(AID: MVSourceID); stdcall;
    function GetSrcFormat: MVSourceStr; stdcall; virtual; abstract;
    function GetSrcType: MVSourceStr; stdcall; virtual; abstract;
    //used for internal purpose only
    function IDP: Pointer; stdcall; virtual; abstract;

    property RefCount: Cardinal read FRefCount;
  public
    constructor Create;

    property SrcID: MVSourceID read GetSrcID;
    property SrcFormat: MVSourceStr read GetSrcFormat;
    property SrcType: MVSourceStr read GetSrcType;
  end;

implementation

{TMInterfacedSource}

constructor TMInterfacedSource.Create;
begin
  inherited Create;
  FID:=-1;
  FRefCount:=0;
end;

function TMInterfacedSource._AddRef: LongInt; stdcall;
begin
  Inc(FRefCount);
  Result:=FRefCount;
end;

function TMInterfacedSource._Release: LongInt; stdcall;
begin
  Dec(FRefCount);
  Result:=FRefCount;
  if FRefCount<=0 then Destroy;
end;

function TMInterfacedSource.GetSrcID: MVSourceID; stdcall;
begin
  Result:=FID;
end;

procedure TMInterfacedSource.SetSrcID(AID: MVSourceID); stdcall;
begin
  if FID<0 then FID:=AID;
end;

end.

