unit VPBuffers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType;

type
  vpRealBuffer     = packed record
    P1,P2,P3,P4: Int64;
  end;

  vpBuffer         = vpRealBuffer;
  vpRealBufferItem = MVFloat;
  pvpRealBufferItem= ^vpRealBufferItem;

  IVPBufferManager = interface (IMInterface)
    ['{3151CB00-E605-4168-9060-90E847517253}']
    function NewBuffer(const ASize: LongWord): vpRealBuffer; stdcall;
    procedure DisposeBuffer(var ABuffer: vpRealBuffer); stdcall;
    function ToVPRealBuffer(const ASize: LongWord; const Data: Pointer): vpRealBuffer; stdcall;
    function GetBufferData(const ABuffer: vpRealBuffer): Pointer; stdcall;
    function SizeOfBuffer(const ABuffer: vpRealBuffer): LongWord; stdcall;
    procedure ResizeBuffer(var ABuffer: vpRealBuffer; const NewSize: LongWord); stdcall;
    function GetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord): vpRealBufferItem; stdcall;
    procedure SetBufferItem(const ABuffer: vpRealBuffer; const Index: LongWord; const Value: vpRealBufferItem); stdcall;
    function GetAddressOfItem(const ABuffer: vpRealBuffer; const Index: LongWord): pvpRealBufferItem; stdcall;
    function Cut(const Source: vpRealBuffer; Pos,Count: LongWord): vpRealBuffer; stdcall;
    function Connect(const Sources: array of vpRealBuffer): vpRealBuffer; stdcall;

    property AddressOfItem[const ABuffer: vpRealBuffer; const Index: LongWord]: pvpRealBufferItem read GetAddressOfItem;
    property Data[const ABuffer: vpRealBuffer]: Pointer;
    property Items[const ABuffer: vpRealBuffer; const Index: LongWord]: vpRealBufferItem read GetBufferItem write SetBufferItem; default;
    property Size[const ABuffer: vpRealBuffer]: LongWord read SizeOfBuffer;
  end;

const
  VPEMPTYBUFFER: vpRealBuffer = (P1:0;P2:0;P3:0;P4:0);

implementation

end.

