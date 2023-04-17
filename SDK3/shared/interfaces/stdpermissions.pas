unit StdPermissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTVSPClientPermission = LongInt;

const
  TPNOLIMIT        = High(TTVSPClientPermission);
  TPNOSENDING      = Low(TTVSPClientPermission);

  TPEXECUTE        = 1;
  TPPARAMMAPPING   = TPNOLIMIT; //2;

  TPHARDWAREACCESS = -1;

implementation

end.

