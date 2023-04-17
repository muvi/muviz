unit
    AsioList;

{$MODE Delphi}

interface

uses
    Classes, ActiveX, Windows;

type
    TAsioDriverDesc = packed record
      id   : TCLSID;
      name : array[0..511] of AnsiChar;
      path : array[0..511] of AnsiChar;
    end;
    PAsioDriverDesc = ^TAsioDriverDesc;

    TAsioDriverList = array of TAsioDriverDesc;


procedure ListAsioDrivers(var List: TAsioDriverList);


implementation

uses
    Registry, SysUtils, ComObj;

const
     ASIODRV_DESC  = 'description';
     INPROC_SERVER = 'InprocServer32';
     ASIO_PATH     = 'software\asio';
     COM_CLSID     = 'clsid';

// ******************************************************************
// Local Functions
// ******************************************************************
function findDrvPath(const clsidstr: AnsiString; var dllpath: AnsiString): longint;
var
   reg      : TRegistry;
   success  : boolean;
   buf      : array[0..1024] of AnsiChar;
   s        : AnsiString;
   temps    : AnsiString;
   filename : AnsiString;
   handle   : THandle;
begin
  Result := -1;

  reg := TRegistry.Create;
  try
    dllpath := '';
    reg.RootKey := HKEY_CLASSES_ROOT;
    success := reg.OpenKeyReadOnly(COM_CLSID + '\' + clsidstr + '\' + INPROC_SERVER);
    if success then
    begin
      filename := '';
      dllpath := reg.ReadString('');
      filename := ExtractFilename(dllpath);

      if (ExtractFilePath(dllpath) = '') and (dllpath <> '') then
      begin
        buf[0] := #0;
        temps := dllpath;   // backup the value
        if GetSystemDirectoryA(buf, 1023) <> 0 then   // try the system directory first
        begin
          s := buf;
          dllpath := s + '\' + temps;

          if not FileExists(dllpath) then               // try system32 (hopefully)
          begin
            s := buf + '32';
            dllpath := s + '\' + temps;
          end;
        end;

        if not FileExists(dllpath) then              // try the windows dir if necessary
        begin
          buf[0] := #0;
          if GetWindowsDirectoryA(buf, 1023) <> 0 then   // try the system directory first
          begin
            s := buf;
            dllpath := s + '\' + temps;
          end;
        end;
      end;

      if FileExists(dllpath) then
        Result := 0
      else if (filename <> '') then  // last resort, try to load the dll
      begin
        dllpath := '';
        handle := SafeLoadLibrary(Filename);
        if handle <> 0 then
        begin
          FreeLibrary(handle);
          Result := 0;
        end;
      end;
    end;
  finally
    reg.Free;
  end;
end;

procedure ListAsioDrivers(var List: TAsioDriverList);
var
   r       : TRegistry;
   keys    : TStringList;
   success : boolean;
   i       : integer;
   id      : AnsiString;
   dllpath : AnsiString;
   count   : integer;
   res     : integer;
begin
  SetLength(List, 0);

  keys := TStringList.Create;
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    success := r.OpenKeyReadOnly(ASIO_PATH);
    if success then
    begin
      r.GetKeyNames(keys);
      r.CloseKey;
    end;

    count := 0;
    for i := 0 to keys.Count-1 do
    begin
      success := r.OpenKeyReadOnly(ASIO_PATH + '\' + keys[i]);
      if success then
      begin
        id := r.ReadString(COM_CLSID);
        dllpath := '';
        res := findDrvPath(id, dllpath);
        if res = 0 then  // check if the dll exists
        begin
          SetLength(List, count+1);
          try
            List[count].id := StringToGUID(id);
            StrPLCopy(List[count].name, keys[i], 512);
            StrPLCopy(List[count].path, dllpath, 512);
            inc(count);
          except on EConvertError do
            // invalid device
            //MessageBox(0, PAnsiChar(Format('ERROR device="%s"  GUID="%s"', [keys[i], id])), 'listing ASIO devices', MB_OK);
          end;
        end;
        r.CloseKey;
      end;
    end;

    // in case of exceptions
    SetLength(List, count);
  finally
    keys.Free;
    r.Free;
  end;
end;

end.
