unit TagParamTypeIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, VisType2, ParamType2, MStrings;

const
  vTag           = $0000000000000001 {0 Byte Value, 0 Sources};

type
  IPTag          = IPCall;
  IChangedTag    = IChangedValue;

  IPTagEntry     = interface
    function GetTag: IString; cdecl;
    function IterateValues(var AIterator: Pointer): IPTag; cdecl;

    property Tag: IString read GetTag;
  end;

  IPTagParamType = interface (IPParamType)
    function IterateTags(var AIterator: Pointer): IPTagEntry; cdecl;
    function GetTag(ATag: IString): IPTagEntry; cdecl;
    property Tags[ATag: IString]: IPTagEntry read GetTag; default;
  end;

implementation

end.

