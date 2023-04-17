unit StdDllType;

interface

{type
  TEInt            = LongInt;
  TEAnchor         = (_akTop,_akLeft,_akRight,_akBottom);
  TEAnchors        = set of TEAnchor;
  TESizeConstraints= record
    MaxHeight,MaxWidth,MinHeight,MinWidth: TEInt;
  end;}

implementation

end.
