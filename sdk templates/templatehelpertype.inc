//TODO: delete this file, TemplateParamType and TemplateParamConverters

type
  TVTemplate = Byte;

  IPTemplate          = interface (IPParam)
    procedure &Set(Value: TVTemplate); cdecl;
    function Get: TVTemplate; cdecl;
  end;

  TVTemplateParam        = class (TInterfacedObject, IPTemplate)
  protected
    function GetType: TPParamType; cdecl; virtual; abstract;
  public
    procedure &Set(Value: TVTemplate); cdecl; virtual; abstract;
    function Get: TVTemplate; cdecl; virtual; abstract;
    property &Type: TPParamType read GetType;
  end;

const
  vTemplate = 789324;
  DEFAULTTEMPLATE = 0;

