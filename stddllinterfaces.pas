unit StdDllInterfaces; 

{$mode objfpc}{$H+}

interface

uses
  PluginType;

const
  eakTop             = 1;
  eakLeft            = 2;
  eakRight           = 4;
  eakBottom          = 8;

  elbStandard         = 0;
  elbOwnerDrawFixed   = 1;
  elbOwnerDrawVariable= 2;
  elbVirtual          = 3;

  ecsDropDown         = 0;
  ecsSimple           = 1;
  ecsDropDownList     = 2;
  ecsOwnerDrawFixed   = 3;
  ecsOwnerDrawVariable= 4;

  ecbStandardColors   = 1;
  ecbExtendedColors   = 2;
  ecbSystemColors     = 4;
  ecbIncludeNone      = 8;
  ecbIncludeDefault   = 16;
  ecbCustomColor      = 32;
  ecbPrettyNames      = 64;
  ecbCustomColors     = 128;

  eodSelected         = 1;
  eodGrayed           = 2;
  eodDisabled         = 4;
  eodChecked          = 8;
  eodFocused          = 16;
  eodDefault          = 32;
  eodHotLight         = 64;
  eodInactive         = 128;
  eodNoAccel          = 256;
  eodNoFocusRect      = 512;
  eodReserved1        = 1024;
  eodReserved2        = 2048;
  eodComboBoxEdit     = 4096;
  eodPainted          = 8192;

  ieUnchecked         = 0;
  ieChecked           = 1;
  ieGrayed            = 2;

  ectButton           = 0;
  ectEdit             = 1;
  ectListBox          = 2;
  ectSpinEdit         = 3;
  ectFloatSpinEdit    = 4;
  ectCheckbox         = 5;
  ectCombobox         = 6;
  ectColorbox         = 7;

type
  IEInt             = LongInt;
  IEFloat           = Double;
  IEColor           = LongInt;

  TERect            = packed record
    Left,Top,Right,Bottom: IEInt;
  end;

  IEControlType     = type Word;
  IECheckState      = type Byte;
  TEAnchors         = MVSet;
  TEListBoxStyle    = type Byte;
  TEComboBoxStyle   = type Byte;
  TEColorBoxStyle   = MVSet;
  TEOwnerDrawState  = MVSet;
  IEControl         = interface;
  TENotifyEvent     = procedure (Sender: IEControl) of object; stdcall;
  TEDrawItemEvent   = procedure(Control: IEControl; Index: IEInt; ARect: TERect; State: TEOwnerDrawState) of object; stdcall;
  TEMeasureItemEvent= procedure(Control: IEControl; Index: IEInt; var AHeight: IEInt) of object; stdcall;


  IEControl      = interface (IMInterface)
    ['{81093E80-45F5-4763-9452-4B64B4782647}']
    function GetAnchors: TEAnchors; stdcall;
    procedure SetAnchors(const Value: TEAnchors); stdcall;
    function GetCaption: ShortString; stdcall;
    procedure SetCaption(const Value: ShortString); stdcall;
    function GetControlType: IEControlType; stdcall;
    function GetEnabled: Boolean; stdcall;
    procedure SetEnabled(const Value: Boolean); stdcall;
    function GetHeight: IEInt; stdcall;
    procedure SetHeight(const Value: IEInt); stdcall;
    function GetLeft: IEInt; stdcall;
    procedure SetLeft(const Value: IEInt); stdcall;
    function GetTabOrder: IEInt; stdcall;
    procedure SetTabOrder(const Value: IEInt); stdcall;
    function GetTop: IEInt; stdcall;
    procedure SetTop(const Value: IEInt); stdcall;
    function GetVisible: Boolean; stdcall;
    procedure SetVisible(const Value: Boolean); stdcall;
    function GetWidth: IEInt; stdcall;
    procedure SetWidth(const Value: IEInt); stdcall;

    property Anchors: TEAnchors read GetAnchors write SetAnchors;
    property Caption: ShortString read GetCaption write SetCaption;
    property ControlType: IEControlType read GetControlType;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Height: IEInt read GetHeight write SetHeight;
    property Left: IEInt read GetLeft write SetLeft;
    property TabOrder: IEInt read GetTabOrder write SetTabOrder;
    property Top: IEInt read GetTop write SetTop;
    property Visible: Boolean read GetVisible write SetVisible;
    property Width: IEInt read GetWidth write SetWidth;
  end;

  IEGUIArea    = interface (IMInterface)
    ['{3B31D5A2-3EF5-433D-9FDA-358E9BB26824}']
    function GetCaption: ShortString; stdcall;
    procedure SetCaption(const Value: ShortString); stdcall;
    function GetLeft: IEInt; stdcall;
    procedure SetLeft(const Value: IEInt); stdcall;
    function GetHeight: IEInt; stdcall;
    procedure SetHeight(const Value: IEInt); stdcall;
    function GetTop: IEInt; stdcall;
    procedure SetTop(const Value: IEInt); stdcall;
    function GetWidth: IEInt; stdcall;
    procedure SetWidth(const Value: IEInt); stdcall;
    function AddControl(const ControlType: IEControlType): IEControl; stdcall;

    property Caption: ShortString read GetCaption write SetCaption;
    property Left: IEInt read GetLeft write SetLeft;
    property Height: IEInt read GetHEight write SetHeight;
    property Top: IEInt read GetTop write SetTop;
    property Width: IEInt read GetWidth write SetWidth;
  end;

  IEButton     = interface (IEControl)
    ['{D7C87EF5-89AB-46AF-9FF0-8FD309F1F2E6}']
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;

    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
  end;

  IEEdit       = interface (IEControl)
    ['{D9F17B9E-1739-4F0B-9CC1-0E3B4C1C1594}']
    function GetAutoSelect: Boolean; stdcall;
    procedure SetAutoSelect(const Value: Boolean); stdcall;
    function GetMaxLength: IEInt; stdcall;
    procedure SetMaxLength(const Value: IEInt); stdcall;
    function GetPasswordChar: Char; stdcall;
    procedure SetPasswordChar(const Value: Char); stdcall;
    function GetReadOnly: Boolean; stdcall;
    procedure SetReadOnly(const Value: Boolean); stdcall;
    function GetText: ShortString; stdcall;
    procedure SetText(const Value: ShortString); stdcall;
    function GetOnChange: TENotifyEvent; stdcall;
    procedure SetOnChange(const Value: TENotifyEvent); stdcall;
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;
    function GetOnDblClick: TENotifyEvent; stdcall;
    procedure SetOnDblClick(const Value: TENotifyEvent); stdcall;
    function GetOnEnter: TENotifyEvent; stdcall;
    procedure SetOnEnter(const Value: TENotifyEvent); stdcall;
    function GetOnExit: TENotifyEvent; stdcall;
    procedure SetOnExit(const Value: TENotifyEvent); stdcall;
    function GetOnEditingDone: TENotifyEvent; stdcall;
    procedure SetOnEditingDone(const Value: TENotifyEvent); stdcall;

    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property MaxLength: IEInt read GetMaxLength write SetMaxLength;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Text: ShortString read GetText write SetText;
    property OnChange: TENotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TENotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnEnter: TENotifyEvent read GetOnEnter write SetOnEnter;
    property OnExit: TENotifyEvent read GetOnExit write SetOnExit;
    property OnEditingDone: TENotifyEvent read GetOnEditingDone write SetOnEditingDone;
  end;

  IEListBox    = interface (IEControl)
    ['{21269D7B-8963-42AE-8BBD-331D982A33A1}']
    function GetItemHeight: IEInt; stdcall;
    procedure SetItemHeight(const Value: IEInt); stdcall;
    function GetItemIndex: IEInt; stdcall;
    procedure SetItemIndex(const Value: IEInt); stdcall;
    function GetMultiSelect: Boolean; stdcall;
    procedure SetMultiSelect(const Value: Boolean); stdcall;
    function GetSorted: Boolean; stdcall;
    procedure SetSorted(const Value: Boolean); stdcall;
    function GetStyle: TEListBoxStyle; stdcall;
    procedure SetStyle(const Value: TEListBoxStyle); stdcall;

    function GetItem(const Index: IEInt): ShortString; stdcall;
    procedure SetItem(const Index: IEInt; const Value: ShortString); stdcall;
    function Items_Count: IEInt; stdcall;
    procedure Items_Clear; stdcall;
    procedure Items_Add(const Value: ShortString); stdcall;
    procedure Items_Delete(const Index: IEInt); stdcall;
    procedure Items_Insert(const Index: IEInt; const Value: ShortString); stdcall;

    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;
    function GetOnDblClick: TENotifyEvent; stdcall;
    procedure SetOnDblClick(const Value: TENotifyEvent); stdcall;
    function GetOnDrawItem: TEDrawItemEvent; stdcall;
    procedure SetOnDrawItem(const Value: TEDrawItemEvent); stdcall;
    function GetOnEnter: TENotifyEvent; stdcall;
    procedure SetOnEnter(const Value: TENotifyEvent); stdcall;
    function GetOnExit: TENotifyEvent; stdcall;
    procedure SetOnExit(const Value: TENotifyEvent); stdcall;
    function GetOnMeasureItem: TEMeasureItemEvent; stdcall;
    procedure SetOnMeasureItem(const Value: TEMeasureItemEvent); stdcall;

    property ItemHeight: IEInt read GetItemHeight write SetItemHeight;
    property ItemIndex: IEInt read GetItemIndex write SetItemIndex;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Style: TEListBoxStyle read GetStyle write SetStyle;
    property Items[Index: IEInt]: ShortString read GetItem write SetItem;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TENotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnDrawItem: TEDrawItemEvent read GetOnDrawItem write SetOnDrawItem;
    property OnEnter: TENotifyEvent read GetOnEnter write SetOnEnter;
    property OnExit: TENotifyEvent read GetOnExit write SetOnExit;
    property OnMeasureItem: TEMeasureItemEvent read GetOnMeasureItem write SetOnMeasureItem;
  end;

  IESpinEdit   = interface (IEControl)
    ['{13C228A3-2736-4086-9A74-093BFE3576CB}']
    function GetAutoSelect: Boolean; stdcall;
    procedure SetAutoSelect(const Value: Boolean); stdcall;
    function GetMaxValue: IEInt; stdcall;
    procedure SetMaxValue(const Value: IEInt); stdcall;
    function GetMinValue: IEInt; stdcall;
    procedure SetMinValue(const Value: IEInt); stdcall;
    function GetReadOnly: Boolean; stdcall;
    procedure SetReadOnly(const Value: Boolean); stdcall;
    function GetValue: IEInt; stdcall;
    procedure SetValue(const Value: IEInt); stdcall;
    function GetOnChange: TENotifyEvent; stdcall;
    procedure SetOnChange(const Value: TENotifyEvent); stdcall;
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;
    function GetOnEnter: TENotifyEvent; stdcall;
    procedure SetOnEnter(const Value: TENotifyEvent); stdcall;
    function GetOnExit: TENotifyEvent; stdcall;
    procedure SetOnExit(const Value: TENotifyEvent); stdcall;
    function GetOnEditingDone: TENotifyEvent; stdcall;
    procedure SetOnEditingDone(const Value: TENotifyEvent); stdcall;

    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property MaxValue: IEInt read GetMaxValue write SetMaxValue;
    property MinValue: IEInt read GetMinValue write SetMinValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Value: IEInt read GetValue write SetValue;
    property OnChange: TENotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
    property OnEnter: TENotifyEvent read GetOnEnter write SetOnEnter;
    property OnExit: TENotifyEvent read GetOnExit write SetOnExit;
    property OnEditingDone: TENotifyEvent read GetonEditingDone write SetOnEditingDone;
  end;

  IEFloatSpinEdit= interface (IEControl)
    ['{DA06BC21-A9B8-452C-9727-3E2A4D6452D5}']
    function GetAutoSelect: Boolean; stdcall;
    procedure SetAutoSelect(const Value: Boolean); stdcall;
    function GetMaxValue: IEFloat; stdcall;
    procedure SetMaxValue(const Value: IEFloat); stdcall;
    function GetMinValue: IEFloat; stdcall;
    procedure SetMinValue(const Value: IEFloat); stdcall;
    function GetReadOnly: Boolean; stdcall;
    procedure SetReadOnly(const Value: Boolean); stdcall;
    function GetValue: IEFloat; stdcall;
    procedure SetValue(const Value: IEFloat); stdcall;
    function GetOnChange: TENotifyEvent; stdcall;
    procedure SetOnChange(const Value: TENotifyEvent); stdcall;
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;
    function GetOnEnter: TENotifyEvent; stdcall;
    procedure SetOnEnter(const Value: TENotifyEvent); stdcall;
    function GetOnExit: TENotifyEvent; stdcall;
    procedure SetOnExit(const Value: TENotifyEvent); stdcall;
    function GetOnEditingDone: TENotifyEvent; stdcall;
    procedure SetOnEditingDone(const Value: TENotifyEvent); stdcall;

    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property MaxValue: IEFloat read GetMaxValue write SetMaxValue;
    property MinValue: IEFloat read GetMinValue write SetMinValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Value: IEFloat read GetValue write SetValue;
    property OnChange: TENotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
    property OnEnter: TENotifyEvent read GetOnEnter write SetOnEnter;
    property OnExit: TENotifyEvent read GetOnExit write SetOnExit;
    property OnEditingDone: TENotifyEvent read GetonEditingDone write SetOnEditingDone;
  end;

  IECheckbox     = interface (IEControl)
    ['{2847A277-C5CE-47D8-9AE4-C5A53A6ADDC2}']
    function GetAllowGrayed: Boolean; stdcall;
    procedure SetAllowGrayed(const Value: Boolean); stdcall;
    function GetChecked: Boolean; stdcall;
    procedure SetChecked(const Value: Boolean); stdcall;
    function GetState: IECheckState; stdcall;
    procedure SetState(const Value: IECheckState); stdcall;
    function GetOnChange: TENotifyEvent; stdcall;
    procedure SetOnChange(const Value: TENotifyEvent); stdcall;
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;

    property AllowGrayed: Boolean read GetAllowGrayed write SetAllowGrayed;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: IECheckState read GetState write SetState;
    property OnChange: TENotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
  end;

  IEComboBox     = interface (IEControl)
    ['{DB233816-D800-4BB4-9036-7AA5A24BEDD7}']
    function GetAutoComplete: Boolean; stdcall;
    procedure SetAutoComplete(const Value: Boolean); stdcall;
    function GetAutoDropDown: Boolean; stdcall;
    procedure SetAutoDropDown(const Value: Boolean); stdcall;
    function GetAutoSelect: Boolean; stdcall;
    procedure SetAutoSelect(const Value: Boolean); stdcall;
    function GetDropDownCount: IEInt; stdcall;
    procedure SetDropDownCount(const Value: IEInt); stdcall;
    function GetItemHeight: IEInt; stdcall;
    procedure SetItemHeight(const Value: IEInt); stdcall;
    function GetItemIndex: IEInt; stdcall;
    procedure SetItemIndex(const Value: IEInt); stdcall;
    function GetMaxLength: IEInt; stdcall;
    procedure SetMaxLength(const Value: IEInt); stdcall;
    function GetReadOnly: Boolean; stdcall;
    procedure SetReadOnly(const Value: Boolean); stdcall;
    function GetSorted: Boolean; stdcall;
    procedure SetSorted(const Value: Boolean); stdcall;
    function GetStyle: TEComboBoxStyle; stdcall;
    procedure SetStyle(const Value: TEComboBoxStyle); stdcall;

    function GetItem(const Index: IEInt): ShortString; stdcall;
    procedure SetItem(const Index: IEInt; const Value: ShortString); stdcall;
    function Items_Count: IEInt; stdcall;
    procedure Items_Clear; stdcall;
    procedure Items_Add(const Value: ShortString); stdcall;
    procedure Items_Delete(const Index: IEInt); stdcall;
    procedure Items_Insert(const Index: IEInt; const Value: ShortString); stdcall;

    function GetOnChange: TENotifyEvent; stdcall;
    procedure SetOnChange(const Value: TENotifyEvent); stdcall;
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;
    function GetOnDblClick: TENotifyEvent; stdcall;
    procedure SetOnDblClick(const Value: TENotifyEvent); stdcall;
    function GetOnDrawItem: TEDrawItemEvent; stdcall;
    procedure SetOnDrawItem(const Value: TEDrawItemEvent); stdcall;
    function GetOnDropDown: TENotifyEvent; stdcall;
    procedure SetOnDropDown(const Value: TENotifyEvent); stdcall;
    function GetOnCloseUp: TENotifyEvent; stdcall;
    procedure SetOnCloseUp(const Value: TENotifyEvent); stdcall;
    function GetOnEditingDone: TENotifyEvent; stdcall;
    procedure SetOnEditingDone(const Value: TENotifyEvent); stdcall;
    function GetOnEnter: TENotifyEvent; stdcall;
    procedure SetOnEnter(const Value: TENotifyEvent); stdcall;
    function GetOnExit: TENotifyEvent; stdcall;
    procedure SetOnExit(const Value: TENotifyEvent); stdcall;
    function GetOnMeasureItem: TEMeasureItemEvent; stdcall;
    procedure SetOnMeasureItem(const Value: TEMeasureItemEvent); stdcall;

    property AutoComplete: Boolean read GetAutoComplete write SetAutoComplete;
    property AutoDropDown: Boolean read GetAutoDropDown write SetAutoDropDown;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property DropDownCount: IEInt read GetDropDownCount write SetDropDownCount;
    property ItemHeight: IEInt read GetItemHeight write SetItemHeight;
    property ItemIndex: IEInt read GetItemIndex write SetItemIndex;
    property MaxLength: IEInt read GetMaxLength write SetMaxLength;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Style: TEComboBoxStyle read GetStyle write SetStyle;
    property Items[Index: IEInt]: ShortString read GetItem write SetItem;
    property OnChange: TENotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
    property OnCloseUp: TENotifyEvent read GetOnCloseUp write SetOnCloseUp;
    property OnDblClick: TENotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnDrawItem: TEDrawItemEvent read GetOnDrawItem write SetOnDrawItem;
    property OnDropDown: TENotifyEvent read GetOnDropDown write SetOnDropDown;
    property OnEditingDone: TENotifyEvent read GetOnEditingDone write SetOnEditingDone;
    property OnEnter: TENotifyEvent read GetOnEnter write SetOnEnter;
    property OnExit: TENotifyEvent read GetOnExit write SetOnExit;
    property OnMeasureItem: TEMeasureItemEvent read GetOnMeasureItem write SetOnMeasureItem;
  end;

  IEColorBox     = interface (IEControl)
    ['{B937CEE3-C562-40D0-93BC-0F983DB3204E}']
    function GetAutoComplete: Boolean; stdcall;
    procedure SetAutoComplete(const Value: Boolean); stdcall;
    function GetAutoDropDown: Boolean; stdcall;
    procedure SetAutoDropDown(const Value: Boolean); stdcall;
    function GetAutoSelect: Boolean; stdcall;
    procedure SetAutoSelect(const Value: Boolean); stdcall;
    function GetDefaultColor: IEColor; stdcall;
    procedure SetDefaultColor(const Value: IEColor); stdcall;
    function GetDropDownCount: IEInt; stdcall;
    procedure SetDropDownCount(const Value: IEInt); stdcall;
    function GetItemHeight: IEInt; stdcall;
    procedure SetItemHeight(const Value: IEInt); stdcall;
    function GetSelected: IEColor; stdcall;
    procedure SetSelected(const Value: IEColor); stdcall;
    function GetStyle: TEColorBoxStyle; stdcall;
    procedure SetStyle(const Value: TEColorBoxStyle); stdcall;
    function GetNoneColor: IEColor; stdcall;
    procedure SetNoneColor(const Value: IEColor); stdcall;

    function GetItem(const Index: IEInt): ShortString; stdcall;
    function Items_Count: IEInt; stdcall;

    function GetOnChange: TENotifyEvent; stdcall;
    procedure SetOnChange(const Value: TENotifyEvent); stdcall;
    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;
    function GetOnDblClick: TENotifyEvent; stdcall;
    procedure SetOnDblClick(const Value: TENotifyEvent); stdcall;
    function GetOnDropDown: TENotifyEvent; stdcall;
    procedure SetOnDropDown(const Value: TENotifyEvent); stdcall;
    function GetOnCloseUp: TENotifyEvent; stdcall;
    procedure SetOnCloseUp(const Value: TENotifyEvent); stdcall;
    function GetOnEditingDone: TENotifyEvent; stdcall;
    procedure SetOnEditingDone(const Value: TENotifyEvent); stdcall;
    function GetOnEnter: TENotifyEvent; stdcall;
    procedure SetOnEnter(const Value: TENotifyEvent); stdcall;
    function GetOnExit: TENotifyEvent; stdcall;
    procedure SetOnExit(const Value: TENotifyEvent); stdcall;

    property AutoComplete: Boolean read GetAutoComplete write SetAutoComplete;
    property AutoDropDown: Boolean read GetAutoDropDown write SetAutoDropDown;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property DefaultColor: IEColor read GetDefaultColor write SetDefaultColor;
    property DropDownCount: IEInt read GetDropDownCount write SetDropDownCount;
    property ItemHeight: IEInt read GetItemHeight write SetItemHeight;
    property Selected: IEColor read GetSelected write SetSelected;
    property Style: TEColorBoxStyle read GetStyle write SetStyle;
    property NoneColor: IEColor read GetNoneColor write SetNoneColor;
    property Items[Index: IEInt]: ShortString read GetItem;
    property OnChange: TENotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TENotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TENotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnEditingDone: TENotifyEvent read GetOnEditingDone write SetOnEditingDone;
    property OnEnter: TENotifyEvent read GetOnEnter write SetOnEnter;
    property OnExit: TENotifyEvent read GetOnExit write SetOnExit;
  end;

implementation

end.

