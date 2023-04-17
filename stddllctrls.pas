unit StdDllCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdDllInterfaces, Controls, StdCtrls, Forms, Spin,
  ColorBox, MInterfacedObject, PluginType, LCLType, ArrayListedObjects,
  StyleControls;

const
  MaxIEControlType = 7;

type
  TegaAdd   = function: IEControl of object;
  TegaRemove= procedure (const AControl: IEControl) of object;
  TEGUIArea = class (TArrayListedObject,IEGUIArea)
  private
    Add_     : array [0..MaxIEControlType] of TegaAdd;
    FRefCount: Integer;
    //Remove_: array [IEControlType] of TegaRemove;
    function Add_ectButton: IEControl;
    function Add_ectEdit: IEControl;
    function Add_ectListBox: IEControl;
    function Add_ectSpinEdit: IEControl;
    function Add_ectFloatSpinEdit: IEControl;
    function Add_ectCheckbox: IEControl;
    function Add_ectComboBox: IEControl;
    function Add_ectColorBox: IEControl;
    {procedure Remove_ectButton(const AControl: IEControl);
    procedure Remove_ectEdit(const AControl: IEControl);
    procedure Remove_ectListBox(const AControl: IEControl);
    procedure Remove_ectSpinEdit(const AControl: IEControl);
    procedure Remove_ectFloatSpinEdit(const AControl: IEControl);
    procedure Remove_ectCheckbox(const AControl: IEControl);
    procedure Remove_ectComboBox(const AControl: IEControl);
    procedure Remove_ectColorBox(const AControl: IEControl);}
  protected
    function _AddRef: LongInt; stdcall; override;
    function _Release: LongInt; stdcall; override;

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
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
    procedure DoFree; override;
  public
    FObj: TStyleBox;
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  end;

  TEControl = class (TMInterfacedObject, IEControl)
  private
    FRefCount: Integer;
    FOwner   : TEGUIArea;
  protected
    function _AddRef: LongInt; stdcall; override;
    function _Release: LongInt; stdcall; override;

    function GetAnchors: TEAnchors; stdcall; virtual; abstract;
    procedure SetAnchors(const Value: TEAnchors); stdcall; virtual; abstract;
    function GetCaption: ShortString; stdcall; virtual; abstract;
    procedure SetCaption(const Value: ShortString); stdcall; virtual; abstract;
    function GetControlType: IEControlType; stdcall; virtual; abstract;
    function GetEnabled: Boolean; stdcall; virtual; abstract;
    procedure SetEnabled(const Value: Boolean); stdcall; virtual; abstract;
    function GetHeight: IEInt; stdcall; virtual; abstract;
    procedure SetHeight(const Value: IEInt); stdcall; virtual; abstract;
    function GetLeft: IEInt; stdcall; virtual; abstract;
    procedure SetLeft(const Value: IEInt); stdcall; virtual; abstract;
    function GetTabOrder: IEInt; stdcall; virtual; abstract;
    procedure SetTabOrder(const Value: IEInt); stdcall; virtual; abstract;
    function GetTop: IEInt; stdcall; virtual; abstract;
    procedure SetTop(const Value: IEInt); stdcall; virtual; abstract;
    function GetVisible: Boolean; stdcall; virtual; abstract;
    procedure SetVisible(const Value: Boolean); stdcall; virtual; abstract;
    function GetWidth: IEInt; stdcall; virtual; abstract;
    procedure SetWidth(const Value: IEInt); stdcall; virtual; abstract;

    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
    function GetObj: TControl; virtual; abstract;

    property Owner: TEGUIArea read FOwner;
  public
    constructor Create(AOwner: TEGUIArea); virtual;
  end;

  TEButton  = class (TEControl,IEButton)
  private
    FOnClick: TENotifyEvent;
    procedure QOnClick(Sender: TObject);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

    function GetOnClick: TENotifyEvent; stdcall;
    procedure SetOnClick(const Value: TENotifyEvent); stdcall;

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  private
    FObj: TButton;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TEEdit    = class (TEControl,IEEdit)
  private
    FOnChange: TENotifyEvent;
    FOnClick: TENotifyEvent;
    FOnDblClick: TENotifyEvent;
    FOnEnter: TENotifyEvent;
    FOnExit: TENotifyEvent;
    FOnEditingDone: TENotifyEvent;
    procedure QOnChange(Sender: TObject);
    procedure QOnClick(Sender: TObject);
    procedure QOnDblClick(Sender: TObject);
    procedure QOnEnter(Sender: TObject);
    procedure QOnExit(Sender: TObject);
    procedure QOnEditingDone(Sender: TObject);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TEdit;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TEListBox = class (TEControl,IEListBox)
  private
    FOnClick: TENotifyEvent;
    FOnDblClick: TENotifyEvent;
    FOnDrawItem: TEDrawItemEvent;
    FOnEnter: TENotifyEvent;
    FOnExit: TENotifyEvent;
    FOnMeasureItem: TEMeasureItemEvent;
    procedure QOnClick(Sender: TObject);
    procedure QOnDblClick(Sender: TObject);
    procedure QOnMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
    procedure QOnEnter(Sender: TObject);
    procedure QOnExit(Sender: TObject);
    procedure QOnDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TListBox;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TESpinEdit    = class (TEControl,IESpinEdit)
  private
    FOnChange: TENotifyEvent;
    FOnClick: TENotifyEvent;
    FOnEnter: TENotifyEvent;
    FOnExit: TENotifyEvent;
    FOnEditingDone: TENotifyEvent;
    procedure QOnChange(Sender: TObject);
    procedure QOnClick(Sender: TObject);
    procedure QOnEnter(Sender: TObject);
    procedure QOnExit(Sender: TObject);
    procedure QOnEditingDone(Sender: TObject);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TSpinEdit;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TEFloatSpinEdit= class (TEControl,IEFloatSpinEdit)
  private
    FOnChange: TENotifyEvent;
    FOnClick: TENotifyEvent;
    FOnEnter: TENotifyEvent;
    FOnExit: TENotifyEvent;
    FOnEditingDone: TENotifyEvent;
    procedure QOnChange(Sender: TObject);
    procedure QOnClick(Sender: TObject);
    procedure QOnEnter(Sender: TObject);
    procedure QOnExit(Sender: TObject);
    procedure QOnEditingDone(Sender: TObject);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TFloatSpinEdit;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TECheckbox     = class (TEControl,IECheckbox)
  private
    FOnChange: TENotifyEvent;
    FOnClick: TENotifyEvent;
    procedure QOnChange(Sender: TObject);
    procedure QOnClick(Sender: TObject);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TCheckbox;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TEComboBox   = class (TEControl,IEComboBox)
  private
    FOnChange     : TENotifyEvent;
    FOnClick      : TENotifyEvent;
    FOnCloseUp    : TENotifyEvent;
    FOnDblClick   : TENotifyEvent;
    FOnDrawItem   : TEDrawItemEvent;
    FOnDropDown   : TENotifyEvent;
    FOnEditingDone: TENotifyEvent;
    FOnEnter      : TENotifyEvent;
    FOnExit       : TENotifyEvent;
    FOnMeasureItem: TEMeasureItemEvent;
    procedure QOnChange(Sender: TObject);
    procedure QOnClick(Sender: TObject);
    procedure QOnCloseUp(Sender: TObject);
    procedure QOnDblClick(Sender: TObject);
    procedure QOnDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure QOnDropDown(Sender: TObject);
    procedure QOnEditingDone(Sender: TObject);
    procedure QOnEnter(Sender: TObject);
    procedure QOnExit(Sender: TObject);
    procedure QOnMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TComboBox;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

  TEColorBox  = class (TEControl,IEColorBox)
  private
    FOnChange     : TENotifyEvent;
    FOnClick      : TENotifyEvent;
    FOnCloseUp    : TENotifyEvent;
    FOnDblClick   : TENotifyEvent;
    FOnDropDown   : TENotifyEvent;
    FOnEditingDone: TENotifyEvent;
    FOnEnter      : TENotifyEvent;
    FOnExit       : TENotifyEvent;
    procedure QOnChange(Sender: TObject);
    procedure QOnClick(Sender: TObject);
    procedure QOnCloseUp(Sender: TObject);
    procedure QOnDblClick(Sender: TObject);
    procedure QOnDropDown(Sender: TObject);
    procedure QOnEditingDone(Sender: TObject);
    procedure QOnEnter(Sender: TObject);
    procedure QOnExit(Sender: TObject);
  protected
    function GetAnchors: TEAnchors; stdcall; override;
    procedure SetAnchors(const Value: TEAnchors); stdcall; override;
    function GetCaption: ShortString; stdcall; override;
    procedure SetCaption(const Value: ShortString); stdcall; override;
    function GetControlType: IEControlType; stdcall; override;
    function GetEnabled: Boolean; stdcall; override;
    procedure SetEnabled(const Value: Boolean); stdcall; override;
    function GetHeight: IEInt; stdcall; override;
    procedure SetHeight(const Value: IEInt); stdcall; override;
    function GetLeft: IEInt; stdcall; override;
    procedure SetLeft(const Value: IEInt); stdcall; override;
    function GetTabOrder: IEInt; stdcall; override;
    procedure SetTabOrder(const Value: IEInt); stdcall; override;
    function GetTop: IEInt; stdcall; override;
    procedure SetTop(const Value: IEInt); stdcall; override;
    function GetVisible: Boolean; stdcall; override;
    procedure SetVisible(const Value: Boolean); stdcall; override;
    function GetWidth: IEInt; stdcall; override;
    procedure SetWidth(const Value: IEInt); stdcall; override;

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

    function GetObj: TControl; override;
    function GetVersion: MVVersion; stdcall; override;
    function Future(const Version: MVVersion): IMInterface; stdcall; override;
  public
    FObj: TColorBox;
    constructor Create(AOwner: TEGUIArea); override;
    destructor Destroy; override;
  end;

{function NewIControl_(const ControlType: IEControlType; const AOwner: TComponent): IEControl; stdcall;
procedure DisposeIControl_(var AControl: IEControl); stdcall;}

function EAnchorsToAnchors(AEAnchors: TEAnchors): TAnchors;
function AnchorsToEAnchors(AAnchors: TAnchors): TEAnchors;
function ListBoxStyleToEListBoxStyle(AListBoxStyle: TListBoxStyle): TEListBoxStyle;
function EListBoxStyleToListBoxStyle(AListBoxStyle: TEListBoxStyle): TListBoxStyle;
function ComboBoxStyleToEComboBoxStyle(AComboBoxStyle: TComboBoxStyle): TEComboBoxStyle;
function EComboBoxStyleToComboBoxStyle(AComboBoxStyle: TEComboBoxStyle): TComboBoxStyle;
function ColorBoxStyleToEColorBoxStyle(AColorBoxStyle: TColorBoxStyle): TEColorBoxStyle;
function EColorBoxStyleToColorBoxStyle(AColorBoxStyle: TEColorBoxStyle): TColorBoxStyle;
function OwnerDrawStateToEOwnerDrawState(AOwnerDrawState: TOwnerDrawState): TEOwnerDrawState;
function EOwnerDrawStateToOwnerDrawState(AOwnerDrawState: TEOwnerDrawState): TOwnerDrawState;
function ERect(const ARect: TRect): TERect;
function CheckboxStateToIECheckState(AState: TCheckboxState): IECheckState;
function IECheckStateToCheckboxState(AState: IECheckState): TCheckboxState;

implementation

{TEGUIArea}

constructor TEGUIArea.Create(AOwner: TComponent);
begin
  inherited Create;
  FRefCount:=0;
  Add_[ectButton]:=@Add_ectButton;
  Add_[ectEdit]:=@Add_ectEdit;
  Add_[ectListBox]:=@Add_ectListBox;
  Add_[ectSpinEdit]:=@Add_ectSpinEdit;
  Add_[ectFloatSpinEdit]:=@Add_ectFloatSpinEdit;
  Add_[ectCheckbox]:=@Add_ectCheckbox;
  Add_[ectComboBox]:=@Add_ectComboBox;
  Add_[ectColorBox]:=@Add_ectColorBox;
  {Remove_[ectButton]:=@Remove_ectButton;
  Remove_[ectEdit]:=@Remove_ectEdit;
  Remove_[ectListBox]:=@Remove_ectListBox;
  Remove_[ectSpinEdit]:=@Remove_ectSpinEdit;
  Remove_[ectFloatSpinEdit]:=@Remove_ectFloatSpinEdit;
  Remove_[ectCheckbox]:=@Remove_ectCheckbox;
  Remove_[ectComboBox]:=@Remove_ectComboBox;
  Remove_[ectColorBox]:=@Remove_ectColorBox;}
  FObj:=TStyleBox.Create(AOwner);
end;

destructor TEGUIArea.Destroy;
begin
  inherited Destroy;
end;

procedure TEGUIArea.DoFree;
begin
  FObj.Destroy;
end;

function TEGUIArea._AddRef: LongInt; stdcall;
begin
  Inc(FRefCount);
  Result:=FRefCount;
end;

function TEGUIArea._Release: LongInt; stdcall;
begin
  Dec(FRefCount);
  Result:=FRefCount;
  if FRefCount<=0 then Destroy;
end;

const
  Local_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TEGUIArea.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Version;
end;

function TEGUIArea.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Version
    then Result:=IMInterface(IEGUIArea(Self))
    else Result:=inherited Future(Version);
end;

{function TEGUIArea.QueryInterface(const iid : tguid; out obj) : longint; stdcall;
begin
  Result:=0;
end;

function TEGUIArea._AddRef : longint;stdcall;
begin
  Result:=1;
end;

function TEGUIArea._Release : longint;stdcall;
begin
  Result:=1;
end;}

function TEGUIArea.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Caption;
end;

procedure TEGUIArea.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Caption:=Value;
end;

function TEGUIArea.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEGUIArea.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEGUIArea.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEGUIArea.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEGUIArea.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEGUIArea.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEGUIArea.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEGUIArea.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEGUIArea.AddControl(const ControlType: IEControlType): IEControl; stdcall;
begin
  if ControlType<=MaxIEControlType
    then Result:=Add_[ControlType]()
    else Result:=nil;
end;

{procedure TEGUIArea.RemoveIControl(var Control: IEControl); stdcall;
begin
  Remove_[Control.ControlType](Control);
  Control:=nil;
end;}

function TEGUIArea.Add_ectButton: IEControl;
var
  ANObj: TEButton;
begin
  ANObj:=TEButton.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IEButton(ANObj));
end;

function TEGUIArea.Add_ectEdit: IEControl;
var
  ANObj: TEEdit;
begin
  ANObj:=TEEdit.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IEEdit(ANObj));
end;

function TEGUIArea.Add_ectListBox: IEControl;
var
  ANObj: TEListBox;
begin
  ANObj:=TEListBox.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IEListBox(ANObj));
end;

function TEGUIArea.Add_ectSpinEdit: IEControl;
var
  ANObj: TESpinEdit;
begin
  ANObj:=TESpinEdit.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IESpinEdit(ANObj));
end;

function TEGUIArea.Add_ectFloatSpinEdit: IEControl;
var
  ANObj: TEFloatSpinEdit;
begin
  ANObj:=TEFloatSpinEdit.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IEFloatSpinEdit(ANObj));
end;

function TEGUIArea.Add_ectCheckbox: IEControl;
var
  ANObj: TECheckbox;
begin
  ANObj:=TECheckbox.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IECheckbox(ANObj));
end;

function TEGUIArea.Add_ectComboBox: IEControl;
var
  ANObj: TEComboBox;
begin
  ANObj:=TEComboBox.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IEComboBox(ANObj));
end;

function TEGUIArea.Add_ectColorBox: IEControl;
var
  ANObj: TEColorBox;
begin
  ANObj:=TEColorBox.Create(Self);
  //FObj.InsertControl(ANObj.FObj);
  Result:=IEControl(IEColorBox(ANObj));
end;

{procedure TEGUIArea.Remove_ectButton(const AControl: IEControl);
var
  ANObj: TButton;
begin
  ANObj:=TButton(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectEdit(const AControl: IEControl);
var
  ANObj: TEdit;
begin
  ANObj:=TEdit(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectListBox(const AControl: IEControl);
var
  ANObj: TListBox;
begin
  ANObj:=TListBox(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectSpinEdit(const AControl: IEControl);
var
  ANObj: TSpinEdit;
begin
  ANObj:=TSpinEdit(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectFloatSpinEdit(const AControl: IEControl);
var
  ANObj: TFloatSpinEdit;
begin
  ANObj:=TFloatSpinEdit(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectCheckbox(const AControl: IEControl);
var
  ANObj: TCheckbox;
begin
  ANObj:=TCheckbox(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectComboBox(const AControl: IEControl);
var
  ANObj: TComboBox;
begin
  ANObj:=TComboBox(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

procedure TEGUIArea.Remove_ectColorBox(const AControl: IEControl);
var
  ANObj: TColorBox;
begin
  ANObj:=TColorBox(AControl.GetObj);
  FObj.RemoveControl(ANObj);
  ANObj.Destroy;
end;

function TEGUIArea.GetObj: Pointer; stdcall;
begin
  Result:=FObj;
end;}

{TEControl}

constructor TEControl.Create(AOwner: TEGUIArea);
begin
  inherited Create;
  FOwner:=AOwner;
  FRefCount:=0;
end;

function TEControl._AddRef: LongInt; stdcall;
begin
  Inc(FRefCount);
  Result:=FRefCount;
end;

function TEControl._Release: LongInt; stdcall;
begin
  Dec(FRefCount);
  Result:=FRefCount;
  if FRefCount<=0 then Destroy;
end;

const
  Local_Control_Version: MVVersion = (Version:0;MainVersion:1;SubVersion:0);

function TEControl.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Control_Version;
end;

function TEControl.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Control_Version
    then Result:=IMInterface(IEControl(Self))
    else Result:=inherited Future(Version);
end;

{TEButton}

constructor TEButton.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TButton.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnClick:=nil;
end;

destructor TEButton.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_Button_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TEButton.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Button_Version;
end;

function TEButton.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Button_Version
    then Result:=IMInterface(IEButton(Self))
    else Result:=inherited Future(Version);
end;

function TEButton.GetObj: TControl;
begin
  Result:=FObj;
end;

function TEButton.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TEButton.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TEButton.GetControlType: IEControlType; stdcall;
begin
  Result:=ectButton;
end;

function TEButton.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TEButton.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TEButton.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEButton.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEButton.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEButton.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEButton.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TEButton.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TEButton.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEButton.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEButton.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TEButton.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TEButton.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEButton.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEButton.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Caption;
end;

procedure TEButton.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Caption:=Value;
end;

function TEButton.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TEButton.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

{function TEButton.GetObj: Pointer; stdcall;
begin
  Result:=FObj;
end;}

{TEButton - Q Methods}

procedure TEButton.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

{TEEdit}

constructor TEEdit.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TEdit.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnChange:=nil;
  FOnClick:=nil;
  FOnDblClick:=nil;
  FOnEnter:=nil;
  FOnExit:=nil;
  FOnEditingDone:=nil;
end;

destructor TEEdit.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_Edit_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TEEdit.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Edit_Version;
end;

function TEEdit.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Edit_Version
    then Result:=IMInterface(IEEdit(Self))
    else Result:=inherited Future(Version);
end;

function TEEdit.GetObj: TControl;
begin
  Result:=FObj;
end;

function TEEdit.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TEEdit.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TEEdit.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Text;
end;

procedure TEEdit.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Text:=Value;
end;

function TEEdit.GetControlType: IEControlType; stdcall;
begin
  Result:=ectEdit;
end;

function TEEdit.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TEEdit.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TEEdit.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEEdit.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEEdit.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEEdit.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEEdit.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TEEdit.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TEEdit.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEEdit.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEEdit.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TEEdit.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TEEdit.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEEdit.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEEdit.GetAutoSelect: Boolean; stdcall;
begin
  Result:=FObj.AutoSelect;
end;

procedure TEEdit.SetAutoSelect(const Value: Boolean); stdcall;
begin
  FObj.AutoSelect:=Value;
end;

function TEEdit.GetMaxLength: IEInt; stdcall;
begin
  Result:=FObj.MaxLength;
end;

procedure TEEdit.SetMaxLength(const Value: IEInt); stdcall;
begin
  FObj.MaxLength:=Value;
end;

function TEEdit.GetPasswordChar: Char; stdcall;
begin
  Result:=FObj.PasswordChar;
end;

procedure TEEdit.SetPasswordChar(const Value: Char); stdcall;
begin
  FObj.PasswordChar:=Value;
end;

function TEEdit.GetReadOnly: Boolean; stdcall;
begin
  Result:=FObj.ReadOnly;
end;

procedure TEEdit.SetReadOnly(const Value: Boolean); stdcall;
begin
  FObj.ReadOnly:=Value;
end;

function TEEdit.GetText: ShortString; stdcall;
begin
  Result:=FObj.Text;
end;

procedure TEEdit.SetText(const Value: ShortString); stdcall;
begin
  FObj.Text:=Value;
end;

function TEEdit.GetOnChange: TENotifyEvent; stdcall;
begin
  Result:=FOnChange;
end;

procedure TEEdit.SetOnChange(const Value: TENotifyEvent); stdcall;
begin
  FOnChange:=Value;
  if FOnChange<>nil
    then FObj.OnChange:=@QOnChange
    else FObj.OnChange:=nil;
end;

function TEEdit.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TEEdit.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

function TEEdit.GetOnDblClick: TENotifyEvent; stdcall;
begin
  Result:=FOnDblClick;
end;

procedure TEEdit.SetOnDblClick(const Value: TENotifyEvent); stdcall;
begin
  FOnDblClick:=Value;
  if FOnDblClick<>nil
    then FObj.OnDblClick:=@QOnDblClick
    else FObj.OnDblClick:=nil;
end;

function TEEdit.GetOnEnter: TENotifyEvent; stdcall;
begin
  Result:=FOnEnter;
end;

procedure TEEdit.SetOnEnter(const Value: TENotifyEvent); stdcall;
begin
  FOnEnter:=Value;
  if FOnEnter<>nil
    then FObj.OnEnter:=@QOnEnter
    else FObj.OnEnter:=nil;
end;

function TEEdit.GetOnExit: TENotifyEvent; stdcall;
begin
  Result:=FOnExit;
end;

procedure TEEdit.SetOnExit(const Value: TENotifyEvent); stdcall;
begin
  FOnExit:=Value;
  if FOnExit<>nil
    then FObj.OnExit:=@QOnExit
    else FObj.OnExit:=nil;
end;

function TEEdit.GetOnEditingDone: TENotifyEvent; stdcall;
begin
  Result:=FOnEditingDone;
end;

procedure TEEdit.SetOnEditingDone(const Value: TENotifyEvent); stdcall;
begin
  FOnEditingDone:=Value;
  if FOnEditingDone<>nil
    then FObj.OnEditingDone:=@QOnEditingDone
    else FObj.OnEditingDone:=nil;
end;

{function TEEdit.GetObj: Pointer; stdcall;
begin
  Result:=FObj;
end;}

{TEEdit - Q Methods}

procedure TEEdit.QOnChange(Sender: TObject);
begin
  FOnChange(IEControl(Future(Local_Control_Version)));
end;

procedure TEEdit.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEEdit.QOnDblClick(Sender: TObject);
begin
  FOnDblClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEEdit.QOnEnter(Sender: TObject);
begin
  FOnEnter(IEControl(Future(Local_Control_Version)));
end;

procedure TEEdit.QOnExit(Sender: TObject);
begin
  FOnExit(IEControl(Future(Local_Control_Version)));
end;

procedure TEEdit.QOnEditingDone(Sender: TObject);
begin
  FOnEditingDone(IEControl(Future(Local_Control_Version)));
end;

{TEListBox}

constructor TEListBox.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TListBox.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnClick:=nil;
  FOnDblClick:=nil;
  FOnDrawItem:=nil;
  FOnEnter:=nil;
  FOnExit:=nil;
  FOnMeasureItem:=nil;
end;

destructor TEListBox.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_ListBox_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TEListBox.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_ListBox_Version;
end;

function TEListBox.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_ListBox_Version
    then Result:=IMInterface(IEListBox(Self))
    else Result:=inherited Future(Version);
end;

function TEListBox.GetObj: TControl;
begin
  Result:=FObj;
end;

function TEListBox.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TEListBox.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TEListBox.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Items.Text;
end;

procedure TEListBox.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Items.Text:=Value;
end;

function TEListBox.GetControlType: IEControlType; stdcall;
begin
  Result:=ectListBox;
end;

function TEListBox.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TEListBox.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TEListBox.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEListBox.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEListBox.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEListBox.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEListBox.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TEListBox.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TEListBox.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEListBox.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEListBox.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TEListBox.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TEListBox.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEListBox.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEListBox.GetItemHeight: IEInt; stdcall;
begin
  Result:=FObj.ItemHeight;
end;

procedure TEListBox.SetItemHeight(const Value: IEInt); stdcall;
begin
  FObj.ItemHeight:=Value;
end;

function TEListBox.GetItemIndex: IEInt; stdcall;
begin
  Result:=FObj.ItemIndex;
end;

procedure TEListBox.SetItemIndex(const Value: IEInt); stdcall;
begin
  FObj.ItemIndex:=Value;
end;

function TEListBox.GetMultiSelect: Boolean; stdcall;
begin
  Result:=FObj.MultiSelect;
end;

procedure TEListBox.SetMultiSelect(const Value: Boolean); stdcall;
begin
  FObj.MultiSelect:=Value;
end;

function TEListBox.GetSorted: Boolean; stdcall;
begin
  Result:=FObj.Sorted;
end;

procedure TEListBox.SetSorted(const Value: Boolean); stdcall;
begin
  FObj.Sorted:=Value;
end;

function TEListBox.GetStyle: TEListBoxStyle; stdcall;
begin
  Result:=ListBoxStyleToEListBoxStyle(FObj.Style);
end;

procedure TEListBox.SetStyle(const Value: TEListBoxStyle); stdcall;
begin
  FObj.Style:=EListBoxStyleToListBoxStyle(Value);
end;

function TEListBox.GetItem(const Index: IEInt): ShortString; stdcall;
begin
  Result:=FObj.Items.Strings[Index];
end;

procedure TEListBox.SetItem(const Index: IEInt; const Value: ShortString); stdcall;
begin
  FObj.Items.Strings[Index]:=Value;
end;

function TEListBox.Items_Count: IEInt; stdcall;
begin
  Result:=FObj.Items.Count;
end;

procedure TEListBox.Items_Clear; stdcall;
begin
  FObj.Items.Clear;
end;

procedure TEListBox.Items_Add(const Value: ShortString); stdcall;
begin
  FObj.Items.Add(Value);
end;

procedure TEListBox.Items_Delete(const Index: IEInt); stdcall;
begin
  FObj.Items.Delete(Index);
end;

procedure TEListBox.Items_Insert(const Index: IEInt; const Value: ShortString); stdcall;
begin
  FObj.Items.Insert(Index,Value);
end;

function TEListBox.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TEListBox.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

function TEListBox.GetOnDblClick: TENotifyEvent; stdcall;
begin
  Result:=FOnDblClick;
end;

procedure TEListBox.SetOnDblClick(const Value: TENotifyEvent); stdcall;
begin
  FOnDblClick:=Value;
  if FOnDblClick<>nil
    then FObj.OnDblClick:=@QOnDblClick
    else FObj.OnDblClick:=nil;
end;

function TEListBox.GetOnDrawItem: TEDrawItemEvent; stdcall;
begin
  Result:=FOnDrawItem;
end;

procedure TEListBox.SetOnDrawItem(const Value: TEDrawItemEvent); stdcall;
begin
  FOnDrawItem:=Value;
  if FOnDrawItem<>nil
    then FObj.OnDrawItem:=@QOnDrawItem
    else FObj.OnDrawItem:=nil;
end;

function TEListBox.GetOnEnter: TENotifyEvent; stdcall;
begin
  Result:=FOnEnter;
end;

procedure TEListBox.SetOnEnter(const Value: TENotifyEvent); stdcall;
begin
  FOnEnter:=Value;
  if FOnEnter<>nil
    then FObj.OnEnter:=@QOnEnter
    else FObj.OnEnter:=nil;
end;

function TEListBox.GetOnExit: TENotifyEvent; stdcall;
begin
  Result:=FOnExit;
end;

procedure TEListBox.SetOnExit(const Value: TENotifyEvent); stdcall;
begin
  FOnExit:=Value;
  if FOnExit<>nil
    then FObj.OnExit:=@QOnExit
    else FObj.OnExit:=nil;
end;

function TEListBox.GetOnMeasureItem: TEMeasureItemEvent; stdcall;
begin
  Result:=FOnMeasureItem;
end;

procedure TEListBox.SetOnMeasureItem(const Value: TEMeasureItemEvent); stdcall;
begin
  FOnMeasureItem:=Value;
  if FOnMeasureItem<>nil
    then FObj.OnMeasureItem:=@QOnMeasureItem
    else FObj.OnMeasureItem:=nil;
end;

{TEListBox - Q Methods}

procedure TEListBox.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEListBox.QOnDblClick(Sender: TObject);
begin
  FOnDblClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEListBox.QOnMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
begin
  FOnMeasureItem(IEControl(Future(Local_Control_Version)),Index,AHeight);
end;

procedure TEListBox.QOnEnter(Sender: TObject);
begin
  FOnEnter(IEControl(Future(Local_Control_Version)));
end;

procedure TEListBox.QOnExit(Sender: TObject);
begin
  FOnExit(IEControl(Future(Local_Control_Version)));
end;

procedure TEListBox.QOnDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  FOnDrawItem(IEControl(Future(Local_Control_Version)),Index,ERect(ARect),OwnerDrawStateToEOwnerDrawState(State));
end;

{TESpinEdit}

constructor TESpinEdit.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TSpinEdit.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnChange:=nil;
  FOnClick:=nil;
  FOnEnter:=nil;
  FOnExit:=nil;
  FOnEditingDone:=nil;
end;

destructor TESpinEdit.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_SpinEdit_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TESpinEdit.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_SpinEdit_Version;
end;

function TESpinEdit.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_SpinEdit_Version
    then Result:=IMInterface(IESpinEdit(Self))
    else Result:=inherited Future(Version);
end;

function TESpinEdit.GetObj: TControl;
begin
  Result:=FObj;
end;

function TESpinEdit.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TESpinEdit.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TESpinEdit.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Text;
end;

procedure TESpinEdit.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Text:=Value;
end;

function TESpinEdit.GetControlType: IEControlType; stdcall;
begin
  Result:=ectSpinEdit;
end;

function TESpinEdit.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TESpinEdit.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TESpinEdit.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TESpinEdit.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TESpinEdit.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TESpinEdit.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TESpinEdit.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TESpinEdit.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TESpinEdit.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TESpinEdit.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TESpinEdit.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TESpinEdit.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TESpinEdit.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TESpinEdit.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TESpinEdit.GetAutoSelect: Boolean; stdcall;
begin
  Result:=FObj.AutoSelect;
end;

procedure TESpinEdit.SetAutoSelect(const Value: Boolean); stdcall;
begin
  FObj.AutoSelect:=Value;
end;

function TESpinEdit.GetMaxValue: IEInt; stdcall;
begin
  Result:=FObj.MaxValue;
end;

procedure TESpinEdit.SetMaxValue(const Value: IEInt); stdcall;
begin
  FObj.MaxValue:=Value
end;

function TESpinEdit.GetMinValue: IEInt; stdcall;
begin
  Result:=FObj.MinValue;
end;

procedure TESpinEdit.SetMinValue(const Value: IEInt); stdcall;
begin
  FObj.MinValue:=Value;
end;

function TESpinEdit.GetReadOnly: Boolean; stdcall;
begin
  Result:=FObj.ReadOnly;
end;

procedure TESpinEdit.SetReadOnly(const Value: Boolean); stdcall;
begin
  FObj.ReadOnly:=Value;
end;

function TESpinEdit.GetValue: IEInt; stdcall;
begin
  Result:=FObj.Value;
end;

procedure TESpinEdit.SetValue(const Value: IEInt); stdcall;
begin
  FObj.Value:=Value;
end;

function TESpinEdit.GetOnChange: TENotifyEvent; stdcall;
begin
  Result:=FOnChange;
end;

procedure TESpinEdit.SetOnChange(const Value: TENotifyEvent); stdcall;
begin
  FOnChange:=Value;
  if FOnChange<>nil
    then FObj.OnChange:=@QOnChange
    else FObj.OnChange:=nil;
end;

function TESpinEdit.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TESpinEdit.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

function TESpinEdit.GetOnEnter: TENotifyEvent; stdcall;
begin
  Result:=FOnEnter;
end;

procedure TESpinEdit.SetOnEnter(const Value: TENotifyEvent); stdcall;
begin
  FOnEnter:=Value;
  if FOnEnter<>nil
    then FObj.OnEnter:=@QOnEnter
    else FObj.OnEnter:=nil;
end;

function TESpinEdit.GetOnExit: TENotifyEvent; stdcall;
begin
  Result:=FOnExit;
end;

procedure TESpinEdit.SetOnExit(const Value: TENotifyEvent); stdcall;
begin
  FOnExit:=Value;
  if FOnExit<>nil
    then FObj.OnExit:=@QOnExit
    else FObj.OnExit:=nil;
end;

function TESpinEdit.GetOnEditingDone: TENotifyEvent; stdcall;
begin
  Result:=FOnEditingDone;
end;

procedure TESpinEdit.SetOnEditingDone(const Value: TENotifyEvent); stdcall;
begin
  FOnEditingDone:=Value;
  if FOnEditingDone<>nil
    then FObj.OnEditingDone:=@QOnEditingDone
    else FObj.OnEditingDone:=nil;
end;

{TESpinEdit - Q Methods}

procedure TESpinEdit.QOnChange(Sender: TObject);
begin
  FOnChange(IEControl(Future(Local_Control_Version)));
end;

procedure TESpinEdit.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

procedure TESpinEdit.QOnEnter(Sender: TObject);
begin
  FOnEnter(IEControl(Future(Local_Control_Version)));
end;

procedure TESpinEdit.QOnExit(Sender: TObject);
begin
  FOnExit(IEControl(Future(Local_Control_Version)));
end;

procedure TESpinEdit.QOnEditingDone(Sender: TObject);
begin
  FOnEditingDone(IEControl(Future(Local_Control_Version)));
end;

{TFloatSpinEdit}

constructor TEFloatSpinEdit.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TFloatSpinEdit.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnChange:=nil;
  FOnClick:=nil;
  FOnEnter:=nil;
  FOnExit:=nil;
  FOnEditingDone:=nil;
end;

destructor TEFloatSpinEdit.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_FloatSpinEdit_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TEFloatSpinEdit.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_FloatSpinEdit_Version;
end;

function TEFloatSpinEdit.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_FloatSpinEdit_Version
    then Result:=IMInterface(IEFloatSpinEdit(Self))
    else Result:=inherited Future(Version);
end;

function TEFloatSpinEdit.GetObj: TControl;
begin
  Result:=FObj;
end;

function TEFloatSpinEdit.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TEFloatSpinEdit.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TEFloatSpinEdit.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Text;
end;

procedure TEFloatSpinEdit.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Text:=Value;
end;

function TEFloatSpinEdit.GetControlType: IEControlType; stdcall;
begin
  Result:=ectFloatSpinEdit;
end;

function TEFloatSpinEdit.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TEFloatSpinEdit.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TEFloatSpinEdit.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEFloatSpinEdit.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEFloatSpinEdit.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEFloatSpinEdit.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEFloatSpinEdit.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TEFloatSpinEdit.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TEFloatSpinEdit.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEFloatSpinEdit.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEFloatSpinEdit.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TEFloatSpinEdit.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TEFloatSpinEdit.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEFloatSpinEdit.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEFloatSpinEdit.GetAutoSelect: Boolean; stdcall;
begin
  Result:=FObj.AutoSelect;
end;

procedure TEFloatSpinEdit.SetAutoSelect(const Value: Boolean); stdcall;
begin
  FObj.AutoSelect:=Value;
end;

function TEFloatSpinEdit.GetMaxValue: IEFloat; stdcall;
begin
  Result:=FObj.MaxValue;
end;

procedure TEFloatSpinEdit.SetMaxValue(const Value: IEFloat); stdcall;
begin
  FObj.MaxValue:=Value
end;

function TEFloatSpinEdit.GetMinValue: IEFloat; stdcall;
begin
  Result:=FObj.MinValue;
end;

procedure TEFloatSpinEdit.SetMinValue(const Value: IEFloat); stdcall;
begin
  FObj.MinValue:=Value;
end;

function TEFloatSpinEdit.GetReadOnly: Boolean; stdcall;
begin
  Result:=FObj.ReadOnly;
end;

procedure TEFloatSpinEdit.SetReadOnly(const Value: Boolean); stdcall;
begin
  FObj.ReadOnly:=Value;
end;

function TEFloatSpinEdit.GetValue: IEFloat; stdcall;
begin
  Result:=FObj.Value;
end;

procedure TEFloatSpinEdit.SetValue(const Value: IEFloat); stdcall;
begin
  FObj.Value:=Value;
end;

function TEFloatSpinEdit.GetOnChange: TENotifyEvent; stdcall;
begin
  Result:=FOnChange;
end;

procedure TEFloatSpinEdit.SetOnChange(const Value: TENotifyEvent); stdcall;
begin
  FOnChange:=Value;
  if FOnChange<>nil
    then FObj.OnChange:=@QOnChange
    else FObj.OnChange:=nil;
end;

function TEFloatSpinEdit.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TEFloatSpinEdit.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

function TEFloatSpinEdit.GetOnEnter: TENotifyEvent; stdcall;
begin
  Result:=FOnEnter;
end;

procedure TEFloatSpinEdit.SetOnEnter(const Value: TENotifyEvent); stdcall;
begin
  FOnEnter:=Value;
  if FOnEnter<>nil
    then FObj.OnEnter:=@QOnEnter
    else FObj.OnEnter:=nil;
end;

function TEFloatSpinEdit.GetOnExit: TENotifyEvent; stdcall;
begin
  Result:=FOnExit;
end;

procedure TEFloatSpinEdit.SetOnExit(const Value: TENotifyEvent); stdcall;
begin
  FOnExit:=Value;
  if FOnExit<>nil
    then FObj.OnExit:=@QOnExit
    else FObj.OnExit:=nil;
end;

function TEFloatSpinEdit.GetOnEditingDone: TENotifyEvent; stdcall;
begin
  Result:=FOnEditingDone;
end;

procedure TEFloatSpinEdit.SetOnEditingDone(const Value: TENotifyEvent); stdcall;
begin
  FOnEditingDone:=Value;
  if FOnEditingDone<>nil
    then FObj.OnEditingDone:=@QOnEditingDone
    else FObj.OnEditingDone:=nil;
end;

{TEFloatSpinEdit - Q Methods}

procedure TEFloatSpinEdit.QOnChange(Sender: TObject);
begin
  FOnChange(IEControl(Future(Local_Control_Version)));
end;

procedure TEFloatSpinEdit.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEFloatSpinEdit.QOnEnter(Sender: TObject);
begin
  FOnEnter(IEControl(Future(Local_Control_Version)));
end;

procedure TEFloatSpinEdit.QOnExit(Sender: TObject);
begin
  FOnExit(IEControl(Future(Local_Control_Version)));
end;

procedure TEFloatSpinEdit.QOnEditingDone(Sender: TObject);
begin
  FOnEditingDone(IEControl(Future(Local_Control_Version)));
end;

{TECheckbox}

constructor TECheckbox.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TCheckbox.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnChange:=nil;
  FOnClick:=nil;
end;

destructor TECheckbox.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_Checkbox_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TECheckbox.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_Checkbox_Version;
end;

function TECheckbox.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_Checkbox_Version
    then Result:=IMInterface(IECheckbox(Self))
    else Result:=inherited Future(Version);
end;

function TECheckBox.GetObj: TControl;
begin
  Result:=FObj;
end;

function TECheckbox.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TECheckbox.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TECheckbox.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Caption;
end;

procedure TECheckbox.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Caption:=Value;
end;

function TECheckbox.GetControlType: IEControlType; stdcall;
begin
  Result:=ectCheckbox;
end;

function TECheckbox.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TECheckbox.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TECheckbox.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TECheckbox.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TECheckbox.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TECheckbox.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TECheckbox.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TECheckbox.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TECheckbox.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TECheckbox.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TECheckbox.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TECheckbox.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TECheckbox.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TECheckbox.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TECheckbox.GetAllowGrayed: Boolean; stdcall;
begin
  Result:=FObj.AllowGrayed;
end;

procedure TECheckbox.SetAllowGrayed(const Value: Boolean); stdcall;
begin
  FObj.AllowGrayed:=Value;
end;

function TECheckbox.GetChecked: Boolean; stdcall;
begin
  Result:=FObj.Checked;
end;

procedure TECheckbox.SetChecked(const Value: Boolean); stdcall;
begin
  FObj.Checked:=Value;
end;

function TECheckbox.GetState: IECheckState; stdcall;
//const
//  _IEStates: array [TCheckboxState] of IECheckState = (ieUnchecked,ieChecked,ieGrayed);
begin
  Result:=CheckboxStateToIECheckState(FObj.State)//_IEStates[FObj.State];
end;

procedure TECheckbox.SetState(const Value: IECheckState); stdcall;
//const
  //_CheckboxStates: array [ieUnchecked..ieGrayed] of TCheckboxState = (cbUnchecked,cbChecked,cbGrayed);
begin
  FObj.State:=IECheckStateToCheckboxState(Value);
  //FObj.State:=_CheckboxStates[Value];
end;

function TECheckbox.GetOnChange: TENotifyEvent; stdcall;
begin
  Result:=FOnChange;
end;

procedure TECheckbox.SetOnChange(const Value: TENotifyEvent); stdcall;
begin
  FOnChange:=Value;
  if FOnChange<>nil
    then FObj.OnChange:=@QOnChange
    else FObj.OnChange:=nil;
end;

function TECheckbox.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TECheckbox.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

{TECheckbox - Q Methods}

procedure TECheckbox.QOnChange(Sender: TObject);
begin
  FOnChange(IEControl(Future(Local_Control_Version)));
end;

procedure TECheckbox.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

{IEComboBox}

constructor TEComboBox.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TComboBox.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FOnChange:=nil;
  FOnClick:=nil;
  FOnCloseUp:=nil;
  FOnDblClick:=nil;
  FOnDrawItem:=nil;
  FOnDropDown:=nil;
  FOnEditingDone:=nil;
  FOnEnter:=nil;
  FOnExit:=nil;
  FOnMeasureItem:=nil;
end;

destructor TEComboBox.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_ComboBox_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TEComboBox.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_ComboBox_Version;
end;

function TEComboBox.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_ComboBox_Version
    then Result:=IMInterface(IEComboBox(Self))
    else Result:=inherited Future(Version);
end;

function TEComboBox.GetObj: TControl;
begin
  Result:=FObj;
end;

function TEComboBox.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TEComboBox.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TEComboBox.GetCaption: ShortString; stdcall;
begin
  Result:=FObj.Text;
end;

procedure TEComboBox.SetCaption(const Value: ShortString); stdcall;
begin
  FObj.Text:=Value;
end;

function TEComboBox.GetControlType: IEControlType; stdcall;
begin
  Result:=ectComboBox;
end;

function TEComboBox.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TEComboBox.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TEComboBox.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEComboBox.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEComboBox.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEComboBox.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEComboBox.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TEComboBox.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TEComboBox.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEComboBox.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEComboBox.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TEComboBox.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TEComboBox.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEComboBox.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEComboBox.GetAutoComplete: Boolean; stdcall;
begin
  Result:=FObj.AutoComplete;
end;

procedure TEComboBox.SetAutoComplete(const Value: Boolean); stdcall;
begin
  FObj.AutoComplete:=Value;
end;

function TEComboBox.GetAutoDropDown: Boolean; stdcall;
begin
  Result:=FObj.AutoDropDown;
end;

procedure TEComboBox.SetAutoDropDown(const Value: Boolean); stdcall;
begin
  FObj.AutoDropDown:=Value;
end;

function TEComboBox.GetAutoSelect: Boolean; stdcall;
begin
  Result:=FObj.AutoSelect;
end;

procedure TEComboBox.SetAutoSelect(const Value: Boolean); stdcall;
begin
  FObj.AutoSelect:=Value;
end;

function TEComboBox.GetDropDownCount: IEInt; stdcall;
begin
  Result:=FObj.DropDownCount;
end;

procedure TEComboBox.SetDropDownCount(const Value: IEInt); stdcall;
begin
  FObj.DropDownCount:=Value;
end;

function TEComboBox.GetItemHeight: IEInt; stdcall;
begin
  Result:=FObj.ItemHeight;
end;

procedure TEComboBox.SetItemHeight(const Value: IEInt); stdcall;
begin
  FObj.ItemHeight:=Value;
end;

function TEComboBox.GetItemIndex: IEInt; stdcall;
begin
  Result:=FObj.ItemIndex;
end;

procedure TEComboBox.SetItemIndex(const Value: IEInt); stdcall;
begin
  FObj.ItemIndex:=Value;
end;

function TEComboBox.GetMaxLength: IEInt; stdcall;
begin
  Result:=FObj.MaxLength;
end;

procedure TEComboBox.SetMaxLength(const Value: IEInt); stdcall;
begin
  FObj.MaxLength:=Value;
end;

function TEComboBox.GetReadOnly: Boolean; stdcall;
begin
  Result:=FObj.ReadOnly;
end;

procedure TEComboBox.SetReadOnly(const Value: Boolean); stdcall;
begin
  FObj.ReadOnly:=Value;
end;

function TEComboBox.GetSorted: Boolean; stdcall;
begin
  Result:=FObj.Sorted;
end;

procedure TEComboBox.SetSorted(const Value: Boolean); stdcall;
begin
  FObj.Sorted:=Value;
end;

function TEComboBox.GetStyle: TEComboBoxStyle; stdcall;
begin
  Result:=ComboBoxStyleToEComboBoxStyle(FObj.Style);
end;

procedure TEComboBox.SetStyle(const Value: TEComboBoxStyle); stdcall;
begin
  FObj.Style:=EComboBoxStyleToComboBoxStyle(Value);
end;

function TEComboBox.GetItem(const Index: IEInt): ShortString; stdcall;
begin
  Result:=FObj.Items.Strings[Index];
end;

procedure TEComboBox.SetItem(const Index: IEInt; const Value: ShortString); stdcall;
begin
  FObj.Items.Strings[Index]:=Value;
end;

function TEComboBox.Items_Count: IEInt; stdcall;
begin
  Result:=FObj.Items.Count;
end;

procedure TEComboBox.Items_Clear; stdcall;
begin
  FObj.Items.Clear;
end;

procedure TEComboBox.Items_Add(const Value: ShortString); stdcall;
begin
  FObj.Items.Add(Value);
end;

procedure TEComboBox.Items_Delete(const Index: IEInt); stdcall;
begin
  FObj.Items.Delete(Index);
end;

procedure TEComboBox.Items_Insert(const Index: IEInt; const Value: ShortString); stdcall;
begin
  FObj.Items.Insert(Index,Value);
end;

function TEComboBox.GetOnChange: TENotifyEvent; stdcall;
begin
  Result:=FOnChange;
end;

procedure TEComboBox.SetOnChange(const Value: TENotifyEvent); stdcall;
begin
  FOnChange:=Value;
  if FOnChange<>nil
    then FObj.OnChange:=@QOnChange
    else FObj.OnChange:=nil;
end;

function TEComboBox.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TEComboBox.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

function TEComboBox.GetOnDblClick: TENotifyEvent; stdcall;
begin
  Result:=FOnDblClick;
end;

procedure TEComboBox.SetOnDblClick(const Value: TENotifyEvent); stdcall;
begin
  FOnDblClick:=Value;
  if FOnDblClick<>nil
    then FObj.OnDblClick:=@QOnDblClick
    else FObj.OnDblClick:=nil;
end;

function TEComboBox.GetOnDrawItem: TEDrawItemEvent; stdcall;
begin
  Result:=FOnDrawItem;
end;

procedure TEComboBox.SetOnDrawItem(const Value: TEDrawItemEvent); stdcall;
begin
  FOnDrawItem:=Value;
  if FOnDrawItem<>nil
    then FObj.OnDrawItem:=@QOnDrawItem
    else FObj.OnDrawItem:=nil;
end;

function TEComboBox.GetOnDropDown: TENotifyEvent; stdcall;
begin
  Result:=FOnDropDown;
end;

procedure TEComboBox.SetOnDropDown(const Value: TENotifyEvent); stdcall;
begin
  FOnDropDown:=Value;
  if FOnDropDown<>nil
    then FObj.OnDropDown:=@QOnDropDown
    else FObj.OnDropDown:=nil;
end;

function TEComboBox.GetOnCloseUp: TENotifyEvent; stdcall;
begin
  Result:=FOnCloseUp;
end;

procedure TEComboBox.SetOnCloseUp(const Value: TENotifyEvent); stdcall;
begin
  FOnCloseUp:=Value;
  if FOnCloseUp<>nil
    then FObj.OnCloseUp:=@QOnCloseUp
    else FObj.OnCloseUp:=nil;
end;

function TEComboBox.GetOnEditingDone: TENotifyEvent; stdcall;
begin
  Result:=FOnEditingDone;
end;

procedure TEComboBox.SetOnEditingDone(const Value: TENotifyEvent); stdcall;
begin
  FOnEditingDone:=Value;
  if FOnEditingDone<>nil
    then FObj.OnEditingDone:=@QOnEditingDone
    else FObj.OnEditingDone:=nil;
end;

function TEComboBox.GetOnEnter: TENotifyEvent; stdcall;
begin
  Result:=FOnEnter;
end;

procedure TEComboBox.SetOnEnter(const Value: TENotifyEvent); stdcall;
begin
  FOnEnter:=Value;
  if FOnEnter<>nil
    then FObj.OnEnter:=@QOnEnter
    else FObj.OnEnter:=nil;
end;

function TEComboBox.GetOnExit: TENotifyEvent; stdcall;
begin
  Result:=FOnExit;
end;

procedure TEComboBox.SetOnExit(const Value: TENotifyEvent); stdcall;
begin
  FOnExit:=Value;
  if FOnExit<>nil
    then FObj.OnExit:=@QOnExit
    else FObj.OnExit:=nil;
end;

function TEComboBox.GetOnMeasureItem: TEMeasureItemEvent; stdcall;
begin
  Result:=FOnMeasureItem;
end;

procedure TEComboBox.SetOnMeasureItem(const Value: TEMeasureItemEvent); stdcall;
begin
  FOnMeasureItem:=Value;
  if FOnMeasureItem<>nil
    then FObj.OnMeasureItem:=@QOnMeasureItem
    else FObj.OnMeasureItem:=nil;
end;

{TEComboBox - Q Methods}

procedure TEComboBox.QOnChange(Sender: TObject);
begin
  FOnChange(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnCloseUp(Sender: TObject);
begin
  FOnCloseUp(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnDblClick(Sender: TObject);
begin
  FOnDblClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  FOnDrawItem(IEControl(Future(Local_Control_Version)),Index,ERect(ARect),OwnerDrawStateToEOwnerDrawState(State));
end;

procedure TEComboBox.QOnDropDown(Sender: TObject);
begin
  FOnDropDown(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnEditingDone(Sender: TObject);
begin
  FOnEditingDone(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnEnter(Sender: TObject);
begin
  FOnEnter(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnExit(Sender: TObject);
begin
  FOnExit(IEControl(Future(Local_Control_Version)));
end;

procedure TEComboBox.QOnMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
begin
  FOnMeasureItem(IEControl(Future(Local_Control_Version)),Index,AHeight);
end;

{TEColorBox}

constructor TEColorBox.Create(AOwner: TEGUIArea);
begin
  inherited Create(AOwner);
  FObj:=TColorBox.Create(AOwner.FObj);
  AOwner.FObj.InsertControl(FObj);
  FObj.Style:=[cbCustomColor,cbPrettyNames,cbExtendedColors,cbStandardColors];
  FOnChange:=nil;
  FOnClick:=nil;
  FOnCloseUp:=nil;
  FOnDblClick:=nil;
  FOnDropDown:=nil;
  FOnEditingDone:=nil;
  FOnEnter:=nil;
  FOnExit:=nil;
end;

destructor TEColorBox.Destroy;
begin
  Owner.FObj.RemoveControl(FObj);
  FObj.Destroy;
  inherited Destroy;
end;

const
  Local_ColorBox_Version: MVVersion = (Version:0;MainVersion:2;SubVersion:0);

function TEColorBox.GetVersion: MVVersion; stdcall;
begin
  Result:=Local_ColorBox_Version;
end;

function TEColorBox.Future(const Version: MVVersion): IMInterface; stdcall;
begin
  if Version=Local_ColorBox_Version
    then Result:=IMInterface(IEColorBox(Self))
    else Result:=inherited Future(Version);
end;

function TEColorBox.GetObj: TControl;
begin
  Result:=FObj;
end;

function TEColorBox.GetAnchors: TEAnchors; stdcall;
begin
  Result:=AnchorsToEAnchors(FObj.Anchors);
end;

procedure TEColorBox.SetAnchors(const Value: TEAnchors); stdcall;
begin
  FObj.Anchors:=EAnchorsToAnchors(Value);
end;

function TEColorBox.GetCaption: ShortString; stdcall;
begin
  Result:=IntToHex(FObj.Selected,6);
end;

procedure TEColorBox.SetCaption(const Value: ShortString); stdcall;
begin

end;

function TEColorBox.GetControlType: IEControlType; stdcall;
begin
  Result:=ectColorBox;
end;

function TEColorBox.GetEnabled: Boolean; stdcall;
begin
  Result:=FObj.Enabled;
end;

procedure TEColorBox.SetEnabled(const Value: Boolean); stdcall;
begin
  FObj.Enabled:=Value;
end;

function TEColorBox.GetHeight: IEInt; stdcall;
begin
  Result:=FObj.Height;
end;

procedure TEColorBox.SetHeight(const Value: IEInt); stdcall;
begin
  FObj.Height:=Value;
end;

function TEColorBox.GetLeft: IEInt; stdcall;
begin
  Result:=FObj.Left;
end;

procedure TEColorBox.SetLeft(const Value: IEInt); stdcall;
begin
  FObj.Left:=Value;
end;

function TEColorBox.GetTabOrder: IEInt; stdcall;
begin
  Result:=FObj.TabOrder;
end;

procedure TEColorBox.SetTabOrder(const Value: IEInt); stdcall;
begin
  FObj.TabOrder:=Value;
end;

function TEColorBox.GetTop: IEInt; stdcall;
begin
  Result:=FObj.Top;
end;

procedure TEColorBox.SetTop(const Value: IEInt); stdcall;
begin
  FObj.Top:=Value;
end;

function TEColorBox.GetVisible: Boolean; stdcall;
begin
  Result:=FObj.Visible;
end;

procedure TEColorBox.SetVisible(const Value: Boolean); stdcall;
begin
  FObj.Visible:=Value;
end;

function TEColorBox.GetWidth: IEInt; stdcall;
begin
  Result:=FObj.Width;
end;

procedure TEColorBox.SetWidth(const Value: IEInt); stdcall;
begin
  FObj.Width:=Value;
end;

function TEColorBox.GetAutoComplete: Boolean; stdcall;
begin
  Result:=FObj.AutoComplete;
end;

procedure TEColorBox.SetAutoComplete(const Value: Boolean); stdcall;
begin
  FObj.AutoComplete:=Value;
end;

function TEColorBox.GetAutoDropDown: Boolean; stdcall;
begin
  Result:=FObj.AutoDropDown;
end;

procedure TEColorBox.SetAutoDropDown(const Value: Boolean); stdcall;
begin
  FObj.AutoDropDown:=Value;
end;

function TEColorBox.GetAutoSelect: Boolean; stdcall;
begin
  Result:=FObj.AutoSelect;
end;

procedure TEColorBox.SetAutoSelect(const Value: Boolean); stdcall;
begin
  FObj.AutoSelect:=Value;
end;

function TEColorBox.GetDefaultColor: IEColor; stdcall;
begin
  Result:=FObj.DefaultColorColor;
end;

procedure TEColorBox.SetDefaultColor(const Value: IEColor); stdcall;
begin
  FObj.DefaultColorColor:=Value;
end;

function TEColorBox.GetDropDownCount: IEInt; stdcall;
begin
  Result:=FObj.DropDownCount;
end;

procedure TEColorBox.SetDropDownCount(const Value: IEInt); stdcall;
begin
  FObj.DropDownCount:=Value;
end;

function TEColorBox.GetItemHeight: IEInt; stdcall;
begin
  Result:=FObj.ItemHeight;
end;

procedure TEColorBox.SetItemHeight(const Value: IEInt); stdcall;
begin
  FObj.ItemHeight:=Value;
end;

function TEColorBox.GetSelected: IEColor; stdcall;
begin
  Result:=FObj.Selected;
end;

procedure TEColorBox.SetSelected(const Value: IEColor); stdcall;
begin
  FObj.Selected:=Value;
end;

function TEColorBox.GetStyle: TEColorBoxStyle; stdcall;
begin
  Result:=ColorBoxStyleToEColorBoxStyle(FObj.Style);
end;

procedure TEColorBox.SetStyle(const Value: TEColorBoxStyle); stdcall;
begin
  FObj.Style:=EColorBoxStyleToColorBoxStyle(Value);
end;

function TEColorBox.GetNoneColor: IEColor; stdcall;
begin
  Result:=FObj.NoneColorColor;
end;

procedure TEColorBox.SetNoneColor(const Value: IEColor); stdcall;
begin
  FObj.NoneColorColor:=Value;
end;

function TEColorBox.GetItem(const Index: IEInt): ShortString; stdcall;
begin
  Result:=FObj.Items.Strings[Index];
end;

function TEColorBox.Items_Count: IEInt; stdcall;
begin
  Result:=FObj.Items.Count;
end;

function TEColorBox.GetOnChange: TENotifyEvent; stdcall;
begin
  Result:=FOnChange;
end;

procedure TEColorBox.SetOnChange(const Value: TENotifyEvent); stdcall;
begin
  FOnChange:=Value;
  if FOnChange<>nil
    then FObj.OnChange:=@QOnChange
    else FObj.OnChange:=nil;
end;

function TEColorBox.GetOnClick: TENotifyEvent; stdcall;
begin
  Result:=FOnClick;
end;

procedure TEColorBox.SetOnClick(const Value: TENotifyEvent); stdcall;
begin
  FOnClick:=Value;
  if FOnClick<>nil
    then FObj.OnClick:=@QOnClick
    else FObj.OnClick:=nil;
end;

function TEColorBox.GetOnDblClick: TENotifyEvent; stdcall;
begin
  Result:=FOnDblClick;
end;

procedure TEColorBox.SetOnDblClick(const Value: TENotifyEvent); stdcall;
begin
  FOnDblClick:=Value;
  if FOnDblClick<>nil
    then FObj.OnDblClick:=@QOnDblClick
    else FObj.OnDblClick:=nil;
end;

function TEColorBox.GetOnDropDown: TENotifyEvent; stdcall;
begin
  Result:=FOnDropDown;
end;

procedure TEColorBox.SetOnDropDown(const Value: TENotifyEvent); stdcall;
begin
  FOnDropDown:=Value;
  if FOnDropDown<>nil
    then FObj.OnDropDown:=@QOnDropDown
    else FObj.OnDropDown:=nil;
end;

function TEColorBox.GetOnCloseUp: TENotifyEvent; stdcall;
begin
  Result:=FOnCloseUp;
end;

procedure TEColorBox.SetOnCloseUp(const Value: TENotifyEvent); stdcall;
begin
  FOnCloseUp:=Value;
  if FOnCloseUp<>nil
    then FObj.OnCloseUp:=@QOnCloseUp
    else FObj.OnCloseUp:=nil;
end;

function TEColorBox.GetOnEditingDone: TENotifyEvent; stdcall;
begin
  Result:=FOnEditingDone;
end;

procedure TEColorBox.SetOnEditingDone(const Value: TENotifyEvent); stdcall;
begin
  FOnEditingDone:=Value;
  if FOnEditingDone<>nil
    then FObj.OnEditingDone:=@QOnEditingDone
    else FObj.OnEditingDone:=nil;
end;

function TEColorBox.GetOnEnter: TENotifyEvent; stdcall;
begin
  Result:=FOnEnter;
end;

procedure TEColorBox.SetOnEnter(const Value: TENotifyEvent); stdcall;
begin
  FOnEnter:=Value;
  if FOnEnter<>nil
    then FObj.OnEnter:=@QOnEnter
    else FObj.OnEnter:=nil;
end;

function TEColorBox.GetOnExit: TENotifyEvent; stdcall;
begin
  Result:=FOnExit;
end;

procedure TEColorBox.SetOnExit(const Value: TENotifyEvent); stdcall;
begin
  FOnExit:=Value;
  if FOnExit<>nil
    then FObj.OnExit:=@QOnExit
    else FObj.OnExit:=nil;
end;

{TEColorBox - Q Methods}

procedure TEColorBox.QOnChange(Sender: TObject);
begin
  FOnChange(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnClick(Sender: TObject);
begin
  FOnClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnCloseUp(Sender: TObject);
begin
  FOnCloseUp(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnDblClick(Sender: TObject);
begin
  FOnDblClick(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnDropDown(Sender: TObject);
begin
  FOnDropDown(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnEditingDone(Sender: TObject);
begin
  FOnEditingDone(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnEnter(Sender: TObject);
begin
  FOnEnter(IEControl(Future(Local_Control_Version)));
end;

procedure TEColorBox.QOnExit(Sender: TObject);
begin
  FOnExit(IEControl(Future(Local_Control_Version)));
end;

{Allgemein}

{function NewIControl_(const ControlType: IEControlType; const AOwner: TComponent): IEControl; stdcall;
var
  ANewControl: TEEdit;
begin
  ANewControl:=TEEdit.Create(AOwner);
  Result:=IEControl(IEEdit(ANewControl));
end;

procedure DisposeIControl_(var AControl: IEControl); stdcall;
begin
  AControl:=nil;
end;}

function EAnchorsToAnchors(AEAnchors: TEAnchors): TAnchors;
begin
  Result:=[];
  if AEAnchors and eakTop>0 then Include(Result,akTop);
  if AEAnchors and eakLeft>0 then Include(Result,akLeft);
  if AEAnchors and eakRight>0 then Include(Result,akRight);
  if AEAnchors and eakBottom>0 then Include(Result,akBottom);
end;

function AnchorsToEAnchors(AAnchors: TAnchors): TEAnchors;
begin
  Result:=0;
  if akTop in AAnchors then Result+=eakTop;
  if akLeft in AAnchors then Result+=eakLeft;
  if akRight in AAnchors then Result+=eakRight;
  if akBottom in AAnchors then Result+=eakBottom;
end;

function ListBoxStyleToEListBoxStyle(AListBoxStyle: TListBoxStyle): TEListBoxStyle;
const
  _EListBoxStyles: array [TListBoxStyle] of TEListBoxStyle = (elbStandard,elbOwnerDrawFixed,elbOwnerDrawVariable,elbVirtual);
begin
  Result:=_EListBoxStyles[AListBoxStyle];
end;

function EListBoxStyleToListBoxStyle(AListBoxStyle: TEListBoxStyle): TListBoxStyle;
const
  _ListBoxStyles: array [elbStandard..elbOwnerDrawVariable] of TListBoxStyle = (lbStandard,lbOwnerDrawFixed,lbOwnerDrawVariable);
begin
  if AListBoxStyle>elbOwnerDrawVariable
    then Result:=lbStandard
    else Result:=_ListBoxStyles[AListBoxStyle];
end;

function ComboBoxStyleToEComboBoxStyle(AComboBoxStyle: TComboBoxStyle): TEComboBoxStyle;
const
  _EComboBoxStyles: array [TComboBoxStyle] of TEComboBoxStyle = (ecsDropDown,ecsSimple,ecsDropDownList,ecsOwnerDrawFixed,ecsOwnerDrawVariable);
begin
  Result:=_EComboBoxStyles[AComboBoxStyle];
end;

function EComboBoxStyleToComboBoxStyle(AComboBoxStyle: TEComboBoxStyle): TComboBoxStyle;
const
  _ComboBoxStyles: array [ecsDropDown..ecsOwnerDrawVariable] of TComboBoxStyle = (csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable);
begin
  if AComboBoxStyle>ecsOwnerDrawVariable
    then Result:=csDropDown
    else Result:=_ComboBoxStyles[AComboBoxStyle];
end;

function ColorBoxStyleToEColorBoxStyle(AColorBoxStyle: TColorBoxStyle): TEColorBoxStyle;
begin
  Result:=0;
  if cbStandardColors in AColorBoxStyle then Result+=ecbStandardColors;
  if cbExtendedColors in AColorBoxStyle then Result+=ecbExtendedColors;
  if cbSystemColors in AColorBoxStyle then Result+=ecbSystemColors;
  if cbIncludeNone in AColorBoxStyle then Result+=ecbIncludeNone;
  if cbIncludeDefault in AColorBoxStyle then Result+=ecbIncludeDefault;
  if cbCustomColor in AColorBoxStyle then Result+=ecbCustomColor;
  if cbPrettyNames in AColorBoxStyle then Result+=ecbPrettyNames;
  if cbCustomColors in AColorBoxStyle then Result+=ecbCustomColors;
end;

function EColorBoxStyleToColorBoxStyle(AColorBoxStyle: TEColorBoxStyle): TColorBoxStyle;
begin
  Result:=[];
  if AColorBoxStyle and ecbStandardColors>0 then Include(Result,cbStandardColors);
  if AColorBoxStyle and ecbExtendedColors>0 then Include(Result,cbExtendedColors);
  if AColorBoxStyle and ecbSystemColors>0 then Include(Result,cbSystemColors);
  if AColorBoxStyle and ecbIncludeNone>0 then Include(Result,cbIncludeNone);
  if AColorBoxStyle and ecbIncludeDefault>0 then Include(Result,cbIncludeDefault);
  if AColorBoxStyle and ecbCustomColor>0 then Include(Result,cbCustomColor);
  if AColorBoxStyle and ecbPrettyNames>0 then Include(Result,cbPrettyNames);
  if AColorBoxStyle and ecbCustomColors>0 then Include(Result,cbCustomColors);
end;

function OwnerDrawStateToEOwnerDrawState(AOwnerDrawState: TOwnerDrawState): TEOwnerDrawState;
begin
  Result:=0;
  if odSelected in AOwnerDrawState then Result+=eodSelected;
  if odGrayed in AOwnerDrawState then Result+=eodGrayed;
  if odDisabled in AOwnerDrawState then Result+=eodDisabled;
  if odChecked in AOwnerDrawState then Result+=eodChecked;
  if odFocused in AOwnerDrawState then Result+=eodFocused;
  if odDefault in AOwnerDrawState then Result+=eodDefault;
  if odHotLight in AOwnerDrawState then Result+=eodHotLight;
  if odInactive in AOwnerDrawState then Result+=eodInactive;
  if odNoAccel in AOwnerDrawState then Result+=eodNoAccel;
  if odNoFocusRect in AOwnerDrawState then Result+=eodNoFocusRect;
  if odReserved1 in AOwnerDrawState then Result+=eodReserved1;
  if odReserved2 in AOwnerDrawState then Result+=eodReserved2;
  if odComboBoxEdit in AOwnerDrawState then Result+=eodComboBoxEdit;
  if odPainted in AOwnerDrawState then Result+=eodPainted;
end;

function EOwnerDrawStateToOwnerDrawState(AOwnerDrawState: TEOwnerDrawState): TOwnerDrawState;
begin
  Result:=[];
  if AOwnerDrawState and eodSelected>0 then Include(Result,odSelected);
  if AOwnerDrawState and eodGrayed>0 then Include(Result,odGrayed);
  if AOwnerDrawState and eodDisabled>0 then Include(Result,odDisabled);
  if AOwnerDrawState and eodChecked>0 then Include(Result,odChecked);
  if AOwnerDrawState and eodFocused>0 then Include(Result,odFocused);
  if AOwnerDrawState and eodDefault>0 then Include(Result,odDefault);
  if AOwnerDrawState and eodHotLight>0 then Include(Result,odHotLight);
  if AOwnerDrawState and eodInactive>0 then Include(Result,odInactive);
  if AOwnerDrawState and eodNoAccel>0 then Include(Result,odNoAccel);
  if AOwnerDrawState and eodNoFocusRect>0 then Include(Result,odNoFocusRect);
  if AOwnerDrawState and eodReserved1>0 then Include(Result,odReserved1);
  if AOwnerDrawState and eodReserved2>0 then Include(Result,odReserved2);
  if AOwnerDrawState and eodComboBoxEdit>0 then Include(Result,odComboBoxEdit);
  if AOwnerDrawState and eodPainted>0 then Include(Result,odPainted);
end;

function ERect(const ARect: TRect): TERect;
begin
  with Result do begin
    Left:=ARect.Left;
    Top:=ARect.Top;
    Right:=ARect.Right;
    Bottom:=ARect.Bottom;
  end;
end;

function CheckboxStateToIECheckState(AState: TCheckboxState): IECheckState;
const
  _IEStates: array [TCheckboxState] of IECheckState = (ieUnchecked,ieChecked,ieGrayed);
begin
  Result:=_IEStates[AState];
end;

function IECheckStateToCheckboxState(AState: IECheckState): TCheckboxState;
const
  _CheckboxStates: array [ieUnchecked..ieGrayed] of TCheckboxState = (cbUnchecked,cbChecked,cbGrayed);
begin
  if AState<=ieGrayed
    then Result:=_CheckboxStates[AState]
    else Result:=cbUnchecked;
end;

end.

