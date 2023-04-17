unit MiscSlaveControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SlaveControl, Controls, AdvCoord, ImageConfig;

type
  TSlaveButtonImages  = class
  private
    FOwnsImages      : Boolean;
    FImages          : TImageConfig;
    FDownIndex       : Integer;
    FHighlightedIndex: Integer;
    FNormalIndex     : Integer;
  public
    constructor Create(AImages: TImageConfig = nil);
    destructor Destroy; override;
    property Images: TImageConfig read FImages;
    property DownIndex: Integer read FDownIndex write FDownIndex;
    property HighlightedIndex: Integer read FHighlightedIndex write FHighlightedIndex;
    property NormalIndex: Integer read FNormalIndex write FNormalIndex;
  end;

  TSlaveButton        = class (TSlaveControl)
  strict private
    FOnClick              : TSlaveControlEvent;
    FDown                 : Boolean;
    FImages               : TSlaveButtonImages;
  strict protected
    procedure DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoSetBounds(var ANewBounds: TBoundsRect); override;
    procedure Click; virtual;

    property Images: TSlaveButtonImages read FImages;
  protected
    procedure DoPaint; override;
    procedure Repaint; override;
  public
    constructor Create(AOwner: TSlaveControl; AImages: TSlaveButtonImages; AMaster: TMasterControl = nil); reintroduce;
    destructor Destroy; override;
    property Down: Boolean read FDown;
    property OnClick: TSlaveControlEvent read FOnClick write FOnClick;
  end;

  TSwitchButtonImages = class (TSlaveButtonImages)
  private
    FSwitchedDownIndex       : Integer;
    FSwitchedHighlightedIndex: Integer;
    FSwitchedNormalIndex     : Integer;
  public
    property SwitchedDownIndex: Integer read FSwitchedDownIndex write FSwitchedDownIndex;
    property SwitchedHighlightedIndex: Integer read FSwitchedHighlightedIndex write FSwitchedHighlightedIndex;
    property SwitchedNormalIndex: Integer read FSwitchedNormalIndex write FSwitchedNormalIndex;
  end;

  TSwitchButton       = class (TSlaveButton)
  private
    FSwitched: Boolean;
    FImages  : TSwitchButtonImages;
  strict protected
    procedure SetSwitched(Value: Boolean);
    procedure Click; override;

    property Images: TSwitchButtonImages read FImages;
  protected
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TSlaveControl; AImages: TSwitchButtonImages; ASwitched: Boolean = false; AMaster: TMasterControl = nil); reintroduce;
    property Switched: Boolean read FSwitched write SetSwitched;
  end;

implementation

{%REGION TSlaveButtonImages}

constructor TSlaveButtonImages.Create(AImages: TImageConfig = nil);
begin
  inherited Create;
  FOwnsImages:=(AImages=nil);
  if FOwnsImages
    then FImages:=TImageConfig.Create
    else FImages:=AImages;
end;

destructor TSlaveButtonImages.Destroy;
begin
  if FOwnsImages
    then FImages.Destroy;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TSlaveButton}

constructor TSlaveButton.Create(AOwner: TSlaveControl; AImages: TSlaveButtonImages; AMaster: TMasterControl = nil);
begin
  inherited Create(AOwner);
  FDown:=false;
  FImages:=AImages;
end;

destructor TSlaveButton.Destroy;
begin
  inherited Destroy;
end;

procedure TSlaveButton.DoMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //inherited DoMouseDown(...) hier auf keinen Fall aufrufen, weil dann
  //WireAtMouse gelöscht würde
  FDown:=true;
  Repaint;
end;

procedure TSlaveButton.DoMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  Click;
  FDown:=false;
  Repaint;
end;

procedure TSlaveButton.Click;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TSlaveButton.DoSetBounds(var ANewBounds: TBoundsRect);
begin
  with Images.Images.OutputBmp do ANewBounds.Size:=Point(Width,Height);
end;

procedure TSlaveButton.DoPaint;
begin
  with Images do begin
    if not Highlighted
      then Images.LoadImage(NormalIndex)
      else if Down
        then Images.LoadImage(DownIndex)
        else Images.LoadImage(HighlightedIndex);
    Canvas.Draw(AbsoluteRect.Left,AbsoluteRect.Top,Images.OutputBmp);
  end;
end;

procedure TSlaveButton.Repaint;
begin
  //this button may need a background
  Master.Repaint(Owner);
end;

{%ENDREGION}
{%REGION TSwitchButton}

constructor TSwitchButton.Create(AOwner: TSlaveControl; AImages: TSwitchButtonImages; ASwitched: Boolean = false; AMaster: TMasterControl = nil);
begin
  inherited Create(AOwner, AImages, AMaster);
  FImages:=AImages;
  FSwitched:=ASwitched;
end;

procedure TSwitchButton.SetSwitched(Value: Boolean);
begin
  FSwitched:=Value;
  Repaint;
end;

procedure TSwitchButton.Click;
begin
  FSwitched:=not FSwitched;
  inherited Click;
end;

procedure TSwitchButton.DoPaint;
begin
  with Images do begin
    if FSwitched then begin
      if not Highlighted
        then Images.LoadImage(SwitchedNormalIndex)
        else if Down
          then Images.LoadImage(SwitchedDownIndex)
          else Images.LoadImage(SwitchedHighlightedIndex);
    end else begin
      if not Highlighted
        then Images.LoadImage(NormalIndex)
        else if Down
          then Images.LoadImage(DownIndex)
          else Images.LoadImage(HighlightedIndex);
    end;
    Canvas.Draw(AbsoluteRect.Left,AbsoluteRect.Top,Images.OutputBmp);
  end;
end;

{%ENDREGION}

end.

