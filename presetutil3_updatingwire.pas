unit PresetUtil3_UpdatingWire;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdParamTypes, PresetUtil3_Wires, PresetUtil3_Path;

type
  TUpdateEvent          = procedure of object;
  TCheckLegalEvent      = function (ANewValue: ISidedParamPath): Boolean of object;
  TWireUpdater          = class;

  EIllegalUpdate        = class (Exception)
  end;

  TUpdaterSide          = class
  strict private
    FPath        : ISidedParamPath;
    FOnChange    : TUpdateEvent;
    FOnCheckLegal: TCheckLegalEvent;
    FUpdater     : TWireUpdater;
    procedure SetPath(AValue: ISidedParamPath);
  public
    constructor Create(AUpdater: TWireUpdater; AOnChange: TUpdateEvent; AOnCheckLegal: TCheckLegalEvent);
    destructor Destroy; override;
    property Path: ISidedParamPath read FPath write SetPath;
    property Updater: TWireUpdater read FUpdater;
  end;

  TWireUpdater          = class
  strict private
    FFirstSide: TUpdaterSide;
    FLastSide : TUpdaterSide;
    FDoEvents : Boolean;
    function DoCheckLegalFirstSide(ANewValue: ISidedParamPath): Boolean;
    function DoCheckLegalLastSide(ANewValue: ISidedParamPath): Boolean;
  strict protected
    function CheckLegalPathCombo(AFirstSide, ALastSide: ISidedParamPath): Boolean; virtual; abstract;
    procedure FirstSideChanged; virtual; abstract;
    procedure LastSideChanged; virtual; abstract;
  protected
    property DoEvents: Boolean read FDoEvents write FDoEvents;
  public
    constructor Create;
    destructor Destroy; override;
    //deletes the wire and pointer param
    procedure Delete; virtual; abstract;
    property FirstSide: TUpdaterSide read FFirstSide;
    property LastSide: TUpdaterSide read FLastSide;
  end;

  TUpdatablePWContainer = class (TBasicPWContainer)
  strict private
    FUpdater: TWireUpdater;
  public
    constructor Create(AConfig: TWireConfiguration; AUpdater: TWireUpdater);
    property Updater: TWireUpdater read FUpdater;
  end;

  TPWContainer          = TUpdatablePWContainer;

implementation

{%REGION TUpdaterSide}

constructor TUpdaterSide.Create(AUpdater: TWireUpdater; AOnChange: TUpdateEvent; AOnCheckLegal: TCheckLegalEvent);
begin
  inherited Create;
  Assert(AOnChange <> nil);
  FOnChange:=AOnChange;
  Assert(AOnCheckLegal <> nil);
  FOnCheckLegal:=AOnCheckLegal;
  FUpdater:=AUpdater;
  FPath:=nil;
end;

destructor TUpdaterSide.Destroy;
begin
  FPath:=nil;
  inherited Destroy;
end;

procedure TUpdaterSide.SetPath(AValue: ISidedParamPath);
begin
  Assert(AValue <> nil);
  if FUpdater.DoEvents then begin
    if FOnCheckLegal(AValue) then begin
      FPath:=AValue;
      FOnChange;
    end;
  end else FPath:=AValue;
end;

{%ENDREGION}
{%REGION TWireUpdater}

constructor TWireUpdater.Create;
begin
  inherited Create;
  FDoEvents:=true;
  FFirstSide:=TUpdaterSide.Create(Self, @FirstSideChanged, @DoCheckLegalFirstSide);
  FLastSide:=TUpdaterSide.Create(Self, @LastSideChanged, @DoCheckLegalLastSide);
end;

destructor TWireUpdater.Destroy;
begin
  FFirstSide.Destroy;
  FLastSide.Destroy;
  inherited Destroy;
end;

function TWireUpdater.DoCheckLegalFirstSide(ANewValue: ISidedParamPath): Boolean;
begin
  Result:=CheckLegalPathCombo(ANewValue, FLastSide.Path);
end;

function TWireUpdater.DoCheckLegalLastSide(ANewValue: ISidedParamPath): Boolean;
begin
  Result:=CheckLegalPathCombo(FFirstSide.Path, ANewValue);
end;

{%ENDREGION}
{%REGION TUpdatablePWContainer}

constructor TUpdatablePWContainer.Create(AConfig: TWireConfiguration; AUpdater: TWireUpdater);
begin
  inherited Create(AConfig);
  FUpdater:=AUpdater;
end;

{%ENDREGION}

end.

