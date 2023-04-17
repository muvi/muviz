unit DiagnoseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, TVSPServerGroups, TVSPGroups, TVSPServerSources, TVSPBasicSources,
  TVSPSources, TVSPType, TVSPServerPermissions, TVSPPermissions;

type

  { TDiagnoseForm }

  TDiagnoseForm = class(TForm)
    PermissionLB: TListBox;
    PermissionLbl: TLabel;
    SourceSocketCountLbl: TLabel;
    ValueLbl: TLabel;
    ValueLB: TListBox;
    SourceInfoGB: TGroupBox;
    GroupInfoGB: TGroupBox;
    SourceRefCountLbl: TLabel;
    ObjectLB: TListBox;
    SourceLB: TListBox;
    SourceLbl: TLabel;
    RefreshBtn: TButton;
    ObjectLbl: TLabel;
    Timer: TTimer;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ObjectLBClick(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure SourceLBClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FSources    : array of TTVSPSrcID;
    FGroups     : array of TGUID;
  end;

var
  DiagnoseForm: TDiagnoseForm;

implementation

{$R *.lfm}

{ TDiagnoseForm }

procedure TDiagnoseForm.FormHide(Sender: TObject);
begin
  Timer.Enabled:=false;
  SetLength(FSources, 0);
end;

procedure TDiagnoseForm.FormShow(Sender: TObject);
begin
  Timer.Enabled:=true;
end;

procedure TDiagnoseForm.ObjectLBClick(Sender: TObject);
var
  AGroup   : TTVSPBasicGroup;
  AID      : TGUID;
  AIterator: TObject;
  AValue   : TTVSPBasicValue;
begin
  if ObjectLB.ItemIndex >= 0 then begin
    AID:=FGroups[ObjectLB.ItemIndex];
    AGroup:=Groups[AID];
    if AGroup <> nil then begin
      ValueLBl.Caption:=GUIDToString(AGroup.Key);

      //show values
      AIterator:=nil;
      AValue:=AGroup.IterateValues(AIterator);
      ValueLB.Items.Clear;
      while AValue <> nil do begin
        ValueLB.Items.Add(AValue.Key + ' [Size: ' + IntToStr(TVSPParamValueSize(AValue.&Type)) + ', Sources: ' + IntToStr(TVSPParamSourceCount(AValue.&Type)) + ']');

        AValue:=AGroup.IterateValues(AIterator);
      end;
    end else ShowMessage('Item does not exist anymore.');
  end;
end;

procedure TDiagnoseForm.RefreshBtnClick(Sender: TObject);
var
  AGroups        : TTVSPBasicGroups;
  AIterator      : TObject;
  I, ACount      : Integer;
  AGroup         : TTVSPBasicGroup;

  ASources       : TTVSPSources;
  ASource        : TTVSPSource;

  APermissions   : TTVSPServerPermissions;
  APermission    : TTVSPPermissionValue;
begin
  //show groups
  AGroups:=Groups;
  AIterator:=AGroups.StartIterate;
  AGroup:=AGroups.Iterate(AIterator);
  ACount:=0;
  ObjectLB.Items.Clear;
  SetLength(FGroups, 0);
  while AGroup<>nil do begin
    SetLength(FGroups, Length(FGroups) + 1);
    FGroups[Length(FGroups) - 1]:=AGroup.Key;

    ObjectLB.Items.Add(GUIDToString(AGroup.Key));
    AGroup:=AGroups.Iterate(AIterator);
    Inc(ACount);
  end;
  ObjectLbl.Caption:='Objects: ' + IntToStr(ACount);

  //show sources
  ASources:=Sources;
  AIterator:=ASources.StartIterate;
  ASource:=ASources.Iterate(AIterator);
  ACount:=0;
  SourceLB.Items.Clear;
  SetLength(FSources, 0);
  while ASource<>nil do begin
    SetLength(FSources, Length(FSources) + 1);
    FSources[Length(FSources) - 1]:=ASource.ID;

    SourceLB.Items.Add(GUIDToString(ASource.ID.ID) + ' ' + IntToStr(ASource.ID.Size) + ' Bytes');
    ASource:=ASources.Iterate(AIterator);
    Inc(ACount);
  end;
  SourceLbl.Caption:='Sources: ' + IntToStr(ACount);

  //show permissions
  APermissions:=Permissions;
  AIterator:=APermissions.StartIterate;
  APermission:=APermissions.Iterate(AIterator);
  ACount:=0;
  PermissionLB.Items.Clear;
  while APermission <> nil do begin
    PermissionLB.Items.Add(IntToStr(APermission.Permission) + ' of ' + GUIDToString(APermission.Group));
    APermission:=APermissions.Iterate(AIterator);
    Inc(ACount);
  end;
  PermissionLbl.Caption:='Permissions: ' + IntToStr(ACount);
end;

procedure TDiagnoseForm.SourceLBClick(Sender: TObject);
var
  ASource: TTVSPSource;
  AID    : TTVSPSrcID;
begin
  if SourceLB.ItemIndex >= 0 then begin
    AID:=FSources[SourceLB.ItemIndex];
    ASource:=Sources[AID.Size, AID.ID];
    if ASource <> nil then begin
      Assert(ASource is TTVSPServerSource);
      SourceRefCountLbl.Caption:='References: ' + IntToStr(TTVSPServerSource(ASource).KnownSocketsCount);
      SourceSocketCountLbl.Caption:='Known by Sockets: ' + IntToStr(ASource.RefCount);
    end else ShowMessage('Item does not exist anymore.');
  end;
end;

procedure TDiagnoseForm.TimerTimer(Sender: TObject);
begin

end;

end.

