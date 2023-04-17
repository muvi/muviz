unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, lNetComponents, TVSPClient, TVSPConst, TestValues,
  TVSPIndexedValues, StdParamTypes, VisType2, TVSPType, TVSPClientSources,
  TVSPSources, TVSPMsg;

type

  { TMainForm }

  TMainForm = class(TForm)
    StringEdit: TEdit;
    IntegerEdit: TSpinEdit;
    SubscibeBtn: TButton;
    CallLbl: TLabel;
    ParamNameEdit: TLabeledEdit;
    ParamTypePC: TPageControl;
    GroupIDEdit: TLabeledEdit;
    SendChangeBtn: TButton;
    ListBox: TListBox;
    CallTS: TTabSheet;
    IntegerTS: TTabSheet;
    StringTS: TTabSheet;
    UpdateListBtn: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure SendChangeBtnClick(Sender: TObject);
    procedure SubscibeBtnClick(Sender: TObject);
    procedure UpdateListBtnClick(Sender: TObject);
    procedure NetReceiveList(AList: array of TTVSPListEntry);
    procedure NetSubscribe(AGroup, AParent: TGUID);
    procedure ValueChanged(Sender: TTVSPIndexedValue; ANewValue: Pointer);
  private
    FNet   : TTVSPClient;
    FValues: TValueIndexMap;
    FSource: ITVSPSource;
    procedure NewInt(out ADest);
    procedure NewCall(out ADest);
    procedure NewString(out ADest);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FNet.Destroy;
  FValues.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  AHost: string;
begin
  AHost:='localhost';
  InputQuery('Enter Host', 'Type the hostname', AHost);
  FNet:=TTVSPClient.Create(Self, AHost, DEFAULTPORT);
  FValues:=TValueIndexMap.Create(FNet);
  FValues.OnChange:=@ValueChanged;
  FNet.OnCreateIndex:=@FValues.GetItem;
end;

procedure TMainForm.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if ListBox.ItemIndex >= 0
    then GroupIDEdit.Text:=ListBox.Items[ListBox.ItemIndex];
end;

procedure TMainForm.NewInt(out ADest);
begin
  TVInteger(ADest):=IntegerEdit.Value;
end;

procedure TMainForm.NewCall(out ADest);
begin
  //do nothing
end;

procedure TMainForm.NewString(out ADest);
begin
  TTVSPSrcID(ADest):=FSource.ID;
  FSource._ValRelease;
  FSource:=nil;
end;

procedure TMainForm.SendChangeBtnClick(Sender: TObject);
var
  AType      : TPParamType;
  ANewValue  : TTVSPNewValueGetter;
  AStr, AStr2: PChar;
  I          : Integer;
  ASrcID     : TTVSPSrcID;
begin
  case ParamTypePC.PageIndex of
    0: begin
        AType:=vCall;
        ANewValue:=@NewCall;
      end;
    1: begin
        AType:=vInteger;
        ANewValue:=@NewInt;
      end;
    2: begin
        ASrcID.Size:=Length(StringEdit.Text);
        CreateGUID(ASrcID.ID);
        //ASrcID.PartOffset:=0;
        //ASrcID.PartSize:=ASrcID.Size;
        AType:=vString;
        ANewValue:=@NewString;

        GetMem(AStr, ASrcID.Size);
        AStr2:=AStr;
        for I:=1 to ASrcID.Size do begin
          AStr2^:=StringEdit.Text[I];
          Inc(AStr2);
        end;
        FSource:=FNet.Sources[ASrcID];
        FSource._ValAddRef;
        //do not delete the source
        FSource._AddRef;
        FSource.&Set(0, ASrcID.Size, AStr^);
        FreeMem(AStr, ASrcID.Size);
    end
    else raise Exception.Create('wrong page');
  end;

  FValues[StringToGUID(GroupIDEdit.Text), AType, ParamNameEdit.Text].SendChange(ANewValue);
end;

procedure TMainForm.SubscibeBtnClick(Sender: TObject);
begin
  FNet.Subscribe(StringToGUID(GroupIDEdit.Text));
end;

procedure TMainForm.UpdateListBtnClick(Sender: TObject);
begin
  FNet.OnReceiveList:=@NetReceiveList;
  FNet.UpdateList;
end;

procedure TMainForm.NetSubscribe(AGroup, AParent: TGUID);
begin
  ShowMessage('Subscribed to ' + GUIDToString(AGroup) + ' (extends ' + GUIDToString(AParent) + ')');
end;

procedure TMainForm.NetReceiveList(AList: array of TTVSPListEntry);
var
  I: Integer;
begin
  ListBox.Clear;
  for I:=0 to Length(AList)-1
    do ListBox.Items.Add(GUIDToString(AList[I]));
end;

procedure TMainForm.ValueChanged(Sender: TTVSPIndexedValue; ANewValue: Pointer);
var
  AStr   : string;
  AInt   : ^TVInteger;
  ASrcID : ^TTVSPSrcID;
  ASrcStr: PChar;
begin
  AStr:='Value Changed' + #$D#$A
      + 'Group: ' + GUIDToString(Sender.Group) + #$D#$A
      + 'Name: ' + Sender.Name + #$D#$A
      + 'Type: ';
  case Sender.&Type of
    vCall   : AStr += 'Call';
    vInteger: AStr += 'Integer';
    vFloat  : AStr += 'Float';
    vColor  : AStr += 'Color';
    vBoolean: AStr += 'Boolean';
    vBuffer : AStr += 'Buffer';
    vString : AStr += 'String';
    vPreset : AStr += 'Preset';
    vPointer: AStr += 'Pointer';
    else AStr += 'Unknown';
  end;
  AStr += #$D#$A + 'Value: ';
  case Sender.&Type of
    vCall   : AStr += '_';
    vInteger: begin
        AInt:=ANewValue;
        AStr += IntToStr(AInt^);
      end;
    vString : begin
        ASrcID:=ANewValue;
        with ASrcID^ do begin
          GetMem(ASrcStr, Size + 1);
          TTVSPClientSource(FNet.Sources.NetItems[Size, ID]).Get(0, Size, ASrcStr^);
          ASrcStr[Size] := #0;
          AStr += ASrcStr;
          FreeMem(ASrcStr, Size + 1);
        end;
      end
    else AStr += 'Unknown';
  end;
  ShowMessage(AStr);
end;

end.

