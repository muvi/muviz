unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, lNetComponents, ArtNet, IPTools;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    UniverseEdit: TSpinEdit;
    ChannelEdit: TSpinEdit;
    ValueTB: TTrackBar;
    UDPListener: TLUDPComponent;
    UDP: TLUDPComponent;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ArtNetDeviceAdded(ADevice: TArtNetDevice);
    procedure ArtNetDeviceRemoved(ADevice: TArtNetDevice);
    procedure ValueTBChange(Sender: TObject);
  private
    FArtNet: TArtNet;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FArtNet.Poll;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  ShowMessage(IPToStr(LocalIP));
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  LocalIP:=ParseIPv4(Dialogs.InputBox('Set Local IP', 'Type your local IP Address here:', IPToStr(LocalIP)));
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FArtNet.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FArtNet:=TArtNet.Create(UDP, UDPListener);
  FArtNet.OnDeviceAdded:=@ArtNetDeviceAdded;
  FArtNet.OnDeviceRemoved:=@ArtNetDeviceRemoved;
end;

procedure TMainForm.ArtNetDeviceAdded(ADevice: TArtNetDevice);
begin
  ShowMessage('Added: ' + ADevice.ShortName + '(' + IPToStr(ADevice.IP) + ')');
end;

procedure TMainForm.ArtNetDeviceRemoved(ADevice: TArtNetDevice);
begin
  ShowMessage('Removed: ' + ADevice.ShortName + '(' + IPToStr(ADevice.IP) + ')');
end;

procedure TMainForm.ValueTBChange(Sender: TObject);
//var
 // ADMX: TDMXArray;
begin
  {
  FillChar(ADMX, SizeOf(ADMX), 0);
  ADMX[ChannelEdit.Value]:=ValueTB.Position;
  }
  FArtNet.Universes[UniverseEdit.Value][ChannelEdit.Value]:=ValueTB.Position;
  FArtNet.DMXFrame;
  //FArtNet.SendDMX(UniverseEdit.Value, ADMX, 1, 200);
end;

end.

