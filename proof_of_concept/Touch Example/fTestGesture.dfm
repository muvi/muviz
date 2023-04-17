object Form1: TForm1
  Left = 319
  Top = 182
  AutoScroll = False
  Caption = 'Touch/Gesture in D7'
  ClientHeight = 270
  ClientWidth = 607
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cbTouchEnabled: TCheckBox
    Left = 16
    Top = 8
    Width = 121
    Height = 17
    Caption = 'Touch Enabled'
    TabOrder = 0
    OnClick = cbTouchEnabledClick
  end
  object btnNextRect: TButton
    Left = 224
    Top = 5
    Width = 92
    Height = 22
    Caption = 'Select rext >'
    TabOrder = 1
    OnClick = btnNextRectClick
  end
end
