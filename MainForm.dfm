object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 507
  ClientWidth = 883
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 883
    Height = 507
    Align = alClient
    ExplicitWidth = 673
    ExplicitHeight = 441
  end
  object Button1: TButton
    Left = 400
    Top = 416
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 400
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 160
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
  end
  object Timer1: TTimer
    Interval = 100
    Left = 432
    Top = 80
  end
end
