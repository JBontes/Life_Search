object Form34: TForm34
  Left = 0
  Top = 0
  Caption = 'Form34'
  ClientHeight = 341
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 424
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 144
    Top = 8
    Width = 294
    Height = 161
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
  end
  object Button3: TButton
    Left = 208
    Top = 184
    Width = 97
    Height = 25
    Caption = 'Do generation'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 444
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Timer'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 440
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 5
  end
end
