object Form75: TForm75
  Left = 0
  Top = 0
  Caption = 'Life64'
  ClientHeight = 769
  ClientWidth = 1135
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1135
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'TopPanel'
    ShowCaption = False
    TabOrder = 0
    object Button9: TButton
      Left = 360
      Top = 8
      Width = 177
      Height = 25
      Caption = 'Test large file write'
      TabOrder = 0
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 584
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button10'
      TabOrder = 1
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 688
      Top = 8
      Width = 145
      Height = 25
      Caption = 'Test lookup table 4x4->2x2'
      TabOrder = 2
    end
    object Button12: TButton
      Left = 864
      Top = 8
      Width = 177
      Height = 25
      Caption = 'TestBitsetLeadingZeros'
      TabOrder = 3
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 712
    Width = 1135
    Height = 57
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'StatusPanel'
    ShowCaption = False
    TabOrder = 1
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 23
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Panel1'
      ShowCaption = False
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 185
      Top = 0
      Width = 950
      Height = 23
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel2'
      ShowCaption = False
      TabOrder = 1
      DesignSize = (
        950
        23)
      object Splitter2: TSplitter
        Left = 0
        Top = 0
        Width = 5
        Height = 23
        Beveled = True
        ExplicitHeight = 41
      end
      object ScrollBar1: TScrollBar
        Left = 0
        Top = 0
        Width = 933
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        PageSize = 0
        Position = 50
        TabOrder = 0
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 23
      Width = 1135
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'Panel3'
      ShowCaption = False
      TabOrder = 2
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 1135
    Height = 671
    ActivePage = TabSheet2
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Testing'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MainPanel: TPanel
        Left = 281
        Top = 0
        Width = 846
        Height = 640
        Align = alClient
        BevelOuter = bvNone
        Caption = 'MainPanel'
        ShowCaption = False
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 0
          Top = 0
          Width = 5
          Height = 640
          Beveled = True
          ExplicitHeight = 670
        end
        object PaintBox1: TPaintBox
          Left = 5
          Top = 0
          Width = 824
          Height = 640
          Align = alClient
          ExplicitLeft = 400
          ExplicitTop = 264
          ExplicitWidth = 105
          ExplicitHeight = 105
        end
        object ScrollBar2: TScrollBar
          Left = 829
          Top = 0
          Width = 17
          Height = 640
          Align = alRight
          Kind = sbVertical
          PageSize = 0
          Position = 50
          TabOrder = 0
        end
        object ScrollBox1: TScrollBox
          Left = 5
          Top = 0
          Width = 824
          Height = 640
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          TabOrder = 1
          object Image1: TImage
            Left = 0
            Top = 0
            Width = 105
            Height = 105
            AutoSize = True
          end
        end
      end
      object ToolPanel: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 640
        Align = alLeft
        BevelOuter = bvNone
        Caption = 'ToolPanel'
        ShowCaption = False
        TabOrder = 1
        object Label1: TLabel
          Left = 32
          Top = -1
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 32
          Top = 23
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 32
          Top = 47
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 32
          Top = 73
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 32
          Top = 97
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 32
          Top = 121
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 32
          Top = 145
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 32
          Top = 169
          Width = 89
          Height = 24
          Caption = 'Progress: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Button1: TButton
          Left = 32
          Top = 200
          Width = 217
          Height = 25
          Caption = 'count all 2^36 ancestors of 4x4 block'
          TabOrder = 0
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 32
          Top = 248
          Width = 217
          Height = 25
          Caption = 'Something with a lookup table'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 32
          Top = 295
          Width = 217
          Height = 25
          Caption = 'Generate heatmap'
          TabOrder = 2
          OnClick = Button3Click
        end
        object TestMap: TButton
          Left = 88
          Top = 336
          Width = 75
          Height = 25
          Caption = 'TestMap'
          TabOrder = 3
          OnClick = TestMapClick
        end
        object Button4: TButton
          Left = 32
          Top = 376
          Width = 217
          Height = 24
          Caption = 'Eliminate duplicates from CountList'
          TabOrder = 4
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 32
          Top = 424
          Width = 217
          Height = 25
          Caption = 'Add popcount to full count table'
          TabOrder = 5
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 32
          Top = 472
          Width = 217
          Height = 25
          Caption = 'Count all bitsets for 2^36'
          TabOrder = 6
          OnClick = Button6Click
        end
        object BtnCombineFiles: TButton
          Left = 32
          Top = 520
          Width = 217
          Height = 25
          Caption = 'Combine file with 4 tables'
          TabOrder = 7
          OnClick = BtnCombineFilesClick
        end
        object BtnMakeCountTable: TButton
          Left = 32
          Top = 560
          Width = 217
          Height = 25
          Caption = 'Make count table'
          TabOrder = 8
          OnClick = BtnMakeCountTableClick
        end
        object Button7: TButton
          Left = 32
          Top = 600
          Width = 217
          Height = 25
          Caption = 'List ancestors for first 8 '
          TabOrder = 9
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 32
          Top = 640
          Width = 217
          Height = 25
          Caption = 'Count ancestors per pixel 2^36'
          TabOrder = 10
          OnClick = Button8Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Demo: deterministic engine'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label9: TLabel
        Left = 472
        Top = 40
        Width = 71
        Height = 25
        Caption = 'Current'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label10: TLabel
        Left = 832
        Top = 40
        Width = 61
        Height = 25
        Caption = 'Future'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label11: TLabel
        Left = 104
        Top = 40
        Width = 39
        Height = 25
        Caption = 'Past'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object StringGrid2: TStringGrid
        Left = 373
        Top = 72
        Width = 377
        Height = 417
        ColCount = 10
        DefaultColWidth = 36
        DrawingStyle = gdsClassic
        RowCount = 10
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        StyleElements = [seFont, seBorder]
        OnMouseDown = StringGrid3MouseDown
        ColWidths = (
          36
          36
          36
          36
          36
          36
          36
          36
          36
          36)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object StringGrid3: TStringGrid
        Left = 751
        Top = 72
        Width = 377
        Height = 417
        ColCount = 10
        DefaultColWidth = 36
        DrawingStyle = gdsClassic
        RowCount = 10
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        StyleElements = [seFont, seBorder]
        OnMouseDown = StringGrid3MouseDown
        ColWidths = (
          36
          36
          36
          36
          36
          36
          36
          36
          36
          36)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object StringGrid1: TStringGrid
        Left = -1
        Top = 72
        Width = 377
        Height = 417
        ColCount = 10
        DefaultColWidth = 36
        DrawingStyle = gdsClassic
        RowCount = 10
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        StyleElements = [seFont, seBorder]
        OnMouseDown = StringGrid3MouseDown
        ColWidths = (
          36
          36
          36
          36
          36
          36
          36
          36
          36
          36)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object Button13: TButton
        Left = 468
        Top = 9
        Width = 75
        Height = 25
        Caption = 'Init grid'
        TabOrder = 3
        OnClick = Button13Click
      end
      object Button14: TButton
        Left = 580
        Top = 9
        Width = 197
        Height = 25
        Caption = 'Process entailments'
        TabOrder = 4
        OnClick = Button14Click
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 208
    Top = 8
    object File1: TMenuItem
      Caption = 'File'
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
    end
    object Animation1: TMenuItem
      Caption = 'Animation'
    end
    object Screen1: TMenuItem
      Caption = 'Screen'
    end
    object Options1: TMenuItem
      Caption = 'Options'
    end
    object View1: TMenuItem
      Caption = 'View'
    end
    object Help1: TMenuItem
      Caption = 'Help'
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 120
    Top = 393
  end
end
