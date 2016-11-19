object FrmNicePreview: TFrmNicePreview
  Left = 304
  Top = 179
  Width = 836
  Height = 650
  Caption = 'NicePreview Demo - priyatna.org'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    828
    616)
  PixelsPerInch = 96
  TextHeight = 13
  object NicePreview1: TNicePreview
    Left = 8
    Top = 64
    Width = 812
    Height = 521
    Title = 'Printing ...'
    MarginLeft = 96
    MarginTop = 96
    MarginRight = 96
    MarginBottom = 96
    OnChange = NicePreview1Change
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Default'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 32
    Width = 74
    Height = 25
    Caption = 'Zoom In'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 168
    Top = 32
    Width = 74
    Height = 25
    Caption = 'Zoom Out'
    TabOrder = 2
    OnClick = Button3Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 597
    Width = 828
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object Button4: TButton
    Left = 424
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Fit To Width'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 504
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Whole Page'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 584
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Two Pages'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 664
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Four Pages'
    TabOrder = 5
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 344
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Actual Size'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button10: TButton
    Left = 248
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Drag'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = Button10Click
  end
  object butRenderGrids: TButton
    Left = 8
    Top = 0
    Width = 75
    Height = 25
    Caption = 'RenderGrids'
    TabOrder = 11
    OnClick = butRenderGridsClick
  end
end
