object FrameMemo: TFrameMemo
  Left = 0
  Top = 0
  Width = 360
  Height = 240
  TabOrder = 0
  object SynMemo: TSynEdit
    Left = 0
    Top = 29
    Width = 360
    Height = 211
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    BorderStyle = bsNone
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Gutter.Width = 0
    ExplicitTop = 32
    ExplicitWidth = 346
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 29
    Align = alTop
    TabOrder = 1
    ExplicitLeft = -66
    ExplicitWidth = 386
    object ShowAsTextButton: TBitBtn
      Left = 0
      Top = 1
      Width = 50
      Height = 25
      Caption = 'Text'
      TabOrder = 0
      OnClick = ShowAsTextButtonClick
    end
    object ShowAsXmlButton: TBitBtn
      Left = 56
      Top = 1
      Width = 50
      Height = 25
      Caption = 'Xml'
      TabOrder = 1
      OnClick = ShowAsXmlButtonClick
    end
    object ShowAsJSonButton: TBitBtn
      Left = 112
      Top = 1
      Width = 50
      Height = 25
      Caption = 'Json'
      TabOrder = 2
      OnClick = ShowAsJSonButtonClick
    end
    object FormatButton: TBitBtn
      Left = 179
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Format'
      TabOrder = 3
      OnClick = FormatButtonClick
    end
    object ShowPopupButton: TBitBtn
      Left = 260
      Top = 1
      Width = 85
      Height = 25
      Caption = 'Show in Popup'
      TabOrder = 4
      OnClick = ShowPopupButtonClick
    end
  end
  object SynJSONSyn: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 24
    Top = 68
  end
  object SynXMLSyn: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeValueAttri.Style = []
    TextAttri.Style = []
    WantBracesParsed = False
    Left = 96
    Top = 68
  end
  object XMLDocument: TXMLDocument
    Active = True
    Left = 176
    Top = 68
    DOMVendorDesc = 'MSXML'
  end
end
