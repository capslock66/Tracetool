object FrameMemo: TFrameMemo
  Left = 0
  Top = 0
  Width = 290
  Height = 221
  TabOrder = 0
  object SynMemo: TSynEdit
    Left = 0
    Top = 25
    Width = 290
    Height = 196
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
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.Visible = False
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Visible = False
        Width = 1
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Visible = False
        Width = 3
      end>
    SelectedColor.Alpha = 0.400000005960464500
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 290
    Height = 25
    Align = alTop
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = PanelTopResize
    object ShowAsTextButton: TBitBtn
      Left = 0
      Top = 0
      Width = 42
      Height = 25
      Caption = 'Text'
      TabOrder = 0
      OnClick = ShowAsTextButtonClick
    end
    object ShowAsXmlButton: TBitBtn
      Left = 48
      Top = 0
      Width = 45
      Height = 25
      Caption = 'Xml'
      TabOrder = 1
      OnClick = ShowAsXmlButtonClick
    end
    object ShowAsJSonButton: TBitBtn
      Left = 99
      Top = 0
      Width = 45
      Height = 25
      Caption = 'Json'
      TabOrder = 2
      OnClick = ShowAsJSonButtonClick
    end
    object FormatButton: TBitBtn
      Left = 150
      Top = 0
      Width = 50
      Height = 25
      Caption = 'Format'
      TabOrder = 3
      OnClick = FormatButtonClick
    end
    object ShowPopupButton: TBitBtn
      Left = 203
      Top = 0
      Width = 85
      Height = 25
      Caption = 'Show in Popup'
      TabOrder = 4
      OnClick = ShowPopupButtonClick
    end
  end
  object SynJSONSyn: TSynJSONSyn
    Left = 24
    Top = 68
  end
  object SynXMLSyn: TSynXMLSyn
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
