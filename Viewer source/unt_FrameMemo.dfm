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
    object LabelSelect: TLabel
      Left = 198
      Top = 5
      Width = 3
      Height = 15
    end
    object WordWrapButton: TSpeedButton
      Left = 198
      Top = 0
      Width = 25
      Height = 25
      AllowAllUp = True
      GroupIndex = 1
      ImageIndex = 44
      Images = Frm_Tool.vilActions16
      OnClick = WordWrapButtonClick
    end
    object FormatButton: TBitBtn
      Left = 51
      Top = 0
      Width = 50
      Height = 25
      Caption = 'Format'
      TabOrder = 0
      OnClick = FormatButtonClick
    end
    object ShowPopupButton: TBitBtn
      Left = 107
      Top = 0
      Width = 85
      Height = 25
      Caption = 'Show in Popup'
      TabOrder = 1
      OnClick = ShowPopupButtonClick
    end
    object ShowAsButton: TButton
      Left = 0
      Top = 0
      Width = 45
      Height = 25
      Caption = 'Text'
      DropDownMenu = PopupShowAs
      Style = bsSplitButton
      TabOrder = 2
      OnClick = ShowAsButtonClick
    end
  end
  object SynJSONSyn: TSynJSONSyn
    Left = 24
    Top = 100
  end
  object SynXMLSyn: TSynXMLSyn
    AttributeValueAttri.Style = []
    TextAttri.Style = []
    WantBracesParsed = False
    Left = 96
    Top = 100
  end
  object XMLDocument: TXMLDocument
    Active = True
    Left = 176
    Top = 100
    DOMVendorDesc = 'MSXML'
  end
  object PopupShowAs: TPopupMenu
    Left = 96
    Top = 32
    object ShowAsText: TMenuItem
      Caption = 'Show As Text'
      OnClick = ShowAsTextClick
    end
    object ShowAsXml: TMenuItem
      Caption = 'Show As Xml'
      OnClick = ShowAsXmlClick
    end
    object ShowAsJson: TMenuItem
      Caption = 'Show As Json'
      OnClick = ShowAsJsonClick
    end
  end
end
