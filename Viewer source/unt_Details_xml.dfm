inherited Frame_XML: TFrame_XML
  object SynMemo: TSynEdit
    Left = 0
    Top = 0
    Width = 320
    Height = 240
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
    Highlighter = SynXMLSyn1
  end
  object SynXMLSyn1: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeValueAttri.Style = []
    TextAttri.Style = []
    WantBracesParsed = False
    Left = 80
    Top = 120
  end
  object XMLDoc: TXMLDocument
    Active = True
    Left = 168
    Top = 120
    DOMVendorDesc = 'MSXML'
  end
end
