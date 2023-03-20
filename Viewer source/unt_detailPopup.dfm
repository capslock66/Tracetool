object DetailPopupForm: TDetailPopupForm
  Left = 0
  Top = 0
  Caption = 'Detail'
  ClientHeight = 369
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object SynMemo: TSynEdit
    Left = 0
    Top = 29
    Width = 386
    Height = 340
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
    ExplicitLeft = 8
    ExplicitTop = 136
    ExplicitWidth = 217
    ExplicitHeight = 136
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 386
    Height = 29
    Align = alTop
    TabOrder = 1
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
  end
  object SynJSONSyn: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 120
    Top = 32
  end
  object SynXMLSyn: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeValueAttri.Style = []
    TextAttri.Style = []
    WantBracesParsed = False
    Left = 200
    Top = 32
  end
  object XMLDocument: TXMLDocument
    Active = True
    Left = 200
    Top = 80
    DOMVendorDesc = 'MSXML'
  end
end
