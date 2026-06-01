object FrmAbout: TFrmAbout
  Left = 405
  Top = 299
  BorderStyle = bsDialog
  Caption = 'About TraceTool'
  ClientHeight = 270
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label5: TLabel
    Left = 16
    Top = 130
    Width = 38
    Height = 13
    Caption = 'Author'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelAuthor: TLabel
    Left = 71
    Top = 130
    Width = 66
    Height = 13
    Caption = 'Thierry Parent'
    OnDblClick = LabelAuthorDblClick
  end
  object Label7: TLabel
    Left = 16
    Top = 172
    Width = 53
    Height = 13
    Caption = 'Web Site'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 151
    Top = 171
    Width = 198
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Caption = 'https://github.com/capslock66/Tracetool'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = lblWebPageClick
  end
  object Label12: TLabel
    Left = 16
    Top = 35
    Width = 376
    Height = 13
    Caption = 
      '- A Dot Net, Java, Python, Javascript, C++, ActiveX and Delphi t' +
      'race framework'
  end
  object Label14: TLabel
    Left = 16
    Top = 50
    Width = 377
    Height = 13
    Caption = 
      '- Send traces, objects or dumps using windows message, socket or' +
      ' web socket '
  end
  object Label16: TLabel
    Left = 151
    Top = 187
    Width = 248
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Caption = 'http://www.codeproject.com/csharp/TraceTool.asp'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = lblWebPageClick
  end
  object Label17: TLabel
    Left = 16
    Top = 65
    Width = 134
    Height = 13
    Caption = '- Display OutputDebugString'
  end
  object Label18: TLabel
    Left = 16
    Top = 80
    Width = 67
    Height = 13
    Caption = '- Tail windows'
  end
  object Label6: TLabel
    Left = 16
    Top = 96
    Width = 137
    Height = 13
    Caption = '- Event Log viewer (real time)'
  end
  object LabelVersion: TLabel
    Left = 112
    Top = 8
    Width = 171
    Height = 16
    Caption = 'TraceTool x.x.x Build xxx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 217
    Width = 222
    Height = 13
    Caption = 'See the license.txt for license information (GPL)'
  end
  object Button1: TButton
    Left = 163
    Top = 236
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 16
    Top = 160
    Width = 393
    Height = 3
    TabOrder = 1
  end
end
