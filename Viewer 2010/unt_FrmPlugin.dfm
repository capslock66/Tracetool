object frmPlugin: TfrmPlugin
  Left = 0
  Top = 0
  Width = 457
  Height = 299
  TabOrder = 0
  object LabelPlugName: TLabel
    Left = 16
    Top = 16
    Width = 112
    Height = 16
    Caption = 'LabelPlugName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelStatus: TLabel
    Left = 16
    Top = 131
    Width = 56
    Height = 13
    Caption = 'LabelStatus'
  end
  object LabelTitleFileName: TLabel
    Left = 16
    Top = 70
    Width = 43
    Height = 13
    Caption = 'FileName'
  end
  object Label2: TLabel
    Left = 16
    Top = 43
    Width = 52
    Height = 13
    Caption = 'ClassName'
  end
  object LabelFileName: TLabel
    Left = 107
    Top = 70
    Width = 68
    Height = 13
    Caption = 'LabelFileName'
  end
  object LabelClassName: TLabel
    Left = 107
    Top = 43
    Width = 77
    Height = 13
    Caption = 'LabelClassName'
  end
  object chkLoadAtStartup: TCheckBox
    Left = 14
    Top = 96
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Load at startup'
    TabOrder = 0
    OnClick = chkLoadAtStartupClick
  end
  object butLoadAndStart: TButton
    Left = 16
    Top = 160
    Width = 89
    Height = 25
    Caption = 'Load and start'
    TabOrder = 1
    OnClick = butLoadAndStartClick
  end
  object butUnload: TButton
    Left = 112
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Unload'
    TabOrder = 2
    OnClick = butUnloadClick
  end
  object butStart: TButton
    Left = 16
    Top = 192
    Width = 60
    Height = 25
    Caption = 'Start'
    TabOrder = 3
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 88
    Top = 192
    Width = 60
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = butStopClick
  end
  object butStopAndUnload: TButton
    Left = 200
    Top = 160
    Width = 97
    Height = 25
    Caption = 'Stop and unload'
    TabOrder = 5
    OnClick = butStopAndUnloadClick
  end
  object butRemove: TButton
    Left = 16
    Top = 240
    Width = 145
    Height = 25
    Caption = 'Remove from configuration'
    TabOrder = 6
    OnClick = butRemoveClick
  end
end
