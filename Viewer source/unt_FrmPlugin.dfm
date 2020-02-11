object frmPlugin: TfrmPlugin
  Left = 0
  Top = 0
  Width = 461
  Height = 339
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
    Left = 149
    Top = 137
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
  object Label1: TLabel
    Left = 13
    Top = 104
    Width = 30
    Height = 13
    Caption = 'Param'
  end
  object chkLoadAtStartup: TCheckBox
    Left = 13
    Top = 136
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Load at startup'
    TabOrder = 1
    OnClick = chkLoadAtStartupClick
  end
  object butLoadAndStart: TButton
    Left = 13
    Top = 200
    Width = 89
    Height = 25
    Caption = 'Load and start'
    TabOrder = 2
    OnClick = butLoadAndStartClick
  end
  object butUnload: TButton
    Left = 109
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Unload'
    TabOrder = 3
    OnClick = butUnloadClick
  end
  object butStart: TButton
    Left = 13
    Top = 232
    Width = 60
    Height = 25
    Caption = 'Start'
    TabOrder = 4
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 85
    Top = 232
    Width = 60
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = butStopClick
  end
  object butStopAndUnload: TButton
    Left = 197
    Top = 200
    Width = 97
    Height = 25
    Caption = 'Stop and unload'
    TabOrder = 6
    OnClick = butStopAndUnloadClick
  end
  object butRemove: TButton
    Left = 13
    Top = 280
    Width = 145
    Height = 25
    Caption = 'Remove from configuration'
    TabOrder = 7
    OnClick = butRemoveClick
  end
  object EditParam: TEdit
    Left = 104
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 0
  end
end
