object frmPlugin: TfrmPlugin
  Left = 0
  Top = 0
  Width = 461
  Height = 339
  TabOrder = 0
  DesignSize = (
    461
    339)
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
    Top = 202
    Width = 56
    Height = 13
    Caption = 'LabelStatus'
  end
  object Label2: TLabel
    Left = 13
    Top = 65
    Width = 45
    Height = 13
    Caption = 'Plug type'
  end
  object LabelPlugType: TLabel
    Left = 85
    Top = 65
    Width = 45
    Height = 13
    Caption = 'Plug type'
  end
  object Label1: TLabel
    Left = 13
    Top = 88
    Width = 30
    Height = 13
    Caption = 'Param'
  end
  object chkLoadAtStartup: TCheckBox
    Left = 13
    Top = 201
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Load at startup'
    TabOrder = 1
    OnClick = chkLoadAtStartupClick
  end
  object butLoadAndStart: TButton
    Left = 13
    Top = 224
    Width = 89
    Height = 25
    Caption = 'Load and start'
    TabOrder = 2
    OnClick = butLoadAndStartClick
  end
  object butUnload: TButton
    Left = 109
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Unload'
    TabOrder = 3
    OnClick = butUnloadClick
  end
  object butStart: TButton
    Left = 13
    Top = 256
    Width = 60
    Height = 25
    Caption = 'Start'
    TabOrder = 4
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 85
    Top = 256
    Width = 60
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = butStopClick
  end
  object butStopAndUnload: TButton
    Left = 197
    Top = 224
    Width = 97
    Height = 25
    Caption = 'Stop and unload'
    TabOrder = 6
    OnClick = butStopAndUnloadClick
  end
  object butRemove: TButton
    Left = 13
    Top = 304
    Width = 145
    Height = 25
    Caption = 'Remove from configuration'
    TabOrder = 7
    OnClick = butRemoveClick
  end
  object MemoParam: TMemo
    Left = 13
    Top = 107
    Width = 425
    Height = 80
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object EditFileName: TEdit
    Left = 13
    Top = 38
    Width = 425
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    Text = 'EditFileName'
    OnKeyPress = EditFileNameKeyPress
  end
end
