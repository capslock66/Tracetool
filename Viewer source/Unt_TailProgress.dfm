object FrmTailProgress: TFrmTailProgress
  Left = 394
  Top = 252
  Caption = 'Progress...'
  ClientHeight = 121
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    480
    121)
  TextHeight = 13
  object LabelCaptionLinesRead: TLabel
    Left = 16
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Lines read'
  end
  object LabelLinesRead: TLabel
    Left = 80
    Top = 16
    Width = 77
    Height = 13
    Caption = 'LabelLinesRead'
  end
  object ProgressBar: TProgressBar
    Left = 16
    Top = 48
    Width = 453
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 449
  end
  object butCancel: TButton
    Left = 16
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = butCancelClick
  end
end
