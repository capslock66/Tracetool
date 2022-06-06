object TestDebugMemForm: TTestDebugMemForm
  Left = 505
  Top = 130
  Width = 192
  Height = 155
  Caption = 'debug mm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object OverrunButton: TButton
    Left = 16
    Top = 16
    Width = 153
    Height = 25
    Caption = 'overrun buffer'
    TabOrder = 0
    OnClick = OverrunButtonClick
  end
  object UnderrunButton: TButton
    Left = 16
    Top = 48
    Width = 153
    Height = 25
    Caption = 'underrun buffer'
    TabOrder = 1
    OnClick = UnderrunButtonClick
  end
  object AccessFreedMemButton: TButton
    Left = 16
    Top = 80
    Width = 153
    Height = 25
    Caption = 'access freed memory'
    TabOrder = 2
    OnClick = AccessFreedMemButtonClick
  end
end
