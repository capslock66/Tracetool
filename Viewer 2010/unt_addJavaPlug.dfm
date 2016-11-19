object FrmAddJavaPlugin: TFrmAddJavaPlugin
  Left = 532
  Top = 389
  Caption = 'Java Plugin'
  ClientHeight = 122
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    400
    122)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 16
    Width = 277
    Height = 13
    Caption = 'Enter Plugin class name (must inherit from tracetool.IPlugin)'
  end
  object EditClassName: TEdit
    Left = 16
    Top = 40
    Width = 305
    Height = 21
    TabOrder = 0
    Text = 'pluginTest.JavaTestPlug'
  end
  object butOk: TButton
    Left = 16
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object butCancel: TButton
    Left = 311
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object butCheck: TButton
    Left = 328
    Top = 39
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Check'
    TabOrder = 3
    OnClick = butCheckClick
  end
end
