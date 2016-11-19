object Form1: TForm1
  Left = 468
  Top = 249
  Width = 214
  Height = 229
  Caption = 'Variant demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SafeArrayCreate1: TButton
    Left = 8
    Top = 64
    Width = 185
    Height = 25
    Caption = 'SafeArrayCreate (VT_VARIANT)'
    TabOrder = 1
    OnClick = SafeArrayCreate1Click
  end
  object butArrayCreate: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = 'Array copy'
    TabOrder = 0
    OnClick = butArrayCreateClick
  end
  object butSA_Integer: TButton
    Left = 8
    Top = 102
    Width = 185
    Height = 25
    Caption = 'SafeArrayCreate (varInteger)'
    TabOrder = 2
    OnClick = butSA_IntegerClick
  end
  object butSA_String: TButton
    Left = 8
    Top = 140
    Width = 185
    Height = 25
    Caption = 'SafeArrayCreate (VT_BSTR)'
    TabOrder = 3
    OnClick = butSA_StringClick
  end
end
