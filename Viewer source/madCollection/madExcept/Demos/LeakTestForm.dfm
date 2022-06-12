object TestLeakForm: TTestLeakForm
  Left = 286
  Top = 123
  Caption = 'leak testing...'
  ClientHeight = 217
  ClientWidth = 200
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
  object LeakDelphiMemButton: TButton
    Left = 16
    Top = 16
    Width = 169
    Height = 25
    Caption = 'leak delphi memory'
    TabOrder = 0
    OnClick = LeakDelphiMemButtonClick
  end
  object LeakOsMemButton: TButton
    Left = 16
    Top = 48
    Width = 169
    Height = 25
    Caption = 'leak os memory'
    TabOrder = 1
    OnClick = LeakOsMemButtonClick
  end
  object LeakKernelHandlesButton: TButton
    Left = 16
    Top = 80
    Width = 169
    Height = 25
    Caption = 'leak kernel handles'
    TabOrder = 2
    OnClick = LeakKernelHandlesButtonClick
  end
  object LeakGdiHandlesButton: TButton
    Left = 16
    Top = 112
    Width = 169
    Height = 25
    Caption = 'leak gdi handles'
    TabOrder = 3
    OnClick = LeakGdiHandlesButtonClick
  end
  object LeakUserHandlesButton: TButton
    Left = 16
    Top = 144
    Width = 169
    Height = 25
    Caption = 'leak user handles'
    TabOrder = 4
    OnClick = LeakUserHandlesButtonClick
  end
  object LeakOtherStuffButton: TButton
    Left = 16
    Top = 176
    Width = 169
    Height = 25
    Caption = 'leak some other stuff'
    TabOrder = 5
    OnClick = LeakOtherStuffButtonClick
  end
end
