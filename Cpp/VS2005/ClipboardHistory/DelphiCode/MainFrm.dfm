object MainForm: TMainForm
  Left = 434
  Top = 288
  Width = 494
  Height = 453
  Caption = 'Presse papier'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 98
    Height = 13
    Caption = 'Formats disponibles: '
  end
  object Label2: TLabel
    Left = 8
    Top = 133
    Width = 70
    Height = 13
    Caption = 'Objet contenu:'
  end
  object lbFormats: TListBox
    Left = 8
    Top = 24
    Width = 468
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbFormatsClick
  end
  object pnlView: TPanel
    Left = 8
    Top = 152
    Width = 471
    Height = 266
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
  end
end
