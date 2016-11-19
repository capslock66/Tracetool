object FrmSelectEvent: TFrmSelectEvent
  Left = 508
  Top = 240
  Width = 334
  Height = 353
  Caption = 'Select Event log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    326
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 14
    Width = 82
    Height = 13
    Caption = 'Select Event Log'
  end
  object ScrollBox: TScrollBox
    Left = -1
    Top = 40
    Width = 326
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvSpace
    BevelKind = bkFlat
    TabOrder = 0
  end
  object butOk: TButton
    Left = 16
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open'
    ModalResult = 1
    TabOrder = 1
  end
  object butCancel: TButton
    Left = 238
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
