object FrmSelectEvent: TFrmSelectEvent
  Left = 508
  Top = 240
  Caption = 'Select Event log'
  ClientHeight = 420
  ClientWidth = 353
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
    353
    420)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 82
    Height = 13
    Caption = 'Select Event Log'
  end
  object ScrollBox: TScrollBox
    Left = 8
    Top = 33
    Width = 337
    Height = 351
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvSpace
    BevelKind = bkFlat
    Color = 16117479
    ParentColor = False
    TabOrder = 0
  end
  object butOk: TButton
    Left = 16
    Top = 390
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open'
    ModalResult = 1
    TabOrder = 1
  end
  object butCancel: TButton
    Left = 273
    Top = 390
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
