object Frm_AddLine: TFrm_AddLine
  Left = 0
  Top = 0
  Caption = 'Add Line'
  ClientHeight = 346
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InsertWhere: TRadioGroup
    Left = 0
    Top = 202
    Width = 556
    Height = 101
    Align = alTop
    Caption = 'Insert'
    Items.Strings = (
      'On first Line'
      'Before selected line'
      'After selected line'
      'At the end')
    TabOrder = 0
  end
  object GroupBoxTextToAdd: TGroupBox
    Left = 0
    Top = 0
    Width = 556
    Height = 202
    Align = alTop
    Caption = 'Text'
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 103
    ExplicitWidth = 540
    object PanelTime: TPanel
      Left = 2
      Top = 15
      Width = 552
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 536
      DesignSize = (
        552
        45)
      object LabelTime: TLabel
        Left = 1
        Top = 2
        Width = 22
        Height = 13
        Caption = 'Time'
      end
      object editTime: TEdit
        Left = 1
        Top = 16
        Width = 541
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitWidth = 525
      end
    end
    object PanelThId: TPanel
      Left = 2
      Top = 60
      Width = 552
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 536
      DesignSize = (
        552
        45)
      object LabelThId: TLabel
        Left = 1
        Top = 2
        Width = 22
        Height = 13
        Caption = 'ThId'
      end
      object EditThId: TEdit
        Left = 1
        Top = 18
        Width = 541
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitWidth = 525
      end
    end
    object PanelLines: TPanel
      Left = 2
      Top = 105
      Width = 552
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitWidth = 536
      DesignSize = (
        552
        45)
      object LabelTraces: TLabel
        Left = 1
        Top = 2
        Width = 32
        Height = 13
        Caption = 'Traces'
      end
      object EditTrace: TEdit
        Left = 1
        Top = 16
        Width = 541
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitWidth = 525
      end
    end
    object PanelComment: TPanel
      Left = 2
      Top = 150
      Width = 552
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitWidth = 536
      DesignSize = (
        552
        45)
      object LabelComment: TLabel
        Left = 1
        Top = 2
        Width = 45
        Height = 13
        Caption = 'Comment'
      end
      object EditComment: TEdit
        Left = 1
        Top = 16
        Width = 541
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitWidth = 525
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 305
    Width = 556
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 160
    ExplicitTop = 312
    ExplicitWidth = 185
    DesignSize = (
      556
      41)
    object butAddLine: TButton
      Left = 16
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add line'
      ModalResult = 1
      TabOrder = 0
    end
    object butCancel: TButton
      Left = 469
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
