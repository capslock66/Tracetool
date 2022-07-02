object Frm_AddLine: TFrm_AddLine
  Left = 0
  Top = 0
  Caption = 'Add Line'
  ClientHeight = 336
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    556
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object butAddLine: TButton
    Left = 40
    Top = 309
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add line'
    ModalResult = 1
    TabOrder = 0
  end
  object butCancel: TButton
    Left = 455
    Top = 309
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PanelUnderline: TPanel
    Left = -7
    Top = 303
    Width = 571
    Height = 2
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    ExplicitTop = 326
  end
  object InsertWhere: TRadioGroup
    Left = 8
    Top = 16
    Width = 153
    Height = 89
    Caption = 'Insert'
    Items.Strings = (
      'On first Line'
      'Before selected line'
      'After selected line'
      'At the end')
    TabOrder = 3
  end
  object GroupBoxTextToAdd: TGroupBox
    Left = 8
    Top = 111
    Width = 540
    Height = 186
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Text'
    TabOrder = 4
    DesignSize = (
      540
      186)
    object Label9: TLabel
      Left = 3
      Top = 18
      Width = 22
      Height = 13
      Caption = 'Time'
    end
    object Label1: TLabel
      Left = 3
      Top = 58
      Width = 22
      Height = 13
      Caption = 'ThId'
    end
    object Label2: TLabel
      Left = 3
      Top = 98
      Width = 32
      Height = 13
      Caption = 'Traces'
    end
    object Label3: TLabel
      Left = 3
      Top = 138
      Width = 45
      Height = 13
      Caption = 'Comment'
    end
    object editTime: TEdit
      Left = 3
      Top = 31
      Width = 525
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object EditThId: TEdit
      Left = 3
      Top = 70
      Width = 525
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object EditTrace: TEdit
      Left = 3
      Top = 110
      Width = 525
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object EditComment: TEdit
      Left = 3
      Top = 150
      Width = 525
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
end
