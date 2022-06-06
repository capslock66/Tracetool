object FrmSearch: TFrmSearch
  Left = 362
  Top = 205
  BorderIcons = [biSystemMenu]
  Caption = 'Find'
  ClientHeight = 193
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    359
    193)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Search what :'
  end
  object EditSearch: TEdit
    Left = 8
    Top = 24
    Width = 340
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 64
    Width = 170
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Find options'
    TabOrder = 1
    object chkMatchCase: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Match &case'
      TabOrder = 0
    end
    object chkMatchWholeWord: TCheckBox
      Left = 8
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Match whole &word'
      TabOrder = 1
    end
    object chkSearchUp: TCheckBox
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Search &up'
      TabOrder = 2
      Visible = False
    end
  end
  object grpLookIn: TGroupBox
    Left = 188
    Top = 64
    Width = 161
    Height = 81
    Anchors = [akTop, akRight]
    Caption = 'Look in : '
    TabOrder = 2
    object rbCurrent: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Current &document'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbAllDocs: TRadioButton
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = '&All documents'
      TabOrder = 1
    end
  end
  object butFindNext: TButton
    Left = 166
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Find Next'
    Default = True
    ModalResult = 6
    TabOrder = 3
  end
  object butHightlightAll: TButton
    Left = 265
    Top = 160
    Width = 83
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Highlight all'
    ModalResult = 14
    TabOrder = 4
  end
  object butCancel: TButton
    Left = 8
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
