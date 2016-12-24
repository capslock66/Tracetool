object FrmTraceWinProp: TFrmTraceWinProp
  Left = 374
  Top = 324
  Caption = 'Trace Window Property'
  ClientHeight = 205
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    458
    205)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 96
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object Label2: TLabel
    Left = 17
    Top = 128
    Width = 224
    Height = 13
    Caption = 'Maximum number of lines per file (-1 is unlimited)'
  end
  object butOk: TButton
    Left = 73
    Top = 166
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 4
    ExplicitTop = 124
  end
  object rbLogEnabled: TRadioButton
    Left = 16
    Top = 40
    Width = 401
    Height = 17
    Caption = 'Log enabled'
    TabOrder = 1
  end
  object rbLogIsDisabled: TRadioButton
    Left = 16
    Top = 16
    Width = 393
    Height = 17
    Caption = 'Log is disabled'
    TabOrder = 0
  end
  object rbDaily: TRadioButton
    Left = 17
    Top = 64
    Width = 408
    Height = 17
    Caption = 'Create a new file every day'
    TabOrder = 2
  end
  object editFilename: TEdit
    Left = 72
    Top = 92
    Width = 341
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ExplicitWidth = 344
  end
  object butFile: TButton
    Left = 422
    Top = 92
    Width = 25
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = butFileClick
    ExplicitLeft = 425
  end
  object butCancel: TButton
    Left = 372
    Top = 166
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
    ExplicitLeft = 374
  end
  object EditMaxLines: TEdit
    Left = 256
    Top = 125
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '-1'
  end
end
