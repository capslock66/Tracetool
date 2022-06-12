object FrmSelectTail: TFrmSelectTail
  Left = 442
  Top = 198
  AlphaBlend = True
  Caption = 'Select a file to watch'
  ClientHeight = 501
  ClientWidth = 580
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    580
    501)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 66
    Height = 13
    Caption = 'Columns Style'
  end
  object Label2: TLabel
    Left = 22
    Top = 437
    Width = 323
    Height = 13
    Caption = 
      'You can also open files by running tracetool with the file in pa' +
      'rameter'
  end
  object ComboColumnsStyle: TComboBox
    Left = 96
    Top = 8
    Width = 472
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cbColumnsClick
    Items.Strings = (
      'Show Approximative time and lines'
      'Show lines only'
      'Multi columns')
  end
  object butSelect: TBitBtn
    Left = 539
    Top = 185
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000C40E0000C40E00000000000000000000FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080808080FF00FF80
      8080808080FF00FF808080808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF000000000000808080000000000000808080000000000000808080FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF00FF00000000
      0000FF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    TabOrder = 3
    OnClick = butSelectClick
  end
  object EditFileName: TEdit
    Left = 32
    Top = 187
    Width = 498
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnKeyUp = EditFileNameKeyUp
  end
  object butOpen: TButton
    Left = 40
    Top = 468
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open'
    ModalResult = 1
    TabOrder = 8
    OnClick = butOpenClick
  end
  object butCancel: TButton
    Left = 455
    Top = 468
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object rbSingleFile: TRadioButton
    Left = 16
    Top = 165
    Width = 113
    Height = 17
    Caption = 'Open single file'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = rbSingleFileClick
  end
  object rbFavorite: TRadioButton
    Left = 16
    Top = 212
    Width = 169
    Height = 17
    Caption = 'Select from favorite list'
    TabOrder = 4
    OnClick = rbSingleFileClick
  end
  object FavoriteFiles: TCheckListBox
    Left = 32
    Top = 232
    Width = 530
    Height = 169
    OnClickCheck = FavoriteFilesClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Items.Strings = (
      'File1'
      'File2'
      'File3')
    TabOrder = 5
    OnDblClick = FavoriteFilesDblClick
  end
  object Panel4: TPanel
    Left = 4
    Top = 456
    Width = 571
    Height = 2
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 10
  end
  object ButAddfile: TButton
    Left = 128
    Top = 407
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add favorite file'
    TabOrder = 6
    OnClick = ButAddfileClick
  end
  object butDeletefile: TButton
    Left = 272
    Top = 407
    Width = 169
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete selected favorite file'
    TabOrder = 7
    OnClick = butDeletefileClick
  end
  object Panel5: TPanel
    Left = 14
    Top = 45
    Width = 264
    Height = 113
    BevelInner = bvLowered
    TabOrder = 11
    object rbFromFirstLine: TRadioButton
      Left = 15
      Top = 26
      Width = 177
      Height = 17
      Caption = 'Detect columns on the first line'
      TabOrder = 0
      OnClick = cbColumnsClick
    end
    object chkTitle: TCheckBox
      Left = 47
      Top = 44
      Width = 185
      Height = 17
      Caption = 'First line contains the title'
      TabOrder = 1
    end
    object rbEachLines: TRadioButton
      Left = 15
      Top = 5
      Width = 233
      Height = 17
      Caption = 'Detect new columns on each lines'
      TabOrder = 2
      OnClick = cbColumnsClick
    end
    object rbFixedcol: TRadioButton
      Left = 15
      Top = 64
      Width = 178
      Height = 17
      Caption = 'Fixed number of columns'
      TabOrder = 3
      OnClick = cbColumnsClick
    end
    object EditColNumber: TEdit
      Left = 47
      Top = 84
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '2'
    end
  end
  object Panel6: TPanel
    Left = 295
    Top = 45
    Width = 269
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvLowered
    TabOrder = 12
    object LabelTxtQualifier: TLabel
      Left = 8
      Top = 88
      Width = 60
      Height = 13
      Caption = 'Text qualifier'
    end
    object LabelSeparator1: TLabel
      Left = 8
      Top = 6
      Width = 211
      Height = 26
      Caption = 
        'Column separator  : integer or any characters'#13#10'like '#39','#39' or '#39';'#39' o' +
        'r a space (9 = Tabulation)'
    end
    object ComboQualifier: TComboBox
      Left = 88
      Top = 84
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'None'
        'Use simple quote'
        'Use double quote')
    end
    object EditSeparator: TEdit
      Left = 9
      Top = 48
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '9'
    end
  end
end
