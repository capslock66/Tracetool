object FrmFilter: TFrmFilter
  Left = 373
  Top = 254
  Caption = 'Filter'
  ClientHeight = 157
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 58
    Width = 523
    Height = 58
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object PanelChildren: TPanel
      Left = 224
      Top = 0
      Width = 299
      Height = 58
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object chkCheckChildren: TCheckBox
        Left = 8
        Top = 8
        Width = 281
        Height = 17
        Caption = 'Search in all lines, including &children'
        Checked = True
        State = cbChecked
        TabOrder = 0
        Visible = False
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 224
      Height = 58
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object rbShow: TRadioButton
        Left = 8
        Top = 8
        Width = 200
        Height = 17
        Caption = '&Show only lines matching criteria'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbHide: TRadioButton
        Left = 8
        Top = 32
        Width = 200
        Height = 17
        Caption = '&Hide lines matching criteria'
        TabOrder = 1
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 116
    Width = 523
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      523
      41)
    object butCancel: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Reset Filter'
      ModalResult = 2
      TabOrder = 0
      OnClick = butCancelClick
    end
    object butFilter: TButton
      Left = 438
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply Filter'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = butFilterClick
    end
  end
  object PanelFilterList: TPanel
    Left = 0
    Top = 29
    Width = 523
    Height = 29
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object ToolBarAdd: TToolBar
    Left = 0
    Top = 0
    Width = 523
    Height = 29
    AutoSize = True
    ButtonHeight = 29
    ButtonWidth = 47
    Caption = 'ToolBarAdd'
    EdgeInner = esNone
    EdgeOuter = esNone
    ShowCaptions = True
    TabOrder = 3
    object ButAdd: TToolButton
      Left = 0
      Top = 0
      AutoSize = True
      Caption = ' And ...'
      DropdownMenu = PopupMenuAndOr
      Style = tbsDropDown
      OnClick = ButAddClick
    end
  end
  object PopupMenuAndOr: TPopupMenu
    Left = 385
    Top = 21
    object mnuAnd: TMenuItem
      Caption = ' And ...'
      OnClick = mnuAndClick
    end
    object mnuOr: TMenuItem
      Caption = ' Or  ...'
      OnClick = mnuOrClick
    end
  end
end
