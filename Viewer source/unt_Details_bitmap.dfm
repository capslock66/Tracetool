inherited frame_BitmapDetails: Tframe_BitmapDetails
  Width = 242
  Height = 219
  ExplicitWidth = 242
  ExplicitHeight = 219
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 242
    Height = 219
    HorzScrollBar.Style = ssFlat
    HorzScrollBar.Tracking = True
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
    object ImageViewer: TImage
      Left = 8
      Top = 8
      Width = 217
      Height = 192
      PopupMenu = PopupDetail
      Transparent = True
    end
  end
  object PopupDetail: TPopupMenu
    Images = Frm_Tool.ilActions
    Left = 24
    Top = 38
    object copyMenu: TMenuItem
      Caption = '&Copy'
      Hint = 'Copy selected lines'
      ImageIndex = 0
      ShortCut = 16451
      OnClick = copyMenuClick
    end
  end
end
