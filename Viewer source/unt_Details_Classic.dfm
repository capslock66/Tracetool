inherited frame_Classic: Tframe_Classic
  Width = 525
  Height = 401
  ExplicitWidth = 525
  ExplicitHeight = 401
  object SplitterH: TSplitter
    Left = 0
    Top = 156
    Width = 525
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 151
  end
  object VstDetail: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 525
    Height = 156
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = 16117479
    Colors.BorderColor = clBlack
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = clGray
    Colors.UnfocusedSelectionBorderColor = clGray
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Height = 17
    Header.MainColumn = 1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    HintMode = hmTooltip
    Indent = 15
    Margin = 0
    NodeAlignment = naFromTop
    ParentFont = False
    ParentShowHint = False
    PopupMenu = PopupDetail
    ScrollBarOptions.AlwaysVisible = True
    ShowHint = True
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.SelectionOptions = [toExtendedFocus]
    OnAfterCellPaint = VstDetailAfterCellPaint
    OnBeforeCellPaint = VstDetailBeforeCellPaint
    OnChange = VstDetailChange
    OnColumnClick = VstDetailColumnClick
    OnCreateEditor = VstDetailCreateEditor
    OnDblClick = VstDetailDblClick
    OnEditCancelled = VstDetailEditCancelled
    OnEdited = VstDetailEdited
    OnEditing = VstDetailEditing
    OnFocusChanged = VstDetailFocusChanged
    OnFreeNode = VstDetailFreeNode
    OnGetText = VstDetailGetText
    OnPaintText = VstDetailPaintText
    OnKeyAction = VstDetailKeyAction
    OnMeasureItem = VstDetailMeasureItem
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Color = 16117479
        MinWidth = 80
        Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 0
        Width = 150
      end
      item
        Color = 16117479
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 1
        Width = 130
      end
      item
        MinWidth = 300
        Position = 2
        Width = 300
      end>
  end
  inline FrameMemo: TFrameMemo
    Left = 0
    Top = 161
    Width = 525
    Height = 240
    Align = alBottom
    TabOrder = 1
    OnCanResize = FrameMemoCanResize
    ExplicitTop = 161
    ExplicitWidth = 525
    ExplicitHeight = 240
    inherited SynMemo: TSynEdit
      Width = 525
      Height = 215
      ExplicitWidth = 525
      ExplicitHeight = 215
    end
    inherited PanelTop: TPanel
      Width = 525
      ExplicitWidth = 525
    end
  end
  object PopupDetail: TPopupMenu
    Images = Frm_Tool.ilActions
    Left = 16
    Top = 30
    object MenuItem2: TMenuItem
      Action = FrmPageContainer.actCopy
    end
    object MenuItem3: TMenuItem
      Action = FrmPageContainer.actCopyCurrentCell
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Action = FrmPageContainer.actSelectAll
    end
  end
end
