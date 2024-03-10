inherited FrmEventLog: TFrmEventLog
  Left = 314
  Top = 235
  Caption = 'FrmEventLog'
  ClientHeight = 379
  ClientWidth = 733
  ExplicitWidth = 745
  ExplicitHeight = 417
  TextHeight = 13
  object GroupPanel: TPanel
    Left = 0
    Top = 0
    Width = 733
    Height = 379
    Align = alClient
    BevelOuter = bvNone
    Caption = 'GroupPanel'
    TabOrder = 0
    object VSplitter: TSplitter
      Left = 345
      Top = 22
      Height = 357
      Align = alRight
      ExplicitLeft = 196
      ExplicitHeight = 149
    end
    object VstEvent: TVirtualStringTree
      Left = 12
      Top = 22
      Width = 333
      Height = 357
      Align = alClient
      BevelInner = bvNone
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
      Colors.UnfocusedSelectionColor = 15385233
      Colors.UnfocusedSelectionBorderColor = 15385233
      DragOperations = []
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Header.AutoSizeIndex = -1
      Header.DefaultHeight = 17
      Header.Height = 17
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
      HintMode = hmTooltip
      Images = Frm_Tool.ImageList1
      Indent = 15
      Margin = 0
      ParentFont = False
      ParentShowHint = False
      PopupMenu = PopupTree
      ScrollBarOptions.AlwaysVisible = True
      ScrollBarOptions.HorizontalIncrement = 100
      ShowHint = True
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideSelection, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
      OnAfterCellPaint = VstEventAfterCellPaint
      OnAfterPaint = VstEventAfterPaint
      OnBeforeCellPaint = VstEventBeforeCellPaint
      OnChange = VstEventChange
      OnCompareNodes = VstEventCompareNodes
      OnCreateEditor = VstEventCreateEditor
      OnDblClick = VstEventDblClick
      OnEditCancelled = VstEventEditCancelled
      OnEdited = VstEventEdited
      OnEditing = VstEventEditing
      OnFreeNode = VstEventFreeNode
      OnGetText = VstEventGetText
      OnPaintText = VstEventPaintText
      OnGetImageIndex = VstEventGetImageIndex
      OnHeaderDragged = VstEventHeaderDragged
      OnKeyAction = VstEventKeyAction
      OnMeasureItem = VstEventMeasureItem
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      ExplicitWidth = 329
      ExplicitHeight = 356
      Columns = <
        item
          Position = 0
          Width = 20
        end
        item
          Position = 1
          Text = 'Time'
          Width = 115
        end
        item
          MaxWidth = 1000
          Position = 2
          Text = 'Source'
          Width = 150
        end
        item
          Color = 16705515
          MinWidth = 3000
          Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 3
          Text = 'Lines'
          Width = 3000
        end>
    end
    object PanelTraceInfo: TPanel
      Left = 348
      Top = 22
      Width = 385
      Height = 357
      Align = alRight
      BevelInner = bvLowered
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      object SplitterH: TSplitter
        Left = 2
        Top = 194
        Width = 381
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 307
      end
      object VstDetail: TVirtualStringTree
        Left = 2
        Top = 2
        Width = 381
        Height = 192
        Align = alClient
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
        Colors.UnfocusedSelectionColor = 15385233
        Colors.UnfocusedSelectionBorderColor = 15385233
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
        ParentFont = False
        ParentShowHint = False
        ScrollBarOptions.AlwaysVisible = True
        ShowHint = True
        TabOrder = 0
        TreeOptions.AutoOptions = []
        TreeOptions.MiscOptions = []
        TreeOptions.PaintOptions = []
        OnBeforeCellPaint = VstDetailBeforeCellPaint
        OnChange = VstDetailChange
        OnColumnClick = VstDetailColumnClick
        OnCreateEditor = VstDetailCreateEditor
        OnDblClick = VstDetailDblClick
        OnEditing = VstDetailEditing
        OnFocusChanged = VstDetailFocusChanged
        OnFreeNode = VstDetailFreeNode
        OnGetText = VstDetailGetText
        OnPaintText = VstDetailPaintText
        OnMeasureItem = VstDetailMeasureItem
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        ExplicitHeight = 191
        Columns = <
          item
            Color = 16117479
            MinWidth = 80
            Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 0
            Width = 120
          end
          item
            Color = 16117479
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 1
            Width = 120
          end
          item
            Color = 16117479
            MinWidth = 3000
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 2
            Width = 3000
          end>
      end
      inline FrameMemo: TFrameMemo
        Left = 2
        Top = 199
        Width = 381
        Height = 156
        Align = alBottom
        TabOrder = 1
        ExplicitLeft = 2
        ExplicitTop = 199
        ExplicitWidth = 381
        ExplicitHeight = 156
        inherited SynMemo: TSynEdit
          Width = 381
          Height = 131
          ExplicitWidth = 381
          ExplicitHeight = 131
        end
        inherited PanelTop: TPanel
          Width = 381
          ExplicitWidth = 381
        end
      end
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 733
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      Color = clCream
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 729
      DesignSize = (
        733
        22)
      object TracesInfo: TLabel
        Left = 3
        Top = 5
        Width = 51
        Height = 13
        Caption = 'TracesInfo'
      end
      object butClose: TBitBtn
        Left = 710
        Top = 0
        Width = 22
        Height = 20
        Anchors = [akTop, akRight]
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000000000FF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
          00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
          0000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
          00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000000000FF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        TabOrder = 0
        OnClick = butCloseClick
        ExplicitLeft = 706
      end
      object butReload: TBitBtn
        Left = 642
        Top = 0
        Width = 65
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Reload (50)'
        TabOrder = 1
        OnClick = butReloadClick
        ExplicitLeft = 638
      end
      object butGetAll: TBitBtn
        Left = 542
        Top = 0
        Width = 97
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Get All messages'
        TabOrder = 2
        OnClick = butGetAllClick
        ExplicitLeft = 538
      end
    end
    object PanelGutter: TPanel
      Left = 0
      Top = 22
      Width = 12
      Height = 357
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      OnDblClick = PanelGutterDblClick
      ExplicitHeight = 356
    end
  end
  object PopupTree: TPopupMenu
    Images = Frm_Tool.ilActions
    Left = 24
    Top = 46
    object Cut1: TMenuItem
      Action = FrmPageContainer.actCut
    end
    object Copy1: TMenuItem
      Action = FrmPageContainer.actCopy
    end
    object Copycurrentcell1: TMenuItem
      Action = FrmPageContainer.actCopyCurrentCell
    end
    object Delete1: TMenuItem
      Action = FrmPageContainer.actDelete
    end
    object mnuTogglebookmark: TMenuItem
      Action = FrmPageContainer.actToggleBookmark
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Action = FrmPageContainer.actSelectAll
    end
  end
end
