object FrmTail: TFrmTail
  Left = 357
  Top = 206
  ClientHeight = 249
  ClientWidth = 596
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object GroupPanel: TPanel
    Left = 0
    Top = 0
    Width = 596
    Height = 249
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object VSplitter: TSplitter
      Left = 190
      Top = 22
      Height = 227
      Align = alRight
      Visible = False
      ExplicitLeft = 162
      ExplicitHeight = 214
    end
    object VstTail: TVirtualStringTree
      Left = 12
      Top = 22
      Width = 178
      Height = 227
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
      Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoVisible]
      HintMode = hmTooltip
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
      TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
      OnAfterCellPaint = VstTailAfterCellPaint
      OnAfterItemPaint = VstTailAfterItemPaint
      OnAfterPaint = VstTailAfterPaint
      OnBeforeCellPaint = VstTailBeforeCellPaint
      OnChange = VstTailChange
      OnCompareNodes = VstTailCompareNodes
      OnCreateEditor = VstTailCreateEditor
      OnDblClick = VstTailDblClick
      OnEditCancelled = VstTailEditCancelled
      OnEdited = VstTailEdited
      OnEditing = VstTailEditing
      OnFreeNode = VstTailFreeNode
      OnGetText = VstTailGetText
      OnPaintText = VstTailPaintText
      OnHeaderDragged = VstTailHeaderDragged
      OnKeyAction = VstTailKeyAction
      OnMeasureItem = VstTailMeasureItem
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      ExplicitWidth = 174
      ExplicitHeight = 226
      Columns = <
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
          Position = 0
          Text = 'Approx. Time'
          Width = 75
        end
        item
          Color = 16705515
          MinWidth = 3000
          Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 1
          Text = 'Lines'
          Width = 3000
        end>
    end
    object PanelTraceInfo: TPanel
      Left = 193
      Top = 22
      Width = 403
      Height = 227
      Align = alRight
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      Visible = False
      object SplitterH: TSplitter
        Left = 1
        Top = 154
        Width = 401
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 1
      end
      object VstDetail: TVirtualStringTree
        Left = 1
        Top = 1
        Width = 401
        Height = 153
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
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.SelectionOptions = [toExtendedFocus]
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
        ExplicitHeight = 152
        Columns = <
          item
            Color = 16117479
            MinWidth = 80
            Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 0
            Width = 150
          end
          item
            MinWidth = 300
            Position = 1
            Width = 300
          end>
      end
      inline FrameMemo: TFrameMemo
        Left = 1
        Top = 159
        Width = 401
        Height = 67
        Align = alBottom
        TabOrder = 1
        ExplicitLeft = 1
        ExplicitTop = 159
        ExplicitWidth = 401
        ExplicitHeight = 67
        inherited SynMemo: TSynEdit
          Width = 401
          Height = 42
          ExplicitWidth = 401
          ExplicitHeight = 42
        end
        inherited PanelTop: TPanel
          Width = 401
          ExplicitWidth = 401
        end
      end
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 596
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      Color = clCream
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 592
      DesignSize = (
        596
        22)
      object TracesInfo: TLabel
        Left = 3
        Top = 5
        Width = 51
        Height = 13
        Caption = 'TracesInfo'
      end
      object butClose: TBitBtn
        Left = 573
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
        ExplicitLeft = 569
      end
      object butReload: TBitBtn
        Left = 518
        Top = 0
        Width = 52
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Reload'
        TabOrder = 1
        OnClick = butReloadClick
        ExplicitLeft = 514
      end
      object butClearContent: TBitBtn
        Left = 408
        Top = 0
        Width = 97
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Clear File Content'
        TabOrder = 2
        OnClick = butClearContentClick
        ExplicitLeft = 404
      end
    end
    object PanelGutter: TPanel
      Left = 0
      Top = 22
      Width = 12
      Height = 227
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      OnDblClick = PanelGutterDblClick
      ExplicitHeight = 226
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
  object PopupDetail: TPopupMenu
    Images = Frm_Tool.ilActions
    Left = 320
    Top = 54
    object MenuItem2: TMenuItem
      Action = FrmPageContainer.actCopy
    end
    object MenuItem3: TMenuItem
      Action = FrmPageContainer.actCopyCurrentCell
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItem1: TMenuItem
      Action = FrmPageContainer.actSelectAll
    end
  end
end
