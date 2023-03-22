inherited FrmEventLog: TFrmEventLog
  Left = 314
  Top = 235
  Caption = 'FrmEventLog'
  ClientHeight = 380
  ClientWidth = 737
  OldCreateOrder = True
  ExplicitWidth = 753
  ExplicitHeight = 419
  PixelsPerInch = 96
  TextHeight = 13
  object GroupPanel: TPanel
    Left = 0
    Top = 0
    Width = 737
    Height = 380
    Align = alClient
    BevelOuter = bvNone
    Caption = 'GroupPanel'
    TabOrder = 0
    ExplicitWidth = 569
    ExplicitHeight = 171
    object VSplitter: TSplitter
      Left = 349
      Top = 22
      Height = 358
      Align = alRight
      ExplicitLeft = 196
      ExplicitHeight = 149
    end
    object VstEvent: TVirtualStringTree
      Left = 12
      Top = 22
      Width = 337
      Height = 358
      Align = alClient
      BevelInner = bvNone
      Colors.BorderColor = clWindowText
      Colors.HotColor = clBlack
      Colors.UnfocusedSelectionColor = clHighlight
      Colors.UnfocusedSelectionBorderColor = clHighlight
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
      ExplicitWidth = 184
      ExplicitHeight = 149
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
      Left = 352
      Top = 22
      Width = 385
      Height = 358
      Align = alRight
      BevelInner = bvLowered
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      object SplitterH: TSplitter
        Left = 2
        Top = 195
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
        Height = 193
        Align = alClient
        BevelOuter = bvNone
        Color = 16117479
        Colors.BorderColor = clWindowText
        Colors.HotColor = clBlack
        Colors.UnfocusedSelectionColor = clHighlight
        Colors.UnfocusedSelectionBorderColor = clHighlight
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
        ExplicitHeight = 167
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
        Top = 200
        Width = 381
        Height = 156
        Align = alBottom
        TabOrder = 1
        ExplicitLeft = 2
        ExplicitTop = 200
        ExplicitWidth = 381
        ExplicitHeight = 156
        inherited SynMemo: TSynEdit
          Width = 381
          Height = 127
          ExplicitTop = 29
          ExplicitWidth = 360
        end
        inherited PanelTop: TPanel
          Width = 381
          ExplicitLeft = 0
          ExplicitWidth = 360
        end
      end
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 737
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      Color = clCream
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 569
      DesignSize = (
        737
        22)
      object TracesInfo: TLabel
        Left = 3
        Top = 5
        Width = 51
        Height = 13
        Caption = 'TracesInfo'
      end
      object butClose: TBitBtn
        Left = 714
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
        ExplicitLeft = 546
      end
      object butReload: TBitBtn
        Left = 646
        Top = 0
        Width = 65
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Reload (50)'
        TabOrder = 1
        OnClick = butReloadClick
        ExplicitLeft = 478
      end
      object butGetAll: TBitBtn
        Left = 546
        Top = 0
        Width = 97
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Get All messages'
        TabOrder = 2
        OnClick = butGetAllClick
        ExplicitLeft = 378
      end
    end
    object PanelGutter: TPanel
      Left = 0
      Top = 22
      Width = 12
      Height = 358
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      OnDblClick = PanelGutterDblClick
      ExplicitHeight = 149
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
