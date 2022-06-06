object Frm_Trace: TFrm_Trace
  Left = 323
  Top = 184
  Caption = 'TTRACE'
  ClientHeight = 325
  ClientWidth = 572
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTTraces: TPanel
    Left = 0
    Top = 0
    Width = 572
    Height = 325
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object VSplitter: TSplitter
      Left = 220
      Top = 22
      Width = 5
      Height = 303
      Align = alRight
      Visible = False
    end
    object PanelLeft: TPanel
      Left = 0
      Top = 22
      Width = 220
      Height = 303
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object vstTrace: TVirtualStringTree
        Left = 12
        Top = 0
        Width = 208
        Height = 303
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvRaised
        Colors.BorderColor = clWindowText
        Colors.HotColor = clBlack
        Colors.UnfocusedSelectionColor = clGray
        Colors.UnfocusedSelectionBorderColor = clGray
        DragOperations = []
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Header.AutoSizeIndex = -1
        Header.DefaultHeight = 17
        Header.Height = 17
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
        HintMode = hmTooltip
        Images = Frm_Tool.ilActions
        Indent = 15
        Margin = 0
        ParentFont = False
        ParentShowHint = False
        PopupMenu = PopupTree
        ScrollBarOptions.AlwaysVisible = True
        ScrollBarOptions.HorizontalIncrement = 100
        SelectionBlendFactor = 150
        ShowHint = True
        TabOrder = 0
        TreeOptions.AutoOptions = []
        TreeOptions.MiscOptions = []
        TreeOptions.PaintOptions = []
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect, toSimpleDrawSelection]
        OnAfterCellPaint = vstTraceAfterCellPaint
        OnAfterPaint = vstTraceAfterPaint
        OnBeforeCellPaint = vstTraceBeforeCellPaint
        OnChange = vstTraceChange
        OnCompareNodes = vstTraceCompareNodes
        OnCreateEditor = vstTraceCreateEditor
        OnDblClick = vstTraceDblClick
        OnEditCancelled = vstTraceEditCancelled
        OnEdited = vstTraceEdited
        OnEditing = vstTraceEditing
        OnFreeNode = vstTraceFreeNode
        OnGetText = vstTraceGetText
        OnPaintText = vstTracePaintText
        OnGetImageIndex = vstTraceGetImageIndex
        OnGetHint = vstTraceGetHint
        OnHeaderDragged = vstTraceHeaderDragged
        OnKeyAction = vstTraceKeyAction
        OnMeasureItem = vstTraceMeasureItem
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
            Position = 0
            Style = vsOwnerDraw
            Width = 20
          end
          item
            Position = 1
            Text = 'Time'
            Width = 75
          end
          item
            Position = 2
            Text = 'ThId'
          end
          item
            Color = 16705515
            MinWidth = 100
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 3
            Text = 'Traces'
            Width = 150
          end
          item
            Color = 16705515
            MinWidth = 3000
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 4
            Text = 'Comment'
            Width = 3000
          end>
      end
      object PanelGutter: TPanel
        Left = 0
        Top = 0
        Width = 12
        Height = 303
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        OnDblClick = PanelGutterDblClick
      end
    end
    object PanelRight: TPanel
      Left = 225
      Top = 22
      Width = 347
      Height = 303
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 572
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      Color = clCream
      ParentBackground = False
      TabOrder = 2
      DesignSize = (
        572
        22)
      object TracesInfo: TLabel
        Left = 3
        Top = 5
        Width = 51
        Height = 13
        Caption = 'TracesInfo'
        OnClick = TracesInfoClick
      end
      object LabelLogFile: TLabel
        Left = 487
        Top = 5
        Width = 60
        Height = 13
        Cursor = crHandPoint
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'LabelLogFile'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = LabelLogFileClick
      end
      object butClose: TBitBtn
        Left = 549
        Top = 0
        Width = 22
        Height = 22
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
      end
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
    object mnuSelectAll: TMenuItem
      Action = FrmPageContainer.actSelectAll
    end
    object mnuExpandAll: TMenuItem
      Caption = 'Expand all'
      OnClick = mnuExpandAllClick
    end
    object mnuCollapseAll: TMenuItem
      Caption = 'Collapse All'
      OnClick = mnuCollapseAllClick
    end
  end
end
