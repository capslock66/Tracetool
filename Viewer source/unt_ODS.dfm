object Frm_ODS: TFrm_ODS
  Left = 358
  Top = 200
  Caption = 'ODS'
  ClientHeight = 250
  ClientWidth = 600
  Color = clBtnFace
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
  object PanelOds: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 568
    ExplicitHeight = 286
    object VSplitter: TSplitter
      Left = 208
      Top = 22
      Height = 228
      Align = alRight
      Visible = False
      ExplicitLeft = 176
      ExplicitHeight = 264
    end
    object VstDebugString: TVirtualStringTree
      Left = 12
      Top = 22
      Width = 196
      Height = 228
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
      Header.MainColumn = 1
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
      OnAfterCellPaint = VstDebugStringAfterCellPaint
      OnAfterPaint = VstDebugStringAfterPaint
      OnBeforeCellPaint = VstDebugStringBeforeCellPaint
      OnChange = VstDebugStringChange
      OnCompareNodes = VstDebugStringCompareNodes
      OnCreateEditor = VstDebugStringCreateEditor
      OnDblClick = VstDebugStringDblClick
      OnEditCancelled = VstDebugStringEditCancelled
      OnEdited = VstDebugStringEdited
      OnEditing = VstDebugStringEditing
      OnFreeNode = VstDebugStringFreeNode
      OnGetText = VstDebugStringGetText
      OnPaintText = VstDebugStringPaintText
      OnHeaderDragged = VstDebugStringHeaderDragged
      OnKeyAction = VstDebugStringKeyAction
      OnMeasureItem = VstDebugStringMeasureItem
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      ExplicitWidth = 164
      ExplicitHeight = 264
      Columns = <
        item
          Position = 0
          Text = 'Time'
          Width = 75
        end
        item
          Position = 1
          Text = 'Process Name'
          Width = 120
        end
        item
          Color = 16705515
          MinWidth = 3000
          Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 2
          Text = 'Lines'
          Width = 3000
        end>
    end
    object PanelTraceInfo: TPanel
      Left = 211
      Top = 22
      Width = 389
      Height = 228
      Align = alRight
      BevelInner = bvLowered
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      Visible = False
      ExplicitLeft = 179
      ExplicitHeight = 264
      object SplitterH: TSplitter
        Left = 2
        Top = 139
        Width = 385
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 2
      end
      object VstDetail: TVirtualStringTree
        Left = 2
        Top = 2
        Width = 385
        Height = 137
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
        ExplicitHeight = 111
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
        Left = 2
        Top = 144
        Width = 385
        Height = 82
        Align = alBottom
        TabOrder = 1
        ExplicitLeft = 2
        ExplicitTop = 144
        ExplicitWidth = 385
        ExplicitHeight = 82
        inherited SynMemo: TSynEdit
          Width = 385
          Height = 53
          ExplicitTop = 29
          ExplicitWidth = 360
        end
        inherited PanelTop: TPanel
          Width = 385
          ExplicitLeft = 0
          ExplicitWidth = 360
        end
      end
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 600
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      Color = clCream
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 568
      DesignSize = (
        600
        22)
      object TracesInfo: TLabel
        Left = 3
        Top = 5
        Width = 51
        Height = 13
        Caption = 'TracesInfo'
      end
      object butClose: TBitBtn
        Left = 577
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
        ExplicitLeft = 545
      end
    end
    object PanelGutter: TPanel
      Left = 0
      Top = 22
      Width = 12
      Height = 228
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      OnDblClick = PanelGutterDblClick
      ExplicitHeight = 264
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
    Left = 384
    Top = 62
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
