inherited frame_Classic: Tframe_Classic
  Width = 525
  Height = 401
  ExplicitWidth = 525
  ExplicitHeight = 401
  object SplitterH: TSplitter
    Left = 0
    Top = 253
    Width = 525
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = -27
    ExplicitTop = 213
    ExplicitWidth = 347
  end
  object VstDetail: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 525
    Height = 253
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = 16117479
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
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
  object PanelDetailBottom: TPanel
    Left = 0
    Top = 256
    Width = 525
    Height = 145
    Align = alBottom
    TabOrder = 1
    object SynMemo: TSynEdit
      Left = 1
      Top = 42
      Width = 523
      Height = 102
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 0
      UseCodeFolding = False
      BorderStyle = bsNone
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Gutter.Width = 0
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 523
      Height = 41
      Align = alTop
      TabOrder = 1
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
  object SynXMLSyn1: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeValueAttri.Style = []
    TextAttri.Style = []
    WantBracesParsed = False
    Left = 200
    Top = 32
  end
  object SynJSONSyn1: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 120
    Top = 32
  end
end
