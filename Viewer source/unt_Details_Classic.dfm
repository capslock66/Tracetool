inherited frame_Classic: Tframe_Classic
  Width = 525
  Height = 401
  ExplicitWidth = 525
  ExplicitHeight = 401
  object SplitterH: TSplitter
    Left = 0
    Top = 251
    Width = 525
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 253
  end
  object VstDetail: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 525
    Height = 251
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
    ExplicitHeight = 253
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
      Top = 22
      Width = 523
      Height = 122
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
      ExplicitLeft = 2
      ExplicitTop = 28
    end
    object ToolBar1: TToolBar
      Left = 1
      Top = 1
      Width = 523
      Height = 21
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 69
      Caption = 'ToolBar1'
      ShowCaptions = True
      TabOrder = 1
      ExplicitTop = 3
      object ShowAsTextButton: TToolButton
        Left = 0
        Top = 0
        AutoSize = True
        Caption = ' Text '
        ImageIndex = 0
        Visible = False
        OnClick = ShowAsTextButtonClick
      end
      object ShowAsXmlButton: TToolButton
        Left = 39
        Top = 0
        AutoSize = True
        Caption = ' Xml '
        ImageIndex = 1
        Visible = False
        OnClick = ShowAsXmlButtonClick
      end
      object ShowAsJSonButton: TToolButton
        Left = 72
        Top = 0
        AutoSize = True
        Caption = ' Json '
        ImageIndex = 2
        Visible = False
        OnClick = ShowAsJSonButtonClick
      end
      object ToolButton4: TToolButton
        Left = 111
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageIndex = 3
        Style = tbsSeparator
        Visible = False
      end
      object FormatButton: TToolButton
        Left = 119
        Top = 0
        AutoSize = True
        Caption = ' Format '
        ImageIndex = 4
        OnClick = FormatButtonClick
      end
      object ToolButton3: TToolButton
        Left = 170
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object ShowPopupButton: TToolButton
        Left = 178
        Top = 0
        AutoSize = True
        Caption = ' Show Popup'
        ImageIndex = 4
      end
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
  object SynXMLSyn: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeValueAttri.Style = []
    TextAttri.Style = []
    WantBracesParsed = False
    Left = 200
    Top = 32
  end
  object SynJSONSyn: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 120
    Top = 32
  end
  object XMLDocument: TXMLDocument
    Active = True
    Left = 200
    Top = 80
    DOMVendorDesc = 'MSXML'
  end
end
