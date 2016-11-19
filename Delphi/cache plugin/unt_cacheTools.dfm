object CacheToolsForm: TCacheToolsForm
  Left = 132
  Top = 140
  Caption = 'Cache Tools'
  ClientHeight = 558
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000008000
    00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF8000
    00FF800000FF800000FF800000FF730000FFFFFFFFFFFFFFFFFFFFFFFFFF7F3B
    00FFB7B36FFFC6C28FFF7F4B00FF7F2B00FFA6A24FFFD7D2AFFF7F5A00FF7F1B
    00FF97932FFFE5E1CEFF7F6B00FF7E0000FF730000FFFFFFFFFFFFFFFFFFB876
    73FFC3C389FFB5B56CFFC79691FFA95754FFD2D2A6FFA6A64FFFD7B6B0FF9A37
    35FFE0E0C2FF999934FFE6D6CFFF7F0B00FF7E6F00FF730000FFFFFFFFFF8543
    0BFFBABA75FFC6C68FFF86550FFF833108FFACAC5AFFD4D4AAFF886712FF811E
    05FF9F9F40FFE0E0C4FF897915FF7F0C00FF777700FF0F0000FF720000FF7F07
    00FF86170EFF881913FF7F0A00FF7F0500FF84150AFF8A1C17FF7F0C00FF7F03
    00FF821306FF8C1E1BFF7F0E00FF7F0C00FF777700FF040000FF7F0000FF7F33
    00FFAF9B60FFBDA87CFF7F4100FF7F2500FFA18C44FFCBB697FF7F4E00FF7F17
    00FF937F29FFD7C3B2FF7F5D00FF7F0100FF771300FF040000FF7F0000FFB06E
    63FFC1C186FFB8B872FFBD8C7DFFA35148FFCCCC9AFFADAD5DFFCBAA98FF9633
    2DFFD6D6AEFFA4A44AFFD8C8B2FF7F0900FF7D5F00FF670000FF7F0000FF8D4B
    1BFFBBBB78FFC4C48AFF905F23FF893714FFB2B266FFCDCD9CFF94732AFF8522
    0CFFA9A954FFD5D5AEFF988832FF7F0C00FF787700FF1F0000FF7F0000FF7F0F
    00FF8D2F1DFF923326FF7F1400FF7F0B00FF892B15FF96382EFF7F1800FF7F07
    00FF85270CFF9A3C37FF7F1C00FF7F0C00FF777700FF040000FF7F0000FF7F2B
    00FFA88351FFB38E68FF7F3700FF7F1F00FF9C773AFFBF9A80FF7F4200FF7F13
    00FF906B22FFCAA597FF7F4E00FF7F0300FF772200FF040000FF7F0000FFA866
    53FFC0C083FFBABA77FFB38269FF9D4B3CFFC6C68EFFB4B46BFFBF9E7FFF922F
    26FFCCCC9AFFAFAF60FFCABA96FF7F0800FF7C5000FF570000FF7F0000FF9553
    2BFFBCBC7BFFC1C185FF9A6937FF8E3C1FFFB7B771FFC6C68EFFA07F43FF8926
    14FFB3B368FFCACA97FFA6964EFF7F0C00FF797700FF2F0000FF7F0000FF7F17
    00FF95472CFF9B4D39FF7F1E00FF7F1100FF8E411FFFA25446FF7F2400FF7F0A
    00FF883A12FFA85A52FF7F2B00FF7F0C00FF777700FF040000FF7F0000FFFFFF
    FFFF7F2100FF7F4900FF7F4900FF7F2F00FF7F1700FF7F4900FF7F4900FF7F39
    00FF7F0D00FF7F4900FF7F4900FF7F4700FF7B3600FF4B4700FF7F0000FFFFFF
    FFFFFFFFFFFF613900FF393900FF392500FF4F1200FF6A3900FF393900FF392C
    00FF460A00FF743900FF393900FF3F3A00FF7F4400FF7F3700FF7F0000FFFFFF
    FFFFFFFFFFFFFFFFFFFF410000FF410000FF550000FF6D0000FF410000FF4100
    00FF4D0000FF750000FF410000FF470500FF7F3900FF800000FF800000FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 763
    Height = 558
    ActivePage = tsSQL
    Align = alClient
    TabOrder = 0
    object tsSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 1
      object Splitter5: TSplitter
        Left = 233
        Top = 0
        Height = 530
        ExplicitLeft = 56
        ExplicitTop = 160
        ExplicitHeight = 100
      end
      object Panel4: TPanel
        Left = 236
        Top = 0
        Width = 519
        Height = 530
        Align = alClient
        Caption = 'Panel4'
        TabOrder = 0
        object Splitter2: TSplitter
          Left = 1
          Top = 405
          Width = 517
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitTop = 270
          ExplicitWidth = 604
        end
        object Splitter4: TSplitter
          Left = 1
          Top = 217
          Width = 517
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitLeft = 2
          ExplicitTop = 186
          ExplicitWidth = 518
        end
        object MemoSQL: TMemo
          Left = 1
          Top = 25
          Width = 517
          Height = 192
          Align = alTop
          HideSelection = False
          Lines.Strings = (
            'place here the SQL statement.'
            ''
            'To execute the query : Button ">" or F5 key'
            'To get the query plan : Button "i" or F8 key')
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object vstQueryPlan: TVirtualStringTree
          Left = 1
          Top = 244
          Width = 517
          Height = 161
          Align = alTop
          Color = clCream
          Header.AutoSizeIndex = -1
          Header.Background = 14215660
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw, hoVisible]
          Header.Style = hsXPStyle
          Images = RecselMdiMain.ImgRecSel
          NodeDataSize = 4
          TabOrder = 1
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
          OnDblClick = vstQueryPlanDblClick
          OnFreeNode = vstQueryPlanFreeNode
          OnGetText = vstQueryPlanGetText
          OnPaintText = vstQueryPlanPaintText
          Columns = <
            item
              Position = 0
              Width = 513
            end>
        end
        object EhGrid: TDBGridEh
          Left = 1
          Top = 408
          Width = 517
          Height = 121
          Align = alClient
          AllowedOperations = [alopUpdateEh, alopDeleteEh]
          AllowedSelections = [gstRecordBookmarks, gstAll]
          BorderStyle = bsNone
          Color = clCream
          ColumnDefValues.Title.Color = 14215660
          DataSource = ds_query
          Flat = False
          FooterColor = clWindow
          FooterFont.Charset = DEFAULT_CHARSET
          FooterFont.Color = clWindowText
          FooterFont.Height = -11
          FooterFont.Name = 'Tahoma'
          FooterFont.Style = []
          Options = [dgTitles, dgColumnResize, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
          OptionsEh = [dghFixed3D, dghResizeWholeRightPart, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghIncSearch, dghRowHighlight, dghDblClickOptimizeColWidth]
          RowHeight = 4
          RowLines = 1
          SumList.VirtualRecords = True
          TabOrder = 2
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          VertScrollBar.Tracking = True
          EmptyMessage = '<No data to display>'
        end
        object ToolBarSql: TToolBar
          Left = 1
          Top = 220
          Width = 517
          Height = 24
          Color = clWhite
          EdgeInner = esNone
          EdgeOuter = esNone
          Images = ImgList
          ParentColor = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          object Button1: TButton
            Left = 0
            Top = 0
            Width = 160
            Height = 22
            Caption = 'Copy query plan to clipboard'
            TabOrder = 0
            OnClick = butCopyQueryPlanToClipBoardClick
          end
        end
        object ToolBar1: TToolBar
          Left = 1
          Top = 1
          Width = 517
          Height = 24
          Color = clWhite
          EdgeInner = esNone
          EdgeOuter = esNone
          Images = ImgList
          ParentColor = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          object ToolButton4: TToolButton
            Left = 0
            Top = 0
            Action = ActionSqlRun
          end
          object ToolButton5: TToolButton
            Left = 23
            Top = 0
            Action = ActionQueryPlan
          end
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 233
        Height = 530
        Align = alLeft
        TabOrder = 1
        object Splitter6: TSplitter
          Left = 1
          Top = 417
          Width = 231
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitLeft = -4
          ExplicitTop = 330
        end
        object VstCatalog: TVirtualStringTree
          Left = 1
          Top = 42
          Width = 231
          Height = 375
          Align = alTop
          Color = clCream
          Header.AutoSizeIndex = -1
          Header.Background = 14215660
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw, hoVisible]
          Header.Style = hsXPStyle
          Images = RecselMdiMain.ImgRecSel
          NodeDataSize = 4
          ScrollBarOptions.AlwaysVisible = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
          OnChange = VstCatalogChange
          OnDblClick = VstCatalogDblClick
          OnFreeNode = vstQueryPlanFreeNode
          OnGetText = vstQueryPlanGetText
          OnPaintText = vstQueryPlanPaintText
          Columns = <
            item
              MinWidth = 2000
              Position = 0
              Width = 2000
            end>
        end
        object Panel6: TPanel
          Left = 1
          Top = 1
          Width = 231
          Height = 41
          Align = alTop
          TabOrder = 1
          object butGetCatalog: TButton
            Left = 12
            Top = 9
            Width = 117
            Height = 22
            Caption = 'Refresh Catalog'
            TabOrder = 0
            OnClick = butGetCatalogClick
          end
        end
        object VstCatalogDetail: TVirtualStringTree
          Left = 1
          Top = 420
          Width = 231
          Height = 109
          Align = alClient
          Color = clCream
          Header.AutoSizeIndex = -1
          Header.Background = 14215660
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw, hoVisible]
          Header.Style = hsXPStyle
          Images = RecselMdiMain.ImgRecSel
          NodeDataSize = 4
          ScrollBarOptions.AlwaysVisible = True
          TabOrder = 2
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
          OnDblClick = VstCatalogDetailDblClick
          OnFreeNode = vstQueryPlanFreeNode
          OnGetText = vstQueryPlanGetText
          OnPaintText = vstQueryPlanPaintText
          Columns = <
            item
              MinWidth = 150
              Position = 0
              Width = 150
            end
            item
              MinWidth = 150
              Position = 1
              Width = 150
            end>
        end
      end
    end
    object tsClasses: TTabSheet
      Caption = 'Class information'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 257
        Top = 0
        Height = 530
        ExplicitLeft = 148
        ExplicitTop = 137
        ExplicitHeight = 413
      end
      object PanelTop: TPanel
        Left = 0
        Top = 0
        Width = 257
        Height = 530
        Align = alLeft
        TabOrder = 0
        object ListBoxClasses: TListBox
          Left = 1
          Top = 57
          Width = 255
          Height = 472
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnClick = ListBoxClassesClick
          OnDblClick = ListBoxClassesDblClick
        end
        object Panel7: TPanel
          Left = 1
          Top = 1
          Width = 255
          Height = 56
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            255
            56)
          object Label1: TLabel
            Left = 8
            Top = 8
            Width = 117
            Height = 13
            Caption = 'Get classes starting with'
          end
          object EditStartWith: TEdit
            Left = 135
            Top = 4
            Width = 114
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object butGetAllClasses: TButton
            Left = 8
            Top = 26
            Width = 75
            Height = 25
            Caption = 'Get Classes'
            TabOrder = 1
            OnClick = butGetAllClassesClick
          end
        end
      end
      object PanelClassDetail: TPanel
        Left = 260
        Top = 0
        Width = 495
        Height = 530
        Align = alClient
        TabOrder = 1
        OnDblClick = PanelClassDetailDblClick
        DesignSize = (
          495
          530)
        object Label2: TLabel
          Left = 15
          Top = 19
          Width = 52
          Height = 13
          Caption = 'class name'
        end
        object EditClass: TEdit
          Left = 86
          Top = 16
          Width = 282
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object butDisplayClass: TButton
          Left = 5
          Top = 43
          Width = 105
          Height = 25
          Caption = 'Display class info'
          TabOrder = 1
          OnClick = butDisplayClassClick
        end
        object butLoadClass: TButton
          Left = 6
          Top = 74
          Width = 105
          Height = 25
          Caption = 'Load class/file'
          TabOrder = 2
          OnClick = butLoadClassClick
        end
        object rbXml: TRadioButton
          Left = 121
          Top = 81
          Width = 51
          Height = 17
          Caption = 'Xml'
          Checked = True
          TabOrder = 3
          TabStop = True
        end
        object rbCDL: TRadioButton
          Left = 121
          Top = 104
          Width = 49
          Height = 17
          Caption = 'CDL'
          TabOrder = 4
        end
        object memClass: TMemo
          Left = 8
          Top = 136
          Width = 479
          Height = 309
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 5
        end
        object MemoResult: TMemo
          Left = 8
          Top = 451
          Width = 476
          Height = 71
          Anchors = [akLeft, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 6
        end
        object PanelTarget: TPanel
          Left = 176
          Top = 67
          Width = 308
          Height = 64
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
          Visible = False
          DesignSize = (
            308
            64)
          object LabelTargetServer: TLabel
            Left = 15
            Top = 9
            Width = 66
            Height = 13
            Caption = 'Target server'
          end
          object TargetServer: TComboBox
            Left = 87
            Top = 6
            Width = 216
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 0
          end
          object ButSaveClass: TButton
            Left = 14
            Top = 33
            Width = 161
            Height = 25
            Caption = 'Write class  to target server'
            TabOrder = 1
            OnClick = ButSaveClassClick
          end
        end
      end
    end
    object tsProcess: TTabSheet
      Caption = 'Process information'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ToolBarProcess: TToolBar
        Left = 0
        Top = 0
        Width = 755
        Height = 24
        Color = clWhite
        EdgeInner = esNone
        EdgeOuter = esNone
        Images = RecselMdiMain.ilDialog
        ParentColor = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object ButGetProcessInfo: TButton
          Left = 0
          Top = 0
          Width = 145
          Height = 22
          Caption = 'get Process info / Refresh'
          TabOrder = 0
          OnClick = ButGetProcessInfoClick
        end
      end
      object vstProcess: TVirtualStringTree
        Left = 0
        Top = 24
        Width = 755
        Height = 506
        Align = alClient
        Color = clCream
        Header.AutoSizeIndex = -1
        Header.Background = 14215660
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw, hoVisible]
        Header.Style = hsXPStyle
        Images = RecselMdiMain.ImgRecSel
        NodeDataSize = 4
        TabOrder = 1
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
        OnFreeNode = vstProcessFreeNode
        OnGetText = vstProcessGetText
        Columns = <
          item
            Position = 0
          end
          item
            Position = 1
            Width = 683
          end>
      end
    end
    object tsGlobal: TTabSheet
      Caption = 'Globals'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter3: TSplitter
        Left = 182
        Top = 0
        Height = 530
        ExplicitLeft = 281
        ExplicitTop = 51
        ExplicitHeight = 439
      end
      object Panel1: TPanel
        Left = 185
        Top = 0
        Width = 570
        Height = 530
        Align = alClient
        TabOrder = 0
        DesignSize = (
          570
          530)
        object LabelElementCount: TLabel
          Left = 373
          Top = 15
          Width = 12
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '...'
        end
        object EditGlobal: TEdit
          Left = 6
          Top = 10
          Width = 275
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = '^mqh'
        end
        object butGlobal: TButton
          Left = 417
          Top = 8
          Width = 81
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Display Global'
          TabOrder = 1
          OnClick = butGlobalClick
        end
        object vstGlobals: TVirtualStringTree
          Left = 6
          Top = 37
          Width = 555
          Height = 486
          Anchors = [akLeft, akTop, akRight, akBottom]
          Color = clCream
          Header.AutoSizeIndex = -1
          Header.Background = 14215660
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw, hoVisible]
          Header.Style = hsXPStyle
          Images = RecselMdiMain.ImgRecSel
          NodeDataSize = 4
          TabOrder = 2
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
          OnFreeNode = vstGlobalsFreeNode
          OnGetText = vstGlobalsGetText
          Columns = <
            item
              Position = 0
              Width = 250
            end
            item
              Position = 1
              Width = 305
            end>
        end
        object butKillGlobal: TButton
          Left = 504
          Top = 8
          Width = 56
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'KillGlobal'
          TabOrder = 3
          Visible = False
          OnClick = butKillGlobalClick
        end
        object butGlobalCount: TButton
          Left = 287
          Top = 8
          Width = 80
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Element count'
          TabOrder = 4
          OnClick = butGlobalCountClick
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 182
        Height = 530
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 182
          Height = 37
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            182
            37)
          object butRefreshGlobals: TButton
            Left = 16
            Top = 6
            Width = 144
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Refresh globals list'
            TabOrder = 0
            OnClick = butRefreshGlobalsClick
          end
        end
        object vstGlobalsList: TVirtualStringTree
          Left = 0
          Top = 37
          Width = 182
          Height = 493
          Align = alClient
          Color = clCream
          Header.AutoSizeIndex = -1
          Header.Background = 14215660
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw]
          Header.Style = hsXPStyle
          Images = RecselMdiMain.ImgRecSel
          NodeDataSize = 4
          TabOrder = 1
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
          OnChange = vstGlobalsListChange
          OnDblClick = vstGlobalsListDblClick
          OnFreeNode = vstGlobalsListFreeNode
          OnGetText = vstGlobalsListGetText
          Columns = <
            item
              Position = 0
              Width = 182
            end>
        end
      end
    end
  end
  object query: TOEDataSet
    hStmt.CursorType = 0
    hStmt.SkipByPosition = True
    hStmt.SkipByCursor = True
    hStmt.Target.Target = (
      'OE6.01.05 (PO COM)'
      ''
      ''
      ''
      '')
    Editable = True
    Params = <>
    Left = 403
  end
  object ds_query: TDataSource
    DataSet = query
    Left = 443
    Top = 65535
  end
  object ServerSchema: TOESchema
    Tables.Tables = (
      'OE6.01.05 (PO COM)'
      ())
    Views.Views = (
      'OE6.01.05 (PO COM)')
    ExecMarker = 'go'
    NameConstraints = False
    Left = 488
  end
  object ActionList1: TActionList
    Images = ImgList
    Left = 584
    Top = 8
    object ActionSqlRun: TAction
      Caption = 'Run'
      Hint = 'Run sql'
      ImageIndex = 1
      ShortCut = 116
      OnExecute = ActionSqlRunExecute
    end
    object ActionQueryPlan: TAction
      Caption = 'Show Query Plan'
      Hint = 'Show Query Plan'
      ImageIndex = 0
      ShortCut = 119
      OnExecute = ActionQueryPlanExecute
    end
  end
  object ImgList: TImageList
    Left = 616
    Top = 8
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5390000B5390000B539
      0000B5390000B5390000B5390000B5390000B5390000B5390000B5390000B539
      0000B5390000B5390000B5390000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000B5390000B5390000B539
      0000B5390000B5390000B5390000B5390000B5390000B5390000B5390000B539
      0000B5390000B5390000B5390000B53900000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000B5390000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5390000B53900000000000000000000000000000000
      0000000000008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5390000B5390000B5390000B5390000B5390000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      0000000000000000000000000000848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5390000B5390000B5390000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      00000000000000000000FF000000000000008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5390000B5390000B5390000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      0000000000000000000000000000FF0000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5390000B5390000B5390000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5390000B5390000B5390000B5390000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5390000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B53900000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000F7F7F700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5390000B5390000B5390000B5390000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7F7F700B53900000000000000000000000000000000
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5390000B5390000B5390000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5390000B5390000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00B5390000B5390000B53900000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5390000B5390000B539
      0000B5390000B5390000B5390000B5390000B5390000B5390000B5390000B539
      0000B5390000B5390000B5390000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF000000008001FFFF00000000
      0000FFFF000000000000F8FF000000000000F07F000000000000F83F00000000
      0000FC1F000000000000FE0F000000000000FF1F000000000000FE3F00000000
      0000FC7F000000000000F8FF000000000000F1FF000000008001FFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
