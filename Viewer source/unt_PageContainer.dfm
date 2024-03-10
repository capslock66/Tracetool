object FrmPageContainer: TFrmPageContainer
  Left = 756
  Top = 244
  ClientHeight = 271
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object PanelPageControl: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 271
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 460
    ExplicitHeight = 270
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 464
      Height = 36
      AutoSize = True
      ButtonHeight = 36
      GradientEndColor = 11319229
      Images = Frm_Tool.ilActions
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      ExplicitWidth = 460
      object tbnClear: TToolButton
        Left = 0
        Top = 0
        Action = actClear
        AutoSize = True
      end
      object tbnCopy: TToolButton
        Left = 23
        Top = 0
        Action = actCopy
        AutoSize = True
      end
      object tbnSave: TToolButton
        Left = 46
        Top = 0
        Action = actSaveToFile
        AutoSize = True
      end
      object ToolButton1: TToolButton
        Left = 69
        Top = 0
        Action = actPrint
      end
      object SepStandard: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Caption = 'SepStandard'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object tbnPause: TToolButton
        Left = 100
        Top = 0
        Action = actPause
        AutoSize = True
      end
      object tbnTraceInfo: TToolButton
        Left = 123
        Top = 0
        Action = actViewTraceInfo
        AutoSize = True
      end
      object SepBookmark: TToolButton
        Left = 146
        Top = 0
        Width = 8
        Caption = 'SepBookmark'
        ImageIndex = 30
        Style = tbsSeparator
      end
      object tbnInsertRow: TToolButton
        Left = 154
        Top = 0
        Action = actInsert
      end
      object tbnToggleBookmark: TToolButton
        Left = 177
        Top = 0
        Action = actToggleBookmark
        AutoSize = True
      end
      object tbnBookmarkPrevious: TToolButton
        Left = 200
        Top = 0
        Action = actPreviousBookmark
        AutoSize = True
      end
      object tbnBookmarkNext: TToolButton
        Left = 223
        Top = 0
        Action = actNextBookmark
        AutoSize = True
      end
      object tbnClearBookmark: TToolButton
        Left = 246
        Top = 0
        Action = actClearBookmarks
        AutoSize = True
      end
      object SepSearch: TToolButton
        Left = 269
        Top = 0
        Width = 8
        Caption = 'SepSearch'
        ImageIndex = 7
        Style = tbsSeparator
      end
      object tbnSearch: TToolButton
        Left = 277
        Top = 0
        Action = actSearch
        AutoSize = True
      end
      object tbnSearchPrevious: TToolButton
        Left = 300
        Top = 0
        Action = actFindPrevious
        AutoSize = True
      end
      object tbnSearchNext: TToolButton
        Left = 323
        Top = 0
        Action = actFindNext
        AutoSize = True
      end
      object tbnClearHighlight: TToolButton
        Left = 346
        Top = 0
        Action = actClearHighlight
        AutoSize = True
      end
      object SepFilter: TToolButton
        Left = 369
        Top = 0
        Width = 8
        Caption = 'SepFilter'
        ImageIndex = 29
        Style = tbsSeparator
      end
      object tbnFilter: TToolButton
        Left = 377
        Top = 0
        Action = actFilter
        AutoSize = True
      end
      object tbnClearFilter: TToolButton
        Left = 400
        Top = 0
        Action = actClearFilter
        AutoSize = True
      end
    end
  end
  object Actions: TActionList
    Images = Frm_Tool.ilActions
    Left = 96
    Top = 104
    object actCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy selected lines'
      ImageIndex = 0
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
    object actCopyCurrentCell: TAction
      Category = 'Edit'
      Caption = 'Copy current cell'
      Hint = 'Copy current cell'
      OnExecute = actCopyCurrentCellExecute
    end
    object actClear: TAction
      Category = 'Edit'
      Caption = 'C&lear All'
      Hint = 'Clear window'
      ImageIndex = 1
      ShortCut = 16460
      OnExecute = actClearExecute
    end
    object actSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select all messages'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
    object actSaveToFile: TAction
      Category = 'Edit'
      Caption = '&Save to File...'
      Hint = 'Save to file...'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = actSaveToFileExecute
    end
    object actDelete: TAction
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete selected lines'
      ImageIndex = 9
      OnExecute = actDeleteExecute
    end
    object actCut: TAction
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut selected lines'
      ImageIndex = 8
      ShortCut = 16472
      OnExecute = actCutExecute
    end
    object actClearFileContent: TAction
      Category = 'Edit'
      Caption = 'Cl&ear File Content'
      Hint = 'Clear File Content'
      ShortCut = 16453
      Visible = False
      OnExecute = actClearFileContentExecute
    end
    object actViewProperty: TAction
      Category = 'Edit'
      Caption = 'View Properties'
      Enabled = False
      Hint = 'View Properties'
      OnExecute = actViewPropertyExecute
    end
    object actPause: TAction
      Category = 'Edit'
      Caption = '&Pause'
      Hint = 'Pause'
      ImageIndex = 4
      ShortCut = 113
      OnExecute = actPauseExecute
    end
    object actViewTraceInfo: TAction
      Category = 'Edit'
      Caption = 'View Trace &Info'
      Hint = 'View Trace Info'
      ImageIndex = 6
      ShortCut = 16457
      OnExecute = actViewTraceInfoExecute
    end
    object actResizeCols: TAction
      Category = 'Edit'
      Caption = 'Resize Col&umns'
      Hint = 'Resize tree columns'
      ShortCut = 16469
      OnExecute = actResizeColsExecute
    end
    object actCloseWin: TAction
      Category = 'Edit'
      Caption = 'Close current &Window'
      Hint = 'Close current Window'
      ShortCut = 16499
      OnExecute = actCloseWinExecute
    end
    object actToggleBookmark: TAction
      Category = 'Edit'
      Caption = 'Toggle Bookmark'
      Hint = 'Toggle bookmark'
      ImageIndex = 24
      ShortCut = 16450
      OnExecute = actToggleBookmarkExecute
    end
    object actPreviousBookmark: TAction
      Category = 'Edit'
      Caption = 'P&revious Bookmark'
      Hint = 'Go to previous bookmark'
      ImageIndex = 26
      ShortCut = 8307
      OnExecute = actPreviousBookmarkExecute
    end
    object actNextBookmark: TAction
      Category = 'Edit'
      Caption = 'Next &Bookmark'
      Hint = 'Go to next bookmark'
      ImageIndex = 27
      ShortCut = 115
      OnExecute = actNextBookmarkExecute
    end
    object actClearBookmarks: TAction
      Category = 'Edit'
      Caption = 'Clear Bookmarks'
      Hint = 'Clear all bookmarks'
      ImageIndex = 28
      OnExecute = actClearBookmarksExecute
    end
    object actSearch: TAction
      Category = 'Edit'
      Caption = '&Find...'
      Hint = 'Find...'
      ImageIndex = 19
      ShortCut = 16454
      OnExecute = actSearchExecute
    end
    object actFindNext: TAction
      Category = 'Edit'
      Caption = 'Find &Next'
      Hint = 'Find next'
      ImageIndex = 30
      ShortCut = 114
      OnExecute = actFindNextExecute
    end
    object actFindPrevious: TAction
      Category = 'Edit'
      Caption = 'Find Previous'
      Hint = 'Find previous'
      ImageIndex = 29
      ShortCut = 8306
      OnExecute = actFindPreviousExecute
    end
    object actClearHighlight: TAction
      Category = 'Edit'
      Caption = 'Clear Search &Highlight'
      Hint = 'Clear search highlight'
      ImageIndex = 22
      OnExecute = actClearHighlightExecute
    end
    object actFilter: TAction
      Category = 'Edit'
      Caption = 'Filte&r...'
      Hint = 'Filter...'
      ImageIndex = 23
      ShortCut = 16455
      OnExecute = actFilterExecute
    end
    object actClearFilter: TAction
      Category = 'Edit'
      Caption = 'Clear Filter'
      Hint = 'Clear filter'
      ImageIndex = 20
      OnExecute = actClearFilterExecute
    end
    object actPrint: TAction
      Category = 'Edit'
      Caption = 'actPrint'
      ImageIndex = 41
      ShortCut = 16464
      OnExecute = actPrintExecute
    end
    object actInsert: TAction
      Category = 'Edit'
      Caption = 'Add line'
      ImageIndex = 42
      OnExecute = actInsertExecute
    end
  end
  object MainMenu: TMainMenu
    Images = Frm_Tool.ilActions
    Left = 24
    Top = 104
    object MnuAction: TMenuItem
      Caption = '&Actions'
      GroupIndex = 1
      object CutSelectedLines1: TMenuItem
        Action = actCut
        GroupIndex = 1
      end
      object CopySelectedLines1: TMenuItem
        Action = actCopy
        GroupIndex = 1
      end
      object DeleteSelected1: TMenuItem
        Action = actDelete
        GroupIndex = 1
      end
      object SelectAll1: TMenuItem
        Action = actSelectAll
        GroupIndex = 1
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object oggleBookmark1: TMenuItem
        Action = actToggleBookmark
        GroupIndex = 1
      end
      object PreviousBookmark1: TMenuItem
        Action = actPreviousBookmark
        GroupIndex = 1
      end
      object actNextBookmark1: TMenuItem
        Action = actNextBookmark
        GroupIndex = 1
      end
      object actClearBookmarks1: TMenuItem
        Action = actClearBookmarks
        GroupIndex = 1
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object actSearch1: TMenuItem
        Action = actSearch
        GroupIndex = 1
      end
      object actFindPrevious1: TMenuItem
        Action = actFindPrevious
        GroupIndex = 1
      end
      object actFindNext1: TMenuItem
        Action = actFindNext
        GroupIndex = 1
      end
      object ClearHighlight1: TMenuItem
        Action = actClearHighlight
        GroupIndex = 1
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object actFilter1: TMenuItem
        Action = actFilter
        GroupIndex = 1
      end
      object ClearFilter1: TMenuItem
        Action = actClearFilter
        GroupIndex = 1
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object actResizeCols1: TMenuItem
        Action = actResizeCols
        GroupIndex = 1
      end
      object ViewTraceInfo1: TMenuItem
        Action = actViewTraceInfo
        GroupIndex = 1
      end
      object Property1: TMenuItem
        Action = actViewProperty
        GroupIndex = 1
      end
      object actClearFileContent1: TMenuItem
        Action = actClearFileContent
        GroupIndex = 1
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object Pause1: TMenuItem
        Action = actPause
        GroupIndex = 1
      end
      object Save1: TMenuItem
        Action = actSaveToFile
        GroupIndex = 1
      end
      object ClearWindow1: TMenuItem
        Action = actClear
        GroupIndex = 1
      end
      object Close1: TMenuItem
        Action = actCloseWin
        GroupIndex = 1
      end
      object actPrint1: TMenuItem
        Action = actPrint
        Caption = '&Print'
        GroupIndex = 1
        ShortCut = 49232
      end
    end
  end
end
