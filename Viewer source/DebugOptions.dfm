object frmDebugOptions: TfrmDebugOptions
  Left = 334
  Top = 199
  BorderStyle = bsSizeToolWin
  Caption = 'TraceTool Options'
  ClientHeight = 540
  ClientWidth = 612
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 612
    Height = 495
    Align = alClient
    TabOrder = 0
    object PanelRight: TPanel
      Left = 617
      Top = 1
      Width = 0
      Height = 493
      Align = alClient
      TabOrder = 0
    end
    object PageControl: TPageControl
      Left = 201
      Top = 1
      Width = 416
      Height = 493
      ActivePage = TabSheetClipboard
      Align = alLeft
      TabOrder = 1
      object TabSheetGeneral: TTabSheet
        Caption = 'General'
        ImageIndex = 2
        object PnlGeneral: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            408
            465)
          object GroupBox2: TGroupBox
            Left = 6
            Top = 0
            Width = 395
            Height = 225
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Display options'
            TabOrder = 0
            DesignSize = (
              395
              225)
            object Label3: TLabel
              Left = 7
              Top = 24
              Width = 72
              Height = 13
              Caption = 'TraceTool Title'
            end
            object Label13: TLabel
              Left = 8
              Top = 68
              Width = 81
              Height = 13
              Caption = 'Optional icon file '
            end
            object Label23: TLabel
              Left = 21
              Top = 131
              Width = 363
              Height = 13
              Caption = 
                ' To hide the viewer, set the HideViewer tag to True in the Trace' +
                'Tool XML file'
            end
            object chkMinmizeSystray: TCheckBox
              Left = 7
              Top = 194
              Width = 233
              Height = 17
              Caption = 'Minmize to Systray'
              TabOrder = 4
            end
            object EditDebugTitle: TEdit
              Left = 7
              Top = 43
              Width = 379
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              Text = 'EditDebugTitle'
            end
            object chkShowOnStartup: TCheckBox
              Left = 7
              Top = 112
              Width = 360
              Height = 17
              Caption = 'Show tracetool screen on startup.'
              TabOrder = 1
            end
            object chkShowOnMessage: TCheckBox
              Left = 7
              Top = 151
              Width = 360
              Height = 17
              Caption = 
                'Show tracetool screen on message received (traces,ODS,tail,Event' +
                'Log)'
              TabOrder = 2
            end
            object chkFocus_OnMessage: TCheckBox
              Left = 7
              Top = 172
              Width = 360
              Height = 17
              Caption = 'Focus to last received message (traces,ODS,tail,EventLog)'
              TabOrder = 3
            end
            object EditIconFile: TEdit
              Left = 7
              Top = 87
              Width = 379
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 5
              Text = 'EditIconFile'
            end
          end
          object GroupBox3: TGroupBox
            Left = 8
            Top = 237
            Width = 395
            Height = 89
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Toolbar'
            TabOrder = 1
            object ChkSmallIcons: TCheckBox
              Left = 7
              Top = 20
              Width = 360
              Height = 17
              Caption = 'Show small toolbar Icons '
              TabOrder = 0
            end
            object chkStandardToolbar: TCheckBox
              Left = 7
              Top = 44
              Width = 145
              Height = 17
              Caption = 'Standard Toolbar'
              TabOrder = 1
            end
            object chkSearchToolbar: TCheckBox
              Left = 7
              Top = 66
              Width = 145
              Height = 17
              Caption = 'Search Toolbar'
              TabOrder = 2
            end
            object chkBookmarkToolbar: TCheckBox
              Left = 176
              Top = 44
              Width = 185
              Height = 17
              Caption = 'Bookmark Toolbar'
              TabOrder = 3
            end
            object chkFilterToolbar: TCheckBox
              Left = 176
              Top = 66
              Width = 177
              Height = 17
              Caption = 'Filter Toolbar'
              TabOrder = 4
            end
          end
          object GroupBox4: TGroupBox
            Left = 8
            Top = 336
            Width = 395
            Height = 97
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Special options'
            TabOrder = 2
            object Label60: TLabel
              Left = 19
              Top = 47
              Width = 297
              Height = 13
              Caption = 'To enter debug mode, run tracetool with the /Debug parameter'
            end
            object Label61: TLabel
              Left = 19
              Top = 66
              Width = 186
              Height = 13
              Caption = '( "Internal Trace" windows is displayed)'
            end
            object chkEnableInternalLog: TCheckBox
              Left = 7
              Top = 24
              Width = 358
              Height = 17
              Caption = 'Enable internal log'
              TabOrder = 0
            end
          end
        end
      end
      object TabSheetFramework: TTabSheet
        Caption = 'Framework'
        ImageIndex = 4
        object PnlFramework: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            408
            465)
          object Label19: TLabel
            Left = 10
            Top = 32
            Width = 74
            Height = 13
            Caption = 'Main traces title'
          end
          object chkShowMainformMenu: TCheckBox
            Left = 10
            Top = 8
            Width = 241
            Height = 17
            Caption = 'Show '#39'main traces'#39' in window menu'
            TabOrder = 0
          end
          object EditMainTrace: TEdit
            Left = 106
            Top = 29
            Width = 217
            Height = 21
            TabOrder = 1
          end
          object GroupBox1: TGroupBox
            Left = 2
            Top = 59
            Width = 395
            Height = 242
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Socket options (set to 0 to disable port)'
            TabOrder = 2
            object Label7: TLabel
              Left = 7
              Top = 24
              Width = 64
              Height = 13
              Caption = 'Socket port 1'
            end
            object Label8: TLabel
              Left = 7
              Top = 220
              Width = 350
              Height = 13
              Caption = 
                'Note : You must restart the viewer AND all the clients when chan' +
                'ging port '
            end
            object Label32: TLabel
              Left = 7
              Top = 133
              Width = 50
              Height = 13
              Caption = 'HTTP port'
            end
            object Label57: TLabel
              Left = 7
              Top = 50
              Width = 64
              Height = 13
              Caption = 'Socket port 2'
            end
            object Label58: TLabel
              Left = 7
              Top = 170
              Width = 270
              Height = 13
              Caption = 'Flash crossdomain.xml or Microsoft clientaccesspolicy.xml'
            end
            object Label59: TLabel
              Left = 7
              Top = 202
              Width = 308
              Height = 13
              Caption = 
                'Note : Policy files must be placed in the same folder as the vie' +
                'wer'
            end
            object Shape1: TShape
              Left = 7
              Top = 193
              Width = 370
              Height = 1
            end
            object Shape2: TShape
              Left = 7
              Top = 122
              Width = 370
              Height = 1
            end
            object EditSocketPort: TEdit
              Left = 84
              Top = 21
              Width = 121
              Height = 21
              TabOrder = 0
              Text = 'EditSocketPort'
            end
            object chkWarningSocket: TCheckBox
              Left = 7
              Top = 95
              Width = 360
              Height = 17
              Caption = 
                'Display warnings when socket connections are not closed properly' +
                '.'
              TabOrder = 2
            end
            object EditHTTPPort: TEdit
              Left = 83
              Top = 129
              Width = 121
              Height = 21
              TabOrder = 1
              Text = 'EditHTTPPort'
            end
            object EditSocketPort2: TEdit
              Left = 83
              Top = 48
              Width = 121
              Height = 21
              TabOrder = 3
              Text = 'EditSocketPort2'
            end
            object chkSocketPolicyServer: TCheckBox
              Left = 7
              Top = 75
              Width = 369
              Height = 17
              Caption = 'Enable Sockets Policy Server (Port 943) : clientaccesspolicy.xml'
              TabOrder = 4
            end
            object chkHttpPolicyServer: TCheckBox
              Left = 7
              Top = 152
              Width = 337
              Height = 17
              Caption = 'Enable Http Policy Server :'
              TabOrder = 5
            end
            object chkUdp1: TCheckBox
              Left = 224
              Top = 22
              Width = 97
              Height = 17
              Caption = 'UDP'
              TabOrder = 6
            end
            object chkUdp2: TCheckBox
              Left = 224
              Top = 50
              Width = 97
              Height = 17
              Caption = 'UDP'
              TabOrder = 7
            end
          end
          object GroupBox5: TGroupBox
            Left = 2
            Top = 307
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 3
            DesignSize = (
              395
              73)
            object Label24: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label25: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object Label26: TLabel
              Left = 256
              Top = 48
              Width = 60
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 258
            end
            object FrameworkTraceFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnChange = FontsChange
              OnDrawItem = FontsDrawItem
            end
            object FrameworkTraceNodeHeight: TEdit
              Left = 328
              Top = 44
              Width = 47
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object FrameworkTraceFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox6: TGroupBox
            Left = 2
            Top = 387
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 4
            DesignSize = (
              395
              73)
            object Label27: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label28: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object Label29: TLabel
              Left = 152
              Top = 48
              Width = 157
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Default node height when no text'
              ExplicitLeft = 154
            end
            object FrameworkInfoFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object FrameworkInfoNodeHeight: TEdit
              Left = 328
              Top = 44
              Width = 47
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object FrameworkInfoFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Delete'
        ImageIndex = 8
        object PnlFrameworkDelete: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 24
            Top = 43
            Width = 73
            Height = 13
            Caption = 'Max root nodes'
          end
          object Label2: TLabel
            Left = 24
            Top = 66
            Width = 70
            Height = 13
            Caption = 'Min root nodes'
          end
          object Label38: TLabel
            Left = 8
            Top = 96
            Width = 354
            Height = 13
            Caption = 
              'Orphans  : When a non existing (deleted) trace node is updated b' +
              'y the API '
          end
          object Label44: TLabel
            Left = 8
            Top = 256
            Width = 226
            Height = 13
            Caption = 'Default texts when a node need to be recreated'
          end
          object Label50: TLabel
            Left = 24
            Top = 283
            Width = 18
            Height = 13
            Caption = 'Left'
          end
          object Label51: TLabel
            Left = 24
            Top = 307
            Width = 25
            Height = 13
            Caption = 'Right'
          end
          object Label52: TLabel
            Left = 8
            Top = 344
            Width = 168
            Height = 13
            Caption = 'Texts for the "LostAndFound" node'
          end
          object Label53: TLabel
            Left = 24
            Top = 371
            Width = 18
            Height = 13
            Caption = 'Left'
          end
          object Label54: TLabel
            Left = 24
            Top = 395
            Width = 25
            Height = 13
            Caption = 'Right'
          end
          object Label55: TLabel
            Left = 8
            Top = 112
            Width = 313
            Height = 13
            Caption = 
              '(like Resend, Append and Focus functions), or children have to b' +
              'e'
          end
          object Label56: TLabel
            Left = 8
            Top = 128
            Width = 106
            Height = 13
            Caption = 'added under this node'
          end
          object chkAutoClearTraces: TCheckBox
            Left = 8
            Top = 16
            Width = 257
            Height = 17
            Caption = 'Enabled automatic clear old messages (root)'
            TabOrder = 0
          end
          object editMaxNodesTraces: TEdit
            Left = 106
            Top = 40
            Width = 111
            Height = 21
            TabOrder = 1
          end
          object EditMinNodesTraces: TEdit
            Left = 106
            Top = 64
            Width = 111
            Height = 21
            TabOrder = 2
          end
          object rbCreateOnRoot: TRadioButton
            Left = 8
            Top = 152
            Width = 385
            Height = 17
            Caption = 'Recreate the node on the root'
            TabOrder = 3
          end
          object rbCreateUnderLostAndFound: TRadioButton
            Left = 8
            Top = 176
            Width = 385
            Height = 17
            Caption = 'Recreate the node under the main "LostAndFound" item'
            TabOrder = 4
          end
          object DefaultLeftText: TEdit
            Left = 80
            Top = 280
            Width = 305
            Height = 21
            TabOrder = 5
          end
          object DefaultRightText: TEdit
            Left = 80
            Top = 304
            Width = 305
            Height = 21
            TabOrder = 6
          end
          object LostAndFoundLeftText: TEdit
            Left = 80
            Top = 368
            Width = 305
            Height = 21
            TabOrder = 7
          end
          object LostAndFoundRightText: TEdit
            Left = 80
            Top = 392
            Width = 305
            Height = 21
            TabOrder = 8
          end
          object rbAddChildrenOnRoot: TRadioButton
            Left = 8
            Top = 200
            Width = 385
            Height = 17
            Caption = 'Ignore updates. Children are added on root'
            TabOrder = 9
          end
          object rbAddChildrenUnderLostAndFound: TRadioButton
            Left = 8
            Top = 224
            Width = 385
            Height = 17
            Caption = 
              'Ignore updates. Children are added under the main "LostAndFound"' +
              ' item'
            TabOrder = 10
          end
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Watches'
        ImageIndex = 7
        object pnlWatches: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            408
            465)
          object Label21: TLabel
            Left = 8
            Top = 40
            Width = 85
            Height = 13
            Caption = 'Main watches title'
          end
          object chkShowMainWatchesMenu: TCheckBox
            Left = 8
            Top = 16
            Width = 241
            Height = 17
            Caption = 'Show '#39'main watches'#39' in window menu'
            TabOrder = 0
          end
          object EditMainWatches: TEdit
            Left = 104
            Top = 37
            Width = 217
            Height = 21
            TabOrder = 1
          end
          object GroupBox7: TGroupBox
            Left = 6
            Top = 288
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 2
            DesignSize = (
              395
              73)
            object Label15: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label16: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object Label17: TLabel
              Left = 256
              Top = 48
              Width = 60
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 258
            end
            object WatchesTraceFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object WatchesTraceNodeHeight: TEdit
              Left = 328
              Top = 44
              Width = 47
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object WatchesTraceFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox8: TGroupBox
            Left = 6
            Top = 368
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 3
            DesignSize = (
              395
              73)
            object Label30: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label31: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object WatchesInfoFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object WatchesInfoFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
        end
      end
      object TabSheetODS: TTabSheet
        Caption = 'OutputDebugString'
        ImageIndex = 5
        object PnlODS: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            408
            465)
          object Label9: TLabel
            Left = 27
            Top = 114
            Width = 52
            Height = 13
            Caption = 'Max nodes'
          end
          object Label10: TLabel
            Left = 27
            Top = 138
            Width = 49
            Height = 13
            Caption = 'Min nodes'
          end
          object Label18: TLabel
            Left = 8
            Top = 40
            Width = 64
            Height = 13
            Caption = 'Tabsheet title'
          end
          object chkAutoClearODS: TCheckBox
            Left = 8
            Top = 80
            Width = 257
            Height = 17
            Caption = 'Enabled automatic clear old node'
            TabOrder = 0
          end
          object editMaxNodesODS: TEdit
            Left = 106
            Top = 112
            Width = 111
            Height = 21
            TabOrder = 1
          end
          object EditMinNodesODS: TEdit
            Left = 106
            Top = 136
            Width = 111
            Height = 21
            TabOrder = 2
          end
          object EditODSTabsheetTitle: TEdit
            Left = 104
            Top = 37
            Width = 289
            Height = 21
            TabOrder = 3
          end
          object chkOdsVisible: TCheckBox
            Left = 8
            Top = 16
            Width = 233
            Height = 17
            Caption = 'Show in '#39'Windows'#39' menu'
            TabOrder = 4
          end
          object GroupBox9: TGroupBox
            Left = 6
            Top = 288
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 5
            DesignSize = (
              395
              73)
            object Label33: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label34: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object Label35: TLabel
              Left = 256
              Top = 48
              Width = 60
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 258
            end
            object OdsTraceFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object OdsTraceNodeHeight: TEdit
              Left = 328
              Top = 44
              Width = 47
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object OdsTraceFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox10: TGroupBox
            Left = 6
            Top = 368
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 6
            DesignSize = (
              395
              73)
            object Label36: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label37: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object OdsInfoFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object OdsInfoFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
        end
      end
      object TabSheetTail: TTabSheet
        Caption = 'Tail windows'
        ImageIndex = 6
        object PnlTail: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            408
            465)
          object Label11: TLabel
            Left = 27
            Top = 114
            Width = 52
            Height = 13
            Caption = 'Max nodes'
          end
          object Label12: TLabel
            Left = 27
            Top = 138
            Width = 49
            Height = 13
            Caption = 'Min nodes'
          end
          object Label14: TLabel
            Left = 8
            Top = 208
            Width = 325
            Height = 13
            Caption = 
              'Max buffer size to use when file is changed after loading (max 6' +
              '5000)'
            Visible = False
          end
          object chkAutoClearTail: TCheckBox
            Left = 8
            Top = 80
            Width = 257
            Height = 17
            Caption = 'Enabled automatic clear old node'
            TabOrder = 0
          end
          object editMaxNodesTail: TEdit
            Left = 106
            Top = 112
            Width = 111
            Height = 21
            TabOrder = 1
          end
          object EditMinNodesTail: TEdit
            Left = 106
            Top = 136
            Width = 111
            Height = 21
            TabOrder = 2
          end
          object EditTailFileSize: TEdit
            Left = 8
            Top = 232
            Width = 121
            Height = 21
            TabOrder = 3
            Text = 'EditTailFileSize'
            Visible = False
          end
          object chkTailVisible: TCheckBox
            Left = 8
            Top = 16
            Width = 233
            Height = 17
            Caption = 'Show in '#39'Windows'#39' menu'
            TabOrder = 4
          end
          object GroupBox11: TGroupBox
            Left = 6
            Top = 288
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 5
            DesignSize = (
              395
              73)
            object Label39: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label40: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object Label41: TLabel
              Left = 256
              Top = 48
              Width = 60
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 258
            end
            object TailTraceFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object TailTraceNodeHeight: TEdit
              Left = 328
              Top = 44
              Width = 47
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object TailTraceFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox12: TGroupBox
            Left = 6
            Top = 368
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 6
            DesignSize = (
              395
              73)
            object Label42: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label43: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object TailInfoFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object TailInfoFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
        end
      end
      object TabSheetEventLog: TTabSheet
        Caption = 'Event Log'
        ImageIndex = 7
        object PnlEventLog: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            408
            465)
          object chkEventLogVisible: TCheckBox
            Left = 8
            Top = 16
            Width = 233
            Height = 17
            Caption = 'Show in '#39'Windows'#39' menu'
            TabOrder = 0
          end
          object GroupBox13: TGroupBox
            Left = 6
            Top = 288
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 1
            DesignSize = (
              395
              73)
            object Label45: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label46: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object Label47: TLabel
              Left = 256
              Top = 48
              Width = 60
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 258
            end
            object EvntLogTraceFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object EvntLogTraceNodeHeight: TEdit
              Left = 328
              Top = 44
              Width = 47
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object EvntLogTraceFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox14: TGroupBox
            Left = 6
            Top = 368
            Width = 395
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 2
            DesignSize = (
              395
              73)
            object Label48: TLabel
              Left = 7
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Font name'
            end
            object Label49: TLabel
              Left = 7
              Top = 48
              Width = 42
              Height = 13
              Caption = 'Font size'
            end
            object EvntLogInfoFonts: TComboBox
              Left = 64
              Top = 19
              Width = 313
              Height = 22
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object EvntLogInfoFontSize: TComboBox
              Left = 64
              Top = 44
              Width = 47
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
        end
      end
      object TabSheetClipboard: TTabSheet
        Caption = 'Clipboard export'
        ImageIndex = 1
        object PnlClipboard: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label4: TLabel
            Left = 8
            Top = 128
            Width = 315
            Height = 39
            Caption = 
              'Column separator  : integer or any characters like '#39','#39' or '#39';'#39' or' +
              ' a space'#13#10'9 = Tabulation'#13#10'13 = Line feed'
          end
          object Label5: TLabel
            Left = 8
            Top = 208
            Width = 60
            Height = 13
            Caption = 'Text qualifier'
          end
          object Label6: TLabel
            Left = 120
            Top = 81
            Width = 177
            Height = 13
            Caption = 'Number of spaces for tree indentation'
          end
          object chkProcessName: TCheckBox
            Left = 8
            Top = 8
            Width = 150
            Height = 17
            Caption = 'Process Name'
            TabOrder = 0
          end
          object ChkThreadID: TCheckBox
            Left = 8
            Top = 32
            Width = 150
            Height = 17
            Caption = 'Thread ID'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object ChkTime: TCheckBox
            Left = 8
            Top = 56
            Width = 150
            Height = 17
            Caption = 'Time'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object EditSeparator: TEdit
            Left = 96
            Top = 146
            Width = 49
            Height = 21
            TabOrder = 3
            Text = '9'
          end
          object ChkTree: TCheckBox
            Left = 8
            Top = 80
            Width = 105
            Height = 17
            Caption = 'Tree message'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object ChkComment: TCheckBox
            Left = 8
            Top = 104
            Width = 150
            Height = 17
            Caption = 'Second column message'
            Checked = True
            State = cbChecked
            TabOrder = 5
          end
          object rbSingle: TRadioButton
            Left = 8
            Top = 232
            Width = 113
            Height = 17
            Caption = 'Use simple quote'
            TabOrder = 6
          end
          object rbDouble: TRadioButton
            Left = 8
            Top = 256
            Width = 113
            Height = 17
            Caption = 'Use double quote'
            TabOrder = 7
          end
          object rbNone: TRadioButton
            Left = 8
            Top = 280
            Width = 113
            Height = 17
            Caption = 'None'
            Checked = True
            TabOrder = 8
            TabStop = True
          end
          object chkIncludeTitle: TCheckBox
            Left = 8
            Top = 176
            Width = 185
            Height = 17
            Caption = 'Include column title in first row'
            TabOrder = 9
          end
          object EditIndentation: TEdit
            Left = 312
            Top = 78
            Width = 49
            Height = 21
            TabOrder = 10
            Text = '3'
          end
        end
      end
      object TabSheetPlugins: TTabSheet
        Caption = 'Plugins'
        ImageIndex = 3
        object PnlPlugins: TPanel
          Left = 0
          Top = 0
          Width = 408
          Height = 465
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object ButNewWin32Plugin: TButton
            Left = 8
            Top = 8
            Width = 121
            Height = 25
            Caption = 'Add Win32 plugin'
            TabOrder = 0
            OnClick = ButNewWin32PluginClick
          end
          object ButNewDotNetPlugin: TButton
            Left = 8
            Top = 48
            Width = 121
            Height = 25
            Caption = 'Add Dot Net plugin'
            TabOrder = 1
            OnClick = ButNewDotNetPluginClick
          end
        end
      end
    end
    object VSTOptions: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 200
      Height = 493
      Align = alLeft
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Height = 17
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      Images = Frm_Tool.ilActions
      TabOrder = 2
      TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect, toSiblingSelectConstraint, toSimpleDrawSelection]
      OnChange = VSTOptionsChange
      OnGetText = VSTOptionsGetText
      OnGetImageIndex = VSTOptionsGetImageIndex
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <>
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 495
    Width = 612
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      612
      45)
    object StatusBar1: TStatusBar
      Left = 0
      Top = 0
      Width = 612
      Height = 45
      Align = alClient
      Panels = <>
      SimplePanel = True
    end
    object btnOK: TButton
      Left = 8
      Top = 12
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object butApply: TButton
      Left = 104
      Top = 12
      Width = 75
      Height = 25
      Caption = '&Apply'
      TabOrder = 1
      OnClick = butApplyClick
    end
    object btnCancel: TButton
      Left = 510
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
      OnClick = btnCancelClick
    end
  end
end
