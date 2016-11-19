object frmDebugOptions: TfrmDebugOptions
  Left = 334
  Top = 199
  BorderStyle = bsSizeToolWin
  Caption = 'TraceTool Options'
  ClientHeight = 665
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 753
    Height = 609
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    TabOrder = 0
    object PanelRight: TPanel
      Left = 759
      Top = 1
      Width = 0
      Height = 607
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 0
    end
    object PageControl: TPageControl
      Left = 247
      Top = 1
      Width = 512
      Height = 607
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ActivePage = TabSheetGeneral
      Align = alLeft
      TabOrder = 1
      object TabSheetGeneral: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'General'
        ImageIndex = 2
        object PnlGeneral: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object GroupBox2: TGroupBox
            Left = 7
            Top = 0
            Width = 489
            Height = 277
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Display options'
            TabOrder = 0
            DesignSize = (
              489
              277)
            object Label3: TLabel
              Left = 9
              Top = 30
              Width = 93
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'TraceTool Title'
            end
            object Label13: TLabel
              Left = 10
              Top = 84
              Width = 101
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Optional icon file '
            end
            object Label23: TLabel
              Left = 26
              Top = 161
              Width = 447
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 
                ' To hide the viewer, set the HideViewer tag to True in the Trace' +
                'Tool XML file'
            end
            object chkMinmizeSystray: TCheckBox
              Left = 9
              Top = 239
              Width = 286
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Minmize to Systray'
              TabOrder = 4
            end
            object EditDebugTitle: TEdit
              Left = 9
              Top = 53
              Width = 468
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              Text = 'EditDebugTitle'
            end
            object chkShowOnStartup: TCheckBox
              Left = 9
              Top = 138
              Width = 443
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Show tracetool screen on startup.'
              TabOrder = 1
            end
            object chkShowOnMessage: TCheckBox
              Left = 9
              Top = 186
              Width = 443
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 
                'Show tracetool screen on message received (traces,ODS,tail,Event' +
                'Log)'
              TabOrder = 2
            end
            object chkFocus_OnMessage: TCheckBox
              Left = 9
              Top = 212
              Width = 443
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Focus to last received message (traces,ODS,tail,EventLog)'
              TabOrder = 3
            end
            object EditIconFile: TEdit
              Left = 9
              Top = 107
              Width = 468
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 5
              Text = 'EditIconFile'
            end
          end
          object GroupBox3: TGroupBox
            Left = 10
            Top = 292
            Width = 488
            Height = 109
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Toolbar'
            TabOrder = 1
            object ChkSmallIcons: TCheckBox
              Left = 9
              Top = 25
              Width = 443
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Show small toolbar Icons '
              TabOrder = 0
            end
            object chkStandardToolbar: TCheckBox
              Left = 9
              Top = 54
              Width = 178
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Standard Toolbar'
              TabOrder = 1
            end
            object chkSearchToolbar: TCheckBox
              Left = 9
              Top = 81
              Width = 178
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Search Toolbar'
              TabOrder = 2
            end
            object chkBookmarkToolbar: TCheckBox
              Left = 217
              Top = 54
              Width = 227
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Bookmark Toolbar'
              TabOrder = 3
            end
            object chkFilterToolbar: TCheckBox
              Left = 217
              Top = 81
              Width = 217
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Filter Toolbar'
              TabOrder = 4
            end
          end
          object GroupBox4: TGroupBox
            Left = 10
            Top = 414
            Width = 488
            Height = 119
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Special options'
            TabOrder = 2
            object Label60: TLabel
              Left = 23
              Top = 58
              Width = 370
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'To enter debug mode, run tracetool with the /Debug parameter'
            end
            object Label61: TLabel
              Left = 23
              Top = 81
              Width = 235
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = '( "Internal Trace" windows is displayed)'
            end
            object chkEnableInternalLog: TCheckBox
              Left = 9
              Top = 30
              Width = 440
              Height = 20
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Enable internal log'
              TabOrder = 0
            end
          end
        end
      end
      object TabSheetFramework: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Framework'
        ImageIndex = 4
        object PnlFramework: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object Label19: TLabel
            Left = 12
            Top = 39
            Width = 92
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Main traces title'
          end
          object chkShowMainformMenu: TCheckBox
            Left = 12
            Top = 10
            Width = 297
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Show '#39'main traces'#39' in window menu'
            TabOrder = 0
          end
          object EditMainTrace: TEdit
            Left = 130
            Top = 36
            Width = 268
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 1
          end
          object GroupBox1: TGroupBox
            Left = 2
            Top = 73
            Width = 489
            Height = 297
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Socket options (set to 0 to disable port)'
            TabOrder = 2
            object Label7: TLabel
              Left = 9
              Top = 30
              Width = 78
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Socket port 1'
            end
            object Label8: TLabel
              Left = 9
              Top = 271
              Width = 428
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 
                'Note : You must restart the viewer AND all the clients when chan' +
                'ging port '
            end
            object Label32: TLabel
              Left = 9
              Top = 164
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'HTTP port'
            end
            object Label57: TLabel
              Left = 9
              Top = 62
              Width = 78
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Socket port 2'
            end
            object Label58: TLabel
              Left = 9
              Top = 209
              Width = 346
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Flash crossdomain.xml or Microsoft clientaccesspolicy.xml'
            end
            object Label59: TLabel
              Left = 9
              Top = 249
              Width = 386
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 
                'Note : Policy files must be placed in the same folder as the vie' +
                'wer'
            end
            object Shape1: TShape
              Left = 9
              Top = 238
              Width = 455
              Height = 1
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
            end
            object Shape2: TShape
              Left = 9
              Top = 150
              Width = 455
              Height = 1
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
            end
            object EditSocketPort: TEdit
              Left = 103
              Top = 26
              Width = 149
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              TabOrder = 0
              Text = 'EditSocketPort'
            end
            object chkWarningSocket: TCheckBox
              Left = 9
              Top = 117
              Width = 443
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 
                'Display warnings when socket connections are not closed properly' +
                '.'
              TabOrder = 2
            end
            object EditHTTPPort: TEdit
              Left = 102
              Top = 159
              Width = 149
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              TabOrder = 1
              Text = 'EditHTTPPort'
            end
            object EditSocketPort2: TEdit
              Left = 102
              Top = 59
              Width = 149
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              TabOrder = 3
              Text = 'EditSocketPort2'
            end
            object chkSocketPolicyServer: TCheckBox
              Left = 9
              Top = 92
              Width = 454
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Enable Sockets Policy Server (Port 943) : clientaccesspolicy.xml'
              TabOrder = 4
            end
            object chkHttpPolicyServer: TCheckBox
              Left = 9
              Top = 187
              Width = 414
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Enable Http Policy Server :'
              TabOrder = 5
            end
            object chkUdp1: TCheckBox
              Left = 276
              Top = 27
              Width = 119
              Height = 21
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'UDP'
              TabOrder = 6
            end
            object chkUdp2: TCheckBox
              Left = 276
              Top = 62
              Width = 119
              Height = 20
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'UDP'
              TabOrder = 7
            end
          end
          object GroupBox5: TGroupBox
            Left = 2
            Top = 378
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 3
            DesignSize = (
              489
              90)
            object Label24: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label25: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object Label26: TLabel
              Left = 317
              Top = 59
              Width = 76
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 315
            end
            object FrameworkTraceFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnChange = FontsChange
              OnDrawItem = FontsDrawItem
            end
            object FrameworkTraceNodeHeight: TEdit
              Left = 406
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object FrameworkTraceFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox6: TGroupBox
            Left = 2
            Top = 476
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 4
            DesignSize = (
              489
              90)
            object Label27: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label28: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object Label29: TLabel
              Left = 189
              Top = 59
              Width = 190
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Default node height when no text'
              ExplicitLeft = 187
            end
            object FrameworkInfoFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object FrameworkInfoNodeHeight: TEdit
              Left = 406
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object FrameworkInfoFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
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
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Delete'
        ImageIndex = 8
        object PnlFrameworkDelete: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 30
            Top = 53
            Width = 92
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Max root nodes'
          end
          object Label2: TLabel
            Left = 30
            Top = 81
            Width = 88
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Min root nodes'
          end
          object Label38: TLabel
            Left = 10
            Top = 118
            Width = 438
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 
              'Orphans  : When a non existing (deleted) trace node is updated b' +
              'y the API '
          end
          object Label44: TLabel
            Left = 10
            Top = 315
            Width = 279
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Default texts when a node need to be recreated'
          end
          object Label50: TLabel
            Left = 30
            Top = 348
            Width = 21
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Left'
          end
          object Label51: TLabel
            Left = 30
            Top = 378
            Width = 31
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Right'
          end
          object Label52: TLabel
            Left = 10
            Top = 423
            Width = 208
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Texts for the "LostAndFound" node'
          end
          object Label53: TLabel
            Left = 30
            Top = 457
            Width = 21
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Left'
          end
          object Label54: TLabel
            Left = 30
            Top = 486
            Width = 31
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Right'
          end
          object Label55: TLabel
            Left = 10
            Top = 138
            Width = 389
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 
              '(like Resend, Append and Focus functions), or children have to b' +
              'e'
          end
          object Label56: TLabel
            Left = 10
            Top = 158
            Width = 134
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'added under this node'
          end
          object chkAutoClearTraces: TCheckBox
            Left = 10
            Top = 20
            Width = 316
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Enabled automatic clear old messages (root)'
            TabOrder = 0
          end
          object editMaxNodesTraces: TEdit
            Left = 130
            Top = 49
            Width = 137
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 1
          end
          object EditMinNodesTraces: TEdit
            Left = 130
            Top = 79
            Width = 137
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 2
          end
          object rbCreateOnRoot: TRadioButton
            Left = 10
            Top = 187
            Width = 474
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Recreate the node on the root'
            TabOrder = 3
          end
          object rbCreateUnderLostAndFound: TRadioButton
            Left = 10
            Top = 217
            Width = 474
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Recreate the node under the main "LostAndFound" item'
            TabOrder = 4
          end
          object DefaultLeftText: TEdit
            Left = 98
            Top = 345
            Width = 376
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 5
          end
          object DefaultRightText: TEdit
            Left = 98
            Top = 374
            Width = 376
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 6
          end
          object LostAndFoundLeftText: TEdit
            Left = 98
            Top = 453
            Width = 376
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 7
          end
          object LostAndFoundRightText: TEdit
            Left = 98
            Top = 482
            Width = 376
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 8
          end
          object rbAddChildrenOnRoot: TRadioButton
            Left = 10
            Top = 246
            Width = 474
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Ignore updates. Children are added on root'
            TabOrder = 9
          end
          object rbAddChildrenUnderLostAndFound: TRadioButton
            Left = 10
            Top = 276
            Width = 474
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 
              'Ignore updates. Children are added under the main "LostAndFound"' +
              ' item'
            TabOrder = 10
          end
        end
      end
      object TabSheet1: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Watches'
        ImageIndex = 7
        object pnlWatches: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object Label21: TLabel
            Left = 10
            Top = 49
            Width = 104
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Main watches title'
          end
          object chkShowMainWatchesMenu: TCheckBox
            Left = 10
            Top = 20
            Width = 296
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Show '#39'main watches'#39' in window menu'
            TabOrder = 0
          end
          object EditMainWatches: TEdit
            Left = 128
            Top = 46
            Width = 267
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 1
          end
          object GroupBox7: TGroupBox
            Left = 7
            Top = 354
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 2
            DesignSize = (
              489
              90)
            object Label15: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label16: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object Label17: TLabel
              Left = 317
              Top = 59
              Width = 76
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 315
            end
            object WatchesTraceFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object WatchesTraceNodeHeight: TEdit
              Left = 406
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object WatchesTraceFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox8: TGroupBox
            Left = 7
            Top = 453
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 3
            DesignSize = (
              489
              90)
            object Label30: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label31: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object WatchesInfoFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object WatchesInfoFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
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
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'OutputDebugString'
        ImageIndex = 5
        object PnlODS: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object Label9: TLabel
            Left = 33
            Top = 140
            Width = 66
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Max nodes'
          end
          object Label10: TLabel
            Left = 33
            Top = 170
            Width = 62
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Min nodes'
          end
          object Label18: TLabel
            Left = 10
            Top = 49
            Width = 81
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tabsheet title'
          end
          object chkAutoClearODS: TCheckBox
            Left = 10
            Top = 98
            Width = 316
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Enabled automatic clear old node'
            TabOrder = 0
          end
          object editMaxNodesODS: TEdit
            Left = 130
            Top = 138
            Width = 137
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 1
          end
          object EditMinNodesODS: TEdit
            Left = 130
            Top = 167
            Width = 137
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 2
          end
          object EditODSTabsheetTitle: TEdit
            Left = 128
            Top = 46
            Width = 356
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 3
          end
          object chkOdsVisible: TCheckBox
            Left = 10
            Top = 20
            Width = 287
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Show in '#39'Windows'#39' menu'
            TabOrder = 4
          end
          object GroupBox9: TGroupBox
            Left = 7
            Top = 354
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 5
            DesignSize = (
              489
              90)
            object Label33: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label34: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object Label35: TLabel
              Left = 317
              Top = 59
              Width = 76
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 315
            end
            object OdsTraceFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object OdsTraceNodeHeight: TEdit
              Left = 406
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object OdsTraceFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox10: TGroupBox
            Left = 7
            Top = 453
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 6
            DesignSize = (
              489
              90)
            object Label36: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label37: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object OdsInfoFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object OdsInfoFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
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
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Tail windows'
        ImageIndex = 6
        object PnlTail: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object Label11: TLabel
            Left = 33
            Top = 140
            Width = 66
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Max nodes'
          end
          object Label12: TLabel
            Left = 33
            Top = 170
            Width = 62
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Min nodes'
          end
          object Label14: TLabel
            Left = 10
            Top = 256
            Width = 401
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 
              'Max buffer size to use when file is changed after loading (max 6' +
              '5000)'
            Visible = False
          end
          object chkAutoClearTail: TCheckBox
            Left = 10
            Top = 98
            Width = 316
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Enabled automatic clear old node'
            TabOrder = 0
          end
          object editMaxNodesTail: TEdit
            Left = 130
            Top = 138
            Width = 137
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 1
          end
          object EditMinNodesTail: TEdit
            Left = 130
            Top = 167
            Width = 137
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 2
          end
          object EditTailFileSize: TEdit
            Left = 10
            Top = 286
            Width = 149
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 3
            Text = 'EditTailFileSize'
            Visible = False
          end
          object chkTailVisible: TCheckBox
            Left = 10
            Top = 20
            Width = 287
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Show in '#39'Windows'#39' menu'
            TabOrder = 4
          end
          object GroupBox11: TGroupBox
            Left = 7
            Top = 354
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 5
            DesignSize = (
              489
              90)
            object Label39: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label40: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object Label41: TLabel
              Left = 317
              Top = 59
              Width = 76
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 315
            end
            object TailTraceFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object TailTraceNodeHeight: TEdit
              Left = 406
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object TailTraceFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox12: TGroupBox
            Left = 7
            Top = 453
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 6
            DesignSize = (
              489
              90)
            object Label42: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label43: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object TailInfoFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object TailInfoFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
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
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Event Log'
        ImageIndex = 7
        object PnlEventLog: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object chkEventLogVisible: TCheckBox
            Left = 10
            Top = 20
            Width = 287
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Show in '#39'Windows'#39' menu'
            TabOrder = 0
          end
          object GroupBox13: TGroupBox
            Left = 7
            Top = 354
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default trace font'
            TabOrder = 1
            DesignSize = (
              489
              90)
            object Label45: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label46: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object Label47: TLabel
              Left = 317
              Top = 59
              Width = 76
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Node Height'
              ExplicitLeft = 315
            end
            object EvntLogTraceFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object EvntLogTraceNodeHeight: TEdit
              Left = 406
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              Text = '0'
            end
            object EvntLogTraceFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Text = '0'
            end
          end
          object GroupBox14: TGroupBox
            Left = 7
            Top = 453
            Width = 489
            Height = 90
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default Trace Information panel font'
            TabOrder = 2
            DesignSize = (
              489
              90)
            object Label48: TLabel
              Left = 9
              Top = 30
              Width = 63
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font name'
            end
            object Label49: TLabel
              Left = 9
              Top = 59
              Width = 53
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Font size'
            end
            object EvntLogInfoFonts: TComboBox
              Left = 79
              Top = 23
              Width = 387
              Height = 22
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Style = csOwnerDrawVariable
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnDrawItem = FontsDrawItem
            end
            object EvntLogInfoFontSize: TComboBox
              Left = 79
              Top = 54
              Width = 58
              Height = 24
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
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
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Clipboard export'
        ImageIndex = 1
        object PnlClipboard: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label4: TLabel
            Left = 10
            Top = 158
            Width = 395
            Height = 48
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 
              'Column separator  : integer or any characters like '#39','#39' or '#39';'#39' or' +
              ' a space'#13#10'9 = Tabulation'#13#10'13 = Line feed'
          end
          object Label5: TLabel
            Left = 10
            Top = 256
            Width = 76
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Text qualifier'
          end
          object Label6: TLabel
            Left = 148
            Top = 100
            Width = 222
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Number of spaces for tree indentation'
          end
          object chkProcessName: TCheckBox
            Left = 10
            Top = 10
            Width = 184
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Process Name'
            TabOrder = 0
          end
          object ChkThreadID: TCheckBox
            Left = 10
            Top = 39
            Width = 184
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Thread ID'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object ChkTime: TCheckBox
            Left = 10
            Top = 69
            Width = 184
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Time'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object EditSeparator: TEdit
            Left = 118
            Top = 180
            Width = 60
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 3
            Text = '9'
          end
          object ChkTree: TCheckBox
            Left = 10
            Top = 98
            Width = 129
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tree message'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object ChkComment: TCheckBox
            Left = 10
            Top = 128
            Width = 184
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Second column message'
            Checked = True
            State = cbChecked
            TabOrder = 5
          end
          object rbSingle: TRadioButton
            Left = 10
            Top = 286
            Width = 139
            Height = 20
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Use simple quote'
            TabOrder = 6
          end
          object rbDouble: TRadioButton
            Left = 10
            Top = 315
            Width = 139
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Use double quote'
            TabOrder = 7
          end
          object rbNone: TRadioButton
            Left = 10
            Top = 345
            Width = 139
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'None'
            Checked = True
            TabOrder = 8
            TabStop = True
          end
          object chkIncludeTitle: TCheckBox
            Left = 10
            Top = 217
            Width = 228
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Include column title in first row'
            TabOrder = 9
          end
          object EditIndentation: TEdit
            Left = 384
            Top = 96
            Width = 60
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            TabOrder = 10
            Text = '3'
          end
        end
      end
      object TabSheetPlugins: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Plugins'
        ImageIndex = 3
        object PnlPlugins: TPanel
          Left = 0
          Top = 0
          Width = 504
          Height = 576
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            504
            576)
          object Label20: TLabel
            Left = 15
            Top = 231
            Width = 246
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'JVM Engine (Home path, relative JVM dll)'
            Visible = False
          end
          object Label22: TLabel
            Left = 15
            Top = 165
            Width = 134
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Java Plugin ClassPath'
            Visible = False
          end
          object ButNewWin32Plugin: TButton
            Left = 10
            Top = 10
            Width = 149
            Height = 31
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Add Win32 plugin'
            TabOrder = 0
            OnClick = ButNewWin32PluginClick
          end
          object ButNewDotNetPlugin: TButton
            Left = 10
            Top = 59
            Width = 149
            Height = 31
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Add Dot Net plugin'
            TabOrder = 1
            OnClick = ButNewDotNetPluginClick
          end
          object ButNewJavaPlugin: TButton
            Left = 10
            Top = 108
            Width = 149
            Height = 31
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Add Java plugin'
            TabOrder = 2
            Visible = False
            OnClick = ButNewJavaPluginClick
          end
          object ComboJVM: TComboBox
            Left = 15
            Top = 252
            Width = 469
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            Visible = False
            OnChange = ComboJVMChange
            OnDropDown = ComboJVMDropDown
          end
          object EditPluginClassPath: TEdit
            Left = 15
            Top = 188
            Width = 469
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
            Visible = False
            OnChange = EditPluginClassPathChange
          end
          object MemoVMClassPath: TMemo
            Left = 15
            Top = 295
            Width = 434
            Height = 247
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight, akBottom]
            BorderStyle = bsNone
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Lines.Strings = (
              'MemoVMClassPath')
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Visible = False
          end
        end
      end
    end
    object VSTOptions: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 246
      Height = 607
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      DefaultNodeHeight = 22
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -15
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.Height = 21
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
      Columns = <>
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 609
    Width = 753
    Height = 56
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      753
      56)
    object StatusBar1: TStatusBar
      Left = 0
      Top = 0
      Width = 753
      Height = 56
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Panels = <>
      SimplePanel = True
    end
    object btnOK: TButton
      Left = 10
      Top = 15
      Width = 92
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object butApply: TButton
      Left = 128
      Top = 15
      Width = 92
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Apply'
      TabOrder = 1
      OnClick = butApplyClick
    end
    object btnCancel: TButton
      Left = 628
      Top = 15
      Width = 92
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
      OnClick = btnCancelClick
    end
  end
end
