object Form4: TForm4
  Left = 389
  Top = 236
  Caption = 'Form4'
  ClientHeight = 504
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 778
    Height = 463
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Basic'
      object Label1: TLabel
        Left = 176
        Top = 354
        Width = 22
        Height = 13
        Caption = 'Host'
      end
      object Label2: TLabel
        Left = 176
        Top = 378
        Width = 19
        Height = 13
        Caption = 'Port'
      end
      object butSimpleTest: TButton
        Left = 8
        Top = 16
        Width = 121
        Height = 25
        Caption = 'Simple test'
        TabOrder = 0
        OnClick = butSimpleTestClick
      end
      object butLongTest: TButton
        Left = 8
        Top = 64
        Width = 169
        Height = 25
        Caption = 'Send many nodes then Flush'
        TabOrder = 1
        OnClick = butLongTestClick
      end
      object ButIndent: TButton
        Left = 8
        Top = 104
        Width = 169
        Height = 25
        Caption = 'Indent/UnIndent'
        TabOrder = 2
        OnClick = ButIndentClick
      end
      object butSaveToText: TButton
        Left = 8
        Top = 160
        Width = 169
        Height = 25
        Caption = 'Save to text file ("c:\log.txt")'
        TabOrder = 3
        OnClick = butSaveToTextClick
      end
      object butSaveToXML: TButton
        Left = 8
        Top = 200
        Width = 169
        Height = 25
        Caption = 'Save To Xml file ("c:\log.xml")'
        TabOrder = 4
        OnClick = butSaveToXMLClick
      end
      object butLogFile: TButton
        Left = 8
        Top = 248
        Width = 169
        Height = 25
        Caption = 'setLogFile'
        TabOrder = 5
        OnClick = butLogFileClick
      end
      object butClear: TButton
        Left = 216
        Top = 160
        Width = 145
        Height = 25
        Caption = 'Clear main traces'
        TabOrder = 6
        OnClick = butClearClick
      end
      object butLoadXMLMain: TButton
        Left = 216
        Top = 200
        Width = 145
        Height = 25
        Caption = 'Load XML ('#39'c:\log.xml'#39')'
        TabOrder = 7
        OnClick = butLoadXMLMainClick
      end
      object butWinMsg: TButton
        Left = 16
        Top = 320
        Width = 129
        Height = 25
        Caption = 'Use windows messages'
        TabOrder = 8
        OnClick = butWinMsgClick
      end
      object butSocket: TButton
        Left = 174
        Top = 320
        Width = 139
        Height = 25
        Caption = 'Use socket messages'
        TabOrder = 9
        OnClick = butSocketClick
      end
      object EditHost: TEdit
        Left = 248
        Top = 351
        Width = 121
        Height = 21
        TabOrder = 10
        Text = '127.0.0.1'
      end
      object EditPort: TEdit
        Left = 248
        Top = 378
        Width = 121
        Height = 21
        TabOrder = 11
        Text = '8090'
      end
      object chkUdp: TCheckBox
        Left = 174
        Top = 405
        Width = 177
        Height = 17
        Caption = 'Connection less socket (UDP)'
        TabOrder = 12
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Nodes operations'
      ImageIndex = 1
      object butResend: TButton
        Left = 128
        Top = 24
        Width = 129
        Height = 25
        Caption = 'ResendRight ("Done 1") '
        Enabled = False
        TabOrder = 0
        OnClick = butResendClick
      end
      object butFocus: TButton
        Left = 296
        Top = 64
        Width = 89
        Height = 25
        Caption = 'Show()'
        Enabled = False
        TabOrder = 1
        OnClick = butFocusClick
      end
      object butSetSelected: TButton
        Left = 296
        Top = 24
        Width = 89
        Height = 25
        Caption = 'SetSelected()'
        Enabled = False
        TabOrder = 2
        OnClick = butSetSelectedClick
      end
      object butAppend: TButton
        Left = 128
        Top = 63
        Width = 129
        Height = 25
        Caption = 'AppendLeft ("Done 2")'
        Enabled = False
        TabOrder = 3
        OnClick = butAppendClick
      end
      object butStart1: TButton
        Left = 16
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Start 1...'
        TabOrder = 4
        OnClick = butStart1Click
      end
      object butStart2: TButton
        Left = 16
        Top = 64
        Width = 75
        Height = 25
        Caption = 'Start 2...'
        TabOrder = 5
        OnClick = butStart2Click
      end
      object ButtonCpt: TButton
        Left = 414
        Top = 24
        Width = 75
        Height = 25
        Caption = 'ResendLeft'
        Enabled = False
        TabOrder = 6
        OnClick = ButtonCptClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Multi pages traces'
      ImageIndex = 2
      object butCreateTraceWin: TButton
        Left = 16
        Top = 24
        Width = 193
        Height = 25
        Caption = 'Create a new window trace '
        TabOrder = 0
        OnClick = butCreateTraceWinClick
      end
      object butDisplayWin: TButton
        Left = 16
        Top = 64
        Width = 193
        Height = 25
        Caption = 'Display that window on the viewer'
        Enabled = False
        TabOrder = 1
        OnClick = butDisplayWinClick
      end
      object butHelloToWintrace: TButton
        Left = 16
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Say Hello'
        Enabled = False
        TabOrder = 2
        OnClick = butHelloToWintraceClick
      end
      object butSaveWinToTxt: TButton
        Left = 16
        Top = 192
        Width = 169
        Height = 25
        Caption = 'Save to text file ('#39'c:\log2.txt'#39')'
        Enabled = False
        TabOrder = 3
        OnClick = butSaveWinToTxtClick
      end
      object butSaveWinToXml: TButton
        Left = 16
        Top = 232
        Width = 169
        Height = 25
        Caption = 'Save To Xml file ('#39'c:\log2.xml'#39')'
        Enabled = False
        TabOrder = 4
        OnClick = butSaveWinToXmlClick
      end
      object butClearWin: TButton
        Left = 224
        Top = 192
        Width = 145
        Height = 25
        Caption = 'Clear win traces'
        Enabled = False
        TabOrder = 5
        OnClick = butClearWinClick
      end
      object butLoadXMLWin: TButton
        Left = 224
        Top = 232
        Width = 145
        Height = 25
        Caption = 'Load XML ('#39'c:\log2.xml'#39')'
        Enabled = False
        TabOrder = 6
        OnClick = butLoadXMLWinClick
      end
      object butMultiCol: TButton
        Left = 224
        Top = 26
        Width = 75
        Height = 25
        Caption = 'Multi Col test'
        TabOrder = 7
        OnClick = butMultiColClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Watches'
      ImageIndex = 3
      object Label6: TLabel
        Left = 24
        Top = 48
        Width = 105
        Height = 13
        Caption = 'Main watches window'
      end
      object butWatch: TButton
        Left = 48
        Top = 80
        Width = 137
        Height = 25
        Caption = 'Send Watches'
        TabOrder = 0
        OnClick = butWatchClick
      end
      object butClearWatchWindow: TButton
        Left = 48
        Top = 120
        Width = 137
        Height = 25
        Caption = 'Clear Watch Window'
        TabOrder = 1
        OnClick = butClearWatchWindowClick
      end
      object butDisplayWatchWindow: TButton
        Left = 48
        Top = 160
        Width = 137
        Height = 25
        Caption = 'Display Watch Window'
        TabOrder = 2
        OnClick = butDisplayWatchWindowClick
      end
      object butCreateWinWatch: TButton
        Left = 280
        Top = 40
        Width = 137
        Height = 25
        Caption = 'Create new WinWatch'
        TabOrder = 3
        OnClick = butCreateWinWatchClick
      end
      object butWinWatchSend: TButton
        Left = 312
        Top = 80
        Width = 137
        Height = 25
        Caption = 'Send Watches'
        TabOrder = 4
        OnClick = butWinWatchSendClick
      end
      object butWinWatchClear: TButton
        Left = 312
        Top = 120
        Width = 137
        Height = 25
        Caption = 'Clear Watch Window'
        TabOrder = 5
        OnClick = butWinWatchClearClick
      end
      object butWinWatchDisplay: TButton
        Left = 312
        Top = 160
        Width = 137
        Height = 25
        Caption = 'Display Watch Window'
        TabOrder = 6
        OnClick = butWinWatchDisplayClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 463
    Width = 778
    Height = 41
    Align = alBottom
    TabOrder = 1
    object butShow: TButton
      Left = 32
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Show Viewer'
      TabOrder = 0
      OnClick = butShowClick
    end
  end
end
