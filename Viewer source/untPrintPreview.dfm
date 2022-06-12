object FrmPrintPreview: TFrmPrintPreview
  Left = 327
  Top = 252
  Caption = 'Print Preview'
  ClientHeight = 564
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 0
    Top = 2
    Width = 2
    Height = 521
    MinSize = 110
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 800
    Height = 2
    Align = alTop
    ExplicitWidth = 792
  end
  object PageControl: TPageControl
    Left = 2
    Top = 2
    Width = 798
    Height = 521
    ActivePage = tabParameters
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    object tabParameters: TTabSheet
      Caption = 'Parameters'
      DesignSize = (
        790
        492)
      object Label2: TLabel
        Left = 8
        Top = 47
        Width = 109
        Height = 14
        Caption = 'Select columns to print'
      end
      object TempImage: TImage
        Left = 424
        Top = 67
        Width = 16
        Height = 16
        Stretch = True
        Transparent = True
        Visible = False
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 790
        Height = 41
        Align = alTop
        TabOrder = 0
        object ButSelectPrinter: TButton
          Left = 2
          Top = 8
          Width = 97
          Height = 25
          Caption = 'SelectPrinter'
          TabOrder = 0
          OnClick = ButSelectPrinterClick
        end
        object ButPreview: TButton
          Left = 105
          Top = 8
          Width = 75
          Height = 25
          Caption = '> Pre&view'
          Default = True
          TabOrder = 1
          Visible = False
          OnClick = ButPreviewClick
        end
      end
      object ColumList: TCheckListBox
        Left = 24
        Top = 67
        Width = 394
        Height = 190
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 14
        TabOrder = 1
        OnClick = ColumListClick
      end
      object rbPrintAll: TRadioButton
        Left = 8
        Top = 272
        Width = 113
        Height = 17
        Caption = 'PrintAll'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object rbSelected: TRadioButton
        Left = 8
        Top = 291
        Width = 113
        Height = 17
        Caption = 'Print selected lines'
        TabOrder = 3
      end
      object butPrintChildren: TCheckBox
        Left = 24
        Top = 312
        Width = 97
        Height = 17
        Caption = 'Print children'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object chkPrintColumnSeparator: TCheckBox
        Left = 217
        Top = 272
        Width = 201
        Height = 17
        Caption = 'Print column separator'
        TabOrder = 5
      end
      object chkTitlleOnEachPage: TCheckBox
        Left = 233
        Top = 318
        Width = 201
        Height = 17
        Caption = 'On each page'
        TabOrder = 6
      end
      object chkTitlle: TCheckBox
        Left = 217
        Top = 295
        Width = 201
        Height = 17
        Caption = 'Print header'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
    end
    object TabPreview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 193
        Top = 39
        Width = 597
        Height = 453
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object PageNavigator: TTabSet
          Left = 0
          Top = 426
          Width = 597
          Height = 27
          Align = alBottom
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          OnChange = PageNavigatorChange
        end
        object PrintPreview: TPrintPreview
          Left = 0
          Top = 0
          Width = 597
          Height = 426
          HorzScrollBar.Margin = 10
          HorzScrollBar.Tracking = True
          VertScrollBar.Margin = 10
          VertScrollBar.Tracking = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          TabOrder = 1
          Annotation = True
          Background = True
          PrintJobTitle = 'TraceTool'
          OnBeginDoc = PrintPreviewBeginDoc
          OnEndDoc = PrintPreviewEndDoc
          OnNewPage = PrintPreviewNewPage
          OnChange = PrintPreviewChange
          OnPrintProgress = PrintPreviewPrintProgress
          OnBeforePrint = PrintPreviewBeforePrint
          OnAfterPrint = PrintPreviewAfterPrint
          OnZoomChange = PrintPreviewZoomChange
          OnAnnotation = PrintPreviewAnnotation
        end
      end
      object Toolbar: TPanel
        Left = 0
        Top = 0
        Width = 790
        Height = 39
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 13
          Width = 30
          Height = 14
          Alignment = taRightJustify
          Caption = 'Zoom:'
          FocusControl = ZoomComboBox
        end
        object ZoomComboBox: TComboBox
          Left = 41
          Top = 9
          Width = 97
          Height = 22
          Style = csDropDownList
          TabOrder = 0
          OnChange = ZoomComboBoxChange
          Items.Strings = (
            '%50'
            '%100'
            '%150'
            '%200'
            'Page Width'
            'Page Height'
            'Whole Page')
        end
        object Grayscale: TCheckBox
          Left = 518
          Top = 0
          Width = 97
          Height = 17
          Caption = 'Grayscale'
          TabOrder = 1
          OnClick = GrayscaleClick
        end
        object AnnotationCheckBox: TCheckBox
          Left = 518
          Top = 16
          Width = 187
          Height = 17
          Caption = 'Show Printer'#39's printable boundary'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = AnnotationCheckBoxClick
        end
      end
      object ThumbnailPreview1: TThumbnailPreview
        Left = 0
        Top = 39
        Width = 193
        Height = 453
        TabOrder = 2
        PrintPreview = PrintPreview
        PaperView.ShadowWidth = 1
        Zoom = 20
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 523
    Width = 800
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      800
      41)
    object ButCancel: TButton
      Left = 705
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object PrintButton: TButton
      Left = 14
      Top = 10
      Width = 105
      Height = 24
      Caption = 'Print'
      Enabled = False
      TabOrder = 1
      OnClick = PrintButtonClick
    end
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 255
    Top = 1
  end
end
