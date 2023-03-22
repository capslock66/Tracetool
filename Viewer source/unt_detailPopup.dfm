object DetailPopupForm: TDetailPopupForm
  Left = 0
  Top = 0
  Caption = 'Detail'
  ClientHeight = 369
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline FrameMemo: TFrameMemo
    Left = 0
    Top = 0
    Width = 386
    Height = 369
    Align = alClient
    TabOrder = 0
    ExplicitTop = 29
    ExplicitWidth = 386
    ExplicitHeight = 340
    inherited SynMemo: TSynEdit
      Width = 386
      Height = 340
      ExplicitTop = 29
      ExplicitWidth = 386
      ExplicitHeight = 311
    end
    inherited PanelTop: TPanel
      Width = 386
      ExplicitLeft = 0
    end
  end
end
