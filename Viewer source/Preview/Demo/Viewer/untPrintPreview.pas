unit untPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Tabs, Preview, Dialogs, jpeg, ComCtrls;

type
  TMainForm = class(TForm)
    Toolbar: TPanel;
    ZoomComboBox: TComboBox;
    Label1: TLabel;
    Image1: TImage;
    UnitComboBox: TComboBox;
    Label2: TLabel;
    Image2: TImage;
    PrinterSetupDialog: TPrinterSetupDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    RichEdit1: TRichEdit;
    Splitter1: TSplitter;
    ThumbnailPreview1: TThumbnailPreview;
    Panel1: TPanel;
    PrintPreview: TPrintPreview;
    PageNavigator: TTabSet;
    FormComboBox: TComboBox;
    Label3: TLabel;
    AnnotationCheckBox: TCheckBox;
    Panel2: TPanel;
    SaveButton: TButton;
    LoadButton: TButton;
    PrintButton: TButton;
    DirectPrintCheckBox: TCheckBox;
    Bevel1: TBevel;
    Grayscale: TCheckBox;
    SavePDFDialog: TSaveDialog;
    SavePDFButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ZoomComboBoxChange(Sender: TObject);
    procedure UnitComboBoxChange(Sender: TObject);
    procedure AnnotationCheckBoxClick(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure PageNavigatorChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PrintPreviewChange(Sender: TObject);
    procedure PrintPreviewBeforePrint(Sender: TObject);
    procedure PrintPreviewPrintProgress(Sender: TObject; PageNum,
      Progress: Integer; var AbortIt: Boolean);
    procedure PrintPreviewAfterPrint(Sender: TObject);
    procedure PrintPreviewNewPage(Sender: TObject);
    procedure PrintPreviewBeginDoc(Sender: TObject);
    procedure PrintPreviewEndDoc(Sender: TObject);
    procedure PrintPreviewZoomChange(Sender: TObject);
    procedure FormComboBoxChange(Sender: TObject);
    procedure PrintPreviewAutoCustomForm(Sender: TObject;
      const CustomFormName: String; Operation: TOperation);
    procedure PrintPreviewAnnotation(Sender: TObject; PageNo: Integer;
      Canvas: TCanvas);
    procedure GrayscaleClick(Sender: TObject);
    procedure SavePDFButtonClick(Sender: TObject);
  private
    FirstActivation: Boolean;
    procedure CreateImageTextPage;
    procedure CreateImageOnlyPage;
    procedure CreateRichTextPage;
    procedure CreatePages;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  Printers;

procedure TMainForm.FormCreate(Sender: TObject);
var
  SampleRTF: String;
begin
  PrintPreview.Zoom := 100;
  PrintPreviewZoomChange(nil);
  UnitComboBox.ItemIndex := Ord(PrintPreview.Units);
  PrintPreview.FetchFormNames(FormComboBox.Items);
  FormComboBox.ItemIndex := FormComboBox.Items.IndexOf(PrintPreview.FormName);
  AnnotationCheckBox.Checked := PrintPreview.Annotation;
  SavePDFButton.Enabled := PrintPreview.CanSaveAsPDF;
  FirstActivation := True;
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    RichEdit1.Lines.LoadFromFile(ParamStr(1))
  else
  begin
    SampleRTF := ExtractFilePath(Application.ExeName) + 'TEAMWORK.rtf';
    if FileExists(SampleRTF) then
      RichEdit1.Lines.LoadFromFile(SampleRTF);
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if FirstActivation then
  begin
    FirstActivation := False;
    Update;
    CreatePages;
  end;
end;

procedure TMainForm.ZoomComboBoxChange(Sender: TObject);
begin
  case ZoomComboBox.ItemIndex of
    0: PrintPreview.Zoom := 50;
    1: PrintPreview.Zoom := 100;
    2: PrintPreview.Zoom := 150;
    3: PrintPreview.Zoom := 200;
    4: PrintPreview.ZoomState := zsZoomToWidth;
    5: PrintPreview.ZoomState := zsZoomToHeight;
    6: PrintPreview.ZoomState := zsZoomToFit;
  end;
end;

procedure TMainForm.UnitComboBoxChange(Sender: TObject);
begin
  CreatePages;
end;

procedure TMainForm.FormComboBoxChange(Sender: TObject);
begin
  CreatePages;
end;

procedure TMainForm.AnnotationCheckBoxClick(Sender: TObject);
begin
  PrintPreview.Annotation := AnnotationCheckBox.Checked;
end;

procedure TMainForm.GrayscaleClick(Sender: TObject);
begin
  if Grayscale.Checked then
    PrintPreview.Grayscale := [gsPreview, gsPrint]
  else
    PrintPreview.Grayscale := [];
end;

procedure TMainForm.PrintButtonClick(Sender: TObject);
begin
  if PrintPreview.State = psReady then
  begin
    PrintPreview.SetPrinterOptions;
    if PrinterSetupDialog.Execute then
    begin
      PrintPreview.UpdateAnnotation;
      if DirectPrintCheckBox.Checked then
      begin
        PrintPreview.DirectPrint := True;
        try
          CreatePages;
        finally
          PrintPreview.DirectPrint := False;
        end;
      end
      else
        PrintPreview.Print;
    end;
  end;
end;

procedure TMainForm.SavePDFButtonClick(Sender: TObject);
begin
  if SavePDFDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    Caption := Application.Title + ' - Saving as PDF...';
    try
      PrintPreview.SaveAsPDF(SavePDFDialog.FileName);
    finally
      Caption := Application.Title;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    Caption := Application.Title + ' - Saving to file...';
    try
      PrintPreview.SaveToFile(SaveDialog.FileName);
    finally
      Caption := Application.Title;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    Caption := Application.Title + ' - Loading from file...';
    try
      PrintPreview.LoadFromFile(OpenDialog.FileName);
    finally
      Caption := Application.Title;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.PrintPreviewAutoCustomForm(Sender: TObject;
  const CustomFormName: String; Operation: TOperation);
begin
  case Operation of
    opInsert: FormComboBox.Items.Add(CustomFormName);
    opRemove: FormComboBox.Items.Delete(FormComboBox.Items.IndexOf(CustomFormName));
  end;
  FormComboBox.ItemIndex := FormComboBox.Items.IndexOf(PrintPreview.FormName);
end;

procedure TMainForm.PageNavigatorChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  PrintPreview.CurrentPage := NewTab + 1;
end;

procedure TMainForm.PrintPreviewChange(Sender: TObject);
begin

  while PageNavigator.Tabs.Count < PrintPreview.TotalPages do
    PageNavigator.Tabs.Add(IntToStr(PageNavigator.Tabs.Count + 1));
  while PageNavigator.Tabs.Count > PrintPreview.TotalPages do
    PageNavigator.Tabs.Delete(PageNavigator.Tabs.Count - 1);
  PageNavigator.TabIndex := PrintPreview.CurrentPage - 1;

  if PrintPreview.State = psCreating then
    // To be eble to see and navigate pages, which are prepared so far
    Application.ProcessMessages
  else
  begin
    PrintButton.Enabled := PrintPreview.PrinterInstalled and (PrintPreview.TotalPages > 0);
    SaveButton.Enabled := (PrintPreview.TotalPages > 0);
    SavePDFButton.Enabled := PrintPreview.CanSaveAsPDF and (PrintPreview.TotalPages > 0);
  end;

end;

procedure TMainForm.PrintPreviewZoomChange(Sender: TObject);
begin
  case PrintPreview.ZoomState of
    zsZoomToFit: ZoomComboBox.ItemIndex := 6;
    zsZoomToHeight: ZoomComboBox.ItemIndex := 5;
    zsZoomToWidth: ZoomComboBox.ItemIndex := 4;
  else
    case PrintPreview.Zoom of
      200: ZoomComboBox.ItemIndex := 3;
      150: ZoomComboBox.ItemIndex := 2;
      100: ZoomComboBox.ItemIndex := 1;
      50: ZoomComboBox.ItemIndex := 0;
    else
      ZoomComboBox.ItemIndex := -1;
    end;
  end;
end;

procedure TMainForm.PrintPreviewBeginDoc(Sender: TObject);
begin
  Caption := Application.Title + ' - Creating pages...';

  UnitComboBox.Enabled := False;
  FormComboBox.Enabled := False;
  PrintButton.Enabled := False;
  SaveButton.Enabled := False;
  LoadButton.Enabled := False;
end;

procedure TMainForm.PrintPreviewEndDoc(Sender: TObject);
begin
  Caption := Application.Title;

  UnitComboBox.Enabled := True;
  FormComboBox.Enabled := True;
  PrintButton.Enabled := PrintPreview.PrinterInstalled and (PrintPreview.TotalPages > 0);
  LoadButton.Enabled := True;
  SaveButton.Enabled := (PrintPreview.TotalPages > 0);
  SavePDFButton.Enabled := PrintPreview.CanSaveAsPDF and (PrintPreview.TotalPages > 0);
end;

procedure TMainForm.PrintPreviewBeforePrint(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  Caption := Application.Title + ' - Preparing to print...';

  UnitComboBox.Enabled := False;
  PrintButton.Enabled := False;
  LoadButton.Enabled := False;
  SaveButton.Enabled := False;
  SavePDFButton.Enabled := False;
end;

procedure TMainForm.PrintPreviewAfterPrint(Sender: TObject);
begin
  Caption := Application.Title;
  Screen.Cursor := crDefault;

  UnitComboBox.Enabled := True;
  PrintButton.Enabled := PrintPreview.PrinterInstalled and (PrintPreview.TotalPages > 0);
  SaveButton.Enabled := (PrintPreview.TotalPages > 0);
  LoadButton.Enabled := True;
end;

procedure TMainForm.PrintPreviewPrintProgress(Sender: TObject; PageNum,
  Progress: Integer; var AbortIt: Boolean);
begin
  Caption := Format('%s - Printing page %d of %d (%%%d done)...',
    [Application.Title, PageNum, PrintPreview.TotalPages, Progress]);
  Update;
end;

// In this example, the code is independent of the Units property of
// PrintPreview. If you use only one measuremnt unit for PrintPreview, you can
// easily use constant values instead of passing them to ConvertUnit method.
// I also tried to write the code independent of the paper size.

procedure TMainForm.PrintPreviewNewPage(Sender: TObject);
var
  R: TRect;
begin
  with PrintPreview do
  begin
    SetStretchBltMode(Canvas.Handle, COLORONCOLOR);
    // The following line ensures one pixel pen width in all mapping modes.
    Canvas.Pen.Width := 0;
    Canvas.Brush.Style := bsCLear;
    // We are going to draw a rectangular frame on the page with 1cm distance
    // from edges of the paper.
    R := PageBounds;
    with ConvertXY(100, 100, mmLoMetric, Units) do InflateRect(R, -X, -Y);
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    // We write also the page number under the frame.
    Canvas.Font.Size := 8;
    SetTextAlign(Canvas.Handle, TA_RIGHT or TA_TOP);
    Canvas.TextOut(R.Right, R.Bottom, Format('Page %d', [TotalPages+1]));
    SetTextAlign(Canvas.Handle, TA_LEFT or TA_TOP);
  end;
end;

procedure TMainForm.PrintPreviewAnnotation(Sender: TObject;
  PageNo: Integer; Canvas: TCanvas);
begin
  with PrintPreview.PrinterPageBounds do
  begin
    Canvas.Pen.Width := 0;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clLtGray;
    Canvas.MoveTo(Left, 0);
    Canvas.LineTo(Left, PrintPreview.PaperHeight);
    Canvas.MoveTo(Right, 0);
    Canvas.LineTo(Right, PrintPreview.PaperHeight);
    Canvas.MoveTo(0, Top);
    Canvas.LineTo(PrintPreview.PaperWidth, Top);
    Canvas.MoveTo(0, Bottom);
    Canvas.LineTo(PrintPreview.PaperWidth, Bottom);
  end;
end;

procedure TMainForm.CreateImageTextPage;
var
  R: TRect;
  OneCM: TPoint;
  SavedBottom: Integer;
begin
  with PrintPreview do
  begin
    // Don't forget that we have already drawn a frame for the page with 1cm
    // margin. We have to exclude it from the paper size. The content would
    // have 1cm margin relative to the frame.
    // First, we convert 1cm to printer's current unit.
    OneCM := ConvertXY(100, 100, mmLoMetric, Units);
    R := PageBounds;
    InflateRect(R, -(2 * OneCM.X), -(2 * OneCM.Y));
    // We want to place an image horizontally in center of the paper and
    // under the top of frame. In addition, we want the image height does
    // not exceed 3cm limit.
    SavedBottom := R.Bottom;
    R.Bottom := R.Top + 3 * OneCM.Y;
    PaintGraphicEx(R, Image1.Picture.Graphic, True, True, True);
    // We are going to draw a frame and write some text inside it. The new
    // frame is 1cm under the image boundary.
    R.Top := R.Bottom + OneCM.Y;
    R.Bottom := SavedBottom;
    // Here, we draw the frame.
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    // To write the frame's dimensions under the frame
    Canvas.Font.Size := 8;
    Canvas.TextOut(R.Left, R.Bottom, Format('%d (%s) x %d (%s)',
      [R.Right - R.Left, UnitComboBox.Items[UnitComboBox.ItemIndex],
       R.Bottom - R.Top, UnitComboBox.Items[UnitComboBox.ItemIndex]]));
    // For the first line of the sample text, we set the font size to 12.
    Canvas.Font.Size := 12;
    // The following two lines ensure the colored text be printed on
    // black and white printers.
    SetBkColor(Canvas.Handle, RGB(255, 255, 255));
    SetBkMode(Canvas.Handle, TRANSPARENT);
    // While we have not reached to the frame's bottom...
    InflateRect(R, -OneCM.X div 5, -OneCM.Y div 5);
    while R.Top - Canvas.Font.Height <= R.Bottom do
    begin
      // Randomly we select a font color
      Canvas.Font.Color := RGB(Random(256), Random(256), Random(256));
      // draw the text
      Canvas.TextRect(R, R.Left, R.Top, 'Powered by Borland Delphi.');
      // move the frame's top to the next line,
      Inc(R.Top, -Canvas.Font.Height);
      // and we increase the font size by 1
      Canvas.Font.Size := Canvas.Font.Size + 1;
    end;
  end;
end;

procedure TMainForm.CreateImageOnlyPage;
var
  R: TRect;
begin
  with PrintPreview do
  begin
    // Don't forget that we have already drawn a frame for the page with 1cm
    // margin. We have to exclude it from the paper size. Besides that, we
    // want to include 5mm margin for the image also.
    R := PageBounds;
    with ConvertXY(150, 150, mmLoMetric, Units) do InflateRect(R, -X, -Y);
    PaintGraphicEx(R, Image2.Picture.Graphic, True, False, True);
  end;
end;

procedure TMainForm.CreateRichTextPage;
var
  R: TRect;
begin
  with PrintPreview do
  begin
    // Don't forget that we have already drawn a frame for the page with 1cm
    // margin. We have to exclude it from the paper size. Besides that, we
    // want to include 1cm margin for the text also.
    R := PageBounds;
    with ConvertXY(200, 200, mmLoMetric, Units) do InflateRect(R, -X, -Y);
    PaintRichText(R, RichEdit1, 0, nil);
  end;
end;

procedure TMainForm.CreatePages;
begin
  with PrintPreview do
  begin
    Units := TUnits(UnitComboBox.ItemIndex);
    if FormComboBox.ItemIndex >= 0 then
      FormName := FormComboBox.Items[FormComboBox.ItemIndex];
    BeginDoc;
    try
      CreateImageTextPage;
      NewPage;
      CreateImageOnlyPage;
      NewPage;
      CreateRichTextPage;
    finally
      EndDoc;
    end;
  end;
end;

end.

