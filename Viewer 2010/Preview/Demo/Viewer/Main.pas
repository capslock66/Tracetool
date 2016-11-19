unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Tabs, Preview, Dialogs, ComCtrls, Menus;

type
  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    MainMenu: TMainMenu;
    FilePopup: TMenuItem;
    ZoomPopup: TMenuItem;
    FileOpen: TMenuItem;
    FilePrint: TMenuItem;
    N1: TMenuItem;
    FileExit: TMenuItem;
    Zoom25: TMenuItem;
    Zoom50: TMenuItem;
    Zoom100: TMenuItem;
    Zoom150: TMenuItem;
    Zoom200: TMenuItem;
    N2: TMenuItem;
    ZoomWholePage: TMenuItem;
    ZoomPageWidth: TMenuItem;
    ZoomPageHeight: TMenuItem;
    PrintDialog: TPrintDialog;
    Splitter1: TSplitter;
    ThumbnailPreview: TThumbnailPreview;
    Panel: TPanel;
    PrintPreview: TPrintPreview;
    PageNavigator: TTabSet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageNavigatorChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure PrintPreviewChange(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure FilePrintClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure Zoom25Click(Sender: TObject);
    procedure Zoom50Click(Sender: TObject);
    procedure Zoom100Click(Sender: TObject);
    procedure Zoom150Click(Sender: TObject);
    procedure Zoom200Click(Sender: TObject);
    procedure ZoomPageWidthClick(Sender: TObject);
    procedure ZoomPageHeightClick(Sender: TObject);
    procedure ZoomWholePageClick(Sender: TObject);
    procedure FilePopupClick(Sender: TObject);
    procedure ZoomPopupClick(Sender: TObject);
  private
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  Printers, ShellAPI;

procedure TMainForm.WMDropFiles(var Msg: TMessage);
var
  DroppedFile: array[0..512] of Char;
begin
  if DragQueryFile(THandle(Msg.WParam), $FFFFFFFF, nil, 0) > 0 then
  begin
    DragQueryFile(THandle(Msg.WParam), 0, DroppedFile, SizeOf(DroppedFile));
    Screen.Cursor := crHourGlass;
    try
      PrintPreview.LoadFromFile(DroppedFile);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  DragFinish(THandle(Msg.WParam));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  if ParamCount > 0 then
    PrintPreview.LoadFromFile(ParamStr(1));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
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
  Update;
end;

procedure TMainForm.FileOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      PrintPreview.LoadFromFile(OpenDialog.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.FilePrintClick(Sender: TObject);
begin
  with PrintDialog do
  begin
    MinPage := 1;
    MaxPage := PrintPreview.TotalPages;
    PrintRange := prAllPages;
    if Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        PrintPreview.PrintJobTitle := Caption + ' - ' + OpenDialog.FileName; 
        case PrintRange of
          prAllPages: PrintPreview.Print;
          prPageNums: PrintPreview.PrintPages(FromPage, ToPage);
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TMainForm.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Zoom25Click(Sender: TObject);
begin
  PrintPreview.Zoom := 25;
end;

procedure TMainForm.Zoom50Click(Sender: TObject);
begin
  PrintPreview.Zoom := 50;
end;

procedure TMainForm.Zoom100Click(Sender: TObject);
begin
  PrintPreview.Zoom := 100;
end;

procedure TMainForm.Zoom150Click(Sender: TObject);
begin
  PrintPreview.Zoom := 150;
end;

procedure TMainForm.Zoom200Click(Sender: TObject);
begin
  PrintPreview.Zoom := 200;
end;

procedure TMainForm.ZoomPageWidthClick(Sender: TObject);
begin
  PrintPreview.ZoomState := zsZoomToWidth;
end;

procedure TMainForm.ZoomPageHeightClick(Sender: TObject);
begin
  PrintPreview.ZoomState := zsZoomToHeight;
end;

procedure TMainForm.ZoomWholePageClick(Sender: TObject);
begin
  PrintPreview.ZoomState := zsZoomToFit;
end;

procedure TMainForm.FilePopupClick(Sender: TObject);
begin
  FilePrint.Enabled := PrintPreview.PrinterInstalled and (PrintPreview.TotalPages > 0); 
end;

procedure TMainForm.ZoomPopupClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ZoomPopup.Count - 1 do
    ZoomPopup.Items[I].Checked := False;
  case PrintPreview.ZoomState of
    zsZoomOther:
      case PrintPreview.Zoom of
        25: Zoom25.Checked := True;
        50: Zoom50.Checked := True;
        100: Zoom100.Checked := True;
        150: Zoom150.Checked := True;
        200: Zoom200.Checked := True;
      end;
    zsZoomToWidth:
      ZoomPageWidth.Checked := True;
    zsZoomToHeight:
      ZoomPageHeight.Checked := True;
    zsZoomToFit:
      ZoomWholePage.Checked := True;
  end;
end;


end.
