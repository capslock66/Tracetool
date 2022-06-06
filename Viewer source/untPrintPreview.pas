unit untPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Tabs, Preview, Dialogs, jpeg, ImgList,
  ComCtrls, CheckLst, VirtualTrees, Math, unt_base;

type
  TFrmPrintPreview = class(TForm)
    PrinterSetupDialog: TPrinterSetupDialog;
    Splitter1: TSplitter;
    Bevel1: TBevel;
    PageControl: TPageControl;
    tabParameters: TTabSheet;
    TabPreview: TTabSheet;
    Panel1: TPanel;
    PageNavigator: TTabSet;
    PrintPreview: TPrintPreview;
    Toolbar: TPanel;
    Label1: TLabel;
    ZoomComboBox: TComboBox;
    Grayscale: TCheckBox;
    ThumbnailPreview1: TThumbnailPreview;
    Panel4: TPanel;
    Panel2: TPanel;
    ButCancel: TButton;
    ButSelectPrinter: TButton;
    ButPreview: TButton;
    ColumList: TCheckListBox;
    Label2: TLabel;
    rbPrintAll: TRadioButton;
    rbSelected: TRadioButton;
    butPrintChildren: TCheckBox;
    chkPrintColumnSeparator: TCheckBox;
    chkTitlleOnEachPage: TCheckBox;
    chkTitlle: TCheckBox;
    AnnotationCheckBox: TCheckBox;
    PrintButton: TButton;
    TempImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ZoomComboBoxChange(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
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
    procedure PrintPreviewAnnotation(Sender: TObject; PageNo: Integer; Canvas: TCanvas);
    procedure GrayscaleClick(Sender: TObject);
    procedure ButPreviewClick(Sender: TObject);
    procedure ButSelectPrinterClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure AnnotationCheckBoxClick(Sender: TObject);
    procedure ColumListClick(Sender: TObject);
  public
    treeview : TVirtualStringTree ;
    TraceWin : TForm ;  // cannot use TFrm_Trace here
    Margin : integer ;
    NodePosY : integer ;
    procedure initialize(tree : TVirtualStringTree; TraceWin : TForm);
  end;

var
  FrmPrintPreview: TFrmPrintPreview;

implementation

{$R *.DFM}

uses
  Printers, unt_TraceWin, Unt_Tool; //, unt_TraceWin;

//var
//  Frm_Trace : TFrm_Trace ;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.FormCreate(Sender: TObject);
begin
  PrintPreview.Zoom := 100;
  PrintPreviewZoomChange(nil);
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.initialize(tree : TVirtualStringTree ; TraceWin : TForm);
var
   SelectedNode : PVirtualNode ;
   ColIdx : integer ;
   col : TVirtualTreeColumn ;
begin
   self.TraceWin := TraceWin ;
   treeview := tree ;
   PrintPreview.Clear ;
   // list all columns
   ColumList.Clear ;
   if ColumList.Count = 0 then begin
      ColIdx := treeview.Header.Columns.GetFirstVisibleColumn ;
      while ColIdx <> InvalidColumn do begin
         col := treeview.header.Columns[ColIdx] ;
         if col.Text = '' then
            ColumList.AddItem('(No Title)',col)
         else
            ColumList.AddItem(col.Text,col);
         ColumList.Checked[ColumList.Count-1] := true ;
         ColIdx := treeview.Header.Columns.GetNextVisibleColumn(ColIdx) ;
      end ;
   end ;

   PageControl.ActivePage := tabParameters ;

   // if no line selected or only one line selected : propose to print all,
   // else propose to print selected
   SelectedNode := treeview.GetFirstSelected() ;
   if SelectedNode = nil then begin
      rbPrintAll.checked := true ;
      exit ;
   end ;
   // get next selected. if nil then only one nod selected
   SelectedNode := treeview.GetNextSelected(SelectedNode) ;
   if SelectedNode = nil then
      rbPrintAll.checked := true
   else   // more than 1 node : propose to print selected lines
      rbSelected.checked := true ;
end ;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewNewPage(Sender: TObject);
var
   NodePosX : integer ;
   ColIdx : integer ;
   col : TVirtualTreeColumn ;
   ColWidth1, ColWidth2 : integer ;
   TheRect: TRect;
   TextSize1: TSize;
   NodeHeight: Integer ;
   Offset3,Offset5 : integer ;
begin
   Offset3 := ConvertUnits (3 , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
   Offset5 := ConvertUnits (5 , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
   GetTextExtentPoint32W(PrintPreview.Canvas.Handle ,PWideChar('XyZ'), 3 ,TextSize1) ;
   NodeHeight := TextSize1.cy ;
   NodePosX := Margin ;
   // NodePosY is initialized in main loop
   for ColIdx := 0 to ColumList.Count-1 do begin
      if ColumList.Checked[ColIdx] then begin
         col := TVirtualTreeColumn (ColumList.Items.Objects[ColIdx]) ;
         // position for the column separator
         ColWidth2 := Offset5 + ConvertUnits (col.Width , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
         // calculate column width to move to next column
         ColWidth1 := Offset5 + ColWidth2 ;

         if chkTitlle.Checked then begin
            if (chkTitlleOnEachPage.checked = true) or (PrintPreview.TotalPages = 0) then begin
               TheRect := Rect(NodePosX,NodePosY,NodePosX+ColWidth2,NodePosY+NodeHeight);
               PrintPreview.Canvas.TextRect (TheRect,NodePosX,NodePosY,col.Text) ;

               // draw header separator
               PrintPreview.Canvas.MoveTo (NodePosX , Margin + NodeHeight + Offset3) ;
               PrintPreview.Canvas.LineTo (NodePosX + ColWidth1 , Margin + NodeHeight + Offset3) ;
               // draw vertical lines
               if chkPrintColumnSeparator.Checked then begin
                  PrintPreview.Canvas.MoveTo (NodePosX + ColWidth2 , Margin) ;
                  PrintPreview.Canvas.LineTo (NodePosX + ColWidth2 , Margin + NodeHeight) ;
               end ;
            end;
         end;
         NodePosX := NodePosX + ColWidth1 ;
      end ;
   end ;
   if (chkTitlle.Checked) and ((chkTitlleOnEachPage.checked = true) or (PrintPreview.TotalPages = 0)) then
      NodePosY := NodePosY + NodeHeight + ConvertUnits (6 , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.ButPreviewClick(Sender: TObject);
var
   Node, NextBrother: PVirtualNode ;
   NodePosX : integer ;
   NodeText : string ;

   TextSize1: TSize;
   xText : integer ;
   TreeRec : PTreeRec ;
   c,d : integer ;
   ColWidth, TxtWidth, NextColWidth : integer ;
   col,NextCol : TVirtualTreeColumn ;
   NodeHeight, NodeHeight2: Integer ;
   TheRect: TRect;
   Offset5 : integer ;
   Offset16 : integer ;
   ImageIndex: Integer ;
   Ghosted: Boolean ;
   IconOffset : integer ;

   function GetNextNode(Node: PVirtualNode; CanUseChildren : boolean) : PVirtualNode;
   begin
      result := node ;
      repeat
         if (CanUseChildren) and (result.FirstChild <> nil) then begin
            result := result.FirstChild;
         end else if result.NextSibling <> nil then begin
           result := result.NextSibling;
         end else begin
            // no more node at this level, go up
            while (result.Parent <> treeview.RootNode) and (result.Parent.NextSibling = nil) do
               result := result.Parent;
            if result.Parent = treeview.RootNode then begin
               result := result.Parent ; // no more node. return the root node
               exit ;
            end;
            result := result.Parent.NextSibling;
         end;
      until treeview.IsVisible[result] ;
   end;
begin

   Offset5 := ConvertUnits (5 , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
   Offset16 := ConvertUnits (16 , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
   Margin  := ConvertUnits(100, Screen.PixelsPerInch, {InUnits} mmLoMetric, {OutUnits} PrintPreview.Units);     // LowMetric to HiMetric
   NodePosY := Margin ;

   //PrintPreview.SetPrinterOptions ;
   PrintPreview.DirectPrint := false ;
   PrintPreview.BeginDoc;  // NodePosY can be changed if header is printed

   // get the first node in tree or the first selected node
   if rbPrintAll.checked then
      Node := treeview.GetFirst()
   else
      node := treeview.GetFirstSelected() ;

   NextBrother := nil ;
   while Node <> nil do begin

      treeRec := nil ;
      if TraceWin <> nil then
         TreeRec := treeview.GetNodeData(Node) ;

      // calculate node height before printing node
      NodeHeight := 0 ;
      NodeHeight2 := 0 ;
      if assigned (treeview.OnMeasureItem) then begin
         treeview.OnMeasureItem(treeview,treeview.Canvas, Node, NodeHeight2);
      end else begin   // all treeview in tracetool implement OnMeasureItem, but in case of future development...
         for c := 0 to ColumList.Count-1 do begin
            if ColumList.Checked[c] then begin
               col := TVirtualTreeColumn (ColumList.Items.Objects[c]) ;
               NodeText := treeview.Text[Node,Col.Index] ;
               PrintPreview.Canvas.font.Color := clBlack ;
               PrintPreview.Canvas.font.Style := [] ;

               if TraceWin <> nil then
                  TFrm_Trace(TraceWin).ChangeFontDetail (not TFrm_Trace(TraceWin).IsWatch, PrintPreview.Canvas, c , TreeRec.FontDetails, {selected}false) ;

               // the node height depend of each cells in the line
               GetTextExtentPoint32W(PrintPreview.Canvas.Handle ,PWideChar(NodeText), length(NodeText) ,TextSize1) ;
               NodeHeight := max (NodeHeight,TextSize1.cy) ;
            end ;
         end ;
      end ;
      NodeHeight2 := ConvertUnits (NodeHeight2 , Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units) ;
      NodeHeight := max (NodeHeight,NodeHeight2) ;

      // if node cannot be printed on this page, create a new one before printing
      if PrintPreview.PageBounds.Top + NodePosY + NodeHeight + Margin > PrintPreview.PageBounds.Bottom then begin
         NodePosY := Margin ;
         PrintPreview.NewPage() ;    // NodePosY can be changed if header is printed
      end ;

      NodePosX := Margin ;

      for c := 0 to ColumList.Count-1 do begin
         if ColumList.Checked[c] then begin
            col := TVirtualTreeColumn (ColumList.Items.Objects[c]) ;

            ColWidth := ConvertUnits (col.Width+10, Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;
            TxtWidth := ConvertUnits (col.Width, Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units ) ;

            // draw vertical lines
            if chkPrintColumnSeparator.Checked then begin
               PrintPreview.Canvas.MoveTo (NodePosX + TxtWidth + Offset5 , NodePosY) ;
               PrintPreview.Canvas.LineTo (NodePosX + TxtWidth + Offset5 , NodePosY+NodeHeight) ;
            end ;

            // limit text width to column width or to next columns if nothing in next columns
            NextColWidth := 0 ;
            for d := c+1 to ColumList.Count - 1 do begin
               NextCol := TVirtualTreeColumn (ColumList.Items.Objects[d]) ;
               NodeText := treeview.Text[Node,NextCol.Index] ;
               if Trim(NodeText) = '' then
                  NextColWidth := NextColWidth + ConvertUnits (NextCol.Width+10, Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units )
               else
                  break ; // stop loop : for this node , this column is not empty
            end ;

            // icons

            IconOffset := 0 ;
            if assigned (treeview.OnGetImageIndex) then begin
               //TVTGetImageEvent = procedure(
               //   Sender: TBaseVirtualTree;
               //   Node: PVirtualNode;
               //   Kind: TVTImageKind;
               //   Column: TColumnIndex;
               //   var Ghosted: Boolean;
               //   var ImageIndex: TImageIndex) of object;

               treeview.OnGetImageIndex(
                 treeview ,
                 Node ,
                 {Kind} ikNormal ,
                 TColumnIndex(Col.Index) ,
                 Ghosted,
                 TImageIndex(ImageIndex));

               if ImageIndex <> -1 then begin
                  // write first the icon to a bitmap
                  TempImage.Picture := nil; // clear before paint
                  treeview.Images.Draw(TempImage.Canvas, 0 , 0 ,ImageIndex) ;
                  TheRect := Rect (NodePosX, NodePosY, NodePosX + Offset16, NodePosY + Offset16) ;
                  PrintPreview.PaintGraphicEx(TheRect, TempImage.Picture.Graphic, True, False, True);
                  IconOffset := Offset16 + Offset5 ;
               end ;
            end ;

            // special case for the first column (debug/warning/error) : Another imagelist is used for this column

            if (TraceWin <> nil) and (Col.Index = 0) and (TFrm_Trace(TraceWin).IsMultiColTree = false) then begin
               if TreeRec.TreeIcon = -1 then
                  ImageIndex := 24
               else
                  ImageIndex := TreeRec.TreeIcon ;

               // write first the icon to a bitmap
               TempImage.Picture := nil; // clear before paint
               Frm_Tool.ImageList1.Draw(TempImage.Canvas, 0 , 0 ,ImageIndex) ;
               TheRect := Rect (NodePosX, NodePosY, NodePosX + Offset16, NodePosY + Offset16) ;
               PrintPreview.PaintGraphicEx(TheRect, TempImage.Picture.Graphic, True, False, True);
               IconOffset := Offset16 + Offset5 ;
            end ;


            NodeText := treeview.Text[Node,Col.Index] ;
            if Trim(NodeText) <> '' then begin // don't print empty string
               PrintPreview.Canvas.font.Color := clBlack ;
               PrintPreview.Canvas.font.Style := [] ;

               if TraceWin <> nil then
                  TFrm_Trace(TraceWin).ChangeFontDetail (not TFrm_Trace(TraceWin).IsWatch, PrintPreview.Canvas, c , TreeRec.FontDetails, {selected}false) ;

               if treeview.header.Columns[treeview.Header.MainColumn] = col then
                  xText := NodePosX + ConvertUnits(treeview.Indent * treeview.GetNodeLevel(Node), Screen.PixelsPerInch, {InUnits}mmLoEnglish, {OutUnits} PrintPreview.Units)   // for main column : add indent
               else
                  xText := NodePosX  ;
               inc (xText,IconOffset) ;

               GetTextExtentPoint32W(PrintPreview.Canvas.Handle ,PWideChar(NodeText), length(NodeText) ,TextSize1) ;

               // limit width to column width (TxtWidth) plus the size of each column at right having empty content
               TheRect := Rect(xText,NodePosY,xText+TxtWidth+NextColWidth,NodePosY+NodeHeight);
               if TextSize1.cx+1 < TxtWidth then
                  TextSize1.cx := TxtWidth ;      // take at least col width ( else problem with italic string too short)

               // don't draw on all empty columns if the text is smaller
               TheRect.Right := min (TheRect.Right, xText + TextSize1.cx+1) ;

               PrintPreview.Canvas.TextRect (TheRect,xText,NodePosY,NodeText) ;
            end ;
            // move to next column
            NodePosX := NodePosX + ColWidth  ; // ColWidth = col.Width+10
         end ;
      end ;

      NodePosY := NodePosY + NodeHeight ;

      // next node : next sibling or next selected
      if rbPrintAll.checked then begin
         // Returns next node in tree (advances to next sibling of the node's parent or its parent, if necessary).
         repeat
            Node := treeview.GetNext(Node);
         until treeview.IsVisible[Node] ;
      end else begin
         // if the butPrintChildren is true, print children of selected
         if NextBrother = nil then begin
            // this is a selected node
            if (butPrintChildren.Checked) and (node.ChildCount <> 0) then begin
               // detect next node : brother or (brother of parent)
               NextBrother := GetNextNode(Node,false);  // can be the root node
               // print all nodes starting from first child up to NextBrother
               node := node.FirstChild ;
            end else begin
               // no children or butPrintChildren is false
               node := treeview.getNextSelected(node) ;
            end;
         end else begin
            // We are printing children of previous selected node
            // get first child or next brother or search for next node in tree
            Node := GetNextNode(Node,true);

            // no more node to print : break
            if node = treeview.RootNode then
               break ;

            // next node is the brother of the printed selected node
            if node = NextBrother then begin
               NextBrother := nil;
               // if this node is not selected then go to next selected

               if treeview.Selected[node] = false then
                  node := treeview.getNextSelected(node);
            end;
         end;   // nextBrother nil
      end ;     // rbPrintAll not checked
   end ;        // loop node

   PrintPreview.EndDoc;
   PageControl.ActivePage := TabPreview ;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.ZoomComboBoxChange(Sender: TObject);
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

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.GrayscaleClick(Sender: TObject);
begin
  if Grayscale.Checked then
    PrintPreview.Grayscale := [gsPreview, gsPrint]
  else
    PrintPreview.Grayscale := [];
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintButtonClick(Sender: TObject);
begin
  if PrintPreview.State = psReady then
  begin
    PrintPreview.SetPrinterOptions;
    //if PrinterSetupDialog.Execute then
    //begin
      PrintPreview.UpdateAnnotation;
      PrintPreview.Print;
    //end;
    modalresult := mrOk ;
  end;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PageNavigatorChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
  PrintPreview.CurrentPage := NewTab + 1;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewChange(Sender: TObject);
begin

  while PageNavigator.Tabs.Count < PrintPreview.TotalPages do
    PageNavigator.Tabs.Add(IntToStr(PageNavigator.Tabs.Count + 1));
  while PageNavigator.Tabs.Count > PrintPreview.TotalPages do
    PageNavigator.Tabs.Delete(PageNavigator.Tabs.Count - 1);
  PageNavigator.TabIndex := PrintPreview.CurrentPage - 1;

  if PrintPreview.State = psCreating then
    // To be able to see and navigate pages, which are prepared so far
    Application.ProcessMessages
  else
  begin
    PrintButton.Enabled := PrintPreview.PrinterInstalled and (PrintPreview.TotalPages > 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewZoomChange(Sender: TObject);
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

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewBeginDoc(Sender: TObject);
begin
  Caption := Application.Title + ' - Creating pages...';
  PrintButton.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewEndDoc(Sender: TObject);
begin
  Caption := Application.Title;
  PrintButton.Enabled := PrintPreview.PrinterInstalled and (PrintPreview.TotalPages > 0);
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewBeforePrint(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  Caption := Application.Title + ' - Preparing to print...';
  PrintButton.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewAfterPrint(Sender: TObject);
begin
  Caption := Application.Title;
  Screen.Cursor := crDefault;
  PrintButton.Enabled := {PrintPreview.PrinterInstalled and} (PrintPreview.TotalPages > 0);
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewPrintProgress(Sender: TObject; PageNum,
  Progress: Integer; var AbortIt: Boolean);
begin
  Caption := Format('%s - Printing page %d of %d (%%%d done)...',
    [Application.Title, PageNum, PrintPreview.TotalPages, Progress]);
  Update;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PrintPreviewAnnotation(Sender: TObject; PageNo: Integer; Canvas: TCanvas);
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

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.ButSelectPrinterClick(Sender: TObject);
begin
   PrintPreview.SetPrinterOptions ;
   PrinterSetupDialog.Execute ;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.ColumListClick(Sender: TObject);
begin
   PrintPreview.Clear ;
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.PageControlChange(Sender: TObject);
begin
   if PageControl.ActivePage = TabPreview then
     ButPreviewClick(nil);
end;

//------------------------------------------------------------------------------

procedure TFrmPrintPreview.AnnotationCheckBoxClick(Sender: TObject);
begin
  PrintPreview.Annotation := AnnotationCheckBox.Checked;
end;

//------------------------------------------------------------------------------

end.

