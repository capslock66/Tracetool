unit unt_Details_table;

interface

uses
  system.Contnrs, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unt_Details_base, VirtualTrees, VirtualTrees.Types,
  unt_Editor, Menus , ExtCtrls, clipbrd,
  unt_TraceWin ,
  unt_utility,
  vstSort,
  VstSelector,
  unt_tool,
  System.Generics.Collections,
  System.TypInfo,
  System.Math,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees.AncestorVCL;            // VstEditor, IVstEditor, TMember

type
  PTableRec = ^TTableRec ;
  TTableRec = record
     OriginalOrder: integer; // Original order when inserted. Used to Unsort nodes
     Columns : TStringList ;
  end ;

  Tframe_table = class(Tframe_BaseDetails)
    VstDetail: TVirtualStringTree;
    PopupDetail: TPopupMenu;
    CopyMenu: TMenuItem;
    N2: TMenuItem;
    SelectAllMenu: TMenuItem;
    procedure VstDetailCreateEditor(Sender: TBaseVirtualTree;Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDetailDblClick(Sender: TObject);
    procedure VstDetailEditCancelled(Sender: TBaseVirtualTree;Column: TColumnIndex);
    procedure VstDetailEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;Column: TColumnIndex);
    procedure VstDetailFreeNode(Sender: TBaseVirtualTree;Node: PVirtualNode);
    procedure VstDetailMeasureItem(Sender: TBaseVirtualTree;TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
    procedure VstDetailPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstDetailColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
    procedure VstDetailBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstDetailFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
    procedure VstDetailSelectorSelectionChanged(Sender: TVstSelector; selectionAsText: string);
  public
    { Public declarations }
    TraceWin: TFrm_Trace;
    Sorter: TVstSort;
    VstDetailSelector: TVstSelector;

    Constructor Create(AOwner: TComponent);  override ;
    Procedure AddDetails(TreeRec: PTreeRec; RootMember : TMember); override;
    function HasFocus : boolean ; override;
    procedure SelectAll() ; override;
    function copySelected():boolean ; override;

  end;

var
  frame_table: Tframe_table;


implementation

uses
unt_TraceConfig, unt_Details_Classic,  unt_search ;

{$R *.dfm}

//------------------------------------------------------------------------------

constructor Tframe_table.Create(AOwner: TComponent);
begin
   inherited create (AOwner) ;

   TraceWin := TFrm_Trace(owner);

   // initialize sort
   Sorter := TVstSort.Create(self);
   Sorter.tree := VstDetail;
   Sorter.UtilityImages := Frm_Tool.UtilityImages;
   Sorter.canUnsort := true;

   // redirect some events to the sorter
   VstDetail.onHeaderClick := Sorter.onHeaderClick;
   VstDetail.OnKeyUp := Sorter.OnKeyUp;
   VstDetail.onHeaderDrawQueryElements := Sorter.onHeaderDrawQueryElements;
   VstDetail.onAdvancedHeaderDraw := Sorter.onAdvancedHeaderDraw;

   VstDetail.NodeDataSize := sizeof (TTableRec) ;
   //VstDetail.Header.SortColumn := 0 ;
   VstDetail.Header.MainColumn := 0 ;
   VstDetail.Header.AutoSizeIndex := -1 ;     // 2

   // header must be visible to enable resize !
   VstDetail.Header.Columns.Items[0].text := '' ;
   VstDetail.Header.Columns.Items[1].text := '' ;
   VstDetail.Header.Columns.Items[2].text := '' ;

   VstDetail.Header.Options           := TraceWin.VstMain.Header.Options ;
   VstDetail.TreeOptions.AutoOptions  := TraceWin.VstMain.TreeOptions.AutoOptions
      + [toAutoSpanColumns]           // Large entries continue into next columns
      - [toDisableAutoscrollOnFocus]  // Disable scrolling a column entirely into view if it gets focused.
      + [toDisableAutoscrollOnEdit];  // Do not center a node horizontally when it is edited.

   VstDetail.TreeOptions.PaintOptions := TraceWin.VstMain.TreeOptions.PaintOptions
      - [toUseBlendedImages]        // Don't use blended images
      - [toShowTreeLines]           // don't Display tree lines to show hierarchy of nodes.
      - [toHideSelection]           // show a grayed selection when the tree lose the focus
      + [toShowRoot]                // show root.
      + [toShowButtons]             // Display collapse/expand buttons left to a node.
      + [toThemeAware]              // Draw UI elements (header, tree buttons etc.) according to the current theme
      + [toHideFocusRect];          // hide focus rect

   VstDetail.TreeOptions.SelectionOptions := TraceWin.VstMain.TreeOptions.SelectionOptions
      + [toDisableDrawSelection]    // Prevent user from selecting with the selection rectangle in multiselect mode.
      + [toExtendedFocus]           // Entries other than in the main column can be selected, edited etc.
      - [toMultiselect]             // Allow more than one node to be selected.
      + [toSimpleDrawSelection]     // Simplifies draw selection, so a node's caption does not need to intersect with the selection rectangle.
      - [toFullRowSelect];          // selection highlight the whole line

   VstDetail.TreeOptions.MiscOptions := TraceWin.VstMain.TreeOptions.MiscOptions
      - [toReportMode]              // Tree behaves like TListView in report mode.
      + [toFullRepaintOnResize]     // Fully invalidate the tree when its window is resized (CS_HREDRAW/CS_VREDRAW).
      + [toWheelPanning]            // Support for mouse panning (wheel mice only).
      - [toFullRowDrag]             // Start node dragging by clicking anywhere in it instead only on the caption or image.
                                    // Must be used together with toDisableDrawSelection.
      + [toGridExtensions]          // Use some special enhancements to simulate and support grid behavior.
      - [toVariableNodeHeight]      // variable node height
      - [toToggleOnDblClick]        // Toggle node expansion state when it is double clicked.
      - [toEditable]                // don't allow edition. Code is used to detect double click
      - [toCheckSupport];           // no checkboxes

   VstDetail.Colors.UnfocusedColor                := TraceWin.VstMain.Colors.UnfocusedColor ;
   VstDetail.Colors.UnfocusedSelectionColor       := TraceWin.VstMain.Colors.UnfocusedSelectionColor ;
   VstDetail.Colors.UnfocusedSelectionBorderColor := TraceWin.VstMain.Colors.UnfocusedSelectionBorderColor ;

   // multiple selection handler
   VstDetailSelector := TVstSelector.Create(self);   // self is owner
   VstDetailSelector.Init(VstDetail);
   VstDetailSelector.OnSelectionChanged := VstDetailSelectorSelectionChanged;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   // scroll into view
   sender.ScrollIntoView (node,false,false);     // center and horizontally false
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailCreateEditor(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();    // unt_tool
      IVstEditor := VstEditor ;                     // unt_tool
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   SelectedNode := VstDetail.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstDetail.ScreenToClient(P);
   MouseNode := VstDetail.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstDetail.TreeOptions.MiscOptions := VstDetail.TreeOptions.MiscOptions + [toEditable] ;

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_MEMBER, Integer(SelectedNode), 0);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
   VstDetail.TreeOptions.MiscOptions := VstDetail.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailEdited(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstDetail.TreeOptions.MiscOptions := VstDetail.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailFreeNode(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
   DetailRec : PTableRec ;
   //c : integer ;
begin
   try
      DetailRec := Sender.GetNodeData(Node) ;
      DetailRec.Columns.free();
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace('VstDetailFreeNode exception when resetting', e.message) ;
      end ;
   end ;

end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailGetText(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;  var CellText: string);
var
   DetailRec : PTableRec ;
begin
   CellText := '' ;
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;

   if Column >= DetailRec.Columns.count then
      CellText := ''
   else

   if (not (toEditable in VstDetail.TreeOptions.MiscOptions)) and (Length(DetailRec.Columns[Column]) > 400) then
      CellText := Copy(DetailRec.Columns[Column], 1, 400) + '...'
   else
      CellText := DetailRec.Columns[Column] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailSelectorSelectionChanged(Sender: TVstSelector; selectionAsText: string);
begin
   Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.LabelSelect.Caption := selectionAsText;
   if (Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.LabelSelect.Caption <> '') then
      Tframe_Classic(TraceWin.TreeDetailFrame).FrameMemo.SetMemoText('',false,false);
end;

procedure Tframe_table.VstDetailFocusChanged(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex);
var
   DetailRec : PTableRec ;
   CellText: String;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;
   CellText := DetailRec.Columns[Column] ;
   if (Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.LabelSelect.Caption = '') then
      Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.SetMemoText(CellText,false,false);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailColumnClick(Sender: TBaseVirtualTree;  Column: TColumnIndex; Shift: TShiftState);
var
   DetailRec : PTableRec ;
   CellText: String;
   SelectedNode: PVirtualNode ;
begin
   SelectedNode := Sender.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   DetailRec := Sender.GetNodeData(SelectedNode) ;
   if DetailRec = nil then
      exit ;

   CellText := DetailRec.Columns[Column] ;

   if (Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.LabelSelect.Caption = '') then
      Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.SetMemoText(CellText,false,false);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;   TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   DetailRec : PTableRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   if (SearchText <> '') {and (SearchKind = mrYesToAll)} then begin  //  mrYesToAll means Highlight all
      if (MatchSearch (DetailRec.Columns[Column]) <> 0) then
         DrawHighlight (TargetCanvas, CellRect,false) ;
   end;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailPaintText(Sender: TBaseVirtualTree;  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;  TextType: TVSTTextType);
//var
//   DetailRec : PTableRec ;
begin
   //DetailRec := Sender.GetNodeData(Node) ;

   // force font
   //TraceWin.ChangeFontDetail ({IsTrace}false,TargetCanvas,  Column, DetailRec.fontDetails,(vsSelected in Node.States)) ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstDetailMeasureItem(Sender: TBaseVirtualTree;  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
var
//   h2,h3 : integer ;
//   DetailRec : PTableRec ;
   newNodeHeight : integer ;
begin
   if TraceWin.IsWatch then
      newNodeHeight := VstDetail.DefaultNodeHeight
   else
      newNodeHeight := TraceConfig.Framework_info_NodeHeight ;
   NodeHeight := newNodeHeight ;

//   DetailRec := Sender.GetNodeData(Node) ;
//
//   // force font
//   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  0, DetailRec.fontDetails,true) ;   // Watch/Framework , Trace/info
//   NodeHeight := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,0) ;
//
//   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  1, DetailRec.fontDetails,true) ;
//   h2 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,1) ;
//
//   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  2, DetailRec.fontDetails,true) ;
//   h3 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,2) ;
//
//   if h2 > NodeHeight then
//      NodeHeight := h2 ;
//
//   if h3 > NodeHeight then
//      NodeHeight := h3 ;
//
//   // if multiline, NodeHeight is bigger than DefaultNodeHeight
//   if NodeHeight = 0 then
//      NodeHeight := newNodeHeight ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.AddDetails(TreeRec: PTreeRec; RootMember: TMember);
var
   cols : TStringList ;
   col : TVirtualTreeColumn ;
   c : integer ;
   SubMember : TMember ;
   DetailNode :  PVirtualNode ;
   DetailRec : PTableRec ;
begin

   VstDetailSelector.ResetSelection();

   VstDetail.Clear ;
   if Sorter.SortColumns.Count > 1 then  
      Sorter.ClearSortExcept(nil);
   
   VstDetail.header.Columns.Clear ;

   // first member, col1 is the title
   cols := getTabStrings(pchar(RootMember.Col1)) ;
   for c := 0 to cols.Count-1 do begin
      col := VstDetail.header.Columns.Add ;
      col.options  := col.options + [coAllowFocus] ;  // ensure user can focus to this column
      col.MinWidth := 10 ;
      col.MaxWidth := 1000 ;
      col.Width := 100 ;
      col.Text := cols[c] ;
   end ;
   VstDetail.Header.MainColumn := 0 ;
   VstDetail.Header.AutoSizeIndex := -1 ;  // auto
   cols.Free ;
   //LowTrace ('before add table');
   //TFrm_Trace.InternalTrace (FormatDateTime('yyyymmdd hh:mm:ss:zzz',now) + ' before add table');
   // add lines
   for c := 0 to RootMember.SubMembers.Count -1 do begin
      SubMember := TMember (RootMember.SubMembers.Items[c]) ;
      DetailNode := VstDetail.AddChild(nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstDetail.ReinitNode(DetailNode,false);
      DetailNode.Align := (VstDetail.DefaultNodeHeight div 2)-2 ;
      DetailRec := VstDetail.GetNodeData(DetailNode) ;

      cols := getTabStrings(pchar(SubMember.Col1)) ;
      DetailRec.OriginalOrder := c;   // for unsort
      DetailRec.Columns := cols ;     // free by OnFreeNodes
   end ;

   //TFrm_Trace.InternalTrace (FormatDateTime('yyyymmdd hh:mm:ss:zzz',now) + ' after add table');

   // resize all columns, using the header text and all visible (true) lines
   AutosizeAll (VstDetail,true) ;

   // force last column width to maximum
   VstDetail.Header.Columns[VstDetail.Header.Columns.Count-1].Width := 9000 ;

   VstDetail.Visible := true ;
   TFrm_Trace(Owner).CurrentViewers.add(self) ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.SelectAll;
begin
   VstDetail.SelectAll(false) ;
end;

//------------------------------------------------------------------------------

function Tframe_table.HasFocus: boolean;
begin
  result := Focused or VstDetail.focused ;
end;

//------------------------------------------------------------------------------

function Tframe_table.copySelected: boolean;
begin
   result := VstDetail.Focused();
   var CopyStrings := TStringList.Create;
   try
      VstDetailSelector.CopySelectedCells(CopyStrings, TraceConfig.TextExport_TextQualifier, TraceConfig.TextExport_Separator);
      var CopyText: PChar := CopyStrings.GetText;
      Clipboard.SetTextBuf(CopyText);
      StrDispose(CopyText);
   finally
      CopyStrings.Free ;
   end ;
end;

//------------------------------------------------------------------------------


end.
