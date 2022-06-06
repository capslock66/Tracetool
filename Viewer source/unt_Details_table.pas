unit unt_Details_table;

interface

uses
  system.Contnrs, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unt_Details_base, VirtualTrees,
  unt_Editor, Menus , ExtCtrls, clipbrd,
  unt_TraceWin ,
  unt_utility,
  unt_tool;            // VstEditor, IVstEditor, TMember

type
  PTableRec = ^TTableRec ;
  TTableRec = record
     Columns : TStringList ;
  end ;

  Tframe_table = class(Tframe_BaseDetails)
    VstTable: TVirtualStringTree;
    PopupDetail: TPopupMenu;
    CopyMenu: TMenuItem;
    N2: TMenuItem;
    SelectAllMenu: TMenuItem;
    procedure VstTableCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstTableDblClick(Sender: TObject);
    procedure VstTableEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstTableEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstTableFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstTableMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstTablePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstTableKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstTableGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VstTableChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
  public
    { Public declarations }
    TraceWin: TFrm_Trace;
    Constructor Create(AOwner: TComponent);  override ;
    Procedure AddDetails(TreeRec: PTreeRec; RootMember : TMember); override;
    function HasFocus : boolean ; override;
    procedure SelectAll() ; override;
    procedure copySelected() ; override;
  end;

var
  frame_table: Tframe_table;

implementation

uses
unt_TraceConfig ;

{$R *.dfm}

//------------------------------------------------------------------------------

constructor Tframe_table.Create(AOwner: TComponent);
begin
   inherited create (AOwner) ;
   
   TraceWin := TFrm_Trace(owner);

   VstTable.NodeDataSize := sizeof (TTableRec) ;
   //VstTable.Header.SortColumn := 0 ;
   VstTable.Header.MainColumn := 0 ;
   VstTable.Header.AutoSizeIndex := -1 ;     // 2

   // header must be visible to enable resize !
   VstTable.Header.Columns.Items[0].text := '' ;
   VstTable.Header.Columns.Items[1].text := '' ;
   VstTable.Header.Columns.Items[2].text := '' ;


   VstTable.Header.Options           := TraceWin.vstTrace.Header.Options ;
   VstTable.TreeOptions.AutoOptions  := TraceWin.vstTrace.TreeOptions.AutoOptions
             + [toDisableAutoscrollOnEdit] ; // Do not center a node horizontally when it is edited.

   VstTable.TreeOptions.PaintOptions := TraceWin.vstTrace.TreeOptions.PaintOptions
             - [toShowTreeLines] ;        // show tree lines in 'members' tree

   VstTable.TreeOptions.SelectionOptions := TraceWin.vstTrace.TreeOptions.SelectionOptions
             + [toExtendedFocus]          // Entries other than in the main column can be selected, edited etc.
             + [toFullRowSelect]          // selection highlight the whole line
             + [toMultiselect] ;          // don't Allow more than one node to be selected.

   VstTable.TreeOptions.MiscOptions := TraceWin.vstTrace.TreeOptions.MiscOptions
             - [toEditable]               // don't allow edition. Code is used to detect double click or F2 key
             - [toReportMode] ;           // Tree behaves like TListView in report mode.

   VstTable.Colors.UnfocusedSelectionColor       := TraceWin.vstTrace.Colors.UnfocusedSelectionColor ;
   VstTable.Colors.UnfocusedSelectionBorderColor := TraceWin.vstTrace.Colors.UnfocusedSelectionBorderColor ;

   //VstTable.OnDrawNode := DrawNode ;

end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   // scroll into view
   sender.ScrollIntoView (node,false,false);     // center and horizontally false
end;
//------------------------------------------------------------------------------

procedure Tframe_table.VstTableCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();    // unt_tool
      IVstEditor := VstEditor ;                     // unt_tool
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   SelectedNode := VstTable.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstTable.ScreenToClient(P);
   MouseNode := VstTable.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions + [toEditable] ;

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
      VstTable.EditNode(Node, VstTable.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
   VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
   DetailRec : PTableRec ;
   //c : integer ;
begin
   try
      DetailRec := Sender.GetNodeData(Node) ;
      DetailRec.Columns.free();
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace('VstTableFreeNode exception when resetting', e.message) ;
      end ;
   end ;

end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
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
      CellText := DetailRec.Columns[Column] ;
end;

//------------------------------------------------------------------------------
procedure Tframe_table.VstTableMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
//   h2,h3 : integer ;
//   DetailRec : PTableRec ;
   newNodeHeight : integer ;
begin
   if TraceWin.IsWatch then
      newNodeHeight := VstTable.DefaultNodeHeight
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

procedure Tframe_table.VstTablePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
//var
//   DetailRec : PTableRec ;
begin
//   DetailRec := Sender.GetNodeData(Node) ;
//   // force font
//   TraceWin.ChangeFontDetail ({IsTrace}false,TargetCanvas,  Column, DetailRec.fontDetails,(vsSelected in Node.States)) ;
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
   VstTable.Clear ;
   VstTable.header.Columns.Clear ;

   // first member, col1 is the title
   cols := getTabStrings(pchar(RootMember.Col1)) ;
   for c := 0 to cols.Count-1 do begin
      col := VstTable.header.Columns.Add ;
      col.options  := col.options + [coAllowFocus] ;  // ensure user can focus to this column
      col.MinWidth := 10 ;
      col.MaxWidth := 1000 ;
      col.Width := 100 ;
      col.Text := cols[c] ;
   end ;
   VstTable.Header.MainColumn := 0 ;
   VstTable.Header.AutoSizeIndex := -1 ;  // auto
   cols.Free ;

   // add lines
   for c := 0 to RootMember.SubMembers.Count -1 do begin
      SubMember := TMember (RootMember.SubMembers.Items[c]) ;
      DetailNode := VstTable.AddChild(nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstTable.ReinitNode(DetailNode,false);
      DetailNode.Align := (VstTable.DefaultNodeHeight div 2)-2 ;
      DetailRec := VstTable.GetNodeData(DetailNode) ;

      cols := getTabStrings(pchar(SubMember.Col1)) ;
      DetailRec.Columns := cols ; // free by OnFreeNodes
   end ;

   // resize all columns, using the header text and all lines
   AutosizeAll (VstTable) ;

   // force last column width to maximum
   VstTable.Header.Columns[VstTable.Header.Columns.Count-1].Width := 9000 ;

   VstTable.Visible := true ;
   TFrm_Trace(Owner).CurrentViewers.add(self) ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.SelectAll;
begin
   VstTable.SelectAll(false) ;
end;

//------------------------------------------------------------------------------

function Tframe_table.HasFocus: boolean;
begin
  result := Focused or VstTable.focused ;
end;

//------------------------------------------------------------------------------


procedure Tframe_table.copySelected;
var
   CopyStrings : TStringList ;
   CopyText: PChar;
   DetailRec : PTableRec ;

   procedure CopyDetail (TestNode : PVirtualNode);
   var
      node : PVirtualNode ;
      NewLine: string;
      c : integer ;
      CellText :string ;
      //col : TVirtualTreeColumn ;
   begin
      if VstTable.Selected [TestNode] then begin
         DetailRec := VstTable.GetNodeData(TestNode) ;
         if DetailRec = nil then
            exit ;
         NewLine := '' ;
         for c := 0 to VstTable.header.Columns.Count-1 do begin
            //col := VstTable.header.Columns[c] ;
            CellText := DetailRec.Columns[c] ;
            if NewLine = '' then
               NewLine := TraceConfig.TextExport_TextQualifier + CellText + TraceConfig.TextExport_TextQualifier
            else
               NewLine := NewLine + TraceConfig.TextExport_Separator  + TraceConfig.TextExport_TextQualifier + CellText + TraceConfig.TextExport_TextQualifier ;
         end ;
         CopyStrings.Add(NewLine);
      end ;

      // multi select
      node := TestNode.FirstChild ;
      while Node <> nil do begin
         CopyDetail (node) ;
         node := node.NextSibling ;
      end ;
   end ;
begin
   CopyStrings := TStringList.Create;
   try
      CopyDetail (VstTable.RootNode);
      CopyText := CopyStrings.GetText;
   finally
      CopyStrings.Free ;
   end ;

   try
      Clipboard.SetTextBuf(CopyText);
   finally
      StrDispose(CopyText);
   end;

end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vstTrace.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set

procedure Tframe_table.VstTableKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if CharCode = VK_F2 then
      VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions + [toEditable] ;
end;

end.
