unit unt_Details_Classic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unt_Details_base, VirtualTrees, Menus , clipbrd, 
  unt_Editor,           // TMoveMemoEditLink, const
  unt_TraceWin ,
  unt_search ,
  unt_tool,             // VstEditor, IVstEditor, TMember
  unt_utility;          // IsSeparator

type
  Tframe_Classic = class(Tframe_BaseDetails)
    VstDetail: TVirtualStringTree;
    PopupDetail: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    procedure VstDetailCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDetailDblClick(Sender: TObject);
    procedure VstDetailEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstDetailEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstDetailFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstDetailMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstDetailPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VstDetailAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VstDetailKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstDetailEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
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
  frame_Classic: Tframe_Classic;

implementation

uses
unt_TraceConfig ;

{$R *.dfm}

//------------------------------------------------------------------------------

constructor Tframe_Classic.Create(AOwner: TComponent);
begin
   inherited create (AOwner) ;
   
   TraceWin := TFrm_Trace(owner);

   VstDetail.parent := TraceWin.PanelRight ;
   VstDetail.Align := alClient ;
   VstDetail.top := 0 ;
   VstDetail.Height := 200 ;

   VstDetail.NodeDataSize := sizeof (TDetailRec) ;
   //VstDetail.Header.SortColumn := 0 ;
   VstDetail.Header.MainColumn := 0 ;
   VstDetail.Header.AutoSizeIndex := -1 ;     // 2

   // header must be visible to enable resize !
   VstDetail.Header.Columns.Items[0].text := '' ;
   VstDetail.Header.Columns.Items[1].text := '' ;
   VstDetail.Header.Columns.Items[2].text := '' ;


   VstDetail.Header.Options           := TraceWin.vstTrace.Header.Options ;
   VstDetail.TreeOptions.AutoOptions  := TraceWin.vstTrace.TreeOptions.AutoOptions
             + [toDisableAutoscrollOnEdit] ; // Do not center a node horizontally when it is edited.

   VstDetail.TreeOptions.PaintOptions := TraceWin.vstTrace.TreeOptions.PaintOptions
             + [toShowTreeLines] ;        // show tree lines in 'members' tree

   VstDetail.TreeOptions.SelectionOptions := TraceWin.vstTrace.TreeOptions.SelectionOptions
             + [toExtendedFocus]          // Entries other than in the main column can be selected, edited etc.
             + [toFullRowSelect]          // selection highlight the whole line
             + [toMultiselect] ;          // don't Allow more than one node to be selected.

   VstDetail.TreeOptions.MiscOptions := TraceWin.vstTrace.TreeOptions.MiscOptions
             - [toEditable]               // don't allow edition. Code is used to detect double click or F2 key
             - [toReportMode] ;           // Tree behaves like TListView in report mode.

   VstDetail.Colors.UnfocusedSelectionColor       := TraceWin.vstTrace.Colors.UnfocusedSelectionColor ;
   VstDetail.Colors.UnfocusedSelectionBorderColor := TraceWin.vstTrace.Colors.UnfocusedSelectionBorderColor ;

   //VstDetail.OnDrawNode := DrawNode ;

end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailChange(Sender: TBaseVirtualTree;  Node: PVirtualNode);
begin
   // scroll into view
   Sender.ScrollIntoView (node,false,false);     // center and horizontally false
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();    // unt_tool
      IVstEditor := VstEditor ;                     // unt_tool
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailDblClick(Sender: TObject);
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

procedure Tframe_Classic.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
   VstDetail.TreeOptions.MiscOptions := VstDetail.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure Tframe_Classic.VstDetailEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
   VstDetail.TreeOptions.MiscOptions := VstDetail.TreeOptions.MiscOptions - [toEditable] ;
end;

procedure Tframe_Classic.VstDetailEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   allowed := true;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
   DetailRec : PDetailRec ;
   //c : integer ;
begin
   try
      DetailRec := Sender.GetNodeData(Node) ;
      DetailRec.Col1 := '' ;
      DetailRec.Col2 := '' ;
      DetailRec.Col3 := '' ;
      //for c := 0 to length(DetailRec.FontDetails) -1 do
      //   DetailRec.FontDetails[c].Free ;
      setlength(DetailRec.FontDetails,0) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace('VstDetailFreeNode exception when resetting', e.message) ;
      end ;
   end ;

end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
   DetailRec : PDetailRec ;
begin
   CellText := '' ;
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;

   // check if separator must be draw
   CellText := DetailRec.Col1 ;
   if ( IsSeparator (CellText)) and (DetailRec.Col2 = '') and (DetailRec.Col3 = '') then begin
      CellText := '' ;   // don't display any text. separtor is draw in VstDetailAfterCellPaint procedure
      exit ;
   end ;

   case Column of
      0 : CellText := DetailRec.Col1 ;
      1 : CellText := DetailRec.Col2 ;
      2 : CellText := DetailRec.Col3 ;
   end ;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   DetailRec : PDetailRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   if (SearchText <> '') {and (SearchKind = mrYesToAll)} then begin  //  mrYesToAll means Highlight all

      case Column of
         0 : if (MatchSearch (DetailRec.col1) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
         1 : if (MatchSearch (DetailRec.col2) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
         2 : if (MatchSearch (DetailRec.col3) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
      end ;
      //if (MatchSearch (DetailRec.col1) <> 0) or
      //   (MatchSearch (DetailRec.col2) <> 0) or
      //   (MatchSearch (DetailRec.col3) <> 0) then begin
      //   DrawHighlight (TargetCanvas, CellRect,false) ;
      //end ;
   end;
end;

//------------------------------------------------------------------------------
// draw separator if DetailRec.Col1 contains at least 3 chars '-' or '='
// if other type chars (including spaces) are present, not lines are draw
// the 2 others columns must be empty
procedure Tframe_Classic.VstDetailAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
   DetailRec : PDetailRec ;
   CellText: string ;
   middle : integer ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;


   CellText := DetailRec.Col1 ;
   if ( IsSeparator (CellText)) and (DetailRec.Col2 = '') and (DetailRec.Col3 = '') then begin
      TargetCanvas.Pen.Color := TargetCanvas.Font.Color ; // clBlack;

      middle := (CellRect.Bottom div 2) - 2 ;   // remove 2 points to display the separator on the position as the horizontal tree line
      if copy (cellText,1,1) = '-' then begin
         TargetCanvas.MoveTo(0, middle);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle );
      end else begin // '='
         TargetCanvas.MoveTo(0, middle - 1);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle - 1 );
         TargetCanvas.MoveTo(0, middle + 1);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle + 1 );
      end ;
   end ;

end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
   h2,h3 : integer ;
   DetailRec : PDetailRec ;
   newNodeHeight : integer ;
begin
   if TraceWin.IsWatch then
      newNodeHeight := VstDetail.DefaultNodeHeight
   else
      newNodeHeight := TraceConfig.Framework_info_NodeHeight ;

   DetailRec := Sender.GetNodeData(Node) ;

   // force font
   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  0, DetailRec.fontDetails,true) ;   // Watch/Framework , Trace/info
   NodeHeight := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,0) ;

   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  1, DetailRec.fontDetails,true) ;
   h2 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,1) ;

   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  2, DetailRec.fontDetails,true) ;
   h3 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,2) ;

   if h2 > NodeHeight then
      NodeHeight := h2 ;

   if h3 > NodeHeight then
      NodeHeight := h3 ;

   // if multiline, NodeHeight is bigger than DefaultNodeHeight
   if NodeHeight = 0 then 
      NodeHeight := newNodeHeight ;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.VstDetailPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
   DetailRec : PDetailRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   // force font
   TraceWin.ChangeFontDetail ({IsTrace}false,TargetCanvas,  Column, DetailRec.fontDetails,(vsSelected in Node.States)) ;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.AddDetails(TreeRec: PTreeRec; RootMember: TMember);

   procedure AddMemberToDetail (Member : TMember ; MasterNode : PVirtualNode) ;
   var
      c,d : integer ;
      ArrayLength : integer ;
      SubMember : TMember ;
      DetailNode :  PVirtualNode ;
      DetailRec : PDetailRec ;
   begin

      DetailNode := VstDetail.AddChild(MasterNode) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstDetail.ReinitNode(DetailNode,false);
      DetailNode.Align := (VstDetail.DefaultNodeHeight div 2)-2 ;
      DetailRec := VstDetail.GetNodeData(DetailNode) ;
      DetailRec.Col1 := Member.col1 ;
      DetailRec.Col2 := Member.col2 ;
      DetailRec.Col3 := Member.col3 ;

      //copy fontDetails Member
      ArrayLength := length(Member.FontDetails) ;
      setlength (DetailRec.FontDetails, ArrayLength) ;
      for d := 0 to ArrayLength-1 do begin
         DetailRec.FontDetails [d] := Member.FontDetails[d] ;
      end ;
      VstDetail.MultiLine[DetailNode] := true ;

      for c := 0 to Member.SubMembers.Count -1 do begin
         SubMember := TMember (Member.SubMembers.Items[c]) ;
         AddMemberToDetail(SubMember , DetailNode);  // recursive add submembers
      end ;
   end ;

begin
   if RootMember.SubMembers.Count > 0 then
        TraceWin.VstDetailHaschildren := true ;
   AddMemberToDetail (RootMember , nil) ;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.SelectAll;
begin
   VstDetail.SelectAll(false) ;
end;

//------------------------------------------------------------------------------

function Tframe_Classic.HasFocus: boolean;
begin
  result := Focused or VstDetail.Focused;
end;

//------------------------------------------------------------------------------

procedure Tframe_Classic.copySelected;
var
   CopyStrings : TStringList ;
   CopyText: PChar;
begin
   CopyStrings := TStringList.Create;
   try
      CopyDetail (VstDetail, CopyStrings, VstDetail.RootNode);
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
procedure Tframe_Classic.VstDetailKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if CharCode = VK_F2 then
      VstDetail.TreeOptions.MiscOptions := VstDetail.TreeOptions.MiscOptions + [toEditable] ;
end;

end.
