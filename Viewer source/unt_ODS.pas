{

  Receive the OutputDebugString messages
  =======================================

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   

}

unit unt_ODS;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, registry,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, Menus, XMLDoc, XMLIntf, pscMenu ,
  application6,  // the generated delphi code for the XML schema (Application6.xsd)
  ComCtrls, ToolWin, ImgList,  ActnList , clipbrd, SyncObjs, Contnrs, Unt_Tool,
  DebugOptions , unt_base,  Buttons, unt_pageContainer, unt_editor , VstSort,unt_filter, untPrintPreview;

{$Include TraceTool.Inc}

const
   WM_ODS = WM_USER +  1 ;      // OutputDebugString

type

  //---------------------------------------------------------------------------------
  //---------------------------------------------------------------------------------

  // the thread that catch outputDebugString
  TODSThread = class(TThread)
  protected
     hCloseEvent : THandle;
     procedure Execute; override;
  end ;

  //---------------------------------------------------------------------------------

  PODSRec = ^TODSRec ;
  TODSRec = record
     OriginalOrder  : cardinal ;     // Original order when inserted. Used to Unsort nodes
     Time        : string ;    // time of send
     ProcessName : string ;    // optional : the name of the process that send traces
     LeftMsg     : string ;    // Left col
  end ;

  //---------------------------------------------------------------------------------

  TODSTemp = class
     OriginalOrder  : cardinal ;     // Original order when inserted. Used to Unsort nodes
     Time        : string ;    // time of send
     ProcessName : string ;    // optional : the name of the process that send traces
     LeftMsg     : string ;    // Left col
  end ;

  //---------------------------------------------------------------------------------

  TFrm_ODS = class(TFrmBase)
    VstDebugString: TVirtualStringTree;
    PanelOds: TPanel;
    PanelTraceInfo: TPanel;
    VSplitter: TSplitter;
    PanelTop: TPanel;
    TracesInfo: TLabel;
    butClose: TBitBtn;
    VstDetail: TVirtualStringTree;
    PanelGutter: TPanel;
    PopupTree: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Copycurrentcell1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    mnuTogglebookmark: TMenuItem;
    SelectAll1: TMenuItem;
    PopupDetail: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N2: TMenuItem;
    MenuItem1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure VstDebugStringGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: String);
    procedure VstDebugStringChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure butCloseClick(Sender: TObject);
    procedure VstDetailCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDetailDblClick(Sender: TObject);
    procedure VstDetailEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VstDetailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure VstDetailMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstDetailPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDebugStringHeaderDragged(Sender: TVTHeader;
      Column: TColumnIndex; OldPosition: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VstDebugStringDblClick(Sender: TObject);
    procedure VstDebugStringCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDebugStringEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VstDebugStringEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstDebugStringKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstDebugStringAfterPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure VstDebugStringCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VstDebugStringBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstDebugStringAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VstDebugStringFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstDetailFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure PanelGutterDblClick(Sender: TObject);
    procedure VstDebugStringMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstDebugStringPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstDebugStringEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

  private
    Sorter : TVstSort ;

    //procedure ODS (var Msg : TMessage);  message WM_ODS ;
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
    procedure WMStartEditingTrace(var Message: TMessage); message WM_STARTEDITING_TRACE;
    function CheckSearchRecord(ODSRec: PODSRec): boolean;
  public
    Gutter: TImage;
    ODSThread : TODSThread ;
    NodeToFocus : PVirtualNode ;
    LastModified : tDateTime ;
  public // TFrmBase
    procedure Print ; override ;
    procedure ClearWin ; override ;
    procedure SaveWin ; override ;
    procedure PauseWin ; override ;
    procedure ViewTraceInfo ; override ;
    procedure ViewProperty ; override ;
    procedure CopySelected ; override ;
    procedure CopyCurrentCell ; override ;
    procedure DeleteSelected ; override ;
    procedure SelectAll ; override ;
    procedure CheckAutoClear ; override ;
    procedure PageControlChange (); override ;
    procedure TimerInfo ; override ;
    procedure CloseWin ;  override ;
    procedure ResizeColumns ;  override ;
    procedure RefreshView ;       override ;
    procedure ShowFilter ;        override ;
    procedure ApplyFont ; override ;
    function  getMembers(Node : PVirtualNode) : TMember ; override ;
    function  SearchNext(start:boolean) : boolean ;        override ;
    function  SearchPrevious (atEnd:boolean) : boolean ;  override ;
  end;

var
  Frm_ODS     : TFrm_ODS;
  LastChildOrder : cardinal ;     // Order of the last child, used to insert sub nodes and unsort them

implementation

uses
   Unt_receiver
   , unt_about
   , Unt_linkedList
   , unt_TraceWin
   , unt_utility
   , unt_TraceConfig
   , unt_search; //,Unt_Tool;


{$R *.dfm}


//------------------------------------------------------------------------------

procedure TFrm_ODS.FormCreate(Sender: TObject);
begin
   inherited ;
   ApplyFont() ;  // set font name and size for the 2 trees (from XMLConfig)

   vst := VstDebugString ;
   with TPSCMenu.create (self) do begin
      DimLevel := 0 ;    // don't gray icon
      Active := true ;
   end ;

   // initialize sort
   LastChildOrder := 1 ;   // 0 is reserved for not yet ordered lines
   Sorter := TVstSort.create (self) ;
   Sorter.tree := VstDebugString ;
   Sorter.UtilityImages := Frm_Tool.UtilityImages ;
   Sorter.canUnsort := true ;

   // redirect some events to the sorter
   VstDebugString.onHeaderClick             := sorter.OnHeaderClick ;
   VstDebugString.OnKeyUp                   := sorter.OnKeyUp ;
   VstDebugString.onHeaderDrawQueryElements := sorter.OnHeaderDrawQueryElements ;
   VstDebugString.onAdvancedHeaderDraw      := sorter.OnAdvancedHeaderDraw ;
   // tips : don't forget to include the hoOwnerDraw in the VstDebugString.Header.Options

   // copy all options from main form
   VstDebugString.Colors.UnfocusedSelectionColor       := Frm_Trace.vstTrace.Colors.UnfocusedSelectionColor ;
   VstDebugString.Colors.UnfocusedSelectionBorderColor := Frm_Trace.vstTrace.Colors.UnfocusedSelectionBorderColor ;
   VstDebugString.NodeDataSize := sizeof (TODSRec) ;
   VstDebugString.Header.MainColumn := 3 ;
   VstDebugString.Header.AutoSizeIndex := -1 ;  // auto
   VstDebugString.Header.Options := VstDebugString.Header.Options
      - [hoDrag]              // columns cannot be moved
      + [hoOwnerDraw]
      + [hoDblClickResize] ;  // allows a column to resize itself to its largest entry

   VstDebugString.TreeOptions.AutoOptions      := Frm_Trace.vstTrace.TreeOptions.AutoOptions ;
   VstDebugString.TreeOptions.SelectionOptions := Frm_Trace.vstTrace.TreeOptions.SelectionOptions ;
   VstDebugString.TreeOptions.MiscOptions      := Frm_Trace.vstTrace.TreeOptions.MiscOptions ;
   VstDebugString.TreeOptions.PaintOptions     := Frm_Trace.vstTrace.TreeOptions.PaintOptions
                                                  - [toShowRoot] ; // don't show Root
   VstDetail.NodeDataSize := sizeof (TDetailRec) ;
   VstDetail.Header.MainColumn := 0 ;
   VstDetail.Header.AutoSizeIndex := -1 ;     // 2
   VstDetail.Header.Columns.Items[0].text := '' ;   // header must be visible to enable resize !
   VstDetail.Header.Columns.Items[1].text := '' ;
   VstDetail.Header.Options               := Frm_Trace.VstDetail.Header.Options ;
   VstDetail.TreeOptions.AutoOptions      := Frm_Trace.VstDetail.TreeOptions.AutoOptions ;
   VstDetail.TreeOptions.PaintOptions     := Frm_Trace.VstDetail.TreeOptions.PaintOptions ;
   VstDetail.TreeOptions.SelectionOptions := Frm_Trace.VstDetail.TreeOptions.SelectionOptions ;
   VstDetail.TreeOptions.MiscOptions      := Frm_Trace.VstDetail.TreeOptions.MiscOptions ;
   VstDetail.Colors.UnfocusedSelectionColor       := Frm_Trace.vstTrace.Colors.UnfocusedSelectionColor ;
   VstDetail.Colors.UnfocusedSelectionBorderColor := Frm_Trace.vstTrace.Colors.UnfocusedSelectionBorderColor ;


   LastModified := now ;
   if TraceConfig.Ods_Enabled = true then begin
      ODSThread := TODSThread.Create (true) ;   // create suspended
      ODSThread.hCloseEvent := CreateEvent( nil, True, False, nil );  // Create the close event
      ODSThread.FreeOnTerminate := true ;
      ODSThread.start ;
   end else begin
      ODSThread := nil ;
   end ;

end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   inherited;
   if filter <> nil then
      Filter.Free ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: String);
var
   ODSRec : PODSRec ;
begin
   CellText := '' ;
   ODSRec := Sender.GetNodeData(Node) ;
   if ODSRec = nil then
      exit ;

   case Column of
      0 : begin
             //LongTimeFormat := 'hh:mm:ss:zzz' ;
             CellText := ODSRec.Time ;
          end ;
      1 : begin
             if (TextType = ttNormal) and (IsSeparator (ODSRec.ProcessName)) then
                CellText := ' '  // check underline / TextType
             else
                CellText := ODSRec.ProcessName ;  // Actually it's the process id
          end ;
      2 : begin
             if (TextType = ttNormal) and (IsSeparator (ODSRec.LeftMsg)) then
                CellText := ' '  // check underline / TextType
             else
                CellText := ODSRec.LeftMsg ;
          end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
   ODSRec1,ODSRec2    : PODSRec ;
   cellText1, cellText2 : String ;
begin
   if Column = -1 then begin
      // no column : unsort or the 2 records are the same
      ODSRec1 := Sender.GetNodeData(Node1) ;
      ODSRec2 := Sender.GetNodeData(Node2) ;
      if ODSRec1.OriginalOrder <= ODSRec2.OriginalOrder then
         result := -1
      else
         result := 1 ;
      exit ;
   end ;
   // Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: UnicodeString
   TVirtualStringTree(Sender).OnGetText (Sender,Node1,Column,ttNormal,CellText1) ;
   TVirtualStringTree(Sender).OnGetText (Sender,Node2,Column,ttNormal,CellText2) ;
   Result := CompareText (CellText1,CellText2) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   ODSRec : PODSRec ;
   FirstSelect : PVirtualNode ;
   SecondSelect : PVirtualNode ;

   procedure AddOneLineDetail (Col1,col2 : String ) ;
   var
      DetailRec : PDetailRec ;
      DetailNode :  PVirtualNode ;
   begin
      DetailNode := VstDetail.AddChild(nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstDetail.ReinitNode(DetailNode,false);
      DetailNode.Align := (VstDetail.DefaultNodeHeight div 2)-2 ;
      DetailRec := VstDetail.GetNodeData(DetailNode) ;
      DetailRec.Col1 := col1 ;
      DetailRec.Col2 := col2 ;
      VstDetail.MultiLine[DetailNode] := true ;
   end ;
begin
   // scroll into view
   if Node <> nil then
      Sender.ScrollIntoView (Node,false,false);     // center and horizontally false

   // get first then second. If second is not nil then it's multiselect : disable info panel
   FirstSelect := VstDebugString.GetNextSelected (nil) ;
   if FirstSelect = nil then
      exit ;

   SecondSelect := VstDebugString.GetNextSelected (FirstSelect) ;
   if SecondSelect <> nil then
      exit ;

   if PanelTraceInfo.Visible = false then
      exit ;
   vstdetail.clear ;

   ODSRec := Sender.GetNodeData(FirstSelect) ;  // node

   // TraceInfo panel
   AddOneLineDetail ('Process Name'   , ODSRec.ProcessName) ;
   AddOneLineDetail ('Time'           , ODSRec.Time) ;
   AddOneLineDetail ('Message'  , ODSRec.LeftMsg) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   // scroll into view
   Sender.ScrollIntoView (Node,false,false);     // center and horizontally false
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();
      IVstEditor := VstEditor ;
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   //InternalTrace ('DetailDblClick ') ;
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

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_MEMBER, Integer(SelectedNode), 0);
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true; // not (Column = 0) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: String);
var
   DetailRec : PDetailRec ;
begin
   CellText := '' ;
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;

   case Column of
      0 : CellText := DetailRec.Col1 ;
      1 : CellText := DetailRec.Col2 ;
      2 : CellText := DetailRec.Col3 ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
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

procedure TFrm_ODS.VstDetailFreeNode(Sender: TBaseVirtualTree;Node: PVirtualNode);
var
   DetailRec : PDetailRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   DetailRec.Col1 := '' ;
   DetailRec.Col2 := '' ;
   DetailRec.Col3 := '' ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.WMStartEditingTrace(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstDebugString.EditNode(Node, VstDebugString.FocusedColumn);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// OutputDebugString support
{ TODSThread }

procedure TODSThread.Execute;
var
   AckEvent : THANDLE;
   ReadyEvent : THANDLE;
   SharedFile : THANDLE;
   SharedMem : pointer;
   ret : DWORD ;
   sa : SECURITY_ATTRIBUTES;
   sd : SECURITY_DESCRIPTOR;
   ODSTemp : TODSTemp ;

   //Buffer: Pointer;
   //dwSizeofDataToWrite : Word ;
   HandlesToWaitFor : array[0..1] of THandle;
begin
   sa.nLength := sizeof(SECURITY_ATTRIBUTES);
   sa.bInheritHandle := TRUE;
   sa.lpSecurityDescriptor := @sd;

   if not InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION) then
     Exit;

   if not SetSecurityDescriptorDacl(@sd, TRUE, nil{(PACL)NULL}, FALSE) then
     Exit;

   AckEvent := CreateEvent(@sa, FALSE, TRUE, 'DBWIN_BUFFER_READY');      // initial state = CLEARED !!!
   if AckEvent = 0 then
     Exit;

   ReadyEvent := CreateEvent(@sa, FALSE, FALSE, 'DBWIN_DATA_READY');     // initial state = CLEARED
   if ReadyEvent = 0 then
     Exit;

   SharedFile := CreateFileMapping( THandle(-1), @sa, PAGE_READWRITE, 0,4096, 'DBWIN_BUFFER');
   if SharedFile = 0 then
     Exit;
         
   SharedMem := MapViewOfFile( SharedFile, FILE_MAP_READ, 0,0,512);
   if SharedMem = nil then
     Exit;

   while not Terminated do begin
      HandlesToWaitFor[0] := hCloseEvent;
      HandlesToWaitFor[1] := ReadyEvent;

     SetEvent( AckEvent); // set ACK event to allow buffer to be used
     ret := WaitForMultipleObjects (2, @HandlesToWaitFor, False {bWaitAll}, 3000 {INFINITE});

     //ret := WaitForSingleObject(ReadyEvent, 10000 {INFINITE} );
     case ret of
       WAIT_TIMEOUT :
          Continue;

       WAIT_OBJECT_0 : begin  // hCloseEvent
                          break ;
                       end ;
       WAIT_OBJECT_0 + 1 : begin
          if TraceConfig.ods_Enabled then begin
             // cannot be added directly from a thread. Send the message to the main thread
             Criticalsection.Enter ;
             try
                ODSTemp := TODSTemp.create ;
                ODSTemp.Time        := FormatDateTime('hh:mm:ss:zzz',now) ;
                ODSTemp.ProcessName := GetExenameForProcess (LPDWORD(SharedMem) ^);  // '$' + inttohex (pThisPid^,2)
                ODSTemp.LeftMsg     := String(PAnsiChar(SharedMem) + sizeof(DWORD)) ;   // the native version of OutputDebugString is ASCII. result is always AnsiString
                ODSTemp.OriginalOrder := LastChildOrder ;
                inc (LastChildOrder) ;

                OdsMessageStack.Add(ODSTemp) ;
             finally
                Criticalsection.Release ;
             end;

          end ;
       end;
       WAIT_FAILED:       // Wait failed.  Shouldn't happen.
          Continue ;
     end;
   end;
   UnmapViewOfFile(SharedMem) ;
   CloseHandle(SharedFile) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.butCloseClick(Sender: TObject);
begin
   getPageContainer().actPause.Checked := true ;
   TraceConfig.ods_Enabled := not getPageContainer().actPause.Checked;
   CloseWin () ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//   TFrmBase
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TFrm_ODS.CloseWin;
begin
   // close windows force thread to shutdow, like the pause button
   visible := false ;
   PauseWin ; // Frm_Tool.actViewOutputDebugStringExecute (nil);
   IsDestroying := true ;
   undock() ;
   IsDestroying := false ;
end;

//------------------------------------------------------------------------------
// here we can modify the "actions" menu items
procedure TFrm_ODS.PageControlChange();
var
   PageContainer : TFrmPageContainer ;
begin
   PageContainer := getPageContainer() ;
   if PageContainer = nil then begin
      TFrm_Trace.InternalTrace ('TFrm_ODS.PageControlChange:PageContainer = nil') ;
      exit ;
   end ;
   PageContainer.actPrint        .Enabled := true ;
   PageContainer.actClear        .Enabled := true ;
   PageContainer.actSaveToFile   .Enabled := true ;
   PageContainer.actViewTraceInfo.Enabled := true ;
   PageContainer.actPause        .Enabled := true ;
   PageContainer.actCopy         .Enabled := true ;
   PageContainer.actDelete       .Enabled := true ;
   PageContainer.actCut          .Enabled := true ;
   PageContainer.actSelectAll    .Enabled := true ;
   PageContainer.actViewProperty .Enabled := false ;
   PageContainer.actSearch       .Enabled := true ;
   PageContainer.actFindNext     .Enabled := true ;

   PageContainer.actViewTraceInfo.checked := PanelTraceInfo.Visible ;
   PageContainer.actPause        .checked := not TraceConfig.ods_Enabled ;
end;

//------------------------------------------------------------------------------

//procedure TFrm_ODS.PageControlChanging();
//begin
//end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.ResizeColumns;
begin
   AutosizeAll (VstDebugString) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.ViewTraceInfo;
begin
   if getPageContainer().actViewTraceInfo.Checked then begin
      PanelTraceInfo.Visible := true ;
      VSplitter.Visible := true ;
      PanelTraceInfo.Left := VSplitter.Left + 10 ;
      VstDebugStringChange(VstDebugString,nil);
   end else begin
      PanelTraceInfo.Visible := false ;
      VSplitter.Visible := false ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.ViewProperty;
begin
  inherited;

end;

//------------------------------------------------------------------------------
// timer : show stat info
procedure TFrm_ODS.TimerInfo;
var
   ODSTemp : TODSTemp ;
   ODSRec : PODSRec ;
   node : pvirtualnode ;
   messageAdded : boolean ;
begin

   messageAdded := false ;
   while true do begin
      // get the first element in the MessageStack
      criticalsection.Enter ;
      try
         if OdsMessageStack.Count = 0 then begin
            ODSTemp := nil ;
         end else begin
            ODSTemp := TODSTemp( OdsMessageStack.Items[0] ) ;
            OdsMessageStack.Delete(0);  // MessageStack  don't own the objects
         end ;
      finally
         criticalsection.Leave ;
      end;

      if ODSTemp = nil then
         break ;  // quit the loop

      if messageAdded = false then
         VstDebugString.BeginUpdate ;
      messageAdded := true ;

      node := VstDebugString.AddChild (nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstDebugString.ReinitNode(node,false);
      ODSRec := VstDebugString.GetNodeData(node) ;
      ODSRec.Time          := ODSTemp.Time ;
      ODSRec.ProcessName   := ODSTemp.ProcessName ;
      ODSRec.LeftMsg       := ODSTemp.LeftMsg ;
      OdsRec.OriginalOrder := ODSTemp.OriginalOrder ;
      ODSTemp.Free ;             // free the message

      if TraceConfig.AppDisplay_FocusToReceivedMessage then
         NodeToFocus := node ;

      LastModified := now ;
      //VstDebugString.RepaintNode(node);

      CheckAutoClear() ;

      // check if the node can be displayed according the filters
      if Filter <> nil then
         Filter.CheckNode(node) ;

   end ;   // loop


   // autosort if at least one column in sort
   if Sorter.SortColumns.Count <> 0 then
      Sorter.sort (nil) ;
   if messageAdded then
      VstDebugString.endUpdate ;
   TracesInfo.Caption := TimeToStr(LastModified)
                         + ', not filtered lines : ' + inttostr(VstDebugString.RootNode.ChildCount) ;
   if NodeToFocus <> nil then begin
      VstDebugString.ClearSelection();
      VstDebugString.Selected [NodeToFocus] := true ;
      VstDebugString.FocusedNode := NodeToFocus;
      VstDebugString.ScrollIntoView (NodeToFocus,false,false);
   end;
   NodeToFocus := nil ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.PauseWin;
begin
   // pause win is also called when the application shutdown, we cannot save EnableODS in that case
   if Frm_ODS.Visible = true then
      TraceConfig.ods_Enabled := not getPageContainer().actPause.Checked;

   // if not visible then close ODS thread
   if Frm_ODS.Visible = false then begin
       if ODSThread <> nil then
          setevent (ODSThread.hCloseEvent) ;
       ODSThread := nil ;
       exit ;
   end ;

   if TraceConfig.ods_Enabled = false then begin
       if ODSThread <> nil then
          setevent (ODSThread.hCloseEvent) ;
       ODSThread := nil ;
   end else begin
       if ODSThread = nil then begin // no need to create it
          ODSThread := TODSThread.Create (true) ;   // create suspended
          ODSThread.hCloseEvent := CreateEvent( nil, True, False, nil );  // Create the close event
          ODSThread.FreeOnTerminate := true ;
          ODSThread.start ;
       end ;
   end ;
end;

//------------------------------------------------------------------------------
// called by ODS procedure when receiving message
procedure TFrm_ODS.CheckAutoClear;
begin
   // check the number of node
   if TraceConfig.Ods_AutoClear and (integer(VstDebugString.RootNode.ChildCount) >= TraceConfig.Ods_MaxNode) then begin

      VstDebugString.BeginUpdate() ;
      try
      if sorter.SortColumns.Count <> 0 then     // unsort before deleting old traces
         sorter.Unsort(nil);
         while integer(VstDebugString.RootNode.ChildCount) > TraceConfig.Ods_MinNode do begin
            VstDebugString.DeleteNode(VstDebugString.RootNode.FirstChild,false) ;
         end ;
      finally
         VstDebugString.EndUpdate() ;
      end ;

      if sorter.SortColumns.Count <> 0 then      // resort
         sorter.Sort(nil);

   end ;
end;

//------------------------------------------------------------------------------
procedure TFrm_ODS.ClearWin;
begin
   VstDebugString.clear ;
   vstdetail.clear ;
end;

//------------------------------------------------------------------------------
// Delete key : deleted selected
procedure TFrm_ODS.DeleteSelected;
var
   node : PVirtualNode ;
begin
   if VstDebugString.Focused = false then
      exit ;
   node := VstDebugString.GetFirstSelected ;
   if node = nil then  // no node selected
      exit ;
   node := VstDebugString.GetPreviousVisible(node) ;
   VstDebugString.DeleteSelectedNodes ;

   // case of the first node : GetPreviousVisible is nil ...
   if node = nil then
      node := VstDebugString.GetFirst
   else if VstDebugString.GetNextVisible(node) <> nil then
      node := VstDebugString.GetNextVisible(node) ;

   VstDebugString.FocusedNode := node ;
   VstDebugString.Selected [node] := true ;

   if node = nil then
     vstdetail.clear ;

end;

//------------------------------------------------------------------------------
// CTRL A : Select all
procedure TFrm_ODS.SelectAll;
begin
   // normally when the VstEditor is not nil, he is visible
   if ((VstDebugString.IsEditing) or VstDetail.IsEditing) and (IVstEditor <> nil) and (TMoveMemoEditLink(IVstEditor).IsVisible) then begin
      VstEditor.SelectAll() ;  // TMoveMemoEditLink(IVstEditor).SelectAll() ;
      exit ;
   end ;
   if VstDebugString.Focused = true then
      VstDebugString.SelectAll(true)       // select all visible items (don't select filtered items)
   else if VstDetail.Focused then
      VstDetail.SelectAll(false) ;         // select all (visible or invisible)
end;

//------------------------------------------------------------------------------
// CTRL C : Copy selected
procedure TFrm_ODS.CopySelected;
var
   CopyStrings: TStrings;
   CopyText: PChar;
   NewLine: string;
   IsFirst : boolean ;
   ODSRec : PODSRec ;
   TreeIndentation : String ;
   focusedComponent : hwnd ;

   procedure CheckIfNodeSelected (TestNode : PVirtualNode) ;
   var
      ChildVtNode : PVirtualNode ;
      LeftMsg : string ;
   begin
      if VstDebugString.Selected [TestNode] then begin
         ODSRec := VstDebugString.GetNodeData(TestNode) ;
         LeftMsg := ODSRec.LeftMsg ;

         IsFirst := true ;
         NewLine := '' ;

         if TraceConfig.TextExport_ProcessName then begin
            NewLine := TraceConfig.TextExport_TextQualifier + ODSRec.ProcessName + TraceConfig.TextExport_TextQualifier ;
            IsFirst := false ;
         end ;

         if TraceConfig.TextExport_Time then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator ;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + ODSRec.Time + TraceConfig.TextExport_TextQualifier ;
            IsFirst := false ;
         end ;

         if TraceConfig.TextExport_Col1 then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator ;

            NewLine := NewLine + string(StrRepeat(TreeIndentation, VstDebugString.GetNodeLevel(TestNode))) ;

            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + LeftMsg + TraceConfig.TextExport_TextQualifier ;
            IsFirst := false ;
         end ;

         CopyStrings.Add(NewLine);

      end ;
      ChildVtNode := TestNode.FirstChild ;
      while ChildVtNode <> nil do begin
         CheckIfNodeSelected (ChildVtNode) ;
         ChildVtNode := ChildVtNode.NextSibling ;
      end ;
   end ;

   procedure CopyDetail (TestNode : PVirtualNode);
   var
      node : PVirtualNode ;
      DetailRec : PDetailRec ;
   begin
      if VstDetail.Selected [TestNode] then begin

         DetailRec := VstDetail.GetNodeData(TestNode) ;
         NewLine := TraceConfig.TextExport_TextQualifier + DetailRec.Col1 + TraceConfig.TextExport_TextQualifier  +
                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + DetailRec.Col2 + TraceConfig.TextExport_TextQualifier +
                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + DetailRec.Col3 + TraceConfig.TextExport_TextQualifier  ;

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
   // reroute CTRL-C to the focused component if it's not the master tree
   if (VstDetail.Focused = false) and (VstDebugString.Focused = false) then begin
      focusedComponent := GetFocus ;
      if focusedComponent <> 0 then
         SendMessage(focusedComponent, WM_COPY, 0, 0);
      exit ;
   end ;

   if VstDebugString.GetFirstSelected = nil then
      exit ;

   TreeIndentation := StrRepeat(' ', TraceConfig.TextExport_TreeIndentation) ;

   SetCursor(Screen.Cursors[crHourGlass]);
   CopyStrings := TStringList.Create;
   try

      if VstDetail.Focused then begin
         CopyDetail (VstDetail.RootNode);
      end else begin
         // add title if needed.
         if TraceConfig.TextExport_GenerateColumnHeader then begin
            IsFirst := true ;
            NewLine := '' ;

            if TraceConfig.TextExport_ProcessName then begin
               NewLine := TraceConfig.TextExport_TextQualifier + 'ProcessName' + TraceConfig.TextExport_TextQualifier ;
               IsFirst := false ;
            end ;

            if TraceConfig.TextExport_Time then begin
               if IsFirst = false then
                  NewLine := NewLine + TraceConfig.TextExport_Separator ;
               NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Time' + TraceConfig.TextExport_TextQualifier ;
               IsFirst := false ;
            end ;

            if TraceConfig.TextExport_Col1 then begin
               if IsFirst = false then
                  NewLine := NewLine + TraceConfig.TextExport_Separator ;
               NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Msg' + TraceConfig.TextExport_TextQualifier ;
               IsFirst := false ;
            end ;

            CopyStrings.Add(NewLine);
         end ;

         // add node starting from the invisible root node (recursive)
         CheckIfNodeSelected (VstDebugString.RootNode) ;
      end ;

      CopyText := CopyStrings.GetText;
      try
         Clipboard.SetTextBuf(CopyText);
      finally
         StrDispose(CopyText);
      end;
   finally
      SetCursor(Screen.Cursors[crDefault]);
      CopyStrings.Free ;
   end;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.CopyCurrentCell;
var
   Node : PVirtualNode ;
   wCellText: String ;
   CellText : string ;
begin
   if VstDebugString.Focused then begin
      Node := VstDebugString.FocusedNode ;
      if node = nil then
         exit ;
      VstDebugStringGetText(VstDebugString, Node, VstDebugString.FocusedColumn,ttStatic,wCellText);   // ttStatic is used to get the real text
   end else if VstDetail.Focused then begin
      Node := VstDetail.FocusedNode ;
      if node = nil then
         exit ;
      VstDetailGetText(VstDetail, Node, VstDetail.FocusedColumn,ttNormal,wCellText);   // ttNormal
   end else begin
      exit ;
   end ;
   CellText := wCellText ;
   Clipboard.SetTextBuf(pchar(CellText));
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.SaveWin;
var
   XMLRootData : IXMLData ;
   MasterTVNode : PVirtualNode ;

   // recursive
   procedure generateNodeXML (NodeTag : IXMLNodeType; VtNode : PVirtualNode) ;
   var
      ChildXmlNode :  IXMLNodeType;  //   IXMLNode
      ChildVtNode : PVirtualNode ;
      ODSRec : PODSRec ;  
   begin
      if VtNode = nil then
         exit ;

      if NodeTag = nil then
         exit ;

      if Supports(NodeTag, IXMLnodeType) = false then
         exit ;

      ODSRec := VstDebugString.GetNodeData(VtNode) ;
      if ODSRec <> nil then begin   // ODSRec can be nil the first time when VtNode is vst.RootNode
         // save the tree col1, time and process. No col2, ID ,thid, icon, and members
         NodeTag.Text := ODSRec.LeftMsg ;
         NodeTag.Time := ODSRec.Time ;
         if ODSRec.ProcessName <> '' then
            NodeTag.Process := ODSRec.ProcessName ;
      end ;

      ChildVtNode := VtNode.FirstChild ;
      while ChildVtNode <> nil do begin
         ChildXmlNode := NodeTag.Node.Add ;
         // add recursive
         generateNodeXML (ChildXmlNode, ChildVtNode);
         ChildVtNode := ChildVtNode.NextSibling ;
      end ;
   end ;

begin
   Frm_Tool.SaveDialog1.InitialDir := TraceConfig.general_LastSavedPath ;
   Frm_Tool.SaveDialog1.Filter := 'Xml file (*.xml)|*.xml' ;
   if Frm_Tool.SaveDialog1.Execute = false then
      exit ;
   TraceConfig.general_LastSavedPath := ExtractFilePath(Frm_Tool.SaveDialog1.FileName) ;

   application.ProcessMessages ;
   SetCursor(Screen.Cursors[crHourGlass]);

   try
      // create the header
      XMLRootData := NewData ;

      // generate nodes
      MasterTVNode := VstDebugString.RootNode ;
      generateNodeXML (XMLRootData ,MasterTVNode) ;  // recursive

      XMLRootData.OwnerDocument.SaveToFile(Frm_Tool.SaveDialog1.FileName);
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
   VstDebugStringChange (VstDebugString,nil);
   VstDebugString.Header.MainColumn := VstDebugString.Header.Columns.GetFirstVisibleColumn ;
   AutosizeAll (VstDebugString) ;
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set

procedure TFrm_ODS.VstDebugStringDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   //InternalTrace ('DetailDblClick ') ;
   SelectedNode := VstDebugString.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstDebugString.ScreenToClient(P);
   MouseNode := VstDebugString.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstDebugString.TreeOptions.MiscOptions := VstDebugString.TreeOptions.MiscOptions + [toEditable] ;

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_TRACE, Integer(SelectedNode), 0);
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set
procedure TFrm_ODS.VstDebugStringKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if CharCode = VK_F2 then
      VstDebugString.TreeOptions.MiscOptions := VstDebugString.TreeOptions.MiscOptions + [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrm_ODS.VstDebugStringEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstDebugString.TreeOptions.MiscOptions := VstDebugString.TreeOptions.MiscOptions - [toEditable] ;
end;

procedure TFrm_ODS.VstDebugStringEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrm_ODS.VstDebugStringEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
   VstDebugString.TreeOptions.MiscOptions := VstDebugString.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();
      IVstEditor := VstEditor ;
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

function TFrm_ODS.CheckSearchRecord (ODSRec : PODSRec) : boolean ;
begin
   result := false ;

   if (MatchSearch (ODSRec.LeftMsg) <> 0) or
      (MatchSearch (ODSRec.ProcessName) <> 0) then begin
      result := true ;
      exit ;
   end ;
end ;

//------------------------------------------------------------------------------
function  TFrm_ODS.SearchNext(start:boolean) : boolean ;
var
   currentNode : PVirtualNode ;
   ODSRec : PODSRec ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if start = true then begin
      currentNode := VstDebugString.GetFirstVisible() ;
   end else begin
      currentNode := VstDebugString.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstDebugString.GetFirst
      else  // when start is false, we are searching in the current document
         currentNode := VstDebugString.GetNextVisible(currentNode) ;   // skip the first selected
   end ;

   while currentNode <> nil do begin
      ODSRec := VstDebugString.GetNodeData (currentNode) ;
      if CheckSearchRecord (ODSRec) then begin
         if ActiveTracePage <> self then   
            SetActivePage() ;
         VstDebugString.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstDebugString.ClearSelection;
         VstDebugString.Selected [currentNode] := true ;
         VstDebugString.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstDebugString.GetNextVisible(currentNode) ;
   end ;
end;

//------------------------------------------------------------------------------

function TFrm_ODS.SearchPrevious(atEnd: boolean): boolean;
var
   currentNode : PVirtualNode ;
   ODSRec : PODSRec ;
   procedure CheckVisible () ;
   begin
      while (currentNode <> nil) and (VstDebugString.IsVisible[currentNode] = false) do begin
         currentNode := VstDebugString.GetPrevious(currentNode) ;
      end ;
   end ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if atEnd = true then begin
      currentNode := VstDebugString.GetLast() ;
   end else begin
      currentNode := VstDebugString.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstDebugString.GetLastVisible()
      else  // when atEnd is false, we are searching in the current document
         currentNode := VstDebugString.GetPrevious(currentNode) ;   // skip the first selected
   end ;

   CheckVisible() ;
   while currentNode <> nil do begin
      ODSRec := VstDebugString.GetNodeData(currentNode) ;
      if CheckSearchRecord (ODSRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         // fully visible ?
         VstDebugString.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstDebugString.ClearSelection;
         VstDebugString.Selected [currentNode] := true ;
         VstDebugString.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstDebugString.GetPrevious(currentNode) ;
      CheckVisible() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.RefreshView;
begin
   VstDebugString.Refresh ;
   VstDetail.Refresh ;
end;

//------------------------------------------------------------------------------

// if the paint area is modified, AfterPaint is called to redisplay the gutter

procedure TFrm_ODS.VstDebugStringAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
   Node : pvirtualNode ;
   BaseOffset : Integer;  // top position of the top node to draw given in absolute tree coordinates
   DispRec, CliRect : TRect ;
   Yposition : integer ;
   NodeHeight : integer ;
   ODSRec : PODSRec ;
   HeaderHeight : integer ;
   gutterCanvas : TCanvas ;
   newgutter : timage ;
   BookmarkPos : integer ;
begin
   // detect header height
   if VstDebugString.Header.Columns.Count <> 0 then begin
      // get the header height from the first header column
      // since the VT.headerRect property is protected :-(
      HeaderHeight := VstDebugString.Header.Columns[0].GetRect.Bottom ;
   end else begin  // should not happens
      HeaderHeight := VstDebugString.header.Height + 2 ;  // plus somme bevels
   end ;

   newgutter := timage.Create(self);
   newgutter.width  := PanelGutter.Width ;
   newgutter.Height := PanelGutter.Height ;
   newgutter.top    := 0 ;
   newgutter.left   := 0 ;
   newgutter.OnDblClick := PanelGutterDblClick ;
   gutterCanvas := newgutter.Canvas ;

   // clear gutter
   CliRect := Rect(0, HeaderHeight, PanelGutter.Width, PanelGutter.Height);
   gutterCanvas.Brush.Color := clBtnFace ;  //clgreen ; //clBtnFace ;
   gutterCanvas.FillRect(CliRect);

   // Determine node to start drawing with.
   BaseOffset := 0 ;
   Node := VstDebugString.GetNodeAt(0, 0, true, BaseOffset);
   if node <> nil then begin    // nothing to display
      // get the first visible node rectangle.
      DispRec := VstDebugString.GetDisplayRect (Node,NoColumn,false,false) ;

      // We just need the TOP node position
      // This top position is zero or negative since the node can be partially visible
      // but can never be more than zero (else we can have previous partial visible node before)
      Yposition := DispRec.Top ;

      // add Header height
      inc (Yposition , HeaderHeight) ;

      // draw each node
      while node <> nil do begin
         NodeHeight := VstDebugString.NodeHeight[Node] ;
         ODSRec := VstDebugString.GetNodeData(Node) ;

         BookmarkPos := bookmarks.IndexOf(Node) ;
         if BookmarkPos <> -1 then begin
            if (bookmarks.Count = 1) or (BookmarkPos > 9) then begin    // only one element or bookmark > 9
               Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 24) ;               // normal rectangle
               if BookmarkPos > 9 then
                  Frm_Tool.UtilityImages.Draw(gutterCanvas, 6 , Yposition+8 ,12) ;       // write "..."
            end else begin    //
               Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 25) ;               // bottom right corner is empty
               Frm_Tool.UtilityImages.Draw(gutterCanvas, 6 , Yposition+8 ,BookmarkPos);  // write the number
            end ;
         end else begin
            if (SearchText <> '') and (SearchKind = mrYesToAll) then
                if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
                  if CheckSearchRecord (ODSRec) then    // check if the node or one of his child match the search text
                     Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 21);
         end ;

         // draw the small dot indicate sub members
         //if (treeRec.Members <> nil) and (treeRec.Members.SubMembers.Count <> 0) then
         //   Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 15);

         inc (Yposition , NodeHeight) ;
         Node := VstDebugString.GetNextVisible(Node) ;
      end ;
   end ;

   // draw the header to hide bullet on previous line.
   CliRect := Rect(0, 0, PanelGutter.Width, HeaderHeight);
   gutterCanvas.Brush.Color := clBtnFace ;
   gutterCanvas.FillRect(CliRect);
   DrawEdge(gutterCanvas.Handle, CliRect, EDGE_RAISED, BF_BOTTOM );

   // replace the old gutter by the new one
   newgutter.Parent := PanelGutter ;
   newgutter.BringToFront ;

   if gutter <> nil then begin
      gutter.parent := nil ;
      gutter.Free ;
   end ;
   gutter := newgutter ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   ODSRec : PODSRec ;
begin
   try
   if bookmarks.IndexOf(node) <> -1 then begin
      DrawHighlight (TargetCanvas, CellRect,true) ;
      exit ;
   end ;
   except
   end ;

   // check if highlight must be draw
   if (SearchText = '') or (SearchKind <> mrYesToAll) then
      exit ;

   ODSRec := VstDebugString.GetNodeData(Node) ;
   if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
      if CheckSearchRecord (ODSRec) then     // check if the node or one of his child match the search text
         DrawHighlight (TargetCanvas, CellRect,false) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
   CellText: String ;
   middle : integer ;
begin
   VstDebugStringGetText(Sender, Node,Column,ttStatic,CellText);   // ttStatic is used to get the real text
   if IsSeparator(CellText) then begin
      TargetCanvas.Pen.Color := clBlack;
      middle := CellRect.Bottom div 2 ;
      if copy (trim(CellText),1,1) = '-' then begin
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

procedure TFrm_ODS.ShowFilter;
begin
   if Filter = nil then
      Filter := TFrmFilter.create (self) ;

   Filter.Vst := VstDebugString ;
   Filter.base := self ;
   Filter.ColumnNameList.Clear ;
   Filter.ColumnNameList.AddObject('Time'       , tObject(0)) ;
   Filter.ColumnNameList.AddObject('Process Id' , tObject(1)) ;
   Filter.ColumnNameList.AddObject('Lines'      , tObject(2)) ;

   filter.FillColumns() ;
   Filter.ShowModal() ;

end;

//----------------------------------------------------------------------------------------------------------------------

function TFrm_ODS.getMembers(Node: PVirtualNode): TMember;
begin
   result := nil ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
   ODSRec : PODSRec ;
   idx : integer ;
begin
   // delete from bookmark list
   if bookmarks <> nil then begin
      idx := bookmarks.IndexOf(node) ;
      if idx <> -1 then
         bookmarks.Delete(idx);
   end ;

   ODSRec := VstDebugString.GetNodeData(Node) ;
   ODSRec.Time        := '' ;
   ODSRec.ProcessName := '' ;
   ODSRec.LeftMsg     := '' ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.PanelGutterDblClick(Sender: TObject);
var
   Node : PVirtualNode ;
   P: TPoint;
   index : integer ;
begin

   GetCursorPos(P);
   P := VstDebugString.ScreenToClient(P);
   Node := VstDebugString.GetNodeAt(0, P.Y) ;
   if Node = nil then
      exit ;

   index := bookmarks.IndexOf(Node) ;
   if index = -1 then begin
      bookmarks.Add(Node) ;
   end else begin
      bookmarks.Delete(index) ;
   end ;

   VstDebugString.InvalidateNode(Node) ;
end;

//------------------------------------------------------------------------------

// main tree : fixed node height
procedure TFrm_ODS.VstDebugStringMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
   NodeHeight := TraceConfig.ods_Trace_NodeHeight ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDebugStringPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.Ods_Trace_FontName ;
   TargetCanvas.Font.size := TraceConfig.Ods_Trace_FontSize ;
end;

//------------------------------------------------------------------------------

// member tree : node height depend of the number of lines (variable node height)
procedure TFrm_ODS.VstDetailMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
   h2 : integer ;
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.Ods_Trace_FontName ;
   TargetCanvas.Font.size := TraceConfig.Ods_Trace_FontSize ;
   NodeHeight := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,0) ;
   h2 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,1) ;

   if h2 > NodeHeight then
      NodeHeight := h2 ;

   // should not happens : node contains at least a title in the first col
   if NodeHeight = 0 then
      NodeHeight := VstDetail.defaultNodeHeight ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.VstDetailPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.Ods_Info_FontName ;
   TargetCanvas.Font.size := TraceConfig.Ods_Info_FontSize ;
   if Column = 0 then
      if node.Parent = VstDetail.RootNode then
         TargetCanvas.font.Style := [fsBold] ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.ApplyFont;
begin
   VstDebugString.BeginUpdate ;
   VstDebugString.Font.Name         := TraceConfig.ods_Trace_FontName ;
   VstDebugString.Font.Size         := TraceConfig.ods_Trace_FontSize ;
   VstDebugString.DefaultNodeHeight := TraceConfig.ods_Trace_NodeHeight ;
   VstDebugString.ReinitChildren (nil,true);
   VstDebugString.EndUpdate ;
   VstDebugString.Refresh ;

   VstDetail.BeginUpdate ;
   VstDetail.Font.Name         := TraceConfig.ods_Info_FontName ;
   VstDetail.Font.Size         := TraceConfig.ods_Info_FontSize ;
   VstDetail.ReinitChildren (nil,true);
   VstDetail.EndUpdate ;
   VstDetail.Refresh ;
end;

//------------------------------------------------------------------------------

procedure TFrm_ODS.Print;
begin
   FrmPrintPreview.initialize(vstDebugString, nil) ;
   FrmPrintPreview.ShowModal ;
end;

end.
