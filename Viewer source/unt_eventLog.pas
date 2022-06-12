{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_eventLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Clipbrd, xmldoc ,
  Dialogs, unt_base, StdCtrls, Buttons, VirtualTrees, Unt_Tool, pscMenu ,
  ExtCtrls, ComCtrls,Eventlog, unt_tracewin, unt_PageContainer, unt_editor, vstSort,unt_filter,
  Menus, untPrintPreview;

type

  PEvntLogRec = ^TEvntLogRec ;
  TEvntLogRec = record
     EventRecordNum : cardinal ; // Original order when inserted. Used to Unsort nodes
     Time           : string ;   // time
     Source         : string ;   // EventLog.EventSource
     MessageText    : string ;   // EventLog.EventMessageText
     Members        : TMember ;
     EventIcon      : integer ;
  end ;

  TFrmEventLog = class(TFrmBase)
    GroupPanel: TPanel;
    VSplitter: TSplitter;
    VstEvent: TVirtualStringTree;
    PanelTraceInfo: TPanel;
    VstDetail: TVirtualStringTree;
    PanelTop: TPanel;
    TracesInfo: TLabel;
    butClose: TBitBtn;
    butReload: TBitBtn;
    butGetAll: TBitBtn;
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
    procedure VstEventChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstEventGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VstEventFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstDetailGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VstEventGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure butGetAllClick(Sender: TObject);
    procedure butReloadClick(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
    procedure VstEventHeaderDragged(Sender: TVTHeader;
      Column: TColumnIndex; OldPosition: Integer);
    procedure VstDetailCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDetailDblClick(Sender: TObject);
    procedure VstDetailMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstDetailPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VstEventCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstEventDblClick(Sender: TObject);
    procedure VstEventEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstEventEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstEventKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstEventAfterPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure VstEventCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VstEventBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstEventAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VstDetailFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure PanelGutterDblClick(Sender: TObject);
    procedure VstEventMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstEventPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstDetailEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VstEventEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);

  private
    fLogName : string ;
    fEventLog : TEventLog ;
    LastModified : tDateTime ;
    LastRead : integer ;
    Sorter : TVstSort ;
    LastChildOrder : cardinal ;     // Order of the last child, used to insert sub nodes and unsort them
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
    procedure WMStartEditingTrace(var Message: TMessage); message WM_STARTEDITING_TRACE;
    procedure OnEventLogMessage(Sender: TEventLog);
    procedure AddLogToTree(Sender: TEventLog) ;
    procedure LoadLast(LineToRead: integer);
    function CheckSearchRecord(EvntLogRec: PEvntLogRec): boolean;

  public
    Gutter: TImage;
    IsPaused : boolean ;
    NodeToFocus : PVirtualNode ;
    procedure SetEventLog(LogName: string; LineToRead: integer);

  public // TFrmBase
    procedure Print ; override ;
    procedure ClearWin ; override ;
    procedure SaveWin ; override ;
    procedure PauseWin ; override ;
    procedure ViewTraceInfo ; override ;
    procedure CopySelected ; override ;
    procedure CopyCurrentCell ; override ;
    procedure DeleteSelected ; override ;
    procedure SelectAll ; override ;
    procedure CheckAutoClear ; override ;
    procedure PageControlChange (); override ;
    procedure TimerInfo ; override ;
    procedure CloseWin ;  override ;
    procedure ResizeColumns ;  override ;
    procedure RefreshView ; override ;
    procedure ShowFilter ;  override ;
    procedure ApplyFont ; override ;
    function  getMembers(Node : PVirtualNode) : TMember ; override ;
    function  SearchNext(start:boolean) : boolean ; override ;
    function  SearchPrevious (atEnd:boolean) : boolean ;  override ;
  end;

var
  FrmEventLog: TFrmEventLog;

implementation

uses
   unt_selectEvent
   , unt_ODS
   , unt_utility
   , DebugOptions
   , application6
   , unt_TraceConfig
   , unt_search;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrmEventLog.FormCreate(Sender: TObject);
begin
   inherited ;
   ApplyFont() ;  // set font name and size for the 2 trees (from XMLConfig)

   vst := VstEvent ;
   with TPSCMenu.create (self) do begin
      DimLevel := 0 ;    // don't gray icon
      Active := true ;
   end ;

   // initialize sort
   LastChildOrder := 1 ;   // 0 is reserved for not yet ordered lines
   Sorter := TVstSort.create (self) ;
   Sorter.tree := VstEvent ;
   Sorter.UtilityImages := Frm_Tool.UtilityImages ;
   Sorter.canUnsort := true ;

   // redirect some events to the sorter
   VstEvent.onHeaderClick             := sorter.OnHeaderClick ;
   VstEvent.OnKeyUp                   := sorter.OnKeyUp ;
   VstEvent.onHeaderDrawQueryElements := sorter.OnHeaderDrawQueryElements ;
   VstEvent.onAdvancedHeaderDraw      := sorter.OnAdvancedHeaderDraw ;
   // tips : don't forget to include the hoOwnerDraw in the VstEvent.Header.Options

   // copy all options from main form
   VstEvent.Colors.UnfocusedSelectionColor       := Frm_Trace.vstTrace.Colors.UnfocusedSelectionColor ;
   VstEvent.Colors.UnfocusedSelectionBorderColor := Frm_Trace.vstTrace.Colors.UnfocusedSelectionBorderColor ;
   VstEvent.NodeDataSize := sizeof (TEvntLogRec) ;
   VstEvent.Header.MainColumn := 2 ;
   VstEvent.Header.AutoSizeIndex := -1 ;  // auto
   VstEvent.Header.Options := VstEvent.Header.Options
      - [hoDrag]              // columns cannot be moved
      + [hoOwnerDraw]         // needed for sort : header items with the owner draw style can be drawn by the application via event
      + [hoDblClickResize] ;  // allows a column to resize itself to its largest entry

   VstEvent.TreeOptions.AutoOptions      := Frm_ODS.VstDebugString.TreeOptions.AutoOptions ;
   VstEvent.TreeOptions.PaintOptions     := Frm_ODS.VstDebugString.TreeOptions.PaintOptions ;
   VstEvent.TreeOptions.SelectionOptions := Frm_ODS.VstDebugString.TreeOptions.SelectionOptions ;
   VstEvent.TreeOptions.MiscOptions      := Frm_ODS.VstDebugString.TreeOptions.MiscOptions ;

   VstDetail.NodeDataSize := 4 ;
   VstDetail.Header.MainColumn := 0 ;

   // header must be visible to enable resize !
   VstDetail.Header.Columns.Items[0].text := '' ;
   VstDetail.Header.Columns.Items[1].text := '' ;
   VstDetail.Header.Columns.Items[2].text := '' ;
   VstDetail.Header.Options := Frm_Trace.VstDetail.Header.Options ;

   VstDetail.TreeOptions.AutoOptions      := Frm_Trace.VstDetail.TreeOptions.AutoOptions ;
   VstDetail.TreeOptions.PaintOptions     := Frm_Trace.VstDetail.TreeOptions.PaintOptions ;
   VstDetail.TreeOptions.SelectionOptions := Frm_Trace.VstDetail.TreeOptions.SelectionOptions ;
   VstDetail.TreeOptions.MiscOptions      := Frm_Trace.VstDetail.TreeOptions.MiscOptions ;
   VstDetail.Colors.UnfocusedSelectionColor       := Frm_Trace.vstTrace.Colors.UnfocusedSelectionColor ;
   VstDetail.Colors.UnfocusedSelectionBorderColor := Frm_Trace.vstTrace.Colors.UnfocusedSelectionBorderColor ;

   LastModified := now ;
   LastRead := -1 ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   inherited;
   if filter <> nil then
      Filter.Free ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.SetEventLog (LogName : string ; LineToRead : integer);
begin
   fLogName := LogName ;
   fEventLog := TEventLog.Create (self) ;  // free on form close
   fEventLog.Log := fLogName ;
   fEventLog.Open ;
   fEventLog.OnChangeEventLog := OnEventLogMessage ;
   LoadLast (LineToRead) ;
end ;

//------------------------------------------------------------------------------

procedure TFrmEventLog.LoadLast (LineToRead : integer);
var
   c, begLoop , endLoop , OldestRecID : integer ;
begin

   application.ProcessMessages ;
   SetCursor(Screen.Cursors[crHourGlass]);
   VstEvent.BeginUpdate ;
   try
      OldestRecID := fEventLog.OldestRecID ;
      endLoop := fEventLog.NewerRecID ;

      LastRead := endLoop ;

      begLoop := endLoop - LineToRead +1 ;
      if begLoop < OldestRecID then
         begLoop := OldestRecID ;
      for c := begLoop to endLoop do begin
         //Frm_Trace.InternalTrace ('ReadEvent ' + inttostr (c));
         fEventLog.ReadEvent (c) ;
         AddLogToTree (fEventLog) ;
      end ;
      // autosort if at least one column in sort
      if Sorter.SortColumns.Count <> 0 then
         Sorter.sort (nil) ;

   finally
      VstEvent.EndUpdate ;
      SetCursor(Screen.Cursors[crDefault]);
   end ;
end ;

//------------------------------------------------------------------------------

procedure TFrmEventLog.OnEventLogMessage (Sender: TEventLog) ;
var
   c , endLoop: integer ;
begin
   if self.IsPaused then
      exit ;

   endLoop := sender.NewerRecID ;

   for c := LastRead+1 to endLoop do begin
      // TFrm_Trace.InternalTrace ('ReadEvent ' + inttostr (c));
      sender.ReadEvent (c) ;
      AddLogToTree (sender) ;
   end ;
   LastRead := endLoop ;
   LastModified := now ;
   // autosort if at least one column in sort
   if Sorter.SortColumns.Count <> 0 then
      Sorter.sort (nil) ;
end ;

//------------------------------------------------------------------------------

procedure TFrmEventLog.butReloadClick(Sender: TObject);
begin
   VstEvent.Clear ;
   LoadLast(50);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.butGetAllClick(Sender: TObject);
begin
   VstEvent.Clear ;
   LoadLast(fEventLog.EventCount);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.AddLogToTree (Sender: TEventLog) ;
var
   node : PVirtualNode ;
   TreeRec : PEvntLogRec ;
   EventType : string ;

   procedure AddDump ;
   var
      c,e, beginLine : integer ;
      hexa_representation : string ;
      Str_representation : string ;
      datalen : DWORD ;
      AnsiPtr : PAnsiChar ;
      OneAnsiChar : AnsiChar ;
      Dump : TMember ;
   begin
      AnsiPtr := Sender.GetEventData (datalen) ; // out datalen  . REturn PAnsiChar
      if datalen <> 0 then begin
         Dump := TMember.Create ('Data') ;
         TreeRec.Members.SubMembers.Add(Dump) ;
         c := 0 ;
         while c <= integer(datalen)-1 do begin
            e := 0 ;

            beginLine := c ;
            hexa_representation := '' ;
            Str_representation := '' ;
            while (c <= integer(datalen)-1) and (e < 16) do begin
               OneAnsiChar := AnsiPtr^ ;
               hexa_representation := hexa_representation + intTohex (integer(OneAnsiChar),2) + ' ' ;

               if ord(OneAnsiChar) = 255 then
                  OneAnsiChar := chr(0) ;

               if CharIsAlphaNum (OneAnsiChar) then
                  Str_representation := Str_representation + string(OneAnsiChar)
               else
                  Str_representation := Str_representation + '.' ;

               inc (e) ;
               inc (c) ;
               Inc (AnsiPtr) ;
            end ;
            if hexa_representation <> '' then
               dump.SubMembers.Add(TMember.Create (inttohex (beginLine,6) , hexa_representation , Str_representation)) ;
         end ;
      end ;
   end ;

   procedure AddMessage ;
   var
      member : TMember ;
      Messages : TStringList ;
      IsEifLike : boolean ;
      c : integer ;
      p : integer ;
      TypePropValue, TypeName,PropName , PropValue : string ;
   begin
      Messages := getStrings (pchar(TreeRec.MessageText), length(TreeRec.MessageText));
      if Messages.Count <= 1 then begin  // single line
         TreeRec.Members.SubMembers.Add(TMember.Create ('Text', TreeRec.MessageText)) ;
      end else begin
         // multline. check if it is a "EIF" like structure :
         // className
         // {
         //    Type prop=value
         // }
         IsEifLike := false ;
         if Messages.Count >= 4 then begin
            if (Messages.Strings[1] = '{') and (Messages.Strings[Messages.Count-1] = '}') then begin
               IsEifLike := true ;
               TreeRec.MessageText := Messages.Strings[0] ;
               member :=  TMember.Create (Messages.Strings[0]) ;
               for c := 2 to Messages.Count-2 do begin
                  TypePropValue := Trim( Messages.Strings[c]) ;
                  p := Pos (' ',TypePropValue) ;
                  if p = 0 then begin
                     TreeRec.MessageText := Sender.EventMessageText ;
                     IsEifLike := false ;
                     member.free ;
                     break ;
                  end ;
                  TypeName := copy (TypePropValue, 1 , p-1) ;
                  TypePropValue := copy (TypePropValue, p+1 , 1000) ;
                  p := pos ('=', TypePropValue) ;
                  if p = 0 then begin
                     TreeRec.MessageText := Sender.EventMessageText ;
                     IsEifLike := false ;
                     member.free ;
                     break ;
                  end ;
                  PropName := trim(copy (TypePropValue, 1 , p-1)) ;
                  PropValue := TrimLeft(copy (TypePropValue, p+1 , 1000)) ;
                  member.SubMembers.Add(TMember.Create(TypeName,PropName,PropValue)) ;
                  if PropName='Message' then
                     TreeRec.MessageText := copy (PropValue,2,length(PropValue)-2) ;
               end ;
               if IsEifLike then   
                  TreeRec.Members.SubMembers.Add(member);
            end ;
         end ;
         if IsEifLike = false then begin  // multiline, but not EIF structure
            member :=  TMember.Create ('Text') ;
            for c := 0 to Messages.Count-1 do
               member.SubMembers.Add(TMember.Create(Messages.Strings[c])) ;

            //member.SubMembers.Add(TMember.Create(TreeRec.MessageText)) ;
            TreeRec.Members.SubMembers.Add(member);
         end ;
      end ;
      Messages.Free ;
   end ;

begin

   node := VstEvent.AddChild (nil) ;
   // ensure node is initialized. Needed when the node is free to call onFreeNode
   VstEvent.ReinitNode(node,false);
   TreeRec := VstEvent.GetNodeData(node) ;

   if TraceConfig.AppDisplay_FocusToReceivedMessage then
      NodeToFocus := node ;

   case Sender.EventType of
      EVENTLOG_SUCCESS           : begin EventType := 'SUCCESS' ;       TreeRec.EventIcon := -1 ; end ;
      EVENTLOG_AUDIT_SUCCESS     : begin EventType := 'AUDIT_SUCCESS' ; TreeRec.EventIcon := -1 ; end ;
      EVENTLOG_AUDIT_FAILURE     : begin EventType := 'AUDIT_FAILURE' ; TreeRec.EventIcon := -1 ; end ;
      EVENTLOG_ERROR_TYPE        : begin EventType := 'ERROR' ;         TreeRec.EventIcon := CST_ICO_ERROR ; end ;
      EVENTLOG_WARNING_TYPE      : begin EventType := 'WARNING' ;       TreeRec.EventIcon := CST_ICO_WARNING ; end ;
      EVENTLOG_INFORMATION_TYPE  : begin EventType := 'INFORMATION';    TreeRec.EventIcon := CST_ICO_INFO ; end ;
   end ;

   TreeRec.Members := TMember.Create () ;
   TreeRec.Time           := DateTimeToStr(Sender.EventTime) ;
   TreeRec.Source         := Sender.EventSource ;
   TreeRec.MessageText    := Sender.EventMessageText ;
   TreeRec.EventRecordNum := Sender.EventRecordNumber ;

   TreeRec.Members.SubMembers.Add(TMember.Create ('Time'     , DateTimeToStr(Sender.EventTime))) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Source'   , Sender.EventSource )) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('EventType', eventtype)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Computer' , Sender.EventComputer)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('id'       , inttostr(Sender.EventId and $ffff))) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('User'     , Sender.EventUser)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Category' , inttostr(Sender.EventCategory))) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('MessageHandler', Sender.EventMessageHandler));
   TreeRec.Members.SubMembers.Add(TMember.Create ('EventRecordNum', inttostr(Sender.EventRecordNumber)));
   Adddump ;
   AddMessage ;
   // check if the node can be displayed according the filters
   if Filter <> nil then
      Filter.CheckNode(node) ;
end ;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventChange(Sender: TBaseVirtualTree;   Node: PVirtualNode);
var
   EvntLogRec : PEvntLogRec ;
   Haschildren : boolean ;
   FirstSelect : PVirtualNode ;
   SecondSelect : PVirtualNode ;

   procedure AddToVst (Member : TMember ; MasterNode : PVirtualNode) ;
   var
      c : integer ;
      SubMember : TMember ;
      ChildNode : PVirtualNode ;
   begin
      if  (MasterNode <> nil) and (Member.SubMembers.Count > 0) then
        Haschildren := true ;
      for c := 0 to Member.SubMembers.Count -1 do begin
         SubMember := TMember (Member.SubMembers.Items[c]) ;
         ChildNode := VstDetail.AddChild(MasterNode , SubMember) ;
         // ensure node is initialized. Needed when the node is free to call onFreeNode
         VstDetail.ReinitNode(ChildNode,false);
         VstDetail.MultiLine[ChildNode] := true ;

         AddToVst (SubMember , ChildNode);  // recursive add submembers
      end ;
   end ;
begin
   // scroll into view
   if Node <> nil then
      Sender.ScrollIntoView (Node,false,false);     // center and horizontally false

   // get first then second. If second is not nil then it's multiselect : disable info panel
   FirstSelect := VstEvent.GetNextSelected (nil) ;
   if FirstSelect = nil then
      exit ;

   SecondSelect := VstEvent.GetNextSelected (FirstSelect) ;
   if SecondSelect <> nil then
      exit ;

   VstDetail.clear ;
   EvntLogRec := TVirtualStringTree (Sender).GetNodeData(FirstSelect) ; // node

   // TraceInfo panel

   Haschildren := false ;
   if (EvntLogRec.Members <> nil) and (EvntLogRec.Members.SubMembers.Count <> 0) then
      AddToVst (EvntLogRec.Members , nil);

   if Haschildren then   // if a node has chidren, show the root
      VstDetail.TreeOptions.PaintOptions := VstDetail.TreeOptions.PaintOptions + [toShowRoot]
   else  // no children : remove the root
      VstDetail.TreeOptions.PaintOptions := VstDetail.TreeOptions.PaintOptions - [toShowRoot] ;

   VstDetail.FullExpand();
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventGetText(Sender: TBaseVirtualTree;
     Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
   EvntLogRec : PEvntLogRec ;
begin
   try
      CellText := '' ;
      EvntLogRec := Sender.GetNodeData(Node) ;
      if EvntLogRec = nil then
         exit ;

      case Column of
         0 : begin
                if TextType = ttStatic then begin   // ttStatic is used to get the real text
                   if EvntLogRec.EventIcon = -1 then
                      CellText := '24 : Debug/Info'
                   else if EvntLogRec.EventIcon = 24 then
                      CellText := '24 : Debug/Info'
                   else if EvntLogRec.EventIcon = 22 then
                      CellText := '22 : Warning'
                   else if EvntLogRec.EventIcon = 23 then
                      CellText := '23 : Error'
                   else
                      CellText := inttostr (EvntLogRec.EventIcon) ;
                end ;
             end ;
         1 : begin
                //LongTimeFormat := 'hh:mm:ss:zzz' ;
                CellText := EvntLogRec.Time ;
             end ;
         2 : begin
                if (TextType = ttNormal) and (IsSeparator (EvntLogRec.Source)) then
                   CellText := ' '  // check underline / TextType
                else
                   CellText := EvntLogRec.Source ;
             end ;
         3 : begin
                if (TextType = ttNormal) and (IsSeparator (EvntLogRec.MessageText)) then
                   CellText := ' '  // check underline / TextType
                else
                   CellText := EvntLogRec.MessageText ;
             end ;
      end ;
   except
      on e : exception do
         TFrm_Trace.InternalTrace(e.Message) ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
   EvntLogRec1,EvntLogRec2    : PEvntLogRec ;
   cellText1, cellText2 : String ;
begin
   if Column = -1 then begin
      // no column : unsort or the 2 records are the same
      EvntLogRec1 := Sender.GetNodeData(Node1) ;
      EvntLogRec2 := Sender.GetNodeData(Node2) ;
      if EvntLogRec1.EventRecordNum <= EvntLogRec2.EventRecordNum then
         result := -1
      else
         result := 1 ;
      exit ;
   end ;
   TVirtualStringTree(Sender).OnGetText (Sender,Node1,Column,ttNormal,CellText1) ;
   TVirtualStringTree(Sender).OnGetText (Sender,Node2,Column,ttNormal,CellText2) ;
   Result := CompareText (CellText1,CellText2) ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
   EvntLogRec : PEvntLogRec ;
begin
   ImageIndex := -1 ;

   if Kind = ikOverlay then
       exit ; // Return a defined overlay here

   if Column <> 0 then
      exit ;

   EvntLogRec := Sender.GetNodeData(Node) ;
   ImageIndex := EvntLogRec.EventIcon ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   EvntLogRec : PEvntLogRec ;
   idx : integer ;
begin
   // delete from bookmark list
   if bookmarks <> nil then begin
      idx := bookmarks.IndexOf(node) ;
      if idx <> -1 then
         bookmarks.Delete(idx);
   end ;

   EvntLogRec := Sender.GetNodeData(Node) ;
   if EvntLogRec.Members <> nil then
      EvntLogRec.Members.free ;  // auto free also sub members

   EvntLogRec.Time        := '' ;  
   EvntLogRec.Source      := '' ;
   EvntLogRec.MessageText := '' ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
   SelectedCompoNode : PVirtualNode ;
   Member : TMember  ;
begin

   CellText := '' ;
   try

   SelectedCompoNode := VstEvent.GetFirstSelected ;
   if SelectedCompoNode = nil then
      exit ;

   //ptr :=  ;

   Member := TMember (TObject (Sender.GetNodeData(Node)^)) ;
   if Member = nil then
      exit ;
   case Column of
      0 : CellText := Member.Col1 ;
      1 : CellText := Member.Col2 ;
      2 : CellText := Member.Col3 ;
      else  CellText := '' ;
   end ;
   except
      on e : exception do
         TFrm_Trace.InternalTrace(e.Message) ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   Member : TMember  ;
begin
   Member := TMember (TObject (Sender.GetNodeData(Node)^)) ;
   if Member = nil then
      exit ;
   if (SearchText <> '') {and (SearchKind = mrYesToAll)} then begin  //  mrYesToAll means Highlight all

      case Column of
         0 : if (MatchSearch (Member.col1) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
         1 : if (MatchSearch (Member.col2) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
         2 : if (MatchSearch (Member.col3) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
      end ;
      //if (MatchSearch (Member.col1) <> 0) or
      //   (MatchSearch (Member.col2) <> 0) or
      //   (MatchSearch (Member.col3) <> 0) then begin
      //   DrawHighlight (TargetCanvas, CellRect,false) ;
      //end ;
   end;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
//
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.butCloseClick(Sender: TObject);
begin
   CloseWin () ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TFrmBase }

procedure TFrmEventLog.CloseWin;
begin
   fEventLog.Close ;  // stop monitoring
   EventForm.Delete (EventForm.IndexOf(fLogName)) ;
   self.close ;                                 // close the form
end;

//------------------------------------------------------------------------------

//procedure TFrmEventLog.PageControlChanging();
//begin
//end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.PageControlChange();
var
   PageContainer : TFrmPageContainer ;
begin
   PageContainer := getPageContainer() ;
   if PageContainer = nil then begin
      TFrm_Trace.InternalTrace ('TFrmEventLog.PageControlChange:PageContainer = nil') ;
      exit ;
   end ;
   PageContainer.actPrint        .Enabled := true ;
   PageContainer.actClear        .Enabled := true ;
   PageContainer.actSaveToFile   .Enabled := true ;
   PageContainer.actViewTraceInfo.Enabled := true ;
   PageContainer.actPause        .Enabled := true  ;
   PageContainer.actCopy         .Enabled := true  ;
   PageContainer.actSelectAll    .Enabled := true  ;
   PageContainer.actDelete       .Enabled := true  ;
   PageContainer.actCut          .Enabled := true  ;
   PageContainer.actSearch       .Enabled := true ;
   PageContainer.actFindNext     .Enabled := true ;

   PageContainer.actViewTraceInfo.checked := PanelTraceInfo.Visible ;
   PageContainer.actPause        .checked := self.IsPaused ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ResizeColumns;
begin
   AutosizeAll (VstEvent) ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.CheckAutoClear;
begin
  // nothing to do , never called, no limit
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.SaveWin;
var
   XMLRootData : IXMLData ;
   MasterTVNode : PVirtualNode ;

   // recursive
   procedure recurMembers (ParentMember : TMember ; ParentMemberTag : IXMLMemberType) ;
   var
      c : integer ;
      SubMember : TMember ;
      SubMemberTag : IXMLMemberType ;
   begin
      if ParentMember <> nil then begin
         for c := 0 to ParentMember.SubMembers.Count -1 do begin
            // note : the generateNodeXML method differ here : we add member from a MemberType
            SubMember := TMember(ParentMember.SubMembers.items[c]) ;
            SubMemberTag := ParentMemberTag.Member.Add ;
            SubMemberTag.Text  := SubMember.Col1 ;
            SubMemberTag.ColB  := SubMember.Col2 ;
            SubMemberTag.ColC  := SubMember.Col3 ;
            // save all sub members
            recurMembers (SubMember, SubMemberTag) ;
          end ;
      end ;
   end ;

   // recursive
   procedure generateNodeXML (NodeTag : IXMLNodeType; VtNode : PVirtualNode) ;
   var
      ChildXmlNode :  IXMLNodeType;  //   IXMLNode
      Member : TMember ;
      MemberTag : IXMLMemberType ;
      ChildVtNode : PVirtualNode ;
      c : integer ;
      EvntLogRec : PEvntLogRec ;
   begin
      if VtNode = nil then
         exit ;

      if NodeTag = nil then
         exit ;

      if Supports(NodeTag, IXMLnodeType) = false then
         exit ;

      EvntLogRec := VstEvent.GetNodeData(VtNode) ;
      if EvntLogRec <> nil then begin   // treeRec can be nil the first time when VtNode is vst.RootNode
         // save the tree col1
         NodeTag.Text := EvntLogRec.Source ;

         // save the tree col 2
         if EvntLogRec.MessageText <> '' then
            NodeTag.Col2 :=  EvntLogRec.MessageText ; // EncodeString () ;

         NodeTag.Time := EvntLogRec.Time ;
         NodeTag.Icon := inttostr (EvntLogRec.EventIcon) ;

         // save all members of the node
         if EvntLogRec.Members <> nil then begin
            for c := 0 to EvntLogRec.Members.SubMembers.Count -1 do begin
               Member := TMember(EvntLogRec.Members.SubMembers.items[c]) ;
               // note : the recurMembers method differ here : we add member from a NodeType
               MemberTag := (NodeTag as IXMLNodeType).Member.Add ;
               MemberTag.Text  := Member.Col1 ;
               MemberTag.ColB  := Member.Col2 ;
               MemberTag.ColC  := Member.Col3 ;
               // save all sub members
               recurMembers (Member, MemberTag) ;
             end ;
         end ;
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

   Frm_Tool.SaveDialog1.InitialDir := TraceConfig.General_LastSavedPath ;
   Frm_Tool.SaveDialog1.Filter := 'Xml file (*.xml)|*.xml' ;
   if Frm_Tool.SaveDialog1.Execute = false then
      exit ;
   TraceConfig.General_LastSavedPath := ExtractFilePath(Frm_Tool.SaveDialog1.FileName) ;

   application.ProcessMessages ;
   SetCursor(Screen.Cursors[crHourGlass]);

   try
      // create the header
      XMLRootData := NewData ;

      // generate nodes
      MasterTVNode := VstEvent.RootNode ;
      generateNodeXML (XMLRootData ,MasterTVNode) ;  // recursive

      XMLRootData.OwnerDocument.SaveToFile(Frm_Tool.SaveDialog1.FileName);
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end ;
end;

//------------------------------------------------------------------------------

// copy time, source, first line of MessageText

procedure TFrmEventLog.CopySelected;
var
   CopyStrings: TStrings;
   CopyText: PChar;
   NewLine: string;
   IsFirst : boolean ;
   EvntLogRec : PEvntLogRec ;
   focusedComponent : hwnd ;

   procedure CheckIfNodeSelected (TestNode : PVirtualNode) ;
   var
      ChildVtNode : PVirtualNode ;
   begin
      if VstEvent.Selected [TestNode] then begin
         EvntLogRec := VstEvent.GetNodeData(TestNode) ;

         IsFirst := true ;
         NewLine := '' ;

         if TraceConfig.TextExport_Time then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator ;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + EvntLogRec.Time + TraceConfig.TextExport_TextQualifier ;
            IsFirst := false ;
         end ;

         if TraceConfig.TextExport_Col1 then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator ;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + EvntLogRec.Source + TraceConfig.TextExport_TextQualifier ;
            IsFirst := false ;
         end ;

         if TraceConfig.TextExport_Col2 then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator ;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + EvntLogRec.MessageText + TraceConfig.TextExport_TextQualifier ;
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
      Member : TMember  ;
   begin
      if VstDetail.Selected [TestNode] then begin

         Member := TMember (TObject (VstDetail.GetNodeData(TestNode)^)) ; 
         NewLine := TraceConfig.TextExport_TextQualifier + Member.Col1 + TraceConfig.TextExport_TextQualifier  +
                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + Member.Col2 + TraceConfig.TextExport_TextQualifier +
                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + Member.Col3 + TraceConfig.TextExport_TextQualifier  ;

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
   if (VstDetail.Focused = false) and (VstEvent.Focused = false) then begin
      focusedComponent := GetFocus ;
      if focusedComponent <> 0 then
         SendMessage(focusedComponent, WM_COPY, 0, 0);
      exit ;
   end ;

   if VstEvent.GetFirstSelected = nil then
      exit ;

   CopyStrings := TStringList.Create;
   SetCursor(Screen.Cursors[crHourGlass]);
   try

      if VstDetail.Focused then begin
         CopyDetail (VstDetail.RootNode);
      end else begin
         // add title if needed.
         if TraceConfig.TextExport_GenerateColumnHeader then begin
            IsFirst := true ;
            NewLine := '' ;

            if TraceConfig.TextExport_Time then begin
               if IsFirst = false then
                  NewLine := NewLine + TraceConfig.TextExport_Separator ;
               NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Time' + TraceConfig.TextExport_TextQualifier ;
               IsFirst := false ;
            end ;

            if TraceConfig.TextExport_Col1 then begin
               if IsFirst = false then
                  NewLine := NewLine + TraceConfig.TextExport_Separator ;
               NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Source' + TraceConfig.TextExport_TextQualifier ;
               IsFirst := false ;
            end ;

            if TraceConfig.TextExport_Col2 then begin
               if IsFirst = false then
                  NewLine := NewLine + TraceConfig.TextExport_Separator ;
               NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Message' + TraceConfig.TextExport_TextQualifier ;
            end ;

            CopyStrings.Add(NewLine);
         end ;

         // add node, starting from the invisible root node (recursive)
         CheckIfNodeSelected (VstEvent.RootNode) ;
      end ;

      CopyText := CopyStrings.GetText;
      try
         Clipboard.SetTextBuf(CopyText);
      finally
         StrDispose(CopyText);
      end;
   finally
      CopyStrings.Free ;
      SetCursor(Screen.Cursors[crDefault]);
   end;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.CopyCurrentCell;
var
   Node : PVirtualNode ;
   CellText : string ;
begin
   if VstEvent.Focused then begin
      Node := VstEvent.FocusedNode ;
      if node = nil then
         exit ;
      VstEventGetText(VstEvent, Node, VstEvent.FocusedColumn,ttStatic,CellText);   // ttStatic is used to get the real text
   end else if VstDetail.Focused then begin
      Node := VstDetail.FocusedNode ;
      if node = nil then
         exit ;
      VstDetailGetText(VstDetail, Node, VstDetail.FocusedColumn,ttNormal,CellText);   // ttNormal
   end else begin
      exit ;
   end ;
   Clipboard.SetTextBuf(pWideChar(CellText));
end;
                                  
//------------------------------------------------------------------------------

procedure TFrmEventLog.DeleteSelected;
var
   node : PVirtualNode ;
begin
   if VstEvent.Focused = false then
     exit ;
   VstDetail.clear ;
   node := VstEvent.GetFirstSelected ;
   if node = nil then   // no node selected
      exit ;
   node := VstEvent.GetPreviousVisible(node) ;
   VstEvent.DeleteSelectedNodes ;

   // case of the first node : GetPreviousVisible is nil ...
   if node = nil then
      node := VstEvent.GetFirst
   else if VstEvent.GetNextVisible(node) <> nil then
      node := VstEvent.GetNextVisible(node) ;

   VstEvent.FocusedNode := node ;
   VstEvent.Selected [node] := true ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ClearWin;
begin
   VstEvent.Clear ;
   VstDetail.clear ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.PauseWin;
begin
   self.IsPaused := getPageContainer().actPause.Checked ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.SelectAll;
begin
   // normally when the IVstEditor is not nil, he is visible
   if ((VstEvent.IsEditing) or VstDetail.IsEditing) and (IVstEditor <> nil) and (TMoveMemoEditLink(IVstEditor).IsVisible) then begin
      VstEditor.SelectAll() ;  // TMoveMemoEditLink(IVstEditor).SelectAll() ;
      exit ;
   end ;
   if VstEvent.Focused = true then
      VstEvent.SelectAll(true)      // select all visible items (don't select filtered items)
   else if VstDetail.Focused then
      VstDetail.SelectAll(false) ;  // select all (visible or invisible)
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.TimerInfo;
begin
   TracesInfo.Caption := TimeToStr(LastModified)
                         + ', not filtered lines : ' + inttostr(VstEvent.RootNode.ChildCount)
                         + '   "' + fLogName+ '"' ;
                        // + '   Total count : ' + inttostr (fEventLog.EventLogThread.userCount) ;
   TracesInfo.Hint := TracesInfo.Caption ;
   if NodeToFocus <> nil then begin
      VstEvent.ClearSelection();
      VstEvent.Selected [NodeToFocus] := true ;
      VstEvent.FocusedNode := NodeToFocus;
      VstEvent.ScrollIntoView (NodeToFocus,false,false);
   end;
   NodeToFocus := nil ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ViewTraceInfo;
begin
  if getPageContainer().actViewTraceInfo.Checked then begin
      PanelTraceInfo.Visible := true ;
      VSplitter.Visible := true ;
      PanelTraceInfo.Left := VSplitter.Left + 10 ;
   end else begin
      PanelTraceInfo.Visible := false ;
      VSplitter.Visible := false ;
   end ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
   VstEventChange (VstEvent,nil);
   VstEvent.Header.MainColumn := VstEvent.Header.Columns.GetFirstVisibleColumn ;
   AutosizeAll (VstEvent) ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   // scroll into view
   sender.ScrollIntoView (node,false,false);     // center and horizontally false
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();
      IVstEditor := VstEditor ;
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailDblClick(Sender: TObject);
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

procedure TFrmEventLog.VstDetailEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.WMStartEditingTrace(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstEvent.EditNode(Node, VstEvent.FocusedColumn);
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set
procedure TFrmEventLog.VstEventDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   //InternalTrace ('DetailDblClick ') ;
   SelectedNode := VstEvent.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstEvent.ScreenToClient(P);
   MouseNode := VstEvent.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstEvent.TreeOptions.MiscOptions := VstEvent.TreeOptions.MiscOptions + [toEditable] ;

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
procedure TFrmEventLog.VstEventKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if CharCode = VK_F2 then
      VstEvent.TreeOptions.MiscOptions := VstEvent.TreeOptions.MiscOptions + [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrmEventLog.VstEventEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
   VstEvent.TreeOptions.MiscOptions := VstEvent.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrmEventLog.VstEventEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstEvent.TreeOptions.MiscOptions := VstEvent.TreeOptions.MiscOptions - [toEditable] ;
end;

procedure TFrmEventLog.VstEventEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();
      IVstEditor := VstEditor ;
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

function TFrmEventLog.CheckSearchRecord  (EvntLogRec : PEvntLogRec) : boolean ;
begin
   result := false ;

   if (MatchSearch (EvntLogRec.Source) <> 0) or
      (MatchSearch (EvntLogRec.MessageText) <> 0) then begin
      result := true ;
      exit ;
   end ;

   if EvntLogRec.Members <> nil then
      result := EvntLogRec.Members.Search() ;
end ;

//------------------------------------------------------------------------------

function  TFrmEventLog.SearchNext(start:boolean) : boolean ;
var
   currentNode : PVirtualNode ;
   EvntLogRec : PEvntLogRec ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if start = true then begin
      currentNode := VstEvent.GetFirstVisible() ;
   end else begin
      currentNode := VstEvent.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstEvent.GetFirstVisible()
      else  // when start is false, we are searching in the current document
         currentNode := VstEvent.GetNextVisible(currentNode) ;   // skip the first selected
   end ;

   while currentNode <> nil do begin
      EvntLogRec := VstEvent.GetNodeData(currentNode) ;
      if CheckSearchRecord (EvntLogRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         VstEvent.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstEvent.ClearSelection;
         VstEvent.Selected [currentNode] := true ;
         VstEvent.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstEvent.GetNextVisible(currentNode) ;
   end ;
end;

//------------------------------------------------------------------------------

function TFrmEventLog.SearchPrevious(atEnd: boolean): boolean;
var
   currentNode : PVirtualNode ;
   EvntLogRec : PEvntLogRec ;
   procedure CheckVisible () ;
   begin
      while (currentNode <> nil) and (VstEvent.IsVisible[currentNode] = false) do begin
         currentNode := VstEvent.GetPrevious(currentNode) ;
      end ;
   end ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if atEnd = true then begin
      currentNode := VstEvent.GetLast() ;
   end else begin
      currentNode := VstEvent.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstEvent.GetLastVisible()
      else  // when atEnd is false, we are searching in the current document
         currentNode := VstEvent.GetPrevious(currentNode) ;   // skip the first selected
   end ;

   CheckVisible() ;
   while currentNode <> nil do begin
      EvntLogRec := VstEvent.GetNodeData(currentNode) ;
      if CheckSearchRecord (EvntLogRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         // fully visible ?
         VstEvent.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstEvent.ClearSelection;
         VstEvent.Selected [currentNode] := true ;
         VstEvent.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstEvent.GetPrevious(currentNode) ;
      CheckVisible() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.RefreshView;
begin
   vstEvent.Refresh ;
   VstDetail.Refresh ;
end;

//------------------------------------------------------------------------------

// if the paint area is modified, AfterPaint is called to redisplay the gutter

procedure TFrmEventLog.VstEventAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
   Node : pvirtualNode ;
   BaseOffset : Integer;  // top position of the top node to draw given in absolute tree coordinates
   DispRec, CliRect : TRect ;
   Yposition : integer ;
   NodeHeight : integer ;
   EvntLogRec : PEvntLogRec ;
   HeaderHeight : integer ;
   gutterCanvas : TCanvas ;
   newgutter : timage ;
   BookmarkPos : integer ;
begin
   // detect header height
   if VstEvent.Header.Columns.Count <> 0 then begin
      // get the header height from the first header column
      // since the VT.headerRect property is protected :-(
      HeaderHeight := VstEvent.Header.Columns[0].GetRect.Bottom ;
   end else begin  // should not happens
      HeaderHeight := VstEvent.header.Height + 2 ;  // plus somme bevels
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
   Node := VstEvent.GetNodeAt(0, 0, true, BaseOffset);
   if node <> nil then begin    // nothing to display
      // get the first visible node rectangle.
      DispRec := VstEvent.GetDisplayRect (Node,NoColumn,false,false) ;

      // We just need the TOP node position
      // This top position is zero or negative since the node can be partially visible
      // but can never be more than zero (else we can have previous partial visible node before)
      Yposition := DispRec.Top ;

      // add Header height
      inc (Yposition , HeaderHeight) ;

      // draw each node
      while node <> nil do begin
         NodeHeight := VstEvent.NodeHeight[Node] ;
         EvntLogRec := VstEvent.GetNodeData(Node) ;

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
                  if CheckSearchRecord (EvntLogRec ) then     // check if the node or one of his child match the search text
                     Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 21);
         end ;

         // draw the small dot indicate sub members
         //if (treeRec.Members <> nil) and (treeRec.Members.SubMembers.Count <> 0) then
         //   Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 15);

         inc (Yposition , NodeHeight) ;
         Node := VstEvent.GetNextVisible(Node) ;
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

procedure TFrmEventLog.VstEventBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   EvntLogRec : PEvntLogRec ;
begin
   if bookmarks.IndexOf(node) <> -1 then begin
      DrawHighlight (TargetCanvas, CellRect,true) ;
      exit ;
   end ;

   // check if highlight must be draw
   if (SearchText = '') or (SearchKind <> mrYesToAll) then
      exit ;

   EvntLogRec := VstEvent.GetNodeData(Node) ;
   if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
      if CheckSearchRecord (EvntLogRec) then     // check if the node or one of his child match the search text
         DrawHighlight (TargetCanvas, CellRect,false) ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
   CellText: String ;
   //EvntLogRec : PEvntLogRec ;
   middle : integer ;
begin
   //EvntLogRec := VstEvent.GetNodeData(Node) ;
   VstEventGetText(Sender, Node,Column,ttStatic,CellText);   // ttStatic is used to get the real text
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

procedure TFrmEventLog.ShowFilter;
begin
   if Filter = nil then
      Filter := TFrmFilter.create (self) ;

   Filter.Vst := VstEvent ;
   Filter.base := self ;
   Filter.ColumnNameList.Clear ;
   Filter.ColumnNameList.AddObject('Trace Kind', tObject(999)) ;  // same as col 0, but force fill with predefined debug,warning,error
   Filter.ColumnNameList.AddObject('Time'      , tObject(1)) ;
   Filter.ColumnNameList.AddObject('Source'    , tObject(2)) ;
   Filter.ColumnNameList.AddObject('Lines'     , tObject(3)) ;
   Filter.ColumnNameList.AddObject('Trace Info', tObject(998)) ;
   filter.FillColumns() ;
   Filter.ShowModal() ;

end;

//------------------------------------------------------------------------------

function TFrmEventLog.getMembers(Node: PVirtualNode): TMember;
var
   EvntLogRec : PEvntLogRec ;
begin
   EvntLogRec := VstEvent.GetNodeData(Node) ;
   result := EvntLogRec.Members ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.PanelGutterDblClick(Sender: TObject);
var
   Node : PVirtualNode ;
   P: TPoint;
   index : integer ;
begin

   GetCursorPos(P);
   P := VstEvent.ScreenToClient(P);
   Node := VstEvent.GetNodeAt(0, P.Y) ;
   if Node = nil then
      exit ;

   index := bookmarks.IndexOf(Node) ;
   if index = -1 then begin
      bookmarks.Add(Node) ;
   end else begin
      bookmarks.Delete(index) ;
   end ;

   VstEvent.InvalidateNode(Node) ;
end;

//------------------------------------------------------------------------------

// main tree : fixed node height
procedure TFrmEventLog.VstEventMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
   NodeHeight := TraceConfig.EventLog_Trace_NodeHeight ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstEventPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.EventLog_Trace_FontName ;
   TargetCanvas.Font.size := TraceConfig.EventLog_Trace_FontSize ;
end;

//------------------------------------------------------------------------------

// member tree : node height depend of the number of lines (variable node height)
procedure TFrmEventLog.VstDetailMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
   h2,h3 : integer ;
   //Member : TMember  ;
begin


   // force font
   TargetCanvas.Font.Name := TraceConfig.EventLog_Info_FontName ;
   TargetCanvas.Font.size := TraceConfig.EventLog_Info_FontSize ;

   NodeHeight := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,0) ;
   h2 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,1) ;
   h3 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,2) ;

   if h2 > NodeHeight then
      NodeHeight := h2 ;
   if h3 > NodeHeight then
      NodeHeight := h3 ;

   // should not happens : node contains at least a title in the first col
   if NodeHeight = 0 then
      NodeHeight := VstDetail.defaultNodeHeight ;

   //Member := TMember (TObject (Sender.GetNodeData(Node)^)) ;
   //TFrm_Trace.InternalTrace(Member.Col1 + ',' + Member.Col2 + ',' + Member.Col3 , inttostr(NodeHeight))
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
   TargetCanvas.Font.Name := TraceConfig.EventLog_Info_FontName ;
   TargetCanvas.Font.size := TraceConfig.EventLog_Info_FontSize ;
   if Column = 0 then
      if node.Parent = VstDetail.RootNode then
         TargetCanvas.font.Style := [fsBold] ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ApplyFont;
begin
   VstEvent.BeginUpdate ;
   VstEvent.Font.Name         := TraceConfig.EventLog_Trace_FontName ;
   VstEvent.Font.Size         := TraceConfig.EventLog_Trace_FontSize ;
   VstEvent.DefaultNodeHeight := TraceConfig.EventLog_Trace_NodeHeight ;
   VstEvent.ReinitChildren (nil,true);
   VstEvent.EndUpdate ;

   VstDetail.BeginUpdate ;
   VstDetail.Font.Name         := TraceConfig.EventLog_Info_FontName ;
   VstDetail.Font.Size         := TraceConfig.EventLog_Info_FontSize ;
   VstDetail.ReinitChildren (nil,true);
   VstDetail.EndUpdate ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.Print;
begin
   FrmPrintPreview.initialize(vstEvent, nil) ;
   FrmPrintPreview.ShowModal ;
end;

end.
