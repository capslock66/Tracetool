{
  Author : Thierry Parent
  HomePage :  https://github.com/capslock66/Tracetool
}

unit unt_eventLog;

interface

uses
  Windows, Messages, SysUtils, StrUtils, System.Variants, Generics.Collections, Classes, Graphics, Controls, Forms, Clipbrd, xmldoc , Menus,
  Dialogs, unt_base, Vcl.StdCtrls, Buttons,  ExtCtrls, ComCtrls, Vcl.ToolWin,
  SynEdit,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, VirtualTrees.Types,
  Unt_Tool,
  unt_tracewin, unt_PageContainer, unt_editor,
  vstSort,
  VstSelector,
  unt_filter,
  untPrintPreview, unt_FrameMemo ;

type

  tInsertionArray = array of string;
  tDataArray = array of integer;

  // Original code : https://delphi-bar.blogspot.com/2022/02/writing-to-and-reading-from-windows.html
  TWindowsEvent = class
  private
    fCategory:         string;
    fCategoryString:   string;
    fComputerName:     string;
    fTypeString:       string;
    fEventType:        integer;
    fEventCode:        integer;
    fEventIdentifier:  integer;
    fRecordNumber:     integer;
    fMessage:          string;
    fLogFile:          string;
    fUser:             string;
    fSourceName:       string;
    fTimeWritten:      TDateTime;   // TimeGenerated is same as TimeWritten
    fTimeGenerated:    TDateTime;
    fInsertionStrings: tInsertionArray;
    fDataArray :       tDataArray;
  public
    property Category:         string           read fCategory         write fCategory;
    property CategoryString:   string           read fCategoryString   write fCategoryString;
    property ComputerName:     string           read fComputerName     write fComputerName;
    property TypeString:       string           read fTypeString       write fTypeString;
    property EventType:        integer          read fEventType        write fEventType;
    property EventCode:        integer          read fEventCode        write fEventCode;
    property EventIdentifier:  integer          read fEventIdentifier  write fEventIdentifier;
    property RecordNumber:     integer          read fRecordNumber     write fRecordNumber;
    property Msg:              string           read fMessage          write fMessage;
    property LogFile:          string           read fLogFile          write fLogFile;
    property User:             string           read fUser             write fUser;
    property SourceName:       string           read fSourceName       write fSourceName;
    property TimeWritten:      TdateTime        read fTimeWritten      write fTimeWritten;
    property TimeGenerated:    TdateTime        read fTimeGenerated    write fTimeGenerated;
    property InsertionStrings: tInsertionArray  read fInsertionStrings write fInsertionStrings;
    property DataArray:        tDataArray       read fDataArray        write fDataArray;

    procedure PopulateFromOleVariant(aEvent: OLEVariant);
  end;

  PEvntLogRec = ^TEvntLogRec ;
  TEvntLogRec = record
     EventRecordNum : integer ; // Original order when inserted. Used to Unsort nodes
     Time           : string ;   // time
     Source         : string ;   // EventLog.EventSource
     MessageText    : string ;   // EventLog.EventMessageText
     Members        : TMember ;
     EventIcon      : integer ;
  end ;

  TFrmEventLog = class(TFrmBase)
    GroupPanel: TPanel;
    VSplitter: TSplitter;
    VstMain: TVirtualStringTree;
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
    SplitterH: TSplitter;
    FrameMemo: TFrameMemo;
    procedure FormCreate(Sender: TObject);
    procedure VstMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VstMainFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstDetailGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VstMainGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure butGetAllClick(Sender: TObject);
    procedure butReloadClick(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
    procedure VstMainHeaderDragged(Sender: TVTHeader;
      Column: TColumnIndex; OldPosition: Integer);
    procedure VstDetailCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDetailDblClick(Sender: TObject);
    procedure VstDetailMeasureItem(Sender: TBaseVirtualTree;TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
    procedure VstDetailPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VstMainCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstMainDblClick(Sender: TObject);
    procedure VstMainEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstMainEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstMainKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstMainAfterPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure VstMainCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VstMainBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstMainAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VstDetailFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure PanelGutterDblClick(Sender: TObject);
    procedure VstMainMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
    procedure VstMainPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstDetailEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VstMainEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VstDetailColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure VstDetailFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure GroupPanelCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure VSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);

  private
    fLogName : string ;
    LastModified : tDateTime ;
    LastRead : integer ;
    FirstChildOrder: integer; // Order of the last child, used to insert sub nodes and unsort them
    LastChildOrder: integer;  // Order of the last child, used to insert sub nodes and unsort them
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
    procedure WMStartEditingTrace(var Message: TMessage); message WM_STARTEDITING_TRACE;
    procedure VstDetailSelectorSelectionChanged(Sender: TVstSelector; selectionAsText: string);

    procedure AddLogToTree(eventLog: TWindowsEvent);
    function GetWindowsEventLogs(aApplicationName: string; MaxNumberOfEntries: integer): TObjectList<TWindowsEvent>;
    function CheckSearchRecord(EvntLogRec: PEvntLogRec): boolean;
    procedure AddWaitingMessage;
  public
    Gutter: TImage;
    Sorter : TVstSort ;
    VstDetailSelector: TVstSelector;
    IsPaused : boolean ;
    NodeToFocus : PVirtualNode ;
    rightPercent : extended;
    procedure SetEventLog(LogName: string; LineToRead: integer);

  public // TFrmBase
    procedure Print ; override ;
    procedure ClearWin ; override ;
    procedure SaveWin ; override ;
    procedure PauseWin ; override ;
    procedure ViewTraceInfo ; override ;
    function  CopySelected: boolean; override ;
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
    procedure InsertRow ; override;
    function  getMembers(Node : PVirtualNode) : TMember ; override ;
    function  SearchNext(start:boolean) : boolean ; override ;
    function  SearchPrevious (atEnd:boolean) : boolean ;  override ;
    procedure ApplyTheme; override;
  end;


var
  FrmEventLog: TFrmEventLog;

implementation

uses
   unt_selectEvent,
   unt_ODS
   , unt_utility
   , DebugOptions
   , application6
   , unt_TraceConfig
   , unt_search, unt_AddLine
   , ComObj, ActiveX, DateUtils
   , SvcMgr, System.JSON;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrmEventLog.FormCreate(Sender: TObject);
begin
   inherited ;
   FrameMemo.Height := 120 ;

   ApplyFont() ;  // set font name and size for the 2 trees (from XMLConfig)

   if PanelTraceInfo.Width < 50 then
      PanelTraceInfo.Width := 50;
   var accept : boolean;
   var size := PanelTraceInfo.Width;
   VSplitterCanResize(self,size,accept); // calculated once left and right percent

   vst := VstMain ;

   // initialize sort
   FirstChildOrder := -1;
   LastChildOrder := 1 ;   // 0 is reserved for not yet ordered lines

   Sorter := TVstSort.create (self) ;
   Sorter.tree := VstMain ;
   Sorter.UtilityImages := Frm_Tool.UtilityImages ;
   Sorter.canUnsort := true ;

   // redirect some events to the sorter
   VstMain.onHeaderClick             := sorter.OnHeaderClick ;
   VstMain.OnKeyUp                   := sorter.OnKeyUp ;
   VstMain.onHeaderDrawQueryElements := sorter.OnHeaderDrawQueryElements ;
   VstMain.onAdvancedHeaderDraw      := sorter.OnAdvancedHeaderDraw ;
   // tips : don't forget to include the hoOwnerDraw in the VstMain.Header.Options

   // copy all options from main form
   VstMain.Colors.UnfocusedColor                := Frm_Trace.VstMain.Colors.UnfocusedColor ;
   VstMain.Colors.UnfocusedSelectionColor       := Frm_Trace.VstMain.Colors.UnfocusedSelectionColor ;
   VstMain.Colors.UnfocusedSelectionBorderColor := Frm_Trace.VstMain.Colors.UnfocusedSelectionBorderColor ;
   VstMain.NodeDataSize := sizeof (TEvntLogRec) ;
   VstMain.Header.MainColumn := 0 ;
   VstMain.Header.AutoSizeIndex := -1 ;  // auto

   VstMain.Header.Options := VstMain.Header.Options
      - [hoDrag]              // columns cannot be moved
      + [hoOwnerDraw]         // needed for sort : header items with the owner draw style can be drawn by the application via event
      + [hoDblClickResize] ;  // allows a column to resize itself to its largest entry

   VstMain.TreeOptions.AutoOptions      := Frm_Trace.VstMain.TreeOptions.AutoOptions ;
   VstMain.TreeOptions.PaintOptions     := Frm_Trace.VstMain.TreeOptions.PaintOptions ;
   VstMain.TreeOptions.SelectionOptions := Frm_Trace.VstMain.TreeOptions.SelectionOptions ;
   VstMain.TreeOptions.MiscOptions      := Frm_Trace.VstMain.TreeOptions.MiscOptions ;

   {$IFDEF WIN64}
     VstDetail.NodeDataSize := 16 ;
   {$ELSE}
     VstDetail.NodeDataSize := 4 ;
   {$ENDIF}

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
   VstDetail.Colors.UnfocusedColor                := Frm_Trace.VstMain.Colors.UnfocusedColor ;
   VstDetail.Colors.UnfocusedSelectionColor       := Frm_Trace.VstMain.Colors.UnfocusedSelectionColor ;
   VstDetail.Colors.UnfocusedSelectionBorderColor := Frm_Trace.VstMain.Colors.UnfocusedSelectionBorderColor ;

   // multiple selection handler
   VstDetailSelector := TVstSelector.Create(self);   // self is owner
   VstDetailSelector.Init(VstDetail);
   VstDetailSelector.OnSelectionChanged := VstDetailSelectorSelectionChanged;

   LastModified := now ;
   LastRead := -1 ;
   ApplyTheme();
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   inherited;
   if filter <> nil then
      Filter.Free ;
end;

//------------------------------------------------------------------------------

// called by the tool menu open log
procedure TFrmEventLog.SetEventLog (LogName : string ; LineToRead : integer);
begin
   fLogName := LogName ;

   VstMain.Clear;
   AddWaitingMessage() ;
   var logs := GetWindowsEventLogs(LogName, LineToRead);
   VstMain.Clear;  // clear again to remove the 'Loading...'
   for var event in logs do
      AddLogToTree(event);
   freeAndNil(logs);
end ;

procedure TFrmEventLog.AddWaitingMessage();

var
   node : PVirtualNode ;
   TreeRec : PEvntLogRec ;
begin
   node := VstMain.AddChild (nil);
   VstMain.ReinitNode(node,false);       // ensure node is initialized. Needed when the node is free to call onFreeNode
   TreeRec := VstMain.GetNodeData(node);

   if TraceConfig.AppDisplay_FocusToReceivedMessage then
      NodeToFocus := node ;

   TreeRec.EventIcon := -1 ;
   TreeRec.Source := 'Loading...';
end;

procedure TFrmEventLog.AddLogToTree (eventLog: TWindowsEvent) ;
var
   TreeRec : PEvntLogRec ;
const
   Punctuation:string = ' ,.;:!@#$%^&*()_-=+<>?/';
   procedure AddDump ;
   begin
      var datalen := length(eventLog.DataArray);
      if datalen = 0 then
         exit;
       var Dump := TMember.Create ('Data') ;
       TreeRec.Members.SubMembers.Add(Dump) ;
       var loop1 := 0;
       while loop1 <= integer(datalen)-1 do begin
          var loop2 := 0;
          var beginLine := loop1;
          var hexa_representation := '';
          var Str_representation := '';
          while (loop1 <= datalen-1) and (loop2 < 16) do begin
             var oneByte := eventLog.DataArray[loop1];
             hexa_representation := hexa_representation + intTohex (integer(oneByte),2) + ' ' ;

             var OneAnsiChar := AnsiChar(oneByte);
             if (CharIsAlphaNum (OneAnsiChar)) or (Pos(string(OneAnsiChar), Punctuation) > 0) then
                Str_representation := Str_representation + string(OneAnsiChar)
             else
                Str_representation := Str_representation + '.' ;

             inc (loop1) ;
             inc (loop2) ;
          end ;
          if hexa_representation <> '' then
             dump.SubMembers.Add(TMember.Create (inttohex (beginLine,6) , hexa_representation , Str_representation)) ;
       end ;
   end ;

   procedure AddMessage ;
   begin
      var datalen := length(eventLog.InsertionStrings);
      if datalen = 0 then
         exit;
       var member: TMember := TMember.Create ('Insertion strings') ;
       TreeRec.Members.SubMembers.Add(member) ;

       for var loop1:integer := 0 to datalen-1 do begin
          var oneLine := eventLog.InsertionStrings[loop1];
          if eventLog.Msg = '' then
             TreeRec.MessageText := TreeRec.MessageText + oneLine + ' ';

          member.SubMembers.Add(TMember.Create (oneLine));
       end;
   end ;

begin
   var node := VstMain.AddChild (nil);
   VstMain.ReinitNode(node,false);       // ensure node is initialized. Needed when the node is free to call onFreeNode
   TreeRec := VstMain.GetNodeData(node);

   if TraceConfig.AppDisplay_FocusToReceivedMessage then
      NodeToFocus := node ;

   case eventLog.EventType of  // Nothing to do with EVENTLOG_SUCCESS and other pair values
      // 1:Error,2:Warning,0 and 3:Information,4:Security Audit Success,5:Security Audit Failure
      0,3: TreeRec.EventIcon := CST_ICO_INFO;
      2  : TreeRec.EventIcon := CST_ICO_WARNING;
      1,5: TreeRec.EventIcon := CST_ICO_ERROR ;
      else TreeRec.EventIcon := -1 ;
   end ;

   TreeRec.Time           := DateTimeToStr(eventLog.TimeWritten) ;
   TreeRec.Source         := eventLog.SourceName ;
   TreeRec.MessageText    := eventLog.Msg ;
   TreeRec.EventRecordNum := eventLog.RecordNumber ;

   LastChildOrder := TreeRec.EventRecordNum ;

   TreeRec.Members := TMember.Create () ;       // Who clear TreeRec.Members ???
   TreeRec.Members.SubMembers.Add(TMember.Create ('EventRecordNum', inttostr(eventLog.RecordNumber)));
   TreeRec.Members.SubMembers.Add(TMember.Create ('Time written'  , DateTimeToStr(eventLog.TimeWritten))) ;
   if eventLog.TimeWritten <> eventLog.TimeGenerated then
      TreeRec.Members.SubMembers.Add(TMember.Create ('Time Generate' , DateTimeToStr(eventLog.TimeGenerated))) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Category'      , eventLog.Category + ' : ' + eventLog.CategoryString)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Source'        , eventLog.SourceName )) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('EventType'     , eventLog.TypeString)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('EventCode'     , inttostr(eventLog.EventCode))) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Message'       , eventLog.Msg)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Computer'      , eventLog.ComputerName)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('id'            , inttostr(eventLog.EventIdentifier ))) ;  // and $ffff
   TreeRec.Members.SubMembers.Add(TMember.Create ('User'          , eventLog.User)) ;
   TreeRec.Members.SubMembers.Add(TMember.Create ('Log File'      , eventLog.LogFile)) ;

   //TreeRec.Members.SubMembers.Add(TMember.Create ('Other'         , eventLog.EventData)) ;
   //TreeRec.Members.SubMembers.Add(TMember.Create ('MessageHandler', eventLog.EventMessageHandler));

   Adddump() ;
   AddMessage() ;

   // check if the node can be displayed according the filters
   if Filter <> nil then
      Filter.CheckNode(node) ;
end ;

//------------------------------------------------------------------------------

procedure TFrmEventLog.butReloadClick(Sender: TObject);
begin
   SetEventLog (fLogName, 500);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.butGetAllClick(Sender: TObject);
begin
   SetEventLog (fLogName, -1);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainChange(Sender: TBaseVirtualTree;   Node: PVirtualNode);
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
         if subMember.Col1 = 'Text' then
           frameMemo.SetMemoText(subMember.Col2,false,false);

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

   VstDetail.clear ;
   frameMemo.SetMemoText('',false,false);
   VstDetailSelector.ResetSelection;

   // get first then second. If second is not nil then it's multiselect : disable info panel
   FirstSelect := VstMain.GetNextSelected (nil) ;
   if FirstSelect = nil then
      exit ;

   SecondSelect := VstMain.GetNextSelected (FirstSelect) ;
   if SecondSelect <> nil then begin
       VstDetail.Clear;
       VstDetail.AddChild(nil); // the Get Text will draw itself number of row selected
       exit;
   end;
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

   frameMemo.SetMemoText(EvntLogRec.MessageText,false,false);

end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.InsertRow;
var
   selectedNode: PVirtualNode;
   selectedTreeRec: PEvntLogRec;
   newTreeNode: PVirtualNode;
   newTreeRec: PEvntLogRec;
   newOrder : integer ;
begin

   selectedNode := VstMain.GetFirstSelected;
   selectedTreeRec := nil;

   if selectedNode <> nil then begin
      selectedTreeRec := VstMain.GetNodeData(selectedNode);
      Frm_AddLine.EditTime.Text := selectedTreeRec.Time;
      Frm_AddLine.EditThId.Text := selectedTreeRec.Source;
   end;

   Frm_AddLine.SetEventLogMode;
   Frm_AddLine.ShowModal;
   if Frm_AddLine.ModalResult = mrCancel then
      exit;

   if (Frm_AddLine.InsertWhere.ItemIndex = 0) then begin          // on first line
      newTreeNode := VstMain.InsertNode(nil,amAddChildFirst);
      dec (FirstChildOrder) ;
      NewOrder := FirstChildOrder ;

   end else if (Frm_AddLine.InsertWhere.ItemIndex = 1) then begin // Before selected line
      if selectedNode = nil then begin
         newTreeNode := VstMain.InsertNode(nil,amAddChildFirst);
         dec (FirstChildOrder) ;
         NewOrder := FirstChildOrder ;

      end else begin
         newTreeNode := VstMain.InsertNode(selectedNode,amInsertBefore);
         newOrder := selectedTreeRec.EventRecordNum-1 ;
         if newOrder = 0 then  // 0 is reserved
            dec(newOrder);
      end;

   end else if (Frm_AddLine.InsertWhere.ItemIndex = 2) then begin  // After selected line
      if selectedNode = nil then begin
         newTreeNode := VstMain.AddChild(nil);
         NewOrder := LastChildOrder ;
         inc (LastChildOrder) ;

      end else begin
         newTreeNode := VstMain.InsertNode(selectedNode,amInsertAfter);
         newOrder := selectedTreeRec.EventRecordNum+1 ;
      end;

   end else begin                                                 // 3: At the end
      newTreeNode := VstMain.AddChild(nil);
      NewOrder := LastChildOrder ;
      inc (LastChildOrder) ;
   end;

   VstMain.ReinitNode(newTreeNode, false);
   newTreeRec := VstMain.GetNodeData(newTreeNode);
   newTreeRec.MessageText := Frm_AddLine.EditTrace.Text;
   newTreeRec.Source      := Frm_AddLine.EditThId.Text;
   newTreeRec.Time        := Frm_AddLine.EditTime.Text;
   newTreeRec.EventRecordNum := NewOrder;
   newTreeRec.Members := TMember.Create;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainGetText(Sender: TBaseVirtualTree;
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
   if toEditable in VstMain.TreeOptions.MiscOptions then
      exit;

   if Length(CellText) > 400 then
      CellText := Copy(CellText, 1, 400) + '...'
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainCompareNodes(Sender: TBaseVirtualTree;
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

procedure TFrmEventLog.VstMainGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
   ImageIndex := -1 ;
   // the Image is displayed by the tree and need 36 pixels
   // See AfterCellPaint to draw on column 0 (no space left)
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
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


procedure TFrmEventLog.VstDetailSelectorSelectionChanged(Sender: TVstSelector; selectionAsText: string);
begin
   frameMemo.LabelSelect.Caption := selectionAsText;
   if (frameMemo.LabelSelect.Caption <> '') then
      FrameMemo.SetMemoText('',false,false);
end;

procedure TFrmEventLog.VstDetailFocusChanged(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex);
var
   CellText: String;
   Member : TMember  ;
begin
   if (Node = nil) then
      exit;
   Member := TMember (TObject (Sender.GetNodeData(Node)^)) ;
   if Member = nil then
      exit ;
   case Column of
      0 : CellText := Member.Col1 ;
      1 : CellText := Member.Col2 ;
      2 : CellText := Member.Col3 ;
      else  CellText := '' ;
   end ;
   if (frameMemo.LabelSelect.Caption = '') then
      frameMemo.SetMemoText(CellText,false,false);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstDetailColumnClick(Sender: TBaseVirtualTree;  Column: TColumnIndex; Shift: TShiftState);
var
   CellText: String;
   SelectedNode : PVirtualNode ;
   Member : TMember  ;
begin
   SelectedNode := VstDetail.GetFirstSelected  ;
   if SelectedNode = nil then
     exit ;
   Member := TMember (TObject (Sender.GetNodeData(SelectedNode)^)) ;
   if Member = nil then
      exit ;
   case Column of
      0 : CellText := Member.Col1 ;
      1 : CellText := Member.Col2 ;
      2 : CellText := Member.Col3 ;
      else  CellText := '' ;
   end ;
   if (frameMemo.LabelSelect.Caption = '') then
      frameMemo.SetMemoText(CellText,false,false);
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
     SelectedCompoNode := VstMain.GetFirstSelected ;
     if SelectedCompoNode = nil then
        exit ;

     var SecondSelect := VstMain.GetNextSelected (SelectedCompoNode) ;
     if SecondSelect <> nil then begin
         var selection := inttostr(VstMain.SelectedCount) + ' lines selected';
         case Column of
            0 : CellText := selection ;
            else  CellText := '' ;
         end ;
         exit;
     end;

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
   if toEditable in VstDetail.TreeOptions.MiscOptions then
      exit;

   if Length(CellText) > 400 then
      CellText := Copy(CellText, 1, 400) + '...'
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
   //fEventLog.Close ;  // stop monitoring
    if not ContainsText(fLogName,' ') then
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
   PageContainer.actFocus        .Enabled := true  ;
   PageContainer.actPause        .Enabled := true  ;
   PageContainer.actCopy         .Enabled := true  ;
   PageContainer.actSelectAll    .Enabled := true  ;
   PageContainer.actDelete       .Enabled := true  ;
   PageContainer.actCut          .Enabled := true  ;
   PageContainer.actSearch       .Enabled := true ;
   PageContainer.actFindNext     .Enabled := true ;

   PageContainer.actViewTraceInfo.checked := PanelTraceInfo.Visible ;
   PageContainer.actFocus        .checked := TraceConfig.AppDisplay_FocusToReceivedMessage; ;
   PageContainer.actPause        .checked := self.IsPaused ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ResizeColumns;
begin
   AutosizeAll (VstMain) ;
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

      EvntLogRec := VstMain.GetNodeData(VtNode) ;
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

   Forms.Application.ProcessMessages ;
   SetCursor(Screen.Cursors[crHourGlass]);

   try
      // create the header
      XMLRootData := NewData ;

      // generate nodes
      MasterTVNode := VstMain.RootNode ;
      generateNodeXML (XMLRootData ,MasterTVNode) ;  // recursive

      XMLRootData.OwnerDocument.SaveToFile(Frm_Tool.SaveDialog1.FileName);
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end ;
end;

//------------------------------------------------------------------------------

// copy time, source, first line of MessageText. Return true if element is focused

function TFrmEventLog.CopySelected: boolean;
var
   CopyStrings: TStringList;
   CopyText: PChar;
   NewLine: string;
   IsFirst : boolean ;
   EvntLogRec : PEvntLogRec ;
   focusedComponent : hwnd ;

   procedure CheckIfNodeSelected (TestNode : PVirtualNode) ;
   var
      ChildVtNode : PVirtualNode ;
   begin
      if VstMain.Selected [TestNode] then begin
         EvntLogRec := VstMain.GetNodeData(TestNode) ;

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

//   procedure CopyDetail (TestNode : PVirtualNode);
//   var
//      node : PVirtualNode ;
//      Member : TMember  ;
//   begin
//      if VstDetail.Selected [TestNode] then begin
//
//         Member := TMember (TObject (VstDetail.GetNodeData(TestNode)^)) ;
//         NewLine := TraceConfig.TextExport_TextQualifier + Member.Col1 + TraceConfig.TextExport_TextQualifier  +
//                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + Member.Col2 + TraceConfig.TextExport_TextQualifier +
//                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + Member.Col3 + TraceConfig.TextExport_TextQualifier  ;
//
//         CopyStrings.Add(NewLine);
//      end ;
//
//      // multi select
//      node := TestNode.FirstChild ;
//      while Node <> nil do begin
//         CopyDetail (node) ;
//         node := node.NextSibling ;
//      end ;
//   end ;

begin
   result := false;
   // reroute CTRL-C to the focused component if it's not the master tree
   if (VstDetail.Focused = false) and (VstMain.Focused = false) then begin
      focusedComponent := GetFocus ;
      if focusedComponent <> 0 then begin
         SendMessage(focusedComponent, WM_COPY, 0, 0);
         result := false;
      end ;
      exit ;
   end ;
   result := true;

   if VstMain.GetFirstSelected = nil then
      exit ;

   CopyStrings := TStringList.Create;
   SetCursor(Screen.Cursors[crHourGlass]);
   try

      if VstDetail.Focused then begin
         //CopyDetail (VstDetail.RootNode);
         VstDetailSelector.CopySelectedCells(CopyStrings, TraceConfig.TextExport_TextQualifier, TraceConfig.TextExport_Separator);

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
         CheckIfNodeSelected (VstMain.RootNode) ;
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
   if VstMain.Focused then begin
      Node := VstMain.FocusedNode ;
      if node = nil then
         exit ;
      VstMainGetText(VstMain, Node, VstMain.FocusedColumn,ttStatic,CellText);   // ttStatic is used to get the real text
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
   if VstMain.Focused = false then
     exit ;
   VstDetail.clear ;
   node := VstMain.GetFirstSelected ;
   if node = nil then   // no node selected
      exit ;
   node := VstMain.GetPreviousVisible(node) ;
   VstMain.DeleteSelectedNodes ;

   // case of the first node : GetPreviousVisible is nil ...
   if node = nil then
      node := VstMain.GetFirst
   else if VstMain.GetNextVisible(node) <> nil then
      node := VstMain.GetNextVisible(node) ;

   VstMain.FocusedNode := node ;
   VstMain.Selected [node] := true ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ClearWin;
begin
   VstMain.Clear ;
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
   if ((VstMain.IsEditing) or VstDetail.IsEditing) and (IVstEditor <> nil) and (TMoveMemoEditLink(IVstEditor).IsVisible) then begin
      VstEditor.SelectAll() ;  // TMoveMemoEditLink(IVstEditor).SelectAll() ;
      exit ;
   end ;
   if VstMain.Focused = true then
      VstMain.SelectAll(true)      // select all visible items (don't select filtered items)
   else if VstDetail.Focused then
      VstDetail.SelectAll(false) ;  // select all (visible or invisible)
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.TimerInfo;
begin
   TracesInfo.Caption := TimeToStr(LastModified)
                         + ', not filtered lines : ' + inttostr(VstMain.RootNode.ChildCount)
                         + '   "' + fLogName+ '"' ;
                        // + '   Total count : ' + inttostr (fEventLog.EventLogThread.userCount) ;
   TracesInfo.Hint := TracesInfo.Caption ;
   if NodeToFocus <> nil then begin
      VstMain.ClearSelection();
      VstMain.Selected [NodeToFocus] := true ;
      VstMain.FocusedNode := NodeToFocus;
      VstMain.ScrollIntoView (NodeToFocus,false,false);
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

procedure TFrmEventLog.VstMainHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
   VstMainChange (VstMain,nil);
   VstMain.Header.MainColumn := VstMain.Header.Columns.GetFirstVisibleColumn ;
   AutosizeAll (VstMain) ;
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
   PostMessage(Self.Handle, WM_STARTEDITING_MEMBER,
      NativeUInt(SelectedNode) and $FFFFFFFF,
      NativeUInt(SelectedNode) shr 32);
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
   Node := PVirtualNode(Pointer(
      (NativeUInt(Message.WParam) and $FFFFFFFF) or
      (NativeUInt(Message.LParam) shl 32)));
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.WMStartEditingTrace(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := PVirtualNode(Pointer(
      (NativeUInt(Message.WParam) and $FFFFFFFF) or
      (NativeUInt(Message.LParam) shl 32)));
   if Assigned(Node) then
      VstMain.EditNode(Node, VstMain.FocusedColumn);
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set
procedure TFrmEventLog.VstMainDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   //InternalTrace ('DetailDblClick ') ;
   SelectedNode := VstMain.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstMain.ScreenToClient(P);
   MouseNode := VstMain.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions + [toEditable] ;

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_TRACE,
      NativeUInt(SelectedNode) and $FFFFFFFF,
      NativeUInt(SelectedNode) shr 32);
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set
procedure TFrmEventLog.VstMainKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if CharCode = VK_F2 then
      VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions + [toEditable] ;
   if CharCode = VK_DELETE then
      DeleteSelected() ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrmEventLog.VstMainEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrmEventLog.VstMainEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions - [toEditable] ;
end;

procedure TFrmEventLog.VstMainEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainCreateEditor(Sender: TBaseVirtualTree;
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
      currentNode := VstMain.GetFirstVisible() ;
   end else begin
      currentNode := VstMain.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstMain.GetFirstVisible()
      else  // when start is false, we are searching in the current document
         currentNode := VstMain.GetNextVisible(currentNode) ;   // skip the first selected
   end ;

   while currentNode <> nil do begin
      EvntLogRec := VstMain.GetNodeData(currentNode) ;
      if CheckSearchRecord (EvntLogRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         VstMain.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstMain.ClearSelection;
         VstMain.Selected [currentNode] := true ;
         VstMain.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstMain.GetNextVisible(currentNode) ;
   end ;
end;

//------------------------------------------------------------------------------

function TFrmEventLog.SearchPrevious(atEnd: boolean): boolean;
var
   currentNode : PVirtualNode ;
   EvntLogRec : PEvntLogRec ;
   procedure CheckVisible () ;
   begin
      while (currentNode <> nil) and (VstMain.IsVisible[currentNode] = false) do begin
         currentNode := VstMain.GetPrevious(currentNode) ;
      end ;
   end ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if atEnd = true then begin
      currentNode := VstMain.GetLast() ;
   end else begin
      currentNode := VstMain.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstMain.GetLastVisible()
      else  // when atEnd is false, we are searching in the current document
         currentNode := VstMain.GetPrevious(currentNode) ;   // skip the first selected
   end ;

   CheckVisible() ;
   while currentNode <> nil do begin
      EvntLogRec := VstMain.GetNodeData(currentNode) ;
      if CheckSearchRecord (EvntLogRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         // fully visible ?
         VstMain.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstMain.ClearSelection;
         VstMain.Selected [currentNode] := true ;
         VstMain.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstMain.GetPrevious(currentNode) ;
      CheckVisible() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.RefreshView;
begin
   VstMain.Refresh ;
   VstDetail.Refresh ;
end;

//------------------------------------------------------------------------------

// if the paint area is modified, AfterPaint is called to redisplay the gutter

procedure TFrmEventLog.VstMainAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
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
   if VstMain.Header.Columns.Count <> 0 then begin
      // get the header height from the first header column
      // since the VT.headerRect property is protected :-(
      HeaderHeight := VstMain.Header.Columns[0].GetRect.Bottom ;
   end else begin  // should not happens
      HeaderHeight := VstMain.header.Height + 2 ;  // plus somme bevels
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
   Node := VstMain.GetNodeAt(0, 0, true, BaseOffset);
   if node <> nil then begin    // nothing to display
      // get the first visible node rectangle.
      DispRec := VstMain.GetDisplayRect (Node,NoColumn,false,false) ;

      // We just need the TOP node position
      // This top position is zero or negative since the node can be partially visible
      // but can never be more than zero (else we can have previous partial visible node before)
      Yposition := DispRec.Top ;

      // add Header height
      inc (Yposition , HeaderHeight) ;

      // draw each node
      while node <> nil do begin
         NodeHeight := VstMain.NodeHeight[Node] ;
         EvntLogRec := VstMain.GetNodeData(Node) ;

         BookmarkPos := bookmarks.IndexOf(Node) ;
         if BookmarkPos <> -1 then begin
            // if line is bookmarked

            // use original color in light and dark mode
            Frm_Tool.vilActionBookmark16.Draw(gutterCanvas, -2, Yposition-1, 0);
            // draw bookmark number or '...' after the bookmark icon
            if (bookmarks.Count > 1) then begin
               gutterCanvas.Font.Size  := 7;
               gutterCanvas.Font.Style := [fsBold];
               gutterCanvas.Font.Color := ClBlack;
               gutterCanvas.Brush.Style := bsClear;
               if BookmarkPos <= 99 then
                  gutterCanvas.TextOut(2, Yposition, IntToStr(BookmarkPos))
               else
                  gutterCanvas.TextOut(2, Yposition-3, #$2026);    // ellipse Unicode
            end;
         end else begin
            // else if line contains the search text with hightlight all
            if (unt_search.SearchText <> '') and (unt_search.SearchKind = mrYesToAll) then
               if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
                  if CheckSearchRecord(EvntLogRec) then // check if the node or one of his child match the search text
                     Frm_Tool.ilActions.Draw(gutterCanvas, 0, Yposition, 21);
         end ;

         // draw the small dot indicate sub members
         //if (treeRec.Members <> nil) and (treeRec.Members.SubMembers.Count <> 0) then
         //   Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 15);

         inc (Yposition , NodeHeight) ;
         Node := VstMain.GetNextVisible(Node) ;
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

procedure TFrmEventLog.VstMainBeforeCellPaint(Sender: TBaseVirtualTree;
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

   EvntLogRec := VstMain.GetNodeData(Node) ;
   if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
      if CheckSearchRecord (EvntLogRec) then     // check if the node or one of his child match the search text
         DrawHighlight (TargetCanvas, CellRect,false) ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainAfterCellPaint(Sender: TBaseVirtualTree;  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;  CellRect: TRect);
begin
   // draw the level icon (warning/error/debug/...) here.
   // This cannot be done using the OnGetImageIndex because it take too much space
   if (Column = 0) then begin
      var EvntLogRec: PEvntLogRec := Sender.GetNodeData(Node);
      var ImageIndex: Integer := EvntLogRec.EventIcon ;
      Frm_Tool.ImageList1.Draw(TargetCanvas, 0, 0, ImageIndex);
   end else begin
      var CellText: String ;
     VstMainGetText(Sender, Node,Column,ttStatic,CellText);   // ttStatic is used to get the real text
     if IsSeparator(CellText) then begin
        TargetCanvas.Pen.Color := clBlack;
        var middle := CellRect.Bottom div 2 ;
        if copy (trim(CellText),1,1) = '-' then begin
           TargetCanvas.MoveTo(0, middle);
           TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle);
        end else begin // '='
           TargetCanvas.MoveTo(0, middle - 1);
           TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle - 1);
           TargetCanvas.MoveTo(0, middle + 1);
           TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle + 1);
        end ;
     end ;
   end;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.ShowFilter;
begin
   if Filter = nil then
      Filter := TFrmFilter.create (self) ;

   Filter.Vst := VstMain ;
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
   EvntLogRec := VstMain.GetNodeData(Node) ;
   result := EvntLogRec.Members ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VSplitterCanResize(Sender: TObject;  var NewSize: Integer; var Accept: Boolean);
begin
   rightPercent := NewSize / (GroupPanel.Width - vsplitter.width);
   if (Width - NewSize < 105) then
      NewSize := Width - 105;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.GroupPanelCanResize(Sender: TObject; var NewWidth,  NewHeight: Integer; var Resize: Boolean);
begin
   PanelTraceInfo.Width := Round(GroupPanel.Width * rightPercent);
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.PanelGutterDblClick(Sender: TObject);
var
   Node : PVirtualNode ;
   P: TPoint;
   index : integer ;
begin

   GetCursorPos(P);
   P := VstMain.ScreenToClient(P);
   Node := VstMain.GetNodeAt(0, P.Y) ;
   if Node = nil then
      exit ;

   index := bookmarks.IndexOf(Node) ;
   if index = -1 then begin
      bookmarks.Add(Node) ;
   end else begin
      bookmarks.Delete(index) ;
   end ;

   VstMain.InvalidateNode(Node) ;
end;

//------------------------------------------------------------------------------

// main tree : fixed node height
procedure TFrmEventLog.VstMainMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
begin
   NodeHeight := TraceConfig.EventLog_Trace_NodeHeight ;
end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.VstMainPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.EventLog_Trace_FontName ;
   TargetCanvas.Font.size := TraceConfig.EventLog_Trace_FontSize ;
end;

//------------------------------------------------------------------------------

// member tree : node height depend of the number of lines (variable node height)
procedure TFrmEventLog.VstDetailMeasureItem(Sender: TBaseVirtualTree;  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
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
   VstMain.BeginUpdate ;
   VstMain.Font.Name         := TraceConfig.EventLog_Trace_FontName ;
   VstMain.Font.Size         := TraceConfig.EventLog_Trace_FontSize ;
   VstMain.DefaultNodeHeight := TraceConfig.EventLog_Trace_NodeHeight ;
   VstMain.ReinitChildren (nil,true);
   VstMain.EndUpdate ;

   VstDetail.BeginUpdate ;
   VstDetail.Font.Name         := TraceConfig.EventLog_Info_FontName ;
   VstDetail.Font.Size         := TraceConfig.EventLog_Info_FontSize ;
   VstDetail.ReinitChildren (nil,true);
   VstDetail.EndUpdate ;
end;

procedure TFrmEventLog.ApplyTheme;
const
  LightColTraces = TColor(16705515);  // lavender
  LightColDetail = TColor(16117479);  // blue-gray
  DarkColTraces  = TColor($00201018); // very dark warm-purple
  DarkColDetail  = TColor($00151520); // very dark blue-gray
var
  J: integer;
  ColTraces, ColDetail: TColor;
begin
    //TFrm_Trace.InternalTrace ('TFrmEventLog.ApplyTheme ' + caption );

    if TraceConfig.Dark_Enabled then begin
      ColTraces := DarkColTraces;
      ColDetail := DarkColDetail;
    end else begin
      ColTraces := LightColTraces;
      ColDetail := LightColDetail;
    end;

    // vstMain — only update columns that carry a known theme color (Lines column);
    // other columns (icon, Time, Source) use clWindow and must not be touched
    // -------
    for J := 0 to VstMain.Header.Columns.Count - 1 do
      if (VstMain.Header.Columns[J].Color = LightColTraces) or
         (VstMain.Header.Columns[J].Color = DarkColTraces) then
        VstMain.Header.Columns[J].Color := ColTraces;
    Frm_Tool.ApplyVstTheme(VstMain);

    // VstDetail
    // -------
    VstDetail.Color := ColDetail;
    for J := 0 to VstDetail.Header.Columns.Count - 1 do
      if (VstDetail.Header.Columns[J].Color = LightColDetail) or
         (VstDetail.Header.Columns[J].Color = DarkColDetail) then
        VstDetail.Header.Columns[J].Color := ColDetail;
    Frm_Tool.ApplyVstTheme(VstDetail);

    // Frame Memo
    // -------
    frameMemo.ApplySynMemoTheme ();

end;

//------------------------------------------------------------------------------

procedure TFrmEventLog.Print;
begin
   FrmPrintPreview.initialize(VstMain, nil) ;
   FrmPrintPreview.ShowModal ;
end;


function TFrmEventLog.GetWindowsEventLogs (aApplicationName: string; MaxNumberOfEntries: integer):TObjectList<TWindowsEvent>;
const
  wbemForwardOnly = 32;
  wbemReturnImmediately = 16;
  wbemFlagReturnWhenComplete = 0;
var
  event : TWindowsEvent;

  function DateTimeToWMI(const ADateTime: TDateTime): string;
  begin
    Result := FormatDateTime('yyyymmddHHnnss', ADateTime) + '.000000+000';
  end;

begin
  result := TObjectList<TWindowsEvent>.Create();
  try
    var iCount := 0;
    // https://www.codeproject.com/Articles/42571/WMI-Windows-Event-Logs-and-User-Privileges
    // https://learn.microsoft.com/en-us/windows/win32/wmisdk/swbemlocator-connectserver
    // https://learn.microsoft.com/fr-be/windows/win32/wmisdk/privilege-constants?redirectedfrom=MSDN
    // https://learn.microsoft.com/en-us/windows/win32/wmisdk/swbemservices-execquery
    // https://learn.microsoft.com/en-us/windows/win32/wmisdk/querying-with-wql

    var BindCtx: IBindCtx;
    OleCheck(CreateBindCtx(0, bindCtx));

    var Moniker: IMoniker;
    var chEaten: Integer;
    OleCheck(MkParseDisplayName(BindCtx, StringToOleStr('winmgmts:'), chEaten, Moniker));    // 'winmgmts:\\localhost\root\cimv2'  // {impersonationLevel=impersonate}

    var dispatch: IDispatch;
    OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, dispatch));

    var WMIService: OLEVariant := dispatch;

    var eventQuery :=
       'SELECT * FROM Win32_NTLogEvent '
       + 'Where Logfile = "' + aApplicationName + '" '
       + 'AND TimeGenerated >= "' + DateTimeToWMI(IncDay(Now(), -10)) + '"'  ;

    var WbemObjectSet: OLEVariant := WMIService.ExecQuery(
       EventQuery,    // text of the query
       'WQL',         // the query language to be used.
       wbemReturnImmediately + wbemForwardOnly); // wbemReturnImmediately + wbemForwardOnly);    // wbemFlagReturnWhenComplete

    var WbemObject: OLEVariant;
    var iValue: LongWord;
    var oEnum: IEnumvariant := IUnknown(WbemObjectSet._NewEnum) as IEnumvariant;

    while oEnum.Next(1, WbemObject, iValue) = 0 do begin
       event := TWindowsEvent.Create;
       event.PopulateFromOleVariant(WbemObject);
       result.Add(event);
       WbemObject := Unassigned;
       inc(iCount);
       if (MaxNumberOfEntries <> -1) and (iCount > MaxNumberOfEntries) then
          Break;
    end;
  except
    on E: EOleException do  begin
      event := TWindowsEvent.Create;
      event.Category := 'Error';
      event.Msg := Format('EOleException %s %x', [E.Message, E.ErrorCode]);
      result.Add(event);
    end ;
    on E: Exception do  begin
      event := TWindowsEvent.Create;
      event.Category := 'Error';
      event.Msg := E.Classname + ':' + E.Message;
      result.Add(event);
    end ;
  end;
end;

{ TWindowsEvent }

procedure TWindowsEvent.PopulateFromOleVariant(aEvent: OLEVariant);
var
  localInsertionArray: array of String;
  localDataArray: array of integer;
  i: integer;

  function WMIToDateTime(const AWMI: string): TDateTime;
  // Parses WMI format: YYYYMMDDHHMMSS.mmmmmm±UUU  (e.g. '20260527085724.063702-000')
  // Returns UTC TDateTime
  var
    Y, Mo, D, H, Mi, Se, Ms: Word;
    OffsetSign, OffsetMins: Integer;
  begin
    Y  := StrToInt(Copy(AWMI,  1, 4));
    Mo := StrToInt(Copy(AWMI,  5, 2));
    D  := StrToInt(Copy(AWMI,  7, 2));
    H  := StrToInt(Copy(AWMI,  9, 2));
    Mi := StrToInt(Copy(AWMI, 11, 2));
    Se := StrToInt(Copy(AWMI, 13, 2));
    Ms := StrToInt(Copy(AWMI, 16, 6)) div 1000;   // microseconds → milliseconds
    // Embedded UTC offset (±UUU in minutes) — subtract to get pure UTC
    if AWMI[22] = '+' then
      OffsetSign := 1
    else
      OffsetSign := -1;
    OffsetMins := StrToInt(Copy(AWMI, 23, 3));
    Result := EncodeDateTime(Y, Mo, D, H, Mi, Se, Ms) - OffsetSign * OffsetMins / (24 * 60);
  end;

begin
  // Identifies the event within the Windows event log file. This is specific to the log file and is used together with the log file name to uniquely identify an instance of this class.

  // Record numbers are always unique; they are not reset to 1 when an event log is cleared.
  // As a result, the highest record number also indicates the number of records that have
  // been written to the event log since the operating system was installed
  fRecordNumber := integer(aEvent.RecordNumber);

  //Classification of the event as determined by the source.
  // This subcategory is source-specific.
  fCategory := string (aEvent.Category);                // '0'

  // Translation of the subcategory. The translation is source-specific
  if not VarIsNull(aEvent.CategoryString) then
     fCategoryString := string (aEvent.CategoryString); // 'Application Crashing Events'

  // Type of event. This is an enumerated string.
  // It is preferable to use the EventType property rather than the "Type" property.
  // 1:Error,2:Warning,4:Information,8:Security Audit Success,16:Security Audit Failure
  if not VarIsNull(aEvent.Type) then
     fTypeString := string (aEvent.Type);               // 'Information'

  // Type of event.
  // 1:Error,2:Warning,3:Information,4:Security Audit Success,5:Security Audit Failure
  if not VarIsNull(aEvent.EventType) then
     fEventType := aEvent.EventType;

  // User name of the logged-on user when the event occurred.
  // If the user name cannot be determined, this will be NULL.
  if not VarIsNull(aEvent.User) then
     fUser := string (aEvent.User);                     // 'NT AUTHORITY\SYSTEM'

  // Name of the source (application, service, driver, or subsystem) that generated the entry.
  // It is used, together with EventIdentifier to uniquely identify a Windows event type.
  if not VarIsNull(aEvent.SourceName) then
     fSourceName := string (aEvent.SourceName);         // Microsoft-Windows-Security-SPP

  // The time when the event is written to the log file
  if not VarIsNull(aEvent.TimeWritten) then begin
     var dateStr : string := aEvent.TimeWritten ;      // '20260527085724.063702-000'
     fTimeWritten := WMIToDateTime(datestr);   // Utc
     fTimeWritten := TTimeZone.Local.ToLocalTime(fTimeWritten);
  end;

  // The time when the event is generated.
  if not VarIsNull(aEvent.TimeGenerated) then begin
     var dateStr : string := aEvent.TimeGenerated ;      // '20240602101605.176003-000'
     fTimeGenerated := WMIToDateTime(datestr);   // Utc
     fTimeGenerated := TTimeZone.Local.ToLocalTime(fTimeGenerated);  // Local Time
  end;

  // Identifier of the event.
  // This is specific to the source that generated the event log entry and is used, together with SourceName,
  // to uniquely identify a Windows event type.
  if not VarIsNull(aEvent.EventIdentifier) then
     fEventIdentifier := aEvent.EventIdentifier;         // 1073758208

  // Name of the computer that generated this event.
  fComputerName := string (aEvent.ComputerName);

  // Value of the lower 16-bits of the EventIdentifier property.
  // It is present to match the value displayed in the Windows Event Viewer.
  fEventCode := integer(aEvent.EventCode);

  // Event message as it appears in the Windows event log.
  // This is a standard message with zero or more insertion strings supplied by the source of the Windows event.
  // The insertion strings are inserted into the standard message in a predefined format.
  // If there are no insertion strings or there is a problem inserting the insertion strings,
  // only the standard message will be present in this field.
  if not VarIsNull(aEvent.Message) then
     fMessage := string (aEvent.Message)
  else
     fMessage := '';

  // Name of Windows event log file.
  // Together with RecordNumber, this is used to uniquely identify an instance of this class.
  fLogFile := string (aEvent.LogFile);             // Name of Windows event log file

  // List of the insertion strings that accompanied the report of the Windows event.
  if not VarIsNull(aEvent.InsertionStrings) then
  begin
     localInsertionArray := aEvent.InsertionStrings;
     var low: integer := VarArrayLowBound(localInsertionArray, 1);
     var high: integer := VarArrayHighBound(localInsertionArray, 1);
     setLength (fInsertionStrings,High+1-low);
     var index:integer := 0;

     for i := low to High do
     begin
        fInsertionStrings[index] := localInsertionArray[i];
        inc(index);
     end;
  end;

  // List of the binary data that accompanied the report of the Windows event. (uint8 array)
  if not VarIsNull(aEvent.Data) then
  begin
     localDataArray := aEvent.Data;
     var low: integer := VarArrayLowBound(localDataArray, 1);
     var high: integer := VarArrayHighBound(localDataArray, 1);
     setLength (fDataArray,High+1-low);
     var index:integer := 0;

     for i := VarArrayLowBound(localDataArray, 1) to VarArrayHighBound(localDataArray, 1) do
     begin
        fDataArray[index] := localDataArray[i];
        inc(index);
     end;
  end;

end;


end.
