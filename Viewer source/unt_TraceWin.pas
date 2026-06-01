{
  Trace window that display TraceNode messages (TraceTool framework)
  Author : Thierry Parent
  HomePage :  https://github.com/capslock66/Tracetool
}

unit unt_TraceWin;

interface

   uses
      system.types, system.UITypes,Windows, Messages, SysUtils, Variants,
      Classes, Graphics, Controls,Vcl.Themes,
      Forms, registry,
      Dialogs, StdCtrls, ExtCtrls,  Menus, XMLDoc, XMLIntf,
      math, printers,
      ComCtrls, ToolWin, ImgList, TrayIcon, ActnList, clipbrd, SyncObjs,
      Contnrs, SynEdit,
      unt_tool, VstSelector,
      Application6, // the generated delphi code for the XML schema (Application6.xsd)
      unt_base, DebugOptions, Buttons, Unt_linkedList, unt_utility,
      MSXML2_TLB,

      unt_pageContainer, unt_editor, unt_search, vstSort, unt_filter, unt_addLine,
      VirtualTrees, VirtualTrees.Types, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL;
   {$INCLUDE TraceTool.Inc}

   const
     // Columns

     // classic traces
     COL_LEVELANDTYPE = 0;
     COL_TIME         = 1;
     COL_THID         = 2;
     COL_TRACE        = 3;
     COL_COMMENT      = 4;

     // watches
     COL_WATCH_TIME   = 0;
     COL_WATCH_THID   = 1;
     COL_WATCH_NAME   = 2;
     COL_WATCH_VALUE  = 3;
     COL_WATCH_TYPE   = 4;

   type

      // ---------------------------------------------------------------------------------

      // can't be moved in implementation section because also used by TNodeLinkedList
      PTreeRec = ^TTreeRec;

      TTreeRec = record // see InitTreeRec for init
         OriginalOrder: integer; // Original order when inserted. Used to Unsort nodes
         FirstChildOrder: integer; // Order of the last child, used to insert sub nodes and unsort them
         LastChildOrder: integer; // Order of the last child, used to insert sub nodes and unsort them
         LeftMsg: string; // Left col
         RightMsg: string; // right col
         TraceID: AnsiString; // the reference of the node : it's a guid  (wrapper compatibility : single byte)
         Time: string; // time of send
         ThreadID: string; // thread id of the sender
         IP: string; // client IP
         ProcessName: string; // optional : the name of the process that send traces
         Members: TMember; // members of the sent object
         TreeIcon: Smallint; // -1 by default, converted to 24
         Columns: TStringList; // multi columns strings
         FontDetails: TFontDetailArray;
      end;

      // ---------------------------------------------------------------------------------

      // detail tree

      PDetailRec = ^TDetailRec;

      TDetailRec = record
         Col1: string;
         Col2: string;
         Col3: String;
         FontDetails: TFontDetailArray;
      end;

      // ---------------------------------------------------------------------------------

      TLevel = class
      public
         Node: PVirtualNode;
         CurrentChild: PVirtualNode;
         Col2Empty: boolean;
         Col3Empty: boolean;
      end;

      // ---------------------------------------------------------------------------------

      TFrm_Trace = class(TFrmBase)
         PanelRight: TPanel;
         VSplitter: TSplitter;
         PanelLeft: TPanel;
         vstMain: TVirtualStringTree;
         PanelTTraces: TPanel;
         PanelTop: TPanel;
         TracesInfo: TLabel;
         butClose: TBitBtn;
         LabelLogFile: TLabel;
         PanelGutter: TPanel;
         PopupTree: TPopupMenu;
         Cut1: TMenuItem;
         Copy1: TMenuItem;
         Copycurrentcell1: TMenuItem;
         Delete1: TMenuItem;
         N1: TMenuItem;
         mnuTogglebookmark: TMenuItem;
         mnuSelectAll: TMenuItem;
         mnuExpandAll: TMenuItem;
         mnuCollapseAll: TMenuItem;
         procedure FormCreate(Sender: TObject);
         procedure vstMainGetText(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
            var CellText: String);
         procedure vstMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
         procedure vstMainFreeNode(Sender: TBaseVirtualTree;
            Node: PVirtualNode);
         procedure vstMainGetImageIndex(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
            var Ghosted: Boolean; var ImageIndex: System.UITypes.TImageIndex);
         procedure butCloseClick(Sender: TObject);
         procedure vstMainGetHint(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex;
            var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
         procedure vstMainHeaderDragged(Sender: TVTHeader;
            Column: TColumnIndex; OldPosition: Integer);
         procedure vstMainAfterPaint(Sender: TBaseVirtualTree;
            TargetCanvas: TCanvas);
         procedure LabelLogFileClick(Sender: TObject);
         procedure TracesInfoClick(Sender: TObject);
         procedure FormClose(Sender: TObject; var Action: TCloseAction);
         procedure vstMainDblClick(Sender: TObject);
         procedure vstMainCreateEditor(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex;
            out EditLink: IVTEditLink);
         procedure vstMainEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
            Column: TColumnIndex);
         procedure vstMainEditCancelled(Sender: TBaseVirtualTree;
            Column: TColumnIndex);
         procedure vstMainKeyAction(Sender: TBaseVirtualTree;
            var CharCode: Word; var Shift: TShiftState; var DoDefault: boolean);
         procedure vstMainCompareNodes(Sender: TBaseVirtualTree;
            Node1, Node2: PVirtualNode; Column: TColumnIndex;
            var Result: Integer);
         procedure vstMainAfterCellPaint(Sender: TBaseVirtualTree;
            TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
            CellRect: TRect);
         procedure PanelGutterDblClick(Sender: TObject);
         procedure vstMainPaintText(Sender: TBaseVirtualTree;
            const TargetCanvas: TCanvas; Node: PVirtualNode;
            Column: TColumnIndex; TextType: TVSTTextType);
         procedure mnuExpandAllClick(Sender: TObject);
         procedure mnuCollapseAllClick(Sender: TObject);
         procedure vstMainBeforeCellPaint(Sender: TBaseVirtualTree;
            TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
            CellPaintMode: TVTCellPaintMode; CellRect: TRect;
            var ContentRect: TRect);
        procedure vstMainEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
            Column: TColumnIndex; var Allowed: Boolean);
        procedure PanelLeftResize(Sender: TObject);
        procedure VSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
        procedure PanelTTracesCanResize(Sender: TObject; var NewWidth,
            NewHeight: Integer; var Resize: Boolean);
        procedure vstMainMeasureItem(Sender: TBaseVirtualTree;
            TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);

      private
         procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
         procedure WMStartEditingTrace(var Message: TMessage);  message WM_STARTEDITING_TRACE;
      public
         class procedure InternalTraceFromThread(LeftMsg: string);
         class function InternalTrace(LeftMsg: string; RightMsg: string = '') : PVirtualNode; overload;
         class function InternalTrace(Node: PVirtualNode; LeftMsg: string; RightMsg: string = ''): PVirtualNode; overload;
         class procedure InternalDumpFromThread(const memory: pointer; const ByteCount: Integer); static;
         class procedure InternalClearTrace; static;
         class procedure checkInternals;
         function ChangeFontDetail(const IsTrace: boolean; const TargetCanvas: TCanvas; const Column: TColumnIndex; const FontDetails: TFontDetailArray; const selected: boolean): boolean;
         function ChangeBackgroundColor(const TargetCanvas: TCanvas; const CellRect: TRect; const Column: TColumnIndex; const FontDetails: TFontDetailArray; const selected: boolean): boolean;
         procedure CalculateDump(Member: TMember);
         procedure ResetDump(Member: TMember);
      public
         procedure AddOneLineDetail(Col1, Col2, Col3: String; isXml:boolean=false; isJson : boolean=false);
         procedure AddToLog(ActiveNode, ParentCompoNode: PVirtualNode);
         procedure ShowLog;
         function CheckSearchRecord(TreeRec: PTreeRec): boolean;
         procedure GenerateCols(cols: String);
         procedure ChangeColWidths(colWidths: String);
         procedure CheckColumns(cols: TStringList); // check if TreeRec.Columns contain more columns than the tree
         function CheckNode(rootVtNode: PVirtualNode; NodeToCheck: AnsiString; IsNew: boolean = false): PVirtualNode;
         function CheckWatch(rootVtNode: PVirtualNode; WatchName: string): PVirtualNode;
         procedure LoadXML(filename: string);
         procedure LoadXML_IE(filename: string);
         procedure SaveToXML(filename: string);
         procedure SaveToTextFile(filename: string; SaveOptions: TSaveTofileOptions); // change the LabelLogFile caption
      public
         // detail frames
         CurrentViewers: TObjectList; // not Owner , array of Tframe_BaseDetails. Viewers that are currently displayed.
         TableFrame: TFrame;
         BitmapFrame: TFrame;
         TreeDetailFrame: TFrame;
         VstDetail: TVirtualStringTree;
         VstDetailHaschildren: boolean;

         Gutter: TImage;
         NodeToFocus: PVirtualNode;
         LostAndFound: PVirtualNode;
         LastChildOrder: cardinal; // Order of the last child, used to insert sub nodes and unsort them
         FirstChildOrder: integer; // Order of the first child, used to insert sub nodes and unsort them
         ID: AnsiString; // id of the trace form (empty for the main trace). Plugin compatibility : Is is single byte
         LastModified: tDateTime;
         IsPaused: boolean;
         IsMultiColTree: boolean;
         IsWatch: boolean;
         IsDateTimeResized: boolean;
         MainCol: Integer;
         rightPercent : extended;
         // Log variables
         LogFileName: string;
         LogFileType: Integer;
         MaxLines: Integer;
         CurrentFileNumber: Integer;
         LinesWritten: Integer;

      public // Plugin.
         // plugins associated to this window
         LinkedPlugins: TObjectList; // owner
         // All resources liste are TPlugResource
         LeftResources: TObjectList; // left resources on the status bar
         RightResources: TObjectList; // right resources on the status bar
         ActionResources: TObjectList; // item menu in the action menu
         WindowResources: TObjectList; // item menu in the window resource
         MostUsedList: TNodeLinkedList; // add, remove, search
         LastUsedList: TNodeLinkedList;
         XMLLogFile: IXMLData;
         Sorter: TVstSort;

         // plugin api
         procedure AddPlugin(pluginName: AnsiString; flags: Integer);
         procedure CreateResource(ResId: Integer; ResType: Integer;
            ResWidth: Integer; ResText: string);
         function SetTextResource(ResId: Integer; ResText: string): boolean;
         procedure DisableResource(ResId: Integer);
         procedure PluginAction(Sender: TObject); // event called by user action
         function askDelete(NodeToDelete: PVirtualNode): boolean;
         // ask plugin and also clear cache list
         function NeedBeforeDelete: boolean; // check if a plugin attached to this window need the BeforeDelete event
         function DoPlugAction(ResId: Integer; NodeId: AnsiString): boolean;
         // call the plugin OnAction
         function DoAskPlugDelete(NodeId: AnsiString): boolean;
         // call the plugin OnBeforeDelete
         procedure DoPlugTimer; // call the plugin OnTimer
         procedure DrawStatusBar; // draw status bar
         procedure DrawActionMenu(ToEnable: boolean); // draw Action menu
         procedure InitColumns;

      public // TFrmBase
         procedure Print; override;
         procedure ClearWin; override;              // CST_ACTION_CLEAR_ALL
         procedure SaveWin; override;               // CST_ACTION_SAVE
         procedure PauseWin; override;              // CST_ACTION_PAUSE
         procedure ViewTraceInfo; override;         // CST_ACTION_VIEW_INFO
         procedure ViewProperty; override;          // CST_ACTION_VIEW_PROP
         function  CopySelected: boolean; override; // CST_ACTION_COPY  Return true if element is focused
         procedure CopyCurrentCell; override;
         procedure DeleteSelected; override;        // CST_ACTION_DELETE
         procedure SelectAll; override;             // CST_ACTION_SELECT_ALL
         procedure CloseWin; override;              // CST_ACTION_CLOSE_WIN
         procedure ResizeColumns; override;         // CST_ACTION_RESIZE_COLS
         procedure CheckAutoClear; override;        // no action
         procedure PageControlChange(); override;   // no action
         procedure TimerInfo; override;             // DoPlugTimer()
         procedure RefreshView; override;
         procedure ShowFilter; override;
         procedure ApplyFont; override;
         procedure InsertRow ; override;
         procedure ApplyTheme; override;

         function getMembers(Node: PVirtualNode): TMember; override;
         function SearchNext(start: boolean): boolean; override;
         function SearchPrevious(atEnd: boolean): boolean; override;
      end;

   var
      Frm_Trace: TFrm_Trace; // main trace form
      Frm_Watches: TFrm_Trace; // main watch form
      FrmInternalTraces: TFrm_Trace; // internal trace form

   function CreateWatchForm(ID: AnsiString; name: string): TFrm_Trace;

implementation

uses
   Unt_receiver
   , unt_about
   , unt_tail
   , xmldom
   , unt_saveDlg
   , unt_TraceConfig
   , unt_ODS
   , unt_traceWinProperty
   , unt_plugin
   , unt_Details_base
   , unt_Details_bitmap
   , unt_Details_table
   , unt_Details_Classic
   , unt_FrameMemo
   , Unt_TailProgress
   , untPrintPreview
   , Preview
   ;
   {$R *.dfm}
   // ------------------------------------------------------------------------------
   // ------------------------------------------------------------------------------
   // ------------------------------------------------------------------------------

function CreateWatchForm(ID: AnsiString; name: string): TFrm_Trace;
var
   col: TVirtualTreeColumn;
begin
   Result := TFrm_Trace.Create(nil);
   Result.ID := ID;
   Result.IsWatch := true;
   Result.IsMultiColTree := true;
   Result.Caption := name;
   Result.DockToMainPanel();
   Result.getPageContainer().actViewTraceInfo.Checked := false;
   Result.ViewTraceInfo;
   if Result.getPageContainer().DockingPagecontrol.GetVisibleClientCount() = 1 then
      Result.SetActivePage;

   Result.VstMain.Header.Columns.Delete(0); // remove ico column (level and Type)

   // new 0 is the Time column       : COL_WATCH_TIME = 0
   // new 1 is the thread id column  : COL_WATCH_THID = 1

   // new 2 is the watch name column : COL_WATCH_NAME = 2
   Result.VstMain.Header.Columns[COL_WATCH_NAME].Text := 'Watch';
   Result.VstMain.Header.Columns[COL_WATCH_NAME].Width := 220;

   // new 3 is the Value column      : COL_WATCH_VALUE = 3
   Result.VstMain.Header.Columns[COL_WATCH_VALUE].Text := 'Value';
   Result.VstMain.Header.Columns[COL_WATCH_VALUE].MinWidth := 100;
   Result.VstMain.Header.Columns[COL_WATCH_VALUE].Width := 220;

   // new 4 id the Type column       : COL_WATCH_TYPE = 4
   col := Result.VstMain.Header.Columns.Add;
   col.MinWidth := 3000;
   col.MaxWidth := 10000;
   col.Width := 9000; // force last column width to maximum
   col.Text := 'Type';
   col.Color := Result.VstMain.Header.Columns[COL_WATCH_VALUE].Color;
   col.options := col.options + [coAllowFocus];
   // ensure user can focus to this column

   Result.VstMain.Header.MainColumn := COL_WATCH_NAME;
   Result.VstMain.Header.AutoSizeIndex := -1; // auto
   // result.VstMain.OnDrawNode := result.DrawNode ;

   Result.ApplyTheme();
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

procedure TFrm_Trace.FormCreate(Sender: TObject);
var
   res: TPlugResource;
   accept : boolean;
   size : integer;
begin
   inherited;

   size := PanelRight.Width;
   VSplitterCanResize(self,size,accept); // calculated once left and right percent

   IsDateTimeResized := false;
   IsWatch := false;
   IsMultiColTree := false;

   vst := VstMain;
   Gutter := nil;

   // initialize sort
   LastChildOrder := 1;
   FirstChildOrder := 0;
   Sorter := TVstSort.Create(self);
   Sorter.tree := VstMain;
   Sorter.UtilityImages := Frm_Tool.UtilityImages;
   Sorter.canUnsort := true;

   // redirect some events to the sorter
   VstMain.onHeaderClick := Sorter.onHeaderClick;
   VstMain.OnKeyUp := Sorter.OnKeyUp;
   VstMain.onHeaderDrawQueryElements := Sorter.onHeaderDrawQueryElements;
   VstMain.onAdvancedHeaderDraw := Sorter.onAdvancedHeaderDraw;

   FormTraceList.Add(self);

   LogFileName := 'TraceLog.xml'; // default
   LogFileType := 0; // 0 : no log , 1 : No limit , 2 : Daily
   MaxLines := -1; // no limit
   CurrentFileNumber := 0;
   LinesWritten := 0;

   // LogStyleSheet := '' ;
   XMLLogFile := nil;
   IsPaused := false;
   LastModified := now;

   CurrentViewers := TObjectList.Create(false);      // viewer list. Current displayed viewers
   LinkedPlugins := TObjectList.Create(true);
   LeftResources := TObjectList.Create(true);        // left resources on the status bar
   RightResources := TObjectList.Create(true);       // right resources on the status bar
   ActionResources := TObjectList.Create(true);      // item menu in the action menu
   WindowResources := TObjectList.Create(true);      // item menu in the window resource

   // add the TracesInfo label to the LeftResources list
   TracesInfo.Tag := CST_ACTION_LABEL_INFO;
   res := TPlugResource.Create;
   res.Obj := TracesInfo;
   res.ID := 20;
   res.Internal := true;
   LeftResources.Add(res);

   // add the LabelLogFile label to the RightResources list
   res := TPlugResource.Create;
   res.Obj := LabelLogFile;
   res.ID := 21;
   res.Internal := true;
   RightResources.Add(res);

   LostAndFound := nil;

   MostUsedList := TNodeLinkedList.Create(50);
   MostUsedList.name := self.Caption + '.MostUsedList';
   LastUsedList := TNodeLinkedList.Create(100);
   LastUsedList.name := self.Caption + '.LastUsedList';

   // ---------------------------------------------------------------------------

   VstMain.NodeDataSize := sizeof(TTreeRec);

   // set the main column
   VstMain.Header.MainColumn := 3;      // must be the "trace column", else the tree will not expandable
   VstMain.Header.AutoSizeIndex := -1; // auto

   VstMain.Header.options := VstMain.Header.options
      - [hoAutoResize]               // no auto resize columns
      + [hovisible]                  // header must be visible to enable resize !
      + [hoOwnerDraw]                // needed for sort
      + [hoDblClickResize];          // allows a column to resize itself to its largest entry

   // VstMain.TreeOptions.AnimatedOptions
   // VstMain.TreeOptions.StringOptions

   VstMain.TreeOptions.AutoOptions := VstMain.TreeOptions.AutoOptions
      + [toAutoSpanColumns]          // Large entries continue into next columns
      + [toDisableAutoscrollOnFocus] // Disable scrolling a column entirely into view if it gets focused.
                                     // TPA : If not set, when the user click a cell, vt will
                                     // scroll the complete column into view. Pose problem with ODS for example
      + [toDisableAutoscrollOnEdit]; // Same for edit : don't scroll to column

   VstMain.TreeOptions.PaintOptions := VstMain.TreeOptions.PaintOptions
      - [toUseBlendedImages]        // Don't use blended images
      - [toShowTreeLines]           // don't Display tree lines to show hierarchy of nodes.
      - [toHideSelection]           // show a grayed selection when the tree lose the focus
      + [toShowRoot]                // show root.
      + [toShowButtons]             // Display collapse/expand buttons left to a node.
      + [toThemeAware]              // Draw UI elements (header, tree buttons etc.) according to the current theme
      + [toHideFocusRect];          // hide focus rect

   VstMain.TreeOptions.SelectionOptions := VstMain.TreeOptions.SelectionOptions
      + [toExtendedFocus]           // Entries other than in the main column can be selected, edited etc.
      + [toFullRowSelect]           // selection highlight the whole line
      + [toSimpleDrawSelection]     // Simplifies draw selection, so a node's caption does not need to intersect with the selection rectangle.
      + [toMultiselect];            // Allow more than one node to be selected.

   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions
      + [toReportMode]              // Tree behaves like TListView in report mode.
      + [toFullRepaintOnResize]     // Fully invalidate the tree when its window is resized (CS_HREDRAW/CS_VREDRAW).
      + [toWheelPanning]            // Support for mouse panning (wheel mice only).
      - [toFullRowDrag]             // Start node dragging by clicking anywhere in it instead only on the caption or image.
                                    // Must be used together with toDisableDrawSelection.
      + [toGridExtensions]          // Use some special enhancements to simulate and support grid behavior.
      - [toVariableNodeHeight]      // variable node height
      - [toToggleOnDblClick]        // Toggle node expansion state when it is double clicked.
      - [toEditable]                // don't allow edition. Code is used to detect double click
      - [toCheckSupport];           // no checkboxes

   if TraceConfig.Framework_KindIconOnLeft then
      vstMain.Header.columns[COL_LEVELANDTYPE].Width := 40
   else
      vstMain.Header.columns[COL_LEVELANDTYPE].Width := 20;
   //if assigned (FrmInternalTraces) then
   //   FrmInternalTraces.InternalTrace('TFrm_Trace.FormCreate ' + Caption +  ', VstMain=' +  inttostr(integer(VstMain)));

   TreeDetailFrame := Tframe_Classic.Create(self);
   Tframe_Classic(TreeDetailFrame).Splitter.Visible := false; // no spliter for main detail
   VstDetail := Tframe_Classic(TreeDetailFrame).VstDetail;    // shortcut

   //if assigned (FrmInternalTraces) then
   //   FrmInternalTraces.InternalTrace('TFrm_Trace.FormCreate ' + Caption +  ', VstDetail=' +  inttostr(integer(VstDetail)));

   ApplyFont();  // set font name and size for the 2 trees (from XMLConfig)
   ShowLog();    // change the LabelLogFile caption
   ApplyTheme(); // display dark theme
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   VstDetail.Clear;
   VstMain.Clear;
   XMLLogFile := nil;
   FormTraceList.Remove(self); // remove from Trace form List. List is not owner

   if (CurrentViewers <> nil) then begin
      CurrentViewers.Clear;
      FreeAndNil(CurrentViewers);
   end; // not owner

   if (LinkedPlugins <> nil) then begin
      LinkedPlugins.Clear;
      FreeAndNil(LinkedPlugins);
   end; // owner
   if (LeftResources <> nil) then begin
      LeftResources.Clear;
      FreeAndNil(LeftResources);
   end; // left resources on the status bar
   if (RightResources <> nil) then begin
      RightResources.Clear;
      FreeAndNil(RightResources);
   end; // right resources on the status bar
   if (ActionResources <> nil) then begin
      ActionResources.Clear;
      FreeAndNil(ActionResources);
   end; // item menu in the action menu
   if (WindowResources <> nil) then begin
      WindowResources.Clear;
      FreeAndNil(WindowResources);
   end; // item menu in the window resource

   if (MostUsedList <> nil) and (MostUsedList.Count <> 0) then begin
      // LowTrace (self.Name + #9 + 'MostUsedList ') ;
      // MostUsedList.Print(VstMain) ;
      MostUsedList.RemoveAll();
   end;
   if (MostUsedList <> nil) then
      FreeAndNil(MostUsedList); // add, remove, search

   if (LastUsedList <> nil) and (LastUsedList.Count <> 0) then begin
      // LowTrace (self.Name + #9 + 'LastUsedList ') ;
      // LastUsedList.Print(VstMain) ;
      LastUsedList.RemoveAll();
   end;
   if (LastUsedList <> nil) then
      FreeAndNil(LastUsedList);

   if filter <> nil then
      filter.Free;
   filter := nil;
   inherited;
end;

// ------------------------------------------------------------------------------
// thread cannot directly use the InternalTrace() method.
// internal traces are stored in a tempory stack
class procedure TFrm_Trace.InternalTraceFromThread(LeftMsg: string);
begin
   if TraceConfig.AppDisplay_DisableInternalLog = true then
      exit;
   if FrmInternalTraces = nil then
      exit;

   // remove command only when you want to trace internalTrace to file
   // LowTrace(Leftmsg);
   Criticalsection.Enter;
   try
      InternalTraceMessageStack.Add(LeftMsg);
   finally
      Criticalsection.leave;
   end;
end;

class procedure TFrm_Trace.InternalDumpFromThread (const memory: pointer; const ByteCount: Integer);
var
   c, d, beginLine: Integer;
   hexa_representation: string;
   Str_representation: string;
   Ptr: PAnsiChar;
   OneIntChar: Integer;
   OneChar: AnsiChar;
   AnsiCharTypes: array [AnsiChar] of Word;
   CurrChar: AnsiChar;
   CurrType: Word;
begin

   for CurrChar := Low(AnsiChar) to High(AnsiChar) do
   begin
      GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, sizeof (AnsiChar), CurrType);
      AnsiCharTypes[CurrChar] := CurrType;
   end;

   Ptr := memory;
   c := 0;
   while c <= ByteCount - 1 do begin
      d := 0;

      beginLine := c;
      hexa_representation := '';
      Str_representation := '';
      while (c <= ByteCount - 1) and (d < 32) do begin
         OneIntChar := Integer(Ptr^);
         hexa_representation := hexa_representation + intTohex
            (OneIntChar, 2) + ' ';

         OneChar := AnsiChar(chr(OneIntChar));

         if AnsiCharTypes[OneChar] and (C1_ALPHA or C1_PUNCT or C1_BLANK or C1_XDIGIT or C1_DIGIT) <> 0 then
            Str_representation := Str_representation + string(OneChar)
         else
            Str_representation := Str_representation + '.';

         inc(d);
         inc(c);
         inc(Ptr);
      end;
      if hexa_representation <> '' then
         InternalTraceFromThread(intTohex(beginLine, 6) + ' ' + hexa_representation + ' ' + Str_representation);
   end;

end;

// ------------------------------------------------------------------------------


// ------------------------------------------------------------------------------

Class procedure TFrm_Trace.checkInternals;
var
   tempMessageStack: TStringList;
begin
   if FrmInternalTraces = nil then
      exit;

   if InternalTraceMessageStack.Count = 0 then
      exit;

   Criticalsection.Enter;
   try
      // swap the message list to limit critical section lock time
      tempMessageStack := InternalTraceMessageStack;
      InternalTraceMessageStack := InternalTraceMessageStack2;
      InternalTraceMessageStack2 := tempMessageStack;
   finally
      Criticalsection.leave;
   end;

   while InternalTraceMessageStack2.Count > 0 do begin
      InternalTrace(InternalTraceMessageStack2.Strings[0]);
      InternalTraceMessageStack2.Delete(0);
   end;
end;

// ------------------------------------------------------------------------------
// create the Internal form if necesary then show it.
// note : low level trace on text file can be done using the unt_utility.LowTrace() method
class function TFrm_Trace.InternalTrace(LeftMsg: string; RightMsg: string = ''): PVirtualNode;
begin
   Result := InternalTrace(nil, LeftMsg, RightMsg);
end;

// ------------------------------------------------------------------------------
// create the Internal form if necesary then show it.
// note : low level trace on text file can be done using the unt_utility.LowTrace() method
class function TFrm_Trace.InternalTrace(Node: PVirtualNode; LeftMsg: string; RightMsg: string = ''): PVirtualNode;
var
   TreeRec: PTreeRec;
begin
   Result := nil;
   if FrmInternalTraces = nil then
      exit;
   if TraceConfig.AppDisplay_DisableInternalLog = true then
      exit;

   // todo : comment this line for production
   // unt_utility.LowTrace(leftMsg + ' ' + RightMsg) ;

   if FrmInternalTraces.Visible = false then begin
      FrmInternalTraces.Visible := true;
      if FrmInternalTraces.Parent = nil then
         FrmInternalTraces.DockToMainPanel();
      FrmInternalTraces.SetActivePage(); // PageControlChange() ;
   end;
   Result := FrmInternalTraces.VstMain.AddChild(Node);
   // ensure node is initialized. Needed when the node is free to call onFreeNode
   FrmInternalTraces.VstMain.ReinitNode(Result, false);
   // FrmInternalTraces.VstMain.FocusedNode := result ;
   TreeRec := FrmInternalTraces.VstMain.GetNodeData(Result);
   TreeRec.LeftMsg := LeftMsg;
   TreeRec.RightMsg := RightMsg;
end;

class procedure TFrm_Trace.InternalClearTrace();
begin

   if FrmInternalTraces.Visible = false then begin
      FrmInternalTraces.Visible := true;
      if FrmInternalTraces.Parent = nil then
         FrmInternalTraces.DockToMainPanel();
      FrmInternalTraces.SetActivePage(); // PageControlChange() ;
   end;
   FrmInternalTraces.VstMain.clear();
end;

// ------------------------------------------------------------------------------

// check if TreeRec.Columns contain more columns than the tree
// if yes, add column to tree
// called by the Parser
procedure TFrm_Trace.CheckColumns(cols: TStringList);
var
   c: Integer;
   col: TVirtualTreeColumn;
begin
   if cols.Count > VstMain.Header.Columns.Count then begin
      for c := VstMain.Header.Columns.Count to cols.Count - 1 do begin
         col := VstMain.Header.Columns.Add;
         col.options := col.options + [coAllowFocus];
         // ensure user can focus to this column
         col.MinWidth := 10;
         col.MaxWidth := 10000;
         col.Width := 100;
      end;

      VstMain.Header.MainColumn := 0; // FixedColCount-1 ;
      VstMain.Header.AutoSizeIndex := -1; // auto

      // resize all columns, using the header text and all lines
      AutosizeAll(VstMain); // VstTail.Header.AutoFitColumns(false);

      // force last column width to maximum
      VstMain.Header.Columns[VstMain.Header.Columns.Count - 1].Width := 9000;
   end;
end;

// ------------------------------------------------------------------------------

// called by the parser (CST_TREE_COLUMNTITLE)
procedure TFrm_Trace.GenerateCols(cols: String);
var
   c: Integer;
   Titles: TStringList;
   col: TVirtualTreeColumn;
begin
   Titles := getTabStrings(pchar(cols));
   IsMultiColTree := true;
   VstMain.Header.Columns.Clear;
   for c := 0 to Titles.Count - 1 do begin
      col := VstMain.Header.Columns.Add;
      col.options := col.options + [coAllowFocus];
      // ensure user can focus to this column
      col.MinWidth := 10;
      col.MaxWidth := 10000;
      col.Width := 100;
      col.Text := Titles[c];
   end;

   VstMain.FocusedColumn := 0;
   if (MainCol > 0) and (MainCol < Titles.Count) then
      VstMain.Header.MainColumn := MainCol
   else
      VstMain.Header.MainColumn := 0; // FixedColCount-1 ;
   VstMain.Header.AutoSizeIndex := -1; // auto

   // resize all columns, using the header text and all lines
   AutosizeAll(VstMain); // VstTail.Header.AutoFitColumns(false);

   // force last column width to maximum
   VstMain.Header.Columns[VstMain.Header.Columns.Count - 1].Width := 9000;

   Titles.Free;
end;

// ------------------------------------------------------------------------------

// called by the parse (CST_TREE_COLUMNWIDTH)
procedure TFrm_Trace.ChangeColWidths(colWidths: String);
var
   c: Integer;
   Widths: TStringList;
   WidthParts: TStringList;
   ColWidthStr: string;
   col: TVirtualTreeColumn;
begin
   Widths := getTabStrings(pchar(colWidths));
   IsMultiColTree := true;
   for c := 0 to Widths.Count - 1 do begin
      if c >= VstMain.Header.Columns.Count then
         break;
      col := VstMain.Header.Columns[c];
      ColWidthStr := Widths[c];
      WidthParts := getDelimStrings(pchar(ColWidthStr), ':');
      if WidthParts.Count > 0 then
         col.Width := StrToIntDef(trim(WidthParts[0]), 100);
      if WidthParts.Count > 1 then
         col.MinWidth := StrToIntDef(trim(WidthParts[1]), 100);
      if WidthParts.Count > 2 then
         col.MaxWidth := StrToIntDef(trim(WidthParts[2]), 100);
      WidthParts.Free;
   end;

   // force last column width to maximum
   VstMain.Header.Columns[VstMain.Header.Columns.Count - 1].Width := 9000;

   Widths.Free;
end;

// ------------------------------------------------------------------------------

// return the node in the root where leftMsg is the watch name
function TFrm_Trace.CheckWatch(rootVtNode: PVirtualNode; WatchName: string) : PVirtualNode;
var
   ChildVtNode: PVirtualNode;
   TreeRec: PTreeRec;
begin
   Result := nil;
   ChildVtNode := rootVtNode.FirstChild;
   while ChildVtNode <> nil do begin
      TreeRec := VstMain.GetNodeData(ChildVtNode);
      if TreeRec.Columns[COL_WATCH_NAME] = WatchName then begin
         Result := ChildVtNode;
         exit;
      end;
      ChildVtNode := ChildVtNode.NextSibling;
   end;
end;

//------------------------------------------------------------------------------

procedure TFrm_Trace.InsertRow;
var
   selectedNode: PVirtualNode;
   selectedTreeRec: PTreeRec;
   newTreeNode: PVirtualNode;
   newTreeRec: PTreeRec;
   newOrder : integer ;
begin

   selectedNode := VstMain.GetFirstSelected;
   selectedTreeRec := nil;

   if selectedNode <> nil then begin
      selectedTreeRec := VstMain.GetNodeData(selectedNode);
      Frm_AddLine.EditTime.Text := selectedTreeRec.Time;
      Frm_AddLine.EditThId.Text := selectedTreeRec.ThreadID;
   end;

   Frm_AddLine.SetTraceWinMode;
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
         newOrder := selectedTreeRec.FirstChildOrder ;
         dec (selectedTreeRec.FirstChildOrder) ;
      end;

   end else if (Frm_AddLine.InsertWhere.ItemIndex = 2) then begin  // After selected line
      if selectedNode = nil then begin
         newTreeNode := VstMain.AddChild(nil);
         NewOrder := LastChildOrder ;
         inc (LastChildOrder) ;

      end else begin
         newTreeNode := VstMain.InsertNode(selectedNode,amInsertAfter);
         newOrder := selectedTreeRec.LastChildOrder ;
         inc (selectedTreeRec.LastChildOrder) ;

      end;

   end else begin                                                 // 3: At the end
      newTreeNode := VstMain.AddChild(nil);
      NewOrder := LastChildOrder ;
      inc (LastChildOrder) ;
   end;

   VstMain.ReinitNode(newTreeNode, false);
   //treeNode.Align := (VstMain.DefaultNodeHeight div 2) - 2;
   newTreeRec := VstMain.GetNodeData(newTreeNode);
   newTreeRec.TraceID     := '';
   newTreeRec.LeftMsg     := Frm_AddLine.EditTrace.Text;
   newTreeRec.RightMsg    := Frm_AddLine.EditComment.Text;
   newTreeRec.ThreadID    := Frm_AddLine.EditThId.Text;
   newTreeRec.Time        := Frm_AddLine.EditTime.Text;
   newTreeRec.IP          := '';
   newTreeRec.ProcessName := '';
   newTreeRec.TreeIcon    := -1;
   newTreeRec.Members := TMember.Create;
   newTreeRec.OriginalOrder := NewOrder;
   newTreeRec.LastChildOrder := 0;
end;

// ------------------------------------------------------------------------------

// used by ParseTraceMsg to determine if a node exist in the tree.
// Node is searched using his ID (ususaly a guid)
// 2 levels of cache are used for fast search
function TFrm_Trace.CheckNode(rootVtNode: PVirtualNode;  NodeToCheck: AnsiString; IsNew: boolean): PVirtualNode;
var
   TreeRec: PTreeRec;

   // -------------------------------------------------
   procedure CheckLostAndFound;
   begin
      if LostAndFound <> nil then
         exit;
      // create the node and ensure node is initialized.
      // (Needed when the node is freed to call onFreeNode)
      LostAndFound := VstMain.AddChild(nil);
      VstMain.ReinitNode(LostAndFound, false);

      // fill the record
      TreeRec := VstMain.GetNodeData(LostAndFound);
      TreeRec.ThreadID := '';
      TreeRec.IP := '';
      TreeRec.Time := '';
      TreeRec.ProcessName := '';
      TreeRec.Members := TMember.Create;
      TreeRec.TreeIcon := -1;
      TreeRec.OriginalOrder := LastChildOrder;
      TreeRec.LastChildOrder := 0;
      TreeRec.LeftMsg  := TraceConfig.Framework_Orphans_LostAndFoundLeftText;
      TreeRec.RightMsg := TraceConfig.Framework_Orphans_LostAndFoundRightText;
   end;

   // -------------------------------------------------

   function CreateDefaultNode(Parent: PVirtualNode): PVirtualNode;
   begin

      Result := VstMain.AddChild(Parent);
      VstMain.ReinitNode(Result, false);

      // fill the record
      TreeRec := VstMain.GetNodeData(Result);
      TreeRec.TraceID := NodeToCheck;
      TreeRec.ThreadID := '';
      TreeRec.IP := '';
      TreeRec.Time := '';
      TreeRec.ProcessName := '';
      TreeRec.Members := TMember.Create;
      TreeRec.TreeIcon := -1;
      TreeRec.OriginalOrder := LastChildOrder;
      TreeRec.LastChildOrder := 0;
      TreeRec.LeftMsg  := TraceConfig.Framework_Orphans_DefaultLeftText;
      TreeRec.RightMsg := TraceConfig.Framework_Orphans_DefaultRightText;
   end;

   // -------------------------------------------------

   function internal_checkNode(ActualVtNode: PVirtualNode): PVirtualNode;
   var
      ChildVtNode: PVirtualNode;
      FoundNode: PVirtualNode;
   begin
      if ActualVtNode <> VstMain.RootNode then begin
         TreeRec := VstMain.GetNodeData(ActualVtNode);
         if TreeRec.TraceID = NodeToCheck then begin
            Result := ActualVtNode;
            exit;
         end;
      end;
      ChildVtNode := ActualVtNode.FirstChild;
      while ChildVtNode <> nil do begin
         FoundNode := internal_checkNode(ChildVtNode);
         if FoundNode <> nil then begin
            Result := FoundNode;
            exit;
         end;
         ChildVtNode := ChildVtNode.NextSibling;
      end;
      Result := nil;
   end;

   // -------------------------------------------------

begin
   if NodeToCheck = '' then begin
      Result := nil;
      exit;
   end;

   // node with the same id as the form are displayed in root.
   if NodeToCheck = ID then begin
      Result := nil;
      exit;
   end;

   // first : search inside the MostUsedList
   Result := MostUsedList.SearchNode(VstMain, NodeToCheck, true);
   // true : increment child count
   if Result <> nil then begin
      exit;
   end;

   // second : search in last added nodes
   Result := LastUsedList.SearchNode(VstMain, NodeToCheck, false);
   // false : don't increment child count
   if Result <> nil then begin
      // the node exist in the last used cache list
      // node don't exist in MostUsedList.
      // check if we can add the node in that list
      MostUsedList.AddToList(Result);
      exit;
   end;

   // recursive search in the tree (slow)
   Result := internal_checkNode(rootVtNode);

   if Result = nil then begin

      if TraceConfig.Framework_Orphans_DeletedNode = 'CreateOnRoot' then begin
         // create parents on root
         Result := CreateDefaultNode(nil);
      end else if TraceConfig.Framework_Orphans_DeletedNode = 'CreateUnderLostAndFound' then begin
         // create parents under a main LostAndFound node
         CheckLostAndFound();
         // create the parent under LostAndFound.
         Result := CreateDefaultNode(LostAndFound);
      end else if TraceConfig.Framework_Orphans_DeletedNode = 'AddChildrenOnRoot' then begin
         Result := nil;
      end else if TraceConfig.Framework_Orphans_DeletedNode = 'AddChildrenUnderLostAndFound' then begin
         if IsNew then
            Result := CreateDefaultNode(LostAndFound);
         // else : CST_USE_NODE -> Ignore updates. result stay to nil
      end;
   end;

   // if node exist, add to MostUsedList
   if Result <> nil then
      MostUsedList.AddToList(Result);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.vstMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
   TreeRec: PTreeRec;
   c: Integer;
begin

   CellText := '';
   if Column < 0 then
      exit;

   TreeRec := Sender.GetNodeData(Node);
   if TreeRec = nil then
      exit;

   if IsMultiColTree then begin   // then IsWatch

      if TreeRec.Columns = nil then
         exit;
      // number of substring can be less than the number of column
      if Column < TreeRec.Columns.Count then begin
         CellText := TreeRec.Columns[Column];
         // if last col and remaining strings...
         if (Column = VstMain.Header.Columns.Count - 1) then begin
            for c := Column + 1 to TreeRec.Columns.Count - 1 do
               CellText := CellText + ' ' + #9 + TreeRec.Columns[c];
         end;
         // to do : check underline / TextType
      end;
      if (TextType = ttNormal) and (IsSeparator(CellText)) then
         CellText := ' '

   end else begin
      case Column of
         COL_LEVELANDTYPE: begin // image, no text
               if TextType = ttStatic then begin
               // ttStatic is used to get the real text
                  if TreeRec.TreeIcon = -1 then
                     CellText := '24 : Debug/Info'
                  else if TreeRec.TreeIcon = 24 then
                     CellText := '24 : Debug/Info'
                  else if TreeRec.TreeIcon = 22 then
                     CellText := '22 : Warning'
                  else if TreeRec.TreeIcon = 23 then
                     CellText := '23 : Error'
                  else
                     CellText := inttostr(TreeRec.TreeIcon);
               end;
            end;

         COL_TIME: begin
               CellText := TreeRec.Time;
               // LongTimeFormat := 'hh:mm:ss:zzz' ;
            end;

         COL_THID: CellText := TreeRec.ThreadID;

         COL_TRACE: begin
               if TreeRec.LeftMsg = '' then
                  CellText := ' '
               else if (TextType = ttNormal) and
                  (IsSeparator(TreeRec.LeftMsg)) then
                  CellText := ' ' // check underline / TextType
               else
                  CellText := TreeRec.LeftMsg;
            end;

         COL_COMMENT: begin
               if (TextType = ttNormal) and (IsSeparator(TreeRec.RightMsg)) then
                  CellText := ' ' // check underline / TextType
               else
                  CellText := TreeRec.RightMsg;
            end;

         998: // used only by the filter : return the info
            begin
            end;
      end;
   end;

   if toEditable in VstMain.TreeOptions.MiscOptions then
      exit;

   if Length(CellText) > 400 then
      CellText := Copy(CellText, 1, 400) + '...'
end;

// ------------------------------------------------------------------------------

// called only if we change HintMode to hmHint.
// for now the HintMode is set to hmTooltip, that mean we display tooltip only if column text is too short.

procedure TFrm_Trace.vstMainGetHint(Sender: TBaseVirtualTree;
   Node: PVirtualNode; Column: TColumnIndex;
   var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
begin
   VstMainGetText(Sender, Node, Column, ttNormal, HintText);
   // LineBreakStyle := hlbForceMultiLine ;
end;

//------------------------------------------------------------------------------

procedure TFrm_Trace.vstMainGetImageIndex(Sender: TBaseVirtualTree;
   Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
   var Ghosted: boolean; var ImageIndex: System.UITypes.TImageIndex);
var
   TreeRec: PTreeRec;
   c: Integer;
   member: TMember;
begin
   ImageIndex := -1;

   //TVTImageKind = (
   //   ikNormal,
   //   ikSelected,
   //   ikState,
   //   ikOverlay
   //);
   if IsMultiColTree then   // then IsWatch too
      exit;

   if (Kind = ikOverlay) or (Kind = ikState) then
      exit; // Return a defined overlay here

   // Normal and selected

   if (Column = COL_TRACE) and ( not TraceConfig.Framework_KindIconOnLeft ) then begin
      TreeRec := Sender.GetNodeData(Node);
      if TreeRec.Members <> nil then begin
         // get first SubMembers with special Viewer and display it
         for c := 0 to TreeRec.Members.SubMembers.Count - 1 do begin
            member := TMember(TreeRec.Members.SubMembers[c]);
            case member.ViewerKind of
               CST_VIEWER_DUMP: begin
                     ImageIndex := 31;
                     break;
                  end; // dump viewer
               CST_VIEWER_XML: begin
                     ImageIndex := 32;
                     break;
                  end; // xml viewer
               CST_VIEWER_TABLE: begin
                     ImageIndex := 33;
                     break;
                  end; // table viewer
               CST_VIEWER_STACK: begin
                     ImageIndex := 34;
                     break;
                  end; // stack
               CST_VIEWER_BITMAP: begin
                     ImageIndex := 35;
                     break;
                  end; // bitmap viewer
               CST_VIEWER_OBJECT: begin
                     ImageIndex := 36;
                     break;
                  end; // object structure
               CST_VIEWER_VALUE: begin
                     ImageIndex := 37;
                     break;
                  end; // object value
               CST_VIEWER_ENTER: begin
                     ImageIndex := 38;
                     break;
                  end; // enter method
               CST_VIEWER_EXIT: begin
                     ImageIndex := 39;
                     break;
                  end; // exit method
               CST_VIEWER_TXT: begin
                     ImageIndex := 40;
                     break;
                  end; // text added to default viewer
            else
               begin ImageIndex := 15;
                  break;
               end; // CST_VIEWER_NONE : draw the small dot indicate sub members
            end;
         end;
      end;
   end;

   // Col0 icons (level and type) are draw in the AfterCellPaint event
   if Column = COL_LEVELANDTYPE then begin
      // TreeRec := Sender.GetNodeData(Node) ;
      //
      // if TreeRec.TreeIcon = -1 then
      // ImageIndex := 24
      // else
      // ImageIndex := TreeRec.TreeIcon ;
   end;
end;

// ------------------------------------------------------------------------------
// since TTreeRec is a record, no destructor can remove properties and strings
procedure TFrm_Trace.vstMainFreeNode(Sender: TBaseVirtualTree;
   Node: PVirtualNode);
var
   TreeRec: PTreeRec;
   c: Integer;
begin
   // lot of exception catching here : another difficult bug to find.

   try
      // delete from bookmark list
      if bookmarks <> nil then begin
         c := bookmarks.IndexOf(Node);
         if c <> -1 then
            bookmarks.Delete(c);
      end;
   except
      on e: exception do begin
         LowTrace ('VstMainFreeNode exception when deleting from bookmark : ' + e.message);
         if (FrmInternalTraces <> nil) and (FrmInternalTraces <> self) then
            InternalTrace('VstMainFreeNode exception when deleting from bookmark', e.message);
      end;
   end;

   try
      if NodeToFocus = Node then
         NodeToFocus := nil;
   except
      on e: exception do begin
         LowTrace ('VstMainFreeNode exception when reseting NodeToFocus : ' + e.message);
         if (FrmInternalTraces <> nil) and (FrmInternalTraces <> self) then
            InternalTrace('VstMainFreeNode exception when reseting NodeToFocus', e.message);
      end;
   end;

   try
      if LostAndFound = Node then
         LostAndFound := nil;
   except
      on e: exception do begin
         LowTrace('VstMainFreeNode exception when reseting LostAndFound : ' +e.message);
         if (FrmInternalTraces <> nil) and (FrmInternalTraces <> self) then
            InternalTrace('VstMainFreeNode exception when reseting LostAndFound',e.message);
      end;
   end;

   try
      // ensure the node is deleted from cache
      if MostUsedList <> nil then
         MostUsedList.RemoveFromList(Node);
      // remove the node and his children
   except
      on e: exception do begin
         LowTrace('VstMainFreeNode exception when deleting from MostUsedList or LastUsedList : ' + e.message);
         if (FrmInternalTraces <> nil) and (FrmInternalTraces <> self) then
            InternalTrace('VstMainFreeNode exception when deleting from MostUsedList',e.message);
      end;
   end;

   try
      // ensure the node is deleted from cache
      if LastUsedList <> nil then
         LastUsedList.RemoveFromList(Node);
      // remove the node and his children
   except
      on e: exception do begin
         LowTrace('VstMainFreeNode exception when deleting from MostUsedList or LastUsedList : ' + e.message);
         if (FrmInternalTraces <> nil) and (FrmInternalTraces <> self) then
            InternalTrace('VstMainFreeNode exception when deleting from LastUsedList',e.message);
      end;
   end;

   try
      TreeRec := Sender.GetNodeData(Node);
      if TreeRec.Members <> nil then
         TreeRec.Members.Free; // auto free also sub members

      if TreeRec.Columns <> nil then
         TreeRec.Columns.Free; // auto clear strings
      TreeRec.LeftMsg := ''; // Left col
      TreeRec.RightMsg := ''; // right col
      TreeRec.TraceID := ''; // the reference of the node : it's a guid
      TreeRec.Time := ''; // time of send
      TreeRec.ThreadID := ''; // thread id of the sender
      TreeRec.ProcessName := ''; // optional : the name of the process that send traces

      for c := 0 to length(TreeRec.FontDetails) - 1 do
         TreeRec.FontDetails[c].Free;
      setlength(TreeRec.FontDetails, 0);

   except
      on e: exception do begin
         LowTrace('VstMainFreeNode exception when clearing TreeRec : ' +e.message);
         if (FrmInternalTraces <> nil) and (FrmInternalTraces <> self) then
            InternalTrace('VstMainFreeNode exception when clearing TreeRec',e.message);
      end;
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.vstMainHeaderDragged(Sender: TVTHeader;  Column: TColumnIndex; OldPosition: Integer);
var
   c: Integer;
begin
   VstMainChange(VstMain, nil);
   if IsMultiColTree then    // then isWatch too
      VstMain.Header.MainColumn := VstMain.Header.Columns.GetFirstVisibleColumn
   else begin
      for c := 0 to 4 do
         VstMain.Header.Columns[c].MinWidth := 10;
      VstMain.Header.Columns[VstMain.Header.Columns.GetLastVisibleColumn].MinWidth := 3000;
   end;
   AutosizeAll(VstMain);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.AddOneLineDetail(Col1, Col2, Col3: String; isXml: boolean=false; isJson: boolean=false);
var
   DetailNode: PVirtualNode;
   DetailRec: PDetailRec;
begin
   if (Col1 = '') and (Col2 = '') and (Col3 = '') then // discard blank lines
      exit;
   DetailNode := VstDetail.AddChild(nil);
   // ensure node is initialized. Needed when the node is free to call onFreeNode
   VstDetail.ReinitNode(DetailNode, false);
   DetailNode.Align := (VstDetail.DefaultNodeHeight div 2) - 2;
   DetailRec := VstDetail.GetNodeData(DetailNode);
   DetailRec.Col1 := Col1;
   DetailRec.Col2 := Col2;
   DetailRec.Col3 := Col3;
   setlength(DetailRec.FontDetails, 1);
   DetailRec.FontDetails[0] := BoldDetail;
   VstDetail.MultiLine[DetailNode] := true;

   if (isXml or isJson) then
      Tframe_Classic(TreeDetailFrame).frameMemo.SetMemoText(col1,isXml,isJson);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.ResetDump(Member: TMember);
var
   c: Integer;
   SubMember: TMember;
begin
   for c := 0 to Member.SubMembers.Count - 1 do begin
      SubMember := TMember(Member.SubMembers.Items[c]);
      SubMember.Col3 := '';
   end;
end;

// ------------------------------------------------------------------------------

// Since 12.4 : When viewer kind is dump, parse col 2 and generate col 3
procedure TFrm_Trace.CalculateDump(Member : TMember);
var
   c : integer ;
   SubMember : TMember ;
   SrcPos      :Integer;
   SrcAsc      :Integer;
   OneChar : AnsiChar ;
begin
   for c := 0 to Member.SubMembers.Count -1 do begin
      SubMember := TMember (Member.SubMembers.Items[c]) ;

      //Frm_Trace.InternalTrace ('SubMember.Col2 : ' + SubMember.Col2 ) ;
      //Frm_Trace.InternalTrace ('SubMember.Col3 : ' + SubMember.Col3 ) ;

      // B0 B1 B2 B3 B4 B5 B6 B7 B8 B9 BA BB BC BD BE BF

      SrcPos:=1;
      SubMember.Col3 := '' ;
      repeat
         SrcAsc := StrToIntDef('$'+ copy(SubMember.Col2,SrcPos,2),0);
         OneChar := AnsiChar( chr(SrcAsc)) ;

         if (SrcAsc <> 0) and (SrcAsc <> 9) and (SrcAsc <> 10) and (SrcAsc <> 13)  then //     AnsiCharTypes[OneChar] and (C1_ALPHA or C1_PUNCT or C1_BLANK or C1_XDIGIT or C1_DIGIT) <> 0 then
            SubMember.Col3 := SubMember.Col3 + string(OneChar)
         else
            SubMember.Col3 := SubMember.Col3 + '.' ;

         SrcPos:=SrcPos + 3;
      until SrcPos >= Length(SubMember.Col2);
   end ;
end;

// ------------------------------------------------------------------------------

// Onchange event : if the right panel is visible, display members
procedure TFrm_Trace.vstMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   TreeRec: PTreeRec;
   FirstSelect: PVirtualNode;
   SecondSelect: PVirtualNode;
   c: Integer;
   col: TVirtualTreeColumn;
   ColIdx: TColumnIndex;
   NoTitle: boolean;
   coltitle: string;
   SubMember: TMember;

   // show viewers if flags are enabled, hide if not
   procedure ShowViewers();
   var
      c: Integer;
      viewer: Tframe_BaseDetails;
   begin

      // if one or more viewers visible display a smaller VSTDetail
      if CurrentViewers.Count > 0 then begin
         VstDetail.Align := alTop;
         VstDetail.top := 0;
         VstDetail.Height := 200;
      end else begin // else no viewer. use full space for VSTDetail
         VstDetail.Align := alClient;
      end;

      for c := 0 to CurrentViewers.Count - 1 do begin
         viewer := Tframe_BaseDetails(CurrentViewers.Items[c]);

         viewer.Splitter.Align := alTop;
         viewer.Splitter.top := 10000; // set as last viewer
         viewer.Splitter.Visible := true;
         // last viewer use client space
         if c = CurrentViewers.Count - 1 then begin
            viewer.Align := alClient;
         end else begin
            // small viewer
            viewer.Align := alTop;
            viewer.Height := 200;
            viewer.top := 10000;
         end;
         viewer.Visible := true;
      end;
   end;

begin
   SetCursor(Screen.Cursors[crHourGlass]);
   Tframe_Classic(TreeDetailFrame).frameMemo.SetMemoText('',false,false);
   Tframe_Classic(TreeDetailFrame).VstDetailSelector.ResetSelection;

   try
      // scroll into view
      if Node <> nil then
         VstMain.ScrollIntoView(Node, false, false);
      // center and horizontally false

      if PanelRight.Visible = false then
         exit;

      // viewers are not visible by default
      for c := 0 to CurrentViewers.Count - 1 do begin
         Tframe_BaseDetails(CurrentViewers[c]).Visible := false;
         Tframe_BaseDetails(CurrentViewers[c]).Splitter.Visible := false;
      end;
      CurrentViewers.Clear; // not Owner.
      VstDetail.Clear;

      // get first then second. If second is not nil then it's multiselect : disable info panel
      FirstSelect := VstMain.GetNextSelected(nil);
      if FirstSelect = nil then begin
         ShowViewers(); // ensure that vstdetail will take the full space
         exit;
      end;

      SecondSelect := VstMain.GetNextSelected(FirstSelect);
      if SecondSelect <> nil then begin
         AddOneLineDetail(inttostr(VstMain.SelectedCount) + ' lines selected', '', '');
         VstDetail.TreeOptions.PaintOptions := VstDetail.TreeOptions.PaintOptions - [toShowRoot];
         ShowViewers();
         exit;
      end;

      TreeRec := VstMain.GetNodeData(FirstSelect); // node

      // fill detail detail tree

      if FirstSelect.ChildCount <> 0 then
         AddOneLineDetail('Sub nodes count', inttostr(FirstSelect.ChildCount) , '');

      if IsMultiColTree then begin  // the isWatch too
         // check if all titles are empty
         NoTitle := true;
         ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
         while ColIdx <> InvalidColumn do begin
            col := VstMain.Header.Columns[ColIdx];
            if col.Text <> '' then begin
               NoTitle := false;
               break;
            end;
            ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
         end;

         ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
         while ColIdx <> InvalidColumn do begin
            col := VstMain.Header.Columns[ColIdx];

            if NoTitle then
               coltitle := 'Col ' + inttostr(ColIdx + 1)
            else
               coltitle := col.Text;

            // TreeRec can contain less column than the tree
            if ColIdx < TreeRec.Columns.Count then
               AddOneLineDetail(coltitle, TreeRec.Columns[ColIdx], '')
            else
               AddOneLineDetail(coltitle, '', '');
            ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
         end;

         // if more TreeRec.Columns than VstMain.Header.Columns then add lines
         for c := VstMain.Header.Columns.Count to TreeRec.Columns.Count - 1
            do begin
            AddOneLineDetail('', TreeRec.Columns[c], '');
         end;

      end  else begin
         if TreeRec.ProcessName <> '' then
            AddOneLineDetail('Process Name', TreeRec.ProcessName, '');

         if TreeRec.Time <> '' then
            AddOneLineDetail('Time', TreeRec.Time, '');

         if TreeRec.ThreadID <> '' then
            AddOneLineDetail('Thread', TreeRec.ThreadID, '');

         if TreeRec.IP <> '' then
            AddOneLineDetail('IP', TreeRec.IP, '');

         AddOneLineDetail('Traces', TreeRec.LeftMsg, '');
         // AddTwoLinesDetail
         if TreeRec.RightMsg <> '' then
            AddOneLineDetail('Comment', TreeRec.RightMsg, '');
         // AddTwoLinesDetail
      end;

      // fill Member tree

      VstDetailHaschildren := false;
      if TreeRec.Members <> nil then begin
         // TreeRec.Members root is not used. Process sub members.
         for c := 0 to TreeRec.Members.SubMembers.Count - 1 do begin
            SubMember := TMember(TreeRec.Members.SubMembers.Items[c]);

            // each member can have a specific viewer.
            case SubMember.ViewerKind of
               CST_VIEWER_XML: // xml viewer
                  begin
                     //if XmlFrame = nil then begin
                     //   XmlFrame := TFrame_XML.Create(self);
                     //   // owner : self -> released by form
                     //   XmlFrame.Parent := PanelRight;
                     //   XmlFrame.Align := alClient;
                     //end;
                     //TFrame_XML(XmlFrame).AddDetails(TreeRec, SubMember);

                     Tframe_Classic(TreeDetailFrame).AddDetails(TreeRec, SubMember);
                     if ( SubMember.col1.StartsWith('<')) then
                        Tframe_Classic(TreeDetailFrame).frameMemo.SetMemoText(SubMember.col1,true,false)
                     else if ( SubMember.col1.StartsWith('{')) then
                        Tframe_Classic(TreeDetailFrame).frameMemo.SetMemoText(SubMember.col1,false,true)
                     else
                        Tframe_Classic(TreeDetailFrame).frameMemo.SetMemoText(SubMember.col1,false,false);

                     // add detail to frame and add frame to CurrentViewers
                  end;
               CST_VIEWER_BITMAP: // bitmap viewer . if many bitmap, only the last will be displayed
                  begin
                     if BitmapFrame = nil then begin
                        BitmapFrame := Tframe_BitmapDetails.Create(self);
                        // owner : self -> released by form
                        BitmapFrame.Parent := PanelRight;
                        BitmapFrame.Align := alClient;
                     end;
                     Tframe_BitmapDetails(BitmapFrame).AddDetails(TreeRec, SubMember); // add detail to frame and add frame to CurrentViewers
                  end;

               CST_VIEWER_TABLE:
                  begin
                     if TableFrame = nil then begin
                        TableFrame := Tframe_table.Create(self);
                        // owner : self -> released by form
                        TableFrame.Parent := PanelRight;
                        TableFrame.Align := alClient;
                     end;
                     Tframe_table(TableFrame).AddDetails (TreeRec, SubMember); // add detail to frame and add frame to CurrentViewers
                  end;

               // CST_VIEWER_NONE  :   // default viewer, no icon
               CST_VIEWER_DUMP  :   // dump viewer
                  begin
                     // since version 12.4 : reset col3 for all submembers if viewer kind is Dump
                     CalculateDump(SubMember) ;
                     Tframe_Classic(TreeDetailFrame).AddDetails(TreeRec, SubMember);
                  end ;

               // CST_VIEWER_STACK :   // stack
               // CST_OBJECT       :   // object structure
               // CST_VALUE        :   // object value
               // CST_VIEWER_ENTER :   // enter method
               // CST_VIEWER_EXIT  :   // exit method
               // CST_VIEWER_TXT   :   // text added to default viewer
            else
               Tframe_Classic(TreeDetailFrame).AddDetails(TreeRec, SubMember);
            end; // end case
         end; // next member
      end;

      if Tframe_Classic(TreeDetailFrame).frameMemo.SynMemo.Text = '' then
         Tframe_Classic(TreeDetailFrame).frameMemo.SetMemoText(TreeRec.LeftMsg,false,false);

      if VstDetailHaschildren then // if a node has chidren, show the root
         VstDetail.TreeOptions.PaintOptions := VstDetail.TreeOptions.PaintOptions + [toShowRoot]
      else // no children : remove the root
         VstDetail.TreeOptions.PaintOptions := VstDetail.TreeOptions.PaintOptions - [toShowRoot];

      ShowViewers();

      VstDetail.FullExpand();
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end;
end;

// ------------------------------------------------------------------------------

// Detect the double click.
// To not allow editing on simple click, the VstMain.TreeOptions.MiscOptions toEditable flag is not set.
// When the user double click the node, the flag is set

procedure TFrm_Trace.vstMainDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode: PVirtualNode;
   Dummy: Integer;
begin
   // InternalTrace ('DetailDblClick ') ;
   SelectedNode := VstMain.GetFirstSelected;

   // no node selected
   if SelectedNode = nil then
      exit;

   GetCursorPos(P);
   P := VstMain.ScreenToClient(P);
   MouseNode := VstMain.GetNodeAt(P.X, P.Y, true, Dummy);

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit;

   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions +  [toEditable];

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(self.Handle, WM_STARTEDITING_TRACE,
      NativeUInt(SelectedNode) and $FFFFFFFF,
      NativeUInt(SelectedNode) shr 32);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.vstMainKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: boolean);
begin
   if CharCode = VK_DELETE then
      DeleteSelected() ;
end;

// ------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrm_Trace.vstMainEdited(Sender: TBaseVirtualTree;
   Node: PVirtualNode; Column: TColumnIndex);
begin
   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions -
      [toEditable];
end;

procedure TFrm_Trace.vstMainEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   inherited;
   Allowed := true;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrm_Trace.vstMainEditCancelled(Sender: TBaseVirtualTree;  Column: TColumnIndex);
begin
   VstMain.TreeOptions.MiscOptions := VstMain.TreeOptions.MiscOptions -
      [toEditable];
end;

// ------------------------------------------------------------------------------
procedure TFrm_Trace.vstMainCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor := TMoveMemoEditLink.Create();
      IVstEditor := VstEditor;
   end;
   EditLink := IVstEditor;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := PVirtualNode(Pointer(
      (NativeUInt(Message.WParam) and $FFFFFFFF) or
      (NativeUInt(Message.LParam) shl 32)));
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.WMStartEditingTrace(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := PVirtualNode(Pointer(
      (NativeUInt(Message.WParam) and $FFFFFFFF) or
      (NativeUInt(Message.LParam) shl 32)));
   if Assigned(Node) then
      VstMain.EditNode(Node, VstMain.FocusedColumn);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.butCloseClick(Sender: TObject);
begin
   // close main trace
   if self = Frm_Trace then
      TraceConfig.Framework_Enabled := false;
   // close main watches
   if self = Frm_Watches then
      TraceConfig.Watches_Enabled := false;
   CloseWin();
end;


// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// TFrmBase
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

procedure TFrm_Trace.CloseWin;
var
   PageControl: TPageControl;
begin
   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_CLOSE_WIN, '') = false then
      exit;

   if (self = Frm_Trace) or (self = FrmInternalTraces) or
      (self = Frm_Watches) then begin
      PageControl := getPageControl();
      Visible := false;
      IsDestroying := true;
      UnDock;
      IsDestroying := false;
      // force redesign toolbar and menu for the new active page
      if PageControl <> nil then
         PageControl.OnChange(nil);
   end
   else begin
      XMLLogFile := nil;
      // LinkedPlugins.Free ;                           // free linked plugins objects
      self.close; // close the form
   end;
end;

// ------------------------------------------------------------------------------
// here we can modify the "actions" menu items
procedure TFrm_Trace.PageControlChange();
var
   PageContainer: TFrmPageContainer;
begin
   PageContainer := getPageContainer();
   if PageContainer = nil then begin
      TFrm_Trace.InternalTrace('self:' + inttostr(Integer(self)) + '.' + 'PageContainer = nil');
      exit;
   end;
   PageContainer.actClear.Enabled := true;
   PageContainer.actSaveToFile.Enabled := true;
   PageContainer.actViewTraceInfo.Enabled := true;

   PageContainer.actPrint.Enabled := true;
   PageContainer.actFocus.Enabled := true;
   PageContainer.actPause.Enabled := not IsWatch; // pause is possible for trace windows. Not possible for watch windows
   PageContainer.actCopy.Enabled := true;
   PageContainer.actDelete.Enabled := true;
   PageContainer.actCut.Enabled := true;
   PageContainer.actSelectAll.Enabled := true;
   PageContainer.actViewProperty.Enabled := true;
   PageContainer.actSearch.Enabled := true;
   PageContainer.actFindNext.Enabled := true;

   // tbnMembers.down := VstProperties.Visible ;
   PageContainer.actViewTraceInfo.Checked := PanelRight.Visible;
   PageContainer.actFocus.Checked := TraceConfig.AppDisplay_FocusToReceivedMessage;
   PageContainer.actPause.Checked := self.IsPaused;

   // enable plugin item menu
   DrawActionMenu(true);
end;

// ------------------------------------------------------------------------------

// procedure TFrm_Trace.PageControlChanging();
// var
// selfStr : string ;
// begin
// selfStr := 'self:' + inttostr (integer(self))+'.' ;
//
// // disable plugin item menu
// TFrm_Trace.InternalTrace (selfStr+'TFrm_Trace.PageControlChanging') ;
// DrawActionMenu (false);
// end ;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.ResizeColumns;
begin
   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_RESIZE_COLS, '') = false then
      exit;
   AutosizeAll(VstMain);
end;

// ------------------------------------------------------------------------------

// check the number of node
// called by the main timer after processing all messages queues
// or after a change in the the option dialog
procedure TFrm_Trace.CheckAutoClear;
var
   NodeToDelete, NextNode: PVirtualNode;

   // check to delete node and children. Plugins can disable removing node or children
procedure RecurDelete(Node: PVirtualNode);
var
   child, nextChild: PVirtualNode;
begin
   child := Node.FirstChild;
   while child <> nil do begin
      nextChild := child.NextSibling;
      if child.ChildCount <> 0 then
         RecurDelete(child)
      else if askDelete(child) then begin
         if NodeToFocus = child then
            NodeToFocus := nil;
         VstMain.DeleteNode(child, false);
      end;
      child := nextChild;
   end;
   if askDelete(Node) then begin
      if NodeToFocus = Node then
         NodeToFocus := nil;
      if LostAndFound = Node then
         LostAndFound := nil;
      VstMain.DeleteNode(Node, false);
   end;
end;

begin
   if TraceConfig.Framework_AutoClear and
      (Integer(VstMain.RootNode.ChildCount)
         >= TraceConfig.Framework_MaxNode) then begin
      // is already called by the caller, except after a change in the the option dialog
      // the number of messages to track is the root element count
      VstMain.BeginUpdate();
      try
         if Sorter.SortColumns.Count <> 0 then
         // unsort before deleting old traces
            Sorter.Unsort(nil);

         if NeedBeforeDelete() then begin // check if a plugin attached to this window need the BeforeDelete event
            NodeToDelete := VstMain.RootNode.FirstChild;
            while (NodeToDelete <> nil) and
               (Integer(VstMain.RootNode.ChildCount)
                  > TraceConfig.Framework_MinNode) do begin
               NextNode := NodeToDelete.NextSibling;
               RecurDelete(NodeToDelete); // check to delete node and children. Plugins can disable removing node or children
               NodeToDelete := NextNode;
            end;
         end
         else begin
            // no plugin attached to the window
            while Integer(VstMain.RootNode.ChildCount)
               > TraceConfig.Framework_MinNode do begin
               NodeToDelete := VstMain.RootNode.FirstChild;
               VstMain.DeleteNode(NodeToDelete, false); // onFreeNode check NodeToFocus and delete from cache (MostUsedList,LastUsedList,...)
            end;
         end;

         if Sorter.SortColumns.Count <> 0 then // resort
            Sorter.Sort(nil);
      finally
         VstMain.EndUpdate();
      end;
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.ClearWin;
begin
   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_CLEAR_ALL, '') = false then
      exit;

   if NeedBeforeDelete() then begin // check if a plugin attached to this window need the BeforeDelete event
      SelectAll();
      DeleteSelected();
      VstMain.ClearSelection;
      if VstMain.GetFirst = nil then
         LastChildOrder := 1; // 0 is reserved for not yet ordered lines
   end
   else begin
      // first clear cache list to speed up VstMainFreeNode method
      MostUsedList.RemoveAll;
      LastUsedList.RemoveAll;
      VstMain.Clear;
      VstDetail.Clear;
      LastChildOrder := 1; // 0 is reserved for not yet ordered lines
   end;
end;

// ------------------------------------------------------------------------------
// CTRL A : Select all
procedure TFrm_Trace.SelectAll;
var
   c: Integer;
   viewer: Tframe_BaseDetails;
begin
   // normally when the IVstEditor is not nil, he is visible
   if ((VstMain.IsEditing) or VstDetail.IsEditing) and (IVstEditor <> nil)
   { and (TMoveMemoEditLink(IVstEditor).IsVisible) } then begin
      try
         VstEditor.SelectAll(); // TMoveMemoEditLink(IVstEditor).SelectAll() ;
      except
         on e: exception do begin
            InternalTrace('Error while selecting lines from the editor',
               e.Message);
            ShowMessage(e.Message);
         end;
      end;
      exit;
   end;

   if VstMain.Focused = true then begin
      if DoPlugAction(CST_ACTION_SELECT_ALL, '') = true then
      // ask first plugins if we can perform the action
         VstMain.SelectAll(true); // select all visible items (don't select filtered items)
   end
   else if VstDetail.Focused then begin
      VstDetail.SelectAll(false); // select all (visible or invisible)
   end
   else begin
      for c := 0 to CurrentViewers.Count - 1 do begin
         viewer := Tframe_BaseDetails(CurrentViewers.Items[c]);
         if viewer.HasFocus() then
            viewer.SelectAll();
      end;
   end;
end;

// ------------------------------------------------------------------------------
procedure TFrm_Trace.PauseWin;
begin
   // first tell to plugins that the button is pressed
   if getPageContainer().actPause.Checked then begin
      DoPlugAction(CST_ACTION_PAUSE, '');
   end
   else begin
      DoPlugAction(CST_ACTION_RESUME, '');
   end;
   self.IsPaused := getPageContainer().actPause.Checked;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.ViewTraceInfo;
begin
   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_VIEW_INFO, '') = false then
      exit;

   if (getPageContainer().actViewTraceInfo.Checked) then begin
      VSplitter.Visible := true;
      PanelRight.Visible := true;
      PanelRight.Left := VSplitter.Left + 10;
      VstMainChange(VstMain, nil);
   end
   else begin
      VSplitter.Visible := false;
      PanelRight.Visible := false;
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.LabelLogFileClick(Sender: TObject);
begin
   ViewProperty();
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.TracesInfoClick(Sender: TObject);
begin
   PluginAction(Sender);
end;

// ------------------------------------------------------------------------------
// change the LabelLogFile caption
// 0 : No log . 1 : No limit, 2: Daily
procedure TFrm_Trace.ShowLog;
var
   FileExt: string;
begin
   if trim(LogFileName) = '' then
      LogFileName := 'TraceLog.xml';

   FileExt := ExtractFileExt(LogFileName);

   if LogFileType = 0 then begin
      LabelLogFile.Caption := 'Log disabled';
   end
   else if LogFileType = 1 then begin
      LabelLogFile.Caption := LogFileName;
      // add CurrentFileNumber before extension
      if CurrentFileNumber <> 0 then
         LabelLogFile.Caption := copy(LogFileName, 1, length(LogFileName)
               - length(FileExt)) + '_' + inttostr(CurrentFileNumber)
            + FileExt;

   end
   else begin // 2 : daily
      FileExt := ExtractFileExt(LogFileName);
      LabelLogFile.Caption := copy(LogFileName, 1, length(LogFileName)
            - length(FileExt)) + FormatDateTime('YYYYMMDD', now);
      if CurrentFileNumber <> 0 then
         LabelLogFile.Caption := LogFileName + '_' + inttostr
            (CurrentFileNumber);

      LabelLogFile.Caption := LabelLogFile.Caption + FileExt;
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.ViewProperty;
begin
   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_VIEW_PROP, '') = false then
      exit;

   FrmTraceWinProp.editFilename.Text := LogFileName;
   // default is 'TraceLog.xml'
   // 0 : No log . 1 : No limit, 2: Daily
   if LogFileType = 0 then
      FrmTraceWinProp.rbLogIsDisabled.Checked := true;

   if LogFileType = 1 then
      FrmTraceWinProp.rbLogEnabled.Checked := true;

   if LogFileType = 2 then
      FrmTraceWinProp.rbDaily.Checked := true;

   FrmTraceWinProp.EditMaxLines.Text := inttostr(MaxLines);

   if FrmTraceWinProp.ShowModal = mrOk then begin
      LogFileName := FrmTraceWinProp.editFilename.Text;
      if trim(LogFileName) = '' then
         LogFileName := 'TraceLog.xml';

      if FrmTraceWinProp.rbLogIsDisabled.Checked then
         LogFileType := 0
      else if FrmTraceWinProp.rbLogEnabled.Checked then
         LogFileType := 1
      else // daily
         LogFileType := 2;

      MaxLines := StrToIntDef(FrmTraceWinProp.EditMaxLines.Text, -1);
      ShowLog();
      // to do : save to config file
   end;
end;

// ------------------------------------------------------------------------------
// timer : show stat info
procedure TFrm_Trace.TimerInfo;
begin

   TracesInfo.Caption := TimeToStr(LastModified) + ', not filtered lines : ' + inttostr(VstMain.RootNode.ChildCount);

   // + ', Cache1 : ' + inttostr(MostUsedList.Count)
   // + ', Cache2 : ' + inttostr(LastUsedList.Count) ;

   try
      if NodeToFocus <> nil then begin
         VstMain.ClearSelection();
         VstMain.Selected [NodeToFocus] := true ;
         VstMain.FocusedNode := NodeToFocus;
         VstMain.ScrollIntoView (NodeToFocus,false,false);
      end;
   except
      on e: exception do
         InternalTrace('TFrm_Trace.TimerInfo exception : ' + e.Message);
   end;

   // check plugin timer
   DoPlugTimer;

   NodeToFocus := nil;
end;

// ------------------------------------------------------------------------------

// Delete key : deleted selected
procedure TFrm_Trace.DeleteSelected;
var
   SelectedNodes: TNodeArray;
   Node: PVirtualNode;
   i: Integer;
   LevelChange: boolean;
   TreeRec: PTreeRec;
begin
   SelectedNodes := nil;

   if VstMain.SelectedCount <= 0 then
      exit;

   if VstMain.Focused = false then
      exit;

   NodeToFocus := nil;
   Node := VstMain.GetFirstSelected;
   if Node = nil then
      exit;
   TreeRec := VstMain.GetNodeData(Node);

   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_DELETE, TreeRec.TraceID) = false then
      exit;

   Node := VstMain.GetPreviousVisible(Node);

   if NeedBeforeDelete() then begin // check if a plugin attached to this window need the BeforeDelete event
      VstMain.BeginUpdate;
      try
         SelectedNodes := VstMain.GetSortedSelection(false);
         for i := High(SelectedNodes) downto 1 do begin
            // if askDelete (SelectedNodes[I]) then begin  // also clear cache list
            TreeRec := VstMain.GetNodeData(SelectedNodes[i]);
            if DoPlugAction(CST_ACTION_DELETE, TreeRec.TraceID) = true then
               begin
               LevelChange := SelectedNodes[i].Parent <> SelectedNodes[i - 1]
                  .Parent;
               VstMain.DeleteNode(SelectedNodes[i], LevelChange);
            end;
            // end ;
         end;
         // if askDelete (SelectedNodes[0]) then   // also clear cache list
         VstMain.DeleteNode(SelectedNodes[0], false);
      finally
         VstMain.EndUpdate;
      end;
   end
   else begin
      // MostUsedList.RemoveFromSelected (VstMain);   // remove the node and his children
      // LastUsedList.RemoveFromSelected (VstMain);   // remove the node and his children
      VstMain.DeleteSelectedNodes; // onFreeNode check NodeToFocus and delete from cache
   end;

   // case of the first node : GetPreviousVisible is nil ...
   // case  of the last node : GetNextVisible is nil ...
   if Node = nil then
      Node := VstMain.GetFirst
   else if VstMain.GetNextVisible(Node) <> nil then
      Node := VstMain.GetNextVisible(Node);

   VstMain.FocusedNode := Node;
   VstMain.selected[Node] := true;
   if Node = nil then
      VstMainChange(nil, nil); // VstDetail.Clear ;
end;

// ------------------------------------------------------------------------------

// CTRL C : Copy selected. Return true if element is focused
function TFrm_Trace.CopySelected : boolean;
var
   CopyStrings: TStrings;
   NewLine: String;
   IsFirst: boolean;
   TreeRec: PTreeRec;
   TreeIndentation: String;
   focusedComponent: hwnd;
   col: TVirtualTreeColumn;
   NoTitle: boolean;
   SelectedNode: PVirtualNode;
   viewer: Tframe_BaseDetails;
   c: Integer;

procedure CheckIfNodeSelected(TestNode: PVirtualNode);
var
   ChildVtNode: PVirtualNode;
   LeftMsg: string;
   ColIdx: TColumnIndex;
   c: Integer;
begin
   if VstMain.selected[TestNode] then begin
      TreeRec := VstMain.GetNodeData(TestNode);
      IsFirst := true;
      NewLine := '';

      if IsMultiColTree then begin     // then IsWatch too

         // set indentation on first col  (no way to know the 'master col')
         NewLine := String(StrRepeat(TreeIndentation, VstMain.GetNodeLevel (TestNode)));
         ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
         while ColIdx <> InvalidColumn do begin
            if ColIdx < TreeRec.Columns.Count then begin
               // check if more header than data
               if IsFirst = false then
                  NewLine := NewLine + TraceConfig.TextExport_Separator;
               // Last column can contain CRLF
               NewLine := NewLine + TraceConfig.TextExport_TextQualifier + RemoveLastCRLF(TreeRec.Columns[ColIdx])+ TraceConfig.TextExport_TextQualifier;
               IsFirst := false;
            end;
            ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
         end;

         // if more data than header : append data
         ColIdx := VstMain.Header.Columns.GetLastVisibleColumn;
         for c := ColIdx + 1 to TreeRec.Columns.Count - 1 do
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + RemoveLastCRLF(TreeRec.Columns[c]) + TraceConfig.TextExport_TextQualifier;

      end else begin

         LeftMsg := TreeRec.LeftMsg;

         if TraceConfig.TextExport_ProcessName then begin
            NewLine := TraceConfig.TextExport_TextQualifier +
               TreeRec.ProcessName + TraceConfig.TextExport_TextQualifier;
            IsFirst := false;
         end;

         if TraceConfig.TextExport_ThreadId then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + TreeRec.ThreadID + TraceConfig.TextExport_TextQualifier;
            IsFirst := false;
         end;

         if TraceConfig.TextExport_Time then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + TreeRec.Time + TraceConfig.TextExport_TextQualifier;
            IsFirst := false;
         end;

         if TraceConfig.TextExport_Col1 then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator;

            NewLine := NewLine + String(StrRepeat(TreeIndentation, VstMain.GetNodeLevel(TestNode)));
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + LeftMsg + TraceConfig.TextExport_TextQualifier;
            IsFirst := false;
         end;

         if TraceConfig.TextExport_Col2 then begin
            if IsFirst = false then
               NewLine := NewLine + TraceConfig.TextExport_Separator;
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + TreeRec.RightMsg + TraceConfig.TextExport_TextQualifier;
         end;

      end;
      CopyStrings.Add(NewLine);
   end; // if selected
   ChildVtNode := TestNode.FirstChild;
   while ChildVtNode <> nil do begin
      CheckIfNodeSelected(ChildVtNode);
      ChildVtNode := ChildVtNode.NextSibling;
   end;
end;

procedure AddTitle;
var
   ColIdx: TColumnIndex;
begin
   if TraceConfig.TextExport_GenerateColumnHeader = false then
      exit;

   IsFirst := true;
   NewLine := '';

   if IsMultiColTree then begin    // then isWatch too
      // check if titles are all empty
      NoTitle := true;
      ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
      while ColIdx <> InvalidColumn do begin
         col := VstMain.Header.Columns[ColIdx];
         if col.Text <> '' then begin
            NoTitle := false;
            break;
         end;
         ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
      end;

      ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
      while ColIdx <> InvalidColumn do begin
         col := VstMain.Header.Columns[ColIdx];

         if IsFirst = false then
            NewLine := NewLine + TraceConfig.TextExport_Separator;
         IsFirst := false;

         // note that last column can contain CRLF
         if NoTitle then
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier +'Col ' + inttostr(ColIdx + 1)
               + TraceConfig.TextExport_TextQualifier
         else
            NewLine := NewLine + TraceConfig.TextExport_TextQualifier + RemoveLastCRLF(col.Text) + TraceConfig.TextExport_TextQualifier;
         ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
      end;

   end
   else begin
      if TraceConfig.TextExport_ProcessName then begin
         NewLine := TraceConfig.TextExport_TextQualifier + 'ProcessName' + TraceConfig.TextExport_TextQualifier;
         IsFirst := false;
      end;

      if TraceConfig.TextExport_ThreadId then begin
         if IsFirst = false then
            NewLine := NewLine + TraceConfig.TextExport_Separator;
         NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'ThreadID' + TraceConfig.TextExport_TextQualifier;
         IsFirst := false;
      end;

      if TraceConfig.TextExport_Time then begin
         if IsFirst = false then
            NewLine := NewLine + TraceConfig.TextExport_Separator;
         NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Time' + TraceConfig.TextExport_TextQualifier;
         IsFirst := false;
      end;

      if TraceConfig.TextExport_Col1 then begin
         if IsFirst = false then
            NewLine := NewLine + TraceConfig.TextExport_Separator;
         NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Msg' + TraceConfig.TextExport_TextQualifier;
         IsFirst := false;
      end;

      if TraceConfig.TextExport_Col2 then begin
         if IsFirst = false then
            NewLine := NewLine + TraceConfig.TextExport_Separator;
         NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Right Msg' + TraceConfig.TextExport_TextQualifier;
      end;
   end;
   CopyStrings.Add(NewLine);
end;

begin
   result := false;
   CopyStrings := TStringList.Create;
   TreeIndentation := StrRepeat(' ', TraceConfig.TextExport_TreeIndentation);
   SetCursor(Screen.Cursors[crHourGlass]);

//    if VstMain.Focused then
//       FrmInternalTraces.InternalTrace('TFrm_Trace.CopySelected VstMain ' + inttostr(integer(VstMain)) + ' Focused')
//    else
//       FrmInternalTraces.InternalTrace('TFrm_Trace.CopySelected VstMain ' + inttostr(integer(VstMain)) + ' not Focused');
//
//    if VstDetail.Focused then
//       FrmInternalTraces.InternalTrace('TFrm_Trace.CopySelected VstDetail ' + inttostr(integer(VstDetail)) + ' Focused')
//    else
//       FrmInternalTraces.InternalTrace('TFrm_Trace.CopySelected VstDetail ' + inttostr(integer(VstDetail)) + ' not Focused');

   try

      if VstMain.Focused then begin

         SelectedNode := VstMain.GetFirstSelected;
         if SelectedNode = nil then
            exit;
         TreeRec := VstMain.GetNodeData(SelectedNode);

         // ask first plugins if we can perform the action
         if DoPlugAction(CST_ACTION_COPY, TreeRec.TraceID) = false then
            exit;

         // add title if needed.
         AddTitle;

         // add node starting from the invisible root node (recursive)
         CheckIfNodeSelected(VstMain.RootNode);

         var CopyText: pchar := CopyStrings.GetText;
         try
            Clipboard.SetTextBuf(CopyText);
         finally
            StrDispose(CopyText);
         end;
         result := true;
      end else if VstDetail.Focused then begin
         // reroute CTRL-C to the focused component (TreeDetailFrame)
         Tframe_Classic(TreeDetailFrame).CopySelected();
         result := true;
      end else begin

         // reroute CTRL-C to the focused component
         // FrmInternalTraces.InternalTrace('TFrm_Trace.CopySelected loop ' + inttostr(CurrentViewers.Count) + ' viewers' );
         for c := 0 to CurrentViewers.Count - 1 do begin
            viewer := Tframe_BaseDetails(CurrentViewers.Items[c]);
            // FrmInternalTraces.InternalTrace('viewer [' + inttostr(c) + '] ' + viewer.ClassName + ' focused : ' + BoolToStr(viewer.HasFocus()));

            if viewer.HasFocus() then begin
               viewer.CopySelected();
               result := true;
               exit;
            end;
         end;

         // viewers don't have focus. ask the current component to copy to clipboard
         focusedComponent := GetFocus;
         if focusedComponent <> 0 then
            SendMessage(focusedComponent, WM_COPY, 0, 0);

      end;

   finally
      CopyStrings.Free;
      SetCursor(Screen.Cursors[crDefault]);
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.CopyCurrentCell;
var
   Node: PVirtualNode;
   CellText: string;
begin
   if VstMain.Focused then begin
      Node := VstMain.FocusedNode;
      if Node = nil then
         exit;
      VstMainGetText(VstMain, Node, VstMain.FocusedColumn, ttStatic,
         CellText); // ttStatic is used to get the real text
   end
   else if VstDetail.Focused then begin
      Node := VstDetail.FocusedNode;
      if Node = nil then
         exit;
      Tframe_Classic(TreeDetailFrame).VstDetailGetText (VstDetail, Node, VstDetail.FocusedColumn, ttNormal, CellText);
      // ttNormal
   end
   else begin
      exit;
   end;

   Clipboard.SetTextBuf(pWideChar(CellText));
end;

// ------------------------------------------------------------------------------

// Not called : it's a try to load xml faster using direct call to XML parser.
//
procedure TFrm_Trace.LoadXML_IE(filename: string);
var
   MSXML: DOMDocument;
   XMLRootData: IXMLDomNode;
   Progress: TFrmTailProgress;
   step: Integer;

procedure LoadTitles(NodeTag: IXMLDomNode);
var
   subnodecount: Integer;
   subXMLNode: IXMLDomNode;
   NodeList: IXMLDOMNodeList;
   c: Integer;
   treeCol: TVirtualTreeColumn;
begin
   NodeList := NodeTag.selectNodes('ColTitle');
   subnodecount := NodeList.Get_length;
   if subnodecount <> 0 then begin
      VstMain.Header.Columns.Clear;
      IsMultiColTree := true;
      for c := 0 to subnodecount - 1 do begin
         subXMLNode := IXMLDomNode(NodeList.item[c]);
         treeCol := VstMain.Header.Columns.Add;
         treeCol.options := treeCol.options + [coAllowFocus];
         // ensure user can focus to this column
         treeCol.Text := subXMLNode.Text;
         // Col.order
      end;

      subXMLNode := NodeTag.selectSingleNode('MainColumn');
      if subXMLNode <> nil then
         VstMain.Header.MainColumn := strToInt(subXMLNode.Text);
      AutosizeAll(VstMain);
   end;
end;

procedure loadNodeXML(ParentNode: PVirtualNode; NodeTag: IXMLDomNode);
var
   NodeList: IXMLDOMNodeList;
   c: Integer;
   subXMLNode: IXMLDomNode;
   subnodecount: Integer;
   FontDetail: TFontDetail;
   Node: PVirtualNode;
   TreeRec: PTreeRec;
   ParentFromBackup: PVirtualNode;
   str: string;
   NodeName: string;
begin

   NodeName := NodeTag.NodeName;
   if (NodeName = 'Node') then begin

      // in backup mode, 'parent' is stored as an attribute. (tree is converted to relations)
      subXMLNode := NodeTag.attributes.getNamedItem('Parent');
      if subXMLNode <> nil then begin
         ParentFromBackup := CheckNode(VstMain.RootNode, AnsiString(subXMLNode.nodeValue));
         Node := VstMain.AddChild(ParentFromBackup);
      end
      else begin
         Node := VstMain.AddChild(ParentNode);
      end;

      VstMain.ReinitNode(Node, false); // ensure node is initialized. Needed when the node is free to call onFreeNode
      TreeRec := VstMain.GetNodeData(Node);

      if NodeTag.childNodes.length >= 1 then begin
         subXMLNode := NodeTag.childNodes[0]; // NodeTag.childNodes.length is always at least 1
         if (subXMLNode.nodeType = NODE_TEXT) and
            (subXMLNode.nodeValue <> null) then
            TreeRec.LeftMsg := CRtoCRLF(subXMLNode.nodeValue);
      end;

      inc(step);
      if step > 100 then begin
         Progress.ProgressBar.StepIt;
         application.ProcessMessages;
         step := 0;
      end;

      // Id attribute
      subXMLNode := NodeTag.attributes.getNamedItem('Id');
      if subXMLNode <> nil then
         TreeRec.TraceID := AnsiString(subXMLNode.nodeValue);

      // ProcessName attribute
      subXMLNode := NodeTag.attributes.getNamedItem('ProcessName');
      if subXMLNode <> nil then
         TreeRec.ProcessName := subXMLNode.nodeValue;

      // Time attribute
      subXMLNode := NodeTag.attributes.getNamedItem('Time');
      if subXMLNode <> nil then
         TreeRec.Time := subXMLNode.nodeValue;

      // ThId attribute
      subXMLNode := NodeTag.attributes.getNamedItem('ThId');
      if subXMLNode <> nil then
         TreeRec.ThreadID := subXMLNode.nodeValue;

      // Icon attribute
      TreeRec.TreeIcon := -1;
      subXMLNode := NodeTag.attributes.getNamedItem('Icon');
      if subXMLNode <> nil then begin
         str := subXMLNode.nodeValue;
         if str = '' then
            TreeRec.TreeIcon := -1
         else
            TreeRec.TreeIcon := StrToIntDef(str, -1);
      end;

      // if date is gived, double the size of the time column, if not already done.
      if (IsDateTimeResized = false) and (length(TreeRec.Time) > 12) then
         begin
         IsDateTimeResized := true;
         VstMain.Header.Columns[1].Width := VstMain.Header.Columns[1].Width * 2;
      end;

      // get col2
      subXMLNode := NodeTag.selectSingleNode('Col2');
      if subXMLNode <> nil then
         TreeRec.RightMsg := CRtoCRLF(subXMLNode.Text); // cannot use nodeValue directly, because the text is the first child of the node

      // <ColValue Order="0">19/07/2007 21:47:05</ColValue>
      NodeList := NodeTag.selectNodes('ColValue');
      subnodecount := NodeList.Get_length;
      if subnodecount <> 0 then begin
         TreeRec.Columns := TStringList.Create;
         // it is possible to have more or less 'col data' than the number of 'col header'
         IsMultiColTree := true;
         for c := 0 to subnodecount - 1 do begin
            subXMLNode := IXMLDomNode(NodeList.item[c]);
            TreeRec.Columns.Add(subXMLNode.Text);
            // Col.order is not used : suposed to be the same order than for header
         end;
      end;

      // read font
      // <FontDetail ColId="3" Bold="True" Color="255" />
      NodeList := NodeTag.selectNodes('FontDetail');
      subnodecount := NodeList.Get_length;
      if subnodecount <> 0 then begin
         setlength(TreeRec.FontDetails, subnodecount);
         for c := 0 to subnodecount - 1 do begin
            subXMLNode := IXMLDomNode(NodeList.item[c]);
            FontDetail := TFontDetail.Create(subXMLNode); // parse xml
            TreeRec.FontDetails[c] := FontDetail;
         end; // fonts
      end;

      // read members
      // <Member>xxx<ColB>xxx</ColB><ColC>xxx</ColC></Member>
      TreeRec.Members := TMember.Create();
      NodeList := NodeTag.selectNodes('Member');
      subnodecount := NodeList.Get_length;
      if subnodecount <> 0 then begin
         for c := 0 to subnodecount - 1 do begin
         end;
      end;

      {
        for c := 0 to NodeTag.Member.Count-1 do begin
        XMLMember := NodeTag.Member.Items[c] ;
        cola := CRtoCRLF(BinaryDecode(XMLMember.GetDOMNode.childNodes[0].nodeValue)) ;

        // get optional ColB
        if XMLMember.ChildNodes.FindNode('ColB') <> nil then
        colb := CRtoCRLF(BinaryDecode(XMLMember.ColB))
        else
        colb := '' ;

        // get optional ColC
        if XMLMember.ChildNodes.FindNode('ColC') <> nil then
        colc := CRtoCRLF(BinaryDecode(XMLMember.ColC))
        else
        colc := '' ;

        Member := TMember.Create (cola ,colb,colc) ;

        if XMLMember.ChildNodes.FindNode('ViewerKind') <> nil then
        Member.ViewerKind := XMLMember.ViewerKind ;

        TreeRec.Members.SubMembers.Add(Member) ;

        // read member font
        setLength(Member.FontDetails,XMLMember.FontDetail.Count) ;
        for d := 0 to XMLMember.FontDetail.Count-1 do begin
        XmlFontDetail := XMLMember.FontDetail.Items[d] ;
        FontDetail := TFontDetail.create(XmlFontDetail);
        Member.FontDetails[d] := FontDetail;
        end ;

        recurMembers (Member,XMLMember) ;
        end ;  // members

      }

      // recursively get sub nodes type 'Node'
      NodeList := NodeTag.selectNodes('Node');
      // NodeList := NodeTag.Get_childNodes ;
      if NodeList <> nil then begin
         subnodecount := NodeList.Get_length;
         for c := 0 to subnodecount - 1 do begin
            subXMLNode := IXMLDomNode(NodeList.item[c]);
            loadNodeXML(Node, subXMLNode); // recursion
         end;
      end;
   end
   else if NodeName = 'Data' then begin
      // get all sub nodes (should be only <Node> )
      NodeList := NodeTag.Get_childNodes;
      if NodeList <> nil then begin
         subnodecount := NodeList.Get_length;
         for c := 0 to subnodecount - 1 do begin
            subXMLNode := IXMLDomNode(NodeList.item[c]);
            loadNodeXML(nil, subXMLNode); // recursion
         end;
      end;
   end;
end;

begin
   MSXML := CoDomDocument.Create;
   MSXML.load(filename); // check if file exist before ...
   XMLRootData := MSXML.selectSingleNode('Data');

   step := MSXML.selectNodes('//Node').length;

   Progress := TFrmTailProgress.Create(nil);
   Progress.ProgressBar.Min := 1;
   Progress.ProgressBar.Max := (step div 100) + 1;
   Progress.ProgressBar.position := 0;
   Progress.ProgressBar.step := 1;
   Progress.LabelCaptionLinesRead.Caption := inttostr(step) + ' Nodes';
   Progress.LabelLinesRead.Visible := false;
   Progress.IsStoped := false;
   Progress.Show;
   step := 0;

   InternalTrace(FormatDateTime('hh:mm:ss:zzz', now), 'new : before parsing xml ');
   LoadTitles(XMLRootData);
   loadNodeXML(nil, XMLRootData);
   InternalTrace(FormatDateTime('hh:mm:ss:zzz', now), 'new : end parsing xml');

   Progress.Hide;
   Progress.Free;

   if IsMultiColTree then
      AutosizeAll(VstMain);
end;

// ------------------------------------------------------------------------------

// called from actLoadXmlExecute or by the client Tracetool API
procedure TFrm_Trace.LoadXML(filename: string);
var
   XMLRootData: IXMLData;
   Progress: TFrmTailProgress;
   step: Integer;
   Select: IDOMNodeSelect;

procedure recurMembers(ParentMember: TMember;
   ParentMemberTag: IXMLMemberType);
var
   c: Integer;
   XMLMember: IXMLMemberType;
   member: TMember;
   cola, colb, colc: string;
begin
   for c := 0 to ParentMemberTag.member.Count - 1 do begin
      XMLMember := ParentMemberTag.member.Items[c];
      // cola := CRtoCRLF(BinaryDecode(XMLMember.GetDOMNode.childNodes[0].nodeValue)) ;
      cola := CRtoCRLF(XMLMember.GetDOMNode.childNodes[0].nodeValue);

      // get optional ColB
      if XMLMember.childNodes.FindNode('ColB') <> nil then
         colb := CRtoCRLF(XMLMember.colb) // CRtoCRLF(BinaryDecode(XMLMember.ColB))
      else
         colb := '';

      // get optional ColC
      if XMLMember.childNodes.FindNode('ColC') <> nil then
         colc := CRtoCRLF(XMLMember.colc) // CRtoCRLF(BinaryDecode(XMLMember.ColC))
      else
         colc := '';

      member := TMember.Create(cola, colb, colc);
      if XMLMember.attributes['ViewerKind'] <> null then
         member.ViewerKind := XMLMember.ViewerKind;

      ParentMember.SubMembers.Add(member);
      recurMembers(member, XMLMember);
   end;
end;

procedure LoadTitles(NodeTag: IXMLNodeType);
var
   c: Integer;
   col: IXMLColumn;
   treeCol: TVirtualTreeColumn;
begin
   if NodeTag.coltitle.Count <> 0 then begin
      VstMain.Header.Columns.Clear;
      IsMultiColTree := true;
      for c := 0 to NodeTag.coltitle.Count - 1 do begin
         col := NodeTag.coltitle.Items[c];
         treeCol := VstMain.Header.Columns.Add;
         treeCol.options := treeCol.options + [coAllowFocus];
         // ensure user can focus to this column
         treeCol.Text := col.Text;
         // Col.order
      end;
      if NodeTag.MainColumn <> '' then
         VstMain.Header.MainColumn := strToInt(NodeTag.MainColumn);
      AutosizeAll(VstMain);
   end;
end;

procedure loadNodeXML(ParentNode: PVirtualNode; NodeTag: IXMLNodeType);
var
   c, d: Integer;
   TreeRec: PTreeRec;
   Node: PVirtualNode;
   XMLMember: IXMLMemberType;
   member: TMember;
   XmlFontDetail: IXMLFontDetail;
   FontDetail: TFontDetail;
   cola, colb, colc: string;
   col: IXMLColumn;
   ParentFromBackup: PVirtualNode;
   // XMLNode : IxmlNode ;
   // ptr : variant ;
begin
   if Supports(NodeTag, IXMLData) then begin
      Node := nil;
   end
   else begin

      inc(step);
      if step > 100 then begin
         Progress.ProgressBar.StepIt;
         application.ProcessMessages;
         step := 0;
      end;

      // in backup mode parent is stored as an attribute. (tree is converted to relations)
      if NodeTag.Parent <> '' then begin
         ParentFromBackup := CheckNode (VstMain.RootNode, AnsiString(NodeTag.Parent));
         Node := VstMain.AddChild(ParentFromBackup);
      end
      else begin
         Node := VstMain.AddChild(ParentNode);
      end;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstMain.ReinitNode(Node, false);
      TreeRec := VstMain.GetNodeData(Node);
      // NodeTag.GetDOMNode.childNodes containt the text (in first child) and then sub nodes
      // convertion on retreived text must be applied to convert CR to CRLF
      if NodeTag.GetDOMNode.childNodes.length <> 0 then
         TreeRec.LeftMsg := CRtoCRLF
            (NodeTag.GetDOMNode.childNodes[0].nodeValue);

      TreeRec.RightMsg := CRtoCRLF(NodeTag.Col2);
      TreeRec.ProcessName := NodeTag.Process;
      TreeRec.Time := NodeTag.Time;
      TreeRec.TraceID := AnsiString(NodeTag.ID);
      TreeRec.ThreadID := NodeTag.ThId;
      if NodeTag.Icon = '' then
         TreeRec.TreeIcon := -1
      else
         TreeRec.TreeIcon := StrToIntDef(NodeTag.Icon, -1);

      // if date is gived, double the size of the time column, if not already done.
      if (IsDateTimeResized = false) and (length(TreeRec.Time) > 12) then
         begin
         IsDateTimeResized := true;
         VstMain.Header.Columns[1].Width := VstMain.Header.Columns[1].Width * 2;
      end;

      if NodeTag.ColValue.Count <> 0 then begin
         TreeRec.Columns := TStringList.Create;

         // it is possible to have more or less 'col data' than the number of 'col header'
         for c := 0 to NodeTag.ColValue.Count - 1 do begin
            IsMultiColTree := true;
            col := NodeTag.ColValue.Items[c];
            TreeRec.Columns.Add(col.Text);
            // Col.order is not used : suposed to be the same order than for header
         end;
      end;

      // read font
      setlength(TreeRec.FontDetails, NodeTag.FontDetail.Count);
      for c := 0 to NodeTag.FontDetail.Count - 1 do begin
         XmlFontDetail := NodeTag.FontDetail.Items[c];
         FontDetail := TFontDetail.Create(XmlFontDetail);
         TreeRec.FontDetails[c] := FontDetail;
      end; // fonts

      // read members
      TreeRec.Members := TMember.Create();
      for c := 0 to NodeTag.member.Count - 1 do begin
         XMLMember := NodeTag.member.Items[c];
         // cola := CRtoCRLF(BinaryDecode(XMLMember.GetDOMNode.childNodes[0].nodeValue)) ;
         cola := CRtoCRLF(XMLMember.GetDOMNode.childNodes[0].nodeValue);

         // get optional ColB
         if XMLMember.childNodes.FindNode('ColB') <> nil then
            colb := CRtoCRLF(XMLMember.colb)
            // CRtoCRLF(BinaryDecode(XMLMember.ColB))
         else
            colb := '';

         // get optional ColC
         if XMLMember.childNodes.FindNode('ColC') <> nil then
            colc := CRtoCRLF(XMLMember.colc)
            // CRtoCRLF(BinaryDecode(XMLMember.ColC))
         else
            colc := '';

         member := TMember.Create(cola, colb, colc);

         if XMLMember.childNodes.FindNode('ViewerKind') <> nil then
            member.ViewerKind := XMLMember.ViewerKind;

         TreeRec.Members.SubMembers.Add(member);

         // read member font
         setlength(member.FontDetails, XMLMember.FontDetail.Count);
         for d := 0 to XMLMember.FontDetail.Count - 1 do begin
            XmlFontDetail := XMLMember.FontDetail.Items[d];
            FontDetail := TFontDetail.Create(XmlFontDetail);
            member.FontDetails[d] := FontDetail;
         end;

         recurMembers(member, XMLMember);
      end; // members
   end; // Supports (NodeTag, IXMLData)

   // read sub nodes
   for c := 0 to NodeTag.Node.Count - 1 do begin
      if Progress.IsStoped = true then
         exit;
      loadNodeXML(Node, NodeTag.Node.Items[c]);
   end;

end;

begin
   XMLRootData := LoadData(filename);

   Select := XMLRootData.DOMNode as IDOMNodeSelect;
   step := Select.selectNodes('//Node').length;
   Progress := TFrmTailProgress.Create(nil);
   Progress.ProgressBar.Min := 1;
   Progress.ProgressBar.Max := (step div 100) + 1;
   Progress.ProgressBar.position := 0;
   Progress.ProgressBar.step := 1;
   Progress.LabelCaptionLinesRead.Caption := inttostr(step) + ' Nodes';
   Progress.LabelLinesRead.Visible := false;
   Progress.IsStoped := false;
   Progress.Show;
   step := 0;

   // InternalTrace(FormatDateTime('hh:mm:ss:zzz',now), 'old : before parsing xml ');
   LoadTitles(XMLRootData);
   loadNodeXML(nil, XMLRootData);
   // InternalTrace(FormatDateTime('hh:mm:ss:zzz',now), 'old : end parsing xml') ;

   Progress.Hide;
   Progress.Free;

   if IsMultiColTree then
      AutosizeAll(VstMain);
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.AddToLog(ActiveNode, ParentCompoNode: PVirtualNode);
var
   TreeRec: PTreeRec;
   ParentTreeRec: PTreeRec;
   ChildXmlNode: IXMLNodeType; // IXMLNode
   member: TMember;
   MemberTag: IXMLMemberType;
   c: Integer;
   ColIdx: TColumnIndex;
   col: IXMLColumn;
   f: file of byte; // textfile ;
   buf: AnsiString;
   toWrite: Integer;
   FileToWrite: string;
   FileExt: string;
   textToWrite: string;
   doc: IXMLDocument;
   // pi : IDOMProcessingInstruction ;

   // recursive
  procedure recurMembers(ParentMember: TMember; ParentMemberTag: IXMLMemberType);
  var
     c: Integer;
     SubMember: TMember;
     SubMemberTag: IXMLMemberType;
  begin
     if ParentMember <> nil then begin
        for c := 0 to ParentMember.SubMembers.Count - 1 do begin
           // note : the generateNodeXML method differ here : we add member from a MemberType
           SubMember := TMember(ParentMember.SubMembers.Items[c]);
           SubMemberTag := ParentMemberTag.member.Add;
           SubMemberTag.Text := SubMember.Col1;

           if SubMember.Col2 <> '' then
              SubMemberTag.colb := SubMember.Col2;

           if SubMember.Col3 <> '' then
              SubMemberTag.colc := SubMember.Col3;

           if SubMember.ViewerKind <> CST_VIEWER_NONE then
              SubMemberTag.ViewerKind := SubMember.ViewerKind;
           // save all sub members
           recurMembers(SubMember, SubMemberTag);
        end;
     end;
  end;

  procedure SaveHeaders();
  var
     ColIdx: TColumnIndex;
     col: IXMLColumn;
  begin
     if IsMultiColTree then begin
        XMLLogFile.MainColumn := inttostr(VstMain.Header.MainColumn);
        ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
        while ColIdx <> InvalidColumn do begin
           col := XMLLogFile.coltitle.Add;
           col.Text := VstMain.Header.Columns.Items[ColIdx].Text;
           col.order := ColIdx;
           ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
        end;
     end;
  end;

begin

   if LogFileType = 0 then // 0 : no log
      exit;

   if XMLLogFile = nil then begin

      // create the document
      doc := NewXMLDocument();
      // doc.Encoding := 'ISO-8859-1' ;
      // get the root
      XMLLogFile := doc.GetDocBinding('Data', TXMLData, TargetNamespace) as IXMLData;
   end;

   // clear any data in tempory XML object
   XMLLogFile.Node.Clear;
   XMLLogFile.coltitle.Clear;

   TreeRec := VstMain.GetNodeData(ActiveNode);
   ChildXmlNode := XMLLogFile.Node.Add;

   if IsMultiColTree then begin // then isWatch too
      ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
      while ColIdx <> InvalidColumn do begin
         if ColIdx < TreeRec.Columns.Count then begin
         // check if more header than data
            col := ChildXmlNode.ColValue.Add;
            col.Text := TreeRec.Columns[ColIdx];
            col.order := ColIdx;
         end;
         ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
      end;
      // if more data than header : append data
      ColIdx := VstMain.Header.Columns.GetLastVisibleColumn;
      for c := ColIdx + 1 to TreeRec.Columns.Count - 1 do begin
         col := ChildXmlNode.ColValue.Add;
         col.Text := TreeRec.Columns[c];
         col.order := c;
      end;
   end
   else begin
      // save the tree col1
      ChildXmlNode.Text := TreeRec.LeftMsg;

      // save the tree col 2
      if TreeRec.RightMsg <> '' then begin
         try
            ChildXmlNode.Col2 := TreeRec.RightMsg;
         except
            on e: exception do
               ChildXmlNode.Col2 := e.message;
         end;
      end;

      if TreeRec.ProcessName <> '' then
         ChildXmlNode.Process := TreeRec.ProcessName;

      ChildXmlNode.Time := TreeRec.Time;
      ChildXmlNode.ID := string(TreeRec.TraceID);
      ChildXmlNode.ThId := TreeRec.ThreadID;

      // don't save default
      if (TreeRec.TreeIcon <> -1) and (TreeRec.TreeIcon <> 24) then
         ChildXmlNode.Icon := inttostr(TreeRec.TreeIcon);
   end;

   // add parent relation if not root
   if (ParentCompoNode <> nil) and (ParentCompoNode <> VstMain.RootNode)
      then begin
      ParentTreeRec := VstMain.GetNodeData(ParentCompoNode);
      ChildXmlNode.Parent := string(ParentTreeRec.TraceID);
   end;

   // save all members of the node
   if TreeRec.Members <> nil then begin
      for c := 0 to TreeRec.Members.SubMembers.Count - 1 do begin
         member := TMember(TreeRec.Members.SubMembers.Items[c]);
         // note : the recurMembers method differ here : we add member from a NodeType
         MemberTag := (ChildXmlNode as IXMLNodeType).member.Add;
         MemberTag.Text := member.Col1;

         if member.Col2 <> '' then
            MemberTag.colb := member.Col2;

         if member.Col3 <> '' then
            MemberTag.colc := member.Col3;

         if member.ViewerKind <> CST_VIEWER_NONE then
            MemberTag.ViewerKind := member.ViewerKind;

         if member.ViewerKind = CST_VIEWER_DUMP then begin
            // since version 12.4 : reset col3 for all submembers if viewer kind is Dump
            ResetDump(Member);
         end;

         // save all sub members
         recurMembers(member, MemberTag);
      end;
   end;
   if trim(LogFileName) = '' then
      LogFileName := 'TraceLog.xml';

   FileExt := ExtractFileExt(LogFileName);

   if LogFileType = 1 then begin
      // add CurrentFileNumber before extension
      if CurrentFileNumber <> 0 then
         FileToWrite := copy(LogFileName, 1, length(LogFileName) - length
               (FileExt)) + '_' + inttostr(CurrentFileNumber) + FileExt
      else
         FileToWrite := LogFileName
   end
   else begin // daily file
      // add CurrentFileNumber before extension
      if CurrentFileNumber <> 0 then
         FileToWrite := copy(LogFileName, 1, length(LogFileName) - length
               (FileExt)) + FormatDateTime('YYYYMMDD', now) + '_' + inttostr
            (CurrentFileNumber) + FileExt
      else
         FileToWrite := copy(LogFileName, 1, length(LogFileName) - length
               (FileExt)) + FormatDateTime('YYYYMMDD', now) + FileExt;
   end;

   // store the file relative to the viewer if no path specified
   if ExtractFilePath(FileToWrite) = '' then
      FileToWrite := Frm_Tool.strRunPath + FileToWrite;
   // strRunPath containt the last backslash

   try
      if not fileExists(FileToWrite) then begin
         SaveHeaders(); // include header in file
         // if false then
         // XMLLogFile.OwnerDocument.SaveToFile(FileToWrite);
         assignFile(f, FileToWrite);
         rewrite(f); // creates a new external file

         /// / add processing instruction : xml-stylesheet
         // if LogStyleSheet <> '' then begin
         // pi := doc.DOMDocument.createProcessingInstruction('xml-stylesheet','type="text/xsl" href="' + LogStyleSheet + '"') ;
         // buf := Utf8Encode (pi.data) ;
         // toWrite := length(buf) ;
         // BlockWrite(f, Pointer(buf)^, toWrite);
         // end ;

         textToWrite := XMLLogFile.XML;
         textToWrite := copy(textToWrite, 0, length(textToWrite) - 7) + #13 + '</Data>';
         buf := Utf8Encode(textToWrite); // XMLLogFile.OwnerDocument.XML : tStrings ;
         toWrite := length(buf);
         BlockWrite(f, pointer(buf)^, toWrite);
      end
      else begin // append only the node
         assignFile(f, FileToWrite);
         reset(f); // open the existing file in Read/Write access
         seek(f, filesize(f) - 7); // override the </data> tag
         textToWrite := ChildXmlNode.XML + #13 + '</Data>';
         buf := Utf8Encode(textToWrite);
         toWrite := length(buf);
         BlockWrite(f, pointer(buf)^, toWrite);
      end;
      closefile(f);
      if (MaxLines <> -1) then begin
         inc(LinesWritten);
         if (LinesWritten >= MaxLines) then begin
            inc(CurrentFileNumber);
            LinesWritten := 0; // reset counter
            ShowLog(); // redisplay filname
         end;
      end;

      // clear any data in tempory XML object
      XMLLogFile.Node.Clear;
      XMLLogFile.coltitle.Clear;
   except
   end;
end;

// ------------------------------------------------------------------------------

// called from the menu
procedure TFrm_Trace.SaveWin;
begin
   // ask first plugins if we can perform the action
   if DoPlugAction(CST_ACTION_SAVE, '') = false then
      exit;

   FrmSave.IsMultiColTree := IsMultiColTree;  // then isWatch too
   FrmSave.CheckOptionsList.Enabled := not IsMultiColTree;
   FrmSave.ShowModal;
   if FrmSave.ModalResult = mrCancel then
      exit;

   application.ProcessMessages;
   SetCursor(Screen.Cursors[crHourGlass]);

   try
      if FrmSave.rbXML.Checked = true then
         if trim(FrmSave.EditStyleSheet.Text) <> '' then
            SaveToXML(FrmSave.EditXml.Text + '|' + trim (FrmSave.EditStyleSheet.Text))
         else
            SaveToXML(FrmSave.EditXml.Text)
         else
            SaveToTextFile(FrmSave.EditText.Text, SaveTofileOptions);
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end;
end;

// ------------------------------------------------------------------------------
// called from SaveWin (menu) or by the client tracetool API
procedure TFrm_Trace.SaveToXML(filename: string);
var
   doc: IXMLDocument;
   pi: IDOMProcessingInstruction;
   XMLRootData: IXMLData;
   MasterTVNode: PVirtualNode;
   StyleSheet: string;
   P: Integer;
   Progress: TFrmTailProgress;
   step: Integer;

   // recursive
procedure recurMembers(ParentMember: TMember; ParentMemberTag: IXMLMemberType);
var
   c: Integer;
   SubMember: TMember;
   SubMemberTag: IXMLMemberType;
begin
   if ParentMember <> nil then begin
      for c := 0 to ParentMember.SubMembers.Count - 1 do begin
         // note : the generateNodeXML method differ here : we add member from a MemberType
         SubMember := TMember(ParentMember.SubMembers.Items[c]);
         SubMemberTag := ParentMemberTag.member.Add;
         SubMemberTag.Text := SubMember.Col1;

         if SubMember.Col2 <> '' then
            SubMemberTag.colb := SubMember.Col2;

         if SubMember.Col3 <> '' then
            SubMemberTag.colc := SubMember.Col3;

         if SubMember.ViewerKind <> CST_VIEWER_NONE then  // viewer kind are awailable only for top members.
            SubMemberTag.ViewerKind := SubMember.ViewerKind;
         // should not happens
         // save all sub members
         recurMembers(SubMember, SubMemberTag);
      end;
   end;
end;

procedure SaveHeaders(NodeTag: IXMLNodeType);
var
   ColIdx: TColumnIndex;
   col: IXMLColumn;
begin
   if IsMultiColTree then begin // then isWatch too
      NodeTag.MainColumn := inttostr(VstMain.Header.MainColumn);
      ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
      while ColIdx <> InvalidColumn do begin
         col := NodeTag.coltitle.Add;
         col.Text := VstMain.Header.Columns.Items[ColIdx].Text;
         col.order := ColIdx;
         ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
      end;
   end;
end;

// recursive
procedure generateNodeXML(NodeTag: IXMLNodeType; VtNode: PVirtualNode);
var
   ChildXmlNode: IXMLNodeType; // IXMLNode
   member: TMember;
   MemberTag: IXMLMemberType;
   FontDetail: TFontDetail;
   FontDetailTag: IXMLFontDetail;
   ChildVtNode: PVirtualNode;
   c, d: Integer;
   TreeRec: PTreeRec;
   ColIdx: TColumnIndex;
   col: IXMLColumn;
begin
   if VtNode = nil then
      exit;

   if NodeTag = nil then
      exit;

   if Supports(NodeTag, IXMLNodeType) = false then
      exit;

   inc(step);
   if step > 100 then begin
      Progress.ProgressBar.StepIt;
      application.ProcessMessages;
      step := 0;
   end;
   TreeRec := VstMain.GetNodeData(VtNode);
   if TreeRec <> nil then begin // treeRec can be nil the first time when VtNode is vst.RootNode

      if IsMultiColTree then begin
         ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
         while ColIdx <> InvalidColumn do begin
            if ColIdx < TreeRec.Columns.Count then begin
            // check if more header than data
               col := NodeTag.ColValue.Add;
               col.Text := TreeRec.Columns[ColIdx];
               col.order := ColIdx;
            end;
            ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
         end;
         // if more data than header : append data
         ColIdx := VstMain.Header.Columns.GetLastVisibleColumn;
         for c := ColIdx + 1 to TreeRec.Columns.Count - 1 do begin
            col := NodeTag.ColValue.Add;
            col.Text := TreeRec.Columns[c];
            col.order := c;
         end;
      end
      else begin
         // save the tree col1
         NodeTag.Text := TreeRec.LeftMsg;

         // save the tree col 2
         if TreeRec.RightMsg <> '' then
            NodeTag.Col2 := TreeRec.RightMsg;

         if TreeRec.ProcessName <> '' then
            NodeTag.Process := TreeRec.ProcessName;

         NodeTag.Time := TreeRec.Time;
         NodeTag.ID := string(TreeRec.TraceID);
         NodeTag.ThId := TreeRec.ThreadID;

         // don't save default
         if (TreeRec.TreeIcon <> -1) and (TreeRec.TreeIcon <> 24) then
            NodeTag.Icon := inttostr(TreeRec.TreeIcon);

      end;

      // save Fonts
      for c := 0 to length(TreeRec.FontDetails) - 1 do begin
         FontDetail := TreeRec.FontDetails[c];
         FontDetailTag := NodeTag.FontDetail.Add;
         FontDetailTag.ColId := FontDetail.ColId;
         if FontDetail.Bold then
            FontDetailTag.Bold := FontDetail.Bold;
         if FontDetail.Italic then
            FontDetailTag.Italic := FontDetail.Italic;
         if FontDetail.Color <> -1 then
            FontDetailTag.Color := FontDetail.Color;
         if FontDetail.Size <> 0 then
            FontDetailTag.Size := FontDetail.Size;
         if FontDetail.Name <> '' then
            FontDetailTag.Name := FontDetail.Name;
         if FontDetail.BackgroundColor <> 0 then
            FontDetailTag.BackgroundColor := FontDetail.BackgroundColor;
      end; // fonts

      // save all members of the node
      if TreeRec.Members <> nil then begin
         for c := 0 to TreeRec.Members.SubMembers.Count - 1 do begin
            member := TMember(TreeRec.Members.SubMembers.Items[c]);
            MemberTag := (NodeTag as IXMLNodeType).member.Add;

            // note : the recurMembers method differ here : we add member from a NodeType
            MemberTag.Text := member.Col1;

            if member.Col2 <> '' then
               MemberTag.colb := member.Col2;

            if member.Col3 <> '' then
               MemberTag.colc := member.Col3;

            if member.ViewerKind <> CST_VIEWER_NONE then
               MemberTag.ViewerKind := member.ViewerKind;

            if member.ViewerKind = CST_VIEWER_DUMP then begin
               // since version 12.4 : reset col3 for all submembers if viewer kind is Dump
               ResetDump(Member);
            end;

            // save member Fonts
            for d := 0 to length(member.FontDetails) - 1 do begin
               FontDetail := member.FontDetails[d];
               FontDetailTag := MemberTag.FontDetail.Add;
               FontDetailTag.ColId := FontDetail.ColId;
               if FontDetail.Bold then
                  FontDetailTag.Bold := FontDetail.Bold;
               if FontDetail.Italic then
                  FontDetailTag.Italic := FontDetail.Italic;
               if FontDetail.Color <> -1 then
                  FontDetailTag.Color := FontDetail.Color;
               if FontDetail.Size <> 0 then
                  FontDetailTag.Size := FontDetail.Size;
               if FontDetail.Name <> '' then
                  FontDetailTag.Name := FontDetail.Name;
               if FontDetail.BackgroundColor <> 0 then
                  FontDetailTag.BackgroundColor :=
                     FontDetail.BackgroundColor;
            end;

            // save all sub members
            recurMembers(member, MemberTag);
         end;
      end; // save members
   end; // treerec <> nil

   ChildVtNode := VtNode.FirstChild;
   while ChildVtNode <> nil do begin
      if Progress.IsStoped = true then
         exit;
      ChildXmlNode := NodeTag.Node.Add;
      // add recursive
      generateNodeXML(ChildXmlNode, ChildVtNode);
      ChildVtNode := ChildVtNode.NextSibling;
   end;
end;

begin
   // FileName parameter : real filename + styleSheet
   StyleSheet := '';
   P := pos('|', filename);
   if P <> 0 then begin
      StyleSheet := trim(copy(filename, P + 1, 1000));
      filename := trim(copy(filename, 1, P - 1));
   end;

   if ExtractFileExt(filename) = '' then
      filename := filename + '.xml';

   // create the document
   doc := NewXMLDocument();

   // add processing instruction : xml-stylesheet
   if StyleSheet <> '' then begin
      pi := doc.DOMDocument.createProcessingInstruction('xml-stylesheet',
         'type="text/xsl" href="' + StyleSheet + '"');
      doc.DOMDocument.appendChild(pi);
   end;

   // get the root
   XMLRootData := doc.GetDocBinding('Data', TXMLData, TargetNamespace)
      as IXMLData;

   // generate nodes
   MasterTVNode := VstMain.RootNode;

   step := 0;
   Progress := TFrmTailProgress.Create(nil);
   Progress.ProgressBar.Min := 1;
   Progress.ProgressBar.Max := (MasterTVNode.TotalCount div 100) + 1;
   Progress.ProgressBar.position := 0;
   Progress.ProgressBar.step := 1;
   Progress.LabelLinesRead.Visible := false;
   Progress.LabelCaptionLinesRead.Caption := inttostr
      (MasterTVNode.TotalCount) + ' Nodes';
   Progress.IsStoped := false;
   Progress.Show;

   SaveHeaders(XMLRootData);
   generateNodeXML(XMLRootData, MasterTVNode); // recursive

   if Progress.IsStoped = false then
      XMLRootData.OwnerDocument.SaveToFile(filename);
   Progress.Hide;
   Progress.Free;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.SaveToTextFile(filename: string;
   SaveOptions: TSaveTofileOptions);
var
   f: textfile;
   NewLine: string;
   IsFirst: boolean;
   TreeRec: PTreeRec;
   TreeIndentation: string;

  procedure recurSave(TestNode: PVirtualNode);
  var
     ChildVtNode: PVirtualNode;
     LeftMsg: string;
     ColIdx: TColumnIndex;
     c: Integer;
  begin

     TreeRec := VstMain.GetNodeData(TestNode);
     if TreeRec <> nil then begin // treeRec can be nil the first time when VtNode is vst.RootNode

        IsFirst := true;
        NewLine := '';

        if IsMultiColTree then begin  // then isWatch too
           // set indentation on first col  (no way to know the 'master col')
           NewLine := StrRepeat(TreeIndentation, VstMain.GetNodeLevel
                 (TestNode));
           ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
           while ColIdx <> InvalidColumn do begin
              if ColIdx < TreeRec.Columns.Count then begin
                 // note that last column can contain CRLF
                 if IsFirst = false then
                    NewLine := NewLine + TraceConfig.TextExport_Separator;
                 NewLine := NewLine + TraceConfig.TextExport_TextQualifier + RemoveLastCRLF(TreeRec.Columns[ColIdx])
                    + TraceConfig.TextExport_TextQualifier;
                 IsFirst := false;
              end;
              ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
           end;
           // if more data than header : append data
           ColIdx := VstMain.Header.Columns.GetLastVisibleColumn;
           for c := ColIdx + 1 to TreeRec.Columns.Count - 1 do
              NewLine := NewLine + TraceConfig.TextExport_TextQualifier + RemoveLastCRLF(TreeRec.Columns[c])
                 + TraceConfig.TextExport_TextQualifier;
        end
        else begin
           LeftMsg := TreeRec.LeftMsg;
           if SaveOptions.Copy_ProcessName then begin
              NewLine := TraceConfig.TextExport_TextQualifier + TreeRec.ProcessName + TraceConfig.TextExport_TextQualifier;
              IsFirst := false;
           end;

           if SaveOptions.Copy_ThreadID then begin
              if IsFirst = false then
                 NewLine := NewLine + TraceConfig.TextExport_Separator;
              NewLine := NewLine + TraceConfig.TextExport_TextQualifier + TreeRec.ThreadID + TraceConfig.TextExport_TextQualifier;
              IsFirst := false;
           end;

           if SaveOptions.Copy_Time then begin
              if IsFirst = false then
                 NewLine := NewLine + TraceConfig.TextExport_Separator;
              NewLine := NewLine + TraceConfig.TextExport_TextQualifier + TreeRec.Time + TraceConfig.TextExport_TextQualifier;
              IsFirst := false;
           end;

           if SaveOptions.Copy_col1 then begin
              if IsFirst = false then
                 NewLine := NewLine + TraceConfig.TextExport_Separator;

              NewLine := NewLine + StrRepeat
                 (TreeIndentation, VstMain.GetNodeLevel(TestNode));

              NewLine := NewLine + TraceConfig.TextExport_TextQualifier + LeftMsg + TraceConfig.TextExport_TextQualifier;
              IsFirst := false;
           end;

           if SaveOptions.Copy_Col2 then begin
              if IsFirst = false then
                 NewLine := NewLine + TraceConfig.TextExport_Separator;
              NewLine := NewLine + TraceConfig.TextExport_TextQualifier + TreeRec.RightMsg + TraceConfig.TextExport_TextQualifier;
           end;

           // members are not saved.
        end;

        writeln(f, NewLine);
     end;

     ChildVtNode := TestNode.FirstChild;
     while ChildVtNode <> nil do begin
        recurSave(ChildVtNode);
        ChildVtNode := ChildVtNode.NextSibling;
     end;
  end;

  procedure AddTitle;
  var
     NoTitle: boolean;
     col: TVirtualTreeColumn;
     ColIdx: TColumnIndex;
  begin
     if SaveOptions.Copy_ColumnTitle = false then
        exit;

     IsFirst := true;
     NewLine := '';

     if IsMultiColTree then begin   // then isWatch too
        // check if titles are all empty
        NoTitle := true;
        ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
        while ColIdx <> InvalidColumn do begin
           col := VstMain.Header.Columns[ColIdx];
           if col.Text <> '' then begin
              NoTitle := false;
              break;
           end;
           ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
        end;

        ColIdx := VstMain.Header.Columns.GetFirstVisibleColumn;
        while ColIdx <> InvalidColumn do begin
           col := VstMain.Header.Columns[ColIdx];

           if IsFirst = false then
              NewLine := NewLine + TraceConfig.TextExport_Separator;
           IsFirst := false;

           // note that last column can contain CRLF
           if NoTitle then
              NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Col ' + inttostr(ColIdx + 1)
                 + TraceConfig.TextExport_TextQualifier
           else
              NewLine := NewLine + TraceConfig.TextExport_TextQualifier +RemoveLastCRLF(col.Text) + TraceConfig.TextExport_TextQualifier;
           ColIdx := VstMain.Header.Columns.GetNextVisibleColumn(ColIdx);
        end;
     end
     else begin
        if SaveOptions.Copy_ProcessName then begin
           NewLine := TraceConfig.TextExport_TextQualifier + 'ProcessName' + TraceConfig.TextExport_TextQualifier;
           IsFirst := false;
        end;

        if SaveOptions.Copy_ThreadID then begin
           if IsFirst = false then
              NewLine := NewLine + TraceConfig.TextExport_Separator;
           NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'ThreadID' + TraceConfig.TextExport_TextQualifier;
           IsFirst := false;
        end;

        if SaveOptions.Copy_Time then begin
           if IsFirst = false then
              NewLine := NewLine + TraceConfig.TextExport_Separator;
           NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Time' + TraceConfig.TextExport_TextQualifier;
           IsFirst := false;
        end;

        if SaveOptions.Copy_col1 then begin
           if IsFirst = false then
              NewLine := NewLine + TraceConfig.TextExport_Separator;
           NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Msg' + TraceConfig.TextExport_TextQualifier;
           IsFirst := false;
        end;

        if SaveOptions.Copy_Col2 then begin
           if IsFirst = false then
              NewLine := NewLine + TraceConfig.TextExport_Separator;
           NewLine := NewLine + TraceConfig.TextExport_TextQualifier + 'Right Msg' + TraceConfig.TextExport_TextQualifier;
        end;
     end;
     writeln(f, NewLine);
  end;

begin
   TreeIndentation := StrRepeat(' ', TraceConfig.TextExport_TreeIndentation);

   assignFile(f, filename);
   rewrite(f);
   try
      // add title if needed.
      AddTitle;

      // add node starting from the invisible root node (recursive)
      recurSave(VstMain.RootNode);

   finally
      closefile(f);
   end;
end;

// ------------------------------------------------------------------------------
// PLUGIN API
// ------------------------------------------------------------------------------

// link a plugin to a window
// AddPlugin is called by the client API (plugin)

procedure TFrm_Trace.AddPlugin(pluginName: AnsiString; flags: Integer);
var
   c, d: Integer;
   plugin: TPlugin;
   LinkedPlugin: TLinkedPlugin;
begin
   pluginName := AnsiString(trim(UpperCase(string(pluginName))));
   for c := 0 to TraceConfig.PluginList.Count - 1 do begin
      plugin := TPlugin(TraceConfig.PluginList.Items[c]);
      if AnsiString(UpperCase(string(plugin.PlugName))) = pluginName then
         begin
         // check if the plugin is not already linked to window before adding it
         for d := 0 to LinkedPlugins.Count - 1 do begin
            LinkedPlugin := TLinkedPlugin(LinkedPlugins.Items[d]);
            if LinkedPlugin.plugin = plugin then begin
               LinkedPlugin.NeedOnAction := (flags and CST_PLUG_ONACTION) <> 0;
               LinkedPlugin.NeedOnbeforeDelete := (flags and CST_PLUG_ONBEFOREDELETE) <> 0;
               LinkedPlugin.NeedTimer := (flags and CST_PLUG_ONTIMER) <> 0;
               exit;
            end;
         end;
         // not yet linked to window
         LinkedPlugin := TLinkedPlugin.Create;
         LinkedPlugin.plugin := plugin;
         LinkedPlugin.NeedOnAction := (flags and CST_PLUG_ONACTION) <> 0;
         LinkedPlugin.NeedOnbeforeDelete := (flags and CST_PLUG_ONBEFOREDELETE) <> 0;
         LinkedPlugin.NeedTimer := (flags and CST_PLUG_ONTIMER) <> 0;
         LinkedPlugins.Add(LinkedPlugin); // owner
         exit;
      end;
   end;
end;

// ------------------------------------------------------------------------------

// create a resource for a window
// ResType : resource type
// CST_RES_BUT_RIGHT        = 1 ;     // Button on right
// CST_RES_BUT_LEFT         = 2 ;     // Button on left
// CST_RES_LABEL_RIGHT      = 3 ;     // Label on right
// CST_RES_LABELH_RIGHT     = 4 ;     // Label on right HyperLink
// CST_RES_LABEL_LEFT       = 5 ;     // Label on left
// CST_RES_LABELH_LEFT      = 6 ;     // Label on left hyperlink
// CST_RES_MENU_ACTION      = 7 ;     // Item menu in the Actions Menu
// CST_RES_MENU_WINDOW      = 8 ;     // Item menu in the Windows Menu. Call CreateResource on the main win trace to create this menu item

procedure TFrm_Trace.CreateResource(ResId: Integer; ResType: Integer; ResWidth: Integer; ResText: string);
var
   res: TPlugResource;
   but: tbutton;
   lab: TLabel;
   itemMenu: TMenuItem;
begin
   if SetTextResource(ResId, ResText) then
      exit;

   res := TPlugResource.Create;
   res.Internal := false;
   res.ID := ResId;

   case ResType of
      CST_RES_BUT_RIGHT, // Button on right
      CST_RES_BUT_LEFT: // Button on left
         begin
            but := tbutton.Create(self);
            but.Parent := PanelTop;
            but.Width := ResWidth;
            but.Caption := ResText;
            but.Tag := ResId;
            but.OnClick := PluginAction;
            but.top := 0;
            but.Height := 20;
            res.Obj := but;
            if ResType = CST_RES_BUT_RIGHT then begin
               but.Anchors := [akTop, akRight];
               RightResources.Add(res);
            end
            else
               LeftResources.Add(res);
            DrawStatusBar();
         end;

      CST_RES_LABEL_RIGHT, // Label on right
      CST_RES_LABELH_RIGHT, // Label on right HyperLink
      CST_RES_LABEL_LEFT, // Label on left
      CST_RES_LABELH_LEFT: // Label on left hyperlink
         begin
            lab := TLabel.Create(self);
            lab.Parent := PanelTop;
            lab.Caption := ResText;
            lab.top := 5;
            if ResWidth = 0 then begin
               lab.AutoSize := true;
               lab.Width := 20;
            end
            else begin
               lab.Width := ResWidth;
               lab.AutoSize := false;
            end;
            // hyperlink
            if (ResType = CST_RES_LABELH_RIGHT) or
               (ResType = CST_RES_LABELH_LEFT) then begin
               lab.Font.Style := [fsUnderline];
               lab.Cursor := crHandPoint;
            end;
            // right anchors
            if (ResType = CST_RES_LABEL_RIGHT) or
               (ResType = CST_RES_LABELH_RIGHT) then
               lab.Anchors := [akTop, akRight];

            lab.Tag := ResId;
            lab.OnClick := PluginAction;
            res.Obj := lab;
            case ResType of
               CST_RES_LABEL_RIGHT:
                  RightResources.Add(res); // Label on right
               CST_RES_LABELH_RIGHT:
                  RightResources.Add(res); // Label on right HyperLink
               CST_RES_LABEL_LEFT:
                  LeftResources.Add(res); // Label on left
               CST_RES_LABELH_LEFT:
                  LeftResources.Add(res); // Label on left hyperlink
            end;
            DrawStatusBar();
         end;

      CST_RES_MENU_ACTION: // Item menu in the Actions Menu
         begin
            // New items are added after the 'view property' item menu
            itemMenu := TMenuItem.Create(self);
            itemMenu.Caption := ResText;
            itemMenu.Tag := ResId;
            itemMenu.OnClick := PluginAction;
            itemMenu.Visible := false;
            itemMenu.GroupIndex := 1;
            res.Obj := itemMenu;
            ActionResources.Add(res);
            DrawActionMenu
               (self.getTabSheet = self.getPageControl.ActivePage);
         end;
      CST_RES_MENU_WINDOW: // Item menu in the Windows Menu. Call CreateResource on the main win trace to create this menu item
         begin
            // New items are added after the 'ODS' item menu
            itemMenu := TMenuItem.Create(self);
            itemMenu.Caption := ResText;
            itemMenu.Tag := ResId;
            itemMenu.OnClick := PluginAction;
            res.Obj := itemMenu;
            Frm_Tool.mnuWindow.Insert(5, itemMenu);
            WindowResources.Add(res);
         end;
   end;
end;

// ------------------------------------------------------------------------------

Procedure TFrm_Trace.DrawActionMenu(ToEnable: boolean);
var
   c: Integer;
   res: TPlugResource;
   itemMenu: TMenuItem;
   MnuAction: TMenuItem;
   container: TFrmPageContainer;
begin
   container := getPageContainer();
   if container = MainPageContainer then
      MnuAction := Frm_Tool.MainMnuAction
   else
      MnuAction := self.getPageContainer.MnuAction;

   for c := 0 to ActionResources.Count - 1 do begin
      res := TPlugResource(ActionResources.Items[c]);
      itemMenu := TMenuItem(res.Obj);
      // delete
      if (itemMenu.Parent <> nil) and
         (itemMenu.Parent.IndexOf(itemMenu) <> -1) then
         itemMenu.Parent.Delete(itemMenu.Parent.IndexOf(itemMenu));
      // .Visible := ToEnable ;

      if ToEnable then begin
         MnuAction.Insert(8 { MnuAction.Count } , itemMenu);
         itemMenu.Visible := true;
      end;
   end;
end;

// ------------------------------------------------------------------------------

Procedure TFrm_Trace.DrawStatusBar;
var
   c: Integer;
   currentPos: Integer;
   res: TPlugResource;
begin
   // Gap between resources : 3 pixels
   currentPos := 3;
   for c := LeftResources.Count - 1 downto 0 do begin
      res := TPlugResource(LeftResources.Items[c]);
      if res.Obj is TLabel then begin
         if TLabel(res.Obj).Enabled = true then begin
            TLabel(res.Obj).Left := currentPos;
            inc(currentPos, TLabel(res.Obj).Width);
            inc(currentPos, 3);
         end;
      end
      else if res.Obj is tbutton then begin
         if tbutton(res.Obj).Enabled = true then begin
            tbutton(res.Obj).Left := currentPos;
            inc(currentPos, tbutton(res.Obj).Width);
            inc(currentPos, 3);
         end;
      end;
   end;

   currentPos := PanelTop.Width - 25; // skip the close button
   for c := RightResources.Count - 1 downto 0 do begin
      res := TPlugResource(RightResources.Items[c]);
      if res.Obj is TLabel then begin
         if TLabel(res.Obj).Enabled = true then begin
            dec(currentPos, TLabel(res.Obj).Width);
            dec(currentPos, 3);
            TLabel(res.Obj).Left := currentPos;
         end;
      end
      else if res.Obj is tbutton then begin
         if tbutton(res.Obj).Enabled = true then begin
            dec(currentPos, tbutton(res.Obj).Width);
            dec(currentPos, 3);
            tbutton(res.Obj).Left := currentPos;
         end;
      end;
   end;
end;

// ------------------------------------------------------------------------------
// return true if the resource exist
function TFrm_Trace.SetTextResource(ResId: Integer; ResText: string)
   : boolean;
procedure CheckResource(list: TObjectList); // : boolean ;
var
   c: Integer;
   res: TPlugResource;
begin
   Result := false;
   for c := 0 to list.Count - 1 do begin
      res := TPlugResource(list.Items[c]);
      if res.ID = ResId then begin
         if res.Obj is TLabel then begin
            TLabel(res.Obj).Caption := ResText;
         end
         else if res.Obj is tbutton then begin
            tbutton(res.Obj).Caption := ResText;
         end
         else if res.Obj is TMenuItem then begin
            TMenuItem(res.Obj).Caption := ResText;
         end;
         Result := true;
         DrawStatusBar();
      end;
   end;
end;

begin
   CheckResource(RightResources);
   if Result then
      exit;
   CheckResource(LeftResources);
   if Result then
      exit;
   CheckResource(ActionResources);
   if Result then
      exit;
   CheckResource(WindowResources);
   if Result then
      exit;
   // check internal
   case ResId of
      CST_ACTION_LABEL_INFO:
         TracesInfo.Caption := ResText; // TracesInfo label
      CST_ACTION_LABEL_LOGFILE:
         LabelLogFile.Caption := ResText; // LabelLogFile label
      // ...
   end;
end;

// ------------------------------------------------------------------------------

// disable tracetool resources
procedure TFrm_Trace.DisableResource(ResId: Integer);
begin
   case ResId of
      // CST_ACTION_CUT           = 1 ;     // cut
      // CST_ACTION_COPY          = 2 ;     // copy
      // CST_ACTION_DELETE        = 3 ;     // delete
      // CST_ACTION_SELECT_ALL    = 4 ;     // select all
      // CST_ACTION_RESIZE_COLS   = 5 ;     // resize columns
      // CST_ACTION_VIEW_INFO     = 6 ;     // view trace info
      // CST_ACTION_VIEW_PROP     = 7 ;     // view properties
      // CST_ACTION_PAUSE         = 8 ;     // Pause
      // CST_ACTION_SAVE          = 9 ;     // SaveToFile
      // CST_ACTION_CLEAR_ALL     = 10 ;    // clear all
      CST_ACTION_LABEL_INFO:
         begin
            TracesInfo.Visible := false; // TracesInfo label
            DrawStatusBar();
         end;
      CST_ACTION_LABEL_LOGFILE:
         begin
            LabelLogFile.Visible := false; // LabelLogFile label
            DrawStatusBar();
         end;
      CST_ACTION_CLOSE_WIN:
         butClose.Visible := false; // close button
      CST_ACTION_VIEW_MAIN:
         Frm_Tool.actViewMainTraces.Visible := false; // View Main trace
      CST_ACTION_VIEW_ODS:
         Frm_Tool.actViewOutputDebugString.Visible := false; // ODS
      CST_ACTION_OPEN_XML:
         Frm_Tool.actLoadXml.Visible := false; // Tracetool XML traces
      CST_ACTION_EVENTLOG:
         Frm_Tool.actEventlog.Visible := false; // Event log
      CST_ACTION_TAIL:
         Frm_Tool.actTailWin.Visible := false; // Tail
   end;
end;

// ------------------------------------------------------------------------------
// Onclick event
procedure TFrm_Trace.PluginAction(Sender: TObject);
var
   resTag: Integer;
   SelectedNode: PVirtualNode;
   TreeRec: PTreeRec;
   NodeId: AnsiString;
begin
   resTag := 0;
   if Sender is Tcomponent then
      resTag := Tcomponent(Sender).Tag;

   SelectedNode := VstMain.GetFirstSelected;
   if SelectedNode = nil then
      NodeId := ''
   else begin
      TreeRec := VstMain.GetNodeData(SelectedNode);
      NodeId := TreeRec.TraceID;
   end;
   DoPlugAction(resTag, NodeId);
end;

// ------------------------------------------------------------------------------

// check if a plugin attached to this window need the BeforeDelete event
function TFrm_Trace.NeedBeforeDelete(): boolean;
var
   c: Integer;
   LinkedPlugin: TLinkedPlugin;
begin
   Result := false;
   for c := 0 to LinkedPlugins.Count - 1 do begin
      LinkedPlugin := TLinkedPlugin(LinkedPlugins.Items[c]);
      if (LinkedPlugin.plugin.status = psStarted) and
         (LinkedPlugin.NeedOnbeforeDelete) then begin
         Result := true;
         exit;
      end;
   end;
end;

// ------------------------------------------------------------------------------

// ask plugin if the node can be deleted  (same as DoPlugDelete, but clear cache list)
function TFrm_Trace.askDelete(NodeToDelete: PVirtualNode): boolean;
var
   TreeRec: PTreeRec;
begin
   TreeRec := VstMain.GetNodeData(NodeToDelete);

   Result := DoAskPlugDelete(TreeRec.TraceID);
   // ask the plugin if the node can be deleted
   // result := DoPlugAction (CST_ACTION_DELETE,TreeRec.TraceID);   // ask the plugin if the node can be deleted

   // if result then begin
   // MostUsedList.RemoveFromList (NodeToDelete);    // remove the node and his children
   // LastUsedList.RemoveFromList (NodeToDelete);    // remove the node and his children
   // end ;
end;

// ------------------------------------------------------------------------------

// ask plugin if the node can be deleted
function TFrm_Trace.DoAskPlugDelete(NodeId: AnsiString): boolean;
var
   c: Integer;
   LinkedPlugin: TLinkedPlugin;
   ActionResult: boolean;
   plugCount: Integer;
begin
   Result := true;
   try
      plugCount := LinkedPlugins.Count;
      for c := 0 to plugCount - 1 do begin
         LinkedPlugin := TLinkedPlugin(LinkedPlugins.Items[c]);
         if LinkedPlugin.NeedOnbeforeDelete then begin // (LinkedPlugin.Plugin.Status = psStarted) and (assigned (LinkedPlugin.Plugin.OnBeforeDelete)
            ActionResult := LinkedPlugin.plugin.DoBeforeDelete
               (pAnsiString(self.ID), pAnsiString(NodeId));
            if ActionResult = false then
               Result := false;
         end;
      end;
   except
      on e: exception do
         InternalTrace('plugin exception : ' + e.Message);
   end;
end;

// ------------------------------------------------------------------------------

// call the OnAction for each plugin attached to the window.
// result is false if one the plugin return false.
function TFrm_Trace.DoPlugAction(ResId: Integer; NodeId: AnsiString)
   : boolean;
var
   c: Integer;
   LinkedPlugin: TLinkedPlugin;
   ActionResult: boolean;
begin
   Result := true;
   try
      for c := 0 to LinkedPlugins.Count - 1 do begin
         if c > LinkedPlugins.Count - 1 then
            break;
         LinkedPlugin := TLinkedPlugin(LinkedPlugins.Items[c]);
         if LinkedPlugin.NeedOnAction then begin // and (LinkedPlugin.Plugin.status = psStarted) and (assigned(LinkedPlugin.Plugin.OnAction))
            ActionResult := LinkedPlugin.plugin.DoAction (pAnsiString(self.ID), ResId, pAnsiString(NodeId));
            if ActionResult = false then
               Result := false;
         end;
      end;
   except
      on e: exception do
         InternalTrace('plugin exception : ' + e.Message);
   end;
end;

// ------------------------------------------------------------------------------

// call the OnTimer for each plugin attached to the window.
procedure TFrm_Trace.DoPlugTimer;
var
   c: Integer;
   LinkedPlugin: TLinkedPlugin;
begin
   try
      for c := 0 to LinkedPlugins.Count - 1 do begin
         LinkedPlugin := TLinkedPlugin(LinkedPlugins.Items[c]);
         if LinkedPlugin.NeedTimer then // and (LinkedPlugin.Plugin.status = psStarted) and (assigned(LinkedPlugin.Plugin.OnTimer)) then
            LinkedPlugin.plugin.DoTimer();
      end;
   except
      on e: exception do
         InternalTrace('TFrm_Trace.TimerInfo exception : ' + e.Message);
   end;
end;

// ------------------------------------------------------------------------------

function TFrm_Trace.CheckSearchRecord(TreeRec: PTreeRec): boolean;
var
   c: Integer;
begin
   Result := false;
   if (MatchSearch(TreeRec.LeftMsg) <> 0) or
      (MatchSearch(TreeRec.RightMsg) <> 0) or
      (MatchSearch(TreeRec.ProcessName) <> 0) or
      (MatchSearch(TreeRec.Time) <> 0) or
      (MatchSearch(TreeRec.ThreadID) <> 0) then begin
      Result := true;
      exit;
   end;

   if TreeRec.Columns <> nil then begin
      for c := 0 to TreeRec.Columns.Count - 1 do begin
         if MatchSearch(TreeRec.Columns[c]) <> 0 then begin
            Result := true;
            exit;
         end;
      end;
   end;

   if TreeRec.Members <> nil then
      Result := TreeRec.Members.Search();
end;

// ------------------------------------------------------------------------------

function TFrm_Trace.SearchNext(start: boolean): boolean;
var
   currentNode: PVirtualNode;
   TreeRec: PTreeRec;

procedure CheckVisible();
begin
   while (currentNode <> nil) and (VstMain.IsVisible[currentNode] = false)
      do begin
      currentNode := VstMain.GetNext(currentNode);
   end;
end;

begin
   Result := false;
   if Visible = false then
      exit;

   if start = true then begin
      currentNode := VstMain.GetFirst();
   end
   else begin
      currentNode := VstMain.GetFirstSelected;
      if currentNode = nil then
         currentNode := VstMain.GetFirstVisible()
      else // when start is false, we are searching in the current document
         currentNode := VstMain.GetNext(currentNode);
      // skip the first selected
   end;

   CheckVisible();
   while currentNode <> nil do begin
      TreeRec := VstMain.GetNodeData(currentNode);
      if CheckSearchRecord(TreeRec) then begin
         if ActiveTracePage <> self then
            SetActivePage();
         // fully visible ?
         VstMain.ScrollIntoView(currentNode, false);
         // ensure the node is fully visible and displayed
         VstMain.ClearSelection;
         VstMain.selected[currentNode] := true;
         VstMain.SetFocus();
         Result := true;
         exit;
      end;
      currentNode := VstMain.GetNext(currentNode);
      CheckVisible();
   end;
end;

// ------------------------------------------------------------------------------

function TFrm_Trace.SearchPrevious(atEnd: boolean): boolean;
var
   currentNode: PVirtualNode;
   TreeRec: PTreeRec;
procedure CheckVisible();
begin
   while (currentNode <> nil) and (VstMain.IsVisible[currentNode] = false)
      do begin
      currentNode := VstMain.GetPrevious(currentNode);
   end;
end;

begin
   Result := false;
   if Visible = false then
      exit;

   if atEnd = true then begin
      currentNode := VstMain.GetLast();
   end
   else begin
      currentNode := VstMain.GetFirstSelected;
      if currentNode = nil then
         currentNode := VstMain.GetLastVisible()
      else // when atEnd is false, we are searching in the current document
         currentNode := VstMain.GetPrevious(currentNode);
      // skip the first selected
   end;

   CheckVisible();
   while currentNode <> nil do begin
      TreeRec := VstMain.GetNodeData(currentNode);
      if CheckSearchRecord(TreeRec) then begin
         if ActiveTracePage <> self then
            SetActivePage();
         // fully visible ?
         VstMain.ScrollIntoView(currentNode, false);
         // ensure the node is fully visible and displayed
         VstMain.ClearSelection;
         VstMain.selected[currentNode] := true;
         VstMain.SetFocus();
         Result := true;
         exit;
      end;
      currentNode := VstMain.GetPrevious(currentNode);
      CheckVisible();
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.RefreshView;
   // force nodes matching criteria to be fully visible
   // recursive
   procedure CheckNode(VtNode: PVirtualNode);
   var
      ChildVtNode: PVirtualNode;
      TreeRec: PTreeRec;
   begin
      if VtNode = nil then
         exit;
      if VstMain.IsVisible[VtNode] = false then
      // node is invisible if filtered.
         exit;
      TreeRec := VstMain.GetNodeData(VtNode);
      // treeRec can be nil the first time when VtNode is vst.RootNode
      if (TreeRec <> nil) and (CheckSearchRecord(TreeRec)) then
         VstMain.FullyVisible[VtNode] := true;

      ChildVtNode := VtNode.FirstChild;
      while ChildVtNode <> nil do begin
         CheckNode(ChildVtNode);
         ChildVtNode := ChildVtNode.NextSibling;
      end;
   end;

begin
   if (unt_search.SearchKind = mrYesToAll) then   // Highlight all matching node
      if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
         CheckNode(VstMain.RootNode); // check if the node or one of his child match the search text

   VstMain.Refresh; // force repaint the gutter
   Tframe_Classic(TreeDetailFrame).VstDetail.Refresh;
   // repaint detail for highlight
end;

// ------------------------------------------------------------------------------

// if the paint area is modified, AfterPaint is called to redisplay the gutter

procedure TFrm_Trace.vstMainAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
   Node: PVirtualNode;
   BaseOffset: Integer; // top position of the top node to draw given in absolute tree coordinates
   DispRec, CliRect: TRect;
   Yposition: Integer;
   NodeHeight: Integer;
   TreeRec: PTreeRec;
   HeaderHeight: Integer;
   gutterCanvas: TCanvas;
   newgutter: TImage;
   BookmarkPos: Integer;
   // c : integer ;
   // SpecialViewer : integer ;
   // member : TMember ;
begin
   // detect header height
   if VstMain.Header.Columns.Count <> 0 then begin
      // get the header height from the first header column
      // since the VT.headerRect property is protected :-(
      HeaderHeight := VstMain.Header.Columns[0].GetRect.Bottom;
   end
   else begin // should not happens
      HeaderHeight := VstMain.Header.Height + 2; // plus somme bevels
   end;

   newgutter := TImage.Create(self);
   newgutter.Width := PanelGutter.Width;
   newgutter.Height := PanelGutter.Height;
   newgutter.top := 0;
   newgutter.Left := 0;
   newgutter.OnDblClick := PanelGutterDblClick;
   gutterCanvas := newgutter.Canvas;

   // clear gutter
   CliRect := Rect(0, HeaderHeight, PanelGutter.Width, PanelGutter.Height);
   gutterCanvas.Brush.Color := clBtnFace; // clgreen ; //clBtnFace ;
   gutterCanvas.FillRect(CliRect);

   // Determine node to start drawing with.
   BaseOffset := 0;
   Node := VstMain.GetNodeAt(0, 0, true, BaseOffset);
   if Node <> nil then begin // nothing to display
      // get the first visible node rectangle.
      DispRec := VstMain.GetDisplayRect(Node, NoColumn, false, false);

      // We just need the TOP node position
      // This top position is zero or negative since the node can be partially visible
      // but can never be more than zero (else we can have previous partial visible node before)
      Yposition := DispRec.top;

      // add Header height
      inc(Yposition, HeaderHeight);

      // draw each node
      while Node <> nil do begin
         NodeHeight := VstMain.NodeHeight[Node];
         TreeRec := VstMain.GetNodeData(Node);

         BookmarkPos := bookmarks.IndexOf(Node);
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
                  if CheckSearchRecord(TreeRec) then // check if the node or one of his child match the search text
                     Frm_Tool.ilActions.Draw(gutterCanvas, 0, Yposition, 21);
         end;
         inc(Yposition, NodeHeight);
         Node := VstMain.GetNextVisible(Node);
      end;
   end;

   // draw the header to hide bullet on previous line.
   CliRect := Rect(0, 0, PanelGutter.Width, HeaderHeight);
   gutterCanvas.Brush.Color := clBtnFace;
   gutterCanvas.FillRect(CliRect);
   DrawEdge(gutterCanvas.Handle, CliRect, EDGE_RAISED, BF_BOTTOM);

   // replace the old gutter by the new one
   newgutter.Parent := PanelGutter;
   newgutter.BringToFront;

   if Gutter <> nil then begin
      Gutter.Parent := nil;
      Gutter.Free;
   end;
   Gutter := newgutter;
end;

// ----------------------------------------------------------------------------------------------------------------------

// BeforeCellPaint is used to draw background cells
procedure TFrm_Trace.vstMainBeforeCellPaint(Sender: TBaseVirtualTree;
   TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
   CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   TreeRec: PTreeRec;
begin

   if Node = nil then
      exit;
   // bookmarks are prioritary
   if bookmarks.IndexOf(Node) <> -1 then begin
      DrawHighlight(TargetCanvas, CellRect, true);
      // IsBookmark=true -> clTeal
      exit;
   end;

   TreeRec := VstMain.GetNodeData(Node);

   // then check if highlight must be draw
   if (SearchText <> '') and (SearchKind = mrYesToAll) then begin // mrYesToAll means Highlight all
      if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
         if CheckSearchRecord(TreeRec) then begin         // check if the node or one of his child match the search text
            DrawHighlight(TargetCanvas, CellRect, false);
            // IsBookmark=true  -> light blue
            exit;
         end;
   end;

   // Then apply API background color if exist
   ChangeBackgroundColor(TargetCanvas, CellRect, Column, TreeRec.FontDetails, (vsSelected in Node.States));
end;

// ----------------------------------------------------------------------------------------------------------------------

// AfterCellPaint is used to draw the level icon (warning/error/debug/...) and separator
procedure TFrm_Trace.vstMainAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
   CellText: String;
   TreeRec: PTreeRec;
   middle: Integer;
   c,ImageIndex: Integer;
   member: TMember;

begin

   // draw the level icon (warning/error/debug/...) here.
   // This cannot be done using the OnGetImageIndex because another imagelist is used for other column
   if (Column = COL_LEVELANDTYPE) and (IsMultiColTree = false) then begin
      TreeRec := Sender.GetNodeData(Node);

      if TreeRec.TreeIcon = -1 then
         ImageIndex := 24
      else
         ImageIndex := TreeRec.TreeIcon;

      Frm_Tool.ImageList1.Draw(TargetCanvas, 0, 0, ImageIndex);

      if TraceConfig.Framework_KindIconOnLeft then begin

        ImageIndex := -1;
        if TreeRec.Members <> nil then begin
           // get first SubMembers with special Viewer and display it
           for c := 0 to TreeRec.Members.SubMembers.Count - 1 do begin
              member := TMember(TreeRec.Members.SubMembers[c]);
              case member.ViewerKind of
                 CST_VIEWER_DUMP: begin
                       ImageIndex := 31;
                       break;
                    end; // dump viewer
                 CST_VIEWER_XML: begin
                       ImageIndex := 32;
                       break;
                    end; // xml viewer
                 CST_VIEWER_TABLE: begin
                       ImageIndex := 33;
                       break;
                    end; // table viewer
                 CST_VIEWER_STACK: begin
                       ImageIndex := 34;
                       break;
                    end; // stack
                 CST_VIEWER_BITMAP: begin
                       ImageIndex := 35;
                       break;
                    end; // bitmap viewer
                 CST_VIEWER_OBJECT: begin
                       ImageIndex := 36;
                       break;
                    end; // object structure
                 CST_VIEWER_VALUE: begin
                       ImageIndex := 37;
                       break;
                    end; // object value
                 CST_VIEWER_ENTER: begin
                       ImageIndex := 38;
                       break;
                    end; // enter method
                 CST_VIEWER_EXIT: begin
                       ImageIndex := 39;
                       break;
                    end; // exit method
                 CST_VIEWER_TXT: begin
                       ImageIndex := 40;
                       break;
                    end; // text added to default viewer
                else
                   begin ImageIndex := 15;
                      break;
                   end; // CST_VIEWER_NONE : draw the small dot indicate sub members
              end;
           end;
        end;
        Frm_Tool.ilActions.Draw(TargetCanvas, 20, 0, ImageIndex);
      end;
   end;

   // TreeRec := VstMain.GetNodeData(Node) ;
   VstMainGetText(Sender, Node, Column, ttStatic, CellText);
   // ttStatic is used to get the real text
   if IsSeparator(CellText) then begin
      TargetCanvas.Pen.Color := TargetCanvas.Font.Color; // clBlack;

      middle := CellRect.Bottom div 2;
      if copy(CellText, 1, 1) = '-' then begin
         TargetCanvas.MoveTo(0, middle);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle);
      end
      else begin // '='
         TargetCanvas.MoveTo(0, middle - 1);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle - 1);
         TargetCanvas.MoveTo(0, middle + 1);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle + 1);
      end;
   end;
end;

// ----------------------------------------------------------------------------------------------------------------------
// sort the tree. Compare 2 nodes on a specific column

procedure TFrm_Trace.vstMainCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
    TreeRec1, TreeRec2: PTreeRec;
    str1, str2: string;
    // ImageIndex1, ImageIndex2 : integer ;

    // --------------------
    function GetText(TreeRec: PTreeRec): string;
    var
       c: Integer;
    begin
       Result := '';
       if TreeRec.Columns = nil then
          exit;
       // number of substring can be less than the number of column
       if Column < TreeRec.Columns.Count then begin
          Result := TreeRec.Columns[Column];
          // if last col and remaining strings...
          if (Column = VstMain.Header.Columns.Count - 1) then begin
             for c := Column + 1 to TreeRec.Columns.Count - 1 do
                Result := Result + ' ' + #9 + TreeRec.Columns[c];
          end;
       end;
    end;

    // --------------------
    function CompareInteger(int1, int2: Integer): Integer;
    begin
       if int1 <= int2 then
          Result := -1
       else
          Result := 1;
    end;
    // --------------------

begin
   TreeRec1 := Sender.GetNodeData(Node1);
   TreeRec2 := Sender.GetNodeData(Node2);

   if Column = -1 then begin
      // no column : unsort or 2 record are the same
      if TreeRec1.OriginalOrder <= TreeRec2.OriginalOrder then
         Result := -1
      else
         Result := 1;
      exit;
   end;

   if IsMultiColTree then begin  // then isWatch too
      str1 := GetText(TreeRec1);
      str2 := GetText(TreeRec2);
      Result := CompareText(str1, str2);
   end
   else begin
      case Column of
         COL_LEVELANDTYPE:
            Result := CompareInteger(TreeRec1.TreeIcon, TreeRec2.TreeIcon);
         // image
         COL_TIME:
            Result := CompareText(TreeRec1.Time, TreeRec2.Time);
         COL_THID:
            Result := CompareText(TreeRec1.ThreadID, TreeRec2.ThreadID);
         COL_TRACE:
            Result := CompareText(TreeRec1.LeftMsg, TreeRec2.LeftMsg);
         COL_COMMENT:
            Result := CompareText(TreeRec1.RightMsg, TreeRec2.RightMsg);
      end;
   end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFrm_Trace.ShowFilter;
begin
   if filter = nil then
      filter := TFrmFilter.Create(self);     // create the filter and add it a empty filter row
   InitColumns() ;            // fill filter.columnsNameList with column names
   filter.FillColumns();      // fill combobox in each filter row with column names
   filter.ShowModal();
end;

// ----------------------------------------------------------------------------------------------------------------------

// fill combobox in each filter row with column names
procedure TFrm_Trace.InitColumns;
var
   c: Integer;
   title: string;
begin
   filter.chkCheckChildren.Visible := true;
   filter.vst := VstMain;
   filter.base := self;
   filter.ColumnNameList.Clear;
   if IsMultiColTree = false then begin
      filter.ColumnNameList.AddObject('Trace Kind', TObject(999)); // same as col 0, but force fill with predefined debug,warning,error
      filter.ColumnNameList.AddObject('Time'      , TObject(COL_TIME));
      filter.ColumnNameList.AddObject('Thread Id' , TObject(COL_THID));
      filter.ColumnNameList.AddObject('Traces'    , TObject(COL_TRACE));
      filter.ColumnNameList.AddObject('Comment'   , TObject(COL_COMMENT));
      filter.ColumnNameList.AddObject('Trace Info', TObject(998));
      // search in members
   end else begin  // IsMultiColTree or isWatch
      for c := 0 to VstMain.Header.Columns.Count - 1 do begin
         title := VstMain.Header.Columns[c].Text;
         if title = '' then
            title := 'Col ' + inttostr(c + 1);
         filter.ColumnNameList.AddObject(title, TObject(c));
      end;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrm_Trace.getMembers(Node: PVirtualNode): TMember;
var
   TreeRec: PTreeRec;
begin
   TreeRec := VstMain.GetNodeData(Node);
   Result := TreeRec.Members;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFrm_Trace.PanelGutterDblClick(Sender: TObject);
var
   Node: PVirtualNode;
   P: TPoint;
   index: Integer;
begin

   GetCursorPos(P);
   P := VstMain.ScreenToClient(P);
   Node := VstMain.GetNodeAt(0, P.Y);
   if Node = nil then
      exit;

   index := bookmarks.IndexOf(Node);
   if index = -1 then begin
      bookmarks.Add(Node);
   end
   else begin
      bookmarks.Delete(index);
   end;

   VstMain.InvalidateNode(Node);
end;

procedure TFrm_Trace.PanelLeftResize(Sender: TObject);
begin
//  InternalTrace('L-left: ' + inttostr(PanelLeft.Width) +
//                ', Right: ' + inttostr(PanelRight.Width) +
//                ', Form:' + inttostr(Width)
//                ) ;

   if (width < 300) then begin
      PanelRight.Width := width div 2;
   end else begin

      if (PanelLeft.Width < 100) then
         PanelRight.Width := width - 105 ; //PanelRight.Width + PanelLeft.Width - 100;

      if (PanelRight.Width < 100) then
         PanelRight.Width := 100;
   end;
end;

procedure TFrm_Trace.PanelTTracesCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   PanelRight.Width := Round(PanelTTraces.Width * rightPercent);
end;

procedure TFrm_Trace.VSplitterCanResize(Sender: TObject; var NewSize: Integer;  var Accept: Boolean);
begin
   rightPercent := NewSize / (PanelTTraces.Width - vsplitter.width);
   if (Width - NewSize < 105) then
      NewSize := Width - 105;
end;

// ------------------------------------------------------------------------------
// apply font change and return true if at least one font change is detected
// called by VstMainMeasureItem, VstMainPaintText, VstDetailMeasureItem and VstDetailPaintText
function TFrm_Trace.ChangeFontDetail(const IsTrace: boolean; const TargetCanvas: TCanvas; const Column: TColumnIndex;
   const FontDetails: TFontDetailArray; const selected: boolean): boolean;
var
   FontDetail: TFontDetail;
   c: Integer;
begin
   // force Font name and size
   if IsTrace then begin
      if IsWatch then begin // Watch Trace
         TargetCanvas.Font.Name := TraceConfig.Watches_Trace_FontName;
         TargetCanvas.Font.Size := TraceConfig.Watches_Trace_FontSize;
      end
      else begin // Framework trace
         TargetCanvas.Font.Name := TraceConfig.Framework_Trace_FontName;
         TargetCanvas.Font.Size := TraceConfig.Framework_Trace_FontSize;
      end;
   end
   else begin
      if IsWatch then begin // watch info
         TargetCanvas.Font.Name := TraceConfig.Watches_Info_FontName;
         TargetCanvas.Font.Size := TraceConfig.Watches_Info_FontSize;
      end
      else begin // Framework info
         TargetCanvas.Font.Name := TraceConfig.Framework_Info_FontName;
         TargetCanvas.Font.Size := TraceConfig.Framework_Info_FontSize;
      end;
   end;

   // then check for special font name, size and color
   Result := false;
   if TraceConfig.Dark_Enabled and selected then  // Focused or not
       TargetCanvas.Font.Color := TraceConfig.Dark_SelectedTextColor;   //  clYellow, White , ...

   for c := 0 to length(FontDetails) - 1 do begin
      FontDetail := FontDetails[c];
      if (FontDetail.ColId = Column) or (FontDetail.ColId = -1) then begin
         Result := true; // at least one font change is detected
         if FontDetail.Bold then
            TargetCanvas.Font.Style := [fsBold];

         if FontDetail.Italic then
            TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic];

         if FontDetail.Name <> '' then
            TargetCanvas.Font.Name := FontDetail.Name;

         if FontDetail.Size <> 0 then
            TargetCanvas.Font.Size := FontDetail.Size;

         if (selected = false) and (FontDetail.Color <> -1) then
            TargetCanvas.Font.Color := FontDetail.Color;
      end;
   end;
end;

// ------------------------------------------------------------------------------
// apply font change and return true if at least one font change is detected
// called by VstMainMeasureItem, VstMainPaintText, VstDetailMeasureItem and VstDetailPaintText
function TFrm_Trace.ChangeBackgroundColor(const TargetCanvas: TCanvas;
   const CellRect: TRect; const Column: TColumnIndex;
   const FontDetails: TFontDetailArray; const selected: boolean): boolean;
var
   FontDetail: TFontDetail;
   c: Integer;
begin

   // then check for special font name, size and color
   Result := false;
   for c := 0 to length(FontDetails) - 1 do begin
      FontDetail := FontDetails[c];
      if (FontDetail.ColId = Column) or (FontDetail.ColId = -1) then begin
         Result := true; // at least one font change is detected
         if FontDetail.BackgroundColor <> 0 then begin
            TargetCanvas.Brush.Color := FontDetail.BackgroundColor;
            TargetCanvas.FillRect(CellRect);
         end;
         // if (selected = false) and (FontDetail.Color <> -1)  then
         // TargetCanvas.font.Color := FontDetail.Color ;
      end;
   end;
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.vstMainMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);
var
   TreeRec: PTreeRec;
   c: Integer;
   newNodeHeight: Integer;
   CellHeight: Integer;
   FontDetailChanged: boolean;
   NewDefaultNodeHeight: Integer;

begin
   TreeRec := Sender.GetNodeData(Node);
   newNodeHeight := 0;

   if IsWatch then
      NewDefaultNodeHeight := TraceConfig.Watches_Trace_NodeHeight
   else
      NewDefaultNodeHeight := TraceConfig.Framework_Trace_NodeHeight;

   for c := 0 to TVirtualStringTree(Sender).Header.Columns.Count - 1 do begin
      // get font formatings for that colummn
      FontDetailChanged := ChangeFontDetail(
        true, TargetCanvas, c, TreeRec.FontDetails, true);

      if (FontDetailChanged = false) then begin
         // no special formating. newNodeHeight must be at least the default node height
         newNodeHeight := Max(newNodeHeight, NewDefaultNodeHeight)
      end
      else begin
         // get the cell height
         CellHeight := TVirtualStringTree(Sender).ComputeNodeHeight
            (TargetCanvas, Node, c);
         newNodeHeight := Max(newNodeHeight, CellHeight);
      end;
   end;
   if newNodeHeight = 0 then
      NodeHeight := NewDefaultNodeHeight
   else
      NodeHeight := newNodeHeight;
end;

// PaintText is used to apply font change
procedure TFrm_Trace.vstMainPaintText(Sender: TBaseVirtualTree;
   const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
   TreeRec: PTreeRec;
begin
   TreeRec := Sender.GetNodeData(Node);
   // force font
   ChangeFontDetail(
      {IsTrace}      true,
      {TargetCanvas} TargetCanvas,
      {Column}       Column,
      {FontDetails}  TreeRec.FontDetails,
      {selected}     (vsSelected in Node.States));
end;

// ------------------------------------------------------------------------------

// apply framework and watches fonts
procedure TFrm_Trace.ApplyFont;
var
   FontName: string;
   FontSize: Integer;
   NodeHeight: Integer;
   InfoFontName: string;
   InfoFontSize: Integer;
begin
   if IsWatch then begin
      FontName := TraceConfig.Watches_Trace_FontName;
      FontSize := TraceConfig.Watches_Trace_FontSize;
      NodeHeight := TraceConfig.Watches_Trace_NodeHeight;
      InfoFontName := TraceConfig.Watches_Info_FontName;
      InfoFontSize := TraceConfig.Watches_Info_FontSize;
   end
   else begin
      FontName := TraceConfig.Framework_Trace_FontName;
      FontSize := TraceConfig.Framework_Trace_FontSize;
      NodeHeight := TraceConfig.Framework_Trace_NodeHeight;
      InfoFontName := TraceConfig.Framework_Info_FontName;
      InfoFontSize := TraceConfig.Framework_Info_FontSize;
   end;

   VstMain.BeginUpdate;
   VstMain.Font.Name := FontName;
   VstMain.Font.Size := FontSize;
   VstMain.DefaultNodeHeight := NodeHeight;
   VstMain.ReinitChildren(nil, true);
   VstMain.EndUpdate;

   // to do : for each Tframe_Classic
   VstDetail.BeginUpdate;
   VstDetail.Font.Name := InfoFontName;
   VstDetail.Font.Size := InfoFontSize;
   if not IsWatch then
      VstDetail.DefaultNodeHeight := TraceConfig.Framework_Info_NodeHeight;
   VstDetail.ReinitChildren(nil, true);
   VstDetail.EndUpdate;
end;

procedure TFrm_Trace.ApplyTheme;
const
  // Original light-theme accent colors (from .dfm)
  LightColTraces = TColor(16705515);  // lavender
  LightColDetail = TColor(16117479);  // blue-gray
  // Dark-theme equivalents (subtle tint on Carbon ~$1E1E1E background)
  DarkColTraces  = TColor($00201018);  // very dark warm-purple
  DarkColDetail  = TColor($00151520);  // very dark blue-gray
var
    //StyleName: string;
    j : integer;
    ColTraces, ColDetail: TColor;
begin
    TFrm_Trace.InternalTrace ('TFrm_Trace.ApplyTheme ' + caption );

    if TraceConfig.Dark_Enabled then
    begin
      ColTraces := DarkColTraces;
      ColDetail := DarkColDetail;
    end else begin
      ColTraces := LightColTraces;
      ColDetail := LightColDetail;
    end;

    // vstMain
    // -------

    if IsMultiColTree then begin
        for j := 0 to vstMain.Header.Columns.Count-1 do
          vstMain.Header.Columns[j].Color := ColDetail;

    end else begin
        // column 3 (Traces)
        if vstMain.Header.Columns.Count > COL_TRACE then
          vstMain.Header.Columns[COL_TRACE].Color := ColTraces;

        // column 4 (Comment)
        if vstMain.Header.Columns.Count > COL_COMMENT then
          vstMain.Header.Columns[COL_COMMENT].Color := ColTraces;
    end;


    Frm_Tool.ApplyVstTheme(vstMain);

    // VstDetail
    // -------

    if VstDetail <> nil then
    begin
      VstDetail.Color := ColDetail;
      for J := 0 to VstDetail.Header.Columns.Count - 1 do
        if VstDetail.Header.Columns[J].Color <> clDefault then
          VstDetail.Header.Columns[J].Color := ColDetail;
      Frm_Tool.ApplyVstTheme(VstDetail);
    end;

    // Frame Memo
    // -------

    if TreeDetailFrame is Tframe_Classic then
       Tframe_Classic(TreeDetailFrame).frameMemo.ApplySynMemoTheme ();
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.mnuExpandAllClick(Sender: TObject);
begin
   VstMain.FullExpand();
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.mnuCollapseAllClick(Sender: TObject);
begin
   VstMain.FullCollapse();
end;

// ------------------------------------------------------------------------------

procedure TFrm_Trace.Print();
begin
   FrmPrintPreview.initialize(VstMain, self);
   FrmPrintPreview.ShowModal;
   // VstMain.SetFocus() ;
   // if VstMain.GetFirstSelected() <> nil then
   // VstMain.FocusedNode := VstMain.GetFirstSelected() ;
end;

// ------------------------------------------------------------------------------

end.
