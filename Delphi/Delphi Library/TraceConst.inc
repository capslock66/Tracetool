// unit TraceConst.inc ;
// used by viewer and client

// version 12.5

Const
   RECIEVETIMEOUT           = 1000; // milliseconds
   WMD                      = 123 ;  // identification code 'traceTool'
   WM_SEND_TO_DEBUG         = WM_USER + 1 ;
   WM_FROM_DEBUG            = WM_USER + 2 ;

   // Icones
   CST_ICO_DEFAULT          = -1 ;    // default
   CST_ICO_FORM             = 0 ;     // Form
   CST_ICO_COMPONENT        = 1 ;     // component
   CST_ICO_CONTROL          = 3 ;     // control
   CST_ICO_PROP             = 5 ;     // property
   CST_ICO_MENU             = 15 ;    // menu
   CST_ICO_MENU_ITEM        = 16 ;    // item menu
   CST_ICO_COLLECT_ITEM     = 21 ;    // collection
   CST_ICO_WARNING          = 22 ;    // Warning
   CST_ICO_ERROR            = 23 ;    // Error
   CST_ICO_INFO             = 24 ;    // Information (default)

   // plugin
   CST_PLUG_ONACTION        = 1 ;     // Action performed event
   CST_PLUG_ONBEFOREDELETE  = 2 ;     // before delete event
   CST_PLUG_ONTIMER         = 4 ;     // timer event

   // viewer kind
   CST_VIEWER_NONE          = 0 ;     // default viewer, no icon
   CST_VIEWER_DUMP          = 1 ;     // dump viewer
   CST_VIEWER_XML           = 2 ;     // xml viewer
   CST_VIEWER_TABLE         = 3 ;     // table viewer
   CST_VIEWER_STACK         = 4 ;     // stack
   CST_VIEWER_BITMAP        = 5 ;     // bitmap viewer
   CST_VIEWER_OBJECT        = 6 ;     // object structure
   CST_VIEWER_VALUE         = 7 ;     // object value
   CST_VIEWER_ENTER         = 8 ;     // enter method
   CST_VIEWER_EXIT          = 9 ;     // exit method
   CST_VIEWER_TXT           = 10 ;    // text added to default viewer

   // resource kind
   CST_RES_BUT_RIGHT        = 1 ;     // Button on right
   CST_RES_BUT_LEFT         = 2 ;     // Button on left
   CST_RES_LABEL_RIGHT      = 3 ;     // Label on right
   CST_RES_LABELH_RIGHT     = 4 ;     // Label on right HyperLink
   CST_RES_LABEL_LEFT       = 5 ;     // Label on left
   CST_RES_LABELH_LEFT      = 6 ;     // Label on left hyperlink
   CST_RES_MENU_ACTION      = 7 ;     // Item menu in the Actions Menu
   CST_RES_MENU_WINDOW      = 8 ;     // Item menu in the Windows Menu. Call CreateResource on the main win trace to create this menu item

   // resource id
   CST_ACTION_CUT           = 1 ;     // cut same as copy then delete
   CST_ACTION_COPY          = 2 ;     // copy
   CST_ACTION_DELETE        = 3 ;     // delete selected
   CST_ACTION_SELECT_ALL    = 4 ;     // select all
   CST_ACTION_RESIZE_COLS   = 5 ;     // resize columns
   CST_ACTION_VIEW_INFO     = 6 ;     // view trace info
   CST_ACTION_VIEW_PROP     = 7 ;     // view properties
   CST_ACTION_PAUSE         = 8 ;     // Pause on
   CST_ACTION_SAVE          = 9 ;     // SaveToFile
   CST_ACTION_CLEAR_ALL     = 10 ;    // clear all
   CST_ACTION_CLOSE_WIN     = 11 ;    // Close win
   CST_ACTION_RESUME        = 12 ;    // resume from Pause 

   CST_ACTION_LABEL_INFO    = 20 ;    // TracesInfo label
   CST_ACTION_LABEL_LOGFILE = 21 ;    // LabelLogFile label
   CST_ACTION_VIEW_MAIN     = 50 ;    // View Main trace
   CST_ACTION_VIEW_ODS      = 51 ;    // ODS
   CST_ACTION_OPEN_XML      = 52 ;    // XML trace -> Tracetool XML traces
   CST_ACTION_EVENTLOG      = 53 ;    // Event log
   CST_ACTION_TAIL          = 54 ;    // Tail

   // Commands. take care of duplicate id when creating new commands
   //--------------------------------------------------------------------

   // INTERNAL
   //--------------------------------------------------------------------------
   CST_ENTER_DEBUG_MODE     = 107 ;   // VIEWER INTERNAL : enter debug mode                                          Param : none
   CST_LEAVE_DEBUG_MODE     = 108 ;   // VIEWER INTERNAL : leave debug mode                                          Param : none
   CST_OPEN_TAIL            = 109 ;   // VIEWER INTERNAL : Open tail file                                            Param : file name
   CST_OPEN_XML             = 113 ;   // VIEWER INTERNAL : Open xml file on a new window (don't confuse with CST_LOADXML)            Param : file name
   CST_GET_OBJECT           = 700 ;   // VIEWER INTERNAL : the user interface ask to retreive an object              param : node
   CST_FLUSH                = 800 ;   // CLIENT INTERNAL : interpreted by the sender thread (not to be send)         param : timeout

   // wintrace / WinWatch. New commands should be added before 80
   //--------------------------------------------------------------------------
   CST_GOTO_FIRST_NODE      = 80 ;    // WinTrace.GotoFirstNode()                                                    param : node
   CST_GOTO_LAST_NODE       = 81 ;    // WinTrace.GotoLastNode() ;                                                   param : node
   CST_FIND_NEXT            = 82 ;    // WinTrace.FindNext(forward) ;                                                param : node
   CST_GOTO_BOOKMARK        = 83 ;    // WinTrace.GotoBookmark(pos : integer);
   CST_CLEAR_BOOKMARK       = 84 ;    // WinTrace.ClearBookmark();                                                   param : node
   CST_CLEAR_FILTER         = 85 ;    // WinTrace.ClearFilter() ;                                                    param : node
   CST_ADD_FILTER           = 86 ;    // WinTrace.AddFilter(column,compare,text : string) ;
   CST_APPLY_FILTER         = 87 ;    // WinTrace.ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren) ;             param : integer (3 bools)
   CST_TREE_COLUMNWIDTH     = 93  ;   // WinTrace.setColumnsWidth (Widths)       : change the columns widths         Param : string
   CST_TREE_MULTI_COLUMN    = 95  ;   // WinTrace.setMultiColumn  (MainColIndex) : display multiple column.          Param : integer
   CST_TREE_COLUMNTITLE     = 96  ;   // WinTrace.setColumnsTitle (Titles)       : change the columns titles         Param : string
   CST_DISPLAY_TREE         = 97  ;   // WinTrace.DisplayWin ()                  : display tree windows              Param : none
   CST_TREE_NAME            = 98  ;   // WinTrace.create(WinTraceID,WinTraceText): Tree Name                         Param : WinTraceText
   CST_USE_TREE             = 99  ;   // WinTrace : Specify the tree to use for other command.                       param : Id : string
   CST_CLEAR_ALL            = 104 ;   // WinTrace.ClearAll() Clear all traces in the current wintrace                param : none
   CST_CLOSE_WIN            = 105 ;   // WinTrace.Close() Close winwatch or wintrace                                 Param : none
   CST_WINWATCH_NAME        = 110 ;   // WinWatch.create (WinWatchID, WinWatchText) : Watch Window name              Param : name
   CST_WINWATCH_ID          = 111 ;   // WinWatch : Watch Window ID                                                  Param : ID
   CST_WATCH_NAME           = 112 ;   // WinWatch.Send(WatchName:string,Obj) watch name                              Param : name
   CST_SAVETOTEXT           = 559 ;   // WinTrace.SaveToTextfile (Filename) save to text file.                       param : filename
   CST_SAVETOXML            = 560 ;   // WinTrace.SaveToXml (Filename,styleSheet) save to XML file.                  param : filename and style sheet
   CST_LOADXML              = 561 ;   // WinTrace.LoadXml (Filename) load an XML file to the current wintrace        param : filename
   CST_LOGFILE              = 562 ;   // WinTrace.setLogFile (Filename,mode,maxLines);define the log file.           param : mode and filename
   CST_LINKTOPLUGIN         = 563 ;   // link a wintrace to a plugin                                                 param : flags (int) , plugin name (string)
   CST_CREATE_RESOURCE      = 564 ;   // create a resource on a wintrace                                             param : resource id ,resource type(int),resource width , resource text
   CST_SET_TEXT_RESOURCE    = 565 ;   // set the text resource                                                       param : resource id (int), resource text (string)
   CST_DISABLE_RESOURCE     = 566 ;   // disable a resource                                                          param : resource id (int)


   // TTrace
   //--------------------------------------------------------------------------
   CST_FIND_TEXT            = 100 ;   // TTrace.Find (text, bool Sensitive, bool WholeWord , bool highlight )        param : int (Sensitive+WholeWord+highlight) , string
   CST_SHOW                 = 102 ;   // TTrace.Show(IsVisible: boolean) : Show or hide the viewer                   param : 1 : show.  0 : hide
   CST_CLOSE_VIEWER         = 106 ;   // TTrace.CloseViewer() : Quit tracetool                                       Param : none

   // Node
   //--------------------------------------------------------------------------
   CST_TRACE_ID             = 101 ;   // Trace Id (CLSID for example)                                                param : id
   CST_ICO_INDEX            = 103 ;   // Trace type, identified by his icon                                          param : image index
   CST_GOTO_NEXTSIBLING     = 114 ;   // ITraceNode.GotoNextSibling ()                                               param : node
   CST_GOTO_PREVSIBLING     = 115 ;   // ITraceNode.GotoPrevSibling ()                                               param : node
   CST_GOTO_FIRST_CHILD     = 116 ;   // ITraceNode.GotoFirstChild  ()                                               param : node
   CST_GOTO_LAST_CHILD      = 117 ;   // ITraceNode.GotoLastChild   ()                                               param : node
   CST_SET_BOOKMARK         = 122 ;   // ITraceNode.SetBookmark (bool enabled)                                       param : int
   CST_SET_BOOKMARK2        = 122 ;   // same as CST_SET_BOOKMARK but give problem on C++ builder !!!
   CST_VISIBLE_NODE         = 123 ;   // ITraceNode.SetVisible  (bool visible)                                       param : int
   CST_CLEAR_NODE           = 300 ;   // TraceNode.Delete() : clear node                                             param : the node to clear
   CST_CLEAR_SUBNODES       = 301 ;   // TraceNode.DeleteChildren() : clear sub nodes                                param : the parent node
   CST_THREAD_ID            = 302 ;   // thread id.                                                                  param : thread ID
   CST_PROCESS_NAME         = 303 ;   // Process name                                                                param : process name
   CST_MESSAGE_TIME         = 304 ;   // message time                                                                param : the time of the message
   CST_THREAD_NAME          = 305 ;   // thread name                                                                 param : thread name (java)
   CST_IP                   = 306 ;   // client ip adress                                                            param : client IP adress
   CST_NEW_NODE             = 550 ;   // create new node                                                             param : parent node ID
   CST_LEFT_MSG             = 551 ;   // set left message                                                            param : left msg
   CST_RIGHT_MSG            = 552 ;   // set right message                                                           param : right msg
   CST_SELECT_NODE          = 553 ;   // set the node as 'Selected' by the user.                                     param : Node id
   CST_GET_NODE             = 554 ;   // return the node id                                                          param : id
   CST_USE_NODE             = 555 ;   // set the current to to update. Called before some other functions            param : Node id
   CST_APPEND_LEFT_MSG      = 556 ;   // append message to left message                                              param : left msg to append
   CST_APPEND_RIGHT_MSG     = 557 ;   // append message to right message                                             param : right msg to append
   CST_FOCUS_NODE           = 558 ;   // Focus to the node.                                                          param : node
   CST_FONT_DETAIL          = 567 ;   // TraceNode.SetFontDetail(...) set font detail                                param : ColId Bold Italic Color size  Fontname
   CST_BACKGROUND_COLOR     = 568 ;   // TraceNode.SetBackgroundColor() set background color                         param : background color

   // Members
   //--------------------------------------------------------------------------
   CST_CREATE_MEMBER        = 500 ;   // create member                                                               param : Member name
   CST_MEMBER_FONT_DETAIL   = 501 ;   // set font detail                                                             param : ColId Bold Italic Color size  Fontname
   CST_MEMBER_COL2          = 502 ;   // member col2                                                                 param : info col 2
   CST_MEMBER_VIEWER_KIND   = 503 ;   // member viewer kind                                                          param : viewer id
   CST_MEMBER_COL3          = 504 ;   // member col3                                                                 param : info col 3
   CST_ADD_MEMBER           = 505 ;   // add member to upper level.                                                  Param : none




// implementation
//
// end.
