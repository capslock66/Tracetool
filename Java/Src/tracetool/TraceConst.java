/*
 * TraceConst.java
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

/**
 * Internal Use.<p>Constant for the viewer communication
 * @author tpa
 */
public class TraceConst {


   /** the windows constant to use to send bloc of data across process */
   /**  identification code 'traceTool'. Other code are discarded by the server*/
   public static final int WMD                   = 123 ;
   /** Use the default Icon on the gutter for the trace */
   public static final int CST_ICO_DEFAULT       = -1 ;
   /** Use the 'form' Icon on the gutter for the trace */
   public static final int CST_ICO_FORM          = 0 ;
   /** Use the 'component' Icon on the gutter for the trace */
   public static final int CST_ICO_COMPONENT     = 1 ;
   /** Use the 'control' Icon on the gutter for the trace */
   public static final int CST_ICO_CONTROL       = 3 ;
   /** Use the 'property' Icon on the gutter for the trace */
   public static final int CST_ICO_PROP          = 5 ;
   /** Use the 'memu' Icon on the gutter for the trace */
   public static final int CST_ICO_MENU          = 15 ;
   /** Use the 'menu item' Icon on the gutter for the trace */
   public static final int CST_ICO_MENU_ITEM     = 16 ;
   /** Use the 'Collection Item' Icon on the gutter for the trace */
   public static final int CST_ICO_COLLECT_ITEM  = 21 ;
   /** Use the 'warning' Icon on the gutter for the trace */
   public static final int CST_ICO_WARNING       = 22 ;
   /** Use the 'Error' Icon on the gutter for the trace */
   public static final int CST_ICO_ERROR         = 23 ;
   /** Use the 'Info' Icon on the gutter for the trace , default*/
   public static final int CST_ICO_INFO          = 24 ;    

   // viewer kind

   /** viewer kind : default viewer, no icon*/
   public static final int CST_VIEWER_NONE   = 0;
   /** viewer kind : dump viewer */
   public static final int CST_VIEWER_DUMP   = 1;
   /** viewer kind : xml viewer */
   public static final int CST_VIEWER_XML    = 2;
   /** viewer kind : table viewer */
   public static final int CST_VIEWER_TABLE  = 3;
   /** viewer kind : stack */
   public static final int CST_VIEWER_STACK  = 4;
   /** viewer kind : bitmap viewer */
   public static final int CST_VIEWER_BITMAP = 5;
   /** viewer kind : object structure */
   public static final int CST_VIEWER_OBJECT = 6;
   /** viewer kind : object value */
   public static final int CST_VIEWER_VALUE  = 7;
   /** viewer kind : enter method */
   public static final int CST_VIEWER_ENTER  = 8;
   /** viewer kind : exit method */
   public static final int CST_VIEWER_EXIT   = 9;
   /** viewer kind : text added to default viewer */
   public static final int CST_VIEWER_TXT    = 10;

   // plugin const

   /**  Ask to receive OnAction event */
   public static final int CST_PLUG_ONACTION        = 1 ;
   /** Ask to receive OnBeforeDelete event */
   public static final int CST_PLUG_ONBEFOREDELETE  = 2 ;
   /** Ask to receive OnTimer event */
   public static final int CST_PLUG_ONTIMER         = 4 ;

   // resource kind

   /** Button on right */
   public static final int CST_RES_BUT_RIGHT        = 1 ;
   /** Button on left */
   public static final int CST_RES_BUT_LEFT         = 2 ;
   /** Label on right */
   public static final int CST_RES_LABEL_RIGHT      = 3 ;
   /** Label on right HyperLink */
   public static final int CST_RES_LABELH_RIGHT     = 4 ;
   /** Label on left */
   public static final int CST_RES_LABEL_LEFT       = 5 ;
   /** Label on left hyperlink */
   public static final int CST_RES_LABELH_LEFT      = 6 ;
   /** Item menu in the Actions Menu */
   public static final int CST_RES_MENU_ACTION      = 7 ;
   /** Item menu in the Windows Menu. Call CreateResource on the main win trace to create this menu item */
   public static final int CST_RES_MENU_WINDOW      = 8 ;

   // resource id

   /** cut same as copy then delete  */
   public static final int CST_ACTION_CUT           = 1 ;
   /** copy  */
   public static final int CST_ACTION_COPY          = 2 ;
   /** delete selected  */
   public static final int CST_ACTION_DELETE        = 3 ;
   /** select all  */
   public static final int CST_ACTION_SELECT_ALL    = 4 ;
   /** resize columns  */
   public static final int CST_ACTION_RESIZE_COLS   = 5 ;
   /** view trace info  */
   public static final int CST_ACTION_VIEW_INFO     = 6 ;
   /** view properties  */
   public static final int CST_ACTION_VIEW_PROP     = 7 ;
   /** Pause  */
   public static final int CST_ACTION_PAUSE         = 8 ;
   /** SaveToFile  */
   public static final int CST_ACTION_SAVE          = 9 ;
   /** clear all  */
   public static final int CST_ACTION_CLEAR_ALL     = 10 ;
   /** Close win  */
   public static final int CST_ACTION_CLOSE_WIN     = 11 ;
   /** resume from Pause  */
   public static final int CST_ACTION_RESUME        = 12 ;
   /** TracesInfo label  */
   public static final int CST_ACTION_LABEL_INFO    = 20 ;
   /** LabelLogFile label  */
   public static final int CST_ACTION_LABEL_LOGFILE = 21 ;
   /** View Main trace  */
   public static final int CST_ACTION_VIEW_MAIN     = 50 ;
   /** ODS  */
   public static final int CST_ACTION_VIEW_ODS      = 51 ;
   /** XML trace -> Tracetool XML traces  */
   public static final int CST_ACTION_OPEN_XML      = 52 ;
   /** Event log  */
   public static final int CST_ACTION_EVENTLOG      = 53 ;
   /** Tail  */
   public static final int CST_ACTION_TAIL          = 54 ;

   // COMMANDS
   //================   
   
   // INTERNAL
   //--------------------------------------------------------------------------
   
   /**VIEWER INTERNAL : enter debug mode  */
   public static final int CST_ENTER_DEBUG_MODE                                = 107;   
   /**VIEWER INTERNAL : leave debug mode   */
   public static final int CST_LEAVE_DEBUG_MODE                                = 108;    
   /**VIEWER INTERNAL : Open tail file . Param : file name */
   public static final int CST_OPEN_TAIL                                       = 109;  
   /**VIEWER INTERNAL : Open xml file on a new window (don't confuse with CST_LOADXML).Param : file name   */
   public static final int CST_OPEN_XML                                        = 113;  
   /** VIEWER INTERNAL : the user interface ask to retrieve an object   */
   public static final int CST_GET_OBJECT                                      = 700;   
   /** VIEWER INTERNAL : Flush remaining traces to server. param : event id*/
   public static final int CST_FLUSH                                           = 800 ;  

   // Wintrace / WinWatch. New commands should be added before 80
   //--------------------------------------------------------------------------
   /** WinTrace.GotoFirstNode()  */
   public static final int CST_GOTO_FIRST_NODE                                 = 80;    
   /** WinTrace.GotoLastNode()  */
   public static final int CST_GOTO_LAST_NODE                                  = 81;    
   /** WinTrace.FindNext(forward)  */
   public static final int CST_FIND_NEXT                                       = 82;    
   /** WinTrace.GotoBookmark(pos) */
   public static final int CST_GOTO_BOOKMARK                                   = 83;    
   /** WinTrace.ClearBookmark()  */
   public static final int CST_CLEAR_BOOKMARK                                  = 84;    
   /** WinTrace.ClearFilter()  */
   public static final int CST_CLEAR_FILTER                                    = 85;    
   /** WinTrace.AddFilter(column,compare,text)  */
   public static final int CST_ADD_FILTER                                      = 86;       
   /** WinTrace.ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren)  */
   public static final int CST_APPLY_FILTER                                    = 87;    
   /** Columns widths */
   public static final int CST_TREE_COLUMNWIDTH                                = 93  ;
   /** change the tree to display multiple column */
   public static final int CST_TREE_MULTI_COLUMN                               = 95  ;
   /** change the columns titles */
   public static final int CST_TREE_COLUMNTITLE                                = 96  ;
   /** display tree windows */
   public static final int CST_DISPLAY_TREE                                    = 97  ;   
   /**  new name of the tree*/
   public static final int CST_TREE_NAME                                       = 98  ;   
   /** the tree to use for other command */
   public static final int CST_USE_TREE                                        = 99  ;  
   /** Clear all nodes on the main window */
   public static final int CST_CLEAR_ALL                                       = 104 ;  
   /** Close the window (wintrace or winwatch) */
   public static final int CST_CLOSE_WIN                                       = 105;    
   /** Watch Window name , param : window name*/
   public static final int CST_WINWATCH_NAME                                   = 110 ;  
   /** Watch Window ID , param : Window id*/
   public static final int CST_WINWATCH_ID                                     = 111 ;    
   /** watch name,  param : watch name */
   public static final int CST_WATCH_NAME                                      = 112 ;   
   /** Save to text file, parameter : filename */
   public static final int CST_SAVETOTEXT                                      = 559 ;  
   /** Save to XML file, parameter : filename */
   public static final int CST_SAVETOXML                                       = 560 ;  
   /** load an XML file to the current wintrace */
   public static final int CST_LOADXML                                         = 561 ;   
   /** Set log file */
   public static final int CST_LOGFILE                                         = 562 ;   
   
   // Wintrace plugins
   //--------------------------------------------------------------------------
   
   /** link a wintrace to a plugin */
   public static final int CST_LINKTOPLUGIN                                    = 563 ;
   /** create a resource on a wintrace */
   public static final int CST_CREATE_RESOURCE                                 = 564 ;
   /** set the text resource */
   public static final int CST_SET_TEXT_RESOURCE                               = 565 ;
   /** disable a resource */
   public static final int CST_DISABLE_RESOURCE                                = 566 ;

   // TTrace
   //--------------------------------------------------------------------------
   
   /**TTrace.Find (text, bool Sensitive, bool WholeWord , bool highlight ) */
   public static final int CST_FIND_TEXT                                       = 100 ;    
   /** The command to bring the trace tool to front */
   public static final int CST_SHOW                                            = 102 ;  
   /** Close the viewer (shutdown) */
   public static final int CST_CLOSE_VIEWER                                    = 106;   

   // Node
   //--------------------------------------------------------------------------

   /** the unique ID (from the server point of view) of the node (preferably a GUID) */
   public static final int CST_TRACE_ID                                        = 101 ;   
   /** the index of the Icon to use (CST_ICO_INFO, CST_ICO_WARNING,...) */
   public static final int CST_ICO_INDEX                                       = 103 ;   
   /** ITraceNode.GotoNextSibling ()  */
   public static final int CST_GOTO_NEXTSIBLING                                = 114;   
   /** ITraceNode.GotoPrevSibling ()  */
   public static final int CST_GOTO_PREVSIBLING                                = 115; 
   /** ITraceNode.GotoFirstChild  ()  */
   public static final int CST_GOTO_FIRST_CHILD                                = 116;   
   /** ITraceNode.GotoLastChild   ()  */
   public static final int CST_GOTO_LAST_CHILD                                 = 117;  
   /** ITraceNode.SetBookmark (bool enabled)  */
   public static final int CST_SET_BOOKMARK                                    = 122;   
   /** ITraceNode.SetVisible  (visible)  */
   public static final int CST_VISIBLE_NODE                                    = 123; 
   /** Delete a node on the viewer */
   public static final int CST_CLEAR_NODE                                      = 300 ;  
   /** Clear children nodes on the viewer */
   public static final int CST_CLEAR_SUBNODES                                  = 301 ;   
   /** The Thread ID of the sender thread (optional) Used when tracing multiple thread */
   public static final int CST_THREAD_ID                                       = 302 ;   
   /** The process name (optional) . Used when tracing multiple process */
   public static final int CST_PROCESS_NAME                                    = 303 ;   
   /** The time of trace */
   public static final int CST_MESSAGE_TIME                                    = 304 ;  
   /** Thread name */
   public static final int CST_THREAD_NAME                                     = 305 ;  
   /** Client Ip adress*/
   public static final int CST_IP                                              = 306;   
     /** Command to create a new trace node */
   public static final int CST_NEW_NODE                                        = 550 ;  
   /** The left message ("traces column") */
   public static final int CST_LEFT_MSG                                        = 551 ;  
   /** the right message ("Comment column") */
   public static final int CST_RIGHT_MSG                                       = 552 ;  
   /** set the node as 'Selected' by the user */
   public static final int CST_SELECT_NODE                                     = 553 ;  
   /** use an existing node  */
   public static final int CST_USE_NODE                                        = 555 ;  
   /** The left message to append to "traces column" */
   public static final int CST_APPEND_LEFT_MSG                                 = 556 ;  
   /** The right message to append to "Comment column" */
   public static final int CST_APPEND_RIGHT_MSG                                = 557 ;  
   /** Focus to the node  */
   public static final int CST_FOCUS_NODE                                      = 558 ;  
   /** Font detail */
   public static final int CST_FONT_DETAIL                                     = 567 ; 
   /** Background color */
   public static final int CST_BACKGROUND_COLOR                                = 568;   
 
   // Members
   //--------------------------------------------------------------------------
   
   /** Command to create a member for the current trace node */
   public static final int CST_CREATE_MEMBER                                   = 500 ; 
   /** Member Font detail  */
   public static final int CST_MEMBER_FONT_DETAIL                              = 501;   
   /** The text of the second member column */
   public static final int CST_MEMBER_COL2                                     = 502 ;  
   /** Viewer kind id */
   public static final int CST_MEMBER_VIEWER_KIND                              = 503;  
   /** The text of the third member column */
   public static final int CST_MEMBER_COL3                                     = 504 ;  
   /** Add the member. Close the previous CST_CREATE_MEMBER */
   public static final int CST_ADD_MEMBER                                      = 505 ; 

}
