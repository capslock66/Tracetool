// WinTrace.cs
//
// WinTrace represent a windows tree where you put traces
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//
// Change the tracetool project option ("conditional compilation constant") to specify the target dot net version :
// NETF1  (dot net framework 1)          , NETF2 ((dot net framework 2) ,
// NETCF1 (dot net compact framework 1)  , NETCF2 (dot net compact framework 2) , NETCF3 (dot net compact framework 3)

using System;
using System.Collections;                 // ArrayList, queue

// generic start in F2
#if (!NETCF1 && !NETF1)
using System.Collections.Generic;
#endif

namespace TraceTool
{
   /// <summary>
   /// WinTrace represent a windows tree where you put traces
   /// </summary>
   public class WinTrace : TraceToSend
   {
      internal TraceToSend fWarning ;
      internal TraceToSend fError;
      internal TraceToSend fDebug;

      //------------------------------------------------------------------------------

      /// <summary>
      /// WinTrace constructor : you can map a WinTrace to an existing window
      /// Nothing is send to the viewer
      /// </summary>
      public WinTrace()
      {
         CreateNodes() ;
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// WinTrace constructor. The Window Trace is create on the viewer (if not already done)
      /// </summary>
      /// <param name="WinTraceID">Required window trace Id. If empty, a guid will be generated</param>
      /// <param name="WinTraceText">The Window Title on the viewer.If empty, a default name will be used</param>
      public WinTrace (string WinTraceID , string WinTraceText)
      {
         if (WinTraceID == null || WinTraceID == "")
            Id = Helper.NewGuid ().ToString() ;
         else
            Id = WinTraceID ;

         CreateNodes();

         if (WinTraceID != null && WinTraceID == "_")
            return ;  // don't create new window on the viewer

         if (WinTraceText == null || WinTraceText == "")
            WinTraceText = Id ;

         // create the trace window
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_TREE_NAME, WinTraceText));
         TTrace.SendToWinTraceClient (CommandList,this.Id);
      }

      //------------------------------------------------------------------------------

      // common to the 2 others constructors
      internal void CreateNodes()
      {
         IconIndex = TraceConst.CST_ICO_DEFAULT;    // TWinTrace don't have icon (for now)
         Enabled = true;
         WinTraceId = Id ;    // winTraceId need to be the same as 'id' if we want to call sendXxx() directly on WinTrace object

         fContextList = new NodeContextList();

         //fWinTraceContext = null ;

         fWarning = new TraceNode(null, false);
         fWarning.IconIndex = TraceConst.CST_ICO_WARNING;
         fWarning.WinTraceId = this.Id;
         fWarning.fWinTraceContext = fContextList;
         fWarning.Enabled = true;

         fError = new TraceNode(null, false);
         fError.IconIndex = TraceConst.CST_ICO_ERROR;
         fError.WinTraceId = this.Id;
         fError.fWinTraceContext = fContextList;
         fError.Enabled = true;

         fDebug = new TraceNode(null, false);
         fDebug.IconIndex = TraceConst.CST_ICO_INFO;
         fDebug.WinTraceId = this.Id;
         fDebug.fWinTraceContext = fContextList;
         fDebug.Enabled = true;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Warning, Error and Debug are the 3 doors to send traces
      /// <example> This sample shows how to send a sample trace.
      /// <code>
      /// TTrace.Error.Send ("Hello", "world") ;       // 2 columns
      /// TTrace.Warning.Send ("Hello") ;              // 1 columns
      /// TTrace.Debug.SendObject("MyObject" , this) ; // 1 columns + members tree
      /// </code>
      /// </example>
      /// </summary>
      public TraceToSend Warning
      {
         get { return fWarning ; }
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Warning, Error and Debug are the 3 doors to send traces
      /// <example> This sample shows how to send a sample trace.
      /// <code>
      /// TTrace.Error.Send ("Hello", "world") ;       // 2 columns
      /// TTrace.Warning.Send ("Hello") ;              // 1 columns
      /// TTrace.Debug.SendObject("MyObject" , this) ; // 1 columns + members tree
      /// </code>
      /// </example>
      /// </summary>
      public TraceToSend Error
      {
         get { return fError ; }
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Warning, Error and Debug are the 3 doors to send traces
      /// <example> This sample shows how to send a sample trace.
      /// <code>
      /// TTrace.Error.Send ("Hello", "world") ;       // 2 columns
      /// TTrace.Warning.Send ("Hello") ;              // 1 columns
      /// TTrace.Debug.SendObject("MyObject" , this) ; // 1 columns + members tree
      /// </code>
      /// </example>
      /// </summary>
      public TraceToSend Debug
      {
         get  { return fDebug ; }
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Save the window tree traces to a text file
      /// </summary>
      /// <param name="FileName">file to save</param>
      public void SaveToTextfile (string FileName)
      {
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_SAVETOTEXT, FileName));
         TTrace.SendToWinTraceClient (CommandList,this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Save the window tree traces to an XML file
      /// </summary>
      /// <param name="FileName">file to save</param>
      public void SaveToXml (string FileName)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_SAVETOXML , FileName);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Save the window tree traces to an XML file
      /// </summary>
      /// <param name="FileName">file to save</param>
      /// <param name="StyleSheet">optional StyleSheet file name added in xml</param>
      public void SaveToXml (string FileName, string StyleSheet)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_SAVETOXML, FileName + '|' + StyleSheet);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Load an XML file to the window tree traces
      /// </summary>
      /// <param name="FileName">file to open</param>
      public void LoadXml (string FileName)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_LOADXML, FileName);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Show the window tree
      /// </summary>
      public void DisplayWin ()
      {
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}", TraceConst.CST_DISPLAY_TREE));
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// change the tree to display user defined multiple columns
      /// must be called before setting column titles. The first column is the main column
      /// </summary>

      public void SetMultiColumn()
      {
         InternalWinTrace TraceForm = TTrace.getInternalTraceForm(this.Id, true);
         TraceForm.IsMultiColTree = true;

         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}{1,11}", TraceConst.CST_TREE_MULTI_COLUMN, 0));
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// change the tree to display user defined multiple columns
      /// must be called before setting column titles
      /// </summary>
      /// <param name="MainColIndex">The Main column index (default is 0)</param>

      public void SetMultiColumn(int MainColIndex)
      {
         InternalWinTrace TraceForm = TTrace.getInternalTraceForm(this.Id, true);
         TraceForm.IsMultiColTree = true;
         TraceForm.MainCol = MainColIndex;

         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}{1,11}", TraceConst.CST_TREE_MULTI_COLUMN, MainColIndex));
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      ///   Set the log file.(Path is relative to the viewer). To enabled
      ///   log on local AND on the viewer call this function twice. To
      ///   don't use the viewer, set the TTrace.options.SendMode to
      ///   None.
      ///   <code>
      ///   The Mode can be one of the following :
      ///   0, Viewer Log is disabled.
      ///   1, Viewer log enabled. No size limit.
      ///   2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      ///   3, Local log is disabled
      ///   4, Local log enabled. No size limit. Ignored in silverlight 2
      ///   5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename). Ignored in silverlight 2
      ///   </code>
      ///   </summary>
      ///   <param name="FileName">\File to open</param>
      ///   <param name="Mode">Local and viewer site log mode. </param>                                 
      
      public void SetLogFile(string FileName, int Mode)
      {
         SetLogFile(FileName, Mode, -1);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      ///   Set the log file.(Path is relative to the viewer). To enabled
      ///   log on local AND on the viewer call this function twice. To
      ///   don't use the viewer, set the TTrace.options.SendMode to
      ///   None.
      ///   <code>
      ///   The Mode can be one of the following :
      ///   0, Viewer Log is disabled.
      ///   1, Viewer log enabled. No size limit.
      ///   2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      ///   3, Local log is disabled
      ///   4, Local log enabled. No size limit. Ignored in silverlight 2
      ///   5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename). Ignored in silverlight 2
      ///   </code>
      ///   </summary>
      ///   <param name="FileName">\File to open</param>
      ///   <param name="Mode">Local and viewer site log mode. </param>
      ///   <param name="MaxLines">Number of lines before starting a new
      ///                          \file (default \: \-1 = unlimited). </param>                         

      public void SetLogFile(string FileName, int Mode, int MaxLines)
      {
         // 3, Local log is disabled
         // 4, Local log enabled. No size limit.
         // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
         if (Mode >= 3) {
            InternalWinTrace TraceForm = TTrace.getInternalTraceForm(this.Id, true);
            if (TraceForm == null)
               return;
            TraceForm.LogFileName = FileName;
            TraceForm.LogFileType = Mode;
            TraceForm.MaxLines = MaxLines;
            // don't send anything to the viewer.
         } else {
            StringList CommandList = new StringList();
            Helper.addCommand(CommandList,TraceConst.CST_LOGFILE, Mode, MaxLines, FileName);
            TTrace.SendToWinTraceClient(CommandList, this.Id);
         }
      }

      //------------------------------------------------------------------------------

      /// <summary>
      ///   Return the last local log file. (when mode 4 or 5 is used). Note : Call TTrace.Flush() to ensure traces are saved
      /// </summary>

      public string GetLocalLogFile()
      {
         InternalWinTrace TraceForm = TTrace.getInternalTraceForm(this.Id, true);
         if (TraceForm == null)
            return "";
         return TraceForm.LastLocalLogFileName;
  
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// set columns title
      /// </summary>
      /// <param name="Titles">Tab separated columns titles
      /// Example : Title1 \t title2
      /// </param>
      public void SetColumnsTitle (string Titles)
      {
         InternalWinTrace TraceForm = TTrace.getInternalTraceForm(this.Id, true);
         TraceForm.IsMultiColTree = true;
         TraceForm.TitleList = Titles ;

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_TREE_COLUMNTITLE, Titles);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// set columns widths
      /// </summary>
      /// <param name="Widths">Tab separated columns width.
      /// The format for each column is width[:Min[:Max]] <p/>
      /// where Min and Max are optional minimum and maximum column width for resizing purpose.<p/>
      /// Example : 100:20:80 \t 200:50 \t 100
      /// </param>
      public void SetColumnsWidth (string Widths)
      {
         StringList CommandList = new StringList();

         Helper.addCommand(CommandList,TraceConst.CST_TREE_COLUMNWIDTH , Widths);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------
 
      /// <summary>
      /// Set the focus to the first trace node
      /// </summary>
      public void GotoFirstNode() 
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_GOTO_FIRST_NODE);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }
       
      //------------------------------------------------------------------------------

      /// <summary>
      /// Set the focus to the last trace node
      /// </summary>
      public void GotoLastNode() 
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_GOTO_LAST_NODE);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set the focus to the next matching node
      /// </summary>
      /// <param name="SearForward">If true search down, else search up  </param>
      public void FindNext(bool SearForward) 
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_FIND_NEXT,SearForward);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set the focus to a bookmarked node identified by his position. Bookmarks are cheched by the user or with the node.SetBookmark() function
      /// </summary>
      /// <param name="Pos">Indice of the bookmark </param>
      public void GotoBookmark(int Pos)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_GOTO_BOOKMARK,Pos);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Clear all bookmarks
      /// </summary>
      public void ClearBookmark()
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_CLEAR_BOOKMARK);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Clear all filters
      /// </summary>
      public void ClearFilter() 
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_CLEAR_FILTER);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Add a filter to node. Multiple calls to this function can be done. Call ApplyFilter() to apply filtering
      /// </summary>
      /// <param name="Column">Column to apply filter.<p/>
      ///   In multicolumn mode the first column start at 0 <p/>
      ///   In normal mode : <p/>
      ///   col icone   = 999    <p/>
      ///   col time    = 1      <p/>
      ///   col thread  = 2      <p/>
      ///   col traces  = 3      <p/>
      ///   col Comment = 4      <p/>
      ///   col members = 998
      /// </param>
      /// <param name="Compare">There is 5 kinds of filters : <p/>
      ///    Equal           = 0  <p/>
      ///    Not equal       = 1  <p/>
      ///    contains       = 2  <p/>
      ///    Don't contains  = 3  <p/>
      ///    (Ignore this filter) = 4 or -1
      ///</param>
      /// <param name="Text">The text to search (insensitive) </param>
      public void AddFilter(int Column , int Compare , string Text) 
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_ADD_FILTER , Column, Compare , Text);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------
   

      /// <summary>
      /// Apply filters after calls to AddFilter().
      /// </summary>
      /// <param name="ConditionAnd">If true, use an 'AND' condition for each filters, else use a "OR" </param>
      /// <param name="ShowMatch">If true, show node that match filter and hide others. If false hide matching node and show others</param>
      /// <param name="IncludeChildren">If true, search in subnodes</param>
      public void ApplyFilter(bool ConditionAnd, bool ShowMatch, bool IncludeChildren) 
      {
         int flags = 0;
         // ConditionAnd<<2+ShowMatch<<1+IncludeChildren
         if (ConditionAnd)
            flags += 4;
         if (ShowMatch)
            flags += 2;
         if (IncludeChildren)
            flags += 1;

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_APPLY_FILTER, flags);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Clear all trace for the window tree
      /// </summary>
      public void ClearAll ()
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_CLEAR_ALL);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Close the window tree
      /// </summary>
      public void Close()
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_CLOSE_WIN);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------
      // PLUGIN API
      //------------------------------------------------------------------------------

      /// <summary>
      /// Plugin API : Create a resource.
      /// </summary>
      /// <param name="ResId">The resource Id (must be >= 100)</param>
      /// <param name="ResType">Resource type. See TraceConst
      /// <code>
      /// CST_RES_BUT_RIGHT    : Button on right
      /// CST_RES_BUT_LEFT     : Button on left
      /// CST_RES_LABEL_RIGHT  : Label on right
      /// CST_RES_LABELH_RIGHT : Label on right HyperLink
      /// CST_RES_LABEL_LEFT   : Label on left
      /// CST_RES_LABELH_LEFT  : Label on left hyperlink
      /// CST_RES_MENU_ACTION  : Item menu in the Actions Menu
      /// CST_RES_MENU_WINDOW  : Item menu in the Windows Menu.
      ///                        Call CreateResource on the main win trace to create this menu item
      /// </code>
      ///</param>
      /// <param name="ResWidth">Width of the resource. Applicable only to button and labels</param>
      /// <param name="ResText">Resource text</param>

      public void CreateResource (int ResId , int ResType , int ResWidth , string ResText)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_CREATE_RESOURCE, ResId, ResType, ResWidth, ResText);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Plugin API : Disable tracetool or user created resources
      /// </summary>
      /// <param name="ResId">The resource Id
      /// ResId: resource id to disable. Tracetool resources :
      /// <code>
      /// CST_ACTION_CUT            : Cut. Same as copy then delete
      /// CST_ACTION_COPY           : Copy
      /// CST_ACTION_DELETE         : Delete selected
      /// CST_ACTION_SELECT_ALL     : Select all
      /// CST_ACTION_RESIZE_COLS    : Resize columns
      /// CST_ACTION_VIEW_INFO      : View trace info
      /// CST_ACTION_VIEW_PROP      : View properties
      /// CST_ACTION_PAUSE          : Pause
      /// CST_ACTION_SAVE           : SaveToFile
      /// CST_ACTION_CLEAR_ALL      : Clear all
      /// CST_ACTION_CLOSE_WIN      : Close win
      /// CST_ACTION_LABEL_INFO     : TracesInfo label
      /// CST_ACTION_LABEL_LOGFILE  : LabelLogFile label
      /// CST_ACTION_VIEW_MAIN      : View Main trace
      /// CST_ACTION_VIEW_ODS       : ODS
      /// CST_ACTION_OPEN_XML       : XML trace -> Tracetool XML traces
      /// CST_ACTION_EVENTLOG       : Event log
      /// CST_ACTION_TAIL           : Tail
      /// </code>
      /// </param>

      public void DisableResource(int ResId)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,TraceConst.CST_DISABLE_RESOURCE, ResId);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Plugin API : Set the resource text (tracetool or user created resources), specified by his Id
      /// </summary>
      /// <param name="ResId">The resource Id </param>
      /// <param name="ResText">Resource text</param>

      public void SetTextResource(int ResId, string ResText)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_SET_TEXT_RESOURCE, ResId, ResText);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------
      //------------------------------------------------------------------------------

      /// <summary>
      /// Plugin API : Attach a winTrace to a plugin. Many winTrace can be attached to a plugin.
      /// Note that a plugin don't need to be attached to a WinTrace.
      /// The plugin is identified by his internal name (not dll name).
      /// When linked, the plugin can receive event (see ITracePLugin).
      /// </summary>
      /// <param name="PluginName">name of the plugin</param>
      /// <param name="flags">combinaison of CST_PLUG_ONACTION , CST_PLUG_ONBEFOREDELETE , CST_PLUG_ONTIMER</param>

      public void LinkToPlugin(string PluginName, int flags)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_LINKTOPLUGIN, flags, PluginName);
         TTrace.SendToWinTraceClient(CommandList, this.Id);
      }
   }
}
