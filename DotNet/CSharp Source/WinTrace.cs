// WinTrace.cs
//
// WinTrace represent a windows tree where you put traces
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information

using System;
using System.Collections.Generic;

// ReSharper disable ClassNeverInstantiated.Global
// ReSharper disable ConvertIfStatementToNullCoalescingExpression
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable IntroduceOptionalParameters.Global
// ReSharper disable FieldCanBeMadeReadOnly.Global
// ReSharper disable UnusedMethodReturnValue.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable InlineOutVariableDeclaration
// ReSharper disable UseStringInterpolation
// ReSharper disable UseObjectOrCollectionInitializer
// ReSharper disable UseNullPropagation
// ReSharper disable MergeCastWithTypeCheck
// ReSharper disable UsePatternMatching
// ReSharper disable ArrangeAccessorOwnerBody

namespace TraceTool
{
    /// <summary>
    /// WinTrace represent a windows tree where you put traces
    /// </summary>
    public class WinTrace : TraceToSend
    {
        private TraceToSend _warning;
        private TraceToSend _error;
        private TraceToSend _debug;

        //------------------------------------------------------------------------------

        /// <summary>
        /// WinTrace constructor : you can map a WinTrace to an existing window
        /// Nothing is send to the viewer
        /// </summary>
        public WinTrace()
        {
            CreateNodes();
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// WinTrace constructor. The Window Trace is create on the viewer (if not already done)
        /// </summary>
        /// <param name="winTraceId">Required window trace Id. If empty, a guid will be generated</param>
        /// <param name="winTraceText">The Window Title on the viewer.If empty, a default name will be used</param>
        public WinTrace(string winTraceId, string winTraceText)
        {
            if (string.IsNullOrEmpty(winTraceId))
                Id = Helper.NewGuid().ToString();
            else
                Id = winTraceId;

            CreateNodes();

            if (winTraceId != null && winTraceId == "_")
                return;  // don't create new window on the viewer

            if (string.IsNullOrEmpty(winTraceText))
                winTraceText = Id;

            // create the trace window
            List<string> commandList = new List<string>();
            commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_TREE_NAME, winTraceText));
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        // common to the 2 others constructors
        internal void CreateNodes()
        {
            IconIndex = TraceConst.CST_ICO_DEFAULT;    // TWinTrace don't have icon (for now)
            Enabled = true;
            WinTraceId = Id;    // winTraceId need to be the same as 'id' if we want to call sendXxx() directly on WinTrace object

            ContextList = new List<NodeContext>();

            //fWinTraceContext = null ;

            _warning = new TraceNode(null, false);
            _warning.IconIndex = TraceConst.CST_ICO_WARNING;
            _warning.WinTraceId = Id;
            _warning.WinTraceContext = ContextList;
            _warning.Enabled = true;

            _error = new TraceNode(null, false);
            _error.IconIndex = TraceConst.CST_ICO_ERROR;
            _error.WinTraceId = Id;
            _error.WinTraceContext = ContextList;
            _error.Enabled = true;

            _debug = new TraceNode(null, false);
            _debug.IconIndex = TraceConst.CST_ICO_INFO;
            _debug.WinTraceId = Id;
            _debug.WinTraceContext = ContextList;
            _debug.Enabled = true;
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
            get { return _warning; }
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
            get { return _error; }
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
            get { return _debug; }
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Save the window tree traces to a text file
        /// </summary>
        /// <param name="fileName">file to save</param>
        public void SaveToTextfile(string fileName)
        {
            List<string> commandList = new List<string>();
            commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_SAVETOTEXT, fileName));
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Save the window tree traces to an XML file
        /// </summary>
        /// <param name="fileName">file to save</param>
        public void SaveToXml(string fileName)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_SAVETOXML, fileName);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Save the window tree traces to an XML file
        /// </summary>
        /// <param name="fileName">file to save</param>
        /// <param name="styleSheet">optional StyleSheet file name added in xml</param>
        public void SaveToXml(string fileName, string styleSheet)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_SAVETOXML, fileName + '|' + styleSheet);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Load an XML file to the window tree traces
        /// </summary>
        /// <param name="fileName">file to open</param>
        public void LoadXml(string fileName)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_LOADXML, fileName);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Show the window tree
        /// </summary>
        public void DisplayWin()
        {
            List<string> commandList = new List<string>();
            commandList.Insert(0, String.Format("{0,5}", TraceConst.CST_DISPLAY_TREE));
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// change the tree to display user defined multiple columns
        /// must be called before setting column titles. The first column is the main column
        /// </summary>

        public void SetMultiColumn()
        {
            InternalWinTrace traceForm = TTrace.GetInternalTraceForm(Id, true);
            traceForm.IsMultiColTree = true;

            List<string> commandList = new List<string>();
            commandList.Insert(0, String.Format("{0,5}{1,11}", TraceConst.CST_TREE_MULTI_COLUMN, 0));
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// change the tree to display user defined multiple columns
        /// must be called before setting column titles
        /// </summary>
        /// <param name="mainColIndex">The Main column index (default is 0)</param>

        public void SetMultiColumn(int mainColIndex)
        {
            InternalWinTrace traceForm = TTrace.GetInternalTraceForm(Id, true);
            traceForm.IsMultiColTree = true;
            traceForm.MainCol = mainColIndex;

            List<string> commandList = new List<string>();
            commandList.Insert(0, String.Format("{0,5}{1,11}", TraceConst.CST_TREE_MULTI_COLUMN, mainColIndex));
            TTrace.SendToWinTraceClient(commandList, Id);
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
        ///   4, Local log enabled. No size limit.
        ///   5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename).
        ///   </code>
        ///   </summary>
        ///   <param name="fileName">\File to open</param>
        ///   <param name="mode">Local and viewer site log mode. </param>                                 

        public void SetLogFile(string fileName, int mode)
        {
            SetLogFile(fileName, mode, -1);
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
        ///   4, Local log enabled. No size limit. 
        ///   5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename). 
        ///   </code>
        ///   </summary>
        ///   <param name="fileName">\File to open</param>
        ///   <param name="mode">Local and viewer site log mode. </param>
        ///   <param name="maxLines">Number of lines before starting a new
        ///                          \file (default \: \-1 = unlimited). </param>                         

        public void SetLogFile(string fileName, int mode, int maxLines)
        {
            // 3, Local log is disabled
            // 4, Local log enabled. No size limit.
            // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
            if (mode >= 3)
            {
                InternalWinTrace traceForm = TTrace.GetInternalTraceForm(Id, true);
                if (traceForm == null)
                    return;
                traceForm.LogFileName = fileName;
                traceForm.LogFileType = mode;
                traceForm.MaxLines = maxLines;
                // don't send anything to the viewer.
            }
            else
            {
                List<string> commandList = new List<string>();
                Helper.AddCommand(commandList, TraceConst.CST_LOGFILE, mode, maxLines, fileName);
                TTrace.SendToWinTraceClient(commandList, Id);
            }
        }

        //------------------------------------------------------------------------------

        /// <summary>
        ///   Return the last local log file. (when mode 4 or 5 is used). Note : Call TTrace.Flush() to ensure traces are saved
        /// </summary>

        public string GetLocalLogFile()
        {
            InternalWinTrace traceForm = TTrace.GetInternalTraceForm(Id, true);
            if (traceForm == null)
                return "";
            return traceForm.LastLocalLogFileName;
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// set columns title
        /// </summary>
        /// <param name="titles">Tab separated columns titles
        /// Example : Title1 \t title2
        /// </param>
        public void SetColumnsTitle(string titles)
        {
            InternalWinTrace traceForm = TTrace.GetInternalTraceForm(Id, true);
            traceForm.IsMultiColTree = true;
            traceForm.TitleList = titles;

            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_TREE_COLUMNTITLE, titles);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// set columns widths
        /// </summary>
        /// <param name="widths">Tab separated columns width.
        /// The format for each column is width[:Min[:Max]] <p/>
        /// where Min and Max are optional minimum and maximum column width for resizing purpose.<p/>
        /// Example : 100:20:80 \t 200:50 \t 100
        /// </param>
        public void SetColumnsWidth(string widths)
        {
            List<string> commandList = new List<string>();

            Helper.AddCommand(commandList, TraceConst.CST_TREE_COLUMNWIDTH, widths);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Set the focus to the first trace node
        /// </summary>
        public void GotoFirstNode()
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_GOTO_FIRST_NODE);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Set the focus to the last trace node
        /// </summary>
        public void GotoLastNode()
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_GOTO_LAST_NODE);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Set the focus to the next matching node
        /// </summary>
        /// <param name="searForward">If true search down, else search up  </param>
        public void FindNext(bool searForward)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_FIND_NEXT, searForward);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Set the focus to a bookmarked node identified by his position. Bookmarks are cheched by the user or with the node.SetBookmark() function
        /// </summary>
        /// <param name="pos">Indice of the bookmark </param>
        public void GotoBookmark(int pos)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_GOTO_BOOKMARK, pos);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Clear all bookmarks
        /// </summary>
        public void ClearBookmark()
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_CLEAR_BOOKMARK);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Clear all filters
        /// </summary>
        public void ClearFilter()
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_CLEAR_FILTER);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Add a filter to node. Multiple calls to this function can be done. Call ApplyFilter() to apply filtering
        /// </summary>
        /// <param name="column">Column to apply filter.<p/>
        ///   In multicolumn mode the first column start at 0 <p/>
        ///   In normal mode : <p/>
        ///   col icone   = 999    <p/>
        ///   col time    = 1      <p/>
        ///   col thread  = 2      <p/>
        ///   col traces  = 3      <p/>
        ///   col Comment = 4      <p/>
        ///   col members = 998
        /// </param>
        /// <param name="compare">There is 5 kinds of filters : <p/>
        ///    Equal           = 0  <p/>
        ///    Not equal       = 1  <p/>
        ///    contains       = 2  <p/>
        ///    Don't contains  = 3  <p/>
        ///    (Ignore this filter) = 4 or -1
        ///</param>
        /// <param name="text">The text to search (insensitive) </param>
        public void AddFilter(int column, int compare, string text)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_ADD_FILTER, column, compare, text);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Apply filters after calls to AddFilter().
        /// </summary>
        /// <param name="conditionAnd">If true, use an 'AND' condition for each filters, else use a "OR" </param>
        /// <param name="showMatch">If true, show node that match filter and hide others. If false hide matching node and show others</param>
        /// <param name="includeChildren">If true, search in subnodes</param>
        public void ApplyFilter(bool conditionAnd, bool showMatch, bool includeChildren)
        {
            int flags = 0;
            // ConditionAnd<<2+ShowMatch<<1+IncludeChildren
            if (conditionAnd)
                flags += 4;
            if (showMatch)
                flags += 2;
            if (includeChildren)
                flags += 1;

            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_APPLY_FILTER, flags);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Clear all trace for the window tree
        /// </summary>
        public void ClearAll()
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_CLEAR_ALL);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Close the window tree
        /// </summary>
        public void Close()
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_CLOSE_WIN);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------
        // PLUGIN API
        //------------------------------------------------------------------------------

        /// <summary>
        /// Plugin API : Create a resource.
        /// </summary>
        /// <param name="resId">The resource Id (must be >= 100)</param>
        /// <param name="resType">Resource type. See TraceConst
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
        /// <param name="resWidth">Width of the resource. Applicable only to button and labels</param>
        /// <param name="resText">Resource text</param>

        public void CreateResource(int resId, int resType, int resWidth, string resText)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_CREATE_RESOURCE, resId, resType, resWidth, resText);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Plugin API : Disable tracetool or user created resources
        /// </summary>
        /// <param name="resId">The resource Id
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

        public void DisableResource(int resId)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_DISABLE_RESOURCE, resId);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Plugin API : Set the resource text (tracetool or user created resources), specified by his Id
        /// </summary>
        /// <param name="resId">The resource Id </param>
        /// <param name="resText">Resource text</param>

        public void SetTextResource(int resId, string resText)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_SET_TEXT_RESOURCE, resId, resText);
            TTrace.SendToWinTraceClient(commandList, Id);
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Plugin API : Attach a winTrace to a plugin. Many winTrace can be attached to a plugin.
        /// Note that a plugin don't need to be attached to a WinTrace.
        /// The plugin is identified by his internal name (not dll name).
        /// When linked, the plugin can receive event (see ITracePLugin).
        /// </summary>
        /// <param name="pluginName">name of the plugin</param>
        /// <param name="flags">combinaison of CST_PLUG_ONACTION , CST_PLUG_ONBEFOREDELETE , CST_PLUG_ONTIMER</param>

        public void LinkToPlugin(string pluginName, int flags)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_LINKTOPLUGIN, flags, pluginName);
            TTrace.SendToWinTraceClient(commandList, Id);
        }
    }
}
