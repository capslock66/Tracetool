/*
 * WinTrace.java
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

import java.util.ArrayList;

/*
 * * WinTrace represent a windows tree where you put traces<p> Sample code :<p> WinTrace myWinTrace ;<p> myWinTrace = new WinTrace ("MyWINID" , "My trace window") ;<p> myWinTrace.debug().send ("Hello",
 * "Can be used to store exceptions, for examples");<p>
 * @author tpa
 */
/*
 * *
 * @author Thierry Parent
 */
public class WinTrace extends TraceToSend
{
   private TraceToSend fWarning;

   private TraceToSend fError;

   private TraceToSend fDebug;

   //------------------------------------------------------------------------------

   /**
    * WinTrace constructor : <p>
    * you can map a WinTrace to an existing window. <p>
    * Nothing Is send to the viewer
    */
   public WinTrace()
   {
      createNodes() ;
   }

   //------------------------------------------------------------------------------
   /**
    * WinTrace constructor. <p>
    * The Window Trace is create on the viewer (if not already done)
    * @param winTraceID Required window trace Id. If empty, a guid will be generated
    * @param winTraceTitle The Window Title on the viewer.If empty, a default name will be used
    */
   public WinTrace(final String winTraceID, final String winTraceTitle)
   {
      this();

      if (winTraceID == null || winTraceID.length() == 0) 
         this.id = new java.rmi.server.UID().toString();
      else
         this.id = winTraceID;

      createNodes() ;

      if (winTraceID != null && winTraceID.compareTo("_") == 0) //$NON-NLS-1$
         return ;  // don't create new window on the viewer

      String wTraceTitle;
      if (winTraceTitle == null || winTraceTitle.length() == 0) 
         wTraceTitle = this.id;
      else
         wTraceTitle = winTraceTitle;

      // create the trace window

      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_TREE_NAME, wTraceTitle);

      TTrace.sendToWinTraceClient(commandList, this.id);
   }

 //------------------------------------------------------------------------------
   /**
    * common to the 2 others constructors
    */
   private void createNodes()
   {

      this.iconIndex = TraceConst.CST_ICO_DEFAULT;    // TWinTrace don't have icon (for now)
      this.enabled = true;
      this.winTraceId = this.id;    // winTraceId need to be the same as 'id' if we want to call sendXxx() directly on WinTrace object

      this.contextList = new ArrayList();

      // Warning, Error and Debug nodes are TraceSend, but since TraceSend is abstract, create a descendant

      this.fWarning = new TraceNode(null, false);
      this.fWarning.iconIndex = TraceConst.CST_ICO_WARNING;
      this.fWarning.winTraceId = this.id;
      this.fWarning.winTraceContext = this.contextList ;
      this.fWarning.enabled = true ;

      this.fError = new TraceNode(null, false);
      this.fError.iconIndex = TraceConst.CST_ICO_ERROR;
      this.fError.winTraceId = this.id;
      this.fError.winTraceContext = this.contextList ;
      this.fError.enabled = true ;

      this.fDebug = new TraceNode(null, false);
      this.fDebug.iconIndex = TraceConst.CST_ICO_INFO;
      this.fDebug.winTraceId = this.id;
      this.fDebug.winTraceContext = this.contextList ;
      this.fDebug.enabled = true ;

   }

   //------------------------------------------------------------------------------

   /**
    * Set the log file.<p>(Path is relative to the viewer)
    * To enabled log on local AND on the viewer call this funtion twice.
    * To don't use the viewer, set the TTrace.options.sendMode to SendMode.None.
    * @param fileName file to save.
    * @param mode <p>Local and viewer site log mode. 
    *  0, Viewer Log is disabled.<p>
    *  1, Viewer log enabled.<p>
    *  2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)<p>
    *  3, Local log is disabled<p>
    *  4, Local log enabled.<p>
    *  5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
    */
   public void setLogFile(final String fileName, final int mode)
   {
      setLogFile(fileName, mode, -1) ;
   }

   //------------------------------------------------------------------------------

   /**
    * Set the log file.<p>(Path is relative to the viewer)
    * To enabled log on local AND on the viewer call this function twice.
    * To don't use the viewer, set the TTrace.options.sendMode to SendMode.None.
    * @param fileName file to save.
    * @param mode <p>Local and viewer site log mode. 
    *  0, Viewer Log is disabled.<p>
    *  1, Viewer log enabled.<p>
    *  2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)<p>
    *  3, Local log is disabled<p>
    *  4, Local log enabled.<p>
    *  5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
    * @param maxLines Number of lines before starting a new file (default : -1 = unlimited)
    */
   public void setLogFile(final String fileName, final int mode, final int maxLines)
   {
      // 3, Local log is disabled
      // 4, Local log enabled. No size limit.
      // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      if (mode >= 3) {
         InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(this.id, true);
         TraceForm.LogFileName = fileName;
         TraceForm.LogFileType = mode;
         TraceForm.MaxLines = maxLines ;
         // don't send anything to the viewer.
      } else {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList,TraceConst.CST_LOGFILE, mode,maxLines, fileName);
         TTrace.sendToWinTraceClient(commandList, this.id);
      }
   }

   //------------------------------------------------------------------------------
   /**
   *   Return the last local log file. (when mode 4 or 5 is used). Note : Call TTrace.Flush() to ensure traces are saved
    * @return last local log file
   */

   public String getLocalLogFile()
   {
      InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(this.id, true);
      if (TraceForm == null)
         return ""; //$NON-NLS-1$
      return TraceForm.LastLocalLogFileName;
   }

   /**
    * change the tree to display user defined multiple columns
    */
   public void setMultiColumn() 
   {
      InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(this.id, true);
      TraceForm.IsMultiColTree = true;

      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_TREE_MULTI_COLUMN);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   /**
    * change the tree to display user defined multiple columns
    * @param mainColIndex The Main column index (default is 0)
    */
   public void setMultiColumn(final int mainColIndex)
   {
       InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(this.id, true);
      TraceForm.IsMultiColTree = true;
      TraceForm.MainCol = mainColIndex;

      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_TREE_MULTI_COLUMN, mainColIndex);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

    /**
    * set columns title
    * @param value tab separated columns titles
    */
   public void setColumnsTitle(final String value)
   {
      InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(this.id, true);
      TraceForm.IsMultiColTree = true;
      TraceForm.TitleList = value ;

      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_TREE_COLUMNTITLE, value);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

  /**
    * set columns widths
    * @param widths : Tab separated columns width. <p>
    * The format for each column is width[:Min[:Max]] <p>
    * where Min and Max are optional minimum and maximum column width for resizing purpose.<p>
    * Example : 100:20:80 tab 200:50 tab 100
    */
   public void setColumnsWidth(final String widths)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_TREE_COLUMNWIDTH, widths);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   
   /** Set the focus to the trace first node */
   public void gotoFirstNode() 
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_GOTO_FIRST_NODE);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }
    
   //------------------------------------------------------------------------------

   /** Set the focus to the trace last node */
   public void gotoLastNode() 
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_GOTO_LAST_NODE);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /** Set the focus to the next matching node
   * @param searForward If true search down, else search up 
   */
   public void findNext(final boolean searForward) 
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_FIND_NEXT,searForward);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /** Set the focus to a bookmarked node identified by his position. Bookmarks are cheched by the user or with the node.SetBookmark() function
   @param pos Indice of the bookmark 
   */
   public void gotoBookmark(int pos)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_GOTO_BOOKMARK,pos);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /** Clear all bookmarks
   */
   public void clearBookmark()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CLEAR_BOOKMARK);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /** Clear all filters
   */
   public void clearFilter() 
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CLEAR_FILTER);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /** Add a filter to node. Multiple calls to this function can be done. Call ApplyFilter() to apply filtering
   @param column column to apply filter.<p>
      In multicolumn mode the first column start at 0 <p>
      In normal mode : <p>
      col icone   = 999    <p>
      col time    = 1      <p>
      col thread  = 2      <p>
      col traces  = 3      <p>
      col Comment = 4      <p>
      col members = 998
   @param compare There is 5 kinds of filters : <p>
      Equal           = 0  <p>
      Not equal       = 1  <p>
      Contains        = 2  <p>
      Don't contains  = 3  <p>
      (Ignore this filter) = 4 or -1
   @param text The text to search (insensitive) 
   */
   public void addFilter(int column , int compare , String text) 
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList, TraceConst.CST_ADD_FILTER , column, compare , text);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /** Apply filters after calls to AddFilter().
   @param conditionAnd If true, use an 'AND' condition for each filters, else use a "OR"  
   @param showMatch If true, show node that match filter and hide others. If false hide matching node and show others 
   @param includeChildren If true, search in subnodes 
   */
   public void applyFilter(boolean conditionAnd, boolean showMatch, boolean includeChildren) 
   {
      int flags = 0;
      // ConditionAnd<<2+ShowMatch<<1+IncludeChildren
      if (conditionAnd)
         flags += 4;
      if (showMatch)
         flags += 2;
      if (includeChildren)
         flags += 1;

      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList, TraceConst.CST_APPLY_FILTER, flags);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /**
    * Debug, Warning, Error are the 3 normal entry points
    * @return The debug TraceNode
    */
   public TraceToSend debug()
   {
      return this.fDebug;
   }

   //------------------------------------------------------------------------------

   /**
    * Debug, Warning, Error are the 3 normal entry points
    * @return The warning TraceNode
    */
   public TraceToSend warning()
   {
      return this.fWarning;
   }

   //------------------------------------------------------------------------------

   /**
    * Debug, Warning, Error are the 3 normal entry points
    * @return The error TraceNode
    */
   public TraceToSend error()
   {
      return this.fError;
   }

   //------------------------------------------------------------------------------

   /**
    * Save window content to text file <p> (Path is relative to the viewer)
    * @param fileName target filename
    */
   public void saveToTextfile(final String fileName)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_SAVETOTEXT, fileName);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /**
    * Save window content to xml file <p> (Path is relative to the viewer)
    * @param fileName target filename
    */
   public void saveToXml(final String fileName)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_SAVETOXML, fileName);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   /**
    * Save window content to xml file <p> (Path is relative to the viewer)
    * @param fileName target filename
    * @param styleSheet optional stylesheet file name
    */
   public void saveToXml(final String fileName, final String styleSheet)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_SAVETOXML, fileName + "|" + styleSheet); //$NON-NLS-1$
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   /**
    * Load xml file to the window <p> (Path is relative to the viewer)
    * @param fileName target filename
    */
   public void loadXml(final String fileName)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_LOADXML, fileName);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   /**
    * Switch viewer to this window
    */
   public void displayWin()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_DISPLAY_TREE);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /**
    * clear all trace in that window
    */
   public void clearAll()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CLEAR_ALL);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /**
    * clear all trace in that window
    */
   public void close()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CLOSE_WIN);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   // PLUGIN API
   //------------------------------------------------------------------------------

   /**
   Plugin API : Create a resource.
   @param resId The resource Id (must be >= 100)
   @param resType resource type
   <pre>
      CST_RES_BUT_RIGHT     :   Button on right
      CST_RES_BUT_LEFT      :   Button on left
      CST_RES_LABEL_RIGHT   :   Label on right
      CST_RES_LABELH_RIGHT  :   Label on right HyperLink
      CST_RES_LABEL_LEFT    :   Label on left
      CST_RES_LABELH_LEFT   :   Label on left hyperlink
      CST_RES_MENU_ACTION   :   Item menu in the Actions Menu
      CST_RES_MENU_WINDOW   :   Item menu in the Windows Menu.

      Call CreateResource on the main win trace to create "Windows" menu item
   </pre>
   @param resWidth Width of the resource. Applicable only to button and labels
   @param resText Resource text
    */

   public void createResource(final int resId, final int resType, final int resWidth, final String resText)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CREATE_RESOURCE, resId, resType, resWidth, resText);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /**
    Plugin API : Disable tracetool or user created resources
    @param resId The resource Id  <p>
    resource id to disable. Tracetool resources : <p>
      CST_ACTION_CUT            cut same as copy then delete <p>
      CST_ACTION_COPY           copy <p>
      CST_ACTION_DELETE         delete selected <p>
      CST_ACTION_SELECT_ALL     select all <p>
      CST_ACTION_RESIZE_COLS    resize columns <p>
      CST_ACTION_VIEW_INFO      view trace info <p>
      CST_ACTION_VIEW_PROP      view properties <p>
      CST_ACTION_PAUSE          Pause <p>
      CST_ACTION_SAVE           SaveToFile <p>
      CST_ACTION_CLEAR_ALL      clear all <p>
      CST_ACTION_CLOSE_WIN      Close win <p>
      CST_ACTION_LABEL_INFO     TracesInfo label <p>
      CST_ACTION_LABEL_LOGFILE  LabelLogFile label <p>
      CST_ACTION_VIEW_MAIN      View Main trace <p>
      CST_ACTION_VIEW_ODS       ODS <p>
      CST_ACTION_OPEN_XML       XML trace -> Tracetool XML traces <p>
      CST_ACTION_EVENTLOG       Event log <p>
      CST_ACTION_TAIL           Tail <p>
    */

   public void disableResource(final int resId)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_DISABLE_RESOURCE, resId);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------

   /**
    Plugin API : Set the resource text (tracetool or user created resources), specified by his Id
    @param resId The resource Id
    @param resText Resource text
    */

   public void setTextResource(final int resId, final String resText)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_SET_TEXT_RESOURCE, resId, resText);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

   //------------------------------------------------------------------------------
   //------------------------------------------------------------------------------

   /**
    Plugin API : Attach a winTrace to a plugin. Many winTrace can be attached to a plugin. <p>
    The inverse is true : a plugin don't need to be attached to a WinTrace <p>
    The plugin identified by his internal name (not dll name). <p>
    When linked, the plugin can receive event. <p>
    @param pluginName name of the plugin
    @param flags combinaison of CST_PLUG_ONACTION , CST_PLUG_ONBEFOREDELETE , CST_PLUG_ONTIMER
    */

   public void linkToPlugin(final String pluginName, final int flags)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_LINKTOPLUGIN, flags, pluginName);
      TTrace.sendToWinTraceClient(commandList, this.id);
   }

}
