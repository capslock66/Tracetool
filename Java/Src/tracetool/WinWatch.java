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

/**
 * WinWatch represent a windows tree where you put watches<p>
 * Sample code :<p>
 * TTrace.watches().send("test2", mySet); <p>
 *
 * @author tpa
 * @since 6.0.0
 */
public class WinWatch
{

   /**
    * The "Required" Id of the window tree, can be any string, or a guid. <p>
    * The Main window watch Id is empty
    */
   public String id ;

   /** When enabled is false, all traces are disabled. Default is true. <p>
   */
   public boolean enabled;

  /** User variable, provided for the convenience of developers */
   public Object tag;

   //------------------------------------------------------------------------------

   /**
    * WinWatch constructor : <p>
    * you can map a WinWatch to an existing window. <p>
    * Nothing Is send to the viewer
    */

   public WinWatch()
   {
      this.enabled = true ;
      this.id = "" ;  //$NON-NLS-1$
   }

   //------------------------------------------------------------------------------

   /**
    * WinWatch constructor. <p>
    * The Window watch is create on the viewer (if not already done)
    * @param winWatchID Required window trace Id. If empty, a guid will be generated
    * @param winWatchText The Window Title on the viewer.If empty, a default name will be used
    */
   public WinWatch (final String winWatchID , final String winWatchText)
   {
      this () ;

      if (winWatchID == null || winWatchID.length() == 0)  
         this.id = new java.rmi.server.UID().toString() ;
      else
         this.id = winWatchID ;

      // create the trace window
      ArrayList commandList = new ArrayList();

      if (winWatchText == null || winWatchText.length() == 0) 
         Utility.addCommand(commandList,TraceConst.CST_WINWATCH_NAME, "Watches " + this.id);  //$NON-NLS-1$
      else
         Utility.addCommand(commandList,TraceConst.CST_WINWATCH_NAME, winWatchText);

      TTrace.sendToWinWatchClient (commandList, this);
   }

   //------------------------------------------------------------------------------

   /**
    * Switch viewer to this window
    */
   public void displayWin()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_DISPLAY_TREE);
      TTrace.sendToWinWatchClient(commandList, this);
   }

   //------------------------------------------------------------------------------

   /**
    * clear all traces in that window
    */
   public void clearAll()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CLEAR_ALL);
      TTrace.sendToWinWatchClient(commandList, this);
   }

   //------------------------------------------------------------------------------

   //------------------------------------------------------------------------------

   /**
    * clear all traces in that window
    */
   public void close()
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_CLOSE_WIN);
      TTrace.sendToWinWatchClient(commandList, this);
   }

   //------------------------------------------------------------------------------
   /**
    * Send a watch
    * @param watchName Watch name
    * @param watchValue Watch value
    */
   public void send (final String watchName , final Object watchValue)
   {
      if (!this.enabled)
         return ;

      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList,TraceConst.CST_WATCH_NAME, watchName);

      // create a node with same properties as "self" with new ID
      TraceNodeEx node = new TraceNodeEx (null, false) ;  // no parent, don't generate node id

      node.addValue(watchValue  ,  TTrace.options.sendPrivate , TTrace.options.objectTreeDepth , "");    // no title //$NON-NLS-1$
      node.members.addToStringList(commandList) ;   // convert all groups and nested items/group to strings

      TTrace.sendToWinWatchClient(commandList, this);
   }

}
