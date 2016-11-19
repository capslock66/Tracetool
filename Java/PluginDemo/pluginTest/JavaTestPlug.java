
package pluginTest;

import tracetool.ITracePLugin;
import tracetool.TTrace;
import tracetool.TraceConst;
import tracetool.TraceNode;
import tracetool.Utility;
import tracetool.WinTrace;


/** 
  * Sample java plugin
  */

public class JavaTestPlug  implements ITracePLugin  // extends PluginBase //
{
   static {
      //TTrace.debug().send("               JavaTestPlug initialize ..."); //$NON-NLS-1$
      //TTrace.flush() ;
   }
   

// ----------------------------------------------------------------------

   /**
    * Plug Name 
    */
   public static final String plugName = "Java Plugin 9" ;     //$NON-NLS-1$
  
   /**
    * Get the plugin name
    * @return plugin name
    * @see tracetool.ITracePLugin#getPlugName()
    */
 
   public String getPlugName() 
   {
      return plugName ;
   }

   //----------------------------------------------------------------------
   
   
   WinTrace plugTraces ;
   TraceNode actionNodes , beforeDeleteNodes, timerNode ;

// ----------------------------------------------------------------------

   /**
    * Initialise the plugin
    * @see tracetool.ITracePLugin#start()
    */
   public void start()
   {
      // create a window and ask to receive timer, action and onBeforeDelete events
      
      this.plugTraces = new WinTrace ("JAVAID" , "Java Plugin 9") ; //$NON-NLS-1$ //$NON-NLS-2$
      
      this.plugTraces.displayWin() ;
      this.plugTraces.linkToPlugin (plugName, TraceConst.CST_PLUG_ONACTION + TraceConst.CST_PLUG_ONBEFOREDELETE + TraceConst.CST_PLUG_ONTIMER) ;

      // disable the  LogFile label
      this.plugTraces.disableResource (TraceConst.CST_ACTION_LABEL_LOGFILE) ;

      // add a menu to the 'window' menu
      this.plugTraces.createResource (100,TraceConst.CST_RES_MENU_WINDOW,0,"My java Plug");//$NON-NLS-1$

      // add a menu to the 'action' menu
      this.plugTraces.createResource (101,TraceConst.CST_RES_MENU_ACTION,0,"My java action Plug");//$NON-NLS-1$

      // add a label on right, autosize (0)
      this.plugTraces.createResource (102,TraceConst.CST_RES_LABEL_RIGHT,0,"My label");//$NON-NLS-1$

      // add a button on right (100 pixels)
      this.plugTraces.createResource (103,TraceConst.CST_RES_BUT_RIGHT,100,"STOP");//$NON-NLS-1$

      // add a label on left, 100 pixels
      this.plugTraces.createResource (104,TraceConst.CST_RES_LABEL_LEFT,100,"My status");//$NON-NLS-1$

      this.actionNodes = this.plugTraces.send("Actions") ; //$NON-NLS-1$
      //this.actionNodes.id = "ActionsNode" ; //$NON-NLS-1$
   
      this.beforeDeleteNodes = this.plugTraces.send("Deleted Nodes") ; //$NON-NLS-1$
      //this.beforeDeleteNodes.id = "BeforeDeletes" ; //$NON-NLS-1$
   
      this.timerNode = this.plugTraces.send("Timer") ; //$NON-NLS-1$
      //this.timerNode.id = "Timer" ; //$NON-NLS-1$
   
      this.plugTraces.send("Sample Java Plugin(9) started"); //$NON-NLS-1$
   }
   //----------------------------------------------------------------------

   /**
    * Stop the plugin
    * @see tracetool.ITracePLugin#stop()
    */
   public void stop()
   {
      this.plugTraces.debug().send("Sample Java Plugin stopped") ; //$NON-NLS-1$
      TTrace.flush();
   }

   //----------------------------------------------------------------------

    /** 
    * @see tracetool.ITracePLugin#onAction(java.lang.String, int, java.lang.String)
    * Called when the user click on a button, label or menu on a WinTrace.
    * The plugin must call WinTrace.LinkToPlugin in order to receive this event
    * @param winId Wintrace Id
    * @param resourceId Resource Id
    * @param nodeId Node id of the current selected trace (can be empty)
    * @return
    *   when true  : tracetool perform the default action
    *   when false : tracetool don't perform any action
    */
   public boolean onAction(final String winId,final int resourceId, final String nodeId)
   {
      this.actionNodes.send("OnAction. WinId : " + winId + ", ResourceId : " + resourceId + ", current NodeId : " + nodeId) ; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      // demo : disable close button
      if (resourceId == TraceConst.CST_ACTION_CLOSE_WIN) 
         return false ;
      return true ;
   }

   //----------------------------------------------------------------------

   /** 
    * Called when a node is to be deleted on a WinTrace
    * The plugin must call WinTrace.LinkToPlugin in order to receive this event
    * @param winId Wintrace Id
    * @param nodeId Node Id
    * @return
    *  when true  : tracetool delete the node
    *  when false : tracetool don't delete the node
    * @see tracetool.ITracePLugin#onBeforeDelete(java.lang.String, java.lang.String)
    */
   public boolean onBeforeDelete(final String winId, final String nodeId)
   {
      this.beforeDeleteNodes.resendRight ("last = " + nodeId) ; //$NON-NLS-1$
      if ( this.beforeDeleteNodes.id == nodeId || this.actionNodes.id == nodeId || this.timerNode.id == nodeId)    
         return false ;
      return true;
   }

   //----------------------------------------------------------------------

   /**
    * Called every 500 ms. Can be used for example to refresh labels
    * The plugin must call LinkToPlugin in order to receive this event
    * @see tracetool.ITracePLugin#onTimer()
    */
   public void onTimer()
   {
      this.plugTraces.setTextResource (102, "My status " + Utility.currentTime()) ; //$NON-NLS-1$
      this.timerNode.resendLeft ("Timer " + Utility.currentTime()) ; //$NON-NLS-1$
   }

   //----------------------------------------------------------------------





}
