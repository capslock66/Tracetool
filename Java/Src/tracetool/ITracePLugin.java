/*
 * ITracePlugin.java
 *
 * DEPRECATED code !!!
 *
 *
 * HomePage : http://www.codeproject.com/csharp/TraceTool.asp
 * Download : http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

/**
 Plugin interface.
*/
public interface ITracePLugin
{
   /** Get the plugin name.
    * @return plugin name
    */
   String getPlugName();

   /**
    * Called when the user click on a button, label or menu on a WinTrace.
    * The plugin must call WinTrace.LinkToPlugin in order to receive this event
    * @param winId Wintrace Id
    * @param resourceId Resource Id
    * @param nodeId Node id of the current selected trace (can be empty)
    * @return
    *   when true  : tracetool perform the default action
    *   when false : tracetool don't perform any action
  */
   boolean onAction(final String winId , int resourceId , String nodeId);

   /** Called when a node is to be deleted on a WinTrace.
    * The plugin must call WinTrace.LinkToPlugin in order to receive this event
    * @param winId Wintrace Id
    * @param nodeId Node Id
    * @return
    *  when true  : tracetool delete the node
    *  when false : tracetool don't delete the node
   */
   boolean onBeforeDelete(String winId , String nodeId);

   /**
    * Called every 500 ms. Can be used for example to refresh labels
    * The plugin must call LinkToPlugin in order to receive this event
    */
   void onTimer();


   /**Initialise the plugin.
    */
   void start();

   /** Stop the plugin.
    */
   void stop();
}
