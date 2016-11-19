/*
 * TraceNodeBase.java
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
 * Base class for TraceSend and TraceNodeEx
 * @author tpa
 */
public class TraceNodeBase
{
   /** The unique ID. Normally it's a GUID, but can be replaced by something else for inter process traces. */
   public String id;

   /** When enabled is false, all traces are disabled. Default is true. <p>
    All node have a enabled property, that lets you define group of enabled trace.<p>
    For example set the TTrace.Debug.enabled to false but <p>
    continue to accept Error and Warning traces
    */
   public boolean enabled;

   /** The parent win tree Id*/
   public String winTraceId;

   /** User variable, provided for the convenience of developers */
   public Object tag;

   /** The index of the icon to use.
    * You can then show an icon for Warning traces different for Error traces */
   public int iconIndex;

   //----------------------------------------------------------------------

   /**
    * Return the node id
    * @see java.lang.Object#toString()
    * @return Node id
    */

   public String toString()
   {
      return this.id;
   }

   //----------------------------------------------------------------------

}
