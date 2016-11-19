/*
 * TTraceOptions.java
 *
 * HomePage : http://www.codeproject.com/csharp/TraceTool.asp
 * Download : http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

// / .
/**
 * Options for the traces (Host,Port,...)
 * @author tpa
 */
public class TTraceOptions
{

   /** The Socket Host address. Default is 127.0.01 */
   public String socketHost = "127.0.0.1"; //$NON-NLS-1$

   /** The socket port. Default is 8090. */
   public int socketPort = 8090;

   /** socket UDP mode. default is false */
   public boolean socketUDP = false;   
   
   /** indicate if the reflection should display functions and constructor. Default is false */
   public boolean sendFunctions = false;

   /** indicate if the reflection should display members from inherited classes. Default is true */
   public boolean sendInherited = true;

   /** indicate if the reflection should also display private members. Default is false */
   public boolean sendPrivate = false;

   /** indicate if the date must be send with the time. Default is false */
   public boolean sendDate = false;

   /** indicate if the thread name must be send . Default is true */
   public boolean sendThreadName = true;
   
   /** Change SendMode to SendMode.None to disable transfer */
   public int sendMode = SendMode.Socket ;


   /** Max Object tree depth for sendValue and Watches */
   public int objectTreeDepth = 3;

   /**
    * default options
    * @return default options
    */
   protected int getDefault()
   {
      // display at least public (inherited) fields and properties
      int flags = TraceDisplayFlags.showModifiers + TraceDisplayFlags.showFields;

      if (this.sendInherited) flags = flags + TraceDisplayFlags.showInheritedMembers;

      if (this.sendFunctions) flags = flags + TraceDisplayFlags.showMethods;

      if (this.sendPrivate) flags = flags + TraceDisplayFlags.showNonPublic;

      return flags;
   }
} // TTraceOptions



