/*
 * TraceDisplayFlags.java
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
 * List of constant.<p>
 * Specify what information to display by SendObject function.<p>
 * sendObject, sendType, addObject  and addType have an optional "flags" in argument (int).<p>
 * If not specified, the flag is calculated from the TTrace options (class TTraceOptions)<p>
 * You can combine some of theses flags like in the following sample :<p>
 *     TTrace.debug().sendObject("this (short)", this, TraceDisplayFlags.ShowFields + TraceDisplayFlags.ShowModifiers) ;<p>
 * @author tpa
 */
public class TraceDisplayFlags
{
   /** show modifiers (public class,...)  */
   public static final int showModifiers        = 1  ;
   /** show class info (assembly,guid,...) and bases classes names  */
   public static final int showClassInfo        = 2  ;
   /** show fields values  */
   public static final int showFields           = 4  ;
   /** show attributes. Java 5 (not yet implemented) and C# */
   public static final int ShowCustomAttributes = 8  ;
   /** show non public (private and protected) members  */
   public static final int showNonPublic        = 16 ;
   /** show Inherited members  */
   public static final int showInheritedMembers = 32 ;
   /** show events (delegates)  */
   public static final int showEvents           = 64 ;
   /** show methods and constructors  */
   public static final int showMethods          = 128 ;
}

