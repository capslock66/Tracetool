/*
 * SendMode.java
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
 Transport layer (enumeration). Use SendMode.None to disable sending messages
*/
public class SendMode
{
   /** Socket message*/
   public static final int Socket=1;
   /** Don't send message to the viewer (Local log file in wintrace must be specified)*/
   public static final int None=2;

}
