/*
 * TraceNode.java 
 * 
 * HomePage : http://www.codeproject.com/csharp/TraceTool.asp 
 * Download : http://sourceforge.net/projects/tracetool/ 
 * See License.txt for license information 
 * 
 * Author : Thierry Parent 
 * Version : 12.4.1
 */

package tracetool;

// reflection import
import java.awt.Color;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.ArrayList;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

/**
 * That class perform the formatting of the message to send.
 * <p>
 * Nothing is send by this class (it's TTrace class job)
 * <p>
 * The constructor is internal : Instances can be created only by this assembly.
 * <p>
 * Use the TTrace or TraceNodeEx as an entry point.
 * <p>
 * @author tpa
 */
public class TraceNode extends TraceToSend
{

   /**
    * constructor : create the node using parameters from the parent.
    * @param parentNode The Parent node
    * @param generateUniqueId Indicate if the ID must be generated
    */
   protected TraceNode(final TraceNode parentNode, final boolean generateUniqueId)
   {
      if (generateUniqueId)
         this.id = new java.rmi.server.UID().toString(); // else : no more reset to empty string if generateUniqueId is false

      if (parentNode == null)
      {
         this.iconIndex = TraceConst.CST_ICO_DEFAULT;
         this.enabled = true;
      } else
      {
         this.iconIndex = parentNode.iconIndex;
         this.enabled = parentNode.enabled;
         this.winTraceId = parentNode.winTraceId;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * Copy constructor : create a TraceNode copy of a TraceSend.
    * @param Source TraceNode to copy
    */
   protected TraceNode(final TraceToSend Source) // TraceSend base class don't have constructor
   {
      this.iconIndex = Source.iconIndex;
      this.enabled = Source.enabled;
      this.winTraceId = Source.winTraceId;
      this.id = Source.id;
      this.tag = Source.tag;
   }

   // ----------------------------------------------------------------------

   /**
    * Copy constructor : create a TraceNode copy of a TraceNodeEx.
    * @param Source TraceNodeEx to copy
    */

   protected TraceNode(final TraceNodeEx Source) // TraceSend base class don't have constructor
   {
      this.iconIndex = Source.iconIndex;
      this.enabled = Source.enabled;
      this.winTraceId = Source.winTraceId;
      this.id = Source.id;
      this.tag = Source.tag;
   }

   // ----------------------------------------------------------------------
   /**
    * Override a previous send message (both column)
    * @param newLeftMsg The new Left message
    * @param newRightMsg The new Right message
    * @return The trace node
    */
   public TraceNode resend(final String newLeftMsg, final String newRightMsg)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : id (this)
         Utility.addCommand(commandList, TraceConst.CST_LEFT_MSG, newLeftMsg); // param : new left string
         Utility.addCommand(commandList, TraceConst.CST_RIGHT_MSG, newRightMsg); // param : new right string

         // don't resend members and icon
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Override a previous send message
    * @param newLeftMsg The new Left message
    * @return The trace node
    */
   public TraceNode resendLeft(final String newLeftMsg)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : id (this)
         Utility.addCommand(commandList, TraceConst.CST_LEFT_MSG, newLeftMsg); // param : new left string

         // don't resend members and icon
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Override a previous send message
    * @param newRightMsg The new Right message
    * @return The trace node
    */
   public TraceNode resendRight(final String newRightMsg)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : id (this)
         Utility.addCommand(commandList, TraceConst.CST_RIGHT_MSG, newRightMsg); // param : new right string

         // don't resend members and icon
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Change the Icon index
    * @param index Index of the icon to use
    * @return The trace node
    */

   public TraceNode resendIconIndex(final int index)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      ArrayList commandList = new ArrayList();

      Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : the node that receive the string
      Utility.addCommand(commandList, TraceConst.CST_ICO_INDEX, index); // param : icon index
      TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * Change Background Color (whole line) of a node
    * @param color new background color of the node
    * @return The trace node
    */
   public TraceNode setBackgroundColor(final int color)
   {
      return setBackgroundColor(color, -1);
   }

   // ------------------------------------------------------------------------------

   /**
    * Change Background Color (whole line) of a node
    * @param color new background color of the node
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @return The trace node
    */

   public TraceNode setBackgroundColor(final int color, final int colId)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      ArrayList CommandList = new ArrayList();

      Utility.addCommand(CommandList, TraceConst.CST_USE_NODE, this.id); // param : the node that receive font change
      Utility.addCommand(CommandList, TraceConst.CST_BACKGROUND_COLOR, color, Utility.intToStr3(colId)); // param : color, colId
      TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @return The trace node
    */

   public TraceNode setFontDetail(final int colId, final boolean bold)
   {
      return setFontDetail(colId, bold, false, null, 0, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @return The trace node
    */

   public TraceNode setFontDetail(final int colId, final boolean bold, final boolean italic)
   {
      return setFontDetail(colId, bold, italic, null, 0, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color
    * @return The trace node
    */

   public TraceNode setFontDetail(final int colId, final boolean bold, final boolean italic, final Color color)
   {
      return setFontDetail(colId, bold, italic, color, 0, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color
    * @param size Change font size, use zero to keep normal size
    * @return The trace node
    */

   public TraceNode setFontDetail(final int colId, final boolean bold, final boolean italic, final Color color, final int size)
   {
      return setFontDetail(colId, bold, italic, color, size, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color
    * @param size Change font size, use zero to keep normal size
    * @param fontName Change font name
    * @return The trace node
    */

   public TraceNode setFontDetail(final int colId, final boolean bold, final boolean italic, final Color color, final int size, final String fontName)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      ArrayList commandList = new ArrayList();

      Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : guid

      StringBuffer tempStr = new StringBuffer();

      tempStr.append(Utility.intToStr5(TraceConst.CST_FONT_DETAIL)).append(Utility.intToStr3(colId));

      if (bold)
         tempStr.append("1"); //$NON-NLS-1$
      else
         tempStr.append("0"); //$NON-NLS-1$

      if (italic)
         tempStr.append("1"); //$NON-NLS-1$
      else
         tempStr.append("0"); //$NON-NLS-1$

      // Color is coded as RGB. convert to BGR
      int colorValue;
      if (color == null)
         colorValue = -1;
      else
         colorValue = (color.getBlue() << 16) | (color.getGreen() << 8) | (color.getRed() << 0);

      tempStr.append(Utility.intToStr11(colorValue)).append(Utility.intToStr11(size)).append(fontName);

      commandList.add(tempStr.toString()); // don't use Utility.addCommand(commandList,...)

      TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Append text to a previous send message (both column)
    * @param newLeftMsg The new Left message to append
    * @param newRightMsg The new Right message to append
    * @return The trace node
    */
   public TraceNode append(final String newLeftMsg, final String newRightMsg)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : id (this)
         Utility.addCommand(commandList, TraceConst.CST_APPEND_LEFT_MSG, newLeftMsg); // param : new left string
         Utility.addCommand(commandList, TraceConst.CST_APPEND_RIGHT_MSG, newRightMsg); // param : new right string

         // don't resend members and icon
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Append text to a previous send message
    * @param newLeftMsg The new Left message to append
    * @return The trace node
    */
   public TraceNode appendLeft(final String newLeftMsg)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : id (this)
         Utility.addCommand(commandList, TraceConst.CST_APPEND_LEFT_MSG, newLeftMsg); // param : new left string

         // don't resend members and icon
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Append text to a previous send message
    * @param newRightMsg The new Right message to append
    * @return The trace node
    */
   public TraceNode appendRight(final String newRightMsg)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : id (this)
         Utility.addCommand(commandList, TraceConst.CST_APPEND_RIGHT_MSG, newRightMsg); // param : new right string

         // don't resend members and icon
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Set a node as selected in the viewer. Don't confuse with show() that force a node to be displayed
    * @return The trace node
    */
   public TraceNode setSelected()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_SELECT_NODE, this.id); // param : id (this)
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Force a node to be displayed. don't confuse with setSelected()
    * @return The trace node
    */
   public TraceNode show()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_FOCUS_NODE, this.id); // param : id (this)
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Delete the node
    * @return The trace node
    */
   public TraceNode delete()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_CLEAR_NODE, this.id); // param : id (this)
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Delete children node
    * @return The trace node
    */
   public TraceNode deleteChildren()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_CLEAR_SUBNODES, this.id); // param : id (this)
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * Set or reset the bookmark for the node
    * @param bookmarked true/false
    * @return The trace node
    */
   public TraceNode setBookmark(boolean bookmarked)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id);
         Utility.addCommand(commandList, TraceConst.CST_SET_BOOKMARK, bookmarked);
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * set a node visible or invisible
    * @param visible true/false
    * @return The trace node
    */
   public TraceNode setVisible(boolean visible)
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;
      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : the node
         Utility.addCommand(commandList, TraceConst.CST_VISIBLE_NODE, visible); // param : visible flag
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * Set focus to next sibling
    * @return The trace node
    */
   public TraceNode gotoNextSibling()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;
      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_GOTO_NEXTSIBLING, this.id);
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * Set focus to previous sibling
    * @return The trace node
    */
   public TraceNode gotoPrevSibling()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;

      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_GOTO_PREVSIBLING, this.id);
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * Set focus to first child
    * @return The trace node
    */
   public TraceNode gotoFirstChild()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;
      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_GOTO_FIRST_CHILD, this.id);
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }

      return this;
   }

   // ------------------------------------------------------------------------------

   /**
    * Set focus to last child
    * @return The trace node
    */
   public TraceNode gotoLastChild()
   {
      if (!this.enabled)
         return this;

      // "Node Id is null, root node cannot be modified (for now)"
      if (this.id.length() == 0)
         return this;
      try
      {
         ArrayList commandList = new ArrayList();
         Utility.addCommand(commandList, TraceConst.CST_GOTO_LAST_CHILD, this.id);
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return this;
   }
}

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

/**
 * Internal Use.
 * <p>
 * Needed by the stackWriter constructor, but not used
 */
class NullWriter extends Writer
{

   /**
    * Override method from java.io.Writer
    * @see java.io.Writer#close()
    */
   public void close()
   {
      // blank
   }

   /**
    * Override method from java.io.Writer
    * @see java.io.Writer#flush()
    */
   public void flush()
   {
      // blank
   }

   /**
    * Override method from java.io.Writer
    * @see java.io.Writer#write(char[], int, int)
    * @param cbuf cbuf
    * @param off off
    * @param len len
    */
   public void write(final char[] cbuf, final int off, final int len)
   {
      // stupid code to discard Checkstyle warning about unused parameter
      boolean b = false ;
      if (b)
         cbuf[0] = (char) (len + off);
   }
}

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

/*
 * * Internal Use. <p> Receive message from the printStackTrace
 */
/*
 * *
 * @author Thierry Parent
 */
class StackWriter extends PrintWriter
{
   private TMemberNode group;

   private int firstLine;

   private boolean singleLine;

   private boolean isAdded;

   private int actualLine;

   // ----------------------------------------------------------------------
   /**
    * StackWriter receive message from the printStackTrace
    * @param group Member Node where to store stack info
    * @param firstLine The starting line to take
    * @param singleLine If true only one stack line is needed
    */
   StackWriter(final TMemberNode group, final int firstLine, final boolean singleLine)
   {
      super(new NullWriter());
      this.group = group;
      this.firstLine = firstLine;
      this.singleLine = singleLine;
      this.isAdded = false;
      this.actualLine = 0;
   }

   // ----------------------------------------------------------------------
   /**
    * called for the first line : Throwable
    * @param o object to print
    * @see java.io.PrintWriter#print(java.lang.Object)
    */
   public void print(final Object o)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(o.toString());
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   //
   /**
    * Seams to be to main function called by printStackTrace()
    * @param chars Chars to add
    * @see java.io.PrintWriter#println(char[])
    */
   public void println(final char[] chars)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(new String(chars));
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   // all other functions seams to be not called

   /**
    * not called function
    * @see java.io.PrintWriter#print(char[])
    * @param chars Cahrs to print
    */
   public void print(final char[] chars)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(new String(chars));
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * not called function
    * @see java.io.PrintWriter#print(java.lang.String)
    * @param s String to print
    */
   public void print(final String s)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(s);
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * not called function
    * @see java.io.PrintWriter#println(java.lang.Object)
    * @param o Object to print
    */
   public void println(final Object o)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(o.toString());
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * not called function
    * @see java.io.PrintWriter#println(java.lang.String)
    * @param s String to print
    */
   public void println(final String s)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(s);
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * not called function
    * @see java.io.Writer#write(char[])
    * @param chars Chars to print
    */
   public void write(final char[] chars)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(new String(chars));
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * not called function
    * @see java.io.Writer#write(char[], int, int)
    * @param chars Chars to print
    * @param off Offest
    * @param len Length
    */
   public void write(final char[] chars, final int off, final int len)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(new String(chars, off, len));
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------

   /**
    * not called function
    * @see java.io.Writer#write(java.lang.String, int, int)
    * @param s String to print
    * @param off Offset
    * @param len Length
    */
   public void write(final String s, final int off, final int len)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(s.substring(off, off + len));
         this.isAdded = true;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * not called function
    * @see java.io.Writer#write(java.lang.String)
    * @param s String to print
    */
   public void write(final String s)
   {
      this.actualLine++;
      if (this.singleLine && this.isAdded)
         return;
      if (this.actualLine >= this.firstLine)
      {
         this.group.add(s);
         this.isAdded = true;
      }
   }
}
