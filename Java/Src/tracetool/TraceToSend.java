/*
 * TraceSend.java
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Iterator;
import java.awt.Color;

/**
 * Base class for TraceNode and WinTrace.<p>
 * Methods of TraceSend can send new traces to the viewer, but cannot be re-send <p>
 * The TTrace.debug() object ,for example, is a TraceSend instance
 * @author tpa
 */

public abstract class TraceToSend extends TraceNodeBase
{
   /**
    * InternalUse. Used to remind indentation node id
    */
   private class NodeContext
   {
      /** Thread name */
      String threadName;

      /** Node id */
      String nodeId;

      protected NodeContext()
      {
         //
      }
   }

   /** context list */
   protected ArrayList contextList;

   protected ArrayList winTraceContext;

   // ----------------------------------------------------------------------

   /**
    * prepare the minimal command List with leftmsg, trace id, ...
    * @param leftMsg The left message
    * @param newId The trace node ID
    * @return A command list
    */
   protected ArrayList prepareNewNode(final String leftMsg, final String newId)
   {
      ArrayList commandList = new ArrayList();
      Utility.addCommand(commandList, TraceConst.CST_NEW_NODE, this.getLastContextId()); // param : parent Node id (this)
      Utility.addCommand(commandList, TraceConst.CST_TRACE_ID, newId); // param : Node id
      Utility.addCommand(commandList, TraceConst.CST_LEFT_MSG, leftMsg); // param :left string
      Utility.addCommand(commandList, TraceConst.CST_ICO_INDEX, this.iconIndex); // param :Icon index
      return commandList;
   }

   // ----------------------------------------------------------------------

   /**
    * The most useful trace function : send just a string
    * @param leftMsg The message to display
    * @return A Trace node. Useful to add sub traces
    */
   public TraceNode send(final String leftMsg)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;
      try
      {
         result = new TraceNodeEx(this, true);
         ArrayList commandList = prepareNewNode(leftMsg, result.id);
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);

      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------

   /**
    * The most useful trace function : send just a string
    * @param leftMsg The message to display
    * @param rightMsg The right message
    * @return A Trace node. Useful to add sub traces
    */
   public TraceNode send(final String leftMsg, final String rightMsg)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;
      try
      {
         result = new TraceNodeEx(this, true);
         ArrayList commandList = prepareNewNode(leftMsg, result.id);
         if (rightMsg != "") //$NON-NLS-1$
            Utility.addCommand(commandList, TraceConst.CST_RIGHT_MSG, rightMsg); // param :right string
         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ------------------------------------------------------------------------------

   /**
    * Get the last context.
    * @return last context for the thread
    */
   protected NodeContext getLastContext()
   {
      ArrayList cList;
      if (this.winTraceContext != null)
         cList = this.winTraceContext;
      else if (this.contextList != null)
         cList = this.contextList;
      else
         return null;

      // no need to lock an empty list
      if (cList.size() == 0)
         return null ;

      String thName = Thread.currentThread().getName();

      synchronized (cList)
      {
         for (Iterator iter = cList.iterator(); iter.hasNext();)
         {
            NodeContext aContext = (NodeContext) iter.next();

            if (aContext.threadName.compareTo(thName) == 0)
               return aContext;
         }
      }
      return null;
   }

   // ------------------------------------------------------------------------------
   /**
    * Get the last context ID.
    * @return last context ID for the thread
    */
   protected String getLastContextId()
   {
      NodeContext aContext = getLastContext();
      if (aContext == null)
         return this.id;
      return aContext.nodeId;
   }

   // ------------------------------------------------------------------------------

   /**
    * Save the context
    * @param newContext the context to push
    */
   private void pushContext(final NodeContext newContext)
   {
      ArrayList cList;
      if (this.winTraceContext != null)
         cList = this.winTraceContext;
      else if (this.contextList != null)
         cList = this.contextList;
      else
      {
         this.contextList = new ArrayList();
         cList = this.contextList;
      }
      synchronized (cList) {
         cList.add(0, newContext);
      }
   }

   // ------------------------------------------------------------------------------

   /**
    * Delete the last context for the thread
    */
   private void deleteLastContext()
   {
      ArrayList cList;
      if (this.winTraceContext != null)
         cList = this.winTraceContext;
      else if (this.contextList != null)
         cList = this.contextList;
      else
         return;

      String thName = Thread.currentThread().getName();
      synchronized (cList)
      {
         for (Iterator iter = cList.iterator(); iter.hasNext();)
         {
            NodeContext aContext = (NodeContext) iter.next();
            if (aContext.threadName.compareTo(thName) == 0)
            {
               cList.remove(aContext) ;  // no problem with cList iterator, since the function return after delete
               return;
            }
         }
      }
   }

   // ------------------------------------------------------------------------------

   /**
    * return current indent level. See Indent()
    * @return current indent level
    */
   public int getIndentLevel()
   {

      ArrayList cList;
      if (this.winTraceContext != null)
         cList = this.winTraceContext;
      else if (this.contextList != null)
         cList = this.contextList;
      else
         return 0;

      // no need to lock an empty list
      if (cList.size() == 0)
         return 0 ;

      String thName = Thread.currentThread().getName();
      int result = 0 ;
      synchronized (cList)
      {
         for (Iterator iter = cList.iterator(); iter.hasNext();)
         {
            NodeContext aContext = (NodeContext) iter.next();

            if (aContext.threadName.compareTo(thName) == 0)
               result++;
         }
      }
      return result;
   }

   // ------------------------------------------------------------------------------
   /**
    * Send a message. further trace to the same node are indented under this one.
    * @param leftMsg Left message to send
    */
   public void indent(final String leftMsg)
   {
      indent(leftMsg, "", null, false); //$NON-NLS-1$
   }

   // ------------------------------------------------------------------------------
   /**
    * Send a message. further trace to the same node are indented under this one.
    * @param leftMsg Left message to send
    * @param rightMsg Right Message to send (optional)
    */
   public void indent(final String leftMsg, final String rightMsg)
   {
      indent(leftMsg, rightMsg, null, false);
   }

   // ------------------------------------------------------------------------------

   /**
    * Send a message. further trace to the same node are indented under this one.
    * @param leftMsg Left message to send
    * @param rightMsg Right Message to send (optional)
    * @param backGroundColor BackGround Color
    */
   public void indent(final String leftMsg, final String rightMsg, final Color backGroundColor)
   {
      indent(leftMsg, rightMsg, backGroundColor, false);
   }

   // ------------------------------------------------------------------------------
   /**
    * Send a message. further trace to the same node are indented under this one.
    * @param leftMsg Left message to send
    * @param rightMsg Right Message to send (optional)
    * @param backGroundColor BackGround Color
    * @param isEnter if true , a special "enter" icon is added on the node
    */
   public void indent(final String leftMsg, final String rightMsg, final Color backGroundColor, final boolean isEnter)
   {
      if (!this.enabled)
         return;

      String threadName = Thread.currentThread().getName();

      NodeContext newContext = new NodeContext();
      newContext.threadName = threadName;

      NodeContext lastContext = getLastContext();
      newContext.nodeId = new java.rmi.server.UID().toString();
      ArrayList commandList = new ArrayList();

      if (lastContext == null)
         Utility.addCommand(commandList, TraceConst.CST_NEW_NODE, this.id); // param : parent Node id
      else
         // context already exist
         Utility.addCommand(commandList, TraceConst.CST_NEW_NODE, lastContext.nodeId); // param : parent Node id

      Utility.addCommand(commandList, TraceConst.CST_TRACE_ID, newContext.nodeId); // param : Node Id
      Utility.addCommand(commandList, TraceConst.CST_LEFT_MSG, leftMsg); // param : left string
      if (rightMsg != "") //$NON-NLS-1$
         Utility.addCommand(commandList, TraceConst.CST_RIGHT_MSG, rightMsg); // param : right string

      if (backGroundColor != null)
      {
         int colorValue = (backGroundColor.getBlue() << 16) | (backGroundColor.getGreen() << 8) | (backGroundColor.getRed() << 0);
         Utility.addCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, "-1"); // param : color, colId //$NON-NLS-1$
      }
      if (isEnter)
      {
         TMemberNode member = new TMemberNode(); // create root member
         member.add("").viewerKind = TraceConst.CST_VIEWER_ENTER; // then add an empty member with special viewer //$NON-NLS-1$
         member.addToStringList(commandList); // convert all groups and nested items/group to strings
      }

      Utility.addCommand(commandList, TraceConst.CST_ICO_INDEX, this.iconIndex); // param : icon index
      TTrace.sendToWinTraceClient(commandList, this.winTraceId);

      pushContext(newContext);
   }

   // ------------------------------------------------------------------------------
   /**
    * Delete indentation to the node added by indent()
    */
   public void unIndent()
   {
      deleteLastContext();
   }

   // ----------------------------------------------------------------------

   /**
    * Delete indentation to the node added by indent()
    * @param leftMsg Message to send to close indentation (optional)
    */
   public void unIndent(final String leftMsg)
   {
      if (!this.enabled)
         return;

      unIndent(leftMsg, "", null, false); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------
   /**
    * Delete indentation to the node added by indent()
    * @param leftMsg Left message to send to close indentation (optional)
    * @param rightMsg Right message to send to close indentation (optional)
    */
   public void unIndent(final String leftMsg, final String rightMsg)
   {
      if (!this.enabled)
         return;
      unIndent(leftMsg, rightMsg, null, false);
   }

   // ----------------------------------------------------------------------

   /**
    * Delete indentation to the node added by indent()
    * @param leftMsg Left message to send to close indentation (optional)
    * @param rightMsg Right message to send to close indentation (optional)
    * @param backGroundColor background color (optional)
    */
   public void unIndent(final String leftMsg, final String rightMsg, final Color backGroundColor)
   {
      if (!this.enabled)
         return;
      unIndent(leftMsg, rightMsg, backGroundColor, false);
   }

   // ----------------------------------------------------------------------

   /**
    * Delete indentation to the node added by indent()
    * @param leftMsg Left message to send to close indentation (optional)
    * @param rightMsg Right message to send to close indentation (optional)
    * @param backGroundColor background color (optional)
    * @param isExit if true, viewer type 'exit' is used (optional)
    */
   public void unIndent(final String leftMsg, final String rightMsg, final Color backGroundColor, final boolean isExit)
   {
      if (!this.enabled)
         return;

      deleteLastContext();

      if (leftMsg != "" || rightMsg != "") //$NON-NLS-1$//$NON-NLS-2$
      {
         String nodeId = new java.rmi.server.UID().toString(); // then give new ID

         ArrayList CommandList = prepareNewNode(leftMsg, nodeId);

         if (rightMsg != null)
            Utility.addCommand(CommandList, TraceConst.CST_RIGHT_MSG, rightMsg); // param : right string

         if (backGroundColor != null) {
            int colorValue = (backGroundColor.getBlue() << 16) | (backGroundColor.getGreen() << 8) | (backGroundColor.getRed() << 0);
            Utility.addCommand(CommandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, "-1"); // param : color, colId //$NON-NLS-1$
         }

         if (isExit)
         {
            TMemberNode member = new TMemberNode(); // create root member
            member.add("").viewerKind = TraceConst.CST_VIEWER_EXIT; // then add an empty member with special viewer //$NON-NLS-1$
            member.addToStringList(CommandList); // convert all groups and nested items/group to strings
         }
         TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
      }
   }

   //------------------------------------------------------------------------------

   /**
    * Indent with "Enter " + left message + right message (optional) + background color (optional)
    * @param leftMsg Left message to send
    */

   public void enterMethod(final String leftMsg)
   {
      indent("Enter " + leftMsg, "", null, true); //$NON-NLS-1$ //$NON-NLS-2$
   }

   //------------------------------------------------------------------------------

   /**
    * Indent with "Enter " + left message + right message (optional) + background color (optional)
    * @param leftMsg Left message to send
    * @param rightMsg Right message to send
    */
   public void enterMethod(final String leftMsg, final String rightMsg) // : TColor = clBlack
   {
      indent("Enter " + leftMsg, rightMsg, null, true); //$NON-NLS-1$
   }

   //------------------------------------------------------------------------------

   /**
    * Indent with "Enter " + left message + right message (optional) + background color (optional)
    * @param leftMsg Left message to send
    * @param rightMsg Right message to send
    * @param backGroundColor BackGround Color
    */
   public void enterMethod(final String leftMsg, final String rightMsg, final Color backGroundColor)
   {
      indent("Enter " + leftMsg, rightMsg, backGroundColor, true); //$NON-NLS-1$
   }

   //------------------------------------------------------------------------------

   /**
    * UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
    */
   public void exitMethod()
   {
      unIndent("Exit", "", null, true); //$NON-NLS-1$//$NON-NLS-2$
   }

   //------------------------------------------------------------------------------

   /**
    * UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
    * @param leftMsg Left message to send
    */
   public void exitMethod(final String leftMsg)
   {
      unIndent("Exit " + leftMsg, "", null, true);//$NON-NLS-1$//$NON-NLS-2$
   }

   //------------------------------------------------------------------------------

   /**
    * UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
    * @param leftMsg Left message to send
    * @param rightMsg Right message to send
    */
   public void exitMethod(final String leftMsg, final String rightMsg)
   {
      unIndent("Exit " + leftMsg, rightMsg, null, true);//$NON-NLS-1$
   }

   //------------------------------------------------------------------------------

   /**
    * UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
    * @param leftMsg Left message to send
    * @param rightMsg Right message to send
    * @param backGroundColor BackGround Color
    */
   public void exitMethod(final String leftMsg, final String rightMsg, final Color backGroundColor)
   {
      unIndent("Exit " + leftMsg, rightMsg, backGroundColor, true);//$NON-NLS-1$
   }

   // ----------------------------------------------------------------------

   /**
    * Send Private and public values of an object. <p>
    * sendValue is quite different from sendObject : less verbal (no class info) but can show many level.
    * @param leftMsg The message text
    * @param objToSend the object to examine
    * @return A trace node
    * @since 4.0.0
    */
   public TraceNode sendValue(final String leftMsg, final Object objToSend)
   {
      return sendValue(leftMsg, objToSend, TTrace.options.sendPrivate);
   }

   // ----------------------------------------------------------------------

   /**
    * Send Private and public values of an object. <p>
    * sendValue is quite different from sendObject : less verbal (no class info) but can show many level.
    * @param leftMsg The message text
    * @param objToSend the object to examine
    * @param sendPrivate flag to send private field
    * @return A trace node
    * @since 4.0.0
    */
   public TraceNode sendValue(final String leftMsg, final Object objToSend, final boolean sendPrivate)
   {
      return sendValue(leftMsg, objToSend, sendPrivate, TTrace.options.objectTreeDepth);
   }

   // ----------------------------------------------------------------------
   /**
    * Send Private and public values of an object. <p>> sendValue is quite different from sendObject : less verbal (no class info) but can show many level.
    * @param leftMsg The message text
    * @param objToSend the object to examine
    * @param sendPrivate flag to send private field
    * @param maxLevel The number of sub element to display. Default is 3
    * @return A trace node
    * @since 4.0.0
    */
   public TraceNode sendValue(final String leftMsg, final Object objToSend, final boolean sendPrivate, final int maxLevel)
   {
      String title;
      if (objToSend == null)
         title = ""; //$NON-NLS-1$
      else
      {
         Class objClass = objToSend.getClass();
         if (objClass.getModifiers() == 0)
            title = objClass.getName();
         else
            title = Modifier.toString(objClass.getModifiers()) + " " + objClass.getName(); //$NON-NLS-1$
      }
      return sendValue(leftMsg, objToSend, sendPrivate, maxLevel, title);
   }

   // ----------------------------------------------------------------------
   /**
    * Send Private and public values of an object. <p>
    * sendValue is quite different from sendObject : less verbal (no class info) but can show many level.
    * @param leftMsg The message text
    * @param objToSend the object to examine
    * @param sendPrivate flag to send private field
    * @param maxLevel The number of sub element to display. Default is 3
    * @param title object title
    * @return A trace node
    * @since 4.0.0
    */
   public TraceNode sendValue(final String leftMsg, final Object objToSend, final boolean sendPrivate, final int maxLevel, final String title)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;

      try
      {

         result = new TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         ArrayList commandList = prepareNewNode(leftMsg, result.id);

         // informations are added to the members array of the new created object.
         result.addValue(objToSend, sendPrivate, maxLevel, title);

         // convert all groups and nested items/group to strings
         result.members.addToStringList(commandList);

         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------

   /**
    * Send a trace and an object (class info, fields, method) <p>
    * Take care that inspecting object is very slow (reflection is slow)
    * @param leftMsg The trace message to send
    * @param objToSend The object to inspect
    * @see #sendObject(String , Object , int )
    * @return A trace node
    */
   public TraceNode sendObject(final String leftMsg, final Object objToSend)
   {
      return sendObject(leftMsg, objToSend, TTrace.options.getDefault());
   }

   // ----------------------------------------------------------------------

   /**
    * Send a trace and an object (class info, fields, method) <p>
    * Take care that inspecting object is very slow (reflection is slow)
    * @param leftMsg The left trace message to send
    * @param objToSend The object to inspect
    * @param flags Let you specify what to send
    * @see #sendObject(String , Object , int )
    * @return A trace node
    */
   public TraceNode sendObject(final String leftMsg, final Object objToSend, final int flags)
   {
      if (!this.enabled)
         return new TraceNode(this);

      Class oType;
      TraceNodeEx result = null;

      try
      {
         if (objToSend == null)
            oType = null;
         else
            oType = objToSend.getClass();

         result = new TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         ArrayList commandList = prepareNewNode(leftMsg, result.id);

         // detect null type
         if (oType == null)
         {
            TMemberNode node = new TMemberNode("Object class is null"); //$NON-NLS-1$
            result.members.add(node);
            result.members.addToStringList(commandList);
            TTrace.sendToWinTraceClient(commandList, this.winTraceId);
            return new TraceNode(result);
         }

         // informations are added to the members array of the new created object.
         // This current instance can be the public 'Warning' node for example used by multithread applcation
         result.addTypeObject(objToSend, oType, flags);

         // convert all groups and nested items/group to strings
         result.members.addToStringList(commandList);

         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------

   /**
    * Send a trace and a type (class info, fields, method) <p>
    * Take care that inspecting object is very slow (reflection is slow)
    * @param leftMsg Trace message to display
    * @param oType The Type to inspect
    * @return a Trace node
    */
   public TraceNode sendType(final String leftMsg, final Class oType)
   {
      return sendType(leftMsg, oType, TTrace.options.getDefault());
   }

   // ----------------------------------------------------------------------

   /**
    * Send a trace and an object (class info, fields, method) <p>
    * Take care that inspecting object is very slow (reflection is slow)
    * @param leftMsg Trace message to display
    * @param oType The Type to inspect
    * @param flags Let you specify what to send
    * @return a Trace node
    */
   public TraceNode sendType(final String leftMsg, final Class oType, final int flags)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;
      try
      {
         result = new TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         ArrayList commandList = prepareNewNode(leftMsg, result.id);

         // detect null type
         if (oType == null)
         {
            TMemberNode node = new TMemberNode("Object class is null"); //$NON-NLS-1$
            result.members.add(node);
            result.members.addToStringList(commandList);
            TTrace.sendToWinTraceClient(commandList, this.winTraceId);
            return new TraceNode(result);
         }

         // informations are added to the members array of the new created object.
         // This current instance can be the public 'Warning' node for example used by multithread applcation
         result.addTypeObject(null, oType, flags);

         // convert all groups and nested items/group to strings
         result.members.addToStringList(commandList);

         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------

   /**
    * Send the call stack
    * @param leftMsg Trace message
    * @param level Level to use (0 is the current function)
    * @return a Trace node
    */
   public TraceNode sendStack(final String leftMsg, final int level)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;
      try
      {
         result = new TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         ArrayList commandList = prepareNewNode(leftMsg, result.id);
         result.addStackTrace(level + 1);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------

   /**
    * Send the caller function name
    * @param leftMsg Trace message
    * @param level Level to use (0 is the current function)
    * @return a Trace node
    */
   public TraceNode sendCaller(final String leftMsg, final int level)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;
      try
      {
         result = new TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         ArrayList commandList = prepareNewNode(leftMsg, result.id);
         result.addCaller(level + 1);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------

   /**
    * send dump
    * @param leftMsg Trace message
    * @param shortTitle A short title displayed on top of the dump
    * @param adr The byte buffer to dump
    * @param count Number of byte to dump
    * @return a Trace node
    */
   public TraceNode sendDump(final String leftMsg, final String shortTitle, final byte[] adr, final int count)
   {
      if (!this.enabled)
         return new TraceNode(this);

      TraceNodeEx result = null;
      try
      {
         result = new TraceNodeEx(this, true); // create a node with same properties as "this" with new ID
         ArrayList commandList = prepareNewNode(leftMsg, result.id);

         result.addDump(shortTitle, adr, 0, count);
         result.members.addToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.sendToWinTraceClient(commandList, this.winTraceId);
      } catch (Exception e)
      {
         // eat exception
      }
      return new TraceNode(result);
   }

   // ----------------------------------------------------------------------
   /**
    * send trace with a specific background color
    * @param leftMsg Trace message
    * @param color background color
    * @return a Trace node
   */


   public TraceNode sendBackgroundColor(final String leftMsg, final Color color)
  {
      if (!this.enabled)
         return new TraceNode(this);

    TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
    ArrayList CommandList = prepareNewNode(leftMsg, result.id);
    int colorValue = (color.getBlue() << 16) | (color.getGreen() << 8) | (color.getRed() << 0);
    Utility.addCommand(CommandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, "-1");      // param : color, colId //$NON-NLS-1$

    TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
       return new TraceNode(result);
 }

 //------------------------------------------------------------------------------

   /**
    * send trace with a specific background color
    * @param leftMsg Trace message
    * @param color background color
    * @param colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
    * @return a Trace node
   */

 public TraceNode sendBackgroundColor(final String leftMsg, final Color color, final int colId)
 {
    if (!this.enabled)
       return new TraceNode(this);

    TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
    ArrayList CommandList = prepareNewNode(leftMsg, result.id);
    int colorValue = (color.getBlue() << 16) | (color.getGreen() << 8) | (color.getRed() << 0);
    Utility.addCommand(CommandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, Utility.intToStr5(colId));      // param : color, colId

    TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
    return new TraceNode(result);
 }

 //------------------------------------------------------------------------------

 /**
  * Send xml text
  * @param leftMsg Trace message
  * @param xml xml text to send
  * @return a Trace node
 */
public TraceNode sendXml(final String leftMsg, final String xml)
 {
    if (!this.enabled)
       return new TraceNode(this);

    TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
    ArrayList CommandList = prepareNewNode(leftMsg, result.id);

    result.addXML(xml);
    result.members.addToStringList(CommandList); // convert all groups and nested items/group to strings

    TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
    return new TraceNode(result);
 }

 //------------------------------------------------------------------------------

 /**
 * Add table to node
 * @param leftMsg  Trace message
 * @param table table to send
 * @return a Trace node
 */
public TraceNode sendTable(final String leftMsg,final TraceTable table)
 {
    if (!this.enabled)
       return new TraceNode(this);

    TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
    ArrayList CommandList = prepareNewNode(leftMsg, result.id);

    result.addTable(table);
    result.members.addToStringList(CommandList); // convert all groups and nested items/group to strings

    TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
    return new TraceNode(result);
 }

//------------------------------------------------------------------------------

/**
 * Add table to node
 * @param leftMsg  Trace message
 * @param list Object collection(Array / Collection / Map) to send
 * @return a Trace node
 */
public TraceNode sendTable(final String leftMsg,final Object list)
 {
    if (!this.enabled)
       return new TraceNode(this);

    TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
    ArrayList CommandList = prepareNewNode(leftMsg, result.id);

    result.addTable(list);
    result.members.addToStringList(CommandList); // convert all groups and nested items/group to strings

    TTrace.sendToWinTraceClient(CommandList, this.winTraceId);
    return new TraceNode(result);
 }



}
