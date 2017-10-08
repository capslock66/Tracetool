// TraceToSend.CS
//
// Base class (abstract) for TraceNode and Wintrace.
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//
// Change the tracetool project option ("conditional compilation constant") to specify the target dot net version :
// NETF1  (dot net framework 1)          , NETF2 ((dot net framework 2) ,
// NETCF1 (dot net compact framework 1)  , NETCF2 (dot net compact framework 2) , NETCF3 (dot net compact framework 3)

using System;
//using System.Text;
//using System.Collections;  // ArrayList, queue

// generic start in F2
#if (!NETCF1 && !NETF1)
//using System.Collections.Generic;
#endif

namespace TraceTool
{

   /// TraceToSend methodes create new traces and send it to the viewer
   /// Common base class for TraceNode and WinTrace
   /// TTrace.warning, debug and error are TraceNode
   public abstract class TraceToSend : TraceNodeBase   // no base constructor
   {

      internal NodeContextList ContextList;              // context list
      internal NodeContextList WinTraceContext = null;   // reference to winTrace Context (fContextList). Can be null

      /// <summary>
      /// The most useful function to send trace
      /// <example> This sample shows how to send a sample trace.
      /// <code>
      /// TTrace.Debug.Send ("Hello world") ;
      /// </code>
      /// <code>
      /// TraceNode FirstNode ;
      /// FirstNode = TTrace.Debug.Send ("Hello") ;
      /// FirstNode.Send ("World") ;   // add a second node under the first one
      /// </code>
      /// </example>
      /// </summary>
      /// <param name="leftMsg">The message to display in the 'traces' column</param>
      /// <returns>the new node</returns>
      public TraceNode Send(string leftMsg)
      {
         if (Enabled == false)
            return new TraceNode(this);
         // create a node with same properties as "this" with new ID
         TraceNodeEx result = new TraceNodeEx(this, true);
         StringList commandList = PrepareNewNode(leftMsg, result.Id);
         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Send a trace specifying the text for 2 columns
      /// <example> This sample shows how to send a sample trace.
      /// <code>
      /// TTrace.Debug.Send ("Hello", "world") ;  // 2 columns trace
      /// </code>
      /// </example>
      /// </summary>
      /// <param name="leftMsg">The message in the "traces" column</param>
      /// <param name="rightMsg">The message in the "Comment" column</param>
      /// <returns>the new node</returns>
      public TraceNode Send(string leftMsg, string rightMsg)
      {
         if (Enabled == false)
            return new TraceNode(this);
         // create a node with same properties as "this" with new ID
         TraceNodeEx result = new TraceNodeEx(this, true);
         StringList commandList = PrepareNewNode(leftMsg, result.Id);
         if (rightMsg != "")
            Helper.AddCommand(commandList, TraceConst.CST_RIGHT_MSG, rightMsg);    // param : right string

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send a 'reflected' representation of the given object
      /// </summary>
      /// <param name="leftMsg">the message to display</param>
      /// <param name="objToSend">The object to inspect</param>
      /// <returns>the new node</returns>
      public TraceNode SendObject(string leftMsg, Object objToSend)
      {
         // no need to check if enabled, done in the other function
         return SendObject(leftMsg, objToSend, TTrace.Options.GetDefault());
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send a 'reflected' representation of the given object
      /// </summary>
      /// <param name="leftMsg">the message to display</param>
      /// <param name="objToSend">The object to inspect</param>
      /// <param name="flags">what information to display</param>
      /// <returns>the new node</returns>
      public TraceNode SendObject(string leftMsg, Object objToSend, TraceDisplayFlags flags)
      {
         Type oType;

         if (Enabled == false)
            return new TraceNode(this);

         if (objToSend == null)
            oType = null;
         else
            oType = objToSend.GetType();

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID

         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         // detect null type
         if (oType == null)
         {
            TMemberNode node = new TMemberNode("Null Type");
            node.ViewerKind = TraceConst.CST_VIEWER_OBJECT ;
            result.Members.Add(node);
            result.Members.AddToStringList(commandList);
            TTrace.SendToWinTraceClient(commandList, WinTraceId);
            return new TraceNode(result);
         }

         // informations are added to the Members array of the new created object.
         // This current instance can be the public 'Warning' node for example used by multi thread application
         result.AddTypeObject(objToSend, oType, flags);
         result.Members.AddToStringList(commandList);   // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send the Value of the given object (useful for base type, variant and array)
      /// properties and array content are also inspected with a maximum
      /// </summary>
      /// <param name="leftMsg">the message to display</param>
      /// <param name="objToSend">The object to show</param>
      /// <returns>the new node</returns>
      public TraceNode SendValue(string leftMsg, Object objToSend)
      {
         return SendValue(leftMsg, objToSend, TTrace.Options.SendPrivate);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send the Value of the given object (useful for base type, variant and array)
      /// </summary>
      /// <param name="leftMsg">the message to display</param>
      /// <param name="objToSend">The object to show</param>
      /// <param name="sendPrivate">Send Private fields (default is false)</param>
      /// <returns>the new node</returns>
      public TraceNode SendValue(string leftMsg, Object objToSend, bool sendPrivate)
      {
         return SendValue(leftMsg, objToSend, sendPrivate, TTrace.Options.ObjectTreeDepth);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send the Value of the given object (useful for base type, variant and array)
      /// </summary>
      /// <param name="leftMsg">the message to display</param>
      /// <param name="objToSend">The object to show</param>
      /// <param name="sendPrivate">Send Private fields (default is false)</param>
      /// <param name="maxLevel">Max level to inspect (default is 3)</param>
      /// <returns>the new node</returns>
      public TraceNode SendValue(string leftMsg, Object objToSend, bool sendPrivate, int maxLevel)
      {
         string strModifier = "";
         string strName = "";
         try
         {
            if (objToSend == null)
            {
               strModifier = "Null Object";
            }
            else
            {
               Type oType = objToSend.GetType();
               ReflectionHelper.Type2String(oType, ref strModifier, ref strName);
               if (strModifier != "")
                  strModifier = strModifier + " ";
            }
         }
         catch
         {
            // no error
         }

         return SendValue(leftMsg, objToSend, sendPrivate, maxLevel, strModifier + strName);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send the Value of the given object (useful for base type, variant and array)
      /// </summary>
      /// <param name="leftMsg">the message to display</param>
      /// <param name="objToSend">The object to show</param>
      /// <param name="sendPrivate">Send Private fields (default is false)</param>
      /// <param name="maxLevel">Max level to inspect (default is 3)</param>
      /// <param name="objTitle">Title of the object</param>
      /// <returns>the new node</returns>
      public TraceNode SendValue(string leftMsg, Object objToSend, bool sendPrivate, int maxLevel, string objTitle)
      {

         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         // informations are added to the Members array of the new created object the actual object.
         // This current instance can be the public 'Warning' node for example used by multi thread application
         result.AddValue(objToSend, sendPrivate, maxLevel, objTitle);
         result.Members.AddToStringList(commandList);   // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Send a 'reflected' representation of the given type
      /// </summary>
      /// <param name="leftMsg">The message to display</param>
      /// <param name="oType">The type to inspect</param>
      /// <returns>the new node</returns>
      public TraceNode SendType(string leftMsg, Type oType)
      {
         // no need to check if enabled, done in the other function
         return SendType(leftMsg, oType, TTrace.Options.GetDefault());
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Send a 'reflected' representation of the given type
      /// </summary>
      /// <param name="leftMsg">The message to display</param>
      /// <param name="oType">The type to inspect</param>
      /// <param name="flags">flags to limit information to send</param>
      /// <returns>the new node</returns>
      public TraceNode SendType(string leftMsg, Type oType, TraceDisplayFlags flags)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID

         StringList commandList = PrepareNewNode(leftMsg, result.Id);
         // detect null type
         if (oType == null)
         {
            TMemberNode node = new TMemberNode("Null Type");
            node.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
            result.Members.Add(node);
            result.Members.AddToStringList(commandList);
            TTrace.SendToWinTraceClient(commandList, WinTraceId);
            return new TraceNode(result);
         }

         // informations are added to the Members array of the new created object the actual object
         // the current instance can be the public 'Warning' node for example used by multi thread application
         result.AddTypeObject(null, oType, flags);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------
#if (!NETCF1 && !NETCF2 && !NETCF3)
       /// <summary>
       /// Send the stack frames.
       /// </summary>
       /// <param name="leftMsg">Trace message</param>
       /// <returns>the new node</returns>
       public TraceNode SendStack(string leftMsg)
       {
           return SendStack(leftMsg, 0);
       }

       /// <summary>
      /// Send the stack frames.
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="level">Number of call to skip</param>
      /// <returns>the new node</returns>
      public TraceNode SendStack(string leftMsg, int level)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID

         StringList commandList = PrepareNewNode(leftMsg, result.Id);


         result.AddStackTrace(level + 1);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send the caller frame.
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="level">Level 0 is self</param>
      /// <returns>the new node</returns>
      public TraceNode SendCaller(string leftMsg, int level)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID

         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddCaller(level + 1);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //------------------------------------------------------------------------------

#if (!SILVERLIGHT)

      /// <summary>
      /// Send a bitmap
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="image">The Image</param>
      /// <returns>the new node</returns>
      public TraceNode SendBitmap(string leftMsg, System.Drawing.Image image)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddBitmap(image);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }
#endif

#endif

// currently not possibe in silverlight 2 : 
// - No way to read the Image content
// - BmpBitmapEncoder is not supported
#if (NETF3) 
      /// <summary>
      /// Send a bitmap
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="image">The Image</param>
      /// <returns>the new node</returns>
      public TraceNode SendBitmap(string leftMsg, System.Windows.Controls.Image image)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddBitmap(image);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

#endif

      //----------------------------------------------------------------------
      /// <summary>
      /// Send byte dump.
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="shortTitle">Tite to display in the first col</param>
      /// <param name="adr">Pointer to the buffer to dump</param>
      /// <param name="count">Number of byte to dump</param>
      /// <returns>the new node</returns>
      public TraceNode SendDump(string leftMsg, string shortTitle, byte[] adr, int count)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddDump(shortTitle, adr, 0, count);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// send trace with a specific background color
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="color">RGB background color (see Color.ToArgb function)</param>
      /// <returns>the new node</returns>
      public TraceNode SendBackgroundColor(string leftMsg, int color)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);
         Helper.AddCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, Helper.ARGB_to_BGR(color), "-1");      // param : color, colId

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// send trace with a specific background color
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="color">RGB background color (see Color.ToArgb function)</param>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <returns>the new node</returns>
      public TraceNode SendBackgroundColor(string leftMsg, int color, int colId) // = -1
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);
         Helper.AddCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, Helper.ARGB_to_BGR(color), colId.ToString());      // param : color, colId

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Send xml text
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="xml">xml text to send</param>
      /// <returns>the new node</returns>
      public TraceNode SendXml(string leftMsg, string xml)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddXML(xml);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Add table to node
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="table">table to send</param>
      /// <returns>the new node</returns>
      public TraceNode SendTable(string leftMsg, TraceTable table)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddTable(table);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }

      //------------------------------------------------------------------------------
      
      /// <summary>
      /// Add table to node
      /// </summary>
      /// <param name="leftMsg">Trace message</param>
      /// <param name="table">Object table to send. Must be an Array or IEnumerable or IDictionary</param>
      /// <returns>the new node</returns>
      public TraceNode SendTable(string leftMsg, Object table)
      {
         if (Enabled == false)
            return new TraceNode(this);

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         StringList commandList = PrepareNewNode(leftMsg, result.Id);

         result.AddTable(table);
         result.Members.AddToStringList(commandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(commandList, WinTraceId);
         return new TraceNode(result);
      }
      /*
       * The code is not necessary, because the "SendTable(string leftMsg, Object table)" perform the same job : IEnumerable<T> inherit from IEnumerable
      #if (!NETCF1 && !NETF1)
            /// <summary>
            /// Add table to node
            /// </summary>
            /// <param name="leftMsg">Trace message</param>
            /// <param name="table">IEnumerable Object table to send.</param>
            /// <returns>the new node</returns>
            public TraceNode SendTable<T>(string leftMsg, IEnumerable<T> table)
            {
               if (Enabled == false)
                  return new TraceNode(this);

               TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
               StringList CommandList = prepareNewNode(leftMsg, result.Id);

               result.AddTable<T>(table);
               result.Members.AddToStringList(CommandList); // convert all groups and nested items/group to strings

               TTrace.SendToWinTraceClient(CommandList, WinTraceId);
               return new TraceNode(result);
            }
      #endif
       * */

      //----------------------------------------------------------------------

      /// Prepare the commandList. Common to all SendXXX function
      internal StringList PrepareNewNode(string leftMsg, string newId)
      {
         StringList commandList = new StringList();
         //AddCommand (CommandList, TraceConst.CST_NEW_NODE, Id) ;           // param : parent Node id
         Helper.AddCommand(commandList, TraceConst.CST_NEW_NODE, GetLastContextId());           // param : parent Node id
         Helper.AddCommand(commandList, TraceConst.CST_TRACE_ID, newId);        // param : guid
         Helper.AddCommand(commandList, TraceConst.CST_LEFT_MSG, leftMsg);        // param : left string
         if (IconIndex != -1)
            Helper.AddCommand(commandList, TraceConst.CST_ICO_INDEX, IconIndex);
         return commandList;
      }
      //----------------------------------------------------------------------

      /**
      * Get the last context.
      * @return last context for the thread
      */
      internal NodeContext GetLastContext()
      {
         NodeContextList cList;

         if (WinTraceContext != null)
            cList = WinTraceContext;
         else if (ContextList != null)
            cList = ContextList;
         else
            return null ;

         if (cList.Count == 0)
            return null ;
         string thId = Helper.GetCurrentThreadId();
         lock (cList)
         {
            foreach (NodeContext aContext in cList)
            {
               if (aContext.ThreadId == thId)
                  return aContext;
            }
         }
         return null;
      }

      //------------------------------------------------------------------------------
      /**
      * Get the last context ID.
      * @return last context ID for the thread
      */
      internal string GetLastContextId()
      {
         NodeContext aContext = GetLastContext();
         if (aContext == null)
            return Id;
         return aContext.NodeId;
      }

      //------------------------------------------------------------------------------

      /**
      * Save the context
      * @param newContext the context to push
      */
      internal void PushContext(NodeContext newContext)
      {
         NodeContextList cList;

         if (WinTraceContext != null)
            cList = WinTraceContext;
         else if (ContextList != null)
            cList = ContextList;
         else
         {
            ContextList = new NodeContextList();
            cList = ContextList;
         }

         lock (cList)
         {
            cList.Insert(0, newContext);
         }
      }

      //------------------------------------------------------------------------------
      /**
       * Delete the last context for the thread
       */
      internal void DeleteLastContext()
      {
         NodeContextList cList;
         if (WinTraceContext != null)
            cList = WinTraceContext;
         else if (ContextList != null)
            cList = ContextList;
         else
            return ;

         string thId = Helper.GetCurrentThreadId();
         lock (cList)
         {
            foreach (NodeContext aContext in cList)
            {
               if (aContext.ThreadId == thId)
               {
                  cList.Remove(aContext);   // no problem with cList iterator, since the function return after delete
                  return;
               }
            }
         }
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// return current indent level. See Indent()
      /// </summary>
      /// <returns>current indent level</returns>
      public int IndentLevel
      {
         get
         {
            NodeContextList cList;

            if (WinTraceContext != null)
               cList = WinTraceContext;
            else if (ContextList != null)
               cList = ContextList;
            else
               return 0;

            // no need to lock an empty list
            if (cList.Count == 0)
               return 0;

            string thId = Helper.GetCurrentThreadId();
            int result = 0 ;
            lock (cList)
            {
               foreach (NodeContext aContext in cList)
               {
                  if (aContext.ThreadId == thId)
                     result++;
               }
            }
            return result;
         }
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Send a message. further trace to the same node are indented under this one.
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      public TraceNode Indent(string leftMsg)
      {
         return Indent (leftMsg,null,-1,false) ;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Send a message. further trace to the same node are indented under this one.
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      public TraceNode Indent(string leftMsg, string rightMsg)
      {
         return Indent(leftMsg, rightMsg, -1, false);
      }
      //------------------------------------------------------------------------------

      /// <summary>
      /// Send a message. further trace to the same node are indented under this one.
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      /// <param name="backGroundColor">RGB BackGround Color (see Color.ToArgb function)</param>
      public TraceNode Indent(string leftMsg, string rightMsg, int backGroundColor)
      {
         return Indent(leftMsg, rightMsg, backGroundColor, false);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Send a message. further trace to the same node are indented under this one.
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      /// <param name="backGroundColor">BackGround Color</param>
      /// <param name="isEnter">if true , a special "enter" icon is added on the node</param>
      public TraceNode Indent(string leftMsg, string rightMsg, int backGroundColor, bool isEnter)
      {
         if (Enabled == false)
            return null;
         string thId = Helper.GetCurrentThreadId();


         NodeContext newContext = new NodeContext() ;
         newContext.ThreadId = thId ;
         newContext.NodeId = Helper.NewGuid().ToString();

         TraceNode result = new TraceNode(this);
         result.Id = newContext.NodeId;

         StringList commandList = new StringList();
         NodeContext lastContext = GetLastContext();
         if (lastContext == null)
         {
            //newContext.level = 1 ;
            Helper.AddCommand(commandList, TraceConst.CST_NEW_NODE, Id);              // param : parent Node id
         }
         else
         {  // context already exist
            //newContext.level = lastContext.level + 1 ;
            Helper.AddCommand(commandList, TraceConst.CST_NEW_NODE, lastContext.NodeId);   // param : parent Node id
         }

         Helper.AddCommand(commandList, TraceConst.CST_TRACE_ID, newContext.NodeId);   // param : Node Id
         Helper.AddCommand(commandList, TraceConst.CST_LEFT_MSG, leftMsg);              // param : left string

         if (rightMsg != null && rightMsg != "")
            Helper.AddCommand(commandList, TraceConst.CST_RIGHT_MSG, rightMsg);        // param : right string

         if (backGroundColor != -1)
            Helper.AddCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, Helper.ARGB_to_BGR(backGroundColor), "-1");      // param : color, colId

         if (isEnter)
         {
            TMemberNode member = new TMemberNode();                     // create root member
            member.Add("").ViewerKind = TraceConst.CST_VIEWER_ENTER;    // then add an empty member with special viewer
            member.AddToStringList(commandList);                        // convert all groups and nested items/group to strings
         }

         Helper.AddCommand(commandList, TraceConst.CST_ICO_INDEX, IconIndex);          // param : icon index
         TTrace.SendToWinTraceClient(commandList, WinTraceId);

         PushContext(newContext) ;
         return result;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Delete indentation to the node added by indent()
      /// </summary>
      public void UnIndent()
      {
         if (Enabled == false)
            return ;
         DeleteLastContext();
      }
      //----------------------------------------------------------------------

      /// <summary>
      /// Delete indentation to the node added by indent()
      /// </summary>
      /// <param name="leftMsg">Message to send to close indentation (optional)</param>
      public void UnIndent(String leftMsg)
      {
         if (Enabled == false)
            return ;
         UnIndent(leftMsg, "", -1, false);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Delete indentation to the node added by indent()
      /// </summary>
      /// <param name="leftMsg">Message to send to close indentation (optional)</param>
      /// <param name="rightMsg">Message to send to close indentation (optional)</param>
      public void UnIndent(String leftMsg, String rightMsg)
      {
         if (Enabled == false)
            return ;
         UnIndent(leftMsg, rightMsg, -1, false);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Delete indentation to the node added by indent()
      /// </summary>
      /// <param name="leftMsg">Message to send to close indentation (optional)</param>
      /// <param name="rightMsg">Message to send to close indentation (optional)</param>
      /// <param name="backGroundColor">RGB background color (optional)(see Color.ToArgb function)</param>
      public void UnIndent(String leftMsg, String rightMsg, int backGroundColor)
      {
         if (Enabled == false)
            return;
         UnIndent(leftMsg, rightMsg, backGroundColor, false);
      }


      //------------------------------------------------------------------------------

      /// <summary>
      /// Delete indentation to the node added by indent()
      /// </summary>
      /// <param name="leftMsg">Message to send to close indentation (optional)</param>
      /// <param name="rightMsg">Message to send to close indentation (optional)</param>
      /// <param name="backGroundColor">RGB background color (optional)(see Color.ToArgb function)</param>
      /// <param name="isExit">if true, viewer type 'exit' is used</param>
      public void UnIndent(String leftMsg, String rightMsg, int backGroundColor, bool isExit)
      {
         if (Enabled == false)
            return;

         DeleteLastContext();

         if (leftMsg != null || rightMsg != null)
         {
            String nodeId = Helper.NewGuid().ToString();  // then give new ID

            StringList commandList = PrepareNewNode(leftMsg,nodeId );

            if (rightMsg != null)
               Helper.AddCommand(commandList, TraceConst.CST_RIGHT_MSG, rightMsg);    // param : right string

            if (backGroundColor != -1)
               Helper.AddCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, Helper.ARGB_to_BGR(backGroundColor), "-1");      // param : color, colId

            if (isExit)
            {
               TMemberNode member = new TMemberNode();                     // create root member
               member.Add("").ViewerKind = TraceConst.CST_VIEWER_EXIT;     // then add an empty member with special viewer
               member.AddToStringList(commandList);                        // convert all groups and nested items/group to strings
            }
            TTrace.SendToWinTraceClient(commandList, WinTraceId);
         }
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Indent with "Enter " + left message + right message (optional) + background color (optional)
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      public TraceNode EnterMethod(String leftMsg)
      {
         return Indent("Enter " + leftMsg, null, -1, true);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Indent with "Enter " + left message + right message (optional) + background color (optional)
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      public TraceNode EnterMethod(String leftMsg, String rightMsg)  // : TColor = clBlack
      {
         return Indent("Enter " + leftMsg, rightMsg, -1, true);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Indent with "Enter " + left message + right message (optional) + background color (optional)
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      /// <param name="backGroundColor">RGB BackGround Color(see Color.ToArgb function)</param>
      public TraceNode EnterMethod(String leftMsg, String rightMsg, int backGroundColor)
      {
         return Indent("Enter " + leftMsg, rightMsg, backGroundColor, true);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
      /// </summary>
      public void ExitMethod()
      {
         UnIndent("Exit" , null, -1, true);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      public void ExitMethod  (String leftMsg)
      {
         UnIndent("Exit " + leftMsg, null, -1, true);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      public void ExitMethod  (String leftMsg , String rightMsg)
      {
         UnIndent("Exit " + leftMsg, rightMsg, -1, true);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
      /// </summary>
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      /// <param name="backGroundColor">RGB BackGround Color(see Color.ToArgb function)</param>
      public void ExitMethod  (String leftMsg , String rightMsg , int backGroundColor)
      {
         UnIndent("Exit " + leftMsg, rightMsg, backGroundColor,true);
      }

   }     // TraceNode class

   //----------------------------------------------------------------------
   // InternalUse. Used to remind indentation node id

   internal class NodeContext
   {
      public string ThreadId;
      public String NodeId;
   }

   //----------------------------------------------------------------------

}
