// TraceNode.CS
//
// construct the trace node
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
using System.Collections;  // ArrayList, queue
using System.Diagnostics;  // Process
using System.Reflection;
using System.Text;

// generic start in F2
#if (!NETCF1 && !NETF1)
using System.Collections.Generic;
#endif

#if (!NETCF1 && !SILVERLIGHT)
using System.Xml.XPath;
#endif


//using System.Runtime.InteropServices;     // for DDL import
//using Microsoft.Win32 ;                   // registry
//using System.IO ;                         // file exist
//using System.Threading ;                  // thread pool, ResetEvent
//using System.Net;
//using System.Net.Sockets;


namespace TraceTool
{
   /// <summary>
   /// TraceNode represent node on the viewer.
   /// </summary>
   public class TraceNode : TraceToSend
   {

      /// <summary>
      /// Constructor. Use TTrace or TraceNodeEx class as entry point
      /// Create a Node with an unique ID (true)
      /// You can also recreated an already send node if you still have the id
      /// </summary>
      /// <param name="ParentNode">The parent node where to place that trace.
      /// The IconIndex and the enabled properties are also recopied
      /// Can be null : the root tree become the parent node, enabled is true and the default icon is used
      /// </param>
      /// <param name="generateUniqueId">When true, a unique ID (a guid) is generated for the trace.
      /// </param>

      public TraceNode(TraceNode ParentNode, bool generateUniqueId)  // TraceToSend base class don't have constructor
      {
         if (generateUniqueId)
            Id = Helper.NewGuid().ToString();// else : no more reset to empty string if generateUniqueId is false

         if (ParentNode == null)
         {
            this.IconIndex = TraceConst.CST_ICO_DEFAULT;
            this.Enabled   = true;
         }
         else
         {
            this.IconIndex  = ParentNode.IconIndex;
            this.Enabled    = ParentNode.Enabled;
            this.WinTraceId = ParentNode.WinTraceId;
         }
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Copy constructor : create a TraceNode copy of a TraceToSend
      /// </summary>
      /// <param name="Source">TraceNode to copy</param>
      internal TraceNode(TraceToSend Source)   // TraceToSend base class don't have constructor
      {
         this.IconIndex  = Source.IconIndex;
         this.Enabled    = Source.Enabled;
         this.WinTraceId = Source.WinTraceId;
         this.Id         = Source.Id;
         this.Tag        = Source.Tag ;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Copy constructor : create a TraceNode copy of a TraceNodeEx
      /// </summary>
      /// <param name="Source">TraceNodeEx to copy</param>
      internal TraceNode(TraceNodeEx Source)   // TraceToSend base class don't have constructor
      {
         this.IconIndex  = Source.IconIndex;
         this.Enabled    = Source.Enabled;
         this.WinTraceId = Source.WinTraceId;
         this.Id         = Source.Id;
         this.Tag        = Source.Tag ;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Resend the left and right trace message to the viewer
      /// </summary>
      /// <param name="NewLeftMsg">new left message</param>
      /// <param name="NewRightMsg">new right message</param>
      /// <returns>The trace node</returns>
      public TraceNode Resend(string NewLeftMsg, string NewRightMsg)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);           // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_LEFT_MSG, NewLeftMsg);       // param : left string
         Helper.addCommand(CommandList, TraceConst.CST_RIGHT_MSG, NewRightMsg);      // param : right string

         // don't resend members and icon
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);

         return this;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// ReSend left trace to the server
      /// </summary>
      /// <param name="NewLeftMsg">new left message</param>
      /// <returns>The trace node</returns>
      public TraceNode ResendLeft(string NewLeftMsg)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);              // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_LEFT_MSG, NewLeftMsg);       // param : left string

         // don't resend members and icon
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// ReSend right trace to the server
      /// </summary>
      /// <param name="NewRightMsg">new right message</param>
      /// <returns>The trace node</returns>
      public TraceNode ResendRight(string NewRightMsg)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);              // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_RIGHT_MSG, NewRightMsg);      // param : right string

         // don't resend members and icon
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Change the Icon index
      /// </summary>
      /// <param name="Index">Index of the icon to use</param>
      /// <returns>The trace node</returns>
      public TraceNode ResendIconIndex(int Index)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);             // param : the node that receive the string
         Helper.addCommand(CommandList, TraceConst.CST_ICO_INDEX, Index);         // param : left string
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Change Background Color (whole line) of a node
      /// </summary>
      /// <param name="color">new background color of the node</param>
      /// <returns>The trace node</returns>
      public TraceNode SetBackgroundColor(int color)
      {
         return SetBackgroundColor(color, -1);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Change Background Color (specific column) of a node
      /// </summary>
      /// <param name="color">new background color of the node</param>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <returns>The trace node</returns>
      public TraceNode SetBackgroundColor(int color, int colId)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);             // param : the node that receive font change
         Helper.addCommand(CommandList, TraceConst.CST_BACKGROUND_COLOR, color, colId.ToString());      // param : color, colId
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <returns>The trace node</returns>
      public TraceNode SetFontDetail(int colId, bool bold)
      {
         return SetFontDetail(colId, bold, false, -1, 0, "");
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to Italic</param>
      /// <returns>The trace node</returns>
      public TraceNode SetFontDetail(int colId, bool bold, bool italic)
      {
         return SetFontDetail(colId, bold, italic, -1, 0, "");
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to Italic</param>
      /// <param name="color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. </param>
      /// <returns>The trace node</returns>
      public TraceNode SetFontDetail(int colId, bool bold, bool italic, int color)
      {
         return SetFontDetail(colId, bold, italic, color, 0, "");
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to Italic</param>
      /// <param name="color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. Use -1 to keep default color</param>
      /// <param name="size">Change font size</param>
      /// <returns>The trace node</returns>
      public TraceNode SetFontDetail(int colId, bool bold, bool italic, int color, int size)
      {
         return SetFontDetail(colId, bold, italic, color, size, "");
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to Italic</param>
      /// <param name="color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. Use -1 to keep default color</param>
      /// <param name="size">Change font size, use zero to keep normal size</param>
      /// <param name="fontName">Change font name</param>
      /// <returns>The trace node</returns>
      public TraceNode SetFontDetail(int colId, bool bold, bool italic, int color, int size, string fontName)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);              // param : guid

         StringBuilder TempStr = new StringBuilder();

         TempStr.Append(String.Format("{0,5}{1,3}", TraceConst.CST_FONT_DETAIL, colId));


         if (bold)
            TempStr.Append("1");
         else
            TempStr.Append("0");

         if (italic)
            TempStr.Append("1");
         else
            TempStr.Append("0");

         if (color != -1)
         {
            // remove Alpha blending
            color = color & 0xFFFFFF;
            // Color is coded as RGB. convert to BGR
            int B = color & 0xff;
            int G = (color >> 8) & 0xff;
            int R = (color >> 0x10) & 0xff;
            color = (B << 0x10) + (G << 8) + R;
         }

         TempStr.Append(String.Format("{0,11}{1,11}", color, size)).Append(fontName);

         CommandList.Add(TempStr.ToString());

         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// append right and left texts to an existing node
      /// </summary>
      /// <param name="LeftMsgtoAdd">left message</param>
      /// <param name="RightMsgtoAdd">right message</param>
      /// <returns>The trace node</returns>
      public TraceNode Append(string LeftMsgtoAdd, string RightMsgtoAdd)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);                  // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_APPEND_LEFT_MSG, LeftMsgtoAdd);   // param : right string
         Helper.addCommand(CommandList, TraceConst.CST_APPEND_RIGHT_MSG, RightMsgtoAdd);  // param : right string

         // don't resend members and icon
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// append left text to an existing node
      /// </summary>
      /// <param name="LeftMsgtoAdd">left message</param>
      /// <returns>The trace node</returns>
      public TraceNode AppendLeft(string LeftMsgtoAdd)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);                  // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_APPEND_LEFT_MSG, LeftMsgtoAdd);   // param : right string

         // don't resend members and icon
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// append right text to an existing node
      /// </summary>
      /// <param name="RightMsgtoAdd">right message</param>
      /// <returns>The trace node</returns>
      public TraceNode AppendRight(string RightMsgtoAdd)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);                  // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_APPEND_RIGHT_MSG, RightMsgtoAdd);  // param : right string

         // don't resend members and icon
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// append right text to an existing node
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode AppendStack()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);                  // param : guid

         TraceNodeEx result = new TraceNodeEx(this, true);  // create a node with same properties as "this" with new ID
         result.AddStackTrace(1);
         result.Members.AddToStringList(CommandList); // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }


      //----------------------------------------------------------------------
      /// <summary>
      /// Show the node in the tree (not means selected, just visible in the tree)
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode Show()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_FOCUS_NODE, Id);                  // param : guid
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Select the node in the viewer
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode SetSelected()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_SELECT_NODE, Id);                  // param : guid
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Delete the node
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode Delete()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_CLEAR_NODE, Id);                  // param : guid
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Delete children node
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode DeleteChildren()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");

         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_CLEAR_SUBNODES, Id);                  // param : guid
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set or reset the bookmark for the node
      /// </summary>
      /// <param name="Bookmarked">true/false</param>
      /// <returns>The trace node</returns>
      public TraceNode SetBookmark(bool Bookmarked) 
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");
         StringList CommandList = new StringList();

         Helper.addCommand(CommandList,  TraceConst.CST_USE_NODE , Id);
         Helper.addCommand(CommandList,  TraceConst.CST_SET_BOOKMARK , Bookmarked);
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// set a node visible or invisible
      /// </summary>
      /// <param name="Visible">true/false</param>
      /// <returns>The trace node</returns>
      public TraceNode SetVisible(bool Visible)
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");
         StringList CommandList = new StringList();

         Helper.addCommand(CommandList,  TraceConst.CST_USE_NODE , Id);             // param : the node
         Helper.addCommand(CommandList, TraceConst.CST_VISIBLE_NODE , Visible);    // param : visible flag
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set focus to next sibling
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode GotoNextSibling()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_GOTO_NEXTSIBLING , Id);
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set focus to previous sibling
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode GotoPrevSibling()
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList, TraceConst.CST_GOTO_PREVSIBLING , Id);

         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set focus to first child
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode GotoFirstChild()              
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,  TraceConst.CST_GOTO_FIRST_CHILD , Id);

         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Set focus to last child
      /// </summary>
      /// <returns>The trace node</returns>
      public TraceNode GotoLastChild()  
      {
         if (Enabled == false)
            return this;

         if (Id == "")
            throw new Exception("Node Id is null, root node cannot be modified (for now)");
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,  TraceConst.CST_GOTO_LAST_CHILD , Id);
         TTrace.SendToWinTraceClient(CommandList, WinTraceId);
         return this;
      }
   }
}       // namespace TraceTool
