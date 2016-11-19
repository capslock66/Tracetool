// traceListener.cs
//
// listener,for the classic Microsoft trace
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

using System.Diagnostics;
using TraceTool;

namespace TraceTool
{
   /// <summary>
   /// TTraceListener is the trace listener, if you want to use the classic Microsoft Trace class.
   /// </summary>

   public class TTraceListener :  TraceListener
   {

      /// <summary>
      /// Specify at any time what is top node that receive the traces
      /// </summary>
      public TraceToSend listener;

      private TraceNode _currentNode ;
      /// <summary>
      /// the current node (read only)
      /// </summary>
      public TraceNode CurrentNode {get {return _currentNode;}}

      //----------------------------------------------------------------------
      /// <summary>
      /// Create a listener. TTrace.Debug is used to send traces
      /// </summary>
      public TTraceListener ()
      {
         listener = TTrace.Debug ;
         base.NeedIndent = true;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Create a listener giving a TraceNode as the parent node.
      /// </summary>
      /// <param name="TraceDoor">Specify Debug,Warning,Error or user TraceNode object</param>
      public TTraceListener (TraceNode TraceDoor)
      {
         listener = TraceDoor ;
         base.NeedIndent = true;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send message to TTrace
      /// </summary>
      /// <param name="message">the message</param>
      public override void Write(string message)
      {
         if (this.listener == null)
            return;

         if (base.NeedIndent)
            WriteIndent();

         _currentNode.AppendLeft(message);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Send message to TTrace
      /// </summary>
      /// <param name="message">the message</param>
      public override void WriteLine(string message)
      {
         if (this.listener == null)
            return;

         if (base.NeedIndent)
            WriteIndent();

         _currentNode.AppendLeft(message);
         base.NeedIndent = true;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Force creation of new trace node
      /// </summary>
      protected override void WriteIndent()
      {
         if (this.listener == null)
            return;
         _currentNode = listener.Send ("") ;
         this.NeedIndent = false;
      }
   }
}
