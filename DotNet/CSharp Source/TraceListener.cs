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

using System.Diagnostics;

// ReSharper disable ClassNeverInstantiated.Global
// ReSharper disable ConvertIfStatementToNullCoalescingExpression
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable IntroduceOptionalParameters.Global
// ReSharper disable FieldCanBeMadeReadOnly.Global
// ReSharper disable UnusedMethodReturnValue.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable InlineOutVariableDeclaration
// ReSharper disable UseStringInterpolation
// ReSharper disable UseObjectOrCollectionInitializer
// ReSharper disable UseNullPropagation
// ReSharper disable MergeCastWithTypeCheck
// ReSharper disable UsePatternMatching
// ReSharper disable ArrangeAccessorOwnerBody

namespace TraceTool
{
    /// <summary>
    /// TTraceListener is the trace listener, if you want to use the classic Microsoft Trace class.
    /// </summary>

    // ReSharper disable once InconsistentNaming
    public class TTraceListener : TraceListener
    {

        /// <summary>
        /// Specify at any time what is top node that receive the traces
        /// </summary>
        public readonly TraceToSend Listener;

        private TraceNode _currentNode;
        /// <summary>
        /// the current node (read only)
        /// </summary>
        public TraceNode CurrentNode { get { return _currentNode; } }

        //----------------------------------------------------------------------
        /// <summary>
        /// Create a listener. TTrace.Debug is used to send traces
        /// </summary>
        public TTraceListener()
        {
            Listener = TTrace.Debug;
            NeedIndent = true;
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Create a listener giving a TraceNode as the parent node.
        /// </summary>
        /// <param name="traceDoor">Specify Debug,Warning,Error or user TraceNode object</param>
        public TTraceListener(TraceNode traceDoor)
        {
            Listener = traceDoor;
            NeedIndent = true;
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Send message to TTrace
        /// </summary>
        /// <param name="message">the message</param>
        public override void Write(string message)
        {
            if (Listener == null)
                return;

            if (NeedIndent)
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
            if (Listener == null)
                return;

            if (NeedIndent)
                WriteIndent();

            _currentNode.AppendLeft(message);
            NeedIndent = true;
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Force creation of new trace node
        /// </summary>
        protected override void WriteIndent()
        {
            if (Listener == null)
                return;
            _currentNode = Listener.Send("");
            NeedIndent = false;
        }
    }
}
