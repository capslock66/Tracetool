// TraceNodeBase.CS
//
// Base class for TraceToSend(TraceNode and Wintrace) and TraceNodeEx.
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

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
    /// base class for TraceToSend (TraceNode, Wintrace) and traceNodeEx
    /// </summary>
    public abstract class TraceNodeBase
    {
        /// <summary>
        /// The unique ID. Normally it's a GUID, but can be replaced by something else for inter process traces.
        /// </summary>
        public string Id;
        /// <summary>
        /// When Enabled is false, all traces are disabled. Default is true.
        /// All node have a Enabled property, that lets you define group of Enabled trace.
        /// For example set the TTrace.Debug.enabled to false but continue to accept Error and Warning traces
        /// </summary>
        public bool Enabled;
        /// <summary>
        /// The window where trace is send.
        /// </summary>
        public string WinTraceId;
        /// <summary>
        /// User variable, provided for the convenience of developers
        /// </summary>
        public object Tag;
        /// <summary>
        /// The index of the icon to use. You can then show an icon for Warning traces different for Error traces
        /// </summary>
        public int IconIndex;

        /// <summary>
        /// return the node id
        /// </summary>
        /// <returns>node id</returns>
        public new string ToString()
        {
            return Id;
        }
    }
}
