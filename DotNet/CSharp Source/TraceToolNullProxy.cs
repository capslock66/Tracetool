// TraceToolNullProxy.cs
//
// Null Method Proxy
//
// classes : TTrace , TTraceOptions , TTraceListener
//

using System;
using System.IO;

#if !DEBUG
namespace TraceTool
{
	[Flags]
	public enum TraceDisplayFlags
	{
		ShowModifiers = 1,
		ShowClassInfo = 2,
		ShowFields = 4,
		ShowCustomAttributes = 8,
		ShowNonPublic = 16,
		ShowInheritedMembers = 32,
		ShowEvents = 64,
		ShowMethods = 128,
		ShowDoc = 256,
	}

	public enum SendMode
	{
		WinMsg = 1,
		Socket = 2,
		None = 3,
	}

	public abstract class TraceNodeBase
	{
		protected TraceNodeBase() { }

		public bool Enabled;
		public int IconIndex;
		public string Id;
		public object Tag;
		public string WinTraceId;

		public override string ToString() { return string.Empty; }
	}

	public class TraceNode: TraceToSend
	{
		static readonly TraceNode _traceNode = new TraceNode( null, false );

		public TraceNode( TraceNode ParentNode, bool generateUniqueId ) { }
		public TraceNode Append( string LeftMsgtoAdd, string RightMsgtoAdd ) { return _traceNode; }
		public TraceNode AppendLeft( string LeftMsgtoAdd ) { return _traceNode; }
		public TraceNode AppendRight( string RightMsgtoAdd ) { return _traceNode; }
		public TraceNode Delete() { return _traceNode; }
		public TraceNode DeleteChildren() { return _traceNode; }
		public TraceNode GotoFirstChild() { return _traceNode; }
		public TraceNode GotoLastChild() { return _traceNode; }
		public TraceNode GotoNextSibling() { return _traceNode; }
		public TraceNode GotoPrevSibling() { return _traceNode; }
		public TraceNode Resend( string NewLeftMsg, string NewRightMsg ) { return _traceNode; }
		public TraceNode ResendIconIndex( int Index ) { return _traceNode; }
		public TraceNode ResendLeft( string NewLeftMsg ) { return _traceNode; }
		public TraceNode ResendRight( string NewRightMsg ) { return _traceNode; }
		public TraceNode SetBackgroundColor( int color, int colId ) { return _traceNode; }
		public TraceNode SetBackgroundColor( int color ) { return _traceNode; }
		public TraceNode SetBookmark( bool Bookmarked ) { return _traceNode; }
		public TraceNode SetFontDetail( int colId, bool bold, bool italic, int color, int size, string fontName ) { return _traceNode; }
		public TraceNode SetFontDetail( int colId, bool bold, bool italic, int color, int size ) { return _traceNode; }
		public TraceNode SetFontDetail( int colId, bool bold, bool italic, int color ) { return _traceNode; }
		public TraceNode SetFontDetail( int colId, bool bold, bool italic ) { return _traceNode; }
		public TraceNode SetFontDetail( int colId, bool bold ) { return _traceNode; }
		public TraceNode SetSelected() { return _traceNode; }
		public TraceNode SetVisible( bool Visible ) { return _traceNode; }
		public TraceNode Show() { return _traceNode; }
	}

	public class TraceToSend: TraceNodeBase
	{
		static readonly TraceNode _traceNode = new TraceNode( null, false );

		protected TraceToSend() { }
		public int IndentLevel { get; protected set; }

		public void EnterMethod( string leftMsg, string rightMsg, int BackGroundColor ) { }
		public void EnterMethod( string leftMsg, string rightMsg ) { }
		public void EnterMethod( string leftMsg ) { }
		public void ExitMethod( string leftMsg, string rightMsg, int BackGroundColor ) { }
		public void ExitMethod( string leftMsg, string rightMsg ) { }
		public void ExitMethod( string leftMsg ) { }
		public void ExitMethod() { }
		public void Indent( string leftMsg, string rightMsg, int BackGroundColor, bool IsEnter ) { }
		public void Indent( string leftMsg, string rightMsg, int BackGroundColor ) { }
		public void Indent( string leftMsg, string rightMsg ) { }
		public void Indent( string leftMsg ) { }
		public TraceNode Send( string leftMsg, string rightMsg ) { return _traceNode; }
		public TraceNode Send( string leftMsg ) { return _traceNode; }
		public TraceNode SendBackgroundColor( string leftMsg, int color, int colId ) { return _traceNode; }
		public TraceNode SendBackgroundColor( string leftMsg, int color ) { return _traceNode; }
		public TraceNode SendDump( string leftMsg, string ShortTitle, byte[] adr, int count ) { return _traceNode; }
		public TraceNode SendObject( string leftMsg, object ObjToSend, TraceTool.TraceDisplayFlags flags ) { return _traceNode; }
		public TraceNode SendObject( string leftMsg, object ObjToSend ) { return _traceNode; }
		public TraceNode SendTable( string leftMsg, object table ) { return _traceNode; }
		public TraceNode SendType( string leftMsg, System.Type oType, TraceTool.TraceDisplayFlags flags ) { return _traceNode; }
		public TraceNode SendType( string leftMsg, System.Type oType ) { return _traceNode; }
		public TraceNode SendValue( string leftMsg, object ObjToSend, bool sendPrivate, int maxLevel, string objTitle ) { return _traceNode; }
		public TraceNode SendValue( string leftMsg, object ObjToSend, bool sendPrivate, int maxLevel ) { return _traceNode; }
		public TraceNode SendValue( string leftMsg, object ObjToSend, bool sendPrivate ) { return _traceNode; }
		public TraceNode SendValue( string leftMsg, object ObjToSend ) { return _traceNode; }
		public TraceNode SendXml( string leftMsg, string xml ) { return _traceNode; }
		public void UnIndent( string leftMsg, string rightMsg, int BackGroundColor, bool isExit ) { }
		public void UnIndent( string leftMsg, string rightMsg, int BackGroundColor ) { }
		public void UnIndent( string leftMsg, string rightMsg ) { }
		public void UnIndent( string leftMsg ) { }
		public void UnIndent() { }
	}

	public class TTrace
	{
		static readonly TraceNode _traceNode = new TraceNode( null, false );
		static readonly WinWatch _watch = new WinWatch();
		static readonly WinTrace _winTrace = new WinTrace();

		public TTrace() { }

		public static long nbDiscarded;
		public static long nbSend;
		public static TTraceOptions Options;

		public static TraceToSend Debug { get { return _traceNode; } }
		public static TraceToSend Error { get { return _traceNode; } }
		public static TraceToSend Warning { get { return _traceNode; } }
		public static WinWatch Watches { get { return _watch; } }
		public static WinTrace WinTrace { get { return _winTrace; } }

		//public static TextWriter Out { get; protected set; }

		public static void ClearAll() { }
		public static void CloseSocket() { }
		public static void CloseViewer() { }
		public static void Find( string Text, bool Sensitive, bool WholeWord, bool Highlight, bool SearchInAllPages ) { }
		public static void Flush() { }
		public static void Show( bool IsVisible ) { }
		public static void Stop() { }
	}

	public class TTraceOptions
	{
		public TTraceOptions() { }

		public bool SendDate { get; set; }

		public int ObjectTreeDepth;
		public bool SendEvents;
		public bool SendFunctions;
		public bool SendInherited;
		public SendMode SendMode;
		public bool SendPrivate;
		public bool SendProcessName;
		public bool SendThreadId;
		public bool SendTypeWithValue;
		public string SocketHost;
		public int SocketPort;
		public bool SocketUdp;
	}

	public class WinWatch
	{
		public WinWatch() { }
		public WinWatch( string WinWatchID, string WinWatchText ) { }

		public bool Enabled;
		public string Id;
		public object Tag;

		public void ClearAll() { }
		public void Close() { }
		public void DisplayWin() { }
		public void Send( string WatchName, object WatchValue ) { }
	}

	public class WinTrace: TraceToSend
	{
		public WinTrace() { }
		public WinTrace( string WinTraceID, string WinTraceText ) { }

		public TraceToSend Debug { get; protected set; }
		public TraceToSend Error { get; protected set; }
		public TraceToSend Warning { get; protected set; }

		public void AddFilter( int Column, int Compare, string Text ) { }
		public void ApplyFilter( bool ConditionAnd, bool ShowMatch, bool IncludeChildren ) { }
		public void ClearAll() { }
		public void ClearBookmark() { }
		public void ClearFilter() { }
		public void Close() { }
		public void CreateResource( int ResId, int ResType, int ResWidth, string ResText ) { }
		public void DisableResource( int ResId ) { }
		public void DisplayWin() { }
		public void FindNext( bool SearForward ) { }
		public void GotoBookmark( int Pos ) { }
		public void GotoFirstNode() { }
		public void GotoLastNode() { }
		public void LinkToPlugin( string PluginName, int flags ) { }
		public void LoadXml( string FileName ) { }
		public void SaveToTextfile( string FileName ) { }
		public void SaveToXml( string FileName ) { }
		public void SaveToXml( string FileName, string StyleSheet ) { }
		public void SetColumnsTitle( string Titles ) { }
		public void SetColumnsWidth( string Widths ) { }
		public void SetLogFile( string FileName, int Mode ) { }
		public void SetLogFile( string FileName, int Mode, int MaxLines ) { }
		public void SetMultiColumn() { }
		public void SetMultiColumn( int MainColIndex ) { }
		public void SetTextResource( int ResId, string ResText ) { }
	}
}
#endif