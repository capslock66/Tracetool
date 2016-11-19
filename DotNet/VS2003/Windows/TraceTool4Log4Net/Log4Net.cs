
// Log4Net.CS
//
// Log4Net Appender (for 1.2.9 beta)
//
// Author : Thierry Parent
// Version : 3.0.0
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/

// NOTE : ensure you have the Log4Net assembly before compiling
using log4net.Appender;   
using log4net.Layout;
using log4net.Core;   // 1.2.9 beta
//using log4net.spi ; // 1.2.0 beta8 

using System;
using TraceTool ;

namespace TraceTool
{
	/// <summary>
	/// Appender for the Log4J library.
	/// See the log4J.properties file for sample configuration file
	/// </summary>
	public class Log4NetAppender : AppenderSkeleton 
	{
		/** target tabsheet */
		protected WinTrace log4WinTrace ; 
   
		/** Wintrace ID  */
		protected String winTraceId ;
   
		/** Wintrace title  */
		protected String winTraceTitle ;
   
		/** Immediate flush means that the traces will be flushed at the end of each append operation.  */
		protected bool immediateFlush = false;
   
		/** send log4j Location Information (filename,method and line number).   */
		protected bool sendLocationInfo = false;
   
		/** multicolumn Title to display  */
		protected String titleLayout ;
   
		/** When Object value is send, Private fields can be filtered */
		protected bool sendPrivateObjectInfo = false;    
      
		/** Log file name */
		protected String logFileName ;
   
		/** Log file type */
		protected int logMode ;
   
		//----------------------------------------------------------------------
		/// <summary>
		/// Initializes a new instance of the <see cref="Log4NetAppender" />.
		/// </summary>
		public Log4NetAppender()
		{
			this.immediateFlush = false ; 		
			this.logMode = -1 ;
		}
 
		//----------------------------------------------------------------------
		/// <summary>
		/// Initializes a new instance of the <see cref="Log4NetAppender" /> 
		/// with a specified layout.
		/// </summary>
		/// <param name="layout">The layout to use with this appender.</param>
		public Log4NetAppender(ILayout layout) 
		{
			Layout = layout;
			this.immediateFlush = false ; 		
		}

		//----------------------------------------------------------------------
		/// <summary>
		/// Gets or sets a value that indicates whether the appender will 
		/// flush at the end of each write.
		/// </summary>
		/// <remarks>
		/// <para>The default behaviour is to flish at the end of each 
		/// write. If the option is set to<c>false</c>, then the underlying 
		/// stream can defer writing to physical medium to a later time. 
		/// </para>
		/// <para>
		/// Avoiding the flush operation at the end of each append results 
		/// in a performance gain of 10 to 20 percent. However, there is safety
		/// trade-off involved in skipping flushing. Indeed, when flushing is
		/// skipped, then it is likely that the last few log events will not
		/// be recorded on disk when the application exits. This is a high
		/// price to pay even for a 20% performance gain.
		/// </para>
		/// </remarks>
		public bool ImmediateFlush 
		{
			get {return immediateFlush; }
			set {immediateFlush = value; }
		}
   
		//----------------------------------------------------------------------
		/// <summary>
		/// send log4j Location Information (filename,method and line number). Default is false.
		/// </summary>
		public bool SendLocationInfo 
		{
			get {return sendLocationInfo; }
			set {sendLocationInfo = value; }
		}

		//----------------------------------------------------------------------
		/// <summary>
		/// When Object value is send, Private fields can be filtered
		/// </summary>
		public bool SendPrivateObjectInfo
		{
			get {return sendPrivateObjectInfo; }
			set {sendPrivateObjectInfo = value; }
		}

		//----------------------------------------------------------------------

		/// <summary>
		/// Define the transport type : windows message or socket
		/// </summary>
		public String SendMode
		{
			get
			{
				if (TTrace.Options.SendMode == TraceTool.SendMode.WinMsg)
					return "WinMsg";
				else
					return "Socket";
			}
			set 
			{ 
				if (value.ToLower().CompareTo("winmsg") == 0)
					TTrace.Options.SendMode = TraceTool.SendMode.WinMsg ;
				else if (value.ToLower().CompareTo("socket") == 0)
					TTrace.Options.SendMode = TraceTool.SendMode.Socket ;
			}
		}
      
		//----------------------------------------------------------------------
      
		/// <summary>
		/// Define the target viewer IP. Default is localhost (127.0.0.1)
		/// </summary>
		public String RemoteHost
		{
			get {return TTrace.Options.SocketHost; }
			set {TTrace.Options.SocketHost = value; }
		}

		//----------------------------------------------------------------------
      
		/// <summary>
		/// Define the target viewer port. Default is 8090 
		/// </summary>
		public int RemotePort
		{
			get {return TTrace.Options.SocketPort; }
			set {TTrace.Options.SocketPort = value; }
		}
      
		//----------------------------------------------------------------------
   
		/// <summary>
		/// multicolumn Title to display 
		/// </summary>
		public String TitleLayout
		{
			get {return titleLayout; }
			set {titleLayout = value; }
		}

		//----------------------------------------------------------------------
        
		/// <summary>
		/// Wintrace ID
		/// </summary>
		public String WinTraceId
		{
			get {return winTraceId; }
			set {winTraceId = value; }
		}

		//----------------------------------------------------------------------
      
		/// <summary>
		/// Wintrace title
		/// </summary>
		public String WinTraceTitle
		{
			get {return winTraceTitle; }
			set {winTraceTitle = value; }
		}

		//----------------------------------------------------------------------
		/// <summary>
		/// Wintrace title
		/// </summary>
		public String LogFile
		{
			//get {return ; }
			set 
			{
				int pos = value.IndexOf(',') ;
				try 
				{
					logFileName = value.Substring(pos+1) ;
					logMode = Int32.Parse(value.Substring(0,pos));
				} 
				catch  
				{
					// no error
				}
			}
		}

		//----------------------------------------------------------------------
		/// <summary>
		/// This appender requires a Layout> to be set.
		/// </summary>
		/// <value><c>true</c></value>
		override protected bool RequiresLayout 
		{
			get {return true; }    // not called !
		}

		//----------------------------------------------------------------------
		/// <summary>
		/// Writes the logging event to the TraceTool system.
		/// </summary>
		/// <param name="loggingEvent">The event to log.</param>
		protected override void Append(LoggingEvent loggingEvent)
		{
			try 
			{
				// if null then first append.
				if (this.log4WinTrace == null) 
				{
					if (this.winTraceId != null || this.winTraceTitle != null) 
					{
						this.log4WinTrace = new WinTrace (this.winTraceId , this.winTraceTitle) ;
						if (this.Layout != null)
							this.log4WinTrace.SetMultiColumn () ;
					} 
					else 
					{ 
						// no wintrace specified
               
						if (this.Layout != null) 
						{
							// Layout on main trace window. create a brother main wintrace
							this.log4WinTrace = new WinTrace ("_" , "_") ; 
							this.log4WinTrace.SetMultiColumn () ;  // must be specified before setting titles
						} 
						else 
						{  // no layout and no wintrace specified, use main wintrace
							this.log4WinTrace = TTrace.WinTrace ;
						}
					}
            
					if (this.titleLayout != null && this.log4WinTrace != TTrace.WinTrace)
						this.log4WinTrace.SetColumnsTitle (this.titleLayout) ; 

					if (this.logMode >= 0)
						this.log4WinTrace.SetLogFile(this.logFileName,this.logMode) ;  
				} 
				TraceNodeEx node = new TraceNodeEx (this.log4WinTrace.Debug) ;
         
				// if layout is used, fill only the leftMsg.
				if (this.Layout != null) 
				{
					node.LeftMsg =  RenderLoggingEvent (loggingEvent) ; //  1.2.0 beta8 and 1.2.9 beta
					//node.LeftMsg =  this.Layout.Format (loggingEvent) ; // 1.2.0 b8
					node.Time = "" ;         // blank time //$NON-NLS-1$
					node.ThreadName = "" ;   // blank thread name //$NON-NLS-1$
				} 
				else 
				{
					// no layout. Use tracetool columns
            
					node.LeftMsg =  loggingEvent.LoggerName  ;
					node.RightMsg =  loggingEvent.RenderedMessage  ;
					node.ThreadName = loggingEvent.ThreadName ;
					node.Time = loggingEvent.TimeStamp.ToString("HH:mm:ss:fff") ;
            
					// to do : change icon
					//int level = event.getLevel ().toInt () ;
					//String levelstr = event.getLevel ().toString () ;
					//node.iconIndex = 8 ;
				}
         
				// add the message object if not a primitive
				Object msg = loggingEvent.MessageObject ;
				if (! (msg is string ))
					node.AddValue(msg,  this.sendPrivateObjectInfo, 3, "Trace Object") ;     
         
				// add throwable info, if any
				// GetExceptionStrRep is Obsolete but is keept for previous version compatibility  (1.2.0)
				// string strException = loggingEvent.GetExceptionString ();  
				string strException = loggingEvent.GetExceptionStrRep ();  
				if (strException != "") 
				{
					TMemberNode localInfo = node.Members.Add ("Exception informations") ; 
               
					string [] split = strException.Split(new Char[] {'\n','\r'}) ;
					foreach (string s in split) 
					{
						if (s.Trim() != "")
							localInfo.Add (s);
					}
				}
         
				// send Local information.
				if (this.sendLocationInfo) 
				{
					TMemberNode localInfo = node.Members.Add ("LocalInfo") ;
					LocationInfo locInfo = loggingEvent.LocationInformation ;
					localInfo.Add (locInfo.FileName , locInfo.MethodName , locInfo.LineNumber ) ;
				}
         
				// finally send the node
				node.Send () ;
         
				if(this.immediateFlush)
					TTrace.Flush ();
			} 
			catch  
			{ 
				// eat exception
			} 
		}

		//----------------------------------------------------------------------

	}
}
