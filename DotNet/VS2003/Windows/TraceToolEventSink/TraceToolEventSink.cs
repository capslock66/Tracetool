using System;
using System.Collections ;                   // IDictionary
using System.Diagnostics;
using Microsoft.EnterpriseInstrumentation;
using Microsoft.EnterpriseInstrumentation.EventSinks;
using Microsoft.EnterpriseInstrumentation.Serialization;
using TraceTool ;

namespace TraceTool
{
	/// <summary>
	/// EIF event sink for TraceTool.
	/// EIF send traces to event sink instance declared in the EnterpriseInstrumentation.config file.
	/// See microsoft EIF documentation to create that file.
	/// </summary>
	/// <example>
	/// The following XML eventSink element is taken from the EnterpriseInstrumentation.config file.
	/// The event sink point to the TraceTool.EIFEventSink class (assembly is TraceToolEventSink.dll)
	/// <![CDATA[
	/// <eventSink name="TraceToolSink" description="Outputs events to the tracetool API."
	///    type="TraceTool.EIFEventSink, TraceToolEventSink, Version=10.1, Culture=neutral, PublicKeyToken=35bb89f90be2164b">
	///    <parameter name="RemoteHost" value="LocalHost" />
	///    <parameter name="RemotePort" value="8090" />
	///    <parameter name="WinTraceId" value="EIF" />
	///    <parameter name="WinTraceTitle" value="Microsoft EIF demo" />
	///    <parameter name="ImmediateFlush" value="true" />
	///    <parameter name="SendMode" value="WinMsg" />
	/// </eventSink>
	/// ]]>
	/// </example>

	public class TraceToolEventSink : EventSink, IEventContainerSink
	{

		private string EventSourceName ;

		/** target tabsheet */
		protected WinTrace log4WinTrace ;

		/** Wintrace ID  */
		protected String winTraceId ;

		/** Wintrace title  */
		protected String winTraceTitle ;

		/** Immediate flush means that the traces will be flushed at the end of each append operation.  */
		protected bool immediateFlush = false;

		/** Log file name */
		protected String logFileName ;

		/** Log file type */
		protected int logMode = -1;

		//----------------------------------------------------------------------

		/// <summary>
		/// Initialize the tracetool EventSink using suplied parameters.
		/// </summary>
		/// <param name="parameters">Parameters for the Event=sink (see eif config file)</param>
		/// <param name="eventSource">event Source</param>
		public TraceToolEventSink(IDictionary parameters, EventSource eventSource) : base(parameters, eventSource)
		{
			EventSourceName = eventSource.Name ;

			if (parameters == null)
				throw new ArgumentNullException("parameters");

			// Use parameters from the eventSink tag
			foreach (Object obj in parameters.Keys)
			{
				string ParamName = obj.ToString().ToLower() ;
				string ParamValue = parameters[obj].ToString() ;

				if (ParamName == "remotehost")
				{
					TTrace.Options.SocketHost = ParamValue;
				}
				if (ParamName == "remoteport")
				{
					TTrace.Options.SocketPort = Convert.ToInt32(ParamValue) ;
				}
				if (ParamName == "wintraceid")
				{
					winTraceId = ParamValue;
				}
				if (ParamName == "wintracetitle")
				{
					winTraceTitle = ParamValue;
				}
				if (ParamName == "immediateflush")
				{
					immediateFlush = Convert.ToBoolean (ParamValue);
				}
				if (ParamName == "sendmode")
				{
					if (ParamValue.ToLower().CompareTo("winmsg") == 0)
						TTrace.Options.SendMode = TraceTool.SendMode.WinMsg ;
					else if (ParamValue.ToLower().CompareTo("socket") == 0)
						TTrace.Options.SendMode = TraceTool.SendMode.Socket ;
				}
				if (ParamName == "logfile")
				{
					int pos = ParamValue.IndexOf(',') ;
					try
					{
						logFileName = ParamValue.Substring(pos+1) ;
						logMode = Int32.Parse(ParamValue.Substring(0,pos));
					}
					catch
					{
						// no error
					}
				}


			}

			if (this.winTraceId != null || this.winTraceTitle != null)
				this.log4WinTrace = new WinTrace (this.winTraceId , this.winTraceTitle) ;
			else
				this.log4WinTrace = TTrace.WinTrace ;

			if (this.logMode >= 0)
				this.log4WinTrace.SetLogFile(this.logFileName,this.logMode) ;

		}

		//----------------------------------------------------------------------

		/// <summary>
		/// write event. Not called if the eventsink implement IEventContainerSink
		/// </summary>
		/// <param name="eventToRaise"></param>
		public override void Write(object eventToRaise)
		{
			throw new NotSupportedException();
		}

		//----------------------------------------------------------------------

		/// <summary>
		/// write the event to tracetool
		/// </summary>
		/// <param name="eventContainer"></param>
		public void Write(EventContainer eventContainer)
		{
			if (eventContainer == null)
			{
				throw new ArgumentNullException("eventContainer");
			}

			// create trace node in the log4WinTrace window
			TraceNodeEx node = new TraceNodeEx (this.log4WinTrace.Debug) ;
			node.LeftMsg = EventSourceName ;
			node.RightMsg = "" ;  // should be replaced by the message field value

			// add line feed before adding fields
			node.Members.Add ("\n") ;

			// add the class name that generate the message
			node.Members.Add ("Event Type",eventContainer.EventType.ToString()) ;

			// deserialize the message using an EventEntrySerializer
			EventEntrySerializer serializer1 = new EventEntrySerializer();
			eventContainer.Evaluate(serializer1);
			EventEntry entry = serializer1.GetEventEntry();

			// get the EventLogEntryTypeID field. the value is a numeric (0 : error, 1: FailureAudit,...)
			Object field = entry["EventLogEntryTypeID"] ;
			if (field != null)
			{
				try
				{
					// convert to "error" or "FailureAudit" or ...
					EventLogEntryType eventLogEntryType = (EventLogEntryType) Enum.Parse(typeof(EventLogEntryType),field.ToString(),true);
					node.Members.Add ("EventLog EntryType",eventLogEntryType.ToString()) ;
				}
				catch {}
			}

			// display the fields (key/value) in entry
			foreach (Object key in entry.Keys)
			{
				// check if key is "message" to change the trace node title
				if (node.RightMsg == "")
					if (key.ToString().ToLower() == "message")
						node.RightMsg = entry[key].ToString() ;
				node.Members.Add (key.ToString() , entry[key].ToString());
			}
			node.Send() ;

			// to do : use the "immediateFlush" option
			//TTrace.Flush() ;
		}

		//----------------------------------------------------------------------

		/// <summary>
		/// Dispose of resources.
		/// </summary>
		/// <param name="disposing"></param>
		protected override void Dispose(bool disposing)
		{
			TTrace.Flush() ;
			if (disposing)
			{
				// Free state-managed objects.
			}
			base.Dispose(disposing);
		}
	}
}
