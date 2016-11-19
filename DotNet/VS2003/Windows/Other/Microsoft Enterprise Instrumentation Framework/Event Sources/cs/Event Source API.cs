
// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.ComponentModel;
using Microsoft.EnterpriseInstrumentation;
using Microsoft.EnterpriseInstrumentation.Schema;
using TraceToolEventSink ;

namespace EventSourceAPI
{
	[RunInstaller(true)]
	public class MyProjectInstaller : ProjectInstaller {}

	/// <summary>
	/// ExplicitTrace class illustrates 3 examples of explicit tracing
	/// </summary>
	class EventSourceAPI
	{
		private static EventSource myEventSource = new EventSource("EventSourceAPI");
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{

         //TraceToolEventSink.EIFEventSink, TraceToolEventSink, Version=1.0.1622.23936, Culture=neutral, PublicKeyToken=35bb89f90be2164b
         System.Console.WriteLine (typeof(TraceToolEventSink.EIFEventSink).AssemblyQualifiedName);


			// explicit
			TraceMessageEvent e1 = new TraceMessageEvent();
			e1.Message = "Costly Message";
			myEventSource.Raise(e1);

			// slightly less costly, if the EventSource will not be enabled much of the time.
			if (myEventSource.IsEnabledForType(typeof(TraceMessageEvent)))
			{
				TraceMessageEvent e2 = new TraceMessageEvent();
				e2.Message = "Less Costly Message";
				myEventSource.Raise(e2);
			}

			// static one liner (which wraps the above code sequence)
			TraceMessageEvent.Raise(myEventSource, "Static One Liner Message");

			// static one liner which is raised through the EventSource.Application EventSource.
			TraceMessageEvent.Raise("Static One Liner Message through the Application event source");

		}
	}
}
