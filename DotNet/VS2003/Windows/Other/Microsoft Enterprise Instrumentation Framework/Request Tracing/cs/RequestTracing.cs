
// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.ComponentModel;
using Microsoft.EnterpriseInstrumentation;
using Microsoft.EnterpriseInstrumentation.Schema;
using Microsoft.EnterpriseInstrumentation.RequestTracing;

namespace RequestTracing
{
	[RunInstaller(true)]
	public class MyProjectInstaller : ProjectInstaller {}

	public class MainClass
	{
		[STAThread]
		static void Main() 
		{
			// show request tracing
			RequestTracing tracing = new RequestTracing();
			tracing.TracedMethod_UsingParadigm();
			tracing.TraceMethod_TryFinallyParadigm();
		}
	}

	/// <summary>
	/// An example class that performs request tracing.
	/// </summary>
	class RequestTracing
	{
		// with request tracing, a RequestEventSource must be created.
		private static RequestEventSource requestEventSource = new RequestEventSource("RequestTracing.RequestTraceTest.GenerateTraceEvents");


		/// <summary>
		///  This method shows an example of request tracing using the 'using()' paradigm.
		/// </summary>
		public void TracedMethod_UsingParadigm()
		{
			using (RequestTrace request = new RequestTrace(requestEventSource))
			{
				// TraceRequestStartEvent generated implicitly
				
				// we raise an example event through the default Application EventSource. 
				// the TraceMessageEvent is also raised through the RequestEventSource.
				TraceMessageEvent.Raise("hello from using sample");

				// TraceRequestEndEvent generated implicitly
			}
		}

		/// <summary>
		///  This method shows an example of request tracing using the try..finally paradigm.
		/// </summary>
		public void TraceMethod_TryFinallyParadigm()
		{
			RequestTrace request = new RequestTrace(requestEventSource);
			// TraceRequestStartEvent generated implicitly

			try
			{
				// we raise an example event through the default Application EventSource. 
				// the TraceMessageEvent is also raised through the RequestEventSource.
				TraceMessageEvent.Raise("hello from try...finally sample");
			}
			finally
			{
				request.Dispose();
				// TraceRequestEndEvent generated implicitly
			}
		}
	}
}
