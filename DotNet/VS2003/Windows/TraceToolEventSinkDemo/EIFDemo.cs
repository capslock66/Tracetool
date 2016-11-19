
// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.ComponentModel;
using Microsoft.EnterpriseInstrumentation;
using Microsoft.EnterpriseInstrumentation.Schema;


/*
Part of the EnterpriseInstrumentation.config file.
The event sink point to the TraceTool.EIFEventSink class (assembly is TraceToolEventSink.dll)

<eventSink name="TraceToolSink" description="Outputs events to the tracetool API." 
type="TraceTool.EIFEventSink, TraceToolEventSink, Version=8.2, Culture=neutral, PublicKeyToken=35bb89f90be2164b">
   <parameter name="RemoteHost" value="LocalHost" />
   <parameter name="RemotePort" value="8090" />
   <parameter name="WinTraceId" value="EIF" />
   <parameter name="WinTraceTitle" value="Microsoft EIF demo" />
   <parameter name="ImmediateFlush" value="true" />
   <parameter name="SendMode" value="WinMsg" /> 		
</eventSink>

*/

namespace EifDemo 
{
   [RunInstaller(true)]
   public class MyProjectInstaller : ProjectInstaller {}

   /// <summary>
   /// some samples trace using EIF redirected to tracetool
   /// </summary>
   class EifDemo {
      private static EventSource myEventSource = new EventSource("EIFDemo");
      /// <summary>
      /// The main entry point for the application.
      /// </summary>
      [STAThread]
      static void Main(string[] args) {
         // sample 1 : use static ErrorMessageEvent
         ErrorMessageEvent.Raise("Error Message Event", 1, "CODE");   // msg, severity, error code

         // sample 2 : create a Trace Message Event instance, fill it then raise from the event source
         TraceMessageEvent messageEvent1  = new TraceMessageEvent();
         messageEvent1.Message = "Costly Message";
         myEventSource.Raise(messageEvent1 );

         // sample 3 : static one line (which wraps the above code sequence)
         TraceMessageEvent.Raise(myEventSource, "Static One Line Message");

         // sample 4 : static one line which is raised through the EventSource.Application EventSource.
         TraceMessageEvent.Raise("Static One Line Message through the Application event source");

      }
   }
}
