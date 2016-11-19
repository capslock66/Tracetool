
// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.ComponentModel;
using Microsoft.EnterpriseInstrumentation;
using Microsoft.EnterpriseInstrumentation.Schema;

namespace SimpleInstrumentation
{
	[RunInstaller(true)]
	public class MyProjectInstaller : ProjectInstaller {}

	/// <summary>
	/// A simple example of raising an event.
	/// </summary>
	public class HelloWorld
	{
		static void Main()
		{
			// Raise a TraceMessageEvent event through the Application EventSource.
			TraceMessageEvent.Raise("Hello World");
		}
	}
}
