//
// TTSocketMode.cs
//
// classes : TTSocketMode
//
// Sample IHttpModule that configure tracetool in socket mode
//
// Author : Somkutas Péter
// Version : 10.1
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information


using System;
using System.Web;
using TraceTool;

namespace TraceToolSocketMode
{
	/// <summary>
	/// Webmodule that sets TraceTool into socket mode.
	/// </summary>
	public class TTSocketMode : System.Web.IHttpModule
	{
		#region IHttpModule Members

		/// <summary>
		/// Initializes a module and prepares it to handle
		/// requests.
		/// </summary>
		/// <param name="context">An <see cref="T:System.Web.HttpApplication"/> that provides access to the methods, properties, and events common to all application objects within an ASP.NET application</param>
		public void Init(HttpApplication context)
		{
			TTrace.Options.SendMode = SendMode.Socket;
		}

		/// <summary>
		/// Disposes of the resources (other than memory) used by the
		/// module that implements <see langword="IHttpModule."/>
		/// </summary>
		public void Dispose()
		{
			// Nothing to do
		}

		#endregion
	}
}
