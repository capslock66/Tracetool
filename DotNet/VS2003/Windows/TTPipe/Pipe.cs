///TTPIPE.EXE - A simple console program to take the input stream
///and output each line to the Trace Tool Viewer via sockets.
///Copyright (C) 2005  Michael Kramer
///
///This program is free software; you can redistribute it and/or
///modify it under the terms of the GNU General Public License
///as published by the Free Software Foundation; either version 2
///of the License, or (at your option) any later version.
///
///This program is distributed in the hope that it will be useful,
///but WITHOUT ANY WARRANTY; without even the implied warranty of
///MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///GNU General Public License for more details.
///
///You should have received a copy of the GNU General Public License
///along with this program; if not, write to the Free Software
///Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
///
///This program links to the following libraries.
///		TraceTool   :  http://www.codeproject.com/csharp/TraceTool.asp
///		CommandLine :  http://www.codeproject.com/csharp/command_line.asp 
///Please consult with these projects for their licensing terms.



using System;
using TraceTool;
using CommandLine.Utility;


namespace TTPipe
{
	/// <summary>
	/// A simple console program to take the input stream
	/// and output each line to the Trace Tool Viewer via sockets.
	/// This can be very usefull if for example you need to
	/// watch the output of a console program that is running
	/// in the windows scheduler. 
	/// 
	/// Author : Mike Kramer
	/// email  : mkramer@rightangles.com
	/// Version : 1.0
	///
	/// HomePage :  
	/// Download :  
	///
	/// Usage: YourProg | TTPIPE [-t TabName] [-i Viewer Host IP Address] [-p Viewer Port]
	/// 
	/// </summary>
	class Pipe
	{
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static int Main(string[] args)
		{
			//A TraceTool
			WinTrace myWinTrace=null;

			bool firstTime=true; //Don't do any work if no input stream to process
			string line;

			//Inititalize Tracing Framework for socket delivery to viewer!!
			TTrace.Options.SendMode = SendMode.Socket ;  

			//Process command line args
			Arguments CommandLine=new Arguments(args);

			// Look for specific arguments values and process
			// them if they exist.
			if(CommandLine["h"] != null || CommandLine["help"] != null || CommandLine["?"] != null) 
			{
				usage();
				return 0;
			}

			if(CommandLine["t"] != null) 
				//Must be Viewer tab name
				myWinTrace = new WinTrace(CommandLine["t"] , CommandLine["t"]) ;
			else
				myWinTrace=new WinTrace();

			if(CommandLine["i"] != null) 
				//Set the host IP address
				TTrace.Options.SocketHost = CommandLine["i"];
					
			if(CommandLine["p"] != null) 
				//Set the host port
				TTrace.Options.SocketPort = Convert.ToInt32(CommandLine["p"]);
					
			//Loop while there are lines to read from the input stream
			
			while ((line = Console.ReadLine()) != null) 
			{
				//Do once the first time
				if(firstTime)
				{
					firstTime=false;
					myWinTrace.Debug.Indent("===========================================================");
				}

				//pipe input stream line to log viewer
				myWinTrace.Debug.Send(line);
			}
			TTrace.Flush();
			TTrace.CloseSocket();
			return 0;
		}

		private static void usage()
		{
			Console.WriteLine("Usage:");
			Console.WriteLine("       YourProg | TTPIPE [-t TabName] [-i Viewer Host IP Address] [-p Viewer Port]");
			Console.WriteLine("");
		}

	}//End Class
}
