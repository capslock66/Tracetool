// Sample plugin for tracetool viewer
//
// Author : Thierry Parent
// Version : 10.1
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

using TraceTool;

namespace MyApp
{
	/// <summary>
	/// Summary description for Class1.
	/// </summary>

	// a demo plugin
	public class SampleCSharpPlugin : ITracePLugin
	{
		WinTrace PlugTraces;
		TraceNode ActionNodes, BeforeDeleteNodes, Timer;
		const string PlugName = "CSharpPlugin9";

		//------------------------------------------------------------------------------

		/// <summary>
		/// Get the plugin name
		/// </summary>
		public string GetPlugName()
		{
			return PlugName;
		}

		//------------------------------------------------------------------------------

		/// <summary>
		/// Initialise the plugin
		/// </summary>
		public void Start()
		{
			// create a window and ask to receive timer, action and onBeforeDelete events
			PlugTraces = new WinTrace("CSHARP", "CSharp Plugin9");
			PlugTraces.DisplayWin();
			PlugTraces.LinkToPlugin(PlugName,
				TraceConst.CST_PLUG_ONACTION +
				TraceConst.CST_PLUG_ONBEFOREDELETE +
				TraceConst.CST_PLUG_ONTIMER);

			// disable the  LogFile label
			PlugTraces.DisableResource(TraceConst.CST_ACTION_LABEL_LOGFILE);

			// add a menu to the 'window' menu
			PlugTraces.CreateResource(100, TraceConst.CST_RES_MENU_WINDOW, 0, "My CSharp Plug");

			// add a menu to the 'action' menu
			PlugTraces.CreateResource(101, TraceConst.CST_RES_MENU_ACTION, 0, "My CSharp action Plug");

			// add a label on right, autosize (0)
			PlugTraces.CreateResource(102, TraceConst.CST_RES_LABEL_RIGHT, 0, "My label");

			// add a button on right (100 pixels)
			PlugTraces.CreateResource(103, TraceConst.CST_RES_BUT_RIGHT, 100, "STOP");

			// add a label on left, 100 pixels
			PlugTraces.CreateResource(104, TraceConst.CST_RES_LABEL_LEFT, 100, "My status");

			TraceNodeEx node;
			node = new TraceNodeEx(PlugTraces.Debug);
			node.LeftMsg = "Actions";
			node.Id = "ActionsNode";
			ActionNodes = node.Send();

			node = new TraceNodeEx(PlugTraces.Debug);
			node.LeftMsg = "Deleted Nodes";
			node.Id = "BeforeDeletes";
			BeforeDeleteNodes = node.Send();

			node = new TraceNodeEx(PlugTraces.Debug);
			node.LeftMsg = "Timer";
			node.Id = "Timer";
			Timer = node.Send();

			PlugTraces.Debug.Send("Sample CSharp 10.1 Plugin started");
		}

		//------------------------------------------------------------------------------

		/// <summary>
		/// Stop the plugin
		/// </summary>
		public void Stop()
		{
			PlugTraces.Debug.Send("Sample CSharp Plugin stopped");
			TTrace.Flush();
		}

		//------------------------------------------------------------------------------

		/// <summary>
		/// Called when the user click on a button, label or menu on a WinTrace.
		/// The plugin must call WinTrace.LinkToPlugin in order to receive this event
		/// </summary>
		/// <param name="WinId">Wintrace Id</param>
		/// <param name="ResourceId">Resource Id</param>
		/// <param name="NodeId">Node id of the current selected trace (can be empty)</param>
		/// <returns>
		///  when true  : tracetool perform the default action
		///  when false : tracetool don't perform any action
		/// </returns>
		public bool OnAction(string WinId, int ResourceId, string NodeId)
		{
			ActionNodes.Send("OnAction. WinId : " + WinId + ", ResourceId : " + ResourceId + ", current NodeId : " + NodeId);
			// demo : disable close button
			if (ResourceId == TraceConst.CST_ACTION_CLOSE_WIN)
				return false;
			return true;
		}

		//------------------------------------------------------------------------------

		/// <summary>
		/// Called when a node is to be deleted on a WinTrace
		/// The plugin must call WinTrace.LinkToPlugin in order to receive this event
		/// </summary>
		/// <param name="WinId">Wintrace Id</param>
		/// <param name="NodeId">Node Id</param>
		/// <returns>
		///  when true  : tracetool delete the node
		///  when false : tracetool don't delete the node
		/// </returns>
		public bool OnBeforeDelete(string WinId, string NodeId)
		{
			BeforeDeleteNodes.ResendRight("last = " + NodeId);
			if (NodeId == "BeforeDeletes" || NodeId == "ActionsNode" || NodeId == "Timer")
				return false;
			return true;
		}

		//------------------------------------------------------------------------------

		/// <summary>
		/// Called every 500 ms. Can be used for example to refresh labels
		/// The plugin must call LinkToPlugin in order to receive this event
		/// </summary>
		public void OnTimer()
		{
			PlugTraces.SetTextResource(102, "My Timer " + System.DateTime.Now.ToString());
			Timer.ResendLeft("Timer " + System.DateTime.Now.ToString());
		}
	}
}
