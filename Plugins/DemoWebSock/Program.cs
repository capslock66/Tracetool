
using TraceTool;

namespace ConsoleAppF461
{
    class Program
    {
        static void Main()
        {
            TTrace.Options.SendMode = SendMode.WebSocket ;
            TTrace.Options.SocketHost = "127.0.0.1" ;
            TTrace.Options.SocketPort = 8091 ;
            TTrace.Debug.Send("console framework 4.6.1 web socket msg",TTrace.Debug.GetType().Assembly.Location) ;
            TTrace.Debug.SendValue("val1",TTrace.Debug);

            System.Threading.Thread.Sleep(1000);
            TTrace.Flush();  // working ?
        }
    }
}
