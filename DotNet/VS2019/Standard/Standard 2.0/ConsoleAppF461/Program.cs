
using TraceTool;

namespace ConsoleAppF461
{
    class Program
    {
        static void Main()
        {
            TTrace.Options.SendMode = SendMode.WebSocket ;
            TTrace.Debug.Send("console framework 4.6.1 web socket msg",TTrace.Debug.GetType().Assembly.Location) ;
            TTrace.Debug.SendValue("val1",TTrace.Debug); 

            TTrace.Flush();

            //TTrace.Stop();
        }
    }
}
