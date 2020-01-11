
using TraceTool;

namespace ConsoleAppF461UsingNugget
{
    class Program
    {
        static void Main()
        {
            TTrace.Options.SendMode = SendMode.WinMsg ;
            TTrace.Debug.Send("console framework 4.6.1 win msg",TTrace.Debug.GetType().Assembly.Location) ;
            TTrace.Flush();
            TTrace.Stop();        
        }
    }
}
