using TraceTool;

namespace ConsoleApp16
{
    class Program
    {
        static void Main()
        {
            TTrace.Options.SocketHost = "127.0.0.1" ;
            TTrace.Options.SocketPort = 8090 ;
            TTrace.Debug.Send("Console net framework 4.x",TTrace.Debug.GetType().Assembly.Location) ;
            TTrace.Flush();
            TTrace.Stop();
        }
    }
}
