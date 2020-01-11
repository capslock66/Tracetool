using System.Reflection;
using TraceTool;

namespace ConsoleAppCore11UsingNugget
{
    class Program
    {
        static void Main()
        {
            TTrace.Options.SocketHost = "127.0.0.1" ;
            TTrace.Options.SocketPort = 8090 ;
            TTrace.Debug.Send("Console net Core 1.1",TTrace.Debug.GetType().GetTypeInfo().Assembly.Location) ;
            TTrace.Flush();
            TTrace.Stop();        
        }
    }
}
