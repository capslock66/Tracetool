using System;
using TraceTool;

namespace ConsoleAppCore20UsingNugget
{
    class Program
    {
        static void Main()
        {
            TTrace.Options.SendMode = SendMode.WinMsg ;
            TTrace.Debug.Send("Console Core 2.0 win msg",TTrace.Debug.GetType().Assembly.Location) ;

            //TTrace.Options.SocketHost = "127.0.0.1" ;
            //TTrace.Options.SocketPort = 8090 ;
            //TTrace.Debug.Send("Console core socket") ;
            TTrace.Flush();
            TTrace.Stop();   
        }
    }
}
