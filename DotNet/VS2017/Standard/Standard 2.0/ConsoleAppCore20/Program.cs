using System;
using TraceTool;

namespace ConsoleAppCore20
{
    class Program
    {
        static void Main(string[] args)
        {
            TTrace.Options.SendMode = SendMode.WinMsg ;
            TTrace.Debug.Send("Console Core win msg") ;

            //TTrace.Options.SocketHost = "127.0.0.1" ;
            //TTrace.Options.SocketPort = 8090 ;
            //TTrace.Debug.Send("Console core socket") ;
            TTrace.Flush();
            TTrace.Stop();        
        }
    }
}
