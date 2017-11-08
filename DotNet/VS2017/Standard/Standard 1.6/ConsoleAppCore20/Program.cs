using System;
using TraceTool;

namespace ConsoleAppCore
{
    class Program
    {
        static void Main(string[] args)
        {
            TTrace.Options.SocketHost = "127.0.0.1" ;
            TTrace.Options.SocketPort = 8090 ;
            TTrace.Debug.Send("Console") ;
            TTrace.Flush();
            TTrace.Stop();
        }
    }
}
