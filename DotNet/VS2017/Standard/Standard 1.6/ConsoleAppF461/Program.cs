using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TraceTool;

namespace ConsoleApp16
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
