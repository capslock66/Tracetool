using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TraceTool;

namespace ConsoleAppF461
{
    class Program
    {
        static void Main(string[] args)
        {
            TTrace.Options.SendMode = SendMode.WinMsg ;
            TTrace.Debug.Send("console framework 4.6.1 win msg") ;
            TTrace.Flush();
            TTrace.Stop();
        }
    }
}
