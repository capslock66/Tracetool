using System;
using TraceTool;

namespace ConsoleAppF461
{
    class Program
    {
        static async System.Threading.Tasks.Task Main()
        {
            TTrace.Options.SocketHost = "127.0.0.1";

            Console.WriteLine("select how traces are sent to viewer");
            Console.WriteLine("1 - socket (worker thread)");       
            Console.WriteLine("2 - socket async");     
            Console.WriteLine("3 - websocket (worker thread)");   
            Console.WriteLine("4 - websocket async");
            Console.WriteLine("5 - windows msg (worker thread)");
            Console.WriteLine("6 - windows msg Async");

            int intChoice = Convert.ToInt32(Console.ReadLine());
            string strChoice;
            switch (intChoice)
            {
                case 1:
                    strChoice = "socket sync"; 
                    TTrace.Options.SendMode = SendMode.Socket;
                    TTrace.Options.SocketPort = 8090;
                    TTrace.Options.UseWorkerThread = true; // sync , default
                    break;
                case 2:
                    strChoice = "socket async"; 
                    TTrace.Options.SendMode = SendMode.Socket;
                    TTrace.Options.SocketPort = 8090;
                    TTrace.Options.UseWorkerThread = false; // async
                    break;

                case 3:
                    strChoice = "websocket sync";   
                    TTrace.Options.SendMode = SendMode.WebSocket;
                    TTrace.Options.SocketPort = 8091;
                    TTrace.Options.UseWorkerThread = true;  
                    break;

                case 4:
                    strChoice = "websocket async";
                    TTrace.Options.SendMode = SendMode.WebSocket;
                    TTrace.Options.SocketPort = 8091;
                    TTrace.Options.UseWorkerThread = false;  
                    break;

                case 5:
                    strChoice = "windows msg sync";
                    TTrace.Options.SendMode = SendMode.WinMsg;
                    TTrace.Options.UseWorkerThread = true; 
                    break;

                case 6:
                    strChoice = "windows msg async";
                    TTrace.Options.SendMode = SendMode.WinMsg;
                    TTrace.Options.UseWorkerThread = false; 
                    break;
                default:
                    return;
            }

            Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} starting test");
            TTrace.ClearAll();
            TTrace.Debug.Send($"console framework {strChoice} ", TTrace.Debug.GetType().Assembly.Location);
            TTrace.Debug.SendValue("val1", TTrace.Debug);
            for (int i = 0; i < 300; i++)
                TTrace.Debug.Send($"{i}");
            TTrace.Debug.Send($"done {strChoice}").Show();
            TTrace.Show(true);
            
            // You need to flush before stopping the application, else you will lose traces
            if (TTrace.Options.UseWorkerThread) 
                TTrace.Flush();                 // blocking
            else
                await TTrace.FlushAsync() ;     // blocking
            
            Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} last error : {TTrace.LastSocketError}");
            TTrace.CloseSocket();
            TTrace.Stop();
        }
    }
}
