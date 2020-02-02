// Sample plugin for tracetool viewer
//
// Author : Thierry Parent
// Version : 12.9
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

using System;
using System.IO;
using System.Text;
using System.Runtime.Serialization;
using System.Collections.Generic;
using System.Linq;

using System.Threading;
using System.Threading.Tasks;

using System.Net;
using System.Net.Sockets;
using Fleck;
using TraceTool;

namespace CSharpPlugin
{
    // Websock plugin
    //[Serializable]
    public class WebsockPlugin : ITracePLugin //, ISerializable MarshalByRefObject
    {
        WinTrace PlugTraces;
        
        const string PlugName = "WebsockPlugin";
        private static byte[] _buffToSend;  // buffer to send to viewer
        private static bool _isSocketError;
        private static Socket _socket;
        private static long _errorTime;

        private static string SocketHost = "127.0.0.1";
        private static int SocketPort;
        private static string _lastError = "";

        //public WebsockPlugin()
        //{
        //}

        //protected WebsockPlugin(SerializationInfo serializationInfo, StreamingContext streamingContext)
        //{
        //    throw new NotImplementedException();
        //}

        //------------------------------------------------------------------------------
        private void trace(String source)
        {
            FileStream f;
            var FileToWrite = "c:\\temp\\DotNetWrapperLog.txt";

            // check if exist

            if (File.Exists(FileToWrite) == false)
            {
                f = new FileStream(FileToWrite, FileMode.Create);
            }
            else
            {  // append only the node
                f = File.Open(FileToWrite, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite);
                f.Seek(0, SeekOrigin.End);
            }
            var info = new UTF8Encoding(true).GetBytes(DateTime.Now.ToString("yyyyMMdd HH:mm:ss:fff ") + source);
            f.Write(info, 0, info.Length);
            f.Close();
        }


        /// <summary>
        /// ITracePLugin.GetPlugName : Get the plugin name
        /// </summary>
        public string GetPlugName()
        {
            //trace("        WebSockPlugin GetPlugName\n") ;
            return PlugName;
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Initialise the plugin
        /// </summary>
        public void Start()
        {
            //trace("        WebSockPlugin Start\n") ;

            TTrace.Options.SendMode = SendMode.Socket ;
            TTrace.Options.SocketHost = "127.0.0.1" ;
            TTrace.Options.SocketPort = 8090 ;
            
            // create a window and ask to receive timer (ignore action and onBeforeDelete events)
            PlugTraces = new WinTrace("Websock", "Websock Plugin");
            PlugTraces.DisplayWin();
            PlugTraces.LinkToPlugin(PlugName, TraceConst.CST_PLUG_ONTIMER);

            PlugTraces.Debug.Send("Websock plugin started");
            

            /*
            FleckLog.Level = LogLevel.Debug;
            var allSockets = new List<IWebSocketConnection>();
            var server = new WebSocketServer("ws://0.0.0.0:8091");
            server.Start(socket =>
            {
                socket.OnOpen = () =>
                {
                    PlugTraces.Debug.Send("Websock Opened");
                    allSockets.Add(socket);
                };
                socket.OnClose = () =>
                {
                    PlugTraces.Debug.Send("Websock Closed");
                    allSockets.Remove(socket);
                };
                socket.OnBinary = buffer =>
                {
                    PlugTraces.Debug.Send($"Message, Len = {buffer.Length}");
                    _buffToSend = buffer;
                    //buffer.CopyTo(_buffToSend, 0);
                    SendMessageToSocket();
                };

                socket.OnMessage = message =>
                {
                    Console.WriteLine(message);
                    allSockets.ToList().ForEach(s => s.Send("Echo: " + message));
                };
            });
            */
        }

        internal static void SendMessageToSocket()
        {
            if (_isSocketError)
            {
                long actTime = DateTime.Now.Ticks;

                if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                    _isSocketError = false;
                else
                    return;  // lose message
            }

            if (string.IsNullOrEmpty(SocketHost))   // TODO : use config file
                SocketHost = "127.0.0.1";

            if (SocketPort == 0)
                SocketPort = 8090;

            if (_socket == null)
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

            // if the socket is disconnect since the last use, reopen it
            if (_socket.Connected == false)
            {
                // resolve adress

                IPHostEntry hostEntry = Dns.GetHostEntry(SocketHost);

                // Don't get the first adress. It's perhaps a IPv6 adress.
                // Thanks BCheng for the IPV4 fix.
                IPEndPoint endPoint = null;
                foreach (IPAddress ip in hostEntry.AddressList)
                {
                    if (ip.AddressFamily == AddressFamily.InterNetwork)
                    {
                        endPoint = new IPEndPoint(ip, SocketPort);
                        break;
                    }
                }

                if (endPoint == null)
                    return;

                try
                {
                    _socket.Connect(endPoint);
                }
                catch (Exception ex)
                {
                    _socket = null; // force recreate socket
                    _isSocketError = true;
                    _errorTime = DateTime.Now.Ticks;
                    _lastError = ex.Message;         // for debug purpose
                    return;
                }

            } // _socket.Connected == false

            try
            {
                _socket.Send(_buffToSend, 0, _buffToSend.Length, 0);
            }
            catch (Exception ex)
            {
                _socket = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
            }
        } // SendMessageToSocket function

        //------------------------------------------------------------------------------

        /// <summary>
        /// Stop the plugin
        /// </summary>
        public void Stop()
        {
            //trace("        WebSockPlugin Stop\n") ;
            PlugTraces.Debug.Send("Websock Plugin stopped");
            TTrace.Flush();
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Called when the user click on a button, label or menu on a WinTrace.
        /// The plugin must call WinTrace.LinkToPlugin in order to receive this event
        /// </summary>
        /// <param name="WinId">Wintrace Id</param>
        /// <param name="ResourceId">Resource Id</param>
        /// <param name="NodeId">Node id of the current selected trace (can be empty)</param>
        /// <returns>
        ///  when true  : tracetool perform the default action
        ///  when false : tracetool don't perform any action
        /// </returns>
        public bool OnAction(string WinId, int ResourceId, string NodeId)
        {
            //trace("        WebSockPlugin OnAction\n");
            PlugTraces.Debug.Send("OnAction. WinId : " + WinId + ", ResourceId : " + ResourceId + ", current NodeId : " + NodeId);            
            return true;
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Called when a node is to be deleted on a WinTrace
        /// The plugin must call WinTrace.LinkToPlugin in order to receive this event
        /// </summary>
        /// <param name="WinId">Wintrace Id</param>
        /// <param name="NodeId">Node Id</param>
        /// <returns>
        ///  when true  : tracetool delete the node
        ///  when false : tracetool don't delete the node
        /// </returns>
        public bool OnBeforeDelete(string WinId, string NodeId)
        {
     
            //trace("        WebSockPlugin onBeforeDelete\n") ;
            //TTrace.Debug.Send("onBeforeDelete") ;
            return true;
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Called every 500 ms. Can be used for example to refresh labels
        /// The plugin must call LinkToPlugin in order to receive this event
        /// </summary>
        public void OnTimer()
        {

            //trace("        WebSockPlugin OnTimer\n") ;
            //TTrace.Debug.Send("OnTimer");

        }

    }
}
