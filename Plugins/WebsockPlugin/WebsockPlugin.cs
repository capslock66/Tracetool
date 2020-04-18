// Web socket plugin for tracetool viewer
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

using System;
using System.Text; // for lowTrace
using System.IO;   // for lowTrace
using System.Net;
using System.Net.Sockets;
using Fleck;
using TraceTool;

namespace CSharpPlugin
{
    // Websock plugin
    public class WebsockPlugin : ITracePlugin
    {
        //WinTrace PlugTraces;
        
        const string PlugName = "Web sock";
        private static byte[] _buffToSend;  // buffer to send to viewer
        private static bool _isSocketError;
        private static Socket _socket;
        private static long _errorTime;

        private static WinTrace PlugTraces;
        const int labelWinsockResourceId = 102 ;

        // incoming websocket 
        private static WebSocketServer server ;
        private static string webSocketHost = "0.0.0.0";
        private static int webSocketPort = 8091;

        // outcoming tracetool socket
        private static string viewerSocketHost = "127.0.0.1";
        private static int viewerSocketPort = 8090;

        // received message count
        private static int messageCount = 0;


        //private static string _lastError = "";

        //------------------------------------------------------------------------------
#pragma warning disable CA1305 // Specify IFormatProvider
        private void LowTrace(String source)
        {
            FileStream f;
            var FileToWrite = "c:\\temp\\DotNetWrapperLog.txt";
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
#pragma warning restore CA1305 // Specify IFormatProvider

        /// <summary>
        /// ITracePlugin.GetPlugName : Get the plugin name
        /// </summary>
        public string GetPlugName()
        {
            //lowTrace("        WebSockPlugin GetPlugName\n") ;
            return PlugName;
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Initialise the plugin
        /// </summary>
        public void Start(string strParameter)
        {
            //lowTrace("        WebSockPlugin Start\n") ;

            if (string.IsNullOrEmpty(strParameter))
                strParameter = "" ;
            
            try
            {
                var paramList = strParameter.Split(',') ;
                foreach (var keyValue in paramList)
                {
                    var keyValueList = keyValue.Split('=') ;
                    var key = keyValueList[0].Trim() ;
                    var value = keyValueList[1].Trim();
                    if (string.Compare(key,"WebSocketHost", StringComparison.OrdinalIgnoreCase) == 0)
                        webSocketHost = value ;
                    else if (string.Compare(key, "WebSocketPort", StringComparison.OrdinalIgnoreCase) == 0)
                        webSocketPort = int.Parse(value);
                    else if (string.Compare(key, "ViewerSocketHost", StringComparison.OrdinalIgnoreCase) == 0)
                        viewerSocketHost = value ;
                    else if (string.Compare(key, "ViewerSocketPort", StringComparison.OrdinalIgnoreCase) == 0)
                        viewerSocketPort = int.Parse(value);
                }
            }
            catch (Exception)
            {
                TTrace.Error.Send($"{PlugName} : Wrong parameters : {strParameter}");
                TTrace.Error.Send("parameters format : WebSocketHost = 0.0.0.0, WebSocketPort = 8091, ViewerSocketHost = 127.0.0.1, ViewerSocketPort = 8090");
                throw;
            }

            TTrace.Options.SendMode = SendMode.Socket ;
            TTrace.Options.SocketHost = viewerSocketHost ;
            TTrace.Options.SocketPort = viewerSocketPort ;

            // create a window and ask to receive timer (ignore action and onBeforeDelete events)
            PlugTraces = new WinTrace("_", "");  // match the default Wintrace. No new windows is created

            // add a hyperlink label on left, 100 pixels
            PlugTraces.CreateResource(labelWinsockResourceId, TraceConst.CST_RES_LABELH_LEFT, 100, "Web Socket");

            PlugTraces.LinkToPlugin(PlugName,
                TraceConst.CST_PLUG_ONACTION 
                //+TraceConst.CST_PLUG_ONBEFOREDELETE 
                //+TraceConst.CST_PLUG_ONTIMER
                );

            //TTrace.Debug.Send($"WebsockPlugin started with param {strParameter}") ;

            FleckLog.Level = LogLevel.Error ;

            // FlecK WebSocketServer
            server = new WebSocketServer($"ws://{webSocketHost}:{webSocketPort}");  
            server.Start(socket =>
            {
                //socket.OnOpen = () =>
                //{
                //    PlugTraces.Debug.Send("Websock Opened");
                //};
                //socket.OnClose = () =>
                //{
                //    PlugTraces.Debug.Send("Websock Closed");
                //};
                //socket.OnMessage = message =>
                //{
                //    PlugTraces.Debug.Send($"OnMessage", message);
                //};
                socket.OnBinary = buffer =>
                {
                    messageCount++ ;
                    //PlugTraces.Debug.Send($"Message, Len = {buffer.Length}");
                    //PlugTraces.SetTextResource(102, $"WebSock {buffer.Length}");
                    _buffToSend = buffer;
                    SendMessageToSocket();
                };
            });
            
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Design", "CA1031:Do not catch general exception types", Justification = "<Pending>")]
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

            if (_socket == null)
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

            // if the socket is disconnect since the last use, reopen it
            if (_socket.Connected == false)
            {
                // resolve adress

                IPHostEntry hostEntry = Dns.GetHostEntry(viewerSocketHost);

                // Don't get the first adress. It's perhaps a IPv6 adress.
                // Thanks BCheng for the IPV4 fix.
                IPEndPoint endPoint = null;
                foreach (IPAddress ip in hostEntry.AddressList)
                {
                    if (ip.AddressFamily == AddressFamily.InterNetwork)
                    {
                        endPoint = new IPEndPoint(ip, viewerSocketPort);
                        break;
                    }
                }

                if (endPoint == null)
                    return;

                try
                {
                    _socket.Connect(endPoint);
                }
                catch (Exception) //ex
                {
                    _socket = null; // force recreate socket
                    _isSocketError = true;
                    _errorTime = DateTime.Now.Ticks;
                    //_lastError = ex.Message;         // for debug purpose
                    return;
                }

            } // _socket.Connected == false

            try
            {
                _socket.Send(_buffToSend, 0, _buffToSend.Length, 0);
            }
            catch (Exception) // ex
            {
                _socket = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                //_lastError = ex.Message;         // for debug purpose
            }
        } // SendMessageToSocket function

        //------------------------------------------------------------------------------

        /// <summary>
        /// Stop the plugin
        /// </summary>
        public void Stop()
        {
            //lowTrace("        WebSockPlugin Stop\n") ;
            //PlugTraces.Debug.Send("Websock Plugin stopped");
            server.Dispose() ;
            server = null ;
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
            try
            {
                if (ResourceId != labelWinsockResourceId)
                    return true;

                PlugTraces.Debug.Send("Incoming websocket", $"{webSocketHost}:{webSocketPort}");
                PlugTraces.Debug.Send("Tracetool viewer", $"{viewerSocketHost}:{viewerSocketPort}");
                PlugTraces.Debug.Send("Received message count", $"{messageCount}");

            }
            catch (Exception e)
            {
                LowTrace($"OnAction {e.Message}") ;
                PlugTraces.Error.Send($"websock plugin : OnAction exception {e.Message}");
                throw;
            }
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
            //lowTrace("        WebSockPlugin onBeforeDelete\n") ;
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
            //lowTrace("        WebSockPlugin OnTimer\n") ;
            //TTrace.Debug.Send("OnTimer");
        }

    }
}
