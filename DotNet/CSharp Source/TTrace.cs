// TTrace.cs
//
// classes : TTrace , TTraceOptions , TTraceListener
//
// Provide access to the warning, eror and debug node and

// send the trace using socket or windows messages
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information

// History :
// 12.7  : 2015 10 06 : Some method no more raise exception.
// 12.7   : 2015 10 06 : Fix AssemblyDocumentationFileName exception
// 12.7   : 2015 10 07 : Added default options in TTraceOptions (from TraceDisplayFlags)
// 12.7   : 2015 10 26 : Added SendStack with default level
// 12.7   : 2015 11 08 : TraceNode constructor is public
// 12.7   : 2015 11 08 : TraceNode.AppendStack()
// 12.7   : 2015 11 08 : TraceToSend.Indent() and TraceToSend.EnterMethod() returns a TraceNode
// 12.8   : 2016 07 26 : Fix stack trace with Lambda
// 12.8   : 2016 07 28 : Fix addDependencyPropertiesValues, DisplayDependencyProperties
// 12.8.5 : 2016 10 05 : stack trace display the class of the method. Modifier is removed (public static,...)
// 12.9   : 2017 11 03 : Add conditional compilation for Dot Net Standard (1.6)
// 12.9   : 2017 11 06 : Add conditional compilation for Dot Net Standard (2.0)
// 13.0   : 2020 04 05 : Add WebSocket mode, add Async mode, remove pocket pc, Silverlight, upgrade dot net framework 4.7.1

using System.Text;
using System;
using System.Collections.Generic;

#if !NETSTANDARD1_6  
using System.Runtime.InteropServices;
using System.Net.WebSockets;
#endif

#if !NETSTANDARD1_6 && !NETSTANDARD2_0  
using System.Configuration;
using System.Diagnostics ;                // Process
#endif

#if !NETSTANDARD2_0  
using System.Reflection ;
#endif

using System.Threading;                  // thead pool, ResetEvent
using System.Net;
using System.Net.Sockets;
using System.IO;                         // file exist
using System.Xml;
using System.Threading.Tasks;

//using System.Collections.Generic;

#if !NETSTANDARD1_6 && !NETSTANDARD2_0
using Microsoft.Win32 ;                   // registry
#endif

// ReSharper disable ClassNeverInstantiated.Global
// ReSharper disable ConvertIfStatementToNullCoalescingExpression
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable IntroduceOptionalParameters.Global
// ReSharper disable FieldCanBeMadeReadOnly.Global
// ReSharper disable UnusedMethodReturnValue.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable InlineOutVariableDeclaration
// ReSharper disable UseStringInterpolation
// ReSharper disable UseObjectOrCollectionInitializer
// ReSharper disable UseNullPropagation
// ReSharper disable MergeCastWithTypeCheck
// ReSharper disable UsePatternMatching
// ReSharper disable ArrangeAccessorOwnerBody

namespace TraceTool
{
    /// <summary>
    /// TTrace is the entry point for all traces.
    /// TTrace give 3 'TraceNode' doors : Warning , Error and Debug.
    /// Theses 3 doors are displayed with a special icon (all of them have the 'enabled' property set to true.
    /// That class is fully static.
    /// </summary>

    // ReSharper disable once InconsistentNaming
    public class TTrace
    {
        // internal members
        //-----------------

        private static readonly AutoResetEvent DataReady;     // data is ready to send
        private static readonly ManualResetEvent StopEvent;
        private static readonly CancellationTokenSource cancellationTocket;

        private static readonly Dictionary<String, Object> FlushList;    // TaskCompletionSource<string> flush list
        private static readonly InternalWinTrace DefaultWinTrace;
        private static readonly List<InternalWinTrace> FormTraceList;
        private static Thread _traceThread;
        private static List<List<string>> _msgQueue;         // store all messages to send
        private static long _errorTime;
        private static string _lastError = "";
        private static WinTrace _winTrace;
        private static WinWatch _winWatch;
        private static byte[] _buffToSend;  // buffer to send to viewer

        private static Socket _socket;                       // Normal socket
        private static bool isAsyncRunning = false;  // indicate if SendToViewerAsync is waiting connections or sending to viewer (await)
        private static bool _isSocketError;

        private static TextWriter _writterOut;

#if !NETSTANDARD1_6
        private static ClientWebSocket webSocketClient;     // Web socket
#endif


#if NETSTANDARD1_6
        // no udp for NETSTANDARD1_6. You need to switch to NETSTANDARD2_0
        //private static UdpClient _udpSocket;
#else //full framework or standard2
        private static UdpClient _udpSocket;
#endif

        // ReSharper disable NotAccessedField.Global
        // ReSharper disable RedundantDefaultMemberInitializer

        /// number of message discarded (socket error)
        public static long NbDiscarded = 0;

        /// number of message send 
        public static long NbSend = 0;

        // ReSharper restore RedundantDefaultMemberInitializer
        // ReSharper restore NotAccessedField.Global

        /// TTrace Options (socket, show functions, ...)
        public static TTraceOptions Options;

        //static initializer , initialise the class
        static TTrace()
        {
            //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} TTrace static init");
            FlushList = new Dictionary<String, Object>();              // TaskCompletionSource<string> flush list
            FormTraceList = new List<InternalWinTrace>();
            DefaultWinTrace = new InternalWinTrace();       // main InternalWinTrace. Id is empty
            FormTraceList.Add(DefaultWinTrace);
            _msgQueue = new List<List<string>>();              // store all messages to send
            Options = new TTraceOptions();           // TTrace Options (socket, show functions, ...)

            // create the lock system and the thread
            DataReady = new AutoResetEvent(false);  // initial state false
            StopEvent = new ManualResetEvent(false);   // ask the thread to quit
            cancellationTocket = new CancellationTokenSource();

            //_traceThread = new Thread(SendThreadEntryPoint); //  new Thread(SendThreadEntryPoint);
            //// force the thread to be killed when all foreground thread are terminated.
            //_traceThread.IsBackground = true;  
            //_traceThread.Start();

            try
            {   // since 12.5 : protect if configuration file is bad
                _isSocketError = false;
                //_socket   : is initialized on first use
                //_winTrace : is initialized on first use
                //_winWatch : is initialized on first use

                // ReSharper disable once JoinDeclarationAndInitializer
                string configFile;

#if NETSTANDARD1_6 || NETSTANDARD2_0
                Options.SendMode = SendMode.Socket;
                Options.SocketHost = "127.0.0.1";

                // 1) check the existence of the App.exe.Config file (with "exe" extension) using classic framework
                //    no API for that in dot Net Standard

                // 2) check the existence of the MyProg.config file (without "exe" or similar extension !)
                configFile = Helper.GetCurrentProcessName() + ".config";
                if (GetConfigFomFile(configFile))
                    return;

                // 3) check the existence of the MyProg.TraceTool file (without "exe" or similar extension !)
                configFile = Helper.GetCurrentProcessName() + ".TraceTool";
                if (GetConfigFomFile(configFile))
                    return;

                // 4) check the existence of tracetool.dll.TraceTool file
#if NETSTANDARD1_6
                configFile = typeof(TTrace).GetTypeInfo().Assembly.ToString();
#else //2.0
                configFile = typeof(TTrace).Module.ToString();
#endif
                configFile = configFile + ".TraceTool";
                if (GetConfigFomFile(configFile))
                    return;

                // 5) last chance : a "traceTool.xml" file in the current folder (compatible in compact, classic or standard)
                configFile = "traceTool.xml";
                GetConfigFomFile(configFile);

#else  // Full framework
                // ReSharper disable once JoinDeclarationAndInitializer
                XmlNode config;

                // 1) check the existence of the App.exe.Config file (with "exe" extension) using classic framework API
#if NETF2
                config = (XmlNode) ConfigurationManager.GetSection("TraceTool");
#else
                config = (XmlNode) System.Configuration.ConfigurationSettings.GetConfig("TraceTool") ;
#endif

                if (config != null) {
                   //Debug.Send ("App.Config file exist") ;
                   ReadConfig(config);
                   return;
                }

                // 2) check the existence of the Web.Config file or MyProg.exe.Config (with extension)
                configFile = AppDomain.CurrentDomain.SetupInformation.ConfigurationFile;
                if (GetConfigFomFile(configFile))
                   return;

                // 3) check the existence of the MyProg.exe.TraceTool file (with extension)
                configFile = AppDomain.CurrentDomain.SetupInformation.ConfigurationFile;
                configFile = configFile.Replace(".config", ".TraceTool");
                if (GetConfigFomFile(configFile))
                   return;

                // 4) check the existence of tracetool.dll.TraceTool file
                Assembly asm = typeof(TTrace).Assembly;
                configFile = asm.Location;                  // Bin/Debug/tracetool.dll
                configFile = configFile + ".TraceTool";     // Bin/Debug/tracetool.dll.TraceTool
                if (GetConfigFomFile(configFile))
                   return;

                // 5) last chance : a "traceTool.xml" file in the current folder (compatible in compact, classic or standard)
                configFile = "traceTool.xml";
                GetConfigFomFile(configFile) ;

#endif // full framework


            }
            catch (Exception ex)
            {
                _lastError = ex.Message;
            }

        }  // TTrace constructor

        //------------------------------------------------------------------------------

        // get the configuration from specified file
        private static bool GetConfigFomFile(string fileName)
        {
            try
            {
                if (!File.Exists(fileName))
                {
                    //Debug.Send ("no " + fileName) ;
                    return false;
                }
                //Debug.Send (fileName + " exist") ;
                XmlDocument doc = new XmlDocument();
                FileStream fileStream = new FileStream(fileName, FileMode.Open);
                doc.Load(fileStream);

#if NETSTANDARD1_6
                XmlNode config = null;
                // Check if the TraceTool node is root or under configuration node
                foreach (XmlNode node in doc.ChildNodes)
                {
                   string tagName = node.Name.ToLower();
                   if (string.Compare(tagName, "tracetool", StringComparison.Ordinal) == 0)
                   {
                       config = node;
                       break;
                   }
                   if (string.Compare(tagName, "configuration", StringComparison.Ordinal) == 0)
                   {
                      foreach (XmlNode subNode in node)
                      {
                         string tagName2 = subNode.Name.ToLower();
                         if (string.Compare(tagName2, "tracetool", StringComparison.Ordinal) == 0)
                         {
                             config = subNode;
                             break;
                         }
                      }
                      break;
                   }
                }
#else // SelectSingleNode :  all dot net framework 
                XmlNode config = doc.SelectSingleNode("//TraceTool");
#endif
                if (config == null)
                {
                    //Debug.Send (fileName + " file exist but with NO TraceTool section") ;
                    return false;
                }

                //Debug.Send (fileName + " file exist with TraceTool section") ;
                return ReadConfig(config);
            }
            catch (Exception ex)
            {
                _lastError = ex.Message;
                return true;   // don't continue if error (else we can miss the error)
            }
        }

        //------------------------------------------------------------------------------
        // read configuration from XML node
        private static bool ReadConfig(XmlNode config)
        {
            if (config == null)
                return false;

            try
            {
                // Since XML is case sensitive and is then source of error, it's better to detect keyword
                // without case sensitive.
                foreach (XmlNode node in config.ChildNodes)
                {
                    // detect 'param' tag not case sensitive
                    string tagName = node.Name.ToLower();
                    // ReSharper disable StringCompareToIsCultureSpecific
                    if (tagName.CompareTo("param") == 0)
                    {
                        // get the name and value attributes
                        string paramName;
                        string paramValue;
                        GetParamNameAndValue(node, out paramName, out paramValue);
                        InitOptions(paramName, paramValue);
                    }
                    else if (tagName.CompareTo("debug") == 0)
                        InitTraceNode(node, Debug);
                    else if (tagName.CompareTo("warning") == 0)
                        InitTraceNode(node, Warning);
                    else if (tagName.CompareTo("error") == 0)
                        InitTraceNode(node, Error);
                    // ReSharper restore StringCompareToIsCultureSpecific
                }  // next tag in config section
            }
            catch (Exception ex)
            {
                _lastError = ex.Message;
                return false;
            }
            return true;
        }

        //------------------------------------------------------------------------------

        private static void GetParamNameAndValue(XmlNode node, out string paramName, out string paramValue)
        {
            paramName = "";
            paramValue = "";
            if (node.Attributes == null)
                return;
            foreach (XmlAttribute attrib in node.Attributes)
            {
                string attribName = attrib.Name.ToLower();
                // ReSharper disable StringCompareToIsCultureSpecific
                if (attribName.CompareTo("name") == 0)
                    paramName = attrib.Value.ToLower();
                if (attribName.CompareTo("value") == 0)
                    paramValue = attrib.Value.ToLower();
                // ReSharper restore StringCompareToIsCultureSpecific
            }
            //Debug.Send (ParamName,ParamValue) ;
        }

        //------------------------------------------------------------------------------

        internal static void InitTraceNode(XmlNode xmlTraceNode, TraceToSend trace)
        {
            foreach (XmlNode node in xmlTraceNode.ChildNodes)
            {
                // detect 'param' tag wihout case sensitive
                string tagName = node.Name.ToLower();
                // ReSharper disable StringCompareToIsCultureSpecific
                if (tagName.CompareTo("param") == 0)
                {
                    // get the name and value attributes
                    string paramName;
                    string paramValue;
                    GetParamNameAndValue(node, out paramName, out paramValue);

                    if (paramName.CompareTo("enabled") == 0)
                    {                // bool Enabled
                        if (paramValue.ToLower().CompareTo("true") == 0)
                            trace.Enabled = true;
                        else if (paramValue.ToLower().CompareTo("false") == 0)
                            trace.Enabled = false;
                    }
                    else if (paramName.CompareTo("iconindex") == 0)
                    {       // int IconIndex
                        try
                        {
                            trace.IconIndex = Int32.Parse(paramValue);
                        }
                        catch
                        {
                            // no change
                        }
                    }     // param name="xxx"
                          // ReSharper restore StringCompareToIsCultureSpecific
                }        // param tag
            }           // next param
        }

        //------------------------------------------------------------------------------

        internal static void InitOptions(string paramName, string paramValue)
        {
            // ReSharper disable StringCompareToIsCultureSpecific
            if (paramName.CompareTo("sendmode") == 0)
            {                // SendMode
                if (paramValue.ToLower().CompareTo("socket") == 0)
                    Options.SendMode = SendMode.Socket;
                else if (paramValue.ToLower().CompareTo("websocket") == 0)
                    Options.SendMode = SendMode.WebSocket;
                else if (paramValue.ToLower().CompareTo("none") == 0)
                    Options.SendMode = SendMode.None;
                // else keep 
            }
            else if (paramName.CompareTo("useworkerthread") == 0)
            {       // bool UseWorkerThread
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.UseWorkerThread = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.UseWorkerThread = false;
            }
            else if (paramName.CompareTo("sockethost") == 0)
            {       // string SocketHost
                Options.SocketHost = paramValue;
            }
            else if (paramName.CompareTo("socketport") == 0)
            {       // int SocketPort
                try
                {
                    Options.SocketPort = Int32.Parse(paramValue);
                }
                catch
                {
                    Options.SocketPort = 8090;
                }
            }
            else if (paramName.CompareTo("objecttreedepth") == 0)
            {       // int ObjectTreeDepth
                try
                {
                    Options.ObjectTreeDepth = Int32.Parse(paramValue);
                }
                catch
                {
                    Options.ObjectTreeDepth = 3;
                }
            }
            else if (paramName.CompareTo("sendmodifiers") == 0)           // 1
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendModifiers = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendModifiers = false;
            }
            else if (paramName.CompareTo("sendclassinfo") == 0)            // 2
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendClassInfo = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendClassInfo = false;
            }
            else if (paramName.CompareTo("sendfields") == 0)               // 4
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendFields = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendFields = false;
            }
            else if (paramName.CompareTo("sendcustomattributes") == 0)     // 8
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendCustomAttributes = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendCustomAttributes = false;
            }
            else if (paramName.CompareTo("sendnonpublic") == 0)            // 16
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendNonPublic = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendNonPublic = false;
            }
            else if (paramName.CompareTo("sendinherited") == 0)            // 32
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendInherited = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendInherited = false;
            }
            else if (paramName.CompareTo("sendevents") == 0)               // 64
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendEvents = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendEvents = false;
            }
            else if (paramName.CompareTo("sendfunctions") == 0)           // 128
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendFunctions = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendFunctions = false;
            }
            else if (paramName.CompareTo("senddoc") == 0)                  // 256
            {
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendDoc = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendDoc = false;
            }
            else if (paramName.CompareTo("sendprivate") == 0)
            {      // bool SendPrivate
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendPrivate = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendPrivate = false;
            }
            else if (paramName.CompareTo("sendprocessname") == 0)
            {    // bool SendProcessName
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendProcessName = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendProcessName = false;
            }
            else if (paramName.CompareTo("senddate") == 0)
            {           // bool SendDate
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendDate = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendDate = false;
            }
            else if (paramName.CompareTo("sendthreadid") == 0)
            {           // bool SendThreadId
                if (paramValue.ToLower().CompareTo("true") == 0)
                    Options.SendThreadId = true;
                else if (paramValue.ToLower().CompareTo("false") == 0)
                    Options.SendThreadId = false;
            }
            // ReSharper restore StringCompareToIsCultureSpecific
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// return initialisation error
        /// </summary>
        public static string InitError()
        {
            return _lastError;
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Stop sub-system (worker thread) before leaving your program. You may call TTrace.Flush() before Stop()
        /// </summary>
        public static void Stop()
        {
            StopEvent.Set();
            DataReady.Set();
#if !NETSTANDARD1_6 && !NETSTANDARD2_0  // Dot net core don't support Thread.Abort
            _traceThread.Abort();
#endif
            _traceThread = null;
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Show or hide the trace program
        /// </summary>
        /// <param name="isVisible">When True : Show. When False : Hide</param>
        public static void Show(bool isVisible)
        {
            List<string> commandList = new List<string>();
            Helper.AddCommand(commandList, TraceConst.CST_SHOW, isVisible);
            SendToViewer(commandList);
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Set the global search criteria. You must call TTrace.Wintrace.FindNext to position to the next or previous matching node
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <param name="sensitive">Search is case sensitive</param>
        /// <param name="wholeWord">match only whole word</param>
        /// <param name="highlight">Highlight results</param>
        /// <param name="searchInAllPages">call to FindNext will search also in other traces windows if true</param>
        public static void Find(string text, bool sensitive, bool wholeWord, bool highlight, bool searchInAllPages)
        {
            List<string> commandList = new List<string>();
            int flags = 0;
            // Sensitive<<3+WholeWord<<2+highlight<<1+SearchInAllPages
            if (sensitive)
                flags += 8;
            if (wholeWord)
                flags += 4;
            if (highlight)
                flags += 2;
            if (searchInAllPages)
                flags += 1;

            Helper.AddCommand(commandList, TraceConst.CST_FIND_TEXT, flags, text);
            SendToViewer(commandList);
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Clear all traces
        /// </summary>
        public static void ClearAll()
        {
            WinTrace.ClearAll();
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Close the viewer
        /// </summary>
        public static void CloseViewer()
        {
            List<string> commandList = new List<string>();

            Helper.AddCommand(commandList, TraceConst.CST_CLOSE_VIEWER);
            SendToViewer(commandList);
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Close the socket
        /// Tips : Socket is automatically closed when destroyed. No need to create destructor
        /// </summary>
        public static void CloseSocket()
        {
            if (Options.SendMode == SendMode.Socket && _socket != null)
            {
#if !NETSTANDARD1_6
                _socket.Close();
#endif
                _socket = null;
            }

#if !NETSTANDARD1_6
            if (Options.SendMode == SendMode.WebSocket && webSocketClient != null && webSocketClient.State == WebSocketState.Open)
            {
                var timeout = new CancellationTokenSource(10000); // MS, 10 sec

                // Bad ?
                webSocketClient.CloseAsync(WebSocketCloseStatus.NormalClosure, "", timeout.Token).Wait();
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} close async done");

                // Good ? 
                //socket.CloseOutputAsync(WebSocketCloseStatus.NormalClosure, "", timeout.Token).Wait();

                webSocketClient = null; // auto close connection
                //cancellationTocket ?

            }
#endif
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// The last socket error
        /// </summary>
        public static string LastSocketError
        {
            get { return _lastError; }
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// The windows where is stored the main tree (read only)
        /// </summary>
        public static WinTrace WinTrace
        {
            get
            {
                if (_winTrace == null)
                {
                    _winTrace = new WinTrace();
                }
                return _winTrace;
            }
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// The main watches window
        /// </summary>
        public static WinWatch Watches
        {
            get
            {
                if (_winWatch == null)
                {
                    _winWatch = new WinWatch();   // internal
                }
                return _winWatch;
            }
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Shortcut to WinTrace.Warning
        /// </summary>
        public static TraceToSend Warning
        {
            get { return WinTrace.Warning; }
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Shortcut to WinTrace.Error
        /// </summary>
        public static TraceToSend Error
        {
            get { return WinTrace.Error; }
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// Shortcut to WinTrace.Debug
        /// </summary>
        public static TraceToSend Debug
        {
            get { return WinTrace.Debug; }
        }

        //------------------------------------------------------------------------------

        /// <summary>
        /// TextWriter output. For Linq to SQL for example : NORTHWNDDataContext.Log = TTrace.Out 
        /// </summary>

        public static TextWriter Out
        {
            get
            {
                if (_writterOut == null)
                    _writterOut = new TTraceWriter();
                return _writterOut;
            }
        }

        //------------------------------------------------------------------------------
        // send the ArrayList to the viewer (using thread)

        internal static void SendToWinTraceClient(List<string> commandList, string winTraceId)
        {
            SendToWinTraceClient(commandList, winTraceId, FormatDate(DateTime.Now), Helper.GetCurrentThreadId()); // DateTime.Now.ToString("HH:mm:ss:fff")
        }

        //------------------------------------------------------------------------------
        // send the trace ArrayList to the viewer (using thread)

        internal static void SendToWinTraceClient(List<string> commandList, string winTraceId, string dateTime, string threadName)
        {
            // insert thread id
            if (Options.SendThreadId)
            {
                if (threadName == null)
                    commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_THREAD_NAME, Helper.GetCurrentThreadId()));
                else
                    commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_THREAD_NAME, threadName));
            }

            // add current time. TTrace.FormatDate is faster than  DateTime.ToString("HH:mm:ss:fff")
            if (dateTime == null)
                commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_MESSAGE_TIME, FormatDate(DateTime.Now))); // DateTime.Now.ToString("HH:mm:ss:fff") ));
            else
                commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_MESSAGE_TIME, dateTime));

            // add process name
            if (Options.SendProcessName)
            {
                commandList.Insert(0, String.Format("{0,5}{1}",
                   TraceConst.CST_PROCESS_NAME,
                   Helper.GetCurrentProcessName()));
            }

            // CST_USE_TREE MUST be inserted at the first position
            if (!string.IsNullOrEmpty(winTraceId))
                commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_USE_TREE, winTraceId));

            SendToViewer(commandList);
        }

        //------------------------------------------------------------------------------

        // send the watch ArrayList to the viewer (using thread)
        internal static void SendToWinWatchClient(List<string> commandList, string winWatchId)
        {
            // insert thread id
            if (Options.SendThreadId)
                commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_THREAD_NAME, Helper.GetCurrentThreadId()));

            // add current time. TTrace.FormatDate is faster than  DateTime.ToString("HH:mm:ss:fff")
            commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_MESSAGE_TIME, FormatDate(DateTime.Now))); // FormatDate will add date if necessary

            // add process name
            if (Options.SendProcessName)
            {
                commandList.Insert(0, String.Format("{0,5}{1}",
                   TraceConst.CST_PROCESS_NAME,
                   Helper.GetCurrentProcessName()));
            }

            // CST_USE_TREE MUST be inserted at the first position
            //if (winWatch != null && winWatch.Id != null && winWatch.Id != "")
            commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_WINWATCH_ID, winWatchId));

            SendToViewer(commandList);
        }

        //------------------------------------------------------------------------------

        // send the ArrayList to the viewer (using thread or async)
        internal static void SendToViewer(List<string> commandList)
        {
            if (Options.UseWorkerThread)
            {
                lock (DataReady)    // don't lock the MsgQue, because we can swap with workQueue
                {

                    if (_traceThread == null)
                    {
                        _traceThread = new Thread(SendToViewerThread);

                        // force the thread to be killed when all foreground thread are terminated.
                        _traceThread.IsBackground = true;
                        _traceThread.Start();
                    }
                    _msgQueue.Add(commandList);
                }
                // signal that data are ready to be send (SendToViewerThread)
                DataReady.Set();
            }
            else
            {
                // no worker thread. send to viewer async
                _ = SendToViewerAsync(commandList);
            }
        }

        //------------------------------------------------------------------------------

        // the thread function that send messages to the server
        private static void SendToViewerThread() // Object obj
        {
            List<List<string>> workQueue = new List<List<string>>();
            while (true)
            {
                //WaitHandle[] handles = new WaitHandle[2];
                //handles[0] = StopEvent;
                //handles[1] = DataReady;
                //if( WaitHandle.WaitAny(handles) == 0 )
                //   break;

                if (StopEvent.WaitOne(0))   // remaining messages are lost 
                    break;
                DataReady.WaitOne(1000);

                // lock the message queue and swap _msgQueue with the empty local queue (workQueue)
                lock (DataReady)
                {
                    //swap = swap + "Begin at " + DateTime.Now.ToString("HH:mm:ss:fff") + " : " + MsgQueue.Count ;

                    var tempList = workQueue; // used for swap queue
                    workQueue = _msgQueue;    // MsgQueue is the list of message to send
                    _msgQueue = tempList;
                }

                if (_isSocketError && Options.SendMode == SendMode.Socket)
                {
                    long actTime = DateTime.Now.Ticks;
                    if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                        _isSocketError = false;
                }

                // loop the outbound messages...
                foreach (List<string> commandList in workQueue)
                {
                    if (StopEvent.WaitOne(0))
                        break;

                    // special case : the CST_FLUSH message is handled by the sender thread, not to be send
                    if (commandList.Count > 0) // only one message
                    {
                        string msg = commandList[0];
                        if (int.Parse(msg.Substring(0, 5)) == TraceConst.CST_FLUSH)
                        {
                            var key = msg.Substring(5, msg.Length - 5);
                            //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} flush {key} received in worker thread");
                            try
                            {
                                var flushEvent = (TaskCompletionSource<string>)FlushList[key];
                                flushEvent.TrySetResult(key);
                            }
                            catch
                            {
                                // ignored
                            }
                            continue;
                        }
                    }

                    // check if the message contains information for local log
                    ParseForInternal(commandList);

                    // If socket mode and socket error : don't send messages.
                    // When the queue is empty, reset the error mode.
                    // The ParseForInternal will be called to save to file and responds to flush events.
                    if (_isSocketError && Options.SendMode == SendMode.Socket)
                    {
                        NbDiscarded++;
                        continue;
                    }

                    if (Options.SendMode != SendMode.None)
                    {
                        // normal command list
                        //tot = 0;
                        StringBuilder sb = new StringBuilder();
                        foreach (string msg in commandList)
                        {
                            //tot += msg.Length + 1;
                            sb.Append(msg).Append('\0');
                        }
                        sb.Append('\0');
                        // For previous version of tracetool,it's important to understand that strings was send in ASCII (1 byte per char),
                        // because it's the common denominator for all languages targetting tracetool.
                        // the tracetool dot net API is now unicode by default. 
                        sb.Insert(0, char.ToString((char)0xFEFF)); // start with the UTF-16, little endian byte order mark

                        //MessageString = sb.ToString();
                        //tot++;

                        try
                        {
#if NETSTANDARD1_6  // only socket
                            if (Options.SendMode == SendMode.Socket)
                                SendToSocketSync(sb);
#else

                            // Sync mode (thread)

                            if (Options.SendMode == SendMode.WinMsg)
                            {
                                SendToWindowsSync(sb);
                            }
                            else if (Options.SendMode == SendMode.Socket)
                            {
                                SendToSocketSync(sb);
                            }
                            else if (Options.SendMode == SendMode.WebSocket)
                            {
                                SendToWebSocketSync(sb);
                            }  // else no transport
                            // else no transport
#endif
                        }
                        catch (Exception ex)
                        {
                            _lastError = ex.Message;         // for debug purpose
                        }
                    }   // sendMode <> none
                }      // loop workQueue
                workQueue.Clear();
            }   // infinite loop 

            if (Options.SendMode == SendMode.Socket)
                _socket = null; // auto close connection

            // ReSharper disable once FunctionNeverReturns
        }  // thread function

        //------------------------------------------------------------------------------
        // the async function that send messages to the server

        private static async System.Threading.Tasks.Task SendToViewerAsync(List<string> newCommandList)
        {
            lock (DataReady)    // don't lock the MsgQue, because we can swap with workQueue
            {
                _msgQueue.Add(newCommandList);
                if (isAsyncRunning)
                {
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} already in SendToViewerAsync. Message added to Queue {_msgQueue.Count}");                  
                    return; // already waiting connection or sending message
                }
                else
                {
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} entering SendToViewerAsync");
                    isAsyncRunning = true;
                }
            }

            List<List<string>> workQueue = new List<List<string>>();
            while (true)
            {
                if (StopEvent.WaitOne(0))   // remaining messages are lost 
                    break;

                // lock the message queue and swap _msgQueue with the empty local queue (workQueue)
                lock (DataReady)
                {
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToViewerAsync : Sending {_msgQueue.Count} message");

                    var tempList = workQueue; // used for swap queue
                    workQueue = _msgQueue;    // MsgQueue is the list of message to send
                    _msgQueue = tempList;
                }

                if (_isSocketError && Options.SendMode == SendMode.Socket)
                {
                    long actTime = DateTime.Now.Ticks;
                    if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                        _isSocketError = false;
                }

                // loop the outbound messages...
                foreach (List<string> commandList in workQueue)
                {
                    if (StopEvent.WaitOne(0))
                        break;

                    // special case : the CST_FLUSH message is handled by the sender thread, not to be send
                    if (commandList.Count > 0) // only one message
                    {
                        string msg = commandList[0];
                        if (int.Parse(msg.Substring(0, 5)) == TraceConst.CST_FLUSH)
                        {
                            string key = msg.Substring(5, msg.Length - 5);
                            try
                            {
                                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} flush {key} received in async");
                                var flushEvent = (TaskCompletionSource<string>)FlushList[key];
                                flushEvent.TrySetResult(key);
                            }
                            catch
                            {
                                // ignored
                            }
                            continue;
                        }
                    }

                    // check if the message contains information for local log
                    ParseForInternal(commandList);

                    // If socket mode and socket error : don't send messages.
                    // When the queue is empty, reset the error mode.
                    // The ParseForInternal will be called to save to file and responds to flush events.
                    if (_isSocketError && Options.SendMode == SendMode.Socket)
                    {
                        NbDiscarded++;
                        continue;
                    }

                    if (Options.SendMode != SendMode.None)
                    {
                        // normal command list
                        //tot = 0;
                        StringBuilder sb = new StringBuilder();
                        foreach (string msg in commandList)
                        {
                            //tot += msg.Length + 1;
                            sb.Append(msg).Append('\0');
                        }
                        sb.Append('\0');
                        // For previous version of tracetool,it's important to understand that strings was send in ASCII (1 byte per char),
                        // because it's the common denominator for all languages targetting tracetool.
                        // the tracetool dot net API is now unicode by default. 

                        sb.Insert(0, char.ToString((char)0xFEFF)); // start with the UTF-16, little endian byte order mark

                        //MessageString = sb.ToString();
                        //tot++;

                        try
                        {
                            // Async mode 
#if !NETSTANDARD1_6 

                            if (Options.SendMode == SendMode.WinMsg)
                            {
                                SendToWindowsSync(sb);   // no need for Async 
                            }
                            else if (Options.SendMode == SendMode.WebSocket)
                            {
                                await SendToWebSocketAsync(sb);
                                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync done");                  
                            }
                            else 
#endif
                            if (Options.SendMode == SendMode.Socket)
                            {
                                await SendToSocketAsync(sb);
                                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToSocketAsync done");                  
                            }
                            // else no transport
                            // else no transport
                        }
                        catch (Exception ex)
                        {
                            _lastError = ex.Message;         // for debug purpose
                            //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToViewerAsync exception {ex.Message}");
                        }
                    }   // sendMode <> none
                }      // loop workQueue
                workQueue.Clear();

                if (_msgQueue.Count == 0)
                {
                    lock (DataReady)   // not needed to use lock. Blazor Single thread
                    {
                        //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} workQueue message are sent and _msgQueue empty, leaving SendToViewerAsync");
                        isAsyncRunning = false;
                    }
                    return;
                }
                else
                {
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} some messages waiting on main queue. continue the SendToViewerAsync loop");                    
                }
            }   // infinite loop 

            if (Options.SendMode == SendMode.Socket)
                _socket = null; // auto close connection

            // Also done in CloseSocket
#if !NETSTANDARD1_6
            if (Options.SendMode == SendMode.WebSocket && webSocketClient != null)
            {
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} Closing websocket");
                var timeout = new CancellationTokenSource(10000); // MS, 10 sec
                                                                  // Bad ?
                webSocketClient.CloseAsync(WebSocketCloseStatus.NormalClosure, "", timeout.Token).Wait();

                // Good ? 
                //socket.CloseOutputAsync(WebSocketCloseStatus.NormalClosure, "", timeout.Token).Wait();

                webSocketClient = null; // auto close connection
                //cancellationTocket ?
            }
#endif
        }  // async task function

        //------------------------------------------------------------------------------
#if !NETSTANDARD1_6
        internal static void SendToWebSocketSync(StringBuilder message)
        {
            if (_isSocketError)
            {
                long actTime = DateTime.Now.Ticks;

                if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                    _isSocketError = false;
                else
                    return;  // lose message
            }

            // allocate and fill the buffToSend array
            Prepare_buffToSend(message);

            if (string.IsNullOrEmpty(Options.SocketHost))
                Options.SocketHost = "127.0.0.1";

            if (Options.SocketPort == 0)
                Options.SocketPort = 8091;

            if (webSocketClient == null)
                webSocketClient = new ClientWebSocket();

            try
            {
                if (webSocketClient.State != WebSocketState.Open)
                {
                    // Connect Async but WAIT
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketSync: connect and wait");
                    string url = "ws://" + Options.SocketHost + ":" + Options.SocketPort.ToString();  // "ws://localhost:8091
                    webSocketClient.ConnectAsync(new Uri(url), cancellationTocket.Token)
                       .Wait();
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketSync : connected");
                }
            }
            catch (Exception ex)
            {
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketSync : connect and wait exception : {ex.Message}");
                webSocketClient = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
                return;
            }

            try
            {
                // send async but WAIT
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketSync : send and wait");
                var sendBuffer = new ArraySegment<byte>(_buffToSend);

                webSocketClient.SendAsync(sendBuffer, WebSocketMessageType.Binary, endOfMessage: true, cancellationToken: cancellationTocket.Token)
                  .Wait();
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync : is send");
            }
            catch (Exception ex)
            {
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketSync : send async exception : {ex.Message}");
                webSocketClient = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
            }
        }

        //------------------------------------------------------------------------------

        internal static async System.Threading.Tasks.Task SendToWebSocketAsync(StringBuilder message)
        {
            if (_isSocketError)
            {
                long actTime = DateTime.Now.Ticks;

                if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                    _isSocketError = false;
                else
                    return;  // lose message
            }

            // allocate and fill the buffToSend array
            Prepare_buffToSend(message);

            if (string.IsNullOrEmpty(Options.SocketHost))
                Options.SocketHost = "127.0.0.1";

            if (Options.SocketPort == 0)
                Options.SocketPort = 8091;

            if (webSocketClient == null)
                webSocketClient = new ClientWebSocket();

            try
            {
                if (webSocketClient.State != WebSocketState.Open)
                {
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync: connect and wait");
                    string url = "ws://" + Options.SocketHost + ":" + Options.SocketPort.ToString();  // "ws://localhost:8091
                    await webSocketClient.ConnectAsync(new Uri(url), cancellationTocket.Token);
                    //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync : connected");
                }
            }
            catch (Exception ex)
            {
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocket : connect and wait exception : {ex.Message}");
                webSocketClient = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
                return;
            }

            try
            {
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync : send and wait");
                var sendBuffer = new ArraySegment<byte>(_buffToSend);

                await webSocketClient.SendAsync(sendBuffer, WebSocketMessageType.Binary, endOfMessage: true, cancellationToken: cancellationTocket.Token);
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync : is send");
            }
            catch (Exception ex)
            {
                //Console.WriteLine($"{DateTime.Now.ToString("hh:mm:ss.fff")} SendToWebSocketAsync : send async exception : {ex.Message}");
                webSocketClient = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
            }
        }
#endif

        //------------------------------------------------------------------------------
        /// flush remaining traces to the viewer

        public static async Task FlushAsync()
        {
            string key = Helper.NewGuid().ToString();
            List<string> commandList = new List<string>();

            var flushEvent = new TaskCompletionSource<string>();
            FlushList.Add(key, flushEvent);
            commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_FLUSH, key));

            await SendToViewerAsync(commandList);
            await flushEvent.Task;

            FlushList.Remove(key);
        }

        //------------------------------------------------------------------------------

        /// flush remaining traces to the viewer
        public static void Flush()
        {
            if (!Options.UseWorkerThread)
                throw new Exception("Dont' call Flush() in Async mode. use FlushAsync()");


            string key = Helper.NewGuid().ToString();
            List<string> commandList = new List<string>();

            var flushEvent = new TaskCompletionSource<string>();
            FlushList.Add(key, flushEvent);

            commandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_FLUSH, key));

            lock (DataReady)    // don't lock the MsgQue, because we can swap with workQueue
            {
                _msgQueue.Add(commandList);
            }

            // signal that data are ready to be send
            DataReady.Set();

            var key2 = flushEvent.Task.Result;   // wait sync
            FlushList.Remove(key2);
        }

#if !NETSTANDARD1_6

        internal static IntPtr VarPtr(object e)
        {
            GCHandle gC = GCHandle.Alloc(e, GCHandleType.Pinned);
            IntPtr gc = gC.AddrOfPinnedObject();
            gC.Free();
            return gc;
        }

        internal static void SendToWindowsSync(StringBuilder message)
        {
            int debugWin = StartTDebug();
            if (debugWin != 0)
            {
                COPYDATASTRUCT cds = new COPYDATASTRUCT();  // StructLayout.CharSet is CharSet.Ansi

                cds.dwData = (IntPtr)TraceConst.WMD;
                cds.cbData = message.Length * 2;    // unicode are 2 bytes
                cds.lpData = VarPtr(message.ToString());     // automatically convert string to unicode (see StructLayout attribute in COPYDATASTRUCT)
                Helper.SendMessage(debugWin, TraceConst.WM_COPYDATA, IntPtr.Zero, VarPtr(cds));
            }
        }
#endif

        //------------------------------------------------------------------------------
        // Prepare the byte[] _buffToSend
        internal static void Prepare_buffToSend(StringBuilder message)
        {
            // For previous version of tracetool,it's important to understand that strings was send in ASCII (1 byte per char),
            // because it's the common denominator for all languages targetting tracetool.
            // the tracetool dot net API is now unicode by default. 

            int intMsgLen = message.Length * 2; // number of bytes in the message : message len * 2 (unicode)

            int c = 0;
            if (c == 0)  // force new version
            {
                // new version : 
                // write the init byte (WMD = 123d) then message lenght (4 bytes) as a DWORD then the message

                _buffToSend = new byte[5 + intMsgLen]; // create the buffer : WMD byte + message len as a DWORD + message
                                                       // write the WMD byte into the buffer
                _buffToSend[0] = TraceConst.WMD;       // the WMD byte ensure the message is valid.
                                                       // Append the intMsgLen into the buffer
                byte[] byteArray = BitConverter.GetBytes(intMsgLen);
                for (c = 0; c <= 3; c++)
                    _buffToSend[c + 1] = byteArray[c];  // start at 1
                c = 5;  // jump over WMD byte and DWORD
            }
            else
            {
                // old version :
                // insert the lenght followed by the null terminator and the message.
                // for compatibility issue, the length is coded as a AnsiString (single byte)

                string strMsgLen = intMsgLen.ToString();
                _buffToSend = new byte[strMsgLen.Length + 1 + intMsgLen]; // create the buffer : message len as an ASCII string + '\0' + message

                for (c = 0; c < strMsgLen.Length; c++)
                {
                    char charNum = strMsgLen[c];
                    byte byteNum;
                    switch (charNum)
                    {
                        case '0': byteNum = 48; break;
                        case '1': byteNum = 49; break;
                        case '2': byteNum = 50; break;
                        case '3': byteNum = 51; break;
                        case '4': byteNum = 52; break;
                        case '5': byteNum = 53; break;
                        case '6': byteNum = 54; break;
                        case '7': byteNum = 55; break;
                        case '8': byteNum = 56; break;
                        case '9': byteNum = 57; break;
                        default: byteNum = 32; break;// space
                    }
                    _buffToSend[c] = byteNum;
                }
                c = strMsgLen.Length;
                _buffToSend[c] = 0;  // add null term
                c++;
            }

            char[] chars = message.ToString().ToCharArray();
            Encoding.Unicode.GetBytes(
               chars,          // source to encode
               0,              // The index of the first character to encode
               chars.Length,   // The number of characters to encode
               _buffToSend,     // The byte array to contain the resulting sequence of bytes
               c);             // The index at which to start writing the resulting sequence of bytes

            //buffToSend = System.Text.Encoding.Unicode.GetBytes(Message.ToString()); // older version : ASCII encoding
        }

        //------------------------------------------------------------------------------

        internal static async System.Threading.Tasks.Task SendToSocketAsync(StringBuilder message)
        {
            if (_isSocketError)
            {
                long actTime = DateTime.Now.Ticks;

                if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                    _isSocketError = false;
                else
                    return;  // lose message
            }

            // allocate and fill the Prepare the byte[] _buffToSend array
            Prepare_buffToSend(message);

            if (string.IsNullOrEmpty(Options.SocketHost))
                Options.SocketHost = "127.0.0.1";

            if (Options.SocketPort == 0)
                Options.SocketPort = 8090;

#if !NETSTANDARD1_6
            if (Options.SocketUdp)
            {
                if (_udpSocket == null)
                    _udpSocket = new UdpClient(Options.SocketHost, Options.SocketPort);
                try
                {
                    _udpSocket.Send(_buffToSend, _buffToSend.Length);
                }
                catch (Exception ex)
                {
                    // With Udp, the only exception that can be throw isd the following :
                    //    "A message sent on a datagram socket was larger than the internal message 
                    //    buffer or some other network limit, or the buffer used to receive a datagram 
                    //    into was smaller than the datagram itself"
                    // This exception don't have to be considered as an error

                    //_isSocketError = true;
                    //_errorTime = DateTime.Now.Ticks;
                    _lastError = ex.Message;         // for debug purpose
                }
                return;
            }
#endif
            // the socket must be connected in the sender thread !!! not here..  to do
            if (_socket == null)
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            // if the socket is disconnect since the last use, reopen it
            if (_socket.Connected == false)
            {
                // resolve adress

#if NETSTANDARD1_6
                Task<IPHostEntry> hostEntryTask = Dns.GetHostEntryAsync(Options.SocketHost);  
                Task.WaitAll(hostEntryTask) ;
                IPHostEntry hostEntry =  hostEntryTask.Result ;
#else // NETF45 or more or standard 2
                IPHostEntry hostEntry = Dns.GetHostEntry(Options.SocketHost);
#endif
                // Don't get the first adress. It's perhaps a IPv6 adress.
                // Thanks BCheng for the IPV4 fix.
                IPEndPoint endPoint = null;
                foreach (IPAddress ip in hostEntry.AddressList)
                {
                    if (ip.AddressFamily == AddressFamily.InterNetwork)
                    {
                        endPoint = new IPEndPoint(ip, Options.SocketPort);
                        break;
                    }
                }

                if (endPoint == null)
                    return;

                try
                {
                    await _socket.ConnectAsync(endPoint);
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
                //public int Send(byte[] buffer, int offset, int size, SocketFlags socketFlags);
                //_socket.Send(_buffToSend, 0, _buffToSend.Length, 0);

                //public static Task<int> SendAsync(this Socket socket, ArraySegment<byte> buffer, SocketFlags socketFlags);
                await _socket.SendAsync(new ArraySegment<byte>(_buffToSend), SocketFlags.None);
            }
            catch (Exception ex)
            {
                _socket = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
            }
        } // SendToSocketAsync function

        //------------------------------------------------------------------------------

        internal static void SendToSocketSync(StringBuilder message)
        {
            if (_isSocketError)
            {
                long actTime = DateTime.Now.Ticks;

                if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                    _isSocketError = false;
                else
                    return;  // lose message
            }

            // allocate and fill the Prepare the byte[] _buffToSend array
            Prepare_buffToSend(message);

            if (string.IsNullOrEmpty(Options.SocketHost))
                Options.SocketHost = "127.0.0.1";

            if (Options.SocketPort == 0)
                Options.SocketPort = 8090;

            // Synchrone communication

#if !NETSTANDARD1_6
            if (Options.SocketUdp)
            {
                if (_udpSocket == null)
                    _udpSocket = new UdpClient(Options.SocketHost, Options.SocketPort);
                try
                {
                    _udpSocket.Send(_buffToSend, _buffToSend.Length);
                }
                catch (Exception ex)
                {
                    // With Udp, the only exception that can be throw isd the following :
                    //    "A message sent on a datagram socket was larger than the internal message 
                    //    buffer or some other network limit, or the buffer used to receive a datagram 
                    //    into was smaller than the datagram itself"
                    // This exception don't have to be considered as an error

                    //_isSocketError = true;
                    //_errorTime = DateTime.Now.Ticks;
                    _lastError = ex.Message;         // for debug purpose
                }
                return;
            }
#endif

            // the socket must be connected in the sender thread !!! not here..  to do
            if (_socket == null)
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            // if the socket is disconnect since the last use, reopen it
            if (_socket.Connected == false)
            {
                // resolve adress

#if NETSTANDARD1_6
                Task<IPHostEntry> hostEntryTask = Dns.GetHostEntryAsync(Options.SocketHost);  
                Task.WaitAll(hostEntryTask) ;
                IPHostEntry hostEntry =  hostEntryTask.Result ;
#else // NETF2 or more or standard 2
                IPHostEntry hostEntry = Dns.GetHostEntry(Options.SocketHost);
#endif
                // Don't get the first adress. It's perhaps a IPv6 adress.
                // Thanks BCheng for the IPV4 fix.
                IPEndPoint endPoint = null;
                foreach (IPAddress ip in hostEntry.AddressList)
                {
                    if (ip.AddressFamily == AddressFamily.InterNetwork)
                    {
                        endPoint = new IPEndPoint(ip, Options.SocketPort);
                        break;
                    }
                }

                if (endPoint == null)
                    return;

                try
                {
                    _socket.Connect(endPoint);    // Sync
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
                _socket.Send(_buffToSend, 0, _buffToSend.Length, SocketFlags.None);  // Sync
            }
            catch (Exception ex)
            {
                _socket = null; // force recreate socket
                _isSocketError = true;
                _errorTime = DateTime.Now.Ticks;
                _lastError = ex.Message;         // for debug purpose
            }
        } // SendToSocketSync function

        //------------------------------------------------------------------------------

        internal static InternalWinTrace GetInternalTraceForm(string traceWinId, bool doCreate)
        {
            if (string.IsNullOrEmpty(traceWinId) || traceWinId == "_")
                return DefaultWinTrace;

            foreach (InternalWinTrace internalForm in FormTraceList)
            {
                if (internalForm.Id == traceWinId)
                    return internalForm;
            }

            // if the trace window don't exist, create it if needed
            InternalWinTrace result = null;
            if (doCreate)
            {
                result = new InternalWinTrace();
                result.LogFileName = "";
                result.LogFileType = 3;   // no log
                result.IsMultiColTree = false;
                FormTraceList.Add(result);
                result.Id = traceWinId;
            }
            return result;
        }

        //------------------------------------------------------------------------------

        internal static void ParseForInternal(List<string> commandList)
        {
            int command;
            int c;
            string commandParams;
            InternalWinTrace traceForm = DefaultWinTrace; // traces are send to the master trace form by default
                                                          // to be valid, CST_USE_TREE or CST_USE_MULTICOL_TREE or CST_WINWATCH_ID must be the first command
            if (commandList.Count > 0) // only one message
            {
                string msg = commandList[0];
                command = Int32.Parse(msg.Substring(0, 5));
                commandParams = msg.Substring(5, msg.Length - 5);

                if (command == TraceConst.CST_USE_TREE)
                    traceForm = GetInternalTraceForm(commandParams, false);
                //else if (command == TraceConst.CST_USE_MULTICOL_TREE)
                //   TraceForm = getInternalTraceForm(commandParams, false);
                else if (command == TraceConst.CST_WINWATCH_ID)
                    return;
            }

            // stop parsing if the winForm is not registered or the winForm don't need to be saved
            // 3, Local log is disabled
            // 4, Local log enabled. No size limit.
            // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
            if (traceForm == null || traceForm.LogFileType == 3)
                return;

            string leftMsg = "";      // Left col
            string rightMsg = "";     // right col
            string traceId = "";      // the reference of the node : it's a guid
            string threadId = "";     // thread id of the sender
            string processName = "";  // optional : the name of the process that send traces
            int treeIcon = -1;        // -1 by default, converted to 24
            string parentId = "";
            string messageTime = "";

            bool isNewNode = false;   // check if message is a new node
            StringBuilder memberXml = new StringBuilder();

            foreach (string msg in commandList)
            {
                command = Int32.Parse(msg.Substring(0, 5));
                commandParams = msg.Substring(5, msg.Length - 5);
                switch (command)
                {
                    case TraceConst.CST_WATCH_NAME: return;  // Bypass watches
                    case TraceConst.CST_MESSAGE_TIME: messageTime = commandParams; break;
                    case TraceConst.CST_PROCESS_NAME: processName = commandParams; break;
                    case TraceConst.CST_THREAD_ID: threadId = "0x" + Int32.Parse(commandParams).ToString("X2"); break;
                    case TraceConst.CST_THREAD_NAME: threadId = commandParams; break;
                    case TraceConst.CST_ICO_INDEX: treeIcon = Int32.Parse(commandParams); break;
                    case TraceConst.CST_TRACE_ID: traceId = commandParams; break;
                    case TraceConst.CST_LEFT_MSG: leftMsg = commandParams; break; // param : msg
                    case TraceConst.CST_RIGHT_MSG: rightMsg = commandParams; break;   // param : msg

                    case TraceConst.CST_NEW_NODE:
                        // param1 : Parent Node
                        parentId = commandParams;
                        isNewNode = true;
                        break;

                    case TraceConst.CST_CREATE_MEMBER:
                        memberXml.Append("<Member>");
                        Helper.HtmlEncode(commandParams, memberXml);
                        break;
                    case TraceConst.CST_MEMBER_COL2:
                        if (commandParams != "")
                        {
                            memberXml.Append("<ColB>");
                            Helper.HtmlEncode(commandParams, memberXml);
                            memberXml.Append("</ColB>");
                        }
                        break;
                    case TraceConst.CST_MEMBER_COL3:
                        if (commandParams != "")
                        {
                            memberXml.Append("<ColC>");
                            Helper.HtmlEncode(commandParams, memberXml);
                            memberXml.Append("</ColC>");
                        }
                        break;
                    case TraceConst.CST_MEMBER_VIEWER_KIND:
                        if (Int32.Parse(commandParams) != TraceConst.CST_VIEWER_NONE)
                            memberXml.Append("<ViewerKind>").Append(commandParams).Append("</ViewerKind>");
                        break;
                    case TraceConst.CST_ADD_MEMBER:
                        memberXml.Append("</Member>");
                        break;
                }  // switch
            }     // for each

            // if new node then save to log file
            if (isNewNode == false)
                return;

            StringBuilder xml = new StringBuilder();
            xml.Append("<Node");
            if (processName != "")
            {
                xml.Append(" Process=\"");
                Helper.HtmlEncode(processName, xml);
                xml.Append("\"");
            }
            if (messageTime != "")
            {
                xml.Append(" Time=\"");
                Helper.HtmlEncode(messageTime, xml);
                xml.Append("\"");
            }
            if (parentId != "")
            {  // add parent relation if not root
                xml.Append(" Parent=\"");
                Helper.HtmlEncode(parentId, xml);
                xml.Append("\"");
            }
            if (traceId != "")
            {
                xml.Append(" Id=\"");
                Helper.HtmlEncode(traceId, xml);
                xml.Append("\"");
            }
            if (threadId != "")
            {
                xml.Append(" ThId=\"");
                Helper.HtmlEncode(threadId, xml);
                xml.Append("\"");
            }
            // don't save default
            if (treeIcon != -1 && treeIcon != 24)
                xml.Append(" Icon=\"").Append(treeIcon).Append("\"");
            xml.Append(">");   // <Node ...>

            if (traceForm.IsMultiColTree)
            {
                //<ColValue Order="2">C3</ColValue>
                // ReSharper disable once RedundantExplicitParamsArrayCreation
                // ReSharper disable once RedundantExplicitArrayCreation
                string[] columns = leftMsg.Split(new char[] { '\t' });
                c = 0;
                foreach (string column in columns)
                {
                    xml.Append("<ColValue Order=\"").Append(c).Append("\">");
                    Helper.HtmlEncode(column, xml);
                    xml.Append("</ColValue>");
                    c++;
                }
            }
            else
            {
                // save the tree col1
                Helper.HtmlEncode(leftMsg, xml);
                // save the tree col 2
                if (rightMsg != "")
                {
                    xml.Append("<Col2>");
                    Helper.HtmlEncode(rightMsg, xml);
                    xml.Append("</Col2>");
                }
            }

            // append member to xml
            xml.Append(memberXml);

            xml.Append("</Node>");

            if (traceForm.LogFileName.Trim() == "")
                traceForm.LogFileName = "TraceLog.xml";

            //string FileToWrite = "";
            if (traceForm.LogFileType == 3)
            {            // 3, Local log disaled.
                         // should not happens. Detected before parsing
                return;
            }
            else if (traceForm.LogFileType == 4)
            {     // 4, Local log enabled. No size limit.
                traceForm.LastLocalLogFileName = traceForm.LogFileName;
                if (traceForm.CurrentFileNumber != 0)
                {
                    // Append CurrentFileNumber Before extension            
                    var pos = traceForm.LastLocalLogFileName.LastIndexOf('.');
                    if (pos == -1)// no extension
                        traceForm.LastLocalLogFileName = traceForm.LastLocalLogFileName + '_' + traceForm.CurrentFileNumber + ".xml"; //$NON-NLS-1$
                    else
                        traceForm.LastLocalLogFileName = traceForm.LastLocalLogFileName.Substring(0, pos - 1) + '_' + traceForm.CurrentFileNumber + traceForm.LastLocalLogFileName.Substring(pos);
                }
            }
            else
            {                                     // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
                string fileExt = Path.GetExtension(traceForm.LogFileName);  // include the dot
                StringBuilder strbBuilder = new StringBuilder();
                strbBuilder.Append(traceForm.LogFileName.Substring(0, traceForm.LogFileName.Length - fileExt.Length));
                // append YYYYMMDD
                DateTime now = DateTime.Now;
                var temp = now.Year;
                strbBuilder.Append(temp);
                temp = now.Month;
                if (temp < 10)
                    strbBuilder.Append('0');
                strbBuilder.Append(temp);
                temp = now.Day;
                if (temp < 10)
                    strbBuilder.Append('0');
                strbBuilder.Append(temp);
                // add CurrentFileNumber if <> 0
                if (traceForm.CurrentFileNumber != 0)
                    strbBuilder.Append('_').Append(traceForm.CurrentFileNumber);
                // append file extension (XML)
                strbBuilder.Append(fileExt);
                traceForm.LastLocalLogFileName = strbBuilder.ToString();
            }

            FileStream f;
            if (File.Exists(traceForm.LastLocalLogFileName) == false)
            {
                f = new FileStream(traceForm.LastLocalLogFileName, FileMode.Create);

                // include header in file
                if (traceForm.IsMultiColTree)
                {
                    StringBuilder strbBuilder = new StringBuilder();
                    strbBuilder.Append("<MainColumn>").Append(traceForm.MainCol).Append("</MainColumn>");
                    // ReSharper disable once RedundantExplicitParamsArrayCreation
                    // ReSharper disable once RedundantExplicitArrayCreation
                    string[] cols = traceForm.TitleList.Split(new char[] { '\t' });
                    c = 0;
                    foreach (string col in cols)
                    {
                        if (col != "")
                            strbBuilder.Append("<ColTitle Order=\"").Append(c).Append("\">").Append(col).Append("</ColTitle>");
                        c++;
                    }
                    xml.Insert(0, strbBuilder.ToString());
                }
                xml.Insert(0, "<Data>");
            }
            else
            {  // append only the node
                f = File.Open(traceForm.LastLocalLogFileName, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite);
                f.Seek(f.Length - 7, SeekOrigin.Begin); // override the </data> tag
            }
            xml.Append("\n</Data>");
            Byte[] info = new UTF8Encoding(true).GetBytes(xml.ToString());
            f.Write(info, 0, info.Length);
#if !NETSTANDARD1_6
            f.Close();
#endif

            // limit file size
            if (traceForm.MaxLines != -1)
            {
                traceForm.LinesWritten++;
                if (traceForm.LinesWritten >= traceForm.MaxLines)
                {
                    traceForm.CurrentFileNumber++;
                    traceForm.LinesWritten = 0;  // reset counter
                }
            }
        }

        //----------------------------------------------------------------------
        // Assembly private function

#if !NETSTANDARD1_6
        internal static int StartTDebug()
        {
            var winHandle = Helper.FindWindow("TFormReceiver", "FormReceiver");
            if (winHandle != 0)
            {
                //MessageBox.Show("StartTDebug : FindWindow : " + WinHandle.ToString());
                return winHandle;
            }

#if !NETSTANDARD2_0
            RegistryKey reg = Registry.LocalMachine.OpenSubKey("Software\\TraceTool");
            if (reg == null)
            {
               //MessageBox.Show("StartTDebug : reg == null");
               return 0;
            }

            String debugFilename = (string) reg.GetValue("FilePath");
            if (File.Exists(debugFilename) == false)
            {
               //MessageBox.Show("StartTDebug : DebugFilename unknow");
               return 0;
            }

            var myProcess = new Process();
            myProcess.StartInfo.FileName = debugFilename;
            myProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
            myProcess.Start();

            Thread.Sleep(2000);

            winHandle = Helper.FindWindow("TFormReceiver", "FormReceiver");
#endif
            return winHandle;
        }
#endif
        //----------------------------------------------------------------------

        // Formats a DateTime in the format "HH:mm:ss:SSS" for example, "15:49:37:459".
        // FormatDateWithoutMillis and FormatDate code are taken from Log4Net source code.

        /// Last stored time with precision up to the second.
        internal static long LastTimeToTheSecond;

        /// Last stored time with precision up to the second, formatted as a string.
        private static readonly StringBuilder LastTime = new StringBuilder();

        /// <summary>
        /// Renders the date into a string. Format is "HH:mm:ss". if Options.SendDate is true , date is added in front
        /// </summary>
        /// <remarks>
        /// This method will be called at most once per second and the result will be
        /// reused if it is needed again during the same second.
        /// </remarks>
        /// <param name="dateToFormat">The date to render into a string.</param>
        /// <param name="buffer">The string builder to write to.</param>
        internal static void FormatDateWithoutMillis(DateTime dateToFormat, StringBuilder buffer)
        {
            int temp;
            if (Options.SendDate)
            {
                temp = dateToFormat.Year;
                buffer.Append(temp);
                temp = dateToFormat.Month;
                if (temp < 10)
                    buffer.Append('0');
                buffer.Append(temp);
                temp = dateToFormat.Day;
                if (temp < 10)
                    buffer.Append('0');
                buffer.Append(temp);
            }

            temp = dateToFormat.Hour;
            if (temp < 10)
                buffer.Append('0');
            buffer.Append(temp);
            buffer.Append(':');

            temp = dateToFormat.Minute;
            if (temp < 10)
                buffer.Append('0');
            buffer.Append(temp);
            buffer.Append(':');

            temp = dateToFormat.Second;
            if (temp < 10)
                buffer.Append('0');
            buffer.Append(temp);
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Renders the date into a string. Format is "HH:mm:ss,SSS".
        /// </summary>
        /// <remarks>
        /// <para>Uses the FormatDateWithoutMillis() method to generate the
        /// time string up to the seconds and then appends the current
        /// milliseconds. The results from FormatDateWithoutMillis() are
        /// cached and FormatDateWithoutMillis() is called at most once
        /// per second.</para>
        /// <para>Sub classes should override FormatDateWithoutMillis()
        /// rather than FormatDate().</para>
        /// </remarks>
        /// <param name="dateToFormat">The date to render into a string.</param>
        /// <returns>The stringbuilder passed.</returns>
        internal static string FormatDate(DateTime dateToFormat)
        {
            StringBuilder buffer = new StringBuilder();
            // Calculate the current time precise only to the second
            long currentTimeToTheSecond = dateToFormat.Ticks - (dateToFormat.Ticks % TimeSpan.TicksPerSecond);

            // Compare this time with the stored last time
            if (LastTimeToTheSecond == currentTimeToTheSecond)
            {
                // If we are in the same second then append
                // the previously calculated time string
                buffer.Append(LastTime);
            }
            else
            {
                // We are in a new second.
                LastTimeToTheSecond = currentTimeToTheSecond;
                LastTime.Length = 0;

                // Calculate the new string for this second
                FormatDateWithoutMillis(dateToFormat, LastTime);
                buffer.Append(LastTime);
            }

            // Append the current milli info
            buffer.Append(':');
            int millis = dateToFormat.Millisecond;
            if (millis < 100)
            {
                buffer.Append('0');
            }
            if (millis < 10)
            {
                buffer.Append('0');
            }
            buffer.Append(millis);

            return buffer.ToString();
        }
    }     // TTrace

    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------

    internal class InternalWinTrace
    {
        public string Id;
        public bool IsMultiColTree;
        public int MainCol;
        public string TitleList;
        public string LogFileName;
        public string LastLocalLogFileName;  // last opened file
        public int LogFileType;
        public int MaxLines;// Max number of lines before starting a new file      
        public int CurrentFileNumber;// Current file number, when MaxLines is set       
        public int LinesWritten;// Number of lines written , when MaxLines is set

        public InternalWinTrace()
        {
            Id = "";
            IsMultiColTree = false;
            MainCol = 0;
            TitleList = "";
            LogFileName = "";
            LastLocalLogFileName = "";
            LogFileType = 3;
            MaxLines = -1;// Max number of lines before starting a new file       
            CurrentFileNumber = 0;// Current file number, when MaxLines is set       
            LinesWritten = 0;// Number of lines written , when MaxLines is set 
        }
    }

    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------

    /// <summary>
    /// Options for the traces.
    /// </summary>
    // ReSharper disable once InconsistentNaming
    public class TTraceOptions
    {
        /// <summary>
        /// WinMsg (for desktop applications only), Socket (ASP , services, or remote computer), WebSocket (webassembly), None (No messages are send, use local log)
        /// </summary>
        public SendMode SendMode =
#if !NETSTANDARD1_6 && !NETSTANDARD2_0
            SendMode.WinMsg ;   // default for windows framework : windows messages
#else
            SendMode.Socket;    // default for .Net standard : socket
#endif

        /// <summary>
        /// Use a background thread to send traces to the viewer. Default is true. 
        /// Set to false for async mode (web assembly)
        /// </summary>
        public bool UseWorkerThread = true;
        /// <summary>
        /// The Socket Host adress
        /// </summary>
        public string SocketHost;
        /// <summary>
        /// The socket port
        /// </summary>
        public int SocketPort;
        /// <summary>
        /// Indicate if the socket use the Udp protocol
        /// </summary>
        public bool SocketUdp = false;
        /// <summary>
        /// indicate if the reflection should display modifiers (public class,...)
        /// </summary>
        /// 
        public bool SendModifiers = true;            // ShowModifiers        = 1  ,
        /// <summary>
        /// indicate if the reflection should display class info (assembly,guid,...) and bases classes names
        /// </summary>
        public bool SendClassInfo;                    // ShowClassInfo        = 2  ,
        /// <summary>
        /// indicate if the reflection should display fields values
        /// </summary>
        public bool SendFields = true;               // ShowFields           = 4  ,
        /// <summary>
        /// indicate if the reflection should display custom attributes
        /// </summary>
        public bool SendCustomAttributes;            // ShowCustomAttributes = 8  ,
        /// <summary>
        /// indicate if the reflection should display non public (private and protected) members
        /// </summary>
        public bool SendNonPublic;                   // ShowNonPublic        = 16 ,
        /// <summary>
        /// indicate if the reflections should display inherited members
        /// </summary>
        public bool SendInherited = true;            // ShowInheritedMembers = 32 ,
        /// <summary>
        /// indicate if the reflections should display the events
        /// </summary>
        public bool SendEvents;                      // ShowEvents           = 64 ,
        /// <summary>
        /// indicate if the reflection should display functions
        /// </summary>
        public bool SendFunctions;                   //  ShowMethods         = 128,
        /// <summary>
        /// indicate if the reflection should display documentation for type, fields, methods,..
        /// </summary>
        public bool SendDoc;                         // ShowDoc              = 256

        /// <summary>
        /// indicate if the reflection should also display private members. Default is false
        /// </summary>
        public bool SendPrivate;
        /// <summary>
        /// Max Object tree depth for SendValue and Watches
        /// </summary>
        public int ObjectTreeDepth = 3;
        /// <summary>
        /// Indicate if SendValue and AddValue function should display members type. Default is true
        /// </summary>
        public bool SendTypeWithValue = true;

        /// <summary>
        /// indicate if the process name must be send. Displayed on the status bar.
        /// </summary>
        public bool SendProcessName;

        private bool _sendDate;
        /// <summary>
        /// indicate if the date must be send with the time.
        /// </summary>
        public bool SendDate
        {
            get { return _sendDate; }
            set
            {
                _sendDate = value;
                TTrace.LastTimeToTheSecond = 0;
            }
        }

        /// <summary>
        /// indicate if the thread id must be send.
        /// </summary>
        public bool SendThreadId = true;

        /// <summary>
        /// return default TraceDisplayFlags options if you don't supply one for the AddObject / AddType / SendObject / SendType functions .
        /// </summary>
        public TraceDisplayFlags GetDefault()
        {
            // display at least public (inherited) fields and properties
            // Privat members are discarded
            TraceDisplayFlags flags = 0;

            if (TTrace.Options.SendModifiers)
                flags = flags | TraceDisplayFlags.ShowModifiers;           // 1
            if (TTrace.Options.SendClassInfo)
                flags = flags | TraceDisplayFlags.ShowClassInfo;           // 2
            if (TTrace.Options.SendFields)
                flags = flags | TraceDisplayFlags.ShowFields;              // 4
            if (TTrace.Options.SendCustomAttributes)
                flags = flags | TraceDisplayFlags.ShowCustomAttributes;    // 8    
            if (TTrace.Options.SendNonPublic)
                flags = flags | TraceDisplayFlags.ShowNonPublic;           // 16
            if (TTrace.Options.SendInherited)
                flags = flags | TraceDisplayFlags.ShowInheritedMembers;    // 32   
            if (TTrace.Options.SendEvents)
                flags = flags | TraceDisplayFlags.ShowEvents;              // 64
            if (TTrace.Options.SendFunctions)
                flags = flags | TraceDisplayFlags.ShowMethods;             // 128
            if (TTrace.Options.SendDoc)
                flags = flags | TraceDisplayFlags.ShowDoc;                 // 256

            return flags;
        }
    }

    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------

#if !NETSTANDARD1_6 && !NETSTANDARD2_0

    /// <summary>
    /// configure tracetool using app.config
    /// </summary>
    public class ConfigSectionHandler : IConfigurationSectionHandler
    {
       /// <summary>
       /// The returned object is added to the configuration collection and is accessed
       /// by System.Configuration.ConfigurationSettings.GetConfig(System.String)
       /// </summary>
       /// <param name="parent"></param>
       /// <param name="configContext"></param>
       /// <param name="section"></param>
       /// <returns>section</returns>
       public Object Create (Object parent , Object configContext , XmlNode section )
       {
          //TTrace.Debug.Send ("ConfigSectionHandler.Create") ;
          return section ;
       }
    }

#endif

    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------

    /// <summary>
    /// TextWriter output. For Linq to SQL for example : NORTHWNDDataContext.Log = TTrace.Out 
    /// </summary>
    // ReSharper disable once InconsistentNaming
    internal class TTraceWriter : TextWriter
    {
#if !NETSTANDARD1_6
        public override void Close()
        {
            Dispose(true);
        }
#endif
        public override void WriteLine(string value)
        {
            TTrace.Debug.Send(value);
        }

        public override void WriteLine()
        {
            // this.Write(this.CoreNewLine);
        }

        public override void Write(char value)
        {
            //this._sb.Append(value);
            TTrace.Debug.Send(value.ToString());
        }

        public override void Write(string value)
        {
            if (value != null)
            {
                //    this._sb.Append(value);
                TTrace.Debug.Send(value);
            }
        }

        private static UnicodeEncoding _encoding;

        public override Encoding Encoding
        {
            get
            {
                if (_encoding == null)
                    _encoding = new UnicodeEncoding(false, false);
                return _encoding;
            }
        }
    } // TTraceWriter
}
