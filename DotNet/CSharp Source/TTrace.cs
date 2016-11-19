// TTrace.cs
//
// classes : TTrace , TTraceOptions , TTraceListener
//
// Provide access to the warning, eror and debug node and

// send the trace using socket or windows messages
//
// Author : Thierry Parent
// Version : 12.9
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//
// Change the tracetool project option ("conditional compilation constant") to specify the target dot net version :
// NETF1  (dot net framework 1)          , NETF2 ((dot net framework 2) ,
// NETCF1 (dot net compact framework 1)  , NETCF2 (dot net compact framework 2) , NETCF3 (dot net compact framework 3)

// History :
// 12.7 : 2015 10 06 : Some method no more raise exception.
// 12.7 : 2015 10 06 : Fix AssemblyDocumentationFileName exception
// 12.7 : 2015 10 07 : Added default options in TTraceOptions (from TraceDisplayFlags)
// 12.7 : 2015 10 26 : Added SendStack with default level
// 12.7 : 2015 11 08 : TraceNode constructor is public
// 12.7 : 2015 11 08 : TraceNode.AppendStack()
// 12.7 : 2015 11 08 : TraceToSend.Indent() and TraceToSend.EnterMethod() returns a TraceNode
// 12.8 : 2016 07 26 : Fix stack trace with Lambda
// 12.8 : 2016 07 28 : Fix addDependencyPropertiesValues, DisplayDependencyProperties
// 12.9 : 2016 10 05 : stack trace display the class of the method. Modifier is removed (public static,...)


using System.Text;
using System;
using System.Xml;
using System.Configuration;
using System.Collections;                 // ArrayList, queue
using System.Threading ;                  // thead pool, ResetEvent
using System.Net;
using System.Net.Sockets;
using System.IO ;                         // file exist
using System.Diagnostics ;                // Process
using System.Reflection ;
using System.Runtime.InteropServices;

// generic start in F2
#if (!NETCF1 && !NETF1)
using System.Collections.Generic;
#endif

#if (!NETCF1)
using Microsoft.Win32 ;                   // registry
#endif

namespace TraceTool
{

   /// <summary>
   /// TTrace is the entry point for all traces.
   /// TTrace give 3 'TraceNode' doors : Warning , Error and Debug.
   /// Theses 3 doors are displayed with a special icon (all of them have the 'enabled' property set to true.
   /// That class is fully static.
   /// </summary>

   public class TTrace
   {
      // internal members
      //-----------------

      static internal MsgQueueList MsgQueue;         // store all messages to send
      static internal StrKeyObjectList flushList;    // AutoResetEvent flush list
      private static Thread TraceThread;
      private static  AutoResetEvent DataReady ;     // data is ready to send
      private static  AutoResetEvent SendDone;       // message is send. Next message can be send.
      static internal bool _isSocketError = false ;
      static internal long _errorTime = 0;
      static internal long ConnectTime = 0;
      static internal string _LastError = "";
      static internal WinTrace _winTrace ;
      static internal WinWatch _winWatch ;
      static internal InternalWinTrace DefaultWinTrace ;
      static internal InternalWinTraceList FormTraceList ;
      static internal byte[] buffToSend;  // buffer to send to viewer
      static internal Socket _socket;
      static internal TextWriter _out;

#if (SILVERLIGHT)
      static internal DnsEndPoint _hostEntry;
      static internal SocketAsyncEventArgs _socketEventArg; 
#else
      static internal UdpClient _UdpSocket;
#endif
      /// number of message discarded (socket error)
      public static long nbDiscarded = 0;
      /// number of message send by silverlight (socket error)
      public static long nbSend = 0;

      /// TTrace Options (socket, show functions, ...)
      public static TTraceOptions Options ;

     
      //static initializer , initialise the class
      static TTrace()
      {       
         flushList = new StrKeyObjectList();              // AutoResetEvent flush list
         FormTraceList = new InternalWinTraceList();
         DefaultWinTrace = new InternalWinTrace() ;       // main InternalWinTrace. Id is empty
         FormTraceList.Add (DefaultWinTrace) ;
         MsgQueue      = new MsgQueueList();              // store all messages to send
         //DataReady     = new AutoResetEvent (false) ;   // data is ready to send
         Options       = new TTraceOptions () ;           // TTrace Options (socket, show functions, ...)
         //StopEvent   = new ManualResetEvent (false) ;   // ask the thread to quit

         // create the lock system and the thread
         DataReady = new AutoResetEvent(false);  // initial state false
         SendDone = new AutoResetEvent(false);   // initial state false
         TraceThread = new Thread(new ThreadStart(SendThreadEntryPoint)); //  new Thread(SendThreadEntryPoint);
         // force the thread to be killed when all foreground thread are terminated.
         // compatibility : don't work on compact framework 1. Work on all platform.
         TraceThread.IsBackground = true;  
         TraceThread.Start();

         try {   // since 12.5 : protect if configuration file is bad

#if (SILVERLIGHT)
         Options.SendMode = SendMode.Socket ;
         Options.SocketPort = 4502;
         Options.SocketHost = "127.0.0.1";
#else   // !SILVERLIGHT


#if (NETCF1 || NETCF2 || NETCF3)
         Options.SendMode = SendMode.Socket;
         Options.SocketHost = "PPP_PEER";   // force PPP_PEER (active sync) for pocket pc
#else
            XmlNode _config;
#endif
            _isSocketError = false;
            //_socket   : is initialized on first use
            //_winTrace : is initialized on first use
            //_winWatch : is initialized on first use


            string configFile;
            // 1) check the existence of the App.Config file

#if (!NETCF1 && !NETCF2 && !NETCF3)
            // call the tracetool ConfigSectionHandler.Create
#if (NETF2)
            _config = (XmlNode)ConfigurationManager.GetSection("TraceTool");
#else
            _config = (XmlNode) System.Configuration.ConfigurationSettings.GetConfig("TraceTool") ;
#endif

            if (_config != null) {
               //Debug.Send ("App.Config file exist") ;
               readConfig(_config);
               return;
            } else {
               //Debug.Send ("no App.Config file") ;
            }
#else  // (!NETCF1 && !NETCF2 && !NETCF3)
         configFile = Helper.GetCurrentProcessName() + ".config";
         if (getConfigFomFile(configFile) == true)
            return;
#endif // (!NETCF1 && !NETCF2 && !NETCF3)

            // 2) check the existence of the Web.Config file

#if (!NETCF1 && !NETCF2 && !NETCF3)
            configFile = AppDomain.CurrentDomain.SetupInformation.ConfigurationFile;
            if (getConfigFomFile(configFile) == true)
               return;
#endif

            // 3) check the existence of the App.TraceTool file

            // AppDomain.CurrentDomain.SetupInformation :  dot net framework 1.0
#if (!NETCF1 && !NETCF2 && !NETCF3)
            configFile = AppDomain.CurrentDomain.SetupInformation.ConfigurationFile;
            configFile = configFile.Replace(".config", ".TraceTool");
#else
         configFile = Helper.GetCurrentProcessName() + ".TraceTool";
#endif
            if (getConfigFomFile(configFile) == true)
               return;

            // 4) check the existence of tracetool.dll.TraceTool file
#if (!NETCF1 && !NETCF2 && !NETCF3)
            Assembly asm = typeof(TTrace).Assembly;
            configFile = asm.Location;
#else
         configFile = typeof(TTrace).Module.ToString();
#endif
            configFile = configFile + ".TraceTool";

            if (getConfigFomFile(configFile) == true)
               return;

            // 5) last chance : a "traceTool.xml" file in the current folder
            configFile = "traceTool.xml";
            if (getConfigFomFile(configFile) == true)
               return;


#endif  // !SILVERLIGHT

         }
         catch (Exception ex) {
            _LastError = ex.Message;
            return;
         }

      }  // TTrace constructor

      //------------------------------------------------------------------------------

#if (!SILVERLIGHT)
      //------------------------------------------------------------------------------
      // get the configuration from specified file
      internal static bool getConfigFomFile (string fileName)
      {
         try {

            XmlNode config;
            if (!File.Exists(fileName)) {
               //Debug.Send ("no " + fileName) ;
               return false;
            }
            //Debug.Send (fileName + " exist") ;
            XmlDocument doc = new XmlDocument();
            doc.Load(fileName);

            // SelectSingleNode :  all dot net framework and since compact framework 2.0
#if (!NETCF1)
            config = doc.SelectSingleNode("//TraceTool");
#else
         // Check if the TraceTool node is root or under configuration node
         foreach (XmlNode node in doc.ChildNodes)
         {
            string tagName = node.Name.ToLower();
            if (tagName.CompareTo("tracetool") == 0)
            {
               _config = node;
               readConfig();
               return;
            }
            if (tagName.CompareTo("configuration") == 0)
            {
               foreach (XmlNode subNode in node)
               {
                  string tagName2 = subNode.Name.ToLower();
                  if (tagName2.CompareTo("tracetool") == 0)
                  {
                     _config = subNode;
                     readConfig();
                     return;
                  }
               }
               return;
            }
         }

#endif


            if (config == null) {
               //Debug.Send (fileName + " file exist but with NO TraceTool section") ;
               return false;
            }

            //Debug.Send (fileName + " file exist with TraceTool section") ;
            return readConfig(config);
         }
         catch (Exception ex) {
            _LastError = ex.Message;
            return true;   // don't continue if error (else we can miss the error)
         }
      }

      //------------------------------------------------------------------------------
      // read configuration from XML node
      internal static bool readConfig (XmlNode config)
      {
         if (config == null)
            return false;

         try {
            // Since XML is case sensitive and is then source of error, it's better to detect keyword
            // without case sensitive.
            foreach (XmlNode node in config.ChildNodes) {
               // detect 'param' tag not case sensitive
               string tagName = node.Name.ToLower();
               if (tagName.CompareTo("param") == 0) {
                  // get the name and value attributes
                  string ParamName = "";
                  string ParamValue = "";
                  getParamNameAndValue(node, out ParamName, out ParamValue);

                  InitOptions(ParamName, ParamValue);
               } else if (tagName.CompareTo("debug") == 0)
                  InitTraceNode(node, Debug);
               else if (tagName.CompareTo("warning") == 0)
                  InitTraceNode(node, Warning);
               else if (tagName.CompareTo("error") == 0)
                  InitTraceNode(node, Error);
            }  // next tag in config section
         }
         catch (Exception ex) {
            _LastError = ex.Message;
            return false;
         }
         return true;
      }

      //------------------------------------------------------------------------------

      internal static void getParamNameAndValue (XmlNode node , out string ParamName, out string ParamValue)
      {
         ParamName   = "" ;
         ParamValue  = "" ;
         foreach ( XmlAttribute attrib in node.Attributes)
         {
            string attribName = attrib.Name.ToLower() ;
            if (attribName.CompareTo ("name") == 0)
               ParamName = attrib.Value.ToLower() ;
            if (attribName.CompareTo ("value") == 0)
               ParamValue = attrib.Value.ToLower() ;
         }
         //Debug.Send (ParamName,ParamValue) ;
      }

      //------------------------------------------------------------------------------

      internal static void InitTraceNode(XmlNode XmlTraceNode, TraceToSend trace)
      {
         foreach (XmlNode node in XmlTraceNode.ChildNodes)
         {
            // detect 'param' tag wihout case sensitive
            string tagName = node.Name.ToLower();
            if (tagName.CompareTo("param") == 0)
            {
               // get the name and value attributes
               string ParamName = "";
               string ParamValue = "";
               getParamNameAndValue(node, out ParamName, out ParamValue);

               if (ParamName.CompareTo("enabled") == 0)
               {                // bool Enabled
                  if (ParamValue.ToLower().CompareTo("true") == 0)
                     trace.Enabled = true;
                  else if (ParamValue.ToLower().CompareTo("false") == 0)
                     trace.Enabled = false;
               }
               else if (ParamName.CompareTo("iconindex") == 0)
               {       // int IconIndex
                  try
                  {
                     trace.IconIndex = Int32.Parse(ParamValue);
                  }
                  catch
                  {
                     // no change
                  }
               }     // param name="xxx"
            }        // param tag
         }           // next param
      }

#endif  // !SILVERLIGHT

      //------------------------------------------------------------------------------

      internal static void InitOptions(string ParamName, string ParamValue)
      {
         if (ParamName.CompareTo ("sendmode") == 0)
         {                // SendMode
            if (ParamValue.ToLower().CompareTo("winmsg") == 0)
               Options.SendMode = SendMode.WinMsg ;
            else if (ParamValue.ToLower().CompareTo("socket") == 0)
               Options.SendMode = SendMode.Socket ;
            else if (ParamValue.ToLower().CompareTo("none") == 0)
               Options.SendMode = SendMode.None;
         }
         else if (ParamName.CompareTo ("sockethost") == 0)
         {       // string SocketHost
            Options.SocketHost = ParamValue ;
         }
         else if (ParamName.CompareTo ("socketport") == 0)
         {       // int SocketPort
            try
            {
               Options.SocketPort = Int32.Parse (ParamValue) ;
            }
            catch
            {
               Options.SocketPort = 8090 ;
            }
         }
         else if (ParamName.CompareTo ("objecttreedepth") == 0)
         {       // int ObjectTreeDepth
            try
            {
               Options.ObjectTreeDepth = Int32.Parse (ParamValue) ;
            }
            catch
            {
               Options.ObjectTreeDepth = 3 ;
            }
         }

         else if (ParamName.CompareTo("sendmodifiers") == 0)           // 1
         {    
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendModifiers = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendModifiers = false;
         }
         else if (ParamName.CompareTo("sendclassinfo") == 0)            // 2
         {    
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendClassInfo = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendClassInfo = false;
         }
         else if (ParamName.CompareTo("sendfields") == 0)               // 4
         {    
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendFields = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendFields = false;
         }
         else if (ParamName.CompareTo("sendcustomattributes") == 0)     // 8
         {    
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendCustomAttributes = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendCustomAttributes = false;
         }
         else if (ParamName.CompareTo("sendnonpublic") == 0)            // 16
         {   
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendNonPublic = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendNonPublic = false;
         }
         else if (ParamName.CompareTo("sendinherited") == 0)            // 32
         {    
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendInherited = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendInherited = false;
         }
         else if (ParamName.CompareTo("sendevents") == 0)               // 64
         {     
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendEvents = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendEvents = false;
         }
         else if (ParamName.CompareTo ("sendfunctions") == 0)           // 128
         {   
            if (ParamValue.ToLower().CompareTo("true") == 0)
               Options.SendFunctions = true ;
            else if (ParamValue.ToLower().CompareTo("false") == 0)
               Options.SendFunctions = false ;
         }
         else if (ParamName.CompareTo("senddoc") == 0)                  // 256
         {
             if (ParamValue.ToLower().CompareTo("true") == 0)
                 Options.SendDoc = true;
             else if (ParamValue.ToLower().CompareTo("false") == 0)
                 Options.SendDoc = false;
         }
         else if (ParamName.CompareTo("sendprivate") == 0)             
         {      // bool SendPrivate
            if (ParamValue.ToLower().CompareTo("true") == 0)
               Options.SendPrivate = true ;
            else if (ParamValue.ToLower().CompareTo("false") == 0)
               Options.SendPrivate = false ;
         }
         else if (ParamName.CompareTo ("sendprocessname") == 0)         
         {    // bool SendProcessName
            if (ParamValue.ToLower().CompareTo("true") == 0)
               Options.SendProcessName = true ;
            else if (ParamValue.ToLower().CompareTo("false") == 0)
               Options.SendProcessName = false ;
         }
         else if (ParamName.CompareTo ("senddate") == 0)                
         {           // bool SendDate
            if (ParamValue.ToLower().CompareTo("true") == 0)
               Options.SendDate = true ;
            else if (ParamValue.ToLower().CompareTo("false") == 0)
               Options.SendDate = false ;
         }
         else if (ParamName.CompareTo("sendthreadid") == 0)             
         {           // bool SendThreadId
            if (ParamValue.ToLower().CompareTo("true") == 0)
               Options.SendThreadId = true;
            else if (ParamValue.ToLower().CompareTo("false") == 0)
               Options.SendThreadId = false;
         }
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// return initialisation error
      /// </summary>
      public static string InitError()
      {
         return _LastError;
      }          

      //------------------------------------------------------------------------------
      /// <summary>
      /// Stop sub-system before leaving your program. You may call TTrace.Flush() before Stop()
      /// </summary>
      public static void Stop ()
      {
         TraceThread.Abort();
         TraceThread = null;
      }                 

      //------------------------------------------------------------------------------
      /// <summary>
      /// Show or hide the trace program
      /// </summary>
      /// <param name="IsVisible">When True : Show. When False : Hide</param>
      public static void Show(bool IsVisible)
      {
         StringList CommandList = new StringList();
         Helper.addCommand(CommandList,  TraceConst.CST_SHOW , IsVisible);
         TTrace.SendToClient (CommandList);
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Set the global search criteria. You must call TTrace.Wintrace.FindNext to position to the next or previous matching node
      /// </summary>
      /// <param name="Text">Text to search</param>
      /// <param name="Sensitive">Search is case sensitive</param>
      /// <param name="WholeWord">match only whole word</param>
      /// <param name="Highlight">Highlight results</param>
      /// <param name="SearchInAllPages">call to FindNext will search also in other traces windows if true</param>
      public static void Find (string Text, bool Sensitive, bool WholeWord , bool Highlight, bool SearchInAllPages) 
      {
         StringList CommandList = new StringList();
         int flags = 0 ;
         // Sensitive<<3+WholeWord<<2+highlight<<1+SearchInAllPages
         if (Sensitive)
            flags += 8;
         if (WholeWord)
            flags += 4;
         if (Highlight)
            flags += 2;
         if (SearchInAllPages)
            flags += 1;

         Helper.addCommand(CommandList,TraceConst.CST_FIND_TEXT ,flags, Text);
         TTrace.SendToClient(CommandList);     
      }


      //------------------------------------------------------------------------------
      /// <summary>
      /// Clear all traces
      /// </summary>
      public static void ClearAll ()
      {
         WinTrace.ClearAll () ;
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Close the viewer
      /// </summary>
      public static void CloseViewer()
      {
         StringList CommandList = new StringList();

         Helper.addCommand(CommandList,TraceConst.CST_CLOSE_VIEWER);
         TTrace.SendToClient(CommandList);
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Close the socket
      /// Tips : Socket is automatically closed when destroyed. No need to create destructor
      /// </summary>
      public static void CloseSocket()
      {
         if (_socket == null)
            return ;
         _socket.Close() ;
         _socket = null ;
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
               _winTrace = new WinTrace() ;
            }
            return _winTrace ;
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
               _winWatch = new WinWatch() ;   // internal
            }
            return _winWatch ;
         }
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Shortcut to WinTrace.Warning
      /// </summary>
      public static TraceToSend Warning
      {
         get { return WinTrace.Warning ; }
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Shortcut to WinTrace.Error
      /// </summary>
      public static TraceToSend Error
      {
         get { return WinTrace.Error ; }
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Shortcut to WinTrace.Debug
      /// </summary>
      public static TraceToSend Debug
      {
         get { return WinTrace.Debug ; }
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// TextWriter output. For Linq to SQL for example : NORTHWNDDataContext.Log = TTrace.Out 
      /// </summary>
      
      public static TextWriter Out
      {
         get
         {
            if (_out == null)
               _out = new TTraceWriter();
            return _out;
         }
      }
 
      //------------------------------------------------------------------------------
      // send the ArrayList to the viewer (using thread)

      internal static void SendToWinTraceClient(StringList CommandList, string winTraceId)
      {
         SendToWinTraceClient(CommandList, winTraceId, FormatDate(DateTime.Now), Helper.GetCurrentThreadId()); // DateTime.Now.ToString("HH:mm:ss:fff")
      }

      //------------------------------------------------------------------------------
      // send the trace ArrayList to the viewer (using thread)

      internal static void SendToWinTraceClient(StringList CommandList, string winTraceId, string dateTime, string threadName)
      {
         // insert thread id
         if (TTrace.Options.SendThreadId)
         {
            if (threadName == null)
               CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_THREAD_NAME, Helper.GetCurrentThreadId()));
            else
               CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_THREAD_NAME, threadName));
         }

         // add current time. TTrace.FormatDate is faster than  DateTime.ToString("HH:mm:ss:fff")
         if (dateTime == null)
            CommandList.Insert (0, String.Format("{0,5}{1}", TraceConst.CST_MESSAGE_TIME , FormatDate(DateTime.Now))) ; // DateTime.Now.ToString("HH:mm:ss:fff") ));
         else
            CommandList.Insert (0, String.Format("{0,5}{1}", TraceConst.CST_MESSAGE_TIME , dateTime ));

         // add process name
         if (TTrace.Options.SendProcessName == true)
         {
            CommandList.Insert (0, String.Format("{0,5}{1}",
               TraceConst.CST_PROCESS_NAME ,
               Helper.GetCurrentProcessName() ));
         }

         // CST_USE_TREE MUST be inserted at the first position
         if (winTraceId != null && winTraceId != "")
            CommandList.Insert (0, String.Format("{0,5}{1}", TraceConst.CST_USE_TREE , winTraceId));

         SendToClient (CommandList);
      }

      //------------------------------------------------------------------------------

      // send the watch ArrayList to the viewer (using thread)
      internal static void SendToWinWatchClient(StringList CommandList, string winWatchId)
      {
         // insert thread id
         if (TTrace.Options.SendThreadId)
            CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_THREAD_NAME, Helper.GetCurrentThreadId()));

         // add current time. TTrace.FormatDate is faster than  DateTime.ToString("HH:mm:ss:fff")
         CommandList.Insert (0, String.Format("{0,5}{1}", TraceConst.CST_MESSAGE_TIME , FormatDate(DateTime.Now))) ; // FormatDate will add date if necessary

         // add process name
         if (TTrace.Options.SendProcessName == true)
         {
            CommandList.Insert (0, String.Format("{0,5}{1}",
               TraceConst.CST_PROCESS_NAME ,
               Helper.GetCurrentProcessName() ));
         }

         // CST_USE_TREE MUST be inserted at the first position
         //if (winWatch != null && winWatch.Id != null && winWatch.Id != "")
         CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_WINWATCH_ID, winWatchId));

         SendToClient (CommandList);
      }

      //------------------------------------------------------------------------------

      // send the ArrayList to the viewer (using thread)
      internal static void SendToClient(StringList CommandList)
      {
         lock (DataReady)    // don't lock the MsgQue, because we can swap with workQueue
         {
            MsgQueue.Add(CommandList);
         }
         // signal that data are ready to be send
         DataReady.Set();
      }

      //------------------------------------------------------------------------------
      /// flush remaining traces to the viewer
      public static void Flush()
      {
         StringList CommandList = new StringList();

         AutoResetEvent flushEvent = new AutoResetEvent(false) ;
         string key = Helper.NewGuid().ToString() ;
         flushList.Add(key, flushEvent);

         CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_FLUSH, key));

         lock (DataReady)    // don't lock the MsgQue, because we can swap with workQueue
         {
            MsgQueue.Add(CommandList);
         }
         // signal that data are ready to be send
         DataReady.Set();
         flushEvent.WaitOne() ;
         flushList.Remove(key);
      }

      //------------------------------------------------------------------------------

      //public static String swap ;

      // the thread function that send messages to the server
      private static void SendThreadEntryPoint () // Object obj
      {
         string MessageString ;
         int tot ;
         MsgQueueList tempList; // used for swap queue
         MsgQueueList workQueue = new MsgQueueList();

         while( true )
         {
            //WaitHandle[] handles = new WaitHandle[2];
            //handles[0] = StopEvent;
            //handles[1] = DataReady;
            //if( WaitHandle.WaitAny(handles) == 0 )
            //   break;

            
#if (NETF3 || SILVERLIGHT)
            DataReady.WaitOne(1000);       // silverlight don't support all overload of WaitOne
#else
           DataReady.WaitOne(1000,false);  // Use this overload to be compatible with dotnet 1 and dotnet 2 prior to SP2 (thanks Robert)
#endif
            // lock the message queue and swap with the empty local queue (workQueue)
            lock (DataReady)
            {
               //swap = swap + "Begin at " + DateTime.Now.ToString("HH:mm:ss:fff") + " : " + MsgQueue.Count ;

               tempList = workQueue ;          // workQueue is the empty list
               workQueue = MsgQueue ;    // MsgQueue is the list of message to send
               MsgQueue = tempList ;
            }

            if (_isSocketError && Options.SendMode == SendMode.Socket)
            {
               long actTime = DateTime.Now.Ticks;
               if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
                  _isSocketError = false;
            }

            // loop the outbound messages...
            foreach (StringList CommandList in workQueue)
            {
               // special case : the CST_FLUSH message is handled by the sender thread, not to be send
               if (CommandList.Count > 0) // only one message
               {
                  string msg = (string) CommandList[0] ;
                  if (Int32.Parse( msg.Substring (0,5) ) == TraceConst.CST_FLUSH)
                  {
                     String key = msg.Substring (5,msg.Length-5) ;
                     AutoResetEvent flushEvent ;
                     try {
                        flushEvent = (AutoResetEvent) flushList[key];
                        flushEvent.Set() ;
                     } catch { }
                   continue ;
                  }
               }

               // check if the message contains information for local log
               ParseForInternal (CommandList);

               // If socket mode and socket error : don't send messages.
               // When the queue is empty, reset the error mode.
               // The ParseForInternal will be called to save to file and responds to flush events.
               if (_isSocketError && Options.SendMode == SendMode.Socket)
               {
                  nbDiscarded++;
                  continue;
               }

               if (Options.SendMode != SendMode.None) 
               {
                  // normal command list
                  tot = 0;
                  StringBuilder sb = new StringBuilder();
                  foreach (string msg in CommandList) {
                     tot += msg.Length + 1;
                     sb.Append(msg).Append('\0');
                  }
                  sb.Append('\0');

                  // For previous version of tracetool,it's important to understand that strings was send in ASCII (1 byte per char),
                  // because it's the common denominator for all languages targetting tracetool.
                  // Since silverlight don't allow ASCII encoding, the tracetool dot net API is now unicode by default. 
                  // System.Text.Encoding in silverlight : BigEndianUnicode,Unicode (little endian),UTF8. 
                  // Other kind of encoding exist for other platforms.

                  sb.Insert(0, char.ToString((char)0xFEFF)); // start with the UTF-16, little endian byte order mark

                  MessageString = sb.ToString();
                  tot++;

                  try
                  { 
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                     if (Options.SendMode == SendMode.WinMsg) 
                        SendMessageToWindows(sb);
                     else if (Options.SendMode == SendMode.Socket)
                        SendMessageToSocket(sb);
                     // else no transport
#else // compact framework or silverlight : only socket
                     if (Options.SendMode == SendMode.Socket)
                        SendMessageToSocket (sb) ;
                  // else no transport
#endif
                  } catch (Exception ex) {
                     _LastError = ex.Message;         // for debug purpose
                  }
               }   // sendMode <> none
            }      // loop workQueue
            workQueue.Clear();
         }         // infinite loop 
      }            // thread function

      //------------------------------------------------------------------------------

#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)

      internal static IntPtr VarPtr(object e)
      {
         GCHandle GC = GCHandle.Alloc(e, GCHandleType.Pinned);
         IntPtr gc = GC.AddrOfPinnedObject();
         GC.Free();
         return gc;
      }

      internal static void SendMessageToWindows (StringBuilder Message)
      {
         int DebugWin = StartTDebug () ;
         if (DebugWin != 0)
         {
            COPYDATASTRUCT CDS = new COPYDATASTRUCT();  // StructLayout.CharSet is CharSet.Ansi

            CDS.dwData = (IntPtr) TraceConst.WMD;            
            CDS.cbData = Message.Length * 2 ;    // unicode are 2 bytes
            CDS.lpData = VarPtr(Message.ToString());     // automatically convert string to unicode (see StructLayout attribute in COPYDATASTRUCT)
            Helper.SendMessage(DebugWin, TraceConst.WM_COPYDATA, IntPtr.Zero, VarPtr(CDS));
         }
      }
#endif

      //------------------------------------------------------------------------------
      // Prepare the byte[] buffToSend
      // Called by SendMessageToSocket
      internal static void Prepare_buffToSend(StringBuilder Message)
      {

         // For previous version of tracetool,it's important to understand that strings was send in ASCII (1 byte per char),
         // because it's the common denominator for all languages targetting tracetool.
         // Since silverlight don't allow ASCII encoding, the tracetool dot net API is now unicode by default. 
         // System.Text.Encoding in silverlight : BigEndianUnicode,Unicode (little endian),UTF8. Other kind of encoding exist for other platforms.


         int intMsgLen = Message.Length * 2; // number of bytes in the message : message len * 2 (unicode)

         int c = 0;
         if (c == 0)  // force new version
         {
            // new version : 
            // write the init byte (WMD = 123) then message lenght as a DWORD then the message

            buffToSend = new byte[5 + intMsgLen]; // create the buffer : WMD byte + message len as a DWORD + message
            // write the WMD byte into the buffer
            buffToSend[0] = TraceConst.WMD;       // the WMD byte ensure the message is valid.
            // Append the intMsgLen into the buffer
            byte[] byteArray = BitConverter.GetBytes(intMsgLen);
            for (c = 0; c <= 3; c++)
               buffToSend[c + 1] = byteArray[c];  // start at 1
            c = 5;  // jump over WMD byte and DWORD

         }
         else
         {
            // old version :
            // insert the lenght followed by the null terminator and the message.
            // for compatibility issue, the length is coded as a AnsiString (single byte)

            string strMsgLen = intMsgLen.ToString();
            buffToSend = new byte[strMsgLen.Length + 1 + intMsgLen]; // create the buffer : message len as an ASCII string + '\0' + message

            for (c = 0; c < strMsgLen.Length; c++)
            {
               char charNum = strMsgLen[c];
               byte byteNum = 32; // space
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
                  default: byteNum = 32; break;
               }
               buffToSend[c] = byteNum;
            }
            c = strMsgLen.Length;
            buffToSend[c] = 0;  // add null term
            c++;
         }

         char[] chars = Message.ToString().ToCharArray();
         System.Text.Encoding.Unicode.GetBytes(
            chars,          // source to encode
            0,              // The index of the first character to encode
            chars.Length,   // The number of characters to encode
            buffToSend,     // The byte array to contain the resulting sequence of bytes
            c);             // The index at which to start writing the resulting sequence of bytes

         //buffToSend = System.Text.Encoding.Unicode.GetBytes(Message.ToString()); // older version : ASCII encoding
      }

      //------------------------------------------------------------------------------

      internal static void SendMessageToSocket(StringBuilder Message)
      {
         if (_isSocketError)
         {
            long actTime = DateTime.Now.Ticks ;

            if (actTime - _errorTime > 50000000) // 5 secs = 50 millions of ticks (100 nanos sec).
               _isSocketError = false ;
            else
               return ;  // lose message
         }

         // allocate and fill the buffToSend array
         Prepare_buffToSend(Message) ;
         
         if (Options.SocketHost == null || Options.SocketHost == "")
            Options.SocketHost = "127.0.0.1" ;

         if (Options.SocketPort == 0)
            Options.SocketPort = 8090 ;

#if (!SILVERLIGHT)
         // Synchrone communication (not supported by silverlight)

         if (Options.SocketUdp)
         {
            if (_UdpSocket == null)
               _UdpSocket = new UdpClient(Options.SocketHost, Options.SocketPort);
            try
            {
               _UdpSocket.Send(buffToSend, buffToSend.Length);
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
               _LastError = ex.Message;         // for debug purpose
            }
            return;
         }

         // the socket must be connected in the sender thread !!! not here..  to do
         if (_socket == null)
            _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
         // if the socket is disconnect since the last use, reopen it
         if (_socket.Connected == false)
         {
            // resolve adress

#if (NETF1 || NETCF1)
            IPHostEntry hostEntry = Dns.Resolve(Options.SocketHost);
#else // NETF2 || NETCF2 or more
            IPHostEntry hostEntry = Dns.GetHostEntry(Options.SocketHost);  // on ppc emulator : host entry = 192.168.55.100
#endif
            if ( hostEntry == null )
            {
               _socket = null ; // force recreate socket
               _isSocketError = true ;
               _errorTime = DateTime.Now.Ticks ;
               return ;
            }

            // Don't get the first adress. It's perhaps a IPv6 adress.
            // Thanks BCheng for the IPV4 fix.
            IPEndPoint endPoint =  null ;
            foreach (IPAddress ip in hostEntry.AddressList)
            {
               if (ip.AddressFamily == AddressFamily.InterNetwork)
               {
                  endPoint = new IPEndPoint(ip, Options.SocketPort);
                  break;
               }
            }

            if (endPoint == null)
               return ;

            try
            {
               _socket.Connect(endPoint);
            }
            catch (Exception ex)
            {
               _socket = null ; // force recreate socket
               _isSocketError = true ;
               _errorTime = DateTime.Now.Ticks ;
               _LastError = ex.Message;         // for debug purpose
               return ;
            }

         } // _socket.Connected == false

    try
         {
            _socket.Send(buffToSend, 0, buffToSend.Length, 0);
         }
         catch  (Exception ex)
         {
            _socket = null ; // force recreate socket
            _isSocketError = true ;
            _errorTime = DateTime.Now.Ticks ;
            _LastError = ex.Message;         // for debug purpose
         }
#else  // SILVERLIGHT

         // asynchrone communication

         if (_socketEventArg == null)
         {
            _hostEntry = new DnsEndPoint(Options.SocketHost, Options.SocketPort);
            _socketEventArg = new SocketAsyncEventArgs();
            _socketEventArg.Completed += new EventHandler<SocketAsyncEventArgs>(SocketEventArg_Completed);
            _socketEventArg.RemoteEndPoint = _hostEntry;
         }
         _socketEventArg.SetBuffer(0, 0);
         _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
         _socketEventArg.UserToken = _socket;
         _socket.ConnectAsync(_socketEventArg);  // connect and send
         ConnectTime = DateTime.Now.Ticks;
         if (SendDone.WaitOne(10000) == false)  // millisecondsTimeout : 10 sec
         {
            _errorTime = DateTime.Now.Ticks;
            _isSocketError = true;
            nbDiscarded++;
         }
#endif // SILVERLIGHT
      } // SendMessageToSocket function

      //------------------------------------------------------------------------------

#if (SILVERLIGHT)
     static void SocketEventArg_Completed(object sender, SocketAsyncEventArgs e)
      {
         switch (e.LastOperation)
         {
            case SocketAsyncOperation.Connect:
               ProcessConnect(e);
               break;
            case SocketAsyncOperation.Receive:
               //ProcessReceive(e);   // no message received.
               break;
            case SocketAsyncOperation.Send:
               ProcessSend(e);
               break;
            //default: 
               //throw new Exception("Invalid operation completed");
         }
      }

     // Called when a ConnectAsync operation completes
     static void ProcessConnect(SocketAsyncEventArgs e)
     {
        if (e.SocketError == SocketError.Success)
        {
           // Successfully connected to the server

           e.SetBuffer(buffToSend, 0, buffToSend.Length);
           Socket sock = e.UserToken as Socket;
           bool willRaiseEvent = sock.SendAsync(e);
           if (!willRaiseEvent)
              ProcessSend(e);

        }
        else   // acess denied
        {
           nbDiscarded++;
           _errorTime = DateTime.Now.Ticks;
           long nbtick = _errorTime - ConnectTime;
           _isSocketError = true;
           SendDone.Set();  // when error is detected, wake up the thread to discard the message
           //throw new SocketException((int)e.SocketError);
        }
     }

     // Called when a SendAsync operation completes
     static void ProcessSend(SocketAsyncEventArgs e)
     {
        if (e.SocketError == SocketError.Success)
        {
           // Sent message to the server successfully
           _socket.Shutdown(SocketShutdown.Both);
           _socket.Close();
           nbSend++;
        }
        else
        {
           _isSocketError = true;
           _errorTime = DateTime.Now.Ticks;
           //throw new SocketException((int)e.SocketError);
        }
        SendDone.Set();  // error or not : wake up thread
     }



#endif // SILVERLIGHT


     //------------------------------------------------------------------------------

      internal static InternalWinTrace getInternalTraceForm(string TraceWinId, bool doCreate)
      {
         if (TraceWinId == null || TraceWinId == "" || TraceWinId == "_")
            return DefaultWinTrace;

         foreach (InternalWinTrace internalForm in FormTraceList)
         {
            if (internalForm.Id == TraceWinId)
               return internalForm;
         }

         // if the trace window don't exist, create it if needed
         InternalWinTrace result = null ;
         if (doCreate)
         {
            result = new InternalWinTrace();
            result.LogFileName = "";
            result.LogFileType = 3;   // no log
            result.IsMultiColTree = false;
            FormTraceList.Add(result);
            result.Id = TraceWinId;
         }
         return result ;
      }

      //------------------------------------------------------------------------------

      internal static void ParseForInternal(StringList CommandList)
      {
         int command ;
         int c ;
         string commandParams ;
         InternalWinTrace TraceForm = DefaultWinTrace ; // traces are send to the master trace form by default
         // to be valid, CST_USE_TREE or CST_USE_MULTICOL_TREE or CST_WINWATCH_ID must be the first command
         if (CommandList.Count > 0) // only one message
         {
            string msg = (string)CommandList[0];
            command = Int32.Parse(msg.Substring(0, 5));
            commandParams = msg.Substring(5, msg.Length-5);

            if (command == TraceConst.CST_USE_TREE)
               TraceForm = getInternalTraceForm(commandParams, false);
            //else if (command == TraceConst.CST_USE_MULTICOL_TREE)
            //   TraceForm = getInternalTraceForm(commandParams, false);
            else if (command == TraceConst.CST_WINWATCH_ID)
               return;
         }

         // local file is disabled in silverlight
         #if (SILVERLIGHT)
         if (CommandList != null) // always != null. Used to bypass "Unreachable code detected" warning
            return;
         #endif

         // stop parsing if the winForm is not registered or the winForm don't need to be saved
         // 3, Local log is disabled
         // 4, Local log enabled. No size limit.
         // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
         if (TraceForm == null || TraceForm.LogFileType == 3)
            return;

         string LeftMsg      = "";   // Left col
         string RightMsg     = "";   // right col
         string TraceID      = "";   // the reference of the node : it's a guid
         string ThreadID     = "";   // thread id of the sender
         string ProcessName  = "" ;  // optional : the name of the process that send traces
         int    TreeIcon     = -1;   // -1 by default, converted to 24
         string ParentId     = "";
         string MessageTime  = "";

         bool   IsNewNode = false;      // check if message is a new node
         StringBuilder MemberXml = new StringBuilder();

         foreach (string msg in CommandList)
         {
            command = Int32.Parse(msg.Substring(0, 5));
            commandParams = msg.Substring(5, msg.Length - 5);
            switch (command)
            {
               case TraceConst.CST_WATCH_NAME:     return;  // Bypass watches
               case TraceConst.CST_MESSAGE_TIME:   MessageTime = commandParams;           break;
               case TraceConst.CST_PROCESS_NAME:   ProcessName = commandParams;           break;
               case TraceConst.CST_THREAD_ID:      ThreadID = "0x" + Int32.Parse(commandParams).ToString("X2");  break;
               case TraceConst.CST_THREAD_NAME:    ThreadID = commandParams;              break;
               case TraceConst.CST_ICO_INDEX:      TreeIcon = Int32.Parse(commandParams); break;
               case TraceConst.CST_TRACE_ID:       TraceID = commandParams;               break;
               case TraceConst.CST_LEFT_MSG:       LeftMsg = commandParams;               break; // param : msg
               case TraceConst.CST_RIGHT_MSG:      RightMsg = commandParams;              break;   // param : msg

               case TraceConst.CST_NEW_NODE:
                  // param1 : Parent Node
                  ParentId = commandParams;
                  IsNewNode = true;
                  break;

               case TraceConst.CST_CREATE_MEMBER:
                  MemberXml.Append("<Member>");
                  Helper.HtmlEncode(commandParams, MemberXml);
                  break;
               case TraceConst.CST_MEMBER_COL2:
                  if (commandParams != "")
                  {
                     MemberXml.Append("<ColB>");
                     Helper.HtmlEncode(commandParams, MemberXml);
                     MemberXml.Append("</ColB>");
                  }
                  break;
               case TraceConst.CST_MEMBER_COL3:
                  if (commandParams != "")
                  {
                     MemberXml.Append("<ColC>");
                     Helper.HtmlEncode(commandParams, MemberXml);
                     MemberXml.Append("</ColC>");
                  }
                  break;
               case TraceConst.CST_MEMBER_VIEWER_KIND:
                  if (Int32.Parse(commandParams) != TraceConst.CST_VIEWER_NONE)
                     MemberXml.Append("<ViewerKind>").Append(commandParams).Append("</ViewerKind>");
                  break;
               case TraceConst.CST_ADD_MEMBER:
                  MemberXml.Append("</Member>");
                  break;
            }  // switch
         }     // for each

         // if new node then save to log file
         if (IsNewNode == false)
            return;

         StringBuilder xml = new StringBuilder();
         xml.Append("<Node");
         if (ProcessName != "") {
            xml.Append(" Process=\"") ;
            Helper.HtmlEncode(ProcessName,xml) ;
            xml.Append("\"");
         }
         if (MessageTime != ""){
            xml.Append(" Time=\"") ;
            Helper.HtmlEncode(MessageTime,xml) ;
            xml.Append("\"");
         }
         if (ParentId != ""){  // add parent relation if not root
            xml.Append(" Parent=\"") ;
            Helper.HtmlEncode(ParentId,xml) ;
            xml.Append("\"");
         }
         if (TraceID != ""){
            xml.Append(" Id=\"") ;
            Helper.HtmlEncode(TraceID,xml) ;
            xml.Append("\"");
         }
         if (ThreadID != ""){
            xml.Append(" ThId=\"") ;
            Helper.HtmlEncode(ThreadID,xml) ;
            xml.Append("\"");
         }
         // don't save default
         if (TreeIcon != -1 && TreeIcon != 24)
            xml.Append(" Icon=\"").Append(TreeIcon).Append("\"");
         xml.Append(">");   // <Node ...>

         if (TraceForm.IsMultiColTree)
         {
            //<ColValue Order="2">C3</ColValue>
            string[] columns = LeftMsg.Split(new Char[] { '\t' });
            c = 0 ;
            foreach (string Column in columns) {
               xml.Append("<ColValue Order=\"").Append(c).Append("\">") ;
               Helper.HtmlEncode(Column,xml) ;
               xml.Append("</ColValue>");
               c++ ;
            }
         } else {
            // save the tree col1
            Helper.HtmlEncode(LeftMsg,xml);
            // save the tree col 2
            if (RightMsg != "") {
               xml.Append("<Col2>") ;
               Helper.HtmlEncode(RightMsg,xml) ;
               xml.Append("</Col2>");
            }
         }

         // append member to xml
         xml.Append(MemberXml);

         xml.Append("</Node>");

         if (TraceForm.LogFileName.Trim() == "")
            TraceForm.LogFileName = "TraceLog.xml";

         //string FileToWrite = "";
         if (TraceForm.LogFileType == 3) {            // 3, Local log disaled.
            // should not happens. Detected before parsing
            return ;
         } else if (TraceForm.LogFileType == 4) {     // 4, Local log enabled. No size limit.
            TraceForm.LastLocalLogFileName = TraceForm.LogFileName;
            if (TraceForm.CurrentFileNumber != 0)
            {
               // Append CurrentFileNumber Before extension            
               int pos = TraceForm.LastLocalLogFileName.LastIndexOf('.');
               if (pos == -1)// no extension
                  TraceForm.LastLocalLogFileName = TraceForm.LastLocalLogFileName + '_' + TraceForm.CurrentFileNumber + ".xml"; //$NON-NLS-1$
               else
                  TraceForm.LastLocalLogFileName = TraceForm.LastLocalLogFileName.Substring(0, pos - 1) + '_' + TraceForm.CurrentFileNumber + TraceForm.LastLocalLogFileName.Substring(pos);
            }
         } else {                                     // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
            string FileExt = Path.GetExtension(TraceForm.LogFileName);  // include the dot
            StringBuilder strbBuilder = new StringBuilder();
            strbBuilder.Append(TraceForm.LogFileName.Substring(0, TraceForm.LogFileName.Length - FileExt.Length ));
            // append YYYYMMDD
            DateTime now = DateTime.Now ;
            int temp;
            temp = now.Year;
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
            if (TraceForm.CurrentFileNumber != 0)
               strbBuilder.Append('_').Append(TraceForm.CurrentFileNumber);
            // append file extension (XML)
            strbBuilder.Append(FileExt);
            TraceForm.LastLocalLogFileName = strbBuilder.ToString();
         }

         FileStream f ;
         if (File.Exists(TraceForm.LastLocalLogFileName) == false)
         {
            f = new FileStream(TraceForm.LastLocalLogFileName, FileMode.Create);

            // include header in file
            if (TraceForm.IsMultiColTree) {
               StringBuilder strbBuilder = new StringBuilder();
               strbBuilder.Append("<MainColumn>").Append(TraceForm.MainCol).Append("</MainColumn>") ;
               string[] cols = TraceForm.TitleList.Split(new Char[] { '\t' });
               c = 0;
               foreach (string col in cols) {
                  if (col != "")
                     strbBuilder.Append("<ColTitle Order=\"").Append(c).Append("\">").Append(col).Append("</ColTitle>");
                  c++;
               }
               xml.Insert(0,strbBuilder.ToString());
            }
            xml.Insert(0,"<Data>");
         } else {  // append only the node
            f = File.Open(TraceForm.LastLocalLogFileName, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite);
            f.Seek(f.Length-7, SeekOrigin.Begin); // override the </data> tag
         }
         xml.Append("\n</Data>");
         Byte[] info = new UTF8Encoding(true).GetBytes(xml.ToString());
         f.Write(info, 0, info.Length);
         f.Close();
         f = null ;
         xml = null ;

         // limit file size
         if (TraceForm.MaxLines != -1)
         {
            TraceForm.LinesWritten++;
            if (TraceForm.LinesWritten >= TraceForm.MaxLines)
            {
               TraceForm.CurrentFileNumber++;
               TraceForm.LinesWritten = 0;  // reset counter
            }
         }
      }

      //------------------------------------------------------------------------------

      //static private void SendCallback (System.IAsyncResult ar)
      //{
      //    Socket so = (Socket) ar.AsyncState ;
      //    //so.EndSend (ar) ;
      //}

      //----------------------------------------------------------------------
      // Assembly private function

#if (!NETCF1  && !NETCF2  && !NETCF3 && !SILVERLIGHT)
      internal static int StartTDebug ()
      {

         int WinHandle ;
         WinHandle = Helper.FindWindow("TFormReceiver", "FormReceiver");
         if (WinHandle != 0)
         {
            //MessageBox.Show("StartTDebug : FindWindow : " + WinHandle.ToString());
            return WinHandle;
         }

         RegistryKey Reg = Registry.LocalMachine.OpenSubKey("Software\\TraceTool");
         if (Reg == null)
         {
            //MessageBox.Show("StartTDebug : reg == null");
            return 0;
         }

         String DebugFilename = (string) Reg.GetValue("FilePath");
         if (File.Exists(DebugFilename) == false)
         {
            //MessageBox.Show("StartTDebug : DebugFilename unknow");
            return 0;
         }

         System.Diagnostics.Process myProcess = new Process();
         myProcess.StartInfo.FileName = DebugFilename;
         myProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
         myProcess.Start();

         System.Threading.Thread.Sleep(2000);

         WinHandle = Helper.FindWindow("TFormReceiver", "FormReceiver");
         return WinHandle;
      }
#endif
      //----------------------------------------------------------------------

      // Formats a DateTime in the format "HH:mm:ss:SSS" for example, "15:49:37:459".
      // FormatDateWithoutMillis and FormatDate code are taken from Log4Net source code.

      /// Last stored time with precision up to the second.
      internal static long s_lastTimeToTheSecond = 0;

      /// Last stored time with precision up to the second, formatted as a string.
      private static StringBuilder s_lastTime = new StringBuilder();

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
         int temp ;
         if (Options.SendDate == true)
         {
            temp = dateToFormat.Year ;
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
         long currentTimeToTheSecond = (dateToFormat.Ticks - (dateToFormat.Ticks % TimeSpan.TicksPerSecond));

         // Compare this time with the stored last time
         if (s_lastTimeToTheSecond == currentTimeToTheSecond)
         {
            // If we are in the same second then append
            // the previously calculated time string
            buffer.Append(s_lastTime);
         }
         else
         {
            // We are in a new second.
            s_lastTimeToTheSecond = currentTimeToTheSecond;
            s_lastTime.Length = 0;

            // Calculate the new string for this second
            FormatDateWithoutMillis(dateToFormat, s_lastTime);
            buffer.Append(s_lastTime);
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
      public string Id ;
      public bool   IsMultiColTree ;
      public int    MainCol ;
      public string TitleList ;
      public string LogFileName ;
      public string LastLocalLogFileName;  // last opened file
      public int    LogFileType;
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
   public class TTraceOptions
   {
      /// <summary>
      /// Change SendMode to Mode.Socket to use it under ASP
      /// </summary>
      public SendMode SendMode = SendMode.WinMsg ;
      /// <summary>
      /// The Socket Host adress
      /// </summary>
      public string SocketHost ;
      /// <summary>
      /// The socket port
      /// </summary>
      public int SocketPort ;

      /// <summary>
      /// Indicate if the socket use the Udp protocol
      /// </summary>
      public bool SocketUdp;


      /// <summary>
      /// indicate if the reflection should display modifiers (public class,...)
      /// </summary>
      public bool SendModifiers = true ;            // ShowModifiers        = 1  ,
      /// <summary>
      /// indicate if the reflection should display class info (assembly,guid,...) and bases classes names
      /// </summary>
      public bool SendClassInfo = false ;           // ShowClassInfo        = 2  ,
      /// <summary>
      /// indicate if the reflection should display fields values
      /// </summary>
      public bool SendFields = true ;               // ShowFields           = 4  ,
      /// <summary>
      /// indicate if the reflection should display custom attributes
      /// </summary>
      public bool SendCustomAttributes = false ;    // ShowCustomAttributes = 8  ,
      /// <summary>
      /// indicate if the reflection should display non public (private and protected) members
      /// </summary>
      public bool SendNonPublic = false ;           // ShowNonPublic        = 16 ,
       /// <summary>
      /// indicate if the reflections should display inherited members
      /// </summary>
      public bool SendInherited = true ;            // ShowInheritedMembers = 32 ,
      /// <summary>
      /// indicate if the reflections should display the events
      /// </summary>
      public bool SendEvents = false ;              // ShowEvents           = 64 ,
      /// <summary>
      /// indicate if the reflection should display functions
      /// </summary>
      public bool SendFunctions = false ;           //  ShowMethods         = 128,
      /// <summary>
      /// indicate if the reflection should display documentation for type, fields, methods,..
      /// </summary>
      public bool SendDoc = false ;                 // ShowDoc              = 256

      /// <summary>
      /// indicate if the reflection should also display private members. Default is false
      /// </summary>
      public bool SendPrivate = false ;
      /// <summary>
      /// Max Object tree depth for SendValue and Watches
      /// </summary>
      public int ObjectTreeDepth = 3 ;
      /// <summary>
      /// Indicate if SendValue and AddValue function should display members type. Default is true
      /// </summary>
      public bool SendTypeWithValue = true;

      /// <summary>
      /// indicate if the process name must be send. Displayed on the status bar.
      /// </summary>
      public bool SendProcessName = false ;

      private bool _SendDate ;
      /// <summary>
      /// indicate if the date must be send with the time.
      /// </summary>
      public bool SendDate
      {
         get {return _SendDate ;}
         set
         {
            _SendDate = value ;
            TTrace.s_lastTimeToTheSecond = 0 ;
         }
      }

      /// <summary>
      /// indicate if the thread id must be send.
      /// </summary>
      public bool SendThreadId = true ;


      /// <summary>
      /// return default TraceDisplayFlags options if you don't supply one for the AddObject / AddType / SendObject / SendType functions .
      /// </summary>
      public TraceDisplayFlags GetDefault()
      {
         // display at least public (inherited) fields and properties
         // Privat members are discarded
         TraceDisplayFlags flags = 0 ;
          
         if (TTrace.Options.SendModifiers)
            flags = flags | TraceDisplayFlags.ShowModifiers ;           // 1
         if (TTrace.Options.SendClassInfo)
            flags = flags | TraceDisplayFlags.ShowClassInfo ;           // 2
         if (TTrace.Options.SendFields)
            flags = flags | TraceDisplayFlags.ShowFields ;              // 4
         if (TTrace.Options.SendCustomAttributes)
            flags = flags | TraceDisplayFlags.ShowCustomAttributes ;    // 8    
         if (TTrace.Options.SendNonPublic)
            flags = flags | TraceDisplayFlags.ShowNonPublic ;           // 16
         if (TTrace.Options.SendInherited)
            flags = flags | TraceDisplayFlags.ShowInheritedMembers ;    // 32   
         if (TTrace.Options.SendEvents)
            flags = flags | TraceDisplayFlags.ShowEvents ;              // 64
         if (TTrace.Options.SendFunctions)
            flags = flags | TraceDisplayFlags.ShowMethods ;             // 128
         if (TTrace.Options.SendDoc)
            flags = flags | TraceDisplayFlags.ShowDoc ;                 // 256
          
         return flags ;
      }
   }

   //------------------------------------------------------------------------------
   //------------------------------------------------------------------------------
   //------------------------------------------------------------------------------

#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)

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
      public Object Create (Object parent , Object configContext , System.Xml.XmlNode section )
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
   internal class TTraceWriter : TextWriter
   {
      public TTraceWriter()
      {
      }

      public override void Close()
      {
         this.Dispose(true);
      }

      protected override void Dispose(bool disposing)
      {
         base.Dispose(disposing);
      }

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

      //public override void Write(char[] buffer, int index, int count)
      //{
      //    //this._sb.Append(buffer, index, count);
      //}
      private static UnicodeEncoding m_encoding;

      public override Encoding Encoding
      {
         get
         {
            if (m_encoding == null)
            {
               m_encoding = new UnicodeEncoding(false, false);
            }
            return m_encoding;
         }
      }
   } // TTraceWriter
}
