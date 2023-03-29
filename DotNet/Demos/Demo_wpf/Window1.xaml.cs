using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using TraceTool;
// ReSharper disable CollectionNeverQueried.Global

namespace Wpf_Demo
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1
    {
        [DllImport("kernel32.dll")]
        extern static void OutputDebugString(String str);

        public static object ShowViewerButton;
        private TestClass _testClass = new TestClass();
        private int _test;
        private WinTrace _multiColTrace;
        private WinTrace _myWinTrace;
        private TestClass test2 = new TestClass();
        private WinWatch _myWinWatch;
        private TraceNode _start1;
        private TraceNode _start2;

        //------------------------------------------------------------------------------

        public Window1()
        {
            InitializeComponent();
            //TestClassHelper.SetEditMode(testClass,true) ;
        }

        //------------------------------------------------------------------------------

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            //comboBox1.SelectedIndex  = 0 ;
            if (TTrace.Options.SendMode == SendMode.WinMsg)
                ComboMode.SelectedIndex = 0;
            else
                ComboMode.SelectedIndex = 1;

            // Not needed. Config is read from tracetool.xml in the running folder   
            //TTrace.Options.SocketHost = "127.0.0.1" ;
            //TTrace.Options.SocketPort = 8090 ;
            SocketHost.Text = TTrace.Options.SocketHost ;
            SocketPort.Text = TTrace.Options.SocketPort.ToString();

            ChkSendEvents.IsChecked = TTrace.Options.SendEvents;
            ChkSendInherited.IsChecked = TTrace.Options.SendInherited;
            ChkSendFunctions.IsChecked = TTrace.Options.SendFunctions;
            ChkSendProcessName.IsChecked = TTrace.Options.SendProcessName;

            UseWorkerThread.IsChecked = true;  // TODO : TTrace.Options.UseWorkerThread
        }

        //------------------------------------------------------------------------------

        private void Window_Closed(object sender, EventArgs e)
        {
            TTrace.Stop();  // ensure trace sub-system is closed. You may also call TTrace.Flush() or FlushAsync() before stopping it.
        }

        //------------------------------------------------------------------------------

        private void butShowtrace_Click(object sender, RoutedEventArgs e)
        {
            TTrace.Show(true);
        }

        //------------------------------------------------------------------------------

        private void butCloseViewer_Click(object sender, RoutedEventArgs e)
        {
            TTrace.CloseViewer();
        }

        //------------------------------------------------------------------------------

        private void comboMode_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (ComboMode.SelectedIndex == 0)                   // windows message
                TTrace.Options.SendMode = SendMode.WinMsg;
            else if (ComboMode.SelectedIndex == 1)              // Socket TCP
            {
                TTrace.Options.SocketUdp = false;
                TTrace.Options.SendMode = SendMode.Socket;
            }
            else if (ComboMode.SelectedIndex == 2)              // Socket UDP
            {
                TTrace.Options.SocketUdp = true;
                TTrace.Options.SendMode = SendMode.Socket;
            }
            else if (ComboMode.SelectedIndex == 3)              // Web socket
            {
                TTrace.Options.SocketUdp = false;
                TTrace.Options.SendMode = SendMode.WebSocket;
                TTrace.Options.SocketPort = 8091 ;
                SocketPort.Text = "8091";
                UseAsync.IsChecked = true;
            }
            else                                                // None
                TTrace.Options.SendMode = SendMode.None;
        }

        //------------------------------------------------------------------------------

        private void chkSendFunctions_Checked(object sender, RoutedEventArgs e)
        {
            if (ChkSendFunctions.IsChecked != null)
                TTrace.Options.SendFunctions = (bool)ChkSendFunctions.IsChecked;
        }

        //------------------------------------------------------------------------------

        private void chkSendInherited_Checked(object sender, RoutedEventArgs e)
        {
            if (ChkSendInherited.IsChecked != null)
                TTrace.Options.SendInherited = (bool)ChkSendInherited.IsChecked;
        }

        //------------------------------------------------------------------------------

        private void chkSendEvents_Checked(object sender, RoutedEventArgs e)
        {
            if (ChkSendEvents.IsChecked != null)
                TTrace.Options.SendEvents = (bool)ChkSendEvents.IsChecked;
        }

        //------------------------------------------------------------------------------

        private void chkSendProcessName_Checked(object sender, RoutedEventArgs e)
        {
            if (ChkSendProcessName.IsChecked != null)
                TTrace.Options.SendProcessName = (bool)ChkSendProcessName.IsChecked;
        }

        //------------------------------------------------------------------------------


        struct StructTest
        {

        }

        public class TemplateTest<T1, T2>
           where T2 : struct
           where T1 : Control, IFrameworkInputElement
        {
            private T1 _item;
            private T2 _aStruct;
            public TU Fct1<TU>(TU param) where TU : class { return param; } // generic method
            public void SetItem(T1 param) { _item = param; }        // normal method that use a generic parameter in a generic class
            public T1 GetItem(int param) { return _item; }
            public T2 GetStruct() { return _aStruct; }
            public void SetStruct(T2 param) { _aStruct = param; }

        }

        private async void butTrace_Click(object sender, RoutedEventArgs e)
        {
            // specify what to send (modifiers, fields, ...). Can be slow on complexe objects
            TraceDisplayFlags flags = TraceDisplayFlags.ShowModifiers |
                                       TraceDisplayFlags.ShowInheritedMembers |
                                       TraceDisplayFlags.ShowNonPublic |
                                       TraceDisplayFlags.ShowFields;

            if (ChkSendFunctions.IsChecked != null && (bool)ChkSendFunctions.IsChecked)
                flags |= TraceDisplayFlags.ShowMethods;


            //TTrace.Debug.SendObject("button1", button1);
            //button1.Width = button1.Width + 10;
            //button1.SetValue(Canvas.LeftProperty, (double)button1.GetValue(Canvas.LeftProperty) + 1); 
            if (ChkSendProcessName.IsChecked != null)
                TTrace.Options.SendProcessName = (bool)ChkSendProcessName.IsChecked;
            string str = '\u2250' + "qwerty & @ € é ù è azerty" + '\u9999';

            // simple traces
            //--------------------------------------------
            TTrace.Debug.Send("Hello").Send("World");  // "World" is a sub trace of "Hello"

            // single separator
            TTrace.Debug.Send("---");

            // send traces with special font style (bold and Italic), color font size and font name
            // use System.Drawing.Color.ToArgb() or (int)Helper.ToArgb(System.Windows.Media.Color) to specify Argb color
            TTrace.Debug.Send("Special font", "Symbol 12")
                .SetFontDetail(-1, false, true)                                        // set whole line to italic 
                .SetFontDetail(3, true, false, System.Drawing.Color.Red.ToArgb())                      // set col 3 (Left Msg)  to bold and Red
                .SetFontDetail(4, false, false, System.Drawing.Color.Green.ToArgb(), 12, "Symbol");      // set col 4 (Right Msg) to Green , font size 12 , Symbol
            TTrace.Debug.Send("Impact Italic")
                .SetFontDetail(3, false, true, System.Drawing.Color.BlueViolet.ToArgb(), 12, "Impact");     // Col3 (left msg), non bold, Italic , Blue-Violet , font 12 , Impact

            TTrace.Debug.Send("Special chars", "€ é ù è $ ");

            // The characters to encode:
            //    Latin Small Letter Z (U+007A)
            //    Latin Small Letter A (U+0061)
            //    Combining Breve (U+0306)
            //    Latin Small Letter AE With Acute (U+01FD)
            //    Greek Small Letter Beta (U+03B2)
            //    a high-surrogate value (U+D8FF)
            //    a low-surrogate value (U+DCFF)
            //char[] myChars = new char[] { 'z', 'a', '\u0306', '\u01FD', '\u03B2', '\uD8FF', '\uDCFF' };
            //TTrace.Debug.Send("Other Special chars", new StringBuilder().Append(myChars).ToString());  // note that myChars.ToString() return  "char[]"

            // double separator
            TTrace.Debug.Send("===");

            //TTrace.Options.SendThreadId = false ;
            //TTrace.Debug.Send("trace without thread id");
            //TTrace.Options.SendThreadId = true;

            //TTrace.Options.SendDate = true;
            //TTrace.Debug.Send("trace with date");
            //TTrace.Options.SendDate = false;

            // traces using Sendxxx method
            //--------------------------------------------
            // Use default display filter. (see TTrace.Options)

            TTrace.Debug.SendType("Object base type", typeof(Object));
            TTrace.Debug.SendType("My interface", typeof(IMyinterface));
            TTrace.Debug.SendObject("My const", TraceConst.CST_CREATE_MEMBER);
            TTrace.Debug.SendObject("My enum", _testClass.FieldDay);
            TTrace.Debug.SendCaller("SendCaller test", 0);
            TTrace.Debug.SendStack("Stack test", 0);
            TTrace.Debug.SendDump("SendDump test", "Unicode", Encoding.Unicode.GetBytes(str), 50);

            TTrace.Warning.SendType("SendType 'testClass'", _testClass.GetType());

            TTrace.Error.SendObject("SendObject 'testClass'", _testClass, flags);

            // traces using TraceNodeEx
            //--------------------------------------------
            TraceNodeEx node = new TraceNodeEx(null);  //  TTrace.Debug
            node.LeftMsg = "TraceNodeEx";
            node.RightMsg = str;
            node.AddFontDetail(3, false, false, System.Drawing.Color.Green.ToArgb());
            node.IconIndex = 8;
            node.Members.Add("My Members", "col2", "col3")
               .SetFontDetail(0, true)                                 // set first column to bold
               .SetFontDetail(1, false, false, System.Drawing.Color.Green.ToArgb())   // set second column to green
               .Add("Sub members")                                     // add sub member node
                  .SetFontDetail(0, false, true);                      // set first column to Italic
            node.AddDump("ASCII", Encoding.ASCII.GetBytes(str), 50);   // 3F 61 7A          ..... 3F
            node.AddDump("UTF8", Encoding.UTF8.GetBytes(str), 50);
            node.AddDump("Unicode", Encoding.Unicode.GetBytes(str), 50); // 50 22 61 00 7A 00 ..... 99 99
            node.AddStackTrace(0);
            node.AddCaller();
            TraceNode sendNode = node.Send();
            sendNode.ResendIconIndex(5);  // change icon index after the node is send

            // XML sample using Send
            //--------------------------------------------
            TTrace.Debug.SendXml("xml", "<?xml version='1.0' ?><Data> Hello XML </Data>");

            // Image sample using Send
            //--------------------------------------------

            //TTrace.Debug.SendBitmap("Bitmap", Image1);

            // Text, image and XML together
            //--------------------------------------------
            node = new TraceNodeEx(TTrace.Debug);
            node.LeftMsg = "Text, image and XML together";
            node.Members.Add("Text displayed in detail");
            //node.AddBitmap(Image1);
            node.AddXML("<?xml version='1.0' ?><Data> Xml in traceNodeEx </Data>");
            node.Send();

            // group of traces enabled / disabled
            //------------------------------------
            TraceNode groupTrace = new TraceNode(null, false);  // dummy node not send to viewer
            groupTrace.IconIndex = 5;
            groupTrace.Enabled = true;
            groupTrace.Send("GroupTrace traces 1");    // send to viewer
            groupTrace.Enabled = false;
            groupTrace.Send("GroupTrace traces 2");    // not send : group not enabled

            // generics
            //------------------------------------
            TTrace.Debug.SendType("typeof(Dictionary<,>)", typeof(Dictionary<,>), flags);   // open generic
            TTrace.Debug.SendType("typeof(Dictionary<Window, structTest>", typeof(Dictionary<Window, StructTest>), flags);// closed generic
            TTrace.Debug.SendObject("new Dictionary<Window, structTest>()", new Dictionary<Window, StructTest>(), flags);

            TTrace.Debug.SendType("typeof(templateTest<,>)", typeof(TemplateTest<,>), flags);
            TTrace.Debug.SendObject("new templateTest<Window, structTest>()", new TemplateTest<Window, StructTest>(), flags);


            // Display tables : use an object that implement : Array or IEnumerable or IDictionary or create a special table
            //--------------------------------------------------------------------------------------------------------------

            // 1) array of int
            int[] numbersA = { 0, 2, 4, 5, 6, 8, 9 };
            int[] numbersB = { 1, 3, 5, 7, 8 };
            TTrace.Debug.SendTable("numbersA", numbersA);
            TTrace.Debug.SendTable("numbersB", numbersB);

            // Linq on array
            var pairs =
               from a in numbersA
               from b in numbersB
               where a < b
               select new { a, b };
            TTrace.Debug.SendTable("Linq query on series", pairs);

            // 2) array of FileInfo[]
            string strTempPath = Path.GetTempPath();
            DirectoryInfo tempPath = new DirectoryInfo(strTempPath);
            TTrace.Debug.SendTable("Files in temp path", tempPath.GetFiles());

            // Linq to array of FileInfo[]
            var linqToObjectQuery = from file in tempPath.GetFiles()
                                    where file.Length > 100
                                    orderby file.Length descending
                                    select new
                                    {
                                        file.Name,
                                        file.Length,
                                        file.LastWriteTime,
                                        file
                                    };
            TTrace.Debug.SendTable("Files having length>100 in temp path (Linq)", linqToObjectQuery);


            // 3) IDictionary : Hashtable
            Hashtable openWith = new Hashtable();
            openWith.Add("txt", "notepad.exe");
            openWith.Add("bmp", "paint.exe");
            openWith.Add("dib", "paint.exe");
            openWith.Add("rtf", "wordpad.exe");
            TTrace.Debug.SendTable("Hashtable", openWith);

            // 4) UnTyped collection : Stack
            Stack windowStack = new Stack(5);
            windowStack.Push(this);
            windowStack.Push(this);
            windowStack.Push(this);
            TTrace.Debug.SendTable("Stack ", windowStack);

            // 5) Typed collection (implement ICollection)
            Collection<Window> windowsCollection = new Collection<Window>();
            for (int c = 0; c < 500; c++)
                windowsCollection.Add(this);
            TTrace.Debug.SendTable("window Collection", windowsCollection);

            Collection<int> intCollection = new Collection<int>();
            for (int c = 0; c < 500; c++)
                intCollection.Add(c);
            TTrace.Debug.SendTable("int Collection", intCollection);

            // 6) create manually a table 
            TraceTable table = new TraceTable();

            // add titles. Individual columns titles can be added or multiple columns , separated by tabs
            table.AddColumnTitle("colA");          // first column title
            table.AddColumnTitle("colB");          // second column title
            table.AddColumnTitle("title column C\tcolD");  // other columns title (tab separated)

            // add first line. Individual columns data can be added or multiple columns , separated by tabs
            table.AddRow();
            table.AddRowData("a");                           // add first col
            table.AddRowData("b" + "\t" + "c" + "\t" + "d" + "\t" + "e");            // then add other columns (tab separated)

            // add second line
            table.AddRow();
            table.AddRowData("aa" + "\t" + "data second column" + "\t" + "cc" + "\t" + "dd" + "\t" + "ee");  // add all columns data in a single step (tab separated)

            // finally send the table
            TTrace.Debug.SendTable("Mytable", table);


            // ensure all traces are send to the viewer
            if (TTrace.Options.UseWorkerThread)
                TTrace.Flush();                 // blocking
            else
                await TTrace.FlushAsync();      // blocking
        }

        //------------------------------------------------------------------------------


        private void butSearch_Click(object sender, RoutedEventArgs e)
        {
            // ttrace.WinTrace.GotoBookmark(1); // second bookmark, noted [1]
            // ttrace.WinTrace.clearBookmark() ;
            // ttrace.WinTrace.GotoFirstNode() ;
            // ttrace.WinTrace.GotoLastNode() ;

            // TTrace.Find just set the criterias and hightlight if asked, but don't move to the next matching node.
            TTrace.Find("StRinG", false, true, true, true);// {Sensitive}{WholeWord}{Highlight}{SearchInAllPages}

            // from the current node : go to the next item matching criteria. Call ttrace.WinTrace.GotoFirstNode() before FindNext to start search from first node
            TTrace.WinTrace.FindNext(true); //{SearForward}
        }

        //------------------------------------------------------------------------------

        private void butFilter_Click(object sender, RoutedEventArgs e)
        {
            TTrace.WinTrace.ClearFilter();

            // 5 kinds of filters
            // -------------------
            //Equal = 0
            //Not equal = 1
            //contains = 2
            //Don't contains = 3
            //(Ignore this filter) = 4 or -1

            // filters can be applied on all columns. Note that the "icone" column is not zero but 999. The members are identified by the column 998
            // On multicolumn mode. Column 0 can be used normally.

            TTrace.WinTrace.AddFilter(/*col icone*/   999,/*Equal*/                 0, "24");           // 999 : Icon column . Filter on "info" (index is 24)
            TTrace.WinTrace.AddFilter(/*col time */     1,/*Not equal*/             1, "string");
            TTrace.WinTrace.AddFilter(/*col thread*/    2,/*contains*/             2, "0x");
            TTrace.WinTrace.AddFilter(/*col traces*/    3,/*Don't contains*/       3, "nothing");
            TTrace.WinTrace.AddFilter(/*col Comment*/   4,/*(Ignore this filter)*/ -1, "string");       // -1 or 4 can be used to disable this filter (not very usefull...)
            TTrace.WinTrace.AddFilter(/*col members*/ 998,/*contains*/             2, "string");       // members info : 998

            TTrace.WinTrace.ApplyFilter(/*{ConditionAnd*/ true, /*ShowMatch*/ true, /*IncludeChildren*/ true);

        }

        //------------------------------------------------------------------------------

        private void butClearFilter_Click(object sender, RoutedEventArgs e)
        {
            TTrace.WinTrace.ClearFilter();
        }

        //------------------------------------------------------------------------------

        private void IndentButton_Click(object sender, RoutedEventArgs e)
        {
            TraceNode node = TTrace.Debug.Send("Tree indentation using Indent and UnIndent methods");

            node.Indent("Indent", "level 1");
            node.Send("Node1");
            node.Indent("Indent level 2");
            node.Send("Node2");

            // UnIndent with no title
            node.Indent("Indent level 3");
            node.Send("Node3");
            node.UnIndent();   // UnIndent without title

            node.Send("Node4");

            node.UnIndent("UnIndent level 2");
            node.UnIndent("UnIndent level 1");

            // node indentation using traceNodeEx
            TTrace.Debug.Send("root 1", TTrace.Debug.IndentLevel.ToString());
            TTrace.Debug.Indent("start indentation");
            TTrace.Debug.Send("under indent 1", TTrace.Debug.IndentLevel.ToString());
            TraceNodeEx nodeEx = new TraceNodeEx(TTrace.Debug);   // Parent depends of the indentation
            nodeEx.LeftMsg = "under indent 2";
            nodeEx.Send();
            TTrace.Debug.UnIndent();
            TTrace.Debug.Send("root 2", TTrace.Debug.IndentLevel.ToString());
        }

        //------------------------------------------------------------------------------

        private void butVariant_Click(object sender, RoutedEventArgs e)
        {
            if (ChkSendProcessName.IsChecked != null)
                TTrace.Options.SendProcessName = (bool)ChkSendProcessName.IsChecked;

            // SendValue is quite different from SendObject 
            // SendValue is recursive and display arrays
            TTrace.Debug.SendValue("SendValue 'testClass'", _testClass);

            TTrace.Debug.SendValue("SendValue 'butVariant'", ButVariant);
            TTrace.Warning.SendValue("SendValue 'testClass', max 3 levels (default)", _testClass, true, 3, "testClass");
            TTrace.Error.SendValue("SendValue 'array', max 5 levels", _testClass.FieldArray, true, 5, "fieldArray");
        }

        //------------------------------------------------------------------------------

        private void butFullInfo_Click(object sender, RoutedEventArgs e)
        {
            // display all informations relative to that form. (slow traces : too much informations to send)
            // Note that the ShowDoc flag don't work here, 
            // because the this object is a "WinForm" type with no documentation
            TTrace.Debug.SendObject("this object (full display)", this,
               TraceDisplayFlags.ShowModifiers |
               TraceDisplayFlags.ShowFields |
               TraceDisplayFlags.ShowClassInfo |
               TraceDisplayFlags.ShowCustomAttributes |
               TraceDisplayFlags.ShowNonPublic |
               TraceDisplayFlags.ShowInheritedMembers |
               TraceDisplayFlags.ShowEvents |
               TraceDisplayFlags.ShowDoc |
               TraceDisplayFlags.ShowMethods);
        }

        //------------------------------------------------------------------------------

        private void butDoc_Click(object sender, RoutedEventArgs e)
        {
            // display a System.Windows.Forms.Button object with his documentation
            TTrace.Debug.SendObject("butDoc object", ButDoc,
               TraceDisplayFlags.ShowModifiers |
               TraceDisplayFlags.ShowFields |
               TraceDisplayFlags.ShowClassInfo |
               TraceDisplayFlags.ShowCustomAttributes |
               TraceDisplayFlags.ShowNonPublic |
               TraceDisplayFlags.ShowInheritedMembers |
               TraceDisplayFlags.ShowEvents |
               TraceDisplayFlags.ShowDoc |
               TraceDisplayFlags.ShowMethods);
        }

        //------------------------------------------------------------------------------

        private void butSaveToTXT_Click(object sender, RoutedEventArgs e)
        {
            TTrace.WinTrace.SaveToTextfile("c:\\temp\\log.txt");
        }

        //------------------------------------------------------------------------------

        private void butSaveToXml_Click(object sender, RoutedEventArgs e)
        {
            TTrace.WinTrace.SaveToXml("c:\\temp\\log.xml");
            TTrace.WinTrace.SaveToXml("c:\\temp\\log.xml", "tracetool.xsl");
        }

        //------------------------------------------------------------------------------

        private void butLoadXml_Click(object sender, RoutedEventArgs e)
        {
            TTrace.WinTrace.LoadXml("c:\\temp\\log.xml");
        }

        //------------------------------------------------------------------------------

        private void butLogFile_Click(object sender, RoutedEventArgs e)
        {
            // set viewer log
            // 0, Viewer Log is disabled.
            // 1, Viewer log enabled. 
            // 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
            TTrace.WinTrace.SetLogFile("c:\\temp\\logFromViewer.xml", 1);
            // set local log
            // 3, Local log is disabled
            // 4, Local log enabled. No size limit.
            // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
            TTrace.WinTrace.SetLogFile("c:\\temp\\LogFromDotNetApi.xml", 5);
        }

        //------------------------------------------------------------------------------

        private void butClear_Click(object sender, RoutedEventArgs e)
        {
            TTrace.ClearAll();
        }

        //------------------------------------------------------------------------------

        private void butDiagnostic_Click(object sender, RoutedEventArgs e)
        {
            _test++;
            Trace.WriteLine("Diagnostics test" + _test);
        }

        //------------------------------------------------------------------------------

        private void butOldTrace_Click(object sender, RoutedEventArgs e)
        {
            _test++;
            OutputDebugString("OutputDebugString test" + _test);
        }

        //------------------------------------------------------------------------------

        private void butEventLog_Click(object sender, RoutedEventArgs e)
        {
            EventLog myLog = new EventLog();
            myLog.Source = "Application";

            _test++;

            // Write an informational entry to the event log.    
            myLog.WriteEntry("Event log test " + _test);
        }

        //------------------------------------------------------------------------------

        private void butTail_Click(object sender, RoutedEventArgs e)
        {
            FileStream fsFileStream = new FileStream("c:\\temp\\log.txt", FileMode.Append);
            StreamWriter swWriter = new StreamWriter(fsFileStream);
            _test++;
            swWriter.WriteLine("test " + _test);
            swWriter.Close();
            fsFileStream.Close();
        }

        //------------------------------------------------------------------------------

        private void butCreateWinTrace_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace = new WinTrace("MyWINID", "My trace window");
            ButDisplayWin.IsEnabled = true;
            ButHelloToWintrace.IsEnabled = true;
            ButSaveWinToTxt.IsEnabled = true;
            ButSaveWinToXml.IsEnabled = true;
            ButClearWin.IsEnabled = true;
            ButWinLoadXml.IsEnabled = true;
            ButCloseWin.IsEnabled = true;
            ButSetLocalLog.IsEnabled = true;
        }

        //------------------------------------------------------------------------------

        private void butDisplayWin_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.DisplayWin();
        }

        //------------------------------------------------------------------------------

        private void butHelloToWintrace_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.Debug.Send("Hello", "Can be used to store exceptions, for examples");
        }

        //------------------------------------------------------------------------------

        private void MulticolBut_Click(object sender, RoutedEventArgs e)
        {
            if (_multiColTrace == null)
            {
                _multiColTrace = new WinTrace("MCOL", "MultiCol trace window");
                _multiColTrace.SetMultiColumn(1);  // must be called before calling setColumnsTitle
                _multiColTrace.SetColumnsTitle("col1 \t col2 \t col3");
                _multiColTrace.SetColumnsWidth("100:20:80 \t 200:50 \t 100");
                _multiColTrace.DisplayWin();
                // set local log
                // 3, Local log is disabled
                // 4, Local log enabled. No size limit.
                // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
                _multiColTrace.SetLogFile("c:\\MultiCol.xml", 4);
            }
            _multiColTrace.Debug.Send("1 \t 2 \t 3");
        }

        //------------------------------------------------------------------------------

        private void butSetLocalLog_Click(object sender, RoutedEventArgs e)
        {
            // set local log
            // 3, Local log is disabled
            // 4, Local log enabled. No size limit.
            // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
            _myWinTrace.SetLogFile("c:\\temp\\myWinTrace.xml", 4);
        }

        //------------------------------------------------------------------------------

        private void butSaveWinToTxt_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.SaveToTextfile("c:\\temp\\log2.txt");
        }

        //------------------------------------------------------------------------------

        private void butClearWin_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.ClearAll();
        }

        //------------------------------------------------------------------------------

        private void butSaveWinToXml_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.SaveToXml("c:\\temp\\log2.xml");
        }

        //------------------------------------------------------------------------------

        private void butWinLoadXml_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.LoadXml("c:\\log2.xml");
        }

        //------------------------------------------------------------------------------

        private void butCloseWin_Click(object sender, RoutedEventArgs e)
        {
            _myWinTrace.Close();
        }

        //------------------------------------------------------------------------------

        private void butWatch_Click(object sender, RoutedEventArgs e)
        {
            TTrace.Watches.Send("test2", test2);
        }

        //------------------------------------------------------------------------------

        private void butClearWatchWindow_Click(object sender, RoutedEventArgs e)
        {
            _myWinWatch = new WinWatch("MyWinWatchID", "My watches");
        }

        //------------------------------------------------------------------------------

        private void butDisplayWatchWindow_Click(object sender, RoutedEventArgs e)
        {
            TTrace.Watches.DisplayWin();
        }

        //------------------------------------------------------------------------------

        private void butCreateWinWatch_Click(object sender, RoutedEventArgs e)
        {
            _myWinWatch = new WinWatch("MyWinWatchID", "My watches");
        }

        //------------------------------------------------------------------------------

        private void butWinWatchSend_Click(object sender, RoutedEventArgs e)
        {
            if (_myWinWatch != null)
                _myWinWatch.Send("Now", DateTime.Now.ToString("HH:mm:ss:fff"));
        }

        //------------------------------------------------------------------------------

        private void butWinWatchClear_Click(object sender, RoutedEventArgs e)
        {
            if (_myWinWatch != null)
                _myWinWatch.ClearAll();
        }

        //------------------------------------------------------------------------------

        private void butWinWatchDisplay_Click(object sender, RoutedEventArgs e)
        {
            if (_myWinWatch != null)
                _myWinWatch.DisplayWin();
        }

        //------------------------------------------------------------------------------

        private void butWinWatchClose_Click(object sender, RoutedEventArgs e)
        {
            if (_myWinWatch != null)
                _myWinWatch.Close();
        }

        //------------------------------------------------------------------------------

        private void butStart1_Click(object sender, RoutedEventArgs e)
        {
            _start1 = TTrace.Debug.Send("Start 1 ..");
            ButDone1.IsEnabled = true;
            ButSetSelected.IsEnabled = true;
        }

        //------------------------------------------------------------------------------

        private void butDone1_Click(object sender, RoutedEventArgs e)
        {
            //start1.ResendRight("Done 1");
            //start1.AppendStack();

            // same creating a duplicate node with same id
            TraceNode newNode = new TraceNode(null, false);
            newNode.Id = _start1.Id;
            newNode.ResendRight("Done 1");
            newNode.AppendStack();

        }

        //------------------------------------------------------------------------------

        private void butSetSelected_Click(object sender, RoutedEventArgs e)
        {
            _start1.SetSelected();
        }

        //------------------------------------------------------------------------------

        private void butstart2_Click(object sender, RoutedEventArgs e)
        {
            _start2 = TTrace.Debug.Send("Start 2 ..");
            ButEnd2.IsEnabled = true;
            ButShowNode.IsEnabled = true;
            ButToogleBookmark.IsEnabled = true;
            ButToogleVisible.IsEnabled = true;
        }

        //------------------------------------------------------------------------------

        private void butEnd2_Click(object sender, RoutedEventArgs e)
        {
            _start2.AppendLeft("Done 2");
        }

        //------------------------------------------------------------------------------

        private void butShowNode_Click(object sender, RoutedEventArgs e)
        {
            _start2.Show();
        }

        //------------------------------------------------------------------------------

        private bool _lastToggleBookmark = true;
        private void butToogleBookmark_Click(object sender, RoutedEventArgs e)
        {
            _start2.SetBookmark(_lastToggleBookmark);
            _lastToggleBookmark = !_lastToggleBookmark;
        }

        //------------------------------------------------------------------------------

        private bool _lastToggleVisible;
        private void butToogleVisible_Click(object sender, RoutedEventArgs e)
        {
            _start2.SetVisible(_lastToggleVisible);
            _lastToggleVisible = !_lastToggleVisible;
        }
        //------------------------------------------------------------------------------

        private void UseWorkerThread_Checked(object sender, RoutedEventArgs e)
        {
            TTrace.Options.UseWorkerThread = true; // sync , default
        }

        //------------------------------------------------------------------------------

        private void UseAsync_Checked(object sender, RoutedEventArgs e)
        {
            TTrace.Options.UseWorkerThread = false; // async
        }

        private void SocketHost_LostFocus(object sender, RoutedEventArgs e)
        {
            TTrace.Options.SocketHost = SocketHost.Text ;
        }

        private void SocketPort_LostFocus(object sender, RoutedEventArgs e)
        {
            int port ;
            int.TryParse(SocketPort.Text,out port);
            TTrace.Options.SocketPort = port ;
        }

        //------------------------------------------------------------------------------

        //private void button2_Click(object sender, RoutedEventArgs e)
        //{
        //if (button1.Parent == gridFill)
        //{
        //   gridFill.Children.Remove(button1);
        //   dockPanelLeft.Children.Add(button1);
        //}
        //else if (button1.Parent == dockPanelLeft)
        //{
        //   dockPanelLeft.Children.Remove(button1);
        //   stackPanelTop.Children.Add(button1);
        //}
        //else if (button1.Parent == stackPanelTop)
        //{
        //   stackPanelTop.Children.Remove(button1);
        //   canvasBottom.Children.Add(button1);
        //} else if (button1.Parent == canvasBottom)
        //{
        //   canvasBottom.Children.Remove(button1);
        //   gridFill.Children.Add(button1);
        //}
        // 
        //button1.SetValue(Canvas.LeftProperty, (double)0);
        //}

    }

    #region Test Classes

    public class TestClassHelper
    {
        public static readonly DependencyProperty EditModeProperty = DependencyProperty.RegisterAttached(
            "EditMode",                                                       // Property name
            typeof(bool?),                                                    // Property type. Not garanted to be set to true or false
            typeof(TestClassHelper),                                          // Owner type. Where is defined the getter and setter
            new PropertyMetadata(false, OnEditModeChanged));                   // Property change callback

        // getter
        public static bool? GetEditMode(DependencyObject obj)
        {
            return (bool?)obj.GetValue(EditModeProperty);
        }

        // setter 
        public static void SetEditMode(DependencyObject obj, bool? value)
        {
            obj.SetValue(EditModeProperty, value);
        }

        private static void OnEditModeChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var newValue = (bool)e.NewValue;
            TestClass testClass = sender as TestClass;
            if (testClass != null)
                testClass.EditMode = newValue;
        }
    }

    public class TestClass : DependencyObject
    {
        public enum Days { Sat = 1, Sun, Mon, Tue, Wed, Thu, Fri };

        // sample Application variables.
        public readonly Days FieldDay;
        public readonly Array FieldArray;
        public readonly Hashtable FieldHash;
        public readonly ArrayList FieldArrayList;
        // ReSharper disable once PrivateFieldCanBeConvertedToLocalVariable
        private readonly Child _fieldClassInstance;
        public object PropButton { get { return Window1.ShowViewerButton; } }
        public Child MyNullChild;

        public bool EditMode { get; set; }
        public bool Edit2Mode { get; set; }
        public new string ToString() { return "BlaBla"; }
        public TestClass()
        {
            // initialize demo vars.

            FieldDay = Days.Tue;

            _fieldClassInstance = new Child(5);
            _fieldClassInstance.FctChild1(new[] { 1, 2, 3 });   // accept only  arrays
            _fieldClassInstance.FctChild3(1, 2, 3);                // parameters are converted to array
            _fieldClassInstance.FctChild3(new int[] { 1, 2, 3 });   // also accept array

            FieldHash = new Hashtable();
            FieldHash.Add("Tuesday", FieldDay);

            FieldArrayList = new ArrayList();
            FieldArrayList.Add(FieldDay);

            // init bound array 
            int[] myLengthsArray = new int[] { 1, 2, 5 };     // 1 by 2 by 5
            int[] myBoundsArray = new int[] { 2, 3, 8 };     // 2..4 , 3..4 , 8..12
            FieldArray = Array.CreateInstance(typeof(Object), myLengthsArray, myBoundsArray);

            // sub array
            int[] myIndArray = new int[3];
            myIndArray[0] = 2;
            myIndArray[1] = 4;
            myIndArray[2] = 6;

            FieldArray.SetValue(238, new[] { 2, 3, 8 });
            FieldArray.SetValue("239", new[] { 2, 3, 9 });
            FieldArray.SetValue(DateTime.Now, new[] { 2, 4, 8 });
            FieldArray.SetValue(_fieldClassInstance, new[] { 2, 4, 9 });
            FieldArray.SetValue(myIndArray, new[] { 2, 4, 10 });

            TestClassHelper.SetEditMode(this, true);       // attached
            SetEdit2Mode(this, true);                      // not attached
        }

        public static readonly DependencyProperty Edit2ModeProperty = DependencyProperty.Register(
             "Edit2Mode",                                                    // Property name
             typeof(bool),                                                    // Property type. Not garanted to be set to true or false
             typeof(TestClass),                                              // Owner type. Where is defined the getter and setter
             new PropertyMetadata(false, OnEdit2ModeChanged));                   // Property change callback

        // getter
        public static bool GetEdit2Mode(DependencyObject obj)
        {
            var value = obj.GetValue(Edit2ModeProperty);
            return value != null && (bool)value;
        }

        // setter 
        public static void SetEdit2Mode(DependencyObject obj, bool value)
        {
            obj.SetValue(Edit2ModeProperty, value);
        }

        private static void OnEdit2ModeChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var newValue = (bool)e.NewValue;
            TestClass testClass = sender as TestClass;
            if (testClass != null)
                testClass.Edit2Mode = newValue;
        }

    }

    //---------------------------------------------------------------------------

    public struct MyStruct
    {
        // ReSharper disable once NotAccessedField.Local
        private double _k;
        public MyStruct(int i) { _k = 200 * i; }
        public long L { get { return 6; } }
    }

    //---------------------------------------------------------------------------

    [AttributeUsage(AttributeTargets.All, AllowMultiple = true)]
    public class TestAttribute : Attribute
    {
        // ReSharper disable once NotAccessedField.Local
        string _value;
        public TestAttribute(string value)
        {
            _value = value;
        }
    }

    //---------------------------------------------------------------------------

    public interface IMyinterface : IMyinterface3
    {
        void FctInterface();
    }

    //---------------------------------------------------------------------------

    public interface IMyinterface2
    {
        void FctInterface2();
    }

    //---------------------------------------------------------------------------

    public interface IMyinterface3
    {
        void FctInterface3();
    }

    //---------------------------------------------------------------------------

    public class Mere
    {
        public virtual int FctMere() { return 1; }
    }

    //---------------------------------------------------------------------------

    [DefaultMember("fctBase1")]
    public abstract class Base : Mere, IMyinterface2
    {
        // ReSharper disable NotAccessedField.Local
        // ReSharper disable UnusedParameter.Local
        public int FldPublicFromBase50 = 50;
        private int _fldPrivateFromBase51 = 51;
        protected int FldProtectedFromBase52 = 52;
        public virtual void Fct_FromBase() { _fldPrivateFromBase51++; }
        public static string PropPublicstaticStr { get { return "5"; } }
        public virtual long PropPublicVirtualLong { get { return 5; } }
        public virtual int PropPublicVirtualInt { get { return 5; } }
        public abstract int PropAbstract { get; }
        protected Base(char c) { }
        protected Base(float f) { }
        protected Base() { }  // no argument, used by derived class

        public override sealed int FctMere() { return 2; }
        public abstract void FctBase1();    // redefined in class child
        public virtual void FctBase2() { }
        public void FctInterface2() { }
        public void BaseFctnonOverridable() { }
        // ReSharper restore UnusedParameter.Local
        // ReSharper restore NotAccessedField.Local
    }


    //---------------------------------------------------------------------------

    public class TestRecur
    {
        public int Field1
        {
            get
            {
                // the get can be called by TTrace when inspecting instance.
                // SendObject block possible recursive call.
                TTrace.Debug.SendObject("field1 GET", this);
                return 10;
            }
        }
    }

    [Test("my class attrib")]      // custom attribute on the class
    [DefaultMember("Event1")]
    public class Child : Base, IMyinterface
    {
        // inner class
        //-----------------
        public class Mysubclass
        {
            public int Myfield;
        }

        // return an inner class
        public Mysubclass Createsubclass() { return new Mysubclass(); }


        // FIELDS
        //-----------------

        //      [Test("my field attrib 1")]      // custom attribute on a field
        //      private double          Fld_PrivateDouble1 = 1;
        //      protected float         Fld_ProtectedFloat2 = 2F;
        //      protected internal int  Fld_ProtectedInternal3 = 3 ;
        //      internal int            Fld_Internal4 = 4 ;
        //      public string           Fld_PublicString5="5";
        //      public MyStruct         Fld_PublicStruct6 = new MyStruct(2);
        //      int                     Fld_Int7 = 7 ;          // private by default
        //      static int              Fld_static8 = 8 ;       // private by default
        //      protected static int    Fld_ProtectedStatic9 = 9 ;
        //      const int               Fld_Const10 = 10 ;
        //      public const int        Fld_PublicConst11 = 11 ;
        private Hashtable eventTable = new Hashtable();

        // PROPERTIES
        //-----------------

        //      [Test("my property attrib 2")]      // custom attribute a property
        //      protected internal long Prop_ProtInternal1      {get { return 1; }}
        //      private static long     Prop_PrivateStaticLong2 {get { return 2; }}
        //      private long            Prop_PrivateLong3       {get { return 3; }}
        //      protected long          Prop_ProtectedLong4     {get { return 4; }}
        //      internal sealed override long  Prop_InternalSealedLong5 {get { return 5; }}
        //      public override long    Prop_PublicVirtualLong {get { return 5; }}
        public long PropPublicLong6
        {
            get { return 6; }
            // ReSharper disable once ValueParameterNotUsed
            set { }
        }
        public override int PropAbstract { get { return 10; } }
        //      public static long      Prop_PublicStaticLong7  {get { return 7; }}
        //      public string           this[int i, char c]     {get { return "8";}}    // indexer

        // METHODS
        //-----------------

        // ReSharper disable UnusedMember.Local
        // ReSharper disable RedundantAssignment
        // ReSharper disable OptionalParameterRefOut
        // ReSharper disable UnusedParameter.Local
        [Test("my method attrib")]      // custom attribute method
        public void Fct_PublicVoid(int c) { }
        public override sealed void Fct_FromBase() { }
        static void Fct_static() { }
        private void Fct_Private() { }
        protected void Fct_protected() { }
        internal void Fct_Internal() { }
        protected internal void Fct_ProtectedInternal() { }

        // note : it's not possible to declare method "sealed" and "virtual".
        public new void BaseFctnonOverridable() { }
        public void FctInterface() { }
        public void FctInterface3() { }
        public override void FctBase1() { }
        public virtual void FctChild1(int[] args) { }
        public void FctChild2(ref string strModifier) { }
        public virtual void FctChild3(params int[] args) { }
        public void FctChild4([Out] out int args) { args = 5; }
        public void FctChild5(ref int args) { args = 5; }

        [return: MarshalAs(UnmanagedType.Interface)]
        object FctChild6() { return null; }

        // special use of parameters or attributes
        void FctChild7([In(), Out(), Optional()] ref Guid param1,
                  [MarshalAs(UnmanagedType.Interface)] out object param2)
        { param2 = null; }
        // ReSharper restore UnusedParameter.Local
        // ReSharper restore UnusedMember.Local
        // ReSharper restore RedundantAssignment
        // ReSharper restore OptionalParameterRefOut


        // EVENT
        //-----------------

        public delegate void MyDelegate(int i);    // delegate are see as inner class
        public event MyDelegate Event1
        {
            add { eventTable["Event1"] = (MyDelegate)eventTable["Event1"] + value; }
            // ReSharper disable once DelegateSubtraction
            remove { eventTable["Event1"] = (MyDelegate)eventTable["Event1"] - value; }
        }

        // CONSTRUCTORS
        //-----------------

        // ReSharper disable once UnusedParameter.Local
        public Child(int a)
        {     // use private member to disable warnings
              // Fld_PrivateDouble1 = 0 ; 
              // Fld_PrivateDouble1++;

            // Fld_static8 = 7;         
            // Fld_static8++;

            // Fld_Int7 = 6;            
            // Fld_Int7 ++;

            // Fct_Private();
        }

    }

    #endregion

}
