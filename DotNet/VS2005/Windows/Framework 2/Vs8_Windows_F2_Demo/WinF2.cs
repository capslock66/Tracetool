
using System;
using System.Collections.Generic;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

using TraceTool;

namespace CSharpDemo
{
    public partial class WinF2 : Form
    {
        TestClass testClass = new TestClass();
        public static object ShowViewerButton;

        public WinF2()
        {
            InitializeComponent();
            ShowViewerButton = butShowtrace;
        }

        private void Win2Form_Load(object sender, EventArgs e)
        {
            //comboBox1.SelectedIndex  = 0 ;
            if (TTrace.Options.SendMode == SendMode.WinMsg)
                comboBox1.SelectedIndex = 0;
            else
                comboBox1.SelectedIndex = 1;
            chkSendEvents.Checked = TTrace.Options.SendEvents;
            chkSendInherited.Checked = TTrace.Options.SendInherited;
            chkSendFunctions.Checked = TTrace.Options.SendFunctions;

        }

        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBox1.SelectedIndex == 0)
                TTrace.Options.SendMode = SendMode.WinMsg;
            else
                TTrace.Options.SendMode = SendMode.Socket;

        }

        private void chkSendFunctions_CheckedChanged(object sender, EventArgs e)
        {
            TTrace.Options.SendFunctions = chkSendFunctions.Checked;
        }

        private void chkSendInherited_CheckedChanged(object sender, EventArgs e)
        {
            TTrace.Options.SendInherited = chkSendInherited.Checked;
        }

        private void chkSendEvents_CheckedChanged(object sender, EventArgs e)
        {
            TTrace.Options.SendEvents = chkSendEvents.Checked;
        }

        private void chkSendProcessName_CheckedChanged(object sender, EventArgs e)
        {

        }

        private void butShowtrace_Click(object sender, EventArgs e)
        {
            TTrace.Show(true);
        }

        private void butCloseViewer_Click(object sender, EventArgs e)
        { 
            TTrace.CloseViewer();
        }

        private void butTrace_Click(object sender, EventArgs e)
        {
            TTrace.Options.SendProcessName = chkSendProcessName.Checked;
            string str = '\u2250' + "qwerty é ù è azerty" + '\u9999';

            // simple traces
            //--------------------------------------------
            TTrace.Debug.Send("Hello").Send("World");  // "World" is a sub trace of "Hello"

            // single separator
            TTrace.Debug.Send("---");

            // send traces with special font style (bold and Italic), color font size and font name
            TTrace.Debug.Send("Special font", "Symbol 12")
               .SetFontDetail(-1, false, true)                                        // set whole line to italic 
               .SetFontDetail(3, true, false, Color.Red.ToArgb())                     // set col 3 (Left Msg)  to bold and Red
               .SetFontDetail(4, false, false, Color.Green.ToArgb(), 12, "Symbol");   // set col 4 (Right Msg) to Green , font size 12 , Symbol
            TTrace.Debug.Send("Impact Italic")
               .SetFontDetail(3, false, true, Color.BlueViolet.ToArgb(), 12, "Impact");     // Col3 (left msg), non bold, Italic , Blue-Violet , font 12 , Impact

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
            TTrace.Debug.SendType("My interface", typeof(Myinterface));
            TTrace.Debug.SendObject("My const", TraceConst.CST_CREATE_MEMBER);
            TTrace.Debug.SendObject("My enum", testClass.fieldDay);
            TTrace.Debug.SendCaller("SendCaller test", 0);
            TTrace.Debug.SendStack("Stack test", 0);
            TTrace.Debug.SendDump("SendDump test", "Unicode", Encoding.Unicode.GetBytes(str), 50);

            TTrace.Warning.SendType("SendType 'testClass'", testClass.GetType());

            // specify what to send (modifiers, fields, ...). Can be slow on complexe objects
            TraceDisplayFlags flags = TraceDisplayFlags.ShowModifiers |
                                      TraceDisplayFlags.ShowInheritedMembers |
                                      TraceDisplayFlags.ShowNonPublic |
                                      TraceDisplayFlags.ShowFields;

            if (chkSendFunctions.Checked)
               flags |= TraceDisplayFlags.ShowMethods;
            TTrace.Error.SendObject("SendObject 'testClass'", testClass, flags);

            // traces using TraceNodeEx
            //--------------------------------------------
            TraceNodeEx node = new TraceNodeEx(null);  //  TTrace.Debug
            node.LeftMsg = "TraceNodeEx";
            node.RightMsg = "demo";
            node.AddFontDetail(3, false, false, Color.Green.ToArgb());
            node.IconIndex = 8;
            node.Members.Add("My Members", "col2" , "col3")
               .SetFontDetail(0,true)                                  // set first column to bold
               .SetFontDetail(1, false, false, Color.Green.ToArgb())   // set second column to green
               .Add("Sub members")                                     // add sub member node
                  .SetFontDetail(0,false,true) ;                          // set first column to Italic
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
            TTrace.Debug.SendBitmap("Bitmap", pictureBox1.Image);

            // Text, image and XML together
            //--------------------------------------------
            node = new TraceNodeEx(TTrace.Debug);
            node.LeftMsg = "Text, image and XML together";
            node.Members.Add("Text displayed in detail");
            node.AddBitmap(pictureBox1.Image);
            node.AddXML("<?xml version='1.0' ?><Data> Xml in traceNodeEx </Data>");
            node.Send();

            // send table detail  
            //--------------------------------------------

            // create the table
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
            TTrace.Flush();

        }

        WinTrace MultiColTrace;
        private void MulticolBut_Click(object sender, EventArgs e)
        {
            if (MultiColTrace == null)
            {
                MultiColTrace = new WinTrace("MCOL", "MultiCol trace window");
                MultiColTrace.SetMultiColumn(1);  // must be called before calling setColumnsTitle
                MultiColTrace.SetColumnsTitle("col1 \t col2 \t col3");
                MultiColTrace.SetColumnsWidth("100:20:80 \t 200:50 \t 100");
                MultiColTrace.DisplayWin();
            }
            MultiColTrace.Debug.Send("1 \t 2 \t 3");

        }

        private void IndentButton_Click(object sender, EventArgs e)
        {
            TraceNode node = TTrace.Debug.Send("Tree indentation using Indent and UnIndent methods");

            node.Indent("Indent","level 1");
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
            TTrace.Debug.Send   ("root 1", TTrace.Debug.IndentLevel.ToString()) ;
            TTrace.Debug.Indent ("start indentation");
            TTrace.Debug.Send   (   "under indent 1", TTrace.Debug.IndentLevel.ToString());
            TraceNodeEx nodeEx = new TraceNodeEx(TTrace.Debug) ;   // Parent depends of the indentation
            nodeEx.LeftMsg =        "under indent 2" ;
            nodeEx.Send() ;
            TTrace.Debug.UnIndent ();
            TTrace.Debug.Send("root 2", TTrace.Debug.IndentLevel.ToString());
        }

        private void butVariant_Click(object sender, EventArgs e)
        {
            TTrace.Options.SendProcessName = chkSendProcessName.Checked;

            // SendValue is quite different from SendObject 
            // SendValue is recursive and display arrays
            TTrace.Debug.SendValue("SendValue 'testClass'", testClass);
            TTrace.Warning.SendValue("SendValue 'testClass', max 3 levels (default)", testClass, true, 3, "testClass");
            TTrace.Error.SendValue("SendValue 'array', max 5 levels", testClass.fieldArray, true, 5, "fieldArray"); 
        }

        private void butFullInfo_Click(object sender, EventArgs e)
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

        private void butDoc_Click(object sender, EventArgs e)
        {
            // display a System.Windows.Forms.Button object with his documentation
            TTrace.Debug.SendObject("butDoc object", butDoc,
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

        private void butSaveToTXT_Click(object sender, EventArgs e)
        {
            TTrace.WinTrace.SaveToTextfile("c:\\log.txt");
        }

        private void butClear_Click(object sender, EventArgs e)
        {
            TTrace.ClearAll();
        }

        private void butSaveToXml_Click(object sender, EventArgs e)
        {
            TTrace.WinTrace.SaveToXml("c:\\log.xml");
            TTrace.WinTrace.SaveToXml("c:\\log.xml","tracetool.xsl");
         }

        private void butLoadXml_Click(object sender, EventArgs e)
        {
            TTrace.WinTrace.LoadXml("c:\\log.xml");
        }

        private void butListenerInit_Click(object sender, EventArgs e)
        {
            Trace.WriteLine("Before setting listener : trace goes to ODS window");
            int[] myArray = new int[3] { 3, 5, 5 };

            Trace.Listeners.Clear();
            Trace.Listeners.Add(new TTraceListener());
            Trace.WriteLine("TraceListener demo in main trace window");
            Trace.Indent();
            Trace.Write("myArray : ");
            Trace.WriteLine(myArray);
            Trace.Unindent(); 
        }

        private void butListenerWrite_Click(object sender, EventArgs e)
        {
            Trace.Write("hello.");
        }

        private void butListenerWriteLine_Click(object sender, EventArgs e)
        {
            Trace.WriteLine("World.");
        }

        private void butDiagnostic_Click(object sender, EventArgs e)
        {
            test++;
            Trace.WriteLine("Diagnostics test" + test);      
        }

        [DllImport("kernel32.dll")]
        extern static void OutputDebugString(String str);
        
        private void butOldTrace_Click(object sender, EventArgs e)
        {
            test++;
            OutputDebugString("OutputDebugString test" + test);

        }

        private void butEventLog_Click(object sender, EventArgs e)
        {
            EventLog myLog = new EventLog();
            myLog.Source = "Application";

            test++;

            // Write an informational entry to the event log.    
            myLog.WriteEntry("Event log test " + test);

        }

        int test = 0;
        
        private void butTail_Click(object sender, EventArgs e)
        {
            FileStream fsFileStream = new FileStream("c:\\log.txt", FileMode.Append);
            StreamWriter swWriter = new StreamWriter(fsFileStream);
            test++;
            swWriter.WriteLine("test " + test);
            swWriter.Close();
            fsFileStream.Close();
        }

        private WinTrace myWinTrace;
        
        private void butCreateWinTrace_Click(object sender, EventArgs e)
        {
            myWinTrace = new WinTrace("MyWINID", "My trace window");
            butDisplayWin.Enabled = true;
            butHelloToWintrace.Enabled = true;
            butSaveWinToTxt.Enabled = true;
            butSaveWinToXml.Enabled = true;
            butClearWin.Enabled = true;
            butWinLoadXml.Enabled = true; 
            butCloseWin.Enabled = true;
        }

        private void butDisplayWin_Click(object sender, EventArgs e)
        {
            myWinTrace.DisplayWin();
        }

        private void butHelloToWintrace_Click(object sender, EventArgs e)
        {
            myWinTrace.Debug.Send("Hello", "Can be used to store exceptions, for examples"); 
        }

        private void butSaveWinToTxt_Click(object sender, EventArgs e)
        {
            myWinTrace.SaveToTextfile("c:\\log2.txt");
        }

        private void butClearWin_Click(object sender, EventArgs e)
        {
            myWinTrace.ClearAll();
        }

        private void butCloseWin_Click(object sender, EventArgs e)
        {
            myWinTrace.Close();
        }


        private void butSaveWinToXml_Click(object sender, EventArgs e)
        {
            myWinTrace.SaveToXml("c:\\log2.xml");
        }

        private void butWinLoadXml_Click(object sender, EventArgs e)
        {
            myWinTrace.LoadXml("c:\\log2.xml");
        }

        TestClass test2 = new TestClass();
        private WinWatch MyWinWatch;

        private void butCreateWinWatch_Click(object sender, EventArgs e)
        {
            MyWinWatch = new WinWatch("MyWinWatchID", "My watches");
        }

        private void butWatch_Click(object sender, EventArgs e)
        {
            TTrace.Watches.Send("test2", test2);     

        }

        private void butWinWatchSend_Click(object sender, EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.Send("Now", DateTime.Now.ToString("HH:mm:ss:fff"));
        }

        private void butClearWatchWindow_Click(object sender, EventArgs e)
        {
            TTrace.Watches.ClearAll();       

        }

        private void butWinWatchClear_Click(object sender, EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.ClearAll();
        }

        private void butDisplayWatchWindow_Click(object sender, EventArgs e)
        {
            TTrace.Watches.DisplayWin();
        }

        private void butWinWatchDisplay_Click(object sender, EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.DisplayWin();
        }

        private void butWinWatchClose_Click(object sender, EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.Close(); 
        }

        TraceNode start1;
        TraceNode start2;
        
        private void butStart1_Click(object sender, EventArgs e)
        {
         start1 = TTrace.Debug.Send ("Start 1 ..") ;
         butDone1.Enabled = true ;
         butSetSelected.Enabled = true ;
        }

        private void butDone1_Click(object sender, EventArgs e)
        {
            start1.ResendRight("Done 1");
        }

        private void butSetSelected_Click(object sender, EventArgs e)
        {
            start1.SetSelected();
        }

        private void butstart2_Click(object sender, EventArgs e)
        {
            start2 = TTrace.Debug.Send("Start 2 ..");
            butEnd2.Enabled = true;
            butShowNode.Enabled = true;
        }

        private void butEnd2_Click(object sender, EventArgs e)
        {
            start2.AppendLeft("Done 2");
        }

        private void butShowNode_Click(object sender, EventArgs e)
        {
            start2.Show();
        }



    }
    //----------------------------------------------------------------------------------------------------

    #region Test Classes

    public class TestClass
    {
        public enum Days { Sat = 1, Sun, Mon, Tue, Wed, Thu, Fri };

        // sample Application variables.
        public Days fieldDay;
        public Array fieldArray;
        public System.Collections.Hashtable fieldHash;
        public System.Collections.ArrayList fieldArrayList;
        private Child fieldClassInstance;
        public object PropButton { get { return WinF2.ShowViewerButton; } }
        public Child myNullChild;

        public new string ToString() { return "BlaBla"; }
        public TestClass()
        {
            // initialize demo vars.

            fieldDay = Days.Tue;

            fieldClassInstance = new Child(5);
            fieldClassInstance.fctChild1(new int[3] { 1, 2, 3 });   // accept only  arrays
            fieldClassInstance.fctChild3(1, 2, 3);                // parameters are converted to array
            fieldClassInstance.fctChild3(new int[3] { 1, 2, 3 });   // also accept array

            fieldHash = new Hashtable();
            fieldHash.Add("Tuesday", fieldDay);

            fieldArrayList = new ArrayList();
            fieldArrayList.Add(fieldDay);

            // init bound array 
            int[] myLengthsArray = new int[3] { 1, 2, 5 };     // 1 by 2 by 5
            int[] myBoundsArray = new int[3] { 2, 3, 8 };     // 2..4 , 3..4 , 8..12
            fieldArray = Array.CreateInstance(typeof(Object), myLengthsArray, myBoundsArray);

            // sub array
            int[] myIndArray = new int[3];
            myIndArray[0] = 2;
            myIndArray[1] = 4;
            myIndArray[2] = 6;

            fieldArray.SetValue(238, new int[3] { 2, 3, 8 });
            fieldArray.SetValue("239", new int[3] { 2, 3, 9 });
            fieldArray.SetValue(DateTime.Now, new int[3] { 2, 4, 8 });
            fieldArray.SetValue(fieldClassInstance, new int[3] { 2, 4, 9 });
            fieldArray.SetValue(myIndArray, new int[3] { 2, 4, 10 });

        }
    }

    //---------------------------------------------------------------------------

    public struct MyStruct
    {
        private double k;
        public MyStruct(int i) { k = 200 * i; }
        public long l { get { return 6; } }
    }

    //---------------------------------------------------------------------------

    [AttributeUsage(AttributeTargets.All, AllowMultiple = true)]
    public class TestAttribute : Attribute
    {
        string value;
        public TestAttribute(string value)
        {
            this.value = value;
        }
    }

    //---------------------------------------------------------------------------

    public interface Myinterface : Myinterface3
    {
        void FctInterface();
    }

    //---------------------------------------------------------------------------

    public interface Myinterface2
    {
        void FctInterface2();
    }

    //---------------------------------------------------------------------------

    public interface Myinterface3
    {
        void FctInterface3();
    }

    //---------------------------------------------------------------------------

    public class Mere
    {
        public virtual int fctMere() { return 1; }
    }

    //---------------------------------------------------------------------------

    [DefaultMember("fctBase1")]
    public abstract class Base : Mere, Myinterface2
    {
        public int Fld_PublicFromBase50 = 50;
        private int Fld_PrivateFromBase51 = 51;
        protected int Fld_ProtectedFromBase52 = 52;
        public virtual void Fct_FromBase() { Fld_PrivateFromBase51++; }
        public static string Prop_PublicstaticStr { get { return "5"; } }
        public virtual long Prop_PublicVirtualLong { get { return 5; } }
        public virtual int Prop_PublicVirtualInt { get { return 5; } }
        public abstract int Prop_abstract { get;}
        protected Base(char c) { }
        protected Base(float f) { }
        protected Base() { }  // no argument, used by derived class

        public override sealed int fctMere() { return 2; }
        public abstract void fctBase1();    // redefined in class child
        public virtual void fctBase2() { }
        public void FctInterface2() { }
        public void baseFctnonOverridable() { }
    }


    //---------------------------------------------------------------------------

    public class TestRecur
    {
        public int field1
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
    public class Child : Base, Myinterface
    {
        // inner class
        //-----------------
        public class mysubclass
        {
            public int myfield;
        }

        // return an inner class
        public mysubclass createsubclass() { return new mysubclass(); }


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
        private System.Collections.Hashtable eventTable = new Hashtable();

        // PROPERTIES
        //-----------------

        //      [Test("my property attrib 2")]      // custom attribute a property
        //      protected internal long Prop_ProtInternal1      {get { return 1; }}
        //      private static long     Prop_PrivateStaticLong2 {get { return 2; }}
        //      private long            Prop_PrivateLong3       {get { return 3; }}
        //      protected long          Prop_ProtectedLong4     {get { return 4; }}
        //      internal sealed override long  Prop_InternalSealedLong5 {get { return 5; }}
        //      public override long    Prop_PublicVirtualLong {get { return 5; }}
        public long Prop_PublicLong6
        {
            get { return 6; }
            set { }
        }
        public override int Prop_abstract { get { return 10; } }
        //      public static long      Prop_PublicStaticLong7  {get { return 7; }}
        //      public string           this[int i, char c]     {get { return "8";}}    // indexer

        // METHODS
        //-----------------

        [Test("my method attrib")]      // custom attribute method
        public void Fct_PublicVoid(int c) { }
        public override sealed void Fct_FromBase() { }
        static void Fct_static() { }
        private void Fct_Private() { }
        protected void Fct_protected() { }
        internal void Fct_Internal() { }
        protected internal void Fct_ProtectedInternal() { }

        // note : it's not possible to declare method "sealed" and "virtual".
        public new void baseFctnonOverridable() { }
        public void FctInterface() { }
        public void FctInterface3() { }
        public override void fctBase1() { }
        public virtual void fctChild1(int[] args) { }
        public void fctChild2(ref string strModifier) { }
        public virtual void fctChild3(params int[] args) { }
        public void fctChild4([Out] out int args) { args = 5; }
        public void fctChild5(ref int args) { args = 5; }


        [return: MarshalAs(UnmanagedType.Interface)]
        object fctChild6() { return null; }

        // special use of parameters or attributes
        void fctChild7([In(), Out(), Optional()] ref Guid param1,
                  [MarshalAs(UnmanagedType.Interface)] out object param2) { param2 = null; }


        // EVENT
        //-----------------

        public delegate void MyDelegate(int i);    // delegate are see as inner class
        public event MyDelegate Event1
        {
            add { eventTable["Event1"] = (MyDelegate)eventTable["Event1"] + value; }
            remove { eventTable["Event1"] = (MyDelegate)eventTable["Event1"] - value; }
        }

        // CONSTRUCTORS
        //-----------------

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