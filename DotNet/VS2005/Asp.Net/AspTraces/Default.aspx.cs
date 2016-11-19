using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using TraceTool;

public partial class _Default : System.Web.UI.Page 
{
    //--------------------------------------------------------------------------------------------
    
    protected void Page_Load(object sender, EventArgs e)        
    {
       // You can switch to socket mode in the Page_Load event or change the Web.Config / Machine.Config  
       //TTrace.Options.SendMode = SendMode.Socket;
       TTrace.Debug.Send("Page_Load ") ;
    }
 
    //--------------------------------------------------------------------------------------------
    
    protected void butSample_Click(object sender, EventArgs e)
    {
        TTrace.Debug.Send("VS8 ASP NET F2 demo");

        TTrace.Options.SendProcessName = false;
        string str = '\u2250' + "qwerty é ù è azerty" + '\u9999';

        // simple traces
        TTrace.Debug.Send("Hello").Send("World");  // "World" is a sub trace of "Hello"

        // traces using Sendxxx method
        // Use default display filter. (see TTrace.Options)

        TTrace.Debug.SendType("SendType 'Trace node Type'", TTrace.Debug.GetType());
        TTrace.Debug.SendObject("My const", TraceConst.CST_CREATE_MEMBER);
        TTrace.Debug.SendDump("SendDump test", "Unicode", System.Text.Encoding.Unicode.GetBytes(str), 50);

        // TTrace.Debug.SendType ("My abstract type" , typeof (Base));  // same as Type.GetType("Project1.Base")

        // traces using TraceNodeEx
        TraceNodeEx node = new TraceNodeEx(null);  //  TTrace.Debug
        node.LeftMsg = "TraceNodeEx";
        node.RightMsg = "demo";
        node.IconIndex = 8;
        node.AddDump("ASCII", System.Text.Encoding.ASCII.GetBytes(str), 50);   // 3F 61 7A          ..... 3F
        node.AddDump("UTF8", System.Text.Encoding.UTF8.GetBytes(str), 50);
        node.AddDump("Unicode", System.Text.Encoding.Unicode.GetBytes(str), 50); // 50 22 61 00 7A 00 ..... 99 99
        node.Send();


        // specify what to send (modifiers, fields, ...). Can be slow on complexe objects
        TraceDisplayFlags flags = TraceDisplayFlags.ShowModifiers |
            TraceDisplayFlags.ShowInheritedMembers |
            TraceDisplayFlags.ShowNonPublic |
            TraceDisplayFlags.ShowFields;

        TTrace.Error.SendObject("SendObject 'Trace node Object'", TTrace.Debug, flags);

        TTrace.Flush();

    }
    
    //--------------------------------------------------------------------------------------------
    
    protected void butIndent_Click(object sender, EventArgs e)
    {
        // Indent and UnIndent 
        TTrace.Debug.Indent("Before", "some work");
        TTrace.Debug.Indent("Level1");
        TTrace.Debug.Send("Level2");
        TTrace.Debug.Send("More level2");
        TTrace.Debug.UnIndent("Done", "level 1");
        TTrace.Debug.UnIndent("Work is done");
    }
    
    //--------------------------------------------------------------------------------------------
    
    protected void butSaveTotext_Click(object sender, EventArgs e)
    {
        TTrace.WinTrace.SaveToTextfile("c:\\log.txt");
    }

    //--------------------------------------------------------------------------------------------

    protected void butSaveToXml_Click(object sender, EventArgs e)
    {
        TTrace.WinTrace.SaveToXml("c:\\log.xml");
    }

    //--------------------------------------------------------------------------------------------

    protected void butLoadXml_Click(object sender, EventArgs e)
    {
        TTrace.WinTrace.LoadXml("c:\\log.xml");
    }

    //--------------------------------------------------------------------------------------------

    protected void butClearMainTraces_Click(object sender, EventArgs e)
    {
        TTrace.ClearAll();
    }

    //--------------------------------------------------------------------------------------------

    protected void butShowViewer_Click(object sender, EventArgs e)
    {
        TTrace.Show(true);
    }

    //--------------------------------------------------------------------------------------------

    TraceNode start1 = null;
    TraceNode start2 = null;
    
    protected void butStart1_Click(object sender, EventArgs e)
    {
        start1 = (TraceNode) Session.Contents["start1"];
        if (start1 == null)
        {
            start1 = TTrace.Debug.Send("Start 1 ..");
            Session.Contents["start1"] = start1;
            butResend.Enabled = true;
            butSetSelected.Enabled = true;
            butNodeIndent.Enabled = true;
        }
    }

    //--------------------------------------------------------------------------------------------

    protected void butResend_Click(object sender, EventArgs e)
    {
        start1 = (TraceNode)Session.Contents["start1"];
        if (start1 == null)
            return;

        start1.ResendRight("Done 1");
    }

    //--------------------------------------------------------------------------------------------

    protected void butSetSelected_Click(object sender, EventArgs e)
    {
        start1 = (TraceNode)Session.Contents["start1"];
        if (start1 == null)
            return;

        start1.SetSelected();
    }

    //--------------------------------------------------------------------------------------------

    protected void butNodeIndent_Click(object sender, EventArgs e)
    {
        start1 = (TraceNode)Session.Contents["start1"];
        if (start1 == null)
            return;

        start1.Send("before indent");    // send text under the start1 node
        start1.Indent("ident 1");        // send text under the start1 node and keep it this trace as the new target for further sub traces
        start1.Send("Level2");           // send text under the "indent 1" node
        start1.UnIndent("done");         // unindent and send text under the start1 node. Text is optional
    }

    //--------------------------------------------------------------------------------------------

    protected void butstart2_Click(object sender, EventArgs e)
    {
        start2 = (TraceNode)Session.Contents["start2"];
        if (start2 == null)
        {
            start2 = TTrace.Debug.Send("Start 2 ..");
            Session.Contents["start2"] = start2;
            butAppend.Enabled = true;
            butShowNode.Enabled = true;
        }
    }

    //--------------------------------------------------------------------------------------------

    protected void butAppend_Click(object sender, EventArgs e)
    {
        start2 = (TraceNode)Session.Contents["start2"];
        if (start2 == null)
            return;

        start2.AppendLeft("..Done");   // Append left part
    }

    //--------------------------------------------------------------------------------------------

    protected void butShowNode_Click(object sender, EventArgs e)
    {
        start2 = (TraceNode)Session.Contents["start2"];
        if (start2 == null)
            return;

        start2.Show();
    }

    //--------------------------------------------------------------------------------------------

    
    private WinTrace myWinTrace;
    
    protected void butCreateWindow_Click(object sender, EventArgs e)
    {
        myWinTrace = new WinTrace("MyWINID", "My trace window");
        butDisplayWindow.Enabled = true;
        butHelloWin.Enabled = true;
        butSaveWinToText.Enabled = true;
        butSaveWinToXml.Enabled = true;
        butLoadWinXml.Enabled = true;
        Session.Contents["myWinTrace"] = myWinTrace;
    }

    //--------------------------------------------------------------------------------------------

    protected void butDisplayWindow_Click(object sender, EventArgs e)
    {
        myWinTrace = (WinTrace)Session.Contents["myWinTrace"];
        if (myWinTrace == null)
            return;

        myWinTrace.DisplayWin();
    }

    //--------------------------------------------------------------------------------------------

    protected void butHelloWin_Click(object sender, EventArgs e)
    {
        myWinTrace = (WinTrace)Session.Contents["myWinTrace"];
        if (myWinTrace == null)
            return;

        myWinTrace.Debug.Send("Hello", "Can be used to store exceptions, for examples");
    }

    //--------------------------------------------------------------------------------------------

    protected void butSaveWinToText_Click(object sender, EventArgs e)
    {
        myWinTrace = (WinTrace)Session.Contents["myWinTrace"];
        if (myWinTrace == null)
            return;

        myWinTrace.SaveToTextfile("c:\\log2.txt");
    }

    //--------------------------------------------------------------------------------------------

    protected void butSaveWinToXml_Click(object sender, EventArgs e)
    {
        myWinTrace = (WinTrace)Session.Contents["myWinTrace"];
        if (myWinTrace == null)
            return;

        myWinTrace.SaveToXml("c:\\log2.xml");
    }

    //--------------------------------------------------------------------------------------------

    protected void butLoadWinXml_Click(object sender, EventArgs e)
    {
        myWinTrace = (WinTrace)Session.Contents["myWinTrace"];
        if (myWinTrace == null)
            return;

        myWinTrace.LoadXml("c:\\log2.xml");
    }


    //--------------------------------------------------------------------------------------------

    WinTrace MultiColTrace;
    protected void butMulticolTest_Click(object sender, EventArgs e)
    {
        MultiColTrace = (WinTrace)Session.Contents["MultiColTrace"];
        if (MultiColTrace == null)
        {
            MultiColTrace = new WinTrace("MCOL", "MultiCol trace window");
            MultiColTrace.SetMultiColumn(1);  // must be called before calling setColumnsTitle
            MultiColTrace.SetColumnsTitle("col1 \t col2 \t col3");
            MultiColTrace.SetColumnsWidth("100:20:80 \t 200:50 \t 100");
            MultiColTrace.DisplayWin();
            Session.Contents["MultiColTrace"] = MultiColTrace;
        }
        MultiColTrace.Debug.Send("1 \t 2 \t 3");
    }


    //--------------------------------------------------------------------------------------------

    int WatchCounter = 123;
    
    protected void butSendMainWatches_Click(object sender, EventArgs e)
    {
        if (Session.Contents["WatchCounter"] != null)
            WatchCounter = (int)Session.Contents["WatchCounter"];
        TTrace.Watches.Send("test2", WatchCounter);
        WatchCounter++;
        Session.Contents["WatchCounter"] = WatchCounter ;
    }

    //--------------------------------------------------------------------------------------------

    protected void butDisplayMainWatches_Click(object sender, EventArgs e)
    {
        TTrace.Watches.DisplayWin();
    }

    //--------------------------------------------------------------------------------------------

    protected void butClearMainWatches_Click(object sender, EventArgs e)
    {
        TTrace.Watches.ClearAll();
    }
    
    //--------------------------------------------------------------------------------------------

    private WinWatch MyWinWatch;
    protected void butCreateWinWatches_Click(object sender, EventArgs e)
    {
        MyWinWatch = (WinWatch)Session.Contents["MyWinWatch"];
        if (MyWinWatch == null)
        {
            MyWinWatch = new WinWatch("MyWinWatchID", "My watches");
            Session.Contents["MyWinWatch"] = MyWinWatch;
            butSendWinWatches.Enabled = true;
            butDisplayWinWatches.Enabled = true;
            butClearWinWatches.Enabled = true;
        }
    }

    //--------------------------------------------------------------------------------------------

    protected void butSendWinWatches_Click(object sender, EventArgs e)
    {
        MyWinWatch = (WinWatch)Session.Contents["MyWinWatch"];
        if (MyWinWatch != null)
            MyWinWatch.Send("Now", DateTime.Now.ToString("HH:mm:ss:fff"));
    }

    //--------------------------------------------------------------------------------------------

    protected void butDisplayWinWatches_Click(object sender, EventArgs e)
    {
        MyWinWatch = (WinWatch)Session.Contents["MyWinWatch"];
        if (MyWinWatch != null)
            MyWinWatch.DisplayWin();
    }

    //--------------------------------------------------------------------------------------------

    protected void butClearWinWatches_Click(object sender, EventArgs e)
    {
        MyWinWatch = (WinWatch)Session.Contents["MyWinWatch"];
        if (MyWinWatch != null)
            MyWinWatch.ClearAll();
    }
}
