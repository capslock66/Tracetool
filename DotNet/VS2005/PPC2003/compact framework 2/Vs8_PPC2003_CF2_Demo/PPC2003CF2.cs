using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using System.Net;
using System.Net.Sockets;
using OpenNETCF.Win32;

using TraceTool;


namespace PPC2003
{
    public partial class PPC2003CF2 : Form
    {
        public PPC2003CF2()
        {
            InitializeComponent();
        }

        void CheckSocket(string StrAdr)
        {
            IPAddress adr;
            IPHostEntry host = null;
            // get host
            try
            {
                // CF1
                //host = Dns.GetHostByName(StrAdr);

                // CF2
                host = Dns.GetHostEntry(StrAdr);

            }
            catch (Exception ex)
            {   // System.Net.Sockets.SocketException
                MessageBox.Show(ex.GetType().ToString(), StrAdr);
                MessageBox.Show(ex.Message, StrAdr);
                return;
            }

            if (host == null)
            {
                MessageBox.Show("host is null", StrAdr);
                return;
            }

            adr = host.AddressList[0];
            MessageBox.Show(adr.ToString(), StrAdr);

            // create an end-point for the first address...
            IPEndPoint endPoint = new IPEndPoint(host.AddressList[0], 8090);

            Socket _Socket;
            _Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            try
            {
                _Socket.Connect(endPoint);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.GetType().ToString(), StrAdr);
                MessageBox.Show(ex.Message, StrAdr);
                return;
            }
            MessageBox.Show("Connection succeed", StrAdr);
            _Socket.Close();
        }


        // check partner
        private void ButPartner_Click(object sender, System.EventArgs e)
        {
            RegistryKey registryKey;

            registryKey = Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows CE Services\\Partners");

            Object PCur = registryKey.GetValue("PCur");
            if (PCur != null)
            {
                //TTrace.Debug.Send("PCur : " + PCur.ToString());

                registryKey = Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows CE Services\\Partners\\P" + PCur.ToString());
                string PName = (string)registryKey.GetValue("PName");
                //TTrace.Debug.Send("Partner Name : " + PName);
                CheckSocket(PName);
            }
        }

        //--------------------------------------------------------------------------------------------

        // check registry to see if a network adapter is available
        private void ButShowHosts_Click(object sender, System.EventArgs e)
        {
            RegistryKey registryKey;
            registryKey = Registry.LocalMachine.OpenSubKey("Comm\\Tcpip\\Hosts");
            string[] hostList = registryKey.GetSubKeyNames();
            foreach (string Host in hostList)
            {
                // the first (and unique) host should be 192.168.55.100
                //TTrace.Debug.Send("Host : " + Host);
                CheckSocket(Host);
            }
        }

        //--------------------------------------------------------------------------------------------

        private void butSample_Click(object sender, System.EventArgs e)
        {
            TTrace.Debug.Send("VS7 ppc2003 CF1 demo");

            TTrace.Options.SendProcessName = false;
            string str = '\u2250' + "qwerty é ù è azerty" + '\u9999';

            // simple traces
            TTrace.Debug.Send("Hello").Send("World");  // "World" is a sub trace of "Hello"

            // single separator
            TTrace.Debug.Send("---");

            // send traces with special font style (bold and Italic), color font size and font name
            TTrace.Debug.Send("Special font", "Symbol 12")
               .SetFontDetail(3, true, false, Color.Red.ToArgb())                     // set col 3 (Left Msg)  to bold and Red
               .SetFontDetail(4, false, false, Color.Green.ToArgb(), 12, "Symbol");   // set col 4 (Right Msg) to Green and font size 12
            TTrace.Debug.Send("Impact Italic")
               .SetFontDetail(3, false, true, Color.BlueViolet.ToArgb(), 12, "Impact");     // Col3 (left msg), non bold, Italic , Blue-Violet , font 12 , Impact

            //TTrace.Options.SendThreadId = false ;
            //TTrace.Debug.Send("trace without thread id");
            //TTrace.Options.SendThreadId = true;

            //TTrace.Options.SendDate = true;
            //TTrace.Debug.Send("trace with date");
            //TTrace.Options.SendDate = false;

           // double separator
            TTrace.Debug.Send("===");


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
            node.Members.Add("My Members", "col2", "col3")
              .SetFontDetail(0, true)                                  // set first column to bold
              .SetFontDetail(1, false, false, Color.Green.ToArgb())   // set second column to green
              .Add("Sub members")                                     // add sub member node
              .SetFontDetail(0, false, true);                          // set first column to Italic
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

        private void butIndent_Click(object sender, System.EventArgs e)
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

        private void butSaveTotext_Click(object sender, System.EventArgs e)
        {
            TTrace.WinTrace.SaveToTextfile("c:\\log.txt");
        }

        //--------------------------------------------------------------------------------------------

        private void butSaveToXml_Click(object sender, System.EventArgs e)
        {
            TTrace.WinTrace.SaveToXml("c:\\log.xml");
        }

        //--------------------------------------------------------------------------------------------

        private void butLoadXml_Click(object sender, System.EventArgs e)
        {
            TTrace.WinTrace.LoadXml("c:\\log.xml");
        }

        //--------------------------------------------------------------------------------------------

        private void butShowViewer_Click(object sender, System.EventArgs e)
        {
            TTrace.Show(true);
        }

        //--------------------------------------------------------------------------------------------

        private void butClearMainTraces_Click(object sender, System.EventArgs e)
        {
            TTrace.ClearAll();
        }

        //--------------------------------------------------------------------------------------------

        TraceNode start1 = null;
        TraceNode start2 = null;

        private void butStart1_Click(object sender, System.EventArgs e)
        {
            if (start1 == null)
            {
                start1 = TTrace.Debug.Send("Start 1 ..");
            }
        }

        //--------------------------------------------------------------------------------------------

        private void butResend_Click(object sender, System.EventArgs e)
        {
            if (start1 == null)
                return;

            start1.ResendRight("Done 1");
        }

        //--------------------------------------------------------------------------------------------

        private void butSetSelected_Click(object sender, System.EventArgs e)
        {
            if (start1 == null)
                return;

            start1.SetSelected();
        }

        //--------------------------------------------------------------------------------------------

        private void butNodeIndent_Click(object sender, System.EventArgs e)
        {
            if (start1 == null)
                return;

            start1.Send("before indent");    // send text under the start1 node
            start1.Indent("ident 1");        // send text under the start1 node and keep it this trace as the new target for further sub traces
            start1.Send("Level2");           // send text under the "indent 1" node
            start1.UnIndent("done");         // unindent and send text under the start1 node. Text is optional
        }

        //--------------------------------------------------------------------------------------------

        private void butstart2_Click(object sender, System.EventArgs e)
        {
            if (start2 == null)
            {
                start2 = TTrace.Debug.Send("Start 2 ..");
            }
        }

        //--------------------------------------------------------------------------------------------

        private void butAppend_Click(object sender, System.EventArgs e)
        {
            if (start2 == null)
                return;

            start2.AppendLeft("..Done");   // Append left part
        }

        //--------------------------------------------------------------------------------------------

        private void butShowNode_Click(object sender, System.EventArgs e)
        {
            if (start2 == null)
                return;

            start2.Show();
        }

        //--------------------------------------------------------------------------------------------

        private WinTrace myWinTrace;

        private void butCreateWindow_Click(object sender, System.EventArgs e)
        {
            myWinTrace = new WinTrace("MyWINID", "My trace window");
            butDisplayWindow.Enabled = true;
            butHelloWin.Enabled = true;
            butSaveWinToText.Enabled = true;
            butSaveWinToXml.Enabled = true;
            butLoadWinXml.Enabled = true;
        }

        //--------------------------------------------------------------------------------------------

        private void butDisplayWindow_Click(object sender, System.EventArgs e)
        {
            myWinTrace.DisplayWin();
        }

        //--------------------------------------------------------------------------------------------

        private void butHelloWin_Click(object sender, System.EventArgs e)
        {
            myWinTrace.Debug.Send("Hello", "Can be used to store exceptions, for examples");
        }

        //--------------------------------------------------------------------------------------------

        private void butSaveWinToText_Click(object sender, System.EventArgs e)
        {
            myWinTrace.SaveToTextfile("c:\\log2.txt");
        }

        //--------------------------------------------------------------------------------------------

        private void butSaveWinToXml_Click(object sender, System.EventArgs e)
        {
            myWinTrace.SaveToXml("c:\\log2.xml");
        }

        //--------------------------------------------------------------------------------------------

        private void butLoadWinXml_Click(object sender, System.EventArgs e)
        {
            myWinTrace.LoadXml("c:\\log2.xml");
        }

        //--------------------------------------------------------------------------------------------

        WinTrace MultiColTrace;
        private void butMulticolTest_Click(object sender, System.EventArgs e)
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

        //--------------------------------------------------------------------------------------------
        int WatchCounter = 123;
        private void butSendMainWatches_Click(object sender, System.EventArgs e)
        {
            TTrace.Watches.Send("test2", WatchCounter);
            WatchCounter++;
        }

        //--------------------------------------------------------------------------------------------

        private void butClearMainWatches_Click(object sender, System.EventArgs e)
        {
            TTrace.Watches.ClearAll();
        }

        //--------------------------------------------------------------------------------------------

        private void butDisplayMainWatches_Click(object sender, System.EventArgs e)
        {
            TTrace.Watches.DisplayWin();
        }

        //--------------------------------------------------------------------------------------------

        private WinWatch MyWinWatch;
        private void butCreateWinWatches_Click(object sender, System.EventArgs e)
        {
            MyWinWatch = new WinWatch("MyWinWatchID", "My watches");
        }

        //--------------------------------------------------------------------------------------------

        private void butSendWinWatches_Click(object sender, System.EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.Send("Now", DateTime.Now.ToString("HH:mm:ss:fff"));
        }

        //--------------------------------------------------------------------------------------------

        private void butClearWinWatches_Click(object sender, System.EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.ClearAll();
        }

        //--------------------------------------------------------------------------------------------

        private void butDisplayWinWatches_Click(object sender, System.EventArgs e)
        {
            if (MyWinWatch != null)
                MyWinWatch.DisplayWin();
        }


    }
}