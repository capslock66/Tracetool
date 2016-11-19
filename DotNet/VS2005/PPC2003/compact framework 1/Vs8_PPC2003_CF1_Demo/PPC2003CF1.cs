#region Using directives

using System;
using System.Drawing;
using System.Collections;
using System.Windows.Forms;
using System.Data;

using System.Net;
using System.Net.Sockets;
using OpenNETCF.Win32;

using TraceTool;

#endregion

namespace PPC
{
    /// <summary>
    /// Summary description for Form2.
    /// </summary>
    public class PPC2003CF1 : System.Windows.Forms.Form
    {
        private TabControl tabControl1;
        private TabPage tabPage1;
        private Label label1;
        private TabPage tabPage3;
        private TabPage tabPage4;
        private Button butClearMainTraces;
        private Button butShowViewer;
        private Button butLoadXml;
        private Button butSaveToXml;
        private Button butSaveTotext;
        private Button butIndent;
        private Button butSample;
        private Button ButShowHosts;
        private Button ButPartner;
        private Button butShowNode;
        private Button butAppend;
        private Button butstart2;
        private Button butNodeIndent;
        private Button butSetSelected;
        private Button butResend;
        private Button butStart1;
        private Button butMulticolTest;
        private Button butLoadWinXml;
        private Button butSaveWinToXml;
        private Button butSaveWinToText;
        private Button butHelloWin;
        private Button butDisplayWindow;
        private Button butCreateWindow;
        private Label label2;
        private Button butClearWinWatches;
        private Button butDisplayWinWatches;
        private Button butSendWinWatches;
        private Button butCreateWinWatches;
        private Button butClearMainWatches;
        private Button butDisplayMainWatches;
        private Button butSendMainWatches;
        private TabPage tabPage2;

        public PPC2003CF1()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.butClearMainTraces = new System.Windows.Forms.Button();
            this.butShowViewer = new System.Windows.Forms.Button();
            this.butLoadXml = new System.Windows.Forms.Button();
            this.butSaveToXml = new System.Windows.Forms.Button();
            this.butSaveTotext = new System.Windows.Forms.Button();
            this.butIndent = new System.Windows.Forms.Button();
            this.butSample = new System.Windows.Forms.Button();
            this.ButShowHosts = new System.Windows.Forms.Button();
            this.ButPartner = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.butShowNode = new System.Windows.Forms.Button();
            this.butAppend = new System.Windows.Forms.Button();
            this.butstart2 = new System.Windows.Forms.Button();
            this.butNodeIndent = new System.Windows.Forms.Button();
            this.butSetSelected = new System.Windows.Forms.Button();
            this.butResend = new System.Windows.Forms.Button();
            this.butStart1 = new System.Windows.Forms.Button();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.butMulticolTest = new System.Windows.Forms.Button();
            this.butLoadWinXml = new System.Windows.Forms.Button();
            this.butSaveWinToXml = new System.Windows.Forms.Button();
            this.butSaveWinToText = new System.Windows.Forms.Button();
            this.butHelloWin = new System.Windows.Forms.Button();
            this.butDisplayWindow = new System.Windows.Forms.Button();
            this.butCreateWindow = new System.Windows.Forms.Button();
            this.tabPage4 = new System.Windows.Forms.TabPage();
            this.butClearWinWatches = new System.Windows.Forms.Button();
            this.butDisplayWinWatches = new System.Windows.Forms.Button();
            this.butSendWinWatches = new System.Windows.Forms.Button();
            this.butCreateWinWatches = new System.Windows.Forms.Button();
            this.butClearMainWatches = new System.Windows.Forms.Button();
            this.butDisplayMainWatches = new System.Windows.Forms.Button();
            this.butSendMainWatches = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Controls.Add(this.tabPage4);
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(240, 291);
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.butClearMainTraces);
            this.tabPage1.Controls.Add(this.butShowViewer);
            this.tabPage1.Controls.Add(this.butLoadXml);
            this.tabPage1.Controls.Add(this.butSaveToXml);
            this.tabPage1.Controls.Add(this.butSaveTotext);
            this.tabPage1.Controls.Add(this.butIndent);
            this.tabPage1.Controls.Add(this.butSample);
            this.tabPage1.Controls.Add(this.ButShowHosts);
            this.tabPage1.Controls.Add(this.ButPartner);
            this.tabPage1.Controls.Add(this.label1);
            this.tabPage1.Location = new System.Drawing.Point(0, 0);
            this.tabPage1.Size = new System.Drawing.Size(240, 268);
            this.tabPage1.Text = "Basic";
            // 
            // butClearMainTraces
            // 
            this.butClearMainTraces.Location = new System.Drawing.Point(122, 220);
            this.butClearMainTraces.Size = new System.Drawing.Size(111, 20);
            this.butClearMainTraces.Text = "Clear main traces";
            this.butClearMainTraces.Click += new System.EventHandler(this.butClearMainTraces_Click);
            // 
            // butShowViewer
            // 
            this.butShowViewer.Location = new System.Drawing.Point(4, 220);
            this.butShowViewer.Size = new System.Drawing.Size(113, 20);
            this.butShowViewer.Text = "Show Viewer";
            this.butShowViewer.Click += new System.EventHandler(this.butShowViewer_Click);
            // 
            // butLoadXml
            // 
            this.butLoadXml.Location = new System.Drawing.Point(4, 184);
            this.butLoadXml.Size = new System.Drawing.Size(231, 20);
            this.butLoadXml.Text = "LoadXml(\"c:\\log.xml\")";
            this.butLoadXml.Click += new System.EventHandler(this.butLoadXml_Click);
            // 
            // butSaveToXml
            // 
            this.butSaveToXml.Location = new System.Drawing.Point(4, 148);
            this.butSaveToXml.Size = new System.Drawing.Size(231, 20);
            this.butSaveToXml.Text = "SaveToXml(\"c:\\log.xml\")";
            this.butSaveToXml.Click += new System.EventHandler(this.butSaveToXml_Click);
            // 
            // butSaveTotext
            // 
            this.butSaveTotext.Location = new System.Drawing.Point(4, 112);
            this.butSaveTotext.Size = new System.Drawing.Size(231, 20);
            this.butSaveTotext.Text = "SaveToText(\"c:\\log.txt\")";
            this.butSaveTotext.Click += new System.EventHandler(this.butSaveTotext_Click);
            // 
            // butIndent
            // 
            this.butIndent.Location = new System.Drawing.Point(122, 76);
            this.butIndent.Size = new System.Drawing.Size(113, 20);
            this.butIndent.Text = "Indent/Unindent";
            this.butIndent.Click += new System.EventHandler(this.butIndent_Click);
            // 
            // butSample
            // 
            this.butSample.Location = new System.Drawing.Point(4, 76);
            this.butSample.Size = new System.Drawing.Size(113, 20);
            this.butSample.Text = "Sample traces";
            this.butSample.Click += new System.EventHandler(this.butSample_Click);
            // 
            // ButShowHosts
            // 
            this.ButShowHosts.Location = new System.Drawing.Point(122, 40);
            this.ButShowHosts.Size = new System.Drawing.Size(113, 20);
            this.ButShowHosts.Text = "Show Hosts";
            this.ButShowHosts.Click += new System.EventHandler(this.ButShowHosts_Click);
            // 
            // ButPartner
            // 
            this.ButPartner.Location = new System.Drawing.Point(4, 40);
            this.ButPartner.Size = new System.Drawing.Size(113, 20);
            this.ButPartner.Text = "Show Partner";
            this.ButPartner.Click += new System.EventHandler(this.ButPartner_Click);
            // 
            // label1
            // 
            this.label1.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
            this.label1.Location = new System.Drawing.Point(0, 0);
            this.label1.Size = new System.Drawing.Size(237, 30);
            this.label1.Text = "Tips : Ensure TraceTool is running. If the demo run under emulator, don\'t forget " +
                "to cradle";
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.butShowNode);
            this.tabPage2.Controls.Add(this.butAppend);
            this.tabPage2.Controls.Add(this.butstart2);
            this.tabPage2.Controls.Add(this.butNodeIndent);
            this.tabPage2.Controls.Add(this.butSetSelected);
            this.tabPage2.Controls.Add(this.butResend);
            this.tabPage2.Controls.Add(this.butStart1);
            this.tabPage2.Location = new System.Drawing.Point(0, 0);
            this.tabPage2.Size = new System.Drawing.Size(240, 268);
            this.tabPage2.Text = "Node Op.";
            // 
            // butShowNode
            // 
            this.butShowNode.Location = new System.Drawing.Point(61, 232);
            this.butShowNode.Size = new System.Drawing.Size(131, 20);
            this.butShowNode.Text = "Show()";
            this.butShowNode.Click += new System.EventHandler(this.butShowNode_Click);
            // 
            // butAppend
            // 
            this.butAppend.Location = new System.Drawing.Point(61, 196);
            this.butAppend.Size = new System.Drawing.Size(131, 20);
            this.butAppend.Text = "Append()";
            this.butAppend.Click += new System.EventHandler(this.butAppend_Click);
            // 
            // butstart2
            // 
            this.butstart2.Location = new System.Drawing.Point(19, 160);
            this.butstart2.Size = new System.Drawing.Size(77, 20);
            this.butstart2.Text = "Start2";
            this.butstart2.Click += new System.EventHandler(this.butstart2_Click);
            // 
            // butNodeIndent
            // 
            this.butNodeIndent.Location = new System.Drawing.Point(61, 124);
            this.butNodeIndent.Size = new System.Drawing.Size(136, 20);
            this.butNodeIndent.Text = "Indent/Unindent";
            this.butNodeIndent.Click += new System.EventHandler(this.butNodeIndent_Click);
            // 
            // butSetSelected
            // 
            this.butSetSelected.Location = new System.Drawing.Point(61, 88);
            this.butSetSelected.Size = new System.Drawing.Size(134, 20);
            this.butSetSelected.Text = "SetSelected()";
            this.butSetSelected.Click += new System.EventHandler(this.butSetSelected_Click);
            // 
            // butResend
            // 
            this.butResend.Location = new System.Drawing.Point(61, 52);
            this.butResend.Size = new System.Drawing.Size(134, 20);
            this.butResend.Text = "Resend()";
            this.butResend.Click += new System.EventHandler(this.butResend_Click);
            // 
            // butStart1
            // 
            this.butStart1.Location = new System.Drawing.Point(19, 16);
            this.butStart1.Size = new System.Drawing.Size(77, 20);
            this.butStart1.Text = "Start1";
            this.butStart1.Click += new System.EventHandler(this.butStart1_Click);
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.butMulticolTest);
            this.tabPage3.Controls.Add(this.butLoadWinXml);
            this.tabPage3.Controls.Add(this.butSaveWinToXml);
            this.tabPage3.Controls.Add(this.butSaveWinToText);
            this.tabPage3.Controls.Add(this.butHelloWin);
            this.tabPage3.Controls.Add(this.butDisplayWindow);
            this.tabPage3.Controls.Add(this.butCreateWindow);
            this.tabPage3.Location = new System.Drawing.Point(0, 0);
            this.tabPage3.Size = new System.Drawing.Size(240, 268);
            this.tabPage3.Text = "Multi pages";
            // 
            // butMulticolTest
            // 
            this.butMulticolTest.Location = new System.Drawing.Point(8, 231);
            this.butMulticolTest.Size = new System.Drawing.Size(221, 20);
            this.butMulticolTest.Text = "Multi col test";
            this.butMulticolTest.Click += new System.EventHandler(this.butMulticolTest_Click);
            // 
            // butLoadWinXml
            // 
            this.butLoadWinXml.Location = new System.Drawing.Point(8, 195);
            this.butLoadWinXml.Size = new System.Drawing.Size(221, 20);
            this.butLoadWinXml.Text = "LoadXml(\"c:\\log2.xml\")";
            this.butLoadWinXml.Click += new System.EventHandler(this.butLoadWinXml_Click);
            // 
            // butSaveWinToXml
            // 
            this.butSaveWinToXml.Location = new System.Drawing.Point(8, 159);
            this.butSaveWinToXml.Size = new System.Drawing.Size(221, 20);
            this.butSaveWinToXml.Text = "SaveToXml(\"c:\\log2.xml\")";
            this.butSaveWinToXml.Click += new System.EventHandler(this.butSaveWinToXml_Click);
            // 
            // butSaveWinToText
            // 
            this.butSaveWinToText.Location = new System.Drawing.Point(8, 123);
            this.butSaveWinToText.Size = new System.Drawing.Size(221, 20);
            this.butSaveWinToText.Text = "SaveToText(\"c:\\log2.txt\")";
            this.butSaveWinToText.Click += new System.EventHandler(this.butSaveWinToText_Click);
            // 
            // butHelloWin
            // 
            this.butHelloWin.Location = new System.Drawing.Point(8, 87);
            this.butHelloWin.Size = new System.Drawing.Size(221, 20);
            this.butHelloWin.Text = "Say Hello";
            this.butHelloWin.Click += new System.EventHandler(this.butHelloWin_Click);
            // 
            // butDisplayWindow
            // 
            this.butDisplayWindow.Location = new System.Drawing.Point(8, 51);
            this.butDisplayWindow.Size = new System.Drawing.Size(221, 20);
            this.butDisplayWindow.Text = "display that window on the viewer";
            this.butDisplayWindow.Click += new System.EventHandler(this.butDisplayWindow_Click);
            // 
            // butCreateWindow
            // 
            this.butCreateWindow.Location = new System.Drawing.Point(8, 15);
            this.butCreateWindow.Size = new System.Drawing.Size(221, 20);
            this.butCreateWindow.Text = "Create a new trace window";
            this.butCreateWindow.Click += new System.EventHandler(this.butCreateWindow_Click);
            // 
            // tabPage4
            // 
            this.tabPage4.Controls.Add(this.butClearWinWatches);
            this.tabPage4.Controls.Add(this.butDisplayWinWatches);
            this.tabPage4.Controls.Add(this.butSendWinWatches);
            this.tabPage4.Controls.Add(this.butCreateWinWatches);
            this.tabPage4.Controls.Add(this.butClearMainWatches);
            this.tabPage4.Controls.Add(this.butDisplayMainWatches);
            this.tabPage4.Controls.Add(this.butSendMainWatches);
            this.tabPage4.Controls.Add(this.label2);
            this.tabPage4.Location = new System.Drawing.Point(0, 0);
            this.tabPage4.Size = new System.Drawing.Size(240, 268);
            this.tabPage4.Text = "Watches";
            // 
            // butClearWinWatches
            // 
            this.butClearWinWatches.Location = new System.Drawing.Point(42, 230);
            this.butClearWinWatches.Size = new System.Drawing.Size(180, 20);
            this.butClearWinWatches.Text = "Clear watch window";
            this.butClearWinWatches.Click += new System.EventHandler(this.butClearWinWatches_Click);
            // 
            // butDisplayWinWatches
            // 
            this.butDisplayWinWatches.Location = new System.Drawing.Point(42, 196);
            this.butDisplayWinWatches.Size = new System.Drawing.Size(180, 20);
            this.butDisplayWinWatches.Text = " Display watch window";
            this.butDisplayWinWatches.Click += new System.EventHandler(this.butDisplayWinWatches_Click);
            // 
            // butSendWinWatches
            // 
            this.butSendWinWatches.Location = new System.Drawing.Point(42, 162);
            this.butSendWinWatches.Size = new System.Drawing.Size(180, 20);
            this.butSendWinWatches.Text = "Send watches";
            this.butSendWinWatches.Click += new System.EventHandler(this.butSendWinWatches_Click);
            // 
            // butCreateWinWatches
            // 
            this.butCreateWinWatches.Location = new System.Drawing.Point(7, 128);
            this.butCreateWinWatches.Size = new System.Drawing.Size(168, 20);
            this.butCreateWinWatches.Text = "Create new WinWatches";
            this.butCreateWinWatches.Click += new System.EventHandler(this.butCreateWinWatches_Click);
            // 
            // butClearMainWatches
            // 
            this.butClearMainWatches.Location = new System.Drawing.Point(42, 94);
            this.butClearMainWatches.Size = new System.Drawing.Size(180, 20);
            this.butClearMainWatches.Text = "Clear main watch win";
            this.butClearMainWatches.Click += new System.EventHandler(this.butClearMainWatches_Click);
            // 
            // butDisplayMainWatches
            // 
            this.butDisplayMainWatches.Location = new System.Drawing.Point(42, 60);
            this.butDisplayMainWatches.Size = new System.Drawing.Size(180, 20);
            this.butDisplayMainWatches.Text = " Display main watch win";
            this.butDisplayMainWatches.Click += new System.EventHandler(this.butDisplayMainWatches_Click);
            // 
            // butSendMainWatches
            // 
            this.butSendMainWatches.Location = new System.Drawing.Point(42, 26);
            this.butSendMainWatches.Size = new System.Drawing.Size(180, 20);
            this.butSendMainWatches.Text = "Send watches";
            this.butSendMainWatches.Click += new System.EventHandler(this.butSendMainWatches_Click);
            // 
            // label2
            // 
            this.label2.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
            this.label2.Location = new System.Drawing.Point(0, 4);
            this.label2.Size = new System.Drawing.Size(237, 19);
            this.label2.Text = "Main watch window";
            // 
            // PPC2003CF1
            // 
            this.ClientSize = new System.Drawing.Size(240, 294);
            this.Controls.Add(this.tabControl1);
            this.Text = "TraceTool for Dot net CF 1 demo";

        }

        #endregion

        //--------------------------------------------------------------------------------------------

        void CheckSocket(string StrAdr)
        {
            IPAddress adr;
            IPHostEntry host = null;
            // get host
            try
            {
                // CF1
                host = Dns.GetHostByName(StrAdr);

                // CF2
                //host = Dns.GetHostEntry(StrAdr);

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
