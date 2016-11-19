using System;
using System.Drawing;
using System.Collections;
using System.Windows.Forms;
using System.Data;
using System.Net;
using System.Net.Sockets;
using OpenNETCF.Win32;
using TraceTool ;

namespace Vs7_PPC2003_CF1_Tracetool_Demo
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class DemoForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.TabPage tabBasic;
		private System.Windows.Forms.Button ButPartner;
		private System.Windows.Forms.Button ButShowHosts;
		private System.Windows.Forms.Button butSample;
		private System.Windows.Forms.Button butIndent;
		private System.Windows.Forms.Button butSaveTotext;
		private System.Windows.Forms.Button butSaveToXml;
		private System.Windows.Forms.Button butLoadXml;
		private System.Windows.Forms.Button butShowViewer;
		private System.Windows.Forms.Button butClearMainTraces;
		private System.Windows.Forms.TabControl tabPages;
		private System.Windows.Forms.TabPage tabNode;
		private System.Windows.Forms.TabPage tabMultiPages;
		private System.Windows.Forms.TabPage tabWatches;
		private System.Windows.Forms.Button butStart1;
		private System.Windows.Forms.Button butResend;
		private System.Windows.Forms.Button butSetSelected;
		private System.Windows.Forms.Button butNodeIndent;
		private System.Windows.Forms.Button butstart2;
		private System.Windows.Forms.Button butAppend;
		private System.Windows.Forms.Button butShowNode;
		private System.Windows.Forms.Button butCreateWindow;
		private System.Windows.Forms.Button butDisplayWindow;
		private System.Windows.Forms.Button butHelloWin;
		private System.Windows.Forms.Button butSaveWinToText;
		private System.Windows.Forms.Button butSaveWinToXml;
		private System.Windows.Forms.Button butLoadWinXml;
		private System.Windows.Forms.Button butMulticolTest;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Button butSendMainWatches;
		private System.Windows.Forms.Button butClearMainWatches;
		private System.Windows.Forms.Button butDisplayMainWatches;
		private System.Windows.Forms.Button butCreateWinWatches;
		private System.Windows.Forms.Button butSendWinWatches;
		private System.Windows.Forms.Button butClearWinWatches;
		private System.Windows.Forms.Button butDisplayWinWatches;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.MainMenu mainMenu1;

		public DemoForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}
		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			base.Dispose( disposing );
		}
		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.tabPages = new System.Windows.Forms.TabControl();
			this.tabBasic = new System.Windows.Forms.TabPage();
			this.label2 = new System.Windows.Forms.Label();
			this.butClearMainTraces = new System.Windows.Forms.Button();
			this.butShowViewer = new System.Windows.Forms.Button();
			this.butLoadXml = new System.Windows.Forms.Button();
			this.butSaveToXml = new System.Windows.Forms.Button();
			this.butSaveTotext = new System.Windows.Forms.Button();
			this.butIndent = new System.Windows.Forms.Button();
			this.butSample = new System.Windows.Forms.Button();
			this.ButShowHosts = new System.Windows.Forms.Button();
			this.ButPartner = new System.Windows.Forms.Button();
			this.tabNode = new System.Windows.Forms.TabPage();
			this.butShowNode = new System.Windows.Forms.Button();
			this.butAppend = new System.Windows.Forms.Button();
			this.butstart2 = new System.Windows.Forms.Button();
			this.butNodeIndent = new System.Windows.Forms.Button();
			this.butSetSelected = new System.Windows.Forms.Button();
			this.butResend = new System.Windows.Forms.Button();
			this.butStart1 = new System.Windows.Forms.Button();
			this.tabMultiPages = new System.Windows.Forms.TabPage();
			this.butMulticolTest = new System.Windows.Forms.Button();
			this.butLoadWinXml = new System.Windows.Forms.Button();
			this.butSaveWinToXml = new System.Windows.Forms.Button();
			this.butSaveWinToText = new System.Windows.Forms.Button();
			this.butHelloWin = new System.Windows.Forms.Button();
			this.butDisplayWindow = new System.Windows.Forms.Button();
			this.butCreateWindow = new System.Windows.Forms.Button();
			this.tabWatches = new System.Windows.Forms.TabPage();
			this.butDisplayWinWatches = new System.Windows.Forms.Button();
			this.butClearWinWatches = new System.Windows.Forms.Button();
			this.butSendWinWatches = new System.Windows.Forms.Button();
			this.butCreateWinWatches = new System.Windows.Forms.Button();
			this.butDisplayMainWatches = new System.Windows.Forms.Button();
			this.butClearMainWatches = new System.Windows.Forms.Button();
			this.butSendMainWatches = new System.Windows.Forms.Button();
			this.label1 = new System.Windows.Forms.Label();
			// 
			// tabPages
			// 
			this.tabPages.Controls.Add(this.tabBasic);
			this.tabPages.Controls.Add(this.tabNode);
			this.tabPages.Controls.Add(this.tabMultiPages);
			this.tabPages.Controls.Add(this.tabWatches);
			this.tabPages.SelectedIndex = 0;
			this.tabPages.Size = new System.Drawing.Size(249, 268);
			// 
			// tabBasic
			// 
			this.tabBasic.Controls.Add(this.label2);
			this.tabBasic.Controls.Add(this.butClearMainTraces);
			this.tabBasic.Controls.Add(this.butShowViewer);
			this.tabBasic.Controls.Add(this.butLoadXml);
			this.tabBasic.Controls.Add(this.butSaveToXml);
			this.tabBasic.Controls.Add(this.butSaveTotext);
			this.tabBasic.Controls.Add(this.butIndent);
			this.tabBasic.Controls.Add(this.butSample);
			this.tabBasic.Controls.Add(this.ButShowHosts);
			this.tabBasic.Controls.Add(this.ButPartner);
			this.tabBasic.Location = new System.Drawing.Point(4, 4);
			this.tabBasic.Size = new System.Drawing.Size(241, 242);
			this.tabBasic.Text = "Basic";
			// 
			// label2
			// 
			this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 7F, System.Drawing.FontStyle.Regular);
			this.label2.Location = new System.Drawing.Point(3, 2);
			this.label2.Size = new System.Drawing.Size(240, 30);
			this.label2.Text = "Tips : Ensure TraceTool is running. If the demo run under emulator, don\'t forget " +
				"to cradle";
			// 
			// butClearMainTraces
			// 
			this.butClearMainTraces.Location = new System.Drawing.Point(120, 208);
			this.butClearMainTraces.Size = new System.Drawing.Size(112, 20);
			this.butClearMainTraces.Text = "Clear main traces";
			this.butClearMainTraces.Click += new System.EventHandler(this.butClearMainTraces_Click);
			// 
			// butShowViewer
			// 
			this.butShowViewer.Location = new System.Drawing.Point(8, 208);
			this.butShowViewer.Size = new System.Drawing.Size(104, 20);
			this.butShowViewer.Text = "Show Viewer";
			this.butShowViewer.Click += new System.EventHandler(this.butShowViewer_Click);
			// 
			// butLoadXml
			// 
			this.butLoadXml.Location = new System.Drawing.Point(8, 168);
			this.butLoadXml.Size = new System.Drawing.Size(224, 20);
			this.butLoadXml.Text = "LoadXml(\"c:\\log.xml\")";
			this.butLoadXml.Click += new System.EventHandler(this.butLoadXml_Click);
			// 
			// butSaveToXml
			// 
			this.butSaveToXml.Location = new System.Drawing.Point(8, 136);
			this.butSaveToXml.Size = new System.Drawing.Size(224, 20);
			this.butSaveToXml.Text = "SaveToXml(\"c:\\log.xml\")";
			this.butSaveToXml.Click += new System.EventHandler(this.butSaveToXml_Click);
			// 
			// butSaveTotext
			// 
			this.butSaveTotext.Location = new System.Drawing.Point(8, 104);
			this.butSaveTotext.Size = new System.Drawing.Size(224, 20);
			this.butSaveTotext.Text = "SaveToText(\"c:\\log.txt\")";
			this.butSaveTotext.Click += new System.EventHandler(this.butSaveTotext_Click);
			// 
			// butIndent
			// 
			this.butIndent.Location = new System.Drawing.Point(120, 72);
			this.butIndent.Size = new System.Drawing.Size(112, 20);
			this.butIndent.Text = "Indent/Unindent";
			this.butIndent.Click += new System.EventHandler(this.butIndent_Click);
			// 
			// butSample
			// 
			this.butSample.Location = new System.Drawing.Point(8, 72);
			this.butSample.Size = new System.Drawing.Size(104, 20);
			this.butSample.Text = "Sample traces";
			this.butSample.Click += new System.EventHandler(this.butSample_Click);
			// 
			// ButShowHosts
			// 
			this.ButShowHosts.Location = new System.Drawing.Point(120, 40);
			this.ButShowHosts.Size = new System.Drawing.Size(112, 20);
			this.ButShowHosts.Text = "Show Hosts";
			this.ButShowHosts.Click += new System.EventHandler(this.ButShowHosts_Click);
			// 
			// ButPartner
			// 
			this.ButPartner.Location = new System.Drawing.Point(8, 40);
			this.ButPartner.Size = new System.Drawing.Size(104, 20);
			this.ButPartner.Text = "Show Partner";
			this.ButPartner.Click += new System.EventHandler(this.ButPartner_Click);
			// 
			// tabNode
			// 
			this.tabNode.Controls.Add(this.butShowNode);
			this.tabNode.Controls.Add(this.butAppend);
			this.tabNode.Controls.Add(this.butstart2);
			this.tabNode.Controls.Add(this.butNodeIndent);
			this.tabNode.Controls.Add(this.butSetSelected);
			this.tabNode.Controls.Add(this.butResend);
			this.tabNode.Controls.Add(this.butStart1);
			this.tabNode.Location = new System.Drawing.Point(4, 4);
			this.tabNode.Size = new System.Drawing.Size(241, 242);
			this.tabNode.Text = "Node Op.";
			// 
			// butShowNode
			// 
			this.butShowNode.Location = new System.Drawing.Point(48, 200);
			this.butShowNode.Size = new System.Drawing.Size(112, 20);
			this.butShowNode.Text = "Show()";
			this.butShowNode.Click += new System.EventHandler(this.butShowNode_Click);
			// 
			// butAppend
			// 
			this.butAppend.Location = new System.Drawing.Point(48, 168);
			this.butAppend.Size = new System.Drawing.Size(112, 20);
			this.butAppend.Text = "Append()";
			this.butAppend.Click += new System.EventHandler(this.butAppend_Click);
			// 
			// butstart2
			// 
			this.butstart2.Location = new System.Drawing.Point(16, 136);
			this.butstart2.Text = "start2";
			this.butstart2.Click += new System.EventHandler(this.butstart2_Click);
			// 
			// butNodeIndent
			// 
			this.butNodeIndent.Location = new System.Drawing.Point(48, 104);
			this.butNodeIndent.Size = new System.Drawing.Size(112, 20);
			this.butNodeIndent.Text = "Indent/Unindent";
			this.butNodeIndent.Click += new System.EventHandler(this.butNodeIndent_Click);
			// 
			// butSetSelected
			// 
			this.butSetSelected.Location = new System.Drawing.Point(48, 72);
			this.butSetSelected.Size = new System.Drawing.Size(112, 20);
			this.butSetSelected.Text = "SetSelected()";
			this.butSetSelected.Click += new System.EventHandler(this.butSetSelected_Click);
			// 
			// butResend
			// 
			this.butResend.Location = new System.Drawing.Point(48, 40);
			this.butResend.Size = new System.Drawing.Size(112, 20);
			this.butResend.Text = "Resend()";
			this.butResend.Click += new System.EventHandler(this.butResend_Click);
			// 
			// butStart1
			// 
			this.butStart1.Location = new System.Drawing.Point(16, 8);
			this.butStart1.Text = "Start1";
			this.butStart1.Click += new System.EventHandler(this.butStart1_Click);
			// 
			// tabMultiPages
			// 
			this.tabMultiPages.Controls.Add(this.butMulticolTest);
			this.tabMultiPages.Controls.Add(this.butLoadWinXml);
			this.tabMultiPages.Controls.Add(this.butSaveWinToXml);
			this.tabMultiPages.Controls.Add(this.butSaveWinToText);
			this.tabMultiPages.Controls.Add(this.butHelloWin);
			this.tabMultiPages.Controls.Add(this.butDisplayWindow);
			this.tabMultiPages.Controls.Add(this.butCreateWindow);
			this.tabMultiPages.Location = new System.Drawing.Point(4, 4);
			this.tabMultiPages.Size = new System.Drawing.Size(241, 242);
			this.tabMultiPages.Text = "Multi Pages";
			// 
			// butMulticolTest
			// 
			this.butMulticolTest.Location = new System.Drawing.Point(8, 216);
			this.butMulticolTest.Size = new System.Drawing.Size(224, 20);
			this.butMulticolTest.Text = "Multi col test";
			this.butMulticolTest.Click += new System.EventHandler(this.butMulticolTest_Click);
			// 
			// butLoadWinXml
			// 
			this.butLoadWinXml.Enabled = false;
			this.butLoadWinXml.Location = new System.Drawing.Point(8, 176);
			this.butLoadWinXml.Size = new System.Drawing.Size(224, 20);
			this.butLoadWinXml.Text = "LoadXml(\"c:\\log2.xml\")";
			this.butLoadWinXml.Click += new System.EventHandler(this.butLoadWinXml_Click);
			// 
			// butSaveWinToXml
			// 
			this.butSaveWinToXml.Enabled = false;
			this.butSaveWinToXml.Location = new System.Drawing.Point(8, 144);
			this.butSaveWinToXml.Size = new System.Drawing.Size(224, 20);
			this.butSaveWinToXml.Text = "SaveToXml(\"c:\\log2.xml\")";
			this.butSaveWinToXml.Click += new System.EventHandler(this.butSaveWinToXml_Click);
			// 
			// butSaveWinToText
			// 
			this.butSaveWinToText.Enabled = false;
			this.butSaveWinToText.Location = new System.Drawing.Point(8, 112);
			this.butSaveWinToText.Size = new System.Drawing.Size(224, 20);
			this.butSaveWinToText.Text = "SaveToText(\"c:\\log2.txt\")";
			this.butSaveWinToText.Click += new System.EventHandler(this.butSaveWinToText_Click);
			// 
			// butHelloWin
			// 
			this.butHelloWin.Enabled = false;
			this.butHelloWin.Location = new System.Drawing.Point(8, 72);
			this.butHelloWin.Size = new System.Drawing.Size(224, 20);
			this.butHelloWin.Text = "Say Hello";
			this.butHelloWin.Click += new System.EventHandler(this.butHelloWin_Click);
			// 
			// butDisplayWindow
			// 
			this.butDisplayWindow.Enabled = false;
			this.butDisplayWindow.Location = new System.Drawing.Point(8, 40);
			this.butDisplayWindow.Size = new System.Drawing.Size(224, 20);
			this.butDisplayWindow.Text = "display that window on the viewer";
			this.butDisplayWindow.Click += new System.EventHandler(this.butDisplayWindow_Click);
			// 
			// butCreateWindow
			// 
			this.butCreateWindow.Location = new System.Drawing.Point(8, 8);
			this.butCreateWindow.Size = new System.Drawing.Size(224, 20);
			this.butCreateWindow.Text = "Create a new trace window";
			this.butCreateWindow.Click += new System.EventHandler(this.butCreateWindow_Click);
			// 
			// tabWatches
			// 
			this.tabWatches.Controls.Add(this.butDisplayWinWatches);
			this.tabWatches.Controls.Add(this.butClearWinWatches);
			this.tabWatches.Controls.Add(this.butSendWinWatches);
			this.tabWatches.Controls.Add(this.butCreateWinWatches);
			this.tabWatches.Controls.Add(this.butDisplayMainWatches);
			this.tabWatches.Controls.Add(this.butClearMainWatches);
			this.tabWatches.Controls.Add(this.butSendMainWatches);
			this.tabWatches.Controls.Add(this.label1);
			this.tabWatches.Location = new System.Drawing.Point(4, 4);
			this.tabWatches.Size = new System.Drawing.Size(241, 242);
			this.tabWatches.Text = "Watches";
			// 
			// butDisplayWinWatches
			// 
			this.butDisplayWinWatches.Location = new System.Drawing.Point(56, 184);
			this.butDisplayWinWatches.Size = new System.Drawing.Size(152, 20);
			this.butDisplayWinWatches.Text = " Display Watch Window";
			this.butDisplayWinWatches.Click += new System.EventHandler(this.butDisplayWinWatches_Click);
			// 
			// butClearWinWatches
			// 
			this.butClearWinWatches.Location = new System.Drawing.Point(56, 216);
			this.butClearWinWatches.Size = new System.Drawing.Size(152, 20);
			this.butClearWinWatches.Text = "clear Watch Window";
			this.butClearWinWatches.Click += new System.EventHandler(this.butClearWinWatches_Click);
			// 
			// butSendWinWatches
			// 
			this.butSendWinWatches.Location = new System.Drawing.Point(56, 152);
			this.butSendWinWatches.Size = new System.Drawing.Size(152, 20);
			this.butSendWinWatches.Text = "Send Watches";
			this.butSendWinWatches.Click += new System.EventHandler(this.butSendWinWatches_Click);
			// 
			// butCreateWinWatches
			// 
			this.butCreateWinWatches.Location = new System.Drawing.Point(8, 120);
			this.butCreateWinWatches.Size = new System.Drawing.Size(160, 20);
			this.butCreateWinWatches.Text = "Create new WinWatches";
			this.butCreateWinWatches.Click += new System.EventHandler(this.butCreateWinWatches_Click);
			// 
			// butDisplayMainWatches
			// 
			this.butDisplayMainWatches.Location = new System.Drawing.Point(56, 56);
			this.butDisplayMainWatches.Size = new System.Drawing.Size(152, 20);
			this.butDisplayMainWatches.Text = " Display Watch Window";
			this.butDisplayMainWatches.Click += new System.EventHandler(this.butDisplayMainWatches_Click);
			// 
			// butClearMainWatches
			// 
			this.butClearMainWatches.Location = new System.Drawing.Point(56, 88);
			this.butClearMainWatches.Size = new System.Drawing.Size(152, 20);
			this.butClearMainWatches.Text = "clear Watch Window";
			this.butClearMainWatches.Click += new System.EventHandler(this.butClearMainWatches_Click);
			// 
			// butSendMainWatches
			// 
			this.butSendMainWatches.Location = new System.Drawing.Point(56, 24);
			this.butSendMainWatches.Size = new System.Drawing.Size(152, 20);
			this.butSendMainWatches.Text = "Send Watches";
			this.butSendMainWatches.Click += new System.EventHandler(this.butSendMainWatches_Click);
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(8, 3);
			this.label1.Size = new System.Drawing.Size(192, 16);
			this.label1.Text = "Main watch window";
			// 
			// DemoForm
			// 
			this.ClientSize = new System.Drawing.Size(249, 268);
			this.Controls.Add(this.tabPages);
			this.Menu = this.mainMenu1;
			this.Text = "TraceTool Demo";

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>

		static void Main() 
		{
			Application.Run(new DemoForm());
		}

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
				MessageBox.Show (ex.GetType().ToString(),StrAdr) ;
				MessageBox.Show (ex.Message,StrAdr) ;
				return;
			}

			if (host == null)
			{
				MessageBox.Show("host is null",StrAdr) ;
				return;
			}

			adr = host.AddressList[0];
			MessageBox.Show(adr.ToString(),StrAdr);

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
				MessageBox.Show (ex.GetType().ToString(),StrAdr) ;
				MessageBox.Show (ex.Message,StrAdr) ;
				return;
			}
			MessageBox.Show ("Connection succeed",StrAdr);
		}

		//--------------------------------------------------------------------------------------------
		
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
			TTrace.Debug.Send ("VS7 ppc2003 CF1 demo") ;

			TTrace.Options.SendProcessName = false ;
			string str =  '\u2250' + "qwerty é ù è azerty" + '\u9999' ;   

			// simple traces
			TTrace.Debug.Send ("Hello").Send ("World") ;  // "World" is a sub trace of "Hello"

			// traces using Sendxxx method
			// Use default display filter. (see TTrace.Options)

			TTrace.Debug.SendType ("SendType 'Trace node Type'",   TTrace.Debug.GetType() ); 
			TTrace.Debug.SendObject   ("My const",          TraceConst.CST_CREATE_MEMBER) ;
			TTrace.Debug.SendDump     ("SendDump test" , "Unicode", System.Text.Encoding.Unicode.GetBytes(str) ,50) ;
        
			// TTrace.Debug.SendType ("My abstract type" , typeof (Base));  // same as Type.GetType("Project1.Base")

			// traces using TraceNodeEx
			TraceNodeEx node = new TraceNodeEx (null) ;  //  TTrace.Debug
			node.LeftMsg = "TraceNodeEx" ;
			node.RightMsg = "demo" ;
			node.IconIndex = 8 ;
			node.AddDump ("ASCII"  , System.Text.Encoding.ASCII.GetBytes(str) ,50) ;   // 3F 61 7A          ..... 3F
			node.AddDump ("UTF8"   , System.Text.Encoding.UTF8.GetBytes(str) ,50) ; 
			node.AddDump ("Unicode", System.Text.Encoding.Unicode.GetBytes(str) ,50) ; // 50 22 61 00 7A 00 ..... 99 99
			node.Send () ;


			// specify what to send (modifiers, fields, ...). Can be slow on complexe objects
			TraceDisplayFlags flags = TraceDisplayFlags.ShowModifiers|
				TraceDisplayFlags.ShowInheritedMembers|
				TraceDisplayFlags.ShowNonPublic|
				TraceDisplayFlags.ShowFields ;

			TTrace.Error.SendObject ("SendObject 'Trace node Object'" , TTrace.Debug , flags) ;

			TTrace.Flush() ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butIndent_Click(object sender, System.EventArgs e)
		{
			// Indent and UnIndent 
			TTrace.Debug.Indent ("Before", "some work");
			TTrace.Debug.Indent ("Level1") ;
			TTrace.Debug.Send ("Level2") ;
			TTrace.Debug.Send ("More level2") ;
			TTrace.Debug.UnIndent ("Done" , "level 1") ;
			TTrace.Debug.UnIndent ("Work is done") ;	
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
			TTrace.Show(true) ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butClearMainTraces_Click(object sender, System.EventArgs e)
		{
			TTrace.ClearAll() ;
		}

		//--------------------------------------------------------------------------------------------
		
		TraceNode start1 = null ;
		TraceNode start2 = null  ;

		private void butStart1_Click(object sender, System.EventArgs e)
		{
			if (start1 == null)
			{
				start1 = TTrace.Debug.Send ("Start 1 ..") ;
			}
		}

		//--------------------------------------------------------------------------------------------
		
		private void butResend_Click(object sender, System.EventArgs e)
		{
			if (start1 == null)
				return ;
   
			start1.ResendRight ("Done 1") ;		
		}

		//--------------------------------------------------------------------------------------------
		
		private void butSetSelected_Click(object sender, System.EventArgs e)
		{
			if (start1 == null)
				return ;

			start1.SetSelected() ;   
		}

		//--------------------------------------------------------------------------------------------
		
		private void butNodeIndent_Click(object sender, System.EventArgs e)
		{
			if (start1 == null)
				return ;

			start1.Send ("before indent") ;    // send text under the start1 node
			start1.Indent ("ident 1") ;        // send text under the start1 node and keep it this trace as the new target for further sub traces
			start1.Send ("Level2") ;           // send text under the "indent 1" node
			start1.UnIndent ("done") ;         // unindent and send text under the start1 node. Text is optional
		}

		//--------------------------------------------------------------------------------------------
		
		private void butstart2_Click(object sender, System.EventArgs e)
		{
			if (start2 == null)
			{
				start2 = TTrace.Debug.Send ("Start 2 ..") ;
			}
		}

		//--------------------------------------------------------------------------------------------
		
		private void butAppend_Click(object sender, System.EventArgs e)
		{
			if (start2 == null)
				return ;

			start2.AppendLeft ("..Done") ;   // Append left part
		}

		//--------------------------------------------------------------------------------------------
		
		private void butShowNode_Click(object sender, System.EventArgs e)
		{
			if (start2 == null)
				return ;

			start2.Show() ; 	
		}

		//--------------------------------------------------------------------------------------------
		
		private WinTrace myWinTrace ;
		
		private void butCreateWindow_Click(object sender, System.EventArgs e)
		{
			myWinTrace = new WinTrace ("MyWINID" , "My trace window") ;
			butDisplayWindow.Enabled = true ;
			butHelloWin.Enabled = true ;
			butSaveWinToText.Enabled = true ;
			butSaveWinToXml.Enabled = true ;
			butLoadWinXml.Enabled = true ;         		
		}

		//--------------------------------------------------------------------------------------------
		
		private void butDisplayWindow_Click(object sender, System.EventArgs e)
		{
			myWinTrace.DisplayWin() ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butHelloWin_Click(object sender, System.EventArgs e)
		{
			myWinTrace.Debug.Send ("Hello", "Can be used to store exceptions, for examples"); 
		}

		//--------------------------------------------------------------------------------------------
		
		private void butSaveWinToText_Click(object sender, System.EventArgs e)
		{
			myWinTrace.SaveToTextfile ("c:\\log2.txt") ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butSaveWinToXml_Click(object sender, System.EventArgs e)
		{
			myWinTrace.SaveToXml ("c:\\log2.xml") ; 
		}

		//--------------------------------------------------------------------------------------------
		
		private void butLoadWinXml_Click(object sender, System.EventArgs e)
		{
		myWinTrace.LoadXml ("c:\\log2.xml") ;
		}

		//--------------------------------------------------------------------------------------------
		
		WinTrace MultiColTrace ;
		private void butMulticolTest_Click(object sender, System.EventArgs e)
		{
			if (MultiColTrace == null) 
			{
				MultiColTrace = new WinTrace ("MCOL" , "MultiCol trace window") ;
				MultiColTrace.SetMultiColumn (1) ;  // must be called before calling setColumnsTitle
				MultiColTrace.SetColumnsTitle("col1 \t col2 \t col3");
				MultiColTrace.SetColumnsWidth("100:20:80 \t 200:50 \t 100");
				MultiColTrace.DisplayWin() ;
			}
			MultiColTrace.Debug.Send("1 \t 2 \t 3") ;
		}

		//--------------------------------------------------------------------------------------------
		int WatchCounter = 123 ;
		private void butSendMainWatches_Click(object sender, System.EventArgs e)
		{
			TTrace.Watches.Send("test2", WatchCounter); 
		    WatchCounter++ ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butClearMainWatches_Click(object sender, System.EventArgs e)
		{
			TTrace.Watches.ClearAll() ; 
		}

		//--------------------------------------------------------------------------------------------
		
		private void butDisplayMainWatches_Click(object sender, System.EventArgs e)
		{
			TTrace.Watches.DisplayWin() ; 
		}

		//--------------------------------------------------------------------------------------------
		
		private WinWatch MyWinWatch ;
		private void butCreateWinWatches_Click(object sender, System.EventArgs e)
		{
			MyWinWatch = new WinWatch ("MyWinWatchID" , "My watches")  ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butSendWinWatches_Click(object sender, System.EventArgs e)
		{
			if (MyWinWatch != null) 
				MyWinWatch.Send ( "Now", DateTime.Now.ToString("HH:mm:ss:fff") ) ;		
		}

		//--------------------------------------------------------------------------------------------
		
		private void butClearWinWatches_Click(object sender, System.EventArgs e)
		{
			if (MyWinWatch != null) 
				MyWinWatch.ClearAll () ;
		}

		//--------------------------------------------------------------------------------------------
		
		private void butDisplayWinWatches_Click(object sender, System.EventArgs e)
		{
			if (MyWinWatch != null) 
				MyWinWatch.DisplayWin () ;
		}
	}
}
