// winform.cs
//
// Author : Thierry Parent
// Version : 10.1
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

// [DefaultMemberAttribute]
   // for DDL import
using System;
using System.Collections;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Drawing;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;
using TraceTool;

namespace Project1
{
   /// <summary>
   /// Summary description for WinForm.
   /// </summary>
   public class WinForm : Form
   {
      /// <summary>
      /// Required designer variable.
      /// </summary>
      private Container components = null;
      TestClass testClass = new TestClass();
      private System.Windows.Forms.Button MulticolBut;
      private System.Windows.Forms.Button IndentButton;
      private System.Windows.Forms.TabPage tabPage5;
      private System.Windows.Forms.Label label4;
      private System.Windows.Forms.Button butWatch;
      private System.Windows.Forms.Button butClearWatchWindow;
      private System.Windows.Forms.Button butDisplayWatchWindow;
      private System.Windows.Forms.Button butCreateWinWatch;
      private System.Windows.Forms.Button butWinWatchSend;
      private System.Windows.Forms.Button butWinWatchClear;
      private System.Windows.Forms.Button butWinWatchDisplay;
      private System.Windows.Forms.PictureBox pictureBox1;
      public static object ShowViewerButton ;


      public WinForm()
      {
         //
         // Required for Windows Form Designer support
         //
         InitializeComponent();
         ShowViewerButton = butShowtrace ;

      }

      /// <summary>
      /// Clean up any resources being used.
      /// </summary>
      protected override void Dispose (bool disposing)
      {
         if (disposing)
         {
            if (components != null)
            {
               components.Dispose();
            }
         }
         base.Dispose(disposing);
      }

      #region Windows Form Designer generated code
      /// <summary>
      /// Required method for Designer support - do not modify
      /// the contents of this method with the code editor.
      /// </summary>
      private void InitializeComponent()
      {
         System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(WinForm));
         this.butOldTrace = new System.Windows.Forms.Button();
         this.butTail = new System.Windows.Forms.Button();
         this.label3 = new System.Windows.Forms.Label();
         this.butDiagnostic = new System.Windows.Forms.Button();
         this.butStart1 = new System.Windows.Forms.Button();
         this.tabControl1 = new System.Windows.Forms.TabControl();
         this.tabPage1 = new System.Windows.Forms.TabPage();
         this.butLoadXml = new System.Windows.Forms.Button();
         this.butSaveToXml = new System.Windows.Forms.Button();
         this.butSaveToTXT = new System.Windows.Forms.Button();
         this.groupBox3 = new System.Windows.Forms.GroupBox();
         this.IndentButton = new System.Windows.Forms.Button();
         this.MulticolBut = new System.Windows.Forms.Button();
         this.chkSendProcessName = new System.Windows.Forms.CheckBox();
         this.label1 = new System.Windows.Forms.Label();
         this.chkSendEvents = new System.Windows.Forms.CheckBox();
         this.chkSendInherited = new System.Windows.Forms.CheckBox();
         this.chkSendFunctions = new System.Windows.Forms.CheckBox();
         this.comboBox1 = new System.Windows.Forms.ComboBox();
         this.butTrace = new System.Windows.Forms.Button();
         this.label2 = new System.Windows.Forms.Label();
         this.butDoc = new System.Windows.Forms.Button();
         this.butFullInfo = new System.Windows.Forms.Button();
         this.butVariant = new System.Windows.Forms.Button();
         this.butClear = new System.Windows.Forms.Button();
         this.tabPage3 = new System.Windows.Forms.TabPage();
         this.butEventLog = new System.Windows.Forms.Button();
         this.groupBox2 = new System.Windows.Forms.GroupBox();
         this.groupBox1 = new System.Windows.Forms.GroupBox();
         this.butListenerWriteLine = new System.Windows.Forms.Button();
         this.butListenerInit = new System.Windows.Forms.Button();
         this.butListenerWrite = new System.Windows.Forms.Button();
         this.tabPage4 = new System.Windows.Forms.TabPage();
         this.butWinLoadXml = new System.Windows.Forms.Button();
         this.butSaveWinToXml = new System.Windows.Forms.Button();
         this.butSaveWinToTxt = new System.Windows.Forms.Button();
         this.butClearWin = new System.Windows.Forms.Button();
         this.butDisplayWin = new System.Windows.Forms.Button();
         this.butHelloToWintrace = new System.Windows.Forms.Button();
         this.butCreateWinTrace = new System.Windows.Forms.Button();
         this.tabPage5 = new System.Windows.Forms.TabPage();
         this.label4 = new System.Windows.Forms.Label();
         this.butWinWatchDisplay = new System.Windows.Forms.Button();
         this.butWinWatchClear = new System.Windows.Forms.Button();
         this.butWinWatchSend = new System.Windows.Forms.Button();
         this.butCreateWinWatch = new System.Windows.Forms.Button();
         this.butDisplayWatchWindow = new System.Windows.Forms.Button();
         this.butClearWatchWindow = new System.Windows.Forms.Button();
         this.butWatch = new System.Windows.Forms.Button();
         this.tabPage2 = new System.Windows.Forms.TabPage();
         this.butShowNode = new System.Windows.Forms.Button();
         this.butSetSelected = new System.Windows.Forms.Button();
         this.butEnd2 = new System.Windows.Forms.Button();
         this.butstart2 = new System.Windows.Forms.Button();
         this.butDone1 = new System.Windows.Forms.Button();
         this.butShowtrace = new System.Windows.Forms.Button();
         this.panel1 = new System.Windows.Forms.Panel();
         this.pictureBox1 = new System.Windows.Forms.PictureBox();
         this.tabControl1.SuspendLayout();
         this.tabPage1.SuspendLayout();
         this.groupBox3.SuspendLayout();
         this.tabPage3.SuspendLayout();
         this.groupBox2.SuspendLayout();
         this.groupBox1.SuspendLayout();
         this.tabPage4.SuspendLayout();
         this.tabPage5.SuspendLayout();
         this.tabPage2.SuspendLayout();
         this.panel1.SuspendLayout();
         this.SuspendLayout();
         //
         // butOldTrace
         //
         this.butOldTrace.Location = new System.Drawing.Point(24, 264);
         this.butOldTrace.Name = "butOldTrace";
         this.butOldTrace.Size = new System.Drawing.Size(192, 23);
         this.butOldTrace.TabIndex = 18;
         this.butOldTrace.Text = "OutputDebugString demo";
         this.butOldTrace.Click += new System.EventHandler(this.butOldTrace_Click);
         //
         // butTail
         //
         this.butTail.Location = new System.Drawing.Point(24, 312);
         this.butTail.Name = "butTail";
         this.butTail.Size = new System.Drawing.Size(192, 23);
         this.butTail.TabIndex = 19;
         this.butTail.Text = "Tail demo : Add lines to c:\\log.txt";
         this.butTail.Click += new System.EventHandler(this.butTail_Click);
         //
         // label3
         //
         this.label3.Location = new System.Drawing.Point(24, 22);
         this.label3.Name = "label3";
         this.label3.Size = new System.Drawing.Size(344, 32);
         this.label3.TabIndex = 27;
         this.label3.Text = "TraceTool receive messages on the ODS window when the debugger is not running";
         //
         // butDiagnostic
         //
         this.butDiagnostic.Location = new System.Drawing.Point(24, 64);
         this.butDiagnostic.Name = "butDiagnostic";
         this.butDiagnostic.Size = new System.Drawing.Size(176, 23);
         this.butDiagnostic.TabIndex = 26;
         this.butDiagnostic.Text = "System.Diagnostics.Debug";
         this.butDiagnostic.Click += new System.EventHandler(this.butDiagnostic_Click);
         //
         // butStart1
         //
         this.butStart1.Location = new System.Drawing.Point(16, 24);
         this.butStart1.Name = "butStart1";
         this.butStart1.TabIndex = 0;
         this.butStart1.Text = "Start 1...";
         this.butStart1.Click += new System.EventHandler(this.butStart1_Click);
         //
         // tabControl1
         //
         this.tabControl1.Controls.Add(this.tabPage1);
         this.tabControl1.Controls.Add(this.tabPage3);
         this.tabControl1.Controls.Add(this.tabPage4);
         this.tabControl1.Controls.Add(this.tabPage5);
         this.tabControl1.Controls.Add(this.tabPage2);
         this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
         this.tabControl1.Location = new System.Drawing.Point(0, 0);
         this.tabControl1.Name = "tabControl1";
         this.tabControl1.SelectedIndex = 0;
         this.tabControl1.Size = new System.Drawing.Size(672, 462);
         this.tabControl1.TabIndex = 0;
         //
         // tabPage1
         //
         this.tabPage1.Controls.Add(this.pictureBox1);
         this.tabPage1.Controls.Add(this.butLoadXml);
         this.tabPage1.Controls.Add(this.butSaveToXml);
         this.tabPage1.Controls.Add(this.butSaveToTXT);
         this.tabPage1.Controls.Add(this.groupBox3);
         this.tabPage1.Controls.Add(this.butDoc);
         this.tabPage1.Controls.Add(this.butFullInfo);
         this.tabPage1.Controls.Add(this.butVariant);
         this.tabPage1.Controls.Add(this.butClear);
         this.tabPage1.Location = new System.Drawing.Point(4, 22);
         this.tabPage1.Name = "tabPage1";
         this.tabPage1.Size = new System.Drawing.Size(664, 436);
         this.tabPage1.TabIndex = 0;
         this.tabPage1.Text = "Basic";
         //
         // butLoadXml
         //
         this.butLoadXml.Location = new System.Drawing.Point(224, 352);
         this.butLoadXml.Name = "butLoadXml";
         this.butLoadXml.Size = new System.Drawing.Size(160, 24);
         this.butLoadXml.TabIndex = 44;
         this.butLoadXml.Text = "LoadXml  (\"c:\\log.xml\")";
         this.butLoadXml.Click += new System.EventHandler(this.butLoadXml_Click_1);
         //
         // butSaveToXml
         //
         this.butSaveToXml.Location = new System.Drawing.Point(16, 352);
         this.butSaveToXml.Name = "butSaveToXml";
         this.butSaveToXml.Size = new System.Drawing.Size(168, 23);
         this.butSaveToXml.TabIndex = 39;
         this.butSaveToXml.Text = "Save To Xml file (\"c:\\log.xml\")";
         this.butSaveToXml.Click += new System.EventHandler(this.butSaveToXml_Click);
         //
         // butSaveToTXT
         //
         this.butSaveToTXT.Location = new System.Drawing.Point(16, 312);
         this.butSaveToTXT.Name = "butSaveToTXT";
         this.butSaveToTXT.Size = new System.Drawing.Size(168, 23);
         this.butSaveToTXT.TabIndex = 38;
         this.butSaveToTXT.Text = "Save to text file (\"c:\\log.txt\")";
         this.butSaveToTXT.Click += new System.EventHandler(this.butSaveToTXT_Click);
         //
         // groupBox3
         //
         this.groupBox3.Controls.Add(this.IndentButton);
         this.groupBox3.Controls.Add(this.MulticolBut);
         this.groupBox3.Controls.Add(this.chkSendProcessName);
         this.groupBox3.Controls.Add(this.label1);
         this.groupBox3.Controls.Add(this.chkSendEvents);
         this.groupBox3.Controls.Add(this.chkSendInherited);
         this.groupBox3.Controls.Add(this.chkSendFunctions);
         this.groupBox3.Controls.Add(this.comboBox1);
         this.groupBox3.Controls.Add(this.butTrace);
         this.groupBox3.Controls.Add(this.label2);
         this.groupBox3.Location = new System.Drawing.Point(8, 16);
         this.groupBox3.Name = "groupBox3";
         this.groupBox3.Size = new System.Drawing.Size(568, 136);
         this.groupBox3.TabIndex = 41;
         this.groupBox3.TabStop = false;
         this.groupBox3.Text = "Samples traces";
         //
         // IndentButton
         //
         this.IndentButton.Location = new System.Drawing.Point(16, 104);
         this.IndentButton.Name = "IndentButton";
         this.IndentButton.Size = new System.Drawing.Size(152, 23);
         this.IndentButton.TabIndex = 47;
         this.IndentButton.Text = "Indent/UnIndent";
         this.IndentButton.Click += new System.EventHandler(this.IndentButton_Click);
         //
         // MulticolBut
         //
         this.MulticolBut.Location = new System.Drawing.Point(16, 64);
         this.MulticolBut.Name = "MulticolBut";
         this.MulticolBut.Size = new System.Drawing.Size(152, 23);
         this.MulticolBut.TabIndex = 46;
         this.MulticolBut.Text = "MultiColumn test";
         this.MulticolBut.Click += new System.EventHandler(this.MulticolBut_Click);
         //
         // chkSendProcessName
         //
         this.chkSendProcessName.Location = new System.Drawing.Point(368, 64);
         this.chkSendProcessName.Name = "chkSendProcessName";
         this.chkSendProcessName.Size = new System.Drawing.Size(128, 24);
         this.chkSendProcessName.TabIndex = 36;
         this.chkSendProcessName.Text = "Send Process name";
         //
         // label1
         //
         this.label1.Location = new System.Drawing.Point(240, 104);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(64, 12);
         this.label1.TabIndex = 34;
         this.label1.Text = "Send mode";
         //
         // chkSendEvents
         //
         this.chkSendEvents.Location = new System.Drawing.Point(240, 64);
         this.chkSendEvents.Name = "chkSendEvents";
         this.chkSendEvents.TabIndex = 33;
         this.chkSendEvents.Text = "Send Events";
         //
         // chkSendInherited
         //
         this.chkSendInherited.Location = new System.Drawing.Point(368, 32);
         this.chkSendInherited.Name = "chkSendInherited";
         this.chkSendInherited.TabIndex = 32;
         this.chkSendInherited.Text = "Send Inherited";
         //
         // chkSendFunctions
         //
         this.chkSendFunctions.Location = new System.Drawing.Point(240, 32);
         this.chkSendFunctions.Name = "chkSendFunctions";
         this.chkSendFunctions.TabIndex = 31;
         this.chkSendFunctions.Text = "Send Functions";
         //
         // comboBox1
         //
         this.comboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.comboBox1.Items.AddRange(new object[] {
                                                       "Windows messages",
                                                       "Socket messages"});
         this.comboBox1.Location = new System.Drawing.Point(312, 99);
         this.comboBox1.Name = "comboBox1";
         this.comboBox1.Size = new System.Drawing.Size(152, 21);
         this.comboBox1.TabIndex = 30;
         this.comboBox1.SelectedIndexChanged += new System.EventHandler(this.comboBox1_SelectedIndexChanged);
         //
         // butTrace
         //
         this.butTrace.Location = new System.Drawing.Point(16, 24);
         this.butTrace.Name = "butTrace";
         this.butTrace.Size = new System.Drawing.Size(152, 23);
         this.butTrace.TabIndex = 28;
         this.butTrace.Text = "Samples traces";
         this.butTrace.Click += new System.EventHandler(this.butTrace_Click);
         //
         // label2
         //
         this.label2.Location = new System.Drawing.Point(237, 12);
         this.label2.Name = "label2";
         this.label2.Size = new System.Drawing.Size(248, 16);
         this.label2.TabIndex = 39;
         this.label2.Text = "Samples traces options :";
         //
         // butDoc
         //
         this.butDoc.Location = new System.Drawing.Point(16, 264);
         this.butDoc.Name = "butDoc";
         this.butDoc.Size = new System.Drawing.Size(208, 23);
         this.butDoc.TabIndex = 37;
         this.butDoc.Text = "Display object (button) documentation";
         this.butDoc.Click += new System.EventHandler(this.butDoc_Click);
         //
         // butFullInfo
         //
         this.butFullInfo.Location = new System.Drawing.Point(16, 224);
         this.butFullInfo.Name = "butFullInfo";
         this.butFullInfo.Size = new System.Drawing.Size(208, 23);
         this.butFullInfo.TabIndex = 36;
         this.butFullInfo.Text = "Display full form information";
         this.butFullInfo.Click += new System.EventHandler(this.butFullInfo_Click);
         //
         // butVariant
         //
         this.butVariant.Location = new System.Drawing.Point(16, 176);
         this.butVariant.Name = "butVariant";
         this.butVariant.Size = new System.Drawing.Size(208, 23);
         this.butVariant.TabIndex = 35;
         this.butVariant.Text = "SendValue (obects and arrays)";
         this.butVariant.Click += new System.EventHandler(this.butVariant_Click);
         //
         // butClear
         //
         this.butClear.Location = new System.Drawing.Point(224, 312);
         this.butClear.Name = "butClear";
         this.butClear.Size = new System.Drawing.Size(160, 23);
         this.butClear.TabIndex = 40;
         this.butClear.Text = "Clear main traces";
         this.butClear.Click += new System.EventHandler(this.butClear_Click);
         //
         // tabPage3
         //
         this.tabPage3.Controls.Add(this.butEventLog);
         this.tabPage3.Controls.Add(this.groupBox2);
         this.tabPage3.Controls.Add(this.groupBox1);
         this.tabPage3.Controls.Add(this.butOldTrace);
         this.tabPage3.Controls.Add(this.butTail);
         this.tabPage3.Location = new System.Drawing.Point(4, 22);
         this.tabPage3.Name = "tabPage3";
         this.tabPage3.Size = new System.Drawing.Size(664, 436);
         this.tabPage3.TabIndex = 3;
         this.tabPage3.Text = "Other traces";
         //
         // butEventLog
         //
         this.butEventLog.Location = new System.Drawing.Point(256, 264);
         this.butEventLog.Name = "butEventLog";
         this.butEventLog.Size = new System.Drawing.Size(160, 23);
         this.butEventLog.TabIndex = 38;
         this.butEventLog.Text = "Event Log demo";
         this.butEventLog.Click += new System.EventHandler(this.butEventLog_Click);
         //
         // groupBox2
         //
         this.groupBox2.Controls.Add(this.label3);
         this.groupBox2.Controls.Add(this.butDiagnostic);
         this.groupBox2.Location = new System.Drawing.Point(8, 112);
         this.groupBox2.Name = "groupBox2";
         this.groupBox2.Size = new System.Drawing.Size(424, 120);
         this.groupBox2.TabIndex = 37;
         this.groupBox2.TabStop = false;
         this.groupBox2.Text = "System.Diagnostics";
         //
         // groupBox1
         //
         this.groupBox1.Controls.Add(this.butListenerWriteLine);
         this.groupBox1.Controls.Add(this.butListenerInit);
         this.groupBox1.Controls.Add(this.butListenerWrite);
         this.groupBox1.Location = new System.Drawing.Point(8, 8);
         this.groupBox1.Name = "groupBox1";
         this.groupBox1.Size = new System.Drawing.Size(424, 88);
         this.groupBox1.TabIndex = 34;
         this.groupBox1.TabStop = false;
         this.groupBox1.Text = "Trace Listener demo";
         //
         // butListenerWriteLine
         //
         this.butListenerWriteLine.Location = new System.Drawing.Point(224, 32);
         this.butListenerWriteLine.Name = "butListenerWriteLine";
         this.butListenerWriteLine.Size = new System.Drawing.Size(144, 23);
         this.butListenerWriteLine.TabIndex = 33;
         this.butListenerWriteLine.Text = "Trace.WriteLine(\"World.\")";
         this.butListenerWriteLine.Click += new System.EventHandler(this.butListenerWriteLine_Click);
         //
         // butListenerInit
         //
         this.butListenerInit.Location = new System.Drawing.Point(8, 32);
         this.butListenerInit.Name = "butListenerInit";
         this.butListenerInit.Size = new System.Drawing.Size(64, 23);
         this.butListenerInit.TabIndex = 32;
         this.butListenerInit.Text = "Initialize";
         this.butListenerInit.Click += new System.EventHandler(this.butListenerInit_Click);
         //
         // butListenerWrite
         //
         this.butListenerWrite.Location = new System.Drawing.Point(88, 32);
         this.butListenerWrite.Name = "butListenerWrite";
         this.butListenerWrite.Size = new System.Drawing.Size(120, 23);
         this.butListenerWrite.TabIndex = 31;
         this.butListenerWrite.Text = "Trace.Write(\"Hello.\")";
         this.butListenerWrite.Click += new System.EventHandler(this.butListenerWrite_Click);
         //
         // tabPage4
         //
         this.tabPage4.Controls.Add(this.butWinLoadXml);
         this.tabPage4.Controls.Add(this.butSaveWinToXml);
         this.tabPage4.Controls.Add(this.butSaveWinToTxt);
         this.tabPage4.Controls.Add(this.butClearWin);
         this.tabPage4.Controls.Add(this.butDisplayWin);
         this.tabPage4.Controls.Add(this.butHelloToWintrace);
         this.tabPage4.Controls.Add(this.butCreateWinTrace);
         this.tabPage4.Location = new System.Drawing.Point(4, 22);
         this.tabPage4.Name = "tabPage4";
         this.tabPage4.Size = new System.Drawing.Size(664, 436);
         this.tabPage4.TabIndex = 2;
         this.tabPage4.Text = "Multi Pages";
         //
         // butWinLoadXml
         //
         this.butWinLoadXml.Enabled = false;
         this.butWinLoadXml.Location = new System.Drawing.Point(224, 352);
         this.butWinLoadXml.Name = "butWinLoadXml";
         this.butWinLoadXml.Size = new System.Drawing.Size(160, 24);
         this.butWinLoadXml.TabIndex = 43;
         this.butWinLoadXml.Text = "LoadXml  (\"c:\\log2.xml\")";
         this.butWinLoadXml.Click += new System.EventHandler(this.butLoadXml_Click);
         //
         // butSaveWinToXml
         //
         this.butSaveWinToXml.Enabled = false;
         this.butSaveWinToXml.Location = new System.Drawing.Point(16, 352);
         this.butSaveWinToXml.Name = "butSaveWinToXml";
         this.butSaveWinToXml.Size = new System.Drawing.Size(168, 23);
         this.butSaveWinToXml.TabIndex = 4;
         this.butSaveWinToXml.Text = "Save To Xml file (\"c:\\log2.xml\")";
         this.butSaveWinToXml.Click += new System.EventHandler(this.butSaveWinToXml_Click);
         //
         // butSaveWinToTxt
         //
         this.butSaveWinToTxt.Enabled = false;
         this.butSaveWinToTxt.Location = new System.Drawing.Point(16, 312);
         this.butSaveWinToTxt.Name = "butSaveWinToTxt";
         this.butSaveWinToTxt.Size = new System.Drawing.Size(168, 23);
         this.butSaveWinToTxt.TabIndex = 3;
         this.butSaveWinToTxt.Text = "Save to text file (\"c:\\log2.txt\")";
         this.butSaveWinToTxt.Click += new System.EventHandler(this.butSaveWinToTxt_Click);
         //
         // butClearWin
         //
         this.butClearWin.Enabled = false;
         this.butClearWin.Location = new System.Drawing.Point(224, 312);
         this.butClearWin.Name = "butClearWin";
         this.butClearWin.Size = new System.Drawing.Size(160, 23);
         this.butClearWin.TabIndex = 5;
         this.butClearWin.Text = "Clear win traces";
         this.butClearWin.Click += new System.EventHandler(this.butClearWin_Click);
         //
         // butDisplayWin
         //
         this.butDisplayWin.Enabled = false;
         this.butDisplayWin.Location = new System.Drawing.Point(16, 96);
         this.butDisplayWin.Name = "butDisplayWin";
         this.butDisplayWin.Size = new System.Drawing.Size(184, 23);
         this.butDisplayWin.TabIndex = 1;
         this.butDisplayWin.Text = "Display that window on the viewer";
         this.butDisplayWin.Click += new System.EventHandler(this.butDisplayWin_Click);
         //
         // butHelloToWintrace
         //
         this.butHelloToWintrace.Enabled = false;
         this.butHelloToWintrace.Location = new System.Drawing.Point(16, 168);
         this.butHelloToWintrace.Name = "butHelloToWintrace";
         this.butHelloToWintrace.Size = new System.Drawing.Size(96, 23);
         this.butHelloToWintrace.TabIndex = 2;
         this.butHelloToWintrace.Text = "Say Hello";
         this.butHelloToWintrace.Click += new System.EventHandler(this.butHelloToWintrace_Click);
         //
         // butCreateWinTrace
         //
         this.butCreateWinTrace.Location = new System.Drawing.Point(16, 24);
         this.butCreateWinTrace.Name = "butCreateWinTrace";
         this.butCreateWinTrace.Size = new System.Drawing.Size(184, 23);
         this.butCreateWinTrace.TabIndex = 0;
         this.butCreateWinTrace.Text = "Create a new window trace ";
         this.butCreateWinTrace.Click += new System.EventHandler(this.butCreateWinTrace_Click);
         //
         // tabPage5
         //
         this.tabPage5.Controls.Add(this.label4);
         this.tabPage5.Controls.Add(this.butWinWatchDisplay);
         this.tabPage5.Controls.Add(this.butWinWatchClear);
         this.tabPage5.Controls.Add(this.butWinWatchSend);
         this.tabPage5.Controls.Add(this.butCreateWinWatch);
         this.tabPage5.Controls.Add(this.butDisplayWatchWindow);
         this.tabPage5.Controls.Add(this.butClearWatchWindow);
         this.tabPage5.Controls.Add(this.butWatch);
         this.tabPage5.Location = new System.Drawing.Point(4, 22);
         this.tabPage5.Name = "tabPage5";
         this.tabPage5.Size = new System.Drawing.Size(664, 436);
         this.tabPage5.TabIndex = 5;
         this.tabPage5.Text = "Watches";
         //
         // label4
         //
         this.label4.Location = new System.Drawing.Point(40, 48);
         this.label4.Name = "label4";
         this.label4.Size = new System.Drawing.Size(144, 16);
         this.label4.TabIndex = 7;
         this.label4.Text = "Main window watches";
         //
         // butWinWatchDisplay
         //
         this.butWinWatchDisplay.Location = new System.Drawing.Point(296, 160);
         this.butWinWatchDisplay.Name = "butWinWatchDisplay";
         this.butWinWatchDisplay.Size = new System.Drawing.Size(144, 23);
         this.butWinWatchDisplay.TabIndex = 6;
         this.butWinWatchDisplay.Text = "Display Watch Window";
         this.butWinWatchDisplay.Click += new System.EventHandler(this.butWinWatchDisplay_Click);
         //
         // butWinWatchClear
         //
         this.butWinWatchClear.Location = new System.Drawing.Point(296, 120);
         this.butWinWatchClear.Name = "butWinWatchClear";
         this.butWinWatchClear.Size = new System.Drawing.Size(144, 23);
         this.butWinWatchClear.TabIndex = 5;
         this.butWinWatchClear.Text = "Clear Watch Window";
         this.butWinWatchClear.Click += new System.EventHandler(this.butWinWatchClear_Click);
         //
         // butWinWatchSend
         //
         this.butWinWatchSend.Location = new System.Drawing.Point(296, 80);
         this.butWinWatchSend.Name = "butWinWatchSend";
         this.butWinWatchSend.Size = new System.Drawing.Size(144, 23);
         this.butWinWatchSend.TabIndex = 4;
         this.butWinWatchSend.Text = "Send Watches";
         this.butWinWatchSend.Click += new System.EventHandler(this.butWinWatchSend_Click);
         //
         // butCreateWinWatch
         //
         this.butCreateWinWatch.Location = new System.Drawing.Point(296, 40);
         this.butCreateWinWatch.Name = "butCreateWinWatch";
         this.butCreateWinWatch.Size = new System.Drawing.Size(144, 23);
         this.butCreateWinWatch.TabIndex = 3;
         this.butCreateWinWatch.Text = "Create new WinWatch";
         this.butCreateWinWatch.Click += new System.EventHandler(this.butCreateWinWatch_Click);
         //
         // butDisplayWatchWindow
         //
         this.butDisplayWatchWindow.Location = new System.Drawing.Point(40, 160);
         this.butDisplayWatchWindow.Name = "butDisplayWatchWindow";
         this.butDisplayWatchWindow.Size = new System.Drawing.Size(136, 23);
         this.butDisplayWatchWindow.TabIndex = 2;
         this.butDisplayWatchWindow.Text = "Display Watch Window";
         this.butDisplayWatchWindow.Click += new System.EventHandler(this.butDisplayWatchWindow_Click);
         //
         // butClearWatchWindow
         //
         this.butClearWatchWindow.Location = new System.Drawing.Point(40, 120);
         this.butClearWatchWindow.Name = "butClearWatchWindow";
         this.butClearWatchWindow.Size = new System.Drawing.Size(136, 23);
         this.butClearWatchWindow.TabIndex = 1;
         this.butClearWatchWindow.Text = "Clear Watch Window";
         this.butClearWatchWindow.Click += new System.EventHandler(this.butClearWatchWindow_Click);
         //
         // butWatch
         //
         this.butWatch.Location = new System.Drawing.Point(40, 80);
         this.butWatch.Name = "butWatch";
         this.butWatch.Size = new System.Drawing.Size(136, 23);
         this.butWatch.TabIndex = 0;
         this.butWatch.Text = "Send Watches";
         this.butWatch.Click += new System.EventHandler(this.butWatch_Click);
         //
         // tabPage2
         //
         this.tabPage2.Controls.Add(this.butShowNode);
         this.tabPage2.Controls.Add(this.butSetSelected);
         this.tabPage2.Controls.Add(this.butEnd2);
         this.tabPage2.Controls.Add(this.butstart2);
         this.tabPage2.Controls.Add(this.butDone1);
         this.tabPage2.Controls.Add(this.butStart1);
         this.tabPage2.Location = new System.Drawing.Point(4, 22);
         this.tabPage2.Name = "tabPage2";
         this.tabPage2.Size = new System.Drawing.Size(664, 436);
         this.tabPage2.TabIndex = 1;
         this.tabPage2.Text = "Nodes operations";
         //
         // butShowNode
         //
         this.butShowNode.Enabled = false;
         this.butShowNode.Location = new System.Drawing.Point(288, 96);
         this.butShowNode.Name = "butShowNode";
         this.butShowNode.TabIndex = 5;
         this.butShowNode.Text = "Show()";
         this.butShowNode.Click += new System.EventHandler(this.butShowNode_Click);
         //
         // butSetSelected
         //
         this.butSetSelected.Enabled = false;
         this.butSetSelected.Location = new System.Drawing.Point(280, 24);
         this.butSetSelected.Name = "butSetSelected";
         this.butSetSelected.Size = new System.Drawing.Size(88, 23);
         this.butSetSelected.TabIndex = 4;
         this.butSetSelected.Text = "SetSelected()";
         this.butSetSelected.Click += new System.EventHandler(this.butSetSelected_Click);
         //
         // butEnd2
         //
         this.butEnd2.Enabled = false;
         this.butEnd2.Location = new System.Drawing.Point(120, 96);
         this.butEnd2.Name = "butEnd2";
         this.butEnd2.Size = new System.Drawing.Size(136, 23);
         this.butEnd2.TabIndex = 3;
         this.butEnd2.Text = "AppendLeft (\"Done 2\")";
         this.butEnd2.Click += new System.EventHandler(this.butEnd2_Click);
         //
         // butstart2
         //
         this.butstart2.Location = new System.Drawing.Point(16, 96);
         this.butstart2.Name = "butstart2";
         this.butstart2.TabIndex = 2;
         this.butstart2.Text = "Start 2...";
         this.butstart2.Click += new System.EventHandler(this.butstart2_Click);
         //
         // butDone1
         //
         this.butDone1.Enabled = false;
         this.butDone1.Location = new System.Drawing.Point(120, 24);
         this.butDone1.Name = "butDone1";
         this.butDone1.Size = new System.Drawing.Size(144, 23);
         this.butDone1.TabIndex = 1;
         this.butDone1.Text = "ResendRight (\"Done 1\") ";
         this.butDone1.Click += new System.EventHandler(this.butDone1_Click);
         //
         // butShowtrace
         //
         this.butShowtrace.FlatStyle = System.Windows.Forms.FlatStyle.System;
         this.butShowtrace.Location = new System.Drawing.Point(24, 16);
         this.butShowtrace.Name = "butShowtrace";
         this.butShowtrace.Size = new System.Drawing.Size(128, 23);
         this.butShowtrace.TabIndex = 29;
         this.butShowtrace.Text = "Show Viewer";
         this.butShowtrace.Click += new System.EventHandler(this.butShowtrace_Click);
         //
         // panel1
         //
         this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
         this.panel1.Controls.Add(this.butShowtrace);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
         this.panel1.Location = new System.Drawing.Point(0, 414);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(672, 48);
         this.panel1.TabIndex = 31;
         //
         // pictureBox1
         //
         this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
         this.pictureBox1.Location = new System.Drawing.Point(320, 176);
         this.pictureBox1.Name = "pictureBox1";
         this.pictureBox1.Size = new System.Drawing.Size(134, 118);
         this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
         this.pictureBox1.TabIndex = 45;
         this.pictureBox1.TabStop = false;
         //
         // WinForm
         //
         this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
         this.ClientSize = new System.Drawing.Size(672, 462);
         this.Controls.Add(this.panel1);
         this.Controls.Add(this.tabControl1);
         this.Location = new System.Drawing.Point(50, 50);
         this.Name = "WinForm";
         this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
         this.Text = "CSharp Demo";
         this.Load += new System.EventHandler(this.WinForm_Load);
         this.tabControl1.ResumeLayout(false);
         this.tabPage1.ResumeLayout(false);
         this.groupBox3.ResumeLayout(false);
         this.tabPage3.ResumeLayout(false);
         this.groupBox2.ResumeLayout(false);
         this.groupBox1.ResumeLayout(false);
         this.tabPage4.ResumeLayout(false);
         this.tabPage5.ResumeLayout(false);
         this.tabPage2.ResumeLayout(false);
         this.panel1.ResumeLayout(false);
         this.ResumeLayout(false);

      }
      #endregion

      /// <summary>
      /// The main entry point for the application.
      /// </summary>
      [STAThread]
      static void Main()
      {
         //Application.EnableVisualStyles() ;
         Application.Run(new WinForm());
      }
      private Button butOldTrace;
      private Button butTail;
      private Button butDiagnostic;
      private Label label3;
      private TabControl tabControl1;
      private TabPage tabPage1;
      private TabPage tabPage2;
      private Button butVariant;
      private CheckBox chkSendProcessName;
      private Button butClear;
      private Label label1;
      private CheckBox chkSendEvents;
      private CheckBox chkSendInherited;
      private CheckBox chkSendFunctions;
      private ComboBox comboBox1;
      private Button butShowtrace;
      public Button butTrace;
      private TabPage tabPage3;
      private Button butListenerWriteLine;
      private Button butListenerInit;
      private Button butListenerWrite;
      private GroupBox groupBox1;
      private Button butStart1;
      private Button butDone1;
      private Button butstart2;
      private Button butEnd2;
      private TabPage tabPage4;
      private Button butCreateWinTrace;
      private Button butHelloToWintrace;
      private Button butFullInfo;
      private Label label2;
      private Button butDoc;
      private GroupBox groupBox3;
      private Button butDisplayWin;
      private Button butSaveToTXT;
      private Button butSaveToXml;
      private Button butSetSelected;
      private Button butShowNode;
      private Panel panel1;
      private Button butSaveWinToXml;
      private Button butSaveWinToTxt;
      private Button butClearWin;
      private Button butWinLoadXml;
      private Button butLoadXml;
      private Button butEventLog;
      private GroupBox groupBox2;




      private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
      {
         if (comboBox1.SelectedIndex == 0)
            TTrace.Options.SendMode = SendMode.WinMsg ;
         else
            TTrace.Options.SendMode = SendMode.Socket ;
      }

      private void WinForm_Load(object sender, EventArgs e)
      {
         //comboBox1.SelectedIndex  = 0 ;
         if (TTrace.Options.SendMode == SendMode.WinMsg)
            comboBox1.SelectedIndex = 0 ;
         else
            comboBox1.SelectedIndex = 1 ;
         chkSendEvents.Checked    = TTrace.Options.SendEvents    ;
         chkSendInherited.Checked = TTrace.Options.SendInherited ;
         chkSendFunctions.Checked = TTrace.Options.SendFunctions ;
      }

      private void butShowtrace_Click(object sender, EventArgs e)
      {
         TTrace.Show (true) ;
      }


      private void butClear_Click(object sender, EventArgs e)
      {
         TTrace.ClearAll () ;
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

         TTrace.Debug.Send("Whole line","in red")
            .SetFontDetail(-1,false,false,Color.Red.ToArgb()) ;                 // -1 : all columns

         TTrace.Debug.SendBackgroundColor("Highlighted col in Fuchsia",Color.Fuchsia.ToArgb(),3) ;  // Background Color sample

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

      private void butVariant_Click (object sender, EventArgs e)
      {
         TTrace.Options.SendProcessName = chkSendProcessName.Checked ;

         // SendValue is quite different from SendObject
         // SendValue is recursive and display arrays
         TTrace.Debug.SendValue   ("SendValue 'testClass'" , testClass);
         TTrace.Warning.SendValue ("SendValue 'testClass', max 3 levels (default)" , testClass,true,3,"testClass" );
         TTrace.Error.SendValue   ("SendValue 'array', max 5 levels" ,               testClass.fieldArray,true,5,"fieldArray" );
      }

      private void butFullInfo_Click(object sender, EventArgs e)
      {
         // display all informations relative to that form. (slow traces : too much informations to send)
         // Note that the ShowDoc flag don't work here,
         // because the this object is a "WinForm" type with no documentation
         TTrace.Debug.SendObject ("this object (full display)" ,  this ,
            TraceDisplayFlags.ShowModifiers |
            TraceDisplayFlags.ShowFields |
            TraceDisplayFlags.ShowClassInfo |
            TraceDisplayFlags.ShowCustomAttributes |
            TraceDisplayFlags.ShowNonPublic |
            TraceDisplayFlags.ShowInheritedMembers |
            TraceDisplayFlags.ShowEvents |
            TraceDisplayFlags.ShowDoc |
            TraceDisplayFlags.ShowMethods );
      }

      private void butDoc_Click(object sender, EventArgs e)
      {
         // display a System.Windows.Forms.Button object with his documentation
         TTrace.Debug.SendObject ("butDoc object" ,  butDoc ,
            TraceDisplayFlags.ShowModifiers |
            TraceDisplayFlags.ShowFields |
            TraceDisplayFlags.ShowClassInfo |
            TraceDisplayFlags.ShowCustomAttributes |
            TraceDisplayFlags.ShowNonPublic |
            TraceDisplayFlags.ShowInheritedMembers |
            TraceDisplayFlags.ShowEvents |
            TraceDisplayFlags.ShowDoc |
            TraceDisplayFlags.ShowMethods );
      }


      int test = 0 ;
      private void butTail_Click(object sender, EventArgs e)
      {
         FileStream fsFileStream = new FileStream("c:\\log.txt", FileMode.Append) ;
         StreamWriter swWriter   = new StreamWriter(fsFileStream) ;
         test ++ ;
         swWriter.WriteLine("test " + test) ;
         swWriter.Close() ;
         fsFileStream.Close() ;
      }

      [DllImport("kernel32.dll")]
      extern static void OutputDebugString(String str);

      private void butOldTrace_Click(object sender, EventArgs e)
      {
         test ++ ;
         OutputDebugString ("OutputDebugString test" + test) ;
      }

      private void butDiagnostic_Click(object sender, EventArgs e)
      {
         test ++ ;
         Trace.WriteLine ("Diagnostics test" + test) ;
      }

      private void chkSendFunctions_CheckedChanged(object sender, EventArgs e)
      {
         TTrace.Options.SendFunctions = chkSendFunctions.Checked ;
      }

      private void chkSendInherited_CheckedChanged(object sender, EventArgs e)
      {
         TTrace.Options.SendInherited = chkSendInherited.Checked ;
      }

      private void chkSendEvents_CheckedChanged(object sender, EventArgs e)
      {
         TTrace.Options.SendEvents = chkSendEvents.Checked ;
      }


      // TraceListener demo
      private void butListenerInit_Click(object sender, EventArgs e)
      {
         Trace.WriteLine ("Before setting listener : trace goes to ODS window") ;
         int[] myArray = new int[3] { 3, 5 , 5};

         Trace.Listeners.Clear() ;
         Trace.Listeners.Add (new TTraceListener ()) ;
         Trace.WriteLine ("TraceListener demo in main trace window") ;
         Trace.Indent () ;
         Trace.Write ("myArray : ") ;
         Trace.WriteLine (myArray) ;
         Trace.Unindent() ;
      }

      private void butListenerWrite_Click(object sender, EventArgs e)
      {
         Trace.Write ("hello.") ;
      }

      private void butListenerWriteLine_Click(object sender, EventArgs e)
      {
         Trace.WriteLine("World.") ;
      }

      TraceNode start1 ;
      TraceNode start2 ;

      private void butStart1_Click(object sender, EventArgs e)
      {
         start1 = TTrace.Debug.Send ("Start 1 ..") ;
         butDone1.Enabled = true ;
         butSetSelected.Enabled = true ;
      }

      private void butDone1_Click(object sender, EventArgs e)
      {
         start1.ResendRight ("Done 1") ;
      }

      private void butSetSelected_Click(object sender, EventArgs e)
      {
         start1.SetSelected() ;
      }

      private void butstart2_Click(object sender, EventArgs e)
      {
         start2 = TTrace.Debug.Send ("Start 2 ..") ;
         butEnd2.Enabled = true ;
         butShowNode.Enabled = true ;
      }

      private void butEnd2_Click(object sender, EventArgs e)
      {
         start2.AppendLeft ("Done 2") ;
      }

      private void butShowNode_Click(object sender, EventArgs e)
      {
         start2.Show() ;
      }


      // long operation on sub nodes.
      // +- 3 sec to complete
      // +- 12 sec to send traces in bakground
      private void butLong_Click(object sender, EventArgs e)
      {
         TraceNode cNode ;
         TraceNode dNode ;

         TTrace.Debug.Send ("Before long data") ;
         for (int c = 0 ; c < 3 ; c++)
         {
            cNode = TTrace.Debug.Send ("level c " + c) ;

            for (int d = 0 ; d < 300 ; d++)
            {
               dNode = cNode.Send ("level d " + d) ;

               for (int f = 0; f < 6;f++)
               {
                  dNode.Send ("level e " + f) ;
               }
            }
         }
         TTrace.Debug.Send ("Before flush") ;
         TTrace.Flush() ;
         TTrace.Debug.Send ("Flush done") ;
      }


      private void butSaveToTXT_Click(object sender, EventArgs e)
      {
         TTrace.WinTrace.SaveToTextfile ("c:\\log.txt") ;
      }

      private void butSaveToXml_Click(object sender, EventArgs e)
      {
         TTrace.WinTrace.SaveToXml ("c:\\log.xml") ;
         TTrace.WinTrace.SaveToXml ("c:\\logWithStyleSheet.xml","tracetool.xsl") ;
      }

      private void butLoadXml_Click_1(object sender, EventArgs e)
      {
         TTrace.WinTrace.LoadXml ("c:\\log.xml") ;
      }

      private WinTrace myWinTrace ;

      private void butCreateWinTrace_Click(object sender, EventArgs e)
      {
         myWinTrace = new WinTrace ("MyWINID" , "My trace window") ;
         butDisplayWin.Enabled = true ;
         butHelloToWintrace.Enabled = true ;
         butSaveWinToTxt.Enabled = true ;
         butSaveWinToXml.Enabled = true ;
         butClearWin.Enabled = true ;
         butWinLoadXml.Enabled = true ;
      }

      private void butHelloToWintrace_Click(object sender, EventArgs e)
      {
         myWinTrace.Debug.Send ("Hello", "Can be used to store exceptions, for examples");

      }

      private void butDisplayWin_Click(object sender, EventArgs e)
      {
         myWinTrace.DisplayWin() ;
      }
      private void butSaveWinToTxt_Click(object sender, EventArgs e)
      {
         myWinTrace.SaveToTextfile ("c:\\log2.txt") ;
      }

      private void butSaveWinToXml_Click(object sender, EventArgs e)
      {
         myWinTrace.SaveToXml ("c:\\log2.xml") ;
      }

      private void butClearWin_Click(object sender, EventArgs e)
      {
         myWinTrace.ClearAll () ;
      }

      private void butLoadXml_Click(object sender, EventArgs e)
      {
         myWinTrace.LoadXml ("c:\\log2.xml") ;
      }

      private void butEventLog_Click(object sender, EventArgs e)
      {
         EventLog myLog = new EventLog();
         myLog.Source = "Application";

         test ++ ;

         // Write an informational entry to the event log.
         myLog.WriteEntry("Event log test " + test);
      }


      WinTrace MultiColTrace ;
      private void MulticolBut_Click(object sender, System.EventArgs e)
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

      private void IndentButton_Click(object sender, System.EventArgs e)
      {
         TraceNode node = TTrace.Debug.Send ("Tree indentation using Indent and UnIndent methods") ;

         node.Indent("Indent level 1") ;
         node.Send ("Node1") ;
         node.Indent("Indent level 2") ;
         node.Send ("Node2") ;

         // UnIndent with no title
         node.Indent ("Indent level 3") ;
         node.Send ("Node3") ;
         node.UnIndent () ;   // UnIndent without title

         node.Send ("Node4") ;

         node.UnIndent ("UnIndent level 2" ) ;
         node.UnIndent ("UnIndent level 1") ;


         TTrace.Debug.Indent ("Begin of procedure") ;
         TTrace.Debug.Send ("inside procedure" );
         TTrace.Debug.Send ("Do some work" );
         TTrace.Debug.UnIndent ("end of procedure") ;

      }

      TestClass test2 = new TestClass() ;

      private void butWatch_Click(object sender, System.EventArgs e)
      {
         TTrace.Watches.Send("test2", test2);
      }

      private void butClearWatchWindow_Click(object sender, System.EventArgs e)
      {
         TTrace.Watches.ClearAll() ;
      }

      private void butDisplayWatchWindow_Click(object sender, System.EventArgs e)
      {
         TTrace.Watches.DisplayWin() ;
      }

      private WinWatch MyWinWatch ;
      private void butCreateWinWatch_Click(object sender, System.EventArgs e)
      {
         MyWinWatch = new WinWatch ("MyWinWatchID" , "My watches")  ;
      }

      private void butWinWatchSend_Click(object sender, System.EventArgs e)
      {
         if (MyWinWatch != null)
            MyWinWatch.Send ( "Now", DateTime.Now.ToString("HH:mm:ss:fff") ) ;
      }

      private void butWinWatchClear_Click(object sender, System.EventArgs e)
      {
         if (MyWinWatch != null)
            MyWinWatch.ClearAll () ;
      }

      private void butWinWatchDisplay_Click(object sender, System.EventArgs e)
      {
         if (MyWinWatch != null)
            MyWinWatch.DisplayWin () ;
      }
   }

   #region Test Classes
   //----------------------------------------------------------------------------------------------------

   public class TestClass
   {
      public enum Days {Sat=1, Sun, Mon, Tue, Wed, Thu, Fri};

      // sample Application variables.
      public  Days fieldDay ;
      public  Array fieldArray ;
      public  Hashtable fieldHash ;
      public  ArrayList fieldArrayList ;
      private Child fieldClassInstance ;
      public  object PropButton {get { return WinForm.ShowViewerButton; }}
      public Child myNullChild ;

      public new string ToString() {return "BlaBla" ;}
      public TestClass()
      {
         // initialize demo vars.

         fieldDay = Days.Tue ;

         fieldClassInstance = new Child(5);
         fieldClassInstance.fctChild1 (new int[3] {1,2,3}) ;   // accept only  arrays
         fieldClassInstance.fctChild3 (1,2,3) ;                // parameters are converted to array
         fieldClassInstance.fctChild3 (new int[3] {1,2,3}) ;   // also accept array

         fieldHash = new Hashtable();
         fieldHash.Add("Tuesday",fieldDay);

         fieldArrayList = new ArrayList();
         fieldArrayList.Add(fieldDay) ;

         // init bound array
         int[] myLengthsArray = new int[3] { 1, 2 , 5};     // 1 by 2 by 5
         int[] myBoundsArray  = new int[3] { 2, 3 , 8};     // 2..4 , 3..4 , 8..12
         fieldArray = Array.CreateInstance( typeof(Object), myLengthsArray, myBoundsArray );

         // sub array
         int[] myIndArray = new int[3] ;
         myIndArray [0] = 2 ;
         myIndArray [1] = 4 ;
         myIndArray [2] = 6 ;

         fieldArray.SetValue(238  ,              new int[3] { 2, 3, 8 });
         fieldArray.SetValue("239",              new int[3] { 2, 3, 9 });
         fieldArray.SetValue(DateTime.Now,       new int[3] { 2, 4, 8 });
         fieldArray.SetValue(fieldClassInstance, new int[3] { 2, 4, 9 });
         fieldArray.SetValue(myIndArray,         new int[3] { 2, 4, 10});

      }
   }

   //---------------------------------------------------------------------------

   public struct MyStruct
   {
      private double k;
      public MyStruct(int i) { k = 200 * i; }
      public long l {get { return 6; }}
   }

   //---------------------------------------------------------------------------

   [AttributeUsage (AttributeTargets.All, AllowMultiple = true)]
   public class TestAttribute : Attribute
   {
      string value ;
      public TestAttribute (string value)
      {
         this.value = value ;
      }
   }

   //---------------------------------------------------------------------------

   public interface Myinterface : Myinterface3
   {
      void FctInterface ();
   }

   //---------------------------------------------------------------------------

   public interface Myinterface2
   {
      void FctInterface2 ();
   }

   //---------------------------------------------------------------------------

   public interface Myinterface3
   {
      void FctInterface3 ();
   }

   //---------------------------------------------------------------------------

   public class Mere
   {
      public virtual int fctMere () {return 1;}
   }

   //---------------------------------------------------------------------------

   [DefaultMember("fctBase1")]
   public abstract class Base : Mere, Myinterface2
   {
      public int            Fld_PublicFromBase50 = 50 ;
      private int           Fld_PrivateFromBase51 = 51 ;
      protected int         Fld_ProtectedFromBase52 = 52 ;
      public virtual void   Fct_FromBase () {Fld_PrivateFromBase51++;}
      public static string  Prop_PublicstaticStr {get { return "5"; }}
      public virtual long   Prop_PublicVirtualLong {get { return 5; }}
      public virtual int    Prop_PublicVirtualInt {get { return 5; }}
      public abstract int   Prop_abstract {get ;}
      protected             Base (char c) {}
      protected             Base (float f) {}
      protected             Base () {}  // no argument, used by derived class

      public override sealed int fctMere () {return 2 ;}
      public abstract void fctBase1 () ;    // redefined in class child
      public virtual void fctBase2 () {}
      public void FctInterface2 () {}
      public void baseFctnonOverridable () {}
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
            TTrace.Debug.SendObject ("field1 GET" , this) ;
            return 10 ;
         }
      }
   }

   [Test("my class attrib")]      // custom attribute on the class
   [DefaultMember("Event1")]
   public class Child :  Base , Myinterface
   {
      // inner class
      //-----------------
      public class mysubclass
      {
         public int myfield ;
      }

      // return an inner class
      public mysubclass createsubclass () {return new mysubclass();}


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
      private Hashtable       eventTable = new Hashtable();

      // PROPERTIES
      //-----------------

      //      [Test("my property attrib 2")]      // custom attribute a property
      //      protected internal long Prop_ProtInternal1      {get { return 1; }}
      //      private static long     Prop_PrivateStaticLong2 {get { return 2; }}
      //      private long            Prop_PrivateLong3       {get { return 3; }}
      //      protected long          Prop_ProtectedLong4     {get { return 4; }}
      //      internal sealed override long  Prop_InternalSealedLong5 {get { return 5; }}
      //      public override long    Prop_PublicVirtualLong {get { return 5; }}
      public long             Prop_PublicLong6        {get { return 6; }
         set {}
      }
      public override int     Prop_abstract {get {return 10;}}
      //      public static long      Prop_PublicStaticLong7  {get { return 7; }}
      //      public string           this[int i, char c]     {get { return "8";}}    // indexer

      // METHODS
      //-----------------

      [Test("my method attrib")]      // custom attribute method
      public void                 Fct_PublicVoid (int c) {}
      public override sealed void Fct_FromBase () {}
      static void                 Fct_static () {}
      private void                Fct_Private  () {}
      protected void              Fct_protected () {}
      internal void               Fct_Internal () {}
      protected internal void     Fct_ProtectedInternal () {}

      // note : it's not possible to declare method "sealed" and "virtual".
      public new void             baseFctnonOverridable () {}
      public void                 FctInterface () {}
      public void                 FctInterface3 () {}
      public override void        fctBase1 () {}
      public virtual void         fctChild1(int [] args) {}
      public void                 fctChild2(ref string strModifier) {}
      public virtual void         fctChild3(params int [] args) {}
      public void                 fctChild4([Out] out int args) {args = 5;}
      public void                 fctChild5(ref int args) {args = 5;}


      [return : MarshalAs(UnmanagedType.Interface)]
      object fctChild6() {return null;}

      // special use of parameters or attributes
      void fctChild7([In(),Out(),Optional()] ref Guid param1,
         [MarshalAs(UnmanagedType.Interface)] out object param2) {param2=null;}


      // EVENT
      //-----------------

      public delegate void MyDelegate (int i);    // delegate are see as inner class
      public event MyDelegate Event1
      {
         add    {eventTable["Event1"] = (MyDelegate)eventTable["Event1"] + value; }
         remove {eventTable["Event1"] = (MyDelegate)eventTable["Event1"] - value; }
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
