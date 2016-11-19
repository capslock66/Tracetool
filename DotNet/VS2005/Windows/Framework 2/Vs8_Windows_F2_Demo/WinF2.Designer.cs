namespace CSharpDemo
{
    partial class WinF2
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
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
           this.tabControl1 = new System.Windows.Forms.TabControl();
           this.tabPage1 = new System.Windows.Forms.TabPage();
           this.pictureBox1 = new System.Windows.Forms.PictureBox();
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
           this.label3 = new System.Windows.Forms.Label();
           this.butDiagnostic = new System.Windows.Forms.Button();
           this.groupBox1 = new System.Windows.Forms.GroupBox();
           this.butListenerWriteLine = new System.Windows.Forms.Button();
           this.butListenerInit = new System.Windows.Forms.Button();
           this.butListenerWrite = new System.Windows.Forms.Button();
           this.butOldTrace = new System.Windows.Forms.Button();
           this.butTail = new System.Windows.Forms.Button();
           this.tabPage4 = new System.Windows.Forms.TabPage();
           this.butCloseWin = new System.Windows.Forms.Button();
           this.butWinLoadXml = new System.Windows.Forms.Button();
           this.butSaveWinToXml = new System.Windows.Forms.Button();
           this.butSaveWinToTxt = new System.Windows.Forms.Button();
           this.butClearWin = new System.Windows.Forms.Button();
           this.butDisplayWin = new System.Windows.Forms.Button();
           this.butHelloToWintrace = new System.Windows.Forms.Button();
           this.butCreateWinTrace = new System.Windows.Forms.Button();
           this.tabPage5 = new System.Windows.Forms.TabPage();
           this.butWinWatchClose = new System.Windows.Forms.Button();
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
           this.butStart1 = new System.Windows.Forms.Button();
           this.panel1 = new System.Windows.Forms.Panel();
           this.butShowtrace = new System.Windows.Forms.Button();
           this.butCloseViewer = new System.Windows.Forms.Button();
           this.tabControl1.SuspendLayout();
           this.tabPage1.SuspendLayout();
           ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
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
           this.tabControl1.Size = new System.Drawing.Size(764, 548);
           this.tabControl1.TabIndex = 1;
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
           this.tabPage1.Size = new System.Drawing.Size(756, 522);
           this.tabPage1.TabIndex = 0;
           this.tabPage1.Text = "Basic";
           // 
           // pictureBox1
           // 
           this.pictureBox1.Image = global::CSharpDemo.Properties.Resources.Logo2;
           this.pictureBox1.InitialImage = null;
           this.pictureBox1.Location = new System.Drawing.Point(275, 158);
           this.pictureBox1.Name = "pictureBox1";
           this.pictureBox1.Size = new System.Drawing.Size(134, 118);
           this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
           this.pictureBox1.TabIndex = 45;
           this.pictureBox1.TabStop = false;
           // 
           // butLoadXml
           // 
           this.butLoadXml.Location = new System.Drawing.Point(224, 352);
           this.butLoadXml.Name = "butLoadXml";
           this.butLoadXml.Size = new System.Drawing.Size(160, 24);
           this.butLoadXml.TabIndex = 44;
           this.butLoadXml.Text = "LoadXml  (\"c:\\log.xml\")";
           this.butLoadXml.Click += new System.EventHandler(this.butLoadXml_Click);
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
           this.chkSendProcessName.CheckedChanged += new System.EventHandler(this.chkSendProcessName_CheckedChanged);
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
           this.chkSendEvents.Size = new System.Drawing.Size(104, 24);
           this.chkSendEvents.TabIndex = 33;
           this.chkSendEvents.Text = "Send Events";
           this.chkSendEvents.CheckedChanged += new System.EventHandler(this.chkSendEvents_CheckedChanged);
           // 
           // chkSendInherited
           // 
           this.chkSendInherited.Location = new System.Drawing.Point(368, 32);
           this.chkSendInherited.Name = "chkSendInherited";
           this.chkSendInherited.Size = new System.Drawing.Size(104, 24);
           this.chkSendInherited.TabIndex = 32;
           this.chkSendInherited.Text = "Send Inherited";
           this.chkSendInherited.CheckedChanged += new System.EventHandler(this.chkSendInherited_CheckedChanged);
           // 
           // chkSendFunctions
           // 
           this.chkSendFunctions.Location = new System.Drawing.Point(240, 32);
           this.chkSendFunctions.Name = "chkSendFunctions";
           this.chkSendFunctions.Size = new System.Drawing.Size(104, 24);
           this.chkSendFunctions.TabIndex = 31;
           this.chkSendFunctions.Text = "Send Functions";
           this.chkSendFunctions.CheckedChanged += new System.EventHandler(this.chkSendFunctions_CheckedChanged);
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
           this.tabPage3.Size = new System.Drawing.Size(756, 522);
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
           // tabPage4
           // 
           this.tabPage4.Controls.Add(this.butCloseWin);
           this.tabPage4.Controls.Add(this.butWinLoadXml);
           this.tabPage4.Controls.Add(this.butSaveWinToXml);
           this.tabPage4.Controls.Add(this.butSaveWinToTxt);
           this.tabPage4.Controls.Add(this.butClearWin);
           this.tabPage4.Controls.Add(this.butDisplayWin);
           this.tabPage4.Controls.Add(this.butHelloToWintrace);
           this.tabPage4.Controls.Add(this.butCreateWinTrace);
           this.tabPage4.Location = new System.Drawing.Point(4, 22);
           this.tabPage4.Name = "tabPage4";
           this.tabPage4.Size = new System.Drawing.Size(756, 522);
           this.tabPage4.TabIndex = 2;
           this.tabPage4.Text = "Multi Pages";
           // 
           // butCloseWin
           // 
           this.butCloseWin.Enabled = false;
           this.butCloseWin.Location = new System.Drawing.Point(16, 413);
           this.butCloseWin.Name = "butCloseWin";
           this.butCloseWin.Size = new System.Drawing.Size(168, 23);
           this.butCloseWin.TabIndex = 44;
           this.butCloseWin.Text = "Close win";
           this.butCloseWin.UseVisualStyleBackColor = true;
           this.butCloseWin.Click += new System.EventHandler(this.butCloseWin_Click);
           // 
           // butWinLoadXml
           // 
           this.butWinLoadXml.Enabled = false;
           this.butWinLoadXml.Location = new System.Drawing.Point(224, 352);
           this.butWinLoadXml.Name = "butWinLoadXml";
           this.butWinLoadXml.Size = new System.Drawing.Size(160, 24);
           this.butWinLoadXml.TabIndex = 43;
           this.butWinLoadXml.Text = "LoadXml  (\"c:\\log2.xml\")";
           this.butWinLoadXml.Click += new System.EventHandler(this.butWinLoadXml_Click);
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
           this.tabPage5.Controls.Add(this.butWinWatchClose);
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
           this.tabPage5.Size = new System.Drawing.Size(756, 522);
           this.tabPage5.TabIndex = 5;
           this.tabPage5.Text = "Watches";
           // 
           // butWinWatchClose
           // 
           this.butWinWatchClose.Location = new System.Drawing.Point(296, 206);
           this.butWinWatchClose.Name = "butWinWatchClose";
           this.butWinWatchClose.Size = new System.Drawing.Size(144, 23);
           this.butWinWatchClose.TabIndex = 8;
           this.butWinWatchClose.Text = "Close Watch window";
           this.butWinWatchClose.UseVisualStyleBackColor = true;
           this.butWinWatchClose.Click += new System.EventHandler(this.butWinWatchClose_Click);
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
           this.tabPage2.Size = new System.Drawing.Size(756, 522);
           this.tabPage2.TabIndex = 1;
           this.tabPage2.Text = "Nodes operations";
           // 
           // butShowNode
           // 
           this.butShowNode.Enabled = false;
           this.butShowNode.Location = new System.Drawing.Point(288, 96);
           this.butShowNode.Name = "butShowNode";
           this.butShowNode.Size = new System.Drawing.Size(75, 23);
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
           this.butstart2.Size = new System.Drawing.Size(75, 23);
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
           // butStart1
           // 
           this.butStart1.Location = new System.Drawing.Point(16, 24);
           this.butStart1.Name = "butStart1";
           this.butStart1.Size = new System.Drawing.Size(75, 23);
           this.butStart1.TabIndex = 0;
           this.butStart1.Text = "Start 1...";
           this.butStart1.Click += new System.EventHandler(this.butStart1_Click);
           // 
           // panel1
           // 
           this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
           this.panel1.Controls.Add(this.butCloseViewer);
           this.panel1.Controls.Add(this.butShowtrace);
           this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
           this.panel1.Location = new System.Drawing.Point(0, 500);
           this.panel1.Name = "panel1";
           this.panel1.Size = new System.Drawing.Size(764, 48);
           this.panel1.TabIndex = 32;
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
           // butCloseViewer
           // 
           this.butCloseViewer.Location = new System.Drawing.Point(645, 16);
           this.butCloseViewer.Name = "butCloseViewer";
           this.butCloseViewer.Size = new System.Drawing.Size(94, 23);
           this.butCloseViewer.TabIndex = 30;
           this.butCloseViewer.Text = "Close Viewer";
           this.butCloseViewer.UseVisualStyleBackColor = true;
           this.butCloseViewer.Click += new System.EventHandler(this.butCloseViewer_Click);
           // 
           // WinF2
           // 
           this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
           this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
           this.ClientSize = new System.Drawing.Size(764, 548);
           this.Controls.Add(this.panel1);
           this.Controls.Add(this.tabControl1);
           this.Name = "WinF2";
           this.Text = "TraceTool for Dot Net framework 2";
           this.Load += new System.EventHandler(this.Win2Form_Load);
           this.tabControl1.ResumeLayout(false);
           this.tabPage1.ResumeLayout(false);
           this.tabPage1.PerformLayout();
           ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
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

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.Button butLoadXml;
        private System.Windows.Forms.Button butSaveToXml;
        private System.Windows.Forms.Button butSaveToTXT;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Button IndentButton;
        private System.Windows.Forms.Button MulticolBut;
        private System.Windows.Forms.CheckBox chkSendProcessName;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox chkSendEvents;
        private System.Windows.Forms.CheckBox chkSendInherited;
        private System.Windows.Forms.CheckBox chkSendFunctions;
        private System.Windows.Forms.ComboBox comboBox1;
        public System.Windows.Forms.Button butTrace;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button butDoc;
        private System.Windows.Forms.Button butFullInfo;
        private System.Windows.Forms.Button butVariant;
        private System.Windows.Forms.Button butClear;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.Button butEventLog;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Button butDiagnostic;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button butListenerWriteLine;
        private System.Windows.Forms.Button butListenerInit;
        private System.Windows.Forms.Button butListenerWrite;
        private System.Windows.Forms.Button butOldTrace;
        private System.Windows.Forms.Button butTail;
        private System.Windows.Forms.TabPage tabPage4;
        private System.Windows.Forms.Button butWinLoadXml;
        private System.Windows.Forms.Button butSaveWinToXml;
        private System.Windows.Forms.Button butSaveWinToTxt;
        private System.Windows.Forms.Button butClearWin;
        private System.Windows.Forms.Button butDisplayWin;
        private System.Windows.Forms.Button butHelloToWintrace;
        private System.Windows.Forms.Button butCreateWinTrace;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Button butWinWatchDisplay;
        private System.Windows.Forms.Button butWinWatchClear;
        private System.Windows.Forms.Button butWinWatchSend;
        private System.Windows.Forms.Button butCreateWinWatch;
        private System.Windows.Forms.Button butDisplayWatchWindow;
        private System.Windows.Forms.Button butClearWatchWindow;
        private System.Windows.Forms.Button butWatch;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.Button butShowNode;
        private System.Windows.Forms.Button butSetSelected;
        private System.Windows.Forms.Button butEnd2;
        private System.Windows.Forms.Button butstart2;
        private System.Windows.Forms.Button butDone1;
        private System.Windows.Forms.Button butStart1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button butShowtrace;
       private System.Windows.Forms.PictureBox pictureBox1;
       private System.Windows.Forms.Button butCloseWin;
       private System.Windows.Forms.Button butWinWatchClose;
       private System.Windows.Forms.Button butCloseViewer;
    }
}