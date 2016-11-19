namespace PPC2003
{
    partial class PPC2003CF2
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
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.tabPage4.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Controls.Add(this.tabPage4);
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(240, 291);
            this.tabControl1.TabIndex = 1;
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
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Size = new System.Drawing.Size(240, 268);
            this.tabPage1.Text = "Basic";
            // 
            // butClearMainTraces
            // 
            this.butClearMainTraces.Location = new System.Drawing.Point(122, 220);
            this.butClearMainTraces.Name = "butClearMainTraces";
            this.butClearMainTraces.Size = new System.Drawing.Size(111, 20);
            this.butClearMainTraces.TabIndex = 0;
            this.butClearMainTraces.Text = "Clear main traces";
            this.butClearMainTraces.Click += new System.EventHandler(this.butClearMainTraces_Click);
            // 
            // butShowViewer
            // 
            this.butShowViewer.Location = new System.Drawing.Point(4, 220);
            this.butShowViewer.Name = "butShowViewer";
            this.butShowViewer.Size = new System.Drawing.Size(113, 20);
            this.butShowViewer.TabIndex = 1;
            this.butShowViewer.Text = "Show Viewer";
            this.butShowViewer.Click += new System.EventHandler(this.butShowViewer_Click);
            // 
            // butLoadXml
            // 
            this.butLoadXml.Location = new System.Drawing.Point(4, 184);
            this.butLoadXml.Name = "butLoadXml";
            this.butLoadXml.Size = new System.Drawing.Size(231, 20);
            this.butLoadXml.TabIndex = 2;
            this.butLoadXml.Text = "LoadXml(\"c:\\log.xml\")";
            this.butLoadXml.Click += new System.EventHandler(this.butLoadXml_Click);
            // 
            // butSaveToXml
            // 
            this.butSaveToXml.Location = new System.Drawing.Point(4, 148);
            this.butSaveToXml.Name = "butSaveToXml";
            this.butSaveToXml.Size = new System.Drawing.Size(231, 20);
            this.butSaveToXml.TabIndex = 3;
            this.butSaveToXml.Text = "SaveToXml(\"c:\\log.xml\")";
            this.butSaveToXml.Click += new System.EventHandler(this.butSaveToXml_Click);
            // 
            // butSaveTotext
            // 
            this.butSaveTotext.Location = new System.Drawing.Point(4, 112);
            this.butSaveTotext.Name = "butSaveTotext";
            this.butSaveTotext.Size = new System.Drawing.Size(231, 20);
            this.butSaveTotext.TabIndex = 4;
            this.butSaveTotext.Text = "SaveToText(\"c:\\log.txt\")";
            this.butSaveTotext.Click += new System.EventHandler(this.butSaveTotext_Click);
            // 
            // butIndent
            // 
            this.butIndent.Location = new System.Drawing.Point(122, 76);
            this.butIndent.Name = "butIndent";
            this.butIndent.Size = new System.Drawing.Size(113, 20);
            this.butIndent.TabIndex = 5;
            this.butIndent.Text = "Indent/Unindent";
            this.butIndent.Click += new System.EventHandler(this.butIndent_Click);
            // 
            // butSample
            // 
            this.butSample.Location = new System.Drawing.Point(4, 76);
            this.butSample.Name = "butSample";
            this.butSample.Size = new System.Drawing.Size(113, 20);
            this.butSample.TabIndex = 6;
            this.butSample.Text = "Sample traces";
            this.butSample.Click += new System.EventHandler(this.butSample_Click);
            // 
            // ButShowHosts
            // 
            this.ButShowHosts.Location = new System.Drawing.Point(122, 40);
            this.ButShowHosts.Name = "ButShowHosts";
            this.ButShowHosts.Size = new System.Drawing.Size(113, 20);
            this.ButShowHosts.TabIndex = 7;
            this.ButShowHosts.Text = "Show Hosts";
            this.ButShowHosts.Click += new System.EventHandler(this.ButShowHosts_Click);
            // 
            // ButPartner
            // 
            this.ButPartner.Location = new System.Drawing.Point(4, 40);
            this.ButPartner.Name = "ButPartner";
            this.ButPartner.Size = new System.Drawing.Size(113, 20);
            this.ButPartner.TabIndex = 8;
            this.ButPartner.Text = "Show Partner";
            this.ButPartner.Click += new System.EventHandler(this.ButPartner_Click);
            // 
            // label1
            // 
            this.label1.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
            this.label1.Location = new System.Drawing.Point(0, 0);
            this.label1.Name = "label1";
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
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Size = new System.Drawing.Size(240, 268);
            this.tabPage2.Text = "Node Op.";
            // 
            // butShowNode
            // 
            this.butShowNode.Location = new System.Drawing.Point(61, 232);
            this.butShowNode.Name = "butShowNode";
            this.butShowNode.Size = new System.Drawing.Size(131, 20);
            this.butShowNode.TabIndex = 0;
            this.butShowNode.Text = "Show()";
            this.butShowNode.Click += new System.EventHandler(this.butShowNode_Click);
            // 
            // butAppend
            // 
            this.butAppend.Location = new System.Drawing.Point(61, 196);
            this.butAppend.Name = "butAppend";
            this.butAppend.Size = new System.Drawing.Size(131, 20);
            this.butAppend.TabIndex = 1;
            this.butAppend.Text = "Append()";
            this.butAppend.Click += new System.EventHandler(this.butAppend_Click);
            // 
            // butstart2
            // 
            this.butstart2.Location = new System.Drawing.Point(19, 160);
            this.butstart2.Name = "butstart2";
            this.butstart2.Size = new System.Drawing.Size(77, 20);
            this.butstart2.TabIndex = 2;
            this.butstart2.Text = "Start2";
            this.butstart2.Click += new System.EventHandler(this.butstart2_Click);
            // 
            // butNodeIndent
            // 
            this.butNodeIndent.Location = new System.Drawing.Point(61, 124);
            this.butNodeIndent.Name = "butNodeIndent";
            this.butNodeIndent.Size = new System.Drawing.Size(136, 20);
            this.butNodeIndent.TabIndex = 3;
            this.butNodeIndent.Text = "Indent/Unindent";
            this.butNodeIndent.Click += new System.EventHandler(this.butNodeIndent_Click);
            // 
            // butSetSelected
            // 
            this.butSetSelected.Location = new System.Drawing.Point(61, 88);
            this.butSetSelected.Name = "butSetSelected";
            this.butSetSelected.Size = new System.Drawing.Size(134, 20);
            this.butSetSelected.TabIndex = 4;
            this.butSetSelected.Text = "SetSelected()";
            this.butSetSelected.Click += new System.EventHandler(this.butSetSelected_Click);
            // 
            // butResend
            // 
            this.butResend.Location = new System.Drawing.Point(61, 52);
            this.butResend.Name = "butResend";
            this.butResend.Size = new System.Drawing.Size(134, 20);
            this.butResend.TabIndex = 5;
            this.butResend.Text = "Resend()";
            this.butResend.Click += new System.EventHandler(this.butResend_Click);
            // 
            // butStart1
            // 
            this.butStart1.Location = new System.Drawing.Point(19, 16);
            this.butStart1.Name = "butStart1";
            this.butStart1.Size = new System.Drawing.Size(77, 20);
            this.butStart1.TabIndex = 6;
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
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Size = new System.Drawing.Size(240, 268);
            this.tabPage3.Text = "Multi pages";
            // 
            // butMulticolTest
            // 
            this.butMulticolTest.Location = new System.Drawing.Point(8, 231);
            this.butMulticolTest.Name = "butMulticolTest";
            this.butMulticolTest.Size = new System.Drawing.Size(221, 20);
            this.butMulticolTest.TabIndex = 0;
            this.butMulticolTest.Text = "Multi col test";
            this.butMulticolTest.Click += new System.EventHandler(this.butMulticolTest_Click);
            // 
            // butLoadWinXml
            // 
            this.butLoadWinXml.Location = new System.Drawing.Point(8, 195);
            this.butLoadWinXml.Name = "butLoadWinXml";
            this.butLoadWinXml.Size = new System.Drawing.Size(221, 20);
            this.butLoadWinXml.TabIndex = 1;
            this.butLoadWinXml.Text = "LoadXml(\"c:\\log2.xml\")";
            this.butLoadWinXml.Click += new System.EventHandler(this.butLoadWinXml_Click);
            // 
            // butSaveWinToXml
            // 
            this.butSaveWinToXml.Location = new System.Drawing.Point(8, 159);
            this.butSaveWinToXml.Name = "butSaveWinToXml";
            this.butSaveWinToXml.Size = new System.Drawing.Size(221, 20);
            this.butSaveWinToXml.TabIndex = 2;
            this.butSaveWinToXml.Text = "SaveToXml(\"c:\\log2.xml\")";
            this.butSaveWinToXml.Click += new System.EventHandler(this.butSaveWinToXml_Click);
            // 
            // butSaveWinToText
            // 
            this.butSaveWinToText.Location = new System.Drawing.Point(8, 123);
            this.butSaveWinToText.Name = "butSaveWinToText";
            this.butSaveWinToText.Size = new System.Drawing.Size(221, 20);
            this.butSaveWinToText.TabIndex = 3;
            this.butSaveWinToText.Text = "SaveToText(\"c:\\log2.txt\")";
            this.butSaveWinToText.Click += new System.EventHandler(this.butSaveWinToText_Click);
            // 
            // butHelloWin
            // 
            this.butHelloWin.Location = new System.Drawing.Point(8, 87);
            this.butHelloWin.Name = "butHelloWin";
            this.butHelloWin.Size = new System.Drawing.Size(221, 20);
            this.butHelloWin.TabIndex = 4;
            this.butHelloWin.Text = "Say Hello";
            this.butHelloWin.Click += new System.EventHandler(this.butHelloWin_Click);
            // 
            // butDisplayWindow
            // 
            this.butDisplayWindow.Location = new System.Drawing.Point(8, 51);
            this.butDisplayWindow.Name = "butDisplayWindow";
            this.butDisplayWindow.Size = new System.Drawing.Size(221, 20);
            this.butDisplayWindow.TabIndex = 5;
            this.butDisplayWindow.Text = "display that window on the viewer";
            this.butDisplayWindow.Click += new System.EventHandler(this.butDisplayWindow_Click);
            // 
            // butCreateWindow
            // 
            this.butCreateWindow.Location = new System.Drawing.Point(8, 15);
            this.butCreateWindow.Name = "butCreateWindow";
            this.butCreateWindow.Size = new System.Drawing.Size(221, 20);
            this.butCreateWindow.TabIndex = 6;
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
            this.tabPage4.Name = "tabPage4";
            this.tabPage4.Size = new System.Drawing.Size(240, 268);
            this.tabPage4.Text = "Watches";
            // 
            // butClearWinWatches
            // 
            this.butClearWinWatches.Location = new System.Drawing.Point(42, 230);
            this.butClearWinWatches.Name = "butClearWinWatches";
            this.butClearWinWatches.Size = new System.Drawing.Size(180, 20);
            this.butClearWinWatches.TabIndex = 0;
            this.butClearWinWatches.Text = "Clear watch window";
            this.butClearWinWatches.Click += new System.EventHandler(this.butClearWinWatches_Click);
            // 
            // butDisplayWinWatches
            // 
            this.butDisplayWinWatches.Location = new System.Drawing.Point(42, 196);
            this.butDisplayWinWatches.Name = "butDisplayWinWatches";
            this.butDisplayWinWatches.Size = new System.Drawing.Size(180, 20);
            this.butDisplayWinWatches.TabIndex = 1;
            this.butDisplayWinWatches.Text = " Display watch window";
            this.butDisplayWinWatches.Click += new System.EventHandler(this.butDisplayWinWatches_Click);
            // 
            // butSendWinWatches
            // 
            this.butSendWinWatches.Location = new System.Drawing.Point(42, 162);
            this.butSendWinWatches.Name = "butSendWinWatches";
            this.butSendWinWatches.Size = new System.Drawing.Size(180, 20);
            this.butSendWinWatches.TabIndex = 2;
            this.butSendWinWatches.Text = "Send watches";
            this.butSendWinWatches.Click += new System.EventHandler(this.butSendWinWatches_Click);
            // 
            // butCreateWinWatches
            // 
            this.butCreateWinWatches.Location = new System.Drawing.Point(7, 128);
            this.butCreateWinWatches.Name = "butCreateWinWatches";
            this.butCreateWinWatches.Size = new System.Drawing.Size(168, 20);
            this.butCreateWinWatches.TabIndex = 3;
            this.butCreateWinWatches.Text = "Create new WinWatches";
            this.butCreateWinWatches.Click += new System.EventHandler(this.butCreateWinWatches_Click);
            // 
            // butClearMainWatches
            // 
            this.butClearMainWatches.Location = new System.Drawing.Point(42, 94);
            this.butClearMainWatches.Name = "butClearMainWatches";
            this.butClearMainWatches.Size = new System.Drawing.Size(180, 20);
            this.butClearMainWatches.TabIndex = 4;
            this.butClearMainWatches.Text = "Clear main watch win";
            this.butClearMainWatches.Click += new System.EventHandler(this.butClearMainWatches_Click);
            // 
            // butDisplayMainWatches
            // 
            this.butDisplayMainWatches.Location = new System.Drawing.Point(42, 60);
            this.butDisplayMainWatches.Name = "butDisplayMainWatches";
            this.butDisplayMainWatches.Size = new System.Drawing.Size(180, 20);
            this.butDisplayMainWatches.TabIndex = 5;
            this.butDisplayMainWatches.Text = " Display main watch win";
            this.butDisplayMainWatches.Click += new System.EventHandler(this.butDisplayMainWatches_Click);
            // 
            // butSendMainWatches
            // 
            this.butSendMainWatches.Location = new System.Drawing.Point(42, 26);
            this.butSendMainWatches.Name = "butSendMainWatches";
            this.butSendMainWatches.Size = new System.Drawing.Size(180, 20);
            this.butSendMainWatches.TabIndex = 6;
            this.butSendMainWatches.Text = "Send watches";
            this.butSendMainWatches.Click += new System.EventHandler(this.butSendMainWatches_Click);
            // 
            // label2
            // 
            this.label2.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
            this.label2.Location = new System.Drawing.Point(0, 4);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(237, 19);
            this.label2.Text = "Main watch window";
            // 
            // PPC2003CF2
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(240, 294);
            this.Controls.Add(this.tabControl1);
            this.Name = "PPC2003CF2";
            this.Text = "TraceTool for Dot net CF 2 demo";
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage2.ResumeLayout(false);
            this.tabPage3.ResumeLayout(false);
            this.tabPage4.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.Button butClearMainTraces;
        private System.Windows.Forms.Button butShowViewer;
        private System.Windows.Forms.Button butLoadXml;
        private System.Windows.Forms.Button butSaveToXml;
        private System.Windows.Forms.Button butSaveTotext;
        private System.Windows.Forms.Button butIndent;
        private System.Windows.Forms.Button butSample;
        private System.Windows.Forms.Button ButShowHosts;
        private System.Windows.Forms.Button ButPartner;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.Button butShowNode;
        private System.Windows.Forms.Button butAppend;
        private System.Windows.Forms.Button butstart2;
        private System.Windows.Forms.Button butNodeIndent;
        private System.Windows.Forms.Button butSetSelected;
        private System.Windows.Forms.Button butResend;
        private System.Windows.Forms.Button butStart1;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.Button butMulticolTest;
        private System.Windows.Forms.Button butLoadWinXml;
        private System.Windows.Forms.Button butSaveWinToXml;
        private System.Windows.Forms.Button butSaveWinToText;
        private System.Windows.Forms.Button butHelloWin;
        private System.Windows.Forms.Button butDisplayWindow;
        private System.Windows.Forms.Button butCreateWindow;
        private System.Windows.Forms.TabPage tabPage4;
        private System.Windows.Forms.Button butClearWinWatches;
        private System.Windows.Forms.Button butDisplayWinWatches;
        private System.Windows.Forms.Button butSendWinWatches;
        private System.Windows.Forms.Button butCreateWinWatches;
        private System.Windows.Forms.Button butClearMainWatches;
        private System.Windows.Forms.Button butDisplayMainWatches;
        private System.Windows.Forms.Button butSendMainWatches;
        private System.Windows.Forms.Label label2;

    }
}

