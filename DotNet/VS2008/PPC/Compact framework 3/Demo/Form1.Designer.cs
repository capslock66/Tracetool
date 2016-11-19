namespace Demo
{
   partial class Form1
   {
      /// <summary>
      /// Required designer variable.
      /// </summary>
      private System.ComponentModel.IContainer components = null;
      private System.Windows.Forms.MainMenu mainMenu1;

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
         this.mainMenu1 = new System.Windows.Forms.MainMenu();
         this.ButPartner = new System.Windows.Forms.Button();
         this.ButShowHosts = new System.Windows.Forms.Button();
         this.butSample = new System.Windows.Forms.Button();
         this.butTextIp = new System.Windows.Forms.Button();
         this.textBoxIP = new System.Windows.Forms.TextBox();
         this.listBox = new System.Windows.Forms.ListBox();
         this.butPPP_PER = new System.Windows.Forms.Button();
         this.butClear = new System.Windows.Forms.Button();
         this.SuspendLayout();
         // 
         // ButPartner
         // 
         this.ButPartner.Location = new System.Drawing.Point(3, 12);
         this.ButPartner.Name = "ButPartner";
         this.ButPartner.Size = new System.Drawing.Size(125, 19);
         this.ButPartner.TabIndex = 0;
         this.ButPartner.Text = "Show Partner";
         this.ButPartner.Click += new System.EventHandler(this.ButPartner_Click);
         // 
         // ButShowHosts
         // 
         this.ButShowHosts.Location = new System.Drawing.Point(134, 12);
         this.ButShowHosts.Name = "ButShowHosts";
         this.ButShowHosts.Size = new System.Drawing.Size(103, 19);
         this.ButShowHosts.TabIndex = 1;
         this.ButShowHosts.Text = "Show Hosts";
         this.ButShowHosts.Click += new System.EventHandler(this.ButShowHosts_Click);
         // 
         // butSample
         // 
         this.butSample.Location = new System.Drawing.Point(3, 245);
         this.butSample.Name = "butSample";
         this.butSample.Size = new System.Drawing.Size(222, 19);
         this.butSample.TabIndex = 2;
         this.butSample.Text = "Tracetool Sample using this ip";
         this.butSample.Click += new System.EventHandler(this.butSample_Click);
         // 
         // butTextIp
         // 
         this.butTextIp.Location = new System.Drawing.Point(3, 212);
         this.butTextIp.Name = "butTextIp";
         this.butTextIp.Size = new System.Drawing.Size(125, 19);
         this.butTextIp.TabIndex = 3;
         this.butTextIp.Text = "check connection";
         this.butTextIp.Click += new System.EventHandler(this.butTextIp_Click);
         // 
         // textBoxIP
         // 
         this.textBoxIP.Location = new System.Drawing.Point(134, 212);
         this.textBoxIP.Name = "textBoxIP";
         this.textBoxIP.Size = new System.Drawing.Size(103, 21);
         this.textBoxIP.TabIndex = 4;
         this.textBoxIP.Text = "10.0.2.15";
         // 
         // listBox
         // 
         this.listBox.Location = new System.Drawing.Point(3, 66);
         this.listBox.Name = "listBox";
         this.listBox.Size = new System.Drawing.Size(234, 128);
         this.listBox.TabIndex = 5;
         this.listBox.SelectedValueChanged += new System.EventHandler(this.listBox_SelectedValueChanged);
         // 
         // butPPP_PER
         // 
         this.butPPP_PER.Location = new System.Drawing.Point(3, 38);
         this.butPPP_PER.Name = "butPPP_PER";
         this.butPPP_PER.Size = new System.Drawing.Size(125, 19);
         this.butPPP_PER.TabIndex = 6;
         this.butPPP_PER.Text = "Show PPP_PER";
         this.butPPP_PER.Click += new System.EventHandler(this.butPPP_PER_Click);
         // 
         // butClear
         // 
         this.butClear.Location = new System.Drawing.Point(134, 38);
         this.butClear.Name = "butClear";
         this.butClear.Size = new System.Drawing.Size(103, 19);
         this.butClear.TabIndex = 7;
         this.butClear.Text = "clear";
         this.butClear.Click += new System.EventHandler(this.butClear_Click);
         // 
         // Form1
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
         this.AutoScroll = true;
         this.ClientSize = new System.Drawing.Size(240, 268);
         this.Controls.Add(this.butClear);
         this.Controls.Add(this.butPPP_PER);
         this.Controls.Add(this.listBox);
         this.Controls.Add(this.textBoxIP);
         this.Controls.Add(this.butTextIp);
         this.Controls.Add(this.butSample);
         this.Controls.Add(this.ButShowHosts);
         this.Controls.Add(this.ButPartner);
         this.Menu = this.mainMenu1;
         this.MinimizeBox = false;
         this.Name = "Form1";
         this.Text = "Compact framework 3 demo";
         this.ResumeLayout(false);

      }

      #endregion

      private System.Windows.Forms.Button ButPartner;
      private System.Windows.Forms.Button ButShowHosts;
      private System.Windows.Forms.Button butSample;
      private System.Windows.Forms.Button butTextIp;
      private System.Windows.Forms.TextBox textBoxIP;
      private System.Windows.Forms.ListBox listBox;
      private System.Windows.Forms.Button butPPP_PER;
      private System.Windows.Forms.Button butClear;
   }
}

