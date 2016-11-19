namespace Vs9_PPC_F2_demo
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
         this.butClear = new System.Windows.Forms.Button();
         this.butPPP_PER = new System.Windows.Forms.Button();
         this.listBox = new System.Windows.Forms.ListBox();
         this.textBoxIP = new System.Windows.Forms.TextBox();
         this.butTextIp = new System.Windows.Forms.Button();
         this.butSample = new System.Windows.Forms.Button();
         this.ButShowHosts = new System.Windows.Forms.Button();
         this.ButPartner = new System.Windows.Forms.Button();
         this.SuspendLayout();
         // 
         // butClear
         // 
         this.butClear.Location = new System.Drawing.Point(134, 34);
         this.butClear.Name = "butClear";
         this.butClear.Size = new System.Drawing.Size(103, 20);
         this.butClear.TabIndex = 15;
         this.butClear.Text = "clear";
         this.butClear.Click += new System.EventHandler(this.butClear_Click);
         // 
         // butPPP_PER
         // 
         this.butPPP_PER.Location = new System.Drawing.Point(3, 34);
         this.butPPP_PER.Name = "butPPP_PER";
         this.butPPP_PER.Size = new System.Drawing.Size(125, 20);
         this.butPPP_PER.TabIndex = 14;
         this.butPPP_PER.Text = "Show PPP_PER";
         this.butPPP_PER.Click += new System.EventHandler(this.butPPP_PER_Click);
         // 
         // listBox
         // 
         this.listBox.Location = new System.Drawing.Point(3, 62);
         this.listBox.Name = "listBox";
         this.listBox.Size = new System.Drawing.Size(234, 142);
         this.listBox.TabIndex = 13;
         this.listBox.SelectedValueChanged += new System.EventHandler(this.listBox_SelectedValueChanged);
         // 
         // textBoxIP
         // 
         this.textBoxIP.Location = new System.Drawing.Point(134, 208);
         this.textBoxIP.Name = "textBoxIP";
         this.textBoxIP.Size = new System.Drawing.Size(103, 21);
         this.textBoxIP.TabIndex = 12;
         this.textBoxIP.Text = "10.0.2.15";
         // 
         // butTextIp
         // 
         this.butTextIp.Location = new System.Drawing.Point(3, 208);
         this.butTextIp.Name = "butTextIp";
         this.butTextIp.Size = new System.Drawing.Size(125, 20);
         this.butTextIp.TabIndex = 11;
         this.butTextIp.Text = "check connection";
         this.butTextIp.Click += new System.EventHandler(this.butTextIp_Click);
         // 
         // butSample
         // 
         this.butSample.Location = new System.Drawing.Point(3, 241);
         this.butSample.Name = "butSample";
         this.butSample.Size = new System.Drawing.Size(222, 20);
         this.butSample.TabIndex = 10;
         this.butSample.Text = "Tracetool Sample using this ip";
         this.butSample.Click += new System.EventHandler(this.butSample_Click);
         // 
         // ButShowHosts
         // 
         this.ButShowHosts.Location = new System.Drawing.Point(134, 8);
         this.ButShowHosts.Name = "ButShowHosts";
         this.ButShowHosts.Size = new System.Drawing.Size(103, 20);
         this.ButShowHosts.TabIndex = 9;
         this.ButShowHosts.Text = "Show Hosts";
         this.ButShowHosts.Click += new System.EventHandler(this.ButShowHosts_Click);
         // 
         // ButPartner
         // 
         this.ButPartner.Location = new System.Drawing.Point(3, 8);
         this.ButPartner.Name = "ButPartner";
         this.ButPartner.Size = new System.Drawing.Size(125, 20);
         this.ButPartner.TabIndex = 8;
         this.ButPartner.Text = "Show Partner";
         this.ButPartner.Click += new System.EventHandler(this.ButPartner_Click);
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
         this.Text = "Compact framework 2 demo";
         this.ResumeLayout(false);

      }

      #endregion

      private System.Windows.Forms.Button butClear;
      private System.Windows.Forms.Button butPPP_PER;
      private System.Windows.Forms.ListBox listBox;
      private System.Windows.Forms.TextBox textBoxIP;
      private System.Windows.Forms.Button butTextIp;
      private System.Windows.Forms.Button butSample;
      private System.Windows.Forms.Button ButShowHosts;
      private System.Windows.Forms.Button ButPartner;
   }
}

