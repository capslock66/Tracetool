// Import log4net classes.
using System;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Forms;
using log4net;

// see AssemblyInfo.cs for log4Net Configuration

namespace Log4NetDemo
{
   /// <summary>
   /// Summary description for Form1.
   /// </summary>
   public class Form1 : Form
   {
      private Button butLog;
      private Button button1;

      /// <summary>
      /// Required designer variable.
      /// </summary>
      private Container components = null;

      public Form1()
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
      protected override void Dispose(bool disposing)
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
         this.butLog = new System.Windows.Forms.Button();
         this.button1 = new System.Windows.Forms.Button();
         this.SuspendLayout();
         // 
         // butLog
         // 
         this.butLog.Location = new System.Drawing.Point(40, 48);
         this.butLog.Name = "butLog";
         this.butLog.Size = new System.Drawing.Size(140, 23);
         this.butLog.TabIndex = 0;
         this.butLog.Text = "Log4Net test";
         this.butLog.Click += new System.EventHandler(this.butLog_Click);
         // 
         // button1
         // 
         this.button1.Location = new System.Drawing.Point(40, 92);
         this.button1.Name = "button1";
         this.button1.Size = new System.Drawing.Size(140, 23);
         this.button1.TabIndex = 1;
         this.button1.Text = "Send 2000 messages";
         this.button1.UseVisualStyleBackColor = true;
         this.button1.Click += new System.EventHandler(this.button1_Click);
         // 
         // Form1
         // 
         this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
         this.ClientSize = new System.Drawing.Size(292, 266);
         this.Controls.Add(this.button1);
         this.Controls.Add(this.butLog);
         this.Name = "Form1";
         this.Text = "Form1";
         this.ResumeLayout(false);

      }

      #endregion

      /// <summary>
      /// The main entry point for the application.
      /// </summary>
      [STAThread]
      static void Main()
      {
         Application.Run(new Form1());
      }

      // Create a logger for use in this class (Log4NetDemo.Form1)
      private static ILog log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

      private void butLog_Click(object sender, EventArgs e)
      {
         // Log4Net is configured by the "assembly: log4net.Config.DOMConfigurator" 
         // directive applied on the Log4NetDemo namespace
         
         // simple test
         log.Info("Hello world");
         log.Info("Hello \n world");
         log.Info("Hello \t world");

         // use an object as message (the object is displayed in the info panel)
         log.Debug(log);  

         // exception test (the exception is displayed in the info panel)
         Exception MyException = new Exception("my exception") ;
         log.Error("Received exception",MyException);
        
      }

      private void button1_Click(object sender, EventArgs e)
      {
         for (int c = 0 ; c <= 2000 ; c++)
            log.Info("trace" + c);

         log.Info("last");
      }
   }
}
