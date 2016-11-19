using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;
using TraceTool ;

namespace AspTraces
{
	/// <summary>
	/// Summary description for WebForm1.
	/// </summary>
	public class WebForm1 : System.Web.UI.Page  
	{
      protected System.Web.UI.WebControls.Button Button1;
   
		private void Page_Load(object sender, System.EventArgs e)
		{
         TTrace.Options.SendMode = SendMode.Socket ; 
         TTrace.Debug.Send ("Page_Load " ) ; // + Convert.ToString(TTrace.TraceCount));
		}

		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			//
			// CODEGEN: This call is required by the ASP.NET Web Form Designer.
			//
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
         this.Button1.Click += new System.EventHandler(this.Button1_Click);
         this.Load += new System.EventHandler(this.Page_Load);

      }
		#endregion

      private void Button1_Click(object sender, System.EventArgs e)
      {
         TTrace.Debug.SendObject ("sender object", sender) ;       
         TTrace.Debug.SendValue ("sender value", sender) ;       
      }
	}
}
