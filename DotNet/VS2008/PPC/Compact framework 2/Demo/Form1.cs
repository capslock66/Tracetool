using System;

using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using System.Net;
using System.Net.Sockets;

using OpenNETCF.Win32; // see registry.cs in this project
using TraceTool;

namespace Vs9_PPC_F2_demo
{
   public partial class Form1 : Form
   {
      public Form1()
      {
         InitializeComponent();
      }
      void CheckSocket(string StrAdr)
      {
         IPAddress adr;
         IPHostEntry host = null;
         listBox.Items.Add("checking " + StrAdr);
         // get host
         try {
            // CF1
            //host = Dns.GetHostByName(StrAdr);

            // CF2
            host = Dns.GetHostEntry(StrAdr);

         } catch (Exception ex) {   // System.Net.Sockets.SocketException            
            listBox.Items.Add(ex.GetType().ToString());
            listBox.Items.Add(ex.Message);
            return;
         }

         if (host == null) {
            listBox.Items.Add("host is null");
            return;
         }

         adr = host.AddressList[0];
         listBox.Items.Add(adr.ToString());

         // create an end-point for the first address...
         IPEndPoint endPoint = new IPEndPoint(host.AddressList[0], 8090);

         Socket _Socket;
         _Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
         try {
            _Socket.Connect(endPoint);
         } catch (Exception ex) {
            listBox.Items.Add(ex.GetType().ToString());
            listBox.Items.Add(ex.Message);
            return;
         }
         listBox.Items.Add("Connection succeed");
         _Socket.Close();
      }

      private void ButPartner_Click(object sender, EventArgs e)
      {
         RegistryKey registryKey;

         registryKey = Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows CE Services\\Partners");

         Object PCur = registryKey.GetValue("PCur");
         if (PCur != null) {
            //TTrace.Debug.Send("PCur : " + PCur.ToString());

            registryKey = Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows CE Services\\Partners\\P" + PCur.ToString());
            string PName = (string)registryKey.GetValue("PName");
            listBox.Items.Add(PName);
            IPHostEntry hostEntry;
            try {
               hostEntry = Dns.GetHostEntry("PPP_PEER");

               foreach (IPAddress ip in hostEntry.AddressList) {
                  if (ip.AddressFamily == AddressFamily.InterNetwork) {
                     listBox.Items.Add(ip.ToString());
                  }
               }
            } catch (Exception ex) {
               listBox.Items.Add(ex.GetType().ToString());
               listBox.Items.Add(ex.Message);
            }

            //TTrace.Debug.Send("Partner Name : " + PName);
            //CheckSocket(PName);
         } else {
            listBox.Items.Add("No Partners");
         }
      }

      private void ButShowHosts_Click(object sender, EventArgs e)
      {
         RegistryKey registryKey;
         registryKey = Registry.LocalMachine.OpenSubKey("Comm\\Tcpip\\Hosts");

         string[] hostList = registryKey.GetSubKeyNames();
         if (hostList.Length == 0)
            listBox.Items.Add("No hosts");

         foreach (string Host in hostList) {
            // the first (and unique) host should be 192.168.55.100
            //TTrace.Debug.Send("Host : " + Host);
            //CheckSocket(Host);
            listBox.Items.Add(Host);
            IPHostEntry hostEntry;
            try {
               hostEntry = Dns.GetHostEntry(Host);

               foreach (IPAddress ip in hostEntry.AddressList) {
                  if (ip.AddressFamily == AddressFamily.InterNetwork) {
                     listBox.Items.Add(ip.ToString());
                  }
               }
            } catch (Exception ex) {
               listBox.Items.Add(ex.GetType().ToString());
               listBox.Items.Add(ex.Message);
            }
         }
      }

      private void butPPP_PER_Click(object sender, EventArgs e)
      {
         listBox.Items.Add("PPP_PEER");
         IPHostEntry hostEntry;
         try {
            hostEntry = Dns.GetHostEntry("PPP_PEER");

            foreach (IPAddress ip in hostEntry.AddressList) {
               if (ip.AddressFamily == AddressFamily.InterNetwork) {
                  listBox.Items.Add(ip.ToString());
               }
            }
         } catch (Exception ex) {
            listBox.Items.Add(ex.GetType().ToString());
            listBox.Items.Add(ex.Message);
         }
      }

      private void butClear_Click(object sender, EventArgs e)
      {
         listBox.Items.Clear();
      }

      private void listBox_SelectedValueChanged(object sender, EventArgs e)
      {
         textBoxIP.Text = listBox.Text;
      }

      private void butTextIp_Click(object sender, EventArgs e)
      {
         CheckSocket(textBoxIP.Text);
      }

      private void butSample_Click(object sender, EventArgs e)
      {
         // use the selected ip
         TTrace.Options.SocketHost = textBoxIP.Text;

         TTrace.Debug.Send("CF3 demo");

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

         // Linq : not available in compact framework 2

         TTrace.Flush();
      }

   }
}