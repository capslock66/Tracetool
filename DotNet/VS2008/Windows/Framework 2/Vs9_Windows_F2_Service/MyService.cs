using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.ServiceProcess;
using System.Text;
using TraceTool;

namespace WindowsService
{
   public partial class MyService : ServiceBase
   {
      public MyService()
      {
         InitializeComponent();
      }

      protected override void OnStart(string[] args)
      {
         // use socket layer, on default port 8090
         TTrace.Options.SendMode = SendMode.Socket ;

         TTrace.Debug.Send("MyService","OnStart");

         // TraceTool, Version=10.1.0.0, Culture=neutral, PublicKeyToken=81da3f4827b33fbd
         TTrace.Debug.Send("tracetool fullname", typeof(TTrace).Assembly.FullName.ToString());

         // D:\TraceTool\DotNet\VS2008\Windows\Framework 2\Vs9_Windows_F2_Service\bin\debug\TraceTool.dll
         TTrace.Debug.Send("tracetool location", typeof(TTrace).Assembly.Location.ToString());
      }

      protected override void OnStop()
      {
      }
   }
}
