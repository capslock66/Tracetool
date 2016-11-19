using System.Reflection;
using System.Runtime.CompilerServices;
using log4net.Config;

//
// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly: AssemblyTitle("")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("")]
[assembly: AssemblyCopyright("")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]		
[assembly: AssemblyVersion("1.0.*")]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]

// Configure log4net using the 'SimpleApp.exe.log4net' file
[assembly: log4net.Config.XmlConfigurator(ConfigFileExtension="log4net", Watch=true)]  // 1.2.9 beta
//[assembly: log4net.Config.DOMConfigurator(ConfigFileExtension="log4net", Watch=true)]    // 1.2.0 beta8 


