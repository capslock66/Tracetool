library DelphiNetPlugin;

// this plugin is implemented as a window plugin (don't implement ITracePlugin)
// but use the Dot net tracetool api to communicate with the viewer


// if you have problem running or compiling the plugin, ensure you have the correct
// version of tracetool. if you got problem, quit delphi and delete the TraceTool.dcpil file
// then rebuild the target

{%File 'ModelSupport\default.txvpck'}
{%DelphiDotNetAssemblyCompiler '..\..\dotnet\vs2003\windows\csharp library\bin\debug\TraceTool.dll'}

uses
  SysUtils,
  Classes,
  System.Reflection,
  System.IO,
  System.Runtime.InteropServices,
  tracetool,                       // dot net version of tracetool
  System.Text;

[assembly: AssemblyTitle('')]
[assembly: AssemblyDescription('')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('')]
[assembly: AssemblyProduct('')]
[assembly: AssemblyCopyright('')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]
[assembly: AssemblyVersion('1.0.*')]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]
[assembly: ComVisible(False)]


{$UNSAFECODE ON}

type
   TbyteArray = array of byte ;

const
   PlugName = 'DelphiNetPlugin' ;
var
   PlugTraces : WinTrace ;
   ActionNodes , BeforeDeleteNodes, Timer : TraceNodeEx;


procedure Start() stdcall ; unsafe; ilcode ;
begin

   PlugTraces := WinTrace.Create ('DNET' , 'Delphi net Plugin') ;
   PlugTraces.DisplayWin ;

   //TraceConstInst := TraceConst.create ;


   // attach the window to the plugin (itself)
   PlugTraces.LinkToPlugin (PlugName, TraceConst.CST_PLUG_ONACTION + TraceConst.CST_PLUG_ONBEFOREDELETE + TraceConst.CST_PLUG_ONTIMER) ;

   // disable the  LogFile label
   PlugTraces.DisableResource (TraceConst.CST_ACTION_LABEL_LOGFILE) ;

   // add a menu to the 'window' menu
   PlugTraces.CreateResource (100,TraceConst.CST_RES_MENU_WINDOW,0,'My Delphi Net Plug');

   // add a menu to the 'action' menu
   PlugTraces.CreateResource (101,TraceConst.CST_RES_MENU_ACTION,0,'My action Plug');

   // add a label on right, autosize (0)
   PlugTraces.CreateResource (102,TraceConst.CST_RES_LABEL_RIGHT,0,'My label');

   // add a button on right (100 pixels)
   PlugTraces.CreateResource (103,TraceConst.CST_RES_BUT_RIGHT,100,'STOP');

   // add a label on left, 100 pixels
   PlugTraces.CreateResource (104,TraceConst.CST_RES_LABEL_LEFT,100,'My status');

   ActionNodes := TraceNodeEx.Create(PlugTraces.debug);
   ActionNodes.LeftMsg := 'Actions' ;
   ActionNodes.id := 'ActionsNode' ;
   ActionNodes .send ;

   BeforeDeleteNodes := TraceNodeEx.Create(PlugTraces.debug);
   BeforeDeleteNodes.LeftMsg := 'Deleted Nodes' ;
   BeforeDeleteNodes.id := 'BeforeDeletes' ;
   BeforeDeleteNodes.send ;

   Timer := TraceNodeEx.Create(PlugTraces.debug);
   Timer.LeftMsg := 'Timer' ;
   Timer.id := 'Timer' ;
   Timer.send ;

   PlugTraces.debug.send ('Sample Delphi Net plugin started');
end ;

//------------------------------------------------------------------------------

// stop the plugin

procedure Stop() stdcall ; unsafe; ilcode ;
begin
   PlugTraces.Debug.send ('plugin stopped') ;
   ttrace.Flush();
end ;

//------------------------------------------------------------------------------
// get the plugin name.
// lpFileName : buffer where to store the plugin name. ANSI : One byte per char
// nSize      : max buffer size
procedure GetPlugName (lpPlugName: PAnsiChar; nSize: LongWord) ; unsafe; ilcode ;
var
   c,d : integer ;
   bytePlugName : array of byte  ;
begin
   // convert widestring to ASCII then copy byte per byte
   bytePlugName := Encoding.ASCII.GetBytes(PlugName) ;

   d := length (PlugName) ;
   for c := 0 to d -1 do
      lpPlugName[c] := AnsiChar (bytePlugName[c]) ;
   lpPlugName[d] := #0 ;
end ;


//------------------------------------------------------------------------------
// called when the user click on a button, label or menu on a WinTrace.
// The plugin must call LinkToPlugin in order to receive this event
// WinId    : Wintrace Id
// ResourceId : Button Id
// return   : when true  : tracetool perform the default action
//            when false : tracetool don't perform any action
function OnAction (WinId : string ; ResourceId : integer; NodeId : string) : LongBool ; unsafe; ilcode ;
begin
   result := true ; // perform default action
   ActionNodes.send ('OnAction. WinId : ' + WinId + ', ResourceId : ' + inttostr(ResourceId) + ', current NodeId : ' + NodeId) ;

   // demo : disable close button
   if ResourceId = TraceConst.CST_ACTION_CLOSE_WIN then
      result := false ;

end ;

//------------------------------------------------------------------------------
// called when a node is to be deleted on a WinTrace
// The plugin must call LinkToPlugin in order to receive this event
// WinId    : Wintrace Id
// NodeId   : node Id
// return   : when true  : tracetool delete the node
//            when false : tracetool don't delete the node
function OnBeforeDelete (WinId : string ; NodeId : string) : longBool ; unsafe; ilcode ;
begin
   result := true ;
   BeforeDeleteNodes.ResendRight ('last = ' + NodeId) ;
   if (nodeId = 'BeforeDeletes') or (nodeId = 'ActionsNode') or (nodeId = 'Timer') then
      result := false ;
end ;

//------------------------------------------------------------------------------
// called every 500 ms. Can be used for example to refresh labels
// The plugin must call LinkToPlugin in order to receive this event
procedure OnTimer() ; unsafe; ilcode ;
begin
   // perform action...
   PlugTraces.SetTextResource (102, 'My status ' + TimeToStr(now)) ;
   Timer.ResendLeft ('Timer ' + TimeToStr(now)) ;
end ;

//------------------------------------------------------------------------------


exports
   GetPlugName , OnAction, OnBeforeDelete, OnTimer , Start , Stop  ;

begin
   PlugTraces := nil ;
   ActionNodes := nil ;
   BeforeDeleteNodes := nil ;
   Timer := nil ;
end.
