unit Sample;

interface

uses windows, SysUtils, tracetool ;

procedure GetPlugName (lpPlugName: PChar) stdcall ;
function  OnAction (WinId : PChar ; ResourceId : integer; NodeId : PChar) : BOOL stdcall ;
function  OnBeforeDelete (WinId : PChar ; NodeId : PChar) : BOOL stdcall ;
procedure OnTimer() stdcall ;
procedure Stop()  stdcall ;
procedure Start() stdcall ;

const
   PlugName = 'D7Plugin' ;
var
   PlugTraces : IWinTrace ;
   ActionNodes , BeforeDeleteNodes, Timer : ITraceNode ;
   
implementation

//------------------------------------------------------------------------------
// initialise the plugin
procedure Start() stdcall ;
var
   NodeEx : ITraceNodeEx ;
begin
   TTrace.start() ; // run trace sub system if stopped

   PlugTraces := TTrace.createWinTrace('D7' , 'Delphi Win32 Plugin') ;
   PlugTraces.DisplayWin ;

   // attach the window to the plugin (itself)
   PlugTraces.LinkToPlugin (PlugName, CST_PLUG_ONACTION + CST_PLUG_ONBEFOREDELETE + CST_PLUG_ONTIMER) ;

   // disable the  LogFile label
   PlugTraces.DisableResource (CST_ACTION_LABEL_LOGFILE) ;

   // add a menu to the 'window' menu
   PlugTraces.CreateResource (100,CST_RES_MENU_WINDOW,0,'My win Plug');

   // add a menu to the 'action' menu
   PlugTraces.CreateResource (101,CST_RES_MENU_ACTION,0,'My action Plug');

   // add a label on right, autosize (0)
   PlugTraces.CreateResource (102,CST_RES_LABEL_RIGHT,0,'My label');

   // add a button on right (100 pixels)
   PlugTraces.CreateResource (103,CST_RES_BUT_RIGHT,100,'STOP');

   // add a label on left, 100 pixels
   PlugTraces.CreateResource (104,CST_RES_LABEL_LEFT,100,'My status');

   NodeEx := PlugTraces.debug.CreateNodeEx ;
   NodeEx.LeftMsg := 'Actions' ;
   NodeEx.id := 'ActionsNode' ;
   ActionNodes := NodeEx.send ;

   NodeEx := PlugTraces.debug.CreateNodeEx ;
   NodeEx.LeftMsg := 'Deleted Nodes' ;
   NodeEx.id := 'BeforeDeletes' ;
   BeforeDeleteNodes := NodeEx.send ;


   NodeEx := PlugTraces.debug.CreateNodeEx ;
   NodeEx.LeftMsg := 'Timer' ;
   NodeEx.id := 'Timer' ;
   Timer := NodeEx.send ;

   PlugTraces.debug.send ('Sample Delphi - Win32 plugin started');
end ;

//------------------------------------------------------------------------------

// stop the plugin

procedure Stop() stdcall ;
begin
   if PlugTraces <> nil then
      PlugTraces.Debug.send ('plugin stopped') ;
   TTrace.Flush();

   TTrace.stop ;

   PlugTraces := nil ;
   ActionNodes := nil ;
   BeforeDeleteNodes := nil ;
   Timer := nil ;
end ;

//------------------------------------------------------------------------------
// get the plugin name.
// lpPlugName : buffer where to store the plugin name. Ansi string : One byte per char . limited to 1024 bytes
procedure GetPlugName (lpPlugName: PChar) stdcall ;
begin
   strcopy (lpPlugName , PlugName) ;
end ;

//------------------------------------------------------------------------------
// called when the user click on a button, label or menu on a WinTrace.
// The plugin must call LinkToPlugin in order to receive this event
// WinId    : Wintrace Id
// ButtonId : Button Id
// return   : when true  : tracetool perform the default action
//            when false : tracetool don't perform any action
function OnAction (WinId : PChar ; ResourceId : integer; NodeId : PChar) : BOOL stdcall ;
begin
   result := true ; // perform default action
   if PlugTraces = nil then  // plugin stoped
      exit ;

   ActionNodes.send ('OnAction. WinId : ' + WinId + ', ResourceId : ' + inttostr(ResourceId) + ', current NodeId : ' + NodeId) ;

   // demo : disable close button
   if ResourceId = CST_ACTION_CLOSE_WIN then
      result := false ;

   if ResourceId = 103 then
      Stop() ;

end ;

//------------------------------------------------------------------------------
// called when a node is to be deleted on a WinTrace
// The plugin must call LinkToPlugin in order to receive this event
// WinId    : Wintrace Id
// NodeId   : node Id
// return   : when true  : tracetool delete the node
//            when false : tracetool don't delete the node
function OnBeforeDelete (WinId : PChar ; NodeId : PChar) : BOOL stdcall ;
begin
   result := true ;
   if PlugTraces = nil then  // plugin stoped
      exit ;
   BeforeDeleteNodes.ResendRight ('last = ' + NodeId) ;
   if (nodeId = 'BeforeDeletes') or (nodeId = 'ActionsNode') or (nodeId = 'Timer') then
      result := false ;
end ;

//------------------------------------------------------------------------------
// called every 500 ms. Can be used for example to refresh labels
// The plugin must call LinkToPlugin in order to receive this event
procedure OnTimer() stdcall ;
begin
   if PlugTraces = nil then  // plugin stoped
      exit ;
   // perform action...
   PlugTraces.SetTextResource (102, 'My status ' + TimeToStr(now)) ;
   Timer.ResendLeft ('Timer ' + TimeToStr(now)) ;
end ;

//------------------------------------------------------------------------------

end.
