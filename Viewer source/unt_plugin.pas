{
  All classes for plugin

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information

}

unit unt_plugin;

interface

uses Classes , windows, SysUtils, controls, Contnrs , forms, Registry, Config;
//, JNIWrapper, jni, javaruntime , JUtils  ;

Type
  TPlugin = class ;
  TWin32Plugin = class ;
  TDotNetPlugin = class ;
  TDotNetManager = class ;

  // Win 32 Plugins functions
  TGetPlugName            = procedure (PlugName: PAnsiString) stdcall ;
  TOnAction               = function  (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString ) : windows.BOOL stdcall ;
  TOnBeforeDelete         = function  (WinId : PAnsiString ; NodeId : PAnsiString) : windows.BOOL stdcall ;
  TOnTimer                = procedure () stdcall ;
  TStart                  = procedure () stdcall ;
  TStop                   = procedure () stdcall ;

  // Managed plugins functions
  // Same as Win 32 Plugins functions, but PlugId is added
  // Since managed exception are not copied to delphi, an StrException is added to get exception messages
  // For now the wrapper is single char string
  TWrapperOnAction        = function  (PlugId : integer ; WinId : PAnsiString ; ButtonId : integer; NodeId, StrException : PAnsiString) : windows.BOOL stdcall ;
  TWrapperOnBeforeDelete  = function  (PlugId : integer ; WinId : PAnsiString ; NodeId,StrException : PAnsiString) : windows.BOOL stdcall ;
  TWrapperOnTimer         = procedure (PlugId : integer ; StrException : PAnsiString) stdcall ;
  TWrapperStart           = procedure (PlugId : integer ; StrException : PAnsiString) stdcall ;
  TWrapperStop            = procedure (PlugId : integer ; StrException : PAnsiString) stdcall ;
  TWrapperUnload          = procedure (PlugId : integer ; StrException : PAnsiString) stdcall ;
  TWrapperCheckPlugInFile = procedure (PlugId : integer ; FileName,PlugName,StrException : PAnsiString) stdcall ;  // PlugName and StrException are OUT
  TCPPTEST = procedure (PlugId : integer ; StrException : PAnsiString) stdcall ;
  //TWrapperload            = procedure (PlugId : integer ; FileName,PlugClassName,PlugName,StrException : PAnsiString) stdcall ;  // PlugName is OUT
  //TWrapperAddPlugin       = procedure (PlugId : integer ; FileName,PlugClassName,StrException : PAnsiString) stdcall ;

  TPlugStatus = (psUnloaded, psLoaded , psStarted) ;

  //----------------------------------------------------------------------------

  TPlugin = class
     PlugName      : AnsiString ;
     Status        : TPlugStatus ;
     FileName      : AnsiString ;
     PlugClassName : AnsiString ;
     startup       : boolean ;
     plugKind      : string ;
     frmPlugin     : TFrame ; // TfrmPlugin
     xmlPlugin     : IXmlPlugin ;
     PlugID        : integer ;    // identify the plugin

     constructor Create () ;
     destructor  Destroy () ; override ;

     function  DoAction        (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ; virtual ; abstract ;
     function  DoBeforeDelete  (WinId : PAnsiString ; NodeId : PAnsiString) : windows.BOOL ; virtual ; abstract ;
     procedure DoTimer         () ; virtual ; abstract ;
     procedure DoStart         () ; virtual ; abstract ;
     procedure DoStop          () ; virtual ; abstract ;
     procedure DoLoad          () ; virtual ; abstract ;
     procedure DoUnload        () ; virtual ; abstract ;
  end ;

  //----------------------------------------------------------------------------

  TWin32Plugin = class (TPlugin)
     DllHandle : THandle ;
     // external functions
     GetPlugName    : TGetPlugName;
     OnAction       : TOnAction  ;
     OnBeforeDelete : TOnBeforeDelete  ;
     OnTimer        : TOnTimer ;
     Start          : TStart ;
     Stop           : TStop ;

     constructor create () ;
     procedure GetName () ;
     function DoCheckPlugInfile : string ;

     // TPlugin
     function  DoAction        (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ; override ;
     function  DoBeforeDelete  (WinId : PAnsiString ; NodeId : PAnsiString) : windows.BOOL ; override ;
     procedure DoTimer         () ; override ;
     procedure DoStart         () ; override ;
     procedure DoStop          () ; override ;
     procedure DoLoad          () ; override ;
     procedure DoUnload        () ; override ;
  end ;

  //----------------------------------------------------------------------------

  // a wrapper plugin is linked to a TPlugManager
  TDotNetPlugin = class (TPlugin)
     //Manager : TPlugManager ;

     constructor create () ;

     // TPlugin
     function  DoAction        (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ; override ;
     function  DoBeforeDelete  (WinId , NodeId : PAnsiString) : windows.BOOL ; override ;
     procedure DoTimer         () ; override ;
     procedure DoStart         () ; override ;
     procedure DoStop          () ; override ;
     procedure DoLoad          () ; override ;  // not called
     procedure DoUnload        () ; override ;
  end ;

  //----------------------------------------------------------------------------

  // TDotNetManager handle dot net plugin.
  // it's like win32Plugin, except that the plug name is passed in parameter to all functions
  TDotNetManager = class
  public
     WrapperFileName : string ;  // DotNetWrapper.dll
     DllHandle : THandle ;
  private
     OnAction          : TWrapperOnAction  ;
     OnBeforeDelete    : TWrapperOnBeforeDelete  ;
     OnTimer           : TWrapperOnTimer ;
     Start             : TWrapperStart ;
     Stop              : TWrapperStop ;
     Unload            : TWrapperUnload ;
     CheckPlugInFile   : TWrapperCheckPlugInFile ;
     cpptest : TCPPTEST ;
  public
     constructor create () ;
     procedure LoadDotNetManager() ;

     procedure DoCheckPlugInfile (Plug : TDotNetPlugin) ;
     procedure DoStart           (Plug : TDotNetPlugin) ;
     procedure DoStop            (Plug : TDotNetPlugin) ;
     function  DoAction          (Plug : TDotNetPlugin; WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ;
     function  DoBeforeDelete    (Plug : TDotNetPlugin; WinId : PAnsiString ; NodeId : PAnsiString) : windows.BOOL ;
     procedure DoTimer           (Plug : TDotNetPlugin) ;
     procedure DoUnload          (Plug : TDotNetPlugin) ;
  end ;

  //----------------------------------------------------------------------------

//  TJavaPlugin = class (TPlugin)
//  private
//     JavaPlugin : jobject ;
//     JavaPlugin_Class : JClass ;
//  public
//     constructor create () ;
//     class function checkClass (name : String ) : string ;
//     class function GetJVMs : TStringList;
//     class procedure Init ;
//     class procedure SetClassPath (JVMClassPath,PLugClassPath : string);
//
//     // TPlugin
//     function  DoAction        (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ; override ;
//     function  DoBeforeDelete  (WinId ,NodeId : PAnsiString) : windows.BOOL ; override ;
//     procedure DoTimer         () ; override ;
//     procedure DoStart         () ; override ;
//     procedure DoStop          () ; override ;
//     procedure getName () ;
//     procedure DoLoad          () ; override ;
//     procedure DoUnload        () ; override ;
//     //procedure Add() ;
//  end ;

  //----------------------------------------------------------------------------

  // plugin (delphi,C#,java) linked to a wintrace
  TLinkedPlugin = class
     Plugin : TPlugin ;
     NeedOnAction : boolean ;
     NeedOnbeforeDelete : boolean ;
     NeedTimer : boolean ;
  end ;

  //---------------------------------------------------------------------------------


  // plugin resources
  TPlugResource = class
     Obj : TObject ;  // TLabel, TButton , TItemMenu
     Id : integer ;
     Internal : boolean ;  // is it an internal tracetool resource or a plugin resource ?
     //width : integer ;   // usefull ? (can be determined from Obj)
     //type is not usefull (can be determined from Obj)
   end ;

  //---------------------------------------------------------------------------------

   procedure StopAllplugins() ;
   procedure UnloadAllplugins() ;
   function getPlugFromFileName (FileName : AnsiString) : TPlugin ;
   function getPlugFromName (Name: AnsiString): TPlugin;
   procedure InitPlugins ;

var
  PluginList    : TObjectList ;
  DotNetManager : TDotNetManager ;
//  // java vars
//  JRuntime : TJavaRuntime ;
//  WrapperClass : TJavaClass ;
//  StringClass  : TJavaClass ;
//  ObjectClass  : TJavaClass ;
  InstanceCount : integer ;
  PluginsInitialized : boolean ;

implementation

uses Unt_Tool, unt_TraceWin , unt_FrmPlugin , DebugOptions , StrUtils, unt_utility;

//------------------------------------------------------------------------------

//function JStringToString (penv: PJNIEnv ; JStr: JString): string;
//var
//  IsCopy: JBoolean;
//  Chars: PChar;
//begin
//  if JStr = nil then
//  begin
//    Result := '';
//    Exit;
//  end;
//
//  Chars := penv^.GetStringUTFChars(penv, JStr, IsCopy);
//  if Chars = nil then
//    Result := ''
//  else
//  begin
//    Result := string(Chars);
//    penv^.ReleaseStringUTFChars(penv, JStr, Chars);
//  end;
//end;
//
//------------------------------------------------------------------------------

procedure StopAllplugins() ;
var
   c : integer ;
   Plugin : TPlugin ;
   FoundPluginStarted : boolean ;
begin
   try
      FoundPluginStarted := true ;
      while FoundPluginStarted do begin
         FoundPluginStarted := false ;
         for c := 0 to PluginList.count-1 do begin
            Plugin := TPlugin (PluginList.Items[c]) ;
            if Plugin.status = psStarted then begin
               Plugin.DoStop ;
               FoundPluginStarted := true ;  // one plugin is FoundPluginStarted. restart loop main loop
               break ;  // break for loop
            end ;
         end ;
     end ;
   except
      on e : exception do
         TFrm_Trace.InternalTrace('StopAllplugins: plugin DoStop exception : ' + e.Message) ;
   end ;  
end ;

//------------------------------------------------------------------------------

procedure UnloadAllplugins() ;
var
   c : integer ;
   Plugin : TPlugin ;
   loaded : boolean ;
begin
   try
      loaded := true ;
      while loaded do begin
         loaded := false ;
         for c := 0 to PluginList.count-1 do begin
            Plugin := TPlugin (PluginList.Items[c]) ;
            if Plugin.status = psLoaded then begin
               Plugin.DoUnload ;
               loaded := true ;  // one plugin is loaded. restart loop main loop
               break ;  // break for loop
            end ;
         end ;
     end ;
   except
      on e : exception do
         TFrm_Trace.InternalTrace('StopAllplugins: plugin DoStop exception : ' + e.Message) ;
   end ;
end ;
//------------------------------------------------------------------------------

// search a plugin in the list using his fileame
function getPlugFromFileName(filename: AnsiString): TPlugin;
var
   c : integer ;
begin
   for c := 0 to PluginList.count-1 do begin
      result := TPlugin (PluginList.Items[c]) ;
      if stricomp (pAnsiChar(result.FileName), pAnsiChar(filename)) = 0 then
         exit ;
   end ;
   result := nil ;
end;

//------------------------------------------------------------------------------

// search a plugin in the list using his name
function getPlugFromName(name: AnsiString): TPlugin;
var
   c : integer ;
begin
   result := nil ;
   exit ;
   for c := 0 to PluginList.count-1 do begin
      result := TPlugin (PluginList.Items[c]) ;
      if stricomp (pAnsiChar(result.PlugName), pAnsiChar(name)) = 0 then
         exit ;
   end ;
   result := nil ;
end;

//------------------------------------------------------------------------------

procedure InitPlugins ;
var
   Win32plugin : TPlugin ;
   DotNetPlugin : TDotNetPlugin ;
//   JavaPlugin : TJavaPlugin;
   plugKind : string ;
   c : integer ;
   xmlPlugin : IXMLPlugin ;
begin
   DotNetManager := nil ;
   //JavaManager   := nil ;

   for c := 0 to XMLConfig.Plugins.Plugin.Count-1 do begin
      xmlPlugin := XMLConfig.Plugins.Plugin[c] ;
      plugKind := xmlPlugin.Kind ;
      // Name          : string ;
      // Status        : TPlugStatus ;
      // FileName      : string ;
      // PlugClassName : string ;
      // startup       : boolean ;
      // plugKind      : string ;
      // frmPlugin     : TFrame ; // TfrmPlugin
      // xmlPlugin     : IXmlPlugin ;

      if stricomp (pchar(plugKind) , 'Win32') = 0 then begin
         Win32plugin := TWin32Plugin.create ;
         Win32plugin.FileName  := AnsiString(xmlPlugin.FileName) ;
         Win32plugin.startup   := xmlPlugin.Enabled.Value ;
         //Win32plugin.PlugName  := xmlPlugin.PlugName ;
         Win32plugin.plugKind  := 'Win32' ;
         Win32plugin.xmlPlugin := xmlPlugin ;

         PluginList.add (Win32plugin) ;
         if Win32plugin.startup = false then
            continue ;
         Win32plugin.DoLoad ;   // load , get name
         Win32plugin.Dostart ;  //  and start
         //if (Win32plugin.PlugName <> '') and (Win32plugin.PlugName <> xmlPlugin.PlugName) then begin
         //   xmlPlugin.PlugName := Win32plugin.PlugName ;
         //   XMLConfig.OwnerDocument.SaveToFile(strConfigFile);
         //end ;
      end else if stricomp (pchar(plugKind) , 'DotNet') = 0 then begin

         if DotNetManager = nil then
            DotNetManager := TDotNetManager.create();

         if DotNetManager.DllHandle = 0 then
            continue ;

         DotNetPlugin := TDotNetPlugin.create ;
         DotNetPlugin.plugKind := 'DotNet' ;
         DotNetPlugin.xmlPlugin := xmlPlugin ;
         DotNetPlugin.FileName := AnsiString(xmlPlugin.FileName) ;
         DotNetPlugin.startup  := xmlPlugin.Enabled.Value ;
         try
            // add the plugin to the dot net wrapper list
            if DotNetManager <> nil then  // check is not necessary, but resolve unassigned variable warning
               DotNetManager.DoCheckPlugInfile(DotNetPlugin);
         except
            on e : exception do begin
               TFrm_Trace.InternalTraceFromThread ('InitPlugins : call DotNetManager.DoCheckPlugInfile() : ' + e.Message) ;
               //continue ;
            end ;
         end ;

         PluginList.add (DotNetPlugin) ;     // add the plugin to the general plugin list

         if DotNetPlugin.startup = false then
            continue ;

         //DotNetPlugin.DoLoad ;

         if DotNetPlugin.status <> psLoaded then
            continue ;
         DotNetPlugin.DoStart ;
         //if DotNetPlugin.status <> psStarted then
         //   continue ;

         //TFrm_Trace.InternalTraceFromThread ('Dot net plugin ' + DotNetPlugin.Name + ' Loaded and started');

//      end else if stricomp (pchar(plugKind) , 'Java') = 0 then begin
//         TJavaPlugin.Init ;
//
//
//         JavaPlugin := TJavaPlugin.create ;
//         JavaPlugin.plugKind      := 'Java' ;
//         JavaPlugin.startup       := xmlPlugin.Enabled.Value ;
//         JavaPlugin.xmlPlugin     := xmlPlugin ;
//         JavaPlugin.PlugName      := '' ; // xmlPlugin.PlugName ;
//         JavaPlugin.PlugClassName := AnsiString(xmlPlugin.ClassName) ; // 'pluginTest.JavaTestPlug' ;
//         // JavaPlugin.FileName is not used for java plugin
//
//         PluginList.add (JavaPlugin) ;   // add the plugin to the general plugin list
//
//         if JavaPlugin.startup = false then
//            continue ;
//
//         JavaPlugin.DoLoad ;               // add the plugin to the java wrapper, load the class and create instance
//         JavaPlugin.getName() ;
//
//         if JavaPlugin.status <> psLoaded then
//            continue ;
//         JavaPlugin.DoStart ;
//         if JavaPlugin.status <> psStarted then
//            continue ;
//         //TFrm_Trace.InternalTrace ('Java plugin ' + JavaPlugin.PlugName + ' Loaded and started');
      end ;
   end ;

end ;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TPlugin }

constructor TPlugin.create;
begin
   inc (InstanceCount) ;
   PlugID    := InstanceCount ;
   status    := psUnloaded ;
   FileName  := '' ;
   PlugName  := '' ;
   frmPlugin := TfrmPlugin.create(frmDebugOptions) ;  //    nil
   frmPlugin.Align := alClient ;
   frmPlugin.name := 'FrmPlugin' + intToStr(InstanceCount) ;
   TfrmPlugin(frmPlugin).plugin := self ;
end;

//------------------------------------------------------------------------------

destructor TPlugin.destroy;
begin
   // the associated configuration plugin frame (frmPlugin) is freed by the form
   //frmPlugin.free ;
   inherited destroy ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TWin32Plugin }

constructor TWin32Plugin.create;
begin
   inherited create() ;  // psUnloaded
   DllHandle := 0 ;
   // functions
   GetPlugName    := nil ;
   OnAction       := nil  ;
   OnBeforeDelete := nil  ;
   OnTimer        := nil ;
end;

//------------------------------------------------------------------------------

function TWin32Plugin.DoCheckPlugInfile : string ;
begin
   result := '' ;
   status := psUnloaded ;

   GetPlugName    := nil ;
   OnAction       := nil ;
   OnBeforeDelete := nil ;
   OnTimer        := nil ;
   Start          := nil ;
   Stop           := nil ;

   try
      //DllHandle := LoadLibrary (pWideChar(String(FileName)));
      DllHandle := LoadLibraryA (PAnsiChar(FileName));
   except
      on e : exception do begin
         DllHandle := 0 ;
         result := e.Message ;
         exit ;
      end ;
   end ;

   if DllHandle < 32 then
   begin
     result := 'LoadLibrary ("' + String(FileName) + '") : not a valid handle : ' + inttostr(DllHandle) ;
     DllHandle := 0;
     Exit;
   end;

   status := psLoaded ;
   PlugName := FileName ;

   GetPlugName    := GetProcAddress(DllHandle , 'GetPlugName') ;
   Start          := GetProcAddress(DllHandle , 'Start')        ;
   Stop           := GetProcAddress(DllHandle , 'Stop')        ;
   OnAction       := GetProcAddress(DllHandle , 'OnAction')       ;
   OnBeforeDelete := GetProcAddress(DllHandle , 'OnBeforeDelete') ;
   OnTimer        := GetProcAddress(DllHandle , 'OnTimer')        ;

end;

//------------------------------------------------------------------------------

procedure TWin32Plugin.DoLoad;      // Load 
var
   error : string ;
begin
   if status <> psUnloaded then
      exit ;

   error := DoCheckPlugInfile ;   // load only
   if error <> '' then begin
      TFrm_Trace.InternalTrace ('Unable to load the plugin : ' + error);
      exit ;
   end ;

   // 'GetPlugName' , 'Start' and 'Stop' are mandatory
   if not assigned (getPlugName) then
      TFrm_Trace.InternalTrace (string(FileName),'no "GetPlugName" function') ;
   if not assigned (Start) then
      TFrm_Trace.InternalTrace (string(FileName),'no "Start" function') ;
   if not assigned (Stop) then
      TFrm_Trace.InternalTrace (string(FileName),'no "Stop" function') ;

   GetName() ;
end;

//------------------------------------------------------------------------------

procedure TWin32Plugin.DoUnload;
begin
   if status <> psLoaded then
      exit ;
   try
      FreeLibrary(DllHandle);
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace ('FreeLibrary ("' + String (FileName) + '")' , e.Message) ;
      end ;
   end ;

   status := psUnloaded ;
   PlugName  := '_' ;
   DllHandle := 0 ;
   GetPlugName    := nil ;
   OnAction       := nil  ;
   OnBeforeDelete := nil  ;
   OnTimer        := nil ;
end;

//------------------------------------------------------------------------------

function TWin32Plugin.DoAction(WinId: PAnsiString; ButtonId: integer; NodeId : PAnsiString): windows.BOOL;
begin
   result := true ;
   try
      if (status = psStarted) and (assigned (OnAction)) then
         result := OnAction (pAnsiString(WinId) , ButtonId, pAnsiString(NodeId)) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoAction '  , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

function TWin32Plugin.DoBeforeDelete(WinId, NodeId: PAnsiString): windows.BOOL;
begin
   result := true ;
   try
      if (status = psStarted) and (assigned (OnBeforeDelete)) then
         result := OnBeforeDelete (pAnsiString(WinId) , pAnsiString(NodeId)) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoBeforeDelete' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TWin32Plugin.GetName;
var
   szName : array of AnsiChar ;
   pName : PAnsiString ;
   tempStringW : string ;
begin
   try
      if assigned (getPlugName) then begin
         SetLength (szName,1200) ;
         pName := PAnsiString(szName) ;
         strcopy (pansiChar(pName), '') ;
         GetPlugName (pName) ;   // get ANSI plugin name
         //PlugName := ansistring(pAnsiString(pName)) ;      // pName is Ansi
         tempStringW := BufToString (pAnsichar(pName),pAnsichar(pName)+1000) ;  // convert null terminated ansiString to string
         PlugName := AnsiString(trim(tempStringW)) ;
      end ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (FileName) + ' : DoGetName ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TWin32Plugin.DoStart;
begin
   if status <> psLoaded then
      exit ;
   try
      if assigned (Start) then
         Start () ;
      status := psStarted ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoStart ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TWin32Plugin.DoStop;
begin
   if status <> psStarted then
      exit ;
   try
      if assigned (Stop) then
         Stop () ;
     status := psLoaded ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoStop ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TWin32Plugin.DoTimer;
begin
   try
      if (status = psStarted) and (assigned (OnTimer)) then
         OnTimer () ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoTimer ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TDotNetPlugin }

constructor TDotNetPlugin.create;  // psUnloaded
begin
   inherited create () ;   // also create the frmPlugin frame linked to frmDebugOptions
   PlugClassName := '' ;
end;

//------------------------------------------------------------------------------

function TDotNetPlugin.DoAction(WinId: PAnsiString; ButtonId: integer;NodeId : PAnsiString): windows.BOOL;
begin
   result := true ;
   try
      if (status = psStarted) and (assigned (DotNetManager.OnAction )) then
         result := DotNetManager.DoAction(self , WinId , ButtonId , NodeId) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoAction ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

function TDotNetPlugin.DoBeforeDelete(WinId, NodeId: PAnsiString): windows.BOOL;
begin
   result := true ;
   try
      if (status = psStarted) and (assigned (DotNetManager.OnBeforeDelete)) then
         result := DotNetManager.DoBeforeDelete (self, WinId , NodeId) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoBeforeDelete ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TDotNetPlugin.DoTimer;
begin
   try
      if (status = psStarted) and (assigned (DotNetManager.OnTimer)) then
         DotNetManager.doTimer (self) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoTimer ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TDotNetPlugin.DoStart;
begin
   if status <> psLoaded then
      exit ;
   try
      if assigned (DotNetManager.Start) then
         DotNetManager.doStart (self) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoStart ' , e.Message) ;
         exit ;
      end ;
   end ;
   status := psStarted ;
end;

//------------------------------------------------------------------------------

procedure TDotNetPlugin.DoStop;
begin
   if status <> psStarted then
      exit ;
   try
      if assigned (DotNetManager.Stop) then
         DotNetManager.doStop (self) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoStop ' , e.Message) ;
         exit ;
      end ;
   end ;
   status := psLoaded ;
end;

//------------------------------------------------------------------------------

procedure TDotNetPlugin.DoLoad;
begin
   if status <> psUnloaded then
      exit ;
   try
      DotNetManager.DoCheckPlugInfile(self) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoLoad ' , e.Message) ;
         exit ;
      end ;
   end ;
   status := psLoaded ;
end;

//------------------------------------------------------------------------------

procedure TDotNetPlugin.DoUnload;
begin
   if status <> psLoaded then
      exit ;
   try
      if assigned (DotNetManager.Unload) then
         DotNetManager.doUnload (self) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoUnload ' , e.Message) ;
         exit ;
      end ;
   end ;
   status := psUnloaded ;
   PlugName  := '_' ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TDotNetManager }

// called by ButNewDotNetPlugin onclick event
constructor TDotNetManager.create();
begin
   LoadDotNetManager() ;
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.LoadDotNetManager;
begin
   OnAction        := nil ;
   OnBeforeDelete  := nil ;
   OnTimer         := nil ;
   Start           := nil ;
   Stop            := nil ;
   Unload          := nil ;
   CheckPlugInFile := nil ;
   //AddPlugin       := nil ;

   if FileExists(strRunPath + 'DotNetWrapper.dll') then
      WrapperFileName := strRunPath + 'DotNetWrapper.dll'
   else if FileExists('d:\GitHub\Tracetool\Plugins\DotNetWrapper\Debug\DotNetWrapper.dll') then
      WrapperFileName := 'd:\GitHub\Tracetool\Plugins\DotNetWrapper\Debug\DotNetWrapper.dll'
   else
      WrapperFileName := 'DotNetWrapper.dll' ;   // try to find it in the current path

   try
      DllHandle := LoadLibrary (pchar(WrapperFileName)) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace ('LoadLibrary ("' + WrapperFileName + '")' , e.Message) ;
      end ;
   end ;

   if DllHandle < 32 then
   begin
     TFrm_Trace.InternalTrace ('LoadLibrary ("' + WrapperFileName + '") : Invalid handle ' + inttostr(DllHandle)) ;
     TFrm_Trace.InternalTrace ('   Check EventLog System (In tracetool : Menu Windows/Open event log/sytem)') ;
     TFrm_Trace.InternalTrace ('   See SideBySide messages. Maybee some dependents libraries are missing') ;
     TFrm_Trace.InternalTrace ('   Install Microsoft Visual C++ 2008 Redistributable (vcredist_x86.exe) from microsoft side') ;
     TFrm_Trace.InternalTrace ('   Also ensure DotNetWrapper.dll is compiled in release mode or install the debug runtime') ;
     DllHandle := 0;
     Exit;
   end;

   OnAction        := GetProcAddress(DllHandle , 'OnAction') ;
   OnBeforeDelete  := GetProcAddress(DllHandle , 'OnBeforeDelete') ;
   OnTimer         := GetProcAddress(DllHandle , 'OnTimer') ;
   Start           := GetProcAddress(DllHandle , 'Start') ;
   Stop            := GetProcAddress(DllHandle , 'Stop') ;
   Unload          := GetProcAddress(DllHandle , 'Unload') ;
   CheckPlugInFile := GetProcAddress(DllHandle , 'CheckPlugInFile') ;
   cpptest := GetProcAddress(DllHandle , 'test') ;
end;

//------------------------------------------------------------------------------

// Check if a plugin exist in the library. May raise exception
// The plugin is loaded and the getName is called.
procedure TDotNetManager.DoCheckPlugInfile (Plug : TDotNetPlugin );
var
   szPlugName  : AnsiString;
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   if not assigned (CheckPlugInfile) then
      exit ;

   SetLength (szPlugName,1024) ;
   strcopy (pAnsiChar(szPlugName) , '') ;

   SetLength (szException,1024) ;
   strcopy (pAnsiChar(szException) , '') ;

   try
      //if assigned(cpptest) then
      //   cpptest (Plug.PlugID,pAnsiString(szException)) ;
      CheckPlugInfile (Plug.PlugID, pAnsiString(Plug.FileName), pAnsiString(szPlugName), pAnsiString(szException)) ;  // wrapper is single byte
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread(e.Message);
         raise Exception.create (e.Message) ;
      end;
   end;

   Plug.PlugName := BufToAnsiString(pAnsiChar(szPlugName),10) ; //   AnsiString(trim(String(szPlugName))) ;
   Plug.status   := psloaded ;   // already loaded

   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then begin
      TFrm_Trace.InternalTraceFromThread(string(TargetException));
      Plug.status := psUnloaded ;  // if error, the plugin is not loaded
   end;

   // todo : save error message in result
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.DoStart (Plug : TDotNetPlugin);
var
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   if not assigned (Start) then
      exit ;

   SetLength (szException,1024) ;
   strcopy (pAnsiChar(szException) , '') ;

   try
      Start (Plug.PlugID , pAnsiString(szException)) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp Start() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp Start() return error message : ' + string(TargetException));
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.DoStop (Plug : TDotNetPlugin) ;
var
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   if not assigned (Stop) then
     exit ;

   SetLength (szException,1024) ;
   strcopy (pAnsiChar(szException) , '') ;

   try
      Stop (Plug.PlugID ,pAnsiString(szException)) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp Stop() exception : ' + e.Message);
      end;
   end;


   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp Stop() return error message : ' + string(TargetException));
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.DoTimer (Plug : TDotNetPlugin) ;
var
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   if not assigned (OnTimer) then
      exit ;

   SetLength (szException,1024) ;
   strcopy (pAnsiChar(szException) , '') ;

   try
      OnTimer (Plug.PlugID ,pAnsiString(szException)) ;    // wrapper is unicode
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp OnTimer() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp OnTimer() return error message : ' + string(TargetException));
end;

//------------------------------------------------------------------------------

function TDotNetManager.DoAction (Plug : TDotNetPlugin; WinId: PAnsiString; ButtonId: integer;NodeId : PAnsiString): windows.BOOL;
var
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   result := true ;
   if not assigned (OnAction) then
      exit ;

   SetLength (szException,1024) ;
   strcopy (pAnsiChar(szException) , '') ;

   try
      result := OnAction (plug.PlugID, WinId , ButtonId, NodeId, pAnsiString(szException) ) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp OnAction() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp OnAction() return error message : ' + string(TargetException));
end;

//------------------------------------------------------------------------------

function TDotNetManager.DoBeforeDelete(Plug : TDotNetPlugin; WinId, NodeId: PAnsiString): windows.BOOL;
var
   szException : AnsiString;
   TargetException : AnsiString;
begin
   result := true ;
   if not assigned (OnBeforeDelete) then
      exit ;

   SetLength (szException,1024) ;
   strcopy (pansiChar(szException), '') ;

   try
      result := OnBeforeDelete (Plug.PlugID,WinId , NodeId, pAnsiString(szException)) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp OnBeforeDelete() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp OnBeforeDelete() return error message : ' + string(TargetException));
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.DoUnload (Plug : TDotNetPlugin) ;
var
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   if not assigned (Unload) then
      exit ;

   SetLength (szException,1024) ;
   strcopy (pAnsiChar(szException) , '') ;

   try
      Unload (Plug.PlugID ,pAnsiString(szException)) ;   // wrapper is unicode
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp Unload() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1024) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp Unload() return error message : ' + string(TargetException));

   Plug.PlugName  := '_' ;
end;

//------------------------------------------------------------------------------

{ TJavaPlugin }

//constructor TJavaPlugin.create;
//begin
//   inherited ;
//end;
//
////------------------------------------------------------------------------------
//
//class function TJavaPlugin.GetJVMs () : TStringList ;
//var
//   reg : TRegistry;
//   subKeys,subKeys2 : TStringList ;
//   JavaHome : string ;
//
//   procedure CheckRegistry (MasterKey : string) ;
//   var
//      c,d : integer ;
//   begin
//      if reg.OpenKey(MasterKey, false) = false then
//         exit ;
//
//      subKeys := TStringList.Create ;
//      reg.GetKeyNames(subKeys);
//      reg.CloseKey ;
//      for c := 0 to subKeys.Count-1 do begin
//         reg.OpenKey(MasterKey + '\'  + subKeys.Strings[c],false) ;
//         subKeys2 := TStringList.Create ;
//         reg.GetKeyNames(subKeys2);
//         reg.CloseKey ;
//         for d := 0 to subKeys2.Count-1 do begin    // versions
//            reg.OpenKey(MasterKey + '\' + subKeys.Strings[c] + '\' + subKeys2.Strings[d] ,false) ;
//
//            // get the javaHome
//            if reg.ValueExists('JavaHome') then
//               JavaHome := reg.ReadString('JavaHome')
//            else if reg.valueExists('Home') then
//               JavaHome := reg.ReadString('Home')
//            else if reg.valueExists('java_home') then
//               JavaHome := reg.ReadString('java_home')
//            else
//               continue ;
//
//            // get the run time lib
//            if FileExists(JavaHome + '\JRE\bin\client\jvm.dll' ) then result.Add(JavaHome + ',JRE\bin\client\jvm.dll' ) ;
//            if FileExists(JavaHome + '\JRE\bin\classic\jvm.dll') then result.Add(JavaHome + ',JRE\bin\classic\jvm.dll') ;
//            if FileExists(JavaHome + '\JRE\bin\hotspot\jvm.dll') then result.Add(JavaHome + ',JRE\bin\hotspot\jvm.dll') ;
//            if FileExists(JavaHome + '\JRE\bin\server\jvm.dll' ) then result.Add(JavaHome + ',JRE\bin\server\jvm.dll' ) ;
//            if FileExists(JavaHome + '\bin\client\jvm.dll'     ) then result.Add(JavaHome + ',bin\client\jvm.dll'     ) ;
//            if FileExists(JavaHome + '\bin\classic\jvm.dll'    ) then result.Add(JavaHome + ',bin\classic\jvm.dll'    ) ;
//            if FileExists(JavaHome + '\bin\hotspot\jvm.dll'    ) then result.Add(JavaHome + ',bin\hotspot\jvm.dll'    ) ;
//            if FileExists(JavaHome + '\bin\server\jvm.dll'     ) then result.Add(JavaHome + ',bin\server\jvm.dll'     ) ;
//
//         end ;   // next version
//         subKeys2.free ;
//      end ;      // next path in masterkey
//      subKeys.Free ;
//   end ;
//begin
//   result := tStringList.Create ;
//   result.Duplicates := dupIgnore ; // no duplicate strings
//   reg := TRegistry.Create;
//   try
//      reg.RootKey := HKEY_LOCAL_MACHINE;
//      CheckRegistry ('\SOFTWARE\JavaSoft') ;
//      CheckRegistry ('\SOFTWARE\IBM') ;
//   finally
//      Reg.Free;
//   end;
//end ;
//
////------------------------------------------------------------------------------
//
//class procedure TJavaPlugin.SetClassPath(JVMClassPath,PLugClassPath: string);
//var
//   penv: PJNIEnv;
//
//   Path : string ;
//   Params : TJavaParams;
//   Method : TJavaMethod;
//
//   MethodID : JMethodID;
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//
//   path := JVMClassPath ;
//   if PLugClassPath <> '' then
//      if RightStr(path,1) = ';' then
//         path := path + PLugClassPath
//      else
//         path := path + ';' + PLugClassPath ;
//
//
//   Penv := TJavaVM.getPenv;
//
//   // public static void setClassPath (String cPath)
//   // ---------------------------
//   Params := TJavaParams.Create;
//   Params.addString(Path);       // class path
//   Method := TJavaMethod.Create(
//      WrapperClass,        // class : TJavaClass
//      'setClassPath',      // function name : String
//      static,              // methodType : TMethodAttribute
//      Void,                // returntype : TJavaType
//      Params,              // params : TJavaParams
//      nil);                // no retclass
//
//   Method.Call(Params, Nil{Class method});
//
//   // Check for exception
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace  ('SetClassPath Exception',ErrMsg) ;
//      exit ;
//   end ;
//   Params.free ;
//   Method.Free ;
//end;
//
////------------------------------------------------------------------------------
//
//class function TJavaPlugin.checkClass(name: String): string ;
//var
//   penv: PJNIEnv;
//   retCall : jvalue ;
//   //JgetName : jClass ;
//
//   CheckParams : TJavaParams;
//   CheckMethod : TJavaMethod;
//
//   MethodID : JMethodID;
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   //
//   Penv := TJavaVM.getPenv;
//
//   // public static String findClass(String name , String CPath)
//   // ---------------------------
//   CheckParams := TJavaParams.Create;
//   CheckParams.addString(name);        // class name
//   CheckMethod := TJavaMethod.Create(
//      WrapperClass,        // class : TJavaClass
//      'findClass',         // function name : String
//      static,              // methodType : TMethodAttribute
//      Aobject,             // returntype : TJavaType
//      CheckParams,         // params : TJavaParams
//      StringClass);        // return String
//
//   retCall := CheckMethod.Call(CheckParams, Nil{Class method});
//
//   // Check for exception
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace  ('checkClass Exception',ErrMsg) ;
//      result := '' ;
//      exit ;
//   end ;
//   CheckParams.free ;
//   CheckMethod.Free ;
//
//   //JgetName := retCall.l ;
//
//   result := JStringToString(penv, retCall.l);
//end;
//
////------------------------------------------------------------------------------
//
//procedure TJavaPlugin.DoLoad;
//var
//   penv: PJNIEnv;
//   retCall : jvalue ;
//
//   LoadParams : TJavaParams;
//   LoadMethod : TJavaMethod;
//
//   MethodID : JMethodID;
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//
//   //Result_getClass : JClass ;
//   //Result_getClass_class : JClass ;
//   //Result_getClass_name : jClass ;
//begin
//
//   if Status <> psUnLoaded then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoLoad(' + PlugName + ') : plugin already loaded') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call doLoad(' + PlugName + ')') ;
//
//   Penv := TJavaVM.getPenv;
//
//   // public static Object load (String plugClassName, int plugId) throws Exception
//   // ---------------------------
//   LoadParams := TJavaParams.Create;
//   LoadParams.addString(PlugClassName);
//   LoadParams.addInt(PlugID);        // Plug name
//   LoadMethod := TJavaMethod.Create(
//      WrapperClass,       // class : TJavaClass
//      'load',             // function name : String
//      static,             // methodType : TMethodAttribute
//      Aobject,            // returntype : TJavaType
//      LoadParams,         // params : TJavaParams
//      ObjectClass);       // retclass : TJavaClass
//
//   retCall := LoadMethod.Call(LoadParams, Nil{Class method});
//
//   // Check for exception
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('load Exception',ErrMsg) ;
//      exit ;
//   end ;
//   LoadParams.free ;
//   LoadMethod.Free ;
//
//   JavaPlugin := retCall.l ;
//
//   // get result class name
//   //--------------------------
//
//   JavaPlugin_Class := Penv^.GetObjectClass(penv,JavaPlugin) ;
//
//   // MethodID := Penv^.GetMethodID(penv, JavaPlugin_Class , 'getClass', '()Ljava/lang/Class;');
//   //Result_getClass := Penv^.CallObjectMethod(penv, JavaPlugin, MethodID);  // call getClass()
//   //Result_getClass_class := Penv^.GetObjectClass(penv,Result_getClass) ;   // get object class
//   //
//   //MethodID := Penv^.GetMethodID(penv, Result_getClass_class , 'getName', '()Ljava/lang/String;');
//   //Result_getClass_name := Penv^.CallObjectMethod(penv, Result_getClass, MethodID);  // getName
//
//   //TFrm_Trace.InternalTrace ('result class name ' + JStringToString(penv, Result_getClass_name)) ;
//   //TFrm_Trace.InternalTrace ('doLoad(' + PlugName + ') done') ;
//   status := psLoaded ;
//end;
//
////------------------------------------------------------------------------------
//
//procedure TJavaPlugin.DoUnload;
//var
//   penv: PJNIEnv;
//   Exc : jthrowable ;
//
//   // public static void unLoad (String plugName) throws Exception
//   unLoadParams : TJavaParams;
//   unLoadMethod : TJavaMethod;
//
//   MethodID : JMethodID;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   if Status = psStarted then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoUnload(' + PlugName + ') : plugin not stoped') ;
//      exit ;
//   end ;
//   if Status <> psLoaded then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoUnload(' + PlugName + ') : plugin not loaded') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call unLoad (' + PlugName + ')') ;
//
//   // public static void unLoad (int plugId) throws Exception
//   // ------------------------------------------------------------
//   unLoadParams := TJavaParams.Create;
//   unLoadParams.addInt(PlugID);
//
//   unLoadMethod := TJavaMethod.Create(
//      WrapperClass,       // class : TJavaClass
//      'unLoad',           // function name : String
//      static,             // methodType : TMethodAttribute
//      Void,               // returntype : TJavaType
//      unLoadParams,       // params : TJavaParams
//      Nil);               // retclass : TJavaClass
//
//   unLoadMethod.Call(unLoadParams, Nil);     // static method
//
//
//   // Check for exception
//   Penv := TJavaVM.getPenv;
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('unLoad Exception',ErrMsg) ;
//   end ;
//   unLoadParams.Free ;
//   unLoadMethod.Free ;
//   PlugName := '_';
//   status := psUnLoaded ;
//end;
//
////------------------------------------------------------------------------------
//
//procedure TJavaPlugin.DoStart;
//var
//   penv: PJNIEnv;
//   MethodID : JMethodID;
//
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   if Status <> psLoaded then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoStart(' + PlugName + ') : plugin not loaded') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call start(' + PlugName + ')') ;
//   Penv := TJavaVM.getPenv;
//
//   //public void start()
//   MethodID := Penv^.GetMethodID(penv, JavaPlugin_Class , 'start', '()V');    // GetMethodID(env: PJNIEnv; clazz : Jclass; const name, sig : PChar) : jmethodID
//
//   if MethodID = nil then begin
//      TFrm_Trace.InternalTrace ('Javawrapper.start method not found') ;
//      exit ;
//   end else
//      Penv^.CallObjectMethod(penv, JavaPlugin, MethodID);  // call start()
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('start Exception',ErrMsg) ;
//      exit ;
//   end ;
//
//   status := psStarted ;
//end;
//
////------------------------------------------------------------------------------
//
//procedure TJavaPlugin.DoStop;
//var
//   penv: PJNIEnv;
//   MethodID : JMethodID;
//
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   if Status <> psStarted then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoStop(' + PlugName + ') : plugin not started') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call DoStop(' + PlugName + ') ') ;
//
//   Penv := TJavaVM.getPenv;
//
//   //public void start()
//   MethodID := Penv^.GetMethodID(penv, JavaPlugin_Class , 'stop', '()V');    // GetMethodID(env: PJNIEnv; clazz : Jclass; const name, sig : PChar) : jmethodID
//
//   if MethodID = nil then
//      TFrm_Trace.InternalTrace ('stop method not found')
//   else
//      Penv^.CallObjectMethod(penv, JavaPlugin, MethodID);  // call start()
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('stop Exception',ErrMsg) ;
//      exit ;
//   end ;
//   status := psLoaded ;
//end;
//
////------------------------------------------------------------------------------
//
//procedure TJavaPlugin.getName;
//var
//   penv: PJNIEnv;
//   JgetName : jClass ;
//
//   MethodID : JMethodID;
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//   name : string ;
//begin
//   if Status <> psLoaded then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.getName (' + PlugName + ') : plugin not loaded') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call getPlugName(' + PlugName + ') ') ;
//
//   Penv := TJavaVM.getPenv;
//
//   MethodID := Penv^.GetMethodID(penv, JavaPlugin_Class , 'getPlugName', '()Ljava/lang/String;');
//   JgetName := Penv^.CallObjectMethod(penv, JavaPlugin, MethodID);  // getName
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('getPlugName Exception',ErrMsg) ;
//      exit ;
//   end ;
//   name := JStringToString(penv, JgetName);
//   //self.PlugName := name ;
//   //TFrm_Trace.InternalTrace ('getPlugName done. Name : ' + self.PlugName) ;
//end;
//
////------------------------------------------------------------------------------
//function TJavaPlugin.DoAction(WinId: PAnsiString; ButtonId: integer; NodeId: PAnsiString): windows.BOOL;
//var
//   penv: PJNIEnv;
//   MethodID : JMethodID;
//
//   onActionParams : TJavaParams;
//
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   result := true ;
//   if Status <> psStarted then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoAction(' + PlugName + ') : plugin not started') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call DoAction(' + PlugName + ') ') ;
//
//   Penv := TJavaVM.getPenv;
//
//   // public static boolean onAction (String winId , int resourceId , String nodeId) throws Exception
//   // ---------------------------
//   onActionParams := TJavaParams.Create;
//   onActionParams.addString(String(WinId));            // winId
//   onActionParams.addInt   (ButtonId);         // resourceId
//   onActionParams.addString(String(NodeId));           // nodeId
//
//   MethodID := penv^.getMethodID(penv, JavaPlugin_Class, 'onAction', Pchar('(' + onActionParams.signature + ')Z'));
//
//   if MethodID = nil then
//      TFrm_Trace.InternalTrace ('boolean onAction (String,int,String) method not found')
//   else begin
//      result := penv^.CallBooleanMethodV(penv, JavaPlugin, MethodID, onActionParams.argpointer);
//      //TFrm_Trace.InternalTrace ('DoAction result' , result) ;
//   end ;
//
//   // Check for exception
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('onAction Exception',ErrMsg) ;
//      exit ;
//   end ;
//   onActionParams.free ;
//end;
//
////------------------------------------------------------------------------------
//function TJavaPlugin.DoBeforeDelete(WinId, NodeId: PAnsiString): windows.BOOL;
//var
//   penv: PJNIEnv;
//   MethodID : JMethodID;
//
//   onBeforeDeleteParams : TJavaParams;
//
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   result := true ;
//   if Status <> psStarted then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoBeforeDelete(' + PlugName + ') : plugin not started') ;
//      exit ;
//   end ;
//   //TFrm_Trace.InternalTrace ('Call DoBeforeDelete(' + PlugName + ') ') ;
//
//   Penv := TJavaVM.getPenv;
//
//   // public static boolean onBeforeDelete (String winId , String nodeId) throws Exception
//   // ---------------------------
//   onBeforeDeleteParams := TJavaParams.Create;
//   onBeforeDeleteParams.addString(WinId);          // winId
//   onBeforeDeleteParams.addString(NodeId);         // nodeId
//
//   MethodID := penv^.getMethodID(penv, JavaPlugin_Class, 'onBeforeDelete', Pchar('(' + onBeforeDeleteParams.signature + ')Z'));
//
//   if MethodID = nil then
//      TFrm_Trace.InternalTrace ('boolean onBeforeDelete (int,String) method not found')
//   else begin
//      result := penv^.CallBooleanMethodV(penv, JavaPlugin, MethodID, onBeforeDeleteParams.argpointer);
//      //TFrm_Trace.InternalTrace ('DoAction result' , result) ;
//   end ;
//
//   // Check for exception
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('onBeforeDelete Exception',ErrMsg) ;
//      exit ;
//   end ;
//   onBeforeDeleteParams.free ;
//end;
//
////------------------------------------------------------------------------------
//
//procedure TJavaPlugin.DoTimer;
//var
//   penv: PJNIEnv;
//   MethodID : JMethodID;
//
//   Exc : jthrowable ;
//   ExceptionClass : jclass ;
//   ExceptionClassGetClass : jclass ;
//   ExceptionClass3 : jclass ;
//   ExceptionName : jClass ;
//   ExceptionMessage : JClass ;
//   ErrMsg: string;
//begin
//   if Status <> psStarted then begin
//      TFrm_Trace.InternalTrace ('TJavaPlugin.DoTimer(' + PlugName + ') : plugin not started') ;
//      exit ;
//   end ;
//
//   //TFrm_Trace.InternalTrace ('Call onTimer(' + PlugName + ') ') ;
//
//   Penv := TJavaVM.getPenv;
//   //public void onTimer()
//   MethodID := Penv^.GetMethodID(penv, JavaPlugin_Class , 'onTimer', '()V');    // GetMethodID(env: PJNIEnv; clazz : Jclass; const name, sig : PChar) : jmethodID
//
//   if MethodID = nil then
//      TFrm_Trace.InternalTrace ('stop method not found')
//   else
//      Penv^.CallObjectMethod(penv, JavaPlugin, MethodID);  // call start()
//
//   Exc := Penv^.ExceptionOccurred (Penv) ;
//   if Exc <> nil then begin
//      // Clear the exception so we can call other methods
//      Penv^.ExceptionClear (Penv) ;
//
//      // Find out about the exception - its class and message
//      ExceptionClass := Penv^.GetObjectClass(penv,Exc) ;
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass , 'getClass', '()Ljava/lang/Class;');
//      ExceptionClassGetClass := Penv^.CallObjectMethod(penv, Exc, MethodID);  // call getClass()
//      ExceptionClass3 := Penv^.GetObjectClass(penv,ExceptionClassGetClass) ;  // get object class
//
//      MethodID := Penv^.GetMethodID(penv, ExceptionClass3 , 'getName', '()Ljava/lang/String;');
//      ExceptionName := Penv^.CallObjectMethod(penv, ExceptionClassGetClass, MethodID);  // getName
//
//      ErrMsg := JStringToString(penv, ExceptionName);
//
//      MethodID := Penv^.GetMethodID(Penv, ExceptionClass, 'getMessage', '()Ljava/lang/String;');
//      if MethodID = nil then
//         raise Exception.Create('Can''t find method: getMessage');
//
//      ExceptionMessage := Penv^.CallObjectMethod(Penv, Exc, MethodID) ;
//      ErrMsg := ErrMsg + ' :' + #13 + JStringToString(Penv,ExceptionMessage);
//      TFrm_Trace.InternalTrace ('onTimer Exception',ErrMsg) ;
//      exit ;
//   end ;
//end;
//
////------------------------------------------------------------------------------
//
//class procedure TJavaPlugin.Init;
//var
//   newclassPath : string ;
//   Home, relative , JavaWrapper , traceJar : string ;
//   p : integer ;
//begin
//   if JRuntime <> nil then
//      exit ;
//
//   p := pos (',' , XMLConfig.Plugins.JVMEngine.Value) ;
//   if p <= 0 then
//      exit ;
//
//   Home := copy (XMLConfig.Plugins.JVMEngine.Value, 1 , p-1) ;
//   relative := copy (XMLConfig.Plugins.JVMEngine.Value, p+1 , 1000) ;
//   JRuntime := TJavaRuntime.Create(Home , Home + '\' + relative, false);
//
//   if FileExists(strRunPath + 'JavaWrapper.jar') then
//      JavaWrapper := strRunPath + 'JavaWrapper.jar;'
//   else if FileExists(strRunPath + 'plugin\JavaWrapper.jar') then
//      JavaWrapper := strRunPath + 'plugin\JavaWrapper.jar;'
//   else
//      JavaWrapper := 'JavaWrapper.jar;' ;   // try to find it in the current path
//
//
//   if FileExists(strRunPath + 'tracetool.jar') then
//      traceJar := strRunPath + 'tracetool.jar'
//   else if FileExists(strRunPath + 'plugin\tracetool.jar') then
//      traceJar := strRunPath + 'plugin\tracetool.jar'
//   else
//      traceJar := 'tracetool.jar' ;   // try to find it in the current path
//
//   JRuntime.Classpath := JavaWrapper + traceJar  ;
//
//   newclassPath := TClassPath.getDefault.FullPath ;
//   //TFrm_Trace.InternalTrace ('new ClassPath' , newclassPath) ;
//
//
//   WrapperClass := TJavaClass.Create('tracetool.JavaWrapper') ;   // load class and initialize class
//   StringClass  := TJavaClass.Create('java.lang.String') ;        // create the String class
//   ObjectClass  := TJavaClass.Create('java.lang.Object') ;        // return an object
//
//   TJavaPlugin.SetClassPath(TClassPath.getDefault.FullPath , XMLConfig.Plugins.JavaPLuginClassPath.value);
//
//end;


initialization
   PluginsInitialized := false ;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.