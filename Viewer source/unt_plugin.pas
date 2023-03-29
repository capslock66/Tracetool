{
  All classes for plugin

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information

}

unit unt_plugin;

interface

uses Classes , windows, AnsiStrings, SysUtils, controls, Contnrs , forms, Registry ;

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
  TStart                  = procedure (Parameter : PAnsiString) stdcall ;
  TStop                   = procedure () stdcall ;

  // Managed plugins functions
  // Same as Win 32 Plugins functions, but PlugId is added
  // Since managed exception are not copied to delphi, an StrException is added to get exception messages
  // For now the wrapper is single char string
  TWrapperOnAction        = function  (PlugId : integer ; WinId : PAnsiString ; ButtonId : integer; NodeId, StrException : PAnsiString) : windows.BOOL stdcall ;
  TWrapperOnBeforeDelete  = function  (PlugId : integer ; WinId : PAnsiString ; NodeId,StrException : PAnsiString) : windows.BOOL stdcall ;
  TWrapperOnTimer         = procedure (PlugId : integer ; StrException : PAnsiString) stdcall ;
  TWrapperStart           = procedure (PlugId : integer ; Parameter : PAnsiString; StrException : PAnsiString) stdcall ;
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
     param         : AnsiString ;
     startup       : boolean ;
     plugKind      : string ;
     frmPlugin     : TFrame ; // TfrmPlugin
     PlugID        : integer ;    // identify the plugin

     constructor Create () ;
     destructor  Destroy () ; override ;

     function  DoAction        (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ; virtual ; abstract ;
     function  DoBeforeDelete  (WinId : PAnsiString ; NodeId : PAnsiString) : windows.BOOL ; virtual ; abstract ;
     procedure DoTimer         () ; virtual ; abstract ;
     procedure DoStart         (Parameter : PAnsiString) ; virtual ; abstract ;
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
     procedure DoStart         (Parameter : PAnsiString) ; override ;
     procedure DoStop          () ; override ;
     procedure DoLoad          () ; override ;
     procedure DoUnload        () ; override ;
  end ;

  //----------------------------------------------------------------------------

  // a wrapper plugin is linked to a TPlugManager
  TDotNetPlugin = class (TPlugin)
     constructor create () ;

     // TPlugin
     function  DoAction        (WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ; override ;
     function  DoBeforeDelete  (WinId , NodeId : PAnsiString) : windows.BOOL ; override ;
     procedure DoTimer         () ; override ;
     procedure DoStart         (Parameter : PAnsiString) ; override ;
     procedure DoStop          () ; override ;
     procedure DoLoad          () ; override ;  // not called
     procedure DoUnload        () ; override ;
  end ;

  //----------------------------------------------------------------------------

  // TDotNetManager handle dot net plugin.
  // it's like win32Plugin, except that the plug name is passed in parameter to all functions
  TDotNetManager = class
  public
     WrapperFileName : string ;  // DotNetWrapper.dll or DotNetWrapper64.dll
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
     procedure LoadDotNetWrapper() ;

     procedure DoCheckPlugInfile (Plug : TDotNetPlugin) ;
     procedure DoStart           (Plug : TDotNetPlugin;Parameter : PAnsiString) ;
     procedure DoStop            (Plug : TDotNetPlugin) ;
     function  DoAction          (Plug : TDotNetPlugin; WinId : PAnsiString ; ButtonId : integer; NodeId : PAnsiString) : windows.BOOL ;
     function  DoBeforeDelete    (Plug : TDotNetPlugin; WinId : PAnsiString ; NodeId : PAnsiString) : windows.BOOL ;
     procedure DoTimer           (Plug : TDotNetPlugin) ;
     procedure DoUnload          (Plug : TDotNetPlugin) ;
  end ;

  //----------------------------------------------------------------------------

  // plugin (delphi,C#) linked to a wintrace
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

var
  PluginInstanceCount : integer ;


implementation

uses Unt_Tool, unt_TraceWin , unt_FrmPlugin , DebugOptions , StrUtils, unt_utility
,unt_TraceConfig
;

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
         for c := 0 to TraceConfig.PluginList.count-1 do begin
            Plugin := TPlugin (TraceConfig.PluginList.Items[c]) ;
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
         for c := 0 to TraceConfig.PluginList.count-1 do begin
            Plugin := TPlugin (TraceConfig.PluginList.Items[c]) ;
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
   for c := 0 to TraceConfig.PluginList.count-1 do begin
      result := TPlugin (TraceConfig.PluginList.Items[c]) ;
      if System.AnsiStrings.stricomp (pAnsiChar(result.FileName), pAnsiChar(filename)) = 0 then
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
   for c := 0 to TraceConfig.PluginList.count-1 do begin
      result := TPlugin (TraceConfig.PluginList.Items[c]) ;
      if System.AnsiStrings.stricomp (pAnsiChar(result.PlugName), pAnsiChar(name)) = 0 then
         exit ;
   end ;
end;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TPlugin }

constructor TPlugin.create;
begin
   inc (PluginInstanceCount) ;
   PlugID    := PluginInstanceCount ;
   status    := psUnloaded ;
   FileName  := '' ;
   PlugName  := '' ;
   frmPlugin := TfrmPlugin.create(frmDebugOptions) ;  //    nil
   frmPlugin.Align := alClient ;
   frmPlugin.name := 'FrmPlugin' + intToStr(PluginInstanceCount) ;
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
         System.AnsiStrings.strcopy (pansiChar(pName), '') ;
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

procedure TWin32Plugin.DoStart(Parameter : PAnsiString);
begin
   if status <> psLoaded then
      exit ;
   try
      if assigned (Start) then
         Start (pAnsiString(Parameter)) ;
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
      if (status = psStarted) and (assigned (TraceConfig.DotNetManager.OnAction )) then
         result := TraceConfig.DotNetManager.DoAction(self , WinId , ButtonId , NodeId) ;
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
      if (status = psStarted) and (assigned (TraceConfig.DotNetManager.OnBeforeDelete)) then
         result := TraceConfig.DotNetManager.DoBeforeDelete (self, WinId , NodeId) ;
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
      if (status = psStarted) and (assigned (TraceConfig.DotNetManager.OnTimer)) then
         TraceConfig.DotNetManager.doTimer (self) ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (String (PlugName) + ' : DoTimer ' , e.Message) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TDotNetPlugin.DoStart(Parameter : PAnsiString);
begin
   if status <> psLoaded then
      exit ;
   try
      if assigned (TraceConfig.DotNetManager.Start) then
         TraceConfig.DotNetManager.doStart (self,Parameter) ;
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
      if assigned (TraceConfig.DotNetManager.Stop) then
         TraceConfig.DotNetManager.doStop (self) ;
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
      if TraceConfig.DotNetManager = nil then begin
         TraceConfig.DotNetManager := TDotNetManager.create();
         if TraceConfig.DotNetManager.DllHandle = 0 then
            exit ;
      end ;
      TraceConfig.DotNetManager.DoCheckPlugInfile(self) ;
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
      if assigned (TraceConfig.DotNetManager.Unload) then
         TraceConfig.DotNetManager.doUnload (self) ;
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
   LoadDotNetWrapper() ;
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.LoadDotNetWrapper;
var
   DllName : string;
begin
   OnAction        := nil ;
   OnBeforeDelete  := nil ;
   OnTimer         := nil ;
   Start           := nil ;
   Stop            := nil ;
   Unload          := nil ;
   CheckPlugInFile := nil ;
   //AddPlugin       := nil ;

   {$IFDEF WIN64}
     DllName := 'DotNetWrapper64.dll' ;
   {$ELSE}
     DllName := 'DotNetWrapper.dll' ;
   {$ENDIF}

   if FileExists(Frm_Tool.strRunPath + DllName) then
      WrapperFileName := Frm_Tool.strRunPath + DllName
   //else if FileExists('c:\GitHub\Tracetool\Plugins\DotNetWrapper\Debug\DotNetWrapper.dll') then
   //   WrapperFileName := 'c:\GitHub\Tracetool\Plugins\DotNetWrapper\Debug\DotNetWrapper.dll'
   else
      WrapperFileName := DllName ;   // try to find it in the current path

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
     TFrm_Trace.InternalTrace ('   Also ensure DotNetWrapper[64].dll is compiled in release mode or install the debug runtime') ;
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

   SetLength (szPlugName,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szPlugName) , '') ;

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szException) , '') ;

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

   TargetException := BufToAnsiString(pAnsiChar(szException),1999) ;
   if trim(string(TargetException)) <> 'OK' then begin
      TFrm_Trace.InternalTraceFromThread(string(TargetException));
      Plug.status := psUnloaded ;  // if error, the plugin is not loaded
   end;

   // todo : save error message in result
end;

//------------------------------------------------------------------------------

procedure TDotNetManager.DoStart (Plug : TDotNetPlugin;Parameter : PAnsiString);
var
   szException  : AnsiString;
   TargetException : AnsiString;
begin
   if not assigned (Start) then
      exit ;

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szException) , '') ;

   try
      //TWrapperStart = procedure (PlugId : integer ; Parameter : PAnsiString; StrException : PAnsiString) stdcall ;
      Start (Plug.PlugID ,Parameter, pAnsiString(szException)) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp Start() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1999) ;
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

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szException) , '') ;

   try
      Stop (Plug.PlugID ,pAnsiString(szException)) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp Stop() exception : ' + e.Message);
      end;
   end;


   TargetException := BufToAnsiString(pAnsiChar(szException),1999) ;
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

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szException) , '') ;

   try
      OnTimer (Plug.PlugID ,pAnsiString(szException)) ;    // wrapper is unicode
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp OnTimer() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),999) ;
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

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szException) , '') ;

   try
      result := OnAction (plug.PlugID, WinId , ButtonId, NodeId, pAnsiString(szException) ) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp OnAction() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1999) ;
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

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pansiChar(szException), '') ;

   try
      result := OnBeforeDelete (Plug.PlugID,WinId , NodeId, pAnsiString(szException)) ;
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp OnBeforeDelete() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1999) ;
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

   SetLength (szException,2000) ;
   System.AnsiStrings.strcopy (pAnsiChar(szException) , '') ;

   try
      Unload (Plug.PlugID ,pAnsiString(szException)) ;   // wrapper is unicode
   except
      on e:exception do begin
         TFrm_Trace.InternalTraceFromThread('call cpp Unload() exception : ' + e.Message);
      end;
   end;

   TargetException := BufToAnsiString(pAnsiChar(szException),1999) ;
   if trim(string(TargetException)) <> 'OK' then
      TFrm_Trace.InternalTraceFromThread('cpp Unload() return error message : ' + string(TargetException));

   Plug.PlugName  := '_' ;
end;


end.