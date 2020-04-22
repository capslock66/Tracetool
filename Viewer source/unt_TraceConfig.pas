unit unt_TraceConfig;

interface

uses
   Contnrs
   , classes
   , unt_plugin ;

type

  TTraceConfig = class  // most of the XmlConfig part often used in the application  (in loop)
  public
    DebugMode                         : boolean ;

    General_LastStyleSheet            : string ;
    General_LastSavedPath             : string ;
    General_InternalLog               : string ;
    General_SocketPort                : Integer ;
    General_SocketPort2               : Integer ;
    General_Udp1                      : boolean ;
    General_Udp2                      : boolean ;
    General_HTTPPort                  : Integer ;
    General_ShowSocketWarning         : boolean ;
    General_SocketPolicyServer        : boolean ;
    General_HttpPolicyServer          : boolean ;

    AppDisplay_SmallBut               : Boolean ;
    AppDisplay_ToolbarStandard        : Boolean ;
    AppDisplay_ToolbarSearch          : Boolean ;
    AppDisplay_ToolbarBookmark        : Boolean ;
    AppDisplay_ToolbarFilter          : Boolean ;
    AppDisplay_HideViewer             : Boolean ;
    AppDisplay_DisableInternalLog     : Boolean ;
    AppDisplay_ApplicationTitle       : string ;
    AppDisplay_left                   : Integer ;
    AppDisplay_top                    : Integer ;
    AppDisplay_Width                  : Integer ;
    AppDisplay_Height                 : Integer ;
    AppDisplay_StayOnTop              : Boolean ;
    AppDisplay_ShowOnstartup          : Boolean ;
    AppDisplay_ShowOnMessageReceived  : Boolean ;
    AppDisplay_FocusToReceivedMessage : Boolean ;
    AppDisplay_Maximized              : Boolean ;
    AppDisplay_IconFile               : string ;
    AppDisplay_MinimizeToSystray      : Boolean ;

    Framework_ShowMembers                   : Boolean ;
    Framework_AutoClear                     : Boolean ;
    Framework_MaxNode                       : Integer ;
    Framework_MinNode                       : Integer ;
    Framework_Enabled                       : Boolean ;
    Framework_VisibleMenu                   : Boolean ;
    Framework_MainTraceTitle                : String ;

    Framework_Trace_FontName                : String ;
    Framework_Trace_FontSize                : Integer ;
    Framework_Trace_NodeHeight              : Integer ;

    Framework_Info_FontName                 : String ;
    Framework_Info_FontSize                 : Integer ;
    Framework_Info_NodeHeight               : Integer ;

    Framework_Orphans_DeletedNode           : String ;
    Framework_Orphans_DefaultLeftText       : String ;
    Framework_Orphans_DefaultRightText      : String ;
    Framework_Orphans_LostAndFoundLeftText  : String ;
    Framework_Orphans_LostAndFoundRightText : String ;

    Watches_Enabled                   : Boolean ;
    Watches_VisibleMenu               : Boolean ;
    Watches_MainWatchesTitle          : String ;

    Watches_Trace_FontName            : String ;
    Watches_Trace_FontSize            : Integer ;
    Watches_Trace_NodeHeight          : Integer ;

    Watches_Info_FontName             : String ;
    Watches_Info_FontSize             : Integer ;
    //Watches_Info_NodeHeight           : Integer ;

    ods_Enabled                       : Boolean ;
    Ods_Title                         : String ;
    Ods_AutoClear                     : Boolean ;
    Ods_MaxNode                       : Integer ;
    Ods_MinNode                       : Integer ;
    Ods_VisibleMenu                   : Boolean ;

    Ods_Trace_FontName                : String ;
    Ods_Trace_FontSize                : Integer ;
    Ods_Trace_NodeHeight              : Integer ;

    Ods_Info_FontName                 : String ;
    Ods_Info_FontSize                 : Integer ;
    //Ods_Info_NodeHeight               : Integer ;

    EventLog_VisibleMenu              : Boolean ;
    EventLog_Trace_FontName           : String ;
    EventLog_Trace_FontSize           : Integer ;
    EventLog_Trace_NodeHeight         : Integer ;

    EventLog_Info_FontName            : String ;
    EventLog_Info_FontSize            : Integer ;
    //EventLog_Info_NodeHeight          : Integer ;

    Tail_LastPath                     : String ;
    Tail_AutoClear                    : Boolean  ;
    Tail_MaxNode                      : Integer  ;
    Tail_MinNode                      : Integer  ;
    tail_ColumnStyle                  : String ;
    tail_AutoCreateColStyle           : String ;
    tail_TextQualifier                : String ;
    tail_Separator                    : String ;
    tail_FirstcolIsTitle              : Boolean  ;
    tail_FixedColCount                : Integer  ;
    tail_OpenFromFavorites            : Boolean  ;
    tail_SizeToLoad                   : Integer ;
    Tail_VisibleMenu                  : Boolean ;

    Tail_Trace_FontName               : String ;
    Tail_Trace_FontSize               : Integer ;
    Tail_Trace_NodeHeight             : Integer ;

    Tail_Info_FontName                : String ;
    Tail_Info_FontSize                : Integer ;
    //Tail_Info_NodeHeight              : Integer ;

    TextExport_ProcessName            : Boolean ;
    TextExport_ThreadId               : Boolean ;
    TextExport_Time                   : Boolean ;
    TextExport_Col1                   : Boolean ;
    TextExport_Col2                   : Boolean ;
    TextExport_GenerateColumnHeader   : Boolean ;
    TextExport_TreeIndentation        : Integer ;
    TextExport_Separator              : String ;
    TextExport_TextQualifier          : String ;

    PluginList                        : TObjectList ;
    FavoriteTailList                  : TStringList ;   // XMLConfig.tail.Favorites

    constructor create () ;
    destructor Destroy () ; override;

    public
    DotNetManager : TDotNetManager ;  // created only when needed


  end ;

var
  TraceConfig : TTraceConfig ;


implementation

uses SysUtils ;

{ TTraceConfig }

constructor TTraceConfig.create;
begin
   General_InternalLog := 'c:\temp\TracetoolInternalLog.txt' ;
   PluginList       := TObjectList.Create (true) ;      // owner
   FavoriteTailList := TStringList.Create (true) ;      // owner
   AppDisplay_DisableInternalLog := true ;

end;



destructor TTraceConfig.Destroy;
begin
   inherited;
   PluginList.Clear ;
   FreeAndNil(PluginList) ;                 // owner

   FavoriteTailList.Clear ;
   FreeAndNil(FavoriteTailList) ;                 // owner

   // dot net plugins
   if DotNetManager <> nil then
      DotNetManager.free ;
end;

end.
