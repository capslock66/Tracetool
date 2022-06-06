{

  Main form : receive TraceNode messages (TraceTool framework) from clients (winmsg,socket,...)
  =============================================================================================

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information

  Indy components must be installed.
}

unit Unt_Tool;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, registry,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, Menus, XMLDoc, XMLIntf, Buttons,
  application6,  // the generated delphi code for the XML schema (Application6.xsd)
  ComCtrls, ToolWin, ImgList, TrayIcon, ActnList , clipbrd, SyncObjs, Contnrs,
  DebugOptions , PSAPI, Tlhelp32 , unt_plugin , unt_editor ,
  IdBaseComponent, IdComponent, IdSocketHandle, IdTCPServer, IdExceptionCore,
  MSXML2_TLB, IdCustomHTTPServer, IdCookie,
  IdThread, idGlobal, IdException, idstack, IdTCPConnection ,  idContext , unt_PageContainer,
  pscMenu, madExceptVcl,
  IdHTTPServer,
  //FileViewer,
  IdCustomTCPServer, IdRawBase, IdRawClient, IdURI,
  IdUDPBase
  , IdUDPServer //, IdCustomTCPServer;
  , Config, System.Actions, System.ImageList, system.UITypes ;

// ensure the path the viewer is on the Options/delphi/library/Library Path
// c:\GitHub\Tracetool\Delphi\Delphi Library\
{$Include TraceTool.Inc}

const
   // StatusBar
   STATUS_Socket       = 1 ;
   STATUS_Received     = 3 ;

type

  TFrm_Tool = class(TForm)
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    ilActions: TImageList;
    Actions: TActionList;
    actOptions: TAction;
    actShutdown: TAction;
    actViewStayOnTop: TAction;
    pmuTaskBar: TPopupMenu;
    mitTrayShow: TMenuItem;
    mitTrayShutdown: TMenuItem;
    mnuShutDown: TMenuItem;
    ImageList1: TImageList;
    StatusBar: TStatusBar;
    TimerTraces: TTimer;
    SaveDialog1: TSaveDialog;
    TCPServer: TIdTCPServer;
    actShow: TAction;
    actHide: TAction;
    Options1: TMenuItem;
    N1: TMenuItem;
    HideWindow1: TMenuItem;
    mnuWindow: TMenuItem;
    actAbout: TAction;
    StayonTop1: TMenuItem;
    N2: TMenuItem;
    About1: TMenuItem;
    mnuTail: TMenuItem;
    OpenDialog1: TOpenDialog;
    actViewOutputDebugString: TAction;
    mnuODS: TMenuItem;
    TimerStatus: TTimer;
    imgMessage: TImage;
    mnuView: TMenuItem;
    mnuLoadXMLfile: TMenuItem;
    mnuEventlog: TMenuItem;
    actLoadXml: TAction;
    actTailWin: TAction;
    actEventlog: TAction;
    actViewMainTraces: TAction;
    mnuViewMainTraces: TMenuItem;
    MainMnuAction: TMenuItem;
    actViewMainWatch: TAction;
    mnuViewMainWatches: TMenuItem;
    UtilityImages: TImageList;
    MadExceptionHandler1: TMadExceptionHandler;
    IdHTTPServer: TIdHTTPServer;
    actDepends: TMenuItem;
    OpenFileDialog: TOpenDialog;
    MadExceptionHandler2: TMadExceptionHandler;
    TCPServer2: TIdTCPServer;
    SocketPolicyServer: TIdTCPServer;
    UDPServer1: TIdUDPServer;
    UDPServer2: TIdUDPServer;
    
    procedure FormCreate(Sender: TObject);
    procedure actShowExecute(Sender: TObject);
    procedure actShutdownExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TimerTracesTimer(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);

    procedure TCPServerConnect(AContext: TIdContext);
    procedure TCPServerDisconnect(AContext: TIdContext);
    procedure TCPServerException(AContext: TIdContext; AException: Exception);
    procedure TCPServerExecute(AContext: TIdContext);
    procedure TCPServerListenException(AThread: TIdListenerThread; AException: Exception);

    procedure actViewStayOnTopExecute(Sender: TObject);
    procedure actHideExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actViewOutputDebugStringExecute(Sender: TObject);
    procedure actLoadXmlExecute(Sender: TObject);
    procedure actTailWinExecute(Sender: TObject);
    procedure actEventlogExecute(Sender: TObject);
    procedure actViewMainTracesExecute(Sender: TObject);

    procedure CMDockClient(var Message: TCMDockClient);  message CM_DOCKCLIENT;
    procedure actViewMainWatchExecute(Sender: TObject);
    procedure MadExceptionHandler1Exception(const exceptIntf: IMEException; var handled: Boolean);
    procedure IdHTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServerException(AContext: TIdContext;
      AException: Exception);
    procedure actDependsClick(Sender: TObject);
    procedure SocketPolicyServerExecute(AContext: TIdContext);
    procedure SocketPolicyServerConnect(AContext: TIdContext);
    procedure SocketPolicyServerDisconnect(AContext: TIdContext);
    procedure SocketPolicyServerException(AContext: TIdContext;
      AException: Exception);
    procedure SocketPolicyServerAfterBind(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    FAllowClose: Boolean;
    FStayOnTop: Boolean;
    FTaskIcon: TTrayIcon;

    procedure ApplicationMsgHandler(var Msg: TMsg; var Handled: Boolean);
    procedure WMEndSession(var Message: TMessage); message WM_ENDSESSION;
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure TrayIconDblClick(Sender: TObject);
    procedure HookSysCmd(var Message:TMessage); message WM_SYSCOMMAND;

    procedure SetStayOnTop(const Value: Boolean);
    procedure TaskBarButton(Visible: Boolean);
    procedure CommandGet(sender: TIdHTTPServer; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);

    procedure UDPServerUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);

   public
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getCommandName (SingleMsg : string;LastParsedTreeRec : pointer = nil) : string ;
    procedure ShowParsedMessage;
    procedure ShowParsedForm(TraceForm: TForm);
    function LoadTracetoolConfig(): IXMLConfig;
    procedure SaveTracetoolConfig(XMLConfig: IXMLConfig);
    procedure LoadConfiguration();
    procedure CheckXml(XMLConfig : IXMLConfig);
    procedure XmlToLocal(XMLConfig : IXMLConfig);
    procedure SaveSettings();
  public
    strConfigFile : string ;
    strRunPath : string ;       // c:\prog file\...
    InitError : string ;
    TailFileName : string ;
    XmlTraceFile : boolean ;
    IsInitMode : boolean ;
    componentHandle :integer ;
  public
    uniqueId : integer ;
  end;

  //---------------------------------------------------------------------------------

  TFontDetail = class (tobject)
     ColId           : integer ;
     Bold            : boolean ;
     Italic          : boolean ;
     Color           : TColor ;    // -$7FFFFFFF-1..$7FFFFFFF;
     BackgroundColor : TColor ;
     Size            : integer ;
     Name            : string ;
     constructor create (pColId : integer ; pBold : boolean ; pItalic : boolean ; pColor: TColor ; pSize: integer ; pName : string) ; overload ;
     constructor create (XmlFontDetail : IXMLFontDetail) ;  overload ;
     constructor create (XmlFontDetail : IXMLDOMNode) ;  overload ;
  end ;
  TFontDetailArray = array of TFontDetail;

  //---------------------------------------------------------------------------------

  TMember = class
     Col1       : string ;
     Col2       : string ;
     Col3       : String ;
     ViewerKind : integer ;
     IsChecked  : boolean ;
     SubMembers : TList ;       // sub members of this member
     FontDetails : TFontDetailArray ;
     procedure AfterConstruction ; override ;
     procedure BeforeDestruction ; override ;  // auto clear sub members
     constructor create (ColA : string = '' ; colB : string = '' ; ColC : string = '' ) ;
     function Search () : boolean ;
  end ;

  //---------------------------------------------------------------------------------


var
  Frm_Tool : TFrm_Tool ;
  MainPageContainer : TFrmPageContainer ;  // main page control

  // to accelerate the process, the viewer use 2 stacks
  // the thread fill the MessageStack while the timer use the second stack

  MessageStack    : TObjectList ;
  SwapStack       : TObjectList ;

  BaseList        : TObjectList ;    // all TFrmBase form (Trace, tail, ods, eventlog)
  FormTraceList   : TObjectList ;    // all trace form . not owner
  TailList        : TObjectList ;    // all Tail
  ContainerList   : TObjectList ;
  ConnectionList  : TObjectList ;
  ScriptMessages  : TStringList ;
  CriticalSection : TCriticalSection ;   // protect the MessageStack and the number of socket connection
  FileCriticalSection : TCriticalSection ; // protect low trace
  Received : integer ;                   // number of messages received
  //ExceptionStep : integer ;

  // used to add messages to the internal message tree
  InternalTraceMessageStack : TStringList ;
  InternalTraceMessageStack2 : TStringList ;

  OdsMessageStack : TObjectList ;

  // memo editor (read only) with grap and move bar. the editor is global for all treeview.
  // The reference is IVTEditLink in place of TMoveMemoEditLink to keep an active reference to it (ref counting)
  VstEditor  : TMoveMemoEditLink ;
  IVstEditor : IVTEditLink ;

  BoldDetail : TFontDetail ;
  PluginsInitialized : boolean ;

implementation

uses unt_receiver, unt_about, unt_tail,Unt_linkedList , unt_TraceWin, unt_parse, unt_base ,
     unt_ODS, unt_utility, unt_selectEvent , unt_eventLog, unt_SelectTail, unt_search, unt_TraceConfig,
  unt_FrmPlugin;


var
  LastParsedTreeRec : PTreeRec ;
//   ServerWintrace : IWinTrace ;

{$R *.dfm}
{$R WindowsXP.RES}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TMember }

procedure TMember.Afterconstruction;
begin
   SubMembers := TList.create ;
end;

//------------------------------------------------------------------------------

procedure TMember.BeforeDestruction;
var
   c : integer ;
   SubMember : TMember ;
begin
   // free font attributes
   for c := 0 to length(FontDetails) -1 do
      FontDetails[c].Free ;
   setlength(FontDetails,0);

   // free sub members
   for c := 0 to SubMembers.Count-1 do begin
      SubMember := TMember (SubMembers.Items[c]) ;
      SubMember.Free ;
   end ;
   SubMembers.Free ;

   Col1 := '' ;
   Col2 := '' ;
   Col3 := '' ;
   inherited;
end;

//------------------------------------------------------------------------------

constructor TMember.create(ColA : string = '' ; colB : string = '' ; ColC : string = '' );
begin
   Col1 := ColA ;
   Col2 := ColB ;
   Col3 := ColC ;
   ViewerKind := CST_VIEWER_NONE ;
end;

//------------------------------------------------------------------------------

function TMember.Search(): boolean;
var
   c : integer ;
begin
   result := false ;
   if (MatchSearch (col1) <> 0) or
      (MatchSearch (col2) <> 0) or
      (MatchSearch (col3) <> 0) then begin
      result := true ;
      exit ;
   end ;
   for c := 0 to SubMembers.Count-1 do begin
      result := TMember (SubMembers.Items[c]).Search() ;
      if result = true then
         exit ;
   end ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// constructor
constructor TFrm_Tool.Create(AOwner: TComponent);
var
   DebugWin: hWnd ;
   MessageString: string;
   CDS: TCopyDataStruct;
   resetDebugMode : boolean ;
   c : integer ;
   EnterDebugMode : boolean;
   LeaveDebugMode : boolean ;
begin

   inherited Create(AOwner);

   // Normally, it's not possible : the main procedure (tracetool.dpr) will detect that the TraceTool mutex is already created.
   // But, in case of, the code is the same as the manyInstances() procedure in unt_Utility...

   TailFileName := '' ;
   resetDebugMode := false ;
   parseParameters(EnterDebugMode,LeaveDebugMode,XmlTraceFile,TailFileName) ;

   if EnterDebugMode then begin
      TraceConfig.DebugMode := true ;
   end else if LeaveDebugMode then begin
      TraceConfig.DebugMode := false ;
      resetDebugMode := true ;
   end ;

   //LowTrace('constructor TFrm_Tool.Create begin ') ;

   DebugWin := FindWindow('TFormReceiver', 'FormReceiver');
   if DebugWin <> 0 then begin
      //LowTrace('Many instances') ;
      InitError := 'Many instances' ;

      if TraceConfig.DebugMode then begin
         MessageString := Format('%5d%11d', [CST_ENTER_DEBUG_MODE,1])  + #0 + #0 ;
         CDS.cbData := 18 ; // 5 + 11 + 1 + 1
      end else if resetDebugMode then begin
         MessageString := Format('%5d%11d', [CST_LEAVE_DEBUG_MODE,1])  + #0 + #0 ;
         CDS.cbData := 18 ; // 5 + 11 + 1 + 1
      end else if (XmlTraceFile) and (TailFileName <> '') then begin
         MessageString := Format('%5d%s', [CST_OPEN_XML,TailFileName]) + #0 + #0 ;
         CDS.cbData := length(MessageString) ; // 5 + len + 1 + 1
      end else if (TailFileName <> '') then begin  // tail ...
         MessageString := Format('%5d%s', [CST_OPEN_TAIL,TailFileName]) + #0 + #0 ;
         CDS.cbData := length(MessageString) ; // 5 + len + 1 + 1
     end else begin // no params or empty filename : ask previous instance to show
         MessageString := Format('%5d%11d', [CST_SHOW,1])  + #0 + #0 ;
         CDS.cbData := 18 ; // 5 + 11 + 1 + 1
      end ;

      // send mesage to other instance
      CDS.dwData := WMD ;   // identification code "123"
      CDS.lpData := pchar (MessageString); // no need to add #0, because String are null terminated
      SendMessage(DebugWin, WM_COPYDATA, 0, LParam(@CDS));  //WParam(Application.Handle)

//      // finish with the CST_SHOW message
//      MessageString := Format('%5d%11d', [CST_SHOW,1])  + #0 + #0 ;
//      CDS.cbData := 18 ; // 5 + 11 + 1 + 1
//
//      // send mesage to other instance
//      CDS.dwData := WMD ;   // identification code "123"
//      CDS.lpData := pAnsiString (AnsiString(MessageString)) ; // no need to add #0, because String are null terminated
//      SendMessage(DebugWin, WM_COPYDATA, 0, LParam(@CDS));  //WParam(Application.Handle)

      halt ;
   end ;

   LowTrace('Load config') ;

   LoadConfiguration() ;

   //LowTrace('Apply config') ;

   UDPServer1.OnUDPRead :=  UDPServerUDPRead ;
   UDPServer2.OnUDPRead :=  UDPServerUDPRead ;

   TCPServer.DefaultPort     := TraceConfig.General_SocketPort ;
   TCPServer2.DefaultPort    := TraceConfig.General_SocketPort2 ;
   UdpServer1.DefaultPort    := TraceConfig.General_SocketPort ;
   UdpServer2.DefaultPort    := TraceConfig.General_SocketPort2 ;

   IdHTTPServer.DefaultPort  := TraceConfig.General_HTTPPort ;
   Caption                   := TraceConfig.AppDisplay_ApplicationTitle ;
   top                       := TraceConfig.AppDisplay_top ;
   left                      := TraceConfig.AppDisplay_left ;
   Height                    := TraceConfig.AppDisplay_height ;
   Width                     := TraceConfig.AppDisplay_width ;
   if TraceConfig.AppDisplay_Maximized then
      WindowState := wsMaximized ;

   mnuViewMainTraces.Visible := TraceConfig.Framework_VisibleMenu ;    // windows menu
   mnuLoadXMLfile.Visible    := TraceConfig.Framework_VisibleMenu ;    // windows menu
   mnuEventlog.Visible       := TraceConfig.EventLog_VisibleMenu ;     // windows menu
   mnuTail.Visible           := TraceConfig.tail_VisibleMenu ;         // windows menu
   mnuODS.Visible            := TraceConfig.ods_VisibleMenu ;          // windows menu

   if (TraceConfig.AppDisplay_IconFile <> '') and (FileExists(TraceConfig.AppDisplay_IconFile)) then begin
     imagelist1.Clear ;
     imagelist1.FileLoad(rtBitmap,TraceConfig.AppDisplay_IconFile, clFuchsia) ;
   end ;

   SaveDialog1.InitialDir   := TraceConfig.General_LastSavedPath ;   // same dialog box for all Save

   if TraceConfig.AppDisplay_StayOnTop then
      actViewStayOnTopExecute(nil);

   // show main form on startup is done on the Tracetool.dpr file :
   // Application.ShowMainForm := ConfigInfo.Show_Startup ;

   FAllowClose := False;
   InitError := '' ;

   // open socket port 1
   try
      if TCPServer.DefaultPort <> 0 then begin
         //LowTrace('Activate tcp/udp 1') ;
         if TraceConfig.General_Udp1 then
            UdpServer1.Active := true
         else
            TCPServer.Active := true ;
      end ;
   except
      on e : EIdSocketError do begin
         if (e.LastError = 10042) then begin  // bad protocol option. happens on config error
            InitError := e.Message ;
         end ;
      end ;
      on e : EIdCouldNotBindSocket do begin
         // InitError := 'EIdCouldNotBindSocket' ;
         MessageDlg('Unable to open socket port ' + inttostr(TraceConfig.General_SocketPort), mtWarning, [mbOK], 0);
         LowTrace('EIdCouldNotBindSocket : Unable to open socket port ' + inttostr(TraceConfig.General_SocketPort)) ;
         FAllowClose := true;
         //TCPServer.Destroy ;
         exit ;
      end ;

      on e : exception do begin
         InitError := e.Message ;
         FAllowClose := true;
      end ;
   end ;
   if (InitError <> '') then
      LowTrace(InitError) ;

   // open socket port 2
   try
      if TCPServer2.DefaultPort <> 0 then begin
         //LowTrace('Activate tcp/upd 2') ;
         if TraceConfig.General_Udp2 then
            UdpServer2.Active := true
         else
            TCPServer2.Active := true ;
      end ;
   except
      on e : EIdSocketError do begin
         if (e.LastError = 10042) then begin  // bad protocol option. happens on config error
            InitError := e.Message ;
         end ;
      end ;
      on e : EIdCouldNotBindSocket do begin
         // InitError := 'EIdCouldNotBindSocket' ;
         MessageDlg('Unable to open socket port ' + inttostr(TraceConfig.General_SocketPort2), mtWarning, [mbOK], 0);
         LowTrace('EIdCouldNotBindSocket : Unable to open socket port ' + inttostr(TraceConfig.General_SocketPort2)) ;
         FAllowClose := true;
         //TCPServer2.Destroy ;
         exit ;
      end ;

      on e : exception do begin
         InitError := e.Message ;
         FAllowClose := true;
      end ;
   end ;

   if (InitError <> '')  then
      LowTrace(InitError) ;

   // open http port
   try
      if IdHTTPServer.DefaultPort <> 0 then begin
         //LowTrace('Activate http') ;
         IdHTTPServer.active := true ;
      end ;
   except
      on e : EIdSocketError do begin
         if (e.LastError = 10042) then begin  // bad protocol option. happens on config error
            InitError := e.Message ;
         end ;
      end ;
      on e : EIdCouldNotBindSocket do begin
         MessageDlg('Unable to open socket port ' + inttostr(IdHTTPServer.DefaultPort), mtWarning, [mbOK], 0);
         //InitError := 'EIdCouldNotBindSocket' ;

         LowTrace('EIdCouldNotBindSocket : Unable to open socket port ' + inttostr(IdHTTPServer.DefaultPort)) ;
      end ;

      on e : exception do begin
         InitError := e.Message ;
      end ;
   end ;

   if (InitError <> '') then
      LowTrace(InitError) ;

   // open socket Policy server
   try
      if TraceConfig.General_SocketPolicyServer <> false then begin
         //LowTrace('Activate PolicyServer') ;
         SocketPolicyServer.active := true ;
      end ;
   except
      on e : EIdSocketError do begin
         if (e.LastError = 10042) then begin  // bad protocol option. happens on config error
            InitError := e.Message ;
         end ;
      end ;
      on e : EIdCouldNotBindSocket do begin
         MessageDlg('Unable to open socket port ' + inttostr(SocketPolicyServer.DefaultPort), mtWarning, [mbOK], 0);
         //InitError := 'EIdCouldNotBindSocket' ;
         LowTrace('EIdCouldNotBindSocket : Unable to open socket port ' + inttostr(SocketPolicyServer.DefaultPort)) ;
      end ;

      on e : exception do begin
         InitError := e.Message ;
      end ;
   end ;
   if (InitError <> '')  then
      LowTrace(InitError) ;

   Application.OnMessage := ApplicationMsgHandler;
   Application.ShowHint := True;

   FTaskIcon := TTrayIcon.Create(Self);
   FTaskIcon.Icon := Icon;
   FTaskIcon.Active := True;
   FTaskIcon.PopupMenu := pmuTaskBar;
   FTaskIcon.ToolTip := Application.Title;
   FTaskIcon.OnDblClick := TrayIconDblClick;
   FTaskIcon.Icon := imgMessage.Picture.Icon;

   if (TraceConfig.AppDisplay_ShowOnstartup = true) and (TraceConfig.AppDisplay_HideViewer = false) then
      Application.ShowMainForm := true
   else
      actHideExecute(nil) ; //TaskBarButton (false) ;

   //LowTrace('constructor TFrm_Tool.Create end ') ;

end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.FormCreate(Sender: TObject);
var
   c : integer ;
   FrmPageContainer : TFrmPageContainer ;
   mnuItem : TMenuItem ;
   Trace : TFrm_Trace ;

begin
   //LowTrace('TFrm_Tool.FormCreate begin') ;

   uniqueId := 0 ;

   with TPSCMenu.create (self) do begin     // main page
      DimLevel := 0 ;    // don't gray icon
      Active := true ;
   end ;

   Application.ProcessMessages ;

   // connection should be actived in TFrm_Tool.Create procedure
   //if (TCPServer.active = false) and (InitError = 'EIdCouldNotBindSocket') then  begin
   if InitError <> '' then begin
      LowTrace('TFrm_Tool.FormCreate terminated : error : ' + InitError) ;
      close ;
      exit ;
   end ;

   // at design time, the FormReceiver caption is blank.
   // setting 'FormReceiver' at run time will ensure that we will no use the design time form
   // The design form of course will not respond to message since it's a Delphi IDE form
   // This will also ensure that only one Tool can receive message, since only one TCP Server can be
   // actived at the same time.

   FormReceiver.Caption := 'FormReceiver' ;

   // handle of the component
   componentHandle := 0 ;
   IsInitMode := true ;

   // create the main page control and toolbar
   MainPageContainer := TFrmPageContainer.Create(self);
   MainPageContainer.PanelPageControl.Parent := self ; //  DockingPagecontrol.Parent := self ;
   MainPageContainer.Align := alClient ;

   // move actions to main menu
   while MainPageContainer.MnuAction.Count <> 0 do begin
      mnuItem := MainPageContainer.MnuAction.Items[0] ;
      MainPageContainer.MnuAction.Delete(0);
      MainMnuAction.Add(mnuItem);
   end ;
                           
   for c := 0 to ContainerList.Count-1 do begin
      FrmPageContainer := TFrmPageContainer(ContainerList[c]) ;
      FrmPageContainer.configureToolbar ;
   end ;

   // create the internal trace window (invisible)
   //LowTrace('create internal trace window (invisible)') ;
   FrmInternalTraces := TFrm_Trace.Create(nil);
   FrmInternalTraces.Name := 'FrmInternalTraces' ;
   FrmInternalTraces.ID := 'ERRID' ;
   FrmInternalTraces.Caption := 'Internal Trace' ;

   if TraceConfig.DebugMode then begin
      FrmInternalTraces.Visible := true ;
      if FrmInternalTraces.Parent = nil then
         FrmInternalTraces.DockToMainPanel() ;
      FrmInternalTraces.SetActivePage() ;  // PageControlChange() ;
   end ;

   // create the main Trace window
   //LowTrace('create the main Trace window') ;
   Frm_Trace := TFrm_Trace.Create(nil);
   Frm_Trace.Id := '' ;  // main trace form don't have ID
   Frm_Trace.Caption := TraceConfig.Framework_MainTraceTitle ;
   Frm_Trace.DockToMainPanel();
   Frm_Trace.getPageContainer().actViewTraceInfo.Checked := TraceConfig.Framework_ShowMembers ;  // remain members button state only for main win
   Frm_Trace.ViewTraceInfo ;
   if TraceConfig.Framework_Enabled = true then
      Frm_Trace.Show
   else
      Frm_Trace.Hide ;

   // create the main watch window
   //LowTrace('create the main watch window') ;
   Frm_Watches := CreateWatchForm('', TraceConfig.Watches_MainWatchesTitle) ;  // windows id and name
   if TraceConfig.Watches_Enabled = true then
      Frm_Watches.Show
   else
      Frm_Watches.Hide ;

   // create the ODS window
   LowTrace('create the ODS window') ;
   Frm_ODS := TFrm_ODS.Create(nil);  // start if ConfigInfo.EnableODS ;
   Frm_ODS.Caption := TraceConfig.Ods_Title  ;  // 'ODS'

   if TraceConfig.ods_Enabled then begin
      Frm_ODS.DockToMainPanel() ;
      Frm_ODS.Visible := true ;
   end ;

   mnuODS.Visible := TraceConfig.Ods_VisibleMenu ;
   mitTrayShow.Enabled := not TraceConfig.AppDisplay_HideViewer ;
   mitTrayShow.Visible := not TraceConfig.AppDisplay_HideViewer ;

   // hide cutter, time and thread id columns for the Internal trace window
   FrmInternalTraces.vstTrace.Header.Columns[0].Options := FrmInternalTraces.vstTrace.Header.Columns[0].Options - [coVisible] ;
   FrmInternalTraces.vstTrace.Header.Columns[1].Options := FrmInternalTraces.vstTrace.Header.Columns[1].Options - [coVisible] ;
   FrmInternalTraces.vstTrace.Header.Columns[2].Options := FrmInternalTraces.vstTrace.Header.Columns[2].Options - [coVisible] ;

   // force redesign toolbar and menu for the new active page
   MainPageContainer.DockingPagecontrol.onChange(nil) ;

   // set the main trace as the active form
   if TraceConfig.Framework_Enabled = true then
      Frm_Trace.SetActivePage ;

   FrmSelectTail := TFrmSelectTail.Create(Application);   // Application.CreateForm(TFrmSelectTail, FrmSelectTail);
   if (XmlTraceFile) and (TailFileName <> '') then begin
      // create the trace form
      Application.CreateForm(TFrm_Trace, Trace);
      Trace.Caption := 'Trace::' + ExtractFileName (TailFileName) ;

      Trace.DockToMainPanel() ;
      Trace.SetActivePage() ;
      Trace.getPageControl.OnChange (nil) ;

      application.ProcessMessages ;
      SetCursor(Screen.Cursors[crHourGlass]);

      try
         Trace.LoadXML(TailFileName);
      finally
         SetCursor(Screen.Cursors[crDefault]);
      end ;
   end else if TailFileName <> '' then begin
      FrmSelectTail.initOptions ;
      FrmSelectTail.OpenFile(TailFileName);
      Frm_Tool.actShowExecute (nil) ;
   end ;
   LowTrace('TFrm_Tool.FormCreate end') ;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.FormShow(Sender: TObject);
begin
   LowTrace('TFrm_Tool.FormShow') ;
end;

//------------------------------------------------------------------------------

destructor TFrm_Tool.Destroy;
var
   c : integer ;
   frm : TFrm_Trace ;
   isclosed : boolean ;
begin
  //SaveSettings ();

  // stop plugins (added in version 9)
  StopAllplugins() ;
  
  // close forms except Frm_Trace , Frm_Watches and FrmInternalTraces
  isclosed := true ;
  while isclosed do begin
     isclosed := false ;
     for c := 0 to FormTraceList.Count-1 do begin   // not owner
        frm := TFrm_Trace (FormTraceList.Items[c]) ;
        if (frm <> FrmInternalTraces) and (frm <> Frm_Trace) and (frm <> Frm_Watches) then begin
           frm.close ;
           frm.Free ;
           isclosed := true ;   // ask to loop again
           break ;              // break loop : the FormTraceList.Count is no more correct
        end ;
     end ;
  end ;
  FormTraceList.Clear ; // not owner

  Frm_ODS.close ;           Frm_ODS.free;             Frm_ODS           := nil ;
  Frm_Watches.close ;       Frm_Watches.free;         Frm_Watches       := nil ;
  Frm_Trace.close ;         Frm_Trace.free;           Frm_Trace         := nil ;
  if FrmInternalTraces <> nil then begin
     FrmInternalTraces.close ;
     FrmInternalTraces.free;
     FrmInternalTraces := nil ;   // TODO : check before close and free
  end ;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

// Application.OnMessage := ApplicationMsgHandler;

procedure TFrm_Tool.ApplicationMsgHandler(var Msg: TMsg; var Handled: Boolean);
begin
  if (Msg.Message = WM_SYSCOMMAND) and (Msg.wParam = SC_RESTORE) then
    Show;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if IVstEditor <> nil then
      IVstEditor := nil ; // reference counting
end;

//------------------------------------------------------------------------------

function TFrm_Tool.LoadTracetoolConfig () : IXMLConfig ;
var
   XmlDocument : IXmlDocument ;
   XMLConfig : IXMLConfig ;
   traceToolStrings : TStringList ;
   traceToolText : string ;
begin
   if FileExists (strConfigFile) then begin   
      traceToolStrings := TStringList.Create ;
      traceToolStrings.LoadFromFile(strConfigFile);
      traceToolText := traceToolStrings.Text ;
      traceToolStrings.Free;
      XmlDocument := LoadXMLData(traceToolText) ;
      XMLConfig := GetConfig (XmlDocument) ;
      XmlDocument := nil ;
   end else begin
      LowTrace('create config file') ;
      XMLConfig := NewConfig() ;  
   end;
     result := XMLConfig ; 
end;

procedure TFrm_Tool.SaveTracetoolConfig (XMLConfig : IXMLConfig) ;
var
   traceToolStrings : TStringList ;
   traceToolText : string ;
begin
   traceToolText := XMLConfig.OwnerDocument.XML.Text ;

   traceToolStrings := TStringList.Create ;
   traceToolStrings.Text := traceToolText ;
   traceToolStrings.SaveToFile(strConfigFile);
   traceToolStrings.Free;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.LoadConfiguration();
var
   XMLConfig : IXMLConfig ;
   //XmlDocument : IXmlDocument ;
   //traceToolStrings : TStringList ;
   //traceToolText : string ;
begin
   strConfigFile := ParamStr(0) ;                   // c:\pf\Tracetool.exe
   strRunPath := ExtractFilePath(strConfigFile) ;   // c:\pf\

   // the donet api (tracetool.dll) has a documentation file : TraceTool.xml 
   // in version 12.x, the tracetool utility had a configuration file : TraceTool.xml 
   // both files were in conflict (and locked when a dotnet plugin was loaded)
   // Starting with version 13.0, the tracetool utility config file is renamed to TraceToolConfig.xml
   
   strConfigFile := strRunPath + 'TraceToolConfig.xml' ;
   if FileExists (strConfigFile) then begin   
      XMLConfig := LoadTracetoolConfig() ;
   end else begin
      strConfigFile := strRunPath + 'TraceTool.xml' ; 
      XMLConfig := LoadTracetoolConfig() ;
      DeleteFile(strConfigFile) ;  // delete old TraceTool.xml file, if exist. 'TraceToolConfig.xml' will be recreated later   
      strConfigFile := strRunPath + 'TraceToolConfig.xml' ; 
   end;
   
   CheckXml(XMLConfig) ;     // check if all options are valid
   XmlToLocal(XMLConfig) ;   // convert Xml to local config
   SaveSettings() ;          // save missing default settings 
   
   XMLConfig := nil ;
end;

//------------------------------------------------------------------------------

// Ensure the xml file is correct
procedure TFrm_Tool.CheckXml (XMLConfig : IXMLConfig);
var
   Reg: TRegistry;
   Buf: array[0..MAX_PATH + 1] of Char;
   c : integer ;
//   jvms : TStringList ;
begin
   Reg := TRegistry.Create;
   try try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey('Software\TraceTool',true);
      GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf)-1);
      Reg.WriteString('FilePath', StrPas(Buf));          // ERegistryException : failed to set data for 'FilePath'
      except
         on e: exception do begin
            //LowTrace('WriteString : ' + e.Message );
         end ;
      end ;
   finally
      Reg.CloseKey ;
      Reg.Free;
   end ;

   // General options
   // -----------------------------------------------------------------------

   if XMLConfig.General.LastStyleSheet.Attributes           ['Value'] = Null then XMLConfig.General.LastStyleSheet.Value := '' ;
   if XMLConfig.General.LastSavedPath.Attributes            ['Value'] = Null then XMLConfig.General.LastSavedPath.Value := '' ;
   if XMLConfig.General.InternalLog.Attributes              ['Value'] = Null then XMLConfig.General.InternalLog.Value := 'c:\temp\TracetoolInternalLog.txt' ;
   if XMLConfig.General.SocketPort.Attributes               ['Value'] = Null then XMLConfig.General.SocketPort.Value := 8090 ;
   if XMLConfig.General.SocketPort2.Attributes              ['Value'] = Null then XMLConfig.General.SocketPort2.Value := 4502 ;
   if XMLConfig.General.Udp1.Attributes                     ['Value'] = Null then XMLConfig.General.Udp1.Value := false ;
   if XMLConfig.General.Udp2.Attributes                     ['Value'] = Null then XMLConfig.General.Udp2.Value := false ;
   if XMLConfig.General.HTTPPort.Attributes                 ['Value'] = Null then XMLConfig.General.HTTPPort.Value := 0 ;  // disabled by default
   if XMLConfig.General.ShowSocketWarning.Attributes        ['Value'] = Null then XMLConfig.General.ShowSocketWarning.Value := false ;
   if XMLConfig.General.SocketPolicyServer.Attributes       ['Value'] = Null then XMLConfig.General.SocketPolicyServer.Value := false ;
   if XMLConfig.General.HttpPolicyServer.Attributes         ['Value'] = Null then XMLConfig.General.HttpPolicyServer.Value := false ;

   // display options
   // -----------------------------------------------------------------------

   if XMLConfig.AppDisplay.SmallBut.Attributes              ['Value'] = Null then XMLConfig.AppDisplay.SmallBut.Value := true ;
   if XMLConfig.AppDisplay.ToolbarStandard.Attributes       ['Value'] = Null then XMLConfig.AppDisplay.ToolbarStandard.Value := true ;
   if XMLConfig.AppDisplay.ToolbarSearch.Attributes         ['Value'] = Null then XMLConfig.AppDisplay.ToolbarSearch.Value   := true ;
   if XMLConfig.AppDisplay.ToolbarBookmark.Attributes       ['Value'] = Null then XMLConfig.AppDisplay.ToolbarBookmark.Value := true ;
   if XMLConfig.AppDisplay.ToolbarFilter.Attributes         ['Value'] = Null then XMLConfig.AppDisplay.ToolbarFilter.Value   := true ;
   if XMLConfig.AppDisplay.HideViewer.Attributes            ['Value'] = Null then XMLConfig.AppDisplay.HideViewer.Value      := false ;
   if XMLConfig.AppDisplay.DisableInternalLog.Attributes    ['Value'] = Null then XMLConfig.AppDisplay.DisableInternalLog.Value := false ;
   if XMLConfig.AppDisplay.ApplicationTitle.Attributes      ['Value'] = Null then XMLConfig.AppDisplay.ApplicationTitle.Value := 'Trace and Object inspector Tool' ;
   if XMLConfig.AppDisplay.left.Attributes                  ['Value'] = Null then XMLConfig.AppDisplay.left.Value := Screen.Width - 700 ;
   if XMLConfig.AppDisplay.top.Attributes                   ['Value'] = Null then XMLConfig.AppDisplay.top.Value := 0 ;
   if XMLConfig.AppDisplay.width.Attributes                 ['Value'] = Null then XMLConfig.AppDisplay.width.Value := 700 ;
   if XMLConfig.AppDisplay.height.Attributes                ['Value'] = Null then XMLConfig.AppDisplay.height.Value := 600 ;
   if XMLConfig.AppDisplay.stayOnTop.Attributes             ['Value'] = Null then XMLConfig.AppDisplay.stayOnTop.Value := false ;
   if XMLConfig.AppDisplay.ShowOnstartup.Attributes         ['Value'] = Null then XMLConfig.AppDisplay.ShowOnstartup.Value := true ;
   if XMLConfig.AppDisplay.ShowOnMessageReceived.Attributes ['Value'] = Null then XMLConfig.AppDisplay.ShowOnMessageReceived.Value := false ;
   if XMLConfig.AppDisplay.FocusToReceivedMessage.Attributes['Value'] = Null then XMLConfig.AppDisplay.FocusToReceivedMessage.Value := true ;
   if XMLConfig.AppDisplay.Maximized.Attributes             ['Value'] = Null then XMLConfig.AppDisplay.Maximized.Value := false ;
   if XMLConfig.AppDisplay.IconFile.Attributes              ['Value'] = Null then XMLConfig.AppDisplay.IconFile.Value := '' ;
   if XMLConfig.AppDisplay.MinimizeToSystray.Attributes     ['Value'] = Null then XMLConfig.AppDisplay.MinimizeToSystray.Value := false ;

   // Trace Framework
   // autoclear, MaxNode and MinNode are used for trace framework
   // -----------------------------------------------------------------------

   if XMLConfig.Framework.ShowMembers.Attributes            ['Value'] = Null then XMLConfig.Framework.ShowMembers.Value := false ;
   if XMLConfig.Framework.AutoClear.Attributes              ['Value'] = Null then XMLConfig.Framework.AutoClear.Value := true ;
   if XMLConfig.Framework.MaxNode.Attributes                ['Value'] = Null then XMLConfig.Framework.MaxNode.Value := 2000 ;
   if XMLConfig.Framework.MinNode.Attributes                ['Value'] = Null then XMLConfig.Framework.MinNode.Value := 1000 ;
   if XMLConfig.Framework.Enabled.Attributes                ['Value'] = Null then XMLConfig.Framework.Enabled.Value := true ;
   if XMLConfig.Framework.VisibleMenu.Attributes            ['Value'] = Null then XMLConfig.Framework.VisibleMenu.value := true ;
   if XMLConfig.Framework.MainTraceTitle.Attributes         ['Value'] = Null then XMLConfig.Framework.MainTraceTitle.value := 'Traces' ;

   if XMLConfig.Framework.Trace.FontName.Attributes         ['Value'] = Null then XMLConfig.Framework.Trace.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.Framework.Trace.FontSize.Attributes         ['Value'] = Null then XMLConfig.Framework.Trace.FontSize.Value := 8 ;
   if XMLConfig.Framework.Trace.NodeHeight.Attributes       ['Value'] = Null then XMLConfig.Framework.Trace.NodeHeight.Value := 18 ;

   if XMLConfig.Framework.Info.FontName.Attributes          ['Value'] = Null then XMLConfig.Framework.Info.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.Framework.Info.FontSize.Attributes          ['Value'] = Null then XMLConfig.Framework.Info.FontSize.Value := 8 ;
   if XMLConfig.Framework.Info.NodeHeight.Attributes        ['Value'] = Null then XMLConfig.Framework.Info.NodeHeight.Value := 18 ;

   if XMLConfig.Framework.Orphans.DeletedNode.Attributes           ['Value'] = Null then XMLConfig.Framework.Orphans.DeletedNode.Value := 'CreateUnderLostAndFound' ;
   if XMLConfig.Framework.Orphans.DefaultLeftText.Attributes       ['Value'] = Null then XMLConfig.Framework.Orphans.DefaultLeftText.Value := 'Restored' ;
   if XMLConfig.Framework.Orphans.DefaultRightText.Attributes      ['Value'] = Null then XMLConfig.Framework.Orphans.DefaultRightText.Value := '' ;
   if XMLConfig.Framework.Orphans.LostAndFoundLeftText.Attributes  ['Value'] = Null then XMLConfig.Framework.Orphans.LostAndFoundLeftText.Value := 'Lost and found' ;
   if XMLConfig.Framework.Orphans.LostAndFoundRightText.Attributes ['Value'] = Null then XMLConfig.Framework.Orphans.LostAndFoundRightText.Value :=  '' ;

   // Watches
   // -----------------------------------------------------------------------

   if XMLConfig.Watches.Enabled.Attributes                  ['Value'] = Null then XMLConfig.Watches.Enabled.Value := false ;  // by default, don't display watches window
   if XMLConfig.Watches.VisibleMenu.Attributes              ['Value'] = Null then XMLConfig.Watches.VisibleMenu.value := true ;
   if XMLConfig.Watches.MainWatchesTitle.Attributes         ['Value'] = Null then XMLConfig.Watches.MainWatchesTitle.value := 'Watches' ;

   if XMLConfig.Watches.Trace.FontName.Attributes           ['Value'] = Null then XMLConfig.Watches.Trace.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.Watches.Trace.FontSize.Attributes           ['Value'] = Null then XMLConfig.Watches.Trace.FontSize.Value := 8 ;
   if XMLConfig.Watches.Trace.NodeHeight.Attributes         ['Value'] = Null then XMLConfig.Watches.Trace.NodeHeight.Value := 18 ;

   if XMLConfig.Watches.Info.FontName.Attributes            ['Value'] = Null then XMLConfig.Watches.Info.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.Watches.Info.FontSize.Attributes            ['Value'] = Null then XMLConfig.Watches.Info.FontSize.Value := 8 ;
   //if XMLConfig.Watches.Info.NodeHeight.Attributes          ['Value'] = Null then XMLConfig.Watches.Info.NodeHeight.Value := 18 ;

   // outputdebugString options
   // -----------------------------------------------------------------------

   if XMLConfig.ods.Enabled.Attributes                      ['Value'] = Null then XMLConfig.ods.Enabled.Value := true ;
   if XMLConfig.ods.Title.Attributes                        ['Value'] = Null then XMLConfig.ods.Title.Value := 'ODS' ;
   if XMLConfig.ods.AutoClear.Attributes                    ['Value'] = Null then XMLConfig.ods.AutoClear.Value := true ;
   if XMLConfig.ods.MaxNode.Attributes                      ['Value'] = Null then XMLConfig.ods.MaxNode.Value := 2000 ;
   if XMLConfig.ods.MinNode.Attributes                      ['Value'] = Null then XMLConfig.ods.MinNode.Value := 1000 ;
   if XMLConfig.ods.VisibleMenu.Attributes                  ['Value'] = Null then XMLConfig.ods.VisibleMenu.value := true ;

   if XMLConfig.ods.Trace.FontName.Attributes               ['Value'] = Null then XMLConfig.ods.Trace.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.ods.Trace.FontSize.Attributes               ['Value'] = Null then XMLConfig.ods.Trace.FontSize.Value := 8 ;
   if XMLConfig.ods.Trace.NodeHeight.Attributes             ['Value'] = Null then XMLConfig.ods.Trace.NodeHeight.Value := 18 ;

   if XMLConfig.ods.Info.FontName.Attributes                ['Value'] = Null then XMLConfig.ods.Info.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.ods.Info.FontSize.Attributes                ['Value'] = Null then XMLConfig.ods.Info.FontSize.Value := 8 ;
   //if XMLConfig.ods.Info.NodeHeight.Attributes              ['Value'] = Null then XMLConfig.ods.Info.NodeHeight.Value := 18 ;

   // event Log options
   //------------------------------------------------------------------------

   if XMLConfig.EventLog.VisibleMenu.Attributes             ['Value'] = Null then XMLConfig.EventLog.VisibleMenu.value := true ;
   if XMLConfig.EventLog.Trace.FontName.Attributes          ['Value'] = Null then XMLConfig.EventLog.Trace.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.EventLog.Trace.FontSize.Attributes          ['Value'] = Null then XMLConfig.EventLog.Trace.FontSize.Value := 8 ;
   if XMLConfig.EventLog.Trace.NodeHeight.Attributes        ['Value'] = Null then XMLConfig.EventLog.Trace.NodeHeight.Value := 18 ;

   if XMLConfig.EventLog.Info.FontName.Attributes           ['Value'] = Null then XMLConfig.EventLog.Info.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.EventLog.Info.FontSize.Attributes           ['Value'] = Null then XMLConfig.EventLog.Info.FontSize.Value := 8 ;
   //if XMLConfig.EventLog.Info.NodeHeight.Attributes         ['Value'] = Null then XMLConfig.EventLog.Info.NodeHeight.Value := 18 ;

   // tail options
   // -----------------------------------------------------------------------

   if XMLConfig.Tail.LastPath.Attributes                    ['Value'] = Null then XMLConfig.Tail.LastPath.Value := '' ;
   if XMLConfig.tail.AutoClear.Attributes                   ['Value'] = Null then XMLConfig.tail.AutoClear.Value := true ;
   if XMLConfig.tail.MaxNode.Attributes                     ['Value'] = Null then XMLConfig.tail.MaxNode.Value := 2000 ;
   if XMLConfig.tail.MinNode.Attributes                     ['Value'] = Null then XMLConfig.tail.MinNode.Value := 1000 ;
   if XMLConfig.tail.ColumnStyle.Attributes                 ['Value'] = Null then XMLConfig.tail.ColumnStyle.value := 'Multi' ; // Classic , Lines , Multi
   if XMLConfig.tail.AutoCreateColStyle.Attributes          ['Value'] = Null then XMLConfig.tail.AutoCreateColStyle.value := 'Each' ; // First, Each , Fixed
   if XMLConfig.tail.TextQualifier.Attributes               ['Value'] = Null then XMLConfig.tail.TextQualifier.value := 'None' ;
   if XMLConfig.tail.Separator.Attributes                   ['Value'] = Null then XMLConfig.tail.Separator.value := '9' ;
   if XMLConfig.tail.FirstcolIsTitle.Attributes             ['Value'] = Null then XMLConfig.tail.FirstcolIsTitle.value := true ;
   if XMLConfig.tail.FixedColCount.Attributes               ['Value'] = Null then XMLConfig.tail.FixedColCount.value := 5 ;
   if XMLConfig.tail.OpenFromFavorites.Attributes           ['Value'] = Null then XMLConfig.tail.OpenFromFavorites.value := false ;
   if XMLConfig.tail.SizeToLoad.Attributes                  ['Value'] = Null then XMLConfig.tail.SizeToLoad.value := 800 ;
   if XMLConfig.tail.VisibleMenu.Attributes                 ['Value'] = Null then XMLConfig.tail.VisibleMenu.value := true ;

   if XMLConfig.tail.Trace.FontName.Attributes              ['Value'] = Null then XMLConfig.tail.Trace.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.tail.Trace.FontSize.Attributes              ['Value'] = Null then XMLConfig.tail.Trace.FontSize.Value := 8 ;
   if XMLConfig.tail.Trace.NodeHeight.Attributes            ['Value'] = Null then XMLConfig.tail.Trace.NodeHeight.Value := 18 ;

   if XMLConfig.tail.Info.FontName.Attributes               ['Value'] = Null then XMLConfig.tail.Info.FontName.Value := 'MS Sans Serif' ;
   if XMLConfig.tail.Info.FontSize.Attributes               ['Value'] = Null then XMLConfig.tail.Info.FontSize.Value := 8 ;
   //if XMLConfig.tail.Info.NodeHeight.Attributes             ['Value'] = Null then XMLConfig.tail.Info.NodeHeight.Value := 18 ;

   // clipboard options
   // -----------------------------------------------------------------------

   if XMLConfig.TextExport.ProcessName.Attributes           ['Value'] = Null then XMLConfig.TextExport.ProcessName.Value := false ;
   if XMLConfig.TextExport.ThreadID.Attributes              ['Value'] = Null then XMLConfig.TextExport.ThreadID.Value := true ;
   if XMLConfig.TextExport.Time.Attributes                  ['Value'] = Null then XMLConfig.TextExport.Time.Value := true ;
   if XMLConfig.TextExport.Col1.Attributes                  ['Value'] = Null then XMLConfig.TextExport.Col1.Value := true ;
   if XMLConfig.TextExport.Col2.Attributes                  ['Value'] = Null then XMLConfig.TextExport.Col2.Value := true ;
   if XMLConfig.TextExport.GenerateColumnHeader.Attributes  ['Value'] = Null then XMLConfig.TextExport.GenerateColumnHeader.Value := false ;
   if XMLConfig.TextExport.TreeIndentation.Attributes       ['Value'] = Null then XMLConfig.TextExport.TreeIndentation.Value := 3 ;
   if XMLConfig.TextExport.Separator.Attributes             ['Value'] = Null then XMLConfig.TextExport.Separator.Value := '9' ;
   if XMLConfig.TextExport.TextQualifier.Attributes         ['Value'] = Null then XMLConfig.TextExport.TextQualifier.Value := 'None' ;

   //plugins
   // -----------------------------------------------------------------------

   for c := 0 to XMLConfig.Plugins.Plugin.Count-1 do begin
      if XMLConfig.Plugins.Plugin[c].Kind = '' then
         XMLConfig.Plugins.Plugin[c].Kind := 'Win32' ;

      if (XMLConfig.Plugins.Plugin[c].Enabled.Attributes['Value'] = Null) or
         (XMLConfig.Plugins.Plugin[c].Enabled.Attributes['Value'] = '') then
         XMLConfig.Plugins.Plugin[c].Enabled.value := false ;
   end ;
end;

//------------------------------------------------------------------------------

// Map IXMLConfig to TTraceConfig instance
procedure TFrm_Tool.XmlToLocal(XMLConfig : IXMLConfig) ;
var
   c : integer ;
   xmlPlugin : IXMLPlugin ;
   plugKind : string ;
   plugin : TPlugin ;
   Win32plugin : TWin32Plugin ;
   DotNetPlugin : TDotNetPlugin ;
begin

   // General options
   // -----------------------------------------------------------------------

   TraceConfig.General_LastStyleSheet                   := XMLConfig.General.LastStyleSheet.Value ;
   TraceConfig.General_LastSavedPath                    := XMLConfig.General.LastSavedPath.Value ;
   TraceConfig.General_InternalLog                      := XMLConfig.General.InternalLog.Value  ;
   TraceConfig.General_SocketPort                       := XMLConfig.General.SocketPort.Value ;
   TraceConfig.General_SocketPort2                      := XMLConfig.General.SocketPort2.Value ;
   TraceConfig.General_Udp1                             := XMLConfig.General.Udp1.Value ;
   TraceConfig.General_Udp2                             := XMLConfig.General.Udp2.Value ;
   TraceConfig.General_HTTPPort                         := XMLConfig.General.HTTPPort.Value ;
   TraceConfig.General_ShowSocketWarning                := XMLConfig.General.ShowSocketWarning.Value ;
   TraceConfig.General_SocketPolicyServer               := XMLConfig.General.SocketPolicyServer.Value ;
   TraceConfig.General_HttpPolicyServer                 := XMLConfig.General.HttpPolicyServer.Value ;

   // display options
   // -----------------------------------------------------------------------

   TraceConfig.AppDisplay_SmallBut                      := XMLConfig.AppDisplay.SmallBut.Value ;
   TraceConfig.AppDisplay_ToolbarStandard               := XMLConfig.AppDisplay.ToolbarStandard.Value ;
   TraceConfig.AppDisplay_ToolbarSearch                 := XMLConfig.AppDisplay.ToolbarSearch.Value   ;
   TraceConfig.AppDisplay_ToolbarBookmark               := XMLConfig.AppDisplay.ToolbarBookmark.Value ;
   TraceConfig.AppDisplay_ToolbarFilter                 := XMLConfig.AppDisplay.ToolbarFilter.Value   ;
   TraceConfig.AppDisplay_HideViewer                    := XMLConfig.AppDisplay.HideViewer.Value      ;
   TraceConfig.AppDisplay_DisableInternalLog            := XMLConfig.AppDisplay.DisableInternalLog.Value ;
   TraceConfig.AppDisplay_ApplicationTitle              := XMLConfig.AppDisplay.ApplicationTitle.Value ;
   TraceConfig.AppDisplay_left                          := XMLConfig.AppDisplay.left.Value ;
   TraceConfig.AppDisplay_top                           := XMLConfig.AppDisplay.top.Value ;
   TraceConfig.AppDisplay_Width                         := XMLConfig.AppDisplay.width.Value ;
   TraceConfig.AppDisplay_Height                        := XMLConfig.AppDisplay.height.Value ;
   TraceConfig.AppDisplay_StayOnTop                     := XMLConfig.AppDisplay.stayOnTop.Value ;
   TraceConfig.AppDisplay_ShowOnstartup                 := XMLConfig.AppDisplay.ShowOnstartup.Value ;
   TraceConfig.AppDisplay_ShowOnMessageReceived         := XMLConfig.AppDisplay.ShowOnMessageReceived.Value ;
   TraceConfig.AppDisplay_FocusToReceivedMessage        := XMLConfig.AppDisplay.FocusToReceivedMessage.Value ;
   TraceConfig.AppDisplay_Maximized                     := XMLConfig.AppDisplay.Maximized.Value ;
   TraceConfig.AppDisplay_IconFile                      := XMLConfig.AppDisplay.IconFile.Value ;
   TraceConfig.AppDisplay_MinimizeToSystray             := XMLConfig.AppDisplay.MinimizeToSystray.Value ;

   // Trace Framework
   // -----------------------------------------------------------------------

   TraceConfig.Framework_ShowMembers                    := XMLConfig.Framework.ShowMembers.Value ;
   TraceConfig.Framework_AutoClear                      := XMLConfig.Framework.AutoClear.Value ;
   TraceConfig.Framework_MaxNode                        := XMLConfig.Framework.MaxNode.Value ;
   TraceConfig.Framework_MinNode                        := XMLConfig.Framework.MinNode.Value ;
   TraceConfig.Framework_Enabled                        := XMLConfig.Framework.Enabled.Value ;
   TraceConfig.Framework_VisibleMenu                    := XMLConfig.Framework.VisibleMenu.value ;
   TraceConfig.Framework_MainTraceTitle                 := XMLConfig.Framework.MainTraceTitle.value ;

   TraceConfig.Framework_Trace_FontName                 := XMLConfig.Framework.Trace.FontName.Value ;
   TraceConfig.Framework_Trace_FontSize                 := XMLConfig.Framework.Trace.FontSize.Value ;
   TraceConfig.Framework_Trace_NodeHeight               := XMLConfig.Framework.Trace.NodeHeight.Value ;

   TraceConfig.Framework_Info_FontName                  := XMLConfig.Framework.Info.FontName.Value ;
   TraceConfig.Framework_Info_FontSize                  := XMLConfig.Framework.Info.FontSize.Value ;
   TraceConfig.Framework_Info_NodeHeight                := XMLConfig.Framework.Info.NodeHeight.Value ;

   TraceConfig.Framework_Orphans_DeletedNode            := XMLConfig.Framework.Orphans.DeletedNode.Value ;
   TraceConfig.Framework_Orphans_DefaultLeftText        := XMLConfig.Framework.Orphans.DefaultLeftText.Value ;
   TraceConfig.Framework_Orphans_DefaultRightText       := XMLConfig.Framework.Orphans.DefaultRightText.Value ;
   TraceConfig.Framework_Orphans_LostAndFoundLeftText   := XMLConfig.Framework.Orphans.LostAndFoundLeftText.Value ;
   TraceConfig.Framework_Orphans_LostAndFoundRightText  := XMLConfig.Framework.Orphans.LostAndFoundRightText.Value ;

   // Watches
   // -----------------------------------------------------------------------

   TraceConfig.Watches_Enabled                          := XMLConfig.Watches.Enabled.Value ;
   TraceConfig.Watches_VisibleMenu                      := XMLConfig.Watches.VisibleMenu.value ;
   TraceConfig.Watches_MainWatchesTitle                 := XMLConfig.Watches.MainWatchesTitle.value ;

   TraceConfig.Watches_Trace_FontName                   := XMLConfig.Watches.Trace.FontName.Value ;
   TraceConfig.Watches_Trace_FontSize                   := XMLConfig.Watches.Trace.FontSize.Value ;
   TraceConfig.Watches_Trace_NodeHeight                 := XMLConfig.Watches.Trace.NodeHeight.Value ;

   TraceConfig.Watches_Info_FontName                    := XMLConfig.Watches.Info.FontName.Value ;
   TraceConfig.Watches_Info_FontSize                    := XMLConfig.Watches.Info.FontSize.Value ;

   // outputdebugString options
   // -----------------------------------------------------------------------

   TraceConfig.ods_Enabled                              := XMLConfig.ods.Enabled.Value ;
   TraceConfig.Ods_Title                                := XMLConfig.ods.Title.Value ;
   TraceConfig.Ods_AutoClear                            := XMLConfig.ods.AutoClear.Value ;
   TraceConfig.Ods_MaxNode                              := XMLConfig.ods.MaxNode.Value ;
   TraceConfig.Ods_MinNode                              := XMLConfig.ods.MinNode.Value ;
   TraceConfig.Ods_VisibleMenu                          := XMLConfig.ods.VisibleMenu.value ;

   TraceConfig.Ods_Trace_FontName                       := XMLConfig.ods.Trace.FontName.Value ;
   TraceConfig.Ods_Trace_FontSize                       := XMLConfig.ods.Trace.FontSize.Value ;
   TraceConfig.Ods_Trace_NodeHeight                     := XMLConfig.ods.Trace.NodeHeight.Value ;

   TraceConfig.Ods_Info_FontName                        := XMLConfig.ods.Info.FontName.Value ;
   TraceConfig.Ods_Info_FontSize                        := XMLConfig.ods.Info.FontSize.Value ;

   // event Log options
   //------------------------------------------------------------------------

   TraceConfig.EventLog_VisibleMenu                     := XMLConfig.EventLog.VisibleMenu.value ;
   TraceConfig.EventLog_Trace_FontName                  := XMLConfig.EventLog.Trace.FontName.Value ;
   TraceConfig.EventLog_Trace_FontSize                  := XMLConfig.EventLog.Trace.FontSize.Value ;
   TraceConfig.EventLog_Trace_NodeHeight                := XMLConfig.EventLog.Trace.NodeHeight.Value ;

   TraceConfig.EventLog_Info_FontName                   := XMLConfig.EventLog.Info.FontName.Value ;
   TraceConfig.EventLog_Info_FontSize                   := XMLConfig.EventLog.Info.FontSize.Value ;

   // tail options
   // -----------------------------------------------------------------------

   TraceConfig.Tail_LastPath                            := XMLConfig.Tail.LastPath.Value ;
   TraceConfig.Tail_AutoClear                           := XMLConfig.tail.AutoClear.Value ;
   TraceConfig.Tail_MaxNode                             := XMLConfig.tail.MaxNode.Value ;
   TraceConfig.Tail_MinNode                             := XMLConfig.tail.MinNode.Value ;
   TraceConfig.tail_ColumnStyle                         := XMLConfig.tail.ColumnStyle.value ;
   TraceConfig.tail_AutoCreateColStyle                  := XMLConfig.tail.AutoCreateColStyle.value ;
   TraceConfig.tail_TextQualifier                       := XMLConfig.tail.TextQualifier.value ;
   TraceConfig.tail_Separator                           := XMLConfig.tail.Separator.value ;
   TraceConfig.tail_FirstcolIsTitle                     := XMLConfig.tail.FirstcolIsTitle.value ;
   TraceConfig.tail_FixedColCount                       := XMLConfig.tail.FixedColCount.value ;
   TraceConfig.tail_OpenFromFavorites                   := XMLConfig.tail.OpenFromFavorites.value ;
   TraceConfig.tail_SizeToLoad                          := XMLConfig.tail.SizeToLoad.value  ;
   TraceConfig.Tail_VisibleMenu                         := XMLConfig.tail.VisibleMenu.value ;

   TraceConfig.Tail_Trace_FontName                      := XMLConfig.tail.Trace.FontName.Value ;
   TraceConfig.Tail_Trace_FontSize                      := XMLConfig.tail.Trace.FontSize.Value ;
   TraceConfig.Tail_Trace_NodeHeight                    := XMLConfig.tail.Trace.NodeHeight.Value ;

   TraceConfig.Tail_Info_FontName                       := XMLConfig.tail.Info.FontName.Value ;
   TraceConfig.Tail_Info_FontSize                       := XMLConfig.tail.Info.FontSize.Value ;

   // clipboard options
   // -----------------------------------------------------------------------

   TraceConfig.TextExport_ProcessName                   := XMLConfig.TextExport.ProcessName.Value ;
   TraceConfig.TextExport_ThreadId                      := XMLConfig.TextExport.ThreadID.Value ;
   TraceConfig.TextExport_Time                          := XMLConfig.TextExport.Time.Value ;
   TraceConfig.TextExport_Col1                          := XMLConfig.TextExport.Col1.Value ;
   TraceConfig.TextExport_Col2                          := XMLConfig.TextExport.Col2.Value ;
   TraceConfig.TextExport_GenerateColumnHeader          := XMLConfig.TextExport.GenerateColumnHeader.Value ;
   TraceConfig.TextExport_TreeIndentation               := XMLConfig.TextExport.TreeIndentation.Value ;

   if strtointDef(XMLConfig.TextExport.Separator.Value,-1) = -1 then
      TraceConfig.TextExport_Separator := XMLConfig.TextExport.Separator.Value
   else
      TraceConfig.TextExport_Separator := chr (StrToIntDef(XMLConfig.TextExport.Separator.Value, 9)) ;

   if XMLConfig.TextExport.TextQualifier.Value = 'None'   then TraceConfig.TextExport_TextQualifier := '' ;
   if XMLConfig.TextExport.TextQualifier.Value = 'Single' then TraceConfig.TextExport_TextQualifier := '''' ;
   if XMLConfig.TextExport.TextQualifier.Value = 'Double' then TraceConfig.TextExport_TextQualifier := '"' ;

   // plugins
   // ----------------------------------

   for c := 0 to XMLConfig.Plugins.Plugin.Count-1 do begin
      xmlPlugin := XMLConfig.Plugins.Plugin[c] ;
      plugKind := xmlPlugin.Kind ;
      plugin := nil;
      if stricomp (pchar(plugKind) , 'Win32') = 0 then begin
         Win32plugin := TWin32Plugin.create ;
         plugin := Win32plugin;
      end else if stricomp (pchar(plugKind) , 'DotNet') = 0 then begin
         DotNetPlugin := TDotNetPlugin.create ;
         plugin := DotNetPlugin ;
      end;

      plugin.plugKind  := pchar(plugKind) ;
      plugin.FileName  := AnsiString(xmlPlugin.FileName) ;
      plugin.param     := AnsiString(xmlPlugin.param) ;
      plugin.startup   := xmlPlugin.Enabled.Value ;
      TraceConfig.PluginList.add (plugin) ;
    end ;

    // Favorites
    // -------------------------------------

   for c := 0 to XMLConfig.Tail.Favorites.Count-1 do
      TraceConfig.FavoriteTailList.add(XMLConfig.Tail.Favorites.FileName[c]) ;

end ;

//------------------------------------------------------------------------------

procedure TFrm_Tool.SaveSettings();
var
   PosAndSize : PWindowPlacement;
   XMLConfig : IXMLConfig ;
   xmlPlugin : IXmlPlugin ;
   c : Integer ;
   plugin: TPlugin;
begin

   try
      XMLConfig := LoadTracetoolConfig() ;

      //XMLConfig := Config.LoadConfig('TraceTool.xml')  ;

      // if the window state is maximized, Delphi return negative values for the Form positions and size.
      // the GetWindowPlacement function return the correct windows position.
      GetMem(PosAndSize,SizeOf(TWindowPlacement));
      try
         PosAndSize^.Length := SizeOf(TWindowPlacement);
         if GetWindowPlacement(Frm_Tool.Handle,PosAndSize) then begin
            XMLConfig.AppDisplay.left.Value := PosAndSize^.rcNormalPosition.Left;
            XMLConfig.AppDisplay.top.Value := PosAndSize^.rcNormalPosition.Top;
            XMLConfig.AppDisplay.width.Value := PosAndSize^.rcNormalPosition.Right - PosAndSize^.rcNormalPosition.Left  ;
            XMLConfig.AppDisplay.height.Value := PosAndSize^.rcNormalPosition.Bottom - PosAndSize^.rcNormalPosition.Top ;
            XMLConfig.AppDisplay.Maximized.Value  := (Frm_Tool.WindowState = wsMaximized) ;

            //if PosAndSize^.ShowCmd = SW_SHOWNORMAL then
            //   ...
            //else if PosAndSize^.ShowCmd = SW_SHOWMINIMIZED then
            //   ...
            //else if PosAndSize^.ShowCmd = SW_SHOWMAXIMIZED then
            //   ...
         end;

         //if GetWindowPlacement(Application.Handle,PosAndSize) then  begin
         //   if PosAndSize^.ShowCmd = SW_SHOWNORMAL then
         //      ...
         //   else if PosAndSize^.ShowCmd = SW_SHOWMINIMIZED then
         //      ...
         //   else if PosAndSize^.ShowCmd = SW_SHOWMAXIMIZED then
         //      ...
         //end;
      finally
         FreeMem(PosAndSize,SizeOf(TWindowPlacement))
      end;

      XMLConfig.AppDisplay.StayOnTop.Value  := Frm_Tool.stayOnTop ;

      if Frm_Trace <> nil then      
         XMLConfig.Framework.ShowMembers.Value := Frm_Trace.PanelRight.Visible ;     // flag for main Frm_Trace form

      // General options
      // -----------------------------------------------------------------------

      XMLConfig.General.LastStyleSheet.Value                  := TraceConfig.General_LastStyleSheet ;
      XMLConfig.General.LastSavedPath.Value                   := TraceConfig.General_LastSavedPath ;
      XMLConfig.General.InternalLog.Value                     := TraceConfig.General_InternalLog ;
      XMLConfig.General.SocketPort.Value                      := TraceConfig.General_SocketPort ;
      XMLConfig.General.SocketPort2.Value                     := TraceConfig.General_SocketPort2 ;
      XMLConfig.General.Udp1.Value                            := TraceConfig.General_Udp1 ;
      XMLConfig.General.Udp2.Value                            := TraceConfig.General_Udp2 ;
      XMLConfig.General.HTTPPort.Value                        := TraceConfig.General_HTTPPort ;
      XMLConfig.General.ShowSocketWarning.Value               := TraceConfig.General_ShowSocketWarning ;
      XMLConfig.General.SocketPolicyServer.Value              := TraceConfig.General_SocketPolicyServer ;
      XMLConfig.General.HttpPolicyServer.Value                := TraceConfig.General_HttpPolicyServer ;

      // display options
      // -----------------------------------------------------------------------

      XMLConfig.AppDisplay.SmallBut.Value                     := TraceConfig.AppDisplay_SmallBut ;
      XMLConfig.AppDisplay.ToolbarStandard.Value              := TraceConfig.AppDisplay_ToolbarStandard ;
      XMLConfig.AppDisplay.ToolbarSearch.Value                := TraceConfig.AppDisplay_ToolbarSearch ;
      XMLConfig.AppDisplay.ToolbarBookmark.Value              := TraceConfig.AppDisplay_ToolbarBookmark ;
      XMLConfig.AppDisplay.ToolbarFilter.Value                := TraceConfig.AppDisplay_ToolbarFilter ;
      XMLConfig.AppDisplay.HideViewer.Value                   := TraceConfig.AppDisplay_HideViewer ;
      XMLConfig.AppDisplay.DisableInternalLog.Value           := TraceConfig.AppDisplay_DisableInternalLog ;
      XMLConfig.AppDisplay.ApplicationTitle.Value             := TraceConfig.AppDisplay_ApplicationTitle ;
      //XMLConfig.AppDisplay.left.Value                         := TraceConfig.AppDisplay_left ;
      //XMLConfig.AppDisplay.top.Value                          := TraceConfig.AppDisplay_top ;
      //XMLConfig.AppDisplay.width.Value                        := TraceConfig.AppDisplay_Width ;
      //XMLConfig.AppDisplay.height.Value                       := TraceConfig.AppDisplay_Height ;
      //XMLConfig.AppDisplay.stayOnTop.Value                    := TraceConfig.AppDisplay_StayOnTop ;
      XMLConfig.AppDisplay.ShowOnstartup.Value                := TraceConfig.AppDisplay_ShowOnstartup ;
      XMLConfig.AppDisplay.ShowOnMessageReceived.Value        := TraceConfig.AppDisplay_ShowOnMessageReceived ;
      XMLConfig.AppDisplay.FocusToReceivedMessage.Value       := TraceConfig.AppDisplay_FocusToReceivedMessage ;
      //XMLConfig.AppDisplay.Maximized.Value                    := TraceConfig.AppDisplay_Maximized ;
      XMLConfig.AppDisplay.IconFile.Value                     := TraceConfig.AppDisplay_IconFile ;
      XMLConfig.AppDisplay.MinimizeToSystray.Value            := TraceConfig.AppDisplay_MinimizeToSystray ;

      // Trace Framework
      // -----------------------------------------------------------------------

      //XMLConfig.Framework.ShowMembers.Value                   := TraceConfig.Framework_ShowMembers ;
      XMLConfig.Framework.AutoClear.Value                     := TraceConfig.Framework_AutoClear ;
      XMLConfig.Framework.MaxNode.Value                       := TraceConfig.Framework_MaxNode ;
      XMLConfig.Framework.MinNode.Value                       := TraceConfig.Framework_MinNode ;
      XMLConfig.Framework.Enabled.Value                       := TraceConfig.Framework_Enabled ;
      XMLConfig.Framework.VisibleMenu.value                   := TraceConfig.Framework_VisibleMenu ;
      XMLConfig.Framework.MainTraceTitle.value                := TraceConfig.Framework_MainTraceTitle ;

      XMLConfig.Framework.Trace.FontName.Value                := TraceConfig.Framework_Trace_FontName ;
      XMLConfig.Framework.Trace.FontSize.Value                := TraceConfig.Framework_Trace_FontSize ;
      XMLConfig.Framework.Trace.NodeHeight.Value              := TraceConfig.Framework_Trace_NodeHeight ;

      XMLConfig.Framework.Info.FontName.Value                 := TraceConfig.Framework_Info_FontName ;
      XMLConfig.Framework.Info.FontSize.Value                 := TraceConfig.Framework_Info_FontSize ;
      XMLConfig.Framework.Info.NodeHeight.Value               := TraceConfig.Framework_Info_NodeHeight ;

      XMLConfig.Framework.Orphans.DeletedNode.Value           := TraceConfig.Framework_Orphans_DeletedNode ;
      XMLConfig.Framework.Orphans.DefaultLeftText.Value       := TraceConfig.Framework_Orphans_DefaultLeftText ;
      XMLConfig.Framework.Orphans.DefaultRightText.Value      := TraceConfig.Framework_Orphans_DefaultRightText ;
      XMLConfig.Framework.Orphans.LostAndFoundLeftText.Value  := TraceConfig.Framework_Orphans_LostAndFoundLeftText ;
      XMLConfig.Framework.Orphans.LostAndFoundRightText.Value := TraceConfig.Framework_Orphans_LostAndFoundRightText ;

      // Watches
      // -----------------------------------------------------------------------

      XMLConfig.Watches.Enabled.Value                         := TraceConfig.Watches_Enabled ;
      XMLConfig.Watches.VisibleMenu.value                     := TraceConfig.Watches_VisibleMenu ;
      XMLConfig.Watches.MainWatchesTitle.value                := TraceConfig.Watches_MainWatchesTitle ;

      XMLConfig.Watches.Trace.FontName.Value                  := TraceConfig.Watches_Trace_FontName ;
      XMLConfig.Watches.Trace.FontSize.Value                  := TraceConfig.Watches_Trace_FontSize ;
      XMLConfig.Watches.Trace.NodeHeight.Value                := TraceConfig.Watches_Trace_NodeHeight ;

      XMLConfig.Watches.Info.FontName.Value                   := TraceConfig.Watches_Info_FontName ;
      XMLConfig.Watches.Info.FontSize.Value                   := TraceConfig.Watches_Info_FontSize ;

      // outputdebugString options
      // -----------------------------------------------------------------------

      XMLConfig.ods.Enabled.Value                             := TraceConfig.ods_Enabled ;
      XMLConfig.ods.Title.Value                               := TraceConfig.Ods_Title ;
      XMLConfig.ods.AutoClear.Value                           := TraceConfig.Ods_AutoClear ;
      XMLConfig.ods.MaxNode.Value                             := TraceConfig.Ods_MaxNode ;
      XMLConfig.ods.MinNode.Value                             := TraceConfig.Ods_MinNode ;
      XMLConfig.ods.VisibleMenu.value                         := TraceConfig.Ods_VisibleMenu ;

      XMLConfig.ods.Trace.FontName.Value                      := TraceConfig.Ods_Trace_FontName ;
      XMLConfig.ods.Trace.FontSize.Value                      := TraceConfig.Ods_Trace_FontSize ;
      XMLConfig.ods.Trace.NodeHeight.Value                    := TraceConfig.Ods_Trace_NodeHeight ;

      XMLConfig.ods.Info.FontName.Value                       := TraceConfig.Ods_Info_FontName ;
      XMLConfig.ods.Info.FontSize.Value                       := TraceConfig.Ods_Info_FontSize ;

      // event Log options
      //------------------------------------------------------------------------

      XMLConfig.EventLog.VisibleMenu.value                    := TraceConfig.EventLog_VisibleMenu ;
      XMLConfig.EventLog.Trace.FontName.Value                 := TraceConfig.EventLog_Trace_FontName ;
      XMLConfig.EventLog.Trace.FontSize.Value                 := TraceConfig.EventLog_Trace_FontSize ;
      XMLConfig.EventLog.Trace.NodeHeight.Value               := TraceConfig.EventLog_Trace_NodeHeight ;

      XMLConfig.EventLog.Info.FontName.Value                  := TraceConfig.EventLog_Info_FontName ;
      XMLConfig.EventLog.Info.FontSize.Value                  := TraceConfig.EventLog_Info_FontSize ;

      // tail options
      // -----------------------------------------------------------------------

      XMLConfig.Tail.LastPath.Value                           := TraceConfig.Tail_LastPath ;
      XMLConfig.tail.AutoClear.Value                          := TraceConfig.Tail_AutoClear ;
      XMLConfig.tail.MaxNode.Value                            := TraceConfig.Tail_MaxNode ;
      XMLConfig.tail.MinNode.Value                            := TraceConfig.Tail_MinNode ;
      XMLConfig.tail.ColumnStyle.value                        := TraceConfig.tail_ColumnStyle ;
      XMLConfig.tail.AutoCreateColStyle.value                 := TraceConfig.tail_AutoCreateColStyle ;
      XMLConfig.tail.TextQualifier.value                      := TraceConfig.tail_TextQualifier ;
      XMLConfig.tail.Separator.value                          := TraceConfig.tail_Separator ;
      XMLConfig.tail.FirstcolIsTitle.value                    := TraceConfig.tail_FirstcolIsTitle ;
      XMLConfig.tail.FixedColCount.value                      := TraceConfig.tail_FixedColCount ;
      XMLConfig.tail.OpenFromFavorites.value                  := TraceConfig.tail_OpenFromFavorites ;
      XMLConfig.tail.SizeToLoad.value                         := TraceConfig.tail_SizeToLoad ;
      XMLConfig.tail.VisibleMenu.value                        := TraceConfig.Tail_VisibleMenu ;

      XMLConfig.tail.Trace.FontName.Value                     := TraceConfig.Tail_Trace_FontName ;
      XMLConfig.tail.Trace.FontSize.Value                     := TraceConfig.Tail_Trace_FontSize ;
      XMLConfig.tail.Trace.NodeHeight.Value                   := TraceConfig.Tail_Trace_NodeHeight ;

      XMLConfig.tail.Info.FontName.Value                      := TraceConfig.Tail_Info_FontName ;
      XMLConfig.tail.Info.FontSize.Value                      := TraceConfig.Tail_Info_FontSize ;

      // clipboard options
      // -----------------------------------------------------------------------

      XMLConfig.TextExport.ProcessName.Value                  := TraceConfig.TextExport_ProcessName ;
      XMLConfig.TextExport.ThreadID.Value                     := TraceConfig.TextExport_ThreadId ;
      XMLConfig.TextExport.Time.Value                         := TraceConfig.TextExport_Time ;
      XMLConfig.TextExport.Col1.Value                         := TraceConfig.TextExport_Col1 ;
      XMLConfig.TextExport.Col2.Value                         := TraceConfig.TextExport_Col2 ;
      XMLConfig.TextExport.GenerateColumnHeader.Value         := TraceConfig.TextExport_GenerateColumnHeader ;
      XMLConfig.TextExport.TreeIndentation.Value              := TraceConfig.TextExport_TreeIndentation ;
      XMLConfig.TextExport.Separator.Value                    := TraceConfig.TextExport_Separator ;

      if TraceConfig.TextExport_TextQualifier = ''   then XMLConfig.TextExport.TextQualifier.Value := 'None'  ;
      if TraceConfig.TextExport_TextQualifier = '''' then XMLConfig.TextExport.TextQualifier.Value := 'Single' ;
      if TraceConfig.TextExport_TextQualifier = '"'  then XMLConfig.TextExport.TextQualifier.Value := 'Double' ;

      // plugins
      // ----------------------------------

      XMLConfig.Plugins.Plugin.Clear() ;

      for c := 0 to TraceConfig.PluginList.Count - 1 do begin
         plugin := TPlugin(TraceConfig.PluginList.Items[c]);
         xmlPlugin := XMLConfig.Plugins.Plugin.Add ;
         if plugin is TWin32Plugin then
            xmlPlugin.Kind := 'Win32'
         else if plugin is TDotNetPlugin then
            xmlPlugin.Kind := 'DotNet' ;

         xmlPlugin.Enabled.Value := plugin.startup ;
         xmlPlugin.FileName := string(plugin.FileName) ;
         xmlPlugin.Param := string(plugin.param) ;
         //xmlPlugin.ClassName => for java, no more needed
         xmlPlugin := nil ;
      end;

       // Favorites
       // -------------------------------------

      XMLConfig.Tail.Favorites.Clear() ;

      for c := 0 to TraceConfig.FavoriteTailList.Count-1 do
         XMLConfig.tail.Favorites.Add(TraceConfig.FavoriteTailList[c] ) ;

      SaveTracetoolConfig(XMLConfig);

      //XMLConfig.OwnerDocument.XML.SaveToFile(strConfigFile);
      //XMLConfig.OwnerDocument.SaveToFile(strConfigFile);
      //TXMLDocument(XMLConfig).Active := False ;

      //XMLConfig.text.
      XMLConfig := nil ;
   except
      on e: exception do begin
         LowTrace('TfrmDebugOptions.SaveSettings : ' + e.Message );
      end ;
   end ;

end;

//------------------------------------------------------------------------------
// The shell creates a button on the taskbar whenever an application creates a window that isn't owned.
// To ensure that the window button is placed on the taskbar, create an unowned window with the WS_EX_APPWINDOW
// extended style. To prevent the window button from being placed on the taskbar, create the unowned window
// with the WS_EX_TOOLWINDOW extended style. As an alternative, you can create a hidden window and make this
// hidden window the owner of your visible window.

//The shell will remove a window's button from the taskbar only if the window's style supports
//visible taskbar buttons. If you want to dynamically change a window's style to one that doesn't
//support visible taskbar buttons, you must hide the window first (by calling ShowWindow with SW_HIDE),
//change the window style, and then show the window.

//The window button typically contains the application icon and title.
//However, if the application does not contain a system menu, the window button is created without the icon.

procedure TFrm_Tool.TaskBarButton(Visible:Boolean);
begin
   ShowWindow(Application.Handle, SW_HIDE);
   if Visible then begin
      SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) and not(WS_EX_TOOLWINDOW));
      ShowWindow(Application.Handle, SW_SHOW);
   end else begin
      SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
   end;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FAllowClose;
  if not CanClose then begin
    TaskBarButton (false) ;
    Hide;
  end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.WMEndSession(var Message: TMessage);
begin
  FAllowClose := True;
  Close;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.WMQueryEndSession(var Message: TMessage);
begin
  FTaskIcon.Active := False;
  FAllowClose := True;
  Close;
  inherited;
end;

//------------------------------------------------------------------------------

// never goes here
procedure TFrm_Tool.CMDockClient(var Message: TCMDockClient);
var
  DockCtl: TControl;
begin
    LowTrace('CMDockClient');
    DockCtl := Message.DockSource.Control;
    LowTrace(DockCtl.Name) ;
    LowTrace(DockCtl.ClassName) ;
end;

//------------------------------------------------------------------------------

// message are delayed in the MessageStack (TObjectList)
// the timer call the performCopyData for each messages.

// critical section is needed, because the list can be writen by some sockets threads
// performCopyData will call virtualTreeView that also check the main message loop.
// the Timer1Timer can then by called recursively if we don't set timer1.enable to false

procedure TFrm_Tool.TimerTracesTimer(Sender: TObject);
var
   MsgList : TStringList ;
   c : integer ;
   FrmTrace : TFrm_Trace ;
   TempStack : TObjectList ;
begin

   TimerTraces.Enabled := false ;   // enabled when the MessageStack is filled (socket or win msg)
   if csDestroying in ComponentState then 
      exit ;                              

   // Disable all ttraces window
   for c := 0 to FormTraceList.Count-1 do begin
      FrmTrace := TFrm_Trace (FormTraceList.Items[c]) ;
      FrmTrace.vstTrace.BeginUpdate ;
      FrmTrace.NodeToFocus := nil ;
   end ;

   try

      while true do begin
         //Frm_Trace.InternalTraceFromThread('TimerTracesTimer start') ;
         criticalsection.Enter ;
         try
            // swap the message list to limit critical section lock time
            TempStack := MessageStack ;
            MessageStack := SwapStack ;
            SwapStack := TempStack ;
         finally
            criticalsection.Leave ;
         end;
         //Frm_Trace.InternalTraceFromThread('TimerTracesTimer end') ;

         if SwapStack.Count = 0 then
            break ;

         // parse all messages in SwapStack
         while true do begin
            // get the first element in the MessageStack
            if SwapStack.Count = 0 then
               break ;       // no more element : berak inner loop. Maybee new messages are waiting in MessageStack

            MsgList := TStringList( SwapStack.Items[0] ) ;
            SwapStack.Delete(0);  // MessageStack/SwapStack  don't own the objects

            if MsgList = nil then
               break ;  // quit the loop

            // parsing is done in unt_traceWin unit
            ParseTraceMsg(MsgList);
            MsgList.Free ;             // free the MessageList only when finished (allocated by WMCopyData or socket receiver)

         end ;

      end ;   // loop

   finally

      // reEnable all ttraces window , check AutoClear if changed and set focused node
      for c := 0 to FormTraceList.Count-1 do begin
         FrmTrace := TFrm_Trace (FormTraceList.Items[c]) ;
         // check the number of node
         // to do : only if changed (add a flag)
         FrmTrace.CheckAutoClear () ;
         if FrmTrace.NodeToFocus <> nil then
            FrmTrace.VstTrace.FocusedNode := FrmTrace.NodeToFocus ;

         FrmTrace.vstTrace.EndUpdate ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------
// display various statictical info
procedure TFrm_Tool.TimerStatusTimer(Sender: TObject);
var
   c : integer ;
   FrmBase : TFrmBase ;
begin
   if PluginsInitialized = false then begin
      if FindWindow('TFormReceiver', 'FormReceiver') <> 0 then begin
         PluginsInitialized := true ;
         frmDebugOptions.InitPlugins ();  // must be called after InternalTrace is created
      end ;
   end ;

   try
      if frm_tool = nil then
         halt ;
      if csDestroying in ComponentState then begin
         TimerStatus.enabled := false ;
         exit ;
      end ;
      // Received is incremented by TFrm_Trace.ParseTraceMsg
      statusbar.Panels[STATUS_received].Text := inttostr (Received);
      statusbar.Panels[STATUS_Socket].Text   := inttostr (TCPServer.Tag) ;

      TFrm_Trace.checkInternals() ;

      // let the window display stat info every second
      for c := 0 to BaseList.Count -1 do begin
         FrmBase := TFrmBase (BaseList[c]) ;
         FrmBase.TimerInfo ;
      end ;
   except
      on e : exception do
         TFrm_Trace.InternalTrace('TimerStatus exception : ' + e.Message) ;
   end ; 
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.TrayIconDblClick(Sender: TObject);
begin
   actShow.Execute;
end;


//------------------------------------------------------------------------------

// TaskBar / Show or double click (TrayIconDblClick) or ParseTraceMsg
procedure TFrm_Tool.actShowExecute(Sender: TObject);
begin
   if TraceConfig.AppDisplay_HideViewer = true then
      exit ;
   Application.ShowMainForm := true ;
   TaskBarButton(true) ;
   Application.Restore ;
   Show;
   BringToFront;
   Application.BringToFront;
end;

//------------------------------------------------------------------------------
// Called by File/hide and ParseTraceMsg
procedure TFrm_Tool.actHideExecute(Sender: TObject);
begin
   Application.ShowMainForm := false ;
   TaskBarButton (false) ;
   Hide ;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.actShutdownExecute(Sender: TObject);
var
   c : integer ;
   List : TObjectList ;
begin
   TimerStatus.Enabled := false ;
   TimerTraces.Enabled := false ;  

   // terminate active socket connection don't work when calling "TCPServer.Active := false"
   // the TCPServerListener never finish.
   // Best way is to close all active connection manually

   with TCPServer.Contexts.LockList do
   try
      for c := 0 to Count - 1 do begin
         // Dont call disconnect with true. Otheriwse it frees the IOHandler and the thread
         // is still running which often causes AVs and other.
         TIdContext(Items[c]).Connection.Disconnect(False);
      end;
   finally
      TCPServer.Contexts.UnLockList;
   end;
   with TCPServer2.Contexts.LockList do
   try
      for c := 0 to Count - 1 do begin
         // Dont call disconnect with true. Otheriwse it frees the IOHandler and the thread
         // is still running which often causes AVs and other.
         TIdContext(Items[c]).Connection.Disconnect(False);
      end;
   finally
      TCPServer2.Contexts.UnLockList;
   end;
   application.ProcessMessages ;
   UDPServer1.Active := false ;
   UDPServer2.Active := false ;
   TCPServer.Active := false ;
   TCPServer2.Active := false ;
   SocketPolicyServer.active := false ;

   // closeWin is implemented in 2 ways : just hide the window or remove it from the pagecontrol
   // we cannot then iterate over the Pagecontrol using a for loop.

   // first duplicate the TFrmBase list
   list := TObjectList.create (false) ;
   list.Assign(BaseList);

   // then iterate the list
   for c := 0 to list.Count-1 do
      TFrmBase (list.Items[c]).CloseWin ;

   list.Clear ;
   List.Free ;
   FAllowClose := True;

   SaveSettings() ;
   Close;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.actAboutExecute(Sender: TObject);
begin
   FrmAbout.ShowModal ;
end;

//------------------------------------------------------------------------------


procedure TFrm_Tool.SetStayOnTop(const Value: Boolean);
var
   c : integer ;
   FrmPageContainer : TFrmPageContainer ;
begin
   FStayOnTop := Value;
   if Value then
      SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE)
   else
      SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);

   for c := 0 to ContainerList.Count-1 do begin
      FrmPageContainer := TFrmPageContainer(ContainerList[c]) ;
      if FrmPageContainer <> MainPageContainer then begin
         if Value then
            SetWindowPos(FrmPageContainer.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE)
         else
            SetWindowPos(FrmPageContainer.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);

      end ;

   end ;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.actViewStayOnTopExecute(Sender: TObject);
begin
  StayOnTop := not StayOnTop;
  actViewStayOnTop.checked := StayOnTop ;
end;

//------------------------------------------------------------------------------

// Called from Main menu
procedure TFrm_Tool.actOptionsExecute(Sender: TObject);
var
   c : integer ;
begin
   frmDebugOptions.FillPlugins() ;
   frmDebugOptions.VSTOptionsChange (frmDebugOptions.VSTOptions, frmDebugOptions.VSTOptions.GetFirst) ;
   frmDebugOptions.ShowCurrentConfig () ;

   frmDebugOptions.VSTOptions.Selected [frmDebugOptions.VSTOptions.GetFirst] := true ;
   frmDebugOptions.VSTOptionsChange (frmDebugOptions.VSTOptions, frmDebugOptions.VSTOptions.GetFirst) ;
   if frmDebugOptions.ShowModal <> mrOk then
      exit ;

   if (TraceConfig.General_SocketPort <> TCPServer.DefaultPort) or
      (TCPServer.Active = TraceConfig.General_Udp1) or
      (UdpServer1.Active <> TraceConfig.General_Udp1) then begin
     try
        UDPServer1.Active := false ;
        TCPServer.Active := false ;
        TCPServer.DefaultPort := TraceConfig.General_SocketPort ;
        TCPServer.Bindings.clear() ;
        TCPServer.Bindings.add() ;
        if TCPServer.DefaultPort <> 0 then
            if TraceConfig.General_Udp1 then
               UdpServer1.Active := true
            else
               TCPServer.Active := true ;
     except
        on e : exception do begin
           InitError := e.Message ;
           FAllowClose := true;
        end ;
     end ;
   end ;

   if (TraceConfig.General_SocketPort2 <> TCPServer2.DefaultPort) or
      (TCPServer2.Active = TraceConfig.General_Udp2) or
      (UdpServer2.Active <> TraceConfig.General_Udp2) then begin
     try
        UDPServer2.Active := false ;
        TCPServer2.Active := false ;
        TCPServer2.DefaultPort := TraceConfig.General_SocketPort2 ;
        TCPServer2.Bindings.clear() ;
        TCPServer2.Bindings.add() ;
        if TCPServer2.DefaultPort <> 0 then
           if TraceConfig.General_Udp2 then
               UdpServer2.Active := true
            else
               TCPServer2.Active := true ;
     except
        on e : exception do begin
           InitError := e.Message ;
           FAllowClose := true;
        end ;
     end ;
   end ;

   if TraceConfig.General_HTTPPort <> IdHTTPServer.DefaultPort then begin
     try
        IdHTTPServer.Active := false ;
        IdHTTPServer.DefaultPort := TraceConfig.General_HTTPPort ;
        IdHTTPServer.Bindings.clear() ;
        IdHTTPServer.Bindings.add() ;
        if IdHTTPServer.DefaultPort <> 0 then
           IdHTTPServer.Active := true ;
     except
        on e : exception do begin
           InitError := e.Message ;
           FAllowClose := true;
        end ;
     end ;
   end ;

   if TraceConfig.General_SocketPolicyServer <> SocketPolicyServer.active then begin
     try
        SocketPolicyServer.Active := false ;
        SocketPolicyServer.Bindings.clear() ;
        SocketPolicyServer.Bindings.add() ;
        if SocketPolicyServer.DefaultPort <> 0 then
           SocketPolicyServer.Active := true ;
     except
        on e : exception do begin
           InitError := e.Message ;
           FAllowClose := true;
        end ;
     end ;
   end;

   // check auto clear ...
   for c := 0 to BaseList.Count-1 do
      TFrmBase (BaseList.Items[c]).CheckAutoClear ;

end;


//------------------------------------------------------------------------------
// MAIN TRACE
//------------------------------------------------------------------------------

procedure TFrm_Tool.actViewMainTracesExecute(Sender: TObject);
var
   PageControl : TPageControl ;
begin
   Frm_Trace.Visible := true ;
   if Frm_Trace.Parent = nil then
      Frm_Trace.DockToMainPanel() ;

   TraceConfig.Framework_Enabled := true ;
   PageControl := Frm_Trace.getPageControl ;
   if PageControl = nil then     // should not happens
      exit ;
   Frm_Trace.SetActivePage() ;
   PageControl.OnChange(nil) ;
end;

//------------------------------------------------------------------------------
// MAIN WATCHES
//------------------------------------------------------------------------------

procedure TFrm_Tool.actViewMainWatchExecute(Sender: TObject);
var
   PageControl : TPageControl ;
begin
   Frm_Watches.Visible := true ;
   if Frm_Watches.Parent = nil then
      Frm_Watches.DockToMainPanel() ;

   TraceConfig.Watches_Enabled := true ;
   PageControl := Frm_Watches.getPageControl ;
   if PageControl = nil then     // should not happens
      exit ;
   Frm_Watches.SetActivePage() ;
   PageControl.OnChange(nil) ;
end;

//------------------------------------------------------------------------------
// ODS
//------------------------------------------------------------------------------

procedure TFrm_Tool.actViewOutputDebugStringExecute(Sender: TObject);
var
   PageControl : TPageControl ;
begin
   Frm_ODS.Visible := true ;
   if Frm_ODS.Parent = nil then
      Frm_ODS.DockToMainPanel() ;
   Frm_ODS.getPageContainer().actPause.Checked := false ;

   Frm_ODS.PauseWin ;  // force ConfigInfo.EnableODS and activate thread
   PageControl := Frm_ODS.getPageControl ;
   if PageControl = nil then     // should not happens
      exit ;
   Frm_ODS.SetActivePage ;
   PageControl.OnChange(nil);
end;

//------------------------------------------------------------------------------
// WIN Tail
//------------------------------------------------------------------------------

procedure TFrm_Tool.actTailWinExecute(Sender: TObject);
begin
   FrmSelectTail.initOptions ;
   FrmSelectTail.ShowModal ;
   Show ;
end;

//------------------------------------------------------------------------------
// TraceTool from XML file
//------------------------------------------------------------------------------

procedure TFrm_Tool.actLoadXmlExecute(Sender: TObject);
var
   Trace : TFrm_Trace ;
begin
   OpenDialog1.Filter := 'Xml file (*.xml)|*.xml' ;
   OpenDialog1.InitialDir := TraceConfig.General_LastSavedPath ;
   if OpenDialog1.Execute = false then
      exit ;
   TraceConfig.General_LastSavedPath := ExtractFilePath(OpenDialog1.FileName) ;

   // create the trace form
   Application.CreateForm(TFrm_Trace, Trace);
   Trace.Caption := 'Trace::' + ExtractFileName (OpenDialog1.FileName) ;

   Trace.DockToMainPanel() ;
   Trace.SetActivePage() ;
   Trace.getPageControl.OnChange (nil) ;

   application.ProcessMessages ;
   SetCursor(Screen.Cursors[crHourGlass]);

   try
      Trace.LoadXML(OpenDialog1.FileName);
   finally
      SetCursor(Screen.Cursors[crDefault]);
   end ;
end;

//------------------------------------------------------------------------------
// Event Log
//------------------------------------------------------------------------------

procedure TFrm_Tool.actEventlogExecute(Sender: TObject);
var
   EVLog : TFrmEventLog;  // not (yet) stored in a list of event log forms
   c : integer ;
   RadioButton: TRadioButton;
begin
   FrmSelectEvent.ShowModal ;
   if FrmSelectEvent.ModalResult <> mrOk then
      exit ;

   // search for selected radiobutton
   for c := 0 to eventFiles.Count - 1 do begin
      RadioButton := TRadioButton (eventFiles.Objects [c]) ;
      if RadioButton.Checked  then begin
         // create the Event log form
         Application.CreateForm(TFrmEventLog, EVLog);      

         EVLog.Caption := 'EventLog::' + eventFiles.Strings[c] ;
         EVLog.DockToMainPanel() ;
         EVLog.Show ;
         EVLog.SetActivePage() ;

         EventForm.AddObject (eventFiles.Strings[c], EVLog) ;
         EVLog.SetEventLog (eventFiles.Strings[c],50) ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// ACTION MENU
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TFrm_Tool.HookSysCmd(var Message: TMessage);
begin
   if (TWMSYSCOMMAND(Message).CmdType and $FFF0 = SC_MINIMIZE) and (Application.MainForm = Self) then begin
      if TraceConfig.AppDisplay_MinimizeToSystray = true then
         close
      else
         inherited ;
   end else begin
      inherited ;
   end ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// this function is under of the control of the TIdPeerThread parameter, not by the main thread
// used by TCPServer and TCPServer2
procedure TFrm_Tool.TCPServerConnect(AContext: TIdContext);
begin
   //Frm_Trace.InternalTraceFromThread('connect tcpserver2');
   criticalsection.Enter ;
   try
      TCPServer.Tag := TCPServer.Tag + 1 ;   // Tag receive the number of connection on the 2 servers
      ConnectionList.Add(AContext.Connection) ;
   finally
      criticalsection.Leave ;
   end ;
   //Frm_Trace.InternalTraceFromThread('connect tcpserver2 done');
end;

//------------------------------------------------------------------------------

// this function is under of the control of the TIdPeerThread parameter, not by the main thread
// used by TCPServer and TCPServer2
procedure TFrm_Tool.TCPServerDisconnect(AContext: TIdContext);
begin
   criticalsection.Enter ;
   try
      TCPServer.Tag := TCPServer.Tag - 1 ;  // Tag receive the number of connection on the 2 servers
      ConnectionList.Delete(ConnectionList.IndexOf(AContext.Connection)) ;
   finally
      criticalsection.Leave ;
   end;
end;

//------------------------------------------------------------------------------

// this function is under of the control of the TIdPeerThread parameter, not by the main thread
// used by TCPServer and TCPServer2
procedure TFrm_Tool.TCPServerException(AContext: TIdContext; AException: Exception);
begin
   if TraceConfig.General_ShowSocketWarning then
      TFrm_Trace.InternalTraceFromThread(AException.Message) ;   // class method
end;

//------------------------------------------------------------------------------

// used by TCPServer and TCPServer2
procedure TFrm_Tool.TCPServerListenException(AThread: TIdListenerThread;
  AException: Exception);
begin
    TFrm_Trace.InternalTraceFromThread(AException.Message) ;   // class method
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.SocketPolicyServerAfterBind(Sender: TObject);
begin
//
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.SocketPolicyServerConnect(AContext: TIdContext);
begin
    //TFrm_Trace.InternalTraceFromThread('SocketPolicyServerConnect') ;
end;

procedure TFrm_Tool.SocketPolicyServerDisconnect(AContext: TIdContext);
begin
    //TFrm_Trace.InternalTraceFromThread('SocketPolicyServerDisconnect') ;
end;

procedure TFrm_Tool.SocketPolicyServerException(AContext: TIdContext; AException: Exception);
begin
    TFrm_Trace.InternalTraceFromThread('SocketPolicyServerException ' + AException.Message) ;
end;

procedure TFrm_Tool.SocketPolicyServerExecute(AContext: TIdContext);
var
   LBytes: TIdBytes;            // received buffer
   AnsiPtr : PAnsiChar ;      // pointer to the buffer (as ansiString)
   command : String ;
   FileToLoad : TStringList ;
   strFileToLoad : AnsiString ;
   c : integer ;
   SourcePtr , DestPtr : PAnsiChar ;
begin
   // read first byte to know the number of remaing bytes
   AContext.Connection.IOHandler.ReadBytes(LBytes, 1, {AAppend} False);

   // append remaining bytes
   AContext.Connection.IOHandler.ReadBytes(LBytes, AContext.Connection.IOHandler.InputBuffer.Size, {AAppend} True);

   // convert single byte (unterminated) string to unicode string
   command := '' ;
   AnsiPtr := PAnsiChar(LBytes) ;
   for c := 0 to Length(LBytes) - 1 do begin
      Command := command + Char(AnsiPtr^) ;
      inc(AnsiPtr) ;
   end;

   //if TraceConfig.DebugMode = true then
   //   TFrm_Trace.InternalTraceFromThread(command) ;

   // check command : <policy-file-request/>
   if command = '<policy-file-request/>'  then begin
      // write the file
      FileToLoad := TStringList.Create() ;
      strFileToLoad := 'clientaccesspolicy.xml' ;
      if FileExists(String(strFileToLoad)) then begin
         FileToLoad.LoadFromFile(String(strFileToLoad)) ;
         // todo : display the file in the debug windows if TraceConfig.DebugMode = true
      end else begin
         if TraceConfig.DebugMode = true then
            TFrm_Trace.InternalTraceFromThread ('clientaccesspolicy.xml not found');
      end;
      strFileToLoad := AnsiString(FileToLoad.Text) ;
      FileToLoad.Clear ;
      FileToLoad.Free ;

      // copy the xml response to the byte array  (single byte)
      SetLength(LBytes, Length(strFileToLoad));
      AnsiPtr := PAnsiChar(LBytes) ;
      SourcePtr := PAnsiChar(strFileToLoad) ;
      DestPtr   := PAnsiChar(AnsiPtr) ;
      for c := 0 to Length(strFileToLoad) - 1 do begin
         DestPtr^ := SourcePtr^ ;
         inc(DestPtr) ;
         inc(SourcePtr) ;
      end;

      AContext.Connection.IOHandler.Write(LBytes) ;
   end;
   AContext.Connection.IOHandler.Close ;
end;

//------------------------------------------------------------------------------

// this function is under of the control of the TCPServerScheduler Thread, not by the main thread
// used by TCPServer and TCPServer2
procedure TFrm_Tool.TCPServerExecute(AContext: TIdContext);
var
   pWordLength : pDword ;     // pointer to a DWORD containing the message length
   TempMsgLen : AnsiString ;  // temp buffer for the message lenght
   MsgLen : integer ;         // decoded message length
   errorLength : integer ;

   LBytes: TIdBytes;          // received buffer
   AnsiPtr : PAnsiChar ;      // pointer to the buffer (as ansiString)

begin
   try
      // The message start with an header : the WMD (123) code stored in a single byte folowed byte the number of bytes to read (DWORD) followed by the message
      // In previous version, the WND code is not present and the number was a string (coded as single byte), followed by a #0 then the message
      // Both case are supported
      //
      // If the message is unicode, he start with the UTF16 - unicode : FE FF  (stored in reverse order : FF FE)
      // If not it'a a single byte string

      // get first byte. If it's a char between '0' and '9', it's the old version
      //Frm_Trace.InternalTraceFromThread('Wait first byte');
      AContext.Connection.IOHandler.ReadBytes(LBytes, 1, {AAppend} False);
      AnsiPtr := pAnsiChar(LBytes) ;
      if (AnsiPtr^ >= '0') and (AnsiPtr^ <= '9') then begin    // 48 .. 57
         TempMsgLen := AnsiPtr^ ;
         while True do begin
            //Frm_Trace.InternalTraceFromThread('Wait next byte');
            AContext.Connection.IOHandler.ReadBytes(LBytes, 1, {AAppend} False);    // read 1 byte at a time : Ansi char
            if AnsiPtr^ = #0 then
               break ;
            TempMsgLen := TempMsgLen + AnsiPtr^ ;
         end;
         Val(string(TempMsgLen), MsgLen, errorLength);
         if errorLength <> 0 then begin
            TFrm_Trace.InternalTraceFromThread('TCP server buffer : Invalid message. Should be an integer : ' + String(TempMsgLen)) ;
            // flush the buffer
            AContext.Connection.IOHandler.ReadBytes(LBytes, AContext.Connection.IOHandler.InputBuffer.Size, {AAppend} False);
            exit ;
         end;
      end else if (ord(AnsiPtr^) = WMD) then begin
         //Frm_Trace.InternalTraceFromThread('Wait 4 bytes');
         AContext.Connection.IOHandler.ReadBytes(LBytes, 4, {AAppend} false);
         pWordLength := pDword (LBytes) ;
         MsgLen := pWordLength^ ;
      end else begin
         TFrm_Trace.InternalTraceFromThread('TCP server buffer : Invalid message. don''t start with a string number or by the WMD byte ') ;
         // flush the buffer
         AContext.Connection.IOHandler.ReadBytes(LBytes, AContext.Connection.IOHandler.InputBuffer.Size, {AAppend} False);
         exit ;
      end;

      // todo : check if MsgLen is <= AContext.Connection.IOHandler.InputBuffer.Size
      //Frm_Trace.InternalTraceFromThread('After header; Wait ' + inttostr(MsgLen) + ' bytes');
      AContext.Connection.IOHandler.ReadBytes(LBytes, MsgLen, {AAppend} False);
      SplitMessage (pAnsiChar(LBytes)) ;

      // TODO if the client need it : send a ACK
      // AContext.Connection.IOHandler.Write(???) ;

   except
      on E: EIdClosedSocket do             // connection closed by the server itself
         exit ;

      on E : EIdConnClosedGracefully do    // connection closed by the client
         exit ;

      on E : EIdSocketError do begin            // Socket Error # 10054 Connection reset by peer.
         if TraceConfig.General_ShowSocketWarning then
            TFrm_Trace.InternalTraceFromThread('TCPServer1Disconnect from ' + AContext.Connection.Socket.Binding.PeerIP + ':' + inttostr(AContext.Connection.Socket.Binding.PeerPort)) ;
         raise ;
      end ;

      on e : exception do begin
         TFrm_Trace.InternalTraceFromThread(e.ClassName + ' : ' + e.Message) ;
         raise ;
      end ;
   end ;

end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TFontDetail }


constructor TFontDetail.create (pColId : integer ; pBold : boolean ; pItalic : boolean ; pColor: TColor ; pSize: integer ; pName : string) ;
begin
   ColId  := pColId ;
   Bold   := pBold ;
   Italic := pItalic ;
   Color  := pColor ;
   Size   := pSize ;
   Name   := pName ;
   BackgroundColor := 0 ;
end;

//------------------------------------------------------------------------------

constructor TFontDetail.create(XmlFontDetail: IXMLFontDetail);
begin
   ColId  := XmlFontDetail.ColId ;
   if XmlFontDetail.Attributes ['Bold'] <> Null then
      Bold   := XmlFontDetail.Bold
   else
      Bold   := false ;
   if XmlFontDetail.Attributes ['Italic'] <> Null then
      Italic := XmlFontDetail.Italic
   else
      Italic := false ;
   if XmlFontDetail.Attributes ['Color'] <> Null then
      Color  := XmlFontDetail.Color
   else
      Color  := -1 ;
   if XmlFontDetail.Attributes ['Size'] <> Null then
      Size   := XmlFontDetail.Size
   else
      Size   := 0 ;
   if XmlFontDetail.Attributes ['Name'] <> Null then
      Name   := XmlFontDetail.Name
   else
      Name   := '' ;
   if XmlFontDetail.Attributes ['BackgroundColor'] <> Null then
      BackgroundColor := XmlFontDetail.BackgroundColor
   else
      BackgroundColor := 0 ;
end;

//------------------------------------------------------------------------------

// <FontDetail ColId="3" Bold="True" Color="255" />
constructor TFontDetail.create(XmlFontDetail: IXMLDOMNode);
var
   attr : IXMLDOMNode ;
begin
   //ColId  := XmlFontDetail.ColId ;
   attr := XmlFontDetail.attributes.getNamedItem('ColId') ;
   if attr <> nil then
      ColId := attr.nodeValue ;

   attr := XmlFontDetail.attributes.getNamedItem('Bold') ;
   if attr <> nil then
      Bold := attr.nodeValue
   else
      Bold := false ;

   attr := XmlFontDetail.attributes.getNamedItem('Italic') ;
   if attr <> nil then
      Italic := attr.nodeValue
   else
      Italic := false ;

   attr := XmlFontDetail.attributes.getNamedItem('Color') ;
   if attr <> nil then
      Color := attr.nodeValue
   else
      Color := -1 ;

   attr := XmlFontDetail.attributes.getNamedItem('Size') ;
   if attr <> nil then
      Size := attr.nodeValue
   else
      Size := 0 ;

   attr := XmlFontDetail.attributes.getNamedItem('Name') ;
   if attr <> nil then
      Name := attr.nodeValue
   else
      Name := '' ;

   attr := XmlFontDetail.attributes.getNamedItem('BackgroundColor') ;
   if attr <> nil then
      BackgroundColor := attr.nodeValue
   else
      BackgroundColor := 0 ;

end;

//------------------------------------------------------------------------------

function TFrm_Tool.getCommandName (SingleMsg : string;LastParsedTreeRec : pointer) : string ;
var
   command : integer ;
   treeRec : PTreeRec ;

   function getStr : String ;
   begin
      result := copy (SingleMsg,6,length (SingleMsg)-5);
   end ;

begin
   treeRec := LastParsedTreeRec ;
   command := strtointdef (copy (SingleMsg, 1,5),-1);
   case command of
   CST_TREE_COLUMNWIDTH      : result := 'CST_TREE_COLUMNWIDTH  (widths) ' ; // change the columns widths
   CST_TREE_MULTI_COLUMN     : result := 'CST_TREE_MULTI_COLUMN (index)  ' ; // change the tree to display multiple column. Param : Main column index
   CST_TREE_COLUMNTITLE      : result := 'CST_TREE_COLUMNTITLE  (Titles) ' ; // change the columns titles
   CST_DISPLAY_TREE          : result := 'CST_DISPLAY_TREE      ()       ' ; // display tree windows
   CST_TREE_NAME             : result := 'CST_TREE_NAME         (name)   ' ; // param : the new name of the tree (use CST_USE_TREE just before to specify the tree)
   CST_USE_TREE              : result := 'CST_USE_TREE          (treeId) ' ; // param : Id (CLSID for example) of the tree to use for other command.
   CST_TRACE_ID              : result := 'CST_TRACE_ID          (strId)  ' ; // param : CLSID
   CST_SHOW                  : result := 'CST_SHOW              (int)    ' ; // param : 1 : show.  0 : hide
   CST_ICO_INDEX             : result := 'CST_ICO_INDEX         (idx)    ' ; // param : image index
   CST_CLEAR_ALL             : result := 'CST_CLEAR_ALL         ()       ' ; // no param
   CST_WINWATCH_NAME         : result := 'CST_WINWATCH_NAME     (name)   ' ; // Watch Window name
   CST_WINWATCH_ID           : result := 'CST_WINWATCH_ID       (id)     ' ; // Watch Window ID
   CST_WATCH_NAME            : result := 'CST_WATCH_NAME        (name)   ' ; // watch name
   CST_SET_BOOKMARK          : result := 'CST_SET_BOOKMARK      (bool)   ' ; // param : 1/0
   CST_VISIBLE_NODE          : result := 'CST_VISIBLE_NODE      (bool)   ' ; // param : 1/0
   CST_CLEAR_NODE            : result := 'CST_CLEAR_NODE        (id)     ' ; // param : the node to clear
   CST_CLEAR_SUBNODES        : result := 'CST_CLEAR_SUBNODES    (id)     ' ; // param : the parent node
   CST_THREAD_ID             : result := 'CST_THREAD_ID         (id)     ' ; // param : thread ID
   CST_PROCESS_NAME          : result := 'CST_PROCESS_NAME      (name)   ' ; // param process name
   CST_MESSAGE_TIME          : result := 'CST_MESSAGE_TIME      (time)   ' ; // param : the time of the message
   CST_THREAD_NAME           : result := 'CST_THREAD_NAME       (name)   ' ; // param : thread name
   CST_IP                    : result := 'CST_IP                (string) ' ; // param : client IP adress
   CST_CREATE_MEMBER         : result := 'CST_CREATE_MEMBER     (col1)   ' ; // param : Member name
   CST_MEMBER_FONT_DETAIL    : result := 'CST_MEMBER_FONT_DETAIL(6 p)    ' ; // param : ColId Bold Italic Color size  Fontname
   CST_MEMBER_COL2           : result := 'CST_MEMBER_COL2       (col2)   ' ; // param : info col 2
   CST_MEMBER_COL3           : result := 'CST_MEMBER_COL3       (col3)   ' ; // param : info col 3
   CST_MEMBER_VIEWER_KIND    : result := 'CST_MEMBER_VIEWER_KIND(id)     ' ; // param : viewer id
   CST_ADD_MEMBER            : result := 'CST_ADD_MEMBER        ()       ' ; // add member to upper level. No param (for now)
   CST_NEW_NODE              : result := 'CST_NEW_NODE          (id)     ' ; // param : parent node ID
   CST_SELECT_NODE           : result := 'CST_SELECT_NODE       (id)     ' ; // set the node as 'Selected' by the user.  param : Node id
   CST_GET_NODE              : result := 'CST_GET_NODE          () ???   ' ; // return the node id
   CST_USE_NODE              : result := 'CST_USE_NODE          (id)     ' ; // use an existing node. param : Node id
   CST_APPEND_LEFT_MSG       : result := 'CST_APPEND_LEFT_MSG   (left)   ' ; // param : left msg to append
   CST_APPEND_RIGHT_MSG      : result := 'CST_APPEND_RIGHT_MSG  (right)  ' ; // param : right msg to append
   CST_FOCUS_NODE            : result := 'CST_FOCUS_NODE        (id)     ' ; // Focus to the node.
   CST_SAVETOTEXT            : result := 'CST_SAVETOTEXT        (file)   ' ; // save to text file, parameter : filename
   CST_SAVETOXML             : result := 'CST_SAVETOXML         (files)  ' ; // save to  XML file, parameter : filename
   CST_LOADXML               : result := 'CST_LOADXML           (file)   ' ; // load an XML file to the current wintrace
   CST_LOGFILE               : result := 'CST_LOGFILE           (mode,limit,file) ' ; // define the log file. Parameter : mode and filename
   CST_LINKTOPLUGIN          : result := 'CST_LINKTOPLUGIN      (f,name) ' ; // link a wintrace to a plugin
   CST_CREATE_RESOURCE       : result := 'CST_CREATE_RESOURCE   (4)      ' ; // create a resource on a wintrace
   CST_SET_TEXT_RESOURCE     : result := 'CST_SET_TEXT_RESOURCE (id,text)' ; // set the text resource
   CST_DISABLE_RESOURCE      : result := 'CST_DISABLE_RESOURCE  (id)     ' ; // disable a resource
   CST_FONT_DETAIL           : result := 'CST_FONT_DETAIL       (6)      ' ; // param : ColId Bold Italic Color size  Fontname
   CST_BACKGROUND_COLOR      : result := 'CST_BACKGROUND_COLOR  (c,id)   ' ; // param : background color
   CST_GET_OBJECT            : result := 'CST_GET_OBJECT        () ???   ' ; // the user interface ask to retreive an object
   CST_FLUSH                 : result := 'CST_FLUSH             (evn)    ' ; // special case to be interpreted by the sender thread (not to be send)
   CST_ENTER_DEBUG_MODE      : result := 'CST_ENTER_DEBUG_MODE  ()       ' ; // Enter debug mode
   CST_LEAVE_DEBUG_MODE      : result := 'CST_LEAVE_DEBUG_MODE  ()       ' ; // Leave debug mode
   CST_OPEN_TAIL             : result := 'CST_OPEN_TAIL         (file)   ' ; // Open Tail file

   // special case for CST_LEFT_MSG and CST_RIGHT_MSG : change the debug node text
   CST_LEFT_MSG :
      begin
         result := 'CST_LEFT_MSG          (left)   ' ; // param : left msg
         if treeRec <> nil then
            treeRec.LeftMsg := '' + getStr() ;
      end ;
   CST_RIGHT_MSG :
      begin
         result := 'CST_RIGHT_MSG         (right)  ' ; // param : right msg
         if treeRec <> nil then
            treeRec.LeftMsg := treeRec.LeftMsg + ' / ' + getStr() ;
      end ;
   else
      result := '?????????                      ' ;
   end ;
   result := trim(result) ;
end ;

//------------------------------------------------------------------------------

procedure TFrm_Tool.ShowParsedMessage() ;
var
   SingleMsg : string ;
   //command : integer ;
   CommandIndex : integer ;
   ParentNode : PVirtualNode ;
   MessageLength : integer ;
begin
   try
      ParentNode := TFrm_Trace.InternalTrace ('ParseTraceMsg') ;
      if ParentNode = nil then
         exit ;
      LastParsedTreeRec := FrmInternalTraces.vstTrace.GetNodeData(ParentNode) ;
      if LastParsedTreeRec = nil then
         exit ;
      if LastParsedTreeRec.Members = nil then
         LastParsedTreeRec.Members := TMember.create();

      MessageLength := 0 ;
      for CommandIndex := 0 to CurrentParseMsgList.Count-1 do begin
          SingleMsg := CurrentParseMsgList[CommandIndex] ;
          MessageLength := MessageLength + length(SingleMsg) ;
          //command := strtointdef (copy (SingleMsg, 1,5),-1);
          LastParsedTreeRec.Members.SubMembers.Add (TMember.create('[' + inttostr(CommandIndex) + '] : ' + getCommandName(SingleMsg,LastParsedTreeRec) , SingleMsg)) ;
      end ;
      LastParsedTreeRec.RightMsg := 'msg count : ' + inttostr(CurrentParseMsgList.Count) + ' / MsgLen : ' + inttostr(MessageLength);
   except
   end ;
end ;

//------------------------------------------------------------------------------

procedure TFrm_Tool.ShowParsedForm(TraceForm : TForm) ;
begin
   if LastParsedTreeRec = nil then
      exit ;
   try
      if TraceForm = nil then
         LastParsedTreeRec.Members.SubMembers.Add (TMember.create('use window nil'))
      else if TraceForm is TFrm_Trace then
         LastParsedTreeRec.Members.SubMembers.Add (TMember.create('use window ' + TraceForm.Name ,'WintraceId="' + string(TFrm_Trace(TraceForm).ID) + '"')) ;
   except
   end ;
end ;

//------------------------------------------------------------------------------

procedure TFrm_Tool.MadExceptionHandler1Exception(
  const exceptIntf: IMEException; var handled: Boolean);
var
   report : string;
   repList : TStringList ;
   c : integer ;
   ParentNode : PVirtualNode ;
   TreeRec : PTreeRec ;
   IsParse : boolean ;
   IsLinkedListAddToList  : boolean ;
   CommandIndex : integer ;
   SingleMsg : string ;
   member : TMember ;
   //p : integer ;
   //command : integer ;

   procedure AddTrace (msg1 : string; msg2 : string = '') ;
   begin
      //if TraceConfig.AppDisplay_DisableInternalLog = true then
      LowTrace(msg1 + ' ' + msg2) ;
      if (treeRec <> nil) then           
         TreeRec.Members.SubMembers.Add(TMember.create(msg1,msg2));
   end ;
begin
   TreeRec := nil ;
   report := string (exceptIntf.BugReport) ; // same as GetBugReport (true) ;

   //if TraceConfig.AppDisplay_DisableInternalLog = true then
   LowTrace(string(exceptIntf.ExceptClass) + ' ' + string(exceptIntf.ExceptMessage))  ;
   //else begin
   try
      ParentNode := TFrm_Trace.InternalTrace (string(exceptIntf.ExceptClass),string(exceptIntf.ExceptMessage)) ;
      TreeRec := FrmInternalTraces.vstTrace.GetNodeData(ParentNode) ;
      if TreeRec.Members = nil then
         TreeRec.Members := TMember.create();
      
   except on E: Exception do
      TreeRec := nil ;
   end;
   
   //end;

   repList := TStringList.create() ;
   repList.Text := report ;

   IsParse := false ;
   IsLinkedListAddToList := false ;
   for c := 0 to repList.Count-1 do begin
      report := trim(repList.Strings[c]) ;
      if pos ('ParseTraceMsg',report) > 0 then
         IsParse := true ;

      if pos ('TNodeLinkedList.AddToList',report) > 0 then
         IsLinkedListAddToList := true ;

      AddTrace(report);
   end ;
   repList.free ;

   if (IsParse = true) and (CurrentParseMsgList <> nil) then begin

      AddTrace('');
      AddTrace('ParseTraceMsg', 'Line (' + inttostr (CurrentParseCommandIndex) + ')');
      AddTrace('--------------');

      for CommandIndex := 0 to CurrentParseMsgList.Count-1 do begin
          SingleMsg := CurrentParseMsgList[CommandIndex] ;
          //command := strtointdef (copy (SingleMsg, 1,5),-1);
          AddTrace ('[' + inttostr(CommandIndex) + '] : ' + getCommandName(SingleMsg, nil) , SingleMsg) ;
      end ;
      // since exception occured, we must free the command list
      CurrentParseMsgList.Free ;             // free the MessageList only when finished (allocated by WMCopyData or socket receiver)

   end ;

   if  (IsLinkedListAddToList = true) then begin
      AddTrace ('') ;
      AddTrace('NodeLinkedList exception');

      if (TraceConfig.AppDisplay_DisableInternalLog = false) then begin
         AddTrace('========================');

         // move prepared traces to members
         while BeforeAddList.count > 0 do begin
            member := TMember (BeforeAddList[0]) ;
            BeforeAddList.Delete(0);
            if TreeRec <> nil then            
               TreeRec.Members.SubMembers.Add(member);
         end ;

         AddTrace('---');
         AddTrace('list after crashing :') ;
         if CurrentNodeLinkedList <> nil then
            CurrentNodeLinkedList.DumpToList (TreeRec.Members.SubMembers) ;
      end;

   end ;

   // No mad exception dialog
   handled := true ;
end;


//------------------------------------------------------------------------------

procedure TFrm_Tool.IdHTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
   CommandGet (IdHTTPServer,AContext, ARequestInfo,AResponseInfo);
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.IdHTTPServerException(AContext: TIdContext;
  AException: Exception);
begin
   TFrm_Trace.internalTrace (AException.Message) ;
end;

//------------------------------------------------------------------------------

procedure TFrm_Tool.CommandGet(sender : TIdHTTPServer ; AContext: TIdContext;ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo );
var
     c : integer ;
     strHtml : string ;
     p : integer ;
     str : string ;
     decoded : string ;
     str_len : integer ;
     msg,msgId,partNum : string ;
     msgLen : integer ;
     Compressed : string ;
     IndexOfMsgId : integer ;
     doc : string ;
     FileToLoad : TStringList ;
     strFileToLoad : string ;
     DebugWin: hWnd;
     CDS: TCopyDataStruct;
     requestInfoParameters : TStringList ;
begin
   doc := ARequestInfo.Document ;
   AResponseInfo.CacheControl := 'no-store,no-cache' ;
   msg := '' ;
   msgId := '' ;
   partNum := '' ;
   msgLen := 0 ;
   Compressed := '' ;
   if TraceConfig.DebugMode = true then
      TFrm_Trace.InternalTraceFromThread (
          'From ' + ARequestInfo.RemoteIP +            // '127.0.0.1'
          ' ' + ARequestInfo.Command  +                //  GET
          ' (' + inttostr(sender.DefaultPort) + ') ' + // (81)
          doc + '?' + ARequestInfo.UnparsedParams) ;   // /WMD?msgId=6_6&msg=%20%20304...

   //UnparsedParams : 
   // msgId=6d3638ef881a4c2aa40fecaf10afd36a_2&msg=
   //%20%2030423%3A04%3A24%3A943%00
   //%20%20550%00
   //%20%201016d3638ef881a4c2aa40fecaf10afd36a_1%00
   //%20%20551description%00
   //%20%20103%20%20%20%20%20%20%20%20%2024%00
   //%20%20552a%B0a

   // decompose the message to individual parameters
   // bug with ARequestInfo.Params.Strings[c] : parameters are not decoded with the correct encoding (ascii or other)
   requestInfoParameters := TStringList.Create ;
   requestInfoParameters.Delimiter := '&';
   requestInfoParameters.DelimitedText := ARequestInfo.UnparsedParams ;     

   for c := 0 to requestInfoParameters.Count-1 do begin
      str := requestInfoParameters[c] ;
      decoded := TIdURI.URLDecode(str) ;     // ,TIdTextEncoding.ASCII
      if decoded = '' then begin
         decoded := TIdURI.URLDecode(str,IndyTextEncoding_ASCII); //IIdTextEncoding.ASCII);  // use ascii decoding if default decoding don't work.
      end ;   

      str_len := length(decoded) ;  // get all bytes
      //TFrm_Trace.InternalTraceFromThread('param after decode: ' + decoded) ;

      p := pos('msgId=',decoded) ;     //6
      if p = 1 then 
         msgId := copy(decoded, 7, str_len-6) ;

      p := pos('partNum=',decoded) ;    //8
      if p = 1 then 
         partNum := copy(decoded, 9, str_len-8) ;

      p := pos('msg=',decoded) ;        //4
      if p = 1 then begin
         msg := copy(decoded, 5, str_len-4) ;
         msgLen := str_len-4 ;
      end;

      p := pos('Compressed=',decoded) ;   //11
      if p = 1 then 
         Compressed := UpperCase(copy(decoded, 12, str_len-11)) ;
   end ;
   requestInfoParameters.Free;

   if StrIComp (pchar(doc),'/WMD') = 0 then begin
      strHtml := '   ttrace._done("' + msgId + '","' + partNum + '"); ' ;
      AResponseInfo.ContentStream := TStringStream.Create(strHtml) ;

      //LowTrace('msgId : ' + msgId + #9 + 'partNum : ' + partNum + #9 + 'msgLen : ' +  inttostr(msgLen)
      ////         + #9 + 'crc : ' + inttostr(Calc_Crc(msg)) + '/' + browserCrc
      //         + #9 + 'msg : ' + msg
      //         );

      // multi part
      if partNum <> '' then begin
         
         IndexOfMsgId := ScriptMessages.IndexOfName(msgId) ;

         // if -1 then this is the first part
         if IndexOfMsgId = -1 then begin
            if partNum = '1' then begin
               ScriptMessages.Add(msgId  + '=' + msg)  ;
               IndexOfMsgId := ScriptMessages.IndexOfName(msgId) ;
            end else begin
               // ignore messages not starting with part "1"
               exit ;
            end ;
         end else begin
            ScriptMessages[IndexOfMsgId] := ScriptMessages[IndexOfMsgId] + msg ;
         end ;

         // if message is completed, process it.
         if partNum = 'Last' then begin
            msg := ScriptMessages.Values[msgId] ;
            msgLen := length (msg) ;
            ScriptMessages.Delete(IndexOfMsgId);
         end else begin
            exit ;     // don't process message
         end ;
      end ;

      //LowTrace('send ' + msg) ;
      DebugWin := FindWindow('TFormReceiver', 'FormReceiver');

      if DebugWin <> 0 then begin
         // start with the UTF-16, little endian byte order mark. Add a #0 to end the last string and add a final #0 to end the message
         msg := #$FEFF + msg + #0 + #0;
         inc(msgLen,3)  ;
         CDS.cbData := msgLen*2;
         CDS.dwData := WMD ;   // identification code for tracetool
         CDS.lpData := pAnsiString (pString(msg));
         SendMessage(DebugWin, WM_COPYDATA, 0, LParam(@CDS));  //WParam(Application.Handle)
      end;
   end else if StrIComp (pchar(doc), '/UniqueClientId') = 0 then begin
      inc(uniqueId) ;
      strHtml := 'ttrace.setClientID("' + inttostr(uniqueId) + '");' ;
      AResponseInfo.ContentStream := TStringStream.Create(strHtml) ;

   end else if StrIComp (pchar(doc), '/clientaccesspolicy.xml') = 0 then begin
      if TraceConfig.General_HttpPolicyServer = false then begin
         if TraceConfig.DebugMode = true then
            TFrm_Trace.InternalTraceFromThread ('Http Policy Server flag not enabled');
         exit ;
      end;

      FileToLoad := TStringList.Create() ;
      strFileToLoad := 'clientaccesspolicy.xml' ;
      if FileExists(strFileToLoad) then begin
         FileToLoad.LoadFromFile(strFileToLoad) ;
         // todo : display the file in the debug windows if TraceConfig.DebugMode = true
      end else begin
         if TraceConfig.DebugMode = true then
            TFrm_Trace.InternalTraceFromThread ('clientaccesspolicy.xml not found');
      end;
      strFileToLoad := FileToLoad.Text ;
      AResponseInfo.ContentStream := TStringStream.Create(strFileToLoad) ;
      FileToLoad.Clear ;
      FileToLoad.Free ;

   end else if StrIComp (pchar(doc), '/crossdomain.xml') = 0 then begin
      if TraceConfig.General_HttpPolicyServer = false then begin
         if TraceConfig.DebugMode = true then
            TFrm_Trace.InternalTraceFromThread ('Http Policy Server flag not enabled');
         exit ;
      end;

      FileToLoad := TStringList.Create() ;
      strFileToLoad := 'crossdomain.xml' ;
      if FileExists(strFileToLoad) then begin
         FileToLoad.LoadFromFile(strFileToLoad) ;
         // todo : display the file in the debug windows if TraceConfig.DebugMode = true
      end else begin
         if TraceConfig.DebugMode = true then
            TFrm_Trace.InternalTraceFromThread ('crossdomain.xml not found');
      end;
      strFileToLoad := FileToLoad.Text ;
      AResponseInfo.ContentStream := TStringStream.Create(strFileToLoad) ;
      FileToLoad.Clear ;
      FileToLoad.Free ;

   end else if StrIComp (pchar(doc), '/TraceTool.js') = 0 then begin
      inc(uniqueId) ;
      FileToLoad := TStringList.Create() ;

      if (Compressed = '1') or (Compressed = 'TRUE') then
         strFileToLoad := 'tracetool.jmin.js'
      else
         strFileToLoad := 'tracetool.js' ;

      if FileExists(strFileToLoad) then
         FileToLoad.LoadFromFile(strFileToLoad)
      else if FileExists('..\Javascript\'+strFileToLoad) then
         FileToLoad.LoadFromFile('..\Javascript\'+strFileToLoad)
      else begin
         FileToLoad.Add('window.alert("TraceTool viewer don''t found the ''' + strFileToLoad + ''' file")')
      end ;
      strFileToLoad := FileToLoad.Text ;

      // if script is loaded from the viewer (should always be the case), remove the call to ask a new client id
      strFileToLoad := StringReplace(strFileToLoad,
        'ttrace.queryClientId();',
        'ttrace.display("Client ID : ' + inttostr(uniqueId) + ', port : ' + ARequestInfo.Host + '");',
        [rfReplaceAll]);

      // change the empty client id
      strFileToLoad := StringReplace(strFileToLoad,
        'var clientID="";',
        'var clientID="' + inttostr(uniqueId) + '";',
        [rfReplaceAll]);

      // change default host and port by the current one
      strFileToLoad := StringReplace(strFileToLoad,
        'var host="127.0.0.1:81"',
        'var host="' + ARequestInfo.Host + '"',
        [rfReplaceAll]);

      AResponseInfo.ContentStream := TStringStream.Create(strFileToLoad) ;
      FileToLoad.Clear ;
      FileToLoad.Free ;
   end ;
end ;

//------------------------------------------------------------------------------

procedure TFrm_Tool.actDependsClick(Sender: TObject);
//var
//   i : integer ;
//   FileName: string ;
begin
//   Screen.Cursor := crHourGlass;
//   try
//      OpenFileDialog.FileName := '';
//      if OpenFileDialog.Execute = false then
//         exit ;
//
//      for I := 0 to OpenFileDialog.Files.Count - 1 do begin
//         FileName := OpenFileDialog.Files[I] ;
//         try
//            TFileViewerChild.Create(Self).FileName := FileName;
//         finally
//         end;
//      end;
//   finally
//      Screen.Cursor := crDefault;
//   end ;

//   DependsMainForm := TDependsMainForm.Create(self);
//   DependsMainForm.Show ;
end;

//------------------------------------------------------------------------------

// procedure TFrm_Tool.UDPServer1UDPRead(AThread: TIdUDPListenerThread;  const AData: TIdBytes; ABinding: TIdSocketHandle);

procedure TFrm_Tool.UDPServerUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
   AnsiPtr : PAnsiChar ;      // pointer to the buffer (as ansiString)
   pWordLength : pDword ;     // pointer to a DWORD containing the message length
   TempMsgLen : AnsiString ;  // temp buffer for the message lenght
   MsgLen : integer ;         // decoded message length
   errorLength : integer ;
begin
   try
      // The message start with an header : the WMD (123) code stored in a single byte folowed byte the number of bytes to read (DWORD) followed by the message
      // In previous version, the WND code is not present and the number was a string (coded as single byte), followed by a #0 then the message
      // Both case are supported
      //
      // If the message is unicode, he start with the UTF16 - unicode : FE FF  (stored in reverse order : FF FE)
      // If not it'a a single byte string
      AnsiPtr := pAnsiChar(AData) ;
      if (AnsiPtr^ >= '0') and (AnsiPtr^ <= '9') then begin    // 48 .. 57
         TempMsgLen := AnsiPtr^ ;
         while True do begin
            inc(AnsiPtr) ;
            if AnsiPtr^ = #0 then
               break ;
            TempMsgLen := TempMsgLen + AnsiPtr^ ;
         end;

         Val(string(TempMsgLen), MsgLen, errorLength);
         if errorLength <> 0 then begin
            TFrm_Trace.InternalTraceFromThread('TCP server buffer : Invalid message. Should be an integer : ' + String(TempMsgLen)) ;
            // flush the buffer .... ???
            exit ;
         end;
         // skip the zero after the number
         inc(AnsiPtr) ;
      end else if (ord(AnsiPtr^) = WMD) then begin
         // skip "WMD"
         inc(AnsiPtr) ;
         // length is stored in next 4 bytes
         pWordLength := pDword (AnsiPtr) ;
         MsgLen := pWordLength^ ;
         // skip the length
         inc(AnsiPtr,4) ;

      end else begin
         TFrm_Trace.InternalTraceFromThread('TCP server buffer : Invalid message. don''t start with a string number or by the WMD byte ') ;
            // flush the buffer .... ???
         exit ;
      end;
      if MsgLen <> 0 then   // in UDP, the MsgLen is not usefull
         SplitMessage (pAnsiChar(AnsiPtr)) ;

   except
      on e : exception do begin
         TFrm_Trace.InternalTraceFromThread(e.ClassName + ' : ' + e.Message) ;
         raise ;
      end ;
   end ;
end;



//------------------------------------------------------------------------------

initialization
   TraceConfig := TTraceConfig.create ;
   TraceConfig.DebugMode := false ;
   CriticalSection := TCriticalSection.create ;
   FileCriticalSection := TCriticalSection.create ;

   InternalTraceMessageStack := TStringList.create ;
   InternalTraceMessageStack2 := TStringList.create ;

   PluginsInitialized := false ;

   MessageStack    := TObjectList.create (false);      // don't own objects
   SwapStack       := TObjectList.create (false);      // don't own objects
   ConnectionList  := TObjectList.Create (false) ;
   BaseList        := TObjectList.Create (false) ;
   FormTraceList   := TObjectList.Create (false) ;
   TailList        := TObjectList.Create (false) ;
   ContainerList   := TObjectList.Create (false) ;
   OdsMessageStack := TObjectList.Create (false) ;
   BoldDetail      := TFontDetail.create(0 , true , false , -1 , 0 , '') ;
   ScriptMessages  := TStringList.create() ;

   Received := 0 ;

finalization
   // register 'intentional' Indy 10 memory leak.
   //RegisterExpectedMemoryLeak (GThreadCount) ;           // register a pointer
   //RegisterExpectedMemoryLeak (TIdCriticalSection) ; //,2) ;   // register a type (2 instances)

   // it's possible that some messages are still in the message queue while the application is shuting down
   while MessageStack.Count <> 0 do begin  // MessageStack is not owner
      MessageStack.First.Free ;  // delete remaining TStringList object message
      MessageStack.Delete(0);
   end ;
   TraceConfig.Free ;
   SwapStack.Clear ;                  FreeAndNil(SwapStack) ;                  // not owner
   MessageStack.Clear ;               FreeAndNil(MessageStack) ;               // not owner
   InternalTraceMessageStack.Clear ;  FreeAndNil(InternalTraceMessageStack) ;  // strings
   InternalTraceMessageStack2.Clear ; FreeAndNil(InternalTraceMessageStack2) ; // strings
   ConnectionList.Clear ;             FreeAndNil(ConnectionList) ;             // not owner
   BaseList.Clear ;                   FreeAndNil(BaseList) ;                   // not owner
   FormTraceList.Clear ;              FreeAndNil(FormTraceList) ;              // not owner
   TailList.Clear ;                   FreeAndNil(TailList) ;                   // not owner
   ContainerList.Clear ;              FreeAndNil(ContainerList) ;              // not owner
   OdsMessageStack.Clear ;            FreeAndNil(OdsMessageStack) ;            // not owner
   criticalsection.Free ;
   BoldDetail.free ;
   ScriptMessages.free ;
   FileCriticalSection.Free ;

end.
