unit cachePluginDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, Contnrs, db ,
  ODSI, OCL, OCIH, tracetool, Grids, ExtCtrls, ImgList;

const
   PlugName = 'CachéPlugin' ;

type
  TServersForm = class(TForm)
    Label1: TLabel;
    editPollingTimeOut: TEdit;
    Label2: TLabel;
    EditPollingRecords: TEdit;
    Label3: TLabel;
    EditMaxRecords: TEdit;
    GroupBox1: TGroupBox;
    ServerList: TListBox;
    butAdd: TButton;
    butDelete: TButton;
    Label4: TLabel;
    EditAlias: TEdit;
    Label5: TLabel;
    EditServer: TEdit;
    Label6: TLabel;
    EditDatabase: TEdit;
    Label7: TLabel;
    EditUser: TEdit;
    Label8: TLabel;
    editPassword: TEdit;
    butCheck: TButton;
    butActivate: TButton;
    Label9: TLabel;
    MemoConnect: TMemo;
    PanelTarget: TPanel;
    butOk: TButton;
    UtilityImages: TImageList;
    EditConnectionTime: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    EditPort: TEdit;
    procedure butOkClick(Sender: TObject);
    procedure ServerListClick(Sender: TObject);
    procedure editPasswordKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure butAddClick(Sender: TObject);
    procedure butDeleteClick(Sender: TObject);
    procedure butCheckClick(Sender: TObject);
    procedure butActivateClick(Sender: TObject);
    procedure editPollingTimeOutKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCacheLog = class
      started : boolean ;
      WinTrace : IWinTrace ;
      LastMsgError : string ;
      LastConnectionStatus : string ;
      LastMessageStatus : string ;
      OriginalName : string ; // original section name before editing
      Name : string ;
      Server : string ;
      Port : string ;
      //WinTraceId : string ;
      User: string ;
      Password : string ;
      Database : string ;
      MaxConnectionTime : integer ;
      hdbc: THdbc ;
      ds: TOEDataSet;
      LastQuery : DWORD ;
      LastConnectionTime : DWORD ;
      LastReadId : integer ;
      job : integer ;  // call getJob()  first
      cacheToolForm : TForm ;
      deleted : boolean ;
      added : boolean ;

      procedure CheckConnect() ;

      // Caché API
      function  GetLastLogId(): integer;
      function  GetLastBlock(): integer;
      procedure LogClear() ;
      function  getClasses(startWidth : string) : integer;
      function  LogQueryPlan(SQL: string): integer;
      function  getQueryPlanCost(SQL: string;var plan : string): string;
      function  LogProcess: integer;
      function  LogGlobal(GlobalName,offset: string): integer;
      function  KillGlobal(GlobalName : string): integer;
      function  getGlobalElementCount(GlobalName : string): integer;
      procedure RunSql(SQL: string;OEQuery : TOEdataset);
      function  getGlobalList : integer ;

      procedure DoTimer();
      function  getJob() : integer;
      function  LogClass (ClassName : String ; Title : String = '') : integer ;
      function  ReadClass(ClassName : String): integer;
      function  ReadClassXml(ClassName : String): integer;
      function  WriteClass(): integer;
      function  WriteClassXml(): integer;
  end;

// plugin API
procedure GetPlugName (lpPlugName: PAnsiChar) stdcall ;
function  OnAction (WinId : PAnsiChar ; ResourceId : integer; NodeId : PAnsiChar) : BOOL stdcall ;
function  OnBeforeDelete (WinId : PAnsiChar ; NodeId : PAnsiChar) : BOOL stdcall ;
procedure OnTimer() stdcall ;
procedure Stop()  stdcall ;
procedure Start() stdcall ;

var
  ServersForm: TServersForm;
  CacheLogList : tObjectList ;
  PollingTimeOut : integer ;
  PollingRecords : integer ;
  MaxRecorsToRead : integer ;

implementation

{$R *.dfm}

uses unt_cacheTools  ;

{User interface}

//------------------------------------------------------------------------------

// a server is selected in the list. copy the CacheLog information in the differents edit boxes
procedure TServersForm.ServerListClick(Sender: TObject);
var
   CacheLog : TCacheLog ;
begin
   if ServerList.ItemIndex < 0 then
      exit ;

   CacheLog := TCacheLog (ServerList.Items.Objects[ServerList.ItemIndex]) ;
   EditAlias   .text := CacheLog.Name ;
   EditServer  .text := CacheLog.Server ;
   EditPort    .text := CacheLog.Port ;
   EditDatabase.text := CacheLog.Database ;
   EditUser    .text := CacheLog.User ;
   editPassword.text := CacheLog.Password ;
   EditConnectionTime.Text := inttostr(CacheLog.MaxConnectionTime) ;

   if CacheLog.WinTrace = nil then
      butActivate.Enabled := true
   else
      butActivate.Enabled := false ;
   MemoConnect.Text := '' ;
end;

//------------------------------------------------------------------------------

// a field is modified. Save all edits to the CacheLog object
procedure TServersForm.editPasswordKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
var
   CacheLog : TCacheLog ;
begin
   if ServerList.ItemIndex < 0 then
      exit ;

   // save edit to object
   CacheLog := TCacheLog (ServerList.Items.Objects[ServerList.ItemIndex]) ;
   CacheLog.Name        :=  EditAlias   .text ;
   CacheLog.Server      :=  EditServer  .text ;
   CacheLog.Port        :=  EditPort    .text ;
   CacheLog.Database    :=  EditDatabase.text ;
   CacheLog.User        :=  EditUser    .text ;
   CacheLog.Password    :=  editPassword.text ;
   CacheLog.MaxConnectionTime := strtointdef(EditConnectionTime.Text,0) ;  // minutes

   ServerList.Items.Strings [ServerList.ItemIndex] := CacheLog.Name ;
end;

//------------------------------------------------------------------------------

procedure TServersForm.editPollingTimeOutKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   PollingTimeOut := strtointdef(ServersForm.editPollingTimeOut.Text,1000) ;
   PollingRecords := strtointdef(ServersForm.EditPollingRecords.Text,1000) ;
   MaxRecorsToRead := strtointdef(ServersForm.EditMaxRecords.Text,5000) ;
end;

//------------------------------------------------------------------------------

// step 1 : create the TCacheLog
procedure TServersForm.butAddClick(Sender: TObject);
var
   CacheLog : TCacheLog ;
begin
   CacheLog := TCacheLog.create() ;
   CacheLogList.Add(CacheLog) ;
   CacheLog.OriginalName := '' ;
   CacheLog.Name := 'MySrv' ;
   CacheLog.Server   := 'MyServer';
   CacheLog.Port     := '1972' ;
   CacheLog.Database := 'MyDB';
   CacheLog.User     := 'User';
   CacheLog.Password := 'Password';
   CacheLog.MaxConnectionTime := 10 ; // minutes

   CacheLog.LastQuery := 0 ;
   CacheLog.LastReadId := -1 ;
   CacheLog.LastMsgError := '' ;
   CacheLog.started := false ;  // by default, server polling is not started
   CacheLog.deleted := false ;
   CacheLog.added := true ;

   CacheLog.hdbc := THdbc.Create(nil);
   CacheLog.hdbc.Driver := 'InterSystems ODBC' ;
   CacheLog.hdbc.Password := CacheLog.Password ;
   CacheLog.hdbc.Attributes.Add('DRIVER={InterSystems ODBC}'   );
   CacheLog.hdbc.Attributes.Add('PORT=' +  CacheLog.Port       );
   CacheLog.hdbc.Attributes.Add('PROTOCOL=TCP'                 );
   CacheLog.hdbc.Attributes.Add('QUERY TIMEOUT=1'              );
   CacheLog.hdbc.Attributes.Add('STATIC CURSORS=1'             );
   CacheLog.hdbc.Attributes.Add('UID=' + CacheLog.User         );
   CacheLog.hdbc.Attributes.Add('SERVER=' + CacheLog.server    );
   CacheLog.hdbc.Attributes.Add('DATABASE=' + CacheLog.Database);
   CacheLog.hdbc.ConnectionPooling := cpOnePerDriver ;

   CacheLog.ds := TOEDataSet.create(nil);
   CacheLog.ds.hDbc := CacheLog.hdbc ;
   CacheLog.ds.hStmt.CursorType := 0 ;
   CacheLog.ds.hStmt.SkipByPosition := True ;
   CacheLog.ds.hStmt.SkipByCursor := True ;
   CacheLog.ds.Table := 'Utility.log' ;
   CacheLog.ds.Editable := false ;

   ServersForm.ServerList.AddItem(CacheLog.Name, CacheLog) ;
   ServersForm.ServerList.ItemIndex := ServersForm.ServerList.Count-1 ;
   ServerListClick(nil);  // force display server
end;

//------------------------------------------------------------------------------

// step 2 : check if we can connect to the server
procedure TServersForm.butCheckClick(Sender: TObject);
var
   CacheLog : TCacheLog ;
begin
   if ServerList.ItemIndex < 0 then
      exit ;

   // save edit to object
   CacheLog := TCacheLog (ServerList.Items.Objects[ServerList.ItemIndex]) ;

   CacheLog.hdbc.Password := CacheLog.Password ;
   CacheLog.hdbc.Attributes.Clear ;
   CacheLog.hdbc.Attributes.Add('DRIVER={InterSystems ODBC}'   );
   CacheLog.hdbc.Attributes.Add('PORT=' + CacheLog.Port        );
   CacheLog.hdbc.Attributes.Add('PROTOCOL=TCP'                 );
   CacheLog.hdbc.Attributes.Add('QUERY TIMEOUT=1'              );
   CacheLog.hdbc.Attributes.Add('STATIC CURSORS=1'             );
   CacheLog.hdbc.Attributes.Add('UID=' + CacheLog.User         );
   CacheLog.hdbc.Attributes.Add('SERVER=' + CacheLog.server    );
   CacheLog.hdbc.Attributes.Add('DATABASE=' + CacheLog.Database);
   CacheLog.hdbc.ConnectionPooling := cpOnePerDriver ;

   MemoConnect.Text := '' ;

   try
      CacheLog.Job := 0 ;
      CacheLog.hdbc.connected := false ;
      CacheLog.CheckConnect() ;
      MemoConnect.Text := 'Ok' ;
   except
      on e : exception do begin
         MemoConnect.Text := e.Message ;
      end;
   end;

end;

//------------------------------------------------------------------------------

// step 3 : create the wintrace
procedure TServersForm.butActivateClick(Sender: TObject);
var
   CacheLog : TCacheLog ;
begin
   if ServerList.ItemIndex < 0 then
      exit ;

   // save edit to object
   CacheLog := TCacheLog (ServerList.Items.Objects[ServerList.ItemIndex]) ;
   if CacheLog.WinTrace <> nil then
      exit ;

   CacheLog.WinTrace := TTrace.createWinTrace( CacheLog.Name , CacheLog.Name) ;   //  CacheLog.server
   CacheLog.WinTrace.setMultiColumn (1) ; // The parameter specify the main column index
   CacheLog.WinTrace.setColumnsTitle('job'+#9+'time'+#9+'Level'+#9+'information'+#9+{colA}''+#9+{ColB}''+#9+{colC}'');
   CacheLog.WinTrace.setColumnsWidth({job}'40' +#9+{Time}'130' + #9 + {Level} '40' + #9 + {info}'80' + #9 + {colA}'100' + #9 + {ColB}'130' + #9 + {colC}'130');
   CacheLog.WinTrace.LinkToPlugin (PlugName, CST_PLUG_ONACTION + CST_PLUG_ONTIMER) ; // attach the window to the plugin (itself)

   CacheLog.WinTrace.DisableResource (CST_ACTION_LABEL_LOGFILE) ; // disable the  LogFile label
   CacheLog.WinTrace.CreateResource (106,CST_RES_BUT_RIGHT,  200,'Connect');
   CacheLog.WinTrace.CreateResource (104,CST_RES_LABEL_RIGHT,400,'');
   CacheLog.WinTrace.CreateResource (101,CST_RES_BUT_RIGHT  ,80 ,'Clear content');
   CacheLog.WinTrace.CreateResource (102,CST_RES_BUT_RIGHT  ,50 ,'Reload');
   CacheLog.WinTrace.CreateResource (103,CST_RES_BUT_RIGHT  ,80 ,'Start polling!!!');   // by default, server polling is not started
   CacheLog.WinTrace.CreateResource (105,CST_RES_BUT_RIGHT  ,80 ,'Caché Tools');
end;


//------------------------------------------------------------------------------

procedure TServersForm.butDeleteClick(Sender: TObject);
var
   CacheLog : TCacheLog ;
begin
   if ServerList.ItemIndex < 0 then
      exit ;

   // save edit to object
   CacheLog := TCacheLog (ServerList.Items.Objects[ServerList.ItemIndex]) ;
   CacheLog.deleted := true ;

   ServersForm.ServerList.Items.Delete(ServerList.ItemIndex) ;

   if ServersForm.ServerList.Count <> 0 then
      ServersForm.ServerList.ItemIndex := ServersForm.ServerList.Count-1 ;
   ServersForm.ServerListClick(nil);  // force display first server

end;

//------------------------------------------------------------------------------

procedure TServersForm.butOkClick(Sender: TObject);
var
   //NodeEx : ITraceNodeEx ;
   c : integer ;
   CacheLog : TCacheLog ;
   databaseList : TStringList ;
   iniFile : tinifile ;
   dllName : array[0..MAX_PATH] of char ;
   dllPath : string ;

begin
   databaseList := TStringList.Create() ;

   GetModuleFileName(HInstance, dllName, MAX_PATH-1) ;
   dllPath := ExtractFilePath (dllName) ;
   iniFile := TIniFile.Create (dllPath +'CachePlugin.ini') ;

   iniFile.WriteInteger ('Config', 'PollingTimeOut' , PollingTimeOut);
   iniFile.WriteInteger ('Config', 'PollingRecords' , PollingRecords) ;
   iniFile.WriteInteger ('Config', 'MaxRecorsToRead', MaxRecorsToRead) ;

   for c := 0 to CacheLogList.Count-1 do begin
      CacheLog := TCacheLog (CacheLogList.Items[c]) ;
      if CacheLog.OriginalName <> '' then
         inifile.EraseSection(CacheLog.OriginalName);
      if CacheLog.deleted = false then begin
         inifile.WriteString (CacheLog.Name, 'Server'  ,CacheLog.Server  );
         inifile.WriteString (CacheLog.Name, 'Port'    ,CacheLog.Port    );
         inifile.WriteString (CacheLog.Name, 'Database',CacheLog.Database);
         inifile.WriteString (CacheLog.Name, 'User'    ,CacheLog.User    );
         inifile.WriteString (CacheLog.Name, 'Password',CacheLog.Password);
         inifile.WriteInteger(CacheLog.Name, 'MaxConnectionTime',CacheLog.MaxConnectionTime);
      end;
   end;
   iniFile.UpdateFile ;
   iniFile.Free ;
   databaseList.Free ;
   Hide ;
end;




//==============================================================================

// initialise the plugin
procedure Start() stdcall ;
var
   //NodeEx : ITraceNodeEx ;
   c : integer ;
   CacheLog : TCacheLog ;
   databaseList : TStringList ;
   iniFile : tinifile ;
   dllName : array[0..MAX_PATH] of char ;
   dllPath : string ;
begin
   TTrace.start() ; // run trace sub system if stopped
   ServersForm := TServersForm.Create(nil);

   databaseList := TStringList.Create() ;

   GetModuleFileName(HInstance, dllName, MAX_PATH-1) ;
   dllPath := ExtractFilePath (dllName) ;
   iniFile := TIniFile.Create (dllPath +'CachePlugin.ini') ;

   PollingTimeOut := iniFile.ReadInteger ('Config', 'PollingTimeOut', 1000);
   ServersForm.editPollingTimeOut.Text := inttostr(PollingTimeOut) ;

   PollingRecords  := iniFile.ReadInteger ('Config', 'PollingRecords' , 1000) ;
   ServersForm.EditPollingRecords.Text := inttostr(PollingRecords) ;

   MaxRecorsToRead := iniFile.ReadInteger ('Config', 'MaxRecorsToRead', 5000) ;
   ServersForm.EditMaxRecords.Text := inttostr(MaxRecorsToRead) ;

   // add a menu to the 'window' menu
   ttrace.WinTrace.CreateResource (100,CST_RES_MENU_WINDOW,0,'Caché servers');
   // attach the main window to the plugin (itself)
   ttrace.WinTrace.LinkToPlugin (PlugName, CST_PLUG_ONACTION ) ;

   iniFile.ReadSections(databaseList);

   CacheLogList := tObjectList.Create(true) ;  // owner
   for c := 0 to databaseList.count-1 do begin
      if databaseList.strings[c] = 'Config' then  // ignore this section
         continue ;

      CacheLog := TCacheLog.create() ;
      CacheLogList.Add(CacheLog) ;
      CacheLog.Name := databaseList.strings[c] ;                // WinDev=10.5.21.236
      CacheLog.OriginalName := CacheLog.Name ;
      CacheLog.Server   := iniFile.ReadString (databaseList[c], 'Server', '');
      CacheLog.Port     := iniFile.ReadString (databaseList[c], 'Port', '1972');
      CacheLog.Database := iniFile.ReadString (databaseList[c], 'Database', '');
      CacheLog.User     := iniFile.ReadString (databaseList[c], 'User', '');
      CacheLog.Password := iniFile.ReadString (databaseList[c], 'Password', '');
      CacheLog.MaxConnectionTime := iniFile.ReadInteger (databaseList[c], 'MaxConnectionTime', 10);

      CacheLog.LastQuery := 0 ;
      CacheLog.LastReadId := -1 ;
      CacheLog.LastMsgError := '' ;
      CacheLog.started := false ;  // by default, server polling is not started
      CacheLog.deleted := false ;
      CacheLog.added := false ;

      ServersForm.ServerList.AddItem(CacheLog.Name, CacheLog) ;

      CacheLog.WinTrace := TTrace.createWinTrace( CacheLog.Name , CacheLog.Name) ;   //  CacheLog.server
      CacheLog.WinTrace.setMultiColumn (1) ; // The parameter specify the main column index
      //CacheLog.WinTrace.setColumnsTitle('job'+#9+'time'+#9+'information'+#9+{colA}''+#9+{ColB}''+#9+{colC}'');
      //CacheLog.WinTrace.setColumnsWidth({job}'40' +#9+{Time}'130' + #9 + {info}'80' + #9 + {colA}'100' + #9 + {ColB}'130' + #9 + {colC}'130');
      CacheLog.WinTrace.setColumnsTitle('job'+#9+'time'+#9+'Level'+#9+'information'+#9+{colA}''+#9+{ColB}''+#9+{colC}'');
      CacheLog.WinTrace.setColumnsWidth({job}'40' +#9+{Time}'130' + #9 + {Level} '40' + #9 + {info}'80' + #9 + {colA}'100' + #9 + {ColB}'130' + #9 + {colC}'130');
      CacheLog.WinTrace.LinkToPlugin (PlugName, CST_PLUG_ONACTION + CST_PLUG_ONTIMER) ; // attach the window to the plugin (itself)

      CacheLog.WinTrace.DisableResource (CST_ACTION_LABEL_LOGFILE) ; // disable the  LogFile label
      CacheLog.WinTrace.CreateResource (106,CST_RES_BUT_RIGHT,  200 ,'Connect');
      CacheLog.WinTrace.CreateResource (104,CST_RES_LABEL_RIGHT,400,'');
      CacheLog.WinTrace.CreateResource (101,CST_RES_BUT_RIGHT  ,80 ,'Clear content');
      CacheLog.WinTrace.CreateResource (102,CST_RES_BUT_RIGHT  ,50 ,'Reload');
      CacheLog.WinTrace.CreateResource (103,CST_RES_BUT_RIGHT  ,80 ,'Start polling!!!');   // by default, server polling is not started
      CacheLog.WinTrace.CreateResource (105,CST_RES_BUT_RIGHT  ,80 ,'Caché Tools');

      CacheLog.hdbc := THdbc.Create(nil);
      CacheLog.hdbc.Driver := 'InterSystems ODBC' ;
      CacheLog.hdbc.Password := CacheLog.Password ;
      CacheLog.hdbc.Attributes.Add('DRIVER={InterSystems ODBC}'   );
      CacheLog.hdbc.Attributes.Add('PORT=' + CacheLog.Port        );
      CacheLog.hdbc.Attributes.Add('PROTOCOL=TCP'                 );
      CacheLog.hdbc.Attributes.Add('QUERY TIMEOUT=1'              );
      CacheLog.hdbc.Attributes.Add('STATIC CURSORS=1'             );
      CacheLog.hdbc.Attributes.Add('UID=' + CacheLog.User         );
      CacheLog.hdbc.Attributes.Add('SERVER=' + CacheLog.server    );
      CacheLog.hdbc.Attributes.Add('DATABASE=' + CacheLog.Database);
      CacheLog.hdbc.ConnectionPooling := cpOnePerDriver ;

      CacheLog.ds := TOEDataSet.create(nil);
      CacheLog.ds.hDbc := CacheLog.hdbc ;
      CacheLog.ds.hStmt.CursorType := 0 ;
      CacheLog.ds.hStmt.SkipByPosition := True ;
      CacheLog.ds.hStmt.SkipByCursor := True ;
      CacheLog.ds.Table := 'Utility.log' ;
      CacheLog.ds.Editable := false ;

   end;
   if ServersForm.ServerList.Count <> 0 then
      ServersForm.ServerList.ItemIndex := 0 ;
   ServersForm.ServerListClick(nil);  // force display first server
   iniFile.Free ;
   databaseList.Free ;
end ;

//------------------------------------------------------------------------------

procedure checkIniFile() ;
var
   c,d : integer ;
   CacheLog : TCacheLog ;
   databaseList : TStringList ;
   iniFile : tinifile ;
   dllName : array[0..MAX_PATH] of char ;
   dllPath : string ;

   NewServer, NewPort, NewDatabase, NewUser, NewPassword : string ;
   NewMaxConnectionTime : integer ;

begin
   if ServersForm.Visible then
      exit ;
   GetModuleFileName(HInstance, dllName, MAX_PATH-1) ;
   dllPath := ExtractFilePath (dllName) ;
   iniFile := TIniFile.Create (dllPath +'CachePlugin.ini') ;

   databaseList := TStringList.Create() ;
   iniFile.ReadSections(databaseList);

   for c := 0 to databaseList.count-1 do begin
      if databaseList.strings[c] = 'Config' then  // ignore this section
         continue ;

      for d := 0 to CacheLogList.Count - 1 do begin
         CacheLog := TCacheLog(CacheLogList.Items[d]) ;
         if CacheLog.Name = databaseList.strings[c] then begin

            NewServer   := iniFile.ReadString (databaseList[c], 'Server', '');           
            NewPort     := iniFile.ReadString (databaseList[c], 'Port', '1972');         
            NewDatabase := iniFile.ReadString (databaseList[c], 'Database', '');         
            NewUser     := iniFile.ReadString (databaseList[c], 'User', '');             
            NewPassword := iniFile.ReadString (databaseList[c], 'Password', '');
            NewMaxConnectionTime := iniFile.ReadInteger (databaseList[c], 'MaxConnectionTime', 10);

            if (NewServer <> CacheLog.Server) or
               (NewPort <>     CacheLog.Port) or
               (NewDatabase <> CacheLog.Database) or
               (NewUser <>     CacheLog.User) or
               (NewPassword <> CacheLog.Password) or
               (NewMaxConnectionTime <> CacheLog.MaxConnectionTime) then begin

               CacheLog.Server             :=  NewServer ;
               CacheLog.Port               :=  NewPort ;
               CacheLog.Database           :=  NewDatabase ;
               CacheLog.User               :=  NewUser ;
               CacheLog.Password           :=  NewPassword ;
               CacheLog.MaxConnectionTime  :=  NewMaxConnectionTime ;

               // refresh

               CacheLog.ds.Active := false ;
               CacheLog.hdbc.Connected := false ;
               CacheLog.hdbc.Attributes.clear();

               CacheLog.hdbc.Driver := 'InterSystems ODBC' ;
               CacheLog.hdbc.Password := CacheLog.Password ;
               CacheLog.hdbc.Attributes.Add('DRIVER={InterSystems ODBC}'   );
               CacheLog.hdbc.Attributes.Add('PORT=' + CacheLog.Port        );
               CacheLog.hdbc.Attributes.Add('PROTOCOL=TCP'                 );
               CacheLog.hdbc.Attributes.Add('QUERY TIMEOUT=1'              );
               CacheLog.hdbc.Attributes.Add('STATIC CURSORS=1'             );
               CacheLog.hdbc.Attributes.Add('UID=' + CacheLog.User         );
               CacheLog.hdbc.Attributes.Add('SERVER=' + CacheLog.server    );
               CacheLog.hdbc.Attributes.Add('DATABASE=' + CacheLog.Database);
               CacheLog.hdbc.ConnectionPooling := cpOnePerDriver ;

            end;
         end;
      end;
   end;

   iniFile.Free ;
   databaseList.Free ;
end ;

//------------------------------------------------------------------------------

// stop the plugin

procedure Stop() stdcall ;
var
   c : integer ;
   CacheLog : TCacheLog ;
begin

   TTrace.stop ;

   for c := 0 to CacheLogList.Count-1 do begin
      CacheLog := TCacheLog (CacheLogList.Items[c]) ;

      CacheLog.WinTrace := nil ;

      CacheLog.ds.Active := false ;
      CacheLog.ds.free ;

      CacheLog.hdbc.Connected := false ;
      CacheLog.hdbc.free ;

      // free forms
      if CacheLog.cacheToolForm <> nil then
         CacheLog.cacheToolForm.Free ;

   end;
   CacheLogList.Free ;  // delete all TCacheLog
   ServersForm.Free ;    // free server form
end ;

//------------------------------------------------------------------------------
// get the plugin name.
// lpPlugName : buffer where to store the plugin name. Ansi string : One byte per char . limited to 1024 bytes
procedure GetPlugName (lpPlugName: PAnsiChar) stdcall ;
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
function OnAction (WinId : PAnsiChar ; ResourceId : integer; NodeId : PAnsiChar) : BOOL stdcall ;
var
   c : integer ;
   CacheLog : TCacheLog ;
begin
   result := true ; // perform default action

   CacheLog := nil ;
   for c := 0 to CacheLogList.Count-1 do
      if TCacheLog (CacheLogList.Items[c]).Name = WinId then begin           // WinTraceId
         CacheLog := TCacheLog (CacheLogList.Items[c]) ;
         break ;
      end ;

   // 100 : servers
   // Can be used to set polling interval, add server, ...
   if ResourceId = 100 then begin
      ServersForm.Show ;
   end ;

   if CacheLog = nil then
      exit ;

   CacheLog.LastMsgError := '' ;
   try
      // 101 : 'Clear content'
      if ResourceId = 101 then begin
         //if CacheLog.started = false then
         //   exit ;
         CacheLog.WinTrace.ClearAll ;
         CacheLog.LogClear () ;
         CacheLog.LastMsgError := '' ;
         CacheLog.LastReadId := -1 ;
         CacheLog.doTimer() ;
      end ;

      // 102 : 'Reload'
      if ResourceId = 102 then begin
         //if CacheLog.started = false then
         //   exit ;

         CacheLog.Job := 0 ;
         CacheLog.hdbc.Connected := false ;
         CacheLog.CheckConnect() ;
         CacheLog.LastMsgError := '' ;
         CacheLog.WinTrace.ClearAll ;
         CacheLog.LastReadId := -1 ;
         CacheLog.started := true ;   // force pollling
         CacheLog.doTimer() ;
      end ;

      // 106 : connect/disconnect
      if ResourceId = 106 then begin
         if CacheLog.hdbc.Connected then begin
            CacheLog.Job := 0 ;
            CacheLog.hdbc.Connected := false ;
            if CacheLog.started then begin
               CacheLog.started := false ;
               CacheLog.WinTrace.SetTextResource(103,'Start !!!');
            end;
         end else begin  // go to connect don't means start polling
            CacheLog.CheckConnect() ;
         end;
         CacheLog.DoTimer() ;
      end;

      // 103 : start/stop
      if ResourceId = 103 then begin
         if CacheLog.started then begin
            CacheLog.Job := 0 ;
            CacheLog.hdbc.Connected := false ;
            CacheLog.started := false ;
            CacheLog.WinTrace.SetTextResource(103,'Start !!!');
         end else begin  // not started. Start it
            CacheLog.LastMsgError := '' ;
            CacheLog.Job := 0 ;
            CacheLog.hdbc.Connected := false ;
            CacheLog.CheckConnect() ;
            CacheLog.started := true ;
            CacheLog.LastMsgError := '' ;
            CacheLog.WinTrace.SetTextResource(103,'Stop');
         end;
         CacheLog.DoTimer() ;
      end;

      // 105 : Caché Tools
      if ResourceId = 105 then begin

         if CacheLog.cacheToolForm = nil then begin
            //CacheLog.getJob() ;
            CacheLog.cacheToolForm := TCacheToolsForm.Create(nil);
            TCacheToolsForm (CacheLog.cacheToolForm).CacheLog := CacheLog ;
            //CacheLog.cacheToolForm.caption := CacheLog.server +':'+CacheLog.Port+ ' (job : ' + inttostr(CacheLog.Job) + ')' ;

            // add servers to TCacheToolsForm (CacheLog.cacheToolForm).TargetServer
            for c := 0 to CacheLogList.Count-1 do begin
               TCacheToolsForm (CacheLog.cacheToolForm).TargetServer.AddItem(
                  TCacheLog (CacheLogList.Items[c]).Name,
                  TCacheLog (CacheLogList.Items[c])) ;
            end ;

         end ;

         CacheLog.cacheToolForm.Show() ;
      end;
      CacheLog.WinTrace.SetTextResource(104,'');

   except on E: Exception do
      begin
         if CacheLog.LastMsgError <> e.message then begin
            CacheLog.LastMsgError := e.message ;
            CacheLog.WinTrace.send (#9 + DateTimeToStr(now) + #9 + #9 + e.message) ;
            CacheLog.WinTrace.SetTextResource(104,'Error');
         end;
      end;
   end;
end ;

//------------------------------------------------------------------------------
// called when a node is to be deleted on a WinTrace
// The plugin must call LinkToPlugin in order to receive this event
// WinId    : Wintrace Id
// NodeId   : node Id
// return   : when true  : tracetool delete the node
//            when false : tracetool don't delete the node
function OnBeforeDelete (WinId : PAnsiChar ; NodeId : PAnsiChar) : BOOL stdcall ;
begin
   result := true ;
end ;

//------------------------------------------------------------------------------
// called every 500 ms. Can be used for example to refresh labels
// The plugin must call LinkToPlugin in order to receive this event

var LastIniTimer : DWORD ;
var diffTimeIni : DWORD ;
procedure OnTimer() stdcall ;
var
   c : integer ;
   CacheLog : TCacheLog ;
begin

   // check ini file every 2 secondes
   if LastIniTimer = 0 then
      LastIniTimer := GetTickCount() ;

   diffTimeIni := (GetTickCount() - LastIniTimer) div 1000;
   if diffTimeIni > 2 then begin
      checkIniFile() ;
      LastIniTimer := GetTickCount() ;
   end;
   for c := 0 to CacheLogList.Count-1 do begin
      CacheLog := TCacheLog (CacheLogList.Items[c]) ;
      CacheLog.DoTimer();
   end;
end ;


//==============================================================================

{ TCacheLog }

procedure TCacheLog.CheckConnect() ;
begin
   if hdbc.connected = false then begin
      hdbc.connected := true ;
      LastConnectionTime := GetTickCount() ;  // max 49.7 days
      getJob() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TCacheLog.DoTimer() ;
var
   Log_logtime: TStringField;
   Log_information: TStringField;
   Log_Level: TStringField;
   LogTime: string;
   timeLength: Integer;
   difftime: integer;
   LastIdInDb: Integer;
   Log_job: TIntegerField;
   MaxRecord : integer ;
   NewConnectionStatus : string ;
   NewMessageStatus : string ;
begin

   try
      // check for connection time
      if (hdbc.Connected = true) then begin
         if (MaxConnectionTime = 0) then begin
            NewConnectionStatus := 'Disconnect' ;
         end else begin
            difftime := (GetTickCount() - LastConnectionTime) div 60000;
            if difftime > MaxConnectionTime then begin  // MaxConnectionTime is gived in minutes
               // auto disconnect
               hdbc.Connected := false ;
               started := false ;
               Job := 0 ;
               WinTrace.SetTextResource(103,'Start !!!');
               NewConnectionStatus := 'Connect for ' + inttostr(MaxConnectionTime) + ' minute(s)' ;
            end else begin
               NewConnectionStatus := 'Connected for ' + inttostr(MaxConnectionTime-difftime) + ' minute(s). Disconnect'
            end;
         end ;
         if cacheToolForm <> nil then
            cacheToolForm.caption := name + ' / ' + server + ':' + port + ' (job : ' + inttostr(Job) + ')' ;
      end else begin  // not connected. the user can 'Connect'
         if (MaxConnectionTime = 0) then
            NewConnectionStatus := 'Connect'
         else
            NewConnectionStatus := 'Connect for ' + inttostr(MaxConnectionTime) + ' minute(s)' ;
         if cacheToolForm <> nil then
            cacheToolForm.caption := name + ' / ' + server + ':' + port + ' (disconnected)' ;
      end ;

      if NewConnectionStatus <> LastConnectionStatus then begin
         LastConnectionStatus := NewConnectionStatus ;
         WinTrace.SetTextResource(106,NewConnectionStatus) ;
      end;

      difftime := (GetTickCount() - LastQuery);
      if difftime <= PollingTimeOut then
         exit ;

      if started = false then
         exit ;

      CheckConnect() ;

      // 1000 ms
      LastQuery := GetTickCount()  ;
      LastIdInDb := GetLastLogId() ;

      if (LastReadId = -1) and (LastIdInDb <> 0) then
         LastReadId := GetLastBlock() ;


      if (LastIdInDb <> LastReadId) and (LastIdInDb = 0) then
      begin
         WinTrace.ClearAll;
         LastReadId := LastIdInDb;
      end;
      LastMsgError := '';

      // read only 5000 records (MaxRecorsToRead)
      if (LastReadId = -1) then
         if LastReadId < LastIdInDb - MaxRecorsToRead then
            LastReadId := LastIdInDb - MaxRecorsToRead ;

      NewMessageStatus := 'lastDB:'+inttostr(LastIdInDb)+',LastRead:'+inttostr(LastReadId) ;
      if NewMessageStatus <> LastMessageStatus then begin
         LastMessageStatus := NewMessageStatus ;
         WinTrace.SetTextResource(104, NewMessageStatus);
      end;

      // read max 1000 records (PollingRecords) for each timer event (1s).
      // that means that all log records will be read. If 5000 records are in db, theses records will be read in 5 s
      MaxRecord := LastReadId + PollingRecords ;
      if MaxRecord > LastIdInDb then
         MaxRecord := LastIdInDb ;

      if LastIdInDb = -1 then
         exit ;  // table don't exist yet

      ds.Active := false;
      //ds.sql := 'select job,{fn CONVERT(logtime,SQL_VARCHAR)} as logtime,information,LogLevel from Utility.log ' +
      //          'where id > ' + inttostr(LastReadId) + ' and id <= ' + inttostr(MaxRecord) ;
      ds.sql := 'select {fn CONVERT(logtime,SQL_VARCHAR)} as logtime,* from Utility.log ' +
                'where id > ' + inttostr(LastReadId) + ' and id <= ' + inttostr(MaxRecord) ;
      ds.Active := true;
      LastReadId := MaxRecord ; // LastIdInDb;

      //Log_ID          := TIntegerField  (ds.fieldbyname('ID'         )) ; // TIntegerField;
      Log_job         := TIntegerField(ds.fieldbyname('job'));
      Log_logtime     := TStringField (ds.fieldbyname('logtime'));
      Log_information := TStringField (ds.fieldbyname('information'));

      if ds.FindField('LogLevel') = nil then
         Log_Level    := nil
      else
         Log_Level    := TStringField (ds.fieldbyname('LogLevel')) ;

      // TStringField;
      while ds.Eof = false do
      begin
         //WinTrace.send (Log_job.AsString + #9 + FormatDateTime('yyyymmdd hh:mm:ss:zzz',Log_logtime.Value) +#9 + Log_information.Value) ;
         //          1         2
         // 12345678901234567890123
         // 2007-11-07 11:07:53.146
         // 2007-11-07 11:07:53.15     -> last zero is lost
         LogTime := Log_logtime.Value;
         // if the string is formatted as CCYY-MM-DD ... , convert to yymmdd hh:mm:ss.zzz
         if (copy(LogTime, 5, 1) = '-') and (copy(LogTime, 8, 1) = '-') then begin
            LogTime := copy(LogTime, 3, 2) + copy(LogTime, 6, 2) + copy(LogTime, 9, 100);
            timeLength := length(LogTime);
            if timeLength = 16 then
               LogTime := LogTime + '000'    // no millisec
            else if timeLength = 17 then
               LogTime := LogTime + '00'     // no millisec
            else if timeLength = 18 then
               LogTime := LogTime + '0';    // no millisec
         end;

         //ttrace.debug.send(Log_job.AsString + ''#9'' + LogTime + ''#9'' + Log_information.Value);

         if Log_Level = nil then
            WinTrace.debug.send(Log_job.AsString + ''#9'' + LogTime + ''#9'' + '' + ''#9'' + Log_information.Value)
         else
            WinTrace.debug.send(Log_job.AsString + ''#9'' + LogTime + ''#9'' + Log_Level.value + ''#9'' + Log_information.Value);
         //ttrace.flush() ;
         ds.next;
      end;

      ds.Active := false;

   except
      on e: Exception do
      begin
         // stop polling on error
         started := false ;
         WinTrace.SetTextResource(103,'Start !!!');

         if LastMsgError = e.message then
            exit ;
         LastMsgError := e.message;
         WinTrace.send(''#9'' + DateTimeToStr(now) + ''#9'' + ''#9'' + e.message);
         WinTrace.SetTextResource(104, 'Error');
      end;
   end;
end;

//------------------------------------------------------------------------------

function TCacheLog.getClasses(startWidth: string) : integer ;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.LogClasses' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;   // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'startWidth'     , ptInput) ;    // 1 : startWidth
      OEQuery.Params[1].AsString := startWidth ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------


function TCacheLog.getGlobalList: integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.getGlobalList' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;   // 0 : return value
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;

end;

//------------------------------------------------------------------------------

function TCacheLog.LogProcess() : integer ;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.LogProcess' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;   // 0 : return value
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TCacheLog.RunSql(SQL: string; OEQuery : TOEdataset);

begin
   CheckConnect() ;

   //OEQuery := TOEQuery.create(nil) ;
   //try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.SQL := SQL ;

      if pos('select', LowerCase(sql)) = 1 then
         OEQuery.active := true
      else
         OEQuery.ExecSQL ;

//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   //finally
      //OEQuery.free ;
   //end ;
end ;

//------------------------------------------------------------------------------

function TCacheLog.LogQueryPlan(SQL: string) : integer ;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.LogQueryPlan' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;   // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'queryText'     , ptInput) ;    // 1 : queryText
      OEQuery.Params[1].AsString := SQL ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.getGlobalElementCount(GlobalName: string): integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.getGlobalElementCount' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'GlobalName'     , ptInput) ;    // 1 : GlobalName
      OEQuery.Params[1].AsString := GlobalName ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.KillGlobal(GlobalName : string) : integer ;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.KillGlobal' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'GlobalName'     , ptInput) ;    // 1 : GlobalName
      OEQuery.Params[1].AsString := GlobalName ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.LogGlobal(GlobalName, offset: string) : integer ;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.LogGlobal' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'GlobalName'     , ptInput) ;    // 1 : GlobalName
      OEQuery.Params.CreateParam(ftString, 'offset'         , ptInput) ;    // 2 : offset
      OEQuery.Params[1].AsString := GlobalName ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.getQueryPlanCost(SQL: string;var plan : string) : string ;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.QueryPlanCost' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'     , ptResult) ;       // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'queryText'  , ptInput) ;        // 1 : queryText
      OEQuery.Params.CreateParam(ftString, 'plan'       , ptInputOutput) ;  // 2 : plan

      OEQuery.Params[1].AsString := SQL ;
      OEQuery.Params[2].AsString := '' ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsString ;                               // 0 : cost
      plan   := OEQuery.Params[2].AsString ;                               // 2 : plan
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.getJob: integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.GetJob' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;   // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'Dummy'          , ptInput) ;    // 1 : dummy

      OEQuery.Params[1].AsString := '' ;                                    // 1 : dummy
      OEQuery.ExecSQL ;
      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : number of records

      job := result ;

//   except
//      on e : Exception do begin
//          result := e.Message ;
//          //TTrace.error.Send(e.Message) ;
//          //DlgShowCacheException (e, RecselMdiMain.language) ;
//          // result stay to false
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.GetLastLogId() : integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.GetLastLogId' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;   // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'Dummy'          , ptInput) ;    // 1 : dummy

      OEQuery.Params[1].AsString := '' ;                                    // 1 : dummy
      OEQuery.ExecSQL ;
      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : number of records

//   except
//      on e : Exception do begin
//          result := e.Message ;
//          //TTrace.error.Send(e.Message) ;
//          //DlgShowCacheException (e, RecselMdiMain.language) ;
//          // result stay to false
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.GetLastBlock() : integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try try
      result := -1 ;
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.GetLastBlock' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'         , ptResult) ;   // 0 : return value
      OEQuery.Params.CreateParam(ftInteger,'NbRecord'       , ptInput) ;    // 1 : NbRecord
      OEQuery.Params[1].AsInteger := MaxRecorsToRead ;                      // 1 : NbRecord
      OEQuery.ExecSQL ;
      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : number of records

   except
      on e : Exception do begin
      result := -1 ;
//          result := e.Message ;
//          //TTrace.error.Send(e.Message) ;
//          //DlgShowCacheException (e, RecselMdiMain.language) ;
//          // result stay to false
      end ;
   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.LogClass(ClassName, Title: String): integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.LogClass' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'ClassName'     , ptInput) ;     // 1 : ClassName
      OEQuery.Params.CreateParam(ftString, 'Title'         , ptInput) ;     // 2 : Title
      OEQuery.Params[1].AsString := ClassName ;
      OEQuery.Params[2].AsString := Title ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.ReadClassXml(ClassName: String): integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.ReadClassXml' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'classname'     , ptInput) ;     // 1 : ClassName
      OEQuery.Params.CreateParam(ftString, 'basepath'      , ptInput) ;     // 2 : basepath
      OEQuery.Params[1].AsString := ClassName ;
      OEQuery.Params[2].AsString := '' ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.ReadClass(ClassName: String): integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.ReadClass' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'classname'     , ptInput) ;     // 1 : ClassName
      OEQuery.Params.CreateParam(ftString, 'basepath'      , ptInput) ;     // 2 : basepath
      OEQuery.Params[1].AsString := ClassName ;
      OEQuery.Params[2].AsString := '' ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.WriteClass(): integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.WriteClass' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'basepath'      , ptInput) ;     // 1 : basepath
      OEQuery.Params[1].AsString := '' ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

function TCacheLog.WriteClassXml(): integer;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.WriteClassXml' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger,'retVal'        , ptResult) ;    // 0 : return value
      OEQuery.Params.CreateParam(ftString, 'basepath'      , ptInput) ;     // 1 : basepath
      OEQuery.Params[1].AsString := '' ;
      OEQuery.ExecSQL ;

      result := OEQuery.Params[0].AsInteger ;                               // 0 : retVal : job number
//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TCacheLog.LogClear;
var
   OEQuery : TOEQuery ;
begin
   CheckConnect() ;

   OEQuery := TOEQuery.create(nil) ;
   try //try
      OEQuery.hDbc := hdbc ;
      OEQuery.active := false ;
      OEQuery.hStmt.CursorType := SQL_CURSOR_FORWARD_ONLY ;

      OEQuery.StoredProc := 'Utility.LogClear' ;
      OEQuery.Params.Clear ;
      OEQuery.Params.CreateParam(ftInteger, 'keepDays' , ptInput) ;    // 0 : keepDays
      OEQuery.Params[0].AsString := '0' ;                              // 0 : keepDays
      OEQuery.ExecSQL ;

//   except
//      on e : Exception do begin
//          //TTrace.error.Send(e.Message) ;
//      end ;
//   end
   finally
      OEQuery.free ;
   end ;
end;


end.
