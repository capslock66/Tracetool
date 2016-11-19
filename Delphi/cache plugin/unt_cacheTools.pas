unit unt_cacheTools;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls , DB, cachePluginDialog,
  odsi, OVCL,OCIH,
  ExtCtrls, ComCtrls, ToolWin,
  VirtualTrees, Clipbrd, VstSort, GridsEh, DBGridEh, ActnList, ImgList;

{
Sample classes to dump :
   %Library.SerialStream
   %SYSTEM.OBJ
}

type
  TStringLine = class
     caption : string ;
     caption2 : string ;
     bold : boolean ;
     color : TColor ;
     tag : Integer ;
     node : PVirtualNode ;
     constructor create();overload ;
     constructor create(col : string; col2 : string = '');overload ;
  end;

  TCacheProcessInfo = class
     CanBeExamined : string ;          /// Process can be examined
     CanBeSuspended : string ;         /// Process can be suspended
     CanBeTerminated : string ;        /// Process can be terminated
     ClientNodeName : string ;         /// Node Name of the client connected to the process
     CurrentDevice : string ;          /// Current Device ($i)
     CurrentLineAndRoutine : string ;  /// Current Line and Routine<br> Returned in +number^routine format
     CurrentSrcLine : string ;         /// Current Source Line
     ClientExecutableName : string ;   /// Executable Name of the process connected on the client
     GlobalReferences : string ;       /// Number of Global References
     InTransaction : string ;          /// In a transaction
     ClientIPAddress : string ;        /// IP Address of client connected to the process
     IsGhost : string ;                ///  Is a Ghost process
     JobNumber : string ;              /// Job number in process table
     JobType : string ;                /// Job type
     LastGlobalReference : string ;    /// Last Global Reference
     LinesExecuted : string ;          /// Number of Lines Executed
     Location : string ;               /// system processes name the value of $g(^%IS(0,Job.CurrentDevice),"")
     MemoryUsed : string ;             /// Memory used in KB (Current $s)
     NameSpace : string ;              /// Namespace executing in
     OpenDevices : string ;            /// Comma "," delimited list of open devices
     //OSInfo : string ;                 /// OSInfo of client connected to process
     Pid : string ;                    /// Process ID, Hex on VMS
     PidInternal : string ;            /// Process PID in decimal
     Priority : string ;               /// Priority
     Routine : string ;                /// Routine currently executing
     State : string ;                  /// State of process
     Switch10 : string ;               /// Process Owns switch 10
     UserName : string ;               /// Username of process
     UIC : string ;                    /// UIC (in display format)
  end;


  TCacheToolsForm = class(TForm)
    PanelTop: TPanel;
    ListBoxClasses: TListBox;
    PanelClassDetail: TPanel;
    Label2: TLabel;
    EditClass: TEdit;
    butDisplayClass: TButton;
    butLoadClass: TButton;
    rbXml: TRadioButton;
    rbCDL: TRadioButton;
    memClass: TMemo;
    MemoResult: TMemo;
    Splitter1: TSplitter;
    PanelTarget: TPanel;
    LabelTargetServer: TLabel;
    TargetServer: TComboBox;
    ButSaveClass: TButton;
    PageControl: TPageControl;
    tsClasses: TTabSheet;
    tsSQL: TTabSheet;
    tsProcess: TTabSheet;
    ToolBarProcess: TToolBar;
    ButGetProcessInfo: TButton;
    vstProcess: TVirtualStringTree;
    tsGlobal: TTabSheet;
    Panel1: TPanel;
    EditGlobal: TEdit;
    butGlobal: TButton;
    Splitter3: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    vstGlobalsList: TVirtualStringTree;
    butRefreshGlobals: TButton;
    query: TOEDataSet;
    ds_query: TDataSource;
    ServerSchema: TOESchema;
    Panel4: TPanel;
    MemoSQL: TMemo;
    Splitter2: TSplitter;
    vstQueryPlan: TVirtualStringTree;
    Splitter4: TSplitter;
    EhGrid: TDBGridEh;
    Splitter5: TSplitter;
    Panel5: TPanel;
    VstCatalog: TVirtualStringTree;
    Panel6: TPanel;
    butGetCatalog: TButton;
    VstCatalogDetail: TVirtualStringTree;
    Splitter6: TSplitter;
    vstGlobals: TVirtualStringTree;
    ToolBarSql: TToolBar;
    Button1: TButton;
    ActionList1: TActionList;
    ActionSqlRun: TAction;
    ImgList: TImageList;
    ActionQueryPlan: TAction;
    ToolBar1: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Panel7: TPanel;
    Label1: TLabel;
    EditStartWith: TEdit;
    butGetAllClasses: TButton;
    butKillGlobal: TButton;
    butGlobalCount: TButton;
    LabelElementCount: TLabel;
    procedure butGetAllClassesClick(Sender: TObject);
    procedure ListBoxClassesClick(Sender: TObject);
    procedure butDisplayClassClick(Sender: TObject);
    procedure butLoadClassClick(Sender: TObject);
    procedure ButSaveClassClick(Sender: TObject);
    procedure PanelClassDetailDblClick(Sender: TObject);
    procedure ButGetQueryPlanCostClick(Sender: TObject);
    procedure ButGetProcessInfoClick(Sender: TObject);
    procedure vstProcessFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstProcessGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure FormCreate(Sender: TObject);
    procedure vstQueryPlanFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstQueryPlanGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure butCopyQueryPlanToClipBoardClick(Sender: TObject);
    procedure vstQueryPlanPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure butGlobalClick(Sender: TObject);
    procedure vstGlobalsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstGlobalsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGlobalsListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstGlobalsListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure butRefreshGlobalsClick(Sender: TObject);
    procedure vstGlobalsListChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstGlobalsListDblClick(Sender: TObject);
    procedure ListBoxClassesDblClick(Sender: TObject);
    procedure butGetCatalogClick(Sender: TObject);
    procedure VstCatalogDblClick(Sender: TObject);
    procedure VstCatalogChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstCatalogDetailDblClick(Sender: TObject);
    procedure ActionSqlRunExecute(Sender: TObject);
    procedure ActionQueryPlanExecute(Sender: TObject);
    procedure vstQueryPlanDblClick(Sender: TObject);
    procedure butKillGlobalClick(Sender: TObject);
    procedure butGlobalCountClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CacheLog : TCacheLog ;
    ProcessSorter : TVstSort ;
    function getSql: string;
  end;

var
  CacheToolsForm: TCacheToolsForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TCacheToolsForm.FormCreate(Sender: TObject);
var
   col : TVirtualTreeColumn ;
begin
   PageControl.ActivePage := tsSql ;

   VstCatalogDetail.TreeOptions.AutoOptions  := VstCatalogDetail.TreeOptions.AutoOptions
             + [toAutoSpanColumns] ;            // Large entries continue into next columns

   // Process sorter
   ProcessSorter                        := TVstSort.create(self) ;
   ProcessSorter.tree                   := VstProcess ;
   ProcessSorter.UtilityImages          := ServersForm.UtilityImages ;
   VstProcess.header.Options            := VstProcess.header.Options + [hoOwnerDraw] ;
   VstProcess.onHeaderClick             := ProcessSorter.onHeaderClick ;
   VstProcess.onKeyUp                   := ProcessSorter.OnKeyUp ;
   VstProcess.onHeaderDrawQueryElements := ProcessSorter.OnHeaderDrawQueryElements ;
   VstProcess.onAdvancedHeaderDraw      := ProcessSorter.OnAdvancedHeaderDraw ;

   vstProcess.Header.Columns.Clear ;
   col := vstProcess.Header.Columns.Add ; col.Width := 160 ; col.Text := 'NameSpace' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 100 ; col.Text := 'UserName' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  90 ; col.Text := 'ClientIPAddress' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 120 ; col.Text := 'ClientExecutableName' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  60 ; col.Text := 'State' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  70 ; col.Text := 'JobNumber' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  58 ; col.Text := 'JobType' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 100 ; col.Text := 'GlobalReferences' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 150 ; col.Text := 'ClientNodeName' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'InTransaction' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  90 ; col.Text := 'CanBeExamined' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 100 ; col.Text := 'CanBeSuspended' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 100 ; col.Text := 'CanBeTerminated' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'CurrentDevice' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'CurrentLineAndRoutine' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  90 ; col.Text := 'CurrentSrcLine' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  55 ; col.Text := 'IsGhost' ;
   col := vstProcess.Header.Columns.Add ; col.Width := 150 ; col.Text := 'LastGlobalReference' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  90 ; col.Text := 'LinesExecuted' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'Location' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'MemoryUsed' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'OpenDevices' ;
   //col := vstProcess.Header.Columns.Add ; col.Width :=  60 ; col.Text := 'OSInfo' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  40 ; col.Text := 'Pid' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  70 ; col.Text := 'PidInternal' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  50 ; col.Text := 'Priority' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  80 ; col.Text := 'Routine' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  60 ; col.Text := 'Switch10' ;
   col := vstProcess.Header.Columns.Add ; col.Width :=  40 ; col.Text := 'UIC' ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   // trees are owners of datas
   vstProcess.clear ;
   vstQueryPlan.clear ;
   vstGlobalsList.clear ;
   vstGlobals.clear ;
   vstCatalog.Clear ;
   VstCatalogDetail.Clear ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butGetAllClassesClick(Sender: TObject);
var
   Log_information: TStringField;
   job : integer ;
begin
   try
      CacheLog.CheckConnect() ;

      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      ListBoxClasses.Clear ;

      job := CacheLog.getClasses (EditStartWith.text) ;
      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      while CacheLog.ds.Eof = false do
      begin
         ListBoxClasses.Items.Add(Log_information.Value) ;
         CacheLog.ds.next;
      end;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
   except
      on e: Exception do
      begin
         if CacheLog.LastMsgError = e.message then
            exit ;
         CacheLog.LastMsgError := e.message;
      end;
   end;

end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.ListBoxClassesClick(Sender: TObject);
begin
   if ListBoxClasses.ItemIndex = -1 then
      exit ;
   if rbXml.checked then
      EditClass.Text := ListBoxClasses.Items[ListBoxClasses.ItemIndex] + '.cls'
   else
      EditClass.Text := ListBoxClasses.Items[ListBoxClasses.ItemIndex] ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.ListBoxClassesDblClick(Sender: TObject);
begin
   butLoadClassClick(nil);
end;

//------------------------------------------------------------------------------

//var
//   dblClickCount : integer ;

procedure TCacheToolsForm.PanelClassDetailDblClick(Sender: TObject);
begin
//   inc (dblClickCount) ;
//   if dblClickCount >= 3 then
//      PanelTarget.Visible := true ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butDisplayClassClick(Sender: TObject);
var
   Log_information: TStringField;
   job : integer ;
   ClassName : string ;
   p : integer ;
begin
   try
      CacheLog.CheckConnect() ;

      memClass.Clear ;
      ClassName := editclass.Text ;
      p := pos('.cls',ClassName) ;
      if p > 0 then
         ClassName := Copy(ClassName, 0 , p - 1);

      job := CacheLog.LogClass (ClassName) ;
      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      while CacheLog.ds.Eof = false do
      begin
         memClass.Lines.Add(Log_information.Value) ;
         CacheLog.ds.next;
      end;
      memClass.SelStart := 0;
      memClass.SelLength := 0 ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
   except
      on e: Exception do
      begin
         if CacheLog.LastMsgError = e.message then
            exit ;
         CacheLog.LastMsgError := e.message;
      end;
   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butLoadClassClick(Sender: TObject);
var
   Log_information: TStringField;
   job : integer ;
begin
   try
      CacheLog.CheckConnect() ;

      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      memClass.Clear ;
      MemoResult.Clear ;

      if rbXml.checked then
         job := CacheLog.ReadClassXml (editclass.Text)
      else
         job := CacheLog.ReadClass (editclass.Text) ;

      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      while CacheLog.ds.Eof = false do
      begin
         memClass.Lines.Add(Log_information.Value) ;
         CacheLog.ds.next;
      end;
      memClass.SelStart := 0;
      memClass.SelLength := 0 ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
   except
      on e: Exception do
      begin
         if CacheLog.LastMsgError = e.message then
            exit ;
         CacheLog.LastMsgError := e.message;
      end;
   end;
end;


//------------------------------------------------------------------------------

procedure TCacheToolsForm.ButSaveClassClick(Sender: TObject);
var
   c : integer ;
   job : integer ;
   Log_information: TStringField;
   target : TCacheLog ;
begin

   MemoResult.Clear ;
   if TargetServer.ItemIndex = -1 then begin
      MemoResult.Lines.add('Select a target server') ;
      exit ;
   end;

   target := TCacheLog (TargetServer.Items.Objects [TargetServer.ItemIndex]) ;
   target.CheckConnect() ;
   job := target.job ;

   target.ds.Active := false;
   target.ds.sql := 'delete from Utility.Log where job = ' + inttostr(job) ;
   target.ds.ExecSQL ;

   for c := 0 to memClass.Lines.Count-1 do begin
      if(memClass.Lines[c] = '') then
         target.ds.sql := 'insert into Utility.Log (job,information) values (' + inttostr(job) + ',' + QuotedStr('(null)')+')'
      else
         target.ds.sql := 'insert into Utility.Log (job,information) values (' + inttostr(job) + ',' +QuotedStr(memClass.Lines[c])+')';
      target.ds.ExecSQL ;
   end;

   if rbXml.checked then
      job := Target.WriteClassXml()
   else
      job := Target.WriteClass() ;

   Target.ds.Active := false;
   Target.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
   Target.ds.Active := true;

   // read result
   MemoResult.Clear ;
   Log_information := TStringField(Target.ds.fieldbyname('information'));
   while Target.ds.Eof = false do
   begin
      MemoResult.Lines.Add(Log_information.Value) ;
      Target.ds.next;
   end;
   // delete result comments
   Target.ds.Active := false;
   Target.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
   Target.ds.ExecSQL ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butGlobalClick(Sender: TObject);
var
   Log_information: TStringField;
   Log_Level: TIntegerField;
   job : integer ;
   info : string ;
   level : integer ;
   StringLine : TStringLine ;
   UpperStringLine : TStringLine ;
   index : integer ;
   Stack : TStringList ;
   node : PVirtualNode ;
begin
   if EditGlobal.text = '' then
      exit ;
   
   // ^RecSel.sVerD
   Stack := TStringList.Create ;
   vstGlobals.clear ;
   vstGlobals.BeginUpdate ;
   Screen.Cursor := crHourglass;
   try try
      CacheLog.CheckConnect() ;
      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      job := CacheLog.LogGlobal(EditGlobal.text,'') ;
      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select LogLevel,information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      Log_Level       := TIntegerField(CacheLog.ds.fieldbyname('LogLevel'));
      while CacheLog.ds.Eof = false do
      begin
         StringLine := TStringLine.create ;
         // get global name and value
         info := Log_information.Value ;
         info := TrimLeft(info) ;
         index := pos (#9 , info) ;
         if index = 0 then begin
            StringLine.caption  := info ;
            StringLine.caption2 := '' ;
         end else begin
            StringLine.caption  := copy (info, 0 , index-1) ;
            StringLine.caption2 := copy (info, index+1 , 1000) ;
         end;

         level := Log_Level.Value ;
         // search the loglevel in stack (node level 10 is under node level 9)
         index := stack.IndexOf(inttostr(level-1)) ;
         if index = -1 then  // the unique main node (global name)
            node := vstGlobals.AddChild(nil, StringLine)
         else begin
            UpperStringLine := TStringLine (stack.Objects[index]) ;
            node := vstGlobals.AddChild(UpperStringLine.node, StringLine) ;
            // all TStringLine AFTER the index must be deleted in the stack
            while stack.Count -1 > index do
               stack.Delete(stack.Count -1);

         end;
         StringLine.node := node ;
         // add current to stack
         if level = 0 then
            stack.Clear ;
         stack.AddObject(inttostr(level),StringLine ) ;

         CacheLog.ds.next;
      end;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
   except
      on e: Exception do
      begin
         vstGlobals.AddChild(nil, TStringLine.create (e.message)) ;
      end;
   end;
   finally
      Stack.Free ;
      Screen.Cursor := crDefault;
      vstGlobals.EndUpdate ;
      vstGlobals.FullExpand() ;
   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butGlobalCountClick(Sender: TObject);
begin
   try
      CacheLog.CheckConnect() ;
      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      LabelElementCount.Caption := inttostr(CacheLog.getGlobalElementCount(EditGlobal.text)) ;
   except
      on e: Exception do
      begin
         vstGlobals.AddChild(nil, TStringLine.create (e.message)) ;
      end;
   end;

end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butKillGlobalClick(Sender: TObject);
begin
   if (MessageDlg('Are you sure to delete this global : ' + EditGlobal.text, mtConfirmation, [mbYes, mbNo], 0) in [mrNo, mrNone]) then
      exit ;
   try
      CacheLog.CheckConnect() ;

      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      CacheLog.KillGlobal(EditGlobal.text) ;
   except
      on e: Exception do
      begin
         vstGlobals.AddChild(nil, TStringLine.create (e.message)) ;
      end;
   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstGlobalsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   StringLine.free ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstGlobalsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   if Column = 0 then
      CellText := StringLine.caption
   else
      CellText := StringLine.caption2 ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.butRefreshGlobalsClick(Sender: TObject);
var
   Log_information: TStringField;
   job : integer ;
begin
   // ^RecSel.sVerD
   vstGlobalsList.clear ;
   vstGlobals.clear ;
   try
      CacheLog.CheckConnect() ;
      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      job := CacheLog.getGlobalList() ;
      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      while CacheLog.ds.Eof = false do
      begin
         vstGlobalsList.AddChild(nil, TStringLine.create (Log_information.Value)) ;
         CacheLog.ds.next;
      end;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
   except
      on e: Exception do
      begin
         vstGlobals.AddChild(nil, TStringLine.create (e.message)) ;
      end;
   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstGlobalsListChange(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   if pos('^', StringLine.caption) <> 0 then
      EditGlobal.Text := StringLine.caption
   else
      EditGlobal.Text := '' ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstGlobalsListDblClick(Sender: TObject);
begin
   butGlobalClick(nil);
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstGlobalsListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   StringLine.free ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstGlobalsListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   CellText := StringLine.caption ;
end;

//------------------------------------------------------------------------------

function TCacheToolsForm.getSql : string ;
var
   size : integer ;
begin
   size := length(MemoSQL.text) ;
   setlength (result,size  + 2) ;
   size := MemoSQL.GetSelTextBuf (pchar(result),size );
   if size = 0 then
      result := MemoSQL.text
   else begin
      setlength (result,size) ;
   end;
end ;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.VstCatalogChange(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
   d : integer ;
   StringLine : TStringLine ;
   SchemaProcedure : TSchemaProcedure ;
   SchemaTable : TSchemaTable ;
   SchemaColumn : TSchemaColumn ;
   ParametersNode, ColumnsNode , IndexesNode, PrimaryKeyNode, UniqueKeyNode : PVirtualNode ;

begin
   VstCatalogDetail.clear ;

   if node = nil then
      exit ;
   StringLine := TStringLine (VstCatalog.GetNodeData(Node)^) ;

   if StringLine.tag = 0 then
      exit ;

   if TObject(StringLine.tag) is TSchemaTable then begin
      SchemaTable := TSchemaTable(TObject(StringLine.tag)) ;

      if SchemaTable.ViewSQL <> ''  then
         VstCatalogDetail.AddChild(nil, TStringLine.create ('ViewSQL ' , SchemaTable.ViewSQL));

      // property Columns: TSchemaColumns read GetColumns write SetColumns;
      if SchemaTable.Columns.Count <> 0 then begin
         ColumnsNode := VstCatalogDetail.AddChild(nil, TStringLine.create ('Columns'));
         for d := 0 to SchemaTable.Columns.Count - 1 do begin
            VstCatalogDetail.AddChild(ColumnsNode, TStringLine.create (
              SchemaTable.Columns[d].ColumnName ,
              SchemaTable.Columns[d].DataTypeName
              + ' ( ' + inttostr(schemaTable.Columns[d].Precision) + ')'
              ));
         end;
      end;

      // property Indexes: TSchemaIndexes read GetIndexes write SetIndexes;
      if SchemaTable.Indexes.Count <> 0 then begin
         IndexesNode := VstCatalogDetail.AddChild(nil, TStringLine.create ('Indexes'));
         for d := 0 to SchemaTable.Indexes.Count - 1 do begin
            //Columns[e].Primary
            //Columns[e].Unique
            VstCatalogDetail.AddChild(IndexesNode, TStringLine.create (
              SchemaTable.Indexes[d].IndexName ,
              SchemaTable.Indexes[d].Columns.CommaText
              ));
         end;
      end;

      // property PrimaryKey: TStrings read GetPrimaryKey;
      if SchemaTable.PrimaryKey.Count <> 0 then begin
         PrimaryKeyNode := VstCatalogDetail.AddChild(nil, TStringLine.create ('PrimaryKey'));
         for d := 0 to SchemaTable.PrimaryKey.Count - 1 do begin
            VstCatalogDetail.AddChild(PrimaryKeyNode, TStringLine.create (
              SchemaTable.PrimaryKey[d]
              ));
         end;
      end;

      // property UniqueKey: TStrings read GetUniqueKey;
      if SchemaTable.UniqueKey.Count <> 0 then begin
         UniqueKeyNode := VstCatalogDetail.AddChild(nil, TStringLine.create ('UniqueKey'));
         for d := 0 to SchemaTable.UniqueKey.Count - 1 do begin
            VstCatalogDetail.AddChild(UniqueKeyNode, TStringLine.create (
              SchemaTable.UniqueKey[d]
              ));
         end;
      end;

   end else if TObject(StringLine.tag) is TSchemaProcedure then begin
      SchemaProcedure := TSchemaProcedure(TObject(StringLine.tag)) ;

      // show the procedure name without parameters
      VstCatalogDetail.AddChild(nil, TStringLine.create (SchemaProcedure.ProcedureOwner + '.' + SchemaProcedure.ProcedureName)) ;
      ColumnsNode := nil ;
      ParametersNode := nil ;
      for d := 0 to SchemaProcedure.Columns.Count - 1 do begin
         SchemaColumn := SchemaProcedure.Columns[d] ;

         case SchemaColumn.ColumnType of
            SQL_RESULT_COL			   : // A result set column.
               begin
                  if ColumnsNode = nil then
                     ColumnsNode := VstCatalogDetail.AddChild(nil, TStringLine.create('columns')) ;
                  VstCatalogDetail.AddChild(ColumnsNode, TStringLine.create (SchemaColumn.ColumnName , SchemaColumn.DataTypeName)) ;
               end;
            SQL_RETURN_VALUE			: // The return value of a procedure.
               begin
                  VstCatalogDetail.AddChild(nil, TStringLine.create ('Return value' , SchemaColumn.DataTypeName)) ;
               end;
            SQL_PARAM_INPUT			, // An input parameter of a procedure.
            SQL_PARAM_OUTPUT			, // An output parameter of a procedure.
            SQL_PARAM_INPUT_OUTPUT	, // An input/output parameter of a procedure.
            SQL_PARAM_TYPE_UNKNOWN	: // A parameter of a procedure of which it is not possible to determine if it is an input, output or input/output parameter.
               begin
                  if ParametersNode = nil then
                     ParametersNode := VstCatalogDetail.AddChild(nil, TStringLine.create ('Parameters')) ;

                  StringLine := TStringLine.create ();
                  if SchemaColumn.ColumnType = SQL_PARAM_INPUT then
                     StringLine.caption := SchemaColumn.ColumnName
                  else
                     StringLine.caption := 'ByRef ' + SchemaColumn.ColumnName ;
                  StringLine.caption2 := SchemaColumn.DataTypeName ;

                  VstCatalogDetail.AddChild(ParametersNode, StringLine) ;
               end;
         end;
      end;
   end;
   VstCatalogDetail.FullExpand() ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.VstCatalogDblClick(Sender: TObject);
var
   StringLine : TStringLine ;
   Node : PVirtualNode ;
   text : string ;
begin
   Node := VstCatalog.GetFirstSelected() ;
   if Node = nil then
      exit ;

   // don't copy if children
   if node.FirstChild <> nil then
      exit ;

   if node.Parent = VstCatalog.RootNode then
      exit ;

   StringLine := TStringLine (VstCatalog.GetNodeData(Node)^) ;
   text := StringLine.caption ;

   node := node.Parent ;
   StringLine := TStringLine (VstCatalog.GetNodeData(Node)^) ;
   text := StringLine.caption + '.' + text ;

   MemoSQL.SetSelTextBuf(PAnsiChar(text));
   MemoSQL.setfocus() ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.VstCatalogDetailDblClick(Sender: TObject);
var
   StringLine : TStringLine ;
   Node : PVirtualNode ;
   text : string ;
begin
   Node := VstCatalogDetail.GetFirstSelected() ;
   if Node = nil then
      exit ;

   // don't copy if children
   if node.FirstChild <> nil then
      exit ;

   StringLine := TStringLine (VstCatalogDetail.GetNodeData(Node)^) ;
   if VstCatalogDetail.FocusedColumn = 0 then
      text := StringLine.caption
   else
      text := StringLine.caption2 ;

   MemoSQL.SetSelTextBuf(PAnsiChar(text));
   MemoSQL.setfocus() ;
end;

//------------------------------------------------------------------------------


procedure TCacheToolsForm.ActionSqlRunExecute(Sender: TObject);
var
   sql : string ;
   time1 : DWORD ;
   difftime: Double;
begin
   vstQueryPlan.clear ;
   sql := getSql() ;
   try
      time1 := GetTickCount() ;
      CacheLog.RunSql(sql, query) ;   // check for connection
      difftime := (GetTickCount - time1);
      vstQueryPlan.AddChild(nil, TStringLine.create ('Done in ' + FloatToStr (difftime) + ' ms')) ;
   except
      on e: Exception do
      begin
         vstQueryPlan.AddChild(nil, TStringLine.create (e.message) ) ;
      end;

   end;
end;


//------------------------------------------------------------------------------

procedure TCacheToolsForm.butGetCatalogClick(Sender: TObject);
var
   c,d : integer ;
   TablesAndViews : PVirtualNode ;
   Procedures : PVirtualNode;
   SchemaProcedure : TSchemaProcedure ;
   SchemaColumn : TSchemaColumn ;
   StringLine : TStringLine ;
   TableOwnerString : string ;
   TableOwnerNode : PVirtualNode ;
   RESULT_COL			   : string ; // A result set column.
   RETURN_VALUE			: string ; // The return value of a procedure.
   PARAM_INPUT_OUTPUT	: string ; // param in or/and out
   ColumnName : string ;
begin
   CacheLog.CheckConnect() ;
   Screen.Cursor := crHourglass;
   try try
      vstQueryPlan.clear ;
      ServerSchema.hDbc := CacheLog.hdbc ;
      ServerSchema.GetTables(False, [ttTable, ttView, ttSystem]);   // OnDemand , TableType

      TablesAndViews := VstCatalog.AddChild(nil, TStringLine.create ('Tables and views') ) ;
      for c  := 0 to ServerSchema.Tables.Count - 1 do begin
         // search Owner node
         TableOwnerString := ServerSchema.Tables[c].TableOwner ;
         TableOwnerNode := TablesAndViews.FirstChild ;
         while (TableOwnerNode <> nil) do begin
            StringLine := TStringLine (VstCatalog.GetNodeData(TableOwnerNode)^) ;
            if StrIComp (Pchar(StringLine.caption) , Pchar(TableOwnerString)) = 0 then
               break ;  // owner if found : stop search
            TableOwnerNode := TableOwnerNode.NextSibling ;
         end;
         // if not found add Owner
         if TableOwnerNode = nil then
            TableOwnerNode := VstCatalog.AddChild(TablesAndViews, TStringLine.create (TableOwnerString)) ;

         StringLine := TStringLine.create(ServerSchema.Tables[c].TableName) ;
         StringLine.tag := integer(ServerSchema.Tables[c]) ;
         VstCatalog.AddChild(TableOwnerNode, StringLine) ;
      end;
      VstCatalog.Expanded[TablesAndViews] := true ;

      ServerSchema.GetProcedures(False);
      Procedures := VstCatalog.AddChild(nil, TStringLine.create ('Procedures')) ;
      for c  := 0 to ServerSchema.Procedures.Count - 1 do begin
         SchemaProcedure := ServerSchema.Procedures[c] ;
         // search Owner node
         TableOwnerString := SchemaProcedure.ProcedureOwner ;
         TableOwnerNode := Procedures.FirstChild ;
         while (TableOwnerNode <> nil) do begin
            StringLine := TStringLine (VstCatalog.GetNodeData(TableOwnerNode)^) ;
            if StrIComp (Pchar(StringLine.caption) , Pchar(TableOwnerString)) = 0 then
               break ;  // owner if found : stop search
            TableOwnerNode := TableOwnerNode.NextSibling ;
         end;
         // if not found add Owner
         if TableOwnerNode = nil then
            TableOwnerNode := VstCatalog.AddChild(Procedures, TStringLine.create (TableOwnerString)) ;
         // add procedure under Owner node
         StringLine := TStringLine.create ;
         StringLine.tag := integer(SchemaProcedure) ;

         RESULT_COL			   := '' ; // A result set column.
         RETURN_VALUE			:= '' ; // The return value of a procedure.
         PARAM_INPUT_OUTPUT	:= '' ; // An input/output parameter of a procedure.

         for d := 0 to SchemaProcedure.Columns.Count - 1 do begin
            SchemaColumn := SchemaProcedure.Columns[d] ;

            //TSchemaColumn.ColumnType
            case SchemaColumn.ColumnType of
               SQL_RESULT_COL			   : // A result set column.
                  begin
                     if RESULT_COL = '' then
                        RESULT_COL := SchemaColumn.ColumnName
                     else
                        RESULT_COL := RESULT_COL + ', ' + SchemaColumn.ColumnName ;
                     RESULT_COL := RESULT_COL + ' as ' + SchemaColumn.DataTypeName ;
                  end;
               SQL_RETURN_VALUE			: // The return value of a procedure.
                  begin
                     // only one value can be returned...
                     // return parameter name is not important ('RetVal')
                     if RETURN_VALUE = '' then
                        RETURN_VALUE := SchemaColumn.DataTypeName
                     else
                        RETURN_VALUE := RETURN_VALUE + ', ' + SchemaColumn.DataTypeName ;

                  end;
               SQL_PARAM_INPUT			, // An input parameter of a procedure.
               SQL_PARAM_OUTPUT			, // An output parameter of a procedure.
               SQL_PARAM_INPUT_OUTPUT	, // An input/output parameter of a procedure.
               SQL_PARAM_TYPE_UNKNOWN	: // A parameter of a procedure of which it is not possible to determine if it is an input, output or input/output parameter.
                  begin
                     if SchemaColumn.ColumnType = SQL_PARAM_INPUT then
                        ColumnName := SchemaColumn.ColumnName
                     else
                        ColumnName := 'ByRef ' + SchemaColumn.ColumnName ;

                     if PARAM_INPUT_OUTPUT = '' then
                        PARAM_INPUT_OUTPUT := ColumnName
                     else
                        PARAM_INPUT_OUTPUT := PARAM_INPUT_OUTPUT + ', ' + ColumnName ;
                     PARAM_INPUT_OUTPUT := PARAM_INPUT_OUTPUT + ' As ' + SchemaColumn.DataTypeName ;
                  end;
            end;
         end;


         if PARAM_INPUT_OUTPUT = '' then
            StringLine.caption := SchemaProcedure.ProcedureName + '()'
         else
            StringLine.caption := SchemaProcedure.ProcedureName + '(' + PARAM_INPUT_OUTPUT + ')' ;

         if RETURN_VALUE <> '' then
            StringLine.caption := StringLine.caption + ' as ' + RETURN_VALUE ;

         if RESULT_COL <> '' then
            StringLine.caption := StringLine.caption + ' Columns : ' + RESULT_COL ;

         VstCatalog.AddChild(TableOwnerNode, StringLine) ;

      end;
      VstCatalog.Expanded[Procedures] := true ;
   except
      on e: Exception do
      begin
         vstQueryPlan.AddChild(nil, TStringLine.create (e.message)) ;
      end;
   end;
   finally
      Screen.Cursor := crDefault;
   end;
end;


//------------------------------------------------------------------------------

{
<plan>
<sql>
select * from recsel.dcan

</sql>
<cost value="22255200"/>
Read master map RecSel.DCAN.IDKEY, looping on ID.
For each row:
    Output the row.
</plan>
}

procedure TCacheToolsForm.ActionQueryPlanExecute(Sender: TObject);
var
   Log_information: TStringField;
   job : integer ;
   StringLine : TStringLine ;
begin
   vstQueryPlan.clear ;
   try
      CacheLog.CheckConnect() ;
      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      job := CacheLog.LogQueryPlan(getSql()) ;
      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      while CacheLog.ds.Eof = false do
      begin
         StringLine := TStringLine.create (Log_information.Value) ;

         if pos ('Read master map', StringLine.caption) > 0 then
            if pos (', looping on ID.', StringLine.caption) > 0 then
               StringLine.color := clRed ;

         vstQueryPlan.AddChild(nil, StringLine) ;
         CacheLog.ds.next;
      end;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
   except
      on e: Exception do
      begin
         vstQueryPlan.AddChild(nil, TStringLine.create (e.message)) ;
      end;
   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstQueryPlanPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   if not (vsSelected in Node.States) then
      TargetCanvas.Font.Color := StringLine.color ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstQueryPlanDblClick(Sender: TObject);
var
   StringLine : TStringLine ;
   Node : PVirtualNode ;
begin
   Node := vstQueryPlan.GetFirstSelected() ;
   if Node = nil then
      exit ;

   StringLine := TStringLine (vstQueryPlan.GetNodeData(Node)^) ;

   ShowMessage(StringLine.caption) ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstQueryPlanFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   StringLine.free ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstQueryPlanGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
   StringLine : TStringLine ;
begin
   if Node = nil then
      exit ;
   StringLine := TStringLine (Sender.GetNodeData(Node)^) ;
   if Column = 0 then
      CellText := StringLine.caption
   else
      CellText := StringLine.caption2 ;
end;


//------------------------------------------------------------------------------


procedure TCacheToolsForm.butCopyQueryPlanToClipBoardClick(Sender: TObject);
var
   Node : PVirtualNode ;
   StringLine : TStringLine ;
   CopyStrings : TStringList;
   CopyText: PChar;
begin
   CopyStrings := TStringList.Create;
   Node := vstQueryPlan.GetFirst() ;
   while Node <> nil do begin
      StringLine := TStringLine (vstQueryPlan.GetNodeData(Node)^) ;
      CopyStrings.Add(StringLine.caption) ;
      Node := vstQueryPlan.GetNext(Node);
   end ;

   CopyText := CopyStrings.GetText;
   try
      Clipboard.SetTextBuf(CopyText);
   finally
      StrDispose(CopyText);
   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.ButGetQueryPlanCostClick(Sender: TObject);
//var
//   cost,plan : string ;
begin
//   MemoPlan.clear ;
//   try
//      cost := CacheLog.getQueryPlanCost(getSql(),plan) ;
//      MemoPlan.Text := plan ;
//   except
//      on e: Exception do
//      begin
//         MemoPlan.Text := e.message ;
//      end;
//   end;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.ButGetProcessInfoClick(Sender: TObject);
var
   Log_information: TStringField;
   job : integer ;
   p : integer ;
   PropName, PropValue : string ;
   CacheProcessInfo : TCacheProcessInfo ;
begin
   try
      CacheLog.CheckConnect() ;
      // ensure old traces are deleted
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(CacheLog.job);
      CacheLog.ds.ExecSQL ;

      job := CacheLog.LogProcess() ;
      CacheLog.LastMsgError := '' ;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'select information from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.Active := true;

      vstProcess.clear ;

      CacheProcessInfo := nil ;
      Log_information := TStringField(CacheLog.ds.fieldbyname('information'));
      while CacheLog.ds.Eof = false do
      begin

         // detect property and value, separated by a tab
         p := pos (#9 , Log_information.Value) ;
         if p = 0 then begin
            PropName := Log_information.Value ;
            PropValue := '' ;
         end else begin
            PropName  := copy (Log_information.Value, 0 , p-1) ;
            PropValue := copy (Log_information.Value, p+1 , 1000) ;
         end;

         if PropName = '---' then begin
            vstProcess.AddChild(nil, CacheProcessInfo) ;   // owner
            CacheProcessInfo := nil ;
         end else if CacheProcessInfo = nil then
            CacheProcessInfo := TCacheProcessInfo.create ;

         if PropName = 'CanBeExamined'              then CacheProcessInfo.CanBeExamined           := PropValue ;
         if PropName = 'CanBeSuspended'             then CacheProcessInfo.CanBeSuspended          := PropValue ;
         if PropName = 'CanBeTerminated'            then CacheProcessInfo.CanBeTerminated         := PropValue ;
         if PropName = 'ClientNodeName'             then CacheProcessInfo.ClientNodeName          := PropValue ;
         if PropName = 'CurrentDevice'              then CacheProcessInfo.CurrentDevice           := PropValue ;
         if PropName = 'CurrentLineAndRoutine'      then CacheProcessInfo.CurrentLineAndRoutine   := PropValue ;
         if PropName = 'CurrentSrcLine'             then CacheProcessInfo.CurrentSrcLine          := PropValue ;
         if PropName = 'ClientExecutableName'       then CacheProcessInfo.ClientExecutableName    := PropValue ;
         if PropName = 'GlobalReferences'           then CacheProcessInfo.GlobalReferences        := PropValue ;
         if PropName = 'InTransaction'              then CacheProcessInfo.InTransaction           := PropValue ;
         if PropName = 'ClientIPAddress'            then CacheProcessInfo.ClientIPAddress         := PropValue ;
         if PropName = 'IsGhost'                    then CacheProcessInfo.IsGhost                 := PropValue ;
         if PropName = 'JobNumber'                  then CacheProcessInfo.JobNumber               := PropValue ;
         if PropName = 'JobType'                    then CacheProcessInfo.JobType                 := PropValue ;
         if PropName = 'LastGlobalReference'        then CacheProcessInfo.LastGlobalReference     := PropValue ;
         if PropName = 'LinesExecuted'              then CacheProcessInfo.LinesExecuted           := PropValue ;
         if PropName = 'Location'                   then CacheProcessInfo.Location                := PropValue ;
         if PropName = 'MemoryUsed'                 then CacheProcessInfo.MemoryUsed              := PropValue ;
         if PropName = 'NameSpace'                  then CacheProcessInfo.NameSpace               := PropValue ;
         if PropName = 'OpenDevices'                then CacheProcessInfo.OpenDevices             := PropValue ;
         //if PropName = 'OSInfo'                     then CacheProcessInfo.OSInfo                  := PropValue ;
         if PropName = 'Pid'                        then CacheProcessInfo.Pid                     := PropValue ;
         if PropName = 'PidInternal'                then CacheProcessInfo.PidInternal             := PropValue ;
         if PropName = 'Priority'                   then CacheProcessInfo.Priority                := PropValue ;
         if PropName = 'Routine'                    then CacheProcessInfo.Routine                 := PropValue ;
         if PropName = 'State'                      then CacheProcessInfo.State                   := PropValue ;
         if PropName = 'Switch10'                   then CacheProcessInfo.Switch10                := PropValue ;
         if PropName = 'UserName'                   then CacheProcessInfo.UserName                := PropValue ;
         if PropName = 'UIC'                        then CacheProcessInfo.UIC                     := PropValue ;

         CacheLog.ds.next;
      end;
      CacheLog.ds.Active := false;
      CacheLog.ds.sql := 'delete from Utility.log where job = ' + inttostr(job);
      CacheLog.ds.ExecSQL ;
      ProcessSorter.Sort(nil);
//      vstProcess.SortTree(0);
   except
      on e: Exception do
      begin
         //MemoPlan.Text := e.message ;
      end;
   end;
end ;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstProcessFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   CacheProcessInfo : TCacheProcessInfo ;
begin
   if Node = nil then
      exit ;
   CacheProcessInfo := TCacheProcessInfo (Sender.GetNodeData(Node)^) ;
   CacheProcessInfo.free ;
end;

//------------------------------------------------------------------------------

procedure TCacheToolsForm.vstProcessGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;var CellText: WideString);
var
   CacheProcessInfo : TCacheProcessInfo ;
   col : TVirtualTreeColumn ;
begin
   if Node = nil then
      exit ;
   CacheProcessInfo := TCacheProcessInfo (Sender.GetNodeData(Node)^) ;

   col := vstProcess.Header.Columns[Column] ;

   // maybee slow to check column name, but used rarely.
   // the advantage is to be independant of column index

   if col.Text = 'CanBeExamined'          then CellText := CacheProcessInfo.CanBeExamined ;
   if col.Text = 'CanBeSuspended'         then CellText := CacheProcessInfo.CanBeSuspended ;
   if col.Text = 'CanBeTerminated'        then CellText := CacheProcessInfo.CanBeTerminated ;
   if col.Text = 'ClientNodeName'         then CellText := CacheProcessInfo.ClientNodeName ;
   if col.Text = 'CurrentDevice'          then CellText := CacheProcessInfo.CurrentDevice ;
   if col.Text = 'CurrentLineAndRoutine'  then CellText := CacheProcessInfo.CurrentLineAndRoutine ;
   if col.Text = 'CurrentSrcLine'         then CellText := CacheProcessInfo.CurrentSrcLine ;
   if col.Text = 'ClientExecutableName'   then CellText := CacheProcessInfo.ClientExecutableName ;
   if col.Text = 'GlobalReferences'       then CellText := CacheProcessInfo.GlobalReferences ;
   if col.Text = 'InTransaction'          then CellText := CacheProcessInfo.InTransaction ;
   if col.Text = 'ClientIPAddress'        then CellText := CacheProcessInfo.ClientIPAddress ;
   if col.Text = 'IsGhost'                then CellText := CacheProcessInfo.IsGhost ;
   if col.Text = 'JobNumber'              then CellText := CacheProcessInfo.JobNumber ;
   if col.Text = 'JobType'                then CellText := CacheProcessInfo.JobType ;
   if col.Text = 'LastGlobalReference'    then CellText := CacheProcessInfo.LastGlobalReference ;
   if col.Text = 'LinesExecuted'          then CellText := CacheProcessInfo.LinesExecuted ;
   if col.Text = 'Location'               then CellText := CacheProcessInfo.Location ;
   if col.Text = 'MemoryUsed'             then CellText := CacheProcessInfo.MemoryUsed ;
   if col.Text = 'NameSpace'              then CellText := CacheProcessInfo.NameSpace ;
   if col.Text = 'OpenDevices'            then CellText := CacheProcessInfo.OpenDevices ;
   //if col.Text = 'OSInfo'                 then CellText := CacheProcessInfo.OSInfo ;
   if col.Text = 'Pid'                    then CellText := CacheProcessInfo.Pid ;
   if col.Text = 'PidInternal'            then CellText := CacheProcessInfo.PidInternal ;
   if col.Text = 'Priority'               then CellText := CacheProcessInfo.Priority ;
   if col.Text = 'Routine'                then CellText := CacheProcessInfo.Routine ;
   if col.Text = 'State'                  then CellText := CacheProcessInfo.State ;
   if col.Text = 'Switch10'               then CellText := CacheProcessInfo.Switch10 ;
   if col.Text = 'UserName'               then CellText := CacheProcessInfo.UserName ;
   if col.Text = 'UIC'                    then CellText := CacheProcessInfo.UIC ;

end;

{ TStringLine }

constructor TStringLine.create;
begin
   caption := '' ;
   caption2 := '' ;
   bold := false ;
   color := clBlack;
   tag := 0 ;
end;

constructor TStringLine.create(col: string; col2 : string = '');
begin
   caption := col ;
   caption2 := col2 ;
   bold := false ;
   color := clBlack;
   tag := 0 ;
end;

end.



