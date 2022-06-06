{
  Display all the options of the server

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit DebugOptions;

interface

uses
  Windows, Messages, SysUtils, AnsiStrings,
  Classes, Graphics, Controls, Forms, Dialogs, Contnrs,
  StdCtrls, ComCtrls, variants, CheckLst, ExtCtrls, ImgList
  , VirtualTrees
  ,ColorPickerButton
  ,unt_TraceConfig
  ;

const
  UM_MEASUREFONTS = WM_USER;
  szSizeArray = 16;
  PointSizes: array[0..szSizeArray-1] of Integer =
                   (8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72);

type

  //------------------------------------------------------------------------------

  TFontSpec = class
     FontName : string ;
     lfCharSet : BYTE ;
     FontType: integer ;
     SizeList : TObjectList ;  // not owner : integer array
  end ;

  // "Save to text file" operations
  TSaveTofileOptions = class
    Copy_ProcessName      : boolean ;   // save process name in clipboard export
    Copy_ThreadID         : boolean ;   // save thread id
    Copy_Time             : boolean ;   // save time
    Copy_Col1             : boolean ;   // save tree column
    Copy_Col2             : boolean ;   // save comment
    Copy_ColumnTitle      : boolean ;   // include column title in clipboard export
  end ;

  //------------------------------------------------------------------------------

  TfrmDebugOptions = class(TForm)
    Panel1: TPanel;
    PanelRight: TPanel;
    Panel2: TPanel;
    btnOK: TButton;
    butApply: TButton;
    btnCancel: TButton;
    PageControl: TPageControl;
    TabSheetGeneral: TTabSheet;
    PnlGeneral: TPanel;
    TabSheetFramework: TTabSheet;
    PnlFramework: TPanel;
    Label19: TLabel;
    chkShowMainformMenu: TCheckBox;
    EditMainTrace: TEdit;
    TabSheetODS: TTabSheet;
    PnlODS: TPanel;
    Label9: TLabel;
    Label10: TLabel;
    Label18: TLabel;
    chkAutoClearODS: TCheckBox;
    editMaxNodesODS: TEdit;
    EditMinNodesODS: TEdit;
    EditODSTabsheetTitle: TEdit;
    chkOdsVisible: TCheckBox;
    TabSheetTail: TTabSheet;
    PnlTail: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    chkAutoClearTail: TCheckBox;
    editMaxNodesTail: TEdit;
    EditMinNodesTail: TEdit;
    EditTailFileSize: TEdit;
    chkTailVisible: TCheckBox;
    TabSheetEventLog: TTabSheet;
    PnlEventLog: TPanel;
    chkEventLogVisible: TCheckBox;
    TabSheetClipboard: TTabSheet;
    PnlClipboard: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    chkProcessName: TCheckBox;
    ChkThreadID: TCheckBox;
    ChkTime: TCheckBox;
    EditSeparator: TEdit;
    ChkTree: TCheckBox;
    ChkComment: TCheckBox;
    rbSingle: TRadioButton;
    rbDouble: TRadioButton;
    rbNone: TRadioButton;
    chkIncludeTitle: TCheckBox;
    EditIndentation: TEdit;
    TabSheetPlugins: TTabSheet;
    PnlPlugins: TPanel;
    VSTOptions: TVirtualStringTree;
    ButNewWin32Plugin: TButton;
    ButNewDotNetPlugin: TButton;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    pnlWatches: TPanel;
    chkShowMainWatchesMenu: TCheckBox;
    Label21: TLabel;
    EditMainWatches: TEdit;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label13: TLabel;
    EditDebugTitle: TEdit;
    chkShowOnStartup: TCheckBox;
    chkShowOnMessage: TCheckBox;
    chkFocus_OnMessage: TCheckBox;
    chkMinmizeSystray: TCheckBox;
    EditIconFile: TEdit;
    GroupBox3: TGroupBox;
    ChkSmallIcons: TCheckBox;
    chkStandardToolbar: TCheckBox;
    chkSearchToolbar: TCheckBox;
    chkBookmarkToolbar: TCheckBox;
    chkFilterToolbar: TCheckBox;
    Label23: TLabel;
    GroupBox1: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    EditSocketPort: TEdit;
    chkWarningSocket: TCheckBox;
    GroupBox4: TGroupBox;
    chkEnableInternalLog: TCheckBox;
    GroupBox5: TGroupBox;
    Label24: TLabel;
    Label25: TLabel;
    GroupBox6: TGroupBox;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    GroupBox7: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    GroupBox8: TGroupBox;
    Label30: TLabel;
    Label31: TLabel;
    GroupBox9: TGroupBox;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    GroupBox10: TGroupBox;
    Label36: TLabel;
    Label37: TLabel;
    GroupBox11: TGroupBox;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    GroupBox12: TGroupBox;
    Label42: TLabel;
    Label43: TLabel;
    GroupBox13: TGroupBox;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    GroupBox14: TGroupBox;
    Label48: TLabel;
    Label49: TLabel;
    FrameworkTraceFonts: TComboBox;
    FrameworkInfoFonts: TComboBox;
    WatchesTraceFonts: TComboBox;
    WatchesInfoFonts: TComboBox;
    OdsTraceFonts: TComboBox;
    OdsInfoFonts: TComboBox;
    TailTraceFonts: TComboBox;
    TailInfoFonts: TComboBox;
    EvntLogTraceFonts: TComboBox;
    EvntLogInfoFonts: TComboBox;
    FrameworkTraceNodeHeight: TEdit;
    FrameworkInfoNodeHeight: TEdit;
    WatchesTraceNodeHeight: TEdit;
    OdsTraceNodeHeight: TEdit;
    TailTraceNodeHeight: TEdit;
    EvntLogTraceNodeHeight: TEdit;
    FrameworkTraceFontSize: TComboBox;
    FrameworkInfoFontSize: TComboBox;
    WatchesTraceFontSize: TComboBox;
    WatchesInfoFontSize: TComboBox;
    OdsTraceFontSize: TComboBox;
    OdsInfoFontSize: TComboBox;
    TailTraceFontSize: TComboBox;
    TailInfoFontSize: TComboBox;
    EvntLogTraceFontSize: TComboBox;
    EvntLogInfoFontSize: TComboBox;
    Label26: TLabel;
    Label32: TLabel;
    EditHTTPPort: TEdit;
    TabSheet2: TTabSheet;
    PnlFrameworkDelete: TPanel;
    chkAutoClearTraces: TCheckBox;
    editMaxNodesTraces: TEdit;
    EditMinNodesTraces: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rbCreateOnRoot: TRadioButton;
    Label38: TLabel;
    rbCreateUnderLostAndFound: TRadioButton;
    Label44: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    DefaultLeftText: TEdit;
    DefaultRightText: TEdit;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    LostAndFoundLeftText: TEdit;
    LostAndFoundRightText: TEdit;
    Label55: TLabel;
    rbAddChildrenOnRoot: TRadioButton;
    rbAddChildrenUnderLostAndFound: TRadioButton;
    Label56: TLabel;
    EditSocketPort2: TEdit;
    Label57: TLabel;
    chkSocketPolicyServer: TCheckBox;
    chkHttpPolicyServer: TCheckBox;
    Label58: TLabel;
    Label59: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    chkUdp1: TCheckBox;
    chkUdp2: TCheckBox;
    Label60: TLabel;
    Label61: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure butApplyClick(Sender: TObject);
    procedure VSTOptionsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VSTOptionsChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure btnCancelClick(Sender: TObject);
    procedure ButNewWin32PluginClick(Sender: TObject);
    procedure ButNewDotNetPluginClick(Sender: TObject);
    procedure VSTOptionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure FontsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FontsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    //XMLConfig : IXMLConfig ;

    FTrueTypeBMP: TBitmap;
    FDeviceBMP: TBitmap;
    FontList : TStringList ;
    Procedure InitFonts( );
  public

    VstPlugNode : PVirtualNode ;

    LastPlugPath : string ;

    procedure ShowCurrentConfig();
    procedure InitPlugins();
    procedure FillPlugins();
  end;

var
  frmDebugOptions: TfrmDebugOptions;

implementation

uses Registry, Unt_Tool, unt_Tail, unt_ods, unt_TraceWin,
  unt_utility, unt_plugin , unt_FrmPlugin,
  unt_PageContainer , unt_base ;

{$R *.dfm}
{$R TTF.RES}

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.FormCreate(Sender: TObject);
var
   frameworkNode : PVirtualNode ;
begin
   FTrueTypeBMP := TBitmap.Create;
   try
      FTrueTypeBMP.LoadFromResourceName(hInstance, 'TRUETYPE_BMPFNT');
   except
      FTrueTypeBMP.Free;
      FTrueTypeBMP := nil;
   end;

   FDeviceBMP := TBitmap.Create;
   try
      FDeviceBMP.LoadFromResourceName(hInstance, 'DEVICE_BMPFNT');
   except
      FDeviceBMP.Free;
      FDeviceBMP := nil;
   end;

   VSTOptions.NodeDataSize := 4 ;

   //VSTOptions.TreeOptions.SelectionOptions := Frm_Trace.vstTrace.TreeOptions.SelectionOptions ;
   //VSTOptions.TreeOptions.AutoOptions      := Frm_Trace.vstTrace.TreeOptions.AutoOptions ;
   //VSTOptions.TreeOptions.MiscOptions      := Frm_Trace.vstTrace.TreeOptions.MiscOptions ;
   //VSTOptions.TreeOptions.PaintOptions     := Frm_Trace.vstTrace.TreeOptions.PaintOptions  - [toShowRoot] ; // don't show Root

   VSTOptions.Clear ;
   VSTOptions.AddChild(nil, PnlGeneral) ;
   frameworkNode := VSTOptions.AddChild(nil, PnlFramework) ;
   VSTOptions.AddChild(frameworkNode, PnlFrameworkDelete) ;
   VSTOptions.Expanded[frameworkNode] := true ;
   VSTOptions.AddChild(nil, pnlWatches) ;
   VSTOptions.AddChild(nil, PnlODS) ;
   VSTOptions.AddChild(nil, PnlTail) ;
   VSTOptions.AddChild(nil, PnlEventLog) ;
   VSTOptions.AddChild(nil, PnlClipBoard) ;
   VstPlugNode := VSTOptions.AddChild(nil, PnlPlugins) ;

   PnlODS       .parent := PanelRight ;
   PnlTail      .parent := PanelRight ;
   PnlEventLog  .parent := PanelRight ;
   PnlClipBoard .parent := PanelRight ;
   PnlFramework .parent := PanelRight ;
   PnlFrameworkDelete.parent := PanelRight ;
   pnlWatches   .parent := PanelRight ;
   PnlPlugins   .parent := PanelRight ;
   PnlGeneral   .parent := PanelRight ;

   PnlGeneral.BringToFront ;
   PageControl.Visible := false ;


   //FillPlugins() ;
   //VSTOptionsChange (VSTOptions, VSTOptions.GetFirst) ;
   //ShowCurrentConfig () ;

end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.FormDestroy(Sender: TObject);
var
   c : integer ;
   FontDetail : TFontSpec ;
begin
   FTrueTypeBMP.Free;
   FTrueTypeBMP := nil;
   FDeviceBMP.Free;
   FDeviceBMP := nil;
   if FontList <> nil then begin
      for c := 0 to FontList.Count-1 do begin
         FontDetail := TFontSpec (FontList.Objects[c]) ;
         FontDetail.SizeList.Free ;  // not owner : integer array
         FontDetail.Free ;
      end ;
      FontList.Clear ;
      FontList.free ;
   end;
end;

//------------------------------------------------------------------------------

// called once by timer after init
procedure TfrmDebugOptions.InitPlugins ;
var
   plugin : TPlugin ;
   DotNetPlugin : TDotNetPlugin ;
   c : integer ;
begin
   TraceConfig.DotNetManager := nil ;

   for c := 0 to TraceConfig.PluginList.Count-1 do begin
      plugin := TPlugin(TraceConfig.PluginList[c]) ;

      if plugin.startup = false then
         continue ;

      if plugin is TDotNetPlugin then begin
         DotNetPlugin := TDotNetPlugin(plugin) ;
         try
            // add the plugin to the dot net wrapper list
            if TraceConfig.DotNetManager <> nil then  // check is not necessary, but resolve unassigned variable warning
               TraceConfig.DotNetManager.DoCheckPlugInfile(DotNetPlugin);
         except
            on e : exception do begin
               TFrm_Trace.InternalTraceFromThread ('InitPlugins : call DotNetManager.DoCheckPlugInfile() : ' + e.Message) ;
               //continue ;
            end ;
         end ;
      end;

      plugin.DoLoad ;   // load , get name
      if plugin.status <> psLoaded then
         continue ;
      plugin.Dostart(pAnsiString(plugin.param)) ;

      if TraceConfig.DebugMode = true then
         TFrm_Trace.InternalTraceFromThread ('plugin ' + string(Plugin.PlugName) + ' Loaded and started');
   end ;

end ;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.FillPlugins ;
var
   c : integer ;
   Plugin : TPlugin ;
begin
   VSTOptions.DeleteChildren(VstPlugNode);
   for c := 0 to TraceConfig.PluginList.Count-1 do begin
      Plugin := TPlugin (TraceConfig.PluginList.Items[c]) ;
      Plugin.frmPlugin.parent := PanelRight ;
      TfrmPlugin(Plugin.frmPlugin).node := VSTOptions.AddChild(VstPlugNode, Plugin) ;
   end ;
   VSTOptions.Expanded [VstPlugNode] := true ;
end ;

//------------------------------------------------------------------------------

// called by FormCreate() or when the user click cancel
// TraceConfig to form component
procedure TfrmDebugOptions.ShowCurrentConfig ;
var
   c : integer ;
   Plugin : TPlugin ;
begin

   InitFonts() ;

   // fonts name
   for c := 0 to FontList.Count-1 do begin
      if FrameworkTraceFonts.items[c] = TraceConfig.Framework_Trace_FontName then
         FrameworkTraceFonts.ItemIndex := c ;
      if FrameworkInfoFonts.items[c] = TraceConfig.Framework_Info_FontName then
         FrameworkInfoFonts.ItemIndex := c ;

      if WatchesTraceFonts.items[c] = TraceConfig.Watches_Trace_FontName then
         WatchesTraceFonts.ItemIndex := c ;
      if WatchesInfoFonts.items[c] = TraceConfig.Watches_Info_FontName then
         WatchesInfoFonts.ItemIndex := c ;

      if OdsTraceFonts.items[c] = TraceConfig.Ods_Trace_FontName then
         OdsTraceFonts.ItemIndex := c ;
      if OdsInfoFonts.items[c] = TraceConfig.Ods_Info_FontName then
         OdsInfoFonts.ItemIndex := c ;

      if TailTraceFonts.items[c] = TraceConfig.Tail_Trace_FontName then
         TailTraceFonts.ItemIndex := c ;
      if TailInfoFonts.items[c] = TraceConfig.Tail_Info_FontName then
         TailInfoFonts.ItemIndex := c ;

      if EvntLogTraceFonts.items[c] = TraceConfig.EventLog_Trace_FontName then
         EvntLogTraceFonts.ItemIndex := c ;
      if EvntLogInfoFonts.items[c] = TraceConfig.EventLog_Info_FontName then
         EvntLogInfoFonts.ItemIndex := c ;
   end ;

   // font size and node height
   FrameworkTraceFontSize.Text   := intToStr(TraceConfig.Framework_Trace_FontSize) ;
   FrameworkTraceNodeHeight.Text := intToStr(TraceConfig.Framework_Trace_NodeHeight) ;
   FrameworkInfoFontSize.Text    := intToStr(TraceConfig.Framework_Info_FontSize) ;
   FrameworkInfoNodeHeight.Text  := intToStr(TraceConfig.Framework_Info_NodeHeight) ;

   WatchesTraceFontSize.Text     := intToStr(TraceConfig.Watches_Trace_FontSize) ;
   WatchesTraceNodeHeight.Text   := intToStr(TraceConfig.Watches_Trace_NodeHeight) ;
   WatchesInfoFontSize.Text      := intToStr(TraceConfig.Watches_Info_FontSize) ;
   //WatchesInfoNodeHeight.Text    := intToStr(XMLConfig.Watches.Info.NodeHeight.Value) ;

   OdsTraceFontSize.Text         := intToStr(TraceConfig.Ods_Trace_FontSize) ;
   OdsTraceNodeHeight.Text       := intToStr(TraceConfig.Ods_Trace_NodeHeight) ;
   OdsInfoFontSize.Text          := intToStr(TraceConfig.Ods_Info_FontSize) ;

   TailTraceFontSize.Text        := intToStr(TraceConfig.Tail_Trace_FontSize) ;
   TailTraceNodeHeight.Text      := intToStr(TraceConfig.Tail_Trace_NodeHeight) ;
   TailInfoFontSize.Text         := intToStr(TraceConfig.Tail_Info_FontSize) ;

   EvntLogTraceFontSize.Text     := intToStr(TraceConfig.EventLog_Trace_FontSize) ;
   EvntLogTraceNodeHeight.Text   := intToStr(TraceConfig.EventLog_Trace_NodeHeight) ;
   EvntLogInfoFontSize.Text      := intToStr(TraceConfig.EventLog_Info_FontSize) ;

   // framework
   chkAutoClearTraces.Checked  := TraceConfig.Framework_AutoClear ;
   editMaxNodesTraces.Text     := intToStr (TraceConfig.Framework_MaxNode) ;
   EditMinNodesTraces.Text     := intToStr (TraceConfig.Framework_MinNode) ;
   chkShowMainformMenu.Checked := TraceConfig.Framework_VisibleMenu ;
   EditMainTrace.Text          := TraceConfig.Framework_MainTraceTitle ;  // 'Traces'

   if TraceConfig.Framework_Orphans_DeletedNode = 'CreateOnRoot' then
      rbCreateOnRoot.Checked := true
   else if TraceConfig.Framework_Orphans_DeletedNode = 'CreateUnderLostAndFound' then
      rbCreateUnderLostAndFound.Checked := true
   else if TraceConfig.Framework_Orphans_DeletedNode = 'AddChildrenOnRoot' then
      rbAddChildrenOnRoot.Checked := true
   else if TraceConfig.Framework_Orphans_DeletedNode = 'AddChildrenUnderLostAndFound' then
      rbAddChildrenUnderLostAndFound.Checked := true ;

   DefaultLeftText.Text        := TraceConfig.Framework_Orphans_DefaultLeftText ;
   DefaultRightText.Text       := TraceConfig.Framework_Orphans_DefaultRightText ;
   LostAndFoundLeftText.Text   := TraceConfig.Framework_Orphans_LostAndFoundLeftText ;
   LostAndFoundRightText.Text  := TraceConfig.Framework_Orphans_LostAndFoundRightText ;

   // watches
   chkShowMainWatchesMenu.Checked := TraceConfig.Watches_VisibleMenu ;
   EditMainWatches.Text           := TraceConfig.Watches_MainWatchesTitle ;  // 'Watches'

   // EventLog
   chkEventLogVisible.Checked := TraceConfig.EventLog_VisibleMenu ;

   // ods
   chkAutoClearODS.Checked    := TraceConfig.ods_AutoClear ;
   editMaxNodesODS.Text       := intToStr (TraceConfig.ods_MaxNode) ;
   EditMinNodesODS.Text       := intToStr (TraceConfig.ods_MinNode) ;
   EditODSTabsheetTitle.Text  := TraceConfig.ods_Title ;
   chkOdsVisible.Checked      := TraceConfig.ods_VisibleMenu;

   // tail
   chkAutoClearTail.Checked   := TraceConfig.tail_AutoClear ;
   editMaxNodesTail.Text      := intToStr (TraceConfig.tail_MaxNode) ;
   EditMinNodesTail.Text      := intToStr (TraceConfig.tail_MinNode) ;
   chkTailVisible.Checked     := TraceConfig.tail_VisibleMenu ;
   EditTailFileSize.Text      := IntToStr(TraceConfig.tail_SizeToLoad) ;

   // general
   EditIconFile.Text          := TraceConfig.AppDisplay_IconFile ;
   EditSocketPort.Text        := IntToStr (TraceConfig.General_SocketPort);
   EditSocketPort2.Text       := IntToStr (TraceConfig.General_SocketPort2);
   chkUdp1.Checked            := TraceConfig.General_Udp1 ;
   chkUdp2.Checked            := TraceConfig.General_Udp2 ;
   EditHTTPPort.Text          := IntToStr (TraceConfig.General_HTTPPort);
   chkWarningSocket.Checked   := TraceConfig.General_ShowSocketWarning ;
   chkSocketPolicyServer.Checked := TraceConfig.General_SocketPolicyServer ;
   chkHttpPolicyServer.Checked   := TraceConfig.General_HttpPolicyServer ;

   EditDebugTitle.text        := TraceConfig.AppDisplay_ApplicationTitle ;
   ChkSmallIcons.Checked      := TraceConfig.AppDisplay_smallBut ;
   chkStandardToolbar.Checked := TraceConfig.AppDisplay_ToolbarStandard ;
   chkSearchToolbar.Checked   := TraceConfig.AppDisplay_ToolbarSearch ;
   chkBookmarkToolbar.Checked := TraceConfig.AppDisplay_ToolbarBookmark ;
   chkFilterToolbar.Checked   := TraceConfig.AppDisplay_ToolbarFilter ;

   chkMinmizeSystray.Checked  := TraceConfig.AppDisplay_MinimizeToSystray ;
   chkShowOnStartup.Checked   := TraceConfig.AppDisplay_ShowOnstartup ;
   chkEnableInternalLog.Checked := not TraceConfig.AppDisplay_DisableInternalLog ;

   chkFocus_OnMessage.Checked := TraceConfig.AppDisplay_FocusToReceivedMessage ;
   chkShowOnMessage.Checked   := TraceConfig.AppDisplay_ShowOnMessageReceived ;


   ChkProcessName.Checked     := TraceConfig.TextExport_ProcessName ;
   ChkThreadID.Checked        := TraceConfig.TextExport_ThreadID ;
   ChkTime.Checked            := TraceConfig.TextExport_Time ;
   ChkTree.Checked            := TraceConfig.TextExport_col1 ;
   ChkComment.Checked         := TraceConfig.TextExport_col2 ;
   ChkIncludeTitle.Checked    := TraceConfig.TextExport_GenerateColumnHeader ;
   EditSeparator.Text         := TraceConfig.TextExport_Separator ;
   EditIndentation.text       := IntTostr(TraceConfig.TextExport_TreeIndentation) ;

   if TraceConfig.TextExport_TextQualifier = ''   then rbNone.Checked   := true ;
   if TraceConfig.TextExport_TextQualifier = '''' then rbSingle.Checked := true ;
   if TraceConfig.TextExport_TextQualifier = '"'  then rbDouble.Checked := true ;

   // plugins

   for c := 0 to TraceConfig.PluginList.Count-1 do begin
      Plugin := TPlugin (TraceConfig.PluginList.Items[c]) ;
      TfrmPlugin(Plugin.frmPlugin).chkLoadAtStartup.checked  := plugin.startup ;
      TfrmPlugin(Plugin.frmPlugin).MemoParam.Text := string(Plugin.Param) ;
   end ;

end ;

//------------------------------------------------------------------------------

// form component to TraceConfig then save
procedure TfrmDebugOptions.butApplyClick(Sender: TObject);
var
   c : integer ;
   FrmPageContainer : TFrmPageContainer ;
   Plugin : TPlugin ;
   FrmBase : TFrmBase ;
begin
   // framework
   TraceConfig.Framework_AutoClear        := chkAutoClearTraces.Checked ;
   TraceConfig.Framework_MaxNode          := StrToIntDef(editMaxNodesTraces.Text,2000) ;
   TraceConfig.Framework_MinNode          := StrToIntDef(EditMinNodesTraces.Text,1000) ;
   TraceConfig.Framework_MainTraceTitle   := EditMainTrace.Text ;  // 'Traces'
   TraceConfig.Framework_VisibleMenu      := chkShowMainformMenu.Checked ;
   frm_tool.mnuViewMainTraces.Visible := TraceConfig.Framework_VisibleMenu ;

   if rbCreateOnRoot.Checked = true then
      TraceConfig.Framework_Orphans_DeletedNode := 'CreateOnRoot'
   else if rbCreateUnderLostAndFound.Checked = true then
      TraceConfig.Framework_Orphans_DeletedNode := 'CreateUnderLostAndFound'
   else if rbAddChildrenOnRoot.Checked = true then
      TraceConfig.Framework_Orphans_DeletedNode := 'AddChildrenOnRoot'
   else if rbAddChildrenUnderLostAndFound.Checked = true then
      TraceConfig.Framework_Orphans_DeletedNode := 'AddChildrenUnderLostAndFound' ;

   TraceConfig.Framework_Orphans_DefaultLeftText       := DefaultLeftText.Text  ;
   TraceConfig.Framework_Orphans_DefaultRightText      := DefaultRightText.Text ;
   TraceConfig.Framework_Orphans_LostAndFoundLeftText  := LostAndFoundLeftText.Text   ;
   TraceConfig.Framework_Orphans_LostAndFoundRightText := LostAndFoundRightText.Text  ;

   Frm_trace.Caption := EditMainTrace.Text ;      // main Frm_Trace form

   // watches
   TraceConfig.Watches_MainWatchesTitle := EditMainWatches.Text ;    // 'Watches'
   Frm_Watches.Caption                  := EditMainWatches.Text ;

   TraceConfig.Watches_VisibleMenu      := chkShowMainWatchesMenu.Checked ;
   frm_tool.mnuViewMainWatches.Visible := TraceConfig.Watches_VisibleMenu ;

   // EventLog
   TraceConfig.EventLog_VisibleMenu     := chkEventLogVisible.Checked ;
   frm_tool.mnuEventlog.Visible         := TraceConfig.EventLog_VisibleMenu ;

   // ods
   TraceConfig.ods_AutoClear            := chkAutoClearODS   .Checked ;
   TraceConfig.ods_MaxNode              := StrToIntDef(editMaxNodesODS   .Text,2000) ;
   TraceConfig.ods_MinNode              := StrToIntDef(EditMinNodesODS   .Text,1000) ;

   TraceConfig.ods_Title                := EditODSTabsheetTitle.Text ;
   Frm_ODS.Caption                      := TraceConfig.Ods_Title ;

   TraceConfig.ods_VisibleMenu          := chkOdsVisible.Checked ;
   frm_tool.mnuODS.Visible              := TraceConfig.Ods_VisibleMenu ;

   // tail
   TraceConfig.Tail_AutoClear           := chkAutoClearTail  .Checked ;
   TraceConfig.Tail_MaxNode             := StrToIntDef(editMaxNodesTail.Text,2000) ;
   TraceConfig.Tail_MinNode             := StrToIntDef(EditMinNodesTail.Text,1000) ;

   frm_tool.mnuTail.Visible             := chkTailVisible.Checked ;
   TraceConfig.tail_VisibleMenu         := chkTailVisible.Checked ;

   TraceConfig.Tail_SizeToLoad          := StrToIntDef(EditTailFileSize.Text,800) ;
   if (TraceConfig.Tail_SizeToLoad <= 0) or (TraceConfig.Tail_SizeToLoad > 65000) then begin
      EditTailFileSize.Text := '800' ;
      TraceConfig.Tail_SizeToLoad := 800 ;
      ModalResult := mrNone ;
   end ;

   // general
   TraceConfig.AppDisplay_IconFile      := EditIconFile.Text ;
   if (TraceConfig.AppDisplay_IconFile <> '') and (FileExists(TraceConfig.AppDisplay_IconFile)) then begin
     frm_tool.imagelist1.Clear ;
     frm_tool.imagelist1.FileLoad(rtBitmap,TraceConfig.AppDisplay_IconFile, clFuchsia) ;
   end ;

   TraceConfig.General_SocketPort                := StrToIntDef(EditSocketPort.Text,8090) ;
   TraceConfig.General_SocketPort2               := StrToIntDef(EditSocketPort2.Text,4502) ;
   TraceConfig.General_Udp1                      := chkUdp1.Checked  ;
   TraceConfig.General_Udp2                      := chkUdp2.Checked  ;
   TraceConfig.General_HTTPPort                  := StrToIntDef(EditHTTPPort.Text,81) ;
   TraceConfig.General_ShowSocketWarning         := chkWarningSocket.Checked  ;
   TraceConfig.General_SocketPolicyServer        := chkSocketPolicyServer.Checked  ;
   TraceConfig.General_HttpPolicyServer          := chkHttpPolicyServer.Checked  ;

   TraceConfig.AppDisplay_ApplicationTitle       := EditDebugTitle.text ;
   frm_tool.caption := EditDebugTitle.text ;

   TraceConfig.AppDisplay_ShowOnstartup          := chkShowOnStartup.Checked  ;
   TraceConfig.AppDisplay_DisableInternalLog     := not chkEnableInternalLog.Checked  ;
   TraceConfig.AppDisplay_ShowOnMessageReceived  := chkShowOnMessage.Checked  ;
   TraceConfig.AppDisplay_FocusToReceivedMessage := chkFocus_OnMessage.Checked  ;
   TraceConfig.AppDisplay_MinimizeToSystray      := chkMinmizeSystray.Checked ;
   TraceConfig.AppDisplay_smallBut               := ChkSmallIcons.Checked ;

   TraceConfig.AppDisplay_ToolbarStandard        := chkStandardToolbar.Checked ;
   TraceConfig.AppDisplay_ToolbarSearch          := chkSearchToolbar.Checked   ;
   TraceConfig.AppDisplay_ToolbarBookmark        := chkBookmarkToolbar.Checked ;
   TraceConfig.AppDisplay_ToolbarFilter          := chkFilterToolbar.Checked   ;

   for c := 0 to ContainerList.Count-1 do begin
      FrmPageContainer := TFrmPageContainer(ContainerList[c]) ;
      FrmPageContainer.configureToolbar ;
   end ;

   // text export
   TraceConfig.TextExport_ProcessName          := ChkProcessName.Checked    ;
   TraceConfig.TextExport_ThreadID             := ChkThreadID.Checked       ;
   TraceConfig.TextExport_Time                 := ChkTime.Checked           ;
   TraceConfig.TextExport_col1                 := ChkTree.Checked           ;
   TraceConfig.TextExport_col2                 := ChkComment.Checked        ;
   TraceConfig.TextExport_GenerateColumnHeader := ChkIncludeTitle.Checked   ;
   TraceConfig.TextExport_Separator            := EditSeparator.Text        ;
   TraceConfig.TextExport_TreeIndentation      := StrToIntDef(EditIndentation.text,3) ;

   if rbNone.checked   = true then TraceConfig.TextExport_TextQualifier := '' ;
   if rbSingle.checked = true then TraceConfig.TextExport_TextQualifier := '''' ;
   if rbDouble.checked = true then TraceConfig.TextExport_TextQualifier := '"' ;

   // plugins
   for c := 0 to TraceConfig.PluginList.Count-1 do begin
      Plugin := TPlugin (TraceConfig.PluginList.Items[c]) ;
      Plugin.startup := TfrmPlugin(Plugin.frmPlugin).chkLoadAtStartup.checked ;
      Plugin.Param := AnsiString(TfrmPlugin(Plugin.frmPlugin).MemoParam.Text) ;
   end ;

   // fonts : name, size and height

   TraceConfig.Framework_Trace_FontName    := FrameworkTraceFonts.Text ;
   TraceConfig.Framework_Trace_FontSize    := StrToIntDef (FrameworkTraceFontSize.Text   , 8 ) ;
   TraceConfig.Framework_Trace_NodeHeight  := StrToIntDef (FrameworkTraceNodeHeight.Text , 18 ) ;
   TraceConfig.Framework_Info_FontName     := FrameworkInfoFonts.Text ;
   TraceConfig.Framework_Info_FontSize     := StrToIntDef (FrameworkInfoFontSize.Text    , 8 ) ;
   TraceConfig.Framework_Info_NodeHeight   := StrToIntDef (FrameworkInfoNodeHeight.Text  , 18 ) ;

   TraceConfig.Watches_Trace_FontName      := WatchesTraceFonts.Text ;
   TraceConfig.Watches_Trace_FontSize      := StrToIntDef (WatchesTraceFontSize.Text     , 8 ) ;
   TraceConfig.Watches_Trace_NodeHeight    := StrToIntDef (WatchesTraceNodeHeight.Text   , 18 ) ;
   TraceConfig.Watches_Info_FontName       := WatchesInfoFonts.Text ;
   TraceConfig.Watches_Info_FontSize       := StrToIntDef (WatchesInfoFontSize.Text      , 8 ) ;

   TraceConfig.Ods_Trace_FontName          := OdsTraceFonts.Text ;
   TraceConfig.Ods_Trace_FontSize          := StrToIntDef (OdsTraceFontSize.Text         , 8 ) ;
   TraceConfig.Ods_Trace_NodeHeight        := StrToIntDef (OdsTraceNodeHeight.Text       , 18 ) ;
   TraceConfig.Ods_Info_FontName           := OdsInfoFonts.Text ;
   TraceConfig.Ods_Info_FontSize           := StrToIntDef (OdsInfoFontSize.Text          , 8 ) ;

   TraceConfig.Tail_Trace_FontName         := TailTraceFonts.Text ;
   TraceConfig.Tail_Trace_FontSize         := StrToIntDef (TailTraceFontSize.Text        , 8 ) ;
   TraceConfig.Tail_Trace_NodeHeight       := StrToIntDef (TailTraceNodeHeight.Text      , 18 ) ;
   TraceConfig.Tail_Info_FontName          := TailInfoFonts.Text ;
   TraceConfig.Tail_Info_FontSize          := StrToIntDef (TailInfoFontSize.Text         , 8 ) ;

   TraceConfig.EventLog_Trace_FontName     := EvntLogTraceFonts.Text ;
   TraceConfig.EventLog_Trace_FontSize     := StrToIntDef (EvntLogTraceFontSize.Text     , 8 ) ;
   TraceConfig.EventLog_Trace_NodeHeight   := StrToIntDef (EvntLogTraceNodeHeight.Text   , 18 ) ;
   TraceConfig.EventLog_Info_FontName      := EvntLogInfoFonts.Text ;
   TraceConfig.EventLog_Info_FontSize      := StrToIntDef (EvntLogInfoFontSize.Text      , 8 ) ;

   Frm_Tool.SaveSettings() ;

   // change font in every base form
   for c := 0 to BaseList.Count -1 do begin
      FrmBase := TFrmBase (BaseList[c]) ;
      FrmBase.ApplyFont ;
   end ; 
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.btnOKClick(Sender: TObject);
begin
   ModalResult := mrOK ;
   butApplyClick (nil) ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.VSTOptionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
   obj : tObject ;
   plugin : TPlugin ;
begin
   CellText := '' ;

   Obj := TObject (Sender.GetNodeData(Node)^) ;
   if Obj = PnlGeneral then
      CellText := 'General'
   else if Obj = PnlFramework then
      CellText := 'Framework'
   else if Obj = PnlFrameworkDelete then
      CellText := 'Delete old nodes'
   else if obj = pnlWatches then
      CellText := 'Watches'
   else if Obj = PnlODS then
      CellText := 'OutputDebugString'
   else if Obj = PnlTail then
      CellText := 'Tail'
   else if Obj = PnlEventLog then
      CellText := 'Event log'
   else if Obj = PnlClipBoard then
      CellText := 'ClipBoard'
   else if Obj = PnlPlugins then
      CellText := 'Plugins'
   else if Obj is TPlugin then begin
      plugin := TPlugin (obj) ;
      CellText := string (plugin.PlugName) ;
      if (CellText = '') or (plugin.PlugName = '_') then
         CellText := ExtractFileName(string(plugin.FileName)) ;
      if CellText = '' then
         CellText := string(plugin.PlugClassName) ;
   end ;

end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.VSTOptionsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
   obj : tObject ;
   plugin : TPlugin ;
begin
   Obj := TObject (Sender.GetNodeData(Node)^) ;
   ImageIndex := -1 ;
   if Obj is TPlugin then begin
      plugin := TPlugin (obj) ;
      if plugin.Status = psUnloaded then
         ImageIndex := 16
      else if plugin.Status = psLoaded then
         ImageIndex := 17
      else if plugin.Status = psStarted then
         ImageIndex := 18 ;
   end ;

end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.VSTOptionsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   obj : tObject ;
begin
   if node = nil then
      exit ;

   Obj := TObject (Sender.GetNodeData(Node)^) ;
   if obj is TPanel then
      TPanel (obj).BringToFront
   else if Obj is TPlugin then begin
      TPlugin (obj).frmPlugin.BringToFront ;
      TfrmPlugin(TPlugin (obj).frmPlugin).Display() ; 
   end ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.btnCancelClick(Sender: TObject);
begin
   ShowCurrentConfig ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.ButNewWin32PluginClick(Sender: TObject);
var
   Win32plugin : TWin32Plugin ;
   OtherPlugin : tPlugin ;
   error : string ;
   tempStringW : string ;
   node : PVirtualNode ;
   szName : array of AnsiChar ;  // [0..1200]
   pName : PAnsiString ;
begin
   if LastPlugPath = '' then
      LastPlugPath := Frm_Tool.strRunPath  ;
   Frm_Tool.OpenDialog1.Filter := 'Win32 Dynamic Link Library file (*.Dll)|*.dll' ;
   Frm_Tool.OpenDialog1.InitialDir := LastPlugPath ;
   if Frm_Tool.OpenDialog1.Execute = false then
      exit ;
   LastPlugPath := ExtractFilePath (Frm_Tool.OpenDialog1.FileName) ;

   Win32plugin := TWin32Plugin.create ;  // also create an associated frmPlugin
   Win32plugin.FileName  := ansiString(Frm_Tool.OpenDialog1.FileName) ;
   Win32plugin.startup   := true ;       // loaded at startup
   Win32plugin.PlugName  := '' ;         // don't know yet the name
   Win32plugin.plugKind  := 'Win32' ;
   Win32plugin.status    := psUnloaded ; // not yet loaded

   error := Win32plugin.DoCheckPlugInfile ;   // load only
   if error <> '' then begin
      Application.MessageBox (pchar('Unable to load the plugin : ' + error),'Win 32 Plugin', MB_OK);
      Win32plugin.free ;
      exit ;
   end ;

   // 'GetPlugName' , 'Start' and 'Stop' are mandatory
   if (not assigned (Win32plugin.getPlugName)) or
      (not assigned (Win32plugin.Start)) or
      (not assigned (Win32plugin.Stop)) then
   begin
      Application.MessageBox ('"Start" , "GetPlugName" and "Stop" are mandatory functions ','Win 32 Plugin', MB_OK);
      Win32plugin.DoUnload ;
      Win32plugin.free ;
      exit ;
   end ;

   // ask for the plugin name and check if the plugin name is not already used.
   try
      if assigned (Win32plugin.getPlugName) then begin
         SetLength (szName,1200) ;
         pName := PAnsiString(szName) ;
         System.AnsiStrings.strcopy (pansiChar(pName), '') ;
         Win32plugin.GetPlugName (pName) ;   // get ANSI plugin name

         tempStringW := BufToString (pAnsichar(pName),pAnsichar(pName)+1000) ;  // convert null terminated ansiString to string
         Win32plugin.PlugName := AnsiString(trim(tempStringW)) ;

         //Win32plugin.PlugName := AnsiString(trim(string(pName))) ;
      end ;
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace (string(Win32plugin.FileName) + ' : DoGetName ' , e.Message) ;
      end ;
   end ;

   //Win32plugin.GetName() ;

   if getPlugFromName (Win32plugin.PlugName) <> nil then begin
      OtherPlugin := getPlugFromName (Win32plugin.PlugName) ;
      Application.MessageBox (pWideChar('A plugin with the same name already exist on this file : ' + string(OtherPlugin.FileName)) ,'Win 32 Plugin', MB_OK);
      Win32plugin.DoUnload ;
      Win32plugin.free ;
      exit ;
   end ;

   // add to list. Will be added later when "Ok" is pressed
   TraceConfig.PluginList.add (Win32plugin) ;

   Win32plugin.frmPlugin.parent := PanelRight ;
   // add to tree
   node := VSTOptions.AddChild(VstPlugNode, Win32plugin) ;
   TfrmPlugin(Win32plugin.frmPlugin).node := node ;
   VSTOptions.Expanded [VstPlugNode] := true ;
   VSTOptions.Selected [node] := true ;          // show it
   VSTOptionsChange(VSTOptions,  node);

   // run the plugin
   Win32plugin.doStart(PAnsiString(Win32plugin.param)) ;
   VSTOptionsChange(VSTOptions,  node);
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.ButNewDotNetPluginClick(Sender: TObject);
var
   DotNetPlugin : TDotNetPlugin ;
   node : PVirtualNode ;
begin
   if LastPlugPath = '' then
      LastPlugPath := Frm_Tool.strRunPath  ;
   Frm_Tool.OpenDialog1.Filter := 'Dot Net Class Library file (*.Dll)|*.dll' ;
   Frm_Tool.OpenDialog1.InitialDir := LastPlugPath ;
   if Frm_Tool.OpenDialog1.Execute = false then
      exit ;
   LastPlugPath := ExtractFilePath (Frm_Tool.OpenDialog1.FileName) ;

   if TraceConfig.DotNetManager = nil then begin
      TraceConfig.DotNetManager := TDotNetManager.create();
      if TraceConfig.DotNetManager.DllHandle = 0 then
         exit ;
   end ;

   if TraceConfig.DotNetManager.DllHandle = 0 then begin
      TraceConfig.DotNetManager.LoadDotNetWrapper() ;
      if TraceConfig.DotNetManager.DllHandle = 0 then
         exit ;
   end ;

   // Check if a plugin exist in the library. May raise exception
   // The plugin is loaded and the getName is called.
   // the DotNetPlugin.frmPlugin frame is created and linked to frmDebugOptions

   DotNetPlugin := TDotNetPlugin.create ;
   DotNetPlugin.startup   := true ;       // loaded at startup
   DotNetPlugin.plugKind  := 'DotNet' ;
   DotNetPlugin.FileName  := AnsiString(Frm_Tool.OpenDialog1.FileName) ;
   DotNetPlugin.Param     := '' ;

   // add to list.Will be saved later when "Ok" is pressed
   TraceConfig.PluginList.add (DotNetPlugin) ;

   TraceConfig.DotNetManager.DoCheckPlugInfile (DotNetPlugin) ;

   // add to tree
   try
      DotNetPlugin.frmPlugin.parent := PanelRight ;
   except
   end ;

   // add to tree
   node := VSTOptions.AddChild(VstPlugNode, DotNetPlugin) ;
   TfrmPlugin(DotNetPlugin.frmPlugin).node := node ;
   VSTOptions.Expanded [VstPlugNode] := true ;
   VSTOptions.Selected [Node] := true ;          // show it
   VSTOptionsChange(VSTOptions,  node);

   // run the plugin
   //DotNetPlugin.doStart (PAnsiString(DotNetPlugin.param));

   VSTOptionsChange(VSTOptions,  node);
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.FontsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
   cb: TCombobox;
   SourceRect, TargetRect: TRect;
   FontDetail : TFontSpec ;
   FontBitmap : TBitmap ;
begin
   cb := Control as TCombobox;
   cb.Canvas.FillRect( rect );
   if (index < 0) or (index >= cb.Items.Count) then
      exit ;

   cb.Canvas.Font.Size := 10;
   if odComboBoxEdit in State then begin
      // Draw the edit portion of the control, use the controls
      // design-time font for this since the edit control is
      // fixed height and drawing a bunch of symbols if the selected
      // font is Symbol etc. is not very informative for the user.
      cb.Canvas.Font := cb.Font;
      If odSelected In State Then
         cb.Canvas.Font.Color := clHighlightText;

      cb.Canvas.TextRect( Rect, rect.left+2, rect.top, cb.Items[index]);
   end else begin
      // draw the list portion
      FontDetail := TFontSpec (cb.items.Objects[index]) ;

      // if FontDetail.lfCharSet = SYMBOL_CHARSET then begin

      // draw the readable font name
      TargetRect := Rect ;
      TargetRect.Right := TargetRect.left + 150 ;
      cb.Canvas.Font := cb.Font;
      cb.Canvas.TextRect( Rect, rect.left+22, rect.top, cb.Items[index]);   // skip 22 pixels for TTF bitmap

      // draw the symbol part
      TargetRect := Rect ;
      TargetRect.left := TargetRect.left + 160 ;
      cb.Canvas.Font.Name := cb.Items[index];
      cb.Canvas.TextRect( TargetRect, TargetRect.left, TargetRect.top, cb.Items[index]);
      // end else begin
      //    cb.Canvas.Font.Name := cb.Items[index];
      //    cb.Canvas.TextRect( Rect, rect.left+22, rect.top, cb.Items[index]);
      // end ;

      if (FontDetail.FontType = TRUETYPE_FONTTYPE) then
         FontBitmap := FTrueTypeBMP
      else if (FontDetail.FontType = DEVICE_FONTTYPE) then
         FontBitmap := FDeviceBMP
      else
         FontBitmap := nil ;

      if (FontBitmap <> nil) then begin
         SourceRect.Left := 0 ;
         SourceRect.Top := 0 ;
         SourceRect.Right := FontBitmap.width ;
         SourceRect.Bottom := FontBitmap.Height ;

         TargetRect := Rect ;
         TargetRect.Right := TargetRect.left + FontBitmap.width ;
         TargetRect.Bottom := TargetRect.top + FontBitmap.Height ;
         cb.Canvas.BrushCopy (TargetRect,FontBitmap,  SourceRect , FontBitmap.TransparentColor) ;
      end ;

   end;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.FontsChange(Sender: TObject);
var
   FontCombo : TCombobox ;
   FontSizeCombo  : TCombobox ;
   FontDetail : TFontSpec ;
   c : integer ;
   newFontSize : integer ;
   newStrFontSize : string ;
   OldFontSize : integer ;
begin
   FontCombo := TCombobox (sender) ;
   FontSizeCombo := TCombobox (FontCombo.tag) ;
   if FontSizeCombo.ItemIndex <> -1 then
      OldFontSize := strtoint(FontSizeCombo.Items.Strings[FontSizeCombo.ItemIndex])
   else
      OldFontSize := strtoint(FontSizeCombo.Text) ;
   FontSizeCombo.Clear;

   FontDetail := TFontSpec (FontCombo.items.Objects[FontCombo.ItemIndex]) ;
   for c := 0 to FontDetail.SizeList.count-1 do begin
      newFontSize := integer( FontDetail.SizeList.Items[c] ) ;
      newstrFontSize := '      ' + inttostr(newFontSize) ;
      newstrFontSize := copy (newstrFontSize, length (newstrFontSize) - 4 , 10) ;
      FontSizeCombo.Items.Add(newstrFontSize);
      if newFontSize = OldFontSize then
         FontSizeCombo.ItemIndex := c ;
   end ;
   if FontSizeCombo.ItemIndex = -1 then
      FontSizeCombo.text := intToStr(OldFontSize) ;
end;

//------------------------------------------------------------------------------

function FontDetailEnumProc(
   var EnumLogFont: TEnumLogFont;       // pointer to logical-font data
   var TextMetric : TNewTextMetric;     // pointer to physical-font data
       FontType   : Integer;            // type of font
       FontDetail : TFontSpec           // address of application-defined data
   ): Integer; StdCall;
var
   LogFont : TLogFont;
   fontSize : integer ;
   c : integer ;
begin
   LogFont := EnumLogFont.elfLogFont;
   FontDetail.lfCharSet := LogFont.lfCharSet ;
   FontDetail.FontType := FontType ;
   if (FontType = TRUETYPE_FONTTYPE) then begin
      for c := 0 to szSizeArray-1 do
         FontDetail.SizeList.Add(tObject(PointSizes[c]));
      result := 0 ;    // stop enumerating fonts
    end else begin
      fontSize := MulDiv(EnumLogFont.elfLogFont.lfHeight, 72, Screen.PixelsPerInch { PPI});

      if FontDetail.SizeList.IndexOf (tObject(fontSize)) = -1 then begin
         FontDetail.SizeList.Add(tObject(fontSize));  // TObjectList
      end ;
      result := 1 ;    // continue enumeration for font size
   end ;
end;

//------------------------------------------------------------------------------

Procedure TfrmDebugOptions.InitFonts( );
var
  c : Integer;
  FontDetail : TFontSpec ;

  procedure InitCombos (ComboFonts , ComboSizes : TCombobox) ;
  //var
  //   i : integer ;
  begin

     ComboFonts.Items := FontList ;
     ComboFonts.tag := integer (ComboSizes) ;
     ComboFonts.itemindex := 0;
     ComboFonts.OnChange := FontsChange ;
     ComboFonts.OnDrawItem := FontsDrawItem ;
     ComboFonts.DropDownCount := 15 ;
     FontsChange (ComboFonts) ;

     // calculate node height
     {
     for i := 0 To FontList.count - 1 Do Begin
        // use form canvas for measurements
        canvas.font.name := ComboFonts.items[i];
        canvas.font.size := 10 ;
        ComboFonts.perform( CB_SETITEMHEIGHT, i, canvas.TextHeight(ComboFonts .items[i])+2);      // too slow
     end;
     }
  end ;

begin
   if FontList <> nil then
      exit ;

   //LowTrace('TfrmDebugOptions.InitFonts') ;
   FontList := TStringList.create ;
   for c := 0 to Screen.Fonts.count - 1 Do Begin
      FontDetail := TFontSpec.create ;
      FontDetail.FontName := Screen.Fonts[c];
      FontDetail.SizeList := TObjectList.create (false) ;

      EnumFontFamilies(Self.Canvas.Handle,
                       pchar(FontDetail.FontName),
                       @FontDetailEnumProc,
                       longint(FontDetail));
      FontList.AddObject (FontDetail.FontName,FontDetail) ;
   end ;
   //LowTrace('TfrmDebugOptions.InitFonts FontList filled') ;

   InitCombos (FrameworkTraceFonts , FrameworkTraceFontSize) ;
   InitCombos (FrameworkInfoFonts  , FrameworkInfoFontSize ) ;
   InitCombos (WatchesTraceFonts   , WatchesTraceFontSize  ) ;
   InitCombos (WatchesInfoFonts    , WatchesInfoFontSize   ) ;
   InitCombos (OdsTraceFonts       , OdsTraceFontSize      ) ;
   InitCombos (OdsInfoFonts        , OdsInfoFontSize       ) ;
   InitCombos (TailTraceFonts      , TailTraceFontSize     ) ;
   InitCombos (TailInfoFonts       , TailInfoFontSize      ) ;
   InitCombos (EvntLogTraceFonts   , EvntLogTraceFontSize  ) ;
   InitCombos (EvntLogInfoFonts    , EvntLogInfoFontSize   ) ;
   //LowTrace('TfrmDebugOptions.InitFonts done') ;
end ;




end.