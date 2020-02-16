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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Contnrs,
  StdCtrls, ComCtrls, variants, CheckLst, ExtCtrls, ImgList, VirtualTrees, javaruntime,
  ColorPickerButton;

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

  TTraceConfig = class  // most of the XmlConfig part often used in the application  (in loop)
    DebugMode                         : boolean ;
    General_ShowSocketWarning         : boolean ;
    AppDisplay_ShowOnMessageReceived  : Boolean ;
    AppDisplay_FocusToReceivedMessage : Boolean ;
    AppDisplay_HideViewer             : Boolean ;
    AppDisplay_DisableInternalLog     : Boolean ;

    Framework_AutoClear               : Boolean ;
    Framework_MaxNode                 : Integer ;
    Framework_MinNode                 : Integer ;

    Ods_AutoClear                     : Boolean ;
    Ods_MaxNode                       : Integer ;
    Ods_MinNode                       : Integer ;

    Tail_AutoClear                    : Boolean  ;
    Tail_MaxNode                      : Integer  ;
    Tail_MinNode                      : Integer  ;

    TextExport_ProcessName            : Boolean ;
    TextExport_ThreadId               : Boolean ;
    TextExport_Time                   : Boolean ;
    TextExport_Col1                   : Boolean ;
    TextExport_Col2                   : Boolean ;
    TextExport_GenerateColumnHeader   : Boolean ;
    TextExport_TextQualifier          : String ;
    TextExport_Separator              : String ;
    TextExport_TreeIndentation        : Integer ;

    // font name, size and node height
    Framework_Trace_FontName          : String ;
    Framework_Trace_FontSize          : Integer ;
    Framework_Trace_NodeHeight        : Integer ;
    Framework_Info_FontName           : String ;
    Framework_Info_FontSize           : Integer ;
    Framework_Info_NodeHeight         : Integer ;
    Watches_Trace_FontName            : String ;
    Watches_Trace_FontSize            : Integer ;
    Watches_Trace_NodeHeight          : Integer ;
    Watches_Info_FontName             : String ;
    Watches_Info_FontSize             : Integer ;
    //Watches_Info_NodeHeight           : Integer ;
    Ods_Trace_FontName                : String ;
    Ods_Trace_FontSize                : Integer ;
    Ods_Trace_NodeHeight              : Integer ;
    Ods_Info_FontName                 : String ;
    Ods_Info_FontSize                 : Integer ;
    //Ods_Info_NodeHeight               : Integer ;
    Tail_Trace_FontName               : String ;
    Tail_Trace_FontSize               : Integer ;
    Tail_Trace_NodeHeight             : Integer ;
    Tail_Info_FontName                : String ;
    Tail_Info_FontSize                : Integer ;
    //Tail_Info_NodeHeight              : Integer ;
    EventLog_Trace_FontName           : String ;
    EventLog_Trace_FontSize           : Integer ;
    EventLog_Trace_NodeHeight         : Integer ;
    EventLog_Info_FontName            : String ;
    EventLog_Info_FontSize            : Integer ;
    //EventLog_Info_NodeHeight          : Integer ;

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
    ButNewJavaPlugin: TButton;
    Label20: TLabel;
    ComboJVM: TComboBox;
    Label22: TLabel;
    EditPluginClassPath: TEdit;
    StatusBar1: TStatusBar;
    MemoVMClassPath: TMemo;
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
    procedure ButNewJavaPluginClick(Sender: TObject);
    procedure ComboJVMDropDown(Sender: TObject);
    procedure ComboJVMChange(Sender: TObject);
    procedure EditPluginClassPathChange(Sender: TObject);
    procedure VSTOptionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FontsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FontsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTrueTypeBMP: TBitmap;
    FDeviceBMP: TBitmap;
    FontList : TStringList ;
    Procedure InitFonts( );
    procedure ShowCurrentConfig;
  public
    VstPlugNode : PVirtualNode ;
    LastPlugPath : string ;
    procedure CheckSettings;
    procedure XmlConfToLocal;
    procedure SaveSettings;
    procedure FillPlugins;
  end;

var
  frmDebugOptions: TfrmDebugOptions;

implementation

uses Registry, Unt_Tool, unt_Tail, unt_ods, unt_TraceWin,
  Config , // to regenerate XSD from power designer : Menu Language/Generate XML Schema definition file (ctrl-G)
  unt_utility, unt_plugin , unt_FrmPlugin,
  unt_addJavaPlug, unt_PageContainer , unt_base ;

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

   FillPlugins() ;

   VSTOptionsChange (VSTOptions, VSTOptions.GetFirst) ;
   ShowCurrentConfig () ;
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
   for c := 0 to FontList.Count-1 do begin
      FontDetail := TFontSpec (FontList.Objects[c]) ;
      FontDetail.SizeList.Free ;  // not owner : integer array
      FontDetail.Free ;
   end ;
   FontList.Clear ;
   FontList.free ;
end;

//------------------------------------------------------------------------------
// called by TFrm_Tool.Create
procedure TfrmDebugOptions.CheckSettings;
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
      Reg.WriteString('FilePath', StrPas(Buf));
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
   if XMLConfig.TextExport.Separator.Attributes             ['Value'] = Null then XMLConfig.TextExport.Separator.Value := '9' ;
   if XMLConfig.TextExport.GenerateColumnHeader.Attributes  ['Value'] = Null then XMLConfig.TextExport.GenerateColumnHeader.Value := false ;
   if XMLConfig.TextExport.TextQualifier.Attributes         ['Value'] = Null then XMLConfig.TextExport.TextQualifier.Value := 'None' ;
   if XMLConfig.TextExport.TreeIndentation.Attributes       ['Value'] = Null then XMLConfig.TextExport.TreeIndentation.Value := 3 ;

   //plugins
   // -----------------------------------------------------------------------

//   if XMLConfig.Plugins.JVMEngine.Attributes['Value'] = Null then begin
//      jvms := TJavaPlugin.GetJVMs () ;
//      if jvms.Count > 0 then
//         XMLConfig.Plugins.JVMEngine.Value := jvms.Strings [0]
//      else
//         XMLConfig.Plugins.JVMEngine.Value := '' ;  // Java is not installed
//      jvms.Free ;
//   end ;
//
//   if XMLConfig.Plugins.JavaPLuginClassPath.Attributes['Value'] = Null then begin
//     XMLConfig.Plugins.JavaPLuginClassPath.Value := '' ;
//   end ;

   for c := 0 to XMLConfig.Plugins.Plugin.Count-1 do begin
      if XMLConfig.Plugins.Plugin[c].Kind = '' then
         XMLConfig.Plugins.Plugin[c].Kind := 'Win32' ;

      if (XMLConfig.Plugins.Plugin[c].Enabled.Attributes['Value'] = Null) or
         (XMLConfig.Plugins.Plugin[c].Enabled.Attributes['Value'] = '') then
         XMLConfig.Plugins.Plugin[c].Enabled.value := false ; 
   end ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.XmlConfToLocal ;
begin
   LowTraceName := XMLConfig.General.InternalLog.Value ;
   TraceConfig.General_ShowSocketWarning         := XMLConfig.General.ShowSocketWarning         .Value ;
   TraceConfig.AppDisplay_ShowOnMessageReceived  := XMLConfig.AppDisplay.ShowOnMessageReceived  .Value ;
   TraceConfig.AppDisplay_FocusToReceivedMessage := XMLConfig.AppDisplay.FocusToReceivedMessage .Value ;
   TraceConfig.AppDisplay_HideViewer             := XMLConfig.AppDisplay.HideViewer             .Value ;
   TraceConfig.AppDisplay_DisableInternalLog     := XMLConfig.AppDisplay.DisableInternalLog     .Value ;

   TraceConfig.Framework_AutoClear               := XMLConfig.Framework.AutoClear               .Value ;
   TraceConfig.Framework_MaxNode                 := XMLConfig.Framework.MaxNode                 .Value ;
   TraceConfig.Framework_MinNode                 := XMLConfig.Framework.MinNode                 .Value ;

   TraceConfig.Ods_AutoClear                     := XMLConfig.Ods.AutoClear                     .Value ;
   TraceConfig.Ods_MaxNode                       := XMLConfig.Ods.MaxNode                       .Value ;
   TraceConfig.Ods_MinNode                       := XMLConfig.Ods.MinNode                       .Value ;

   TraceConfig.Tail_AutoClear                    := XMLConfig.Tail.AutoClear                    .Value  ;
   TraceConfig.Tail_MaxNode                      := XMLConfig.Tail.MaxNode                      .Value  ;
   TraceConfig.Tail_MinNode                      := XMLConfig.Tail.MinNode                      .Value  ;

   TraceConfig.TextExport_ProcessName            := XMLConfig.TextExport.ProcessName            .Value ;
   TraceConfig.TextExport_ThreadId               := XMLConfig.TextExport.ThreadId               .Value ;
   TraceConfig.TextExport_Time                   := XMLConfig.TextExport.Time                   .Value ;
   TraceConfig.TextExport_Col1                   := XMLConfig.TextExport.Col1                   .Value ;
   TraceConfig.TextExport_Col2                   := XMLConfig.TextExport.Col2                   .Value ;
   TraceConfig.TextExport_GenerateColumnHeader   := XMLConfig.TextExport.GenerateColumnHeader   .Value ;
   TraceConfig.TextExport_TreeIndentation        := XMLConfig.TextExport.TreeIndentation        .Value ;

   if XMLConfig.TextExport.TextQualifier.Value = 'None'   then TraceConfig.TextExport_TextQualifier := '' ;
   if XMLConfig.TextExport.TextQualifier.Value = 'Single' then TraceConfig.TextExport_TextQualifier := '''' ;
   if XMLConfig.TextExport.TextQualifier.Value = 'Double' then TraceConfig.TextExport_TextQualifier := '"' ;
   if strtointDef(XMLConfig.TextExport.Separator.Value,-1) = -1 then
      TraceConfig.TextExport_Separator := XMLConfig.TextExport.Separator.Value
   else
      TraceConfig.TextExport_Separator := chr (StrToIntDef(XMLConfig.TextExport.Separator.Value, 9)) ;

   // font name, size and node height
   TraceConfig.Framework_Trace_FontName   :=  XMLConfig.Framework.Trace.FontName.Value   ;
   TraceConfig.Framework_Trace_FontSize   :=  XMLConfig.Framework.Trace.FontSize.Value   ;
   TraceConfig.Framework_Trace_NodeHeight :=  XMLConfig.Framework.Trace.NodeHeight.Value ;
   TraceConfig.Framework_Info_FontName    :=  XMLConfig.Framework.Info.FontName.Value    ;
   TraceConfig.Framework_Info_FontSize    :=  XMLConfig.Framework.Info.FontSize.Value    ;
   TraceConfig.Framework_Info_NodeHeight  :=  XMLConfig.Framework.Info.NodeHeight.Value  ;
   TraceConfig.Watches_Trace_FontName     :=  XMLConfig.Watches.Trace.FontName.Value     ;
   TraceConfig.Watches_Trace_FontSize     :=  XMLConfig.Watches.Trace.FontSize.Value     ;
   TraceConfig.Watches_Trace_NodeHeight   :=  XMLConfig.Watches.Trace.NodeHeight.Value   ;
   TraceConfig.Watches_Info_FontName      :=  XMLConfig.Watches.Info.FontName.Value      ;
   TraceConfig.Watches_Info_FontSize      :=  XMLConfig.Watches.Info.FontSize.Value      ;
   //TraceConfig.Watches_Info_NodeHeight    :=  XMLConfig.Watches.Info.NodeHeight.Value    ;
   TraceConfig.Ods_Trace_FontName         :=  XMLConfig.Ods.Trace.FontName.Value         ;
   TraceConfig.Ods_Trace_FontSize         :=  XMLConfig.Ods.Trace.FontSize.Value         ;
   TraceConfig.Ods_Trace_NodeHeight       :=  XMLConfig.Ods.Trace.NodeHeight.Value       ;
   TraceConfig.Ods_Info_FontName          :=  XMLConfig.Ods.Info.FontName.Value          ;
   TraceConfig.Ods_Info_FontSize          :=  XMLConfig.Ods.Info.FontSize.Value          ;
   //TraceConfig.Ods_Info_NodeHeight        :=  XMLConfig.Ods.Info.NodeHeight.Value        ;
   TraceConfig.Tail_Trace_FontName        :=  XMLConfig.Tail.Trace.FontName.Value        ;
   TraceConfig.Tail_Trace_FontSize        :=  XMLConfig.Tail.Trace.FontSize.Value        ;
   TraceConfig.Tail_Trace_NodeHeight      :=  XMLConfig.Tail.Trace.NodeHeight.Value      ;
   TraceConfig.Tail_Info_FontName         :=  XMLConfig.Tail.Info.FontName.Value         ;
   TraceConfig.Tail_Info_FontSize         :=  XMLConfig.Tail.Info.FontSize.Value         ;
   //TraceConfig.Tail_Info_NodeHeight       :=  XMLConfig.Tail.Info.NodeHeight.Value       ;
   TraceConfig.EventLog_Trace_FontName    :=  XMLConfig.EventLog.Trace.FontName.Value    ;
   TraceConfig.EventLog_Trace_FontSize    :=  XMLConfig.EventLog.Trace.FontSize.Value    ;
   TraceConfig.EventLog_Trace_NodeHeight  :=  XMLConfig.EventLog.Trace.NodeHeight.Value  ;
   TraceConfig.EventLog_Info_FontName     :=  XMLConfig.EventLog.Info.FontName.Value     ;
   TraceConfig.EventLog_Info_FontSize     :=  XMLConfig.EventLog.Info.FontSize.Value     ;
   //TraceConfig.EventLog_Info_NodeHeight   :=  XMLConfig.EventLog.Info.NodeHeight.Value   ;

end ;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.SaveSettings;
var
   PosAndSize : PWindowPlacement;

begin

   if XMLConfig <> nil then begin
      try

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
      XMLConfig.Framework.ShowMembers.Value := Frm_Trace.PanelRight.Visible ;     // flag for main Frm_Trace form
      XMLConfig.OwnerDocument.SaveToFile(strConfigFile);
      except
         on e: exception do begin
            //LowTrace('TfrmDebugOptions.SaveSettings : ' + e.Message );
         end ;
      end ;
   end else begin
      //LowTrace('XMLConfig nil') ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.FillPlugins ;
var
   c : integer ;
   Plugin : TPlugin ;
begin
   VSTOptions.DeleteChildren(VstPlugNode);
   for c := 0 to PluginList.Count-1 do begin
      Plugin := TPlugin (PluginList.Items[c]) ;
      Plugin.frmPlugin.parent := PanelRight ;
      if Plugin.xmlPlugin <> nil then
         TfrmPlugin(Plugin.frmPlugin).node := VSTOptions.AddChild(VstPlugNode, Plugin) ;
   end ;
   VSTOptions.Expanded [VstPlugNode] := true ;
end ;

//------------------------------------------------------------------------------

// called by  FormCreate() or when the user click cancel
procedure TfrmDebugOptions.ShowCurrentConfig ;
var
   c : integer ;
   Plugin : TPlugin ;
begin
   InitFonts() ;

   // fonts name
   for c := 0 to FontList.Count-1 do begin
      if FrameworkTraceFonts.items[c] = XMLConfig.Framework.Trace.FontName.Value then
         FrameworkTraceFonts.ItemIndex := c ;
      if FrameworkInfoFonts.items[c] = XMLConfig.Framework.Info.FontName.Value then
         FrameworkInfoFonts.ItemIndex := c ;

      if WatchesTraceFonts.items[c] = XMLConfig.Watches.Trace.FontName.Value then
         WatchesTraceFonts.ItemIndex := c ;
      if WatchesInfoFonts.items[c] = XMLConfig.Watches.Info.FontName.Value then
         WatchesInfoFonts.ItemIndex := c ;

      if OdsTraceFonts.items[c] = XMLConfig.Ods.Trace.FontName.Value then
         OdsTraceFonts.ItemIndex := c ;
      if OdsInfoFonts.items[c] = XMLConfig.Ods.Info.FontName.Value then
         OdsInfoFonts.ItemIndex := c ;

      if TailTraceFonts.items[c] = XMLConfig.Tail.Trace.FontName.Value then
         TailTraceFonts.ItemIndex := c ;
      if TailInfoFonts.items[c] = XMLConfig.Tail.Info.FontName.Value then
         TailInfoFonts.ItemIndex := c ;

      if EvntLogTraceFonts.items[c] = XMLConfig.EventLog.Trace.FontName.Value then
         EvntLogTraceFonts.ItemIndex := c ;
      if EvntLogInfoFonts.items[c] = XMLConfig.EventLog.Info.FontName.Value then
         EvntLogInfoFonts.ItemIndex := c ;     
   end ;

   // font size and node height
   FrameworkTraceFontSize.Text   := intToStr(XMLConfig.Framework.Trace.FontSize.Value) ;
   FrameworkTraceNodeHeight.Text := intToStr(XMLConfig.Framework.Trace.NodeHeight.Value) ;
   FrameworkInfoFontSize.Text    := intToStr(XMLConfig.Framework.Info.FontSize.Value) ;
   FrameworkInfoNodeHeight.Text  := intToStr(XMLConfig.Framework.Info.NodeHeight.Value) ;

   WatchesTraceFontSize.Text     := intToStr(XMLConfig.Watches.Trace.FontSize.Value) ;
   WatchesTraceNodeHeight.Text   := intToStr(XMLConfig.Watches.Trace.NodeHeight.Value) ;
   WatchesInfoFontSize.Text      := intToStr(XMLConfig.Watches.Info.FontSize.Value) ;
   //WatchesInfoNodeHeight.Text    := intToStr(XMLConfig.Watches.Info.NodeHeight.Value) ;

   OdsTraceFontSize.Text         := intToStr(XMLConfig.Ods.Trace.FontSize.Value) ;
   OdsTraceNodeHeight.Text       := intToStr(XMLConfig.Ods.Trace.NodeHeight.Value) ;
   OdsInfoFontSize.Text          := intToStr(XMLConfig.Ods.Info.FontSize.Value) ;
   //OdsInfoNodeHeight.Text        := intToStr(XMLConfig.Ods.Info.NodeHeight.Value) ;

   TailTraceFontSize.Text        := intToStr(XMLConfig.Tail.Trace.FontSize.Value) ;
   TailTraceNodeHeight.Text      := intToStr(XMLConfig.Tail.Trace.NodeHeight.Value) ;
   TailInfoFontSize.Text         := intToStr(XMLConfig.Tail.Info.FontSize.Value) ;
   //TailInfoNodeHeight.Text       := intToStr(XMLConfig.Tail.Info.NodeHeight.Value) ;

   EvntLogTraceFontSize.Text     := intToStr(XMLConfig.EventLog.Trace.FontSize.Value) ;
   EvntLogTraceNodeHeight.Text   := intToStr(XMLConfig.EventLog.Trace.NodeHeight.Value) ;
   EvntLogInfoFontSize.Text      := intToStr(XMLConfig.EventLog.Info.FontSize.Value) ;
   //EvntLogInfoNodeHeight.Text    := intToStr(XMLConfig.EventLog.Info.NodeHeight.Value) ;

   // framework
   chkAutoClearTraces.Checked  := XMLConfig.Framework.AutoClear.Value ;
   editMaxNodesTraces.Text     := intToStr (XMLConfig.Framework.MaxNode.Value) ;
   EditMinNodesTraces.Text     := intToStr (XMLConfig.Framework.MinNode.Value) ;
   chkShowMainformMenu.Checked := XMLConfig.Framework.VisibleMenu.Value ;
   EditMainTrace.Text          := XMLConfig.Framework.MainTraceTitle.Value ;  // 'Traces'

   if XMLConfig.Framework.Orphans.DeletedNode.Value = 'CreateOnRoot' then
      rbCreateOnRoot.Checked := true
   else if XMLConfig.Framework.Orphans.DeletedNode.Value = 'CreateUnderLostAndFound' then
      rbCreateUnderLostAndFound.Checked := true
   else if XMLConfig.Framework.Orphans.DeletedNode.Value = 'AddChildrenOnRoot' then
      rbAddChildrenOnRoot.Checked := true
   else if XMLConfig.Framework.Orphans.DeletedNode.Value = 'AddChildrenUnderLostAndFound' then
      rbAddChildrenUnderLostAndFound.Checked := true ;

   DefaultLeftText.Text  := XMLConfig.Framework.Orphans.DefaultLeftText.Value ;
   DefaultRightText.Text := XMLConfig.Framework.Orphans.DefaultRightText.Value ;
   LostAndFoundLeftText.Text   := XMLConfig.Framework.Orphans.LostAndFoundLeftText.Value ;
   LostAndFoundRightText.Text  := XMLConfig.Framework.Orphans.LostAndFoundRightText.Value ;

   // watches
   chkShowMainWatchesMenu.Checked := XMLConfig.Watches.VisibleMenu.Value ;
   EditMainWatches.Text           := XMLConfig.Watches.MainWatchesTitle.Value ;  // 'Watches'

   // EventLog
   chkEventLogVisible.Checked := XMLConfig.EventLog.VisibleMenu.value ;

   // ods
   chkAutoClearODS.Checked    := XMLConfig.ods.AutoClear.Value ;
   editMaxNodesODS.Text       := intToStr (XMLConfig.ods.MaxNode.Value) ;
   EditMinNodesODS.Text       := intToStr (XMLConfig.ods.MinNode.Value) ;
   EditODSTabsheetTitle.Text  := XMLConfig.ods.Title.Value ;
   chkOdsVisible.Checked      := XMLConfig.ods.VisibleMenu.value;

   // tail
   chkAutoClearTail.Checked   := XMLConfig.tail.AutoClear.Value ;
   editMaxNodesTail.Text      := intToStr (XMLConfig.tail.MaxNode.Value) ;
   EditMinNodesTail.Text      := intToStr (XMLConfig.tail.MinNode.Value) ;
   //EditTailFileSize.Text      := IntToStr (XMLConfig.tail.SizeToLoad.Value);
   chkTailVisible.Checked     := XMLConfig.tail.VisibleMenu.Value ;

   // general
   EditIconFile.Text          := XMLConfig.AppDisplay.IconFile.Value ;
   EditSocketPort.Text        := IntToStr (XMLConfig.General.SocketPort.Value);
   EditSocketPort2.Text       := IntToStr (XMLConfig.General.SocketPort2.Value);
   chkUdp1.Checked            := XMLConfig.General.Udp1.Value ;
   chkUdp2.Checked            := XMLConfig.General.Udp2.Value ;
   EditHTTPPort.Text          := IntToStr (XMLConfig.General.HTTPPort.Value);
   chkWarningSocket.Checked   := XMLConfig.General.ShowSocketWarning.Value ;
   chkSocketPolicyServer.Checked   := XMLConfig.General.SocketPolicyServer.Value ;
   chkHttpPolicyServer.Checked   := XMLConfig.General.HttpPolicyServer.Value ;

   EditDebugTitle.text        := XMLConfig.AppDisplay.ApplicationTitle.Value ;
   ChkSmallIcons.Checked      := XMLConfig.AppDisplay.smallBut.Value ;
   chkStandardToolbar.Checked := XMLConfig.AppDisplay.ToolbarStandard.Value ;
   chkSearchToolbar.Checked   := XMLConfig.AppDisplay.ToolbarSearch.Value ;
   chkBookmarkToolbar.Checked := XMLConfig.AppDisplay.ToolbarBookmark.Value ;
   chkFilterToolbar.Checked   := XMLConfig.AppDisplay.ToolbarFilter.Value ;

   chkMinmizeSystray.Checked  := XMLConfig.AppDisplay.MinimizeToSystray.Value ;
   chkShowOnStartup.Checked   := XMLConfig.AppDisplay.ShowOnstartup.Value ;
   chkEnableInternalLog.Checked  := not XMLConfig.AppDisplay.DisableInternalLog.Value ;

   chkFocus_OnMessage.Checked := XMLConfig.AppDisplay.FocusToReceivedMessage.Value ;
   chkShowOnMessage.Checked   := XMLConfig.AppDisplay.ShowOnMessageReceived.Value ;


   ChkProcessName.Checked     := XMLConfig.TextExport.ProcessName.Value ;
   ChkThreadID.Checked        := XMLConfig.TextExport.ThreadID.Value ;
   ChkTime.Checked            := XMLConfig.TextExport.Time.Value ;
   ChkTree.Checked            := XMLConfig.TextExport.col1.Value ;
   ChkComment.Checked         := XMLConfig.TextExport.col2.Value ;
   ChkIncludeTitle.Checked    := XMLConfig.TextExport.GenerateColumnHeader.Value ;
   EditSeparator.Text         := XMLConfig.TextExport.Separator.Value ;
   EditIndentation.text       := IntTostr(XMLConfig.TextExport.TreeIndentation.Value) ;

   if XMLConfig.TextExport.TextQualifier.Value = 'None'   then rbNone.Checked   := true ;
   if XMLConfig.TextExport.TextQualifier.Value = 'Single' then rbSingle.Checked := true ;
   if XMLConfig.TextExport.TextQualifier.Value = 'Double' then rbDouble.Checked := true ;

   // plugins
   ComboJVM.Text            := XMLConfig.Plugins.JVMEngine.Value ;
   EditPluginClassPath.Text := XMLConfig.Plugins.JavaPLuginClassPath.value ;
   ButNewJavaPlugin.Enabled := true ;

   for c := 0 to PluginList.Count-1 do begin
      Plugin := TPlugin (PluginList.Items[c]) ;
      if Plugin.xmlPlugin = nil then   // check if plugin is not removed from configuration
         continue ;
      plugin.startup := Plugin.xmlPlugin.Enabled.Value ;
      TfrmPlugin(Plugin.frmPlugin).chkLoadAtStartup.checked  := Plugin.xmlPlugin.Enabled.Value ;
      TfrmPlugin(Plugin.frmPlugin).MemoParam.Text := Plugin.xmlPlugin.Param ;

   end ;

end ;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.butApplyClick(Sender: TObject);
var
   c : integer ;
   FrmPageContainer : TFrmPageContainer ;
   Plugin : TPlugin ;
   FrmBase : TFrmBase ;
begin
   // framework
   XMLConfig.Framework.AutoClear.Value        := chkAutoClearTraces.Checked ;
   XMLConfig.Framework.MaxNode.Value          := StrToIntDef(editMaxNodesTraces.Text,2000) ;
   XMLConfig.Framework.MinNode.Value          := StrToIntDef(EditMinNodesTraces.Text,1000) ;
   XMLConfig.Framework.VisibleMenu.Value      := chkShowMainformMenu.Checked ;
   XMLConfig.Framework.MainTraceTitle.Value   := EditMainTrace.Text ;  // 'Traces'

   if rbCreateOnRoot.Checked = true then
      XMLConfig.Framework.Orphans.DeletedNode.Value := 'CreateOnRoot'
   else if rbCreateUnderLostAndFound.Checked = true then
      XMLConfig.Framework.Orphans.DeletedNode.Value := 'CreateUnderLostAndFound'
   else if rbAddChildrenOnRoot.Checked = true then
      XMLConfig.Framework.Orphans.DeletedNode.Value := 'AddChildrenOnRoot'
   else if rbAddChildrenUnderLostAndFound.Checked = true then
      XMLConfig.Framework.Orphans.DeletedNode.Value := 'AddChildrenUnderLostAndFound' ;

   XMLConfig.Framework.Orphans.DefaultLeftText.Value  := DefaultLeftText.Text  ;
   XMLConfig.Framework.Orphans.DefaultRightText.Value := DefaultRightText.Text ;
   XMLConfig.Framework.Orphans.LostAndFoundLeftText.Value   := LostAndFoundLeftText.Text   ;
   XMLConfig.Framework.Orphans.LostAndFoundRightText.Value  := LostAndFoundRightText.Text  ;

   Frm_trace.Caption := EditMainTrace.Text ;      // main Frm_Trace form
   frm_tool.mnuViewMainTraces.Visible := XMLConfig.Framework.VisibleMenu.value ;

   // watches
   XMLConfig.Watches.VisibleMenu.Value      := chkShowMainWatchesMenu.Checked ;
   XMLConfig.Watches.MainWatchesTitle.Value := EditMainWatches.Text ;    // 'Watches'
   Frm_Watches.Caption := EditMainWatches.Text ;
   frm_tool.mnuViewMainWatches.Visible := XMLConfig.Watches.VisibleMenu.value ;

   // EventLog
   XMLConfig.EventLog.VisibleMenu.value     := chkEventLogVisible.Checked ;
   frm_tool.mnuEventlog.Visible := XMLConfig.EventLog.VisibleMenu.value ;

   // ods
   XMLConfig.ods.AutoClear.Value            := chkAutoClearODS   .Checked ;
   XMLConfig.ods.MaxNode.Value              := StrToIntDef(editMaxNodesODS   .Text,2000) ;
   XMLConfig.ods.MinNode.Value              := StrToIntDef(EditMinNodesODS   .Text,1000) ;
   XMLConfig.ods.Title.Value                := EditODSTabsheetTitle.Text ;

   Frm_ODS.Caption := XMLConfig.Ods.Title.Value  ;  // 'ODS'
   frm_tool.mnuODS.Visible := XMLConfig.Ods.VisibleMenu.Value ;

   // tail
   XMLConfig.Tail.AutoClear.Value           := chkAutoClearTail  .Checked ;
   XMLConfig.Tail.MaxNode.Value             := StrToIntDef(editMaxNodesTail  .Text,2000) ;
   XMLConfig.Tail.MinNode.Value             := StrToIntDef(EditMinNodesTail  .Text,1000) ;
   XMLConfig.Tail.SizeToLoad.Value          := StrToIntDef(EditTailFileSize.Text,800) ;
   frm_tool.mnuTail.Visible                 := chkTailVisible.Checked ;
   XMLConfig.tail.VisibleMenu.Value         := chkTailVisible.Checked ;
   frm_tool.mnuTail.Visible := XMLConfig.tail.VisibleMenu.value ;

   if (XMLConfig.Tail.SizeToLoad.Value <= 0) or (XMLConfig.Tail.SizeToLoad.Value > 65000) then begin
      EditTailFileSize.Text := '800' ;
      XMLConfig.Tail.SizeToLoad.Value := 800 ;
      ModalResult := mrNone ;
   end ;

   // general
   XMLConfig.AppDisplay.IconFile.Value      := EditIconFile.Text ;
   if (XMLConfig.AppDisplay.IconFile.Value <> '') and (FileExists(XMLConfig.AppDisplay.IconFile.Value)) then begin
     frm_tool.imagelist1.Clear ;
     frm_tool.imagelist1.FileLoad(rtBitmap,XMLConfig.AppDisplay.IconFile.Value, clFuchsia) ;
   end ;

   XMLConfig.General.SocketPort.Value                := StrToIntDef(EditSocketPort.Text,8090) ;
   XMLConfig.General.SocketPort2.Value               := StrToIntDef(EditSocketPort2.Text,4502) ;
   XMLConfig.General.Udp1.Value                      := chkUdp1.Checked  ;
   XMLConfig.General.Udp2.Value                      := chkUdp2.Checked  ;
   XMLConfig.General.HTTPPort.Value                  := StrToIntDef(EditHTTPPort.Text,81) ;
   XMLConfig.General.ShowSocketWarning.Value         := chkWarningSocket.Checked  ;
   XMLConfig.General.SocketPolicyServer.Value        := chkSocketPolicyServer.Checked  ;
   XMLConfig.General.HttpPolicyServer.Value          := chkHttpPolicyServer.Checked  ;

   XMLConfig.AppDisplay.ApplicationTitle.Value       := EditDebugTitle.text ;
   frm_tool.caption := EditDebugTitle.text ;

   XMLConfig.AppDisplay.ShowOnstartup.Value          := chkShowOnStartup.Checked  ;
   XMLConfig.AppDisplay.DisableInternalLog.Value     := not chkEnableInternalLog.Checked  ;
   XMLConfig.AppDisplay.ShowOnMessageReceived.Value  := chkShowOnMessage.Checked  ;
   XMLConfig.AppDisplay.FocusToReceivedMessage.Value := chkFocus_OnMessage.Checked  ;
   XMLConfig.AppDisplay.MinimizeToSystray.Value      := chkMinmizeSystray.Checked ;
   XMLConfig.AppDisplay.smallBut.Value               := ChkSmallIcons.Checked ;

   XMLConfig.AppDisplay.ToolbarStandard.Value        := chkStandardToolbar.Checked ;
   XMLConfig.AppDisplay.ToolbarSearch.Value          := chkSearchToolbar.Checked   ;
   XMLConfig.AppDisplay.ToolbarBookmark.Value        := chkBookmarkToolbar.Checked ;
   XMLConfig.AppDisplay.ToolbarFilter.Value          := chkFilterToolbar.Checked   ;

   for c := 0 to ContainerList.Count-1 do begin
      FrmPageContainer := TFrmPageContainer(ContainerList[c]) ;
      FrmPageContainer.configureToolbar ;
   end ;

   // text export
   XMLConfig.TextExport.ProcessName.Value          := ChkProcessName.Checked    ;
   XMLConfig.TextExport.ThreadID.Value             := ChkThreadID.Checked       ;
   XMLConfig.TextExport.Time.Value                 := ChkTime.Checked           ;
   XMLConfig.TextExport.col1.Value                 := ChkTree.Checked           ;
   XMLConfig.TextExport.col2.Value                 := ChkComment.Checked        ;
   XMLConfig.TextExport.GenerateColumnHeader.Value := ChkIncludeTitle.Checked   ;
   XMLConfig.TextExport.Separator.Value            := EditSeparator.Text        ;
   XMLConfig.TextExport.TreeIndentation.Value      := StrToIntDef(EditIndentation.text,3) ;

   if rbNone.checked   = true then XMLConfig.TextExport.TextQualifier.Value := 'None' ;
   if rbSingle.checked = true then XMLConfig.TextExport.TextQualifier.Value := 'Single' ;
   if rbDouble.checked = true then XMLConfig.TextExport.TextQualifier.Value := 'Double' ;

   // plugins
   ButNewJavaPlugin.Enabled := true ;
   XMLConfig.Plugins.JVMEngine.Value           := ComboJVM.Text ;
   XMLConfig.Plugins.JavaPLuginClassPath.value := EditPluginClassPath.Text ;
//   if JRuntime <> nil then
//      TJavaPlugin.SetClassPath(TClassPath.getDefault.FullPath , XMLConfig.Plugins.JavaPLuginClassPath.value);
   for c := 0 to PluginList.Count-1 do begin
      Plugin := TPlugin (PluginList.Items[c]) ;
      if Plugin.xmlPlugin <> nil then begin
         Plugin.xmlPlugin.Enabled.Value := TfrmPlugin(Plugin.frmPlugin).chkLoadAtStartup.checked ;
         Plugin.xmlPlugin.Param := TfrmPlugin(Plugin.frmPlugin).MemoParam.Text ;
      end;
   end ;

   // fonts : name, size and height

   XMLConfig.Framework.Trace.FontName.Value    := FrameworkTraceFonts.Text ;
   XMLConfig.Framework.Trace.FontSize.Value    := StrToIntDef (FrameworkTraceFontSize.Text   , 8 ) ;
   XMLConfig.Framework.Trace.NodeHeight.Value  := StrToIntDef (FrameworkTraceNodeHeight.Text , 18 ) ;
   XMLConfig.Framework.Info.FontName.Value     := FrameworkInfoFonts.Text ;
   XMLConfig.Framework.Info.FontSize.Value     := StrToIntDef (FrameworkInfoFontSize.Text    , 8 ) ;
   XMLConfig.Framework.Info.NodeHeight.Value   := StrToIntDef (FrameworkInfoNodeHeight.Text  , 18 ) ;

   XMLConfig.Watches.Trace.FontName.Value      := WatchesTraceFonts.Text ;
   XMLConfig.Watches.Trace.FontSize.Value      := StrToIntDef (WatchesTraceFontSize.Text     , 8 ) ;
   XMLConfig.Watches.Trace.NodeHeight.Value    := StrToIntDef (WatchesTraceNodeHeight.Text   , 18 ) ;
   XMLConfig.Watches.Info.FontName.Value       := WatchesInfoFonts.Text ;
   XMLConfig.Watches.Info.FontSize.Value       := StrToIntDef (WatchesInfoFontSize.Text      , 8 ) ;
   //XMLConfig.Watches.Info.NodeHeight.Value     := StrToIntDef (WatchesInfoNodeHeight.Text    , 18 ) ;

   XMLConfig.Ods.Trace.FontName.Value          := OdsTraceFonts.Text ;
   XMLConfig.Ods.Trace.FontSize.Value          := StrToIntDef (OdsTraceFontSize.Text         , 8 ) ;
   XMLConfig.Ods.Trace.NodeHeight.Value        := StrToIntDef (OdsTraceNodeHeight.Text       , 18 ) ;
   XMLConfig.Ods.Info.FontName.Value           := OdsInfoFonts.Text ;
   XMLConfig.Ods.Info.FontSize.Value           := StrToIntDef (OdsInfoFontSize.Text          , 8 ) ;
   //XMLConfig.Ods.Info.NodeHeight.Value         := StrToIntDef (OdsInfoNodeHeight.Text        , 18 ) ;

   XMLConfig.Tail.Trace.FontName.Value         := TailTraceFonts.Text ;
   XMLConfig.Tail.Trace.FontSize.Value         := StrToIntDef (TailTraceFontSize.Text        , 8 ) ;
   XMLConfig.Tail.Trace.NodeHeight.Value       := StrToIntDef (TailTraceNodeHeight.Text      , 18 ) ;
   XMLConfig.Tail.Info.FontName.Value          := TailInfoFonts.Text ;
   XMLConfig.Tail.Info.FontSize.Value          := StrToIntDef (TailInfoFontSize.Text         , 8 ) ;
   //XMLConfig.Tail.Info.NodeHeight.Value        := StrToIntDef (TailInfoNodeHeight.Text       , 18 ) ;

   XMLConfig.EventLog.Trace.FontName.Value     := EvntLogTraceFonts.Text ;
   XMLConfig.EventLog.Trace.FontSize.Value     := StrToIntDef (EvntLogTraceFontSize.Text     , 8 ) ;
   XMLConfig.EventLog.Trace.NodeHeight.Value   := StrToIntDef (EvntLogTraceNodeHeight.Text   , 18 ) ;
   XMLConfig.EventLog.Info.FontName.Value      := EvntLogInfoFonts.Text ;
   XMLConfig.EventLog.Info.FontSize.Value      := StrToIntDef (EvntLogInfoFontSize.Text      , 8 ) ;
   //XMLConfig.EventLog.Info.NodeHeight.Value    := StrToIntDef (EvntLogInfoNodeHeight.Text    , 18 ) ;

   SaveSettings() ;
   XmlConfToLocal() ;

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
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
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
      LastPlugPath := strRunPath  ;
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
   Win32plugin.xmlPlugin := nil ;        // not yet saved in xml
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
         strcopy (pansiChar(pName), '') ;
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

   // add to list
   PluginList.add (Win32plugin) ;

   // add to xml
   Win32plugin.xmlPlugin := XMLConfig.Plugins.Plugin.Add ;
   Win32plugin.xmlPlugin.Enabled.Value := true ;
   Win32plugin.xmlPlugin.Kind     := 'Win32' ;
   Win32plugin.xmlPlugin.FileName := string(Win32plugin.FileName) ;
   Win32plugin.xmlPlugin.Enabled.Value := Win32plugin.startup ;
   Win32plugin.xmlPlugin.Param := string(Win32plugin.param) ;

   // save to xml
   XMLConfig.OwnerDocument.SaveToFile(strConfigFile);


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
      LastPlugPath := strRunPath  ;
   Frm_Tool.OpenDialog1.Filter := 'Dot Net Class Library file (*.Dll)|*.dll' ;
   Frm_Tool.OpenDialog1.InitialDir := LastPlugPath ;
   if Frm_Tool.OpenDialog1.Execute = false then
      exit ;
   LastPlugPath := ExtractFilePath (Frm_Tool.OpenDialog1.FileName) ;

   if DotNetManager = nil then begin
      DotNetManager := TDotNetManager.create();
      if DotNetManager.DllHandle = 0 then
         exit ;
   end ;

   if DotNetManager.DllHandle = 0 then begin
      DotNetManager.LoadDotNetWrapper() ;
      if DotNetManager.DllHandle = 0 then
         exit ;
   end ;

   // Check if a plugin exist in the library. May raise exception
   // The plugin is loaded and the getName is called.
   // the DotNetPlugin.frmPlugin frame is created and linked to frmDebugOptions

   DotNetPlugin := TDotNetPlugin.create ;
   DotNetPlugin.startup   := true ;       // loaded at startup
   DotNetPlugin.plugKind  := 'DotNet' ;
   DotNetPlugin.xmlPlugin := nil ;        // not yet saved in xml
   DotNetPlugin.FileName  := AnsiString(Frm_Tool.OpenDialog1.FileName) ;
   DotNetPlugin.Param     := '' ;

   DotNetManager.DoCheckPlugInfile (DotNetPlugin) ;

   // add to list
   PluginList.add (DotNetPlugin) ;

   // add to xml
   DotNetPlugin.xmlPlugin := XMLConfig.Plugins.plugin.Add ;
   DotNetPlugin.xmlPlugin.Param         := '' ;
   DotNetPlugin.xmlPlugin.Kind          := 'DotNet' ;
   DotNetPlugin.xmlPlugin.FileName      := string(DotNetPlugin.FileName) ;
   DotNetPlugin.xmlPlugin.Enabled.Value := DotNetPlugin.startup ;
   //DotNetPlugin.xmlPlugin.ClassName     := string(DotNetPlugin.PlugClassName) ;

   // save to xml
   XMLConfig.OwnerDocument.SaveToFile(strConfigFile);

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
   DotNetPlugin.doStart (PAnsiString(DotNetPlugin.param));
   VSTOptionsChange(VSTOptions,  node);
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.ButNewJavaPluginClick(Sender: TObject);
//var
//   JavaPlugin : TJavaPlugin;
//   node : PVirtualNode ;
begin
//   // init JVM
//   TJavaPlugin.Init;
//
//   // send the classPath
//   TJavaPlugin.SetClassPath(TClassPath.getDefault.FullPath , XMLConfig.Plugins.JavaPLuginClassPath.value);
//   if FrmAddJavaPlugin.ShowModal <> mrOk then
//      exit ;
//
//   if TJavaPlugin.checkClass(FrmAddJavaPlugin.EditClassName.Text) = '' then begin
//      MessageDlg('Class don''t exist', mtError, [mbOK], 0);
//      exit ;
//   end ;
//
//   JavaPlugin := TJavaPlugin.create ;
//   JavaPlugin.plugKind      := 'Java' ;
//   JavaPlugin.startup       := true ;
//   JavaPlugin.xmlPlugin     := nil ;
//   JavaPlugin.PlugName      := '' ;
//   JavaPlugin.PlugClassName := AnsiString(FrmAddJavaPlugin.EditClassName.Text) ;
//
//   PluginList.add (JavaPlugin) ;   // add the plugin to the general plugin list
//
//   //JavaPlugin.Add ;                // add the plugin to the java wrapper
//   JavaPlugin.DoLoad ;             // add the plugin to the java wrapper and Load the plugin
//   JavaPlugin.getName ;            // get the real plugin name
//
//   // add to xml
//   JavaPlugin.xmlPlugin := XMLConfig.Plugins.plugin.Add ;
//   JavaPlugin.xmlPlugin.Enabled.Value := true ;
//   JavaPlugin.xmlPlugin.Kind          := JavaPlugin.plugKind ;
//   JavaPlugin.xmlPlugin.FileName      := '' ;    // not used for java
//   JavaPlugin.xmlPlugin.Enabled.Value := JavaPlugin.startup ;
//   JavaPlugin.xmlPlugin.ClassName     := string(JavaPlugin.PlugClassName) ;
//
//   // save to xml
//   XMLConfig.OwnerDocument.SaveToFile(strConfigFile);
//
//   // add to tree
////   JavaPlugin.frmPlugin.parent := PanelRight ;
//   node := VSTOptions.AddChild(VstPlugNode, JavaPlugin) ;
//   TfrmPlugin(JavaPlugin.frmPlugin).node := node ;
//   VSTOptions.Expanded [VstPlugNode] := true ;
//   VSTOptions.Selected [Node] := true ;          // show it
//   VSTOptionsChange(VSTOptions,  node);
//
//   // run the plugin
//   JavaPlugin.doStart ;
//   VSTOptionsChange(VSTOptions,  node);
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.ComboJVMDropDown(Sender: TObject);
//var
//   jvms : TstringList ;
begin
//   ComboJVM.Items.Clear ;
//   jvms := TJavaPlugin.GetJVMs () ;
//   ComboJVM.Items.AddStrings (jvms) ;
//   jvms.Free ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.ComboJVMChange(Sender: TObject);
begin
//   if JRuntime <> nil then begin
//      MessageDlg('JVM changed, please restart tracetool.', mtWarning, [mbOK], 0);
//      // must click apply before adding plugin
//      ButNewJavaPlugin.Enabled := false ;
//      // free old JVM
//      JRuntime.free ;
//      JRuntime := nil ;
//   end ;
//
//   // recreate the JVM
//   TJavaPlugin.Init;
//   MemoVMClassPath.Text := TClassPath.getDefault.FullPath ;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugOptions.EditPluginClassPathChange(Sender: TObject);
begin
   // must click apply before adding plugin
   ButNewJavaPlugin.Enabled := false ;
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
  var
     i : integer ;
  begin

     ComboFonts.Items := FontList ;
     ComboFonts.tag := integer (ComboSizes) ;
     ComboFonts.itemindex := 0;
     ComboFonts.OnChange := FontsChange ;
     ComboFonts.OnDrawItem := FontsDrawItem ;
     ComboFonts.DropDownCount := 15 ;
     FontsChange (ComboFonts) ;

     // calculate node height
     for i := 0 To FrameworkTraceFonts.items.count - 1 Do Begin
        // use form canvas for measurements
        canvas.font.name := ComboFonts.items[i];
        canvas.font.size := 10 ;
        ComboFonts.perform( CB_SETITEMHEIGHT, i, canvas.TextHeight(ComboFonts .items[i])+2);
     end;
  end ;

begin
   if FontList <> nil then
      exit ;
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
end ;




end.