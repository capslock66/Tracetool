
{*********************************************************************************}
{                                                                                 }
{                                XML Data Binding                                 }
{                                                                                 }
{         Generated on: 11/03/2009 21:53:13                                       }
{       Generated from: \\.host\Shared Folders\tracetool\Viewer 2009\Config.xsd   }
{   Settings stored in: \\.host\Shared Folders\tracetool\Viewer 2009\Config.xdb   }
{                                                                                 }
{*********************************************************************************}

unit Config;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLConfig = interface;
  IXMLGeneral = interface;
  IXMLBooleanTagValue = interface;
  IXMLStringTagValue = interface;
  IXMLIntegerTagValue = interface;
  IXMLAppDisplay = interface;
  IXMLTail = interface;
  IXMLFavorites = interface;
  IXMLDisplayNode = interface;
  IXMLTextExport = interface;
  IXMLColumnTagValue = interface;
  IXMLOds = interface;
  IXMLEventLog = interface;
  IXMLFramework = interface;
  IXMLOrphans = interface;
  IXMLPlugins = interface;
  IXMLPlugin = interface;
  IXMLPluginList = interface;
  IXMLWatches = interface;
  IXMLStringTagValue5 = interface;
  IXMLStringTagValue4 = interface;
  IXMLStringTagValue3 = interface;
  IXMLStringTagValue2 = interface;
  IXMLIntegerTagValue3 = interface;
  IXMLIntegerTagValue2 = interface;

{ IXMLConfig }

  IXMLConfig = interface(IXMLNode)
    ['{3EE0B907-D79C-42FA-9029-86C391302437}']
    { Property Accessors }
    function Get_General: IXMLGeneral;
    function Get_AppDisplay: IXMLAppDisplay;
    function Get_Tail: IXMLTail;
    function Get_TextExport: IXMLTextExport;
    function Get_Ods: IXMLOds;
    function Get_EventLog: IXMLEventLog;
    function Get_Framework: IXMLFramework;
    function Get_Plugins: IXMLPlugins;
    function Get_Watches: IXMLWatches;
    { Methods & Properties }
    property General: IXMLGeneral read Get_General;
    property AppDisplay: IXMLAppDisplay read Get_AppDisplay;
    property Tail: IXMLTail read Get_Tail;
    property TextExport: IXMLTextExport read Get_TextExport;
    property Ods: IXMLOds read Get_Ods;
    property EventLog: IXMLEventLog read Get_EventLog;
    property Framework: IXMLFramework read Get_Framework;
    property Plugins: IXMLPlugins read Get_Plugins;
    property Watches: IXMLWatches read Get_Watches;
  end;

{ IXMLGeneral }

  IXMLGeneral = interface(IXMLNode)
    ['{89FAAF1A-D305-4899-A240-7733D65EC0D8}']
    { Property Accessors }
    function Get_ShowSocketWarning: IXMLBooleanTagValue;
    function Get_LastSavedPath: IXMLStringTagValue;
    function Get_InternalLog: IXMLStringTagValue;
    function Get_SocketPort: IXMLIntegerTagValue;
    function Get_SocketPort2: IXMLIntegerTagValue;
    function Get_HTTPPort: IXMLIntegerTagValue;
    function Get_SocketPolicyServer: IXMLBooleanTagValue;
    function Get_HttpPolicyServer: IXMLBooleanTagValue;
    function Get_LastStyleSheet: IXMLStringTagValue;
    function Get_Udp1: IXMLBooleanTagValue;
    function Get_Udp2: IXMLBooleanTagValue;
    { Methods & Properties }
    property ShowSocketWarning: IXMLBooleanTagValue read Get_ShowSocketWarning;
    property LastSavedPath: IXMLStringTagValue read Get_LastSavedPath;
    property InternalLog: IXMLStringTagValue read Get_InternalLog;
    property SocketPort: IXMLIntegerTagValue read Get_SocketPort;
    property SocketPort2: IXMLIntegerTagValue read Get_SocketPort2;
    property HTTPPort: IXMLIntegerTagValue read Get_HTTPPort;
    property SocketPolicyServer: IXMLBooleanTagValue read Get_SocketPolicyServer;
    property HttpPolicyServer: IXMLBooleanTagValue read Get_HttpPolicyServer;
    property LastStyleSheet: IXMLStringTagValue read Get_LastStyleSheet;
    property Udp1: IXMLBooleanTagValue read Get_Udp1;
    property Udp2: IXMLBooleanTagValue read Get_Udp2;
  end;

{ IXMLBooleanTagValue }

  IXMLBooleanTagValue = interface(IXMLNode)
    ['{491DDE8F-3BD4-451A-A8F2-BCA1CCE8A2FE}']
    { Property Accessors }
    function Get_Value: Boolean;
    procedure Set_Value(Value: Boolean);
    { Methods & Properties }
    property Value: Boolean read Get_Value write Set_Value;
  end;

{ IXMLStringTagValue }

  IXMLStringTagValue = interface(IXMLNode)
    ['{38ACFA52-3F2F-474D-9F36-326D1521E275}']
    { Property Accessors }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLIntegerTagValue }

  IXMLIntegerTagValue = interface(IXMLNode)
    ['{CF8EF074-D7A6-467D-BE3C-6AF780D91E2A}']
    { Property Accessors }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
    { Methods & Properties }
    property Value: Integer read Get_Value write Set_Value;
  end;

{ IXMLAppDisplay }

  IXMLAppDisplay = interface(IXMLNode)
    ['{7F2812E8-C072-4627-A5D2-9BC077DF9E27}']
    { Property Accessors }
    function Get_Left: IXMLIntegerTagValue;
    function Get_Top: IXMLIntegerTagValue;
    function Get_Width: IXMLIntegerTagValue;
    function Get_Height: IXMLIntegerTagValue;
    function Get_StayOnTop: IXMLBooleanTagValue;
    function Get_SmallBut: IXMLBooleanTagValue;
    function Get_ApplicationTitle: IXMLStringTagValue;
    function Get_ShowOnstartup: IXMLBooleanTagValue;
    function Get_ShowOnMessageReceived: IXMLBooleanTagValue;
    function Get_FocusToReceivedMessage: IXMLBooleanTagValue;
    function Get_IconFile: IXMLStringTagValue;
    function Get_Maximized: IXMLBooleanTagValue;
    function Get_MinimizeToSystray: IXMLBooleanTagValue;
    function Get_SearchUnderline: IXMLBooleanTagValue;
    function Get_ToolbarStandard: IXMLBooleanTagValue;
    function Get_ToolbarSearch: IXMLBooleanTagValue;
    function Get_ToolbarBookmark: IXMLBooleanTagValue;
    function Get_ToolbarFilter: IXMLBooleanTagValue;
    function Get_HideViewer: IXMLBooleanTagValue;
    function Get_DisableInternalLog: IXMLBooleanTagValue;
    { Methods & Properties }
    property Left: IXMLIntegerTagValue read Get_Left;
    property Top: IXMLIntegerTagValue read Get_Top;
    property Width: IXMLIntegerTagValue read Get_Width;
    property Height: IXMLIntegerTagValue read Get_Height;
    property StayOnTop: IXMLBooleanTagValue read Get_StayOnTop;
    property SmallBut: IXMLBooleanTagValue read Get_SmallBut;
    property ApplicationTitle: IXMLStringTagValue read Get_ApplicationTitle;
    property ShowOnstartup: IXMLBooleanTagValue read Get_ShowOnstartup;
    property ShowOnMessageReceived: IXMLBooleanTagValue read Get_ShowOnMessageReceived;
    property FocusToReceivedMessage: IXMLBooleanTagValue read Get_FocusToReceivedMessage;
    property IconFile: IXMLStringTagValue read Get_IconFile;
    property Maximized: IXMLBooleanTagValue read Get_Maximized;
    property MinimizeToSystray: IXMLBooleanTagValue read Get_MinimizeToSystray;
    property SearchUnderline: IXMLBooleanTagValue read Get_SearchUnderline;
    property ToolbarStandard: IXMLBooleanTagValue read Get_ToolbarStandard;
    property ToolbarSearch: IXMLBooleanTagValue read Get_ToolbarSearch;
    property ToolbarBookmark: IXMLBooleanTagValue read Get_ToolbarBookmark;
    property ToolbarFilter: IXMLBooleanTagValue read Get_ToolbarFilter;
    property HideViewer: IXMLBooleanTagValue read Get_HideViewer;
    property DisableInternalLog: IXMLBooleanTagValue read Get_DisableInternalLog;
  end;

{ IXMLTail }

  IXMLTail = interface(IXMLNode)
    ['{0F1BD76B-64CC-4431-A902-072EE45CC5EB}']
    { Property Accessors }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_AutoClear: IXMLBooleanTagValue;
    function Get_MaxNode: IXMLIntegerTagValue;
    function Get_MinNode: IXMLIntegerTagValue;
    function Get_SizeToLoad: IXMLIntegerTagValue;
    function Get_LastPath: IXMLStringTagValue;
    function Get_ColumnStyle: IXMLStringTagValue;
    function Get_AutoCreateColStyle: IXMLStringTagValue;
    function Get_TextQualifier: IXMLStringTagValue;
    function Get_Separator: IXMLStringTagValue;
    function Get_FirstcolIsTitle: IXMLBooleanTagValue;
    function Get_FixedColCount: IXMLIntegerTagValue;
    function Get_Favorites: IXMLFavorites;
    function Get_OpenFromFavorites: IXMLBooleanTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
    { Methods & Properties }
    property VisibleMenu: IXMLBooleanTagValue read Get_VisibleMenu;
    property AutoClear: IXMLBooleanTagValue read Get_AutoClear;
    property MaxNode: IXMLIntegerTagValue read Get_MaxNode;
    property MinNode: IXMLIntegerTagValue read Get_MinNode;
    property SizeToLoad: IXMLIntegerTagValue read Get_SizeToLoad;
    property LastPath: IXMLStringTagValue read Get_LastPath;
    property ColumnStyle: IXMLStringTagValue read Get_ColumnStyle;
    property AutoCreateColStyle: IXMLStringTagValue read Get_AutoCreateColStyle;
    property TextQualifier: IXMLStringTagValue read Get_TextQualifier;
    property Separator: IXMLStringTagValue read Get_Separator;
    property FirstcolIsTitle: IXMLBooleanTagValue read Get_FirstcolIsTitle;
    property FixedColCount: IXMLIntegerTagValue read Get_FixedColCount;
    property Favorites: IXMLFavorites read Get_Favorites;
    property OpenFromFavorites: IXMLBooleanTagValue read Get_OpenFromFavorites;
    property Trace: IXMLDisplayNode read Get_Trace;
    property Info: IXMLDisplayNode read Get_Info;
  end;

{ IXMLFavorites }

  IXMLFavorites = interface(IXMLNodeCollection)
    ['{5E843F42-815D-412C-9AC2-4E8A5FBCC84C}']
    { Property Accessors }
    function Get_FileName(Index: Integer): WideString;
    { Methods & Properties }
    function Add(const FileName: WideString): IXMLNode;
    function Insert(const Index: Integer; const FileName: WideString): IXMLNode;
    property FileName[Index: Integer]: WideString read Get_FileName; default;
  end;

{ IXMLDisplayNode }

  IXMLDisplayNode = interface(IXMLNode)
    ['{2AB7537C-4181-4984-A95A-926D350180AC}']
    { Property Accessors }
    function Get_NodeHeight: IXMLIntegerTagValue;
    function Get_FontSize: IXMLIntegerTagValue;
    function Get_FontName: IXMLStringTagValue;
    { Methods & Properties }
    property NodeHeight: IXMLIntegerTagValue read Get_NodeHeight;
    property FontSize: IXMLIntegerTagValue read Get_FontSize;
    property FontName: IXMLStringTagValue read Get_FontName;
  end;

{ IXMLTextExport }

  IXMLTextExport = interface(IXMLNode)
    ['{1010B82F-A665-4769-B4AC-D4249D829EB5}']
    { Property Accessors }
    function Get_ProcessName: IXMLColumnTagValue;
    function Get_ThreadId: IXMLColumnTagValue;
    function Get_Time: IXMLColumnTagValue;
    function Get_Col1: IXMLColumnTagValue;
    function Get_Col2: IXMLColumnTagValue;
    function Get_GenerateColumnHeader: IXMLBooleanTagValue;
    function Get_TextQualifier: IXMLStringTagValue;
    function Get_Separator: IXMLStringTagValue;
    function Get_TreeIndentation: IXMLIntegerTagValue;
    { Methods & Properties }
    property ProcessName: IXMLColumnTagValue read Get_ProcessName;
    property ThreadId: IXMLColumnTagValue read Get_ThreadId;
    property Time: IXMLColumnTagValue read Get_Time;
    property Col1: IXMLColumnTagValue read Get_Col1;
    property Col2: IXMLColumnTagValue read Get_Col2;
    property GenerateColumnHeader: IXMLBooleanTagValue read Get_GenerateColumnHeader;
    property TextQualifier: IXMLStringTagValue read Get_TextQualifier;
    property Separator: IXMLStringTagValue read Get_Separator;
    property TreeIndentation: IXMLIntegerTagValue read Get_TreeIndentation;
  end;

{ IXMLColumnTagValue }

  IXMLColumnTagValue = interface(IXMLNode)
    ['{479CF710-E0D4-4271-BD05-BDB88261BC46}']
    { Property Accessors }
    function Get_Value: Boolean;
    function Get_Position: Integer;
    procedure Set_Value(Value: Boolean);
    procedure Set_Position(Value: Integer);
    { Methods & Properties }
    property Value: Boolean read Get_Value write Set_Value;
    property Position: Integer read Get_Position write Set_Position;
  end;

{ IXMLOds }

  IXMLOds = interface(IXMLNode)
    ['{A4C433EF-0CD4-411D-BE0C-180B419936BF}']
    { Property Accessors }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_AutoClear: IXMLBooleanTagValue;
    function Get_MaxNode: IXMLIntegerTagValue;
    function Get_MinNode: IXMLIntegerTagValue;
    function Get_Enabled: IXMLBooleanTagValue;
    function Get_Title: IXMLStringTagValue;
    function Get_Info: IXMLDisplayNode;
    function Get_Trace: IXMLDisplayNode;
    { Methods & Properties }
    property VisibleMenu: IXMLBooleanTagValue read Get_VisibleMenu;
    property AutoClear: IXMLBooleanTagValue read Get_AutoClear;
    property MaxNode: IXMLIntegerTagValue read Get_MaxNode;
    property MinNode: IXMLIntegerTagValue read Get_MinNode;
    property Enabled: IXMLBooleanTagValue read Get_Enabled;
    property Title: IXMLStringTagValue read Get_Title;
    property Info: IXMLDisplayNode read Get_Info;
    property Trace: IXMLDisplayNode read Get_Trace;
  end;

{ IXMLEventLog }

  IXMLEventLog = interface(IXMLNode)
    ['{86AC8B3D-3753-4BCD-8980-103968714E10}']
    { Property Accessors }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
    { Methods & Properties }
    property VisibleMenu: IXMLBooleanTagValue read Get_VisibleMenu;
    property Trace: IXMLDisplayNode read Get_Trace;
    property Info: IXMLDisplayNode read Get_Info;
  end;

{ IXMLFramework }

  IXMLFramework = interface(IXMLNode)
    ['{7BBCD400-199B-4DB6-8451-2A02FBB278A1}']
    { Property Accessors }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_AutoClear: IXMLBooleanTagValue;
    function Get_MaxNode: IXMLIntegerTagValue;
    function Get_MinNode: IXMLIntegerTagValue;
    function Get_Enabled: IXMLBooleanTagValue;
    function Get_ShowMembers: IXMLBooleanTagValue;
    function Get_MainTraceTitle: IXMLStringTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
    function Get_Orphans: IXMLOrphans;
    { Methods & Properties }
    property VisibleMenu: IXMLBooleanTagValue read Get_VisibleMenu;
    property AutoClear: IXMLBooleanTagValue read Get_AutoClear;
    property MaxNode: IXMLIntegerTagValue read Get_MaxNode;
    property MinNode: IXMLIntegerTagValue read Get_MinNode;
    property Enabled: IXMLBooleanTagValue read Get_Enabled;
    property ShowMembers: IXMLBooleanTagValue read Get_ShowMembers;
    property MainTraceTitle: IXMLStringTagValue read Get_MainTraceTitle;
    property Trace: IXMLDisplayNode read Get_Trace;
    property Info: IXMLDisplayNode read Get_Info;
    property Orphans: IXMLOrphans read Get_Orphans;
  end;

{ IXMLOrphans }

  IXMLOrphans = interface(IXMLNode)
    ['{D2936DBE-29CC-4E5A-9723-0ADC9DF9972F}']
    { Property Accessors }
    function Get_DeletedNode: IXMLStringTagValue;
    function Get_DefaultLeftText: IXMLStringTagValue;
    function Get_DefaultRightText: IXMLStringTagValue;
    function Get_LostAndFoundLeftText: IXMLStringTagValue;
    function Get_LostAndFoundRightText: IXMLStringTagValue;
    { Methods & Properties }
    property DeletedNode: IXMLStringTagValue read Get_DeletedNode;
    property DefaultLeftText: IXMLStringTagValue read Get_DefaultLeftText;
    property DefaultRightText: IXMLStringTagValue read Get_DefaultRightText;
    property LostAndFoundLeftText: IXMLStringTagValue read Get_LostAndFoundLeftText;
    property LostAndFoundRightText: IXMLStringTagValue read Get_LostAndFoundRightText;
  end;

{ IXMLPlugins }

  IXMLPlugins = interface(IXMLNode)
    ['{1C0D6009-B26C-4495-BF62-51AB5B6E2D74}']
    { Property Accessors }
    function Get_JVMEngine: IXMLStringTagValue;
    function Get_JavaPLuginClassPath: IXMLStringTagValue;
    function Get_Plugin: IXMLPluginList;
    { Methods & Properties }
    property JVMEngine: IXMLStringTagValue read Get_JVMEngine;
    property JavaPLuginClassPath: IXMLStringTagValue read Get_JavaPLuginClassPath;
    property Plugin: IXMLPluginList read Get_Plugin;
  end;

{ IXMLPlugin }

  IXMLPlugin = interface(IXMLNode)
    ['{B2196454-E963-487E-9636-CBD0D5D201FE}']
    { Property Accessors }
    function Get_FileName: WideString;
    function Get_ClassName: WideString;
    function Get_Kind: WideString;
    function Get_Param: WideString;
    function Get_Enabled: IXMLBooleanTagValue;
    procedure Set_FileName(Value: WideString);
    procedure Set_ClassName(Value: WideString);
    procedure Set_Kind(Value: WideString);
    procedure Set_Param(Value: WideString);
    { Methods & Properties }
    property FileName: WideString read Get_FileName write Set_FileName;
    property ClassName: WideString read Get_ClassName write Set_ClassName;
    property Kind: WideString read Get_Kind write Set_Kind;
    property Param: WideString read Get_Param write Set_Param;
    property Enabled: IXMLBooleanTagValue read Get_Enabled;
  end;

{ IXMLPluginList }

  IXMLPluginList = interface(IXMLNodeCollection)
    ['{EA385043-FC13-489D-9225-A9F761472174}']
    { Methods & Properties }
    function Add: IXMLPlugin;
    function Insert(const Index: Integer): IXMLPlugin;
    function Get_Item(Index: Integer): IXMLPlugin;
    property Items[Index: Integer]: IXMLPlugin read Get_Item; default;
  end;

{ IXMLWatches }

  IXMLWatches = interface(IXMLNode)
    ['{CDFEB6A2-5A1D-401F-B127-E74E7AA8EFA8}']
    { Property Accessors }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_Enabled: IXMLBooleanTagValue;
    function Get_MainWatchesTitle: IXMLStringTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
    { Methods & Properties }
    property VisibleMenu: IXMLBooleanTagValue read Get_VisibleMenu;
    property Enabled: IXMLBooleanTagValue read Get_Enabled;
    property MainWatchesTitle: IXMLStringTagValue read Get_MainWatchesTitle;
    property Trace: IXMLDisplayNode read Get_Trace;
    property Info: IXMLDisplayNode read Get_Info;
  end;

{ IXMLStringTagValue5 }

  IXMLStringTagValue5 = interface(IXMLNode)
    ['{4DF49D75-E49B-436B-91AF-3224CFE45F0C}']
    { Property Accessors }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLStringTagValue4 }

  IXMLStringTagValue4 = interface(IXMLNode)
    ['{DB2CA20B-D47F-4792-9D9E-80013541D5A8}']
    { Property Accessors }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLStringTagValue3 }

  IXMLStringTagValue3 = interface(IXMLNode)
    ['{CB5446E4-AD03-46E2-B627-7B3CC00CE921}']
    { Property Accessors }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLStringTagValue2 }

  IXMLStringTagValue2 = interface(IXMLNode)
    ['{B98CEF4E-A526-4899-AB16-2F8A5019C72F}']
    { Property Accessors }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLIntegerTagValue3 }

  IXMLIntegerTagValue3 = interface(IXMLNode)
    ['{C3413461-7951-4324-843E-BE8272AC8364}']
    { Property Accessors }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
    { Methods & Properties }
    property Value: Integer read Get_Value write Set_Value;
  end;

{ IXMLIntegerTagValue2 }

  IXMLIntegerTagValue2 = interface(IXMLNode)
    ['{CDD34281-5198-475B-85E2-53E68220AC3A}']
    { Property Accessors }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
    { Methods & Properties }
    property Value: Integer read Get_Value write Set_Value;
  end;

{ Forward Decls }

  TXMLConfig = class;
  TXMLGeneral = class;
  TXMLBooleanTagValue = class;
  TXMLStringTagValue = class;
  TXMLIntegerTagValue = class;
  TXMLAppDisplay = class;
  TXMLTail = class;
  TXMLFavorites = class;
  TXMLDisplayNode = class;
  TXMLTextExport = class;
  TXMLColumnTagValue = class;
  TXMLOds = class;
  TXMLEventLog = class;
  TXMLFramework = class;
  TXMLOrphans = class;
  TXMLPlugins = class;
  TXMLPlugin = class;
  TXMLPluginList = class;
  TXMLWatches = class;
  TXMLStringTagValue5 = class;
  TXMLStringTagValue4 = class;
  TXMLStringTagValue3 = class;
  TXMLStringTagValue2 = class;
  TXMLIntegerTagValue3 = class;
  TXMLIntegerTagValue2 = class;

{ TXMLConfig }

  TXMLConfig = class(TXMLNode, IXMLConfig)
  protected
    { IXMLConfig }
    function Get_General: IXMLGeneral;
    function Get_AppDisplay: IXMLAppDisplay;
    function Get_Tail: IXMLTail;
    function Get_TextExport: IXMLTextExport;
    function Get_Ods: IXMLOds;
    function Get_EventLog: IXMLEventLog;
    function Get_Framework: IXMLFramework;
    function Get_Plugins: IXMLPlugins;
    function Get_Watches: IXMLWatches;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGeneral }

  TXMLGeneral = class(TXMLNode, IXMLGeneral)
  protected
    { IXMLGeneral }
    function Get_ShowSocketWarning: IXMLBooleanTagValue;
    function Get_LastSavedPath: IXMLStringTagValue;
    function Get_InternalLog: IXMLStringTagValue;
    function Get_SocketPort: IXMLIntegerTagValue;
    function Get_SocketPort2: IXMLIntegerTagValue;
    function Get_HTTPPort: IXMLIntegerTagValue;
    function Get_SocketPolicyServer: IXMLBooleanTagValue;
    function Get_HttpPolicyServer: IXMLBooleanTagValue;
    function Get_LastStyleSheet: IXMLStringTagValue;
    function Get_Udp1: IXMLBooleanTagValue;
    function Get_Udp2: IXMLBooleanTagValue;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBooleanTagValue }

  TXMLBooleanTagValue = class(TXMLNode, IXMLBooleanTagValue)
  protected
    { IXMLBooleanTagValue }
    function Get_Value: Boolean;
    procedure Set_Value(Value: Boolean);
  end;

{ TXMLStringTagValue }

  TXMLStringTagValue = class(TXMLNode, IXMLStringTagValue)
  protected
    { IXMLStringTagValue }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
  end;

{ TXMLIntegerTagValue }

  TXMLIntegerTagValue = class(TXMLNode, IXMLIntegerTagValue)
  protected
    { IXMLIntegerTagValue }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
  end;

{ TXMLAppDisplay }

  TXMLAppDisplay = class(TXMLNode, IXMLAppDisplay)
  protected
    { IXMLAppDisplay }
    function Get_Left: IXMLIntegerTagValue;
    function Get_Top: IXMLIntegerTagValue;
    function Get_Width: IXMLIntegerTagValue;
    function Get_Height: IXMLIntegerTagValue;
    function Get_StayOnTop: IXMLBooleanTagValue;
    function Get_SmallBut: IXMLBooleanTagValue;
    function Get_ApplicationTitle: IXMLStringTagValue;
    function Get_ShowOnstartup: IXMLBooleanTagValue;
    function Get_ShowOnMessageReceived: IXMLBooleanTagValue;
    function Get_FocusToReceivedMessage: IXMLBooleanTagValue;
    function Get_IconFile: IXMLStringTagValue;
    function Get_Maximized: IXMLBooleanTagValue;
    function Get_MinimizeToSystray: IXMLBooleanTagValue;
    function Get_SearchUnderline: IXMLBooleanTagValue;
    function Get_ToolbarStandard: IXMLBooleanTagValue;
    function Get_ToolbarSearch: IXMLBooleanTagValue;
    function Get_ToolbarBookmark: IXMLBooleanTagValue;
    function Get_ToolbarFilter: IXMLBooleanTagValue;
    function Get_HideViewer: IXMLBooleanTagValue;
    function Get_DisableInternalLog: IXMLBooleanTagValue;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTail }

  TXMLTail = class(TXMLNode, IXMLTail)
  protected
    { IXMLTail }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_AutoClear: IXMLBooleanTagValue;
    function Get_MaxNode: IXMLIntegerTagValue;
    function Get_MinNode: IXMLIntegerTagValue;
    function Get_SizeToLoad: IXMLIntegerTagValue;
    function Get_LastPath: IXMLStringTagValue;
    function Get_ColumnStyle: IXMLStringTagValue;
    function Get_AutoCreateColStyle: IXMLStringTagValue;
    function Get_TextQualifier: IXMLStringTagValue;
    function Get_Separator: IXMLStringTagValue;
    function Get_FirstcolIsTitle: IXMLBooleanTagValue;
    function Get_FixedColCount: IXMLIntegerTagValue;
    function Get_Favorites: IXMLFavorites;
    function Get_OpenFromFavorites: IXMLBooleanTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFavorites }

  TXMLFavorites = class(TXMLNodeCollection, IXMLFavorites)
  protected
    { IXMLFavorites }
    function Get_FileName(Index: Integer): WideString;
    function Add(const FileName: WideString): IXMLNode;
    function Insert(const Index: Integer; const FileName: WideString): IXMLNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDisplayNode }

  TXMLDisplayNode = class(TXMLNode, IXMLDisplayNode)
  protected
    { IXMLDisplayNode }
    function Get_NodeHeight: IXMLIntegerTagValue;
    function Get_FontSize: IXMLIntegerTagValue;
    function Get_FontName: IXMLStringTagValue;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextExport }

  TXMLTextExport = class(TXMLNode, IXMLTextExport)
  protected
    { IXMLTextExport }
    function Get_ProcessName: IXMLColumnTagValue;
    function Get_ThreadId: IXMLColumnTagValue;
    function Get_Time: IXMLColumnTagValue;
    function Get_Col1: IXMLColumnTagValue;
    function Get_Col2: IXMLColumnTagValue;
    function Get_GenerateColumnHeader: IXMLBooleanTagValue;
    function Get_TextQualifier: IXMLStringTagValue;
    function Get_Separator: IXMLStringTagValue;
    function Get_TreeIndentation: IXMLIntegerTagValue;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLColumnTagValue }

  TXMLColumnTagValue = class(TXMLNode, IXMLColumnTagValue)
  protected
    { IXMLColumnTagValue }
    function Get_Value: Boolean;
    function Get_Position: Integer;
    procedure Set_Value(Value: Boolean);
    procedure Set_Position(Value: Integer);
  end;

{ TXMLOds }

  TXMLOds = class(TXMLNode, IXMLOds)
  protected
    { IXMLOds }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_AutoClear: IXMLBooleanTagValue;
    function Get_MaxNode: IXMLIntegerTagValue;
    function Get_MinNode: IXMLIntegerTagValue;
    function Get_Enabled: IXMLBooleanTagValue;
    function Get_Title: IXMLStringTagValue;
    function Get_Info: IXMLDisplayNode;
    function Get_Trace: IXMLDisplayNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLEventLog }

  TXMLEventLog = class(TXMLNode, IXMLEventLog)
  protected
    { IXMLEventLog }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFramework }

  TXMLFramework = class(TXMLNode, IXMLFramework)
  protected
    { IXMLFramework }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_AutoClear: IXMLBooleanTagValue;
    function Get_MaxNode: IXMLIntegerTagValue;
    function Get_MinNode: IXMLIntegerTagValue;
    function Get_Enabled: IXMLBooleanTagValue;
    function Get_ShowMembers: IXMLBooleanTagValue;
    function Get_MainTraceTitle: IXMLStringTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
    function Get_Orphans: IXMLOrphans;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLOrphans }

  TXMLOrphans = class(TXMLNode, IXMLOrphans)
  protected
    { IXMLOrphans }
    function Get_DeletedNode: IXMLStringTagValue;
    function Get_DefaultLeftText: IXMLStringTagValue;
    function Get_DefaultRightText: IXMLStringTagValue;
    function Get_LostAndFoundLeftText: IXMLStringTagValue;
    function Get_LostAndFoundRightText: IXMLStringTagValue;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPlugins }

  TXMLPlugins = class(TXMLNode, IXMLPlugins)
  private
    FPlugin: IXMLPluginList;
  protected
    { IXMLPlugins }
    function Get_JVMEngine: IXMLStringTagValue;
    function Get_JavaPLuginClassPath: IXMLStringTagValue;
    function Get_Plugin: IXMLPluginList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPlugin }

  TXMLPlugin = class(TXMLNode, IXMLPlugin)
  protected
    { IXMLPlugin }
    function Get_FileName: WideString;
    function Get_ClassName: WideString;
    function Get_Kind: WideString;
    function Get_Param: WideString;
    function Get_Enabled: IXMLBooleanTagValue;
    procedure Set_FileName(Value: WideString);
    procedure Set_ClassName(Value: WideString);
    procedure Set_Kind(Value: WideString);
    procedure Set_Param(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPluginList }

  TXMLPluginList = class(TXMLNodeCollection, IXMLPluginList)
  protected
    { IXMLPluginList }
    function Add: IXMLPlugin;
    function Insert(const Index: Integer): IXMLPlugin;
    function Get_Item(Index: Integer): IXMLPlugin;
  end;

{ TXMLWatches }

  TXMLWatches = class(TXMLNode, IXMLWatches)
  protected
    { IXMLWatches }
    function Get_VisibleMenu: IXMLBooleanTagValue;
    function Get_Enabled: IXMLBooleanTagValue;
    function Get_MainWatchesTitle: IXMLStringTagValue;
    function Get_Trace: IXMLDisplayNode;
    function Get_Info: IXMLDisplayNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStringTagValue5 }

  TXMLStringTagValue5 = class(TXMLNode, IXMLStringTagValue5)
  protected
    { IXMLStringTagValue5 }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
  end;

{ TXMLStringTagValue4 }

  TXMLStringTagValue4 = class(TXMLNode, IXMLStringTagValue4)
  protected
    { IXMLStringTagValue4 }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
  end;

{ TXMLStringTagValue3 }

  TXMLStringTagValue3 = class(TXMLNode, IXMLStringTagValue3)
  protected
    { IXMLStringTagValue3 }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
  end;

{ TXMLStringTagValue2 }

  TXMLStringTagValue2 = class(TXMLNode, IXMLStringTagValue2)
  protected
    { IXMLStringTagValue2 }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
  end;

{ TXMLIntegerTagValue3 }

  TXMLIntegerTagValue3 = class(TXMLNode, IXMLIntegerTagValue3)
  protected
    { IXMLIntegerTagValue3 }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
  end;

{ TXMLIntegerTagValue2 }

  TXMLIntegerTagValue2 = class(TXMLNode, IXMLIntegerTagValue2)
  protected
    { IXMLIntegerTagValue2 }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
  end;

{ Global Functions }

function GetConfig(Doc: IXMLDocument): IXMLConfig;
function LoadConfig(const FileName: WideString): IXMLConfig;
function NewConfig: IXMLConfig;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetConfig(Doc: IXMLDocument): IXMLConfig;
begin
  Result := Doc.GetDocBinding('Config', TXMLConfig, TargetNamespace) as IXMLConfig;
end;

function LoadConfig(const FileName: WideString): IXMLConfig;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('Config', TXMLConfig, TargetNamespace) as IXMLConfig;
end;

function NewConfig: IXMLConfig;
begin
  Result := NewXMLDocument.GetDocBinding('Config', TXMLConfig, TargetNamespace) as IXMLConfig;
end;

{ TXMLConfig }

procedure TXMLConfig.AfterConstruction;
begin
  RegisterChildNode('General', TXMLGeneral);
  RegisterChildNode('AppDisplay', TXMLAppDisplay);
  RegisterChildNode('Tail', TXMLTail);
  RegisterChildNode('TextExport', TXMLTextExport);
  RegisterChildNode('Ods', TXMLOds);
  RegisterChildNode('EventLog', TXMLEventLog);
  RegisterChildNode('Framework', TXMLFramework);
  RegisterChildNode('Plugins', TXMLPlugins);
  RegisterChildNode('Watches', TXMLWatches);
  inherited;
end;

function TXMLConfig.Get_General: IXMLGeneral;
begin
  Result := ChildNodes['General'] as IXMLGeneral;
end;

function TXMLConfig.Get_AppDisplay: IXMLAppDisplay;
begin
  Result := ChildNodes['AppDisplay'] as IXMLAppDisplay;
end;

function TXMLConfig.Get_Tail: IXMLTail;
begin
  Result := ChildNodes['Tail'] as IXMLTail;
end;

function TXMLConfig.Get_TextExport: IXMLTextExport;
begin
  Result := ChildNodes['TextExport'] as IXMLTextExport;
end;

function TXMLConfig.Get_Ods: IXMLOds;
begin
  Result := ChildNodes['Ods'] as IXMLOds;
end;

function TXMLConfig.Get_EventLog: IXMLEventLog;
begin
  Result := ChildNodes['EventLog'] as IXMLEventLog;
end;

function TXMLConfig.Get_Framework: IXMLFramework;
begin
  Result := ChildNodes['Framework'] as IXMLFramework;
end;

function TXMLConfig.Get_Plugins: IXMLPlugins;
begin
  Result := ChildNodes['Plugins'] as IXMLPlugins;
end;

function TXMLConfig.Get_Watches: IXMLWatches;
begin
  Result := ChildNodes['Watches'] as IXMLWatches;
end;

{ TXMLGeneral }

procedure TXMLGeneral.AfterConstruction;
begin
  RegisterChildNode('ShowSocketWarning', TXMLBooleanTagValue);
  RegisterChildNode('LastSavedPath', TXMLStringTagValue);
  RegisterChildNode('InternalLog', TXMLStringTagValue);
  RegisterChildNode('SocketPort', TXMLIntegerTagValue);
  RegisterChildNode('SocketPort2', TXMLIntegerTagValue);
  RegisterChildNode('HTTPPort', TXMLIntegerTagValue);
  RegisterChildNode('SocketPolicyServer', TXMLBooleanTagValue);
  RegisterChildNode('HttpPolicyServer', TXMLBooleanTagValue);
  RegisterChildNode('LastStyleSheet', TXMLStringTagValue);
  RegisterChildNode('Udp1', TXMLBooleanTagValue);
  RegisterChildNode('Udp2', TXMLBooleanTagValue);
  inherited;
end;

function TXMLGeneral.Get_ShowSocketWarning: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ShowSocketWarning'] as IXMLBooleanTagValue;
end;

function TXMLGeneral.Get_LastSavedPath: IXMLStringTagValue;
begin
  Result := ChildNodes['LastSavedPath'] as IXMLStringTagValue;
end;

function TXMLGeneral.Get_InternalLog: IXMLStringTagValue;
begin
  Result := ChildNodes['InternalLog'] as IXMLStringTagValue;
end;

function TXMLGeneral.Get_SocketPort: IXMLIntegerTagValue;
begin
  Result := ChildNodes['SocketPort'] as IXMLIntegerTagValue;
end;

function TXMLGeneral.Get_SocketPort2: IXMLIntegerTagValue;
begin
  Result := ChildNodes['SocketPort2'] as IXMLIntegerTagValue;
end;

function TXMLGeneral.Get_HTTPPort: IXMLIntegerTagValue;
begin
  Result := ChildNodes['HTTPPort'] as IXMLIntegerTagValue;
end;

function TXMLGeneral.Get_SocketPolicyServer: IXMLBooleanTagValue;
begin
  Result := ChildNodes['SocketPolicyServer'] as IXMLBooleanTagValue;
end;

function TXMLGeneral.Get_HttpPolicyServer: IXMLBooleanTagValue;
begin
  Result := ChildNodes['HttpPolicyServer'] as IXMLBooleanTagValue;
end;

function TXMLGeneral.Get_LastStyleSheet: IXMLStringTagValue;
begin
  Result := ChildNodes['LastStyleSheet'] as IXMLStringTagValue;
end;

function TXMLGeneral.Get_Udp1: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Udp1'] as IXMLBooleanTagValue;
end;

function TXMLGeneral.Get_Udp2: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Udp2'] as IXMLBooleanTagValue;
end;

{ TXMLBooleanTagValue }

function TXMLBooleanTagValue.Get_Value: Boolean;
begin
  Result := AttributeNodes['Value'].NodeValue;
end;

procedure TXMLBooleanTagValue.Set_Value(Value: Boolean);
begin
  SetAttribute('Value', Value);
end;

{ TXMLStringTagValue }

function TXMLStringTagValue.Get_Value: WideString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLStringTagValue.Set_Value(Value: WideString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLIntegerTagValue }

function TXMLIntegerTagValue.Get_Value: Integer;
begin
  Result := AttributeNodes['Value'].NodeValue;
end;

procedure TXMLIntegerTagValue.Set_Value(Value: Integer);
begin
  SetAttribute('Value', Value);
end;

{ TXMLAppDisplay }

procedure TXMLAppDisplay.AfterConstruction;
begin
  RegisterChildNode('Left', TXMLIntegerTagValue);
  RegisterChildNode('Top', TXMLIntegerTagValue);
  RegisterChildNode('Width', TXMLIntegerTagValue);
  RegisterChildNode('Height', TXMLIntegerTagValue);
  RegisterChildNode('StayOnTop', TXMLBooleanTagValue);
  RegisterChildNode('SmallBut', TXMLBooleanTagValue);
  RegisterChildNode('ApplicationTitle', TXMLStringTagValue);
  RegisterChildNode('ShowOnstartup', TXMLBooleanTagValue);
  RegisterChildNode('ShowOnMessageReceived', TXMLBooleanTagValue);
  RegisterChildNode('FocusToReceivedMessage', TXMLBooleanTagValue);
  RegisterChildNode('IconFile', TXMLStringTagValue);
  RegisterChildNode('Maximized', TXMLBooleanTagValue);
  RegisterChildNode('MinimizeToSystray', TXMLBooleanTagValue);
  RegisterChildNode('SearchUnderline', TXMLBooleanTagValue);
  RegisterChildNode('ToolbarStandard', TXMLBooleanTagValue);
  RegisterChildNode('ToolbarSearch', TXMLBooleanTagValue);
  RegisterChildNode('ToolbarBookmark', TXMLBooleanTagValue);
  RegisterChildNode('ToolbarFilter', TXMLBooleanTagValue);
  RegisterChildNode('HideViewer', TXMLBooleanTagValue);
  RegisterChildNode('DisableInternalLog', TXMLBooleanTagValue);
  inherited;
end;

function TXMLAppDisplay.Get_Left: IXMLIntegerTagValue;
begin
  Result := ChildNodes['Left'] as IXMLIntegerTagValue;
end;

function TXMLAppDisplay.Get_Top: IXMLIntegerTagValue;
begin
  Result := ChildNodes['Top'] as IXMLIntegerTagValue;
end;

function TXMLAppDisplay.Get_Width: IXMLIntegerTagValue;
begin
  Result := ChildNodes['Width'] as IXMLIntegerTagValue;
end;

function TXMLAppDisplay.Get_Height: IXMLIntegerTagValue;
begin
  Result := ChildNodes['Height'] as IXMLIntegerTagValue;
end;

function TXMLAppDisplay.Get_StayOnTop: IXMLBooleanTagValue;
begin
  Result := ChildNodes['StayOnTop'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_SmallBut: IXMLBooleanTagValue;
begin
  Result := ChildNodes['SmallBut'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_ApplicationTitle: IXMLStringTagValue;
begin
  Result := ChildNodes['ApplicationTitle'] as IXMLStringTagValue;
end;

function TXMLAppDisplay.Get_ShowOnstartup: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ShowOnstartup'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_ShowOnMessageReceived: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ShowOnMessageReceived'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_FocusToReceivedMessage: IXMLBooleanTagValue;
begin
  Result := ChildNodes['FocusToReceivedMessage'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_IconFile: IXMLStringTagValue;
begin
  Result := ChildNodes['IconFile'] as IXMLStringTagValue;
end;

function TXMLAppDisplay.Get_Maximized: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Maximized'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_MinimizeToSystray: IXMLBooleanTagValue;
begin
  Result := ChildNodes['MinimizeToSystray'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_SearchUnderline: IXMLBooleanTagValue;
begin
  Result := ChildNodes['SearchUnderline'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_ToolbarStandard: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ToolbarStandard'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_ToolbarSearch: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ToolbarSearch'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_ToolbarBookmark: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ToolbarBookmark'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_ToolbarFilter: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ToolbarFilter'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_HideViewer: IXMLBooleanTagValue;
begin
  Result := ChildNodes['HideViewer'] as IXMLBooleanTagValue;
end;

function TXMLAppDisplay.Get_DisableInternalLog: IXMLBooleanTagValue;
begin
  Result := ChildNodes['DisableInternalLog'] as IXMLBooleanTagValue;
end;

{ TXMLTail }

procedure TXMLTail.AfterConstruction;
begin
  RegisterChildNode('VisibleMenu', TXMLBooleanTagValue);
  RegisterChildNode('AutoClear', TXMLBooleanTagValue);
  RegisterChildNode('MaxNode', TXMLIntegerTagValue);
  RegisterChildNode('MinNode', TXMLIntegerTagValue);
  RegisterChildNode('SizeToLoad', TXMLIntegerTagValue);
  RegisterChildNode('LastPath', TXMLStringTagValue);
  RegisterChildNode('ColumnStyle', TXMLStringTagValue);
  RegisterChildNode('AutoCreateColStyle', TXMLStringTagValue);
  RegisterChildNode('TextQualifier', TXMLStringTagValue);
  RegisterChildNode('Separator', TXMLStringTagValue);
  RegisterChildNode('FirstcolIsTitle', TXMLBooleanTagValue);
  RegisterChildNode('FixedColCount', TXMLIntegerTagValue);
  RegisterChildNode('Favorites', TXMLFavorites);
  RegisterChildNode('OpenFromFavorites', TXMLBooleanTagValue);
  RegisterChildNode('Trace', TXMLDisplayNode);
  RegisterChildNode('Info', TXMLDisplayNode);
  inherited;
end;

function TXMLTail.Get_VisibleMenu: IXMLBooleanTagValue;
begin
  Result := ChildNodes['VisibleMenu'] as IXMLBooleanTagValue;
end;

function TXMLTail.Get_AutoClear: IXMLBooleanTagValue;
begin
  Result := ChildNodes['AutoClear'] as IXMLBooleanTagValue;
end;

function TXMLTail.Get_MaxNode: IXMLIntegerTagValue;
begin
  Result := ChildNodes['MaxNode'] as IXMLIntegerTagValue;
end;

function TXMLTail.Get_MinNode: IXMLIntegerTagValue;
begin
  Result := ChildNodes['MinNode'] as IXMLIntegerTagValue;
end;

function TXMLTail.Get_SizeToLoad: IXMLIntegerTagValue;
begin
  Result := ChildNodes['SizeToLoad'] as IXMLIntegerTagValue;
end;

function TXMLTail.Get_LastPath: IXMLStringTagValue;
begin
  Result := ChildNodes['LastPath'] as IXMLStringTagValue;
end;

function TXMLTail.Get_ColumnStyle: IXMLStringTagValue;
begin
  Result := ChildNodes['ColumnStyle'] as IXMLStringTagValue;
end;

function TXMLTail.Get_AutoCreateColStyle: IXMLStringTagValue;
begin
  Result := ChildNodes['AutoCreateColStyle'] as IXMLStringTagValue;
end;

function TXMLTail.Get_TextQualifier: IXMLStringTagValue;
begin
  Result := ChildNodes['TextQualifier'] as IXMLStringTagValue;
end;

function TXMLTail.Get_Separator: IXMLStringTagValue;
begin
  Result := ChildNodes['Separator'] as IXMLStringTagValue;
end;

function TXMLTail.Get_FirstcolIsTitle: IXMLBooleanTagValue;
begin
  Result := ChildNodes['FirstcolIsTitle'] as IXMLBooleanTagValue;
end;

function TXMLTail.Get_FixedColCount: IXMLIntegerTagValue;
begin
  Result := ChildNodes['FixedColCount'] as IXMLIntegerTagValue;
end;

function TXMLTail.Get_Favorites: IXMLFavorites;
begin
  Result := ChildNodes['Favorites'] as IXMLFavorites;
end;

function TXMLTail.Get_OpenFromFavorites: IXMLBooleanTagValue;
begin
  Result := ChildNodes['OpenFromFavorites'] as IXMLBooleanTagValue;
end;

function TXMLTail.Get_Trace: IXMLDisplayNode;
begin
  Result := ChildNodes['Trace'] as IXMLDisplayNode;
end;

function TXMLTail.Get_Info: IXMLDisplayNode;
begin
  Result := ChildNodes['Info'] as IXMLDisplayNode;
end;

{ TXMLFavorites }

procedure TXMLFavorites.AfterConstruction;
begin
  ItemTag := 'FileName';
  ItemInterface := IXMLNode;
  inherited;
end;

function TXMLFavorites.Get_FileName(Index: Integer): WideString;
begin
  Result := List[Index].Text;
end;

function TXMLFavorites.Add(const FileName: WideString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := FileName;
end;

function TXMLFavorites.Insert(const Index: Integer; const FileName: WideString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := FileName;
end;

{ TXMLDisplayNode }

procedure TXMLDisplayNode.AfterConstruction;
begin
  RegisterChildNode('NodeHeight', TXMLIntegerTagValue);
  RegisterChildNode('FontSize', TXMLIntegerTagValue);
  RegisterChildNode('FontName', TXMLStringTagValue);
  inherited;
end;

function TXMLDisplayNode.Get_NodeHeight: IXMLIntegerTagValue;
begin
  Result := ChildNodes['NodeHeight'] as IXMLIntegerTagValue;
end;

function TXMLDisplayNode.Get_FontSize: IXMLIntegerTagValue;
begin
  Result := ChildNodes['FontSize'] as IXMLIntegerTagValue;
end;

function TXMLDisplayNode.Get_FontName: IXMLStringTagValue;
begin
  Result := ChildNodes['FontName'] as IXMLStringTagValue;
end;

{ TXMLTextExport }

procedure TXMLTextExport.AfterConstruction;
begin
  RegisterChildNode('ProcessName', TXMLColumnTagValue);
  RegisterChildNode('ThreadId', TXMLColumnTagValue);
  RegisterChildNode('Time', TXMLColumnTagValue);
  RegisterChildNode('Col1', TXMLColumnTagValue);
  RegisterChildNode('Col2', TXMLColumnTagValue);
  RegisterChildNode('GenerateColumnHeader', TXMLBooleanTagValue);
  RegisterChildNode('TextQualifier', TXMLStringTagValue);
  RegisterChildNode('Separator', TXMLStringTagValue);
  RegisterChildNode('TreeIndentation', TXMLIntegerTagValue);
  inherited;
end;

function TXMLTextExport.Get_ProcessName: IXMLColumnTagValue;
begin
  Result := ChildNodes['ProcessName'] as IXMLColumnTagValue;
end;

function TXMLTextExport.Get_ThreadId: IXMLColumnTagValue;
begin
  Result := ChildNodes['ThreadId'] as IXMLColumnTagValue;
end;

function TXMLTextExport.Get_Time: IXMLColumnTagValue;
begin
  Result := ChildNodes['Time'] as IXMLColumnTagValue;
end;

function TXMLTextExport.Get_Col1: IXMLColumnTagValue;
begin
  Result := ChildNodes['Col1'] as IXMLColumnTagValue;
end;

function TXMLTextExport.Get_Col2: IXMLColumnTagValue;
begin
  Result := ChildNodes['Col2'] as IXMLColumnTagValue;
end;

function TXMLTextExport.Get_GenerateColumnHeader: IXMLBooleanTagValue;
begin
  Result := ChildNodes['GenerateColumnHeader'] as IXMLBooleanTagValue;
end;

function TXMLTextExport.Get_TextQualifier: IXMLStringTagValue;
begin
  Result := ChildNodes['TextQualifier'] as IXMLStringTagValue;
end;

function TXMLTextExport.Get_Separator: IXMLStringTagValue;
begin
  Result := ChildNodes['Separator'] as IXMLStringTagValue;
end;

function TXMLTextExport.Get_TreeIndentation: IXMLIntegerTagValue;
begin
  Result := ChildNodes['TreeIndentation'] as IXMLIntegerTagValue;
end;

{ TXMLColumnTagValue }

function TXMLColumnTagValue.Get_Value: Boolean;
begin
  Result := AttributeNodes['Value'].NodeValue;
end;

procedure TXMLColumnTagValue.Set_Value(Value: Boolean);
begin
  SetAttribute('Value', Value);
end;

function TXMLColumnTagValue.Get_Position: Integer;
begin
  Result := AttributeNodes['Position'].NodeValue;
end;

procedure TXMLColumnTagValue.Set_Position(Value: Integer);
begin
  SetAttribute('Position', Value);
end;

{ TXMLOds }

procedure TXMLOds.AfterConstruction;
begin
  RegisterChildNode('VisibleMenu', TXMLBooleanTagValue);
  RegisterChildNode('AutoClear', TXMLBooleanTagValue);
  RegisterChildNode('MaxNode', TXMLIntegerTagValue);
  RegisterChildNode('MinNode', TXMLIntegerTagValue);
  RegisterChildNode('Enabled', TXMLBooleanTagValue);
  RegisterChildNode('Title', TXMLStringTagValue);
  RegisterChildNode('Info', TXMLDisplayNode);
  RegisterChildNode('Trace', TXMLDisplayNode);
  inherited;
end;

function TXMLOds.Get_VisibleMenu: IXMLBooleanTagValue;
begin
  Result := ChildNodes['VisibleMenu'] as IXMLBooleanTagValue;
end;

function TXMLOds.Get_AutoClear: IXMLBooleanTagValue;
begin
  Result := ChildNodes['AutoClear'] as IXMLBooleanTagValue;
end;

function TXMLOds.Get_MaxNode: IXMLIntegerTagValue;
begin
  Result := ChildNodes['MaxNode'] as IXMLIntegerTagValue;
end;

function TXMLOds.Get_MinNode: IXMLIntegerTagValue;
begin
  Result := ChildNodes['MinNode'] as IXMLIntegerTagValue;
end;

function TXMLOds.Get_Enabled: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Enabled'] as IXMLBooleanTagValue;
end;

function TXMLOds.Get_Title: IXMLStringTagValue;
begin
  Result := ChildNodes['Title'] as IXMLStringTagValue;
end;

function TXMLOds.Get_Info: IXMLDisplayNode;
begin
  Result := ChildNodes['Info'] as IXMLDisplayNode;
end;

function TXMLOds.Get_Trace: IXMLDisplayNode;
begin
  Result := ChildNodes['Trace'] as IXMLDisplayNode;
end;

{ TXMLEventLog }

procedure TXMLEventLog.AfterConstruction;
begin
  RegisterChildNode('VisibleMenu', TXMLBooleanTagValue);
  RegisterChildNode('Trace', TXMLDisplayNode);
  RegisterChildNode('Info', TXMLDisplayNode);
  inherited;
end;

function TXMLEventLog.Get_VisibleMenu: IXMLBooleanTagValue;
begin
  Result := ChildNodes['VisibleMenu'] as IXMLBooleanTagValue;
end;

function TXMLEventLog.Get_Trace: IXMLDisplayNode;
begin
  Result := ChildNodes['Trace'] as IXMLDisplayNode;
end;

function TXMLEventLog.Get_Info: IXMLDisplayNode;
begin
  Result := ChildNodes['Info'] as IXMLDisplayNode;
end;

{ TXMLFramework }

procedure TXMLFramework.AfterConstruction;
begin
  RegisterChildNode('VisibleMenu', TXMLBooleanTagValue);
  RegisterChildNode('AutoClear', TXMLBooleanTagValue);
  RegisterChildNode('MaxNode', TXMLIntegerTagValue);
  RegisterChildNode('MinNode', TXMLIntegerTagValue);
  RegisterChildNode('Enabled', TXMLBooleanTagValue);
  RegisterChildNode('ShowMembers', TXMLBooleanTagValue);
  RegisterChildNode('MainTraceTitle', TXMLStringTagValue);
  RegisterChildNode('Trace', TXMLDisplayNode);
  RegisterChildNode('Info', TXMLDisplayNode);
  RegisterChildNode('Orphans', TXMLOrphans);
  inherited;
end;

function TXMLFramework.Get_VisibleMenu: IXMLBooleanTagValue;
begin
  Result := ChildNodes['VisibleMenu'] as IXMLBooleanTagValue;
end;

function TXMLFramework.Get_AutoClear: IXMLBooleanTagValue;
begin
  Result := ChildNodes['AutoClear'] as IXMLBooleanTagValue;
end;

function TXMLFramework.Get_MaxNode: IXMLIntegerTagValue;
begin
  Result := ChildNodes['MaxNode'] as IXMLIntegerTagValue;
end;

function TXMLFramework.Get_MinNode: IXMLIntegerTagValue;
begin
  Result := ChildNodes['MinNode'] as IXMLIntegerTagValue;
end;

function TXMLFramework.Get_Enabled: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Enabled'] as IXMLBooleanTagValue;
end;

function TXMLFramework.Get_ShowMembers: IXMLBooleanTagValue;
begin
  Result := ChildNodes['ShowMembers'] as IXMLBooleanTagValue;
end;

function TXMLFramework.Get_MainTraceTitle: IXMLStringTagValue;
begin
  Result := ChildNodes['MainTraceTitle'] as IXMLStringTagValue;
end;

function TXMLFramework.Get_Trace: IXMLDisplayNode;
begin
  Result := ChildNodes['Trace'] as IXMLDisplayNode;
end;

function TXMLFramework.Get_Info: IXMLDisplayNode;
begin
  Result := ChildNodes['Info'] as IXMLDisplayNode;
end;

function TXMLFramework.Get_Orphans: IXMLOrphans;
begin
  Result := ChildNodes['Orphans'] as IXMLOrphans;
end;

{ TXMLOrphans }

procedure TXMLOrphans.AfterConstruction;
begin
  RegisterChildNode('DeletedNode', TXMLStringTagValue);
  RegisterChildNode('DefaultLeftText', TXMLStringTagValue);
  RegisterChildNode('DefaultRightText', TXMLStringTagValue);
  RegisterChildNode('LostAndFoundLeftText', TXMLStringTagValue);
  RegisterChildNode('LostAndFoundRightText', TXMLStringTagValue);
  inherited;
end;

function TXMLOrphans.Get_DeletedNode: IXMLStringTagValue;
begin
  Result := ChildNodes['DeletedNode'] as IXMLStringTagValue;
end;

function TXMLOrphans.Get_DefaultLeftText: IXMLStringTagValue;
begin
  Result := ChildNodes['DefaultLeftText'] as IXMLStringTagValue;
end;

function TXMLOrphans.Get_DefaultRightText: IXMLStringTagValue;
begin
  Result := ChildNodes['DefaultRightText'] as IXMLStringTagValue;
end;

function TXMLOrphans.Get_LostAndFoundLeftText: IXMLStringTagValue;
begin
  Result := ChildNodes['LostAndFoundLeftText'] as IXMLStringTagValue;
end;

function TXMLOrphans.Get_LostAndFoundRightText: IXMLStringTagValue;
begin
  Result := ChildNodes['LostAndFoundRightText'] as IXMLStringTagValue;
end;

{ TXMLPlugins }

procedure TXMLPlugins.AfterConstruction;
begin
  RegisterChildNode('JVMEngine', TXMLStringTagValue);
  RegisterChildNode('JavaPLuginClassPath', TXMLStringTagValue);
  RegisterChildNode('Plugin', TXMLPlugin);
  FPlugin := CreateCollection(TXMLPluginList, IXMLPlugin, 'Plugin') as IXMLPluginList;
  inherited;
end;

function TXMLPlugins.Get_JVMEngine: IXMLStringTagValue;
begin
  Result := ChildNodes['JVMEngine'] as IXMLStringTagValue;
end;

function TXMLPlugins.Get_JavaPLuginClassPath: IXMLStringTagValue;
begin
  Result := ChildNodes['JavaPLuginClassPath'] as IXMLStringTagValue;
end;

function TXMLPlugins.Get_Plugin: IXMLPluginList;
begin
  Result := FPlugin;
end;

{ TXMLPlugin }

procedure TXMLPlugin.AfterConstruction;
begin
  RegisterChildNode('Enabled', TXMLBooleanTagValue);
  inherited;
end;

function TXMLPlugin.Get_FileName: WideString;
begin
  Result := ChildNodes['FileName'].Text;
end;

procedure TXMLPlugin.Set_FileName(Value: WideString);
begin
  ChildNodes['FileName'].NodeValue := Value;
end;

function TXMLPlugin.Get_ClassName: WideString;
begin
  Result := ChildNodes['ClassName'].Text;
end;

procedure TXMLPlugin.Set_ClassName(Value: WideString);
begin
  ChildNodes['ClassName'].NodeValue := Value;
end;

function TXMLPlugin.Get_Kind: WideString;
begin
  Result := ChildNodes['Kind'].Text;
end;

procedure TXMLPlugin.Set_Kind(Value: WideString);
begin
  ChildNodes['Kind'].NodeValue := Value;
end;

function TXMLPlugin.Get_Param: WideString;
begin
  Result := ChildNodes['Param'].Text;
end;

procedure TXMLPlugin.Set_Param(Value: WideString);
begin
  ChildNodes['Param'].NodeValue := Value;
end;

function TXMLPlugin.Get_Enabled: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Enabled'] as IXMLBooleanTagValue;
end;

{ TXMLPluginList }

function TXMLPluginList.Add: IXMLPlugin;
begin
  Result := AddItem(-1) as IXMLPlugin;
end;

function TXMLPluginList.Insert(const Index: Integer): IXMLPlugin;
begin
  Result := AddItem(Index) as IXMLPlugin;
end;
function TXMLPluginList.Get_Item(Index: Integer): IXMLPlugin;
begin
  Result := List[Index] as IXMLPlugin;
end;

{ TXMLWatches }

procedure TXMLWatches.AfterConstruction;
begin
  RegisterChildNode('VisibleMenu', TXMLBooleanTagValue);
  RegisterChildNode('Enabled', TXMLBooleanTagValue);
  RegisterChildNode('MainWatchesTitle', TXMLStringTagValue);
  RegisterChildNode('Trace', TXMLDisplayNode);
  RegisterChildNode('Info', TXMLDisplayNode);
  inherited;
end;

function TXMLWatches.Get_VisibleMenu: IXMLBooleanTagValue;
begin
  Result := ChildNodes['VisibleMenu'] as IXMLBooleanTagValue;
end;

function TXMLWatches.Get_Enabled: IXMLBooleanTagValue;
begin
  Result := ChildNodes['Enabled'] as IXMLBooleanTagValue;
end;

function TXMLWatches.Get_MainWatchesTitle: IXMLStringTagValue;
begin
  Result := ChildNodes['MainWatchesTitle'] as IXMLStringTagValue;
end;

function TXMLWatches.Get_Trace: IXMLDisplayNode;
begin
  Result := ChildNodes['Trace'] as IXMLDisplayNode;
end;

function TXMLWatches.Get_Info: IXMLDisplayNode;
begin
  Result := ChildNodes['Info'] as IXMLDisplayNode;
end;

{ TXMLStringTagValue5 }

function TXMLStringTagValue5.Get_Value: WideString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLStringTagValue5.Set_Value(Value: WideString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLStringTagValue4 }

function TXMLStringTagValue4.Get_Value: WideString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLStringTagValue4.Set_Value(Value: WideString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLStringTagValue3 }

function TXMLStringTagValue3.Get_Value: WideString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLStringTagValue3.Set_Value(Value: WideString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLStringTagValue2 }

function TXMLStringTagValue2.Get_Value: WideString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLStringTagValue2.Set_Value(Value: WideString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLIntegerTagValue3 }

function TXMLIntegerTagValue3.Get_Value: Integer;
begin
  Result := AttributeNodes['Value'].NodeValue;
end;

procedure TXMLIntegerTagValue3.Set_Value(Value: Integer);
begin
  SetAttribute('Value', Value);
end;

{ TXMLIntegerTagValue2 }

function TXMLIntegerTagValue2.Get_Value: Integer;
begin
  Result := AttributeNodes['Value'].NodeValue;
end;

procedure TXMLIntegerTagValue2.Set_Value(Value: Integer);
begin
  SetAttribute('Value', Value);
end;

end.