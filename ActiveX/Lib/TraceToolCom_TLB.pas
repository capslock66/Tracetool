unit TraceToolCom_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 16059 $
// File generated on 30/03/2009 16:20:43 from Type Library described below.

// ************************************************************************  //
// Type Lib: \\.host\Shared Folders\tracetool\ActiveX\Lib\TraceToolCom (1)
// LIBID: {9E046325-C625-4726-B6F4-16D2E3D5CFBD}
// LCID: 0
// Helpfile:
// HelpString: TraceToolCom Library
// DepndLst:
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  TraceToolComMajorVersion = 11;
  TraceToolComMinorVersion = 0;

  LIBID_TraceToolCom: TGUID = '{9E046325-C625-4726-B6F4-16D2E3D5CFBD}';

  IID_IXTrace: TGUID = '{3DF44A95-5044-46B1-A947-9CB5135943DD}';
  IID_IXTraceNodeBase: TGUID = '{B5604A34-11A5-4CC6-AED3-CC9B542AF198}';
  IID_IXTraceNodeEx: TGUID = '{2267AF79-9C4B-4689-9E5F-1AE2EC0946D7}';
  IID_IXMemberNode: TGUID = '{A1C8577E-15E7-4190-ACD9-256BE360F388}';
  IID_IXWinWatch: TGUID = '{AEF54A58-09A3-49FE-8FD8-31290649BB35}';
  IID_IXTraceOptions: TGUID = '{8F7167DD-8300-4D43-B434-C536C93D5CB8}';
  IID_IXTraceToSend: TGUID = '{2564ABB2-A952-42E9-A412-67F45384D3AB}';
  CLASS_XTrace: TGUID = '{14C8EA3E-1930-48E1-9BCC-63E7737CEF29}';
  IID_IXTraceNode: TGUID = '{67D4B6E1-C1E0-4715-BB23-8A289E77D907}';
  CLASS_XTraceNodeEx: TGUID = '{03DF9B86-BF3A-415C-A6ED-52531CD0D533}';
  CLASS_XMemberNode: TGUID = '{DC02B3E8-FA4A-4A7D-A967-14CA31AF8968}';
  CLASS_XWinWatch: TGUID = '{D1CA152D-E3AC-49D5-84B4-409FA0CCF750}';
  CLASS_XTraceOptions: TGUID = '{58F75201-E834-4B63-84B9-24CCFF36F75E}';
  IID_IXWinTrace: TGUID = '{4365CFCE-6247-4F05-ADBB-2F42FF8B6348}';
  CLASS_XWinTrace: TGUID = '{1DF8CC03-48DB-4EA2-B0F6-F3116F33C38F}';
  CLASS_XTraceNode: TGUID = '{0D7E252F-7FD3-4173-B53B-0DFAE57CE9D1}';
  CLASS_XTraceToSend: TGUID = '{418EA4E6-F6F8-438F-AB27-B19067842B2C}';
  IID_IXTraceTable: TGUID = '{1EF2CA18-A338-4F31-A420-6F03C4FEFE63}';
  CLASS_XTraceTable: TGUID = '{BAFF53E4-4E34-4059-9599-132713F81521}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum SendMode
type
  SendMode = TOleEnum;
const
  WinMsg = $00000000;
  Socket = $00000001;
  None = $00000002;

// Constants for enum LogMode
type
  LogMode = TOleEnum;
const
  ViewerLogDisabled = $00000000;
  ViewerLogEnabled = $00000001;
  ViewerLogDaily = $00000002;
  LocalLogDisabled = $00000003;
  LocalLogEnabled = $00000004;
  LocalLogDaily = $00000005;

// Constants for enum ColorKind
type
  ColorKind = TOleEnum;
const
  BGR = $00000000;
  RGB = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IXTrace = interface;
  IXTraceDisp = dispinterface;
  IXTraceNodeBase = interface;
  IXTraceNodeBaseDisp = dispinterface;
  IXTraceNodeEx = interface;
  IXTraceNodeExDisp = dispinterface;
  IXMemberNode = interface;
  IXMemberNodeDisp = dispinterface;
  IXWinWatch = interface;
  IXWinWatchDisp = dispinterface;
  IXTraceOptions = interface;
  IXTraceOptionsDisp = dispinterface;
  IXTraceToSend = interface;
  IXTraceToSendDisp = dispinterface;
  IXTraceNode = interface;
  IXTraceNodeDisp = dispinterface;
  IXWinTrace = interface;
  IXWinTraceDisp = dispinterface;
  IXTraceTable = interface;
  IXTraceTableDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  XTrace = IXTrace;
  XTraceNodeEx = IXTraceNodeEx;
  XMemberNode = IXMemberNode;
  XWinWatch = IXWinWatch;
  XTraceOptions = IXTraceOptions;
  XWinTrace = IXWinTrace;
  XTraceNode = IXTraceNode;
  XTraceToSend = IXTraceToSend;
  XTraceTable = IXTraceTable;


// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
  PPUserType1 = ^IXTraceTable;
  PPUserType2 = ^IXTraceTable;
  PPUserType3 = ^IXTraceTable;
  PPUserType4 = ^IXTraceTable;
  PPUserType5 = ^IXTraceTable;
  PPUserType6 = ^IXTraceTable;
  PPUserType7 = ^IXTraceTable;
  PPUserType8 = ^IXTraceTable; {*}


// *********************************************************************//
// Interface: IXTrace
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DF44A95-5044-46B1-A947-9CB5135943DD}
// *********************************************************************//
  IXTrace = interface(IDispatch)
    ['{3DF44A95-5044-46B1-A947-9CB5135943DD}']
    function Get_Warning: IXTraceToSend; safecall;
    function Get_Error: IXTraceToSend; safecall;
    function Get_Debug: IXTraceToSend; safecall;
    function Get_Options: IXTraceOptions; safecall;
    function Get_WinTrace: IXWinTrace; safecall;
    function Get_Watches: IXWinWatch; safecall;
    procedure ClearAll; safecall;
    procedure Show(IsVisible: WordBool); safecall;
    procedure Flush(FlushTimeOut: SYSINT); safecall;
    function CreateWinTrace(const WinTraceId: WideString; const WinTraceText: WideString): IXWinTrace; safecall;
    function CreateWinWatch(const WinWatchId: WideString; const WinWatchText: WideString): IXWinWatch; safecall;
    function CreateTraceTable: IXTraceTable; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    procedure CloseViewer; safecall;
    property Warning: IXTraceToSend read Get_Warning;
    property Error: IXTraceToSend read Get_Error;
    property Debug: IXTraceToSend read Get_Debug;
    property Options: IXTraceOptions read Get_Options;
    property WinTrace: IXWinTrace read Get_WinTrace;
    property Watches: IXWinWatch read Get_Watches;
  end;

// *********************************************************************//
// DispIntf:  IXTraceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DF44A95-5044-46B1-A947-9CB5135943DD}
// *********************************************************************//
  IXTraceDisp = dispinterface
    ['{3DF44A95-5044-46B1-A947-9CB5135943DD}']
    property Warning: IXTraceToSend readonly dispid 102;
    property Error: IXTraceToSend readonly dispid 103;
    property Debug: IXTraceToSend readonly dispid 104;
    property Options: IXTraceOptions readonly dispid 105;
    property WinTrace: IXWinTrace readonly dispid 106;
    property Watches: IXWinWatch readonly dispid 107;
    procedure ClearAll; dispid 108;
    procedure Show(IsVisible: WordBool); dispid 109;
    procedure Flush(FlushTimeOut: SYSINT); dispid 110;
    function CreateWinTrace(const WinTraceId: WideString; const WinTraceText: WideString): IXWinTrace; dispid 201;
    function CreateWinWatch(const WinWatchId: WideString; const WinWatchText: WideString): IXWinWatch; dispid 202;
    function CreateTraceTable: IXTraceTable; dispid 203;
    procedure Start; dispid 204;
    procedure Stop; dispid 205;
    procedure CloseViewer; dispid 206;
  end;

// *********************************************************************//
// Interface: IXTraceNodeBase
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B5604A34-11A5-4CC6-AED3-CC9B542AF198}
// *********************************************************************//
  IXTraceNodeBase = interface(IDispatch)
    ['{B5604A34-11A5-4CC6-AED3-CC9B542AF198}']
    function Get_Id: WideString; safecall;
    procedure Set_Id(const Value: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_IconIndex: SYSINT; safecall;
    procedure Set_IconIndex(Value: SYSINT); safecall;
    function Get_Tag: SYSINT; safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    function Get_WinTraceId: WideString; safecall;
    procedure Set_WinTraceId(const Value: WideString); safecall;
    function CreateNodeEx: IXTraceNodeEx; safecall;
    property Id: WideString read Get_Id write Set_Id;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property IconIndex: SYSINT read Get_IconIndex write Set_IconIndex;
    property Tag: SYSINT read Get_Tag write Set_Tag;
    property WinTraceId: WideString read Get_WinTraceId write Set_WinTraceId;
  end;

// *********************************************************************//
// DispIntf:  IXTraceNodeBaseDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B5604A34-11A5-4CC6-AED3-CC9B542AF198}
// *********************************************************************//
  IXTraceNodeBaseDisp = dispinterface
    ['{B5604A34-11A5-4CC6-AED3-CC9B542AF198}']
    property Id: WideString dispid 200;
    property Enabled: WordBool dispid 201;
    property IconIndex: SYSINT dispid 202;
    property Tag: SYSINT dispid 203;
    property WinTraceId: WideString dispid 204;
    function CreateNodeEx: IXTraceNodeEx; dispid 205;
  end;

// *********************************************************************//
// Interface: IXTraceNodeEx
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {2267AF79-9C4B-4689-9E5F-1AE2EC0946D7}
// *********************************************************************//
  IXTraceNodeEx = interface(IXTraceNodeBase)
    ['{2267AF79-9C4B-4689-9E5F-1AE2EC0946D7}']
    function Get_LeftMsg: WideString; safecall;
    procedure Set_LeftMsg(const Value: WideString); safecall;
    function Get_RightMsg: WideString; safecall;
    procedure Set_RightMsg(const Value: WideString); safecall;
    function Get_ParentNodeId: WideString; safecall;
    procedure Set_ParentNodeId(const Value: WideString); safecall;
    function Get_Members: IXMemberNode; safecall;
    procedure AddObject(Obj: OleVariant); safecall;
    procedure AddValue(Obj: OleVariant; const ObjTitle: WideString; MaxLevel: Integer); safecall;
    procedure AddDump(const ShortTitle: WideString; Adr: PChar; Count: SYSINT); safecall;
    procedure AddFontDetail(ColId: SYSINT; Bold: WordBool; Italic: WordBool; Color: SYSINT;
                            Size: SYSINT; const FontName: WideString); safecall;
    procedure AddTable(var Table: IXTraceTable); safecall;
    procedure AddXml(const Xml: WideString); safecall;
    procedure AddBackgroundColor(ColId: SYSINT; Color: SYSINT); safecall;
    function Send: IXTraceNode; safecall;
    function Resend: IXTraceNode; safecall;
    property LeftMsg: WideString read Get_LeftMsg write Set_LeftMsg;
    property RightMsg: WideString read Get_RightMsg write Set_RightMsg;
    property ParentNodeId: WideString read Get_ParentNodeId write Set_ParentNodeId;
    property Members: IXMemberNode read Get_Members;
  end;

// *********************************************************************//
// DispIntf:  IXTraceNodeExDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {2267AF79-9C4B-4689-9E5F-1AE2EC0946D7}
// *********************************************************************//
  IXTraceNodeExDisp = dispinterface
    ['{2267AF79-9C4B-4689-9E5F-1AE2EC0946D7}']
    property LeftMsg: WideString dispid 250;
    property RightMsg: WideString dispid 251;
    property ParentNodeId: WideString dispid 252;
    property Members: IXMemberNode readonly dispid 253;
    procedure AddObject(Obj: OleVariant); dispid 254;
    procedure AddValue(Obj: OleVariant; const ObjTitle: WideString; MaxLevel: Integer); dispid 255;
    procedure AddDump(const ShortTitle: WideString; Adr: {??PChar}OleVariant; Count: SYSINT); dispid 256;
    procedure AddFontDetail(ColId: SYSINT; Bold: WordBool; Italic: WordBool; Color: SYSINT;
                            Size: SYSINT; const FontName: WideString); dispid 257;
    procedure AddTable(var Table: IXTraceTable); dispid 258;
    procedure AddXml(const Xml: WideString); dispid 259;
    procedure AddBackgroundColor(ColId: SYSINT; Color: SYSINT); dispid 260;
    function Send: IXTraceNode; dispid 261;
    function Resend: IXTraceNode; dispid 262;
    property Id: WideString dispid 200;
    property Enabled: WordBool dispid 201;
    property IconIndex: SYSINT dispid 202;
    property Tag: SYSINT dispid 203;
    property WinTraceId: WideString dispid 204;
    function CreateNodeEx: IXTraceNodeEx; dispid 205;
  end;

// *********************************************************************//
// Interface: IXMemberNode
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A1C8577E-15E7-4190-ACD9-256BE360F388}
// *********************************************************************//
  IXMemberNode = interface(IDispatch)
    ['{A1C8577E-15E7-4190-ACD9-256BE360F388}']
    function Get_Col1: WideString; safecall;
    procedure Set_Col1(const Value: WideString); safecall;
    function Get_Col2: WideString; safecall;
    procedure Set_Col2(const Value: WideString); safecall;
    function Get_Col3: WideString; safecall;
    procedure Set_Col3(const Value: WideString); safecall;
    function Get_Tag: SYSINT; safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    function Add(const Col1: WideString; const Col2: WideString; const Col3: WideString): IXMemberNode; safecall;
    function SetFontDetail(ColId: SYSINT; Bold: WordBool; Italic: WordBool; Color: SYSINT;
                           Size: SYSINT; const FontName: WideString): IXMemberNode; safecall;
    property Col1: WideString read Get_Col1 write Set_Col1;
    property Col2: WideString read Get_Col2 write Set_Col2;
    property Col3: WideString read Get_Col3 write Set_Col3;
    property Tag: SYSINT read Get_Tag write Set_Tag;
  end;

// *********************************************************************//
// DispIntf:  IXMemberNodeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A1C8577E-15E7-4190-ACD9-256BE360F388}
// *********************************************************************//
  IXMemberNodeDisp = dispinterface
    ['{A1C8577E-15E7-4190-ACD9-256BE360F388}']
    property Col1: WideString dispid 201;
    property Col2: WideString dispid 202;
    property Col3: WideString dispid 203;
    property Tag: SYSINT dispid 101;
    function Add(const Col1: WideString; const Col2: WideString; const Col3: WideString): IXMemberNode; dispid 204;
    function SetFontDetail(ColId: SYSINT; Bold: WordBool; Italic: WordBool; Color: SYSINT;
                           Size: SYSINT; const FontName: WideString): IXMemberNode; dispid 205;
  end;

// *********************************************************************//
// Interface: IXWinWatch
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AEF54A58-09A3-49FE-8FD8-31290649BB35}
// *********************************************************************//
  IXWinWatch = interface(IDispatch)
    ['{AEF54A58-09A3-49FE-8FD8-31290649BB35}']
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Tag: SYSINT; safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    procedure DisplayWin; safecall;
    procedure ClearAll; safecall;
    procedure Send(const WatchName: WideString; v: OleVariant); safecall;
    function Get_Id: WideString; safecall;
    procedure Set_Id(const Value: WideString); safecall;
    procedure Close; safecall;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Tag: SYSINT read Get_Tag write Set_Tag;
    property Id: WideString read Get_Id write Set_Id;
  end;

// *********************************************************************//
// DispIntf:  IXWinWatchDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AEF54A58-09A3-49FE-8FD8-31290649BB35}
// *********************************************************************//
  IXWinWatchDisp = dispinterface
    ['{AEF54A58-09A3-49FE-8FD8-31290649BB35}']
    property Enabled: WordBool dispid 201;
    property Tag: SYSINT dispid 202;
    procedure DisplayWin; dispid 203;
    procedure ClearAll; dispid 204;
    procedure Send(const WatchName: WideString; v: OleVariant); dispid 205;
    property Id: WideString dispid 101;
    procedure Close; dispid 206;
  end;

// *********************************************************************//
// Interface: IXTraceOptions
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {8F7167DD-8300-4D43-B434-C536C93D5CB8}
// *********************************************************************//
  IXTraceOptions = interface(IDispatch)
    ['{8F7167DD-8300-4D43-B434-C536C93D5CB8}']
    function Get_SocketHost: WideString; safecall;
    procedure Set_SocketHost(const Value: WideString); safecall;
    function Get_SendMode: SendMode; safecall;
    procedure Set_SendMode(Value: SendMode); safecall;
    function Get_SendDate: WordBool; safecall;
    procedure Set_SendDate(Value: WordBool); safecall;
    function Get_SendFunctions: WordBool; safecall;
    procedure Set_SendFunctions(Value: WordBool); safecall;
    function Get_SendThreadId: WordBool; safecall;
    procedure Set_SendThreadId(Value: WordBool); safecall;
    function Get_SendProcessName: WordBool; safecall;
    procedure Set_SendProcessName(Value: WordBool); safecall;
    function Get_SocketPort: SYSINT; safecall;
    procedure Set_SocketPort(Value: SYSINT); safecall;
    function Get_ColorKind: ColorKind; safecall;
    procedure Set_ColorKind(Value: ColorKind); safecall;
    function Get_SocketUdp: WordBool; safecall;
    procedure Set_SocketUdp(Value: WordBool); safecall;
    property SocketHost: WideString read Get_SocketHost write Set_SocketHost;
    property SendMode: SendMode read Get_SendMode write Set_SendMode;
    property SendDate: WordBool read Get_SendDate write Set_SendDate;
    property SendFunctions: WordBool read Get_SendFunctions write Set_SendFunctions;
    property SendThreadId: WordBool read Get_SendThreadId write Set_SendThreadId;
    property SendProcessName: WordBool read Get_SendProcessName write Set_SendProcessName;
    property SocketPort: SYSINT read Get_SocketPort write Set_SocketPort;
    property ColorKind: ColorKind read Get_ColorKind write Set_ColorKind;
    property SocketUdp: WordBool read Get_SocketUdp write Set_SocketUdp;
  end;

// *********************************************************************//
// DispIntf:  IXTraceOptionsDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {8F7167DD-8300-4D43-B434-C536C93D5CB8}
// *********************************************************************//
  IXTraceOptionsDisp = dispinterface
    ['{8F7167DD-8300-4D43-B434-C536C93D5CB8}']
    property SocketHost: WideString dispid 201;
    property SendMode: SendMode dispid 202;
    property SendDate: WordBool dispid 203;
    property SendFunctions: WordBool dispid 204;
    property SendThreadId: WordBool dispid 102;
    property SendProcessName: WordBool dispid 101;
    property SocketPort: SYSINT dispid 103;
    property ColorKind: ColorKind dispid 205;
    property SocketUdp: WordBool dispid 206;
  end;

// *********************************************************************//
// Interface: IXTraceToSend
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2564ABB2-A952-42E9-A412-67F45384D3AB}
// *********************************************************************//
  IXTraceToSend = interface(IXTraceNodeBase)
    ['{2564ABB2-A952-42E9-A412-67F45384D3AB}']
    function Send(const LeftMsg: WideString; const RightMsg: WideString): IXTraceNode; safecall;
    function SendObject(const LeftMsg: WideString; Obj: OleVariant): IXTraceNode; safecall;
    function SendValue(const LeftMsg: WideString; Obj: OleVariant; const ObjTitle: WideString): IXTraceNode; safecall;
    function SendValueIDisptach(const LeftMsg: WideString; const Obj: IDispatch;
                                const ObjTitle: WideString): IXTraceNode; safecall;
    function SendDump(const LeftMsg: WideString; const ShortTitle: WideString; Adress: PChar;
                      Count: SYSINT): IXTraceNode; safecall;
    procedure SendTable(const LeftMsg: WideString; var Table: IXTraceTable); safecall;
    procedure SendXml(const LeftMsg: WideString; const Xml: WideString); safecall;
    procedure SendBackgroundColor(const LeftMsg: WideString; Color: SYSINT; ColId: SYSINT); safecall;
    procedure EnterMethod(const LeftMsg: WideString; const RightMsg: WideString); safecall;
    procedure ExitMethod(const LeftMsg: WideString; const RightMsg: WideString); safecall;
    procedure Indent(const LeftMsg: WideString; const RightMsg: WideString); safecall;
    procedure UnIndent(const LeftMsg: WideString; const RightMsg: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IXTraceToSendDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2564ABB2-A952-42E9-A412-67F45384D3AB}
// *********************************************************************//
  IXTraceToSendDisp = dispinterface
    ['{2564ABB2-A952-42E9-A412-67F45384D3AB}']
    function Send(const LeftMsg: WideString; const RightMsg: WideString): IXTraceNode; dispid 250;
    function SendObject(const LeftMsg: WideString; Obj: OleVariant): IXTraceNode; dispid 251;
    function SendValue(const LeftMsg: WideString; Obj: OleVariant; const ObjTitle: WideString): IXTraceNode; dispid 252;
    function SendValueIDisptach(const LeftMsg: WideString; const Obj: IDispatch;
                                const ObjTitle: WideString): IXTraceNode; dispid 253;
    function SendDump(const LeftMsg: WideString; const ShortTitle: WideString;
                      Adress: {??PChar}OleVariant; Count: SYSINT): IXTraceNode; dispid 254;
    procedure SendTable(const LeftMsg: WideString; var Table: IXTraceTable); dispid 255;
    procedure SendXml(const LeftMsg: WideString; const Xml: WideString); dispid 256;
    procedure SendBackgroundColor(const LeftMsg: WideString; Color: SYSINT; ColId: SYSINT); dispid 257;
    procedure EnterMethod(const LeftMsg: WideString; const RightMsg: WideString); dispid 258;
    procedure ExitMethod(const LeftMsg: WideString; const RightMsg: WideString); dispid 259;
    procedure Indent(const LeftMsg: WideString; const RightMsg: WideString); dispid 260;
    procedure UnIndent(const LeftMsg: WideString; const RightMsg: WideString); dispid 261;
    property Id: WideString dispid 200;
    property Enabled: WordBool dispid 201;
    property IconIndex: SYSINT dispid 202;
    property Tag: SYSINT dispid 203;
    property WinTraceId: WideString dispid 204;
    function CreateNodeEx: IXTraceNodeEx; dispid 205;
  end;

// *********************************************************************//
// Interface: IXTraceNode
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {67D4B6E1-C1E0-4715-BB23-8A289E77D907}
// *********************************************************************//
  IXTraceNode = interface(IXTraceToSend)
    ['{67D4B6E1-C1E0-4715-BB23-8A289E77D907}']
    function Resend(const LeftMsg: WideString; const RightMsg: WideString): IXTraceNode; safecall;
    function ResendLeft(const LeftMsg: WideString): IXTraceNode; safecall;
    function ResendRight(const RightMsg: WideString): IXTraceNode; safecall;
    function Append(const NewLeftMsg: WideString; const NewRightMsg: WideString): IXTraceNode; safecall;
    function AppendLeft(const NewLeftMsg: WideString): IXTraceNode; safecall;
    function AppendRight(const NewRightMsg: WideString): IXTraceNode; safecall;
    function Show: IXTraceNode; safecall;
    function SetSelected: IXTraceNode; safecall;
    function Delete: IXTraceNode; safecall;
    function DeleteChildren: IXTraceNode; safecall;
    function SetFontDetail(ColId: SYSINT; Bold: WordBool; Italic: WordBool; Color: SYSINT;
                           Size: SYSINT; const FontName: WideString): IXTraceNode; safecall;
    function ResendIconIndex(Index: SYSINT): IXTraceNode; safecall;
    function SetBackgroundColor(Color: SYSINT; ColId: SYSINT): IXTraceNode; safecall;
  end;

// *********************************************************************//
// DispIntf:  IXTraceNodeDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {67D4B6E1-C1E0-4715-BB23-8A289E77D907}
// *********************************************************************//
  IXTraceNodeDisp = dispinterface
    ['{67D4B6E1-C1E0-4715-BB23-8A289E77D907}']
    function Resend(const LeftMsg: WideString; const RightMsg: WideString): IXTraceNode; dispid 300;
    function ResendLeft(const LeftMsg: WideString): IXTraceNode; dispid 301;
    function ResendRight(const RightMsg: WideString): IXTraceNode; dispid 302;
    function Append(const NewLeftMsg: WideString; const NewRightMsg: WideString): IXTraceNode; dispid 303;
    function AppendLeft(const NewLeftMsg: WideString): IXTraceNode; dispid 304;
    function AppendRight(const NewRightMsg: WideString): IXTraceNode; dispid 305;
    function Show: IXTraceNode; dispid 306;
    function SetSelected: IXTraceNode; dispid 307;
    function Delete: IXTraceNode; dispid 308;
    function DeleteChildren: IXTraceNode; dispid 309;
    function SetFontDetail(ColId: SYSINT; Bold: WordBool; Italic: WordBool; Color: SYSINT;
                           Size: SYSINT; const FontName: WideString): IXTraceNode; dispid 310;
    function ResendIconIndex(Index: SYSINT): IXTraceNode; dispid 311;
    function SetBackgroundColor(Color: SYSINT; ColId: SYSINT): IXTraceNode; dispid 312;
    function Send(const LeftMsg: WideString; const RightMsg: WideString): IXTraceNode; dispid 250;
    function SendObject(const LeftMsg: WideString; Obj: OleVariant): IXTraceNode; dispid 251;
    function SendValue(const LeftMsg: WideString; Obj: OleVariant; const ObjTitle: WideString): IXTraceNode; dispid 252;
    function SendValueIDisptach(const LeftMsg: WideString; const Obj: IDispatch;
                                const ObjTitle: WideString): IXTraceNode; dispid 253;
    function SendDump(const LeftMsg: WideString; const ShortTitle: WideString;
                      Adress: {??PChar}OleVariant; Count: SYSINT): IXTraceNode; dispid 254;
    procedure SendTable(const LeftMsg: WideString; var Table: IXTraceTable); dispid 255;
    procedure SendXml(const LeftMsg: WideString; const Xml: WideString); dispid 256;
    procedure SendBackgroundColor(const LeftMsg: WideString; Color: SYSINT; ColId: SYSINT); dispid 257;
    procedure EnterMethod(const LeftMsg: WideString; const RightMsg: WideString); dispid 258;
    procedure ExitMethod(const LeftMsg: WideString; const RightMsg: WideString); dispid 259;
    procedure Indent(const LeftMsg: WideString; const RightMsg: WideString); dispid 260;
    procedure UnIndent(const LeftMsg: WideString; const RightMsg: WideString); dispid 261;
    property Id: WideString dispid 200;
    property Enabled: WordBool dispid 201;
    property IconIndex: SYSINT dispid 202;
    property Tag: SYSINT dispid 203;
    property WinTraceId: WideString dispid 204;
    function CreateNodeEx: IXTraceNodeEx; dispid 205;
  end;

// *********************************************************************//
// Interface: IXWinTrace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {4365CFCE-6247-4F05-ADBB-2F42FF8B6348}
// *********************************************************************//
  IXWinTrace = interface(IXTraceToSend)
    ['{4365CFCE-6247-4F05-ADBB-2F42FF8B6348}']
    function Get_Warning: IXTraceToSend; safecall;
    function Get_Error: IXTraceToSend; safecall;
    function Get_Debug: IXTraceToSend; safecall;
    function Get_WinTraceText: WideString; safecall;
    procedure Set_WinTraceText(const Value: WideString); safecall;
    procedure SaveToTextfile(const Filename: WideString); safecall;
    procedure SaveToXml(const Filename: WideString; const StyleSheet: WideString); safecall;
    procedure LoadXml(const Filename: WideString); safecall;
    procedure DisplayWin; safecall;
    procedure setMultiColumn(MainColIndex: SYSINT); safecall;
    procedure setColumnsTitle(const Titles: WideString); safecall;
    procedure setColumnsWidth(const Widths: WideString); safecall;
    procedure ClearAll; safecall;
    procedure setLogFile(const Filename: WideString; Mode: LogMode; MaxLines: Integer); safecall;
    procedure Close; safecall;
    property Warning: IXTraceToSend read Get_Warning;
    property Error: IXTraceToSend read Get_Error;
    property Debug: IXTraceToSend read Get_Debug;
    property WinTraceText: WideString read Get_WinTraceText write Set_WinTraceText;
  end;

// *********************************************************************//
// DispIntf:  IXWinTraceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {4365CFCE-6247-4F05-ADBB-2F42FF8B6348}
// *********************************************************************//
  IXWinTraceDisp = dispinterface
    ['{4365CFCE-6247-4F05-ADBB-2F42FF8B6348}']
    property Warning: IXTraceToSend readonly dispid 300;
    property Error: IXTraceToSend readonly dispid 301;
    property Debug: IXTraceToSend readonly dispid 302;
    property WinTraceText: WideString dispid 303;
    procedure SaveToTextfile(const Filename: WideString); dispid 304;
    procedure SaveToXml(const Filename: WideString; const StyleSheet: WideString); dispid 305;
    procedure LoadXml(const Filename: WideString); dispid 306;
    procedure DisplayWin; dispid 307;
    procedure setMultiColumn(MainColIndex: SYSINT); dispid 308;
    procedure setColumnsTitle(const Titles: WideString); dispid 309;
    procedure setColumnsWidth(const Widths: WideString); dispid 310;
    procedure ClearAll; dispid 311;
    procedure setLogFile(const Filename: WideString; Mode: LogMode; MaxLines: Integer); dispid 312;
    procedure Close; dispid 401;
    function Send(const LeftMsg: WideString; const RightMsg: WideString): IXTraceNode; dispid 250;
    function SendObject(const LeftMsg: WideString; Obj: OleVariant): IXTraceNode; dispid 251;
    function SendValue(const LeftMsg: WideString; Obj: OleVariant; const ObjTitle: WideString): IXTraceNode; dispid 252;
    function SendValueIDisptach(const LeftMsg: WideString; const Obj: IDispatch;
                                const ObjTitle: WideString): IXTraceNode; dispid 253;
    function SendDump(const LeftMsg: WideString; const ShortTitle: WideString;
                      Adress: {??PChar}OleVariant; Count: SYSINT): IXTraceNode; dispid 254;
    procedure SendTable(const LeftMsg: WideString; var Table: IXTraceTable); dispid 255;
    procedure SendXml(const LeftMsg: WideString; const Xml: WideString); dispid 256;
    procedure SendBackgroundColor(const LeftMsg: WideString; Color: SYSINT; ColId: SYSINT); dispid 257;
    procedure EnterMethod(const LeftMsg: WideString; const RightMsg: WideString); dispid 258;
    procedure ExitMethod(const LeftMsg: WideString; const RightMsg: WideString); dispid 259;
    procedure Indent(const LeftMsg: WideString; const RightMsg: WideString); dispid 260;
    procedure UnIndent(const LeftMsg: WideString; const RightMsg: WideString); dispid 261;
    property Id: WideString dispid 200;
    property Enabled: WordBool dispid 201;
    property IconIndex: SYSINT dispid 202;
    property Tag: SYSINT dispid 203;
    property WinTraceId: WideString dispid 204;
    function CreateNodeEx: IXTraceNodeEx; dispid 205;
  end;

// *********************************************************************//
// Interface: IXTraceTable
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1EF2CA18-A338-4F31-A420-6F03C4FEFE63}
// *********************************************************************//
  IXTraceTable = interface(IDispatch)
    ['{1EF2CA18-A338-4F31-A420-6F03C4FEFE63}']
    procedure AddColumnTitle(const ColTitle: WideString); safecall;
    procedure AddRow; safecall;
    procedure AddRowData(const Cell: WideString); safecall;
    function GetTitle: WideString; safecall;
    function GetRowData(Row: Integer): WideString; safecall;
    function Get_RowCount: Integer; safecall;
    property RowCount: Integer read Get_RowCount;
  end;

// *********************************************************************//
// DispIntf:  IXTraceTableDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1EF2CA18-A338-4F31-A420-6F03C4FEFE63}
// *********************************************************************//
  IXTraceTableDisp = dispinterface
    ['{1EF2CA18-A338-4F31-A420-6F03C4FEFE63}']
    procedure AddColumnTitle(const ColTitle: WideString); dispid 201;
    procedure AddRow; dispid 202;
    procedure AddRowData(const Cell: WideString); dispid 203;
    function GetTitle: WideString; dispid 204;
    function GetRowData(Row: Integer): WideString; dispid 205;
    property RowCount: Integer readonly dispid 206;
  end;

// *********************************************************************//
// The Class CoXTrace provides a Create and CreateRemote method to
// create instances of the default interface IXTrace exposed by
// the CoClass XTrace. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXTrace = class
    class function Create: IXTrace;
    class function CreateRemote(const MachineName: string): IXTrace;
  end;

// *********************************************************************//
// The Class CoXTraceNodeEx provides a Create and CreateRemote method to
// create instances of the default interface IXTraceNodeEx exposed by
// the CoClass XTraceNodeEx. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXTraceNodeEx = class
    class function Create: IXTraceNodeEx;
    class function CreateRemote(const MachineName: string): IXTraceNodeEx;
  end;

// *********************************************************************//
// The Class CoXMemberNode provides a Create and CreateRemote method to
// create instances of the default interface IXMemberNode exposed by
// the CoClass XMemberNode. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXMemberNode = class
    class function Create: IXMemberNode;
    class function CreateRemote(const MachineName: string): IXMemberNode;
  end;

// *********************************************************************//
// The Class CoXWinWatch provides a Create and CreateRemote method to
// create instances of the default interface IXWinWatch exposed by
// the CoClass XWinWatch. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXWinWatch = class
    class function Create: IXWinWatch;
    class function CreateRemote(const MachineName: string): IXWinWatch;
  end;

// *********************************************************************//
// The Class CoXTraceOptions provides a Create and CreateRemote method to
// create instances of the default interface IXTraceOptions exposed by
// the CoClass XTraceOptions. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXTraceOptions = class
    class function Create: IXTraceOptions;
    class function CreateRemote(const MachineName: string): IXTraceOptions;
  end;

// *********************************************************************//
// The Class CoXWinTrace provides a Create and CreateRemote method to
// create instances of the default interface IXWinTrace exposed by
// the CoClass XWinTrace. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXWinTrace = class
    class function Create: IXWinTrace;
    class function CreateRemote(const MachineName: string): IXWinTrace;
  end;

// *********************************************************************//
// The Class CoXTraceNode provides a Create and CreateRemote method to
// create instances of the default interface IXTraceNode exposed by
// the CoClass XTraceNode. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXTraceNode = class
    class function Create: IXTraceNode;
    class function CreateRemote(const MachineName: string): IXTraceNode;
  end;

// *********************************************************************//
// The Class CoXTraceToSend provides a Create and CreateRemote method to
// create instances of the default interface IXTraceToSend exposed by
// the CoClass XTraceToSend. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXTraceToSend = class
    class function Create: IXTraceToSend;
    class function CreateRemote(const MachineName: string): IXTraceToSend;
  end;

// *********************************************************************//
// The Class CoXTraceTable provides a Create and CreateRemote method to
// create instances of the default interface IXTraceTable exposed by
// the CoClass XTraceTable. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoXTraceTable = class
    class function Create: IXTraceTable;
    class function CreateRemote(const MachineName: string): IXTraceTable;
  end;

implementation

uses ComObj;

class function CoXTrace.Create: IXTrace;
begin
  Result := CreateComObject(CLASS_XTrace) as IXTrace;
end;

class function CoXTrace.CreateRemote(const MachineName: string): IXTrace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XTrace) as IXTrace;
end;

class function CoXTraceNodeEx.Create: IXTraceNodeEx;
begin
  Result := CreateComObject(CLASS_XTraceNodeEx) as IXTraceNodeEx;
end;

class function CoXTraceNodeEx.CreateRemote(const MachineName: string): IXTraceNodeEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XTraceNodeEx) as IXTraceNodeEx;
end;

class function CoXMemberNode.Create: IXMemberNode;
begin
  Result := CreateComObject(CLASS_XMemberNode) as IXMemberNode;
end;

class function CoXMemberNode.CreateRemote(const MachineName: string): IXMemberNode;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XMemberNode) as IXMemberNode;
end;

class function CoXWinWatch.Create: IXWinWatch;
begin
  Result := CreateComObject(CLASS_XWinWatch) as IXWinWatch;
end;

class function CoXWinWatch.CreateRemote(const MachineName: string): IXWinWatch;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XWinWatch) as IXWinWatch;
end;

class function CoXTraceOptions.Create: IXTraceOptions;
begin
  Result := CreateComObject(CLASS_XTraceOptions) as IXTraceOptions;
end;

class function CoXTraceOptions.CreateRemote(const MachineName: string): IXTraceOptions;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XTraceOptions) as IXTraceOptions;
end;

class function CoXWinTrace.Create: IXWinTrace;
begin
  Result := CreateComObject(CLASS_XWinTrace) as IXWinTrace;
end;

class function CoXWinTrace.CreateRemote(const MachineName: string): IXWinTrace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XWinTrace) as IXWinTrace;
end;

class function CoXTraceNode.Create: IXTraceNode;
begin
  Result := CreateComObject(CLASS_XTraceNode) as IXTraceNode;
end;

class function CoXTraceNode.CreateRemote(const MachineName: string): IXTraceNode;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XTraceNode) as IXTraceNode;
end;

class function CoXTraceToSend.Create: IXTraceToSend;
begin
  Result := CreateComObject(CLASS_XTraceToSend) as IXTraceToSend;
end;

class function CoXTraceToSend.CreateRemote(const MachineName: string): IXTraceToSend;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XTraceToSend) as IXTraceToSend;
end;

class function CoXTraceTable.Create: IXTraceTable;
begin
  Result := CreateComObject(CLASS_XTraceTable) as IXTraceTable;
end;

class function CoXTraceTable.CreateRemote(const MachineName: string): IXTraceTable;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XTraceTable) as IXTraceTable;
end;

end.

