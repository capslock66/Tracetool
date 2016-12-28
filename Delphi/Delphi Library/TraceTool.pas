///  TraceTool Delphi API.                                                                                 <p>
///                                                                                                        <p>
///  Author : Thierry Parent                                                                               <p>
///  Version : 12.8                                                                                        <p>
///  See License.txt for license information                                                               <p>
///                                                                                                        <p>
///  Optional stack traces can be done using the provided StackTrace unit that use jedi code source,       <p>
///  See http://www.delphi-jedi.org then go to "JCL Code Library" Or http://sourceforge.net/projects/jcl/  <p>
///                                                                                                        <p>
///  Optional Socket mode can be done using the provided SocketTrace unt that use Indy component.          <p>
///  If you use another socket library, you have to create a TSocketTrace descendant class                 <p>

// history :
// 12.6 : 2015/05/13 : removed deprecated warning on HtmlEncode
// 12.7 : 2015/10/18 : EnterMethod and Indent return the node
// 12.7 : 2015/11/08 : Added AppendStack
// 12.7 : 2015/11/15 : Added SendStack overload with right and left msg.
// 12.7 : 2015/11/15 : Added IndentWithStack
// 12.8 : 2016/12/28 : AAdd support for properties arrays 

unit TraceTool;

interface           

{$Include TraceTool.Inc}      

uses Classes , windows, ActiveX , sysutils, Registry , Messages, Forms, //Dialogs,
     comobj , AxCtrls , SyncObjs , Contnrs, Graphics , TypInfo,
     SynCommons,


{$ifdef COMPILER_10_UP}    // starting 2006
   WideStrings ,
{$endif COMPILER_10_UP}

{$ifdef COMPILER_12_UP}    // starting 2009
   generics.collections ,
     HTTPApp ,
{$endif COMPILER_12_UP}
   menus ;

{$Include TraceConst.inc}    


{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

type

   // forward declaration

   ITraceNodeBase = interface ;
   ITraceNode     = interface ;
   ITraceNodeEx   = interface ;
   ITraceToSend   = interface ;
   IWinTrace      = interface ;
   IWinWatch      = interface ;
   ITraceTable    = interface ;

   TTrace         = class ;
   TTraceOptions  = class ;
   TSocketTrace   = class ;


   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   { The main class. All methods are class procedure or functions }
   TTrace = class
   private
      class procedure SendToWinTraceClient (CommandList : TStringList ; winTraceId : string; DateTime : TDateTime = 0; ThreadName : string = '') ;
      class procedure SendToWinWatchClient (CommandList : TStringList ; winWatchId : string) ;

   public
      class procedure SendToClient (CommandList : TStringList) ; overload ;   // SendToClient is now public to allow transfert commands 

   public

      /// Shortcut to WinTrace.Debug
      /// <returns>WinTrace.Debug</returns>
      class function  Debug : ITraceToSend ;

      /// Shortcut to WinTrace.Warning
      /// <returns>WinTrace.Warning</returns>
      class function  Warning : ITraceToSend ;

      /// Shortcut to WinTrace.Error
      /// <returns>WinTrace.Error</returns>
      class function  Error : ITraceToSend ;


      /// TTrace Options (socket, show functions, ...)
      /// <returns>TTrace Options</returns>
      class function  Options : TTraceOptions ;

      /// The windows where is stored the main tree (read only)
      /// <returns>the main wintrace</returns>
      class function  WinTrace : IWinTrace ;

      /// The main watches window
      /// <returns>The main watches window</returns>
      class function  Watches : IWinWatch ;

      /// Clear all traces on the main wintrace
      class procedure ClearAll ()  ;

      /// Show or hide the trace program. To close completly the viewer, call TTrace.CloseViewer
      /// <param name="IsVisible">When false, the viewer is minimized in systray</param>
      class procedure Show ( IsVisible : boolean) ;

      /// Set the global search criteria. You must call TTrace.Wintrace.FindNext to position to the next or previous matching node
      /// <param name="Text">Text to search</param>
      /// <param name="Sensitive">Search is case sensitive</param>
      /// <param name="WholeWord">match only whole word</param>
      /// <param name="Highlight">Highlight results</param>
      /// <param name="SearchInAllPages">call to FindNext will search also in other traces windows if true</param>
      class procedure Find (Text : string; Sensitive, WholeWord , Highlight, SearchInAllPages : bool) ;

      /// ShutDown the viewer. It's no the same as TTrace.Show(false) that just hide it.
      class procedure CloseViewer() ;

      /// flush traces to the viewer.
      /// <param name="FlushTimeOut">Number of millisec to wait</param>
      class function Flush (FlushTimeOut : integer = 5000) : integer;

      /// helper function : create a trace id
      /// <returns>A new unque ID</returns>
      class function  CreateTraceID: string;

      /// Create a window traces
      /// <param name="WinTraceID">Wintrace id</param>
      /// <param name="WinTraceText">WinTrace Text</param>
      /// <returns>a new wintrace</returns>
      class function  CreateWinTrace (const WinTraceID : string ; const WinTraceText : string = '') : IWinTrace ;

      /// Create a window watches
      /// <param name="WinWatchID">Winwatch id</param>
      /// <param name="WinWatchText">Winwatch Text</param>
      /// <returns>a new IWinWatch</returns>
      class function  CreateWinWatch (const WinWatchID : string ; const WinWatchText : string = '') : IWinWatch ;

      /// Create a TraceNodeEx from a ITraceToSend parent node
      /// <param name="ParentNode">Parent node (can be nil)</param>
      /// <returns>a new ItraceNodeEx</returns>
      class function  CreateNodeEx   (ParentNode : ITraceToSend) : ItraceNodeEx ;

      /// Re create a TraceNode from using his Id
      /// <param name="ParentNode">Parent node (can be nil)</param>
      /// <param name="NodeId">Node Id</param>
      /// <returns>a new ItraceNode</returns>
      class function  CreateNodeFromId   (ParentNode: ITraceToSend;NodeId : string) : ItraceNode ;

      /// create a Trace table
      /// <returns>a new ITraceTable</returns>
      class function  CreateTraceTable () : ITraceTable ;

      /// Stop tracetool client sub system. Must be called before exiting plugin
      class procedure stop() ;

      /// start or restart tracetool client sub system
      class procedure start() ;

   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   // <summary>
   // WinWatch represent a windows tree where you put watches
   // </summary>
   IWinWatch = interface
   ['{4EC8650B-E855-47B4-B032-B506A20DA664}']

      function getID : string ;
      procedure setID (const v : string) ;
      function  getEnabled : boolean ;
      procedure setEnabled (const v : boolean) ;
      function  getTag : integer ;
      procedure setTag (const v : integer) ;

      /// The "Required" Id of the window tree, can be any string, or a guid.<p>
      /// The Main window trace Id is empty
      property id : string read getId write setID ;

      /// If false, methods like send and clearAll are disabled
      property  Enabled : boolean read getEnabled write setEnabled ;

      /// User variable, provided for the convenience of developers
      property Tag : integer read getTag write setTag ;

      /// Display the window
      procedure DisplayWin () ;

      /// Clear all watch in this window
      procedure ClearAll () ;

      /// Close the watch window
      procedure Close() ;

      /// Send a string watch
      /// <param name="WatchName">watch name</param>
      /// <param name="watchValue">watch value</param>
      procedure  Send (const WatchName : string ; const watchValue: string); Overload ;

      /// Send a variant watch
      /// <param name="WatchName">watch name</param>
      /// <param name="watchValue">watch value</param>
      procedure  Send (const WatchName : string; const watchValue: variant);  Overload ;

      /// Send a TObject watch
      /// <param name="WatchName">watch name</param>
      /// <param name="watchValue">watch value</param>
      procedure  Send (const WatchName : string; const watchValue : TObject);  Overload ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   /// <summary>
   /// Specify how to encode traces
   // </summary>
   TEncodingTrace =
   (
      enSingleByte, // single byte encoding
      enUnicode     // unicode encoding (little endian)
   ) ;

   /// <summary>
   /// Specify how traces are send.
   /// </summary>
   TSendMode =
   (
      tmWinMsg,          // windows message (fast)
      tmAlternate,       // socket message (usefull for network communication)
      tmNone             // don't send message to the viewer (Local log file in wintrace must be specified)
   );

   // <summary>
   // Socket communication with the viewer. This is an Abstract base class. The default implementation use indy component.<p>
   // If your application another socket API, override SetHost,SetPort and Send methods.
   // </summary>
   TSocketTrace = class
      /// Change the socket host,port, tcp/udp
      procedure InitClientSocket(); virtual ; abstract ;
      /// send the string to the viewer.
      /// <param name="str">string to send to the viewer</param>
      /// <param name="nbchar">message char count</param>
      procedure Send (const str : string; const nbchar : integer) ; virtual ; abstract ;
   end ;

   // The followings procedures type are used to extend TraceTool. See StackTrace unit

   /// define how to fill a node with stack information. See StackTrace unit
   TSendStackProc   = procedure (NodeEx : ITraceNodeEx ; Level: Integer = 0) ;
   /// define how to fill a node with caller information. See StackTrace unit
   TSendCallerProc  = procedure (NodeEx : ITraceNodeEx ; Level: Integer = 0) ;
   /// define how to fill a node with method information. See StackTrace unit
   TSendMethodProc  = function (NodeEx : ITraceNodeEx ; const Addr: Pointer) : string ;

   /// Options for SendObject methods.<p>
   /// Some flags are not usefull in delphi
   TraceDisplayFlag =
   (
      ShowModifiers        ,      // Not implemented (dot net or java)
      ShowCustomAttributes ,      // Not implemented (dot net or Java5)
      ShowNonPublic        ,      // Not implemented (dot net or java)
      ShowInheritedMembers ,      // Not implemented (dot net or java)
      ShowClassInfo        ,      // Show class information
      ShowFields           ,      // show fields
      ShowEvents           ,      // show events
      ShowMethods                 // show Methods
   ) ;

   /// Options for SendObject methods <p>
   /// Some flags are not usefull in delphi
   TraceDisplayFlags = set of TraceDisplayFlag ;

   //--------------------------------------------------------------------------------------------------

   /// <summary>
   /// Options for the traces.
   /// </summary>
   TTraceOptions = class
   private
      fProcessFileName : string ;
      fSendMode : TSendMode;
      fSendFunctions : boolean ;
      fSendInherited : boolean ;
      fSendEvents : boolean ;
      fSocketPort: integer;
      fSocketHost: string;
      fSocketUdp : boolean ;
      fObjectTreeDepth : integer ;
      fSendDate : boolean ;
      fSendThreadId : boolean ;
      //fEncoding : TEncodingTrace ;     // todo

      constructor Create;
      procedure _SendProcessName (const Value : boolean) ;
      function _IsProcessName() : boolean ;
      procedure setSocketHost(const Value: string);
      procedure setSocketPort(const Value: integer);
      procedure setSocketUdp(const Value: boolean);
   protected
      function GetDefault() : TraceDisplayFlags ;
   public
      /// Change SendMode to Mode.Socket to use it under ASP
      property SendMode : TSendMode read fSendMode write fSendMode ;
      /// Specify if the process name should be send
      property SendProcessName : boolean read _IsProcessName  write _SendProcessName ;
      /// Specify if the date should be send
      property SendDate : boolean read fSendDate              write fSendDate ;
      /// Indicate if the reflection should display functions
      property SendFunctions   : boolean read fSendFunctions  write fSendFunctions ;
      /// Indicate if the reflections should display inherited members
      property SendInherited   : boolean read fSendInherited  write fSendInherited ;
      /// Indicate if the reflections should display the events
      property SendEvents      : boolean read fSendEvents     write fSendEvents ;
      /// Indicate if the thread id should be send (not usefull for OLE)
      property SendThreadId : boolean read fSendThreadId write fSendThreadId ;
      { The Socket Host address }
      property SocketHost : string  read fSocketHost write setSocketHost ;
      /// The socket port
      property SocketPort : integer read fSocketPort write setSocketPort ;
      /// Indicate if the socket is UDP in place of TCP
      property SocketUdp : boolean read fSocketUdp write setSocketUdp ;

      /// Max Object tree depth for sendValue and Watches
      property ObjectTreeDepth : integer read fObjectTreeDepth write fObjectTreeDepth ;
      // Encoding : enSingleByte or enUnicode (little endian)
      //property Encoding : TEncodingTrace read fEncoding write fEncoding ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   // Specify a font detail for traces columns items and members.
   TFontDetail = class
      ColId           : integer ;
      Bold            : boolean ;
      Italic          : boolean ;
      Color           : TColor ;    // -$7FFFFFFF-1..$7FFFFFFF;
      Size            : integer ;
      FontName        : string ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   { TMemberNode represent a node inside the trace information
     panel                                                     }
   TMemberNode = class
      procedure BeforeDestruction ; override ;
   private
      FontDetails : array of TFontDetail ;
   public
      /// The first columns to display
      Col1 : string ;

      /// The second columns to display
      Col2 : string ;

      /// The third columns to display
      Col3 : string ;

      /// an array of sub members (TMemberNode).
      Members : array of TMemberNode  ;

      /// User defined tag, NOT SEND to the viewer
      Tag : Integer ;

      /// Type of viewer
      ViewerKind : integer ;

      /// Add a member to the members list.
      /// <param name="member">a already constructed member</param>
      /// <returns>the TMember node to add</returns>
      function Add (const member : TMemberNode ) : TMemberNode ; overload ;

      /// Add a member to the members list, giving column 1
      /// <param name="col1">first column member </param>
      /// <returns>the TMember node to add</returns>
      function Add (const col1  : string) : TMemberNode ; overload ;

      /// Add a member to the members list, giving column 1 and 2
      /// <param name="col1">first column member </param>
      /// <param name="col2">second column member</param>
      /// <returns>the TMember node to add</returns>
      function Add (const col1 , col2 : string) : TMemberNode ;  overload ;

      /// Add a member to the members list, giving column 1,2 and 3
      /// <param name="col1">first column member </param>
      /// <param name="col2">second column member</param>
      /// <param name="col3">third column member</param>
      /// <returns>the TMember node to add</returns>
      function Add (const col1 , col2 , col3 : string) : TMemberNode ; overload ;

      /// Set the font detail for the whole line or for a column
      /// <param name="ColId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
      /// <param name="Bold">Change font to bold</param>
      /// <param name="Italic">Change font to Italic</param>
      /// <param name="Color">Change Color. Use -1 to keep default color</param>
      /// <param name="Size">Change font size, use zero to keep normal size</param>
      /// <param name="FontName">Change font name</param>
      /// <returns>itself</returns>
      function SetFontDetail(const ColId: integer; const Bold : boolean ; const Italic: boolean = false; const Color: integer = -1 ; const Size: integer = 0; const FontName: string = ''): TMemberNode;

      /// recursively convert members to a string list
      /// <param name="CommandList">target collection</param>
      procedure AddToStringList (CommandList : TStringList ) ;

      /// Create a TMemberNode with no text in the 3 columns
      constructor create () ; overload ;

      /// Create a TMemberNode with text for the first column
      /// <param name="col1">first column member</param>
      constructor create (const col1 : string) ; overload ;

      /// Create a TMemberNode with text for the first 2 columns
      /// <param name="col1">first column member </param>
      /// <param name="col2">second column member</param>
      constructor create (const col1 , col2 : string) ; overload ;

      /// Create a TMemberNode with text for the 3 columns
      /// <param name="col1">first column member </param>
      /// <param name="col2">second column member</param>
      /// <param name="col3">third column member</param>
      constructor create (const col1 , col2 , col3 : string) ; overload ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   /// <summary>
   /// Construct a table of row to display in the viewer on a node.<p>
   /// The table must be associated with a node. <p>
   /// See TraceNodeEx.AddTable() and TraceToSend.SendTable()
   /// </summary>
   ITraceTable = interface
   ['{E6647A85-38B5-4EB5-8A6B-49CD78A7C1C3}']
      /// Add columns title : one or more columns titles separated by tabs
      /// <param name="colTitle">one or more columns titles separated by tabs. Can also be called several times to add titles</param>
      procedure AddColumnTitle (const ColTitle : string) ;
      /// Add an empty row
      procedure AddRow() ;
      /// Add data to current row
      /// <param name="cell">one or more columns data separated by tabs. Can also be called several times to add cells</param>
      procedure AddRowData (const cell : string) ;
      /// convert to members
      /// <param name="nodeMembers">target</param>
      procedure CopyToNodeMembers(NodeMembers : TMemberNode) ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   /// <summary>
   /// Base class for TraceToSend (TraceNode, Wintrace) and traceNodeEx
   /// </summary>
   ITraceNodeBase = interface
   ['{E0C81242-AFC2-470A-A3CA-A71B2D867011}']
      function getID : string ;
      procedure setID (const v : string) ;
      function  getEnabled : boolean ;
      procedure setEnabled (const v : boolean) ;
      function  getIconIndex: integer;
      procedure setIconIndex (const v : integer) ;
      function  getWinTraceId: string;
      procedure setWinTraceId (const v : string);
      function  getTag : integer ;
      procedure setTag (const v : integer) ;

      /// The unique ID. Normally it's a GUID, but can be replaced by something else for interprocess traces.
      property id : string read getId write setID ;

      /// When Enabled is false, all traces are disabled. Default is true.<p>
      /// All node have a Enabled property, that lets you define group of Enabled trace.<p>
      /// For example set the TTrace.Debug.enabled to false but continue to accept Error and Warning traces
      property  Enabled : boolean read getEnabled write setEnabled ;

      /// The index of the icon to use. You can then show an icon for Warning traces different for Error traces
      property  IconIndex: integer read getIconIndex write setIconIndex ;

      /// The window where trace is send.
      property  WinTraceId: string read getWinTraceId write setWinTraceId ;

      /// User variable, provided for the convenience of developers
      property Tag : integer read getTag write setTag ;

      /// Create a new TraceNodeEx node from the current node
      /// <returns>A TraceNodeEx child node of the current node</returns>
      function CreateNodeEx : ITraceNodeEx ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   {$ifdef COMPILER_12_UP}  // since delphi 2009
   TEnumerableTObject = class(TEnumerable<tObject>);
   {$endif COMPILER_12_UP}

   /// ITraceToSend methodes create new traces and send it to the viewer.<p>
   /// Common interface for ITraceNode and IWinTrace.<p>
   /// TTrace.warning, debug and error are ITraceNode
   ITraceToSend = interface (ITraceNodeBase)
   ['{FD1BD5BD-DABC-4F51-B798-B91361621CBF}']

      { send a trace (one string)
        <param name="leftMsg">The message to display in the 'traces'
                              column</param>
        <returns>
        the new node
        </returns>                                                   }
      function  Send (const leftMsg : string) : ITraceNode ; Overload ;

      { send a trace (2 strings)
        <param name="leftMsg">The message in the "traces" column</param>
        <param name="rightMsg">The message in the "Comment" column</param>
        <returns>
        the new node
        </returns>                                                         }
      function  Send (const leftMsg,RightMsg: string): ITraceNode; Overload ;

      { send a trace (string a variant)
        <param name="leftMsg">The message in the "traces" column</param>
        <returns>
        the new node
        </returns>                                                       }
      function  Send (const leftMsg : variant ): ITraceNode; Overload ;

      { send a trace (2 strings as variant)
        <param name="leftMsg">The message in the "traces" column</param>
        <param name="rightMsg">The message in the "Comment" column</param>
        <returns>
        the new node
        </returns>                                                         }
      function  Send (const leftMsg : variant; const RightMsg: variant): ITraceNode; Overload ;

      /// send a trace with a Tobject definition
      /// <param name="leftMsg">the message to display</param>
      /// <param name="Obj">The object to inspect</param>
      /// <returns>the new node</returns>
      function  SendObject (const leftMsg : string ; const Obj : TObject) : ITraceNode; overload ;

      /// send a trace with a Tobject definition (filtered options)
      /// <param name="leftMsg">the message to display</param>
      /// <param name="Obj">The object to inspect</param>
      /// <param name="flags">what information to display</param>
      /// <returns>the new node</returns>
      function  SendObject (const leftMsg : string ; const Obj : TObject; const flags : TraceDisplayFlags) : ITraceNode; overload ;

      /// send a trace with an IDispatch definition
      /// <param name="leftMsg">the message to display</param>
      /// <param name="disp">The (idispatch) object to inspect </param>
      /// <returns>the new node</returns>
      function  SendObject (const leftMsg : string ; const disp : IDispatch) : ITraceNode; overload ;

      /// send a trace with an IDispatch definition (filtered options)
      /// <param name="leftMsg">the message to display</param>
      /// <param name="disp">The (idispatch) object to inspect </param>
      /// <param name="flags">what information to display</param>
      /// <returns>the new node</returns>
      function  SendObject (const leftMsg : string ; const disp : IDispatch; const flags : TraceDisplayFlags) : ITraceNode; overload ;

      // Ambigious overloaded call under delphi 5 :-(

      {$IFDEF DELPHI_7_UP}
      { send a trace with a variant definition
        <param name="leftMsg">the message to display</param>
        <param name="v">The variant to inspect</param>
        <returns>
        the new node
        </returns>                                           }
      function  SendObject (const leftMsg : string ; const v : variant) : ITraceNode; overload ;

      /// send a trace with a variant definition (filtered options)
      /// <param name="leftMsg">the message to display</param>
      /// <param name="v">The object to inspect</param>
      /// <param name="flags">what information to display</param>
      /// <returns>the new node</returns>
      function  SendObject (const leftMsg : string ; const v : variant; const flags : TraceDisplayFlags) : ITraceNode; overload ;
      {$else}

      /// send a trace with a variant definition (before delphi 7)
      /// <param name="leftMsg">the message to display</param>
      /// <param name="v">The object to inspect</param>
      /// <returns>the new node</returns>
      function  SendObjectV (const leftMsg : string ; const v : variant) : ITraceNode; overload ;

      /// send a trace with a variant definition (filtered options) (before delphi 7)
      /// <param name="leftMsg">the message to display</param>
      /// <param name="v">The object to inspect</param>
      /// <param name="flags">what information to display</param>
      /// <returns>the new node</returns>
      function  SendObjectV (const leftMsg : string ; const v : variant; const flags : TraceDisplayFlags) : ITraceNode; overload ;
      {$ENDIF}

      /// send a trace with variant values
      /// <param name="leftMsg">the message to display</param>
      /// <param name="v">The object to inspect</param>
      /// <param name="ObjTitle">title to display on top of the Value</param>
      /// <returns>the new node</returns>
      function  SendValue (const leftMsg: string; const v: Variant; const ObjTitle:string = 'Object ' ): ITraceNode; overload ;

      /// send a trace with Tobject values
      /// <param name="leftMsg">the message to display</param>
      /// <param name="obj">The object to inspect</param>
      /// <param name="ObjTitle">title to display on top of the Value</param>
      /// <returns>the new node</returns>
      function  SendValue (const leftMsg: string; const Obj : TObject; const ObjTitle:string = 'Object ' ): ITraceNode; overload ;

      /// send a trace with dump
      /// <param name="leftMsg">the message to display</param>
      /// <param name="Title">title to display on top of the dump</param>
      /// <param name="memory">pointer to byte to dump</param>
      /// <param name="ByteCount">number of bytes</param>
      /// <returns>the new node</returns>
      function  SendDump (const leftMsg, Title: string; const memory: pointer; const ByteCount: integer): ITraceNode ;

      /// send a trace with TStrings values
      /// <param name="leftMsg">the message to display</param>
      /// <param name="strings">string collection to display</param>
      /// <returns>the new node</returns>
      function  SendStrings (const leftMsg: string; const strings: TStrings): ITraceNode ;

      /// send a trace with Stack information. Need StackTrace unit
      /// <param name="leftMsg">the message to display</param>
      /// <param name="level">Number of call to skip</param>
      /// <returns>the new node</returns>
      function  SendStack (const leftMsg: string; const level: integer = 0): ITraceNode ; overload ;
      function  SendStack (const leftMsg: string; const rightMsg:string; const level: integer = 0): ITraceNode ;  overload ;

      /// send a trace with Caller information. Need StackTrace unit
      /// <param name="leftMsg">the message to display</param>
      /// <param name="level">Number of call to skip</param>
      /// <returns>the new node</returns>
      function  SendCaller (const leftMsg: string; const level: integer = 0): ITraceNode ;

      /// send a trace with Method information. Need StackTrace unit
      /// <param name="leftMsg">the message to display</param>
      /// <param name="Meth">adress of the method</param>
      /// <returns>the new node</returns>
      function  SendMethod (const leftMsg: string; const Meth: Pointer) : ITraceNode ;

      /// send a trace with a Table content
      /// <param name="leftMsg">the message to display</param>
      /// <param name="table">Table to display</param>
      /// <returns>the new node</returns>
      function  SendTable (const leftMsg : string; table : ITraceTable): ITraceNode  ; overload ;

      /// send a trace with an array as a table
      /// <param name="leftMsg">the message to display</param>
      /// <param name="arr">array of objects to display as a table. All objects MUST be of the same kind</param>
      /// <returns>the new node</returns>
      function SendTable(const leftMsg : string;arr : Array of TObject): ITraceNode   ; overload ;

      /// send a trace with a Tlist as a table
      /// <param name="leftMsg">the message to display</param>
      /// <param name="list">TList of objects to display as a table. All objects MUST be of the same kind</param>
      /// <returns>the new node</returns>
      function SendTable(const leftMsg : string;list : TList): ITraceNode   ; overload ;

      /// send a trace with a collection as a table
      /// <param name="leftMsg">the message to display</param>
      /// <param name="collection">Collection of objects to display as a table. All objects MUST be of the same kind</param>
      /// <returns>the new node</returns>
      function SendTable(const leftMsg : string;collection : TCollection): ITraceNode   ; overload ;

      /// send a trace with strings as a table
      /// <param name="leftMsg">the message to display</param>
      /// <param name="strings">TStrings descendant to display as a table. All objects MUST be of the same kind</param>
      /// <returns>the new node</returns>
      function SendTable(const leftMsg : string;strings : TStrings): ITraceNode   ; overload ;

      {$ifdef COMPILER_10_UP}
      /// send a trace with strings as a table
      /// <param name="leftMsg">the message to display</param>
      /// <param name="strings">TStrings descendant to display as a table. All objects MUST be of the same kind</param>
      /// <returns>the new node</returns>
      function SendTable(const leftMsg : string;strings : TWideStrings): ITraceNode   ; overload ;
      {$endif COMPILER_10_UP}

      {$ifdef COMPILER_12_UP}
      /// send a trace with TEnumerable\<T> as a table
      /// <param name="leftMsg">the message to display</param>
      /// <param name="list">TObject descendant enumeration(TEnumerable\<TObject>) to display as a table. All objects MUST be of the same kind</param>
      /// <returns>the new node</returns>
      function SendTable(const leftMsg: string;list: TEnumerableTObject): ITraceNode ;overload ;
      {$endif COMPILER_12_UP}

      /// send a trace with an Xml string
      /// <param name="leftMsg">the message to display</param>
      /// <param name="XML">xml string</param>
      /// <returns>the new node</returns>
      function  SendXml (const leftMsg : string; const XML: string): ITraceNode ;

      /// send a trace with a Bitmap
      /// <param name="leftMsg">the message to display</param>
      /// <param name="bitmap">bitmap to display</param>
      /// <returns>the new node</returns>
      function  SendBitmap (const leftMsg : string; const Bitmap: TBitmap): ITraceNode ;

      /// send a trace with specific background color
      /// <param name="leftMsg">the message to display</param>
      /// <param name="Color">background color message</param>
      /// <param name="ColId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <returns>the new node</returns>
      function  SendBackgroundColor (const leftMsg: string; const Color: integer; const ColId: integer = -1): ITraceNode ;

      /// Send a trace with an "enter" icon then indent next traces
      /// <param name="leftMsg">the message to display</param>
      /// <param name="rightMsg">the right message to display</param>
      /// <param name="BackGroundColor">background color message</param>
      /// <returns>the new node</returns>
      function EnterMethod (const leftMsg : string ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack) : ITraceNode;

      /// Unindent then send a trace with an "exit" icon
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      /// <param name="BackGroundColor">BackGround Color</param>
      /// <returns>the new node</returns>
      Procedure ExitMethod  (const leftMsg : string = '' ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack);

      /// Send a message with optional color and optional "enter" icon
      /// <param name="leftMsg">Left message to send</param>
      /// <param name="rightMsg">Right message to send</param>
      /// <param name="BackGroundColor">BackGround Color</param>
      /// <param name="IsEnter">if true , a special "enter" icon is added on the node</param>
      /// <returns>the new node</returns>
      function Indent (const leftMsg : string ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack ; const IsEnter:boolean=false) : ITraceNode;
      function IndentWithStack (const leftMsg : string ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack ; const IsEnter:boolean=false) : ITraceNode;

      /// send a trace then unindent
      /// <param name="leftMsg">Message to send to close indentation (optional)</param>
      /// <param name="rightMsg">Message to send to close indentation (optional)</param>
      /// <param name="BackGroundColor">background color </param>
      /// <param name="isExit">if true, viewer type 'exit' is used</param>
      /// <returns>the new node</returns>
      procedure UnIndent (const leftMsg : string = '' ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack ; const IsExit:boolean=false) ;

      /// Return the id of the current node (depends of indentation and thread)
      /// <returns>id</returns>
      function getLastContextId : string;

      /// Return the indentation level (depends of thread)
      /// <returns>indentation level</returns>
      function GetIndentLevel: integer;

      /// Return the indentation level (depends of thread)
      property IndentLevel: integer read GetIndentLevel ;
   end ;

   //--------------------------------------------------------------------------------------------------

   // ItraceNode represents node on the viewer
   ITraceNode = interface (ITraceToSend)
   ['{0287CF63-9832-44CF-824C-9334D79B82A3}']
      /// Resend the trace to the viewer (only left and right message)
      /// <param name="leftMsg">new left message</param>
      /// <param name="RightMsg">new right message</param>
      /// <returns>itself</returns>
      function  Resend (const leftMsg,RightMsg: string) : ITraceNode;overload ;

      /// ReSend left trace to the viewer
      /// <param name="leftMsg">new left message</param>
      /// <returns>itself</returns>
      function  ResendLeft (const leftMsg: string) : ITraceNode;

      /// ReSend right trace to the viewer
      /// <param name="RightMsg">new right message</param>
      /// <returns>itself</returns>
      function  ResendRight (const RightMsg: string) : ITraceNode;

      /// Change the Icon index
      /// <param name="Index">Index of the icon to use</param>
      /// <returns>itself</returns>
      function  ResendIconIndex (const index : integer) : ITraceNode;

      /// append right and left texts to an existing node
      /// <param name="leftMsg">left message</param>
      /// <param name="RightMsg">right message</param>
      /// <returns>itself</returns>
      function  Append (const leftMsg,RightMsg: string): ITraceNode;

      /// append left text to an existing node
      /// <param name="leftMsg">left message</param>
      /// <returns>itself</returns>
      function  AppendLeft (const leftMsg : string) : ITraceNode ;

      /// append right text to an existing node
      /// <param name="RightMsg">right message</param>
      /// <returns>itself</returns>
      function  AppendRight (const RightMsg : string) : ITraceNode ;

      /// ReSend the node with the stack to the viewer
      /// <returns>itself</returns>
      function  AppendStack     () : ITraceNode;

      /// Show the node in the tree (not means selected, just visible in the tree)
      /// <returns>itself</returns>
      function  Show () : ITraceNode ;

      /// Select the node in the viewer
      /// <returns>itself</returns>
      function  SetSelected () : ITraceNode ;

      /// Delete the node
      /// <returns>itself</returns>
      function  Delete () : ITraceNode;

      /// Delete children node
      /// <returns>itself</returns>
      function  DeleteChildren () : ITraceNode;

      /// Change font detail for an item in the trace (whole line or specific column)
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to Italic</param>
      /// <param name="color">Change Color. Use -1 to keep default color</param>
      /// <param name="size">Change font size, use zero to keep normal size</param>
      /// <param name="fontName">Change font name</param>
      /// <returns>itself</returns>
      function  SetFontDetail (const ColId : integer ; const Bold : boolean; const Italic: boolean = false; const Color: integer = -1 ; const Size: integer = 0; const FontName: string = '') : ITraceNode ;

      /// Change Background Color (whole line or specific column) of a node
      /// <param name="color">new background color of the node</param>
      /// <param name="colId">Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined
      /// <returns>itself</returns>
      function  SetBackgroundColor (const Color: TColor; const ColId: integer = -1) : ITraceNode;

      /// Set or reset the bookmark for the node
      /// <param name="Bookmarked">true/false</param>
      /// <returns>itself</returns>
      function  SetBookmark (Bookmarked : boolean) : ITraceNode ;

      /// set a node visible or invisible
      /// <param name="Visible">true/false</param>
      /// <returns>itself</returns>
      function  SetVisible (Visible: boolean) : ITraceNode ;

      /// Set focus to next sibling
      /// <returns>itself</returns>
      function  GotoNextSibling () : ITraceNode;

      /// Set focus to previous sibling
      /// <returns>itself</returns>
      function  GotoPrevSibling () : ITraceNode;

      /// Set focus to first child
      /// <returns>itself</returns>
      function  GotoFirstChild  () : ITraceNode;

      /// Set focus to last child
      /// <returns>itself</returns>
      function  GotoLastChild   () : ITraceNode;

   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   /// second way to send traces : create an object and fill the fields then send it.<p>
   /// the send function return a ITraceNode interface
   ITraceNodeEx = interface (ITraceNodeBase)
   ['{20EDD6B8-D44A-49CE-A304-E42364C733B5}']
      function  getLeftMsg : string ;
      procedure setLeftMsg (const v : string) ;
      function  getRightMsg : string ;
      procedure setRightMsg (const v : string) ;
      function  getParentNodeId : string ;
      procedure setParentNodeId (const v : string) ;
      function  getTime : TDateTime ;
      procedure setTime (const v : TDateTime) ;
      function  getThreadName : string ;
      procedure setThreadName (const v : string) ;
      function getMembers : TMemberNode ;

      /// Left Message
      property  LeftMsg : string read getLeftMsg write setLeftMsg ;

      /// Right Message
      property  RightMsg : string read getRightMsg write setRightMsg ;

      /// The Id of the parent node
      property ParentNodeId : string read getParentNodeId write setParentNodeId ;

      /// time (optional, default is sytem time if empty)
      property time : TDateTime read getTime write setTime ;

      /// Thread Name (optional, default is thread id if empty)
      property ThreadName : string read getThreadName write setThreadName ;

      { \Members, displayed in the trace info panel }
      property Members : TMemberNode read getMembers ;

      /// Add byte dump to the Members
      /// <param name="Title">Tite to display in the first col</param>
      /// <param name="memory">Pointer to the buffer to dump</param>
      /// <param name="ByteCount">Number of bytes to dump</param>
      procedure AddDump (const Title: string; const memory: pointer; const ByteCount: integer) ;

      /// Add TObject information
      /// <param name="Obj">Object to send</param>
      procedure AddObject (const Obj : TObject) ; overload ;

      /// Add TObject information (filtered)
      /// <param name="Obj">Object to send</param>
      /// <param name="flags">determine what information to send</param>
      procedure AddObject (const Obj : TObject ; const flags : TraceDisplayFlags) ; overload ;

      /// Add IDispatch information
      /// <param name="Disp">Object to send</param>
      /// <param name="flags">determine what information to send</param>
      procedure AddObject (const Disp : IDispatch); overload ;

      /// Add IDispatch information (filtered)
      /// <param name="Disp">Object to send</param>
      /// <param name="flags">determine what information to send</param>
      procedure AddObject (const Disp : IDispatch; const flags : TraceDisplayFlags); overload ;

      // Ambigious overloaded call under delphi 5 :-(
      {$IFDEF DELPHI_7_UP}

      /// Add variant information
      /// <param name="v">Object to send</param>
      procedure AddObject (const v : variant); overload ;

      /// Add variant information (filtered)
      /// <param name="v">Object to send</param>
      /// <param name="flags">determine what information to send</param>
      procedure AddObject (const v : variant; const flags : TraceDisplayFlags); overload ;

      {$Else}

      /// Add variant information (before delphi7)
      /// <param name="v">Object to send</param>
      procedure AddObjectV (const v : variant); overload ;

      /// Add variant information (filtered) (before delphi7)
      /// <param name="v">Object to send</param>
      /// <param name="flags">determine what information to send</param>
      procedure AddObjectV (const v : variant; const flags : TraceDisplayFlags); overload ;

      {$ENDIF}

      /// Add value object (variant)
      /// <param name="v">Object to display</param>
      /// <param name="objTitle">Title to display for the object</param>
      procedure AddValue (const v: Variant; const ObjTitle : String = '') ; overload ;

      /// Add value object (TObject)
      /// <param name="obj">Object to display</param>
      /// <param name="objTitle">Title to display for the object</param>
      procedure AddValue (const Obj: TObject; const ObjTitle : String = '') ; overload ;

      /// Add value object (TObject) with max level
      /// <param name="obj">Object to display</param>
      /// <param name="objTitle">Title to display for the object</param>
      /// <param name="maxLevel">Number of sub component to display in tree</param>
      procedure AddValue (const Obj: TObject; const ObjTitle : String ; const MaxLevel : integer) ; overload ;

      /// add string collection.
      /// <param name="strings">The collection</param>
      procedure AddStrings (const strings: TStrings);

      /// Add the stack frame to the Members.
      /// <param name="level">start level (default 1)</param>
      procedure AddStackTrace (const Level : integer = 0);

      /// Add the caller stack information. It's like the call stack, but display only 1 line
      /// <param name="level">Level 0 is self</param>
      procedure AddCaller (const Level : integer = 0);

      /// Add the method information. It's like the call stack, but display only 1 line
      /// <param name="Meth">method pointer</param>
      procedure AddMethod (const Meth : Pointer) ;

      /// Add table to node
      /// <param name="table">table to send</param>
      procedure AddTable (table : ITraceTable) ; overload ;

      /// Add array as a table to node. All objects MUST be of the same kind
      /// <param name="arr">array to send</param>
      procedure AddTable(arr : Array of TObject) ; overload ;

      /// Add list as a table to node. All objects MUST be of the same kind
      /// <param name="list">list to send</param>
      procedure AddTable(list : TList) ; overload ;

      /// Add collection as a table to node. All objects MUST be of the same kind
      /// <param name="collection">collection to send</param>
      procedure AddTable(collection : TCollection) ; overload ;

      /// Add strings as a table to node. All objects MUST be of the same kind
      /// <param name="strings">strings to send</param>
      procedure AddTable(strings : TStrings) ; overload ;

      {$ifdef COMPILER_10_UP}
      /// Add strings as a table to node. All objects MUST be of the same kind
      /// <param name="strings">strings to send</param>
      procedure AddTable(strings : TWideStrings) ; overload ;
      {$endif COMPILER_10_UP}

      {$ifdef COMPILER_12_UP}
      /// Add enumerable object list as a table to node. All objects MUST be of the same kind
      /// <param name="list">list to send</param>
      procedure AddTable(list: TEnumerableTObject);overload ;
      {$endif COMPILER_12_UP}

      /// Add a bitmap
      /// <param name="Bitmap">The Image</param>
      procedure AddBitmap (const Bitmap : TBitmap) ;

      /// Add xml text
      /// <param name="xml">xml text to send</param>
      procedure AddXML (const xml : string) ;

      /// Change font detail for an item in the trace
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to italic</param>
      /// <param name="color">color.Use -1 to keep default color</param>
      /// <param name="size">Change font size, use zero to keep normal size</param>
      /// <param name="fontName">Change font name. Use empty string for default</param>
      /// <returns>itself</returns>
      function  AddFontDetail (const ColId : integer ; const Bold : boolean; const Italic: boolean = false; const Color: integer = -1 ; const Size: integer = 0; const FontName: string = '') : ITraceNodeEx ;

      /// Change background font color
      /// <param name="color">background color</param>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined
      procedure AddBackgroundColor (const Color : TColor; const ColId: integer = -1) ;

      /// send left,right and members
      /// <returns>a new ItraceNode node</returns>
      function Send : ItraceNode ;

      /// resend currrent Left and right msg
      /// <returns>the preview created ItraceNode node</returns>
      function Resend : ItraceNode ; overload ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   /// WinTrace represent a windows tree where you put traces
   IWinTrace = interface (ITraceToSend)
   ['{3BD46634-FBF1-4E54-B554-7EE65C34962A}']

      /// Warning, Error and Debug are the 3 doors to send traces
      /// <returns>The warning node</returns>
      function Warning : ITraceToSend ;

      /// Warning, Error and Debug are the 3 doors to send traces
      /// <returns>The Error node</returns>
      function Error : ITraceToSend ;

      /// Warning, Error and Debug are the 3 doors to send traces
      /// <returns>The Debug node</returns>
      function Debug : ITraceToSend ;

      /// Save the window tree traces to a text file
      /// <param name="FileName">file to save</param>
      procedure SaveToTextfile (const Filename : string) ;

      /// Save the window tree traces to an XML file
      /// <param name="FileName">file to save</param>
      /// <param name="StyleSheet">optional StyleSheet file name added in xml</param>
      procedure SaveToXml (const Filename : string ; const styleSheet : string = '') ;

      /// Load an XML file to the window tree traces
      /// <param name="FileName">file to open</param>
      procedure LoadXml (const Filename : string) ;

      /// Show the window tree
      procedure DisplayWin () ;

      /// change the tree to display user defined multiple columns.<p>
      /// must be called before setting column titles
      /// <param name="MainColIndex">The Main column index (default is 0)</param>
      procedure SetMultiColumn (const MainColIndex : integer = 0) ;

      /// set columns title
      /// <param name="Titles">Tab separated columns titles</param>
      procedure SetColumnsTitle (const Titles:string) ;

      /// set columns widths
      /// <param name="Widths">Tab separated columns width.
      /// The format for each column is width[:Min[:Max]]<p>
      /// where Min and Max are optional minimum and maximum column width for resizing purpose.<p>
      /// Example : 100:20:80 \t 200:50 \t 100
      /// </param>
      procedure SetColumnsWidth (const Widths:string) ;

      /// Set the focus to the trace first node
      procedure GotoFirstNode() ;

      /// Set the focus to the trace last node
      procedure GotoLastNode() ;

      /// Set the focus to the next matching node
      /// <param name="SearForward">If true search down, else search up  </param>
      procedure FindNext(SearForward : boolean) ;

      /// Set the focus to a bookmarked node identified by his position. Bookmarks are cheched by the user or with the node.SetBookmark() function
      /// <param name="Pos">Indice of the bookmark </param>
      procedure GotoBookmark(Pos : integer);

      /// Clear all bookmarks
      procedure ClearBookmark();

      /// Clear all filters
      procedure ClearFilter() ;

      /// Add a filter to node. Multiple calls to this function can be done. Call ApplyFilter() to apply filtering
      /// <param name="Column">Column to apply filter.<p/>
      ///   In multicolumn mode the first column start at 0 <p/>
      ///   In normal mode : <p/>
      ///   col icone   = 999    <p/>
      ///   col time    = 1      <p/>
      ///   col thread  = 2      <p/>
      ///   col traces  = 3      <p/>
      ///   col Comment = 4      <p/>
      ///   col members = 998
      /// </param>
      /// <param name="Compare">There is 5 kinds of filters : <p>
      ///    Equal           = 0  <p/>
      ///    Not equal       = 1  <p/>
      ///    Containts       = 2  <p/>
      ///    Don't contains  = 3  <p/>
      ///    (Ignore this filter) = 4 or -1
      ///</param>
      /// <param name="Text">The text to search (insensitive) </param>
      Procedure AddFilter(Column : integer; Compare : integer ; Text : string) ;

      /// Apply filters after calls to AddFilter().
      /// <param name="ConditionAnd">If true, use an 'AND' condition for each filters, else use a "OR" </param>
      /// <param name="ShowMatch">If true, show node that match filter and hide others. If false hide matching node and show others</param>
      /// <param name="IncludeChildren">If true, search in subnodes</param>
      procedure ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren : boolean) ;

      /// Clear all trace for the window tree
      procedure ClearAll () ;

      /// Set the log file.(Path is relative to the viewer or the client)
      /// To enabled log on local AND on the viewer call this funtion twice.
      /// To don't use the viewer, set the TTrace.options.SendMode to tmNone
      /// <param name="FileName">file to open</param>
      /// <param name="Mode">Local and viewer site log mode.
      /// 0, Viewer Log is disabled.<p>
      /// 1, Viewer log enabled.<p>
      /// 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename).<p>
      /// 3, Local log is disabled.<p>
      /// 4, Local log enabled.<p>
      /// 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename).<p>
      /// </param>
      /// <param name="maxLines">Number of lines before starting a new file (default : -1 = unlimited)</param>
      procedure SetLogFile (const Filename : string; mode : integer; maxLines : integer = -1) ;

      /// Return the last local log file. (when mode 4 or 5 is used). Note : Call TTrace.Flush() to ensure traces are saved
      function GetLocalLogFile : string ;

      /// Close the window tree
      procedure Close() ;

      // plugin API
      //-------------------------

      /// <summary>
      /// Plugin API : Attach a winTrace to a plugin. Many winTrace can be attached to a plugin.<p>
      /// Note that a plugin don't need to be attached to a WinTrace.<p>
      /// The plugin is identified by his internal name (not dll name).<p>
      /// When linked, the plugin can receive event (see ITracePLugin).
      /// </summary>
      /// <param name="PluginName">name of the plugin</param>
      /// <param name="flags">combinaison of CST_PLUG_ONACTION , CST_PLUG_ONBEFOREDELETE , CST_PLUG_ONTIMER</param>
      procedure LinkToPlugin (const PluginName : string ; const flags : integer) ;

      /// <summary>
      /// Plugin API : Create a resource.
      /// </summary>
      /// <param name="ResId">The resource Id (must be >= 100)</param>
      /// <param name="ResType">Resource type. See TraceConst
      /// <code>
      /// CST_RES_BUT_RIGHT    : Button on right
      /// CST_RES_BUT_LEFT     : Button on left
      /// CST_RES_LABEL_RIGHT  : Label on right
      /// CST_RES_LABELH_RIGHT : Label on right HyperLink
      /// CST_RES_LABEL_LEFT   : Label on left
      /// CST_RES_LABELH_LEFT  : Label on left hyperlink
      /// CST_RES_MENU_ACTION  : Item menu in the Actions Menu
      /// CST_RES_MENU_WINDOW  : Item menu in the Windows Menu.
      ///                        Call CreateResource on the main win trace to create this menu item
      /// </code>
      ///</param>
      /// <param name="ResWidth">Width of the resource. Applicable only to button and labels</param>
      /// <param name="ResText">Resource text</param>
      procedure CreateResource (const ResId : integer ; const ResType : integer ; const ResWidth : integer ; const ResText : string) ;

      /// <summary>
      /// Plugin API : Disable tracetool or user created resources
      /// </summary>
      /// <param name="ResId">The resource Id
      /// ResId: resource id to disable. Tracetool resources :
      /// <code>
      /// CST_ACTION_CUT            : Cut. Same as copy then delete
      /// CST_ACTION_COPY           : Copy
      /// CST_ACTION_DELETE         : Delete selected
      /// CST_ACTION_SELECT_ALL     : Select all
      /// CST_ACTION_RESIZE_COLS    : Resize columns
      /// CST_ACTION_VIEW_INFO      : View trace info
      /// CST_ACTION_VIEW_PROP      : View properties
      /// CST_ACTION_PAUSE          : Pause
      /// CST_ACTION_SAVE           : SaveToFile
      /// CST_ACTION_CLEAR_ALL      : Clear all
      /// CST_ACTION_CLOSE_WIN      : Close win
      /// CST_ACTION_LABEL_INFO     : TracesInfo label
      /// CST_ACTION_LABEL_LOGFILE  : LabelLogFile label
      /// CST_ACTION_VIEW_MAIN      : View Main trace
      /// CST_ACTION_VIEW_ODS       : ODS
      /// CST_ACTION_OPEN_XML       : XML trace -> Tracetool XML traces
      /// CST_ACTION_EVENTLOG       : Event log
      /// CST_ACTION_TAIL           : Tail
      /// </code>
      /// </param>
      procedure DisableResource (const ResId : integer) ;

      /// <summary>
      /// Plugin API : Set the resource text (tracetool or user created resources), specified by his Id
      /// </summary>
      /// <param name="ResId">The resource Id </param>
      /// <param name="ResText">Resource text</param>
      procedure SetTextResource (const ResId : integer ; const ResText : string) ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   // Check if the viewer is running. If not try to run it.
   function StartDebugWin: hWnd;
   function TT_getVariantType (const TypeInfo : ITypeInfo ; const ObjTypeDesc: PTypeDesc) : String ; overload ;
   function TT_GetVariantType (const VarType: integer): String; overload ;
   function TT_GetVarValue (const v: Variant): string; overload ;
   function TT_GetVarValue (const v: Variant; var strType: String): string; overload ;
   function TT_GetDispatchName (const Disp : IDispatch) : string ;
   procedure TT_GetDispathDescriptions (const TypeAttr : PTypeAttr; var strGuid,strProg, strTypeLibGuid, strTypeLib : string) ; overload ;
   procedure TT_GetDispathDescriptions (const Disp : IDispatch; var strGuid,strProg, strTypeLibGuid, strTypeLib : string) ; overload ;
   procedure TT_SetCurrentThreadName(const Name: AnsiString);

   function TT_getPushedMessageCount() : integer ;
   function TT_getSendMessageCount() : integer ;
   function TT_getLastExceptionMessage() : TstringList ;

var
   AlternateSend : TSocketTrace ;    // the alternate procedure (if for example SocketTrace unit is specified)

   // The followings procedures are used to extend TraceTool. By default theses procedures are nil
   // the developer need to include SocketTrace or stacktrace to automatically set theses procedures.
   SendStackProc     : TSendStackProc  ;
   SendCallerProc    : TSendCallerProc ;
   SendMethodProc    : TSendMethodProc ;

implementation

{$ifdef DELPHI_6_UP}uses variants; {$endif}    // add variant for delphi 6 and upper

type

   TMsgThread   = class ;
   TTraceTable  = class ;
   TWinWatch    = class ;
   TNodeContext = class ;
   TTraceNode   = class ;
   TWinTrace    = class ;
   TInternalWinTrace = class ;

   // from Delphi 7 (don't exist in delphi 5)

   PPointer = ^Pointer ;
   PMethodInfoHeader = ^TMethodInfoHeader;
   TMethodInfoHeader = packed record
     Len: Word;
     Addr: Pointer;
     Name: ShortString;
   end;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   TInternalWinTrace = class (TObject)
      ID : string ;
      IsMultiColTree : boolean ;
      MainCol : integer ;
      TitleList : TStringList ;
      LogFileName : string ;
      LastLocalLogFileName : string ;
      LogFileType : integer ;

      MaxLines : integer;             // Max number of lines before starting a new file
      CurrentFileNumber : integer ;   // Current file number, when MaxLines is set
      LinesWritten : integer ;        // Number of lines written , when MaxLines is set

      procedure AfterConstruction() ;  override ;
      procedure BeforeDestruction() ;  override ;
   end ;

   // the thread that send to the viewer
   TMsgThread = class(TThread)
   private
      procedure ParseForInternal(MsgList: TstringList) ;
   public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;

   protected
      procedure Execute; override;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   TTraceTable = class (TInterfacedObject , ITraceTable)
   public
      procedure AddColumnTitle (const ColTitle : string) ;
      procedure AddRow() ;
      procedure AddRowData (const cell : string) ;
      procedure CopyToNodeMembers (NodeMembers : TMemberNode);
   private
      fMembers : TMemberNode ;
      fCurrentRow : TMemberNode ;
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   TWinWatch = class (TInterfacedObject, IWinWatch)
   private
      fId : string ;
      fEnabled : boolean ;
      fTag : integer ;
      constructor create () ; overload ;
   public
      constructor create (const WinWatchID : string ; const WinWatchText : string = '') ; overload ;

      // Window Id
      function  getId : string ;
      procedure setId (const v : string) ;
      property  Id : string read getId write setId ;

      // Enabled
      function  getEnabled : boolean ;
      procedure setEnabled (const v : boolean) ;
      property  Enabled : boolean read getEnabled write setEnabled ;

      // User variable, provided for the convenience of developers
      function  getTag : integer ;
      procedure setTag (const v : integer) ;
      property Tag : integer read getTag write setTag ;

      procedure DisplayWin () ;
      procedure ClearAll () ;
      procedure Close() ;

      // Send functions
      procedure  Send (const WatchName : string ;const RightMsg: string); Overload ;
      procedure  Send (const WatchName : string; const v: variant);  Overload ;
      procedure  Send (const WatchName : string; const Obj : TObject);  Overload ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   TNodeContext = class
   public
     ThreadId : DWORD ;
     //Level    : integer ;
     NodeId : string ;
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   TTraceNode = class (TInterfacedObject, ITraceNode , ITracenodeEx)
   private
      fId : string ;
      fEnabled : boolean ;
      fIconIndex: integer ;
      fMembers : TMemberNode ;  // Members object is create only when needed (AddDump, AddObject,...)
      FontDetails : array of TFontDetail ;   // used by AddFontDetail, not by SetFontDetail (command is directly send)
      fLeftMsg , fRightMsg : string ; // used in ITraceNodeEx mode
      fParentNodeId : string ;
      fWinTraceId : string ;
      fTag : integer ;           // User variable, provided for the convenience of developers
      fContextList : TObjectList ;      // own objects
      fWinTraceContext : TObjectList ;  //  WinTrace context. reference to wintrace.fContextList is set
      fListCriticalSection : TCriticalSection ;
      fTime : TDateTime ;
      fThreadName : string ;

      // TOject reflection functions
      function  GetPropertyTypeString (const TypeKind: TTypeKind): String;
      procedure inner_addValue (const AObject: TObject; const upperNode : TMemberNode; const MaxLevel : integer; AlreadyParsedObject:TObjectList);
      procedure DisplayFields  (const AObject: TObject; const flags : TraceDisplayFlags);
      procedure DisplayMethods (const AObject: TObject; const flags : TraceDisplayFlags);
      procedure DisplayBases   (const AObject: TObject; const flags : TraceDisplayFlags);

      // create new node and context related fcts.
      function  prepareNewNode (const leftMsg, newId: string): TStringList;
      function  getLastContext  : TNodeContext ;
      procedure PushContext (NewContext: TNodeContext);
      procedure deleteLastContext ;
   public

      constructor create (const ParentNode : TTraceNode ; const generateUniqueId : boolean ) ; overload ;
      constructor create (const ParentNode : ITraceToSend ; const generateUniqueId : boolean ) ; overload ;
      procedure BeforeDestruction ; override ;

      // ITraceNodeBase : ID, Enabled , IconIndex , WinTraceId , tag and  CreateNodeEx
      //----------------------------------------------------------------------------------------------

      function  getId : string ;
      procedure setID (const v : string) ;
      property id : string read getId write setID ;

      function  getEnabled : boolean ;
      procedure setEnabled (const v : boolean) ;
      property  Enabled : boolean read getEnabled write setEnabled ;

      function  getIconIndex: integer;
      procedure setIconIndex (const v : integer) ;
      property  IconIndex: integer read getIconIndex write setIconIndex ;

      function  getWinTraceId: string;
      procedure setWinTraceId (const v : string);
      property  WinTraceId: string read getWinTraceId write setWinTraceId ;

      function  getTag : integer ;
      procedure setTag (const v : integer) ;
      property Tag : integer read getTag write setTag ;


      // create a new TraceNodeEx node with the current instance as parent.
      function CreateNodeEx : ITraceNodeEx ;

      // ITraceNode
      //--------------------------------------------


      // Send functions
      function  Send        (const lMsg : string) : ITraceNode ; Overload ;
      function  Send        (const leftMsg, RightMsg: string) : ITraceNode; Overload ;
      function  Send        (const leftMsg : variant): ITraceNode; Overload ;
      function  Send        (const leftMsg : variant; const RightMsg: variant): ITraceNode; Overload ;

      function  SendObject  (const leftMsg : string ; const Obj : TObject) : ITraceNode ; overload ;
      function  SendObject  (const leftMsg : string ; const Obj : TObject; const flags : TraceDisplayFlags) : ITraceNode ; overload ;
      function  SendObject  (const leftMsg : string ; const disp : IDispatch) : ITraceNode; overload ;
      function  SendObject  (const leftMsg : string ; const disp : IDispatch; const flags : TraceDisplayFlags) : ITraceNode; overload ;

      // Ambigious overloaded call under delphi 5 :-(
      {$IFDEF DELPHI_7_UP}
      function  SendObject  (const leftMsg : string ; const v : variant) : ITraceNode; overload ;
      function  SendObject  (const leftMsg : string ; const v : variant; const flags : TraceDisplayFlags) : ITraceNode; overload ;
      {$ELSE (before delphi 7)}
      function  SendObjectV (const leftMsg : string ; const v : variant) : ITraceNode; overload ;
      function  SendObjectV (const leftMsg : string ; const v : variant; const flags : TraceDisplayFlags) : ITraceNode; overload ;
      {$ENDIF}

      function  SendValue   (const leftMsg: string; const v: Variant; const ObjTitle:string = 'Object ' ): ITraceNode; overload ;
      function  SendValue   (const leftMsg: string; const Obj : TObject; const ObjTitle:string = 'Object ' ): ITraceNode; overload ;
      function  SendDump    (const leftMsg, Title: string; const memory: pointer; const ByteCount: integer) : ITraceNode ;
      function  SendStrings (const leftMsg: string; const strings: TStrings) : ITraceNode ;
      function  SendStack   (const leftMsg: string; const level: integer = 0) : ITraceNode ; overload ;
      function  SendStack   (const leftMsg: string; const rightMsg:string; const level: integer = 0): ITraceNode ;  overload ;
      function  SendCaller  (const leftMsg: string; const level: integer = 0) : ITraceNode ;
      function  SendMethod  (const leftMsg: string; const Meth: Pointer) : ITraceNode;

      function  SendTable   (const leftMsg : string ; table : ITraceTable): ITraceNode  ; overload ;
      function  SendTable   (const leftMsg : string ; arr : Array of TObject) : ITraceNode  ; overload ;
      function  SendTable   (const leftMsg : string ; list : TList) : ITraceNode  ; overload ;
      function  SendTable   (const leftMsg : string ; collection : Tcollection) : ITraceNode  ; overload ;
      function  SendTable   (const leftMsg : string ; strings : TStrings): ITraceNode   ; overload ;
      {$ifdef COMPILER_10_UP}
      function  SendTable   (const leftMsg : string ; strings : TWideStrings): ITraceNode   ; overload ;
      {$endif COMPILER_10_UP}

      {$ifdef COMPILER_12_UP}
      function SendTable(const leftMsg: string;list: TEnumerableTObject): ITraceNode ;overload ;
      {$endif COMPILER_12_UP}

      function  SendXml     (const leftMsg : string; const XML: string): ITraceNode ;
      function  SendBitmap  (const leftMsg : string; const Bitmap: TBitmap): ITraceNode ;
      function  SendBackgroundColor (const leftMsg: string; const Color: integer; const ColId: integer = -1): ITraceNode ;

      function EnterMethod (const leftMsg : string ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack) : ITraceNode;
      Procedure ExitMethod  (const leftMsg : string = '' ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack);

      function Indent   (const leftMsg : string ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack; const IsEnter:boolean=false) : ITraceNode;
      function IndentWithStack   (const leftMsg : string ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack; const IsEnter:boolean=false) : ITraceNode;
      procedure UnIndent (const leftMsg : string = '' ; const rightMsg : string = ''; const BackGroundColor: TColor = clBlack; const IsExit:boolean=false) ;
      function  GetIndentLevel: integer;
      property  IndentLevel: integer read GetIndentLevel ;
      function  getLastContextId : string;


      // Resend functions
      function  Resend : ItraceNode ;  overload ;  // resend the node (don't recreate a node and the target)
      function  Resend          (const leftMsg, RightMsg: string) : ITraceNode ; overload ;
      function  ResendLeft      (const leftMsg: string) : ITraceNode;
      function  ResendRight     (const RightMsg: string) : ITraceNode;
      function  ResendIconIndex (const index : integer) : ITraceNode;
      function  AppendStack     () : ITraceNode;
      function  Append          (const leftMsg, RightMsg: string) : ITraceNode; Overload ;
      function  AppendLeft      (const leftMsg : string) : ITraceNode ;
      function  AppendRight     (const RightMsg : string) : ITraceNode ;

      function  Show            () : ITraceNode ;
      function  SetSelected     () : ITraceNode ;
      function  Delete          () : ITraceNode;
      function  DeleteChildren  () : ITraceNode;
      function  SetFontDetail   (const ColId : integer ; const Bold : boolean; const Italic: boolean = false; const Color: integer = -1 ; const Size: integer = 0; const FontName: string = '') : ITraceNode ;
      function  SetBackgroundColor (const Color: TColor; const ColId: integer = -1) : ITraceNode;

      function  SetBookmark     (Bookmarked : boolean) : ITraceNode ;
      function  SetVisible      (visible: boolean) : ITraceNode ;
      function  GotoNextSibling () : ITraceNode;
      function  GotoPrevSibling () : ITraceNode;
      function  GotoFirstChild  () : ITraceNode;               // link to the node's first child...
      function  GotoLastChild   () : ITraceNode;               // link to the node's last child...

      // ITraceNodeEx
      //--------------------------------------------

      // LeftMsg
      function  getLeftMsg : string ;
      procedure setLeftMsg (const v : string) ;
      property  LeftMsg : string read getLeftMsg write setLeftMsg ;

      // RightMsg
      function  getRightMsg : string ;
      procedure setRightMsg (const v : string) ;
      property  RightMsg : string read getRightMsg write setRightMsg ;

      // ParentNodeId : The Id of the parent node
      function  getParentNodeId : string ;
      procedure setParentNodeId (const v : string) ;
      property ParentNodeId : string  read FParentNodeId write SetParentNodeId;

      // time (optional, default is sytem time if empty)
      function  getTime : TDateTime ;
      procedure setTime (const v : TDateTime) ;
      property time : TDateTime read getTime write setTime ;

      // ThreadName (optional, default is thread id if empty)
      function  getThreadName : string ;
      procedure setThreadName (const v : string) ;
      property ThreadName : string read getThreadName write setThreadName ;

      // Members
      function getMembers : TMemberNode ;
      property Members : TMemberNode read getMembers ;

      // Add procedure
      procedure AddDump (const Title: string; const memory: pointer; const ByteCount: integer) ;
      procedure AddObject (const Obj : TObject; const flags : TraceDisplayFlags) ;  overload ;
      procedure AddObject (const Obj : TObject) ; overload ;
      procedure AddObject (const disp: IDispatch; const flags : TraceDisplayFlags); overload ;
      procedure AddObject (const disp: IDispatch); overload ;
      function  AddFontDetail (const ColId : integer ; const Bold : boolean; const Italic: boolean = false; const Color: integer = -1 ; const Size: integer = 0; const FontName: string = '') : ITraceNodeEx ;
      procedure AddBackgroundColor (const Color : TColor; const ColId: integer = -1) ;


      {$IFDEF DELPHI_7_UP}
      procedure AddObject (const v : variant; const flags : TraceDisplayFlags); overload ;
      procedure AddObject (const v : variant); overload ;
      {$ELSE (before delphi 7)}
      procedure AddObjectV (const v : variant; const flags : TraceDisplayFlags); overload ;
      procedure AddObjectV (const v : variant); overload ;
      {$ENDIF}

      procedure AddValue (const v: Variant; const ObjTitle : String = '') ; overload ;
      procedure AddValue (const Obj: TObject; const ObjTitle : String = '') ; overload ;
      procedure AddValue (const Obj: TObject; const ObjTitle : String ; const MaxLevel : integer) ; overload ;

      procedure AddStrings (const strings: TStrings);
      procedure AddStackTrace (const Level : integer = 0);
      procedure AddCaller (const Level : integer = 0);
      procedure AddMethod (const Meth : Pointer) ;

      procedure AddTable (table : ITraceTable) ; overload ;
      procedure AddTable(arr : Array of TObject) ; overload ;
      procedure AddTable(list : TList) ; overload ;
      procedure AddTable(Collection : TCollection) ; overload ;
      procedure AddTable(strings : TStrings) ; overload ;
      {$ifdef COMPILER_10_UP}
      procedure AddTable(strings : TWideStrings) ; overload ;
      {$endif COMPILER_10_UP}

      {$ifdef COMPILER_12_UP}
      procedure AddTable(list: TEnumerableTObject);overload ;
      {$endif COMPILER_12_UP}

      procedure AddXML (const xml : string) ;
      procedure AddBitmap (const Bitmap : TBitmap) ;

      // send left, right , fonts and members
      function Send : ItraceNode ; Overload ;

   private
      procedure AddValue (const Obj: TObject; const ObjTitle : String ; const MaxLevel : integer; AlreadyParsedObject:TObjectList) ; overload ;
      procedure innerAddFieldsToTable(TableMembers : TMemberNode ; CurrentRow : TMemberNode ; const AObject: TObject ; isFirstRow,isFirstCol : bool);
   end ;

   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

   TWinTrace = class (TTraceNode, IWinTrace)   //     TInterfacedObject
   private
      fDebug    : ITraceNode ;
      fError    : ITraceNode ;
      fWarning  : ITraceNode ;
   public
      constructor create (const WinTraceID : string ; const WinTraceText : string = '') ; overload ;
      constructor create () ; overload ;
      function Warning : ITraceToSend ;
      function Error : ITraceToSend ;
      function Debug : ITraceToSend ;

      procedure SaveToTextfile (const Filename : string) ;
      procedure SaveToXml (const Filename : string; const styleSheet : string = '') ;
      procedure DisplayWin () ;
      procedure setMultiColumn (const MainColIndex : integer = 0) ;
      procedure setColumnsTitle (const Titles:string) ;
      procedure setColumnsWidth (const Widths:string) ;

      procedure ClearAll () ;
      procedure Close() ;
      procedure LoadXml (const Filename : string) ;
      procedure setLogFile (const Filename : string; mode : integer;maxLines : integer = -1) ;
      function GetLocalLogFile : string ;

      procedure GotoFirstNode() ;
      procedure GotoLastNode() ;
      procedure FindNext(SearForward : boolean) ;
      procedure GotoBookmark(pos : integer);
      procedure clearBookmark();
      procedure ClearFilter() ;
      Procedure AddFilter(column : integer; compare : integer ; text : string) ;
      procedure ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren : boolean) ;

      // plugin API
      procedure LinkToPlugin (const PluginName : string ; const flags : integer) ;
      procedure CreateResource (const ResId : integer ; const ResType : integer ; const ResWidth : integer ; const ResText : string) ;
      procedure DisableResource (const ResId : integer) ;
      procedure SetTextResource (const ResId : integer ; const ResText : string) ;

      // low level
      //function CreateNode () : ITraceNode ;
   end ;


   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------

var
   AnsiCharTypes: array [AnsiChar] of Word;
   MsgThread : TMsgThread ;
   _WinTrace : IWinTrace ;
   __WinTrace : TWinTrace ;
   _Watches  : IWinWatch ;
   _opt      : TTraceOptions = nil ;

   // Messages list
   criticalSection : TCriticalSection ;
   setMessageList  : TObjectList ;
   getMessageList  : TObjectList;
   FormTraceList   : TObjectList ;         // all trace form (owner)
   DefaultWinTrace : TInternalWinTrace ;

   ClosedEvent  : THandle ;     // signal that the thread is terminated
   CloseEvent   : THandle ;     // Handle to close the thread.
   MessageReady : THandle ;     // signal that messages are ready to be send

   pushedMessageCount : integer ;
   sendMessageCount : integer ;
   LastExceptionMessage : TStringList ;
   Line : integer ;   // for exception handling

threadvar
   SendObjectRecursiveStatus : short ;

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

// general purpose procedure and functions

// code only
procedure addCommand(CommandList: TStringList; code: integer); overload ;
begin
   CommandList.add (Format('%5d', [code])) ;
end;

//------------------------------------------------------------------------------

// code + int
procedure addCommand(CommandList: TStringList; code, intvalue: integer); overload ;
begin
   CommandList.add (Format('%5d%11d', [code,intvalue]));
end;

//------------------------------------------------------------------------------

// code + string
procedure addCommand(CommandList: TStringList; code: integer; lib: string); overload ;
begin
   CommandList.add (Format('%5d', [code]) + lib);
end;

//------------------------------------------------------------------------------

// code + int + string
procedure addCommand(CommandList: TStringList; code, intvalue: integer; lib: string); overload ;
begin
   CommandList.add (Format('%5d%11d', [code,intvalue]) + lib);
end;

//------------------------------------------------------------------------------

// code + int + int + string
procedure addCommand(CommandList: TStringList; code, intvalue1,intvalue2: integer; lib: string); overload ;
begin
   CommandList.add (Format('%5d%11d%11d', [code,intvalue1,intvalue2]) + lib);
end;

//------------------------------------------------------------------------------

// code + int + int + int + string
procedure addCommand(CommandList: TStringList; code, intvalue1,intvalue2,intvalue3: integer; lib: string); overload ;
begin
   CommandList.add (Format('%5d%11d%11d%11d', [code,intvalue1,intvalue2,intvalue3]) + lib);
end;

//------------------------------------------------------------------------------

// code + bool
procedure addCommand(CommandList: TStringList; code: integer; boolvalue: boolean); overload ;
begin
   if boolvalue = true then
      addCommand (CommandList, code, '1')
   else
      addCommand (CommandList, code,'0') ;
end;

//------------------------------------------------------------------------------

function CharIsAlphaNum(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or
    ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

//--------------------------------------------------------------------------------------------------

function CharIsAlpha(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

//------------------------------------------------------------------------------

procedure LoadCharTypes;
var
  CurrChar: AnsiChar;
  CurrType: Word;
begin
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
    AnsiCharTypes[CurrChar] := CurrType;
  end;
end;

//------------------------------------------------------------------------------

// from delphi 6
function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

// from delphi 5
//function GetPropList(TypeInfo: PTypeInfo; PropList: PPropList): Integer;
//var
//  I, Count: Integer;
//  PropInfo: PPropInfo;
//  TempList: PPropList;
//begin
//  Result := 0;
//  Count := GetTypeData(TypeInfo)^.PropCount;
//  if Count > 0 then
//  begin
//    GetMem(TempList, Count * SizeOf(Pointer));
//    try
//      GetPropInfos(TypeInfo, TempList);
//      for I := 0 to Count - 1 do
//      begin
//        PropInfo := TempList^[I];
//        if PropList <> nil then PropList^[Result] := PropInfo;
//        Inc(Result);
//      end;
//    finally
//      FreeMem(TempList, Count * SizeOf(Pointer));
//    end;
//  end;
//end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TInternalWinTrace.AfterConstruction;
begin
   ID := '' ;
   IsMultiColTree := false ;
   MainCol := 0 ;
   TitleList := nil ;
   LogFileName := '' ;
   LogFileType := 3 ;
   MaxLines := -1 ;
   CurrentFileNumber := 0 ;
   LinesWritten := 0 ;
end;

//------------------------------------------------------------------------------

procedure TInternalWinTrace.BeforeDestruction;
begin
   if TitleList <> nil then
      TitleList.free ;
end;

//------------------------------------------------------------------------------

{ TTraceTable }

procedure TTraceTable.AfterConstruction;
begin
   inherited;
   fMembers := TMemberNode.create ;
   fMembers.ViewerKind := CST_VIEWER_TABLE ;
   fCurrentRow := nil ;
end;

//------------------------------------------------------------------------------

procedure TTraceTable.BeforeDestruction;
begin
   inherited;
   if fMembers <> nil then
      fMembers.free ;
   fMembers := nil ;
end;

//------------------------------------------------------------------------------

procedure TTraceTable.AddColumnTitle(const ColTitle: string);
begin
   if fMembers.Col1 = '' then
      fMembers.Col1 := ColTitle
   else
      fMembers.Col1 := fMembers.Col1 + #9 + ColTitle ;
end;

//------------------------------------------------------------------------------

procedure TTraceTable.AddRow();
begin
   fCurrentRow := fMembers.Add('') ;
end;

//------------------------------------------------------------------------------

procedure TTraceTable.AddRowData(const cell: string);
begin
   if fCurrentRow = nil then
      AddRow() ;

   if fCurrentRow.Col1 = '' then
      fCurrentRow.Col1 := cell
   else
      fCurrentRow.Col1 := fCurrentRow.Col1 + #9 + cell ;
end;

//------------------------------------------------------------------------------

// create a copy of the internal member to node members
procedure TTraceTable.CopyToNodeMembers(NodeMembers: TMemberNode);
var
   c : integer ;
   TableMembers : TMemberNode ;
begin
   TableMembers := NodeMembers.Add(fMembers.Col1) ;
   TableMembers.ViewerKind := CST_VIEWER_TABLE ;
   for c := 0 to length(fMembers.Members)-1 do
      TableMembers.Add(fMembers.Members[c].Col1) ;
   // members will be free by destructor
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TWinTrace }

// public constructor. The trace windows is created on the viewer
constructor TWinTrace.create(const WinTraceID : string ; const WinTraceText : string = '');
var
   CommandList : TStringList;
   wText : string ;
begin
   if WinTraceID = '' then
      self.fId := TTrace.CreateTraceID()
   else
      self.fId := WinTraceID ;

   create () ;  // public constructor. Create wintrace context. Init Debug, Warning and error node.

   if WinTraceID = '_' then
      exit ;

   CommandList := TStringList.create ;  // will be freed by the thread

   if WinTraceText = '' then
      wText := 'Trace ' + self.fId
   else
      wText := WinTraceText ;

   addCommand (CommandList, CST_TREE_NAME, wText);
   TTrace.SendToWinTraceClient(CommandList, fId);   // SendToWinTraceClient will add a CST_USE_TREE with the windows ID
end;


//------------------------------------------------------------------------------
// public constructor : you can map a WinTrace to an existing window
// Nothing is send to the viewer
// Create winrtace context. Init Debug, Warning and error node.
constructor TWinTrace.create;
var
   TraceNode : TTraceNode ;
begin
   // don't call inherited constructor.

   self.fIconIndex := CST_ICO_DEFAULT ;    // TWinTrace don't have icon (for now)
   self.fEnabled := true ;
   self.fParentNodeId := '' ;
   self.WinTraceId := self.id ;    // winTraceId need to be the same as 'id' if we want to call sendXxx() directly on WinTrace object
   fMembers := nil ; // created only when needed ;

   fContextList := TObjectList.create (true) ;  // own context

   TraceNode := TTraceNode.create (nil,false);
   TraceNode.fWinTraceId := self.fId ;
   TraceNode.fIconIndex :=  CST_ICO_INFO ;
   TraceNode.fWinTraceContext := fContextList ;   // use the same context as wintrace
   fDebug := TraceNode ;

   TraceNode := TTraceNode.create (nil,false);
   TraceNode.fWinTraceId := self.fId ;
   TraceNode.fIconIndex := CST_ICO_WARNING ;
   TraceNode.fWinTraceContext := fContextList ;   // use the same context as wintrace
   fWarning := TraceNode ;

   TraceNode := TTraceNode.create (nil,false);
   TraceNode.fWinTraceId := self.fId ;
   TraceNode.fIconIndex := CST_ICO_ERROR ;
   TraceNode.fWinTraceContext := fContextList ;   // use the same context as wintrace
   fError := TraceNode ;
end;

//------------------------------------------------------------------------------

function TWinTrace.Debug: ITraceToSend;
begin
   result := fDebug ;
end;

//------------------------------------------------------------------------------

function TWinTrace.Error: ITraceToSend;
begin
   result := fError ;
end;

//------------------------------------------------------------------------------

function TWinTrace.Warning: ITraceToSend;
begin
   result := fWarning ;
end;

//------------------------------------------------------------------------------

procedure TWinTrace.ClearAll;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLEAR_ALL);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.Close;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList,CST_CLOSE_WIN );
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.DisplayWin;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_DISPLAY_TREE);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.GotoFirstNode() ;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_FIRST_NODE);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.GotoLastNode() ;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_LAST_NODE);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.FindNext(SearForward : boolean) ;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   if SearForward then
      addCommand (CommandList, CST_FIND_NEXT,1)
   else
      addCommand (CommandList, CST_FIND_NEXT,0);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.GotoBookmark(pos : integer);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_BOOKMARK,pos);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.clearBookmark();
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLEAR_BOOKMARK);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.ClearFilter() ;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLEAR_FILTER);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

Procedure TWinTrace.AddFilter(column : integer; compare : integer ; text : string) ;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_ADD_FILTER , column, compare , text);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren : boolean) ;
var
   CommandList : TStringList;
   flags : integer ;
begin
   // ConditionAnd<<2+ShowMatch<<1+IncludeChildren
   flags := 0 ;
   if ConditionAnd then
      inc(flags, 4) ;
   if ShowMatch then
      inc(flags, 2) ;
   if IncludeChildren then
      inc(flags, 1) ;

   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_APPLY_FILTER,flags);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.setColumnsWidth (const Widths:string) ;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_TREE_COLUMNWIDTH,Widths);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.SaveToTextfile (const Filename: string);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_SAVETOTEXT, Filename);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.SaveToXml (const Filename: string; const styleSheet : string = '');
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   if styleSheet = '' then
      addCommand (CommandList, CST_SAVETOXML, Filename)
   else
      addCommand (CommandList, CST_SAVETOXML, Filename + '|' + styleSheet);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.LoadXml (const Filename: string);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_LOADXML, Filename);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

function getInternalTraceForm (TraceWinID :string; doCreate : boolean) : TInternalWinTrace ;
var
   c : integer ;
begin
   if (TraceWinID = '') or (TraceWinID = '_') then begin
      result := DefaultWinTrace ;
      exit ;
   end ;

   for c := 0 to FormTraceList.Count-1 do begin
      result := TInternalWinTrace (FormTraceList.Items[c]) ;
      if result.ID = TraceWinID then begin
         exit ;
      end ;
   end ;

   // if the trace window don't exist, create it if needed
   result := nil ;
   if doCreate = true then begin
      result := TInternalWinTrace.Create();
      result.LogFileName := '' ;
      result.LogFileType := 3 ;   // no log
      result.IsMultiColTree := false ;
      FormTraceList.Add(result) ;
      result.ID := TraceWinID ;
   end ;
end ;

//------------------------------------------------------------------------------

{$WARN SYMBOL_DEPRECATED OFF}
// Html encoding : convert special chars to &#nnn;
// differ from delphi HttApp unit implementation (fusion of HTTPEncode and HTMLEncode)
function HtmlEncode(const AStr: String): String;
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',' ', ':', #13 , #10 ,
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PChar;
begin
  SetLength(Result, Length(AStr) * 6);    // each bytes can be converted to 6 chars
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '&': begin
             FormatBuf(Rp^, 5, '&amp;', 5, []);
             Inc(Rp,4);                            // 4+1
           end;
      '<': begin
             FormatBuf(Rp^, 4, '&lt;', 4, []);
             Inc(Rp,3);                            // 3+1
           end;
      '>': begin
             FormatBuf(Rp^, 4, '&gt;', 4, []);
             Inc(Rp,3);                            // 3+1
           end;
      '"': begin
             FormatBuf(Rp^, 6, '&quot;', 6, []);
             Inc(Rp,5);                            // 5+1
           end;
    else // case else
       Rp^ := Sp^ ;    // will be converted in UTF8

       //if Sp^ in NoConversion then
       //  Rp^ := Sp^
       //else begin
       //  FormatBuf(Rp^, 6, '&#%3.3d;', 8, [Ord(Sp^)]);    // 6 chars
       //  Inc(Rp,5);    // 5 + 1 in the next statement
       //end;
    end ;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

{$WARN SYMBOL_DEPRECATED DEFAULT}

//------------------------------------------------------------------------------

// convert a string (pchar) delimited with a special char like Tab (9) to a Tstringlist
// getDelimStrings (source, #9)
function getDelimStrings(source: pchar; delim : char): TStringList;
var
   Ptr1,ptr2 : pchar ;
begin
   result := TStringList.create ;
   ptr2 := source ;
   ptr1 := ptr2 ;

   // loop until we found a #0
   while (true ) do begin
      if (ptr2^ = #0) then begin
         result.Add(ptr1) ;
         exit ;
      end else if (ptr2^ = delim) then begin       // separator
         ptr2^ := #0 ;
         result.Add(ptr1) ;
         ptr2^ := delim ;    // keep source unchanged
         inc (ptr2) ;
         ptr1 := ptr2 ;
      end else begin
         inc (ptr2);
      end ;
   end ;
end ;

//------------------------------------------------------------------------------

procedure TWinTrace.setMultiColumn (const MainColIndex : integer = 0) ;
var
   CommandList : TStringList;
   TraceForm : TInternalWinTrace ;
begin
   // since tracetool 10.1 : save the MultiColumn mode and MainColIndex
   TraceForm := getInternalTraceForm (fId,true) ;
   TraceForm.IsMultiColTree := true ;
   TraceForm.MainCol := MainColIndex ;

   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_TREE_MULTI_COLUMN, MainColIndex);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

procedure TWinTrace.setColumnsTitle (const Titles:string) ;
var
   CommandList : TStringList;
   TraceForm : TInternalWinTrace ;
   strTiles : string ;
begin
   strTiles := trim(Titles) ; // copy before parse with getDelimStrings
   TraceForm := getInternalTraceForm (fId,true) ;
   TraceForm.IsMultiColTree := true ;
   TraceForm.TitleList := getDelimStrings (pchar(strTiles),#9)  ;  // change the columns titles

   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_TREE_COLUMNTITLE,Titles);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

// Set the log file.(Path is relative to the viewer)
// fileName : target filename.
// mode : 0, Viewer Log is disabled.
//        1, Viewer log enabled. 
//        2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
//        3, Local log is disabled
//        4, Local log enabled. 
//        5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
// To enabled log on local AND on the viewer call this funtion twice.
// To don't use the viewer, set the TTrace.options.SendMode to tmNone

procedure TWinTrace.setLogFile (const Filename: string; mode: integer ; maxLines : integer = -1);
var
   CommandList : TStringList;
   TraceForm : TInternalWinTrace ;
begin

   // 3, Local log is disabled
   // 4, Local log enabled. No size limit.
   // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   if mode >= 3 then begin
      // since tracetool 10.1 : save the Filename and mode if this mode is >= 3
      TraceForm := getInternalTraceForm (fId,true) ;
      if (TraceForm = nil) then
         exit;
      TraceForm.LogFileName := Filename ;
      TraceForm.LogFileType := mode ;
      TraceForm.MaxLines    := maxLines ;
   end else begin
      CommandList := TStringList.create ;  // will be freed by the thread
      addCommand (CommandList, CST_LOGFILE, mode, maxLines, Filename) ;
      TTrace.SendToWinTraceClient (CommandList,fId);
   end ;
end;

// Return the last local log file. (when mode 4 or 5 is used). Note : Call TTrace.Flush() to ensure traces are saved
function TWinTrace.GetLocalLogFile : string ;
var
   TraceForm : TInternalWinTrace ;
begin
   TraceForm := getInternalTraceForm (fId,true) ;
   if (TraceForm = nil) then
      exit;
   result := TraceForm.LastLocalLogFileName;

end;
//------------------------------------------------------------------------------

// Attach a winTrace to a plugin. Many winTrace can be attached to a plugin.
// The inverse is true : a plugin don't need to be attached to a WinTrace
// The plugin identified by his internal name (not dll name).
// When linked, the plugin can receive event.
// PluginName : name of the plugin.
// flags : combinaison of CST_PLUG_ONACTION , CST_PLUG_ONBEFOREDELETE , CST_PLUG_ONTIMER

procedure TWinTrace.LinkToPlugin (const PluginName: string; const flags: integer);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_LINKTOPLUGIN, flags, PluginName);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

// create a resource on a WinTrace.
// ResId   : The resource Id (must be >= 100)
// ResType : resource type
//   CST_RES_BUT_RIGHT        = 1 ;     // Button on right
//   CST_RES_BUT_LEFT         = 2 ;     // Button on left
//   CST_RES_LABEL_RIGHT      = 3 ;     // Label on right
//   CST_RES_LABELH_RIGHT     = 4 ;     // Label on right HyperLink
//   CST_RES_LABEL_LEFT       = 5 ;     // Label on left
//   CST_RES_LABELH_LEFT      = 6 ;     // Label on left hyperlink
//   CST_RES_MENU_ACTION      = 7 ;     // Item menu in the Actions Menu
//   CST_RES_MENU_WINDOW      = 8 ;     // Item menu in the Windows Menu. Call CreateResource on the main win trace to create this menu item
// ResWidth : Width of the resource. Applicable only to button and labels
// ResText : the resource text

procedure TWinTrace.CreateResource(const ResId : integer ; const ResType : integer ; const ResWidth : integer ; const ResText : string);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CREATE_RESOURCE, ResId , ResType, ResWidth , ResText);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

// Disable tracetool or user created resources
// ResId: resource id to disable. Tracetool resources :
//   CST_ACTION_CUT           = 1 ;     // cut same as copy then delete
//   CST_ACTION_COPY          = 2 ;     // copy
//   CST_ACTION_DELETE        = 3 ;     // delete selected
//   CST_ACTION_SELECT_ALL    = 4 ;     // select all
//   CST_ACTION_RESIZE_COLS   = 5 ;     // resize columns
//   CST_ACTION_VIEW_INFO     = 6 ;     // view trace info
//   CST_ACTION_VIEW_PROP     = 7 ;     // view properties
//   CST_ACTION_PAUSE         = 8 ;     // Pause
//   CST_ACTION_SAVE          = 9 ;     // SaveToFile
//   CST_ACTION_CLEAR_ALL     = 10 ;    // clear all
//   CST_ACTION_CLOSE_WIN     = 11 ;    // Close win
//   CST_ACTION_LABEL_INFO    = 20 ;    // TracesInfo label
//   CST_ACTION_LABEL_LOGFILE = 21 ;    // LabelLogFile label
//   CST_ACTION_VIEW_MAIN     = 50 ;    // View Main trace
//   CST_ACTION_VIEW_ODS      = 51 ;    // ODS
//   CST_ACTION_OPEN_XML      = 52 ;    // XML trace -> Tracetool XML traces
//   CST_ACTION_EVENTLOG      = 53 ;    // Event log
//   CST_ACTION_TAIL          = 54 ;    // Tail

procedure TWinTrace.DisableResource(const ResId: integer);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_DISABLE_RESOURCE, ResId);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

// Set the resource text (tracetool or user created resources), specified by his Id
procedure TWinTrace.SetTextResource(const ResId: integer; const ResText: string);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_SET_TEXT_RESOURCE, ResId, ResText);
   TTrace.SendToWinTraceClient (CommandList,fId);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TWinWatch }

// private constructor, used by TTrace
constructor TWinWatch.create;
begin
   fEnabled := true ;
end;

//------------------------------------------------------------------------------

// public constructor
constructor TWinWatch.create (const WinWatchID, WinWatchText: string);
var
   CommandList : TStringList;
   wId, wText : string ;
begin
   create () ;
   if WinWatchID = '' then
      wId := TTrace.CreateTraceID()
   else
      wId := WinWatchID ;

   fId := wId ;
   CommandList := TStringList.create ;  // will be freed by the thread

   if WinWatchText = '' then
      wText := 'Watches ' + fId
   else
      wText := WinWatchText ;
   addCommand (CommandList, CST_WINWATCH_NAME, wText);
   TTrace.SendToWinWatchClient (CommandList,fId);
end;

//------------------------------------------------------------------------------

function TWinWatch.getEnabled: boolean;
begin
   result := fEnabled ;
end;

//------------------------------------------------------------------------------

procedure TWinWatch.setEnabled (const v: boolean);
begin
   fEnabled := v ;
end;

//------------------------------------------------------------------------------

function TWinWatch.getId: string;
begin
   result := fId ;
end;

//------------------------------------------------------------------------------

procedure TWinWatch.setId (const v: string);
begin
   fId := v ;
end;

//------------------------------------------------------------------------------

function TWinWatch.getTag: integer;
begin
   result := fTag ;
end;

//------------------------------------------------------------------------------

procedure TWinWatch.setTag (const v: integer);
begin
   fTag := v ;
end;

//------------------------------------------------------------------------------

procedure TWinWatch.Send (const WatchName, RightMsg: string);
var
   v : variant ;
begin
   v := RightMsg ;
   Send(WatchName, v);
end;

//------------------------------------------------------------------------------

procedure TWinWatch.Send (const WatchName:string; const Obj : TObject);
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (fEnabled = false) then
      exit ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (nil,false) ;  // no parent, don't generate node id

   CommandList := TStringList.create ;     // will be freed by the thread
   addCommand (CommandList, CST_WATCH_NAME, WatchName) ;  // param : watch name

   node.AddValue (Obj);      // no title and maxlevel is the ttrace.options.objectTreeDepth
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   node.free ;

   TTrace.SendToWinWatchClient (CommandList, fId);
end;

//------------------------------------------------------------------------------

procedure TWinWatch.Send (const WatchName: string; const v: variant);
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (fEnabled = false) then
      exit ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (nil,false) ;  // no parrent, don't generate node id

   CommandList := TStringList.create ;     // will be freed by the thread
   addCommand (CommandList, CST_WATCH_NAME, WatchName) ;  // param : watch name

   node.AddValue (v);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   node.free ;

   TTrace.SendToWinWatchClient (CommandList, fId);
end;

//------------------------------------------------------------------------------

procedure TWinWatch.ClearAll;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLEAR_ALL);
   TTrace.SendToWinWatchClient (CommandList,fId);
end;

//------------------------------------------------------------------------------


procedure TWinWatch.Close;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLOSE_WIN);
   TTrace.SendToWinWatchClient (CommandList,fId);
end;


//------------------------------------------------------------------------------

procedure TWinWatch.DisplayWin;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_DISPLAY_TREE);
   TTrace.SendToWinWatchClient (CommandList,fId);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TTrace }

class function TTrace.Debug: ITraceToSend;
begin
   result := WinTrace().Debug ;
end;

//------------------------------------------------------------------------------

class function TTrace.Error: ITraceToSend;
begin
   result := WinTrace().Error ;
end;

//------------------------------------------------------------------------------

class function TTrace.Warning: ITraceToSend;
begin
   result := WinTrace().Warning ;
end;

//------------------------------------------------------------------------------

class function TTrace.WinTrace: IWinTrace;
begin
   if _WinTrace = nil then begin
      __WinTrace := TWinTrace.create ;
      _WinTrace := __WinTrace ;
   end ;
   result := _WinTrace ;
end;

//------------------------------------------------------------------------------

class function TTrace.Watches: IWinWatch;
begin
   if _Watches = nil then
      _Watches := TWinWatch.create ;
   result := _Watches ;
end;

//------------------------------------------------------------------------------

class function TTrace.Options: TTraceOptions;
begin
   if _opt = nil then
      _opt := TTraceOptions.Create ;
   result := _opt ;
end;

//------------------------------------------------------------------------------

class function TTrace.CreateTraceID: string;
var
//  P: PWideChar;
  TID: TGUID ;
begin
  CoCreateGuid(TID); //   UuidCreate
  Result := Format('%8.8x%4.4x%4.4x%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x', [TID.D1,TID.D2,TID.D3,TID.D4[0],TID.D4[1],TID.D4[2],TID.D4[3],TID.D4[4],TID.D4[5],TID.D4[6],TID.D4[7] ]) ;
  //StringFromCLSID(TID, P);
  //Result := P;
  //CoTaskMemFree(P);
end;

//------------------------------------------------------------------------------

class function TTrace.createWinTrace(const WinTraceID, WinTraceText: string): IWinTrace;
var
  res : TWinTrace ;
begin
   res := TWinTrace.create (WinTraceID, WinTraceText) ;
   result := res ;
end;

//------------------------------------------------------------------------------

class function TTrace.CreateNodeEx(ParentNode: ITraceToSend): ItraceNodeEx;
var
   node : TTraceNode ;
begin
   node := TTraceNode.create (ParentNode,true);

   //node.fIconIndex    := ParentNode.IconIndex ;
   //node.fEnabled      := ParentNode.Enabled ;
   //node.fParentNodeId := ParentNode.getlastcontextId() ;  // get parent node id (or node context id)
   //node.fWinTraceId   := ParentNode.WinTraceId ;
   result := node ;
end;

//------------------------------------------------------------------------------

class function TTrace.CreateNodeFromId (ParentNode: ITraceToSend;NodeId : string) : ItraceNode ;
var
   node : TTraceNode ;
begin
   node := TTraceNode.create (ParentNode,false);
   node.id := NodeId ;
   result := node ;
end;

//------------------------------------------------------------------------------

class function TTrace.createWinWatch(const WinWatchID, WinWatchText: string): IWinWatch;
begin
   result := TWinWatch.create (WinWatchID, WinWatchText) ;
end;

//------------------------------------------------------------------------------

class function TTrace.CreateTraceTable: ITraceTable;
begin
   result := TTraceTable.create () ;
end;

//------------------------------------------------------------------------------

class procedure TTrace.ClearAll () ;
begin
   WinTrace.ClearAll() ;
end;

//--------------------------------------------------------------------------------------------------

class procedure TTrace.Show(IsVisible: boolean);
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   if IsVisible = true then
      addCommand (CommandList, CST_SHOW, 1)
   else
      addCommand (CommandList, CST_SHOW, 0);

   SendToClient (CommandList);
end;

//--------------------------------------------------------------------------------------------------

class procedure TTrace.Find (Text : string; Sensitive, WholeWord , Highlight, SearchInAllPages : bool) ;
var
   CommandList : TStringList;
   flags : integer ;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   flags := 0 ;
   // Sensitive<<3+WholeWord<<2+highlight<<1+SearchInAllPages
   if Sensitive then
      inc(flags, 8) ;
   if WholeWord then
      inc(flags, 4) ;
   if Highlight then
      inc(flags, 2) ;
   if SearchInAllPages then
      inc(flags, 1) ;

   addCommand (CommandList, CST_FIND_TEXT, flags, Text);
   SendToClient (CommandList);
end;

//--------------------------------------------------------------------------------------------------

class procedure TTrace.CloseViewer;
var
   CommandList : TStringList;
begin
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLOSE_VIEWER);
   SendToClient (CommandList);
end;

//------------------------------------------------------------------------------

function TT_getPushedMessageCount() : integer ;
begin
   result := pushedMessageCount;
end ;

//------------------------------------------------------------------------------
function TT_getSendMessageCount() : integer ;
begin
   result := sendMessageCount;
end ;

//------------------------------------------------------------------------------

function TT_getLastExceptionMessage() : TstringList ;
begin
   result := LastExceptionMessage;
end ;

//------------------------------------------------------------------------------

class procedure TTrace.start;
   // adapted from Classes.TThread.Resume() to bypass warning 'Resume is deprecated'
   procedure Resume;
   var
     SuspendCount: Integer;
     SThreadError : string ;
   begin
     SuspendCount := ResumeThread(MsgThread.Handle);
     //CheckThreadError(SuspendCount >= 0);
     if SuspendCount < 0 then
       if GetLastError <> 0 then begin
          SThreadError := 'Thread Error: %s (%d)';
          raise EThread.CreateResFmt(@SThreadError, [SysErrorMessage(GetLastError), GetLastError]);
       end;

     if SuspendCount = 1 then
       MsgThread.Suspended := False;
   end;
begin
   pushedMessageCount := 0 ;
   sendMessageCount := 0 ;
   LastExceptionMessage := tStringList.create ;

   if MsgThread <> nil then
      exit ;
   MsgThread := TMsgThread.Create (true) ;   // create suspended , also create CloseEvent and MessageReady events
   MsgThread.FreeOnTerminate := true ;       // let the thread delete itself
   Resume() ;
end;

//------------------------------------------------------------------------------

class procedure TTrace.stop;
begin
   if MsgThread = nil then
      exit ;
   if CloseEvent = 0 then
      exit ;

   if LastExceptionMessage <> nil then
      LastExceptionMessage.free ;

   // ask the thread to shutdown
   setevent(CloseEvent) ;

   // wait for closed event (max 1 second)
   WaitForSingleObject(ClosedEvent, 1000) ;

   // Let the thread delete itself with FreeOnTerminate := true ;

   CloseHandle (CloseEvent) ;
   CloseHandle (ClosedEvent) ;
   CloseHandle (MessageReady) ;

   CloseEvent   := 0 ;
   ClosedEvent  := 0 ;
   MessageReady := 0 ;
   MsgThread := nil ;
end;

//------------------------------------------------------------------------------

class procedure TTrace.SendToWinWatchClient (CommandList: TStringList ; winWatchId : string);
begin
   // check if sub system is down
   if MsgThread = nil then begin
      CommandList.Free;
      exit ;
   end;

   // CST_PROCESS_NAME,CST_THREAD_ID and CST_MESSAGE_TIME must be processsed before other command

   // insert process name
   if Options.fProcessFileName <> '' then
      CommandList.Insert(0,Format('%5d%s', [CST_PROCESS_NAME,Options.fProcessFileName]));

   // insert thread id
   if Options.fSendThreadId then
      CommandList.Insert(0,Format('%5d%11d', [CST_THREAD_ID,getCurrentThreadID()])) ;

   // add current Date/time
   if options.fSendDate then       // system date and time
      CommandList.Insert(0,Format('%5d%s', [CST_MESSAGE_TIME,FormatDateTime('yyyymmdd hh:mm:ss:zzz',now)]))
   else                                     // system time
      CommandList.Insert(0,Format('%5d%s', [CST_MESSAGE_TIME,FormatDateTime('hh:mm:ss:zzz',now)])) ;

   // CST_USE_TREE must be inserted at the FIRST position
   //if winWatch <> nil then
   //   if winWatch.ID <> '' then
   CommandList.Insert(0, Format('%5d%s', [CST_WINWATCH_ID,winWatchId])) ;
   SendToClient (CommandList) ;
end ;

//------------------------------------------------------------------------------

class procedure TTrace.SendToWinTraceClient (CommandList: TStringList ; winTraceId : string; DateTime : TDateTime = 0; ThreadName : string = '');
begin
   // check if sub system is down
   if MsgThread = nil then begin
      CommandList.Free;
      exit ;
   end;

   // CST_PROCESS_NAME,CST_THREAD_ID and CST_MESSAGE_TIME must be processsed before other command

   // insert process name
   if Options.fProcessFileName <> '' then
      CommandList.Insert(0,Format('%5d%s', [CST_PROCESS_NAME,Options.fProcessFileName]));

   // insert thread id
   if Options.fSendThreadId then
      if ThreadName = '' then
         CommandList.Insert(0,Format('%5d%11d', [CST_THREAD_ID,getCurrentThreadID()]))
      else
         CommandList.Insert(0,Format('%5d%s', [CST_THREAD_NAME,ThreadName]));

   // add current Date/time
   if DateTime <> 0 then begin
      if options.fSendDate then             // user date and time
         CommandList.Insert(0,Format('%5d%s', [CST_MESSAGE_TIME,FormatDateTime('yyyymmdd hh:mm:ss:zzz',DateTime)]))
      else                                  // user time
         CommandList.Insert(0,Format('%5d%s', [CST_MESSAGE_TIME,FormatDateTime('hh:mm:ss:zzz',DateTime)]))
   end else if options.fSendDate then       // system date and time
      CommandList.Insert(0,Format('%5d%s', [CST_MESSAGE_TIME,FormatDateTime('yyyymmdd hh:mm:ss:zzz',now)]))
   else                                     // system time
      CommandList.Insert(0,Format('%5d%s', [CST_MESSAGE_TIME,FormatDateTime('hh:mm:ss:zzz',now)])) ;

   // CST_USE_TREE must be inserted at the FIRST position
   if winTraceId <> '' then
      CommandList.Insert(0, Format('%5d%s', [CST_USE_TREE,winTraceId])) ;

   SendToClient (CommandList) ;
end;

//------------------------------------------------------------------------------

class function TTrace.Flush (FlushTimeOut : integer = 5000) : integer;
var
   flushEvent : THandle ;
   CommandList: TStringList ;
begin
   result := -100 ;
   try
      // check if sub system is down
      if MsgThread = nil then begin
         exit ;
      end;

      // create the event
      flushEvent := CreateEvent( nil, True, False, nil );  // Create the close event. Manualreset = true, initial = false
      ResetEvent(flushEvent) ;

      // create the flush event message
      CommandList := TStringList.Create ;
      CommandList.Insert(0,Format('%5d%d', [CST_FLUSH ,flushEvent]));

      // add it to queue and tell the sender thread to read the queue
      SendToClient (CommandList);

      // wait for the sender read events. (max 5 sec)
      result := WaitForSingleObject(flushEvent, FlushTimeOut) ;

      // release event
      CloseHandle (flushEvent) ;
   except
      on e : exception do begin
         result := -111 ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

class procedure TTrace.SendToClient (CommandList: TStringList);
begin
   // check if sub system is down
   if MsgThread = nil then begin
      CommandList.Free;
      exit ;
   end;

   criticalSection.Acquire ;
   InterlockedIncrement(pushedMessageCount) ;
   setMessageList.Add(CommandList);
   setevent(MessageReady) ;
   criticalSection.Leave ;
end ;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TMemberNode }

constructor TMemberNode.create;
begin
   create ('','','') ;
end;

//------------------------------------------------------------------------------

constructor TMemberNode.create (const col1: string);
begin
   create (col1 , '' , '') ;
end;

//------------------------------------------------------------------------------

constructor TMemberNode.create (const col1, col2: string);
begin
   create (col1 , col2 , '') ;
end;

//------------------------------------------------------------------------------

constructor TMemberNode.create (const col1, col2, col3: string);
begin
   self.Col1 := col1 ;
   self.Col2 := col2 ;
   self.Col3 := col3 ;
   ViewerKind := 0 ;  // normal viewer
end;

//------------------------------------------------------------------------------

/// clear the sub members
procedure TMemberNode.BeforeDestruction;
var
   c : integer ;
begin
   inherited BeforeDestruction;
   // FontDetails is empty when the node is send.
   // If not send, the destructor must free it
   for c := 0 to length (FontDetails)-1 do begin
      FontDetails[c].Free ;
      FontDetails[c] := nil ;
   end ;
   setlength (FontDetails,0) ;

   for c := 0 to length (Members)-1 do
      Members[c].free ;
end;

//------------------------------------------------------------------------------

function TMemberNode.Add (const member: TMemberNode): TMemberNode;
var
   le : integer ;
begin
   le := length (Members) ;
   SetLength (Members, le+1);
   Members[le] := member ;
   result := member ;
end;

//------------------------------------------------------------------------------

function TMemberNode.Add (const col1: string): TMemberNode;
begin
   result := Add (col1 , '' , '') ;
end;

//------------------------------------------------------------------------------

function TMemberNode.Add (const col1, col2: string): TMemberNode;
begin
   result := Add (col1 , col2 , '') ;
end;

//------------------------------------------------------------------------------

function TMemberNode.Add (const col1, col2, col3: string): TMemberNode;
var
   member : TMemberNode ;
begin
   member := TMemberNode.create (col1,col2,col3) ;
   Add (member) ;
   result := member ;
end;

//------------------------------------------------------------------------------

function TMemberNode.SetFontDetail(const ColId: integer; const Bold, Italic: boolean; const Color, Size: integer; const FontName: string): TMemberNode;
var
   FontDetail : TFontDetail ;
   ArrayLen : integer ;
begin
   FontDetail := TFontDetail.create ;
   FontDetail.ColId    := ColId ;
   FontDetail.Bold     := Bold ;
   FontDetail.Italic   := Italic ;
   FontDetail.Color    := Color ;
   FontDetail.Size     := Size ;
   FontDetail.FontName := FontName ;
   ArrayLen := length(FontDetails) ;
   SetLength (FontDetails,ArrayLen+1);
   FontDetails [ArrayLen] := FontDetail ;
   result := self ;
end;

//------------------------------------------------------------------------------

procedure TMemberNode.AddToStringList(CommandList: TStringList);
var
   node : TMemberNode ;
   c : integer ;

   procedure _AddToStringList(subnode : TMemberNode ; CommandList: TStringList);
   var
      SSnode : TMemberNode ;
      FontDetail : TFontDetail ;
      d : integer ;
      TempStr : string ;
   begin

      // create the member and set col1
      addCommand (CommandList, CST_CREATE_MEMBER, subnode.Col1);
      // add command if col2 and/or col3 exist
      if (subnode.Col2 <> '') then
         addCommand (CommandList, CST_MEMBER_COL2, subnode.Col2) ;

      if (subnode.Col3 <> '') then
         addCommand (CommandList, CST_MEMBER_COL3, subnode.Col3);

      // add viewer kind
      if subnode.ViewerKind <> 0 then
         addCommand (CommandList, CST_MEMBER_VIEWER_KIND, subnode.ViewerKind);

      // add font detail
      for d := 0 to length (subnode.FontDetails)-1 do begin
         FontDetail := subnode.FontDetails[d] ;

         TempStr := Format('%5d%3d', [CST_MEMBER_FONT_DETAIL,FontDetail.ColId]) ;

         if FontDetail.Bold then
            TempStr := TempStr + '1'
         else
            TempStr := TempStr + '0' ;

         if FontDetail.Italic then
            TempStr := TempStr + '1'
         else
            TempStr := TempStr + '0' ;

         TempStr := TempStr + Format('%11d%11d', [FontDetail.Color,FontDetail.Size]) + FontDetail.FontName  ;
         CommandList.add (TempStr);

         FontDetail.Free ;   // once copied to Commandlist, free the node
         subnode.FontDetails[d] := nil ;
      end ;
      setlength (subnode.FontDetails,0) ;  // once copied to Commandlist, clear the array

      // recursive add sub nodes, if any
      for d := 0 to length (subnode.Members)-1 do begin
         SSnode := subnode.Members[d] ;
         _AddToStringList (SSnode, CommandList);
         SSnode.Free ;   // once copied to Commandlist, free the node
         subnode.Members[d] := nil ;
      end ;
      setlength (subnode.Members,0) ;  // once copied to Commandlist, clear the array

      // close the member group
      addCommand (CommandList, CST_ADD_MEMBER) ;

  end;
begin

   // the root node node itself is not send for now.
   // Later we can send the 3 columns text to specify the header, if specfied.
   // the text should be like that : "Myfirstcol:150" where 150 is the column with
   for c := 0 to length (Members)-1 do begin
      node := Members[c] ;
      _AddToStringList (node,CommandList);
      node.Free ;   // once copied to Commandlist, free the node
      Members[c] := nil ;
   end ;
   setlength (Members,0) ;  // once copied to Commandlist, clear the array
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ ItraceNodeBase common functions }

function TTraceNode.getId: string;
begin
   result := fId ;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.setID (const v: string);
begin
   fId := v ;
end;

//------------------------------------------------------------------------------

function TTraceNode.getEnabled: boolean;
begin
   result := fEnabled ;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.setEnabled (const v: boolean);
begin
   fEnabled := v ;
end;

//------------------------------------------------------------------------------

function TTraceNode.getIconIndex: integer;
begin
   result := fIconIndex ;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.setIconIndex (const v: integer);
begin
   fIconIndex := v ;
end;

//------------------------------------------------------------------------------

function TTraceNode.getWinTraceId: string;
begin
   result := fWinTraceId
end;

//------------------------------------------------------------------------------

procedure TTraceNode.setWinTraceId(const v: string);
begin
   fWinTraceId := v ;
end;

//------------------------------------------------------------------------------

function TTraceNode.getTag: integer;
begin
   result := fTag ;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.setTag (const v: integer);
begin
   fTag := v ;
end;

//------------------------------------------------------------------------------

function TTraceNode.CreateNodeEx: ITraceNodeEx;
var
   node : TTraceNode ;
begin
   node := TTraceNode.create (self,true);
   result := node ;
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.Send (const leftMsg: variant): ITraceNode;
begin
   result := Send (tt_GetVarValue (leftMsg)) ;
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.Send (const leftMsg, RightMsg: variant): ITraceNode;
begin
   result := Send (tt_GetVarValue (leftMsg),tt_GetVarValue (RightMsg)) ;
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.Send (const lMsg: string): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
   pc : pchar ;
   lMsg2 : string ;
begin
   if (fEnabled = false) then begin
      result := self ;
      exit ;
   end ;

   // Ajust leftMsg in case of bstr
   pc := Pchar (lMsg) ;
   lMsg2 := pc ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (lMsg2 , node.Id) ;  // will be freed by the thread

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.Send (const leftMsg, RightMsg: string): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
   pc : pchar ;
   leftMsg2, RightMsg2 : string ;
begin
   if (fEnabled = false) then begin
      result := self ;
      exit ;
   end ;

   // Ajust leftMsg and rightMsg in case of bstr
   pc := Pchar (leftMsg) ;  leftMsg2 := pc ;
   pc := Pchar (rightMsg) ; rightMsg2 := pc ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg2, node.Id) ;  // will be freed by the thread
   if rightMsg2 <> '' then
      addCommand (CommandList, CST_RIGHT_MSG,rightMsg2);    // param : right string

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
// send IDispatch
function TTraceNode.SendObject (const leftMsg: string;  const disp: IDispatch): ITraceNode;
begin
   result := SendObject (leftMsg , disp , TTrace.options.GetDefault) ;
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendObject (const leftMsg: string; const disp: IDispatch; const flags: TraceDisplayFlags): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddObject (disp,flags);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
// send variant

{$IFDEF DELPHI_7_UP}
function TTraceNode.SendObject (const leftMsg: string; const v: variant): ITraceNode;
begin
   result := SendObject (leftMsg , v , TTrace.options.GetDefault) ;
end;

{$else}

function TTraceNode.SendObjectV (const leftMsg: string; const v: variant): ITraceNode;
begin
   result := SendObjectV (leftMsg , v , TTrace.options.GetDefault) ;
end;
{$ENDIF}

//------------------------------------------------------------------------------

// ITraceToSend
{$IFDEF DELPHI_7_UP}
function TTraceNode.SendObject (const leftMsg: string; const v: variant; const flags: TraceDisplayFlags): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddObject (v,flags);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

{$Else}

function TTraceNode.SendObjectV (const leftMsg: string; const v: variant; const flags: TraceDisplayFlags): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddObjectV (v,flags);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;
{$ENDIF}

//------------------------------------------------------------------------------

// ITraceToSend
// send TObject
function TTraceNode.SendObject (const leftMsg: string; const Obj: TObject): ITraceNode;
begin
   result := SendObject (leftMsg , Obj , TTrace.options.GetDefault) ;
end ;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendObject (const leftMsg: string; const Obj: TObject; const flags : TraceDisplayFlags): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddObject (Obj, flags);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendDump (const leftMsg: string; const Title: string; const memory: pointer; const ByteCount: integer): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddDump(Title, memory, ByteCount );
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendStrings (const leftMsg: string; const strings: TStrings): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddStrings (strings);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

function TTraceNode.SendTable(const leftMsg: string;  arr: array of TObject): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(arr);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

function TTraceNode.SendTable(const leftMsg: string; strings: tStrings): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(strings);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

{$ifdef COMPILER_10_UP} // since delphi 2006
function TTraceNode.SendTable(const leftMsg: string;  strings: TWideStrings): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(strings);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;
{$endif COMPILER_10_UP}

//------------------------------------------------------------------------------

{$ifdef COMPILER_12_UP}
function TTraceNode.SendTable(const leftMsg: string; list: TEnumerableTObject): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(list);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

{$endif COMPILER_12_UP}

//------------------------------------------------------------------------------

function TTraceNode.SendTable(const leftMsg: string; list: TList): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(list);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

function TTraceNode.SendTable(const leftMsg: string; collection: Tcollection): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(collection);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendTable(const leftMsg: string; table: ITraceTable): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddTable(table);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendStack (const leftMsg: string; const level : integer): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddStackTrace(level+1);
   if node.fMembers <> nil then
      node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

// ITraceToSend
function TTraceNode.SendStack (const leftMsg: string; const rightMsg: string; const level : integer): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread
   if RightMsg <> '' then
      addCommand (CommandList, CST_RIGHT_MSG, RightMsg);         // param : right string
   node.AddStackTrace(level+1);
   if node.fMembers <> nil then
      node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendCaller (const leftMsg: string; const level : integer): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddCaller(level+1);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendMethod (const leftMsg: string; const Meth : Pointer): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddMethod(Meth);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendBackgroundColor(const leftMsg: string;  const Color: integer; const ColId: integer = -1): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread
   addCommand (CommandList, CST_BACKGROUND_COLOR, Color,inttostr(ColId)) ;      // param : color, colId

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendBitmap(const leftMsg: string;  const Bitmap: TBitmap): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddBitmap(Bitmap);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendXml(const leftMsg, XML: string): ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddXML(XML);
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendValue (const leftMsg: string; const v: Variant ; const ObjTitle : string = 'Object ') : ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddValue (v,Objtitle) ;
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end ;

//------------------------------------------------------------------------------

// ITraceToSend
function TTraceNode.SendValue (const leftMsg: string; const Obj : TObject ; const ObjTitle : string = 'Object ') : ITraceNode;
var
   node : TTraceNode ;
   CommandList : TStringList;
begin
   if (Enabled = false) then begin
      result := self ;
      exit ;
   end ;

   // create a node with same properties as "self" with new ID
   node := TTraceNode.create (self, true) ;
   result := node ;
   CommandList := prepareNewNode (leftMsg , node.Id) ;  // will be freed by the thread

   node.AddValue (obj,Objtitle) ;
   node.fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end ;

//------------------------------------------------------------------------------

function TTraceNode.GetIndentLevel: integer;
var
   c : integer ;
   thId: DWORD ;
   cList : TObjectList ;
   NodeContext : TNodeContext ;
begin
   result := 0 ;
   if fWinTraceContext <> nil then
      cList := fWinTraceContext
   else if fContextList <> nil then
      cList := fContextList
   else
      exit ;

   // no need to lock an empty list
   if cList.Count = 0 then
      exit ;
   thId := GetCurrentThreadId() ;
   if fListCriticalSection = nil then
      fListCriticalSection := TCriticalSection.Create ;

   fListCriticalSection.Acquire ;
   try
      for c := 0 to cList.Count-1 do begin
         NodeContext := TNodeContext (cList.Items[c]) ;
         if NodeContext.ThreadId = thId then
            inc(result) ;
      end ;
   finally
      fListCriticalSection.Release ;
   end ;

end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.Indent (const leftMsg: string ; const RightMsg : string = ''; const BackGroundColor: TColor = clBlack ; const IsEnter:boolean=false) : ITraceNode;
var
   node : TTraceNode ;
   currentThreadId : integer ;
   LastContext , NewContext : TNodeContext ;
   CommandList : TStringList ;
   member : TMemberNode ;
begin
   if (fEnabled = false) then
      exit ;
   currentThreadId := GetCurrentThreadId() ;
   NewContext := TNodeContext.create ;
   NewContext.ThreadId := currentThreadId ;

   LastContext := getLastContext () ;
   NewContext.NodeId := TTrace.CreateTraceID ;
   CommandList := TStringList.create ;     // will be freed by the sender thread

   node := TTraceNode.create (self, false) ;
   node.id := NewContext.NodeId ;
   result := node ;

   if LastContext = nil then begin
      //NewContext.level := 1 ;
      addCommand (CommandList, CST_NEW_NODE, self.Id) ;              // param : parent Node id
   end else begin  // context already exist
      //NewContext.level := LastContext.level + 1 ;
      addCommand (CommandList, CST_NEW_NODE, LastContext.NodeId) ;   // param : parent Node id
   end ;

   addCommand (CommandList, CST_TRACE_ID, NewContext.NodeId) ;   // param : Node Id
   addCommand (CommandList, CST_LEFT_MSG, leftMsg);              // param : left string
   if RightMsg <> '' then
      addCommand (CommandList, CST_RIGHT_MSG, RightMsg);         // param : right string

   if BackGroundColor <> clBlack then
      addCommand (CommandList, CST_BACKGROUND_COLOR, BackGroundColor,'-1') ;      // param : color, colId

   if IsEnter then begin
      member := TMemberNode.create ;                     // create root member
      member.Add('').ViewerKind := CST_VIEWER_ENTER ;    // then add an empty member with special viewer
      Member.AddToStringList (CommandList) ;            // convert all groups and nested items/group to strings
      Member.Free ;
   end ;

   addCommand (CommandList, CST_ICO_INDEX, IconIndex) ;          // param : icon index
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);

   PushContext (NewContext) ;
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.IndentWithStack (const leftMsg: string ; const RightMsg : string = ''; const BackGroundColor: TColor = clBlack ; const IsEnter:boolean=false) : ITraceNode;
begin
   if (fEnabled = false) then
      exit ;
   Result := Indent(leftMsg,RightMsg,BackGroundColor,IsEnter) ;
   Result.AppendStack() ;
end;

//------------------------------------------------------------------------------

// ITraceNode
procedure TTraceNode.UnIndent (const leftMsg : string = '' ; const rightMsg : string = '';const BackGroundColor: TColor = clBlack ; const IsExit:boolean=false);
var
   CommandList : TStringList;
   pc : pchar ;
   leftMsg2, RightMsg2 : string ;
   member : TMemberNode ;
   nodeId : string ;
begin
   if (fEnabled = false) then
      exit ;

   deleteLastContext () ;
   if (leftMsg <> '') or (rightMsg <> '') then begin
      // Ajust leftMsg and rightMsg in case of bstr
      pc := Pchar (leftMsg) ;  leftMsg2 := pc ;
      pc := Pchar (rightMsg) ; rightMsg2 := pc ;

      nodeId := TTrace.CreateTraceID() ;

      CommandList := prepareNewNode (leftMsg2, nodeId) ;  // will be freed by the thread
      if rightMsg2 <> '' then
         addCommand (CommandList, CST_RIGHT_MSG,rightMsg2);    // param : right string

      if BackGroundColor <> clBlack then
         addCommand (CommandList, CST_BACKGROUND_COLOR, BackGroundColor,inttostr(-1)) ;      // param : color, colId

      if IsExit then begin
         member := TMemberNode.create ;                     // create root member
         member.Add('').ViewerKind := CST_VIEWER_EXIT ;     // then add an empty member with special viewer
         Member.AddToStringList (CommandList) ;            // convert all groups and nested items/group to strings
         Member.Free ;
      end ;
      TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
   end ;
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.EnterMethod(const leftMsg, rightMsg: string; const BackGroundColor: TColor) : ITraceNode;
begin
   Result := Self.Indent('Enter ' + LeftMsg, RightMsg, BackGroundColor, true);
end;

//------------------------------------------------------------------------------

// ITraceNode
procedure TTraceNode.ExitMethod(const leftMsg, rightMsg: string; const BackGroundColor: TColor);
begin
   Self.UnIndent('Exit ' + LeftMsg, RightMsg, BackGroundColor,true);
end;

//------------------------------------------------------------------------------

// ITraceNode

function TTraceNode.Resend (const leftMsg, RightMsg: string): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_RIGHT_MSG,RightMsg);      // param : right string
   addCommand (CommandList, CST_LEFT_MSG,leftMsg);        // param : left string
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.ResendLeft (const leftMsg: string): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_LEFT_MSG,leftMsg);        // param : left string
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.ResendRight (const RightMsg: string): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_RIGHT_MSG,RightMsg);      // param : right string
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.ResendIconIndex(const index: integer): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_ICO_INDEX,index);        // param : left string
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

// ITraceNode
function TTraceNode.AppendStack () : ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string

   // the stack trace is performed in the supplied StackTrace unit (use jedi code lib)
   if assigned (SendStackProc) then
      SendStackProc (self,1) ;

   if fMembers <> nil then
      fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;
//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.AppendLeft (const LeftMsg: string): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_APPEND_LEFT_MSG,leftMsg);        // param : left string

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.AppendRight (const RightMsg: string): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_APPEND_RIGHT_MSG,RightMsg);        // param : left string

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.Append (const LeftMsg, RightMsg: string): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_APPEND_RIGHT_MSG,RightMsg);      // param : right string
   addCommand (CommandList, CST_APPEND_LEFT_MSG,LeftMsg);        // param : left string

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.Show : ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be used') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_FOCUS_NODE,id);              // param : the node that receive focus
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.GotoNextSibling () : ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_NEXTSIBLING,id);
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.GotoPrevSibling () : ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_PREVSIBLING,id);
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.GotoFirstChild  () : ITraceNode;               // link to the node's first child...
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_FIRST_CHILD,id);
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.GotoLastChild   () : ITraceNode;               // link to the node's last child...
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_GOTO_LAST_CHILD,id);
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.SetBookmark (Bookmarked : boolean) : ITraceNode ;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node
   addCommand (CommandList, CST_SET_BOOKMARK2,Bookmarked); // param : enabled flag

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.SetVisible (visible: boolean) : ITraceNode ;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node
   addCommand (CommandList, CST_VISIBLE_NODE,visible);        // param : visible flag

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.Delete : ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be used') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLEAR_NODE,id);              // param : the node that must be deleted
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.DeleteChildren : ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be used') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_CLEAR_SUBNODES,id);              // param : the node to kill children
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.SetFontDetail(const ColId: integer; const Bold, Italic: boolean; const Color, Size: integer; const FontName: string): ITraceNode;
var
   CommandList : TStringList;
   TempStr : string ;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive font change
   TempStr := Format('%5d%3d', [CST_FONT_DETAIL,ColId]) ;

   if Bold then
      TempStr := TempStr + '1'
   else
      TempStr := TempStr + '0' ;

   if Italic then
      TempStr := TempStr + '1'
   else
      TempStr := TempStr + '0' ;

   TempStr := TempStr + Format('%11d%11d', [Color,Size]) + FontName  ;

   CommandList.add (TempStr);
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.SetBackgroundColor(const Color: TColor; const ColId: integer = -1): ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;
   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive font change
   addCommand (CommandList, CST_BACKGROUND_COLOR, Color,inttostr(ColId)) ;      // param : color, colId
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNode
function TTraceNode.SetSelected: ITraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be used') ;
   CommandList := TStringList.create ;  // will be freed by the thread
   addCommand (CommandList, CST_SELECT_NODE,id);              // param : the node that receive focus
   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
function TTraceNode.getLeftMsg: string;
begin
   result := fLeftMsg ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.setLeftMsg (const v: string);
begin
   fLeftMsg := v ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
function TTraceNode.getRightMsg: string;
begin
   result := fRightMsg ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.setRightMsg (const v: string);
begin
   fRightMsg := v ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
function TTraceNode.getThreadName: string;
begin
    result := fThreadName ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.setThreadName(const v: string);
begin
   fThreadName := v ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
function TTraceNode.getTime: TDateTime;
begin
   result := fTime ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.setTime(const v: TDateTime);
begin
   fTime := v ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
function TTraceNode.getParentNodeId: string;
begin
   result := fParentNodeId ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.setParentNodeId (const v: string);
begin
   fParentNodeId := v ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddStrings (const strings : TStrings);
var
   StringsGroup : TMemberNode ;
   i : integer ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create

   if (strings = nil) then begin
      fMembers.Add ('Null Object').ViewerKind := CST_VIEWER_TXT ;
      exit ;
   end ;

   StringsGroup := TMemberNode.create ('Strings').SetFontDetail(0,true) ;
   StringsGroup.ViewerKind := CST_VIEWER_TXT ;
   fMembers.Add (StringsGroup) ;

   for I := 0 to Strings.Count -1 do
      StringsGroup.add (intTostr (i) , Strings.strings[i]) ;  // col3 empty

end ;

//------------------------------------------------------------------------------

// called by all addTable flavors
procedure TTraceNode.innerAddFieldsToTable(TableMembers : TMemberNode ; CurrentRow : TMemberNode ; const AObject: TObject ; isFirstRow,isFirstCol : bool) ;
var

   I, Count: Integer;
   TypeInfo : PTypeInfo ;
   PropInfo: PPropInfo;
   TempList: PPropList;
   VariantPropValue : variant ;
   strType : String ;
   intPropValue : LongInt ;

   Prop_Name      : String ;
   Prop_Value     : string ;

   //------------------------------------------------------
   procedure AddToFieldGroup () ;
   begin
      if isFirstRow then begin
         if isFirstCol then
            TableMembers.Col1 := Prop_Name
         else
            TableMembers.Col1 := TableMembers.Col1 + #9 + Prop_Name;
      end;

      // add data
      if isFirstCol then
         CurrentRow.Col1 := prop_value
      else
         CurrentRow.Col1 := CurrentRow.Col1 + #9 + Prop_value;

      isFirstCol := false ;
   end;

begin

   if AObject = nil then
      exit ;

   TypeInfo := PTypeInfo(AObject.ClassInfo) ;
   if TypeInfo = nil then
      exit ;
   Count := GetPropList(typeInfo, TempList);   // if Count is zero , FreeMem(TempList) don't have to be called
   if Count > 0 then
   try

      for I := 0 to Count - 1 do begin
         PropInfo := TempList^[I];

         Prop_Name := String(PropInfo.Name) ;
         Prop_Value := '' ;

         //TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
         //  tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
         //  tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString,
         //  tkClassRef, tkPointer, tkProcedure {, tkMRecord});

         case PropInfo^.PropType^.Kind of
            tkInteger,tkString,tkWChar,tkLString,tkWString,tkInt64, tkUString :
               begin
                  if AObject <> nil then begin
                     VariantPropValue := GetPropValue(AObject, String(PropInfo.Name),true) ;
                     Prop_Value := VariantPropValue ;
                  end ;
                  AddToFieldGroup() ;
               end ;
            tkClassRef, tkPointer, tkProcedure {, tkMRecord} :
               begin
                  Prop_Value := '???' ;
               end;
            tkClass :
               begin
                  intPropValue := GetOrdProp(AObject, PropInfo) ;
                  if intPropValue = 0 then  // the property point to a nil TObject
                     Prop_Value := 'nil'
                  else begin
                     Prop_Value := intToStr (intPropValue) ;  // value is the adresse reference
                  end;
                  AddToFieldGroup() ;
               end ;
            tkMethod :begin end ;  // events
            else
               begin  // enumeration, ...
                  if AObject <> nil then begin
                     VariantPropValue := GetPropValue(AObject, string(PropInfo.Name),true) ;
                     Prop_Value := tt_GetVarValue (VariantPropValue,strType) ;
                  end ;
                  AddToFieldGroup() ;
               end ;
         end ;
      end;  // for each prop
   finally
      FreeMem(TempList);
   end;
end ;

//------------------------------------------------------------------------------

procedure TTraceNode.AddTable(strings: tStrings);
var
   TableMembers : TMemberNode ;
   CurrentRow : TMemberNode ;
   obj : TObject ;
   c : integer ;
   isFirstRow : bool ;
begin
   getMembers () ;  // ensure members array is create
   // create table
   TableMembers := fMembers.Add('Strings');
   TableMembers.ViewerKind := CST_VIEWER_TABLE;

   // col1 : text, col2 and follow : object properties
   isFirstRow := true;
   for c := 0 to Strings.Count -1 do begin
      obj := Strings.Objects[c] ;
      CurrentRow := TableMembers.add(Strings.strings[c]) ;
      innerAddFieldsToTable(TableMembers, CurrentRow, obj, isFirstRow,false) ;
      isFirstRow := false;
   end;
end;

//------------------------------------------------------------------------------

{$ifdef COMPILER_10_UP}
procedure TTraceNode.AddTable(strings: TWideStrings);
var
   TableMembers : TMemberNode ;
   CurrentRow : TMemberNode ;
   obj : TObject ;
   c : integer ;
   isFirstRow : bool ;
begin
   getMembers () ;  // ensure members array is create
   // create table
   TableMembers := fMembers.Add('Strings');
   TableMembers.ViewerKind := CST_VIEWER_TABLE;

   // col1 : text, col2 and follow : object properties
   isFirstRow := true;
   for c := 0 to Strings.Count -1 do begin
      obj := Strings.Objects[c] ;
      CurrentRow := TableMembers.add(Strings.strings[c]) ;
      innerAddFieldsToTable(TableMembers, CurrentRow, obj, isFirstRow,false) ;
      isFirstRow := false;
   end;
end;
{$endif COMPILER_10_UP}

{$ifdef COMPILER_12_UP}
procedure TTraceNode.AddTable(list: TEnumerableTObject);
var
   TableMembers : TMemberNode ;
   CurrentRow : TMemberNode ;
   isFirstRow : bool ;
   item : TObject ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   // create table
   TableMembers := fMembers.Add('');
   TableMembers.ViewerKind := CST_VIEWER_TABLE;

   isFirstRow := true;
   for item in list do
   begin
      CurrentRow := TableMembers.add('') ;
      innerAddFieldsToTable(TableMembers, CurrentRow, item, isFirstRow,true) ;
      isFirstRow := false;
      //ttrace.Debug.Send(item.classname) ;
   end;
end;
{$endif COMPILER_12_UP}



//------------------------------------------------------------------------------

procedure TTraceNode.AddTable(arr: array of TObject);
var
   TableMembers : TMemberNode ;
   CurrentRow : TMemberNode ;
   obj : TObject ;
   c : integer ;
   isFirstRow : bool ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   // create table
   TableMembers := fMembers.Add('');
   TableMembers.ViewerKind := CST_VIEWER_TABLE;

   isFirstRow := true;
   for c := 0 to length(arr) - 1 do begin
      obj := arr[c] ;
      CurrentRow := TableMembers.add('') ;
      innerAddFieldsToTable(TableMembers, CurrentRow, obj, isFirstRow,true) ;
      isFirstRow := false;
   end;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.AddTable(list: TList);
var
   TableMembers : TMemberNode ;
   CurrentRow : TMemberNode ;
   obj : TObject ;
   c : integer ;
   isFirstRow : bool ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   // create table
   TableMembers := fMembers.Add('');
   TableMembers.ViewerKind := CST_VIEWER_TABLE;

   isFirstRow := true;
   for c := 0 to List.Count - 1 do begin
      obj := List[c] ;
      CurrentRow := TableMembers.add('') ;
      innerAddFieldsToTable(TableMembers, CurrentRow, obj, isFirstRow,true) ;
      isFirstRow := false;
   end;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.AddTable(Collection: TCollection);
var
   TableMembers : TMemberNode ;
   CurrentRow : TMemberNode ;
   item : TCollectionItem ;
   c : integer ;
   isFirstRow : bool ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   // create table
   TableMembers := fMembers.Add('');
   TableMembers.ViewerKind := CST_VIEWER_TABLE;

   isFirstRow := true;
   for c := 0 to Collection.Count - 1 do begin
      item := Collection.Items[c] ;
      CurrentRow := TableMembers.add('') ;
      innerAddFieldsToTable(TableMembers, CurrentRow, item, isFirstRow,true) ;
      isFirstRow := false;
   end;
end;

//------------------------------------------------------------------------------
// ITraceNodeEx
// to do : create a procedure AddTable with the table and the commandList to bypass the copy member step
procedure TTraceNode.AddTable(table: ITraceTable);
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   // convert ITraceTable to members
   table.CopyToNodeMembers(fMembers); // copy member to node. Member viewer kind is already set
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure Array_AddValue (vElem : variant; UpperNode: TMemberNode; PreTitle : string) ;
var
   strValue, IndexStr : string ;
   strType : String ;
   DimCount, LowBound, HighBound,c,tempint : integer ;
   ArrayNode : TMemberNode ;
   index : array [0..49] of LongInt ;   // 50 dimensions maximum
   cd : integer ;  // current dimension
   ARR : psafearray ;
   ArrElement : OleVariant ;
begin
   // arrays is a special case, we need to display all fields
   if (VarType(vElem) and VT_ARRAY) = VT_ARRAY then begin
      // get the safearray
      ARR := tagVARIANT(vElem).parray ;

      DimCount  := VarArrayDimCount(vElem);
      IndexStr := '' ;
      for c := 0 to DimCount-1 do begin
         LowBound  := VarArrayLowBound (vElem,c+1) ;
         HighBound := VarArrayHighBound(vElem,c+1) ;
         index [c] := LowBound ;
         IndexStr := IndexStr + '[' + inttostr (LowBound) + '..' + inttostr (HighBound) + ']' ;
      end ;
      tempint := VarType(vElem) ;
      tempint := tempint xor VT_ARRAY ;
      ArrayNode := UpperNode.Add (PreTitle + 'Array of ' + String(TT_GetVariantType (tempint)), IndexStr ) ;

      cd := DimCount-1 ;
      while cd >= 0 do begin       // loop 1
         IndexStr := '' ;
         for c := 0 to DimCount-1 do begin
            IndexStr := IndexStr + '[' + inttostr (index[c]) + ']' ;
         end ;

         if tempint = varVariant then begin
            OleCheck(SafeArrayGetElement(ARR, index, ArrElement));
         end else begin
            SafeArrayGetElement(ARR, index, TVarData(ArrElement).VPointer) ;
            TVarData(ArrElement).VType := tempint;
         end ;

         if (VarType(ArrElement) and VT_ARRAY) = VT_ARRAY then begin
            // the element is an array : recursive call
            Array_AddValue (ArrElement , ArrayNode, IndexStr + ' : ') ;
         end else begin
            strValue := tt_GetVarValue (ArrElement, strType)  ;
            // print the indices , value, type
            ArrayNode.Add (IndexStr,strValue,string(strType)) ;
         end ;

         cd := DimCount-1 ;
         while cd >= 0 do begin    // loop 2
            inc (index [cd]) ;
            if index [cd] > VarArrayHighBound (vElem,cd+1) then begin
               // out of bound. Reset current dimension and try to increment upper dimension
               index [cd] := VarArrayLowBound (vElem,cd+1) ;
               dec (cd) ;
            end else begin      // current dimension is not out of bound
               break ;          // loop 2
            end ;
         end ;
      end ;
      exit ;  // if array : stop
   end ;

   strValue := tt_GetVarValue (vElem, strType)  ;
   UpperNode.Add (PreTitle+string(strType),strValue);

end ;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure Variant_AddValue (disp: IDispatch; UpperNode: TMemberNode; PreTitle : string) ;
var
   TypeInfo: ITypeInfo;
   intfName : WideString ;
   strTemp : string ;
   pc : Pchar ;

   TypeAttr: PTypeAttr;
   Result: HResult ;

   MemberTypeDesc: PTypeDesc ;    // member type description
   FuncDesc: PFuncDesc;
   VarDesc: PVarDesc;

   ObjectMembers : TMemberNode ;
   MemberValue : OleVariant ;
   MemberName: WideString;
   MemberTypeReal,MemberTypeDeclared : String ;
   strMembervalue : string ;

   //----------------------------------------------

   procedure GetProperty(Index: Integer; var Value: TVarData);
   var
      //Status: HResult;
      ExcepInfo: TExcepInfo;
      NullParams: TDISPPARAMS;
   begin
      with NullParams do begin
         rgvarg := nil;
         rgdispidNamedArgs := nil;
         cArgs := 0;
         cNamedArgs := 0;
      end;
      Value.VType := varEmpty;
      {Status :=} disp.Invoke(Index, GUID_NULL, 0,
               DISPATCH_PROPERTYGET, NullParams, @Value, @ExcepInfo, nil);
   end;

   //----------------------------------------------

   procedure AddFields ;
   var
      i : integer ;
   begin
      for I := 0 to TypeAttr.cVars - 1 do
      begin
         OleCheck(TypeInfo.GetVarDesc(I, VarDesc));
         try
            // get documentation for the field. Only member name is needed
            OleCheck(TypeInfo.GetDocumentation(VarDesc.memid, @MemberName, nil, nil, nil));

            try
               // get the field value and his real type
               VarClear(MemberValue);
               GetProperty(VarDesc.memid, TVarData(MemberValue));
               strMembervalue := tt_GetVarValue (MemberValue,MemberTypeReal) ;
            except
            end ;

            MemberTypeDesc := @VarDesc.elemdescVar.tdesc ;
            MemberTypeDeclared := tt_getVariantType (Typeinfo,MemberTypeDesc) ;

            if MemberTypeReal <> MemberTypeDeclared then begin
               if MemberTypeReal = 'Null' then
                  MemberTypeReal := MemberTypeDeclared
               else if MemberTypeReal = 'IDispatch' then
                  MemberTypeReal := MemberTypeDeclared
               else if MemberTypeDeclared <> 'HRESULT' then
                  MemberTypeReal := MemberTypeReal + ' (' + MemberTypeDeclared + ')' ;
            end ;

            ObjectMembers.Add(MemberName , strMemberValue , String(MemberTypeReal)) ;

         finally
            TypeInfo.ReleaseVarDesc(VarDesc);
         end;
      end;
   end ;

   //----------------------------------------------

   procedure AddProperties ;
   var
      i : integer ;
      memberNode : TMemberNode ;
   begin
      for I := 0 to TypeAttr.cFuncs - 1 do
      begin
         OleCheck(TypeInfo.GetFuncDesc(I, FuncDesc));
         try

            // check if the method is a get property
            if FuncDesc.invkind and INVOKE_PROPERTYGET <> 0 then begin

               // bypass function from IDispatch and IUnknown
               if FuncDesc.memid >= $60000000 then
                  continue ;

               // get documentation for the method. Only member name is needed
               OleCheck(TypeInfo.GetDocumentation(FuncDesc.memid, @MemberName, nil, nil, nil)); // @DocString, @helpcontext, @helpFile

               MemberTypeDesc := @FuncDesc.elemdescFunc.tdesc ;
               MemberTypeDeclared := tt_GetVariantType (Typeinfo,MemberTypeDesc) ;

               strMembervalue := '' ;
               MemberTypeReal := '' ;
               // get the property value and his real type
               VarClear(MemberValue);
               GetProperty(FuncDesc.memid, TVarData(MemberValue));
               strMembervalue := tt_GetVarValue (MemberValue,MemberTypeReal) ;

               if MemberTypeReal <> MemberTypeDeclared then begin
                  if MemberTypeReal = 'Null' then
                     MemberTypeReal := MemberTypeDeclared
                  else if MemberTypeReal = 'IDispatch' then
                     MemberTypeReal := MemberTypeDeclared
                  else if MemberTypeDeclared <> 'HRESULT' then
                     MemberTypeReal := MemberTypeReal + ' (' + MemberTypeDeclared + ')' ;
               end ;

               memberNode := TMemberNode.create (MemberName  , strMembervalue , String(MemberTypeReal)) ;
               ObjectMembers.Add (memberNode) ;
            end;
         finally
            TypeInfo.ReleaseFuncDesc(FuncDesc);
         end;
      end;   // next func
   end ;

   //----------------------------------------------

begin   // procedure Variant_AddValue (disp: IDispatch; UpperNode: TMemberNode; PreTitle : string) ;

   try
      Result := disp.GetTypeInfo(0,0,TypeInfo) ;
      if not Succeeded(Result) then
         TypeInfo := nil ;  // ensure TypeInfo don't contain bad result
   except
   end ;

   if TypeInfo = nil then begin
      UpperNode.Add ('No infos') ;
      Exit;
   end ;

   OleCheck(TypeInfo.GetTypeAttr(TypeAttr));
   try


      // get documenation for the interface (-1)
      OleCheck(TypeInfo.GetDocumentation(-1, @intfName, nil, nil, nil));

      // widestring to string, with possible BSTR problem
      strTemp := intfName ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      ObjectMembers  := TMemberNode.create (strTemp).SetFontDetail(0,true);
      ObjectMembers.ViewerKind := CST_VIEWER_OBJECT ;

      // add fields
      AddFields () ;
      // add properties
      AddProperties() ;

      if length (ObjectMembers.Members) <> 0 then
         UpperNode.Add (ObjectMembers)
      else
         ObjectMembers.free ;

   finally
      TypeInfo.ReleaseTypeAttr(TypeAttr);
   end;
end ;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddValue (const v: Variant; const ObjTitle : String = '');
var
   strType : String ;
   strValue : string ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create

   try
      if (VarType(v) and VT_ARRAY) = VT_ARRAY then
         Array_AddValue (v, fMembers, ObjTitle)
      else if VarType(V) = VT_DISPATCH then begin
         Variant_AddValue (IDispatch(TVarData(v).VDispatch) , fMembers ,ObjTitle) ;
      end else begin
         strValue := tt_GetVarValue (v , strType) ;
         fMembers.Add ('',strValue,String(strType)) ;             // add name, value , type
      end ;
      if length(fMembers.Members) > 0 then
         fMembers.Members[0].ViewerKind := CST_VIEWER_VALUE ;
   except
   end ;
end;

//------------------------------------------------------------------------------
// ITraceNodeEx
// add IDispatch
procedure TTraceNode.AddObject (const disp: IDispatch);
begin
   AddObject(disp,TTrace.Options.GetDefault);
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddObject (const disp: IDispatch; const flags: TraceDisplayFlags);
var
   TypeInfo: ITypeInfo;
   intfName : WideString ;

   MyITypeInfo2 : ITypeInfo2 ;
   TypeAttr: PTypeAttr;
   strTemp : string ;
   Result: HResult ;

   MemberTypeDesc: PTypeDesc ;    // member type description
   FuncDesc: PFuncDesc;
   VarDesc: PVarDesc;

   GeneralInfo, Fields, Properties, Methods  : TMemberNode ;
   MemberValue : OleVariant ;
   MemberName: WideString;
   MemberTypeReal,MemberTypeDeclared : String ;
   MemberDoc, MemberDocCR : string ;
   MemberFlags : string ;
   strMembervalue : string ;
   cNames   : integer;
   DocString, helpcontext,helpFile, HelpString, HelpStringContext, HelpStringDll : widestring ;
   pc : Pchar ;

   //----------------------------------------------

   procedure GetProperty(Index: Integer; var Value: TVarData);
   var
      //Status: HResult;
      ExcepInfo: TExcepInfo;
      NullParams: TDISPPARAMS;
   begin
      with NullParams do begin
         rgvarg := nil;
         rgdispidNamedArgs := nil;
         cArgs := 0;
         cNamedArgs := 0;
      end;
      Value.VType := varEmpty;
      {Status :=} disp.Invoke(Index, GUID_NULL, 0,
               DISPATCH_PROPERTYGET, NullParams, @Value, @ExcepInfo, nil);
   end;

   //----------------------------------------------

   procedure GetMemberDoc (MemId : integer) ;
   begin
      // get Help String for the field (from ITypeInfo2)
      MemberDoc := '' ;
      MemberDocCR := '' ;
      If succeeded(TypeInfo.QueryInterface(ITypeInfo2,MyITypeInfo2)) then
      begin
         MyITypeInfo2.GetDocumentation2(MemId ,0, @HelpString, nil,  nil);
         MemberDoc := HelpString ;
         pc := Pchar (MemberDoc) ;
         MemberDoc := pc ;
         if MemberDoc <> '' then
            MemberDocCR := #10 + MemberDoc ;
      end;
   end ;

   //----------------------------------------------

   procedure AddGeneralInfo ;
   var
      i : integer ;
      TypeInfoInterface : ITypeInfo ;
    begin
      if not (ShowClassInfo in flags) then
         exit ;

      // get documenation for the interface (-1)
      OleCheck(TypeInfo.GetDocumentation(-1, @intfName, @DocString, @helpcontext, @helpFile));
      If succeeded(TypeInfo.QueryInterface(ITypeInfo2,MyITypeInfo2)) then
         MyITypeInfo2.GetDocumentation2(-1 ,0, @HelpString, @HelpStringContext,  @HelpStringDll);

      GeneralInfo := TMemberNode.create ('General info').SetFontDetail(0,true) ;
      GeneralInfo.ViewerKind := CST_VIEWER_OBJECT ;

      Members.Add (GeneralInfo) ;

      // widestring to string, with possible BSTR problem
      strTemp := intfName ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      GeneralInfo.Add ('Interface name',strTemp ) ;

      for i := 0 to TypeAttr.cImplTypes -1 do begin
         OleCheck(TypeInfo.GetRefTypeInfo(i, TypeInfoInterface));
         OleCheck(TypeInfoInterface.GetDocumentation(MEMBERID_NIL, @intfName, nil,nil,nil));
         // widestring to string, with possible BSTR problem
         strTemp := intfName ;
         pc := Pchar (strTemp) ;
         strTemp := pc ;
         GeneralInfo.Add ('Implemented interface', strTemp ) ;
      end ;

      strTemp := DocString ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      if strTemp <> '' then
         GeneralInfo.Add ('DocString',strTemp ) ;

      if HelpString <> DocString then begin
         strTemp := HelpString ;
         pc := Pchar (strTemp) ;
         strTemp := pc ;
         if strTemp <> '' then
            GeneralInfo.Add ('HelpString',strTemp ) ;
      end ;

      strTemp := helpcontext ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      if strTemp <> '' then
         GeneralInfo.Add ('helpcontext',strTemp ) ;

      strTemp := helpFile ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      if strTemp <> '' then
         GeneralInfo.Add ('helpFile',strTemp ) ;

      strTemp := HelpStringContext ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      if strTemp <> '' then
         GeneralInfo.Add ('HelpStringContext',strTemp ) ;

      strTemp := HelpStringDll ;
      pc := Pchar (strTemp) ;
      strTemp := pc ;
      if strTemp <> '' then
         GeneralInfo.Add ('HelpStringDll',strTemp ) ;

   end ;

   //----------------------------------------------

   procedure AddFields ;
   var
      i : integer ;
   begin
      fields := TMemberNode.create ('Fields').SetFontDetail(0,true) ;
      fields.ViewerKind := CST_VIEWER_OBJECT ;

      for I := 0 to TypeAttr.cVars - 1 do
      begin
         OleCheck(TypeInfo.GetVarDesc(I, VarDesc));
         try
            // get documentation for the field. Only member name is needed
            OleCheck(TypeInfo.GetDocumentation(VarDesc.memid, @MemberName, nil, nil, nil));

            // get Help String for the field (from ITypeInfo2)
            GetMemberDoc (VarDesc.memid) ;

            try
               // get the field value and his real type
               VarClear(MemberValue);
               GetProperty(VarDesc.memid, TVarData(MemberValue));
               strMembervalue := tt_GetVarValue (MemberValue,MemberTypeReal) ;
            except
               on e : exception do begin end ;
            end ;

            MemberTypeDesc := @VarDesc.elemdescVar.tdesc ;
            MemberTypeDeclared := tt_getVariantType (Typeinfo,MemberTypeDesc) ;

            if MemberTypeReal = 'IDispatch' then MemberTypeReal := ''
            else if MemberTypeReal = 'Null' then MemberTypeReal := ''
            else if MemberTypeReal = MemberTypeDeclared then MemberTypeReal := ''
            else MemberTypeReal := ' {' + MemberTypeReal + '}' ;

            MemberFlags := '' ;
            if (VarDesc.wVarFlags and VARFLAG_FREADONLY        <> 0 ) then MemberFlags := MemberFlags + '{Read Only} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FSOURCE          <> 0 ) then MemberFlags := MemberFlags + '{Source} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FBINDABLE        <> 0 ) then MemberFlags := MemberFlags + '{Bindable} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FREQUESTEDIT     <> 0 ) then MemberFlags := MemberFlags + '{Request Edit} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FDISPLAYBIND     <> 0 ) then MemberFlags := MemberFlags + '{Display Bind} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FDEFAULTBIND     <> 0 ) then MemberFlags := MemberFlags + '{Default Bind} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FHIDDEN          <> 0 ) then MemberFlags := MemberFlags + '{Hidden} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FRESTRICTED      <> 0 ) then MemberFlags := MemberFlags + '{Restricted} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FDEFAULTCOLLELEM <> 0 ) then MemberFlags := MemberFlags + '{Default Coll Elem} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FUIDEFAULT       <> 0 ) then MemberFlags := MemberFlags + '{UI Default} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FNONBROWSABLE    <> 0 ) then MemberFlags := MemberFlags + '{Non Browsable} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FREPLACEABLE     <> 0 ) then MemberFlags := MemberFlags + '{Replaceable} ' ;
            if (VarDesc.wVarFlags and VARFLAG_FIMMEDIATEBIND   <> 0 ) then MemberFlags := MemberFlags + '{Immediate Bind} ' ;

            fields.Add('[' + inttostr (VarDesc.memid) + '] ' + MemberName + ' ' + MemberFlags,
                       strMemberValue + String(MemberTypeReal),
                       String(MemberTypeDeclared) + MemberDoc) ;

         finally
            TypeInfo.ReleaseVarDesc(VarDesc);
         end;
      end;
   end ;

   //----------------------------------------------

   procedure AddProperty ;
   var
      memberNode : TMemberNode ;
      memberfound : boolean ;
      j : integer ;
      PosCR : integer ;
      PreviousCol3 : string ;
      PreviousMemberType : String ;  // Property type and documentation
   begin
      MemberFlags := '' ;
      strMembervalue := '' ;
      MemberTypeReal := '' ;
      if (FuncDesc.invkind and INVOKE_PROPERTYGET <> 0 ) then begin
         try
            // get the property value and his real type
            VarClear(MemberValue);
            GetProperty(FuncDesc.memid, TVarData(MemberValue));
            strMembervalue := tt_GetVarValue (MemberValue,MemberTypeReal) ;

            if MemberTypeReal = 'IDispatch' then MemberTypeReal := ''
            else if MemberTypeReal = 'Null' then MemberTypeReal := ''
            else if MemberTypeReal = MemberTypeDeclared then MemberTypeReal := ''
            else MemberTypeReal := ' {' + MemberTypeReal + '}' ;

         except
            on e : exception do begin end ;
         end ;
         MemberFlags := MemberFlags + ' {get}' ;
      end ;

      if (FuncDesc.invkind and INVOKE_PROPERTYPUT    <> 0 ) then
         MemberFlags := MemberFlags + ' {put}' ;
      if (FuncDesc.invkind and INVOKE_PROPERTYPUTREF <> 0 ) then
         MemberFlags := MemberFlags + ' {put ref}' ;

      // first function parameter is the member type
      if FuncDesc.cParams > 0 then begin
         MemberTypeDeclared := tt_GetVariantType(Typeinfo,@funcdesc.lprgelemdescParam[0].tdesc) ;
      end ;

      // search for previous node with the same Id AND same declared value type
      memberfound := false ;
      For j := 0 to length(properties.Members) -1 do begin
         memberNode := properties.Members [j] ;
         if memberNode.Tag = FuncDesc.memid then begin
            PreviousCol3 := memberNode.Col3;  // Property type and documentation
            PosCR := pos (#10, PreviousCol3) ;
            if PosCR = 0 then // no doc
               PreviousMemberType := PreviousCol3
            else
               PreviousMemberType := copy (PreviousCol3,1 , posCR-1) ;

            if PreviousMemberType = MemberTypeDeclared then begin
               memberfound := true ;
               memberNode.Col1 := memberNode.Col1 + MemberFlags ;  // append new member flags
               memberNode.Col2 := memberNode.Col2 + strMembervalue + String(MemberTypeReal) ; // add value if get is after the set
               break ;
            end ;
         end ;
      end ;

      if memberfound = false then begin
         memberNode := TMemberNode.create ('[' + inttostr (FuncDesc.memid) + '] ' + MemberName + MemberFlags ,
                        strMembervalue + String(MemberTypeReal), string(MemberTypeDeclared) + MemberDocCR) ;
         memberNode.tag := FuncDesc.memid ;    // save member ID for later retreival
         properties.Add (memberNode) ;
      end ;
   end ;

   //----------------------------------------------

   procedure AddMethod ;
   var
      j : integer ;
      ParamName : WideString ;
      FuncParametersNames : array[0..30] of WideString;    // function parameters names
      ParamTypeDesc : PTypeDesc;    // param type description
      ParamTypeDeclared : String ;
      ParamElemDesc : TElemDesc ;
      lParamFlags: SmallInt;
      strParamflags : string;
      lParamDefaultValue: OLEVariant;
   begin
      // retrieve function parameters names
      TypeInfo.GetNames(funcdesc.memid,@FuncParametersNames,30,cNames);

      MemberName := MemberName + ' (' ;
      for j := 0 to FuncDesc.cParams-1 do
      begin
         ParamElemDesc := funcdesc.lprgelemdescParam[j] ;
         ParamTypeDesc := @ParamElemDesc.tdesc ;    // get TTypeDesc
         // get param name (the more easy task)
         ParamName := FuncParametersNames[j+1];
         // get param type
         ParamTypeDeclared := tt_GetVariantType(Typeinfo,ParamTypeDesc) ;

         // lParamFlags tells whether a parameter is input/output/default values/optional, etc
         lParamFlags := ParamElemDesc.paramdesc.wParamFlags;
         strParamflags := '' ;
         // Check if Param has a default value
         if (lParamFlags and PARAMFLAG_FHASDEFAULT ) <> 0 then begin
            lParamDefaultValue := OLEVariant( ParamElemDesc.paramdesc.pparamdescex.varDefaultValue);
            strParamflags := strParamflags + '[Default = ' + tt_GetVarValue (lParamDefaultValue) + ']' ;
         end;

         if (lParamFlags and PARAMFLAG_FIN    ) <> 0 then strParamflags := '[IN] ' + strParamflags ;
         if (lParamFlags and PARAMFLAG_FOUT   ) <> 0 then strParamflags := '[OUT] ' + strParamflags ;
         if (lParamFlags and PARAMFLAG_FLCID  ) <> 0 then strParamflags := '[LCID] ' + strParamflags ;
         if (lParamFlags and PARAMFLAG_FRETVAL) <> 0 then strParamflags := '[RETVAL] ' + strParamflags ;
         if (lParamFlags and PARAMFLAG_FOPT   ) <> 0 then strParamflags := '[OPT] ' + strParamflags ;

         if j = 0 then
            MemberName := MemberName + strParamflags + String(ParamTypeDeclared) + ' ' + ParamName
         else
            MemberName := MemberName + ', ' + strParamflags + String(ParamTypeDeclared) + ' ' + ParamName ;

      end;
      MemberName := MemberName + ')' ;
      methods.Add('[' + inttostr (FuncDesc.memid) + '] ' + String(MemberTypeDeclared) , MemberName , Memberdoc) ;
   end ;

   //----------------------------------------------

   //function GetInterfaceForFunction (const inTypeinfo: ITypeInfo;
   //                                  inMemid:TDispID;
   //                                  href: cardinal;
   //                                  var IsGenericBaseInterface: Boolean): string;
   //var
   //LTypeInfoRef  : ITypeInfo;
   //InterfaceName : WideString;
   //ARefType      : HRefType;
   //begin
   //  InterfaceName := '' ;
   //  IsGenericBaseInterface := TRUE;
   //  Case (inMemId) of
   //    // Méthode pour trouver Interface des fonctions héritées IUnknown,IDispatch (qui sont répétées dans tlb)
   //    1610612736,1610612737,1610612738 :
   //       begin
   //          InterfaceName:='IUnknown'; // IUnkn. QI, Addref, release,
   //       end ;
   //    1610678272,1610678273,1610678274,1610678275 :
   //       begin
   //          InterfaceName:='IDispatch'; // ...
   //       end ;
   //    else
   //    begin
   //      if succeeded(inTypeinfo.GetRefTypeInfo(href,LTypeInfoRef)) then begin
   //         LTypeInfoRef.GetDocumentation(-1,@InterfaceName,nil,nil,nil);
   //      end ;
   //      IsGenericBaseInterface:=False;
   //    end;
   //  end;
   //  Result:=InterfaceName;
   //end;


   procedure AddMethodsAndProperties ;
   var
      i : integer ;
      //InheritStr : string ;
      //isGenericInterface: Boolean ;
   begin
      Properties  := TMemberNode.create ('Properties').SetFontDetail(0,true) ;
      Properties.ViewerKind := CST_VIEWER_OBJECT ;

      methods     := TMemberNode.create ('Methods').SetFontDetail(0,true);
      Properties.ViewerKind := CST_VIEWER_OBJECT ;

      for I := 0 to TypeAttr.cFuncs - 1 do
      begin
         OleCheck(TypeInfo.GetFuncDesc(I, FuncDesc));
         try try
            // bypass function from IDispatch and IUnknown
            if FuncDesc.memid >= $60000000 then
               continue ;

            // get documentation for the method. Only member name is needed
            OleCheck(TypeInfo.GetDocumentation(FuncDesc.memid, @MemberName, nil, nil, nil)); // @DocString, @helpcontext, @helpFile

            //if (FuncDesc.elemdescFunc.tdesc.vt=VT_PTR) then
            //   InheritStr:=GetInterfaceForFunction(TypeInfo,FuncDesc.memid,FuncDesc.elemdescFunc.tdesc.ptdesc.hreftype,isGenericInterface)
            //else
            //   InheritStr:=GetInterfaceForFunction(TypeInfo,FuncDesc.memid,FuncDesc.elemdescFunc.tdesc.hreftype,isGenericInterface);
            //MemberName := InheritStr + ':' + MemberName ;

            // get Help String for the method (from ITypeInfo2)
            GetMemberDoc (FuncDesc.memid) ;

            MemberTypeDesc := @FuncDesc.elemdescFunc.tdesc ;
            MemberTypeDeclared := tt_GetVariantType (Typeinfo,MemberTypeDesc) ;

            // check if the method is a property
            if ((FuncDesc.invkind and INVOKE_PROPERTYGET    <> 0 ) or
                (FuncDesc.invkind and INVOKE_PROPERTYPUT    <> 0 ) or
                (FuncDesc.invkind and INVOKE_PROPERTYPUTREF <> 0 )) then begin
               AddProperty () ;
            end else if ShowMethods in flags then begin  // METHOD
               AddMethod ();
            end;
         except
            on e : exception do begin end ;
         end finally
            TypeInfo.ReleaseFuncDesc(FuncDesc);
         end;
      end;
   end ;

   //----------------------------------------------

begin   // procedure AddObject

   if (Enabled = false) then
      exit ;
   getMembers ;
   try
      Result := disp.GetTypeInfo(0,0,TypeInfo) ;
      if not Succeeded(Result) then
         TypeInfo := nil ;  // ensure TypeInfo don't contain bad result
   except
   end ;

   if TypeInfo = nil then begin
      members.Add ('No infos') ;
      Exit;
   end ;

   OleCheck(TypeInfo.GetTypeAttr(TypeAttr));
   try
      // add general info (interface name, help strings, ...)
      AddGeneralInfo ();

      // add fields
      AddFields () ;
      if length (Fields.Members) <> 0 then
         Members.Add (Fields)
      else
         Fields.free ;

      // add Methods and properties
      AddMethodsAndProperties() ;
      if length (Properties.Members) <> 0 then
         Members.Add (Properties)
      else
         Properties.free ;

      if length (methods.Members) <> 0 then
         Members.Add (methods)
      else
         methods.free ;

   finally
      TypeInfo.ReleaseTypeAttr(TypeAttr);
   end;
end;

//------------------------------------------------------------------------------
// ITraceNodeEx
// add variant

{$IFDEF DELPHI_7_UP}
procedure TTraceNode.AddObject (const v: variant);
begin
   AddObject(v,TTrace.Options.GetDefault);
end;

{$Else}

procedure TTraceNode.AddObjectV (const v: variant);
begin
   AddObjectV (v,TTrace.Options.GetDefault);
end;
{$ENDIF}

//------------------------------------------------------------------------------

// ITraceNodeEx
{$IFDEF DELPHI_7_UP}
procedure TTraceNode.AddObject (const v: variant; const flags: TraceDisplayFlags);
var
   strType : String ;
   strValue : string ;
begin
   if (Enabled = false) then
      exit ;
   if VarType(V) = VT_DISPATCH then begin
      AddObject (IDispatch(TVarData(v).VDispatch)) // add idispatch
   end else begin
      getMembers ;
      strValue := tt_GetVarValue (v , strType) ;
      Members.Add (String(strType),strValue) ;             // add type and value
   end ;
end;

{$else}

procedure TTraceNode.AddObjectV (const v: variant; const flags: TraceDisplayFlags);
var
   strType : string ;
   strValue : string ;
begin
   if (Enabled = false) then
      exit ;
   if VarType(V) = VT_DISPATCH then begin
      AddObjectV (IDispatch(TVarData(v).VDispatch)) // add idispatch
   end else begin
      getMembers ;
      strValue := tt_GetVarValue (v , strType) ;
      Members.Add (strType,strValue) ;             // add type and value
   end ;
end;
{$ENDIF}

//------------------------------------------------------------------------------

// ITraceNodeEx
// add TObject
procedure TTraceNode.AddObject (const Obj: TObject);
begin
   AddObject(Obj,TTrace.Options.GetDefault);
end ;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddObject (const Obj: TObject; const flags : TraceDisplayFlags);
var
   Collection : TCollection ;
   CollectionItemClass : TCollectionItemClass ;
   ObjectGroup : TMemberNode ;
   ErrorGroup : TMemberNode ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create

   if (Obj = nil) then begin
      fMembers.Add ('Null Object').ViewerKind := CST_VIEWER_OBJECT ;
      exit ;
   end ;

   // detect recursive call to SendObject when DisplayFields try to get fields values
   if SendObjectRecursiveStatus > 0 then begin
      ErrorGroup := TMemberNode.create ('Recursive call detected') ;
      ErrorGroup.ViewerKind := CST_VIEWER_OBJECT ;
      ErrorGroup.add ('Object adress' , '$' + inttohex (integer(obj),2) ) ;
      ErrorGroup.add ('Class Name' , obj.className ) ;
      ErrorGroup.add ('Error : A Field Read method call AddObject().') ;
      ErrorGroup.add ('->Possible recursive call is stopped now') ;
      fMembers.add (ErrorGroup) ;
      SendObjectRecursiveStatus := 2 ; // tell the calling SendObject that a recursive call was stopped
      exit ;
   end ;

   SendObjectRecursiveStatus := 1 ;

   try

      // no quick info to display (Object.ToString)
      //-------------------------------------------

      // AddValue(obj);

      // Class information
      // ------------------

      if ShowClassInfo in flags then begin
         ObjectGroup := TMemberNode.create ('Class information').SetFontDetail(0,true) ;
         ObjectGroup.ViewerKind := CST_VIEWER_OBJECT ;
         fMembers.Add(ObjectGroup) ;

         ObjectGroup.add ('Object adress' , '$' + inttohex (integer(obj),2) ) ;
         ObjectGroup.add ('Class Name' , obj.className ) ;

         if obj is TComponent then
            ObjectGroup.add ('TComponent name' , TComponent(obj).name ) ;

         if obj is TMenu then
            ObjectGroup.add ('Is TMenu' , 'True' ) ;

         if obj is TMenuItem then
            ObjectGroup.add ('Is TMenu Item' , 'True' ) ;

         if OBJ is TCollection then begin
            Collection := TCollection (obj) ;
            CollectionItemClass := Collection.ItemClass ;
            ObjectGroup.add ('Is TCollection' , 'ItemClass' , CollectionItemClass.ClassName) ;
         end ;
      end ;

      // no type information for that object.
      if PTypeInfo(obj.ClassInfo) = nil then begin
         ErrorGroup := TMemberNode.create ('No Type information for that object') ;
         ErrorGroup.ViewerKind := CST_VIEWER_OBJECT ;
         ErrorGroup.Add('Derive your object from TPersistent or') ;
         ErrorGroup.Add('use {$M+} and {$M-} directives (see TPersistent)') ;
         fMembers.add (ErrorGroup) ;
         exit ;
      end ;

      // fields and events
      // -----------------
      DisplayFields (obj, flags);

      // functions
      //-----------
      if ShowMethods in flags then
         DisplayMethods (obj, flags) ;

      // inherit classes and interfaces
      //-------------------------------
      if ShowClassInfo in flags then
         DisplayBases (obj, flags) ;

      if SendObjectRecursiveStatus = 2 then begin
         ErrorGroup := TMemberNode.create ('Warning : One or more Field Read method called AddObject') ;
         ErrorGroup.ViewerKind := CST_VIEWER_OBJECT ;
         ErrorGroup.Add('->Possible recursive call was stopped') ;
         fMembers.add (ErrorGroup) ;
      end ;

   finally
      SendObjectRecursiveStatus := 0 ;
   end ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddValue (const Obj: TObject; const ObjTitle: String);
begin
   AddValue (obj, ObjTitle, ttrace.options.objectTreeDepth) ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddValue (const Obj: TObject; const ObjTitle: String; const MaxLevel: integer);
var
   AlreadyParsedObject : TObjectList ;
begin
   AlreadyParsedObject := TObjectList.Create(false) ;    // not owner
   AddValue(Obj,ObjTitle,MaxLevel,AlreadyParsedObject);
   AlreadyParsedObject.Clear ;
   AlreadyParsedObject.Free ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
{Private}
procedure TTraceNode.AddValue (const Obj: TObject; const ObjTitle: String; const MaxLevel: integer; AlreadyParsedObject: TObjectList);
var
   topMember : TMemberNode ;
   ErrorGroup : TMemberNode ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create

   if (Obj = nil) then begin
      fMembers.Add ('Null Object').ViewerKind := CST_VIEWER_VALUE ;
      exit ;
   end ;

   // detect recursive call to SendObject when DisplayFields try to get fields values
   if {threadvar} SendObjectRecursiveStatus > 0 then begin
      ErrorGroup := TMemberNode.create ('Recursive call detected') ;
      ErrorGroup.ViewerKind := CST_VIEWER_VALUE ;
      ErrorGroup.add ('Object adress' , '$' + inttohex (integer(obj),2) ) ;
      ErrorGroup.add ('Class Name' , obj.className ) ;
      ErrorGroup.add ('Error : A Field Read method call AddObject().') ;
      ErrorGroup.add ('->Possible recursive call is stopped now') ;
      fMembers.add (ErrorGroup) ;
      SendObjectRecursiveStatus := 2 ; // tell the calling SendObject that a recursive call was stopped
      exit ;
   end ;

   SendObjectRecursiveStatus := 1 ;

   try
      // Create the top node using only title.
      // Value (col2) and Type (col3) will be added by inner_addValue
      topMember := fMembers.add (ObjTitle) ;
      topMember.ViewerKind := CST_VIEWER_VALUE ;
      inner_addValue (obj,topMember,MaxLevel,AlreadyParsedObject);
   finally
      SendObjectRecursiveStatus := 0 ;
   end ;
end;

//type
//  TAccessStyle = (asFieldData, asAccessor, asIndexedAccessor);
//
////------------------------------------------------------------------------------
//function GetAccessToProperty(Instance: TObject; PropInfo: PPropInfo;
//  AccessorProc: Longint; out FieldData: Pointer;
//  out Accessor: TMethod): TAccessStyle;
//begin
//  if (AccessorProc and $FF000000) = $FF000000 then
//  begin  // field - Getter is the field's offset in the instance data
//    FieldData := Pointer(Integer(Instance) + (AccessorProc and $00FFFFFF));
//    Result := asFieldData;
//  end
//  else
//  begin
//    if (AccessorProc and $FF000000) = $FE000000 then
//      // virtual method  - Getter is a signed 2 byte integer VMT offset
//      Accessor.Code := Pointer(PInteger(PInteger(Instance)^ + SmallInt(AccessorProc))^)
//    else
//      // static method - Getter is the actual address
//      Accessor.Code := Pointer(AccessorProc);
//
//    Accessor.Data := Instance;
//    if PropInfo^.Index = Integer($80000000) then  // no index
//      Result := asAccessor
//    else
//      Result := asIndexedAccessor;
//  end;
//end;
//
//function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
//type
//  { Need a(ny) dynamic array type to force correct call setup.
//    (Address of result passed in EDX) }
//  TDynamicArray = array of Byte;
//type
//  TDynArrayGetProc = function: TDynamicArray of object;
//  TDynArrayIndexedGetProc = function (Index: Integer): TDynamicArray of object;
//var
//  M: TMethod;
//begin
//  case GetAccessToProperty(Instance, PropInfo, Longint(PropInfo^.GetProc), Result, M) of
//
//    asFieldData:
//      Result := PPointer(Result)^;  // null
//
//    asAccessor:
//      Result := Pointer(TDynArrayGetProc(M)());
//
//    asIndexedAccessor:
//      Result := Pointer(TDynArrayIndexedGetProc(M)(PropInfo^.Index));
//
//  end;
//end;


// ITraceNodeEx

// Private
// append type name to upper node (col3)
// write value to upper node (col2)
// insert dictionary or arry in sub nodes (C# or java)
// insert fields and prpperties

procedure TTraceNode.inner_addValue (const AObject: TObject; const upperNode : TMemberNode; const MaxLevel : integer; AlreadyParsedObject:TObjectList);
var
   subObj : TObject ;
   I,J,K, PropCount: Integer;
   TypeInfo : PTypeInfo ;
   PropInfo: PPropInfo;
   TempList: PPropList;
   VariantPropValue : variant ;
   strType : String ;
   intPropValue : LongInt ;
   ArrayNode, fieldNode : TMemberNode ;

   Prop_Name      : String ;
   Prop_Type      : String ;
   Prop_Value     : string ;
   //Prop_ClassType : String ;

   DynArrayPointer: Pointer;
   DynArrayObject: TDynArray;
   DynArrayElementPointer : Pointer ;
   DynArrayElementValue1 : byte ;
   DynArrayElementValue2 : word ;
   DynArrayElementValue4 : int32 ;     
   TypeInfoName : RawUTF8 ;  // AnsiString
   ClassTypeFound : boolean ;
   
type
   PPPTypeInfo = ^PPTypeInfo;

begin
   if AObject = nil then begin
      upperNode.Col2 := 'nil' ;
      exit ;
   end ;

   // append type name to upper node (col3)
   //upperNode.Col3 := upperNode.Col3 + AObject.className ;

   if AlreadyParsedObject.IndexOf (AObject) <> -1 then begin
      upperNode.Col2 := 'see ' + AObject.className + '@' + inttohex (integer(AObject),2) ;
      exit ;
   end ;

   // write value to upper node (col2)
   upperNode.Col2 := AObject.className + '@' + inttohex (integer(AObject),2) ;

   // max level reached
   if MaxLevel <= 1 then
      exit ;

   // no more display object content
   AlreadyParsedObject.Add (AObject) ;   // not owner

   TypeInfo := PTypeInfo(AObject.ClassInfo) ;
   if TypeInfo = nil then
      exit ;
   PropCount := GetPropList(typeInfo, TempList);   // if Count is zero , FreeMem(TempList) don't have to be called
   if PropCount > 0 then
   try
      for I := 0 to PropCount - 1 do begin
         PropInfo := TempList^[I];
         Prop_Name := String(PropInfo.Name) ;

         case PropInfo^.PropType^.Kind of
            tkInteger,tkString,tkChar,tkWChar,tkLString,tkWString,tkInt64,tkUString,tkFloat :
               begin
                   VariantPropValue := GetPropValue(AObject, string(PropInfo.Name),true) ;
                   Prop_Value := VariantPropValue ;
                   upperNode.Add( string(prop_name) , prop_value) ;   // no type
               end ;
               
            tkClass  :
               begin
                  fieldNode := upperNode.Add( string(prop_name)) ;   // col2 (value) will be added by recursion
                  intPropValue := GetOrdProp(AObject, PropInfo) ;    // property value can be nil
                  subObj := TObject (intPropValue);  
                  inner_addValue (subObj ,fieldNode , MaxLevel-1, AlreadyParsedObject);   // recursion.
               end ;
               
            tkClassRef, tkPointer, tkProcedure, tkMethod {, tkMRecord} :
               begin

               end;
              
            tkDynArray :   // tkArray
               begin                                                                 
                  DynArrayPointer := GetDynArrayProp(AObject, PropInfo);  // from System.TypInfo => GetPropValue where kind = tkDynArray 
                  TypeInfoName := TypeInfoToName(PropInfo^.PropType^) ;
                 
                  DynArrayObject.Init(PropInfo^.PropType^,DynArrayPointer) ;     // aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);
                  prop_type := string(TypeInfoName) + ' : ' + GetPropertyTypeString (PropInfo^.PropType^.Kind) + '[' + IntToStr(DynArrayObject.Count) + ']' + ', element size = ' + IntToStr(DynArrayObject.ElemSize) ;
                  Prop_Value := '@' + inttohex (Integer(DynArrayPointer),2) ; 
                  subObj := nil ;

                  ArrayNode := upperNode.Add(prop_name , prop_value, prop_type) ; 
                  
                  ClassTypeFound := false ;
                  for j := 0 to DynArrayObject.Count-1 do
                  begin
                     DynArrayElementPointer := DynArrayObject.ElemPtr(j) ;
                     case DynArrayObject.ElemSize of
                        1 : begin
                            DynArrayElementValue1 := byte(DynArrayElementPointer^) ;               // 0 .. FF
                            Prop_Value := '$' + inttohex (DynArrayElementValue1,2) ;
                            ArrayNode.Add('[' + inttostr(j) + ']' , prop_value) ;       // no recursion
                        end ;
                        2 : begin
                            DynArrayElementValue2 := word(DynArrayElementPointer^) ;               // 0.. FF FF
                            Prop_Value := '$' + inttohex (DynArrayElementValue2,4) ;
                            ArrayNode.Add('[' + inttostr(j) + ']' , prop_value) ;
                        end ;
                        SizeOf(TObject) : begin
                            DynArrayElementValue4 := int32(DynArrayElementPointer^) ;              // 0 .. FF FF FF FF
                            // is it an integer or a pointer or a 4 bytes value ?      
                            try                         
                               subObj := TObject (DynArrayElementValue4);
                               Prop_Value := '@' + inttohex (DynArrayElementValue4,8) ;
                               if (ClassTypeFound = false) and (DynArrayElementValue4 <> 0) then begin
                                  // if debugger stop here, ignore and continue
                                  ArrayNode.Col3 := ArrayNode.Col3 + ', class type = ' + subObj.ClassName ;
                                  ClassTypeFound := True;
                               end;
                            except 
                               on e : Exception do begin // eat exception. Element is not a class instance. Is it a pointer or a 4 bytes value like an int32?
                                  Prop_Value := '$' + inttohex (DynArrayElementValue4,8) ;
                                  subObj := nil ;                              
                               end;
                            end;                                              
                          
                            fieldNode := ArrayNode.Add('[' + inttostr(j) + ']' , prop_value) ;
                            if subObj <> nil then                  
                               inner_addValue  (subObj ,fieldNode , MaxLevel-1, AlreadyParsedObject);   // Recursive                          
                        end ;
                        else begin    // 3 bytes or more than 4 bytes
                            Prop_Value := 'Dump :' ;
                            for k := 0 to DynArrayObject.ElemSize -1 do begin
                               DynArrayElementValue1 := byte(DynArrayElementPointer^) ;               // 0 .. FF
                               Prop_Value := Prop_Value + ' $' + inttohex (DynArrayElementValue1,2) ;
                               DynArrayElementPointer := pbyte(DynArrayElementPointer) + 1;
                            end;
                            ArrayNode.Add('[' + inttostr(j) + ']' , prop_value) ;
                        end ;
                     end;                        
                  end;                   
               end;
               
            else
               begin
                  // tkSet 
                  // tkEnumeration
                  // tkUnknown
                  // tkVariant
                  // tkRecord
                  // tkInterface
                  
                  VariantPropValue := GetPropValue(AObject,  string(PropInfo.Name),true) ;
                  Prop_Value := tt_GetVarValue (VariantPropValue,strType) ;

                  upperNode.Add(String(prop_name) , prop_value) ;
               end ;
         end ;
      end;  // for each prop
   finally
      FreeMem(TempList);
   end;
end ;

//------------------------------------------------------------------------------

// ITraceNodeEx

// called by AddObject
procedure TTraceNode.DisplayBases (const AObject: TObject; const flags : TraceDisplayFlags);
var
   Group : TMemberNode ;
   ClassPtr: TClass;
   IntfTable: PInterfaceTable;
   Entry : PInterfaceEntry ;
   I: Integer;
   interfacesNames : string ;
begin
   Group := nil ;
   ClassPtr := AObject.ClassType;
   while ClassPtr <> nil do begin
      if classPtr = TObject then
         break ;

      interfacesNames := '' ;
      IntfTable := ClassPtr.GetInterfaceTable;

      if IntfTable <> nil then begin
          for I := 0 to IntfTable.EntryCount-1 do begin
            Entry := @IntfTable.Entries[I];
             if (interfacesNames = '') then
               interfacesNames := GUIDToString(Entry.IID)
            else
               interfacesNames := interfacesNames + ',' + GUIDToString(Entry.IID) ;
          end;
      end ;

      if Group = nil then begin
          Group := TMemberNode.create ('Classes and interfaces').SetFontDetail(0,true) ;
          Group.ViewerKind := CST_VIEWER_OBJECT ;
          fMembers.Add(Group) ;
      end ;
      Group.add (ClassPtr.ClassName, interfacesNames) ;

      ClassPtr := ClassPtr.ClassParent;
   end;
end ;

//------------------------------------------------------------------------------

// ITraceNodeEx

procedure TTraceNode.DisplayMethods (const AObject: TObject; const flags : TraceDisplayFlags);
var
   MethodGroup : TMemberNode ;
   VMT: Pointer;
   MethodInfo: Pointer;
   Count: Integer;
   r : PMethodInfoHeader ;
   strMethodInfo : string ;
begin
   MethodGroup := nil ;
   VMT := PPointer(AObject)^;
   repeat
      MethodInfo := PPointer(Integer(VMT) + vmtMethodTable)^;
      if MethodInfo <> nil then
      begin
         // Scan method table for the method
         Count := PWord(MethodInfo)^;
         Inc(Integer(MethodInfo), 2);
         while Count > 0 do
         begin
            r := MethodInfo;
            // add the method
            if MethodGroup = nil then begin
               MethodGroup := TMemberNode.create ('Methods').SetFontDetail(0,true) ;
               MethodGroup.ViewerKind := CST_VIEWER_OBJECT ;
               fMembers.Add(MethodGroup) ;
            end ;
            if assigned (SendMethodProc) then
               strMethodInfo := SendMethodProc (nil,r^.Addr) ;
            MethodGroup.add (String(r^.Name) , strMethodInfo) ;

            Inc(Integer(MethodInfo), PMethodInfoHeader(MethodInfo)^.Len);
            Dec(Count);
         end;
      end;
      // Find the parent VMT
      VMT := PPointer(Integer(VMT) + vmtParent)^;
      if VMT = nil then
         break ;
      VMT := PPointer(VMT)^;
   until False;

end ;

//------------------------------------------------------------------------------

// ITraceNodeEx

function ParamsToString(P:Pointer;Count:Integer):String;
type
  PByte=^Byte;
  PShortString=^ShortString;
var
  i:Integer;
  S:String;
  Flags:TParamFlags;
begin
  Result:='(';
  for i:=1 to Count do
    begin
      if i>1 then S:='; ' else S:='';
      Flags:=TParamFlags(P^);
      if pfVar in Flags then S:=S + ' var ';
      if pfConst in Flags then S:=S + ' const ';
      P:=Pointer(Integer(P)+1);    // P is the param name
      S:=S+String(PShortString(P)^);
      P:=Pointer(Integer(P)+Length(PShortString(P)^)+1);
      S := S + ' : ' + String(PShortString(P)^);
      P:=Pointer(Integer(P)+Length(PShortString(P)^)+1);
      Result:=Result+S;
    end;
  Result:=Result+')';
end;

//------------------------------------------------------------------------------

// ITraceNodeEx

function MethodSyntax(P:PPropInfo):String;
var
  PList:Pointer;
  PCount:Integer;
  TypeData:TTypeData;
begin
  Result:='';
  if not Assigned(P) or (P^.PropType^.Kind<>tkMethod) then exit;
  TypeData:=GetTypeData(P^.PropType^)^;
  PList:=@(TypeData.ParamList);
  PCount:=TypeData.ParamCount;
  Result:=ParamsToString(PList,PCount)+' of object;';
  case TypeData.MethodKind of
    mkProcedure:Result:='procedure '+Result;
    mkFunction:Result:='function '+Result;
    mkSafeProcedure:Result:='procedure '+Result+' safecall;';
    mkSafeFunction:Result:='function '+Result+' safecall;';
  end;
  Result := String(P^.PropType^.Name) + ' : ' + Result;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx

// called by addObject
procedure TTraceNode.DisplayFields (const AObject: TObject; const flags : TraceDisplayFlags);
var
   FieldGroup : TMemberNode ;
   EventGroup : TMemberNode ;
   subObj : TObject ;
   I,J,K, Count: Integer;
   TypeInfo : PTypeInfo ;
   PropInfo: PPropInfo;
   TempList: PPropList;
   VariantPropValue : variant ;
   strType : String ;
   intPropValue : LongInt ;

   Prop_Name      : String ;
   Prop_Type      : String ;
   Prop_Value     : string ;

   DynArrayPointer: Pointer;
   DynArrayObject: TDynArray;
   DynArrayElementPointer : Pointer ;
   DynArrayElementValue1 : byte ;
   DynArrayElementValue2 : word ;
   DynArrayElementValue4 : int32 ;
   TypeInfoName : RawUTF8 ;  // AnsiString
   ClassTypeFound : Boolean ;
   

   //------------------------------------------------------
   // add to the property group
   procedure AddToFieldGroup () ;
   begin
      if ShowFields in flags then begin
         if FieldGroup = nil then begin
            FieldGroup := TMemberNode.create ('Properties').SetFontDetail(0,true) ;
            FieldGroup.ViewerKind := CST_VIEWER_OBJECT ;
            fMembers.Add(FieldGroup) ;
         end ;
         FieldGroup.add (prop_name , prop_value, prop_type) ;
      end ;
   end ;

   //------------------------------------------------------
   // add to the event group
   procedure AddToEventGroup () ;
   begin
      if ShowEvents in flags then begin
         if EventGroup = nil then begin
            EventGroup := TMemberNode.create ('Events').SetFontDetail(0,true) ;
            EventGroup.ViewerKind := CST_VIEWER_OBJECT ;
            fMembers.Add(EventGroup) ;
         end ;
         EventGroup.add (prop_name , prop_value, prop_type) ;
      end ;
   end ;
   //------------------------------------------------------

begin
   if AObject = nil then begin
      fMembers.Add( TMemberNode.create ('nil').SetFontDetail(0,true)) ;
      exit ;
   end ;

   TypeInfo := PTypeInfo(AObject.ClassInfo) ;
   if TypeInfo = nil then
      exit ;
   Count := GetPropList(typeInfo, TempList);   // if Count is zero , FreeMem(TempList) don't have to be called
   if Count > 0 then
   try
      FieldGroup := nil ;
      EventGroup := nil ;

      for I := 0 to Count - 1 do begin
         PropInfo := TempList^[I];

         Prop_Name := String(PropInfo.Name) ;
         Prop_Type := GetPropertyTypeString (PropInfo^.PropType^.Kind);

         case PropInfo^.PropType^.Kind of
            tkInteger,tkString,tkWChar,tkLString,tkWString,tkInt64, tkUString :
               begin

                  VariantPropValue := GetPropValue(AObject, String(PropInfo.Name),true) ;
                  Prop_Value := VariantPropValue ;   
                  AddToFieldGroup() ;   // prop_name , prop_value, prop_type
               end ;

            tkClass :
               begin
                  intPropValue := GetOrdProp(AObject, PropInfo) ;
                  if intPropValue = 0 then  // the property point to a nil TObject
                     Prop_Value := 'nil'
                  else begin
                     subObj := TObject (intPropValue);
                     Prop_Type := subObj.ClassName ;
                     Prop_Value := '@' + inttohex (integer(intPropValue),2) ;  // value is the adresse reference
                  end;
                  AddToFieldGroup() ;  // prop_name , prop_value, prop_type
               end ;

            tkClassRef, tkPointer, tkProcedure {, tkMRecord} :
               begin

               end;
               
            tkMethod :   // events
               begin
                  prop_type := MethodSyntax (PropInfo) ;

                  intPropValue := GetOrdProp(AObject, PropInfo) ;  // how to handle that value ? negative or small
                  if intPropValue = 0 then
                     Prop_Value := 'nil'
                  else
                     Prop_Value := '@' + inttohex (integer(intPropValue),2) ;

                  AddToEventGroup() ;
               end ;
               
            tkDynArray :   // tkArray
               begin                                                                 
                  DynArrayPointer := GetDynArrayProp(AObject, PropInfo);  // from System.TypInfo => GetPropValue where kind = tkDynArray 
                  TypeInfoName := TypeInfoToName(PropInfo^.PropType^) ;
                 
                  DynArrayObject.Init(PropInfo^.PropType^,DynArrayPointer) ;     // aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);
                  prop_type := string(TypeInfoName) + ' : ' + GetPropertyTypeString (PropInfo^.PropType^.Kind) + '[' + IntToStr(DynArrayObject.Count) + ']' + ', element size = ' + IntToStr(DynArrayObject.ElemSize) ;
                  Prop_Value := '@' + inttohex (Integer(DynArrayPointer),2) + ' :' ; 

                  ClassTypeFound := False ;
                  for j := 0 to DynArrayObject.Count-1 do
                  begin
                     DynArrayElementPointer := DynArrayObject.ElemPtr(j) ;
                     case DynArrayObject.ElemSize of
                        1 : begin
                            DynArrayElementValue1 := byte(DynArrayElementPointer^) ;               // 0 .. FF
                            Prop_Value := Prop_Value + ' $' + inttohex (DynArrayElementValue1,2) ;
                        end ;
                        2 : begin
                            DynArrayElementValue2 := word(DynArrayElementPointer^) ;               // 0.. FF FF
                            Prop_Value := Prop_Value + ' $' + inttohex (DynArrayElementValue2,4) ;
                        end ;
                        SizeOf(TObject) : begin
                            DynArrayElementValue4 := int32(DynArrayElementPointer^) ;              // 0 .. FF FF FF FF
                            // is it an integer or a pointer ?      
                            try                         
                               subObj := TObject (DynArrayElementValue4);
                               if (ClassTypeFound = false) and (DynArrayElementValue4 <> 0) then begin                               
                                  prop_type := prop_type + ', class type = ' + subObj.ClassName ;
                                  ClassTypeFound := True;
                               end;
                            except 
                                // eat exception. Element is not a class instance                                                         
                            end;                                              
                          
                            Prop_Value := Prop_Value + ' $' + inttohex (DynArrayElementValue4,8) ;
                        end ;
                        else begin  // record ?
                            for k := 0 to DynArrayObject.ElemSize -1 do begin
                               DynArrayElementValue1 := byte(DynArrayElementPointer^) ;               // 0 .. FF
                               Prop_Value := Prop_Value + ' $' + inttohex (DynArrayElementValue1,2) ;
                               DynArrayElementPointer := pbyte(DynArrayElementPointer) + 1;
                            end;
                        end ;
                     end;                        
                  end;                   
                  AddToFieldGroup() ;  // prop_name , prop_value, prop_type
               end;

            else
               begin  
                  // tkSet 
                  // tkEnumeration
                  // tkUnknown
                  // tkVariant
                  // tkRecord
                  // tkInterface

                  VariantPropValue := GetPropValue(AObject, string(PropInfo.Name),true) ;
                  Prop_Value := tt_GetVarValue (VariantPropValue,strType) ;
                  if PropInfo^.PropType^.Kind = tkVariant then
                     Prop_Type := Prop_Type + '(' + strType + ')' ;

                  AddToFieldGroup() ;
               end ;
         end ;
      end;  // for each prop
   finally
      FreeMem(TempList);
   end;
end ;

//------------------------------------------------------------------------------

// ITraceNodeEx

//TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
//  tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
//  tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString,
//  tkClassRef, tkPointer, tkProcedure {, tkMRecord});

function TTraceNode.GetPropertyTypeString (const TypeKind : TypInfo.TTypeKind) : String ;
begin
  result := '' ;
  case TypeKind of
     tkUnknown      : result := 'Unknown' ;      // A place holder value. Never used
     tkInteger      : result := 'Integer' ;      // Used for any ordinal type and sub-range types
     tkChar         : result := 'Char' ;         // Char and AnsiChar types (where Char and AnsiChar are synonyms)
     tkEnumeration  : result := 'Enumeration' ;  // All enumerated types. This includes Boolean, ByteBool, WordBool, LongBool and Bool
     tkFloat        : result := 'Float' ;        // Any floating point type except Real, which explains why Real properties are not fully supported
     tkString       : result := 'String' ;       // Old-style string types, e.g. String[12] and ShortString
     tkSet          : result := 'Set' ;          // Set types
     tkClass        : result := 'Class' ;        // Class types
     tkMethod       : result := 'Method' ;       // Procedure and function method types
     tkWChar        : result := 'WChar' ;        // WideChar type, new in Delphi 2
     tkLString      : result := 'LString' ;      // Delphi 2+ long strings (made of AnsiChars)
     tkWString      : result := 'WString' ;      // The Delphi 3 constant which replaces tkLWString
     tkVariant      : result := 'Variant' ;      // Variant type, new in Delphi 2
     tkArray        : result := 'Array' ;        // Array types, new in Delphi 3
     tkRecord       : result := 'Record' ;       // Record types, new in Delphi 3
     tkInterface    : result := 'Interface' ;    // Interface types, new in Delphi 3
     tkInt64        : result := 'Int64' ;        // 64-bit integers, new in Delphi 4
     tkDynArray     : result := 'Array' ;        // Dynamic array types, new in Delphi 4
     tkUString      : result := 'UString' ;      //
     tkClassRef     : result := 'ClassRef' ;     //
     tkPointer      : result := 'Pointer' ;      //
     tkProcedure    : result := 'Procedure' ;    //
     //tkMRecord      : result := 'MRecord' ;
  end ;
end ;


//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddDump (const Title: string; const memory: pointer; const ByteCount: integer);
var
   c,d, beginLine : integer ;
   hexa_representation : string ;
   Ptr : PAnsiChar ;
   //Str_representation : string ;
   //OneChar : AnsiChar ;
   OneIntChar : integer ;
   DumpGroup : TMemberNode ;
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   DumpGroup := TMemberNode.create (Title) ;
   DumpGroup.SetFontDetail(0,true) ;
   DumpGroup.ViewerKind := CST_VIEWER_DUMP ;
   fMembers.Add (DumpGroup) ;

   Ptr := memory ;
   c := 0 ;
   while c <= ByteCount-1 do begin
      d := 0 ;

      beginLine := c ;
      hexa_representation := '' ;
      //Str_representation := '' ;
      while (c <= ByteCount-1) and (d < 16) do begin
         try
         OneIntChar := integer(Ptr^) ;
         hexa_representation := hexa_representation + intTohex (OneIntChar,2) + ' ' ;

         // since 12.4 : string representation is no more send to viewer. The viewer will calculate hitself the string

         //if OneIntChar = 255 then
         //   OneIntChar := 0 ;
         //OneChar := AnsiChar( chr(OneIntChar)) ;
         //if AnsiCharTypes[OneChar] and (C1_ALPHA or C1_PUNCT or C1_BLANK or C1_XDIGIT or C1_DIGIT) <> 0 then
         //   Str_representation := Str_representation + string(OneChar)
         //else
         //   Str_representation := Str_representation + '.' ;
         except
            on e:exception do begin
               c := ByteCount + 1 ;
               break ;  // byte cannot be read. Stop loop
            end ;
         end;


         inc (d) ;
         inc (c) ;
         Inc (Ptr) ;
      end ;
      if hexa_representation <> '' then
         DumpGroup.add (inttohex (beginLine,6) , hexa_representation ) ; //  , Str_representation) ;
   end ;

end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddCaller (const Level: integer);
begin
   if (Enabled = false) then
      exit ;
   if assigned (SendCallerProc) then
      SendCallerProc (self, Level+1) ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddMethod (const Meth: Pointer);
begin
   if (Enabled = false) then
      exit ;
   if assigned (SendMethodProc) then
      SendMethodProc (self,Meth) ;
end;

//------------------------------------------------------------------------------

function TTraceNode.AddFontDetail(const ColId: integer; const Bold, Italic: boolean; const Color, Size: integer; const FontName: string): ITraceNodeEx;
var
   FontDetail : TFontDetail ;
   ArrayLen : integer ;
begin
   if (Enabled = false) then
      exit ;
   FontDetail := TFontDetail.create ;
   FontDetail.ColId    := ColId ;
   FontDetail.Bold     := Bold ;
   FontDetail.Italic   := Italic ;
   FontDetail.Color    := Color ;
   FontDetail.Size     := Size ;
   FontDetail.FontName := FontName ;
   ArrayLen := length(FontDetails) ;
   SetLength (FontDetails,ArrayLen+1);
   FontDetails [ArrayLen] := FontDetail ;
   result := self ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
// In order to use stack info
// add the StackTrace unit after Tracetool in the use list
// and set "Debug information" (project compiler options)
// and set "Detailed map file" (project linker options)

procedure TTraceNode.AddStackTrace (const Level: integer);
begin
   if (Enabled = false) then
      exit ;
   // the stack trace is performed in the supplied StackTrace unit (use jedi code lib)
   if assigned (SendStackProc) then
      SendStackProc (self,level+1) ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
procedure TTraceNode.AddBackgroundColor(const Color: TColor; const ColId: integer = -1);
var
   FontDetail : TFontDetail ;
   ArrayLen : integer ;
begin
   if (Enabled = false) then
      exit ;
   FontDetail := TFontDetail.create ;
   FontDetail.ColId    := ColId ;
   FontDetail.Color    := color ;
   FontDetail.FontName := 'BackgroundColor' ;  // special name.

   ArrayLen := length(FontDetails) ;
   SetLength (FontDetails,ArrayLen+1);
   FontDetails [ArrayLen] := FontDetail ;
end;

//------------------------------------------------------------------------------
// ITraceNodeEx

{encode functions from MadCrypt}
type
  TAByte         = array [0..maxInt      -1] of byte;
  TPAByte        = ^TAByte;

// ITraceNodeEx
procedure TTraceNode.AddBitmap(const Bitmap: TBitmap);
Var
   Stream: TMemoryStream;
   StreamSize : integer ;
   Temp: AnsiString;
   member : TMemberNode ;

   function Encode(data: pAnsichar; len: integer) : AnsiString; //overload;
   const b64 : array [0..63] of Ansichar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
   var ic     : integer;
       pi, po : TPAByte;
       c1     : dword;
   begin
     if len > 0 then begin
       SetLength(result, ((len + 2) div 3) * 4);
       pi := pointer(data);
       po := pointer(result);
       for ic := 1 to len div 3 do begin
         c1 := pi^[0] shl 16 + pi^[1] shl 8 + pi^[2];
         po^[0] := byte(b64[(c1 shr 18) and $3f]);
         po^[1] := byte(b64[(c1 shr 12) and $3f]);
         po^[2] := byte(b64[(c1 shr  6) and $3f]);
         po^[3] := byte(b64[(c1       ) and $3f]);
         inc(dword(po), 4);
         inc(dword(pi), 3);
       end;
       case len mod 3 of
         1 : begin
               c1 := pi^[0] shl 16;
               po^[0] := byte(b64[(c1 shr 18) and $3f]);
               po^[1] := byte(b64[(c1 shr 12) and $3f]);
               po^[2] := byte('=');
               po^[3] := byte('=');
             end;
         2 : begin
               c1 := pi^[0] shl 16 + pi^[1] shl 8;
               po^[0] := byte(b64[(c1 shr 18) and $3f]);
               po^[1] := byte(b64[(c1 shr 12) and $3f]);
               po^[2] := byte(b64[(c1 shr  6) and $3f]);
               po^[3] := byte('=');
             end;
       end;
     end else
       result := '';
   end;

begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   Stream := TMemoryStream.Create;
   Try
      Bitmap.SaveToStream(Stream);
      StreamSize := Stream.size ;
      Stream.Position := 0;

      SetLength(temp, StreamSize);
      Stream.Read(PAnsiChar(temp)^, StreamSize);

      member := TMemberNode.create ('','','') ;
      fMembers.Add (member) ;
      member.Col1 := string(Encode(PAnsiChar(temp), StreamSize)) ;
      member.ViewerKind := CST_VIEWER_BITMAP ;
   Finally
      Stream.Free;
   End;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
// to do : create a procedure AddXML with the xml and the commandList to bypass the copy member step
procedure TTraceNode.AddXML(const xml: string);
begin
   if (Enabled = false) then
      exit ;

   getMembers () ;  // ensure members array is create
   fMembers.Add(xml).ViewerKind := CST_VIEWER_XML ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
// return the member array.
// That let user code to add infos in the member pane
function TTraceNode.getMembers: TMemberNode;
begin
   if (fMembers = nil) then
      fMembers := TMemberNode.create ;
   result := fMembers ;
end;

//------------------------------------------------------------------------------

// ITraceNodeEx
function TTraceNode.Send: ItraceNode;
var
   CommandList : TStringList;
   c : integer ;
   FontDetail : TFontDetail ;
   TempStr : string ;
begin
   result := self ;
   if Enabled = false then
      exit ;

   CommandList := TStringList.create ;  // will be freed by the thread

   addCommand (CommandList, CST_NEW_NODE  , fParentNodeId) ; // param : parent Node id
   addCommand (CommandList, CST_TRACE_ID  , Id) ;            // param : guid
   if LeftMsg <> '' then
      addCommand (CommandList, CST_LEFT_MSG  , LeftMsg);     // param : left string
   if RightMsg <> '' then
      addCommand (CommandList, CST_RIGHT_MSG , RightMsg);    // param : right string
   addCommand (CommandList, CST_ICO_INDEX , IconIndex) ;     // param : the icon index

   // add font detail
   for c := 0 to length (FontDetails)-1 do begin
      FontDetail := FontDetails[c] ;

      if FontDetail.FontName = 'BackgroundColor' then begin
         //special color : background color
         addCommand (CommandList, CST_BACKGROUND_COLOR, FontDetail.Color,inttostr(FontDetail.ColId)) ;      // param : color, colId

      end else begin
         TempStr := Format('%5d%3d', [CST_FONT_DETAIL,FontDetail.ColId]) ;

         if FontDetail.Bold then
            TempStr := TempStr + '1'
         else
            TempStr := TempStr + '0' ;

         if FontDetail.Italic then
            TempStr := TempStr + '1'
         else
            TempStr := TempStr + '0' ;

         TempStr := TempStr + Format('%11d%11d', [FontDetail.Color,FontDetail.Size]) + FontDetail.FontName  ;
         CommandList.add (TempStr);
      end ;

      FontDetail.Free ;   // once copied to Commandlist, free the node
      FontDetails[c] := nil ;
   end ;
   setlength (FontDetails,0) ;  // once copied to Commandlist, clear the array

   // check if some "addxxx" functions has create the Members.
   if fMembers <> nil then
      fMembers.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId, fTime , fThreadName);

end;

//------------------------------------------------------------------------------
// ITraceNodeEx

function TTraceNode.Resend: ItraceNode;
var
   CommandList : TStringList;
begin
   result := self ;
   if Enabled = false then
      exit ;

   CommandList := TStringList.create ;  // will be freed by the thread
   if id = '' then
      raise exception.create ('Node Id is null, root node cannot be modified (for now)') ;

   addCommand (CommandList, CST_USE_NODE,id);             // param : the node that receive the string
   addCommand (CommandList, CST_RIGHT_MSG,RightMsg);      // param : right string
   addCommand (CommandList, CST_LEFT_MSG,leftMsg);        // param : left string

   // resend members or not ?
   // if yes, does the new members must override already sent members or must be appened ?
   // For now, the resend just do nothing with members....
   // Members.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

   TTrace.SendToWinTraceClient (CommandList, fWinTraceId);
end;

//------------------------------------------------------------------------------


{ TTraceNode }

constructor TTraceNode.create (const ParentNode : TTraceNode ; const generateUniqueId: boolean);
begin
   //inherited {TInterfacedObject} create() ;  // empty TInterfacedObject constructor, but ...
   // create a node id if needed. No more reset id to empty string if generateUniqueId is false
   if (generateUniqueId) then
      self.fId := TTrace.CreateTraceID ;

   if (ParentNode = nil) then begin
      self.fIconIndex    := CST_ICO_DEFAULT ;
      self.fEnabled      := true ;
      self.fWinTraceId   := '' ;
      self.fParentNodeId := '' ;
   end else begin
      self.fIconIndex    := ParentNode.IconIndex ;
      self.fEnabled      := ParentNode.Enabled ;
      self.fWinTraceId   := ParentNode.WinTraceId ;
      self.fParentNodeId := ParentNode.getlastcontextId() ;  // get parent node id (or node context id)
   end ;
   fMembers := nil ; // created only when needed ;
   fTime := 0 ;      // tracenodeEx
end;

//------------------------------------------------------------------------------

constructor TTraceNode.create (const ParentNode : ITraceToSend ; const generateUniqueId: boolean);
begin
   //inherited {TInterfacedObject} create() ;  // empty TInterfacedObject constructor, but ...
   // create a node id if needed. No more reset id to empty string if generateUniqueId is false
   if (generateUniqueId) then
      self.fId := TTrace.CreateTraceID ;

   if (ParentNode = nil) then begin
      self.fIconIndex    := CST_ICO_DEFAULT ;
      self.fEnabled      := true ;
      self.fParentNodeId := '' ;
      self.fWinTraceId   := '' ;
   end else begin
      self.fIconIndex    := ParentNode.IconIndex ;
      self.fEnabled      := ParentNode.Enabled ;
      self.fParentNodeId := ParentNode.getlastcontextId() ;  // get parent node id (or node context id)
      self.fWinTraceId   := ParentNode.WinTraceId ;
   end ;
   fMembers := nil ; // created only when needed ;
   fTime := 0 ;      // tracenodeEx
end;

//------------------------------------------------------------------------------

procedure TTraceNode.BeforeDestruction;
var
   c : integer ;
begin
   inherited {TInterfacedObject} BeforeDestruction() ;
   if fContextList <> nil then
      fContextList.free ;  // own contexts

   // FontDetails is empty when the node is send.
   // If not send, the destructor must free it
   for c := 0 to length (FontDetails)-1 do begin
      FontDetails[c].Free ;
      FontDetails[c] := nil ;
   end ;
   setlength (FontDetails,0) ;

   if fMembers <> nil then
      fMembers.free ;      // auto destroy children

   if fListCriticalSection <> nil then
      fListCriticalSection.free ;
end;

//------------------------------------------------------------------------------

function TTraceNode.prepareNewNode (const leftMsg : string ; const newId : string) : TStringList ;
begin
   result := TStringList.create ;     // will be freed by the thread

   addCommand (result, CST_NEW_NODE, getlastcontextId()) ;  // param : parent Node id (self or context)

   addCommand (result, CST_TRACE_ID, newId) ;      // param : Node Id
   addCommand (result, CST_LEFT_MSG, leftMsg);     // param : left string
   if IconIndex <> -1 then
      addCommand (result, CST_ICO_INDEX, IconIndex) ; // param : icon index
end ;

//------------------------------------------------------------------------------

function TTraceNode.getLastContext : TNodeContext;
var
   c : integer ;
   thId: DWORD ;
   cList : TObjectList ;
begin
   result := nil ;
   if fWinTraceContext <> nil then
      cList := fWinTraceContext
   else if fContextList <> nil then
      cList := fContextList
   else
      exit ;

   // no need to lock an empty list
   if cList.Count = 0 then
      exit ;
   thId := GetCurrentThreadId() ;
   if fListCriticalSection = nil then
      fListCriticalSection := TCriticalSection.Create ;
   fListCriticalSection.Acquire ;
   try
      for c := 0 to cList.Count-1 do begin
         result := TNodeContext (cList.Items[c]) ;
         if result.ThreadId = thId then
            exit ;
      end ;
   finally
      fListCriticalSection.Release ;
   end ;
   result := nil ;
end;

//------------------------------------------------------------------------------

function TTraceNode.getLastContextId : string;
var
   context : TNodeContext ;
begin
   context := getLastContext () ;
   if context = nil then
      result := self.id
   else
      result := context.NodeId ;
end;

//------------------------------------------------------------------------------

procedure TTraceNode.PushContext (NewContext : TNodeContext) ;
var
   cList : TObjectList ;
begin
   if fWinTraceContext <> nil then
      cList := fWinTraceContext
   else if fContextList <> nil then
      cList := fContextList
   else begin
      fContextList := TObjectList.create (true) ;  // own context
      cList := fContextList ;
   end ;

   if fListCriticalSection = nil then
      fListCriticalSection := TCriticalSection.Create ;
   fListCriticalSection.Acquire ;
   try
      cList.Insert (0, NewContext) ;
   finally
      fListCriticalSection.Release ;
   end ;
end ;

//------------------------------------------------------------------------------

procedure TTraceNode.deleteLastContext ;
var
   c : integer ;
   context : TNodeContext ;
   thId: DWORD ;
   cList : TObjectList ;
begin
   if fWinTraceContext <> nil then
      cList := fWinTraceContext
   else if fContextList <> nil then
      cList := fContextList
   else
      exit ;

   thId := GetCurrentThreadId() ;
   if fListCriticalSection = nil then
      fListCriticalSection := TCriticalSection.Create ;
   fListCriticalSection.Acquire ;
   try
      for c := 0 to cList.Count-1 do begin
         context := TNodeContext (cList.Items[c]) ;
         if context.ThreadId = thId then begin
            cList.Delete (c) ;
            exit ;
         end ;
      end ;
   finally
      fListCriticalSection.Release ;
   end ;
end ;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TTraceOptions }

function TTraceOptions._IsProcessName() : boolean ;
begin
   result := not (fProcessFileName = '') ;
end;

//------------------------------------------------------------------------------

procedure TTraceOptions._SendProcessName(const Value: boolean);
var
   buf : array [0..MAX_PATH] of char ;
begin
   if Value = true then begin
      buf [GetModuleFileName(HInstance, buf, sizeof (buf))] := #0 ;
      fProcessFileName := buf ;
      // get only the module name and extension
      fProcessFileName := ExtractFileName(fProcessFileName) ;
   end else begin
      fProcessFileName := '' ;
   end ;
end;

//------------------------------------------------------------------------------

constructor TTraceOptions.Create;
begin
   fSendThreadId := true ;
   fSendMode := tmWinMsg ;
   fSendDate := false ;
   fSendFunctions := false ;
   fSendInherited := false ;
   fSendEvents    := false ;
   fProcessFileName := '' ;
   fSocketPort := 8090 ;
   fSocketHost := '127.0.0.1' ;
   fObjectTreeDepth := 3 ;

   // force encoding to Unicode if the char is coded in 2 bytes
   //if SizeOf(char) = 2 then
   //   fEncoding := enUnicode   // little endian
   //else
   //   fEncoding := enSingleByte ;
end;

//------------------------------------------------------------------------------

function TTraceOptions.GetDefault: TraceDisplayFlags;
begin
   // display at least public (inherited) fields and properties
   // Privat members are discarded
   result := [ShowFields] ; // ShowModifiers , ShowInheritedMembers

   if fSendEvents then
      result := result + [ShowEvents] ;

   if fSendFunctions then
      result := result + [ShowMethods] ;

   //if fSendInherited then
   //   result := result + [ShowInheritedMembers] ;    // not implemented
end;

//------------------------------------------------------------------------------

procedure TTraceOptions.setSocketHost(const Value: string);
begin
   fSocketHost := value ;
   if assigned (AlternateSend) then
      AlternateSend.InitClientSocket();
end;

//------------------------------------------------------------------------------

procedure TTraceOptions.setSocketPort(const Value: integer);
begin
   fSocketPort := Value ;
   if assigned (AlternateSend) then
      AlternateSend.InitClientSocket();
end;


//------------------------------------------------------------------------------

procedure TTraceOptions.setSocketUdp(const Value: boolean);
begin
   fSocketUdp := Value ;
   if assigned (AlternateSend) then
      AlternateSend.InitClientSocket();
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TMsgThread }

procedure TMsgThread.AfterConstruction;
begin
   inherited;
   CloseEvent   := CreateEvent( nil, True, False, nil );  // Create the close event. Manualreset = true, initial = false
   ClosedEvent  := CreateEvent( nil, True, False, nil );  // Create the closed event. Manualreset = true, initial = false
   MessageReady := CreateEvent( nil, True, False, nil );  // Create the ready event. Manualreset = true, initial = false

   // CloseHandle will be called by the Stop procedure
end;

//------------------------------------------------------------------------------

procedure TMsgThread.BeforeDestruction;
begin
   inherited;
end;

//------------------------------------------------------------------------------

procedure TMsgThread.Execute;
var
   dwHandleSignaled:   DWORD;
   flushEvent : THandle ;
   CDS: TCopyDataStruct;
   DebugWin: hWnd;
   MessageString : String ;
   SingleMessage : String ;
   c,i : integer ;
   tot : integer ;
   isTStringList : boolean ;

   tempList : TObjectList ; // used for swap of list
   TempObject : TObject ;
   CommandList : TStringList ;
   HandlesToWaitFor: array[0..1] of THandle;

begin
   TT_SetCurrentThreadName('Tracetool thread') ;
   // We will be waiting on these objects.
   HandlesToWaitFor[0] := CloseEvent;
   HandlesToWaitFor[1] := MessageReady ;

   // This is the main loop.  Loop until we break out.
   while True do
   begin
       // wait for a message or the CloseEvent. The Update will wait max 60 sec
       dwHandleSignaled := WaitForMultipleObjects({count} 2, {handelList} @HandlesToWaitFor, {bwaitall} False, {millisec} 60000);

       case dwHandleSignaled of
          WAIT_OBJECT_0:     // CloseEvent signaled
             begin
                 // time to exit
                 ResetEvent(CloseEvent) ;
                 SetEvent  (ClosedEvent) ;  // tell the main thread that the thread can be free
                 break ;
             end;

           WAIT_OBJECT_0 + 1: // MessageReady signaled : New message was received.
              begin
                 try  // ensure that the trace will not generate error

                    line := 1 ; // for exception handling
                    criticalSection.Acquire ;

                    // swap the 2 list to release the lock as fast as possible
                    tempList := getMessageList ;          // getMessageList is the empty list
                    getMessageList := setMessageList ;    // setMessageList is the list of message to send
                    setMessageList := tempList ;
                    // let other thread setting the ready flag
                    ResetEvent(MessageReady) ;
                    criticalSection.Leave ;

                    // loop over the getMessageList

                    for c := 0 to getMessageList.Count-1 do begin
                       TempObject := getMessageList.Items[c] ;   // TOBJECT

                       isTStringList := false ;
                       try
                          if TempObject is TStringList then
                             isTStringList := true ;
                       except
                          on e : exception do begin
                             LastExceptionMessage.Add('check if object is TStringList : ' + e.Message) ;
                             isTStringList := false ;
                          end ;
                       end ;

                       if isTStringList = false then begin       // should not happens...
                          continue ;
                       end ;

                       line := 2 ; // for exception handling
                       CommandList := TStringList (TempObject) ;

                       // check if the message contains information for the client
                       try
                          ParseForInternal(CommandList) ;
                       except
                          on e : exception do begin
                             LastExceptionMessage.add('ParseForInternal : ' + e.Message) ;
                          end ;
                       end ;
                       line := 3 ; // for exception handling

                       InterlockedIncrement(sendMessageCount);
                       if CommandList.Count >= 0 then begin
                          // special case : the CST_FLUSH is handled by the sender thread and is not send to viewer
                          SingleMessage := CommandList.Strings[0] ;
                          SingleMessage := Copy (SingleMessage,0,5) ;   // out of memory...

                          if strtoint(SingleMessage) = CST_FLUSH then begin
                             flushEvent := strtoint(Copy (CommandList.Strings[0],5,20)) ;
                             SetEvent(flushEvent);
                             CommandList.free ;
                             continue ;
                          end ;
                       end;

                       line := 4 ; // for exception handling
                       // by default, encoding is single byte for delphi prior to version 2009. Unicode starting at version 2009
                       // messagestring is defined as string
                       // todo : if SizeOf(char) is 2 but the ttrace.Options.encoding is enSingleByte then convert to single byte (without the UTF-16 byte order mark)
                       // todo : if SizeOf(char) is 1 but the ttrace.Options.encoding is enUnicode then convert to unicode (and add the UTF-16 byte order mark)
                       if SizeOf(char) = 2 then begin
                          tot := 1 ;
                          MessageString := chr(65279) ; // #$FEFF ; // start with the UTF-16, little endian byte order mark
                       end else begin // string are single bytes
                          tot := 0 ;
                          MessageString := '' ;
                       end ;

                       for i := 0 to CommandList.Count -1 do begin
                          SingleMessage := CommandList.Strings[i] ;
                          tot := tot + Length(SingleMessage) + 1 ;
                          MessageString := MessageString + SingleMessage + #0;
                       end ;
                       MessageString := MessageString + #0 ;
                       inc (tot);
                       CommandList.free ;
                       line := 5 ; // for exception handling

                       if TTrace.Options.fSendMode = tmNone then begin
                          // don't send to viewer
                       end else if TTrace.Options.fSendMode = tmAlternate then begin
                          try
                             if assigned (AlternateSend) then
                                AlternateSend.Send (MessageString , tot) ;
                          finally
                          end ;

                       end else begin  // windows messages

                          DebugWin := StartDebugWin;
                          if DebugWin <> 0 then begin

                            if SizeOf(char) = 2 then begin // wide
                               CDS.cbData := tot * 2 ;
                               CDS.dwData := WMD ;   // identification code for tracetool
                               CDS.lpData := pAnsiString (pString(MessageString)); // no need to add #0, because String are null terminated
                            end else begin // single byte
                               CDS.cbData := tot ;
                               CDS.dwData := WMD ;   // identification code for tracetool
                               CDS.lpData := pAnsiString (AnsiString(MessageString)); // no need to add #0, because String are null terminated
                            end ;

                            SendMessage(DebugWin, WM_COPYDATA, 0, LParam(@CDS));  //WParam(Application.Handle)

                          end;
                       end ;
                    end ;  // next message
                    getMessageList.Clear ;
                 except
                    on e : exception do begin
                       LastExceptionMessage.add('MessageReady signaled. Exception after line ' + inttostr(line) + ' : ' + e.Message) ;
                    end ;
                 end ;
              end;

           WAIT_FAILED:       // Wait failed.  Shouldn't happen.
              begin
                 //sendDebugEX ('WAIT_FAILED: ' + IntToStr(GetLastError),mtError);
                 break;
              end;

           WAIT_TIMEOUT:
              begin
                 // time out, perform some jobs...
                 // ...
                 continue ;
              end ;

           else                // This case should never occur.
              begin
                 //sendDebugEX( 'Unexpected Wait return value ' +IntToStr(dwHandleSignaled),mtError );
                 break;
              end;
       end; // case dwHandleSignaled of
    end; {main loop}
    // freeOnterminate = true
end;

//------------------------------------------------------------------------------

procedure TMsgThread.ParseForInternal(MsgList: TstringList);
var
   LeftMsg        : string ;       // Left col
   RightMsg       : string ;       // right col
   TraceID        : string ;       // the reference of the node : it's a guid
   ThreadID       : string ;       // thread id of the sender
   ProcessName    : string ;       // optional : the name of the process that send traces
   TreeIcon       : Smallint ;     // -1 by default, converted to 24
   ParentId       : string ;
   MessageTime    : string ;

   SingleMsg : string ;
   command : integer ;
   CommandIndex : integer ;
   commandParams : string ;
   TraceForm : TInternalWinTrace ;
   IsNewNode : boolean ;
   header : string ;

   c : integer ;
   f : file of byte ; // textfile ;
   buf : AnsiString ;
   toWrite: Integer;
   //FileToWrite : string ;
   FileExt : string ;
   xml : string ;
   MemberXml : string ;
   Columns : TStrings ;       // multi columns strings
   StateIo : integer ;
   FileModeSave : byte ;      // used the save the system FileMode (default is fmOpenReadWrite)

 begin
   ProcessName := '' ;
   ThreadID    := '' ;
   MessageTime := '' ;
   TreeIcon    := -1 ;
   LeftMsg     := '' ;       // Left col
   RightMsg    := '' ;       // right col
   TraceID     := '' ;       // the node reference
   ParentId    := '' ;
   MemberXml   := '' ;

   IsNewNode      := false ;      // check if message is a new node
   TraceForm      := DefaultWinTrace ;  // traces are send to the master trace form by default

   // to be valid, CST_USE_TREE or CST_USE_MULTICOL_TREE or CST_WINWATCH_ID must be the first command
   if MsgList.Count > 0 then begin
      SingleMsg := MsgList[0] ;
      command := StrToIntDef (copy (SingleMsg, 1,5),-1);
      commandParams := copy (SingleMsg,6,length (SingleMsg)-5);
      if command = CST_USE_TREE then begin
         TraceForm := getInternalTraceForm (commandParams,false) ;
      end else if command = CST_WINWATCH_ID then begin
         exit ;
      end ;
   end ;

   // stop parsing if the winForm is not registered or the winForm don't need to be saved
   // 3, Local log is disabled
   // 4, Local log enabled. No size limit.
   // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   if (TraceForm = nil) or (TraceForm.LogFileType = 3) then
      exit ;

   for CommandIndex := 0 to MsgList.Count-1 do begin
      SingleMsg := MsgList[CommandIndex] ;

      command := StrToIntDef (copy (SingleMsg, 1,5),-1);
      commandParams := copy (SingleMsg,6,length (SingleMsg)-5);

      // only a few commands are used to save to file. Others are ignored
      case command of

      CST_WATCH_NAME :         exit ;  // Bypass watches
      CST_MESSAGE_TIME :       MessageTime := commandParams ;
      CST_PROCESS_NAME :       ProcessName := commandParams ;
      CST_THREAD_ID :          ThreadID := '0x' + IntToHex (StrToInt (commandParams),3) ;
      CST_THREAD_NAME :        ThreadID := commandParams ;
      CST_ICO_INDEX :          TreeIcon := StrToInt (commandParams) ;
      CST_NEW_NODE :           begin
                                  // param1 : Parent Node
                                  ParentId := commandParams ;
                                  IsNewNode := true ;
                               end ;
      CST_TRACE_ID :           TraceID := commandParams ;
      CST_LEFT_MSG :           LeftMsg := commandParams ;  // param : msg
      CST_RIGHT_MSG :          RightMsg := commandParams ;    // param : msg
      CST_CREATE_MEMBER :      MemberXml := MemberXml + '<Member>'+ HtmlEncode(commandParams) ;
      CST_MEMBER_COL2 :        begin
                                  if commandParams <> '' then
                                     MemberXml := MemberXml + '<ColB>' + HtmlEncode(commandParams) + '</ColB>' ;
                               end ;
      CST_MEMBER_COL3 :        begin
                                  if commandParams <> '' then
                                     MemberXml := MemberXml + '<ColC>' + HtmlEncode(commandParams) + '</ColC>' ;
                               end ;
      CST_MEMBER_VIEWER_KIND : begin
                                  if StrToInt (commandParams) <> CST_VIEWER_NONE then
                                     MemberXml := MemberXml + '<ViewerKind>' + commandParams + '</ViewerKind>' ;
                               end ;
      CST_ADD_MEMBER :         MemberXml := MemberXml + '</Member>' ;


      end ;
   end ; // next line to interpret

   // if new node then save to log file
   if IsNewNode = false then
      exit ;

   xml := '<Node' ;
   if ProcessName <> '' then
      xml := xml + ' Process="' + HtmlEncode(ProcessName) + '"';

   // add parent relation if not root
   if MessageTime <> '' then
      xml := xml + ' Time="' + HtmlEncode(MessageTime) + '"' ;
   if ParentId <> '' then
      xml := xml + ' Parent="' + HtmlEncode(ParentId) + '"' ;  
   if TraceID <> '' then
      xml := xml + ' Id="' + HtmlEncode(TraceID) + '"' ;
   if ThreadID <> '' then
      xml := xml + ' ThId="' +HtmlEncode(ThreadID) + '"';

   // don't save default
   if (TreeIcon <> -1) and (TreeIcon <> 24) then
      xml := xml + ' Icon="' + inttostr (TreeIcon) + '"' ;
   xml := xml + '>' ;   // <Node ...>

   if TraceForm.IsMultiColTree then begin
      //<ColValue Order="2">C3</ColValue>
      columns := getDelimStrings(pchar(LeftMsg),#9) ;
      for c := 0 to Columns.Count-1 do
         xml := xml + '<ColValue Order="' + intToStr(c) + '">' + HtmlEncode(Columns[c]) + '</ColValue>' ;
      columns.free ;
   end else begin
      // save the tree col1
      xml := xml + HtmlEncode(LeftMsg) ;
      // save the tree col 2
      if RightMsg <> '' then
         xml := xml + '<Col2>' + HtmlEncode(RightMsg) + '</Col2>' ;
   end ;

   if MemberXml <> '' then
      xml := xml + MemberXml ;

   xml := xml + '</Node>' ;
   if trim(TraceForm.LogFileName) = '' then
      TraceForm.LogFileName := 'TraceLog.xml' ;

   FileExt := ExtractFileExt(TraceForm.LogFileName) ;
   if TraceForm.LogFileType = 3 then
      // should not happens. Detected before parsing
   else if TraceForm.LogFileType = 4 then begin   // 4, Local log enabled.
      // Append CurrentFileNumber Before extension
      if TraceForm.CurrentFileNumber = 0 then
         TraceForm.LastLocalLogFileName := TraceForm.LogFileName
      else
         TraceForm.LastLocalLogFileName := copy (TraceForm.LogFileName, 1, length (TraceForm.LogFileName) - length (FileExt)) + '_' + inttostr(TraceForm.CurrentFileNumber) + FileExt ;

   end else begin                          // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      // Append CurrentFileNumber Before extension
      if TraceForm.CurrentFileNumber = 0 then
         TraceForm.LastLocalLogFileName := copy (TraceForm.LogFileName, 1, length (TraceForm.LogFileName) - length (FileExt)) + FormatDateTime('YYYYMMDD',now) + FileExt
      else
         TraceForm.LastLocalLogFileName := copy (TraceForm.LogFileName, 1, length (TraceForm.LogFileName) - length (FileExt)) + '_' + inttostr(TraceForm.CurrentFileNumber) + FormatDateTime('YYYYMMDD',now) + FileExt ;
   end ;

   FileModeSave := fileMode ; // save the system.fileMode. Can be modified by the process that use tracetool
   
   try try

      if not fileExists(TraceForm.LastLocalLogFileName) then begin
         assignFile (f, TraceForm.LastLocalLogFileName) ;
         filemode := fmOpenWrite ;  // The FileMode variable defines the mode in which the Reset procedure opens a typed or untyped binary file.
         {$I-}                         // disable the default Delphi IO error trapping
         rewrite (f) ;                 // creates a new external file
         stateIo := IOResult ;         // The IOResult function retrieves the result of the last I/O (file input/output) operation.
         {$I+}
         // include header in file
         header := '' ;
         if TraceForm.IsMultiColTree then begin
            header := '<MainColumn>' + inttostr(TraceForm.MainCol) + '</MainColumn>' ;
            for c := 0 to TraceForm.TitleList.Count-1 do
               header := header + '<ColTitle Order="' + IntToStr(c) + '">' + TraceForm.TitleList[c] + '</ColTitle>' ;
         end ;
         xml := // '<?xml version="1.0" encoding="utf-8"?>'+
                '<Data>'+header+xml ;  //
      end else begin  // append only the node
         assignFile (f, TraceForm.LastLocalLogFileName) ;
         filemode := fmOpenWrite ;    // The FileMode variable defines the mode in which the Reset procedure opens a typed or untyped binary file.
         {$I-}                        // disable the default Delphi IO error trapping
         reset (f) ;                  // open the existing file in Read/Write access
         stateIo := IOResult ;        // The IOResult function retrieves the result of the last I/O (file input/output) operation.
         {$I+}
         
         if (StateIO = 0) then begin
            if (filesize(f)-7 > 0) then      // file should always have the xml header and footer, except if the user delete content
               seek (f, filesize(f)-7)       // override the </data> tag
            else
               xml := // '<?xml version="1.0" encoding="utf-8"?>'+ 
                      '<Data>';
         end;
      end ;

      if (stateIo = 0) then begin               // don't save if IO error
         xml := xml + #13+ '</Data>' ;
         buf := Utf8Encode (XML) ;
         toWrite := length(buf) ;
         BlockWrite(f, Pointer(buf)^, toWrite);
         closefile (f) ;
      end;
      
      // limit file size
      if (TraceForm.MaxLines <> -1) then begin
         inc(TraceForm.LinesWritten) ;
         if TraceForm.LinesWritten >= TraceForm.MaxLines then begin
            inc(TraceForm.CurrentFileNumber) ;
            TraceForm.LinesWritten := 0 ;  // reset counter
         end ;
      end ;
   except 
      on e : exception do
         LastExceptionMessage.add(e.Message) ;
   end finally
      fileMode := FileModeSave ;    // restore previous fileMode modified by the host process
   end;
end ;

//------------------------------------------------------------------------------

{
SocketHost=LocalHost
SocketPort=8090
DebugEnabled=true
WarningEnabled=true
ErrorEnabled=true
SendMode=socket
SendProcessName=false
SendFunctions=false
SendInherited=false
SendEvents=false
}

procedure LoadConfig ;
var
   configString : TStringList ;
   c,d : integer ;
   line, prop, value : string ;

   function getBool () : boolean ;
   begin
      result := false ;
      if StrIComp(pchar(value) , 'true') = 0 then
         result := true
      else if value = '1' then
         result := true ;
   end ;
begin
   configString := TStringlist.create ;
   try try
      if FileExists('TraceTool.config') then begin
         configString.LoadFromFile('TraceTool.config');
         for c := 0 to configString.Count-1 do begin
            line := configString.Strings[c] ;
            d := pos ('=',line) ;
            if d <> 0 then begin
               prop := copy (line, 1 , d-1) ;
               value := copy (line, d+1, 1000) ;

               if StrIComp(pchar(prop),          'SocketHost'     ) = 0 then begin  // string
                  ttrace.Options.SocketHost := value  ;
               end else if StrIComp(pchar(prop), 'SocketPort'     ) = 0 then begin  // integer
                  ttrace.Options.SocketPort := strToIntDef (value,8090) ;
      			   end else if StrIComp(pchar(prop),'ObjectTreeDepth' ) = 0 then begin  // integer
     				      ttrace.options.objectTreeDepth := strToIntDef(value,3) ;
               end else if StrIComp(pchar(prop), 'SocketUdp' ) = 0 then begin  // boolean
                  ttrace.Options.SocketUdp := getBool () ;
               end else if StrIComp(pchar(prop), 'DebugEnabled'   ) = 0 then begin  // boolean
                  ttrace.Debug.Enabled := getBool () ;
               end else if StrIComp(pchar(prop), 'WarningEnabled' ) = 0 then begin  // boolean
                  ttrace.warning.Enabled := getBool () ;
               end else if StrIComp(pchar(prop), 'ErrorEnabled'   ) = 0 then begin  // boolean
                  ttrace.error.Enabled := getBool () ;
               end else if StrIComp(pchar(prop), 'SendMode'       ) = 0 then begin  // TSendMode
                  if StrIComp(pchar(value) , 'WinMsg') = 0 then
                     ttrace.Options.SendMode := tmWinMsg
                  else if StrIComp(pchar(value) , 'Alternate') = 0 then
                     ttrace.Options.SendMode := tmAlternate
                  else if StrIComp(pchar(value) , 'socket') = 0 then
                     ttrace.Options.SendMode := tmAlternate
                  else if StrIComp(pchar(value) , 'none') = 0 then
                     ttrace.Options.SendMode := tmNone ;
               end else if StrIComp(pchar(prop), 'SendProcessName') = 0 then begin  // boolean
                  ttrace.Options.SendProcessName := getBool () ;
               end else if StrIComp(pchar(prop), 'SendDate') = 0 then begin  // boolean
                  ttrace.Options.SendDate := getBool () ;
               end else if StrIComp(pchar(prop), 'SendFunctions'  ) = 0 then begin  // boolean
                  ttrace.Options.SendFunctions := getBool () ;
               end else if StrIComp(pchar(prop), 'SendInherited'  ) = 0 then begin  // boolean
                  ttrace.Options.SendInherited := getBool () ;
               end else if StrIComp(pchar(prop), 'SendEvents'     ) = 0 then begin  // boolean
                  ttrace.Options.SendEvents := getBool () ;
               end else if StrIComp(pchar(prop), 'SendDate'     ) = 0 then begin  // boolean
                  ttrace.Options.SendDate := getBool () ;
               end else if StrIComp(pchar(prop), 'SendThreadId' ) = 0 then begin  // boolean
                  ttrace.Options.SendThreadId := getBool () ;

               //end else if StrIComp(pchar(prop), 'Encoding' ) = 0 then begin  // string
               //   if StrIComp(pchar(value) , 'SingleByte') = 0 then
               //      ttrace.Options.encoding := enSingleByte
               //   else if StrIComp(pchar(value) , 'Unicode') = 0 then
               //      ttrace.Options.encoding := enUnicode
               end;
            end ;
         end ;
      end ;
   except
      on e : exception do begin end ;
   end finally
      configString.Clear ;
      configString.free ;
   end ;
end;

//------------------------------------------------------------------------------

// Check if the viewer is running. If not try to run it.
function StartDebugWin: hWnd;
var
  Reg: TRegistry;
  DebugFilename: string;
  Buf: array[0..MAX_PATH + 1] of Char;
  si: TStartupInfo;
  pi: TProcessInformation;
  c : integer ;
begin
   result := FindWindow('TFormReceiver', 'FormReceiver');

   if result <> 0 then
      exit ;

   // retry in case of another thread already launch the process
   result := FindWindow('TFormReceiver', 'FormReceiver');
   if result <> 0 then
      exit ;

   Result := 0 ;

   Reg := TRegistry.Create;
   try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey('Software\TraceTool',true);
      DebugFilename := Reg.ReadString('FilePath');
      Reg.CloseKey ;
   finally
      Reg.Free;
   end ;

   if (Trim(DebugFilename) = '') or not FileExists(DebugFilename) then begin
      GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf)-1);
      DebugFileName := ExtractFilePath(StrPas(Buf))+'TraceTool.exe';
   end ;

   if (Trim(DebugFilename) = '') then
      exit ;

   if not FileExists(DebugFilename) then
      exit ;

   FillChar(si, SizeOf(si), #0);
   si.cb := SizeOf(si);
   si.dwFlags := STARTF_USESHOWWINDOW;    // If this value is not specified, the wShowWindow member is ignored.
   si.wShowWindow := SW_NORMAL ; // SW_SHOWNOACTIVATE ; // SW_SHOWMINNOACTIVE  ; //  SW_HIDE; //SW_MINIMIZE ; // SW_SHOWNA ;  // Displays the window as an icon. The window that is currently active remains active

   if not CreateProcess(PChar(DebugFilename), nil, nil, nil, False, 0, nil, nil, si, pi) then
   begin
     Result := 0;
     Exit;
   end;

   try
     WaitForInputIdle(pi.hProcess, 3 * 1000); // wait for 3 seconds to get idle
   finally
     CloseHandle(pi.hThread);
     CloseHandle(pi.hProcess);
   end;

   // wait max 5 secondes
   for c := 1 to 50 do begin
      Result := FindWindow('TFormReceiver', 'FormReceiver');
      if result <> 0 then
         exit ;
      WaitForSingleObject (MsgThread.Handle,100) ;   // 50*100 = 5 sec
   end ;
end;

//------------------------------------------------------------------------------

// get the member type (field or method) or parameter type
// don't work with member from IDispatch and IUnknown
function tt_getVariantType (const TypeInfo : ITypeInfo ; const ObjTypeDesc: PTypeDesc) : String ;
var
   ARefType : Cardinal;
   TypeInfoBis : ITypeInfo;
   intfName : WideString ;
begin
   if (ObjTypeDesc.vt=VT_PTR) then
   begin
      // associated typeInfo to this type
      if (ObjTypeDesc.ptDesc.vt = VT_USERDEFINED) then begin
         ARefType := ObjTypeDesc.ptDesc^.hreftype ; // object href
         OleCheck(TypeInfo.GetRefTypeInfo(ARefType,TypeInfoBis))
      end else
         TypeInfoBis := TypeInfo;

      OleCheck(TypeInfoBis.GetDocumentation (-1 ,@intfName, nil,nil,  nil));
      result := intfName ;   // widestring to string

   end else begin
      result := tt_GetVariantType (ObjTypeDesc.vt) ;
   end ;
end ;

//------------------------------------------------------------------------------
// convert varType (integer) to string representation
// if VT_PTR (dispatch), the real type is not returned
function tt_GetVariantType (const VarType : integer) : String ;
begin
   // [V] - may appear in a VARIANT
   // [T] - may appear in a TYPEDESC
   // [P] - may appear in an OLE property set
   // [S] - may appear in a Safe Array }

   result := '' ;
   if (VarType) = VT_ARRAY then begin
      result := 'Array ' ;
      exit ;
   end ;

   case VarType of
      vt_empty           : result := 'Empty' ;                       // 0        [V]   [P]  nothing
      vt_null            : result := 'Null' ;                        // 1        [V]        SQL style Null
      VT_I2              : result := 'Small int' ;                   // 2        [V][T][P]  2 byte signed int
      VT_I4              : result := 'Integer' ;                     // 3        [V][T][P]  4 byte signed int
      VT_R4              : result := 'Single' ;                      // 4        [V][T][P]  4 byte real
      VT_R8              : result := 'Double' ;                      // 5        [V][T][P]  8 byte real
      VT_CY              : result := 'Currency' ;                    // 6        [V][T][P]  currency
      VT_DATE            : result := 'Date' ;                        // 7        [V][T][P]  date
      VT_BSTR            : result := 'String' ;                      // 8        [V][T][P]  binary string
      varString          : result := 'String' ;                      // $0100               256 : Pascal string; not OLE compatible
      VT_DISPATCH        : result := 'IDispatch';                    // 9        [V][T]     IDispatch FAR*
      VT_ERROR           : result := 'Error';                        // 10       [V][T]     SCODE
      VT_BOOL            : result := 'Boolean' ;                     // 11       [V][T][P]  True=-1, False=0
      VT_VARIANT         : result := 'Variant';                      // 12       [V][T][P]  VARIANT FAR*
      VT_UNKNOWN         : result := 'Unknown';                      // 13       [V][T]     IUnknown FAR*
      VT_DECIMAL         : result := 'Decimal';                      // 14       [V][T][S]  16 byte fixed point
      15                 : result := 'Undefined';                    // 15                  undefined
      VT_I1              : result := 'signed char';                  // 16        [T]       signed char
      VT_UI1             : result := 'unsigned char';                // 17        [T]       unsigned char
      VT_UI2             : result := 'unsigned short';               // 18        [T]       unsigned short
      VT_UI4             : result := 'unsigned short';               // 19        [T]       unsigned short
      VT_I8              : result := 'signed 64-bit int';            // 20        [T][P]    signed 64-bit int
      VT_UI8             : result := 'unsigned 64-bit int';          // 21        [T]       unsigned 64-bit int
      VT_INT             : result := 'signed machine int';           // 22        [T]       signed machine int
      VT_UINT            : result := 'unsigned machine int';         // 23        [T]       unsigned machine int
      VT_VOID            : result := 'C style void';                 // 24        [T]       C style void
      VT_HRESULT         : result := 'HRESULT';                      // 25        [T]       HRESULT
      VT_PTR             : result := 'pointer type';                 // 26        [T]       pointer type
      VT_SAFEARRAY       : result := 'VT_ARRAY in VARIANT';          // 27        [T]       (use VT_ARRAY in VARIANT)
      VT_CARRAY          : result := 'C style array';                // 28        [T]       C style array
      VT_USERDEFINED     : result := 'user defined type';            // 29        [T]       user defined type
      VT_LPSTR           : result := 'null terminated string';       // 30        [T][P]    null terminated string
      VT_LPWSTR          : result := 'wide null terminated string';  // 31        [T][P]    wide null terminated string
      VT_FILETIME        : result := 'FILETIME';                     // 64           [P]    FILETIME
      VT_BLOB            : result := 'Length prefixed bytes';        // 65           [P]    Length prefixed bytes
      VT_STREAM          : result := 'Name of the stream follows';   // 66           [P]    Name of the stream follows
      VT_STORAGE         : result := 'Name of the storage follows';  // 67           [P]    Name of the storage follows
      VT_STREAMED_OBJECT : result := 'Stream contains an object';    // 68           [P]    Stream contains an object
      VT_STORED_OBJECT   : result := 'Storage contains an object';   // 69           [P]    Storage contains an object
      VT_BLOB_OBJECT     : result := 'Blob contains an object';      // 70           [P]    Blob contains an object
      VT_CF              : result := 'Clipboard format';             // 71           [P]    Clipboard format
      VT_CLSID           : result := 'A Class ID';                   // 72           [P]    A Class ID
      VT_VECTOR          : result := 'simple counted array';         // $1000        [P]    simple counted array
      VT_ARRAY           : result := 'SAFEARRAY*';                   // $2000  [V]          SAFEARRAY*
      VT_BYREF           : result := 'Byref';                        // $4000  [V]          Byref
      VT_RESERVED        : result := 'Reserved';                     // $8000               Reserved
      VT_ILLEGAL         : result := 'Illegal';                      // $ffff               Illegal
      VT_ILLEGALMASKED   : result := 'IllegalMask';                  // $0fff               IllegalMask
      else                 result := 'unknow ' + inttostr(VarType);
   end;
end ;

//------------------------------------------------------------------------------

function tt_GetDispatchName (const Disp : IDispatch) : string ;
var
   p : PWideChar ;
   TypeInfo: ITypeInfo;
   TypeAttr: PTypeAttr;
   Res: HResult ;
   strGuid : string ;
begin
   try
      res := disp.GetTypeInfo(0,0,TypeInfo) ;   // can generate strange exception (try except don't work)
      if not Succeeded(Res) then
         TypeInfo := nil ;  // ensure TypeInfo don't contain bad result
   except
      result := '' ;
   end ;

   if TypeInfo = nil then
      Exit;

   try
      OleCheck(TypeInfo.GetTypeAttr(TypeAttr));
   except
      result := '' ;
      exit ;
   end ;

   try
      StringFromCLSID(TypeAttr.guid, P);
      strGuid := P;
      CoTaskMemFree(P);

      result := '' ;
      with TRegistry.Create do
      try
         RootKey := HKEY_CLASSES_ROOT;
         OpenKey (Format ('Interface\%s', [strGuid]), False);
         result := ReadString ('');
         CloseKey ;
      finally
         Free ;
      end ;
   finally
      TypeInfo.ReleaseTypeAttr(TypeAttr);
   end ;
end ;

//------------------------------------------------------------------------------

procedure tt_GetDispathDescriptions (const Disp : IDispatch; var strGuid,strProg, strTypeLibGuid, strTypeLib : string) ;
var
   TypeInfo: ITypeInfo;
   TypeAttr: PTypeAttr;
   Result: HResult ;
begin
   strGuid := '' ;
   strProg := '' ;
   strTypeLibGuid := '' ;
   strTypeLib := '' ;

   try
      result := disp.GetTypeInfo(0,0,TypeInfo) ;
      if not Succeeded(Result) then
         TypeInfo := nil ;  // ensure TypeInfo don't contain bad result
   except
   end ;

   if TypeInfo = nil then
      Exit;

   OleCheck(TypeInfo.GetTypeAttr(TypeAttr));
   try
      tt_GetDispathDescriptions (TypeAttr , strGuid,strProg, strTypeLibGuid, strTypeLib) ;
   finally
      TypeInfo.ReleaseTypeAttr(TypeAttr);
   end ;
end ;

//------------------------------------------------------------------------------

procedure tt_GetDispathDescriptions (const TypeAttr : PTypeAttr; var strGuid,strProg, strTypeLibGuid, strTypeLib : string) ;
var
   p : PWideChar ;
   strTypeLibVersion : string ;
begin
   StringFromCLSID(TypeAttr.guid, P);
   strGuid := P;
   CoTaskMemFree(P);

   strTypeLibGuid := '' ;
   strTypeLib     := '' ;
   strProg        := '' ;
   with TRegistry.Create do
   try
      RootKey := HKEY_CLASSES_ROOT;
      OpenKey (Format ('Interface\%s', [strGuid]), False);
      strProg := ReadString ('');
      CloseKey ;

      OpenKey (Format ('Interface\%s\TypeLib', [strGuid]), False);
      strTypeLibGuid := ReadString ('') ;
      strTypeLibVersion := ReadString ('Version') ;
      CloseKey ;

      OpenKey (Format ('TypeLib\%s\%s', [strTypeLibGuid,strTypeLibVersion]), False);
      strTypeLib := ReadString ('') ;
      CloseKey ;
   finally
      Free ;
   end ;
end ;

//------------------------------------------------------------------------------

function tt_GetVarValue (const v: Variant): string; overload ;
var
   strType : String ;
begin
   result := tt_GetVarValue (v,strType) ;
end ;

//------------------------------------------------------------------------------

// return variant value and his type by reference
function tt_GetVarValue (const v: Variant ; var strType : String ) : string ;
var
   pc : pchar ;
   disp : IDispatch ;
   ptr : integer ;
   vt : word ;
begin
   result := '' ;
   vt := VarType(v) ;
   if (vt and VT_ARRAY) = VT_ARRAY then begin
      strType := 'Array ' ;
      exit ;
   end ;
   strType := tt_GetVariantType (vt);

   case vt of
      VT_EMPTY : result := 'Empty' ;       // better than display empty ('') string
      VT_NULL : result := 'Null' ;         // better than display empty ('') string

      VT_DISPATCH :
      begin
         disp := IDispatch (v);
         ptr := integer (disp) ;
         result := '$' + inttohex (ptr,8) ;
         // get the real type
//         if disp <> nil then
//            strType := tt_GetDispatchName (disp ) ;      // can generate strange exception (try except don't work) ...
      end ;

      VT_I2..VT_BSTR ,      // 2..8    : standard type
      varString ,           // $0100   : pascal string
      VT_BOOL   ,           // 11      : bool
      VT_I1..VT_UINT  :     // 16..23  : chars , shorts , int64
      begin
         result := v ;
         // Ajust string in case of bstr
         pc := Pchar (result) ;
         result := pc ;
      end ;
      else
      begin
         try
            result := v ;
         except
         end;
      end;
   end;
end ;

procedure TT_SetCurrentThreadName(const Name: AnsiString);
type
  TThreadNameInfo =
    record
      RecType: LongWord;
      Name: PAnsiChar;
      ThreadID: LongWord;
      Flags: LongWord;
    end;
var
  info:TThreadNameInfo;
begin
  // This code is extremely strange, but it's the documented way of doing it!
  info.RecType:=$1000;
  info.Name:=PAnsiChar(Name);
  info.ThreadID:=$FFFFFFFF;
  info.Flags:=0;
  try
    {$ifdef COMPILER_18_UP}
    RaiseException($406D1388, 0, SizeOf(info) div SizeOf(LongWord), PUINT_PTR (@info));  // PUINT_PTR >= XE4
    {$else}
    RaiseException($406D1388, 0, SizeOf(info) div SizeOf(LongWord), PDWord (@info));     // PDWord for delphi < XE4
    {$endif}
  except
  end;
end;
//------------------------------------------------------------------------------

initialization
   LoadCharTypes;          // this table first
   criticalSection := TCriticalSection.Create ;
   FormTraceList   := TObjectList.Create (true) ;  // internal list of form
   setMessageList := TObjectList.Create (false) ;  // don't own object
   getMessageList := TObjectList.Create (false) ;  // don't own object
   MsgThread := nil ;   // ensure MsgThread is nil before creating it.
   DefaultWinTrace := TInternalWinTrace.Create ;
   FormTraceList.Add(DefaultWinTrace) ;
   TTrace.start() ;     // create the Msg thread and run it
   LoadConfig() ;

finalization
   TTrace.stop() ; // terminated thread
   _WinTrace := nil ;
   criticalSection.Free ;
   setMessageList.Free;    // clear any remaining trace not send
   getMessageList.Free;    // clear any remaining trace not send
   FormTraceList.Free ;    // owner
   if _opt <> nil then
      _opt.Free ;
end.