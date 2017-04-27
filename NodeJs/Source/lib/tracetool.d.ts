

interface ITracetool {
    host                : string ;
    environment         : string ;
    waitingMessageCount : number ;
    sendMessageCount    : number ;
    clientId            : any ;
    winTrace            : IWinTrace ;
    watches             : IWinWatch ;
    debug               : ITraceToSend ;
    warning             : ITraceToSend ;
    error               : ITraceToSend ;
    classes             : Array<any> ;
    options             : ITraceOptions ;

    queryClientId() : void;
    show(isVisible :any) : void ;
    closeViewer() : void ;
    clearAll() : void ;
    find (text :string, sensitive:boolean, wholeWord:boolean , highlight:boolean, searchInAllPages:boolean) : void ;
}

interface ITraceClasses {
    FontDetail() : IFontDetail;     // no parameters
    Context():ITraceContext;        // no parameters
    TraceToSend():ITraceToSend;     // no parameters
    TraceNode(parentNode:string, generateUniqueId:boolean):ITraceNode;
    WinTrace(winTraceId?:string, winTraceText?:string):IWinTrace; 
    TraceNodeEx(parentNode?:string, generateUniqueId?:string):ITraceNodeEx; 
    TraceTable():ITraceTable;       // no parameters
    WinWatch(winWatchId?:string , winWatchText?:string):IWinWatch; 
    MemberNode(col1?:string, col2?:string, col3?:string):IMemberNode; 
}

interface ITraceToSend {
    id         :  string   ;
    iconIndex  : number ;
    enabled    : boolean ;
    winTraceId : string ;
    context    : ITraceContext;

    send (leftMsg:string,rightMsg?:string) : ITraceNode ;
    indent (leftMsg:string, rightMsg?:string, backGroundColor?:string, isEnter?:boolean) : void ;
    unIndent (leftMsg?:string, rightMsg?:string, backGroundColor?:string, isExit?:boolean) : void ;
    sendBackgroundColor(leftMsg:string, color:string, colId:number): ITraceNode;
    enterMethod  (leftMsg:string, rightMsg?:string, backGroundColor?:string) :void;
    exitMethod  (leftMsg?:string, rightMsg?:string, backGroundColor?:string) :void;
    sendValue (leftMsg:string, objToSend:any, maxLevel?:number, title?:string): ITraceNode ;
    sendObject  (leftMsg:string, objToSend:any, displayFunctions?:boolean): ITraceNode;
    sendStack (leftMsg?:string, level?:number) : ITraceNode;
    sendCaller (leftMsg?:string, level?:number) : ITraceNode;
    sendDump  (leftMsg:string, shortTitle:string, buffer:string, count?:number): ITraceNode;
    sendXml  (leftMsg:string, xml:string): ITraceNode;
    sendTable  (leftMsg:string, table:any): ITraceNode;
    indentLevel() : number ;
}

interface IWinTrace extends ITraceToSend {
    debug               : ITraceToSend ;
    warning             : ITraceToSend ;
    error               : ITraceToSend ;

    saveToTextFile(fileName:string) : void ;
    saveToXml(fileName:string,styleSheet?:string) : void ;
    loadXml(fileName:string) : void ;
    setLogFile(fileName:string, mode?:number, maxLines?:number) : void ;
    displayWin() : void ;
    setMultiColumn(mainColIndex?:number) : void ;
    setColumnsTitle(titles:string) : void ;
    setColumnsWidth(widths:string) : void ;
    gotoFirstNode() : void ;
    gotoLastNode() : void ;
    findNext(searForward:boolean) : void ;
    displayWin() : void ;
    gotoBookmark(pos:number) : void ;
    clearBookmark(searForward:boolean) : void ;
    clearFilter() : void ;
    addFilter(column:number , compare:number , text:string) : void ;
    applyFilter(conditionAnd:boolean,  showMatch:boolean,  includeChildren:boolean) : void ;
    clearAll() : void ;
    close() : void ;
}

interface IWinWatch {
    id         :  string   ;
    enabled    : boolean ;
    winWatchId : string ;

    displayWin() : void ;
    clearAll() : void ;
    close() : void ;
    send(watchName:string ,watchValue:any) : void ;
}

interface ITraceOptions {
    sendFunctions   : boolean ;
    sendDate        : boolean ;
    objectTreeDepth : number ;
}

interface IFontDetail {
    colId    : number ;
    bold     : boolean ;
    italic   : boolean;
    color    : string ;
    size     : number ;
    fontName : string ;
}

interface ITraceContext {
    contextList     : Array<any> ;
    winTraceContext : any ;

    getLast() : void;
    push (): void; 
    level (): void; 
    deleteLast (): void;
}

interface ITraceNode extends ITraceToSend {  
    resend (newLeftMsg?:string, newRightMsg?:string): ITraceNode;
    resendLeft (newLeftMsg:string): ITraceNode;
    resendRight (newRightMsg:string): ITraceNode;
    resendIconIndex (index:number): ITraceNode;
    setBackgroundColor (color:string, colId:number): ITraceNode;
    append (leftMsgtoAdd?:string, rightMsgtoAdd?:string): ITraceNode;
    appendLeft (leftMsgtoAdd:string): ITraceNode;
    appendRight (rightMsgtoAdd:string): ITraceNode;
    show (): ITraceNode;
    setSelected (): ITraceNode;
    deleteIt (): ITraceNode;
    deleteChildren (): ITraceNode;
    setBookmark (bookmarked:boolean): ITraceNode;
    setVisible (visible:boolean): ITraceNode;
    gotoNextSibling (): ITraceNode;
    gotoPrevSibling (): ITraceNode;
    gotoFirstChild (): ITraceNode;
    gotoLastChild (): ITraceNode;
    setFontDetail (colId?:any, bold?:boolean, italic?:boolean, color?:string, size?:number, fontName?:string): ITraceNode;
}

interface ITraceNodeEx {
    fontDetails  : Array<IFontDetail> ;
    leftMsg      : string ;
    rightMsg     : string ;
    time         : string ;
    threadName   : string ;
    members      : IMemberNode ;
    parentNodeId : string ;
    iconIndex    : number ;
    enabled      : boolean ;
    winTraceId   : string ;
    id           : string   ;

    addXML (xml:string): void;
    addDump (shortTitle: string, buffer: string, count?:number): void;
    addTable (table:any): void;
    resend (): void;
    addBackgroundColor (color: string, colId:number): void;
    addFontDetail (colId:any, bold?:boolean, italic?:boolean, color?:string, size?:number, fontName?:string): void;
    addValue (objToSend:any, maxLevel?:number, title?:string): void;
    addObject (objToSend:any, displayFunctions?:boolean): void;
    addCaller (level?:number): void;
    addStackTrace (level?:number): void;
    send (): ITraceNode;
}

interface ITraceTable {
    members : IMemberNode ;  
    addColumnTitle (colTitle : string): void;
    addRow (): void;
    addRowData (cell : string): void;
    copyToNodeMembers (nodeMembers : any): void;
}


interface IMemberNode {
    viewerKind :number;
    col1       : string;
    col2       : string;
    col3       : string;
    members    : Array<IMemberNode> ;
    fontDetails: Array<IFontDetail> ;

    add (col1?: string, col2?: string, col3?: string): IMemberNode;
    setFontDetail (colId?:any, bold?:boolean, italic?:boolean, color?:string, size?:number, fontName?:string): IMemberNode;
    addToStringList (commandList:Array<string>): void;
}


declare var ttrace : ITracetool;

