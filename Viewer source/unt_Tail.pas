{

  UNIX like Tail-f windows
  ========================
                                 
   Author : Thierry Parent

   HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
   Download :  http://sourceforge.net/projects/tracetool/
   See License.txt for license information   

}

unit unt_Tail;

interface

uses
  system.Contnrs , system.types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, pscMenu ,
  Dialogs, ExtCtrls, VirtualTrees, StdCtrls, ComCtrls , dirmon, ToolWin, unt_TraceWin,
  Buttons, Clipbrd, unt_base , unt_pageContainer ,  unt_editor , VstSort,unt_filter, unt_tool,
  generics.collections,
  Menus, untPrintPreview;

const
   //BUFSIZE = 1024 {400h} ;
   BUFSIZE = 130000  ;

type

  PTailRec = ^TTailRec ;
  TTailRec = record
     OriginalOrder  : cardinal ;     // Original order when inserted. Used to Unsort nodes
     Msg            : string ;       // a line
     Time           : string ;       // time of read (nothing to do with time of file write)
     Columns        : TStringList ;  // multi columns strings
  end ;

  TTail = class
     OriginalOrder  : cardinal ;     // Original order when inserted. Used to Unsort nodes
     Msg            : string ;       // a line
     Time           : string ;       // time of read (nothing to do with time of file write)
     Columns        : TStringList ;  // multi columns strings
  end;

  TBlockMem = class
    buf:array[1..BUFSIZE + 5] of byte;
    //Ptr : PAnsiChar ;
    //refCount : LongInt ;
  end;

  TFrmTail = class(TFrmBase)
    VstTail: TVirtualStringTree;
    GroupPanel: TPanel;
    PanelTraceInfo: TPanel;
    VSplitter: TSplitter;
    PanelTop: TPanel;
    butClose: TBitBtn;
    butReload: TBitBtn;
    TracesInfo: TLabel;
    VstDetail: TVirtualStringTree;
    butClearContent: TBitBtn;
    PanelGutter: TPanel;
    PopupTree: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Copycurrentcell1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    mnuTogglebookmark: TMenuItem;
    SelectAll1: TMenuItem;
    PopupDetail: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N2: TMenuItem;
    MenuItem1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure VstTailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure butReloadClick(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
    procedure VstTailFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstTailPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstDetailDblClick(Sender: TObject);
    procedure VstDetailMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstDetailPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstDetailEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VstTailHeaderDragged(Sender: TVTHeader; Column: TColumnIndex;
      OldPosition: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure butClearContentClick(Sender: TObject);
    procedure VstTailDblClick(Sender: TObject);
    procedure VstTailCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstTailEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstTailEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstTailKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstTailAfterPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure VstTailCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VstTailAfterItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
    procedure VstTailBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstTailAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VstDetailFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure PanelGutterDblClick(Sender: TObject);
    procedure VstTailMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstDetailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure VstTailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;  Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstTailEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
  private
    Sorter : TVstSort ;
    LastChildOrder : cardinal ;     // Order of the last child, used to insert sub nodes and unsort them
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
    procedure WMStartEditingTrace(var Message: TMessage); message WM_STARTEDITING_TRACE;
    function AddTrace(PtrBeg,PtrEnd: PAnsiChar; Ontop,IsNotCompleted : boolean): pVirtualNode;
    function getDelimitedStringsfromLine (SourceBegin : pAnsiChar; SourceEnd : pAnsiChar = nil) : TStringList ; overload ;
    function getDelimitedStringsfromWLine (SourceBegin : pChar; SourceEnd : pChar = nil) : TStringList ; overload ;
    procedure readFirstLine ;
    function GotoBeginLine(source, bufferBegin: PAnsiChar): PAnsiChar;
    function GotoEndOfPreviousLine(source, bufferBegin: PAnsiChar): PAnsiChar;
    function IsEOL(source: PAnsiChar): boolean;
    function CheckSearchRecord(   TailRec : PTailRec): boolean;
    procedure AddSeparator;
    procedure AddRedComment(comment: string);
    { Private declarations }
  public   // directory monitor
    Gutter: TImage;
    NodeToFocus : PVirtualNode ;
    IsPaused : boolean ;
    DirMon : TDirMon;
    lastpos : Longint  ;
    TitleOffset : Longint ;
    LastModified : tDateTime ;
    FileStatus : string ;
    File_Size : integer ;
    TailFile : string ;
    IsColumnWidthChanged : boolean ;
    NotCompleteNode : PVirtualNode ;
    NotCompleteIndex : integer ;
    BlockMemList : TList<TBlockMem> ;

    // how to display each lines :
    ShowTimeAndLines            : boolean ;
    ShowOnlyLines               : boolean ;
    ShowManycolumns             : boolean ;
    DetectSeparatorOnfirstLine  : boolean ;
    DetectSeparatorOnEachLine   : boolean ;
    DetectTitleOnFirstLine      : boolean ;
    IsFixedColumns              : boolean ;
    FixedColCount               : integer ;
    TextQualifier               : ansiChar ;
    ColSeparator                : ansiChar ;
    // ColCount : integer ;

  public
    procedure DirMon1Created(Sender: TObject; FileName: String);
    procedure DirMon1Deleted(Sender: TObject; FileName: String);
    procedure DirMon1Modified(Sender: TObject; FileName: String);
    procedure InitColumns ;
    procedure Display;
    procedure LoadFile;
    procedure ClearFileContent ;

  public // TFrmBase
    procedure Print ; override ;
    procedure ClearWin ; override ;
    procedure SaveWin ; override ;
    procedure PauseWin ; override ;
    procedure ViewTraceInfo ; override ;
    procedure ViewProperty ; override ;
    procedure CopySelected ; override ;
    procedure CopyCurrentCell ; override ;
    procedure DeleteSelected ; override ;
    procedure SelectAll ; override ;
    procedure CheckAutoClear ; override ;
    procedure PageControlChange (); override ;
    procedure TimerInfo ; override ;
    procedure CloseWin ;  override ;
    procedure ResizeColumns ;  override ;
    procedure RefreshView ;       override ;
    procedure ShowFilter ;        override ;
    procedure ApplyFont ; override ;
    function  getMembers(Node : PVirtualNode) : TMember ; override ;
    function  SearchNext(start:boolean) : boolean ;        override ;
    function  SearchPrevious (atEnd:boolean) : boolean ;  override ;
  end;

var
  FrmTail: TFrmTail;

  
implementation

uses
  debugOptions
  , unt_TraceConfig
  , unt_utility
  , Unt_TailProgress
  , unt_search
  , unt_selectTail;


{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrmTail.FormCreate(Sender: TObject);
begin
   inherited ;
   ApplyFont() ;  // set font name and size for the 2 trees (from XMLConfig)

   vst := VstTail ;
   with TPSCMenu.create (self) do begin
      DimLevel := 0 ;    // don't gray icon
      Active := true ;
   end ;

   // initialize sort
   LastChildOrder := 1 ;   // 0 is reserved for not yet ordered lines
   Sorter := TVstSort.create (self) ;
   Sorter.tree := VstTail ;
   Sorter.UtilityImages := Frm_Tool.UtilityImages ;
   Sorter.canUnsort := true ;

   // redirect some events to the sorter
   VstTail.onHeaderClick             := sorter.OnHeaderClick ;
   VstTail.OnKeyUp                   := sorter.OnKeyUp ;
   VstTail.onHeaderDrawQueryElements := sorter.OnHeaderDrawQueryElements ;
   VstTail.onAdvancedHeaderDraw      := sorter.OnAdvancedHeaderDraw ;
   // tips : don't forget to include the hoOwnerDraw in the VstTail.Header.Options

   TailList.Add (self) ;

   // copy all options from main form
   VstTail.Colors.UnfocusedSelectionColor       := Frm_Trace.vstTrace.Colors.UnfocusedSelectionColor ;
   VstTail.Colors.UnfocusedSelectionBorderColor := Frm_Trace.vstTrace.Colors.UnfocusedSelectionBorderColor ;
   VstTail.NodeDataSize := sizeof (TTailRec) ;
   VstTail.TreeOptions.AutoOptions      := Frm_Trace.vstTrace.TreeOptions.AutoOptions ;
   VstTail.TreeOptions.SelectionOptions := Frm_Trace.vstTrace.TreeOptions.SelectionOptions ;
   VstTail.TreeOptions.MiscOptions      := Frm_Trace.vstTrace.TreeOptions.MiscOptions ;
   VstTail.TreeOptions.PaintOptions     := Frm_Trace.vstTrace.TreeOptions.PaintOptions
                                           - [toShowRoot] ; // don't show Root

   VstTail.Header.Options := VstTail.Header.Options
      + [hoColumnResize]     // resizing columns is allowed
      + [hoDblClickResize]   // allows a column to resize itself to its largest entry
      + [hoDrag]             // dragging columns is allowed
      + [hoRestrictDrag]     // header can only be dragged horizontally
      + [hoOwnerDraw]        // needed for sort : header items with the owner draw style can be drawn by the application via event
      + [hoVisible]          // header is visible

      - [hoAutoResize]       // adjust a column so that the header never exceeds client width of owner control
      - [hoHotTrack]         // header captions are highlighted when mouse is over a particular column
      - [hoShowHint]         // show application defined header hint
      - [hoShowImages]       // show images
      - [hoShowSortGlyphs] ; // show sort glyphs


   VstDetail.NodeDataSize := sizeof (TDetailRec) ;
   VstDetail.Header.MainColumn := 0 ;
   VstDetail.Header.AutoSizeIndex := -1 ;     // 2
   VstDetail.Header.Columns.Items[0].text := '' ;   // header must be visible to enable resize !
   VstDetail.Header.Columns.Items[1].text := '' ;
   VstDetail.Header.Options               := Frm_Trace.VstDetail.Header.Options ;
   VstDetail.TreeOptions.AutoOptions      := Frm_Trace.VstDetail.TreeOptions.AutoOptions ;
   VstDetail.TreeOptions.PaintOptions     := Frm_Trace.VstDetail.TreeOptions.PaintOptions ;
   VstDetail.TreeOptions.SelectionOptions := Frm_Trace.VstDetail.TreeOptions.SelectionOptions ;
   VstDetail.TreeOptions.MiscOptions      := Frm_Trace.VstDetail.TreeOptions.MiscOptions ;
   VstDetail.Colors.UnfocusedSelectionColor       := Frm_Trace.vstTrace.Colors.UnfocusedSelectionColor ;
   VstDetail.Colors.UnfocusedSelectionBorderColor := Frm_Trace.vstTrace.Colors.UnfocusedSelectionBorderColor ;


   DirMon := TDirMon.Create (self);  // owned by form
   DirMon.OnCreated := DirMon1Created ;
   DirMon.OnDeleted := DirMon1Deleted ;
   DirMon.OnModified := DirMon1Modified ;
   DirMon.WatchSubtree := false ;
   DirMon.WatchFilters :=  [nfFILE_NAME , nfDIR_NAME , nfSIZE , nfLAST_WRITE] ;
   IsPaused := false ;
   File_Size := 0 ;
   lastpos := 0 ;
   FileStatus := '' ;
   LastModified := now ;
   NotCompleteNode := nil ;
   BlockMemList := TObjectList<TBlockMem>.create(true) ;   // owner

end;

//------------------------------------------------------------------------------

procedure TFrmTail.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   inherited;
   BlockMemList.Free ;      // owner
   TailList.Remove (self) ;
   if filter <> nil then
      Filter.Free ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.DirMon1Created(Sender: TObject; FileName: String);
begin
   if StrIComp(PWideChar(ExtractFileName(TailFile)), PWideChar(FileName)) = 0 then begin
      FileStatus := '' ;
      AddRedComment ('File created at ' +  FormatDateTime('hh:mm:ss:zzz',now)) ;
      AddSeparator() ; // add line separator
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.DirMon1Modified(Sender: TObject; FileName: String);
begin
   if StrIComp(PWideChar(ExtractFileName(TailFile)), PWideChar(FileName)) = 0 then begin
      LastModified := now ;
      FileStatus := '' ;
      Display() ;  // load chars from the latest position
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.DirMon1Deleted(Sender: TObject; FileName: String);
begin
   if StrIComp(PWideChar(ExtractFileName(TailFile)), PWideChar(FileName)) = 0 then begin
      FileStatus := 'File deleted' ;
      lastpos := 0 ;
      LastModified := now ;
      AddRedComment ('File deleted at ' +  FormatDateTime('hh:mm:ss:zzz',now)) ;
      AddSeparator() ;  // add line separator
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.butReloadClick(Sender: TObject);
begin
   VstTail.Clear ;
   LastChildOrder := 1 ;   // 0 is reserved for not yet ordered lines
   NotCompleteNode := nil ;
   lastpos := 0 ;
   loadfile() ;
   SetActivePage() ;
end;

//------------------------------------------------------------------------------
// called by TFrmSelectTail.LoadFile
procedure TFrmTail.InitColumns;
var
   col : TVirtualTreeColumn ;
   c : integer ;
begin
   TitleOffset := 0 ;
   // normal way to display lines
   if ShowTimeAndLines then begin
      VstTail.Header.MainColumn := 1 ;
      VstTail.Header.AutoSizeIndex := -1 ;  // auto
   end else if ShowOnlyLines then begin   // don't display time
      VstTail.header.Columns.Delete(0);
      VstTail.Header.MainColumn := 0 ;
      VstTail.FocusedColumn := 0 ;
   end else begin // ShowManycolumns

      if DetectSeparatorOnfirstLine then begin
         readFirstLine () ;
      end else if DetectSeparatorOnEachLine then begin
         VstTail.header.Columns.Clear ;
      end else if IsFixedColumns then begin
         VstTail.header.Columns.Clear ;
         for c := 0 to FixedColCount-1 do begin
            col := VstTail.header.Columns.Add ;
            col.options  := col.options + [coAllowFocus] ;  // ensure user can focus to this column
            col.MinWidth := 50 ;
            col.MaxWidth := 10000 ;
            //col.Width := 100 ;
            col.Text  := 'Col ' + inttostr (c) ;
         end ;
         VstTail.Header.MainColumn := 0 ; // FixedColCount-1 ;
         VstTail.Header.AutoSizeIndex := -1 ;  // auto
         // size must be changed after adding all columns
         for c := 0 to FixedColCount-1 do begin
            col := VstTail.header.Columns[c] ;
            if c = FixedColCount-1 then
               col.Width := 3000
            else
               col.Width := 100 ;
         end ;
      end ;
   end ;
   LoadFile() ;
end;

//------------------------------------------------------------------------------

// called by InitColumns
procedure TFrmTail.readFirstLine ;
var
   f:file of byte;
   buf:array[1..65005] of byte;    // AnsiChar
   readchars:integer;
   PtrBeg,PtrEnd , EndBuffer: pAnsichar ;
   strCols : TStringList ;
   col : TVirtualTreeColumn ;
   c : integer ;
begin
   if self.IsPaused then
      exit ;

   if not fileExists(TailFile) then
     exit;

   assignfile(f,TailFile);
   filemode:=0;
   reset(f);

   File_Size := filesize(f) ;

   if lastpos >= File_Size then begin
      closefile(f);
      exit ;
   end ;

   seek(f,0) ;

   // 5000 bytes (AnsiChar) for the first line should be enough
   blockread (f , buf , 5000 , readchars);
   buf[readchars+1] := 0 ;

   PtrBeg := pAnsiChar(@buf) ;
   ptrEnd := PtrBeg ;
   EndBuffer := PtrBeg + readchars ;

   // loop until we found end of line
   while (true ) do begin
      if (ptrEnd >= EndBuffer) then
         break ;
      if (ptrEnd^ = #13) and ((ptrEnd+1)^ = #10) then begin   // CR + LF
         ptrEnd^ := #0 ;
         break ;
      end else if (ptrEnd^ = #13) then begin                // CR
         ptrEnd^ := #0 ;
         break ;
      end else if (ptrEnd^ = #10) then begin                // LF
         ptrEnd^ := #0 ;
         break ;
      end else begin
         inc (ptrEnd);
      end ;
   end ;

   closefile(f);
   lastpos := ptrEnd - PtrBeg ;
   TitleOffset := lastpos ;
   dec(ptrEnd) ;
   strCols := getDelimitedStringsfromLine (PtrBeg, ptrEnd) ;    // Ansi version

   VstTail.header.Columns.Clear ;
   for c := 0 to strCols.Count-1 do begin
      col := VstTail.header.Columns.Add ;
      col.options  := col.options + [coAllowFocus] ;  // ensure user can focus to this column
      if DetectTitleOnFirstLine then
         col.Text := strCols.Strings[c] ;
      col.MinWidth := 10 ;
      col.MaxWidth := 10000 ;
      col.Width := 100 ;
   end ;
   VstTail.Header.MainColumn := 0 ; // FixedColCount-1 ;
   VstTail.Header.AutoSizeIndex := -1 ;  // auto

   // size must be changed after adding all columns
   for c := 0 to strCols.Count-1 do begin
      col := VstTail.header.Columns[c] ;
      if c = strCols.Count-1 then
         col.Width := 3000
      else
         col.Width := 100 ;
   end ;
   strCols.free ;

   //AutosizeAll (VstTail) ;      // use only the header text to determine column width
end ;

//------------------------------------------------------------------------------

// line must finish by zero or cr / crlf
function TFrmTail.getDelimitedStringsfromLine(SourceBegin : pAnsiChar; SourceEnd : pAnsiChar = nil): TStringList;
var
   PtrBeg,ptrEnd : pAnsiChar ;
begin
   result := TStringList.create ;

   ptrEnd := SourceBegin ;
   if TextQualifier = '' then begin
      PtrBeg := ptrEnd ;
      while (true ) do begin
         if (SourceEnd = nil) and (ptrEnd^ = #0) then begin
            result.Add(String(PtrBeg)) ;
            break ;
         end else if (ptrEnd^ = #0) then begin  // replace bad char with a '0' char
            ptrEnd^ := '0' ;
         end ;

         if (SourceEnd <> nil) and (ptrEnd >= SourceEnd) then begin
            result.Add(String(PtrBeg)) ;
            break ;
         end ;

         if (ptrEnd^ = ColSeparator) then begin       // separator
            ptrEnd^ := #0 ;
            result.Add(String(PtrBeg)) ;
            ptrEnd^ := ColSeparator ;                 // restore separator
            inc (ptrEnd) ;
            PtrBeg := ptrEnd ;
         end else begin
            inc (ptrEnd);
         end ;
      end ;
   end else begin
      // textQualifier is a single quote or double quote
      // perform 3 steps :
      // 1 : search the first textQualifier
      // 2 : search the second textqualifier
      // 3 : search the colum delimiter

      while (true ) do begin
         PtrBeg := ptrEnd ;
         // step 1 : search Q1
         while (true ) do begin
            if (ptrEnd^ = #0) then begin                          // empty line ?
               result.Add(String(PtrBeg)) ;
               exit ;
            end else if (ptrEnd^ = TextQualifier) then begin      // TextQualifier Q1
               inc (ptrEnd) ;
               PtrBeg := ptrEnd ;
               break ;
            end else if (ptrEnd^ = ' ') then begin                // space before TextQualifier Q1
               inc (ptrEnd) ;
               break ;
            end else begin                                      // chars before Q1 ?
               break ;
            end ;
         end ;     // loop 1

         // step 2 : search Q2
         while (true ) do begin
            if (ptrEnd^ = #0) then begin                          // empty line ?
               result.Add(String(PtrBeg)) ;
               exit ;
            end else if (ptrEnd^ = TextQualifier) then begin      // TextQualifier Q2
               ptrEnd^ := #0 ;
               result.Add(String(PtrBeg)) ;
               inc (ptrEnd) ;
               break ;
            end else begin                                      // normal char
               inc (ptrEnd);
            end ;
         end ;       // loop 2

         // step 3 : search col separator
         while (true ) do begin
            if (ptrEnd^ = #0) then begin                          // no more col
               exit ;
            end else if (ptrEnd^ = ' ') then begin                // space before TextQualifier Q1
               inc (ptrEnd) ;
            end else if (ptrEnd^ = ColSeparator) then begin       // TextQualifier Q2
               inc (ptrEnd) ;
               break ;
            end else begin                                      // bad line , restart steps from here
               break;
            end ;
         end ;         // loop 3

      end ;       // loop all steps
   end ;          // text qualifier
end;

//------------------------------------------------------------------------------
// called from readFirstLine
// convert a Single byte string to an unicode string
function TFrmTail.getDelimitedStringsfromWLine(SourceBegin : pWideChar; SourceEnd : pWideChar = nil): TStringList;
var
   PtrBeg,ptrEnd : pWideChar ;
begin
   result := TStringList.create ;

   if SourceBegin = nil then
      exit ;

   ptrEnd := SourceBegin ;
   if TextQualifier = '' then begin
      PtrBeg := ptrEnd ;
      while (true ) do begin
         if (SourceEnd = nil) and (ptrEnd^ = #0) then begin
            result.Add(String(PtrBeg)) ;
            break ;
         end else if (ptrEnd^ = #0) then begin  // replace bad char with a '0' char
            ptrEnd^ := '0' ;
         end ;

         if (SourceEnd <> nil) and (ptrEnd >= SourceEnd) then begin
            //result.Add(String(PtrBeg)) ;
            result.Add('test') ;
            break ;
         end ;

         if (ptrEnd^ = char(ColSeparator)) then begin       // separator
            ptrEnd^ := #0 ;
            result.Add(String(PtrBeg)) ;

            ptrEnd^ := char(ColSeparator) ;    // restore separator
            inc (ptrEnd) ;
            PtrBeg := ptrEnd ;
         end else begin
            inc (ptrEnd);
         end ;
      end ;
   end else begin
      // textQualifier is a single quote or double quote
      // perform 3 steps :
      // 1 : search the first textQualifier
      // 2 : search the second textqualifier
      // 3 : search the colum delimiter

      while (true ) do begin
         PtrBeg := ptrEnd ;
         // step 1 : search Q1
         while (true ) do begin
            if (ptrEnd^ = #0) then begin                          // empty line ?
               result.Add(String(PtrBeg)) ;
               exit ;
            end else if (ptrEnd^ = char(TextQualifier)) then begin      // TextQualifier Q1
               inc (ptrEnd) ;
               PtrBeg := ptrEnd ;
               break ;
            end else if (ptrEnd^ = ' ') then begin                // space before TextQualifier Q1
               inc (ptrEnd) ;
               break ;
            end else begin                                      // chars before Q1 ?
               break ;
            end ;
         end ;     // loop 1

         // step 2 : search Q2
         while (true ) do begin
            if (ptrEnd^ = #0) then begin                          // empty line ?
               result.Add(String(PtrBeg)) ;
               exit ;
            end else if (ptrEnd^ = char(TextQualifier)) then begin      // TextQualifier Q2
               ptrEnd^ := #0 ;
               result.Add(String(PtrBeg)) ;
               inc (ptrEnd) ;
               break ;
            end else begin                                      // normal char
               inc (ptrEnd);
            end ;
         end ;       // loop 2

         // step 3 : search col separator
         while (true ) do begin
            if (ptrEnd^ = #0) then begin                          // no more col
               exit ;
            end else if (ptrEnd^ = ' ') then begin                // space before TextQualifier Q1
               inc (ptrEnd) ;
            end else if (ptrEnd^ = char(ColSeparator)) then begin       // TextQualifier Q2
               inc (ptrEnd) ;
               break ;
            end else begin                                      // bad line , restart steps from here
               break;
            end ;
         end ;         // loop 3

      end ;       // loop all steps
   end ;          // text qualifier
end;

//------------------------------------------------------------------------------

function TFrmTail.GotoBeginLine (source: PAnsiChar ; bufferBegin : PAnsiChar): PAnsiChar;
begin
   result := source ;
   // loop until we found begin of line
   while (result > bufferBegin ) do begin
      if (result > bufferBegin+1) and  ((result-2)^ = #13) and ((result-1)^ = #10) then begin   // CR + LF
         break ;
      end else if ((result-1)^ = #13) then begin                       // CR
         break ;
      end else if ((result-1)^ = #10) then begin                       // LF
         break ;
      end else begin
         dec (result);
      end ;
   end ;
end;

//------------------------------------------------------------------------------

function TFrmTail.IsEOL(source: PAnsiChar): boolean;
begin
   if (source^ = #0) then
      result := true  // not found
   else if ((source-1)^ = #13) and (source^ = #10) then  // CR + LF
      result := true
   else if (source^ = #13) then                          // CR
      result := true
   else if (source^ = #10) then                          // LF
      result := true
   else
      result := false ;
end ;

//------------------------------------------------------------------------------

// go one or two char back (end of previous line)
// source must point on a EOL before calling GotoEndOfPreviousLine
function TFrmTail.GotoEndOfPreviousLine(source: PAnsiChar; bufferBegin : PAnsiChar): PAnsiChar ;
begin
   result := source ;
   if source = bufferBegin then
      exit ;
   if (source > bufferBegin+1) and  ((source-1)^ = #13) and ((source)^ = #10) then  // CR + LF
      result := source-2
   else if (source^ = #13) then                          // CR
      result := source-1
   else if (source^ = #10) then                          // LF
      result := source-1 ;
end ;

//------------------------------------------------------------------------------

// called by butReloadClick() or TFrmSelectTail.butOpenClick
procedure TFrmTail.LoadFile ;
var
   f:file of byte;
   //buf:array[1..BUFSIZE + 5] of byte;
   blockMem : TBlockMem ;

   readchars:integer;
   PtrBeg, PtrEnd : PAnsichar ;
   iores : Integer ;
   nbBlockToRead, BeginBlock, EndBlock : integer ;
   NbLinesRead : integer ;
   IsLastBlock : boolean ;
   NotCompleted : boolean ;
   LastLine : PVirtualNode ;
   LastShowTime : integer ;

   procedure CheckOrder ;
   var
      TailRec : PTailRec ;
      node : pvirtualNode ;
   begin
      Node := VstTail.GetFirst;
      while Node <> nil do begin
         TailRec := VstTail.GetNodeData(node) ;
         if TailRec.OriginalOrder = 0 then begin
            TailRec.OriginalOrder := LastChildOrder ;
            inc (LastChildOrder) ;
         end ;
         Node := Node.NextSibling ;
      end ;
   end ;

   procedure CheckOrder2 ;
   var
      TailRec : PTailRec ;
      node : pvirtualNode ;
   begin
      Node := VstTail.GetLastChild(nil);
      while Node <> nil do begin
         TailRec := VstTail.GetNodeData(node) ;
         if TailRec.OriginalOrder = 0 then begin
            TailRec.OriginalOrder := LastChildOrder ;
            inc (LastChildOrder) ;
         end ;
         Node := Node.PrevSibling ;
      end ;
   end ;


   procedure ParseBlock ;
   begin

      blockMem.buf[readchars+1] := 0;
      PtrBeg := pAnsichar(@blockMem.buf) ;        // buf : array of byte
      PtrEnd := PtrBeg + readchars -1 ;

      //(blockMem.Ptr + readchars)^ := #0;
      //PtrBeg := blockMem.Ptr  ;
      //PtrEnd := PtrBeg + readchars -1 ;

      NotCompleted := false ;
      if IsLastBlock then begin
         lastpos := FilePos (f) ;

         // For the last block and for the last line, EOL should exist.
         // if EOL is not found, the pointer must be moved to previous line
         if not IsEol (PtrEnd) then begin
            ///PtrBeg := PtrEnd ;      // remain last position
            ///PtrEnd := GotoBeginLine (PtrEnd,  pchar(@buf)) ; // move back
            ///dec (PtrEnd) ; // go to EOL
            ///lastpos := lastpos - (PtrBeg - PtrEnd) ;         // substract difference
            NotCompleted := true ;
         end ;
         IsLastBlock := false ;
      end ;

      // go back just one or 2 char (cr/lf) if needed
      PtrEnd := GotoEndOfPreviousLine (PtrEnd,  pAnsiChar(@blockMem.buf)) ;
      //PtrEnd := GotoEndOfPreviousLine (PtrEnd,  blockMem.Ptr) ;

      // search the begin of line
      PtrBeg := GotoBeginLine (PtrEnd,  PAnsiChar(@blockMem.buf)) ;
      //PtrBeg := GotoBeginLine (PtrEnd,  blockMem.Ptr) ;

      while true do begin
         // replace cr lf with null terminator
         (PtrEnd+1)^ := #0 ;

         // add a line to the tail tree and call CheckAutoClear
         AddTrace (PtrBeg,PtrEnd, true,NotCompleted) ;
         if NotCompleted then
            LastLine := NotCompleteNode ;

         NotCompleted := false ;
         inc (NbLinesRead) ;

         // display progress bar and check if user press Stop
         if DateTimeToTimeStamp(now).Time - LastShowTime > 1000 then begin
            LastShowTime := DateTimeToTimeStamp(now).Time ;
            application.ProcessMessages ;
            FrmTailProgress.LabelLinesRead.caption := intToStr(NbLinesRead) ;
            if FrmTailProgress.IsStoped then
               break ;
         end ;

         // stop reading file when max lines is loaded
         if (TraceConfig.Tail_AutoClear) and (NbLinesRead >= TraceConfig.Tail_MaxNode) then begin
            EndBlock := 0 ;  // quit the 2 loops
            break ;
         end ;

         // PtrBeg point to the begin of the current line added to the tree

         // calculate new Endblock.
         EndBlock := BeginBlock + (PtrBeg - pchar(@blockMem.buf)) -1 ;
         //EndBlock := BeginBlock + (PtrBeg - blockMem.Ptr) -1 ;

         // EndBlock point now point to CRLF

         if EndBlock <= TitleOffset+1 then
            break ;

         // go to EOL
         dec (PtrBeg) ;


         // go back just one or 2 char (cr/lf)
         PtrEnd := GotoEndOfPreviousLine (PtrBeg,  PAnsiChar(@blockMem.buf)) ;
         //PtrEnd := GotoEndOfPreviousLine (PtrBeg,  blockMem.Ptr) ;


         // check if no more lines
         if PtrEnd = PAnsiChar(@blockMem.buf) then begin
         //if PtrEnd = blockMem.Ptr then begin
            if BeginBlock = 0 then  // begin of file (block start at zero)
               EndBlock := 0 ;      // force quit the 2 loops
            break ;    // read previous line
         end ;

         // PtrEnd is not at te begin of the buffer. Some chars (or maybe some lines are on the buffer)
         PtrBeg := GotoBeginLine (PtrEnd,  PAnsiChar(@blockMem.buf)) ;
         //PtrBeg := GotoBeginLine (PtrEnd,  blockMem.Ptr) ;

         if PtrBeg = PAnsiChar(@blockMem.buf) then begin
         //if PtrBeg = blockMem.Ptr then begin
            if BeginBlock <> 0 then  // if not begin of file (block start at zero)
               break ;          // read previous block starting at the end of the current line
            // else it's the first line of the first block. Will be read at next loop
         end ;
      end ;
   end ;

begin
   IsColumnWidthChanged := false ;
   if not fileExists(TailFile) then
     exit;

   {$I-}
   AssignFile(F, TailFile);
   FileMode := fmOpenRead;  {Set file access to read only }
   Reset(F);
   {$I+}
   iores := IOResult ;
   if iores <> 0 then begin
      //Frm_Trace.InternalTrace('Reset(F) : ' + inttostr(iores)) ;
      exit ;
   end ;

   LastShowTime := DateTimeToTimeStamp(now).Time ;
   LastLine := nil ;

   VstTail.BeginUpdate ;
   try
      File_Size := filesize(f) ;

      // if another file is copied to the watched file, the size become zero before getting the real size
      if File_Size = 0 then begin
         Sleep(200);
         File_Size := filesize(f) ;
      end ;

      NotCompleteNode := nil ;
      NbLinesRead := 0 ;
      nbBlockToRead := (File_Size div BUFSIZE) + 5 ;

      if FrmTailProgress = nil then
         FrmTailProgress := TFrmTailProgress.Create(Application);
      FrmTailProgress.ProgressBar.Min := 1 ;
      FrmTailProgress.ProgressBar.Max := nbBlockToRead +1;
      FrmTailProgress.ProgressBar.position := 0 ;
      FrmTailProgress.ProgressBar.Step := 1 ;
      FrmTailProgress.LabelLinesRead.caption := '0' ;
      FrmTailProgress.IsStoped := false ;
      FrmTailProgress.Show ;

      if File_Size < BUFSIZE then
         BeginBlock := 0
      else
         BeginBlock := File_Size-BUFSIZE ;

      EndBlock := File_Size ;
      IsLastBlock := true ;

      while EndBlock > TitleOffset do begin
         FrmTailProgress.ProgressBar.StepIt ;
         application.ProcessMessages ;

         //Frm_Trace.InternalTrace('Read from ' + inttostr (BeginBlock) + ' to ' + inttostr (EndBlock) + ' (' + inttostr (EndBlock-BeginBlock+1) + ' chars)');
         seek(f,BeginBlock) ;


         blockMem := TBlockMem.Create ;
         BlockMemList.Add(blockMem) ;

         blockread (f,blockMem.buf,EndBlock-BeginBlock,readchars);
         //blockread (f,blockMem.Ptr, 5 {EndBlock-BeginBlock} ,readchars);

         ParseBlock() ;

         if EndBlock <= TitleOffset+1 then
            break ;

         if FrmTailProgress.IsStoped then
            break ;

         BeginBlock := EndBlock -  BUFSIZE ;

         if BeginBlock < 0 then
            BeginBlock := 0 ;
      end ;  // while EndBlock > 0

      // convert all TTailRec.OriginalOrder having value 0 to an incremental value
      CheckOrder2() ;
      Sorter.sort (nil) ;  // Unsort
      CheckAutoClear() ;
      if IsColumnWidthChanged then begin
         //AutosizeAll(VstTail);
         IsColumnWidthChanged := false ;
      end ;

   finally
      if LastLine <> nil then
         NotCompleteNode := LastLine ;

      FrmTailProgress.hide ;
      VstTail.endUpdate ;
      closefile(f);
      BlockMemList.Clear ;   // owner
   end ;
end ;

//------------------------------------------------------------------------------

procedure TFrmTail.butClearContentClick(Sender: TObject);
begin
  ClearFileContent() ; // getPageContainer.actClearFileContent.Execute ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.ClearFileContent;
var
   f:file of byte;
begin

   if Application.MessageBox('Are you sure to erase file content ', pchar(TailFile), MB_YESNO) <> IDYES	then
      exit ;
   if not fileexists(TailFile) then
      exit;
   {$I-}
   AssignFile(F, TailFile);
   FileMode := fmOpenWrite;  {Set file access to write only }
   Rewrite(F);
   {$I+}

   closefile(f);
   // reset screen is done later when file size is detected. 
end;

//------------------------------------------------------------------------------
// called when file is modified ,using the main thread. No need to use message stack to add to the tree
procedure TFrmTail.Display ;
var
   f:file of byte;
   //buf:array[1..65005] of byte;
   blockMem : TBlockMem ;
   readchars:integer;
   PtrBeg, PtrEnd , EndBuffer : PAnsiChar ;
   seekPos : integer ;
   iores : Integer ;
begin

   IsColumnWidthChanged := false ;
   if self.IsPaused then
      exit ;

   if not fileexists(TailFile) then
     exit;

   {$I-}
   AssignFile(F, TailFile);
   FileMode := fmOpenRead;  {Set file access to read only }
   Reset(F);
   {$I+}
   iores := IOResult ;
   if iores <> 0 then begin
      //Frm_Trace.InternalTrace('Reset(F) : ' + inttostr(iores)) ;
      exit ;
   end ;

   //TFrm_Trace.InternalTrace(FormatDateTime('hh:mm:ss:zzz',now), 'begin display. Lines : ' + inttostr(LastChildOrder)) ;
   VstTail.BeginUpdate ;
   try
      while true do begin    // loop until end of file
         File_Size := filesize(f) ;

         // if another file is copied to the watched file, the size become zero before getting the real size
         if File_Size = 0 then begin
            Sleep(200);
            File_Size := filesize(f) ;
         end ;

         // no change (supposed)
         if lastpos = File_Size then
            break ;

         // special case : if new file size is lower than saved saved : we can supose that the file was rewrited.
         if lastpos > File_Size then begin
            LastChildOrder := 1 ;   // 0 is reserved for not yet ordered lines
            VstTail.Clear ;
            lastpos := 0 ;
         end ;

         seekPos := lastPos ;
         seek(f,seekPos) ;

         blockMem := TBlockMem.Create ;
         BlockMemList.Add(blockMem) ;
         //blockMem.refCount := 0 ;

         //TFrm_Trace.InternalTrace(FormatDateTime('hh:mm:ss:zzz',now), 'before read') ;
         blockread (f,blockMem.buf,BUFSIZE,readchars);
         //TFrm_Trace.InternalTrace(FormatDateTime('hh:mm:ss:zzz',now), 'BlockRead ' + inttostr (readchars)) ;

         blockMem.buf[readchars+1] := 0;

         PtrBeg := PAnsiChar(@blockMem.buf) ;
         PtrEnd := PtrBeg ;
         EndBuffer := PtrBeg + readchars ;

         lastpos := FilePos (f) ;

         // loop until all bytes in the buffer are read
         while (true ) do begin
            if (ptrEnd >= EndBuffer) then
               break ;
            if (ptrEnd^ = #13) and ((ptrEnd+1)^ = #10) then begin   // CR + LF
               ptrEnd^ := #0 ;
               AddTrace(PtrBeg,ptrEnd-1,false,false) ;
               inc (ptrEnd,2) ;
               PtrBeg := ptrEnd ;
            end else if (ptrEnd^ = #13) then begin                // CR
               ptrEnd^ := #0 ;
               AddTrace(PtrBeg,ptrEnd-1,false,false) ;
               inc (ptrEnd) ;
               PtrBeg := ptrEnd ;
            end else if (ptrEnd^ = #10) then begin                // LF
               ptrEnd^ := #0 ;
               AddTrace(PtrBeg,ptrEnd-1,false,false) ;
               inc (ptrEnd) ;
               PtrBeg := ptrEnd ;
            end else begin
               inc (ptrEnd);
            end ;
         end ;
         //TFrm_Trace.InternalTrace(FormatDateTime('hh:mm:ss:zzz',now), 'block parsed. Lines : '+ inttostr(LastChildOrder)) ;

         if ptrEnd <> PtrBeg then
            AddTrace(PtrBeg,ptrEnd-1,false,true) ;
      end ;  // loop until end of file

      // if a sort is defined, resort after a trace is inserted
      if Sorter.SortColumns.Count <> 0 then
         Sorter.sort (nil) ;

      if IsColumnWidthChanged then begin
         AutosizeAll(VstTail);
         IsColumnWidthChanged := false ;
      end ;

   finally
      closefile(f);
      VstTail.EndUpdate ;
      BlockMemList.Clear ;   // owner
   end ;
end ;

//------------------------------------------------------------------------------
// called by the main thread.
// add a line to the tail tree and call CheckAutoClear
function TFrmTail.AddTrace (PtrBeg,PtrEnd: PAnsiChar; Ontop,IsNotCompleted : boolean) :  pVirtualNode ;
var
   TailRec : PTailRec ;
   col : TVirtualTreeColumn ;
   c : integer ;
   Appended : boolean ;
begin

   Appended := false ;

   if OnTop then begin   // onTop is true when the file is loaded the first time
      result := VstTail.AddChild (nil) ;
   end else if NotCompleteNode <> nil then begin
      result := NotCompleteNode ;
      Appended := true ;
   end else begin
      result := VstTail.AddChild (nil) ;
   end ;

   if IsNotCompleted then
      NotCompleteNode := result
   else
      NotCompleteNode := nil ;

   // ensure node is initialized. Needed when the node is free to call onFreeNode
   VstTail.ReinitNode(result,false);

   if TraceConfig.AppDisplay_FocusToReceivedMessage then
      NodeToFocus := result ;


   TailRec := VstTail.GetNodeData(result) ;

   if Appended then begin
      // append text to last record
      TailRec.Msg := TailRec.Msg + BufToString (PtrBeg,PtrEnd)     // remove zero inside the string
   end else begin
      // new record added
      TailRec.Msg := BufToString (PtrBeg,PtrEnd) ;    // remove zero inside the string
      if OnTop then begin
         TailRec.OriginalOrder := 0 ;
      end else begin
         TailRec.OriginalOrder := LastChildOrder ;
         inc (LastChildOrder) ;
      end ;
   end ;
   TailRec.Time := FormatDateTime('hh:mm:ss:zzz',now) ;

   if Filter <> nil then
      Filter.CheckNode(result) ;

   if DetectSeparatorOnfirstLine or DetectSeparatorOnEachLine or IsFixedColumns then begin
      // get sub strings
      TailRec.Columns := getDelimitedStringsfromWLine(pWidechar(TailRec.Msg)) ;  // use here the unicode version

      // detect new columns if in DetectSeparatorOnEachLine mode
      if (DetectSeparatorOnEachLine) and (TailRec.Columns <> nil) and (TailRec.Columns.Count > VstTail.Header.Columns.Count) then begin
         for c := VstTail.Header.Columns.Count to TailRec.Columns.Count-1 do begin
            col := VstTail.header.Columns.Add ;
            col.options  := col.options + [coAllowFocus] ;  // ensure user can focus to this column
            col.MinWidth := 10 ;
            col.MaxWidth := 10000 ;
            col.Width := 100 ;
         end ;
         IsColumnWidthChanged := true ;   // force autosize columns

         VstTail.Header.MainColumn := 0 ; // FixedColCount-1 ;
         VstTail.Header.AutoSizeIndex := -1 ;  // auto

         // force last column width to maximum
         //VstTail.Header.Columns[VstTail.Header.Columns.Count-1].Width := 9000 ;

      end ;
   end ;

   // when the file is read the first time, don't check size and don't try to show the viewer
   if OnTop = false then begin
      // check the number of node
      CheckAutoClear() ;

      if TraceConfig.AppDisplay_ShowOnMessageReceived then
         Frm_Tool.actShowExecute(nil) ;
   end ;

      //TailRec.Columns := getDelimitedStringsfromWLine(pWidechar(TailRec.Msg)) ;  // use here the unicode version
end ;

//------------------------------------------------------------------------------

// called by DirMon1Created(), DirMon1Deleted() and Display() when buffer > 65000
procedure TFrmTail.AddSeparator ();
var
   TailRec : PTailRec ;
   node : pvirtualNode ;
begin
   node := VstTail.AddChild (nil) ;
   // ensure node is initialized. Needed when the node is free to call onFreeNode
   VstTail.ReinitNode(node,false);

   TailRec := VstTail.GetNodeData(node) ;
   tailrec.Columns := TStringList (-2) ; // -2 indicate a Separator message
   TailRec.Msg := '' ;
   TailRec.OriginalOrder := LastChildOrder ;
   inc (LastChildOrder) ;
end ;

//------------------------------------------------------------------------------

procedure TFrmTail.AddRedComment (comment : string);
var
   TailRec : PTailRec ;
   node : pvirtualNode ;
begin
   node := VstTail.AddChild (nil) ;
   // ensure node is initialized. Needed when the node is free to call onFreeNode
   VstTail.ReinitNode(node,false);

   TailRec := VstTail.GetNodeData(node) ;
   tailrec.Columns := TStringList (-1) ; // -2 indicate a Separator message
   TailRec.Msg := comment ;
   TailRec.OriginalOrder := LastChildOrder ;
   inc (LastChildOrder) ;
   NodeToFocus := node ;
end ;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailFreeNode(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
   TailRec : PTailRec ;
   idx : integer ;
begin
   // delete from bookmark list
   if bookmarks <> nil then begin
      idx := bookmarks.IndexOf(node) ;
      if idx <> -1 then
         bookmarks.Delete(idx);
   end ;

   TailRec := Sender.GetNodeData(Node) ;

   // -1 indicate a RED TailRec.Msg message
   if integer (tailrec.Columns) = -1 then
      exit ;

   // -2 indicate a line separator
   if integer (tailrec.Columns) = -2 then
      exit ;

   if TailRec.Columns <> nil then
      TailRec.Columns.free ;

   TailRec.Msg  := '' ;
   TailRec.Time := '' ;


   if node = NotCompleteNode then
      NotCompleteNode := nil ;

   if node = NodeToFocus then
      NodeToFocus := nil ;  
end;

procedure TFrmTail.VstTailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
   TailRec : PTailRec ;
   c : integer ;
begin
   CellText := '' ;
   // unsort
   if Column = -1 then
      exit ;
   TailRec := Sender.GetNodeData(Node) ;
   if TailRec = nil then
      exit ;

   // -1 indicate a RED TailRec.Msg message
   if integer (tailrec.Columns) = -1 then begin
      if Column = 0 then
         CellText := TailRec.Msg ;
      exit ;
   end ;

   // -2 indicate a line separator (no text)
   if integer (tailrec.Columns) = -2 then begin
      CellText := ' ' ;
      exit ;
   end ;

   if ShowTimeAndLines then begin
      case Column of
         0 : begin
                CellText := TailRec.Time ;
             end ;
         1 : begin
                if (TextType = ttNormal) and (IsSeparator (TailRec.Msg)) then
                   CellText := ' '
                else
                   CellText := TailRec.Msg ;
             end ;
      end ;
   end else if ShowOnlyLines then begin
      if (TextType = ttNormal) and (IsSeparator (TailRec.Msg)) then
         CellText := ' '
      else
         CellText := TailRec.Msg ;
   end else if ShowManycolumns then begin
      if tailrec.Columns = nil then
         exit ;
      // number of substring can be less than the number of column
      if Column < tailrec.Columns.Count then begin
         CellText := tailrec.Columns[Column] ;
         // if last col and remaining strings...
         if (Column = VstTail.Header.Columns.Count-1) then begin
            for c := Column+1 to tailrec.Columns.Count-1 do
               CellText := CellText + ' ' + string(ColSeparator) + tailrec.Columns[c] ;
         end ;
         if (TextType = ttNormal) and (IsSeparator (CellText)) then
            CellText := ' ' ;
      end ;
   end ;

end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
   VstTailChange (VstTail,nil);
   if ShowManycolumns then
      VstTail.Header.MainColumn := VstTail.Header.Columns.GetFirstVisibleColumn ;
   AutosizeAll (VstTail) ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
   TailRec1,TailRec2    : PTailRec ;
   cellText1, cellText2 : String ;
begin
   if Column = -1 then begin
      // no column : unsort or the 2 records are the same
      TailRec1 := Sender.GetNodeData(Node1) ;
      TailRec2 := Sender.GetNodeData(Node2) ;
      if TailRec1.OriginalOrder <= TailRec2.OriginalOrder then
         result := -1
      else
         result := 1 ;
      exit ;
   end ;
   TVirtualStringTree(Sender).OnGetText (Sender,Node1,Column,ttNormal,CellText1) ;
   TVirtualStringTree(Sender).OnGetText (Sender,Node2,Column,ttNormal,CellText2) ;
   Result := CompareText (CellText1,CellText2) ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
   TailRec : PTailRec ;
begin
   TailRec := Sender.GetNodeData(Node) ;

   // -2 indicate a line separator (no text)
   if integer (tailrec.Columns) = -2 then begin
      TargetCanvas.Pen.Color := clBlack;
      TargetCanvas.MoveTo(2, ItemRect.Bottom div 2);
      TargetCanvas.LineTo(TargetCanvas.ClipRect.Right-2, ItemRect.Bottom div 2 );  //    ItemRect.Right-2
   end ;

   //if IsSeparator(TailRec.Msg) then begin
   //   TargetCanvas.Pen.Color := clBlack;
   //   TargetCanvas.MoveTo(2, ItemRect.Bottom div 2);
   //   TargetCanvas.LineTo(TargetCanvas.ClipRect.Right-2, ItemRect.Bottom div 2 );  //    ItemRect.Right-2
   //end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   TailRec : PTailRec ;
begin
   if bookmarks.IndexOf(node) <> -1 then begin
      DrawHighlight (TargetCanvas, CellRect,true) ;
      exit ;
   end ;

   // check if highlight must be draw
   if (SearchText = '') or (SearchKind <> mrYesToAll) then
      exit ;

   TailRec := VstTail.GetNodeData(Node) ;
   if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
      if CheckSearchRecord (TailRec) then     // check if the node or one of his child match the search text
         DrawHighlight (TargetCanvas, CellRect,false) ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
   CellText: String ;
   //TailRec : PTailRec ;
   middle : integer ;
begin
   //TailRec := Sender.GetNodeData(Node) ;
   VstTailGetText(Sender, Node,Column,ttStatic,CellText);   // ttStatic is used to get the real text
   if IsSeparator(CellText) then begin
      TargetCanvas.Pen.Color := clBlack;
      middle := CellRect.Bottom div 2 ;
      if copy (trim(CellText),1,1) = '-' then begin
         TargetCanvas.MoveTo(0, middle);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle );
      end else begin // '='
         TargetCanvas.MoveTo(0, middle - 1);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle - 1 );
         TargetCanvas.MoveTo(0, middle + 1);
         TargetCanvas.LineTo(TargetCanvas.ClipRect.Right, middle + 1 );
      end ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   TailRec : PTailRec ;
   FirstSelect : PVirtualNode ;
   SecondSelect : PVirtualNode ;
   c : integer ;
   ColIdx : TColumnIndex ;
   col : TVirtualTreeColumn ;
   NoTitle : boolean ;
   coltitle : string ;

   procedure AddOneLineDetail (Col1,col2 : String ) ;
   var
      DetailRec : PDetailRec ;
      DetailNode :  PVirtualNode ;
   begin
      DetailNode := VstDetail.AddChild(nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstDetail.ReinitNode(DetailNode,false);
      DetailNode.Align := (VstDetail.DefaultNodeHeight div 2)-2 ;
      DetailRec := VstDetail.GetNodeData(DetailNode) ;
      DetailRec.Col1 := col1 ;
      DetailRec.Col2 := col2 ;
      VstDetail.MultiLine[DetailNode] := true ;
   end ;

begin
   // scroll into view
   if Node <> nil then
      Sender.ScrollIntoView (Node,false,false);     // center and horizontally false

   // get first then second. If second is not nil then it's multiselect : disable info panel
   FirstSelect := VstTail.GetNextSelected (nil) ;
   if FirstSelect = nil then
      exit ;

   SecondSelect := VstTail.GetNextSelected (FirstSelect) ;
   if SecondSelect <> nil then
      exit ;

   if PanelTraceInfo.Visible = false then
      exit ;

   vstdetail.Clear ;

   TailRec := TVirtualStringTree (Sender).GetNodeData(FirstSelect) ; // node

   // TraceInfo panel
   if TailRec.Columns = nil then begin
      AddOneLineDetail ('Time '  , TailRec.Time) ;
      AddOneLineDetail ('Msg  '  , TailRec.Msg) ;
   end else begin

      // -1 indicate a RED TailRec.Msg message
      if integer(TailRec.Columns) = -1 then
         exit ;

      // -2 indicate a line separator
      if integer(TailRec.Columns) = -2 then
         exit ;

      // check if titles are empty
      NoTitle := true ;
      ColIdx := vstTail.Header.Columns.GetFirstVisibleColumn ;
      while ColIdx <> InvalidColumn do begin
         col := vstTail.header.Columns[ColIdx] ;
         if col.text <> '' then begin
            NoTitle := false ;
            break ;
         end ;
         ColIdx := vstTail.Header.Columns.GetNextVisibleColumn(ColIdx) ;
      end ;

      ColIdx := vstTail.Header.Columns.GetFirstVisibleColumn ;
      while ColIdx <> InvalidColumn do begin
         col := vstTail.header.Columns[ColIdx] ;

         if NoTitle then
            coltitle := 'Col ' + inttostr (ColIdx+1)
         else
            coltitle := col.Text ;

         // tailRec can contain less column than the tree
         if ColIdx < TailRec.Columns.count then
            AddOneLineDetail (coltitle , RemoveLastCRLF(TailRec.Columns[ColIdx]))
         else
            AddOneLineDetail (coltitle, '') ;
         ColIdx := vstTail.Header.Columns.GetNextVisibleColumn(ColIdx) ;
      end ;

      // if more TreeRec.Columns than vst.Header.Columns then add lines
      for c := vstTail.Header.Columns.Count to TailRec.Columns.count-1 do begin
         AddOneLineDetail ('', TailRec.Columns[c]) ;
      end ;

   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.butCloseClick(Sender: TObject);
begin
   CloseWin () ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//   TFrmBase
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TFrmTail.CloseWin;
begin
   DirMon.Active := false ;                     // stop monitoring
   self.close ;                                 // close the tail form  and perform undocking + free container
end;

//------------------------------------------------------------------------------
// here we can modify the "actions" menu items
procedure TFrmTail.PageControlChange();
var
   PageContainer : TFrmPageContainer ;
begin
   PageContainer := getPageContainer() ;
   if PageContainer = nil then begin
      TFrm_Trace.InternalTrace ('TFrmTail.PageControlChange:PageContainer = nil') ;
      exit ;
   end ;
   PageContainer.actPrint        .Enabled := true ;
   PageContainer.actClear        .Enabled := true ;
   PageContainer.actSaveToFile   .Enabled := false ;  // no sense to save something already on disk
   PageContainer.actViewTraceInfo.Enabled := true ;
   PageContainer.actPause        .Enabled := true  ;
   PageContainer.actCopy         .Enabled := true  ;
   PageContainer.actSelectAll    .Enabled := true  ;
   PageContainer.actDelete       .Enabled := true  ;
   PageContainer.actCut          .Enabled := true  ;
   PageContainer.actViewProperty .Enabled := false  ;   
   PageContainer.actSearch       .Enabled := true ;
   PageContainer.actFindNext     .Enabled := true ;

   PageContainer.actClearFileContent.Visible := true ;

   PageContainer.actViewTraceInfo.checked := PanelTraceInfo.Visible ;
   PageContainer.actPause        .checked := self.IsPaused ;
end;

//------------------------------------------------------------------------------

//procedure TFrmTail.PageControlChanging();
//begin
//   getPageContainer().actClearFileContent.Visible := false ;
//end;

//------------------------------------------------------------------------------

procedure TFrmTail.ResizeColumns;
begin
   AutosizeAll (vstTail) ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.ViewTraceInfo;
begin
  if getPageContainer().actViewTraceInfo.Checked then begin
      PanelTraceInfo.Visible := true ;
      VSplitter.Visible := true ;
      PanelTraceInfo.Left := VSplitter.Left + 10 ;
      VstTailChange(VstTail,nil);
   end else begin
      PanelTraceInfo.Visible := false ;
      VSplitter.Visible := false ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.ViewProperty;
begin
  inherited;

end;

//------------------------------------------------------------------------------
// timer : show stat info
procedure TFrmTail.TimerInfo;
begin
   if FileStatus <> '' then
      TracesInfo.Caption := TimeToStr(LastModified) + '  ' + FileStatus
   else
      TracesInfo.Caption := TimeToStr(LastModified)
                         + '  File size : ' + inttostr(File_Size)
                         //+ ', Read : ' + inttostr (lastpos)
                         + ', not filtered lines : ' + inttostr(VstTail.RootNode.ChildCount)
                         + '   ' + TailFile ;

   TracesInfo.Hint := TracesInfo.Caption ;

   // focus to node.
   // if autoclear is enabled, the NodeToFocus may be inexisting
   try
      if NodeToFocus <> nil then begin
         VstTail.ClearSelection();
         VstTail.Selected [NodeToFocus] := true ;
         VstTail.FocusedNode := NodeToFocus;
         VstTail.ScrollIntoView (NodeToFocus,false,false);
      end;
   except
   end ;

   NodeToFocus := nil ;
end;

//------------------------------------------------------------------------------
// check the number of node. Called by AddTrace
procedure TFrmTail.CheckAutoClear;
var
   NodeTodelete , next : PVirtualNode ;
   nodeCount : integer ;
begin
   nodeCount := VstTail.RootNode.ChildCount ;
   if TraceConfig.Tail_AutoClear and (nodeCount > TraceConfig.Tail_MaxNode) then begin
      VstTail.BeginUpdate ;
      try
         if sorter.SortColumns.Count <> 0 then     // unsort before deleting old traces
            sorter.Unsort(nil);

         NodeTodelete := VstTail.RootNode.FirstChild ;
         while nodeCount > TraceConfig.Tail_MinNode do begin
            next := NodeTodelete.NextSibling ;
            if NodeTodelete = NodeToFocus then  // will be also checked in OnFreeNode
               NodeToFocus := nil ;
            VstTail.DeleteNode(NodeTodelete,false) ;
            dec (nodeCount) ;
            NodeTodelete := next ;
         end ;

         if sorter.SortColumns.Count <> 0 then      // resort
            sorter.Sort(nil);
      finally
         VstTail.EndUpdate ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------
procedure TFrmTail.PauseWin;
begin
   self.IsPaused := getPageContainer().actPause.Checked ;
end;

//------------------------------------------------------------------------------
procedure TFrmTail.SelectAll;
begin
   // normally when the VstEditor is not nil, he is visible
   if ((VstTail.IsEditing) or VstDetail.IsEditing) and (IVstEditor <> nil) and (TMoveMemoEditLink(IVstEditor).IsVisible) then begin
      VstEditor.SelectAll() ;  // TMoveMemoEditLink(IVstEditor).SelectAll() ;
      exit ;
   end ;
   if VstTail.Focused = true then
      VstTail.SelectAll(true)       // select all visible items (don't select filtered items)
   else if VstDetail.Focused then
      VstDetail.SelectAll(false) ;  // select all (visible or invisible)
end;

//------------------------------------------------------------------------------
// CTRL C : Copy selected
procedure TFrmTail.CopySelected;
var
   CopyStrings: TStrings;
   CopyText: PChar;
   NewLine: string;
   IsFirst : boolean ;
   TailRec : PTailRec ;
   focusedComponent : hwnd ;
   ColIdx : TColumnIndex ;

   procedure CheckIfNodeSelected (TestNode : PVirtualNode) ;
   var
      ChildVtNode : PVirtualNode ;
      LeftMsg : string ;
      ColIdx : TColumnIndex ;
      c : integer ;
   begin
      if VstTail.Selected [TestNode] then begin
         TailRec := VstTail.GetNodeData(TestNode) ;
         LeftMsg := TailRec.Msg ;
         // -1 indicate a RED TailRec.Msg message
         if integer (tailrec.Columns) = -1 then
            exit ;

         // -2 indicate a line separator
         if integer (tailrec.Columns) = -2 then
            exit ;

         IsFirst := true ;
         NewLine := '' ;

         if (TraceConfig.TextExport_Time) and (ShowTimeAndLines or ShowOnlyLines) then begin
            if IsFirst = false then
               NewLine := NewLine + string(ColSeparator) ;
            NewLine := NewLine + string(TextQualifier) + TailRec.Time + String(TextQualifier) ;
            IsFirst := false ;
         end ;

         if (TraceConfig.TextExport_Col1) and (ShowTimeAndLines or ShowOnlyLines) then begin
            if IsFirst = false then
               NewLine := NewLine + String(ColSeparator) ;

            NewLine := NewLine + String(TextQualifier) + LeftMsg + String(TextQualifier) ;
            IsFirst := false ;
         end ;

         if ShowManycolumns then begin
            ColIdx := VstTail.Header.Columns.GetFirstVisibleColumn ;
            while ColIdx <> InvalidColumn do begin
               if ColIdx < TailRec.Columns.Count  then begin  // check if more header than data
                  if IsFirst = false then
                     NewLine := NewLine + String(ColSeparator) ;
                  NewLine := NewLine + String(TextQualifier) + RemoveLastCRLF (TailRec.Columns[ColIdx]) + String(TextQualifier) ;
                  IsFirst := false ;
               end ;
               ColIdx := VstTail.Header.Columns.GetNextVisibleColumn(ColIdx) ;
            end ;
            // if more data than header : append data
            ColIdx := VstTail.Header.Columns.GetLastVisibleColumn ;
            for c := ColIdx+1 to TailRec.Columns.Count-1 do
               NewLine := NewLine + String(TextQualifier) + RemoveLastCRLF (TailRec.Columns[c]) + String(TextQualifier) ;
        end ;

         CopyStrings.Add(NewLine);

      end ;
      ChildVtNode := TestNode.FirstChild ;
      while ChildVtNode <> nil do begin
         CheckIfNodeSelected (ChildVtNode) ;
         ChildVtNode := ChildVtNode.NextSibling ;
      end ;
   end ;

   procedure CopyDetail (TestNode : PVirtualNode);
   var
      node : PVirtualNode ;
      DetailRec : PDetailRec ;
   begin
      if VstDetail.Selected [TestNode] then begin

         DetailRec := VstDetail.GetNodeData(TestNode) ;
         NewLine := TraceConfig.TextExport_TextQualifier + DetailRec.Col1 + TraceConfig.TextExport_TextQualifier  +
                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + DetailRec.Col2 + TraceConfig.TextExport_TextQualifier +
                    TraceConfig.TextExport_Separator + TraceConfig.TextExport_TextQualifier + DetailRec.Col3 + TraceConfig.TextExport_TextQualifier  ;

         CopyStrings.Add(NewLine);
      end ;

      // multi select
      node := TestNode.FirstChild ;
      while Node <> nil do begin
         CopyDetail (node) ;
         node := node.NextSibling ;
      end ;
   end ;

begin
   // reroute CTRL-C to the focused component if it's not the master tree
   if (VstDetail.Focused = false) and (VstTail.Focused = false) then begin
      focusedComponent := GetFocus ;
      if focusedComponent <> 0 then
         SendMessage(focusedComponent, WM_COPY, 0, 0);
      exit ;
   end ;

   if VstTail.GetFirstSelected = nil then
      exit ;

   CopyStrings := TStringList.Create;
   SetCursor(Screen.Cursors[crHourGlass]);
   try

      if VstDetail.Focused then begin
         CopyDetail (VstDetail.RootNode);
      end else begin
         // add title if needed.
         if TraceConfig.TextExport_GenerateColumnHeader then begin
            IsFirst := true ;
            NewLine := '' ;

            if (TraceConfig.TextExport_Time) and (ShowTimeAndLines) then begin
               if IsFirst = false then
                  NewLine := NewLine + String(ColSeparator) ;
               NewLine := NewLine + String(TextQualifier) + 'Time' + String(TextQualifier) ;
               IsFirst := false ;
            end ;

            if (TraceConfig.TextExport_Col1) and (ShowTimeAndLines or ShowOnlyLines ) then begin
               if IsFirst = false then
                  NewLine := NewLine + String(ColSeparator) ;
               NewLine := NewLine + String(TextQualifier) + 'Msg' + String(TextQualifier) ;
               IsFirst := false ;
            end ;

            // copy column name if available
            if (ShowManycolumns) and (DetectTitleOnFirstLine) then begin
               ColIdx := vsttail.Header.Columns.GetFirstVisibleColumn ;
               while ColIdx <> InvalidColumn do begin
                  if IsFirst = false then
                     NewLine := NewLine + String(ColSeparator) ;
                  NewLine := NewLine + String(TextQualifier) + vsttail.header.Columns[ColIdx].Text + String(TextQualifier) ;
                  IsFirst := false ;
                  ColIdx := VstTail.Header.Columns.GetNextVisibleColumn(ColIdx) ;
               end ;
            end ;

            CopyStrings.Add(NewLine);
         end ;

         // add node starting from the invisible root node (recursive)
         CheckIfNodeSelected (VstTail.RootNode) ;
      end ;
      CopyText := CopyStrings.GetText;
      try
         if CopyStrings.Count > 0 then
            Clipboard.SetTextBuf(CopyText);
      finally
         StrDispose(CopyText);
      end;
   finally
      SetCursor(Screen.Cursors[crDefault]);
      CopyStrings.Free ;
   end;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.CopyCurrentCell;
var
   Node : PVirtualNode ;
   wCellText: String ;
   CellText : string ;
begin
   if VstTail.Focused then begin
      Node := VstTail.FocusedNode ;
      if node = nil then
         exit ;
      VstTailGetText(VstTail, Node, VstTail.FocusedColumn,ttStatic,wCellText);   // ttStatic is used to get the real text
   end else if VstDetail.Focused then begin
      Node := VstDetail.FocusedNode ;
      if node = nil then
         exit ;
      VstDetailGetText(VstDetail, Node, VstDetail.FocusedColumn,ttNormal,wCellText);   // ttNormal
   end else begin
      exit ;
   end ;

   CellText := wCellText ;
   Clipboard.SetTextBuf(pchar(CellText));
end;

//------------------------------------------------------------------------------

procedure TFrmTail.ClearWin;
begin
   VstTail.Clear ;
   vstdetail.Clear ;
end;

//------------------------------------------------------------------------------
procedure TFrmTail.DeleteSelected;
var
   node : PVirtualNode ;
begin
   if VstTail.Focused = false then
     exit ;
   node := VstTail.GetFirstSelected ;
   if node = nil then  // no node selected
      exit ;
   node := VstTail.GetPreviousVisible(node) ;
   VstTail.DeleteSelectedNodes ;

   // case of the first node : GetPreviousVisible is nil ...
   if node = nil then
      node := VstTail.GetFirst
   else if VstTail.GetNextVisible(node) <> nil then
      node := VstTail.GetNextVisible(node) ;

   VstTail.FocusedNode := node ;
   VstTail.Selected [node] := true ;
   if node = nil then
      vstdetail.Clear ;
end;


//------------------------------------------------------------------------------
procedure TFrmTail.SaveWin;
begin
  // nothing to do
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


procedure TFrmTail.VstDetailChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   // scroll into view
   Sender.ScrollIntoView (Node,false,false);     // center and horizontally false
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();
      IVstEditor := VstEditor ;
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true ; //not (Column = 0) ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   //InternalTrace ('DetailDblClick ') ;
   SelectedNode := VstDetail.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstDetail.ScreenToClient(P);
   MouseNode := VstDetail.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_MEMBER, Integer(SelectedNode), 0);
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: String);
var
   DetailRec : PDetailRec ;
begin
   CellText := '' ;
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;

   case Column of
      0 : CellText := DetailRec.Col1 ;
      1 : CellText := DetailRec.Col2 ;
      2 : CellText := DetailRec.Col3 ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   DetailRec : PDetailRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   if (SearchText <> '') {and (SearchKind = mrYesToAll)} then begin  //  mrYesToAll means Highlight all

      case Column of
         0 : if (MatchSearch (DetailRec.col1) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
         1 : if (MatchSearch (DetailRec.col2) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
         2 : if (MatchSearch (DetailRec.col3) <> 0) then DrawHighlight (TargetCanvas, CellRect,false) ;
      end ;
      //if (MatchSearch (DetailRec.col1) <> 0) or
      //   (MatchSearch (DetailRec.col2) <> 0) or
      //   (MatchSearch (DetailRec.col3) <> 0) then begin
      //   DrawHighlight (TargetCanvas, CellRect,false) ;
      //end ;
   end;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
   DetailRec : PDetailRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   DetailRec.Col1 := '' ;
   DetailRec.Col2 := '' ;
   DetailRec.Col3 := '' ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstDetail.EditNode(Node, VstDetail.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure TFrmTail.WMStartEditingTrace(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstTail.EditNode(Node, VstTail.FocusedColumn);
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set
procedure TFrmTail.VstTailDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
   TailRec : PTailRec ;
begin
   SelectedNode := VstTail.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   TailRec := VstTail.GetNodeData(SelectedNode) ;

   // -2 indicate a line separator. No edit allow
   if integer (tailrec.Columns) = -2 then
      exit ;

   GetCursorPos(P);
   P := VstTail.ScreenToClient(P);
   MouseNode := VstTail.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstTail.TreeOptions.MiscOptions := VstTail.TreeOptions.MiscOptions + [toEditable] ;

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_TRACE, Integer(SelectedNode), 0);
end;

//------------------------------------------------------------------------------

// Detect the F2 key.
// To not allow editing on simple click, the vst.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set
procedure TFrmTail.VstTailKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if CharCode = VK_F2 then
      VstTail.TreeOptions.MiscOptions := VstTail.TreeOptions.MiscOptions + [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrmTail.VstTailEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
   VstTail.TreeOptions.MiscOptions := VstTail.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

// After node is edited, reset the toEditable flag to not allow editing on simple click
procedure TFrmTail.VstTailEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstTail.TreeOptions.MiscOptions := VstTail.TreeOptions.MiscOptions - [toEditable] ;
end;

procedure TFrmTail.VstTailEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := true;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();
      IVstEditor := VstEditor ;
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

function TFrmTail.CheckSearchRecord  (TailRec : PTailRec) : boolean ;
var
   c : integer ;
begin
   result := false ;

   if (MatchSearch (TailRec.Msg) <> 0) then begin
      result := true ;
      exit ;
   end ;

   // -1 indicate a RED TailRec.Msg message
   if integer (tailrec.Columns) = -1 then
      exit ;

   // -2 indicate a line separator
   if integer (tailrec.Columns) = -2 then
      exit ;

   if TailRec.Columns <> nil then begin
      for c := 0 to TailRec.Columns.Count-1 do begin
         if MatchSearch (TailRec.Columns[c]) <> 0 then begin
            result := true ;
            exit ;
         end ;
      end ;
   end ;
end ;

//------------------------------------------------------------------------------

function TFrmTail.SearchNext(start:boolean) : boolean ;
var
   currentNode : PVirtualNode ;
   TailRec : PTailRec ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if start = true then begin
      currentNode := VstTail.GetFirstVisible() ;
   end else begin
      currentNode := VstTail.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstTail.GetFirstVisible()
      else  // when start is false, we are searching in the current document
         currentNode := VstTail.GetNextVisible(currentNode) ;   // skip the first selected
   end ;

   while currentNode <> nil do begin
      TailRec := VstTail.GetNodeData(currentNode) ;
      if CheckSearchRecord (TailRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         VstTail.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstTail.ClearSelection;
         VstTail.Selected [currentNode] := true ;
         VstTail.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstTail.GetNextVisible(currentNode) ;
   end ;
end;

//------------------------------------------------------------------------------

function TFrmTail.SearchPrevious(atEnd: boolean): boolean;
var
   currentNode : PVirtualNode ;
   TailRec : PTailRec ;
   procedure CheckVisible () ;
   begin
      while (currentNode <> nil) and (VstTail.IsVisible[currentNode] = false) do begin
         currentNode := VstTail.GetPrevious(currentNode) ;
      end ;
   end ;
begin
   result := false ;
   if Visible = false then
      exit ;

   if atEnd = true then begin
      currentNode := VstTail.GetLast() ;
   end else begin
      currentNode := VstTail.GetFirstSelected ;
      if currentNode = nil then
         currentNode := VstTail.GetLastVisible()
      else  // when atEnd is false, we are searching in the current document
         currentNode := VstTail.GetPrevious(currentNode) ;   // skip the first selected
   end ;

   CheckVisible() ;
   while currentNode <> nil do begin
      TailRec := VstTail.GetNodeData(currentNode) ;
      if CheckSearchRecord (TailRec) then begin
         if ActiveTracePage <> self then
            SetActivePage() ;
         // fully visible ?
         VstTail.ScrollIntoView (currentNode,false);  // ensure the node is fully visible and displayed
         VstTail.ClearSelection;
         VstTail.Selected [currentNode] := true ;
         VstTail.SetFocus() ;
         result := true ;
         exit ;
      end ;
      currentNode := VstTail.GetPrevious(currentNode) ;
      CheckVisible() ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.RefreshView;
begin
   vstTail.Refresh ;
   VstDetail.Refresh ;
end;

//------------------------------------------------------------------------------

// if the paint area is modified, AfterPaint is called to redisplay the gutter

procedure TFrmTail.VstTailAfterPaint(Sender: TBaseVirtualTree;  TargetCanvas: TCanvas);
var
   Node : pvirtualNode ;
   BaseOffset : Integer;  // top position of the top node to draw given in absolute tree coordinates
   DispRec, CliRect : TRect ;
   Yposition : integer ;
   NodeHeight : integer ;
   TailRec : PTailRec ;
   HeaderHeight : integer ;
   gutterCanvas : TCanvas ;
   newgutter : timage ;
   BookmarkPos : integer ;
begin
   // detect header height
   if VstTail.Header.Columns.Count <> 0 then begin
      // get the header height from the first header column
      // since the VT.headerRect property is protected :-(
      HeaderHeight := VstTail.Header.Columns[0].GetRect.Bottom ;
   end else begin  // should not happens
      HeaderHeight := VstTail.header.Height + 2 ;  // plus somme bevels
   end ;

   newgutter := timage.Create(self);
   newgutter.width  := PanelGutter.Width ;
   newgutter.Height := PanelGutter.Height ;
   newgutter.top    := 0 ;
   newgutter.left   := 0 ;
   newgutter.OnDblClick := PanelGutterDblClick ;
   gutterCanvas := newgutter.Canvas ;

   // clear gutter
   CliRect := Rect(0, HeaderHeight, PanelGutter.Width, PanelGutter.Height);
   gutterCanvas.Brush.Color := clBtnFace ;  //clgreen ; //clBtnFace ;
   gutterCanvas.FillRect(CliRect);

   // Determine node to start drawing with.
   BaseOffset := 0 ;
   Node := VstTail.GetNodeAt(0, 0, true, BaseOffset);
   if node <> nil then begin    // nothing to display
      // get the first visible node rectangle.
      DispRec := VstTail.GetDisplayRect (Node,NoColumn,false,false) ;

      // We just need the TOP node position
      // This top position is zero or negative since the node can be partially visible
      // but can never be more than zero (else we can have previous partial visible node before)
      Yposition := DispRec.Top ;

      // add Header height
      inc (Yposition , HeaderHeight) ;

      // draw each node
      while node <> nil do begin
         NodeHeight := VstTail.NodeHeight[Node] ;
         TailRec := VstTail.GetNodeData(Node) ;

         BookmarkPos := bookmarks.IndexOf(Node) ;
         if BookmarkPos <> -1 then begin
            if (bookmarks.Count = 1) or (BookmarkPos > 9) then begin    // only one element or bookmark > 9
               Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 24) ;               // normal rectangle
               if BookmarkPos > 9 then
                  Frm_Tool.UtilityImages.Draw(gutterCanvas, 6 , Yposition+8 ,12) ;       // write "..."
            end else begin    //
               Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 25) ;               // bottom right corner is empty
               Frm_Tool.UtilityImages.Draw(gutterCanvas, 6 , Yposition+8 ,BookmarkPos);  // write the number
            end ;
         end else begin
            if (SearchText <> '') and (SearchKind = mrYesToAll) then
               if (unt_search.SearchInAllPages) or (ActiveTracePage = self) then
                  if CheckSearchRecord (TailRec) then     // check if the node or one of his child match the search text
                     Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 21);

         end ;

         // draw the small dot indicate sub members
         //if (treeRec.Members <> nil) and (treeRec.Members.SubMembers.Count <> 0) then
         //   Frm_Tool.ilActions.Draw(gutterCanvas, 0 , Yposition , 15);

         inc (Yposition , NodeHeight) ;
         Node := VstTail.GetNextVisible(Node) ;
      end ;
   end ;

   // draw the header to hide bullet on previous line.
   CliRect := Rect(0, 0, PanelGutter.Width, HeaderHeight);
   gutterCanvas.Brush.Color := clBtnFace ;
   gutterCanvas.FillRect(CliRect);
   DrawEdge(gutterCanvas.Handle, CliRect, EDGE_RAISED, BF_BOTTOM );

   // replace the old gutter by the new one
   newgutter.Parent := PanelGutter ;
   newgutter.BringToFront ;
   
   if gutter <> nil then begin
      gutter.parent := nil ;
      gutter.Free ;
   end ;
   gutter := newgutter ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.ShowFilter;
var
   c : integer ;
   title : string ;
begin
   if Filter = nil then
      Filter := TFrmFilter.create (self) ;

   Filter.Vst := VstTail ;
   Filter.base := self ;
   Filter.ColumnNameList.Clear ;

   if ShowTimeAndLines then begin
      Filter.ColumnNameList.AddObject('Time' , tObject(0)) ;
      Filter.ColumnNameList.AddObject('Lines', tObject(1)) ;
   end else if ShowOnlyLines then begin
      Filter.ColumnNameList.AddObject('Lines', tObject(1)) ;
   end else if ShowManycolumns then begin
      for c := 0 to VstTail.Header.Columns.Count-1 do begin
         Title := VstTail.Header.Columns.Items[c].Text ;
         if Title = '' then
            Title := 'Col' + inttostr(c) ;
         Filter.ColumnNameList.AddObject (Title , tObject(c)) ;
      end ;
   end ;

   filter.FillColumns() ;
   Filter.ShowModal() ;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrmTail.getMembers(Node: PVirtualNode): TMember;
begin
   result := nil ;   // no members
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrmTail.PanelGutterDblClick(Sender: TObject);
var
   Node : PVirtualNode ;
   P: TPoint;
   index : integer ;
begin

   GetCursorPos(P);
   P := vstTail.ScreenToClient(P);
   Node := vstTail.GetNodeAt(0, P.Y) ;
   if Node = nil then
      exit ;

   index := bookmarks.IndexOf(Node) ;
   if index = -1 then begin
      bookmarks.Add(Node) ;
   end else begin
      bookmarks.Delete(index) ;
   end ;

   vstTail.InvalidateNode(Node) ;
end;

//------------------------------------------------------------------------------

// main tree : fixed node height
procedure TFrmTail.VstTailMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
   NodeHeight := TraceConfig.Tail_Trace_NodeHeight ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstTailPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
   TailRec : PTailRec ;
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.Tail_Trace_FontName ;
   TargetCanvas.Font.size := TraceConfig.Tail_Trace_FontSize ;

   TailRec := Sender.GetNodeData(Node) ;

   // -1 indicate a RED TailRec.Msg message
   if integer (tailrec.Columns) = -1 then
      TargetCanvas.Font.Color := clRed ;
end;

//------------------------------------------------------------------------------

// member tree : node height depend of the number of lines (variable node height)
procedure TFrmTail.VstDetailMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
   h2 : integer ;
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.Tail_Info_FontName ;
   TargetCanvas.Font.size := TraceConfig.Tail_Info_FontSize ;

   NodeHeight := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,0) ;
   h2 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,1) ;

   if h2 > NodeHeight then
      NodeHeight := h2 ;

   // should not happens : node contains at least a title in the first col
   if NodeHeight = 0 then
      NodeHeight := VstDetail.defaultNodeHeight
end;

//------------------------------------------------------------------------------

procedure TFrmTail.VstDetailPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
   // force font
   TargetCanvas.Font.Name := TraceConfig.Tail_Info_FontName ;
   TargetCanvas.Font.size := TraceConfig.Tail_Info_FontSize ;

   if Column = 0 then
      if node.Parent = VstDetail.RootNode then
         TargetCanvas.font.Style := [fsBold] ;
end;

//------------------------------------------------------------------------------

procedure TFrmTail.ApplyFont;
begin
   VstTail.BeginUpdate ;
   VstTail.Font.Name         := TraceConfig.Tail_Trace_FontName ;
   VstTail.Font.Size         := TraceConfig.Tail_Trace_FontSize ;
   VstTail.DefaultNodeHeight := TraceConfig.Tail_Trace_NodeHeight ;
   VstTail.ReinitChildren (nil,true);
   VstTail.EndUpdate ;

   VstDetail.BeginUpdate ;
   VstDetail.Font.Name         := TraceConfig.Tail_Info_FontName ;
   VstDetail.Font.Size         := TraceConfig.Tail_Info_FontSize ;
   VstDetail.ReinitChildren (nil,true);
   VstDetail.EndUpdate ;

end;

procedure TFrmTail.Print;
begin
   FrmPrintPreview.initialize(vstTail, nil) ;
   FrmPrintPreview.ShowModal ;
end;

end.



