unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TraceToolCom_TLB, activex, comobj, ComCtrls, ExtCtrls; //, tracetool;

type

  TForm4 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    butSimpleTest: TButton;
    butLongTest: TButton;
    ButIndent: TButton;
    butSaveToText: TButton;
    butSaveToXML: TButton;
    butLogFile: TButton;
    butClear: TButton;
    butLoadXMLMain: TButton;
    butResend: TButton;
    butFocus: TButton;
    butSetSelected: TButton;
    butAppend: TButton;
    butStart1: TButton;
    butStart2: TButton;
    ButtonCpt: TButton;
    TabSheet3: TTabSheet;
    butCreateTraceWin: TButton;
    butDisplayWin: TButton;
    butHelloToWintrace: TButton;
    butSaveWinToTxt: TButton;
    butSaveWinToXml: TButton;
    butClearWin: TButton;
    butLoadXMLWin: TButton;
    TabSheet4: TTabSheet;
    Label6: TLabel;
    butWatch: TButton;
    butClearWatchWindow: TButton;
    butDisplayWatchWindow: TButton;
    butCreateWinWatch: TButton;
    butWinWatchSend: TButton;
    butWinWatchClear: TButton;
    butWinWatchDisplay: TButton;
    Panel3: TPanel;
    butShow: TButton;
    butMultiCol: TButton;
    butWinMsg: TButton;
    butSocket: TButton;
    EditHost: TEdit;
    EditPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    chkUdp: TCheckBox;
    procedure butSimpleTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure butLongTestClick(Sender: TObject);
    procedure ButIndentClick(Sender: TObject);
    procedure butShowClick(Sender: TObject);
    procedure butSaveToTextClick(Sender: TObject);
    procedure butSaveToXMLClick(Sender: TObject);
    procedure butLogFileClick(Sender: TObject);
    procedure butClearClick(Sender: TObject);
    procedure butLoadXMLMainClick(Sender: TObject);
    procedure butStart1Click(Sender: TObject);
    procedure butResendClick(Sender: TObject);
    procedure butSetSelectedClick(Sender: TObject);
    procedure ButtonCptClick(Sender: TObject);
    procedure butStart2Click(Sender: TObject);
    procedure butAppendClick(Sender: TObject);
    procedure butFocusClick(Sender: TObject);
    procedure butCreateTraceWinClick(Sender: TObject);
    procedure butDisplayWinClick(Sender: TObject);
    procedure butHelloToWintraceClick(Sender: TObject);
    procedure butSaveWinToTxtClick(Sender: TObject);
    procedure butSaveWinToXmlClick(Sender: TObject);
    procedure butClearWinClick(Sender: TObject);
    procedure butLoadXMLWinClick(Sender: TObject);
    procedure butMultiColClick(Sender: TObject);
    procedure butWatchClick(Sender: TObject);
    procedure butClearWatchWindowClick(Sender: TObject);
    procedure butDisplayWatchWindowClick(Sender: TObject);
    procedure butCreateWinWatchClick(Sender: TObject);
    procedure butWinWatchSendClick(Sender: TObject);
    procedure butWinWatchClearClick(Sender: TObject);
    procedure butWinWatchDisplayClick(Sender: TObject);
    procedure butWinMsgClick(Sender: TObject);
    procedure butSocketClick(Sender: TObject);
  private
    procedure ArrayTest;
    { Private declarations }
  public
    { Public declarations }
    ttrace : IXTrace ;
    NodeEx1,NodeEx2 : IXTraceNodeEx ;
    Node1 : IXTraceNode ;
    WinTrace1 : IXWinTrace ;
    WinWatch : IXWinWatch ;

    start1 : IxTraceNode ;
    start2 : IxTraceNode ;
    start3 : IxTraceNode ;

    myWinTrace : IxWinTrace ;
    MyWinWatch : IxWinWatch ;
    MulticolWintrace : IxWinTrace ;

    lastTest : integer ;

  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
   lastTest := 0 ;
   ttrace := CoXTrace.Create ;
   //ttrace.Options.SocketHost := 'localhost' ;
   //ttrace.Options.SendMode := Socket ;
   ttrace.ClearAll ;
end;


procedure TForm4.FormDestroy(Sender: TObject);
begin
   WinTrace1 := nil ;
   WinWatch := nil ;
   ttrace.Flush(0);
   ttrace := nil ;
end;

procedure TForm4.butSimpleTestClick(Sender: TObject);
var
   Line2 : IXTraceNode ;
   table : IXTraceTable ;
   MemberNode : IXMemberNode ;
   XmlDoc : variant ;
   v : Variant ;
begin

   // TTraceNode sample
   //--------------------------------------------

   Line2 := TTrace.debug.Send('hello2','') ;
   Line2.Send('World2','') ;

   TTrace.Warning.Send('hello' , 'world') ;

   // long text
   TTrace.debug.Send('qwerty  qwerty qwerty qwerty qwerty qwerty qwerty qwerty qwerty qwerty ','');
   TTrace.debug.Send('hello' + #13 + #10 + 'world','');

   // single separator
   TTrace.debug.Send('---','');

   // send traces with special font style (bold and Italic), color font size and font name
   TTrace.debug.Send('Special font ', 'Symbol 12')
      .SetFontDetail(3,true,false,ClRed,0,'')                     // set col 3 (Left Msg)  to bold and Red
      .SetFontDetail(4,false,false,clGreen,12, 'Symbol') ;   // set col 4 (Right Msg) to Green and font size 12
   TTrace.debug.Send('Impact Italic','')
      .SetFontDetail(3,false,true,clBlack,12,'Impact') ;     // Col3 (left msg), non bold, Italic , Black , font 12 , Impact

   TTrace.debug.Send('Whole line','in red')
      .SetFontDetail(-1,false,false,clRed,0,'') ;                 // -1 : all columns

   // double separator
   TTrace.debug.Send('===','');

   //TTrace.Options.SendDate := true ;
   //TTrace.debug.Send ('trace with date','') ;
   //TTrace.Options.SendDate := false ;

   // TTraceNodeEx sample
   //--------------------------------------------
   NodeEx1 := ttrace.Debug.CreateNodeEx();
   NodeEx1.LeftMsg := 'ttrace.Debug.CreateNodeEx()' ;
   NodeEx1.IconIndex := 3 ; // CST_ICO_CONTROL ;
   NodeEx1.Members.Add('A', 'B' , 'C') ;
   NodeEx1.AddFontDetail(3,false,false,clFuchsia,0,'') ;    // change font detail col 3
   NodeEx1.AddBackgroundColor(clFuchsia,4 );                // change background color col 4
   NodeEx1.IconIndex := {CST_ICO_CONTROL} 3 ;

   Line2 := NodeEx1.Send ;
   Line2.ResendIconIndex ({CST_ICO_PROP} 5) ;               // change icon index after the node is send

   NodeEx2 := Line2.CreateNodeEx() ;
   NodeEx2.LeftMsg := 'CreateNodeEx from another node' ;
   NodeEx2.AddFontDetail(3,false,false,clGreen,0,'') ;
   MemberNode := NodeEx2.Members.Add('My Members', 'thid' , '0x' + IntToHex (getCurrentThreadID(),3)) ;

   MemberNode.SetFontDetail(-1,true,false,clBlack,0,'');      // set all columns to bold
   MemberNode.SetFontDetail(1,false,false,clGreen,0,'');      // set second column to green (0 index based)
   MemberNode.Add('Sub members','','')                        // add sub member node
             .SetFontDetail(0,false,true,clBlack,0,'') ;      // set first column to Italic (0 index based)
   NodeEx2.Send ;

   // SendObject samples
   //---------------------------------
   ttrace.Debug.SendObject('SendObject',ttrace) ;
   ttrace.Debug.SendValue('SendValue',ttrace,'ttrace') ;
   ttrace.Debug.SendValueIDisptach({left trace} 'SendValueIDisptach', ttrace,{ObjTitle} 'ttrace') ;

   // null variant
   tagVARIANT(v).vt := vt_null ;
   TTrace.debug.SendValue ('SendValue (null variant)' , v, '') ;

   // integer variant
   v := 123 ;
   TTrace.debug.SendValue ('SendValue (integer variant)' , v, '') ;

   // IDispatch Variant
   XmlDoc := CreateOleObject('msxml') ;
   TTrace.debug.SendValue('SendValue (xmldoc : IDispatch )',xmldoc, '') ;

   // variant : arrays
   ArrayTest ;

   // dump, xml
   //---------------------------------
   ttrace.Debug.SendDump({left trace} 'Dump of Hello World',{Short title}'Dump 15 bytes',{adress} Pchar('hello world'), {length} 15) ;
   TTrace.debug.SendXml('xml','<?xml version="1.0" ?><Data> Hello XML </Data>') ;

   // tables
   //---------------------------------
   table := ttrace.CreateTraceTable() ;
   // add titles. Individual columns titles can be added or multiple columns , separated by tabs
   table.AddColumnTitle('colA');          // first column title
   table.AddColumnTitle('colB');          // second column title
   table.AddColumnTitle('title column C'#9'colD');  // other columns title (tab separated)

   // add first line. Individual columns data can be added or multiple columns , separated by tabs
   table.AddRow() ;
   table.AddRowData('a');                           // add first col
   table.AddRowData('b'#9'c'#9'd'#9'e');            // then add other columns (tab separated)

   // add second line
   table.AddRow() ;
   table.AddRowData('aa'#9'data second column'#9'cc'#9'dd'#9'ee');  // add all columns data in a single step (tab separated)

   // finally send the table
   ttrace.Debug.SendTable('Mytable',table) ;

   // Text, dump , table and XML together
   //--------------------------------------------
   table := ttrace.CreateTraceTable() ;
   table.AddColumnTitle('col1'#9'col2');
   table.AddRowData('1'#9'2');

   NodeEx1 := ttrace.Debug.CreateNodeEx();
   NodeEx1.LeftMsg := 'Text, dump , table and XML together' ;
   NodeEx1.Members.Add('Text displayed in detail','','') ;
   NodeEx1.AddDump({Short title}'Dump 15 bytes',{adress} Pchar('hello world'), {length} 15) ;
   NodeEx1.AddXML('<?xml version="1.0" ?><Data> Xml in traceNodeEx </Data>');
   NodeEx1.AddTable(table);
   NodeEx1.Send ;

end;

// Array variant sample
procedure  TForm4.ArrayTest ;
var
   result : OleVariant ;

   psa : psafearray ;  // PSafeArray = ^TSafeArray.   TSafeArray = tagSAFEARRAY = record
   saBound : array [0..1] of TSafeArrayBound;  // record
   c,d : integer ;
   index : array [0..1] of LongInt ;
   v : variant ;
   unassigned : Variant ;
   Null : Variant ;
begin
   // simple array
   v := varArrayCreate ([3,9],varVariant) ;
   TTrace.debug.SendValue ('simple array VT_VARIANT' , v,'') ;

   // generate varEmpty and varNull variant (from Delphi 5 source code)
   TVarData (unassigned).VType := varEmpty ;
   TVarData (Null).VType := varNull ;

   saBound[0].lLbound := 0 ;
   saBound[0].cElements := 3;

   saBound[1].lLbound := 0 ;
   saBound[1].cElements := 15;

   psa := SafeArrayCreate (VT_VARIANT, 2 {dimension} , saBound) ;

   // put some kind of variant on the first column
   index [0] := 0 ;
   index [1] :=  0 ;                                 SafeArrayPutElement(psa, index, v) ;
   index [1] :=  1 ;   v := null ;                   SafeArrayPutElement(psa, index, v) ;
   index [1] :=  2 ;   v := 123 ;                    SafeArrayPutElement(psa, index, v) ;
   index [1] :=  3 ;   v := now ;                    SafeArrayPutElement(psa, index, v) ;
   index [1] :=  4 ;   v := true ;                   SafeArrayPutElement(psa, index, v) ;
   index [1] :=  5 ;   v := 'str' ;                  SafeArrayPutElement(psa, index, v) ;
   index [1] :=  6 ;   v := 100 / 3 ;                SafeArrayPutElement(psa, index, v) ;
   index [1] :=  7 ;   v := 123 ;                    SafeArrayPutElement(psa, index, v) ;
   index [1] :=  8 ;   tagVARIANT(v).vt := VT_I2 ;   SafeArrayPutElement(psa, index, v) ;
   index [1] :=  9 ;   tagVARIANT(v).vt := VT_I4 ;   SafeArrayPutElement(psa, index, v) ;
   index [1] := 10 ;   tagVARIANT(v).vt := VT_I1  ;  SafeArrayPutElement(psa, index, v) ;
   index [1] := 11 ;   tagVARIANT(v).vt := VT_UI1 ;  SafeArrayPutElement(psa, index, v) ;

   index [1] := 12 ;
   v := varArrayCreate ([0,1],varVariant) ;
   v[0] := '789' ;
   OleCheck(SafeArrayPutElement(psa, index, v)) ;

   // fill the other cells with integers
   for c := 1 to 2 do begin
      for d := 0 to 14 do begin
         index [0] := c ;   // 0..2
         index [1] := d ;   // 0..14
         v := c * 100 + d ;
         OleCheck(SafeArrayPutElement(psa, index, v)) ;
      end ;
   end ;

   // convert a safearray to a variant (array of variant)
   tagVARIANT(result).vt := VT_VARIANT or VT_ARRAY ;
   tagVARIANT(result).parray := psa ;

   TTrace.debug.SendValue ('array VT_VARIANT' , result,'') ;

end;


procedure TForm4.butLongTestClick(Sender: TObject);
var
   c,d,f : integer ;
   cNode : IxTraceNode;
   dNode : IxTraceNode;
begin
    TTrace.Debug.Send ('begin','') ;
    for c := 1 to 3 do begin
       cNode := TTrace.Debug.Send ('level c ' + inttostr(c),'') ;
       for d := 1 to 300 do begin
          dNode := cNode.Send ('level d ' + inttostr(d),'') ;
          for f := 1 to 6 do begin
             dNode.Send ('level e ' + inttostr(f),'') ;
             TTrace.Flush(0) ;     // wait until all previous messages (from all threads) are send

          end ;
       end ;
    end ;
    TTrace.Debug.Send ('Last message','') ;
    TTrace.Flush(0) ;     // wait until all previous messages (from all threads) are send
    TTrace.Debug.Send ('flush done','') ;
end;

procedure TForm4.ButIndentClick(Sender: TObject);
var
   node : IxTraceNode ;
begin
   //TTrace.debug.Enabled := false ;
   node := TTrace.debug.send ('Tree indentation using Indent and UnIndent methods','') ;

   node.Indent ('Indent','level 1') ;
      node.send ('Node1','') ;
      node.Indent ('Indent level 2','') ;
         node.send ('Node2','') ;


         // UnIndent with no title
         node.Indent ('Indent level 3','') ;
            node.send ('Node3','') ;
         node.UnIndent ('','') ;   // UnIndent without title

         node.send ('Node4','') ;

      node.UnIndent ('UnIndent level 2', '2') ;
   node.UnIndent ('UnIndent level 1','') ;
end;

procedure TForm4.butShowClick(Sender: TObject);
begin
   // show the viewer
   ttrace.Show(true);
end;

procedure TForm4.butSaveToTextClick(Sender: TObject);
begin
   TTrace.WinTrace.SaveToTextfile('c:\log.txt');
end;

procedure TForm4.butSaveToXMLClick(Sender: TObject);
begin
   TTrace.WinTrace.SaveToXml('c:\log.xml','');
   TTrace.WinTrace.SaveToXml('c:\logWithStyleSheet.xml', 'tracetool.xsl');
end;

procedure TForm4.butLogFileClick(Sender: TObject);
begin
   // set viewer log
   // 0, Viewer Log is disabled.
   // 1, Viewer log enabled.
   // 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   TTrace.WinTrace.setLogFile('c:\logFromViewer.xml',ViewerLogEnabled,-1);
   // set local log
   // 3, Local log is disabled
   // 4, Local log enabled.
   // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   TTrace.WinTrace.setLogFile('c:\LogFromActiveXApi.xml',LocalLogDaily, -1);
end;

procedure TForm4.butClearClick(Sender: TObject);
begin
   TTrace.ClearAll ;
end;

procedure TForm4.butLoadXMLMainClick(Sender: TObject);
begin
   TTrace.WinTrace.LoadXml('c:\log.xml');
end;

procedure TForm4.butStart1Click(Sender: TObject);
begin
   start1 := TTrace.Debug.Send ('Start 1 ..','') ;

   butResend.Enabled := true ;
   butSetSelected.Enabled := true ;

   inc (lasttest) ;
   start3 := start1.Send('Cpt' +inttostr(lasttest),'');
   ButtonCpt.Enabled := true ;
end;

procedure TForm4.butResendClick(Sender: TObject);
begin
   start1.ResendRight ('Done 1') ;
end;

procedure TForm4.butSetSelectedClick(Sender: TObject);
begin
   start1.SetSelected() ;
end;

procedure TForm4.ButtonCptClick(Sender: TObject);
begin
   inc (lasttest) ;
   start3.ResendLeft('Cpt' +inttostr(lasttest)) ;
end;

procedure TForm4.butStart2Click(Sender: TObject);
begin
   start2 := TTrace.Debug.Send ('Start 2 ..','') ;
   butappend.Enabled := true ;
   butfocus.Enabled  := true ;
end;

procedure TForm4.butAppendClick(Sender: TObject);
begin
   start2.AppendLeft ('Done 2') ;
end;

procedure TForm4.butFocusClick(Sender: TObject);
begin
  start2.Show() ;
end;

procedure TForm4.butCreateTraceWinClick(Sender: TObject);
begin
   myWinTrace := ttrace.createWinTrace ('MyWINID' , 'My trace window') ;
   butDisplayWin.Enabled := true ;
   butHelloToWintrace.Enabled := true ;
   butSaveWinToTxt.Enabled := true ;
   butSaveWinToXml.Enabled := true ;
   butClearWin.Enabled := true ;
   butLoadXMLWin.Enabled := true ;
end;

procedure TForm4.butDisplayWinClick(Sender: TObject);
begin
   myWinTrace.DisplayWin() ;
end;

procedure TForm4.butHelloToWintraceClick(Sender: TObject);
begin
   if myWinTrace = nil then
      exit ;

   myWinTrace.Debug.Send ('Hello', 'Can be used to store exceptions, for examples');

   Node1 := myWinTrace.debug.send ('warning in WinTrace1','Symbol') ;
   Node1.SetFontDetail(3,false,false,clGreen,12, 'Symbol') ;
   Node1.send ('child','');

   // create traceNodeEx
   NodeEx1 := myWinTrace.Debug.CreateNodeEx;
   NodeEx1.LeftMsg := 'create traceNodeEx from a existing TraceNode' ;
   NodeEx1.RightMsg := 'with members' ;
   NodeEx1.Members.Add('col1','col2','').Add('child' , '' , '') ;
   NodeEx1.Send ;

end;

procedure TForm4.butSaveWinToTxtClick(Sender: TObject);
begin
   myWinTrace.SaveToTextfile ('c:\log2.txt') ;
end;

procedure TForm4.butSaveWinToXmlClick(Sender: TObject);
begin
   myWinTrace.SaveToXml ('c:\log2.xml','') ;
end;

procedure TForm4.butClearWinClick(Sender: TObject);
begin
   myWinTrace.ClearAll ;
end;

procedure TForm4.butLoadXMLWinClick(Sender: TObject);
begin
   myWinTrace.LoadXml ('c:\log2.xml') ;
end;

procedure TForm4.butMultiColClick(Sender: TObject);
var
   str : string ;
begin
   if MulticolWintrace = nil then begin
      // create a new trace window.
      // First parameter  : Window ID
      // Second parameter : Window Title
      MulticolWintrace := ttrace.createWinTrace ('MCOLID' , 'MultiCol trace window') ;
      // Change the window mode to multi columns
      // The parameter specify the main column index
      // must be called before calling setColumnsTitle.
      MulticolWintrace.setMultiColumn (1) ;
      // set column titles. Each column namles are separated by a tabulation
      MulticolWintrace.setColumnsTitle('col1'+#9+'col2'+#9'col3');
      // set column width
      // Col1 : 150 pixels with a minimum of 130 and a maximum of 250
      // Col2 : 220 pixels with a minimum of 220 (no maximum)
      // Col3 :  70 pixels (no minimum and maximum since this is the last column)
      MulticolWintrace.setColumnsWidth('150:130:250' +#9+'220:100' +#9'70');
      MulticolWintrace.DisplayWin() ;
   end ;
   random (100) ;
   str := DateTimeToStr(now)+#9+inttostr(random(100))+#9+inttostr(random(100)) ;

   MulticolWintrace.Debug.Send(str,'')             // first line
      .SetFontDetail(0,false,false,clGreen,0,'')   // Change font color first line (column 0)
      .Send(#9+'C2'+#9+'C3','' )                   // Add Sub node
      .SetFontDetail(1,true,false,clBlack,0,'')    // Change Sub node style (column 1)
      .SetFontDetail(2,false,true,clBlack,0,'') ;  // Change Sub node style (column 2)
end;

procedure TForm4.butWatchClick(Sender: TObject);
begin

   ttrace.Watches.Send ('Now', now()) ;
   ttrace.Watches.Send ('Now as String', TimeToStr(now)) ;
end;

procedure TForm4.butClearWatchWindowClick(Sender: TObject);
begin
   ttrace.Watches.clearAll() ;
end;

procedure TForm4.butDisplayWatchWindowClick(Sender: TObject);
begin
   ttrace.Watches.DisplayWin() ;
end;

procedure TForm4.butCreateWinWatchClick(Sender: TObject);
begin
   MyWinWatch := ttrace.createWinWatch ('MyWinWatchID' , 'My watches')  ;
end;

procedure TForm4.butWinWatchSendClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
      MyWinWatch.Send ( 'Now', now() ) ;
end;

procedure TForm4.butWinMsgClick(Sender: TObject);
begin
   ttrace.Options.SendMode := WinMsg ;
end;


procedure TForm4.butSocketClick(Sender: TObject);
begin

   // note : when the connection is already opened, it's not possible to switch to another server or port
   ttrace.Options.SendMode := Socket ;
   ttrace.Options.SocketHost := EditHost.Text ;
   ttrace.Options.SocketPort := StrToIntDef(EditPort.Text,8090);
   ttrace.Options.SocketUdp := chkUdp.Checked ;

end;


procedure TForm4.butWinWatchClearClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
      MyWinWatch.ClearAll () ;
end;

procedure TForm4.butWinWatchDisplayClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
      MyWinWatch.DisplayWin () ;
end;

end.
