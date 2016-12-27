// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information

unit unitTest1;

interface

{$INCLUDE TraceTool.Inc}


uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   StdCtrls, TypInfo, ExtCtrls, CheckLst, Grids, comobj, activex, Contnrs,
   ComCtrls, svcmgr,

   TraceTool, {The API}
   SocketTrace, {note : remove the comment on front to enable socket mode (require indy 10)}
   StackTrace,      { note : remove the comment on front to enable stack trace (require jvcl 1.9)}

   SynCommons,

{$IFDEF COMPILER_12_UP}    // delphi 2009 and upper
   generics.collections,
   generics.Defaults,
{$ENDIF COMPILER_12_UP}
   Menus;

type

   // ---------------------------------------------------------------------------
{$M+}

   // demo class
   TClassTest = class(TMemo)
   private
      fTest: TStringList;
   public
      function getTest: TStringList;
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
   published
      property test: TStringList read getTest write fTest;
   end;

   TRecordTest = record
      a : integer ;
      //b : string ;
   end;

   TTestList = array of TRecordTest ; //TClassTest ; // TRecordTest ; //integer ;

   // add type information
   TClassTest2 = class
   private
      ffield1: TClassTest2;
      ffield2: TStringList;
      fstep: integer;
      fTestList : TTestList ;
   published
      property field1: TClassTest2 read ffield1 write ffield1;
      property field2: TStringList read ffield2 write ffield2;
      property step: integer read fstep write fstep;
      property testList : TTestList read fTestList write fTestList;
   end;

   TClassTest3 = class(TClassTest2)
   public
      field3: integer;
      ffield4: integer;
   published
      property field4: integer read ffield4;
   end;



{$M-}

   // ---------------------------------------------------------------------------

   Tform1 = class(TForm)
      PageControl1: TPageControl;
      TabSheet1: TTabSheet;
      TabSheet2: TTabSheet;
      Label2: TLabel;
      butSample: TButton;
      butClear: TButton;
      btnODS: TButton;
      butTail1: TButton;
      Panel2: TPanel;
      butLongTest: TButton;
      butStack: TButton;
      butCreateTraceWin: TButton;
      butResend: TButton;
      butFocus: TButton;
      butSetSelected: TButton;
      butSaveToXML: TButton;
      butSaveToText: TButton;
      butAppend: TButton;
      Panel3: TPanel;
      Memo1: TMemo;
      butShow: TButton;
      TabSheet3: TTabSheet;
      TabSheet4: TTabSheet;
      butDisplayWin: TButton;
      butHelloToWintrace: TButton;
      butSaveWinToTxt: TButton;
      butSaveWinToXml: TButton;
      butClearWin: TButton;
      butStart1: TButton;
      butStart2: TButton;
      Label3: TLabel;
      butLoadXMLWin: TButton;
      butLoadXMLMain: TButton;
      butEventLog: TButton;
      Panel1: TPanel;
      Label4: TLabel;
      Label5: TLabel;
      ButIndent: TButton;
      butTail2: TButton;
      TabSheet5: TTabSheet;
      butWatch: TButton;
      butClearWatchWindow: TButton;
      butDisplayWatchWindow: TButton;
      butCreateWinWatch: TButton;
      butWinWatchSend: TButton;
      butWinWatchClear: TButton;
      butWinWatchDisplay: TButton;
      butTail3: TButton;
      Memo2: TMemo;
      butresendleft: TButton;
      butLogFile: TButton;
      Label6: TLabel;
      Image1: TImage;
      butCloseWinTrace: TButton;
      butIndent2: TButton;
      butCloseWinWatch: TButton;
      butCloseViewer: TButton;
      butMultiCol: TButton;
      butAddChild: TButton;
      GroupBox1: TGroupBox;
      LabelSocketError1: TLabel;
      LabelSocketError2: TLabel;
      EditIP: TEdit;
      rbWin: TRadioButton;
      rbNone: TRadioButton;
      rbLocalHost: TRadioButton;
      rbSocketOther: TRadioButton;
      Edit1: TEdit;
      chkUdp: TCheckBox;
      StatusBar1: TStatusBar;
      butToggleBookmark: TButton;
      butToggleVisible: TButton;
      butSearch: TButton;
      butFilter: TButton;
      butResetFilter: TButton;
    butresendStack: TButton;
      procedure FormCreate(Sender: TObject);
      procedure butSampleClick(Sender: TObject);
      procedure butResendClick(Sender: TObject);
      procedure butShowClick(Sender: TObject);
      procedure butClearClick(Sender: TObject);
      procedure btnODSClick(Sender: TObject);
      procedure butTail1Click(Sender: TObject);
      procedure butAppendClick(Sender: TObject);
      procedure butLongTestClick(Sender: TObject);
      procedure FormDblClick(Sender: TObject);
      procedure butStackClick(Sender: TObject);
      procedure butCreateTraceWinClick(Sender: TObject);
      procedure butFocusClick(Sender: TObject);
      procedure butSetSelectedClick(Sender: TObject);
      procedure butSaveToXMLClick(Sender: TObject);
      procedure butSaveToTextClick(Sender: TObject);
      procedure butStart1Click(Sender: TObject);
      procedure butStart2Click(Sender: TObject);
      procedure butHelloToWintraceClick(Sender: TObject);
      procedure butClearWinClick(Sender: TObject);
      procedure butSaveWinToTxtClick(Sender: TObject);
      procedure butSaveWinToXmlClick(Sender: TObject);
      procedure butDisplayWinClick(Sender: TObject);
      procedure butLoadXMLWinClick(Sender: TObject);
      procedure butLoadXMLMainClick(Sender: TObject);
      procedure butEventLogClick(Sender: TObject);
      procedure butMultiColClick(Sender: TObject);
      procedure ButIndentClick(Sender: TObject);
      procedure butTail2Click(Sender: TObject);
      procedure butWatchClick(Sender: TObject);
      procedure butClearWatchWindowClick(Sender: TObject);
      procedure butDisplayWatchWindowClick(Sender: TObject);
      procedure butCreateWinWatchClick(Sender: TObject);
      procedure butWinWatchSendClick(Sender: TObject);
      procedure butWinWatchClearClick(Sender: TObject);
      procedure butWinWatchDisplayClick(Sender: TObject);
      procedure butTail3Click(Sender: TObject);
      procedure butresendleftClick(Sender: TObject);
      procedure butLogFileClick(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure butCloseWinTraceClick(Sender: TObject);
      procedure butIndent2Click(Sender: TObject);
      procedure butCloseViewerClick(Sender: TObject);
      procedure butCloseWinWatchClick(Sender: TObject);
      procedure butAddChildClick(Sender: TObject);
      procedure rbNoneClick(Sender: TObject);
      procedure EditIPChange(Sender: TObject);
      procedure chkUdpClick(Sender: TObject);
      procedure butToggleBookmarkClick(Sender: TObject);
      procedure butToggleVisibleClick(Sender: TObject);
      procedure butSearchClick(Sender: TObject);
      procedure butFilterClick(Sender: TObject);
      procedure butResetFilterClick(Sender: TObject);
    procedure butresendStackClick(Sender: TObject);
   public
      { Public declarations }
      procedure ArrayTest;
   end;

var
   form1: Tform1;
   start1: ITraceNode;
   start2: ITraceNode;
   start3 : ITraceNode ;
   start3Id : string ;
   node: ITraceNodeEx;
   SubNode: ITraceNodeEx;
   sendNode: ITraceNode;
   HelloNode: ITraceNode;

   myWinTrace: IWinTrace;
   MyWinWatch: IWinWatch;
   MulticolWintrace: IWinTrace;

implementation

// needed to add variant for delphi 6 and upper
{$INCLUDE TraceTool.Inc}

{$IFDEF DELPHI_6_UP} uses variants; {$ENDIF}    // add variant for delphi 6 and upper

{$R *.DFM}

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

{ TClassTest }

// read accessor for 'test' field.
// will be called by TraceTool when we call SendObject on form1 object
procedure TClassTest.AfterConstruction;
begin
   test := TStringList.create;
end;

// ------------------------------------------------------------------------------

procedure TClassTest.BeforeDestruction;
begin
   test.Free;
end;

// ------------------------------------------------------------------------------

function TClassTest.getTest: TStringList;
begin
   result := fTest;
   // recursion ! Blocked by sendObject
   TTrace.Debug.SendObject('getTest', self);
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

procedure Tform1.FormCreate(Sender: TObject);
begin
   left := 0;
   Top := 0;

   TTrace.Options.SendMode := tmWinMsg;
   rbWin.Checked := true;

   if not assigned(AlternateSend) then begin
      LabelSocketError1.caption := 'Socket mode not available.';
      LabelSocketError2.caption := 'Add SocketTrace after TraceTool in the use list.';
      rbLocalHost.Enabled := false;
      rbSocketOther.Enabled := false;
      EditIP.Enabled := false;
   end
   else begin
      LabelSocketError1.caption := '';
      LabelSocketError2.caption := '';
   end;

   PageControl1.ActivePage := TabSheet1;
end;

// ------------------------------------------------------------------------------
// do nothing, just to show that the DblClick event is 'assigned'
procedure Tform1.FormDblClick(Sender: TObject);
begin
   //
end;

// ------------------------------------------------------------------------------

procedure Tform1.rbNoneClick(Sender: TObject);
begin
   if rbNone.Checked then begin
      TTrace.Options.SendMode := tmNone;
   end
   else if rbWin.Checked then begin // windows socket
      TTrace.Options.SendMode := tmWinMsg;
   end
   else if rbLocalHost.Checked then begin
      TTrace.Options.SocketHost := '127.0.0.1';
      TTrace.Options.SendMode := tmAlternate;
   end
   else if rbSocketOther.Checked then begin
      TTrace.Options.SocketHost := EditIP.Text;
      TTrace.Options.SendMode := tmAlternate;
   end;
end;

// ------------------------------------------------------------------------------

procedure Tform1.EditIPChange(Sender: TObject);
begin
   TTrace.Options.SocketHost := EditIP.Text;
end;

// ------------------------------------------------------------------------------

procedure Tform1.chkUdpClick(Sender: TObject);
begin
   TTrace.Options.SocketUdp := chkUdp.Checked;
end;

// ------------------------------------------------------------------------------

// display the tracer
procedure Tform1.butShowClick(Sender: TObject);
begin
   TTrace.Show(true);
end;

// ------------------------------------------------------------------------------

procedure Tform1.butCloseViewerClick(Sender: TObject);
begin
   TTrace.CloseViewer;
end;

// ------------------------------------------------------------------------------

// clear all traces
procedure Tform1.butClearClick(Sender: TObject);
begin
   TTrace.ClearAll;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSampleClick(Sender: TObject);
var
   v: Variant;
   ClassTest: TClassTest;
   ClassTest2: TClassTest2;
   ClassTest3: TClassTest3;
   XmlDoc: Variant;
   table: ITraceTable;

   stringList: TStringList;
   objectList: TObjectList;
   ObjectArray: array [0 .. 2] of TObject;

   WideStr: string;
   SingleByteString: AnsiString;
   Allchars : AnsiString ;
   c : integer ;

   //DynArrayObject: TDynArray;
   DynArrayElementPointer : Pointer ;

   subClassTest0: TClassTest ;
   subClassTest1: TClassTest ;
   subClassTest2: TClassTest ;


{$IFDEF COMPILER_12_UP}     // delphi 2009
   myTlist: TList<TForm>;
{$ENDIF COMPILER_12_UP}

begin

   ClassTest2 := TClassTest2.Create;
   SetLength(ClassTest2.fTestList,3);

   subClassTest0 := TClassTest.Create(nil); ;
   subClassTest1 := TClassTest.Create(nil); ;
   subClassTest2 := TClassTest.Create(nil); ;

//   TTrace.Debug.SendValue('subClassTest0',ClassTest2.testList[0]) ;
//   TTrace.Debug.SendValue('subClassTest1',subClassTest1) ;
//   TTrace.Debug.SendValue('subClassTest2',subClassTest2) ;

   ClassTest2.testList[0].a := 125 ;
   ClassTest2.testList[1].a := 126 ;
   ClassTest2.testList[2].a := 127 ;

//   ClassTest2.testList[0] := subClassTest0;
//   ClassTest2.testList[1] := subClassTest1;
//   ClassTest2.testList[2] := subClassTest2;

   //DynArrayObject.Init(PropInfo^.PropType^,DynArrayPointer) ;     // aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);



   TTrace.Debug.SendValue('array test',ClassTest2) ;

   Exit;



   // the euro char is coded in $AC $20 in unicode and $80 in single byte
   WideStr := Edit1.Text;
   // 'é ù è $ €' ;          // $E9 $00 $20 $00 $F9 $00 $20 $00 $E8 $00 $20 $00 $24 $00 $20 $00 $AC $20
   SingleByteString := AnsiString(WideStr); // $E9 $20 $75 $20 $65 $20 $24 $20 $80

   // TTraceNode sample
   // --------------------------------------------

   TTrace.Debug.Send('delphi demo (unicode)');

   HelloNode := TTrace.Debug.Send('hello');
   with HelloNode.Send('World') do begin
      Send('level3a');
      Send('level3b');
   end;
   HelloNode.Send('level2');
   // HelloNode.GotoNextSibling() ;
   // HelloNode.GotoPrevSibling() ;
   // HelloNode.GotoFirstChild() ;
   // HelloNode.GotoLastChild() ;

   TTrace.Warning.Send('hello2', 'world2');

   TTrace.error.Send('Hello' + #13 + ' World');

   TTrace.Debug.Send('Unicode string', WideStr);
   TTrace.Debug.Send('Single byte string', String(SingleByteString));

   // long text
   TTrace.Debug.Send('qwerty  qwerty qwerty qwerty qwerty qwerty qwerty qwerty qwerty qwerty ');
   TTrace.Debug.Send('hello' + #13 + #10 + 'world');

   // single separator
   TTrace.Debug.Send('---');

   // send traces with special font style (bold and Italic), color font size and font name
   TTrace.Debug.Send('Special font ', 'Symbol 12')
      .SetFontDetail(3, true, false, ClRed) // set col 3 (Left Msg)  to bold and Red
      .SetFontDetail(4, false, false, clGreen, 12, 'Symbol'); // set col 4 (Right Msg) to Green and font size 12
   TTrace.Debug.Send('Impact Italic')
      .SetFontDetail(3, false, true, clBlack, 12, 'Impact');
   // Col3 (left msg), non bold, Italic , Black , font 12 , Impact

   TTrace.Debug.Send('Whole line', 'in red')
      .SetFontDetail(-1, false, false, ClRed); // -1 : all columns

   TTrace.Debug.SendBackgroundColor('Highlighted col in Fuchsia', clFuchsia, 3); // Background Color sample

   // double separator
   TTrace.Debug.Send('===');

   // send also the date close to the time
   // TTrace.Options.SendDate := true ;
   // TTrace.debug.Send ('trace with date') ;
   // TTrace.Options.SendDate := false ;

   // don't send the thread id
   // TTrace.Options.SendThreadId := false ;
   // TTrace.debug.Send ('trace without thread id') ;
   // TTrace.Options.SendThreadId := true ;

   // TTraceNodeEx sample
   // --------------------------------------------
   node := TTrace.Debug.CreateNodeEx;
   node.LeftMsg := 'CreateNodeEx'; // set left msg (col 3)
   node.RightMsg := 'on TTrace.debug'; // set right msg (col4)
   node.AddFontDetail(3, false, false, clFuchsia); // change font detail col 3
   node.AddBackgroundColor(clFuchsia, 4); // change background color col 4
   node.IconIndex := { CST_ICO_CONTROL } 3;
   sendNode := node.Send;

   sendNode.ResendIconIndex( { CST_ICO_PROP } 5); // change icon index after the node is send

   // create sub node of the previsously created node.
   SubNode := node.CreateNodeEx;
   SubNode.LeftMsg := 'CreateNodeEx from another node';
   SubNode.AddFontDetail(3, false, false, clGreen);
   SubNode.Members.Add('My Members', 'col2', 'col3')
      .SetFontDetail(-1, true) // set all columns to bold
      .SetFontDetail(1, false, false, clGreen) // set second column to green (0 index based)
      .Add('Sub members', WideStr) // add sub member node
      .SetFontDetail(0, false, true); // set first column to Italic (0 index based)
   SubNode.Send;

   // SendObject samples
   // --------------------------------------------

   // trace the self object (Tform1)
   TTrace.Debug.SendObject('Form (short)', self); // use default flags
   TTrace.Debug.SendObject('Form (full info)', self, [ShowClassInfo, ShowFields, ShowEvents, ShowMethods]);

   // send Constraints
   TTrace.error.SendObject('Constraints', Constraints);

   // send Memo1
   TTrace.Warning.SendObject('Memo1', Memo1);

   // The read method ( getTest) of the 'test' field  call SendObject
   ClassTest := TClassTest.create(nil);
   TTrace.Debug.SendObject('recursive check', ClassTest);
   ClassTest.Free;

   // Send string sample using CreateNodeEx
   // --------------------------------------------
   node := TTrace.Debug.CreateNodeEx;
   node.LeftMsg := 'Memo1.lines';
   node.AddStrings(Memo1.lines);
   node.Send;

   // Dump sample using CreateNodeEx
   // --------------------------------------------
   node := TTrace.Debug.CreateNodeEx;
   node.LeftMsg := 'test Dump for ' + WideStr;
   node.AddDump('Wide string', pchar(pwidestring(WideStr)), 20); // 9 wide -> 18 bytes
   node.Members.Add('---'); // add separator between the 2 dumps
   node.AddDump('Single byte string', pAnsiString(SingleByteString), 20);
   node.Send;

   Allchars := '' ;
   for c := 0 to 255 do
      Allchars := Allchars + AnsiChar(Chr(c)) ;
   node := TTrace.Debug.CreateNodeEx;
   node.LeftMsg := 'Ansi chars from 0 .. 255 ' ;
   node.AddDump('all chars', Pointer(allchars), 256); // 0..255 = 256
   node.Send;

   // XML sample using Send
   // --------------------------------------------
   TTrace.Debug.SendXml('xml', '<?xml version="1.0" ?><Data> Hello XML </Data>');

   // Image sample using Send
   // --------------------------------------------
   TTrace.Debug.SendBitmap('Bitmap', Image1.Picture.Bitmap);

   // Text, image and XML together
   // --------------------------------------------
   node := TTrace.Debug.CreateNodeEx;
   node.LeftMsg := 'Text, image and XML together';
   node.Members.Add('Text displayed in detail');
   node.AddBitmap(Image1.Picture.Bitmap);
   node.AddXML('<?xml version="1.0" ?><Data> Xml in traceNodeEx </Data>');
   node.Send;

   // send table detail
   // --------------------------------------------

   // create the table
   table := TTrace.CreateTraceTable();

   // add titles. Individual columns titles can be added or multiple columns , separated by tabs
   table.AddColumnTitle('colA'); // first column title
   table.AddColumnTitle('colB'); // second column title
   table.AddColumnTitle('title column C'#9'colD'); // other columns title (tab separated)

   // add first line. Individual columns data can be added or multiple columns , separated by tabs
   table.AddRow();
   table.AddRowData('a'); // add first col
   table.AddRowData('b'#9'c'#9'd'#9'e'); // then add other columns (tab separated)

   // add second line
   table.AddRow();
   table.AddRowData('aa'#9'data second column'#9'cc'#9'dd'#9'ee');
   // add all columns data in a single step (tab separated)

   // finally send the table
   TTrace.Debug.SendTable('Mytable', table);

   // faster way to send table : TStrings / TObjectList / Array of TObject
   // ------------------

   TTrace.Debug.SendTable('memo1.lines', Memo1.lines);

   stringList := TStringList.create;
   stringList.AddObject('self', self);
   stringList.AddObject('self', self);
   stringList.AddObject('self', self);
   TTrace.Debug.SendTable('stringList as table', stringList);
   stringList.Free;

   objectList := TObjectList.create(false);
   objectList.Add(self);
   objectList.Add(self);
   objectList.Add(self);
   TTrace.Debug.SendTable('objectList as table', objectList);
   objectList.Free;

   ObjectArray[0] := self;
   ObjectArray[1] := self;
   ObjectArray[2] := self;
   TTrace.Debug.SendTable('ObjectArray as table', ObjectArray);

   TTrace.Debug.SendTable('status bar', StatusBar1.Panels);

{$IFDEF COMPILER_12_UP}     // delphi 2009
   myTlist := TList<TForm>.create;
   myTlist.Add(self);
   myTlist.Add(self);
   myTlist.Add(self);
   TTrace.Debug.SendTable('myTlist : SendTable', TEnumerableTObject(myTlist));
   // you must cast TList<T> to TEnumerableTObject
{$ENDIF COMPILER_12_UP}


   // SendValue
   // -----------------------------------------

   // variant sample : base type
   TTrace.Debug.SendValue('SendValue (unassigned variant)', v);

   // null variant
   tagVARIANT(v).vt := vt_null;
   TTrace.Debug.SendValue('SendValue (null variant)', v);

   // integer variant
   v := 123;
   TTrace.Debug.SendValue('SendValue (integer variant)', v);

   // IDispatch Variant
   XmlDoc := CreateOleObject('msxml');
   TTrace.Debug.SendValue('SendValue (xmldoc : IDispatch )', XmlDoc);

   // TObject
   TTrace.Debug.SendValue('SendValue (self : TObject)', self);

   ClassTest3 := TClassTest3.create();
   TTrace.Debug.SendValue('SendValue(ClassTest3):inherit fields from ClassTest2', ClassTest3);
   ClassTest3.Free;

   // variant : arrays
   ArrayTest;
end;

// ------------------------------------------------------------------------------

// Array variant sample
procedure Tform1.ArrayTest;
var
   result: OleVariant;

   psa: psafearray; // PSafeArray = ^TSafeArray.   TSafeArray = tagSAFEARRAY = record
   saBound: array [0 .. 1] of TSafeArrayBound; // record
   c, d: integer;
   index: array [0 .. 1] of LongInt;
   v: Variant;
   unassigned: Variant;
   Null: Variant;
begin
   // simple array
   v := varArrayCreate([3, 9], varVariant);
   TTrace.Debug.SendValue('simple array VT_VARIANT', v);

   // generate varEmpty and varNull variant (from Delphi 5 source code)
   TVarData(unassigned).VType := varEmpty;
   TVarData(Null).VType := varNull;

   saBound[0].lLbound := 0;
   saBound[0].cElements := 3;

   saBound[1].lLbound := 0;
   saBound[1].cElements := 15;

   psa := SafeArrayCreate(VT_VARIANT, 2 { dimension } , saBound);

   // put some kind of variant on the first column
   index[0] := 0;
   index[1] := 0;
   SafeArrayPutElement(psa, index, v);
   index[1] := 1;
   v := Null;
   SafeArrayPutElement(psa, index, v);
   index[1] := 2;
   v := 123;
   SafeArrayPutElement(psa, index, v);
   index[1] := 3;
   v := now;
   SafeArrayPutElement(psa, index, v);
   index[1] := 4;
   v := true;
   SafeArrayPutElement(psa, index, v);
   index[1] := 5;
   v := 'str';
   SafeArrayPutElement(psa, index, v);
   index[1] := 6;
   v := 100 / 3;
   SafeArrayPutElement(psa, index, v);
   index[1] := 7;
   v := 123;
   SafeArrayPutElement(psa, index, v);
   index[1] := 8;
   tagVARIANT(v).vt := VT_I2;
   SafeArrayPutElement(psa, index, v);
   index[1] := 9;
   tagVARIANT(v).vt := VT_I4;
   SafeArrayPutElement(psa, index, v);
   index[1] := 10;
   tagVARIANT(v).vt := VT_I1;
   SafeArrayPutElement(psa, index, v);
   index[1] := 11;
   tagVARIANT(v).vt := VT_UI1;
   SafeArrayPutElement(psa, index, v);

   index[1] := 12;
   v := varArrayCreate([0, 1], varVariant);
   v[0] := '789';
   OleCheck(SafeArrayPutElement(psa, index, v));

   // fill the other cells with integers
   for c := 1 to 2 do begin
      for d := 0 to 14 do begin
         index[0] := c; // 0..2
         index[1] := d; // 0..14
         v := c * 100 + d;
         OleCheck(SafeArrayPutElement(psa, index, v));
      end;
   end;

   // convert a safearray to a variant (array of variant)
   tagVARIANT(result).vt := VT_VARIANT or VT_ARRAY;
   tagVARIANT(result).parray := psa;

   TTrace.Debug.SendValue('array VT_VARIANT', result);

end;

// ------------------------------------------------------------------------------

// stack related functions
procedure Tform1.butStackClick(Sender: TObject);
var
   node: ITraceNodeEx;
begin
   if not assigned(SendStackProc) then begin
      Forms.application.MessageBox('In order to use stack info, ' + #13#10 +
         'add the StackTrace unit after Tracetool in this demo use list ' + #13#10 +
         'and set "Debug information" (project compiler options)' + #13#10 +
         'and set "Detailed map file" (project linker options)'
         , 'error', MB_OK);
   end
   else begin
      // stack sample
      node := TTrace.Debug.CreateNodeEx;
      node.LeftMsg := 'Stack information';
      node.AddStackTrace;
      node.Send;

      // Caller sample
      node := TTrace.Debug.CreateNodeEx;
      node.LeftMsg := 'Caller(0)';
      node.AddCaller(0); // 0 : from this line
      node.Send;

      // send method sample
      node := TTrace.Debug.CreateNodeEx;
      node.LeftMsg := 'butShow onclick procedure';
      node.AddMethod(@form1.butShow.OnClick);
      node.Send;

      TTrace.Debug.IndentWithStack('left');
      TTrace.Debug.Send('inner');
      TTrace.Debug.UnIndent();

   end;

end;

// ------------------------------------------------------------------------------
// resend part of a node
procedure Tform1.butResendClick(Sender: TObject);
begin
   start1.ResendRight('Done 1');
end;

// ------------------------------------------------------------------------------

// sample OutputDebugString
// don't work if you run it under the debuger.
var lastTest: integer = 0;

procedure Tform1.btnODSClick(Sender: TObject);
begin
   inc(lastTest);
   // the native version of OutputDebugString is ASCII. So the A and W versions write messages as ansiString.
   OutputDebugStringA(pAnsiChar(AnsiString('test Ansi String ' + inttostr(lastTest))));
   OutputDebugStringW(pWideChar('test Unicode String ' + inttostr(lastTest)));
end;

// ------------------------------------------------------------------------------

procedure Tform1.butTail1Click(Sender: TObject);
const
   fName: string = 'c:\log.txt';
var
   f: textfile;
begin
   assignfile(f, fName);
   if fileexists(fName)
   then
         append(f)
   else
         rewrite(f);
   inc(lastTest);
   writeln(f, 'test ' + inttostr(lastTest) + #0 + 'x');
   closefile(f);
end;

// ------------------------------------------------------------------------------

procedure Tform1.butTail2Click(Sender: TObject);
const
   fName: string = 'c:\log.txt';
var
   f: textfile;
begin
   assignfile(f, fName);
   if fileexists(fName)
   then
         append(f)
   else
         rewrite(f);
   inc(lastTest);
   write(f, 'A ' + inttostr(lastTest) + #0 + 'x');
   closefile(f);
end;

// ------------------------------------------------------------------------------

procedure Tform1.butTail3Click(Sender: TObject);
const
   fName: string = 'c:\log.txt';
var
   f: textfile;
begin
   assignfile(f, fName);
   if fileexists(fName)
   then
         append(f)
   else
         rewrite(f);

   writeln(f, Memo2.lines.Text);
   closefile(f);
end;

procedure Tform1.butAppendClick(Sender: TObject);
begin
   start2.AppendLeft('Done 2');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butLongTestClick(Sender: TObject);
var
   c, d, f: integer;
   cNode: ITraceNode;
   dNode: ITraceNode;
begin
   TTrace.Debug.Send('begin');
   for c := 1 to 3 do begin
      cNode := TTrace.Debug.Send('level c ' + inttostr(c));
      for d := 1 to 300 do begin
         dNode := cNode.Send('level d ' + inttostr(d));
         for f := 1 to 6 do begin
            dNode.Send('level e ' + inttostr(f));
         end;
      end;
   end;
   TTrace.Debug.Send('Last message');
   TTrace.Flush; // wait until all previous messages (from all threads) are send
   TTrace.Debug.Send('flush done');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butCreateTraceWinClick(Sender: TObject);
begin
   myWinTrace := TTrace.createWinTrace('MyWINID', 'My trace window');
   butDisplayWin.Enabled := true;
   butHelloToWintrace.Enabled := true;
   butSaveWinToTxt.Enabled := true;
   butSaveWinToXml.Enabled := true;
   butClearWin.Enabled := true;
   butLoadXMLWin.Enabled := true;
   butCloseWinTrace.Enabled := true;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butFocusClick(Sender: TObject);
begin
   start2.Show();
end;

// ------------------------------------------------------------------------------
var lastToggleBookmark: boolean;

procedure Tform1.butToggleBookmarkClick(Sender: TObject);
begin
   start2.SetBookmark(lastToggleBookmark);
   lastToggleBookmark := not lastToggleBookmark;
end;

// ------------------------------------------------------------------------------

var lastToggleVisible: boolean;

procedure Tform1.butToggleVisibleClick(Sender: TObject);
begin
   start2.SetVisible(lastToggleVisible);
   lastToggleVisible := not lastToggleVisible;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSearchClick(Sender: TObject);
begin


   // ttrace.WinTrace.GotoBookmark(1); // second bookmark, noted [1]
   // ttrace.WinTrace.clearBookmark() ;
   // ttrace.WinTrace.GotoFirstNode() ;
   // ttrace.WinTrace.GotoLastNode() ;

   // TTrace.Find just set the criterias and hightlight if asked, but don't move to the next matching node.
   TTrace.Find('StRinG', { Sensitive } false, { WholeWord } true, { Highlight } true, { SearchInAllPages } true);

   // from the current node : go to the next item matching criteria. Call ttrace.WinTrace.GotoFirstNode() before FindNext to start search from first node
   TTrace.WinTrace.FindNext( { SearForward } true);
end;

// ------------------------------------------------------------------------------

procedure Tform1.butFilterClick(Sender: TObject);
begin
   TTrace.WinTrace.ClearFilter();

   // 5 kinds of filters
   // -------------------
   // Equal = 0
   // Not equal = 1
   // Contains = 2
   // Don't contains = 3
   // (Ignore this filter) = 4 or -1

   // filters can be applied on all columns. Note that the "icone" column is not zero but 999. The members are identified by the column 998
   // On multicolumn mode. Column 0 can be used normally.

   TTrace.WinTrace.AddFilter( { col icone } 999, { Equal } 0, '24');
   // 999 : Icon column . Filter on "info" (index is 24)
   TTrace.WinTrace.AddFilter( { col time } 1, { Not equal } 1, 'string');
   TTrace.WinTrace.AddFilter( { col thread } 2, { Contains } 2, '0x');
   TTrace.WinTrace.AddFilter( { col traces } 3, { Don't contains } 3, 'nothing');
   TTrace.WinTrace.AddFilter( { col Comment } 4, { (Ignore this filter) } -1, 'string');
   // -1 or 4 can be used to disable this filter (not very usefull...)
   TTrace.WinTrace.AddFilter( { col members } 998, { Contains } 2, 'string'); // members info : 998

   TTrace.WinTrace.ApplyFilter( { ConditionAnd } true, { ShowMatch } true, { IncludeChildren } true);

end;

// ------------------------------------------------------------------------------

procedure Tform1.butResetFilterClick(Sender: TObject);
begin
   TTrace.WinTrace.ClearFilter();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSetSelectedClick(Sender: TObject);
begin
   start1.SetSelected();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSaveToXMLClick(Sender: TObject);
begin
   // TTrace.WinTrace.SaveToXml('c:\log.xml');
   TTrace.WinTrace.SaveToXml('c:\logWithStyleSheet.xml', 'tracetool.xsl');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butLoadXMLMainClick(Sender: TObject);
begin
   TTrace.WinTrace.LoadXml('c:\log.xml');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSaveToTextClick(Sender: TObject);
begin
   TTrace.WinTrace.SaveToTextfile('c:\log.txt');
end;

// ------------------------------------------------------------------------------



procedure Tform1.butStart1Click(Sender: TObject);
begin
   start1 := TTrace.Debug.Send('Start 1 ..');
   butResend.Enabled := true;
   butSetSelected.Enabled := true;
   butresendleft.Enabled := true;
   butAddChild.Enabled := true;
   butresendStack.Enabled := true;

   inc(lastTest);
   start3 := start1.Send('Cpt' + inttostr(lastTest));
   start3Id := start3.id ;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butresendleftClick(Sender: TObject);
var
   recreatedNode : ITraceNode ;
begin
   inc(lastTest);

   // normal method : use the start3 node
   //start3.ResendLeft('Cpt : ' + inttostr(lastTest));

   // advanced method : recreate the node from saved Id then call normal method
   recreatedNode := TTrace.CreateNodeFromId(nil, start3Id) ;
   recreatedNode.ResendLeft('Cpt : ' + inttostr(lastTest));
end;

// ------------------------------------------------------------------------------

procedure Tform1.butresendStackClick(Sender: TObject);
begin
   start1.AppendStack() ;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butAddChildClick(Sender: TObject);
begin
   start1.Send('child');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butStart2Click(Sender: TObject);
begin
   start2 := TTrace.Debug.Send('Start 2 ..');
   butAppend.Enabled := true;
   butFocus.Enabled := true;
   butToggleBookmark.Enabled := true;
   butToggleVisible.Enabled := true;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butHelloToWintraceClick(Sender: TObject);
begin
   myWinTrace.Debug.Send('Hello', 'Can be used to store exceptions, for examples');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butClearWinClick(Sender: TObject);
begin
   myWinTrace.ClearAll;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSaveWinToTxtClick(Sender: TObject);
begin
   myWinTrace.SaveToTextfile('c:\log2.txt');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butSaveWinToXmlClick(Sender: TObject);
begin
   myWinTrace.SaveToXml('c:\log2.xml');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butDisplayWinClick(Sender: TObject);
begin
   myWinTrace.DisplayWin();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butLoadXMLWinClick(Sender: TObject);
begin
   myWinTrace.LoadXml('c:\log2.xml');
end;

// ------------------------------------------------------------------------------

procedure Tform1.butCloseWinTraceClick(Sender: TObject);
begin
   myWinTrace.Close();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butEventLogClick(Sender: TObject);
begin
   inc(lastTest);
   with TEventLogger.create('Application') do begin
      // LogMessage('---');
      LogMessage('test' + inttostr(lastTest));
      // LogMessage('===');
      Free;
   end;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butMultiColClick(Sender: TObject);
var
   str: string;
begin
   if MulticolWintrace = nil then begin
      // create a new trace window.
      // First parameter  : Window ID
      // Second parameter : Window Title
      MulticolWintrace := TTrace.createWinTrace('MCOLID', 'MultiCol trace window');
      // Change the window mode to multi columns
      // The parameter specify the main column index
      // must be called before calling setColumnsTitle.
      MulticolWintrace.setMultiColumn(1);
      // set column titles. Each column namles are separated by a tabulation
      MulticolWintrace.setColumnsTitle('col1' + #9 + 'col2' + #9'col3');
      // set column width
      // Col1 : 150 pixels with a minimum of 130 and a maximum of 250
      // Col2 : 220 pixels with a minimum of 220 (no maximum)
      // Col3 :  70 pixels (no minimum and maximum since this is the last column)
      MulticolWintrace.setColumnsWidth('150:130:250' + #9 + '220:100' + #9'70');
      MulticolWintrace.DisplayWin();
      // set local log
      // 3, Local log is disabled
      // 4, Local log enabled. No size limit.
      // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      MulticolWintrace.SetLogFile('c:\MultiCol.xml', 4);
   end;
   random(100);
   str := DateTimeToStr(now) + #9 + inttostr(random(100)) + #9 + inttostr(random(100));

   MulticolWintrace.Debug.Send(str) // first line
      .SetFontDetail(0, false, false, clGreen) // Change font color first line (column 0)
      .Send(#9 + 'C2' + #9 + 'C3') // Add Sub node
      .SetFontDetail(1, true, false) // Change Sub node style (column 1)
      .SetFontDetail(2, false, true); // Change Sub node style (column 2)
end;

// ------------------------------------------------------------------------------

procedure Tform1.ButIndentClick(Sender: TObject);
var
   node: ITraceNode;
   indentNode : ITraceNode ;
begin
   // TTrace.debug.Enabled := false ;
   node := TTrace.Debug.Send('Tree indentation using Indent and UnIndent methods');
   node.Send('indent is', node.IndentLevel);
   node.Indent('Indent', inttostr(node.IndentLevel));
   node.Send('Node1', node.IndentLevel);
   node.Indent('Indent', inttostr(node.IndentLevel));
   node.Send('Node2', node.IndentLevel);

   // UnIndent with no title
   indentNode := node.Indent('Indent', inttostr(node.IndentLevel));
   indentNode.ResendLeft('Indent2') ;
   node.Send('Node3', node.IndentLevel);
   node.UnIndent(); // UnIndent without title

   node.Send('Node4', node.IndentLevel);

   node.UnIndent('UnIndent');
   node.UnIndent('UnIndent');

   TTrace.Debug.Indent('debug.indent');
   TTrace.Warning.Send('warning.send');
   TTrace.error.UnIndent('Error.UnIndent');
   TTrace.Debug.Send('debug after unindent');

   TTrace.Debug.EnterMethod('ButIndentClick', '', clLime);
   TTrace.Debug.Send('level 0');
   TTrace.Debug.ExitMethod('ButIndentClick', '', clLime);
end;

// ------------------------------------------------------------------------------

procedure Tform1.butIndent2Click(Sender: TObject);
var
   node: ITraceNodeEx;
begin
   TTrace.Debug.Send('root 1', TTrace.Debug.IndentLevel);
   TTrace.Debug.Indent('start indentation');
   TTrace.Debug.Send('under indent 1', TTrace.Debug.IndentLevel);

   node := TTrace.Debug.CreateNodeEx();
   // node := TTrace.createNodeEx (TTrace.debug) ;

   node.LeftMsg := 'under indent 2';
   node.Send;
   TTrace.Debug.UnIndent();
   TTrace.Debug.Send('root 2', TTrace.Debug.IndentLevel);
end;

// ------------------------------------------------------------------------------

var
   test2: TClassTest2;

procedure Tform1.butWatchClick(Sender: TObject);
begin
   if test2 = nil then begin
      test2 := TClassTest2.create;
      test2.step := 0;
   end;

   if test2.step = 0 then begin
      test2.ffield1 := nil;
      test2.ffield2 := nil;
   end
   else if test2.step = 1 then begin
      test2.ffield1 := TClassTest2.create;
   end
   else if test2.step = 2 then begin
      test2.ffield2 := TStringList.create;
   end
   else if test2.step = 3 then begin
      test2.ffield2.Add('line');
   end;

   test2.step := test2.step + 1;
   TTrace.Watches.Send('test2', test2);
   TTrace.Watches.Send('Now', now());
   TTrace.Watches.Send('Now as String', TimeToStr(now));
end;

// ------------------------------------------------------------------------------

procedure Tform1.butClearWatchWindowClick(Sender: TObject);
begin
   TTrace.Watches.ClearAll();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butDisplayWatchWindowClick(Sender: TObject);
begin
   TTrace.Watches.DisplayWin();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butCreateWinWatchClick(Sender: TObject);
begin
   MyWinWatch := TTrace.createWinWatch('MyWinWatchID', 'My watches');
   butWinWatchSend.Enabled := true;
   butWinWatchClear.Enabled := true;
   butWinWatchDisplay.Enabled := true;
   butCloseWinWatch.Enabled := true;
end;

// ------------------------------------------------------------------------------

procedure Tform1.butWinWatchSendClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
         MyWinWatch.Send('Now', now());
end;

// ------------------------------------------------------------------------------

procedure Tform1.butWinWatchClearClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
         MyWinWatch.ClearAll();
end;

// ------------------------------------------------------------------------------

procedure Tform1.butWinWatchDisplayClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
         MyWinWatch.DisplayWin();
end;
// ------------------------------------------------------------------------------

procedure Tform1.butCloseWinWatchClick(Sender: TObject);
begin
   if MyWinWatch <> nil then
         MyWinWatch.Close();

end;

// ------------------------------------------------------------------------------
procedure Tform1.butLogFileClick(Sender: TObject);
begin
   // set viewer log
   // 0, Viewer Log is disabled.
   // 1, Viewer log enabled.
   // 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   TTrace.WinTrace.SetLogFile('c:\logFromViewer.xml', 1);
   // set local log
   // 3, Local log is disabled
   // 4, Local log enabled.
   // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   TTrace.WinTrace.SetLogFile('c:\LogFromDelphiApi.xml', 4);
end;
// ------------------------------------------------------------------------------

procedure Tform1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   try
      if test2 <> nil then begin
         if test2.field1 <> nil then
               test2.field1.Free;
         if test2.field2 <> nil then
               test2.field2.Free;
         test2.Free;
      end;
   except
   end;
end;

// ------------------------------------------------------------------------------

end.
