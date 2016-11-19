' Import type library : right click Project then properties. 
' In the reference tab, click 'add', select 'COM' then tracetoolCom 9.0 library

Imports TraceToolCom


Public Class Form1
   Public TTrace As XTrace
   Public NodeEx1, NodeEx2 As IXTraceNodeEx

   Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
      ' instanciate tracetool system
      TTrace = New XTrace()
      TTrace.Options.ColorKind = ColorKind.RGB ' tracetool need to know how colors are coded
   End Sub

   Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
      Dim Line2 As IXTraceNode
      Dim v

      ' TTraceNode sample
      '--------------------------------------------

      Line2 = TTrace.Debug.Send("hello2")
      Line2.Send("World2", "")

      TTrace.Warning.Send("hello", "world")

      ' long text
      TTrace.Debug.Send("qwerty  qwerty qwerty qwerty qwerty qwerty qwerty qwerty qwerty qwerty ", "")
      TTrace.Debug.Send("hello" + Chr(13) + "world")

      ' single separator
      TTrace.Debug.Send("---")

      ' send traces with special font style (bold and Italic), color font size and font name
      With TTrace.Debug.Send("Special font ", "Symbol 12")
         .SetFontDetail(3, True, False, Color.Red.ToArgb())                     ' set col 3 (Left Msg)  to bold and Red
         .SetFontDetail(4, False, False, Color.Green.ToArgb(), 12, "Symbol")    ' set col 4 (Right Msg) to Green and font size 12
      End With

      With TTrace.Debug.Send("Impact Italic", "")
         .SetFontDetail(3, False, True, Color.Black.ToArgb(), 12, "Impact")      ' Col3 (left msg), non bold, Italic , Black , font 12 , Impact
      End With

      With TTrace.Debug.Send("Whole line", "in red")
         .SetFontDetail(-1, False, False, Color.Red.ToArgb())                    ' -1 : all columns
      End With

      ' double separator
      TTrace.Debug.Send("===")

      'TTrace.Options.SendDate = true 
      'TTrace.debug.Send ("trace with date")
      'TTrace.Options.SendDate = false 

      ' TTraceNodeEx sample
      '--------------------------------------------
      NodeEx1 = TTrace.Debug.CreateNodeEx()
      NodeEx1.LeftMsg = "ttrace.Debug.CreateNodeEx()"
      NodeEx1.IconIndex = 3  ' CST_ICO_CONTROL 
      NodeEx1.Members.Add("A", "B", "C")
      NodeEx1.AddFontDetail(3, False, False, Color.Fuchsia.ToArgb())     ' change font detail col 3
      NodeEx1.AddBackgroundColor(Color.Fuchsia.ToArgb(), 4)                ' change background color col 4
      NodeEx1.IconIndex = 3 ' CST_ICO_CONTROL

      Line2 = NodeEx1.Send
      Line2.ResendIconIndex(5)                ' change icon index after the node is send {CST_ICO_PROP}

      NodeEx2 = Line2.CreateNodeEx()
      NodeEx2.LeftMsg = "CreateNodeEx from another node"
      NodeEx2.AddFontDetail(3, False, False, Color.Green.ToArgb())
      With NodeEx2.Members.Add("My Members", "Col2", "col3")
         .SetFontDetail(-1, True, False, Color.Black.ToArgb())      ' set all columns to bold
         .SetFontDetail(1, False, False, Color.Green.ToArgb())      ' set second column to green (0 index based)
         .Add("Sub members", "", "")                                ' add sub member node
         .SetFontDetail(0, False, True, Color.Black.ToArgb())       ' set first column to Italic (0 index based)
      End With
      NodeEx2.Send()

      ' SendObject samples
      '---------------------------------
      TTrace.Debug.SendObject("SendObject", TTrace)
      TTrace.Debug.SendValue("SendValue", TTrace, "ttrace")
      TTrace.Debug.SendValueIDisptach("SendValueIDisptach", TTrace, "ttrace")

      ' null variant
      v = DBNull.Value

      TTrace.Debug.SendValue("SendValue (null variant)", v)

      ' integer variant
      v = 123
      TTrace.Debug.SendValue("SendValue (integer variant)", v)

      ' IDispatch Variant. Note : Add the Microsoft XML 3 "COM" library in your project reference 
      Dim xmldoc As New MSXML2.DOMDocument30

      TTrace.Debug.SendValue("SendValue (xmldoc : IDispatch )", xmldoc)

      ' dump, xml
      '---------------------------------
      Dim str As String = "qwerty é ù è azerty"
      TTrace.Debug.SendDump("Dump", "Dump 15 bytes", str, 15)
      TTrace.Debug.SendXml("xml", "<?xml version='1.0' ?><Data> Hello XML </Data>")

      ' tables
      '---------------------------------
      Dim table As IXTraceTable

      table = TTrace.CreateTraceTable()
      ' add titles. Individual columns titles can be added or multiple columns , separated by tabs
      table.AddColumnTitle("colA")          ' first column title
      table.AddColumnTitle("colB")          ' second column title
      table.AddColumnTitle("title column C" + Chr(9) + "colD")  ' other columns title (tab separated)

      ' add first line. Individual columns data can be added or multiple columns , separated by tabs
      table.AddRow()
      table.AddRowData("a")                           ' add first col
      table.AddRowData("b" + Chr(9) + "c" + Chr(9) + "d" + Chr(9) + "e")            ' then add other columns (tab separated)

      ' add second line
      table.AddRow()
      table.AddRowData("aa" + Chr(9) + "data second column" + Chr(9) + "cc" + Chr(9) + "dd" + Chr(9) + "ee")  ' add all columns data in a single step (tab separated)

      ' finally send the table
      TTrace.Debug.SendTable("Mytable", table)

      ' Text, dump , table and XML together
      '--------------------------------------------
      table = TTrace.CreateTraceTable()
      table.AddColumnTitle("col1" + Chr(9) + "col2")
      table.AddRowData("1" + Chr(9) + "2")

      NodeEx1 = TTrace.Debug.CreateNodeEx()
      NodeEx1.LeftMsg = "Text, dump , table and XML together"
      NodeEx1.Members.Add("Text displayed in detail")
      NodeEx1.AddDump("Dump 15 bytes", "hello world", 15)
      NodeEx1.AddXml("<?xml version='1.0' ?><Data> Xml in traceNodeEx </Data>")
      NodeEx1.AddTable(table)
      NodeEx1.Send()

   End Sub

End Class
