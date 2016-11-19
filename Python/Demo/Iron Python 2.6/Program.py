import clr
clr.AddReference('System.Windows.Forms')
clr.AddReference('System.Drawing')

from System.Windows.Forms import Application

Application.EnableVisualStyles()

from tracetool import *

import platform
import sys
import inspect
import threading
from sys import *

# -----------------------------------------------------------------------------------------

# used by the demo
class sampleClass1:
    def __init__(self):
        TTrace.debug.send ("__init__")
    @staticmethod
    def sampleStaticMethod(p3, p4):
        pass
    
    def instanceMethod (self,p1,p2):
        pass
    
    class InnerClass:
        def __init__(self):
            pass
        def InnerFunction(self):
            pass 
        
def sampleFunction(p5, p6=5):
    """ doc string for sample"""
    ret1 = p6
    ret2 = "hello"
    ret3 = ("%s" % (ret2),p5,ret1)   # tuple
    return ret3                      # return a tuple

# -----------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------

class Demo(object):
    def __init__(self):
        pass
    
    # -----------------------------------------------------------------------------------------

    def ButSampleClick(self, sender, e):
        ttrace.debug.send ("hello world")
        ttrace.debug.send ("hello","world")
        ttrace.debug.send (leftMsg="hello")
        ttrace.debug.send (rightMsg="world")
        ttrace.debug.send ("hello").send ("world")
        # single separator
        ttrace.debug.send("---")
        
        #ttrace.debug.send ("python version : "  , version_info)
        ttrace.debug.send ("Platform.system(): ", platform )    # 'Windows' or 'cli'
        ttrace.debug.send ("sys.version : "     , sys.version)               # CPython


        # send traces with special font style (bold and Italic), color font size and font name
        # use System.Drawing.Color.ToArgb() (not supported in Silverlight) or (int)Helper.ToArgb(System.Windows.Media.Color) to specify Argb color
        node = ttrace.debug.send("Special font", "Symbol 12") 
        node.setFontDetail(-1, False, True)                                             # set whole line to italic 
        node.setFontDetail(3, True, False, ttrace.RgbColors["Red"])                     # set col 3 (Left Msg)  to bold and Red
        node.setFontDetail(4, False, False, ttrace.RgbColors["Green"], 12, "Symbol")    # set col 4 (Right Msg) to Green , font size 12 , Symbol
        node = ttrace.debug.send("Impact Italic")
        node.setFontDetail(3, False, True, ttrace.RgbColors["Fuchsia"], 12, "Impact")   # Col3 (left msg), non bold, Italic , Blue-Violet , font 12 , Impact
        # double separator
        ttrace.debug.send("===")

        # traces using TraceNodeEx
        #--------------------------------------------
        node = TraceNodeEx(ttrace.debug)
        node.leftMsg = "TraceNodeEx"
        node.rightMsg = str
        node.addFontDetail(3, False, False, ttrace.RgbColors["Green"])
        node.iconIndex = 8
        member = node.members.add("My Members", "col2", "col3")
        member.setFontDetail(0, True)                                 # set first column to bold
        member.setFontDetail(1, False, False, ttrace.RgbColors["Green"])   # set second column to green
        submember = member.add("Sub members")                                     # add sub member node
        submember.setFontDetail(0, False, True)                      # set first column to Italic
        node.addStackTrace()   # don't work properly on IronPython 2.6
        node.addCaller()
        
        sendNode = node.send()
        sendNode.resendIconIndex(5)  # change icon index after the node is send

        # XML sample using Send
        #--------------------------------------------
        ttrace.debug.sendXml("xml", "<?xml version='1.0' ?><Data> Hello XML </Data>")

        # Text, and XML together
        #--------------------------------------------
        node = TraceNodeEx(ttrace.debug)
        node.leftMsg = "Text and XML together"
        node.members.add("Text displayed in detail")
        node.addXML("<?xml version='1.0' ?><Data> Xml in traceNodeEx </Data>")
        node.send()

        # group of traces enabled / disabled
        #------------------------------------
        groupTrace = TraceNode(None, False)  # dummy node not send to viewer 
        groupTrace.iconIndex = 5
        groupTrace.enabled = True
        groupTrace.send("GroupTrace traces 1")    # send to viewer
        groupTrace.enabled = False
        groupTrace.send("GroupTrace traces 2")    # not send : group not enabled

        ttrace.debug.sendDump("doc de str", "".__doc__)   

        # ensure all traces are send to the viewer
        ttrace.flush()

    # -----------------------------------------------------------------------------------------

    def ButIndentClick(self, sender, e):
        node = TTrace.debug.send("Tree indentation using Indent and UnIndent methods")

        node.indent("Indent", "level 1", TTrace.RgbColors["Green"])  # Line in color
        node.send("Node1")
        node.indent("Indent level 2", isEnter=True)                  # add a "enter" icon
        node.send("Node2")
        node.indent("Indent level 3")
        node.send("Node3")
        node.unIndent()                                              # UnIndent without title
        node.send("root 1")
        node.unIndent("UnIndent level 2", isExit=True)               # add a "Leave" icon
        node.unIndent("UnIndent level 1")

        TTrace.debug.send("root 2", TTrace.debug.getIndentLevel())
        TTrace.debug.indent("start indentation")
        TTrace.debug.send("under indent 1", TTrace.debug.getIndentLevel())
        
        # node indentation using traceNodeEx
        nodeEx = TraceNodeEx(TTrace.debug)   # Parent depends of the indentation
        nodeEx.leftMsg = "under indent 2"
        nodeEx.send()
        TTrace.debug.unIndent()
        TTrace.debug.send("root 3", TTrace.debug.getIndentLevel())
        
        # Speed test
        #for c in range (30000):
        #    TTrace.debug.send("test",c)

    # -----------------------------------------------------------------------------------------

    def ButSendObjectClick(self, sender, e):
        #sendObject samples
        #-----------     
        TTrace.debug.sendObject("TTrace class"        , TTrace)                           
        TTrace.debug.sendObject("TTrace.winTrace.debug"       , TTrace.winTrace.debug)        # TraceNode object
        
        # some functions
        TTrace.debug.sendObject("TTrace.__new__"  ,TTrace.__new__)   
        TTrace.debug.sendObject("sampleFunction"                  , sampleFunction)                   
        TTrace.debug.sendObject("sampleClass.sampleStaticMethod"  , sampleClass1.sampleStaticMethod)   
        TTrace.debug.sendObject("TTrace.winTrace.debug.send"           , TTrace.winTrace.debug.send)  # (bound method)
        TTrace.debug.sendObject("TTrace.clearAll"                 , TTrace.clearAll)                  # (unbound method)
        TTrace.debug.sendObject("lambda expression"               , lambda s: " ".join(s.split()))    
        TTrace.debug.sendObject("isinstance"                      , isinstance)                       
        TTrace.debug.sendObject("[].append"                       , [].append)   
        
        # basic objects
        TTrace.debug.sendObject("integer 5"                       , 5)                                
        TTrace.debug.sendObject("set : set([4,5,6])"              , set([4,5,6]))
        TTrace.debug.sendObject("tuple : (1,2,3)"                 , (1,2,3) )
        TTrace.debug.sendObject("array : [4,5,6]"                 , [4,5,6])
        TTrace.debug.sendObject("map :ttrace.RgbColors"           , ttrace.RgbColors)

    # -----------------------------------------------------------------------------------------

    def ButSendValueClick(self, sender, e):
        testobj = TTrace.debug
        listObjToSend = inspect.classify_class_attrs(type(testobj))
        lst = [TTrace.winTrace,TMemberNode(),TTrace]
        
        TTrace.debug.sendValue("sendValue : classify_class_attrs ", listObjToSend, True, 3, "debug node")    
        TTrace.debug.sendValue("sendValue : list", lst, True, 3, "title")
        TTrace.debug.sendValue("integer 5", 5)
        TTrace.debug.sendValue("long 5", 5L)
        TTrace.debug.sendValue("ttrace.RgbColors", ttrace.RgbColors)

    # -----------------------------------------------------------------------------------------

    def ButSendTableClick(self, sender, e):
        # array of object (same types)
        testobj = TTrace.debug # create an object and print some of it's attributes
        TTrace.debug.sendTable("sendTable : classify_class_attrs ", 
                inspect.classify_class_attrs(type(testobj))
                )    
        
        # array of object of different types (no sense : not well displayed)
        TTrace.debug.sendTable("sendTable : list ", 
                [TTrace.winTrace,TMemberNode(),TTrace]
                )   
        
        # map         
        TTrace.debug.sendTable("map :ttrace.RgbColors" ,ttrace.RgbColors)
        
        # array of map
        TTrace.debug.sendTable("array of map" ,[
                {"k1":10 , "k2":20 , "k3":33 ,"k4":44},
                {"k1":11 , "k2":22 , "k8":88 ,"k4":44},
                {"k1":15 , "k2":25 , "k3":33 ,"k8":86}
                ])
        
        # send tuple of map
        TTrace.debug.sendTable("tuple of map" ,(
                {"k1":10 , "k2":20 , "k3":33 ,"k4":44},
                {"k1":11 , "k2":22 , "k8":88 ,"k4":44},
                {"k1":15 , "k2":25 , "k3":33 ,"k8":86}
                ))
        
        # array of tuple 
        TTrace.debug.sendTable("array of tuple" ,[
                (1,2,3),
                ("hello","world"),
                (True, "hello", TTrace , TTrace.debug, TTrace.debug),
                ])        
        # map
        TTrace.debug.sendTable("map"  ,{
                "k1": (1,2,3),
                "k2": 123,
                "k3": "123"
                })
        
        # range() , xrange() and enumerate()
        TTrace.debug.sendTable("xrange(10) ", xrange(10))
        TTrace.debug.sendTable("xrange(5,10) ", xrange(5,10))
        TTrace.debug.sendTable("enumerate(['a','b','c']) ", enumerate(['a','b','c']))
        
        TTrace.debug.sendTable("threads", threading.enumerate())

    # -----------------------------------------------------------------------------------------

    def ButStackClick(self, sender, e):
        try:
            5 / 0
        except Exception: #IGNORE:W0703
            TTrace.debug.sendStack("exception")

    # -----------------------------------------------------------------------------------------


    def ChkSendFunctionsCheckedChanged(self, sender, e):
        ttrace.options.sendFunctions = form._chkSendFunctions.Checked

    # -----------------------------------------------------------------------------------------

    def ChkSendClassInfoCheckedChanged(self, sender, e):
        ttrace.options.sendClassInfo = form._chkSendClassInfo.Checked

    # -----------------------------------------------------------------------------------------

    def ChkSendPrivateCheckedChanged(self, sender, e):
        ttrace.options.sendPrivate = form._chkSendPrivate.Checked

    # -----------------------------------------------------------------------------------------

    def ChkSendDocCheckedChanged(self, sender, e):
        ttrace.options.sendDoc = form._chkSendDoc.Checked

    # -----------------------------------------------------------------------------------------

    def ChkSendThtreadNameCheckedChanged(self, sender, e):
        ttrace.options.sendThreadName = form._chkSendThtreadName.Checked

    # -----------------------------------------------------------------------------------------

    def ChkSendMessageUsingThreadCheckedChanged(self, sender, e):
        ttrace.options.useThread = form._chkSendMessageUsingThread.Checked
 
    def TextBoxTreeDepthTextChanged(self, sender, e):
        try:
            ttrace.options.objectTreeDepth = int(form._textBoxTreeDepth.Text)
        except BaseException :
            ttrace.options.objectTreeDepth = 0
            
    def TextBoxSocketHostTextChanged(self, sender, e):
        ttrace.options.socketHost = form._textBoxSocketHost.Text  

    def TextBoxSocketPortTextChanged(self, sender, e):
        try:
            ttrace.options.socketPort = int( form._textBoxSocketPort.Text) 
        except BaseException :
            ttrace.options.socketPort = 8090 

    # -----------------------------------------------------------------------------------------

    def ButCreateWindowClick(self, sender, e):
        self.winTrace = WinTrace(winTraceID = "MyWinTraceId" , winTraceTitle = "My WinTrace")       #IGNORE:W0201
        
        form._butWinClose.Enabled = True
        form._butWinClear.Enabled = True
        form._butWinLoadXml.Enabled = True
        form._butWinSaveToXml.Enabled = True
        form._butWinSaveToText.Enabled = True
        form._butWinSetLogFile.Enabled = True
        form._butWinSayHello.Enabled = True
        form._butWinDisplay.Enabled = True

    # -----------------------------------------------------------------------------------------

    def ButWinDisplayClick(self, sender, e):
        self.winTrace.displayWin()

    # -----------------------------------------------------------------------------------------

    def ButWinSayHelloClick(self, sender, e):
        self.winTrace.debug.send("Hello", "World") 

    # -----------------------------------------------------------------------------------------

    def ButWinSetLogFileClick(self, sender, e):
        # viewer log :
        # 0, Viewer Log is disabled.
        # 1, Viewer log enabled. 
        # 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
        # local log :
        # 3, Local log is disabled
        # 4, Local log enabled. No size limit.
        # 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
        self.winTrace.setLogFile("c:\\MyWinTrace.xml", 1)

    # -----------------------------------------------------------------------------------------

    def ButWinSaveToTextClick(self, sender, e):
        self.winTrace.saveToTextfile("c:\\MyWinTrace.txt")

    # -----------------------------------------------------------------------------------------

    def ButWinSaveToXmlClick(self, sender, e):
        self.winTrace.saveToXml("c:\\MyWinTrace.xml") 

    # -----------------------------------------------------------------------------------------

    def ButWinLoadXmlClick(self, sender, e):
        self.winTrace.loadXml("c:\\MyWinTrace.xml")

    # -----------------------------------------------------------------------------------------

    def ButWinClearClick(self, sender, e):
        self.winTrace.clearAll()

    # -----------------------------------------------------------------------------------------

    def ButWinCloseClick(self, sender, e):
        self.winTrace.close()

    # -----------------------------------------------------------------------------------------

    def ButMultiColTestClick(self, sender, e):
        if not hasattr(self, "MultiColTrace"):
            self.MultiColTrace = WinTrace("MCOL", "MultiCol trace window")          #IGNORE:W0201
            self.MultiColTrace.setMultiColumn(1) # must be called before calling setColumnsTitle
            self.MultiColTrace.setColumnsTitle("col1 \t col2 \t col3")
            self.MultiColTrace.setColumnsWidth("100:20:80 \t 200:50 \t 100")
            self.MultiColTrace.displayWin()
         
        self.MultiColTrace.debug.send("1 \t 2 \t 3")

    # -----------------------------------------------------------------------------------------

    def ButMainSendWatchClick(self, sender, e):
        TTrace.watches.send("testobj", TTrace.debug)

    # -----------------------------------------------------------------------------------------

    def ButMainClearWatchesClick(self, sender, e):
        TTrace.watches.clearAll()

    # -----------------------------------------------------------------------------------------

    def ButMainDisplayWatchesClick(self, sender, e):
        TTrace.watches.displayWin()
        
    # -----------------------------------------------------------------------------------------

    def ButCreateNewWinWatchesClick(self, sender, e):
        self.winWatch = WinWatch(winWatchID = "MyWinWatchId" , winWatchText = "My WinWatch")        #IGNORE:W0201
        
        form._butWinWatchSend.Enabled = True
        form._butWinWatchClear.Enabled = True
        form._butWinWatchDisplay.Enabled = True
        form._butWinWatchClose.Enabled = True

    # -----------------------------------------------------------------------------------------

    def ButWinWatchSendClick(self, sender, e):
        self.winWatch.send("time", TTrace.Utility.currentTime())
        
    # -----------------------------------------------------------------------------------------

    def ButWinWatchClearClick(self, sender, e):
        self.winWatch.clearAll()

    # -----------------------------------------------------------------------------------------

    def ButWinWatchDisplayClick(self, sender, e):
        self.winWatch.displayWin()

    # -----------------------------------------------------------------------------------------

    def ButWinWatchCloseClick(self, sender, e):
        self.winWatch.close()
        
    # -----------------------------------------------------------------------------------------

    def ButStart1Click(self, sender, e):
        self.start1 = TTrace.debug.send("Start 1 ...")      
        form._butResendRight.Enabled = True
        form._butSetSelected.Enabled = True
        
    # -----------------------------------------------------------------------------------------

    def ButResendRightClick(self, sender, e):
        self.start1.resend(newRightMsg="Done1")

    # -----------------------------------------------------------------------------------------

    def ButSetSelectedClick(self, sender, e):
        self.start1.setSelected()

    # -----------------------------------------------------------------------------------------

    def ButStart2Click(self, sender, e):
        self.start2 = TTrace.debug.send("Start 2 ...")  #IGNORE:W0201
        form._butAppendLeft.Enabled = True
        form._butnodeShow.Enabled = True
        form._butToggleBookmark.Enabled = True
        form._butToggleVisible.Enabled = True

    # -----------------------------------------------------------------------------------------

    def ButAppendLeftClick(self, sender, e):
        self.start2.append(newLeftMsg="Done2")

    # -----------------------------------------------------------------------------------------

    def ButnodeShowClick(self, sender, e):
        self.start2.show()

    # -----------------------------------------------------------------------------------------

    def ButToggleBookmarkClick(self, sender, e):
        if not hasattr(self, 'lastToggleBookmark') :
            self.lastToggleBookmark = True
        self.start2.setBookmark(self.lastToggleBookmark)
        self.lastToggleBookmark = not self.lastToggleBookmark

    # -----------------------------------------------------------------------------------------

    def ButToggleVisibleClick(self, sender, e):
        if not hasattr(self, 'lastToggleVisible') :
            self.lastToggleVisible = False            
        self.start2.setVisible(self.lastToggleVisible)
        self.lastToggleVisible = not self.lastToggleVisible

    # -----------------------------------------------------------------------------------------

    def ButShowViewerClick(self, sender, e):
        TTrace.show()

    # -----------------------------------------------------------------------------------------

    def ButCloseViewerClick(self, sender, e):
        TTrace.closeViewer()

    # -----------------------------------------------------------------------------------------

    def ButClearMainClick(self, sender, e):
        TTrace.clearAll()
    
    # -----------------------------------------------------------------------------------------


demo = Demo()

from MainForm import *
form = MainForm(demo)

form._chkSendFunctions.Checked = ttrace.options.sendFunctions
form._chkSendClassInfo.Checked = ttrace.options.sendClassInfo
form._chkSendPrivate.Checked = ttrace.options.sendPrivate
form._chkSendDoc.Checked = ttrace.options.sendDoc
form._chkSendThtreadName.Checked = ttrace.options.sendThreadName
form._chkSendMessageUsingThread.Checked = ttrace.options.useThread
form._textBoxTreeDepth.Text = str(ttrace.options.objectTreeDepth)
form._textBoxSocketHost.Text = ttrace.options.socketHost
form._textBoxSocketPort.Text = str(ttrace.options.socketPort)



#form.demo = demo


Application.Run(form)
