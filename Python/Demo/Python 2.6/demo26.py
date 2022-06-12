
# for the majority of traces, you can import only "TTrace" or the "ttrace" synonym
from tracetool import *  #IGNORE:W0401
from tracetool import _Internals 

from threading import enumerate as threadEnum
from inspect import classify_class_attrs as SampleList

from tkinter import *
import platform

import sys
from sys import *

def sampleFunction(p5, p6=5):
    """ doc string for sample"""
    ret1 = p6
    ret2 = "hello"
    ret3 = ("%s" % (ret2),p5,ret1)   # tuple
    return ret3                      # return a tuple

#-----------------------------------------------------------------------------

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
        
#-----------------------------------------------------------------------------

class PageControl:
    def __init__(self, parent):
        #PageControls vars
        self.choice = IntVar()
        self.choice.set(0)
        self.count = 0
        self.activeFrame = None
        #create page control
        self.pageControl = Frame(parent)
        self.pageControl.pack(side=TOP)
        self.pageFrame = Frame(parent)
        self.pageFrame.pack(side=BOTTOM)
        self.parent = parent

    # hides the former active frame and shows 
    # another one, keeping its reference
    def display(self,frame):
        self.activeFrame.forget()
        frame.pack(fill=BOTH, expand=1)
        self.activeFrame = frame
    
    # add a new frame (screen) to the (bottom/left of the) notebook
    def add_screen(self,frame, title):
        radioButton = Radiobutton(self.pageControl, text=title, indicatoron=0, value=self.count, variable=self.choice, command=lambda: self.display(frame)) #
        radioButton.pack(fill=BOTH, side=LEFT)
        
        # ensures the first frame will be
        # the first selected/enabled
        if not self.activeFrame:
            frame.pack(fill=BOTH, expand=1)
            self.activeFrame = frame    
        self.count += 1        
        # returns a reference to the newly created (allowing its configuration/destruction)        
        return radioButton

#-----------------------------------------------------------------------------


class Demo():
    def __init__(self) :
        #create main window , the page control, and tab sheets
        self.tk = Tk()
        self.pageControl = PageControl(self.tk)
        
        self.sendFunctions = BooleanVar()
        self.sendClassInfo = BooleanVar()
        self.sendPrivate = BooleanVar()
        self.objectTreeDepth = IntVar()
        self.sendDoc= BooleanVar()
        self.sendThreadName= BooleanVar()
        self.socketHost= StringVar()
        self.socketPort= IntVar()
        self.useThread=BooleanVar()

        self.sendFunctions.set(TTrace.options.sendFunctions)
        self.sendClassInfo.set(TTrace.options.sendClassInfo)
        self.sendPrivate.set(TTrace.options.sendPrivate)
        self.objectTreeDepth.set(TTrace.options.objectTreeDepth)
        self.sendDoc.set(TTrace.options.sendDoc)
        self.sendThreadName.set(TTrace.options.sendThreadName)
        self.socketHost.set(TTrace.options.socketHost)
        self.socketPort.set(TTrace.options.socketPort) 
        self.useThread.set(TTrace.options.useThread)  
        
        # create tab sheet
        self.createBasicFrame()
        self.CreateMultiPages()    
        self.createWatchesFrame()    
        self.createNodeOperationFrame()
        
        self.createBottomFrame()
        self.tk.mainloop()
        
        
    def optionUpdate(self):
        TTrace.options.sendFunctions = self.sendFunctions.get()
        TTrace.options.sendClassInfo = self.sendClassInfo.get()
        TTrace.options.sendPrivate = self.sendPrivate.get()
        TTrace.options.objectTreeDepth= self.objectTreeDepth.get()
        TTrace.options.sendDoc= self.sendDoc.get()
        TTrace.options.sendThreadName= self.sendThreadName.get()
        TTrace.options.socketHost= self.socketHost.get()
        TTrace.options.socketPort= self.socketPort.get()
        TTrace.options.useThread= self.useThread.get()

    # user interface
    #-------------------------------------------------      
    def createBottomFrame (self):
        bottomFrame = LabelFrame(self.tk)
        bottomFrame.config(height=45,width=600, borderwidth=1)
        Button(bottomFrame, width = 20, text="Show viewer"       , command=self.showViewer)    .pack(side=LEFT)    
        Button(bottomFrame, width = 20, text="Close viewer"      , command=self.closeViewer)   .pack(side=LEFT)  
        Button(bottomFrame, width = 20, text="Clear main traces" , command=self.clearMainTrace).pack(side=LEFT)   
        bottomFrame.pack(side=BOTTOM ,fill = BOTH)

    def createBasicFrame(self):
        basicFrame = Frame(self.tk)
        basicFrame.config(height=300,width=600 )
        Button(basicFrame, width = 20, text="Sample traces"  , command=self.sampleTrace)   .place(x=10, y=10)        
        Button(basicFrame, width = 20, text="Indent/UnIndent", command=self.indentUnindent).place( x=10, y=50)
        Button(basicFrame, width = 20, text="sendObject"     , command=self.sendObject)    .place( x=10, y=90)
        Button(basicFrame, width = 20, text="sendValue"      , command=self.sendValue)     .place( x=10, y=130)
        Button(basicFrame, width = 20, text="sendTable"      , command=self.sendTable)     .place( x=10, y=170)
        Button(basicFrame, width = 20, text="stack"          , command=self.stackDemo)     .place( x=10, y=210)
        
        options = LabelFrame(basicFrame, text="Options", padx=5) 
        options.place(x=300, y=5)        
        Checkbutton(options, variable=self.sendFunctions, onvalue=True, offvalue=False, command=self.optionUpdate , text='sendFunctions' ).grid(sticky=W)
        Checkbutton(options, variable=self.sendClassInfo, onvalue=True, offvalue=False, command=self.optionUpdate , text='sendClassInfo').grid(sticky=W)
        Checkbutton(options, variable=self.sendPrivate, onvalue=True, offvalue=False, command=self.optionUpdate , text='sendPrivate').grid(sticky=W)
        Checkbutton(options, variable=self.sendDoc, onvalue=True, offvalue=False, command=self.optionUpdate , text='sendDoc').grid(sticky=W)
        Checkbutton(options, variable=self.sendThreadName, onvalue=True, offvalue=False, command=self.optionUpdate , text='sendThreadName').grid(sticky=W)
        Checkbutton(options, variable=self.useThread, onvalue=True, offvalue=False, command=self.optionUpdate , text='send message using a thread').grid(sticky=W)
        
        
        Label(options, text="objectTreeDepth").grid(sticky=W)
        Entry(options, textvariable=self.objectTreeDepth).grid(sticky=W)
        Label(options, text="socketHost").grid(sticky=W)
        Entry(options, textvariable=self.socketHost, width=30).grid(sticky=W)
        Label(options, text="socketPort" ).grid(sticky=W)
        Entry(options, textvariable=self.socketPort, width=30).grid(sticky=W)

        self.pageControl.add_screen(basicFrame, "Basic")
                
    def CreateMultiPages(self):
        multiPagesFrame = Frame(self.tk)
        multiPagesFrame.config(height=300,width=600 )

        Button(multiPagesFrame, width = 20, text="Create a new trace window"  , command=self.createNewTraceWindow).place(x=10, y=10)  
              
        self.butDisplayWindow = Button(multiPagesFrame, width = 20, text="Display that window"               , command=self.displayWindow)   #IGNORE:W0201    
        self.butDisplayWindow.place(x=10, y=40)     
        self.butDisplayWindow.config(state=DISABLED) 
           
        self.butSayHelloWindow = Button(multiPagesFrame, width = 20, text="Say Hello"                        , command=self.sayHelloWindow)  #IGNORE:W0201    
        self.butSayHelloWindow.place(x=10, y=70)      
        self.butSayHelloWindow.config(state=DISABLED) 
          
        self.butSetLogFile = Button(multiPagesFrame, width = 20, text="Set Log file"                         , command=self.setLogFile)   #IGNORE:W0201          
        self.butSetLogFile.place(x=10, y=100)    
        self.butSetLogFile.config(state=DISABLED) 
            
        self.butSaveToText = Button(multiPagesFrame, width = 20, text="Viewer:Save to text"                  , command=self.saveToText)   #IGNORE:W0201       
        self.butSaveToText.place(x=10, y=130)    
        self.butSaveToText.config(state=DISABLED) 
            
        self.butSaveToXml= Button(multiPagesFrame, width = 20, text="Viewer:Save to Xml"                     , command=self.saveToXml)  #IGNORE:W0201         
        self.butSaveToXml.place(x=10, y=160)  
        self.butSaveToXml.config(state=DISABLED) 
              
        self.butLoadXml = Button(multiPagesFrame, width = 20, text="Viewer:Load XML"                         , command=self.loadXml)   #IGNORE:W0201          
        self.butLoadXml.place(x=10, y=190)
        self.butLoadXml.config(state=DISABLED) 
                
        self.butClearWindow = Button(multiPagesFrame, width = 20, text="Clear windows"                       , command=self.clearWindow)   #IGNORE:W0201      
        self.butClearWindow.place(x=200, y=130) 
        self.butClearWindow.config(state=DISABLED) 
               
        self.butCloseWindow = Button(multiPagesFrame, width = 20, text="Close window"                        , command=self.closeWindow)  #IGNORE:W0201       
        self.butCloseWindow.place(x=200, y=160)  
        self.butCloseWindow.config(state=DISABLED) 
        
        Button(multiPagesFrame, width = 20, text="Multi column test"          , command=self.multiColTest)        .place(x=10,  y=250)        
        self.pageControl.add_screen(multiPagesFrame, "Multi pages")
    
    def createWatchesFrame(self):
        watchesFrame = Frame(self.tk)
        watchesFrame.config(height=300,width=600 )

        Label(watchesFrame, text="Main watches window" ).place( x=10, y=10)
        Button(watchesFrame, width = 20, text="Send watch"                    , command=self.sampleMainWatches)   .place( x=10, y=70)
        Button(watchesFrame, width = 20, text="Clear watches"                 , command=self.clearMainWatches)    .place( x=10, y=100)
        Button(watchesFrame, width = 20, text="Display window"                , command=self.displayMainWatches)  .place( x=10, y=130)

        Label(watchesFrame, text="New watches window" ).place( x=200, y=10)
        Button(watchesFrame, width = 20, text="Create new winwatch"           , command=self.createWinWatches).place( x=200, y=40)
        
        self.butSampleWinWatches = Button(watchesFrame, width = 20, text="Send watch"  , command=self.sampleWinWatches)    #IGNORE:W0201
        self.butSampleWinWatches.place( x=200, y=70)
        self.butSampleWinWatches.config(state=DISABLED) 
        
        self.butClearWinWatches = Button(watchesFrame, width = 20, text="Clear watches"  , command=self.clearWinWatches)     #IGNORE:W0201
        self.butClearWinWatches.place( x=200, y=100)
        self.butClearWinWatches.config(state=DISABLED) 
        
        self.butDisplayWinWatches = Button(watchesFrame, width = 20, text="Display window" , command=self.displayWinWatches)   #IGNORE:W0201
        self.butDisplayWinWatches.place( x=200, y=130)
        self.butDisplayWinWatches.config(state=DISABLED) 
        
        self.butCloseWinWatches = Button(watchesFrame, width = 20, text="Close window"     , command=self.closeWinWatches)  #IGNORE:W0201   
        self.butCloseWinWatches.place( x=200, y=160)
        self.butCloseWinWatches.config(state=DISABLED) 

        self.pageControl.add_screen(watchesFrame, "Watches")
        
    def createNodeOperationFrame(self):
        nodeOperationFrame = Frame(self.tk)
        nodeOperationFrame.config(height=300,width=600 )
        
        Button(nodeOperationFrame, width = 20, text="Start1", command=self.startNode1).place( x=10 , y=10)
        
        self.butResendRight    = Button(nodeOperationFrame, width = 20, text="resendRight"     , command=self.resendRight)  #IGNORE:W0201 
        self.butResendRight.place( x=100, y=40)
        self.butResendRight.config(state=DISABLED) 
            
        self.butSetSelected    = Button(nodeOperationFrame, width = 20, text="setSelected"     , command=self.setSelected)   #IGNORE:W0201
        self.butSetSelected.place( x=100, y=70)
        self.butSetSelected.config(state=DISABLED) 
       
        Button(nodeOperationFrame, width = 20, text="Start2", command=self.startNode2).place( x=10 , y=100)
        
        self.butAppendLeft     = Button(nodeOperationFrame, width = 20, text="appendLeft"      , command=self.appendLeft) #IGNORE:W0201   
        self.butAppendLeft.place( x=100, y=130)
        self.butAppendLeft.config(state=DISABLED) 
        
        self.butShowNode2      = Button(nodeOperationFrame, width = 20, text="show"            , command=self.showNode2)   #IGNORE:W0201  
        self.butShowNode2.place( x=100, y=160)
        self.butShowNode2.config(state=DISABLED) 
        
        self.butToggleBookmark = Button(nodeOperationFrame, width = 20, text="toggle bookmark" , command=self.toggleBookmark) #IGNORE:W0201
        self.butToggleBookmark.place( x=100, y=190)
        self.butToggleBookmark.config(state=DISABLED) 
        
        self.butToggleVisible  = Button(nodeOperationFrame, width = 20, text="toggle visible"  , command=self.toggleVisible) #IGNORE:W0201
        self.butToggleVisible.place( x=100, y=220)
        self.butToggleVisible.config(state=DISABLED) 
               
               
        
        self.pageControl.add_screen(nodeOperationFrame, "Node operations")
    
    # demo functions  
    
    # -----------------------------------------------------------------------------------------
    def showViewer(self):
        TTrace.show()
        
    # -----------------------------------------------------------------------------------------
        
    def closeViewer(self):
        TTrace.closeViewer()
        
    # -----------------------------------------------------------------------------------------

    def clearMainTrace(self):
        TTrace.clearAll()
    
    # -----------------------------------------------------------------------------------------
    def sampleTrace(self):
        TTrace.debug.send ("hello world")
        TTrace.debug.send ("hello","world")
        TTrace.debug.send (leftMsg="hello")
        TTrace.debug.send (rightMsg="world")
        TTrace.debug.send ("hello").send ("world")
        # single separator
        TTrace.debug.send("---")
        
        ttrace.debug.send ("python version : "  , version_info)
        ttrace.debug.send ("Platform.system(): ", platform )    # 'Windows' or 'cli'
        ttrace.debug.send ("sys.version : "     , sys.version)               # CPython

        # send traces with special font style (bold and Italic), color font size and font name
        # use System.Drawing.Color.ToArgb() (not supported in Silverlight) or (int)Helper.ToArgb(System.Windows.Media.Color) to specify Argb color
        node = TTrace.debug.send("Special font", "Symbol 12") 
        node.setFontDetail(-1, False, True)                                             # set whole line to italic 
        node.setFontDetail(3, True, False, TTrace.RgbColors["Red"])                     # set col 3 (Left Msg)  to bold and Red
        node.setFontDetail(4, False, False, TTrace.RgbColors["Green"], 12, "Symbol")    # set col 4 (Right Msg) to Green , font size 12 , Symbol
        node = TTrace.debug.send("Impact Italic")
        node.setFontDetail(3, False, True, TTrace.RgbColors["Fuchsia"], 12, "Impact")   # Col3 (left msg), non bold, Italic , Blue-Violet , font 12 , Impact
        # double separator
        TTrace.debug.send("===")

        # traces using TraceNodeEx
        #--------------------------------------------
        node = TraceNodeEx(TTrace.debug)
        node.leftMsg = "TraceNodeEx"
        node.rightMsg = str
        node.addFontDetail(3, False, False, TTrace.RgbColors["Green"])
        node.iconIndex = 8
        member = node.members.add("My Members", "col2", "col3")
        member.setFontDetail(0, True)                                 # set first column to bold
        member.setFontDetail(1, False, False, TTrace.RgbColors["Green"])   # set second column to green
        submember = member.add("Sub members")                                     # add sub member node
        submember.setFontDetail(0, False, True)                      # set first column to Italic
        node.addStackTrace()
        node.addCaller()
        sendNode = node.send()
        sendNode.resendIconIndex(5)  # change icon index after the node is send

        # XML sample using Send
        #--------------------------------------------
        TTrace.debug.sendXml("xml", "<?xml version='1.0' ?><Data> Hello XML </Data>")

        # Text, and XML together
        #--------------------------------------------
        node = TraceNodeEx(TTrace.debug)
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

        TTrace.debug.sendDump("doc de str", "".__doc__)   

        # ensure all traces are send to the viewer
        TTrace.flush()

        
    # -----------------------------------------------------------------------------------------

    def indentUnindent(self):
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
  
    def sendValue(self):
        testobj = TTrace.debug
        listObjToSend = SampleList(type(testobj)) 
        lst = [TTrace.winTrace,TMemberNode(),TTrace]
        
        TTrace.debug.sendValue("sendValue : classify_class_attrs ", listObjToSend, True, 3, "debug node")    
        TTrace.debug.sendValue("sendValue : list", lst, True, 3, "title")
        TTrace.debug.sendValue("integer 5", 5)
        #TTrace.debug.sendValue("long 5", 5L)   # 5L generate an invalid syntaxe in python 3.1
        
    
    # -----------------------------------------------------------------------------------------

    def sendTable(self):
        # array of object (same types)
        testobj = TTrace.debug # create an object and print some of it's attributes
        TTrace.debug.sendTable("sendTable : classify_class_attrs list",
                SampleList(type(testobj))
                )    
        
        # array of object of different types (no sense : not well displayed)
        TTrace.debug.sendTable("sendTable : list ", 
                [TTrace.winTrace,TMemberNode(),TTrace]
                )   
        
        # map         
        TTrace.debug.sendTable("map :_Internals.TraceConst" ,_Internals.TraceConst)
        
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
        
        # range() and enumerate()
        TTrace.debug.sendTable("range(10) ", range(10))
        TTrace.debug.sendTable("range(5,10) ", range(5,10))
        TTrace.debug.sendTable("enumerate(['a','b','c']) ", enumerate(['a','b','c']))
        
        TTrace.debug.sendTable("threads",  threadEnum())  # threading.enumerate()
              
    
    # -----------------------------------------------------------------------------------------

    def stackDemo(self):
        try:
            5 / 0
        except Exception: #IGNORE:W0703
            TTrace.debug.sendStack("exception")
    
    # -----------------------------------------------------------------------------------------

    def sendObject(self):
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
        TTrace.debug.sendObject("map :TTrace.TraceConst"         ,_Internals.TraceConst)

    # -----------------------------------------------------------------------------------------

    def createNewTraceWindow(self):
        self.winTrace = WinTrace(winTraceID = "MyWinTraceId" , winTraceTitle = "My WinTrace")       #IGNORE:W0201
        
        self.butDisplayWindow.config(state=NORMAL) 
        self.butSayHelloWindow.config(state=NORMAL) 
        self.butSetLogFile.config(state=NORMAL) 
        self.butSaveToText.config(state=NORMAL) 
        self.butSaveToXml.config(state=NORMAL) 
        self.butLoadXml.config(state=NORMAL) 
        self.butClearWindow.config(state=NORMAL) 
        self.butCloseWindow.config(state=NORMAL) 
    
    # -----------------------------------------------------------------------------------------
    
    def displayWindow(self):
        self.winTrace.displayWin()
    
    # -----------------------------------------------------------------------------------------
    
    def sayHelloWindow(self):
        self.winTrace.debug.send("Hello", "World") 
    
    # -----------------------------------------------------------------------------------------
    
    def setLogFile(self):
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
    
    def saveToText(self):
        self.winTrace.saveToTextfile("c:\\MyWinTrace.txt")
    
    # -----------------------------------------------------------------------------------------
    
    def saveToXml(self):
        self.winTrace.saveToXml("c:\\MyWinTrace.xml") 
    
    # -----------------------------------------------------------------------------------------
    
    def clearWindow(self):
        self.winTrace.clearAll()
    
    # -----------------------------------------------------------------------------------------
    
    def loadXml(self):
        self.winTrace.loadXml("c:\\MyWinTrace.xml")
    
    # -----------------------------------------------------------------------------------------
    
    def closeWindow(self):
        self.winTrace.close()
    
    # -----------------------------------------------------------------------------------------
    
    def multiColTest(self):
        if not hasattr(self, "MultiColTrace"):
            self.MultiColTrace = WinTrace("MCOL", "MultiCol trace window")          #IGNORE:W0201
            self.MultiColTrace.setMultiColumn(1) # must be called before calling setColumnsTitle
            self.MultiColTrace.setColumnsTitle("col1 \t col2 \t col3")
            self.MultiColTrace.setColumnsWidth("100:20:80 \t 200:50 \t 100")
            self.MultiColTrace.displayWin()
         
        self.MultiColTrace.debug.send("1 \t 2 \t 3")
    
   
    # -----------------------------------------------------------------------------------------
    
    def sampleMainWatches (self):
        TTrace.watches.send("testobj", TTrace.debug)

    # -----------------------------------------------------------------------------------------

    def clearMainWatches (self):
        TTrace.watches.clearAll()

    # -----------------------------------------------------------------------------------------

    def displayMainWatches (self):
        TTrace.watches.displayWin()

    # -----------------------------------------------------------------------------------------

    def createWinWatches (self):
        self.winWatch = WinWatch(winWatchID = "MyWinWatchId" , winWatchText = "My WinWatch")        #IGNORE:W0201
        self.butSampleWinWatches.config(state=NORMAL) 
        self.butClearWinWatches.config(state=NORMAL) 
        self.butDisplayWinWatches.config(state=NORMAL) 
        self.butCloseWinWatches.config(state=NORMAL) 

    # -----------------------------------------------------------------------------------------

    def sampleWinWatches (self):
        self.winWatch.send("time", TTrace.Utility.currentTime())

    # -----------------------------------------------------------------------------------------

    def clearWinWatches (self):
        self.winWatch.clearAll()

    # -----------------------------------------------------------------------------------------

    def displayWinWatches (self):
        self.winWatch.displayWin()

    # -----------------------------------------------------------------------------------------

    def closeWinWatches (self):
        self.winWatch.close()
    
    # -----------------------------------------------------------------------------------------

    def startNode1(self):
        self.start1 = TTrace.debug.send("Start 1 ...")      #IGNORE:W0201
        self.butResendRight.config(state=NORMAL) 
        self.butSetSelected.config(state=NORMAL) 
    
    # -----------------------------------------------------------------------------------------

    def resendRight(self):
        self.start1.resend(newRightMsg="Done1")
    
    # -----------------------------------------------------------------------------------------

    def setSelected(self):
        self.start1.setSelected()
    
    # -----------------------------------------------------------------------------------------

    def startNode2(self):
        self.start2 = TTrace.debug.send("Start 2 ...")  #IGNORE:W0201
        self.lastToggleBookmark = True      #IGNORE:W0201
        self.lastToggleVisible = False      #IGNORE:W0201
        self.butAppendLeft.config(state=NORMAL) 
        self.butShowNode2.config(state=NORMAL) 
        self.butToggleBookmark.config(state=NORMAL) 
        self.butToggleVisible.config(state=NORMAL) 
    
    # -----------------------------------------------------------------------------------------

    def appendLeft(self):
        self.start2.append(newLeftMsg="Done2")
    
    # -----------------------------------------------------------------------------------------

    def showNode2(self):
        self.start2.show()
    
    # -----------------------------------------------------------------------------------------

    def toggleBookmark(self):
        self.start2.setBookmark(self.lastToggleBookmark)
        self.lastToggleBookmark = not self.lastToggleBookmark
    
    # -----------------------------------------------------------------------------------------

    def toggleVisible(self):
        self.start2.setVisible(self.lastToggleVisible)
        self.lastToggleVisible = not self.lastToggleVisible
    

# MAIN
#====================        
Demo()


