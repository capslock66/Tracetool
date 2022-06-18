# -*- coding: cp1252 -*-
"""
tracetool API for python

@version: 12.5.0
@contact: http://www.codeproject.com/KB/trace/tracetool.aspx
@Author: Thierry Parent
@copyright: Copyright (C) 2010 Thierry Parent
@license: see License.txt for license information

Sample Use:

  >>> from tracetool import ttrace
  >>> ttrace.debug.send ("hello world")

"""

# class exported
__all__ = ["TraceNodeBase","FontDetail","TMemberNode","TraceToSend","TraceToSend","TraceNode","TraceNodeEx","WinWatch","WinTrace", "TTraceOptions","TTrace","ttrace"]

from socket import socket, AF_INET, SOCK_STREAM  #, error
import threading
from datetime import datetime
import random
from collections import namedtuple
import types
import inspect
from sys import version_info
import platform

try:
    import queue as Queue         #IGNORE:F0401   # python 3
except Exception as importError:  #IGNORE:W0703
    import Queue                                  # python 2

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TraceNodeBase(object):
    """ Base class for TraceSend and TraceNodeEx  """

    def __init__(self):
        """ Internal abstract constructor.Don't create direct instance.See TraceToSend and TraceNodeEx"""

        self.id = ""
        """ The unique ID. Normally it's a GUID/UUID, but can be replaced by something else for inter process traces."""

        self.enabled = True
        """When enabled is false, all traces are disabled. Default is true.
        All node have a enabled property, that lets you define group of enabled trace.
        For example set the TTrace.Debug.enabled to false but
        continue to accept Error and Warning traces
        """

        self.winTraceId = ""
        """ The parent win tree Id"""

        self.tag = None
        """ User variable, provided for the convenience of developers"""

        self.iconIndex = 0
        """ The index of the icon to use. You can then show an icon for Warning traces different for Error traces"""

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class _NodeContext(object):
    """ internal class used to remind indentation node id"""
    def __init__(self):
        """ NodeContext constructor. You don't have to create instance """
        self.threadId = ""
        self.nodeId = ""
        self.threadName = ""

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class FontDetail(object):
    """ Font detail for traces and members """
    def __init__(self):
        """ Construct a default FontDetail object"""

        self.colId = 0
        """column id on where to apply special font """

        self.bold = False
        """use bold """

        self.italic = False
        """use italic """

        self.color = -1
        """color code to use """

        self.size = 0
        """font size """

        self.fontName = ""
        """font name """

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TMemberNode(object):
    """ TMemberNode represent a sub node information in the "info" trace tree. See TraceNodeEx
    See TraceNode class for sample use"""
    def __init__(self,col1="", col2="", col3=""):
        """ Create a TMemberNode with text for the 3 columns
        @param col1 : text of col1
        @param col2 : text of col2
        @param col3 : text of col3
        """

        self.col1 = col1
        """Member column 1 text """

        self.col2 = col2
        """Member column 2 text """

        self.col3 = col3
        """Member column 3 text """

        self.members = []
        """sub members """

        self.tag = None
        """user defined object (not send to the viewer) """

        self.viewerKind = 0
        """viewer kind.
            - 0 : (CST_VIEWER_NONE)  : viewer kind : default viewer, no icon
            - 1 : (CST_VIEWER_DUMP)  : dump viewer
            - 2 : (CST_VIEWER_XML)   : xml viewer
            - 3 : (CST_VIEWER_TABLE) : table viewer
            - 4 : (CST_VIEWER_STACK) : stack
            - 5 : (CST_VIEWER_BITMAP): bitmap viewer
            - 6 : (CST_VIEWER_OBJECT): object structure
            - 7 : (CST_VIEWER_VALUE) : object value
            - 8 : (CST_VIEWER_ENTER) : enter method
            - 9 : (CST_VIEWER_EXIT)  : exit method
            - 10 : (CST_VIEWER_TXT)  : text added to default viewer
        """

        self._fontDetails = []
        """Array of font details to apply to the member """

        self.header = []   # for SendTable/AddTable : save columns in temp array

    #-----------------------------------------------------------------------------
    def add (self, col1OrMember, col2="", col3=""):
        """ Add a sub member giving the 3 columns or TMemberNode instance
        @param col1OrMember : TMemberNode instance or col1 text
        @param col2 : col2 text
        @param col3 : col3 text
        @returns: The TMember node
        """
        if isinstance(col1OrMember, TMemberNode):
            self.members.append(col1OrMember)
            return col1OrMember
        else:
            member = TMemberNode(col1OrMember, col2, col3)
            self.members.append(member)
            return member

    #-----------------------------------------------------------------------------
    def setFontDetail(self,colId, bold = False, italic = False, color = -1, size = 0, fontName = ""):
        """ Change font detail for an item in the trace
        @param colId : Column index : All columns=-1, Col1=0, Col2=1, Col3=2
        @param bold : Change font to bold
        @param italic : Change font to Italic
        @param color : Change Color : -1 for the default color or tuple (R,G,B)
        @param size : Change font size, use zero to keep normal size
        @param fontName : Change font name
        @returns: The TMember node
        """

        fontDetail = FontDetail()
        fontDetail.colId    = colId
        fontDetail.bold     = bold
        fontDetail.italic   = italic
        fontDetail.color    = color
        fontDetail.size     = size
        fontDetail.fontName = fontName

        self._fontDetails.append(fontDetail)
        return self

    #-----------------------------------------------------------------------------
    def addToStringList(self,commandList):
        """Internal use : recursively add members to the node commandList
        @param commandList : Where to store members
        """

        # the root node node itself is not send for now.
        # Later we can send the 3 columns text to specify the header, if specfied.
        # the text should be like that : "Myfirstcol:150" where 150 is the column with sub nodes, if any
        for node in self.members:
            node._internalAddToStringList(commandList) #IGNORE:W0212

        # once copied to Commandlist, clear the array
        self.members = []

    #-----------------------------------------------------------------------------

    def _internalAddToStringList(self,commandList):
        """
        internal use. Recursively add sub members to the array
        @param commandList : Where to store members
        """
        # create the member and set col1
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_CREATE_MEMBER"], self.col1))

        # add command if col2 and/or col3 exist
        if self.col2 != "":
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_MEMBER_COL2"], self.col2))

        if self.col3 != "":
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_MEMBER_COL3"], self.col3))

        # add viewer kind
        if self.viewerKind != 0:
            commandList.append("%5d%d" % (_Internals.TraceConst["CST_MEMBER_VIEWER_KIND"], self.viewerKind))

        # add font detail
        for fontDetail in self._fontDetails:

            tempStr = "%5d%3d" % (_Internals.TraceConst["CST_MEMBER_FONT_DETAIL"],fontDetail.colId )

            if fontDetail.bold:
                tempStr = tempStr + "1"
            else:
                tempStr = tempStr + "0"

            if fontDetail.italic:
                tempStr = tempStr + "1"
            else:
                tempStr = tempStr + "0"

            tempStr = tempStr + "%11d%11d%s" % (TTrace.Utility.rgbToBgr(fontDetail.color), fontDetail.size,fontDetail.fontName)
            commandList.append(tempStr)

        # recursive add sub nodes, if any
        for node in self.members:
            node._internalAddToStringList(commandList)   #IGNORE:W0212

        # close the member group
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_ADD_MEMBER"], ""))

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TraceToSend(TraceNodeBase):
    """ Base class for TraceNode and WinTrace.
    Methods of TraceSend can send new traces to the viewer, but cannot be re-send
    The TTrace.debug object ,for example, is a TraceSend instance """

    #-----------------------------------------------------------------------------
    def __init__(self):
        """ TraceToSend base constructor.You don't have to create TraceToSend instance."""
        # TraceNodeBase fields : id ,  enabled , winTraceId, tag, iconIndex
        TraceNodeBase.__init__(self)  # base class constructor
        # TraceToSend new fields : _contextList, _winTraceContext, _lock
        self._contextList = []
        self._winTraceContext = []
        self._lock = threading.Lock()

    #-----------------------------------------------------------------------------
    def getLastContext(self):
        """ Get the last context.
        @returns: last context for the thread
        """

        if self._winTraceContext != None:
            cList = self._winTraceContext
        elif self._contextList != None:
            cList = self._contextList
        else:
            return None

        # no need to lock an empty list
        if cList.count == 0:
            return None

        thName = threading.current_thread().name

        self._lock.acquire()
        try:
            for aContext in cList:
                if aContext.threadName == thName:
                    return aContext
        finally:
            self._lock.release()
        return None

    #-----------------------------------------------------------------------------
    def getLastContextId(self):
        """ Get the last context ID.
        @returns: last context ID for the thread
        """

        aContext = self.getLastContext()
        if (aContext is None):
            return self.id
        return aContext.nodeId

    #-----------------------------------------------------------------------------
    def pushContext(self,newContext):
        """ Save the context
        @param newContext : the context to push
        """

        if self._winTraceContext != None:
            cList = self._winTraceContext
        elif self._contextList != None:
            cList = self._contextList
        else:
            self._contextList = []
            cList = self._contextList

        self._lock.acquire()
        try:
            cList.insert(0,newContext)
        finally:
            self._lock.release()

    #-----------------------------------------------------------------------------
    def deleteLastContext(self):
        """ Delete the last context for the thread """
        if self._winTraceContext != None:
            cList = self._winTraceContext
        elif self._contextList != None:
            cList = self._contextList
        else:
            return

        thName = threading.current_thread().name

        self._lock.acquire()
        try:
            for aContext in cList:
                if aContext.threadName == thName:
                    cList.remove(aContext)
                return
        finally:
            self._lock.release()

    #-----------------------------------------------------------------------------
    def getIndentLevel(self):
        """ Return current indent level. See Indent()
        @returns: current indent level
        """

        if self._winTraceContext != None:
            cList = self._winTraceContext
        elif self._contextList != None:
            cList = self._contextList
        else:
            return 0

        thName = threading.current_thread().name
        result = 0

        self._lock.acquire()
        try:
            for aContext in cList:
                if aContext.threadName == thName:
                    result = result + 1
        finally:
            self._lock.release()
        return result

    #-----------------------------------------------------------------------------
    def indent(self, leftMsg, rightMsg="", backGroundColor = -1, isEnter = False):
        """ Send a message. further trace to the same node are indented under this one.
        @param rightMsg : Right Message to send (optional)
        @param backGroundColor : BackGround Color. -1 for the default color or tuple (R,G,B)
        @param isEnter : if true , a special "enter" icon is added on the node
        @returns: The leftMsg : Left message to send
        """
        if self.enabled == False:
            return TraceNode(self)

        thName = threading.current_thread().name

        newContext = _NodeContext()
        newContext.threadName = thName

        lastContext = self.getLastContext()
        newContext.nodeId = TTrace.Utility.uuid()

        commandList = []

        if lastContext is None :
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_NEW_NODE"] , self.id)) # param : parent Node id
        else:
            # context already exist
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_NEW_NODE"] , lastContext.nodeId)) # param : parent Node id

        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TRACE_ID"] ,newContext.nodeId ))  # param : Node Id
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_LEFT_MSG"] ,leftMsg ))   # param : left string

        if rightMsg != "":
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_RIGHT_MSG"] ,rightMsg )) # param : right string

        if (backGroundColor is not None) and (backGroundColor != -1):
            commandList.append("%5d%11d%s" % (_Internals.TraceConst["CST_BACKGROUND_COLOR"] ,TTrace.Utility.rgbToBgr( backGroundColor), "-1")) #param : color, colId

        if isEnter:
            member = TMemberNode() # create root member
            member.add("").viewerKind =_Internals.TraceConst["CST_VIEWER_ENTER"] # then add an empty member with special viewer #
            member.addToStringList(commandList) # convert all groups and nested items/group to strings

        commandList.append("%5d%s" % (_Internals.TraceConst["CST_ICO_INDEX"] , self.iconIndex))   # param : icon index
        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)

        self.pushContext(newContext)

    #-----------------------------------------------------------------------------
    def unIndent(self,leftMsg="", rightMsg="", backGroundColor=-1, isExit=False):
        """ Delete indentation to the node added by indent()
        @param leftMsg : Left message to send to close indentation (optional)
        @param rightMsg : Right message to send to close indentation (optional)
        @param backGroundColor : background color (optional) -1 for the default color or tuple (R,G,B)
        @param isExit : if true, viewer type 'exit' is used (optional)
        """
        if self.enabled == False:
            return TraceNode(self)

        self.deleteLastContext()

        if leftMsg != "" or rightMsg != "":
            nodeId = TTrace.Utility.uuid()

            commandList = self.prepareNewNode(leftMsg, nodeId)

            if rightMsg is not None:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_RIGHT_MSG"] , rightMsg))  # param : right string

            if (backGroundColor is not None) and (backGroundColor != -1):
                commandList.append("%5d%11d%s" % (_Internals.TraceConst["CST_BACKGROUND_COLOR"] ,TTrace.Utility.rgbToBgr(backGroundColor), "-1")) #param : color, colId

            if isExit:
                member = TMemberNode() # create root member
                member.add("").viewerKind =_Internals.TraceConst["CST_VIEWER_EXIT"] # then add an empty member with special viewer #
                member.addToStringList(commandList) # convert all groups and nested items/group to strings

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)

    #-----------------------------------------------------------------------------
    def enterMethod(self, leftMsg="", rightMsg="", backGroundColor=None):
        """ Indent with "Enter " + left message + right message (optional) + background color (optional)
        @param leftMsg : Left message to send
        @param rightMsg : Right message to send
        @param backGroundColor : BackGround Color. -1 for the default color or tuple (R,G,B)
        """
        self.indent("Enter " + leftMsg, rightMsg, backGroundColor, True)

    #-----------------------------------------------------------------------------
    def exitMethod(self, leftMsg="", rightMsg="", backGroundColor=None):
        """ UnIndent with "Exit " + left message (optional) + right message (optional) + background color (optional)
        @param leftMsg : Left message to send
        @param rightMsg : Right message to send
        @param backGroundColor : BackGround Color. -1 for the default color or tuple (R,G,B)
        """
        self.unIndent("Exit " + leftMsg, rightMsg, backGroundColor, True)

    #-----------------------------------------------------------------------------
    def prepareNewNode(self,leftMsg, newId=None):
        """ Prepare the minimal command List with leftmsg, trace id, ...
        @param leftMsg : The left message
        @param newId : The trace node ID
        @returns: A command list
        """
        if newId is None:
            newId=TTrace.Utility.uuid()
        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_NEW_NODE"] , self.getLastContextId())) # param : parent Node id
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TRACE_ID"] , newId))                   # param : Node id
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_LEFT_MSG"] , leftMsg))                 # param :left string
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_ICO_INDEX"], self.iconIndex))          # param :Icon index
        return commandList

    #-----------------------------------------------------------------------------
    def send(self, leftMsg="", rightMsg=""):
        """ The most useful trace function : send just a string
        @param leftMsg : The message to display
        @param rightMsg : The right message
        @returns: A Trace node. Useful to add sub traces
        """
        if self.enabled == False:
            return TraceNode(self)

        try:
            result = TraceNodeEx(self, True)
            commandList = self.prepareNewNode(leftMsg, result.id)
            if rightMsg != "":
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_RIGHT_MSG"], rightMsg))         # param :right string
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException:
            return TraceNode(self)
        return TraceNode(result)

    #-----------------------------------------------------------------------------

    def sendValue(self,leftMsg, objToSend, sendPrivate = False, maxLevel = 3, title = None):
        """ Send Private and public values of an object.
        sendValue is quite different from sendObject : less verbal (no class info) but can show many level.
        Take care that sendValue, sendObject , addValue and addObject functions are time consuming. Many informations are collected
        @param leftMsg : The message text
        @param objToSend : the object to examine
        @param sendPrivate : flag to send private field
        @param maxLevel : The number of sub element to display. Default is 3
        @param title : object title
        @returns: A trace node
        """

        if self.enabled == False:
            return TraceNode(self)

        result = None
        try:

            if title is None:
                title = type(objToSend)

            result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
            commandList = self.prepareNewNode(leftMsg, result.id)

            # informations are added to the members array of the new created object.
            result.addValue(objToSend, sendPrivate, maxLevel, title)

            # convert all groups and nested items/group to strings
            result.members.addToStringList(commandList)

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return TraceNode(result)

    # ----------------------------------------------------------------------
    def sendObject(self,leftMsg, objToSend,bypassFields=None):
        """ Send a trace and an object (class info, fields, method)
        Take care that sendValue, sendObject , addValue and addObject functions are time consuming. Many informations are collected
        @param leftMsg : The left trace message to send
        @param objToSend : The object to inspect. To specify what to print, see the TraceOption flags
        @returns: A trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        if bypassFields==None:
            bypassFields=[]
        result = None
        try:

            result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
            commandList = self.prepareNewNode(leftMsg, result.id)

            # informations are added to the members array of the new created object.
            # This current instance can be the public 'Warning' node for example used by multithread applcation
            result.addObject(objToSend,bypassFields)

            # convert all groups and nested items/group to strings
            result.members.addToStringList(commandList)

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

        return TraceNode(result)

    # ----------------------------------------------------------------------
    def sendStack(self,leftMsg):
        """ Send the call stack
        @param leftMsg : Trace message
        @returns: a Trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        result = None
        try:
            result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
            commandList = self.prepareNewNode(leftMsg, result.id)
            result.addStackTrace()
            result.members.addToStringList(commandList)

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

        return TraceNode(result)

    # ----------------------------------------------------------------------

    def sendCaller(self,leftMsg):
        """ Send the caller function name
        @param leftMsg : Trace message
        @returns: a Trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        result = None
        try:
            result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
            commandList = self.prepareNewNode(leftMsg, result.id)
            result.addCaller()
            result.members.addToStringList(commandList)

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return TraceNode(result)

    # ----------------------------------------------------------------------

    def sendDump(self,leftMsg,  bytesBuffer, shortTitle= "Dump", count=0):
        """ Send dump
        @param leftMsg : Trace message
        @param shortTitle : A short title displayed on top of the dump
        @param bytesBuffer : The byte buffer to dump :
        string (single byte char or unicode) , bytearray or any array like object that has char or int elements
        @param count : Number of byte to dump
        @returns: a Trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        result = None
        try:
            result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
            commandList = self.prepareNewNode(leftMsg, result.id)

            result.addDump(bytesBuffer, shortTitle, 0, count)
            result.members.addToStringList(commandList)

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return TraceNode(result)

    # ----------------------------------------------------------------------

    def sendBackgroundColor(self,leftMsg, color = -1, colId=None):
        """  Send trace with a specific background color
        @param leftMsg : Trace message
        @param color : background color. -1 for the default color or a tuple (R,G,B)
        @param colId : Column index : All columns=-1, Col1=0, Col2=1, Col3=2
        @returns: a Trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
        commandList = self.prepareNewNode(leftMsg, result.id)
        if colId is None:
            commandList.append("%5d%11d%s" % (_Internals.TraceConst["CST_BACKGROUND_COLOR"] , TTrace.Utility.rgbToBgr(color), "-1")) #param : color, colId
        else:
            commandList.append("%5d%11d%5d" % (_Internals.TraceConst["CST_BACKGROUND_COLOR"] , TTrace.Utility.rgbToBgr(color), colId)) #param : color, colId

        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return TraceNode(result)

    # ----------------------------------------------------------------------

    def sendXml(self,leftMsg, xml):
        """ Send xml text
        @param leftMsg : Trace message
        @param xml : xml text to send
        @returns: a Trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
        commandList = self.prepareNewNode(leftMsg, result.id)

        result.addXML(xml)
        result.members.addToStringList(commandList)

        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return TraceNode(result)

    # ----------------------------------------------------------------------

    def sendTable(self,leftMsg,table):
        """ Add table to node
        @param leftMsg : Trace message
        @param table : TraceTable or Object collection(Array / Collection / Map) to send
        @returns: a Trace node
        """
        if self.enabled == False:
            return TraceNode(self)

        result = TraceNodeEx(self, True) # create a node with same properties as "this" with new ID
        commandList = self.prepareNewNode(leftMsg, result.id)

        result.addTable(table)
        result.members.addToStringList(commandList)

        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return TraceNode(result)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TraceNode(TraceToSend):
    """ TraceNode represent node on the viewer.
    You don't have to create an instance of that class.
    tracetool create instance when you send traces.
    """
    def __init__(self,source=None,generateUniqueId=False):
        """ Internal TraceNode constructor. Use TTrace or TraceNodeEx class as entry point
        @param source : In case of "Copy constructor" : source to duplicate, else None
        @param generateUniqueId : If True, generated an unique id for the trace. If False an "Copy constructor" : same as the source
        """
        # TraceNodeBase fields : id ,  enabled , winTraceId, tag, iconIndex
        # TraceToSend fields : _contextList ,  _winTraceContext , _lock
        TraceToSend.__init__(self)  # base class constructor
        if source is None:
            self.iconIndex    =_Internals.TraceConst["CST_ICO_DEFAULT"]
            self.enabled      = True
            self.winTraceId   = None
            self.parentNodeId = ""
            self.id           = ""
        else:
            self.iconIndex    = source.iconIndex
            self.enabled      = source.enabled
            self.winTraceId   = source.winTraceId
            if isinstance(source, TraceToSend):
                self.parentNodeId = source.getLastContextId()  #only if source is
            self.id           = source.id

        if generateUniqueId:
            self.id = TTrace.Utility.uuid()

        # TraceNode don't introduce new fields

    # ----------------------------------------------------------------------
    def resend(self,newLeftMsg=None, newRightMsg=None):
        """ Override a previous send message (both column)
        @param newLeftMsg : The new Left message
        @param newRightMsg : The new Right message
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)


        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
            if newLeftMsg is not None:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_LEFT_MSG"] , newLeftMsg)) # param : new left string
            if newRightMsg is not None:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_RIGHT_MSG"] , newRightMsg)) # param : new right string

            # don't resend members and icon
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

        return self

    # ----------------------------------------------------------------------
    def resendIconIndex(self,index):
        """ Change the Icon index
        @param index : Index of the icon to use
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_ICO_INDEX"] ,index ))  # param : icon index
        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return self

    # ----------------------------------------------------------------------
    def setBackgroundColor(self,color=-1, colId=-1) :
        """ Change Background Color (whole line) of a node
        @param color : new background color of the node. -1 for the default color or tuple (R,G,B)
        @param colId : Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
        commandList.append("%5d%11d%3d" % (_Internals.TraceConst["CST_BACKGROUND_COLOR"] , TTrace.Utility.rgbToBgr(color), colId)) # param : color, colId
        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return self

    # ----------------------------------------------------------------------
    def setFontDetail(self,colId=-1, bold=False, italic=False, color=None, size=0, fontName=""):
        """ Change font detail for an item in the trace
        @param colId : Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
        @param bold : Change font to bold
        @param italic : Change font to Italic
        @param color : Change Color.-1 for the default color or a tuple (R,G,B)
        @param size : Change font size, use zero to keep normal size
        @param fontName : Change font name
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id

        tempStr = "%5d%3d" % (_Internals.TraceConst["CST_FONT_DETAIL"] ,colId)

        if bold:
            tempStr += "1"
        else:
            tempStr += "0"

        if italic:
            tempStr += "1"
        else:
            tempStr += "0"

        tempStr += "%11d%11d%s" % (TTrace.Utility.rgbToBgr(color),size,fontName)

        commandList.append(tempStr) #don't use commandList.append
        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return self

    # ----------------------------------------------------------------------
    def append(self,newLeftMsg=None, newRightMsg=None):
        """ Append text to a previous send message (both column)
        @param newLeftMsg : The new Left message to append
        @param newRightMsg : The new Right message to append
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
            if newLeftMsg is not None:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_APPEND_LEFT_MSG"] , newLeftMsg)) # new left string
            if newRightMsg is not None:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_APPEND_RIGHT_MSG"] , newRightMsg)) # new right string

            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def setSelected(self):
        """ Set a node as selected in the viewer. Don't confuse with show() that force a node to be displayed
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_SELECT_NODE"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def show(self):
        """ Force a node to be displayed. don't confuse with setSelected()
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_FOCUS_NODE"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def delete(self):
        """ Delete the node
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_CLEAR_NODE"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def deleteChildren(self):
        """ Delete children node
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_CLEAR_SUBNODES"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def setBookmark(self, bookmarked = True):
        """ Set or reset the bookmark for the node
        @param bookmarked : True/False
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
            if bookmarked:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_SET_BOOKMARK"] ,1 ))
            else:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_SET_BOOKMARK"] ,0 ))
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def setVisible(self,visible = True):
        """ set a node visible or invisible
        @param visible : True/False
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)
        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
            if visible:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_VISIBLE_NODE"] , 1)) # param : visible flag
            else:
                commandList.append("%5d%s" % (_Internals.TraceConst["CST_VISIBLE_NODE"] , 0)) # param : visible flag
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def gotoNextSibling(self):
        """ Set focus to next sibling
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)
        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_GOTO_NEXTSIBLING"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def gotoPrevSibling(self):
        """ Set focus to previous sibling
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)

        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_GOTO_PREVSIBLING"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def gotoFirstChild(self):
        """ Set focus to first child
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)
        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_GOTO_FIRST_CHILD"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self

    # ----------------------------------------------------------------------
    def gotoLastChild(self):
        """ Set focus to last child
        @returns: The trace node
        """
        if self.enabled == False or self.id is None:
            return TraceNode(self)
        try:
            commandList = []
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_GOTO_LAST_CHILD"] , self.id)) # param : Node id
            TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)
        return self


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TraceNodeEx(TraceNodeBase):
    """ Alternate way to send traces : prepare a TraceNodeEx with all properties then send it.

    Sample code :

    >>> node = TraceNodeEx(TTrace.debug)                                   # create a node
    >>> node.leftMsg = "Hello"                                             # set left message
    >>> node.rightMsg = "World"                                            # set right message
    >>> node.addFontDetail(3, False, False, TTrace.RgbColors["Green"])     # change font color for a specific column
    >>> node.iconIndex = 8                                                 # set icon
    >>> member = node.members.add("My Members", "col2", "col3")            # add member
    >>> member.setFontDetail(0, True)                                      # set first column to bold
    >>> member.setFontDetail(1, False, False, TTrace.RgbColors["Green"])   # set second column to green
    >>> submember = member.add("Sub members")                              # add sub member node
    >>> submember.setFontDetail(0, False, True)                            # set first column to Italic
    >>> node.addStackTrace()                                               # add stack info
    >>> sendNode = node.send()                                             # Finally send the node

    """
    def __init__(self,source=None,generateUniqueId=False):
        """ TraceNodeEx constructor : create a Node with an unique ID
        @param source : In case of "Copy constructor" : source to duplicate, else None
        @param generateUniqueId : If True, generated an unique id for the trace. If False an "Copy constructor" : same as the source
        """
        # TraceNodeBase fields : id, iconIndex, enabled, winTraceId, parentNodeId
        TraceNodeBase.__init__(self)  # base class constructor

        if source is None:
            self.iconIndex    =_Internals.TraceConst["CST_ICO_DEFAULT"]
            self.enabled      = True
            self.winTraceId   = None
            self.parentNodeId = ""
            self.id           = ""
        else:
            self.iconIndex    = source.iconIndex
            self.enabled      = source.enabled
            self.winTraceId   = source.winTraceId
            self.parentNodeId = source.getLastContextId()
            self.id           = source.id

        if generateUniqueId:
            self.id = TTrace.Utility.uuid()

        # TraceNodeEx new fields
        self.parentNodeId = ""
        """ Parent Node Id """
        self.leftMsg = ""
        """ Left message """
        self.rightMsg = ""
        """ right message (comment) """
        self.time = ""
        """ time """
        self.threadName = ""
        """ thread name """
        self.members = TMemberNode()
        """ the root for the Member tree """
        self._fontDetails = []

    # ----------------------------------------------------------------------
    def addObject(self,objToSend, bypassFields = None):
        """ Call addObject to fill the "member" tree with the object value.
        To specify what to print, see the TraceOption flags
        Take care that sendValue, sendObject , addValue and addObject functions are time consuming. Many informations are collected
        @param objToSend : the object to send.
        @param bypassFields : Optional array of fieldName to don't print
        """
        try:
            if self.enabled == False :
                return

            if bypassFields == None:
                bypassFields = []

            if (objToSend is None):
                objClass = None
            else:
                objClass = type(objToSend)

            if objClass is None:         # should not happens
                self.members.add("No type")
                return

            # Display class information
            # ------------------------------------

            if TTrace.options.sendClassInfo == True:
                infoGroup = TMemberNode("Type information").setFontDetail(0, True)
                self.members.add(infoGroup)

                info = TTrace.Utility.getObjectInfo(objToSend)
                if info.name == "":              # same as properties
                    result = TMemberNode("(no name)" , info.returnType + info.memberValue , info.memberType)
                elif info.isFunction:
                    result = TMemberNode("Function name:'" + info.memberValue + "'" ,info.memberType )
                else:
                    result = TMemberNode("Object name:'" + info.name + "'",
                                         info.returnType + info.memberValue , info.memberType)
                result.viewerKind =_Internals.TraceConst["CST_VIEWER_OBJECT"]
                infoGroup.add(result)

                # add object documentation
                # ----------------------------
                if TTrace.options.sendDoc == True and info.doc != "":
                    infoGroup.add('Doc', info.doc)

                # indicate if the object is callable
                # ----------------------------
                result = TMemberNode('callable')
                if hasattr(objToSend, '__call__') :
                    result.col2 = "True"
                else:
                    result.col2 = "False"
                infoGroup.add(result)

                # object ID
                # ----------------------------
                infoGroup.add('object id', id(objToSend))

                # Super classes and interfaces. Note : multiple inheriting
                # -----------------------------
                if TTrace.options.sendInherited == True:
                    ancestorsGroup = TMemberNode("Ancestor").setFontDetail(0, True)
                    ancestorsGroup.viewerKind =_Internals.TraceConst["CST_VIEWER_OBJECT"]

                    ancestors = list(objClass.__mro__)
                    for ancestor in ancestors:
                        # don't display itself and the base class common to all objects : object
                        if ancestor != objClass and ancestor != object:
                            ancestorsGroup.add(str(ancestor))

                    # show the group only if not empty
                    if len(ancestorsGroup.members) != 0:
                        self.members.add(ancestorsGroup)

            # Display fields and methods
            #-------------------------------

            if TTrace.options.sendFunctions == True or TTrace.options.sendFields == True :
                fieldGroup = TMemberNode("Fields").setFontDetail(0, True)
                fieldGroup.viewerKind =_Internals.TraceConst["CST_VIEWER_OBJECT"]

                methodGroup = TMemberNode("Methods").setFontDetail(0, True)
                methodGroup.viewerKind =_Internals.TraceConst["CST_VIEWER_OBJECT"]

                # looping __dict__ is not necessary, since all theses informations are in dir()
                items = dir(objToSend)
                for fieldName in items:
                    if fieldName in bypassFields:
                        pass
                        #fieldGroup.add(str(fieldName), "???")
                    elif fieldName.__contains__(" "):  #bad chars in fieldname (happens in blender)
                        if TTrace.options.sendFields == True:
                            fieldGroup.add("???", "???")
                    elif TTrace.options.sendPrivate == True or fieldName.startswith('_') == False:
                        try:
                            fieldValue = getattr(objToSend, fieldName)
                            info = TTrace.Utility.getObjectInfo(fieldValue)

                            memberValue = info.memberValue
                            #don't print doc again
                            if fieldName == "__doc__" :
                                memberValue = "[" + str(len(memberValue))+ "]"

                            if info.isFunction :
                                if TTrace.options.sendFunctions == True:
                                    methodGroup.add(memberValue,info.memberType, info.doc)
                            else:
                                if TTrace.options.sendFields == True:
                                    fieldGroup.add(str(fieldName), info.returnType + memberValue)
                        except BaseException as e :
                            infoError = TTrace.Utility.getObjectInfo(e)
                            fieldValue = str(infoError.memberType) + " " +  str(e.message)
                            #TTrace.debug.sendObject("exception", e)
                            if TTrace.options.sendFields == True:  # error are considered as field
                                fieldGroup.add(str(fieldName), fieldValue)

                # show groups only if not empty
                if len(fieldGroup.members) != 0:
                    self.members.add(fieldGroup)
                if len(methodGroup.members) != 0:
                    self.members.add(methodGroup)

        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)


    # ----------------------------------------------------------------------

    def addValue(self,objToSend, sendPrivate=False, maxLevel=3, title= None, alreadyParsedObject = None):
        """Send Private and public values of an object.
        addValue is quite different from addObject : less verbal (no class info) but can show many level.
        Take care that sendValue, sendObject , addValue and addObject functions are time consuming. Many informations are collected
        @param objToSend : The object to examine
        @param sendPrivate : flag to send private field
        @param maxLevel : The number of sub element to display. Default is 3
        @param title : Object title
        @param alreadyParsedObject : List of already parsed objects
        """
        if self.enabled == False:
            return

        if objToSend is None:
            self.members.add("None")
            return

        if title is None:
            title = type(objToSend)

        if alreadyParsedObject is None :
            alreadyParsedObject = []
        try:
            # create the top node using only title.
            # Value (col2) and Type (col3) will be added by inner_addValue
            result = TMemberNode(title)
            result.viewerKind = _Internals.TraceConst["CST_VIEWER_VALUE"]
            self.members.add(result)

            # recursive fill members
            self._innerAddValue(objToSend, result, maxLevel, sendPrivate, alreadyParsedObject)
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

    # ----------------------------------------------------------------------
    def _innerAddValue(self,objToSend, upperNode, maxLevel, sendPrivate, alreadyParsedObject):
        """ internal AddValue (recursive)
        @param objToSend : The object to examine
        @param upperNode : The upper node (recursive add sub objects)
        @param maxLevel : The number of sub element to display.
        @param sendPrivate : flag to send private field
        @param alreadyParsedObject : List of already parsed objects
        """

        try:
            if (objToSend == None):
                upperNode.col2 = "None"
                return

            objClass = type(objToSend)

            # display the modifier and type name in upper node (col 3). Old col3 content is keept
            upperNode.col3 += str (objClass)

            # display value in upper node (col2)
            info = TTrace.Utility.getObjectInfo(objToSend)
            hashCode = id(objToSend)

            # check primitive and well know type
            if (objClass in _Internals.primitives) or (str (objClass) in _Internals.primitives) :   # number, boolean, string, date
                upperNode.col2 = info.returnType + str(objToSend) #+ "@" + str(hashCode)
                return
            if objClass in TTrace.options.nativeClasses:          # your business classes
                upperNode.col2 = info.returnType + str(objToSend) + "@" + str(hashCode)
                return

            # check if the object is already parsed
            # if (Utility.containObject(alreadyParsedObject,objToSend))
            if hashCode in alreadyParsedObject:
                upperNode.col2 = "see " + str(hashCode)
                return

            # add the hashCode
            #if info.name != "" :

            upperNode.col2 = info.returnType + info.name + "@" + str(hashCode)

            #else:
            #    upperNode.col2 = info.returnType + info.memberType + "@" + str(hashCode)

            # max level reached : display the hashCode, since ToString don't tell what object is
            if maxLevel <= 1:
                return

            # no more display this object content (array or fields)
            try:
                #/ this is the only place where object is added to alreadyParsedObject list
                # adding object to hashtable use the object hash code.
                # detect if the object hash code function generate exception
                alreadyParsedObject.append(hashCode)
            except BaseException as e:
                tr =   inspect.trace() [0]
                print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

            # send normal collections (array and tuples). ToDo : enumerable
            # Note that named tuple are indirect instance of tuples but will not be printed as an array
            if isinstance(objToSend ,(list, set, frozenset)) or objClass is tuple:
                c = 0
                for elem in objToSend:
                    title = "[" + str(c) + "]"
                    #create the node with just array indice title
                    node = TMemberNode(title)
                    upperNode.add(node)
                    self._innerAddValue(elem, node, maxLevel - 1, sendPrivate, alreadyParsedObject)
                    c=c+1
                return

            # send Map
            if isinstance(objToSend,dict) or type(objToSend) is type(type.__dict__):    # types.DictProxyType
                c = 0
                for key in objToSend.keys():
                    if (key == None):
                        title = "[None]"
                    else:
                        title = "[" +str(key) + "]"
                    elem = objToSend[key]
                    #create the node with just array indice title
                    node = TMemberNode(title)
                    upperNode.add(node)
                    self._innerAddValue(elem, node, maxLevel - 1, sendPrivate, alreadyParsedObject)
                    c=c+1
                return

            # not a collection, parse the object
            items = dir(objToSend)
            for fieldName in items:
                if sendPrivate == True or fieldName.startswith('_') == False:
                    try:
                        fieldValue = getattr(objToSend, fieldName)
                        info = TTrace.Utility.getObjectInfo(fieldValue)
                        #memberValue = info.memberValue
                        if info.isFunction :
                            continue

                        node = TMemberNode(str(fieldName))
                        upperNode.add(node)
                        self._innerAddValue(fieldValue, node, maxLevel - 1, sendPrivate, alreadyParsedObject) #memberValue
                    except BaseException as e :
                        infoError = TTrace.Utility.getObjectInfo(e)
                        fieldValue = str(infoError.memberType) + " " +  e.message
                        #TTrace.debug.sendObject("exception", e)
                        upperNode.add(str(fieldName), fieldValue)

        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)


    # ----------------------------------------------------------------------
    # deprecated
    #def addTable(self,table):
    #    """ Add table to node
    #    @param table xml table to send
    #    """
    #    if self.enabled == False :
    #        return
    #
    #    table.copyToNodeMembers(self.members) # copy member to node. Member viewer kind is already set

    # ----------------------------------------------------------------------
    def addTable(self,table):
        """ Add table to node
        @param table : Object collection(Array / Collection / Map) to send
        """
        if self.enabled == False :
            return


        # create table
        tableMembers = TMemberNode("")
        tableMembers.viewerKind = _Internals.TraceConst["CST_VIEWER_TABLE"]
        self.members.add(tableMembers)

        # fill table

        # send normal collections (array and tuples). ToDo : enumerable
        # Note that named tuple are indirect instance of tuples but will not be printed as an array
        if isinstance(table ,(list, set, frozenset)) or type(table) is tuple:
            if len(table) > 0:
                tableMembers.col1 = "[]"
            rowNum = 0
            for elem in table:
                self._innerAddTable( tableMembers,  elem,  rowNum)
                rowNum += 1

        # send Map
        elif isinstance(table, dict) or type(table) is type(type.__dict__):         # types.DictProxyType
            for key in table.keys():
                if (key == None):
                    title = "None"
                else:
                    title = str(key)
                elem = table[key]
                tableMembers.col1 = "Keys" + "\t" + "Values"
                tableMembers.add(title + "\t" + str(elem))

        elif type(table) is enumerate:
            for rowNum, elem in table:
                self._innerAddTable( tableMembers,  elem,  rowNum)

        elif str(type(table)) == "<type 'xrange'>" :   # python 2 compatibility
            tableMembers.col1 = "Values"
            for rowNum in table:
                self._innerAddTable( tableMembers,  rowNum,  None)
        # not a collection : print object
        #innerAddTable( TableMembers,  list,  isFirst)


    # ----------------------------------------------------------------------
    def _innerAddTable(self,tableMembers, rowObject, rowNum):

        def checkMapInHeader ():
            for fieldName in tableMembers.header:
                try:
                    if _Internals.pyVersion <= 2:       # python 2 : map.has_key(fieldName)
                        haskey = rowObject.has_key(fieldName)
                    else:                               # python 3 : use fieldName in rowObject
                        haskey = fieldName in rowObject

                    if not haskey:
                        memberValue = " "     # use empty string in place of value
                    else:
                        fieldValue = rowObject[fieldName]
                        info = TTrace.Utility.getObjectInfo(fieldValue)
                        memberValue = info.memberValue
                        if info.isFunction :
                            memberValue = " " # use empty string in place of value

                except BaseException as e :
                    infoError = TTrace.Utility.getObjectInfo(e)
                    memberValue = str(infoError.memberType) + " " +  e.message

                fCurrentRow.col1 = fCurrentRow.col1 + "\t" + str(memberValue)

        def checkObjectInHeader ():
            #first check for existing fields saved in temp array "header"
            for fieldName in tableMembers.header:
                try:
                    if not hasattr(rowObject, fieldName):
                        memberValue = " "     # use empty string in place of value
                    else:
                        fieldValue = getattr(rowObject, fieldName)
                        info = TTrace.Utility.getObjectInfo(fieldValue)
                        memberValue = info.memberValue
                        if info.isFunction :
                            memberValue = " " # use empty string in place of value

                except BaseException as e :
                    infoError = TTrace.Utility.getObjectInfo(e)
                    memberValue = str(infoError.memberType) + " " +  e.message

                fCurrentRow.col1 = fCurrentRow.col1 + "\t" + str(memberValue)

        def addFieldAndValue(): # fieldName and fieldValue
            # this is a new column
            info = TTrace.Utility.getObjectInfo(fieldValue)
            memberValue = info.memberValue
            if info.isFunction :
                return
            tableMembers.header.append(fieldName)
            tableMembers.col1 = tableMembers.col1 + "\t" + fieldName
            fCurrentRow.col1 = fCurrentRow.col1 + "\t" + str(memberValue)

        try:

            if rowNum == None:
                fCurrentRow = tableMembers.add("")
            else:
                fCurrentRow = tableMembers.add(str(rowNum))
            objClass = type(rowObject)

            # The second dimension can be : primitives, NativeClasses, array , map or object

            # check None
            if rowObject == None:
                if (rowNum == 0):
                    tableMembers.col1 += "\t" +"Values"
                fCurrentRow.col1 += "\t" + "None"

            # check primitive , your business classes , and well know type : number, boolean, string, date
            elif objClass in _Internals.primitives or str (objClass) in _Internals.primitives or objClass in TTrace.options.nativeClasses:
                # Add Column Title if first line
                if rowNum == 0:
                    tableMembers.col1 = "\t" +"Values"
                if fCurrentRow.col1 == "":
                    fCurrentRow.col1 = str(rowObject)
                else:
                    fCurrentRow.col1 += "\t" + str(rowObject)

            # collection
            elif isinstance(rowObject ,(list, set, frozenset)) or type(rowObject) is tuple:
                # Note that named tuple are indirect instance of tuples but will not be printed as an array
                # print tuple content
                colNum = 1
                for fieldValue in rowObject:
                    fieldName = str(colNum)

                    if len(tableMembers.header) < colNum :
                        tableMembers.header.append(fieldName)
                        tableMembers.col1 = tableMembers.col1 + "\t" + fieldName
                    colNum += 1
                    fCurrentRow.col1 = fCurrentRow.col1 + "\t" + str(fieldValue)

            # Map
            elif isinstance(rowObject, dict) or type(rowObject) is type(type.__dict__) :
                checkMapInHeader ()
                for fieldName in rowObject.keys():
                    if fieldName in tableMembers.header:
                        continue  # already put in previous loop
                    if TTrace.options.sendPrivate == False and fieldName.startswith('_') == True:
                        continue
                    fieldValue = rowObject[fieldName]
                    # add fieldName and fieldValue vars
                    addFieldAndValue()

            # Object or named tuple : use attributes as second dimension
            else:
                checkObjectInHeader()
                items = dir(rowObject)
                for fieldName in items:
                    if fieldName in tableMembers.header:
                        continue  # already put in previous loop
                    if TTrace.options.sendPrivate == False and fieldName.startswith('_') == True:
                        continue

                    try:
                        fieldValue = getattr(rowObject, fieldName)
                    except BaseException as e :
                        fieldValue = e
                        #infoError = TTrace.Utility.getObjectInfo(e)
                        #memberValue = str(infoError.memberType) + " " +  e.message

                    # add fieldName and fieldValue vars
                    addFieldAndValue()

        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)



    # ----------------------------------------------------------------------

    def addDump(self, bytesBuffer, shortTitle = "Dump",  index=0, count=0):
        """ Display dump
        @param shortTitle : A short title displayed on top of the dump
        @param bytesBuffer : The buffer to dump :
            string (single byte char or unicode) , bytearray or any array like object that has char or int elements
        @param index : start index (default is first)
        @param count : Number of byte to dump (default is all)
        """

        #print("input buffer :", bytesBuffer)
        if self.enabled == False :
            return

        if count <= 0:
            count = len(bytesBuffer)

        c = index
        byteDumped = 0

        try:
            dumpGroup = TMemberNode(shortTitle).setFontDetail(0, True)
            dumpGroup.viewerKind = _Internals.TraceConst["CST_VIEWER_DUMP"]
            self.members.add(dumpGroup)

            while byteDumped < count and c < len(bytesBuffer):

                d = 0 # inner loop. From 0 to 15 max
                beginLine = c # used to print the offset
                hexaRepresentation = ""
                adr = ""
                #strRepresentation = ""

                while byteDumped < count and d < 16 and c < len(bytesBuffer):
                    oneByte = bytesBuffer[c]


                    if type(oneByte) is int:
                        valChar = oneByte
                        #repChar = chr(oneByte)
                    else: # the byte is a char
                        valChar = ord(oneByte)
                        #repChar = oneByte

                    if _Internals.pyVersion <= 2:
                        strChar =  "%02X" % valChar
                    else:  # starting in python 3, string are unicode so chars are 2 bytes length
                        strChar =  "%04X" % valChar
                    hexaRepresentation += strChar + " "

                    # only the zero cannot be copied to the stream
                    #if (valChar == 0):
                    #    strRepresentation += '.'
                    #else:
                    #    strRepresentation += repChar

                    byteDumped += 1
                    d += 1
                    c += 1

                adr  += "%06X" % beginLine

                #Utility.leftPadding(adr, 6, '0')
                dumpGroup.add(adr, hexaRepresentation) #, strRepresentation)

            dumpGroup.col2 = str(byteDumped) + " byte(s) dumped"
        except BaseException as e:
            tr =   inspect.trace() [0]
            print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)


    # ----------------------------------------------------------------------
    def addStackTrace(self):
        """ add stack trace  """
        if self.enabled == False :
            return

        group = TMemberNode("Caller stack").setFontDetail(0, True)
        group.viewerKind = _Internals.TraceConst["CST_VIEWER_STACK"]
        self.members.add(group)

        try:
            # some implementations don't give the stack correctly (IronPyhthon 2.6 for example)
            st = inspect.stack()
        except AttributeError as e:
            group.add ("Unable to get call stack : " + str(e) ) # IronPyhthon
            return

        for frame, filename , lineno, function, code_context, index in st :
            if (filename.endswith("tracetool.py") or filename.endswith("tracetool.pyc")) and function in ("addStackTrace","sendStack") :
                continue
            oneFrame = TMemberNode("Frame")
            group.add(oneFrame)
            oneFrame.add("frame",str(frame))
            oneFrame.add("filename",str(filename))
            oneFrame.add("lineno",str(lineno))
            oneFrame.add("function",str(function))
            oneFrame.add("code_context",str(code_context))
            oneFrame.add("index",str(index))

        group = None

        st = inspect.trace()
        for frame, filename , lineno, function, code_context, index in st:
            if (filename.endswith("tracetool.py") or filename.endswith("tracetool.pyc")) and function in ("addStackTrace","sendStack") :
                continue
            if group == None:
                group = TMemberNode("Exception stack").setFontDetail(0, True)
                group.viewerKind = _Internals.TraceConst["CST_VIEWER_STACK"]
                self.members.add(group)
            oneFrame = TMemberNode("Frame")
            group.add(oneFrame)
            oneFrame.add("frame",str(frame))
            oneFrame.add("filename",str(filename))
            oneFrame.add("lineno",str(lineno))
            oneFrame.add("function",str(function))
            oneFrame.add("code_context",str(code_context))
            oneFrame.add("index",str(index))

    # ----------------------------------------------------------------------
    def addCaller(self):
        """ show caller information.
        level 0 correspond at the first line of StackTrace(0)
        It's like the call stack, but display only 1 line
        """

        if self.enabled == False :
            return

        if self.enabled == False :
            return

        group = TMemberNode("Caller stack").setFontDetail(0, True)
        group.viewerKind = _Internals.TraceConst["CST_VIEWER_STACK"]
        self.members.add(group)

        try:
            # some implementations don't give the stack correctly (IronPyhthon 2.6 for example)
            st = inspect.stack()
        except AttributeError as e:
            group.add ("Unable to get call stack : " + str(e) ) # IronPyhthon
            return
        for frame, filename , lineno, function, code_context, index in st :
            if (filename.endswith("tracetool.py") or filename.endswith("tracetool.pyc")) and function in ("addCaller","sendCaller") :
                continue
            oneFrame = TMemberNode("Frame")
            group.add(oneFrame)
            oneFrame.add("frame",str(frame))
            oneFrame.add("filename",str(filename))
            oneFrame.add("lineno",str(lineno))
            oneFrame.add("function",str(function))
            oneFrame.add("code_context",str(code_context))
            oneFrame.add("index",str(index))
            return  # only first trace

    # ----------------------------------------------------------------------
    def addXML(self,xml):
        """ Add xml text
        @param xml : xml text to send
        """

        if self.enabled == False :
            return

        member = self.members.add(xml)
        member.viewerKind = _Internals.TraceConst["CST_VIEWER_XML"]

    # ----------------------------------------------------------------------
    def addBackgroundColor(self,color, colId=-1):
        """ Change background font color
        @param color : xml background.-1 for the default color or tuple (R,G,B)
        @param colId : Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
        """

        if self.enabled == False:
            return

        fontDetail = FontDetail()
        fontDetail.colId = colId
        fontDetail.color = color
        fontDetail.fontName = "BackgroundColor"  # special name. Indicate that color is for background, not font itself
        self._fontDetails.append(fontDetail)

    # ----------------------------------------------------------------------
    def addFontDetail(self,colId, bold=False, italic=False, color = -1, size = 0, fontName=""):
        """ Change font detail for an item in the trace
        @param colId : Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
        @param bold : Change font to bold
        @param italic : Change font to Italic
        @param color : Change Color. -1 for the default color or tuple (R,G,B)
        @param size : Change font size, use zero to keep normal size
        @param fontName : Change font name
        @returns: The trace node
        """

        if self.enabled == False :
            return

        fontDetail = FontDetail()
        fontDetail.colId = colId
        fontDetail.bold = bold
        fontDetail.italic = italic
        fontDetail.color = color
        fontDetail.size = size
        fontDetail.fontName = fontName

        self._fontDetails.append(fontDetail)
        return self

    # ----------------------------------------------------------------------
    def send(self):
        """ Send the traceNodeEx to the viewer (left + right + members)
        @returns: a traceNode
        """

        if self.enabled == False :
            return
        result = TraceNode(self) # create a copy
        commandList = []

        commandList.append("%5d%s" % (_Internals.TraceConst["CST_NEW_NODE"] , self.parentNodeId)) # param : parent Node id
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TRACE_ID"] , self.id)) # param : Node id

        if self.leftMsg != None:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_LEFT_MSG"] , self.leftMsg)) # param : left string
        if self.rightMsg != None:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_RIGHT_MSG"] , self.rightMsg)) # param : right string

        commandList.append("%5d%d" % (_Internals.TraceConst["CST_ICO_INDEX"] , self.iconIndex)) # param : Icon index

        # add font detail
        if self._fontDetails != None:
            for fontDetail in self._fontDetails:
                colorValue = TTrace.Utility.rgbToBgr(fontDetail.color)
                if fontDetail.fontName == "BackgroundColor":
                    #special color : background color
                    commandList.append("%5d%3d%s" % (_Internals.TraceConst["CST_BACKGROUND_COLOR"] , colorValue, str(fontDetail.colId)))      # param : color, colId
                else:
                    tempStr = "%5d%3d" % (_Internals.TraceConst["CST_FONT_DETAIL"],fontDetail.colId )
                    if fontDetail.bold:
                        tempStr = tempStr + "1"
                    else:
                        tempStr = tempStr + "0"

                    if fontDetail.italic:
                        tempStr = tempStr + "1"
                    else:
                        tempStr = tempStr + "0"

                    tempStr = tempStr + "%11d%11d%s" % (TTrace.Utility.rgbToBgr(fontDetail.color), fontDetail.size,fontDetail.fontName)
                    commandList.append(tempStr)
            self._fontDetails = []
        self.members.addToStringList(commandList) #IGNORE:W0212 #convert all groups and nested items/group to strings

        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)
        return result

    # ----------------------------------------------------------------------
    def resend(self):
        """ Re-send the trace to the viewer (only left and right message)"""
        if self.enabled == False :
            return

        # don't re-send members and icon
        commandList = []

        commandList.append("%5d%s" % (_Internals.TraceConst["CST_USE_NODE"] , self.id)) # param : Node id
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_LEFT_MSG"] , self.leftMsg)) # param : left string
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_RIGHT_MSG"] , self.rightMsg)) # param : right string

        TTrace.Utility.sendToWinTraceClient(commandList, self.winTraceId)


#-----------------------------------------------------------------------------

class WinWatch(object):
    """ WinWatch represent a windows tree where you put watches

    Sample code :

    >>> TTrace.watches().send("test2", TTrace.Utility.currentTime())
    """
    def __init__(self,winWatchID = None , winWatchText = None):
        """ WinWatch constructor.

        You can map a WinWatch to an existing window.

        Nothing Is send to the viewer if no param is given else the Window watch is create on the viewer (if not already done)
        @param winWatchID : Required window trace Id. If empty, a guid will be generated
        @param winWatchText : The Window Title on the viewer.If empty, a default name will be used
        """
        self.enabled = True # When enabled is false, all traces are disabled. Default is true
        self.id = ""        # The "Required" Id of the window tree, can be any string, or a guid. The Main window watch Id is empty
        self.tag = None     # User variable, provided for the convenience of developers

        if winWatchID == None and winWatchText == None:
            return

        if winWatchID == None or len(winWatchID) == 0:
            self.id = TTrace.Utility.uuid()
        else:
            self.id = winWatchID

        # create the trace window
        commandList = []

        if winWatchText == None or len(winWatchText) == 0:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_WINWATCH_NAME"] , "Watches " +self.id))
        else:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_WINWATCH_NAME"] , winWatchText))

        TTrace.Utility.sendToWinWatchClient(commandList,self)


    # ----------------------------------------------------------------------
    def displayWin(self):
        """ Switch viewer to this window """

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_DISPLAY_TREE"] ))
        TTrace.Utility.sendToWinWatchClient(commandList,self)

    # ----------------------------------------------------------------------
    def clearAll(self):
        """ clear all traces in that window """

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLEAR_ALL"]  ))
        TTrace.Utility.sendToWinWatchClient(commandList,self)

    # ----------------------------------------------------------------------
    def close(self):
        " clear all traces in that window"""

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLOSE_WIN"]  ))
        TTrace.Utility.sendToWinWatchClient(commandList,self)

    # ----------------------------------------------------------------------
    def send (self, watchName , watchValue):
        """ Send a watch (displayed as sendValue does)
        @param watchName : Watch name
        @param watchValue : Watch value
        """

        if self.enabled == False :
            return

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_WATCH_NAME"] , watchName))

        node = TraceNodeEx(None, False)

        # informations are added to the members array of the new created object.
        node.addValue(watchValue, TTrace.options.sendPrivate , TTrace.options.objectTreeDepth , "")   # no title

        # convert all groups and nested items/group to strings
        node.members.addToStringList(commandList)

        TTrace.Utility.sendToWinWatchClient(commandList,self)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class WinTrace(TraceToSend):
    """ WinTrace represent a windows tree where you put traces

    Sample code :

    >>> myWinTrace = WinTrace ("MyWINID" , "My trace window")
    >>> myWinTrace.debug.send ("Hello", "Can be used to store exceptions, for examples")
    """

    #-----------------------------------------------------------------------------

    def __init__(self,winTraceID=None,winTraceTitle=""):
        """ WinTrace constructor.
        The Window Trace is create on the viewer (if not already done)
        @param winTraceID : Required window trace Id. If empty, a guid will be generated
        @param winTraceTitle : The Window Title on the viewer.If empty, a default name will be used
        """

        # TraceNodeBase fields : id ,  enabled , winTraceId, tag, iconIndex
        # TraceToSend   fields : _contextList ,  _winTraceContext , _lock
        TraceToSend.__init__(self)  # base class constructor
        if winTraceID is None or len(winTraceID) == 0:
            self.id = TTrace.Utility.uuid()
        else:
            self.id = winTraceID

        self.iconIndex = _Internals.TraceConst["CST_ICO_DEFAULT"]  # WinTrace don't have icon (for now)
        self.enabled = True
        self.winTraceId = self.id    # winTraceId need to be the same as 'id' if we want to call sendXxx() directly on WinTrace object

        # Warning, Error and Debug nodes are TraceSend, but since TraceSend is abstract, create a descendant

        self.warning = TraceNode(None, False)
        self.warning.iconIndex =_Internals.TraceConst["CST_ICO_WARNING"]
        self.warning.winTraceId = self.id
        self.warning.winTraceContext = self._contextList
        self.warning.enabled = True

        self.error = TraceNode(None, False)
        self.error.iconIndex =_Internals.TraceConst["CST_ICO_ERROR"]
        self.error.winTraceId = self.id
        self.error.winTraceContext = self._contextList
        self.error.enabled = True

        self.debug = TraceNode(None, False)
        self.debug.iconIndex =_Internals.TraceConst["CST_ICO_INFO"]
        self.debug.winTraceId = self.id
        self.debug.winTraceContext = self._contextList
        self.debug.enabled = True

        # if the winTraceID is "_" then this is the default main wintrace
        if winTraceID == "_": #winTraceID != None and
            return   # don't create new window on the viewer

        if winTraceTitle is None or len(winTraceTitle) == 0:
            wTraceTitle = self.id
        else:
            wTraceTitle = winTraceTitle

        # create the trace window

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TREE_NAME"], wTraceTitle))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def clearAll(self):
        """ clear all trace in that window"""

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLEAR_ALL"]))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)


    # ----------------------------------------------------------------------
    def setLogFile(self, fileName, mode, maxLines=-1):
        """Set the log file.

        This function is partially developped : Local log (mode 3,4 and 5) is not yet implemented

        TODO : To enabled log on local AND on the viewer call this function twice.

        TODO : to don't use the viewer, set the TTrace.options.SendMode to none
        @param fileName : file to save.
        @param mode : Local and viewer site log mode.
          -  0 : Viewer Log is disabled.
          -  1 : Viewer log enabled. (Path is relative to the viewer)
          -  2 : Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
          -  3 : TODO : Local log is disabled
          -  4 : TODO : Local log enabled. (Path is relative to the client application)
          -  5 : TODO : Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
        @param maxLines : Number of lines before starting a new file (default : -1 = unlimited)
        """

        # 3, Local log is disabled
        # 4, Local log enabled. No size limit.
        # 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
        if mode >= 3:
            # not yet implemented
            pass

            #InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(self.id, true)
            #TraceForm.LogFileName = fileName
            #TraceForm.LogFileType = mode
            #TraceForm.MaxLines = maxLines
            # don't send anything to the viewer.
        else:
            commandList = []
            commandList.append("%5d%11d%11d%s" % (_Internals.TraceConst["CST_LOGFILE"],mode,maxLines, fileName ))
            TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def setMultiColumn(self, mainColIndex=0):
        """ change the tree to display user defined multiple columns
        @param mainColIndex : The Main column index (default is 0)
        """

        #InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(self.id, true)
        #TraceForm.IsMultiColTree = true
        #TraceForm.MainCol = mainColIndex

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TREE_MULTI_COLUMN"],mainColIndex ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def setColumnsTitle(self, value):
        """ set columns title
        @param value : tab separated columns titles
        """

        #InternalWinTrace TraceForm = SenderThread.getInternalTraceForm(self.id, true)
        #TraceForm.IsMultiColTree = true
        #TraceForm.TitleList = value

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TREE_COLUMNTITLE"], value))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def setColumnsWidth(self, widths):
        """ set columns widths
        @param widths : Tab separated columns width.
        The format for each column is width[:Min[:Max]]
        where Min and Max are optional minimum and maximum column width for resizing purpose.
        Example : 100:20:80 tab 200:50 tab 100
        """

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_TREE_COLUMNWIDTH"], widths))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def gotoFirstNode(self) :
        """ Set the focus to the trace first node """

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_GOTO_FIRST_NODE"] ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def gotoLastNode(self) :
        """ Set the focus to the trace last node """

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_GOTO_LAST_NODE"] ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def findNext(self, searchForward=True) :
        """ Set the focus to the next matching node
        @param searchForward : If true search down, else search up
        """

        commandList = []
        if searchForward:
            commandList.append("%5d%11d" % (_Internals.TraceConst["CST_FIND_NEXT"], 1))
        else:
            commandList.append("%5d%11d" % (_Internals.TraceConst["CST_FIND_NEXT"], 0))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def gotoBookmark(self, pos):
        """ Set the focus to a bookmarked node identified by his position. Bookmarks are cheched by the user or with the node.SetBookmark() function
        @param pos : Indice of the bookmark
        """

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_GOTO_BOOKMARK"],pos ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def clearBookmark(self):
        """ Clear all bookmarks """
        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLEAR_BOOKMARK"] ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def clearFilter(self) :
        """ Clear all filters """
        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLEAR_FILTER"], ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def addFilter(self, column , compare , text) :
        """ Add a filter to node. Multiple calls to this function can be done. Call ApplyFilter() to apply filtering
        @param column : column to apply filter.
           - In multicolumn mode the first column start at 0
           - In normal mode :
           - col icone   = 999
           - col time    = 1
           - col thread  = 2
           - col traces  = 3
           - col Comment = 4
           - col members = 998
        @param compare : There is 5 kinds of filters :
           - Equal           = 0
           - Not equal       = 1
           - Contains        = 2
           - Don't contains  = 3
           - (Ignore this filter) = 4 or -1
        @param text : The text to search (insensitive)
        """

        commandList = []
        commandList.append("%5d%11d%11d%s" % (_Internals.TraceConst["CST_ADD_FILTER"],column, compare , text ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def applyFilter(self, conditionAnd, showMatch, includeChildren) :
        """ Apply filters after calls to AddFilter().
        @param conditionAnd : If true, use an 'AND' condition for each filters, else use a "OR"
        @param showMatch : If true, show node that match filter and hide others. If false hide matching node and show others
        @param includeChildren : If true, search in subnodes
        """

        flags = 0
        # ConditionAnd<<2+ShowMatch<<1+IncludeChildren
        if conditionAnd:
            flags += 4
        if showMatch:
            flags += 2
        if includeChildren:
            flags += 1

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_APPLY_FILTER"], flags))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def saveToTextfile(self, fileName):
        """ Save window content to text file  (Path is relative to the viewer)
        @param fileName : target filename
        """

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_SAVETOTEXT"],fileName ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def saveToXml(self, fileName, styleSheet = ""):
        """ Save window content to xml file
        (Path is relative to the viewer)
        @param fileName : target filename
        @param styleSheet : optional stylesheet file name
        """

        commandList = []
        if styleSheet == "":
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_SAVETOXML"], fileName ))
        else:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_SAVETOXML"], fileName + "|" + styleSheet))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)


    # ----------------------------------------------------------------------
    def loadXml(self, fileName):
        """ Load xml file to the window
        (Path is relative to the viewer)
        @param fileName : target filename
        """

        commandList = []
        commandList.append("%5d%s" % (_Internals.TraceConst["CST_LOADXML"], fileName))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def displayWin(self):
        """ Switch viewer to this window """

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_DISPLAY_TREE"] ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

    # ----------------------------------------------------------------------
    def close(self):
        """ clear all trace in that window"""

        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLOSE_WIN"] ))
        TTrace.Utility.sendToWinTraceClient(commandList, self.id)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TTraceOptions(object):
    """ Public options (Singleton) for the traces (Host,Port,...). """

    def __init__(self):
        self.socketHost = "127.0.0.1"
        """ The Socket Host address. Default is 127.0.01 """

        self.socketPort = 8090
        """ The socket port. Default is 8090 """

        self.socketUDP = False
        """ socket UDP mode. (UDP mode is not yet implemented). default is false """

        # sendObject or addObject flags

        self.sendClassInfo = True
        """ When True (default) , the class information is send by sendObject or addObject """

        self.sendInherited = True
        """ When True (default) , class information ancestors is send by by sendObject or addObject """

        self.sendFields = True
        """ When True (default) , fields are send by sendObject or addObject """

        self.sendFunctions = False
        """ When True , functions are send by sendObject or addObject. Default is False """

        self.sendPrivate = True
        """ When True (default) , private properties and functions are send by sendObject or addObject """

        self.sendDoc = True
        """ When True (default) , doc is send by sendObject or addObject """

        # general flags

        self.useThread = False
        """ When True , traces are send using a thread. Default is False.
        You may need to call TTrace.flush() to ensure all traces are send before closing your application.
        Dependending of the python host, thread synchronisation can slow down the application.
        """

        self.sendDate = False
        """ Indicate if the date must be send with the time. Default is false """

        self.sendThreadName = True
        """ Indicate if the thread name must be send . Default is true """

        self.objectTreeDepth = 3
        """ Max Object tree depth for sendValue and Watches. Default is 3 levels """

        self.nativeClasses = []
        """Your native business classes type that don't have to be parsed by addValue and addTable. """


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class _Internals(object):
    """ Private singleton container for internals tracetool vars """

    # private : primitives list
    primitives = [
        int,
        "<type 'long'>",   # long is no more supported in python 3, so we store the string type
        float,
        bool,
        complex,
        #unicode,
        #buffer
        str,
    ]

    # private list of const
    TraceConst = {
       "WMD":123 ,   #  Identification code 'traceTool'. Other code are discarded by the server
       "CST_ICO_DEFAULT":-1 ,   # Use the default Icon on the gutter for the trace
       "CST_ICO_FORM":0 ,   # Use the 'form' Icon on the gutter for the trace
       "CST_ICO_COMPONENT":1 ,   # Use the 'component' Icon on the gutter for the trace
       "CST_ICO_CONTROL":3 ,   # Use the 'control' Icon on the gutter for the trace
       "CST_ICO_PROP":5 ,   # Use the 'property' Icon on the gutter for the trace
       "CST_ICO_MENU":15 ,   # Use the 'memu' Icon on the gutter for the trace
       "CST_ICO_MENU_ITEM":16 ,   # Use the 'menu item' Icon on the gutter for the trace
       "CST_ICO_COLLECT_ITEM":21 ,   # Use the 'Collection Item' Icon on the gutter for the trace
       "CST_ICO_WARNING":22 ,   # Use the 'warning' Icon on the gutter for the trace
       "CST_ICO_ERROR":23 ,   # Use the 'Error' Icon on the gutter for the trace
       "CST_ICO_INFO":24 ,   # Use the 'Info' Icon on the gutter for the trace , default

       # viewer kind

       "CST_VIEWER_NONE":0,   # viewer kind : default viewer, no icon
       "CST_VIEWER_DUMP":1,   # viewer kind : dump viewer
       "CST_VIEWER_XML":2,   # viewer kind : xml viewer
       "CST_VIEWER_TABLE":3,   # viewer kind : table viewer
       "CST_VIEWER_STACK":4,   # viewer kind : stack
       "CST_VIEWER_BITMAP":5,   # viewer kind : bitmap viewer
       "CST_VIEWER_OBJECT":6,   # viewer kind : object structure
       "CST_VIEWER_VALUE":7,   # viewer kind : object value
       "CST_VIEWER_ENTER":8,   # viewer kind : enter method
       "CST_VIEWER_EXIT":9,   # viewer kind : exit method
       "CST_VIEWER_TXT":10,   # viewer kind : text added to default viewer

       # plugin const

       "CST_PLUG_ONACTION":1 ,   #  Ask to receive OnAction event
       "CST_PLUG_ONBEFOREDELETE":2 ,   # Ask to receive OnBeforeDelete event
       "CST_PLUG_ONTIMER":4 ,   # Ask to receive OnTimer event

       # resource kind

       "CST_RES_BUT_RIGHT":1 ,   # Button on right
       "CST_RES_BUT_LEFT":2 ,   # Button on left
       "CST_RES_LABEL_RIGHT":3 ,   # Label on right
       "CST_RES_LABELH_RIGHT":4 ,   # Label on right HyperLink
       "CST_RES_LABEL_LEFT":5 ,   # Label on left
       "CST_RES_LABELH_LEFT":6 ,   # Label on left hyperlink
       "CST_RES_MENU_ACTION":7 ,   # Item menu in the Actions Menu
       "CST_RES_MENU_WINDOW":8 ,   # Item menu in the Windows Menu. Call CreateResource on the main win trace to create this menu item

       # resource id

       "CST_ACTION_CUT":1 ,   # cut same as copy then delete
       "CST_ACTION_COPY":2 ,   # copy
       "CST_ACTION_DELETE":3 ,   # delete selected
       "CST_ACTION_SELECT_ALL":4 ,   # select all
       "CST_ACTION_RESIZE_COLS":5 ,   # resize columns
       "CST_ACTION_VIEW_INFO":6 ,   # view trace info
       "CST_ACTION_VIEW_PROP":7 ,   # view properties
       "CST_ACTION_PAUSE":8 ,   # Pause
       "CST_ACTION_SAVE":9 ,   # SaveToFile
       "CST_ACTION_CLEAR_ALL":10 ,   # clear all
       "CST_ACTION_CLOSE_WIN":11 ,   # Close win
       "CST_ACTION_RESUME":12 ,   # resume from Pause
       "CST_ACTION_LABEL_INFO":20 ,   # TracesInfo label
       "CST_ACTION_LABEL_LOGFILE":21 ,   # LabelLogFile label
       "CST_ACTION_VIEW_MAIN":50 ,   # View Main trace
       "CST_ACTION_VIEW_ODS":51 ,   # ODS
       "CST_ACTION_OPEN_XML":52 ,   # XML trace -> Tracetool XML traces
       "CST_ACTION_EVENTLOG":53 ,   # Event log
       "CST_ACTION_TAIL":54 ,   # Tail

       # COMMANDS
       #================

       # INTERNAL
       #--------------------------------------------------------------------------


       "CST_ENTER_DEBUG_MODE":107,   # VIEWER INTERNAL : enter debug mode
       "CST_LEAVE_DEBUG_MODE":108,   # VIEWER INTERNAL : leave debug mode
       "CST_OPEN_TAIL":109,   # VIEWER INTERNAL : Open tail file . Param : file name
       "CST_OPEN_XML":113,   # VIEWER INTERNAL : Open xml file on a new window (don't confuse with CST_LOADXML).Param : file name
       "CST_GET_OBJECT":700,   # VIEWER INTERNAL : the user interface ask to retrieve an object
       "CST_FLUSH":800 ,   # VIEWER INTERNAL : Flush remaining traces to server. param : event id

       # Wintrace / WinWatch. New commands should be added before 80
       #--------------------------------------------------------------------------

       "CST_GOTO_FIRST_NODE":80,   # WinTrace.GotoFirstNode()
       "CST_GOTO_LAST_NODE":81,   # WinTrace.GotoLastNode()
       "CST_FIND_NEXT":82,   # WinTrace.FindNext(forward)
       "CST_GOTO_BOOKMARK":83,   # WinTrace.GotoBookmark(pos)
       "CST_CLEAR_BOOKMARK":84,   # WinTrace.ClearBookmark()
       "CST_CLEAR_FILTER":85,   # WinTrace.ClearFilter()
       "CST_ADD_FILTER":86,   # WinTrace.AddFilter(column,compare,text)
       "CST_APPLY_FILTER":87,   # WinTrace.ApplyFilter(ConditionAnd, ShowMatch,IncludeChildren)
       "CST_TREE_COLUMNWIDTH":93  ,   # Columns widths
       "CST_TREE_MULTI_COLUMN":95  ,   # change the tree to display multiple column
       "CST_TREE_COLUMNTITLE":96  ,   # change the columns titles
       "CST_DISPLAY_TREE":97  ,   # display tree windows
       "CST_TREE_NAME":98  ,   #  new name of the tree
       "CST_USE_TREE":99  ,   # the tree to use for other command
       "CST_CLEAR_ALL":104 ,   # Clear all nodes on the main window
       "CST_CLOSE_WIN":105,   # Close the window (wintrace or winwatch)
       "CST_WINWATCH_NAME":110 ,   # Watch Window name , param : window name
       "CST_WINWATCH_ID":111 ,   # Watch Window ID , param : Window id
       "CST_WATCH_NAME":112 ,   # watch name,  param : watch name
       "CST_SAVETOTEXT":559 ,   # Save to text file, parameter : filename
       "CST_SAVETOXML":560 ,   # Save to XML file, parameter : filename
       "CST_LOADXML":561 ,   # load an XML file to the current wintrace
       "CST_LOGFILE":562 ,   # Set log file

       # Wintrace plugins
       #--------------------------------------------------------------------------

       "CST_LINKTOPLUGIN":563 ,   # link a wintrace to a plugin
       "CST_CREATE_RESOURCE":564 ,   # create a resource on a wintrace
       "CST_SET_TEXT_RESOURCE":565 ,   # set the text resource
       "CST_DISABLE_RESOURCE":566 ,   # disable a resource

       # TTrace
       #--------------------------------------------------------------------------

       "CST_FIND_TEXT":100 ,   #TTrace.Find (text, bool Sensitive, bool WholeWord , bool highlight )
       "CST_SHOW":102 ,   # The command to bring the trace tool to front
       "CST_CLOSE_VIEWER":106,   # Close the viewer (shutdown)

       # Node
       #--------------------------------------------------------------------------

       "CST_TRACE_ID":101 ,   # the unique ID (from the server point of view) of the node (preferably a GUID)
       "CST_ICO_INDEX":103 ,   # the index of the Icon to use (CST_ICO_INFO, CST_ICO_WARNING,...)
       "CST_GOTO_NEXTSIBLING":114,   # ITraceNode.GotoNextSibling ()
       "CST_GOTO_PREVSIBLING":115,   # ITraceNode.GotoPrevSibling ()
       "CST_GOTO_FIRST_CHILD":116,   # ITraceNode.GotoFirstChild  ()
       "CST_GOTO_LAST_CHILD":117,   # ITraceNode.GotoLastChild   ()
       "CST_SET_BOOKMARK":122,   # ITraceNode.SetBookmark (bool enabled)
       "CST_VISIBLE_NODE":123,   # ITraceNode.SetVisible  (visible)
       "CST_CLEAR_NODE":300 ,   # Delete a node on the viewer
       "CST_CLEAR_SUBNODES":301 ,   # Clear children nodes on the viewer
       "CST_THREAD_ID":302 ,   # The Thread ID of the sender thread (optional) Used when tracing multiple thread
       "CST_PROCESS_NAME":303 ,   # The process name (optional) . Used when tracing multiple process
       "CST_MESSAGE_TIME":304 ,   # The time of trace
       "CST_THREAD_NAME":305 ,   # Thread name
       "CST_IP":306,   # Client Ip adress
       "CST_NEW_NODE":550 ,   # Command to create a new trace node
       "CST_LEFT_MSG":551 ,   # The left message ("traces column")
       "CST_RIGHT_MSG":552 ,   # the right message ("Comment column")
       "CST_SELECT_NODE":553 ,   # set the node as 'Selected' by the user
       "CST_USE_NODE":555 ,   # use an existing node
       "CST_APPEND_LEFT_MSG":556 ,   # The left message to append to "traces column"
       "CST_APPEND_RIGHT_MSG":557 ,   # The right message to append to "Comment column"
       "CST_FOCUS_NODE":558 ,   # Focus to the node
       "CST_FONT_DETAIL":567 ,   # Font detail
       "CST_BACKGROUND_COLOR":568,   # Background color

       # Members
       #--------------------------------------------------------------------------

       "CST_CREATE_MEMBER":500 ,   # Command to create a member for the current trace node
       "CST_MEMBER_FONT_DETAIL":501,   # Member Font detail
       "CST_MEMBER_COL2":502 ,   # The text of the second member column
       "CST_MEMBER_VIEWER_KIND":503,   # Viewer kind id
       "CST_MEMBER_COL3":504 ,   # The text of the third member column
       "CST_ADD_MEMBER":505    # Add the member. Close the previous CST_CREATE_MEMBER
    }

    socket = None                           # socket for viewer connection
    socketConnected = False                 # socket is connected flag
    queue = Queue.Queue()                   # queue for thread communication
    flushEvent = threading.Semaphore(0)     # flush semaphore
    pyVersion = version_info[0]             # python version (2 or 3)
    pyPLatform = platform.system()          # platform : "cli" for dot net


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

class TTrace(object):
    """ TTrace is the entry point for all traces.

    TTrace give 3 'TraceNode' doors : Warning , Error and Debug.

    Theses 3 doors are displayed with a special icon (all of them have the 'enabled' property set to true

    The class is fully static. you can the ttrace var in place of TTrace

    Sample use :
      >>> TTrace.debug.send ("hello", "world")
      >>> ttrace.warning.send ("hello").send ("world")

    """

    # preinitialize TTrace class properties for code insign and epydoc documentation
    # some properties are set to None, because the TTrace is still not defined
    # final initialization will be done just after the TTrace definition
    options = TTraceOptions()
    """ tracetool options . See TTraceOptions class"""

    winTrace = None     # WinTrace("_")
    """ Main Wintrace """

    watches = None      # WinWatch()
    """ Main WinWatch """

    debug = None        # TTrace.winTrace.debug
    """debug, warning, error are the 3 normal entry points """

    warning = None      # TTrace.winTrace.warning
    """debug, warning, error are the 3 normal entry points """

    error = None        # TTrace.winTrace.error
    """debug, warning, error are the 3 normal entry points """

    # RGB colors
    RgbColors = {
      "Black"  : (00,00,00),
      "Maroon" : (128,00,00),
      "Green"  : (00,128,00),
      "Olive"  : (128,128,00),
      "Navy"   : (00,00,128),
      "Purple" : (128,00,128),
      "Teal"   : (00,128,128),
      "Gray"   : (128,128,128),
      "Silver" : (192,192,192),
      "Red"    : (255,00,00),
      "Lime"   : (00,255,00),
      "Yellow" : (255,255,00),
      "Blue"   : (00,00,255),
      "Aqua"   : (00,255,255),
      "LtGray" : (192,192,192),
      "DkGray" : (128,128,128),
      "White"  : (255,255,255),
      "Cream"  : (255,251,240),
      "SkyBlue" : (166,202,240),
      "MedGray" : (160,160,164),
      "Fuchsia" : (255,00,255),
      "MoneyGreen" : (192,220,192)
    }
    """ Some RGB colors you can use to set trace color """

    #-----------------------------------------------------------------------------

    @staticmethod
    def show(isVisible=True):
        """ Show or hide the trace program
        @param isVisible : True to show the viewer. False to hide
        """
        commandList = []
        if isVisible:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_SHOW"], "1"))
        else:
            commandList.append("%5d%s" % (_Internals.TraceConst["CST_SHOW"], "0"))
        TTrace.Utility.sendToClient(commandList)

    #-----------------------------------------------------------------------------

    @staticmethod
    def find (Text, Sensitive=True, WholeWord=False , Highlight=False, SearchInAllPages=False):
        """ Set the global search criteria. You must call TTrace.winTrace.findNext() to position to the next or previous matching node
        @param Text : Text to search
        @param Sensitive : Search is case sensitive
        @param WholeWord : Match only whole word
        @param Highlight : Highlight results
        @param SearchInAllPages : Call to FindNext will search also in other traces windows if true
        """
        commandList = []
        flags = 0
        # Sensitive<<3+WholeWord<<2+highlight<<1+SearchInAllPages
        if Sensitive:
            flags += 8
        if WholeWord:
            flags += 4
        if Highlight:
            flags += 2
        if SearchInAllPages:
            flags += 1

        commandList.append("%5d%11d%s" % (_Internals.TraceConst["CST_FIND_TEXT"], flags, Text))
        TTrace.Utility.sendToClient(commandList)

    #-----------------------------------------------------------------------------

    @staticmethod
    def closeViewer():
        """Close the viewer (don't confuse with show(false)"""
        commandList = []
        commandList.append("%5d" % (_Internals.TraceConst["CST_CLOSE_VIEWER"]))
        TTrace.Utility.sendToClient(commandList)

    #-----------------------------------------------------------------------------

    @staticmethod
    def clearAll():
        """Clear all traces"""
        TTrace.winTrace.clearAll()

    #-----------------------------------------------------------------------------

    @staticmethod
    def stop():
        """Stop sub-system before leaving your program. You may call ttrace.flush() before stop()"""

        # if socket is already none then stop was already called.
        if _Internals.socket == None :
            return

        if TTrace.options.useThread:
            _Internals.queue.put("QUIT")    # put the message as plain text in the queue (python 3 or not)
        else:  # no thread. Just put the socket in the garbage collector to close it
            _Internals.socket = None   # socket is automatically closed
            _Internals.socketConnected = False

    #-----------------------------------------------------------------------------

    @staticmethod
    def flush():
        """Flush remaining traces to the viewer """

        # if stoped or if we don't use thread, do nothing
        if _Internals.socket == None or TTrace.options.useThread == False:
            return

        #print ("before put FLUSH to queue : flushEvent value :",_Internals.flushEvent._Semaphore__value)
        _Internals.queue.put("FLUSH") # put the message as plain text in the queue (python 3 or not)
        _Internals.flushEvent.acquire(1) # wait for thread to call release()

    #-----------------------------------------------------------------------------
    #-----------------------------------------------------------------------------

    class Utility(object):   # TTrace.Utility()....
        """Utility class """

        # private : return tuples for getObjectInfo
        fctSpec = namedtuple('_fctSpec', 'memberType, memberValue, isFunction, returnType, name, doc')

        @staticmethod
        def getObjectInfo(objToSend):   # TTrace.Utility().getObjectInfo(objToSend)
            """ helper function : return a named tuple (_fctSpec) that describe the object
            @param objToSend : Object to get info
            """

            # Same as str() for list but don't add [] charaters
            def listToString(collection):
                result = ""
                for item in collection:
                    if result == "":
                        result = item
                    else:
                        result += "," + str(item)
                return result

            try:
                objClass = type(objToSend)
                try:
                    memberType = str(objClass)
                except BaseException as e:
                    memberType = '?'

                try:
                    memberValue = str(objToSend)
                except BaseException as e:
                    memberValue = '?'
                isFunction = False

                # return type
                #-----------------------------
                returnType = ""
                if hasattr(objToSend, '__self__'):
                    if objToSend.__self__ is not None:
                        if hasattr(objToSend.__self__, "__name__"):
                            returnType = objToSend.__self__.__name__ + ':'
                        elif hasattr(objToSend.__self__, ".__class__"):
                            returnType = objToSend.__self__.__class__.__name__ + ':'
                elif hasattr(objToSend, '__objclass__'):
                    if objToSend.__objclass__ is not None:
                        if hasattr(objToSend.__objclass__, "__name__"):
                            returnType = objToSend.__objclass__.__name__ + ':'
                        elif hasattr(objToSend.__objclass__, ".__class__"):
                            returnType = objToSend.__objclass__.__class__.__name__ + ':'
                elif hasattr(objToSend, '__class__') :
                    if hasattr(objToSend.__class__, "__name__"):
                        returnType = objToSend.__class__.__name__ + ':'


                # member name
                #-----------------------------
                memberName = ""
                if hasattr(objToSend, "__name__"):
                    memberName = objToSend.__name__

                if objToSend is None:
                    memberValue = "None"

                # functions
                #----------------------
                elif (memberType == "<type 'method-wrapper'>" or
                     memberType =="<type 'method_descriptor'>" or
                     memberType == "<type 'wrapper_descriptor'>"):
                    memberType = "method wrapper"
                    isFunction = True

                elif memberType == "<type 'classmethod'>":  # <type 'classmethod'>
                    memberType = "class method"
                    isFunction = True

                elif objClass is types.FunctionType:                                    # <type 'function'>
                    # note that @staticmethod are detected as simple function
                    memberType = "Function"
                    isFunction = True

                elif objClass is types.LambdaType: # <type 'function'>
                    isFunction = True
                    memberType = 'lamda function'

                elif objClass is types.MethodType: # <type 'instancemethod'>
                    ObjSelf = objToSend.__self__
                    isFunction = True

                    if type(ObjSelf) is type:
                        memberType = "@classmethod"
                    else:
                        memberType = "instance method"
                        returnType = str(ObjSelf.__class__.__name__)

                elif objClass is types.BuiltinFunctionType: #<type 'builtin_function_or_method'>
                    isFunction = True
                    memberType = "builtin function or method"

                elif objClass is types.BuiltinMethodType: #<type 'builtin_function_or_method'>
                    isFunction = True
                    memberType = "Builtin Method"

                #elif objClass is types.UnboundMethodType:  # <type 'instancemethod'>
                #    isFunction = True
                #    memberType = 'instancemethod'

                #list, tuple, set, dict content don't have to be displayed here. Just display the count
                #------------------------------------------------------------------------------------

                elif objClass is tuple:                                                 # <type 'tuple'>
                    memberType = "Tuple"
                    memberValue = "count=%d" %(len(objToSend))

                elif objClass is list:                                                  # <type 'list'>
                    memberType = "List"
                    memberValue = "count=%d" %(len(objToSend))

                elif isinstance(objToSend, set):                                        # <type 'set'>
                    memberType = "set"
                    memberValue = "count=%d" %(len(objToSend))

                elif isinstance(objToSend, frozenset):                                  # <type 'frozenset'>
                    memberType = "set"
                    memberValue = "count=%d" %(len(objToSend))

                elif objClass is dict:                                                  # <type 'dict'>
                    memberType = "Dictionary"
                    memberValue = "count=%d" %(len(objToSend))

                elif objClass is type(type.__dict__):                                   # <type 'dictproxy'>
                    memberType = 'dictproxy'
                    memberValue = "count=%d" %(len(objToSend))

                # numbers
                elif objClass is int: memberType = 'int'                                   # <type 'int'>
                elif memberType == "<type 'long'>": memberType = 'long'                    # <type 'long'>  Converted to 'int' in python 3.*
                elif objClass is float: memberType = 'float'                               # <type 'float'>
                elif objClass is bool: memberType = 'bool'                                 # <type 'bool'>
                elif objClass is complex: memberType = 'complex'                           # <type 'complex'>

                #strings
                elif objClass is str: memberType = 'str'                                   # <type 'str'>
                elif memberType == "<type 'unicode'>": memberType = 'unicode'              # <type 'unicode'>
                #elif objClass is types.StringTypes: memberType = 'str, unicode'           # (<type 'str'>, <type 'unicode'>)
                elif memberType == "<type 'buffer'>" : memberType = 'buffer'               # <type 'buffer'>
                elif memberType == "<type 'memoryview'>" : memberType = 'buffer'           # <type 'memoryview'>

                #objectsn
                elif objClass is type: memberType = 'classobj'                             # <type 'classobj'>
                elif objClass is types.ModuleType: memberType = 'module'                   # <type 'module'>
                #elif objClass is types.InstanceType: memberType = 'instance'               # <type 'instance'>
                #elif objClass is types.NotImplementedType: memberType = 'NotImplementedType'   # <type 'NotImplementedType'>

                #others
                elif objClass is types.CodeType: memberType = 'code'                       # <type 'code'>
                elif objClass is types.GeneratorType: memberType = 'generator'             # <type 'generator'>
                #elif objClass is types.FileType: memberType = 'file'                      # <type 'file'>
                elif memberType == "<type 'xrange'>": memberType = 'xrange'                # <type 'xrange'>
                #elif objClass is types.TypeError: memberType = ''                         # ???
                #elif objClass is types.AttributeError: memberType = ''                    # ???
                elif objClass is types.TracebackType: memberType = 'traceback'             # <type 'traceback'>
                elif objClass is types.FrameType: memberType = 'frame'                     # <type 'frame'>
                elif objClass is slice: memberType = 'slice'                               # <type 'slice'> slice([start,] stop[, step])
                elif objClass is type(Ellipsis): memberType = 'ellipsis'                   # <type 'ellipsis'>
                elif objClass is types.GetSetDescriptorType: memberType = 'getset_descriptor' # <type 'getset_descriptor'>
                elif objClass is types.MemberDescriptorType: memberType = 'member_descriptor' # <type 'member_descriptor'>

                if isFunction == True:
                    #print ("ismethoddescriptor" , inspect.ismethoddescriptor(objToSend))
                    #print ("isdatadescriptor" , inspect.isdatadescriptor(objToSend))
                    #print ("ismethod" ,inspect.ismethod (objToSend))
                    #print ("isfunction" , inspect.isfunction(objToSend))

                    #im_self             instance
                    #im_class            type where the method
                    #im_func             function impementing the method (func_name, ...)

                    #func_name           str: sample
                    #func_dict           dict: {}
                    #func_defaults       tuple: (5,)
                    #func_doc            str:  doc string for sample
                    #func_closure        NoneType: None
                    #func_globals        dict: {...........}
                    #func_code
                    #    co_argcount     int: 2
                    #    co_cellvars     tuple: ()
                    #    co_code         str:...
                    #    co_consts       tuple: (None, 1, 'hello')
                    #    co_filename     str: C:\\TraceTool\\Python\\Workspace\\tracetool\\src\\tracetool.py
                    #    co_firstlineno  int: 3061
                    #    co_flags        int: 67
                    #    co_freevars     tuple: ()
                    #    co_lnotab       str:
                    #    co_name         str: sample
                    #    co_names        tuple: ('_lastUuid',)
                    #    co_nlocals      int: 5
                    #    co_stacksize    int: 2
                    #    co_varnames     tuple: ('a', 'b', 'ret1', 'ret2', 'ret3') -> co_argcount = 2
                    if hasattr(objToSend, 'func_code'):
                        info = inspect.getargs(objToSend.func_code)
                        if hasattr(objToSend, 'func_name'):
                            memberValue = objToSend.func_name + "(" + listToString(info.args) + ")"
                        else:
                            memberValue = memberName + "(" + listToString(info.args) + ")"
                    else:
                        memberValue = memberName + "()"
                # get the Doc. Note that it's not possible to get documentation for properties,
                # because the doc come from the result. Result is just stored in a list !!!
                #-------------
                if TTrace.options.sendDoc == True and hasattr(objToSend, '__doc__') and objToSend.__doc__ is not None :
                    try:
                        #if isinstance(objToSend.__doc__,str):
                        Doc = str(objToSend.__doc__)
                        #else:
                        #    Doc = "???"
                    except BaseException as e:
                        Doc = "??? " + str(e)
                    else:
                        Doc = "????"

                else:
                    Doc = ""

                return TTrace.Utility.fctSpec(memberType,memberValue,isFunction,returnType,memberName,Doc)

            except BaseException as e:
                tr =   inspect.trace() [0]
                print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , str(e))
            else:
                tr =   inspect.trace() [0]
                print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , str(e))


        #-----------------------------------------------------------------------------

        @staticmethod
        def rgbToBgr(color):    # TTrace.Utility().rgbToBgr(color)
            """ Helper function : convert RGB tuple to bgr integer
            @param color : color to convert to bgr (integer)
            """
            if isinstance(color, int): # already an integer
                return color
            elif color == None:
                return -1
            else:                                # else an array or tuple of 3 intger (RGB)
                return (color[2] << 16) + (color[1]  << 8) + (color[0]   << 0)   # B=2 G=1 R=0

        #-----------------------------------------------------------------------------

        @staticmethod
        def currentTime():   # TTrace.Utility().currentTime()
            """ Return current time with miliseconds"""
            dt = datetime.now()
            return dt.strftime("%H:%M:%S") + ":%03d" % (dt.microsecond / 1000)

        #-----------------------------------------------------------------------------

        @staticmethod
        def uuid():  # TTrace.Utility().uuid()
            """ Create unique id. Note that the "import uuid" give error on blender/windows."""
            if hasattr(TTrace, "_lastUuid"):
                uid = getattr(TTrace, "_lastUuid")
                setattr(TTrace, "_lastUuid",uid  + 1)
            else:
                uid = 1
                setattr(TTrace, "_lastUuid", 2)
            return str(uid) + str(random.randrange(0, 9999))

        #-----------------------------------------------------------------------------

        @staticmethod
        def sendToWinTraceClient(commandList, winTraceId, dateTime = None, threadName = None):   # TTrace.Utility().sendToWinTraceClient(...)
            """ Send the ArrayList to the viewer (using thread)
            @param commandList : The message list to send
            @param winTraceId : the receiver window
            @param dateTime : message time
            @param threadName : Thread name
            """
            # insert thread id
            if TTrace.options.sendThreadName == True:
                if (threadName is None):
                    commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_THREAD_NAME"] , threading.current_thread().name))
                else:
                    commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_THREAD_NAME"] , threadName))

            # add current time
            if (dateTime is None):
                commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_MESSAGE_TIME"] , TTrace.Utility.currentTime()))
            else:
                commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_MESSAGE_TIME"] , dateTime))

            # CST_USE_TREE MUST be inserted at the first position
            if (winTraceId != None and winTraceId != ""):
                commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_USE_TREE"] , winTraceId))

            TTrace.Utility.sendToClient(commandList)

        #-----------------------------------------------------------------------------

        @staticmethod

        def sendToWinWatchClient(commandList, winWatch, dateTime = None, threadName = None): # TTrace.Utility().sendToWinWatchClient(...)
            """ Send the ArrayList to the viewer (using thread)
            @param commandList : The message list to send
            @param winWatch : the receiver window
            @param dateTime : message time
            @param threadName : Thread name
            """

            # insert thread id
            if TTrace.options.sendThreadName == True:
                if (threadName is None):
                    commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_THREAD_NAME"] , threading.current_thread().name))
                else:
                    commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_THREAD_NAME"] , threadName))

            # add current time
            if (dateTime is None):
                commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_MESSAGE_TIME"] , TTrace.Utility.currentTime()))
            else:
                commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_MESSAGE_TIME"] , dateTime))

            # CST_WINWATCH_ID MUST be inserted at the first position
            #if (winWatch != None and winWatch.id != None and winWatch.id != ""):
            commandList.insert(0,"%5d%s" % (_Internals.TraceConst["CST_WINWATCH_ID"] , winWatch.id))

            TTrace.Utility.sendToClient(commandList)

        #-----------------------------------------------------------------------------

        @staticmethod
        def sendToClient (commands):   # TTrace.Utility().sendToClient (commands)
            """ Send commands from queue to destination
            @param commands : array of commands to send
            """

            if _Internals.pyVersion <= 2:
                cmdIndex = 0
                for cmd in commands:
                    #Check if the command don't contains bad chars.
                    try:
                        buf = bytearray(cmd,'utf-8' )
                    except BaseException as e:
                        #tr =   inspect.trace() [0]
                        #print ( "convertion exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

                        # replace all bad chars in the current commands by the '?' char
                        newStr = ""
                        errorCount = 0
                        #cpt = 0
                        #out = ""
                        for oneByte in cmd:
                            valChar = ord(oneByte)
                            #out += "%d:%X " % (cpt, valChar)
                            if valChar < 128:
                                newStr += oneByte
                            else:
                                newStr += '?'
                                errorCount += 1
                                if errorCount >= 5:   # stop after 5 bad chars
                                    break
                            #cpt += 1
                        #print ("dump : ",out)
                        #print ("new string : " , newStr)

                        commands[cmdIndex] = newStr
                    cmdIndex += 1

                msg = "\x00".join(commands)
                msgLen = str(len(msg)+2)  # add 2 for the last message terminator and the final terminator

                packed = "\x00".join([msgLen, msg, "\x00"])

                #print ("python version : " , version_info)
                #print ("Platform.system(): ", platform.system() )    # 'Windows' or 'cli'
                #print ("sys.version : " , sys.version)               # CPython

                if _Internals.pyPLatform == "cli":
                    buf = packed
                else:
                    buf = bytearray(packed,'utf-8' )  # convert all commands to utf8 (without error)

            else:  # python 3 or more : byte (123) + dword(len) + messages + byte(0)
                # join all command using the "\x00" separator then add a "\x00" for the last line and a final "\x00"
                msg = "\x00".join(commands) + "\x00" + "\x00"
                utf16 = bytearray (msg,'utf-16')   # create an unicode string , starting with the FF FE UTF8 signature
                msgLen = len(utf16)
                #print("msgLen :", msgLen)

                buf = bytearray ()
                # the WMD byte ensure the message is valid.
                buf.append (_Internals.TraceConst["WMD"])

                # append size
                buf.append (msgLen & 0xFF)
                buf.append ((msgLen >> 8 ) & 0xFF)
                buf.append ((msgLen >> 16) & 0xFF)
                buf.append ((msgLen >> 24) & 0xFF)

                buf += utf16

            try:
                if _Internals.socket == None:
                    _Internals.socket = socket(AF_INET, SOCK_STREAM)
                    if TTrace.options.useThread:
                        th= _SocketThread()
                        th.start()


                if TTrace.options.useThread:
                    #print ("before put to queue")
                    _Internals.queue.put(buf)
                else:
                    if _Internals.socketConnected == False:
                        host = TTrace.options.socketHost
                        port = TTrace.options.socketPort
                        _Internals.socket.connect((host,port))
                    _Internals.socketConnected = True
                    _Internals.socket.send(buf)

            except BaseException as e:
                tr =   inspect.trace() [0]
                print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

    #-----------------------------------------------------------------------------

class _SocketThread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)

    def run(self):
        #print ("thread started")
        while True:
            try:
                buf = _Internals.queue.get(timeout=3)
                #print ("after wait")
                if buf == "QUIT":    # the "QUIT" is always send as plain text, not bytearray (Python3)
                    #print (buf)
                    _Internals.socket = None # socket is automatically closed
                    _Internals.socketConnected = False
                    return
                if buf == "FLUSH":   # the "FLUSH" is always send as plain text, not bytearray (Python3)
                    #print ("before release : flushEvent value :",_Internals.flushEvent._Semaphore__value)
                    #sleep(2)
                    _Internals.flushEvent.release() # release waiting thread
                    continue

                try:
                    if _Internals.socketConnected == False:
                        host = TTrace.options.socketHost
                        port = TTrace.options.socketPort
                        _Internals.socket.connect((host,port))
                    _Internals.socketConnected = True
                except BaseException as e:
                    # message is lost if no connection can be established
                    tr =   inspect.trace() [0]
                    print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)

                try:
                    _Internals.socket.send(buf)
                except BaseException as e:
                    # message is lost if no connection can be established
                    tr =   inspect.trace() [0]
                    print ( "exception : " + str(tr[3]) + "(" + str(tr[2]) + ")" , e)


                _Internals.queue.task_done()
            except Queue.Empty as e:
                # get the main thread to determine if the thread must close
                for th in threading.enumerate():
                    if th.name=="MainThread":
                        if not th.isAlive():
                            #print ("shutdown")
                            _Internals.socket = None  # socket is automatically closed
                            _Internals.socketConnected = False
                            return
                continue


#-----------------------------------------------------------------------------

# finalize Tracetool initialisation

TTrace.winTrace = WinTrace("_")
TTrace.watches = WinWatch()
TTrace.debug = TTrace.winTrace.debug
TTrace.warning = TTrace.winTrace.warning
TTrace.error = TTrace.winTrace.error

ttrace = TTrace  #just a synonym for faster coding and python usage


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

if __name__ == '__main__':

    # Some sample call to the tracetool library. See demo.py for full samples

    # from tracetool import ttrace
    ttrace.clearAll()
    ttrace.debug.send("Hello", "Python")                                        # basic traces
    ttrace.debug.sendObject("sendObject globals()",globals())                   # sendObject example
    ttrace.warning.sendValue("sendValue globals()",globals(), maxLevel = 3)     # sendValue example
    ttrace.error.sendTable("sendTable globals()",globals())                     # sendTable example

    #str2 = u' world'
    #buf2 = bytearray(str2, 'utf-16')
    #ttrace.debug.sendDump("buf2", buf2)

    if _Internals.pyVersion <= 2:
        ttrace.debug.sendDump("hello", "worsdgsdgdsfgfdvhfcsdscvfghjjhgfjbjfg&��!����ld")
    else:  # some unicode characters
        ttrace.debug.sendDump("hello", "worsdgsdgdsfgfdvhfcsds" + chr(500) + chr(1000) + chr(2000) + chr(60000) +  " cvfghjjhgfjbjfg&��!����ld")

    ttrace.flush()   # ensure remaining traces are send to the viewer (optional)
    ttrace.stop()    # better to close before leaving
    pass

