import clr 
clr.AddReference("System.Drawing") 
clr.AddReference("System.Windows.Forms") 

import System.Drawing
import System.Windows.Forms

from System.Drawing import *
from System.Windows.Forms import *


class MainForm(Form):
	def __init__(self, demo):
		self.InitializeComponent()
		self.demo = demo
		#self._chkSendFunctions.Checked = True
	def InitializeComponent(self):
		self._tabControl1 = System.Windows.Forms.TabControl()
		self._tabBasic = System.Windows.Forms.TabPage()
		self._tabNodeOperation = System.Windows.Forms.TabPage()
		self._tabMultiPages = System.Windows.Forms.TabPage()
		self._tabWatches = System.Windows.Forms.TabPage()
		self._butSample = System.Windows.Forms.Button()
		self._butIndent = System.Windows.Forms.Button()
		self._butSendObject = System.Windows.Forms.Button()
		self._butSendValue = System.Windows.Forms.Button()
		self._butSendTable = System.Windows.Forms.Button()
		self._butStack = System.Windows.Forms.Button()
		self._groupBox1 = System.Windows.Forms.GroupBox()
		self._chkSendFunctions = System.Windows.Forms.CheckBox()
		self._chkSendClassInfo = System.Windows.Forms.CheckBox()
		self._chkSendPrivate = System.Windows.Forms.CheckBox()
		self._chkSendDoc = System.Windows.Forms.CheckBox()
		self._chkSendThtreadName = System.Windows.Forms.CheckBox()
		self._chkSendMessageUsingThread = System.Windows.Forms.CheckBox()
		self._label1 = System.Windows.Forms.Label()
		self._textBoxTreeDepth = System.Windows.Forms.TextBox()
		self._textBoxSocketHost = System.Windows.Forms.TextBox()
		self._label2 = System.Windows.Forms.Label()
		self._textBoxSocketPort = System.Windows.Forms.TextBox()
		self._label3 = System.Windows.Forms.Label()
		self._panel1 = System.Windows.Forms.Panel()
		self._butShowViewer = System.Windows.Forms.Button()
		self._butCloseViewer = System.Windows.Forms.Button()
		self._butClearMain = System.Windows.Forms.Button()
		self._groupBox2 = System.Windows.Forms.GroupBox()
		self._textBox4 = System.Windows.Forms.TextBox()
		self._label4 = System.Windows.Forms.Label()
		self._textBox5 = System.Windows.Forms.TextBox()
		self._label5 = System.Windows.Forms.Label()
		self._textBox6 = System.Windows.Forms.TextBox()
		self._label6 = System.Windows.Forms.Label()
		self._checkBox7 = System.Windows.Forms.CheckBox()
		self._checkBox8 = System.Windows.Forms.CheckBox()
		self._checkBox9 = System.Windows.Forms.CheckBox()
		self._checkBox10 = System.Windows.Forms.CheckBox()
		self._checkBox11 = System.Windows.Forms.CheckBox()
		self._checkBox12 = System.Windows.Forms.CheckBox()
		self._button3 = System.Windows.Forms.Button()
		self._button4 = System.Windows.Forms.Button()
		self._button5 = System.Windows.Forms.Button()
		self._button6 = System.Windows.Forms.Button()
		self._butCreateWindow = System.Windows.Forms.Button()
		self._butWinDisplay = System.Windows.Forms.Button()
		self._butWinSayHello = System.Windows.Forms.Button()
		self._butWinSetLogFile = System.Windows.Forms.Button()
		self._butWinSaveToText = System.Windows.Forms.Button()
		self._butWinSaveToXml = System.Windows.Forms.Button()
		self._butWinLoadXml = System.Windows.Forms.Button()
		self._butMultiColTest = System.Windows.Forms.Button()
		self._butWinClear = System.Windows.Forms.Button()
		self._butWinClose = System.Windows.Forms.Button()
		self._label7 = System.Windows.Forms.Label()
		self._butMainSendWatch = System.Windows.Forms.Button()
		self._butMainClearWatches = System.Windows.Forms.Button()
		self._butMainDisplayWatches = System.Windows.Forms.Button()
		self._butCreateNewWinWatches = System.Windows.Forms.Button()
		self._butWinWatchSend = System.Windows.Forms.Button()
		self._butWinWatchClear = System.Windows.Forms.Button()
		self._butWinWatchDisplay = System.Windows.Forms.Button()
		self._butWinWatchClose = System.Windows.Forms.Button()
		self._label8 = System.Windows.Forms.Label()
		self._butStart1 = System.Windows.Forms.Button()
		self._butResendRight = System.Windows.Forms.Button()
		self._butSetSelected = System.Windows.Forms.Button()
		self._butStart2 = System.Windows.Forms.Button()
		self._butAppendLeft = System.Windows.Forms.Button()
		self._butnodeShow = System.Windows.Forms.Button()
		self._butToggleBookmark = System.Windows.Forms.Button()
		self._butToggleVisible = System.Windows.Forms.Button()
		self._tabControl1.SuspendLayout()
		self._tabBasic.SuspendLayout()
		self._tabNodeOperation.SuspendLayout()
		self._tabMultiPages.SuspendLayout()
		self._tabWatches.SuspendLayout()
		self._groupBox1.SuspendLayout()
		self._panel1.SuspendLayout()
		self._groupBox2.SuspendLayout()
		self.SuspendLayout()
		# 
		# tabControl1
		# 
		self._tabControl1.Controls.Add(self._tabBasic)
		self._tabControl1.Controls.Add(self._tabMultiPages)
		self._tabControl1.Controls.Add(self._tabWatches)
		self._tabControl1.Controls.Add(self._tabNodeOperation)
		self._tabControl1.Dock = System.Windows.Forms.DockStyle.Fill
		self._tabControl1.Location = System.Drawing.Point(0, 0)
		self._tabControl1.Name = "tabControl1"
		self._tabControl1.SelectedIndex = 0
		self._tabControl1.Size = System.Drawing.Size(585, 428)
		self._tabControl1.TabIndex = 0
		# 
		# tabBasic
		# 
		self._tabBasic.Controls.Add(self._groupBox1)
		self._tabBasic.Controls.Add(self._butStack)
		self._tabBasic.Controls.Add(self._butSendTable)
		self._tabBasic.Controls.Add(self._butSendValue)
		self._tabBasic.Controls.Add(self._butSendObject)
		self._tabBasic.Controls.Add(self._butIndent)
		self._tabBasic.Controls.Add(self._butSample)
		self._tabBasic.Location = System.Drawing.Point(4, 22)
		self._tabBasic.Name = "tabBasic"
		self._tabBasic.Padding = System.Windows.Forms.Padding(3)
		self._tabBasic.Size = System.Drawing.Size(577, 402)
		self._tabBasic.TabIndex = 0
		self._tabBasic.Text = "Basic"
		self._tabBasic.UseVisualStyleBackColor = True
		# 
		# tabNodeOperation
		# 
		self._tabNodeOperation.Controls.Add(self._butToggleVisible)
		self._tabNodeOperation.Controls.Add(self._butToggleBookmark)
		self._tabNodeOperation.Controls.Add(self._butnodeShow)
		self._tabNodeOperation.Controls.Add(self._butAppendLeft)
		self._tabNodeOperation.Controls.Add(self._butStart2)
		self._tabNodeOperation.Controls.Add(self._butSetSelected)
		self._tabNodeOperation.Controls.Add(self._butResendRight)
		self._tabNodeOperation.Controls.Add(self._butStart1)
		self._tabNodeOperation.Location = System.Drawing.Point(4, 22)
		self._tabNodeOperation.Name = "tabNodeOperation"
		self._tabNodeOperation.Padding = System.Windows.Forms.Padding(3)
		self._tabNodeOperation.Size = System.Drawing.Size(577, 402)
		self._tabNodeOperation.TabIndex = 3
		self._tabNodeOperation.Text = "Nodes operations"
		self._tabNodeOperation.UseVisualStyleBackColor = True
		# 
		# tabMultiPages
		# 
		self._tabMultiPages.Controls.Add(self._butWinClose)
		self._tabMultiPages.Controls.Add(self._butWinClear)
		self._tabMultiPages.Controls.Add(self._butMultiColTest)
		self._tabMultiPages.Controls.Add(self._butWinLoadXml)
		self._tabMultiPages.Controls.Add(self._butWinSaveToXml)
		self._tabMultiPages.Controls.Add(self._butWinSaveToText)
		self._tabMultiPages.Controls.Add(self._butWinSetLogFile)
		self._tabMultiPages.Controls.Add(self._butWinSayHello)
		self._tabMultiPages.Controls.Add(self._butWinDisplay)
		self._tabMultiPages.Controls.Add(self._butCreateWindow)
		self._tabMultiPages.Location = System.Drawing.Point(4, 22)
		self._tabMultiPages.Name = "tabMultiPages"
		self._tabMultiPages.Padding = System.Windows.Forms.Padding(3)
		self._tabMultiPages.Size = System.Drawing.Size(577, 402)
		self._tabMultiPages.TabIndex = 1
		self._tabMultiPages.Text = "Multi Pages"
		self._tabMultiPages.UseVisualStyleBackColor = True
		# 
		# tabWatches
		# 
		self._tabWatches.Controls.Add(self._label8)
		self._tabWatches.Controls.Add(self._butWinWatchClose)
		self._tabWatches.Controls.Add(self._butWinWatchDisplay)
		self._tabWatches.Controls.Add(self._butWinWatchClear)
		self._tabWatches.Controls.Add(self._butWinWatchSend)
		self._tabWatches.Controls.Add(self._butCreateNewWinWatches)
		self._tabWatches.Controls.Add(self._butMainDisplayWatches)
		self._tabWatches.Controls.Add(self._butMainClearWatches)
		self._tabWatches.Controls.Add(self._butMainSendWatch)
		self._tabWatches.Controls.Add(self._label7)
		self._tabWatches.Location = System.Drawing.Point(4, 22)
		self._tabWatches.Name = "tabWatches"
		self._tabWatches.Padding = System.Windows.Forms.Padding(3)
		self._tabWatches.Size = System.Drawing.Size(577, 402)
		self._tabWatches.TabIndex = 2
		self._tabWatches.Text = "Watches"
		self._tabWatches.UseVisualStyleBackColor = True
		# 
		# butSample
		# 
		self._butSample.Location = System.Drawing.Point(6, 13)
		self._butSample.Name = "butSample"
		self._butSample.Size = System.Drawing.Size(130, 23)
		self._butSample.TabIndex = 0
		self._butSample.Text = "Sample Traces"
		self._butSample.UseVisualStyleBackColor = True
		self._butSample.Click += self.ButSampleClick
		# 
		# butIndent
		# 
		self._butIndent.Location = System.Drawing.Point(6, 51)
		self._butIndent.Name = "butIndent"
		self._butIndent.Size = System.Drawing.Size(130, 23)
		self._butIndent.TabIndex = 1
		self._butIndent.Text = "Indent / UnIndent"
		self._butIndent.UseVisualStyleBackColor = True
		self._butIndent.Click += self.ButIndentClick
		# 
		# butSendObject
		# 
		self._butSendObject.Location = System.Drawing.Point(6, 89)
		self._butSendObject.Name = "butSendObject"
		self._butSendObject.Size = System.Drawing.Size(130, 23)
		self._butSendObject.TabIndex = 2
		self._butSendObject.Text = "Send Object"
		self._butSendObject.UseVisualStyleBackColor = True
		self._butSendObject.Click += self.ButSendObjectClick
		# 
		# butSendValue
		# 
		self._butSendValue.Location = System.Drawing.Point(6, 127)
		self._butSendValue.Name = "butSendValue"
		self._butSendValue.Size = System.Drawing.Size(130, 23)
		self._butSendValue.TabIndex = 3
		self._butSendValue.Text = "Send Value"
		self._butSendValue.UseVisualStyleBackColor = True
		self._butSendValue.Click += self.ButSendValueClick
		# 
		# butSendTable
		# 
		self._butSendTable.Location = System.Drawing.Point(6, 165)
		self._butSendTable.Name = "butSendTable"
		self._butSendTable.Size = System.Drawing.Size(130, 23)
		self._butSendTable.TabIndex = 4
		self._butSendTable.Text = "Send Table"
		self._butSendTable.UseVisualStyleBackColor = True
		self._butSendTable.Click += self.ButSendTableClick
		# 
		# butStack
		# 
		self._butStack.Location = System.Drawing.Point(8, 205)
		self._butStack.Name = "butStack"
		self._butStack.Size = System.Drawing.Size(130, 23)
		self._butStack.TabIndex = 5
		self._butStack.Text = "stack"
		self._butStack.UseVisualStyleBackColor = True
		self._butStack.Click += self.ButStackClick
		# 
		# groupBox1
		# 
		self._groupBox1.Controls.Add(self._textBoxSocketPort)
		self._groupBox1.Controls.Add(self._label3)
		self._groupBox1.Controls.Add(self._textBoxSocketHost)
		self._groupBox1.Controls.Add(self._label2)
		self._groupBox1.Controls.Add(self._textBoxTreeDepth)
		self._groupBox1.Controls.Add(self._label1)
		self._groupBox1.Controls.Add(self._chkSendMessageUsingThread)
		self._groupBox1.Controls.Add(self._chkSendThtreadName)
		self._groupBox1.Controls.Add(self._chkSendDoc)
		self._groupBox1.Controls.Add(self._chkSendPrivate)
		self._groupBox1.Controls.Add(self._chkSendClassInfo)
		self._groupBox1.Controls.Add(self._chkSendFunctions)
		self._groupBox1.Location = System.Drawing.Point(181, 6)
		self._groupBox1.Name = "groupBox1"
		self._groupBox1.Size = System.Drawing.Size(309, 296)
		self._groupBox1.TabIndex = 6
		self._groupBox1.TabStop = False
		self._groupBox1.Text = "Options"
		# 
		# chkSendFunctions
		# 
		self._chkSendFunctions.Location = System.Drawing.Point(6, 19)
		self._chkSendFunctions.Name = "chkSendFunctions"
		self._chkSendFunctions.Size = System.Drawing.Size(180, 24)
		self._chkSendFunctions.TabIndex = 0
		self._chkSendFunctions.Text = "send functions"
		self._chkSendFunctions.UseVisualStyleBackColor = True
		self._chkSendFunctions.CheckedChanged += self.ChkSendFunctionsCheckedChanged
		# 
		# chkSendClassInfo
		# 
		self._chkSendClassInfo.Location = System.Drawing.Point(6, 49)
		self._chkSendClassInfo.Name = "chkSendClassInfo"
		self._chkSendClassInfo.Size = System.Drawing.Size(180, 24)
		self._chkSendClassInfo.TabIndex = 1
		self._chkSendClassInfo.Text = "send class info"
		self._chkSendClassInfo.UseVisualStyleBackColor = True
		self._chkSendClassInfo.CheckedChanged += self.ChkSendClassInfoCheckedChanged
		# 
		# chkSendPrivate
		# 
		self._chkSendPrivate.Location = System.Drawing.Point(6, 79)
		self._chkSendPrivate.Name = "chkSendPrivate"
		self._chkSendPrivate.Size = System.Drawing.Size(180, 24)
		self._chkSendPrivate.TabIndex = 2
		self._chkSendPrivate.Text = "send private"
		self._chkSendPrivate.UseVisualStyleBackColor = True
		self._chkSendPrivate.CheckedChanged += self.ChkSendPrivateCheckedChanged
		# 
		# chkSendDoc
		# 
		self._chkSendDoc.Location = System.Drawing.Point(6, 109)
		self._chkSendDoc.Name = "chkSendDoc"
		self._chkSendDoc.Size = System.Drawing.Size(155, 24)
		self._chkSendDoc.TabIndex = 3
		self._chkSendDoc.Text = "send doc"
		self._chkSendDoc.UseVisualStyleBackColor = True
		self._chkSendDoc.CheckedChanged += self.ChkSendDocCheckedChanged
		# 
		# chkSendThtreadName
		# 
		self._chkSendThtreadName.Location = System.Drawing.Point(6, 139)
		self._chkSendThtreadName.Name = "chkSendThtreadName"
		self._chkSendThtreadName.Size = System.Drawing.Size(180, 24)
		self._chkSendThtreadName.TabIndex = 4
		self._chkSendThtreadName.Text = "send thread name"
		self._chkSendThtreadName.UseVisualStyleBackColor = True
		self._chkSendThtreadName.CheckedChanged += self.ChkSendThtreadNameCheckedChanged
		# 
		# chkSendMessageUsingThread
		# 
		self._chkSendMessageUsingThread.Location = System.Drawing.Point(6, 169)
		self._chkSendMessageUsingThread.Name = "chkSendMessageUsingThread"
		self._chkSendMessageUsingThread.Size = System.Drawing.Size(227, 24)
		self._chkSendMessageUsingThread.TabIndex = 5
		self._chkSendMessageUsingThread.Text = "send messages using a thread"
		self._chkSendMessageUsingThread.UseVisualStyleBackColor = True
		self._chkSendMessageUsingThread.CheckedChanged += self.ChkSendMessageUsingThreadCheckedChanged
		# 
		# label1
		# 
		self._label1.Location = System.Drawing.Point(7, 201)
		self._label1.Name = "label1"
		self._label1.Size = System.Drawing.Size(95, 23)
		self._label1.TabIndex = 6
		self._label1.Text = "object tree depth"
		# 
		# textBoxTreeDepth
		# 
		self._textBoxTreeDepth.Location = System.Drawing.Point(108, 201)
		self._textBoxTreeDepth.Name = "textBoxTreeDepth"
		self._textBoxTreeDepth.Size = System.Drawing.Size(178, 20)
		self._textBoxTreeDepth.TabIndex = 7
		self._textBoxTreeDepth.TextChanged += self.TextBoxTreeDepthTextChanged
		# 
		# textBoxSocketHost
		# 
		self._textBoxSocketHost.Location = System.Drawing.Point(108, 227)
		self._textBoxSocketHost.Name = "textBoxSocketHost"
		self._textBoxSocketHost.Size = System.Drawing.Size(178, 20)
		self._textBoxSocketHost.TabIndex = 9
		self._textBoxSocketHost.TextChanged += self.TextBoxSocketHostTextChanged
		# 
		# label2
		# 
		self._label2.Location = System.Drawing.Point(7, 227)
		self._label2.Name = "label2"
		self._label2.Size = System.Drawing.Size(73, 23)
		self._label2.TabIndex = 8
		self._label2.Text = "socket Host"
		# 
		# textBoxSocketPort
		# 
		self._textBoxSocketPort.Location = System.Drawing.Point(108, 253)
		self._textBoxSocketPort.Name = "textBoxSocketPort"
		self._textBoxSocketPort.Size = System.Drawing.Size(178, 20)
		self._textBoxSocketPort.TabIndex = 11
		self._textBoxSocketPort.TextChanged += self.TextBoxSocketPortTextChanged
		# 
		# label3
		# 
		self._label3.Location = System.Drawing.Point(7, 253)
		self._label3.Name = "label3"
		self._label3.Size = System.Drawing.Size(73, 23)
		self._label3.TabIndex = 10
		self._label3.Text = "socket Port"
		# 
		# panel1
		# 
		self._panel1.Controls.Add(self._butClearMain)
		self._panel1.Controls.Add(self._butCloseViewer)
		self._panel1.Controls.Add(self._butShowViewer)
		self._panel1.Dock = System.Windows.Forms.DockStyle.Bottom
		self._panel1.Location = System.Drawing.Point(0, 386)
		self._panel1.Name = "panel1"
		self._panel1.Size = System.Drawing.Size(585, 42)
		self._panel1.TabIndex = 1
		# 
		# butShowViewer
		# 
		self._butShowViewer.Location = System.Drawing.Point(12, 10)
		self._butShowViewer.Name = "butShowViewer"
		self._butShowViewer.Size = System.Drawing.Size(100, 23)
		self._butShowViewer.TabIndex = 0
		self._butShowViewer.Text = "Show viewer"
		self._butShowViewer.UseVisualStyleBackColor = True
		self._butShowViewer.Click += self.ButShowViewerClick
		# 
		# butCloseViewer
		# 
		self._butCloseViewer.Location = System.Drawing.Point(118, 10)
		self._butCloseViewer.Name = "butCloseViewer"
		self._butCloseViewer.Size = System.Drawing.Size(100, 23)
		self._butCloseViewer.TabIndex = 1
		self._butCloseViewer.Text = "Close viewer"
		self._butCloseViewer.UseVisualStyleBackColor = True
		self._butCloseViewer.Click += self.ButCloseViewerClick
		# 
		# butClearMain
		# 
		self._butClearMain.Location = System.Drawing.Point(224, 10)
		self._butClearMain.Name = "butClearMain"
		self._butClearMain.Size = System.Drawing.Size(100, 23)
		self._butClearMain.TabIndex = 2
		self._butClearMain.Text = "Clear main traces"
		self._butClearMain.UseVisualStyleBackColor = True
		self._butClearMain.Click += self.ButClearMainClick
		# 
		# groupBox2
		# 
		self._groupBox2.Controls.Add(self._textBox4)
		self._groupBox2.Controls.Add(self._label4)
		self._groupBox2.Controls.Add(self._textBox5)
		self._groupBox2.Controls.Add(self._label5)
		self._groupBox2.Controls.Add(self._textBox6)
		self._groupBox2.Controls.Add(self._label6)
		self._groupBox2.Controls.Add(self._checkBox7)
		self._groupBox2.Controls.Add(self._checkBox8)
		self._groupBox2.Controls.Add(self._checkBox9)
		self._groupBox2.Controls.Add(self._checkBox10)
		self._groupBox2.Controls.Add(self._checkBox11)
		self._groupBox2.Controls.Add(self._checkBox12)
		self._groupBox2.Location = System.Drawing.Point(181, 6)
		self._groupBox2.Name = "groupBox2"
		self._groupBox2.Size = System.Drawing.Size(309, 296)
		self._groupBox2.TabIndex = 6
		self._groupBox2.TabStop = False
		self._groupBox2.Text = "Options"
		# 
		# textBox4
		# 
		self._textBox4.Location = System.Drawing.Point(108, 253)
		self._textBox4.Name = "textBox4"
		self._textBox4.Size = System.Drawing.Size(178, 20)
		self._textBox4.TabIndex = 11
		# 
		# label4
		# 
		self._label4.Location = System.Drawing.Point(7, 253)
		self._label4.Name = "label4"
		self._label4.Size = System.Drawing.Size(73, 23)
		self._label4.TabIndex = 10
		self._label4.Text = "socket Port"
		# 
		# textBox5
		# 
		self._textBox5.Location = System.Drawing.Point(108, 227)
		self._textBox5.Name = "textBox5"
		self._textBox5.Size = System.Drawing.Size(178, 20)
		self._textBox5.TabIndex = 9
		# 
		# label5
		# 
		self._label5.Location = System.Drawing.Point(7, 227)
		self._label5.Name = "label5"
		self._label5.Size = System.Drawing.Size(73, 23)
		self._label5.TabIndex = 8
		self._label5.Text = "socket Host"
		# 
		# textBox6
		# 
		self._textBox6.Location = System.Drawing.Point(108, 201)
		self._textBox6.Name = "textBox6"
		self._textBox6.Size = System.Drawing.Size(178, 20)
		self._textBox6.TabIndex = 7
		# 
		# label6
		# 
		self._label6.Location = System.Drawing.Point(7, 201)
		self._label6.Name = "label6"
		self._label6.Size = System.Drawing.Size(95, 23)
		self._label6.TabIndex = 6
		self._label6.Text = "object tree depth"
		# 
		# checkBox7
		# 
		self._checkBox7.Location = System.Drawing.Point(6, 169)
		self._checkBox7.Name = "checkBox7"
		self._checkBox7.Size = System.Drawing.Size(104, 24)
		self._checkBox7.TabIndex = 5
		self._checkBox7.Text = "checkBox7"
		self._checkBox7.UseVisualStyleBackColor = True
		# 
		# checkBox8
		# 
		self._checkBox8.Location = System.Drawing.Point(6, 139)
		self._checkBox8.Name = "checkBox8"
		self._checkBox8.Size = System.Drawing.Size(104, 24)
		self._checkBox8.TabIndex = 4
		self._checkBox8.Text = "checkBox8"
		self._checkBox8.UseVisualStyleBackColor = True
		# 
		# checkBox9
		# 
		self._checkBox9.Location = System.Drawing.Point(6, 109)
		self._checkBox9.Name = "checkBox9"
		self._checkBox9.Size = System.Drawing.Size(104, 24)
		self._checkBox9.TabIndex = 3
		self._checkBox9.Text = "checkBox9"
		self._checkBox9.UseVisualStyleBackColor = True
		# 
		# checkBox10
		# 
		self._checkBox10.Location = System.Drawing.Point(6, 79)
		self._checkBox10.Name = "checkBox10"
		self._checkBox10.Size = System.Drawing.Size(104, 24)
		self._checkBox10.TabIndex = 2
		self._checkBox10.Text = "checkBox10"
		self._checkBox10.UseVisualStyleBackColor = True
		# 
		# checkBox11
		# 
		self._checkBox11.Location = System.Drawing.Point(6, 49)
		self._checkBox11.Name = "checkBox11"
		self._checkBox11.Size = System.Drawing.Size(104, 24)
		self._checkBox11.TabIndex = 1
		self._checkBox11.Text = "checkBox11"
		self._checkBox11.UseVisualStyleBackColor = True
		# 
		# checkBox12
		# 
		self._checkBox12.Location = System.Drawing.Point(6, 19)
		self._checkBox12.Name = "checkBox12"
		self._checkBox12.Size = System.Drawing.Size(104, 24)
		self._checkBox12.TabIndex = 0
		self._checkBox12.Text = "checkBox12"
		self._checkBox12.UseVisualStyleBackColor = True
		# 
		# button3
		# 
		self._button3.Location = System.Drawing.Point(6, 127)
		self._button3.Name = "button3"
		self._button3.Size = System.Drawing.Size(130, 23)
		self._button3.TabIndex = 3
		self._button3.Text = "Send Value"
		self._button3.UseVisualStyleBackColor = True
		# 
		# button4
		# 
		self._button4.Location = System.Drawing.Point(6, 89)
		self._button4.Name = "button4"
		self._button4.Size = System.Drawing.Size(130, 23)
		self._button4.TabIndex = 2
		self._button4.Text = "Send Object"
		self._button4.UseVisualStyleBackColor = True
		# 
		# button5
		# 
		self._button5.Location = System.Drawing.Point(6, 51)
		self._button5.Name = "button5"
		self._button5.Size = System.Drawing.Size(130, 23)
		self._button5.TabIndex = 1
		self._button5.Text = "Indent / UnIndent"
		self._button5.UseVisualStyleBackColor = True
		# 
		# button6
		# 
		self._button6.Location = System.Drawing.Point(6, 13)
		self._button6.Name = "button6"
		self._button6.Size = System.Drawing.Size(130, 23)
		self._button6.TabIndex = 0
		self._button6.Text = "Sample Traces"
		self._button6.UseVisualStyleBackColor = True
		# 
		# butCreateWindow
		# 
		self._butCreateWindow.Location = System.Drawing.Point(8, 12)
		self._butCreateWindow.Name = "butCreateWindow"
		self._butCreateWindow.Size = System.Drawing.Size(150, 23)
		self._butCreateWindow.TabIndex = 0
		self._butCreateWindow.Text = "Create a new trace window"
		self._butCreateWindow.UseVisualStyleBackColor = True
		self._butCreateWindow.Click += self.ButCreateWindowClick
		# 
		# butWinDisplay
		# 
		self._butWinDisplay.Enabled = False
		self._butWinDisplay.Location = System.Drawing.Point(8, 48)
		self._butWinDisplay.Name = "butWinDisplay"
		self._butWinDisplay.Size = System.Drawing.Size(150, 23)
		self._butWinDisplay.TabIndex = 1
		self._butWinDisplay.Text = "Display that window"
		self._butWinDisplay.UseVisualStyleBackColor = True
		self._butWinDisplay.Click += self.ButWinDisplayClick
		# 
		# butWinSayHello
		# 
		self._butWinSayHello.Enabled = False
		self._butWinSayHello.Location = System.Drawing.Point(8, 84)
		self._butWinSayHello.Name = "butWinSayHello"
		self._butWinSayHello.Size = System.Drawing.Size(150, 23)
		self._butWinSayHello.TabIndex = 2
		self._butWinSayHello.Text = "Say Hello"
		self._butWinSayHello.UseVisualStyleBackColor = True
		self._butWinSayHello.Click += self.ButWinSayHelloClick
		# 
		# butWinSetLogFile
		# 
		self._butWinSetLogFile.Enabled = False
		self._butWinSetLogFile.Location = System.Drawing.Point(8, 120)
		self._butWinSetLogFile.Name = "butWinSetLogFile"
		self._butWinSetLogFile.Size = System.Drawing.Size(150, 23)
		self._butWinSetLogFile.TabIndex = 3
		self._butWinSetLogFile.Text = "Set log file"
		self._butWinSetLogFile.UseVisualStyleBackColor = True
		self._butWinSetLogFile.Click += self.ButWinSetLogFileClick
		# 
		# butWinSaveToText
		# 
		self._butWinSaveToText.Enabled = False
		self._butWinSaveToText.Location = System.Drawing.Point(8, 156)
		self._butWinSaveToText.Name = "butWinSaveToText"
		self._butWinSaveToText.Size = System.Drawing.Size(150, 23)
		self._butWinSaveToText.TabIndex = 4
		self._butWinSaveToText.Text = "Viewer : Save to text"
		self._butWinSaveToText.UseVisualStyleBackColor = True
		self._butWinSaveToText.Click += self.ButWinSaveToTextClick
		# 
		# butWinSaveToXml
		# 
		self._butWinSaveToXml.Enabled = False
		self._butWinSaveToXml.Location = System.Drawing.Point(8, 192)
		self._butWinSaveToXml.Name = "butWinSaveToXml"
		self._butWinSaveToXml.Size = System.Drawing.Size(150, 23)
		self._butWinSaveToXml.TabIndex = 5
		self._butWinSaveToXml.Text = "Viewer : Save to Xml"
		self._butWinSaveToXml.UseVisualStyleBackColor = True
		self._butWinSaveToXml.Click += self.ButWinSaveToXmlClick
		# 
		# butWinLoadXml
		# 
		self._butWinLoadXml.Enabled = False
		self._butWinLoadXml.Location = System.Drawing.Point(8, 228)
		self._butWinLoadXml.Name = "butWinLoadXml"
		self._butWinLoadXml.Size = System.Drawing.Size(150, 23)
		self._butWinLoadXml.TabIndex = 6
		self._butWinLoadXml.Text = "Viewer : Load Xml"
		self._butWinLoadXml.UseVisualStyleBackColor = True
		self._butWinLoadXml.Click += self.ButWinLoadXmlClick
		# 
		# butMultiColTest
		# 
		self._butMultiColTest.Location = System.Drawing.Point(8, 294)
		self._butMultiColTest.Name = "butMultiColTest"
		self._butMultiColTest.Size = System.Drawing.Size(150, 23)
		self._butMultiColTest.TabIndex = 7
		self._butMultiColTest.Text = "Multi column test"
		self._butMultiColTest.UseVisualStyleBackColor = True
		self._butMultiColTest.Click += self.ButMultiColTestClick
		# 
		# butWinClear
		# 
		self._butWinClear.Enabled = False
		self._butWinClear.Location = System.Drawing.Point(182, 156)
		self._butWinClear.Name = "butWinClear"
		self._butWinClear.Size = System.Drawing.Size(150, 23)
		self._butWinClear.TabIndex = 8
		self._butWinClear.Text = "Clear window"
		self._butWinClear.UseVisualStyleBackColor = True
		self._butWinClear.Click += self.ButWinClearClick
		# 
		# butWinClose
		# 
		self._butWinClose.Enabled = False
		self._butWinClose.Location = System.Drawing.Point(182, 192)
		self._butWinClose.Name = "butWinClose"
		self._butWinClose.Size = System.Drawing.Size(150, 23)
		self._butWinClose.TabIndex = 9
		self._butWinClose.Text = "Close window"
		self._butWinClose.UseVisualStyleBackColor = True
		self._butWinClose.Click += self.ButWinCloseClick
		# 
		# label7
		# 
		self._label7.Location = System.Drawing.Point(27, 28)
		self._label7.Name = "label7"
		self._label7.Size = System.Drawing.Size(150, 23)
		self._label7.TabIndex = 0
		self._label7.Text = "Main watches window"
		# 
		# butMainSendWatch
		# 
		self._butMainSendWatch.Location = System.Drawing.Point(27, 102)
		self._butMainSendWatch.Name = "butMainSendWatch"
		self._butMainSendWatch.Size = System.Drawing.Size(150, 23)
		self._butMainSendWatch.TabIndex = 1
		self._butMainSendWatch.Text = "Send watch"
		self._butMainSendWatch.UseVisualStyleBackColor = True
		self._butMainSendWatch.Click += self.ButMainSendWatchClick
		# 
		# butMainClearWatches
		# 
		self._butMainClearWatches.Location = System.Drawing.Point(26, 138)
		self._butMainClearWatches.Name = "butMainClearWatches"
		self._butMainClearWatches.Size = System.Drawing.Size(150, 23)
		self._butMainClearWatches.TabIndex = 2
		self._butMainClearWatches.Text = "Clear watches"
		self._butMainClearWatches.UseVisualStyleBackColor = True
		self._butMainClearWatches.Click += self.ButMainClearWatchesClick
		# 
		# butMainDisplayWatches
		# 
		self._butMainDisplayWatches.Location = System.Drawing.Point(26, 174)
		self._butMainDisplayWatches.Name = "butMainDisplayWatches"
		self._butMainDisplayWatches.Size = System.Drawing.Size(150, 23)
		self._butMainDisplayWatches.TabIndex = 3
		self._butMainDisplayWatches.Text = "Display window"
		self._butMainDisplayWatches.UseVisualStyleBackColor = True
		self._butMainDisplayWatches.Click += self.ButMainDisplayWatchesClick
		# 
		# butCreateNewWinWatches
		# 
		self._butCreateNewWinWatches.Location = System.Drawing.Point(245, 66)
		self._butCreateNewWinWatches.Name = "butCreateNewWinWatches"
		self._butCreateNewWinWatches.Size = System.Drawing.Size(150, 23)
		self._butCreateNewWinWatches.TabIndex = 4
		self._butCreateNewWinWatches.Text = "Create new winwatch"
		self._butCreateNewWinWatches.UseVisualStyleBackColor = True
		self._butCreateNewWinWatches.Click += self.ButCreateNewWinWatchesClick
		# 
		# butWinWatchSend
		# 
		self._butWinWatchSend.Enabled = False
		self._butWinWatchSend.Location = System.Drawing.Point(245, 102)
		self._butWinWatchSend.Name = "butWinWatchSend"
		self._butWinWatchSend.Size = System.Drawing.Size(150, 23)
		self._butWinWatchSend.TabIndex = 5
		self._butWinWatchSend.Text = "Send watch"
		self._butWinWatchSend.UseVisualStyleBackColor = True
		self._butWinWatchSend.Click += self.ButWinWatchSendClick
		# 
		# butWinWatchClear
		# 
		self._butWinWatchClear.Enabled = False
		self._butWinWatchClear.Location = System.Drawing.Point(245, 138)
		self._butWinWatchClear.Name = "butWinWatchClear"
		self._butWinWatchClear.Size = System.Drawing.Size(150, 23)
		self._butWinWatchClear.TabIndex = 6
		self._butWinWatchClear.Text = "Clear watches"
		self._butWinWatchClear.UseVisualStyleBackColor = True
		self._butWinWatchClear.Click += self.ButWinWatchClearClick
		# 
		# butWinWatchDisplay
		# 
		self._butWinWatchDisplay.Enabled = False
		self._butWinWatchDisplay.Location = System.Drawing.Point(245, 174)
		self._butWinWatchDisplay.Name = "butWinWatchDisplay"
		self._butWinWatchDisplay.Size = System.Drawing.Size(150, 23)
		self._butWinWatchDisplay.TabIndex = 7
		self._butWinWatchDisplay.Text = "Display window"
		self._butWinWatchDisplay.UseVisualStyleBackColor = True
		self._butWinWatchDisplay.Click += self.ButWinWatchDisplayClick
		# 
		# butWinWatchClose
		# 
		self._butWinWatchClose.Enabled = False
		self._butWinWatchClose.Location = System.Drawing.Point(245, 210)
		self._butWinWatchClose.Name = "butWinWatchClose"
		self._butWinWatchClose.Size = System.Drawing.Size(150, 23)
		self._butWinWatchClose.TabIndex = 8
		self._butWinWatchClose.Text = "Close window"
		self._butWinWatchClose.UseVisualStyleBackColor = True
		self._butWinWatchClose.Click += self.ButWinWatchCloseClick
		# 
		# label8
		# 
		self._label8.Location = System.Drawing.Point(245, 28)
		self._label8.Name = "label8"
		self._label8.Size = System.Drawing.Size(150, 23)
		self._label8.TabIndex = 9
		self._label8.Text = "new watches window"
		# 
		# butStart1
		# 
		self._butStart1.Location = System.Drawing.Point(10, 6)
		self._butStart1.Name = "butStart1"
		self._butStart1.Size = System.Drawing.Size(150, 23)
		self._butStart1.TabIndex = 1
		self._butStart1.Text = "Start1"
		self._butStart1.UseVisualStyleBackColor = True
		self._butStart1.Click += self.ButStart1Click
		# 
		# butResendRight
		# 
		self._butResendRight.Enabled = False
		self._butResendRight.Location = System.Drawing.Point(64, 50)
		self._butResendRight.Name = "butResendRight"
		self._butResendRight.Size = System.Drawing.Size(150, 23)
		self._butResendRight.TabIndex = 2
		self._butResendRight.Text = "resendRight"
		self._butResendRight.UseVisualStyleBackColor = True
		self._butResendRight.Click += self.ButResendRightClick
		# 
		# butSetSelected
		# 
		self._butSetSelected.Enabled = False
		self._butSetSelected.Location = System.Drawing.Point(64, 81)
		self._butSetSelected.Name = "butSetSelected"
		self._butSetSelected.Size = System.Drawing.Size(150, 23)
		self._butSetSelected.TabIndex = 3
		self._butSetSelected.Text = "setSelected"
		self._butSetSelected.UseVisualStyleBackColor = True
		self._butSetSelected.Click += self.ButSetSelectedClick
		# 
		# butStart2
		# 
		self._butStart2.Location = System.Drawing.Point(10, 133)
		self._butStart2.Name = "butStart2"
		self._butStart2.Size = System.Drawing.Size(150, 23)
		self._butStart2.TabIndex = 4
		self._butStart2.Text = "Start2"
		self._butStart2.UseVisualStyleBackColor = True
		self._butStart2.Click += self.ButStart2Click
		# 
		# butAppendLeft
		# 
		self._butAppendLeft.Enabled = False
		self._butAppendLeft.Location = System.Drawing.Point(64, 175)
		self._butAppendLeft.Name = "butAppendLeft"
		self._butAppendLeft.Size = System.Drawing.Size(150, 23)
		self._butAppendLeft.TabIndex = 5
		self._butAppendLeft.Text = "appendLeft"
		self._butAppendLeft.UseVisualStyleBackColor = True
		self._butAppendLeft.Click += self.ButAppendLeftClick
		# 
		# butnodeShow
		# 
		self._butnodeShow.Enabled = False
		self._butnodeShow.Location = System.Drawing.Point(64, 209)
		self._butnodeShow.Name = "butnodeShow"
		self._butnodeShow.Size = System.Drawing.Size(150, 23)
		self._butnodeShow.TabIndex = 6
		self._butnodeShow.Text = "show"
		self._butnodeShow.UseVisualStyleBackColor = True
		self._butnodeShow.Click += self.ButnodeShowClick
		# 
		# butToggleBookmark
		# 
		self._butToggleBookmark.Enabled = False
		self._butToggleBookmark.Location = System.Drawing.Point(64, 244)
		self._butToggleBookmark.Name = "butToggleBookmark"
		self._butToggleBookmark.Size = System.Drawing.Size(150, 23)
		self._butToggleBookmark.TabIndex = 7
		self._butToggleBookmark.Text = "toggle bookmark"
		self._butToggleBookmark.UseVisualStyleBackColor = True
		self._butToggleBookmark.Click += self.ButToggleBookmarkClick
		# 
		# butToggleVisible
		# 
		self._butToggleVisible.Enabled = False
		self._butToggleVisible.Location = System.Drawing.Point(64, 279)
		self._butToggleVisible.Name = "butToggleVisible"
		self._butToggleVisible.Size = System.Drawing.Size(150, 23)
		self._butToggleVisible.TabIndex = 8
		self._butToggleVisible.Text = "toggle visible"
		self._butToggleVisible.UseVisualStyleBackColor = True
		self._butToggleVisible.Click += self.ButToggleVisibleClick
		# 
		# MainForm
		# 
		self.ClientSize = System.Drawing.Size(585, 428)
		self.Controls.Add(self._panel1)
		self.Controls.Add(self._tabControl1)
		self.Name = "MainForm"
		self._tabControl1.ResumeLayout(False)
		self._tabBasic.ResumeLayout(False)
		self._tabNodeOperation.ResumeLayout(False)
		self._tabMultiPages.ResumeLayout(False)
		self._tabWatches.ResumeLayout(False)
		self._groupBox1.ResumeLayout(False)
		self._groupBox1.PerformLayout()
		self._panel1.ResumeLayout(False)
		self._groupBox2.ResumeLayout(False)
		self._groupBox2.PerformLayout()
		self.ResumeLayout(False)

	#--------------------------------------------------------------------------
    # don't put demo code in same file as the user interface

	def ButSampleClick(self, sender, e):
		self.demo.ButSampleClick(sender, e)

	def ButIndentClick(self, sender, e):
		self.demo.ButIndentClick(sender, e)

	def ButSendObjectClick(self, sender, e):
		self.demo.ButSendObjectClick(sender, e)

	def ButSendValueClick(self, sender, e):
		self.demo.ButSendValueClick(sender, e)

	def ButSendTableClick(self, sender, e):
		self.demo.ButSendTableClick(sender, e)

	def ButStackClick(self, sender, e):
		self.demo.ButStackClick(sender, e)

	def ChkSendFunctionsCheckedChanged(self, sender, e):
		self.demo.ChkSendFunctionsCheckedChanged(sender, e)

	def ChkSendClassInfoCheckedChanged(self, sender, e):
		self.demo.ChkSendClassInfoCheckedChanged(sender, e)

	def ChkSendPrivateCheckedChanged(self, sender, e):
		self.demo.ChkSendPrivateCheckedChanged(sender, e)

	def ChkSendDocCheckedChanged(self, sender, e):
		self.demo.ChkSendDocCheckedChanged(sender, e)

	def ChkSendThtreadNameCheckedChanged(self, sender, e):
		self.demo.ChkSendThtreadNameCheckedChanged(sender, e)

	def ChkSendMessageUsingThreadCheckedChanged(self, sender, e):
		self.demo.ChkSendMessageUsingThreadCheckedChanged(sender, e)

	def TextBoxTreeDepthTextChanged(self, sender, e):
		self.demo.TextBoxTreeDepthTextChanged(sender, e)

	def TextBoxSocketHostTextChanged(self, sender, e):
		self.demo.TextBoxSocketHostTextChanged(sender, e)

	def TextBoxSocketPortTextChanged(self, sender, e):
		self.demo.TextBoxSocketPortTextChanged(sender, e)
	
	def ButCreateWindowClick(self, sender, e):
		self.demo.ButCreateWindowClick(sender, e)

	def ButWinDisplayClick(self, sender, e):
		self.demo.ButWinDisplayClick(sender, e)

	def ButWinSayHelloClick(self, sender, e):
		self.demo.ButWinSayHelloClick(sender, e)

	def ButWinSetLogFileClick(self, sender, e):
		self.demo.ButWinSetLogFileClick(sender, e)

	def ButWinSaveToTextClick(self, sender, e):
		self.demo.ButWinSaveToTextClick(sender, e)

	def ButWinSaveToXmlClick(self, sender, e):
		self.demo.ButWinSaveToXmlClick(sender, e)

	def ButWinLoadXmlClick(self, sender, e):
		self.demo.ButWinLoadXmlClick(sender, e)

	def ButWinClearClick(self, sender, e):
		self.demo.ButWinClearClick(sender, e)

	def ButWinCloseClick(self, sender, e):
		self.demo.ButWinCloseClick(sender, e)

	def ButMultiColTestClick(self, sender, e):
		self.demo.ButMultiColTestClick(sender, e)

	def ButMainSendWatchClick(self, sender, e):
		self.demo.ButMainSendWatchClick(sender, e)

	def ButMainClearWatchesClick(self, sender, e):
		self.demo.ButMainClearWatchesClick(sender, e)

	def ButMainDisplayWatchesClick(self, sender, e):
		self.demo.ButMainDisplayWatchesClick(sender, e)

	def ButWinWatchSendClick(self, sender, e):
		self.demo.ButWinWatchSendClick(sender, e)
		
	def ButWinWatchClearClick(self, sender, e):
		self.demo.ButWinWatchClearClick(sender, e)
		
	def ButCreateNewWinWatchesClick(self, sender, e):
		self.demo.ButCreateNewWinWatchesClick(sender, e)

	def ButStart1Click(self, sender, e):
		self.demo.ButStart1Click(sender, e)

	def ButResendRightClick(self, sender, e):
		self.demo.ButResendRightClick(sender, e)

	def ButSetSelectedClick(self, sender, e):
		self.demo.ButSetSelectedClick(sender, e)

	def ButStart2Click(self, sender, e):
		self.demo.ButStart2Click(sender, e)

	def ButAppendLeftClick(self, sender, e):
		self.demo.ButAppendLeftClick(sender, e)

	def ButnodeShowClick(self, sender, e):
		self.demo.ButnodeShowClick(sender, e)

	def ButToggleBookmarkClick(self, sender, e):
		self.demo.ButToggleBookmarkClick(sender, e)

	def ButToggleVisibleClick(self, sender, e):
		self.demo.ButToggleVisibleClick(sender, e)

	def ButShowViewerClick(self, sender, e):
		self.demo.ButShowViewerClick(sender, e)

	def ButCloseViewerClick(self, sender, e):
		self.demo.ButCloseViewerClick(sender, e)

	def ButClearMainClick(self, sender, e):
		self.demo.ButClearMainClick(sender, e)
		
	def ButWinWatchDisplayClick(self, sender, e):
		self.demo.ButWinWatchDisplayClick(sender, e)
	   
	def ButWinWatchCloseClick(self, sender, e):
		self.demo.ButWinWatchCloseClick(sender, e)
	