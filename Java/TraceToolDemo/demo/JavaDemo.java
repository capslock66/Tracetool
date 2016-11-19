package demo;
/*
 * JavaDemo.java
 *

 */

import java.awt.Color;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JTextField;
import javax.swing.event.CaretListener;
import org.apache.log4j.Category;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

import tracetool.SendMode;
import tracetool.TTrace;
import tracetool.TraceDisplayFlags;
import tracetool.TraceNode;
import tracetool.TraceNodeEx;
import tracetool.TraceTable;
import tracetool.Utility;
import tracetool.WinTrace;
import tracetool.WinWatch;

//import org.apache.log4j.BasicConfigurator;
// import org.apache.log4j;

/**
 *
 * @author  tpa
 */
public class JavaDemo extends javax.swing.JFrame
{
   
   /**
    * 
    */
   private static final long serialVersionUID = 1L;

   /** Creates new form Project1 */
   public JavaDemo()
   {
      initComponents();
   }
   
   private void initComponents()//GEN-BEGIN:initComponents
   {
      this.jTabbedPane1 = new javax.swing.JTabbedPane();
      this.butTest1 = new javax.swing.JButton();
 
      this.butSearch = new javax.swing.JButton();
      this.butFilter = new javax.swing.JButton();
      this.butResetFilter = new javax.swing.JButton();
      this.butBookmark  = new javax.swing.JButton();
      this.butToogleVisible = new javax.swing.JButton() ;      
      
      this.chkSendFct = new javax.swing.JCheckBox();
      this.chkUdp = new javax.swing.JCheckBox();
      this.editTarget = new javax.swing.JTextField() ; 
      this.butTest2 = new javax.swing.JButton();
      this.butTestMultiCol = new javax.swing.JButton();
      this.butIndent = new javax.swing.JButton();
      this.butTailDemo = new javax.swing.JButton();
      this.butMainLog = new javax.swing.JButton();
      this.butNoTransport = new javax.swing.JButton();
      this.butSocketTransport = new javax.swing.JButton();
      this.butSaveMainToText = new javax.swing.JButton();
      this.butSaveMainToXml = new javax.swing.JButton();
      this.butClearMain = new javax.swing.JButton();
      this.butLoadMainXml = new javax.swing.JButton();
      this.log4jDemo = new javax.swing.JButton();
      this.tabNodesOperations = new javax.swing.JPanel();
      this.butstart1 = new javax.swing.JButton();
      this.butResendRight = new javax.swing.JButton();
      this.butSetSelected = new javax.swing.JButton();
      this.butstart2 = new javax.swing.JButton();
      this.butAppendLeft = new javax.swing.JButton();
      this.butShowNode = new javax.swing.JButton();
      this.tabMultipage = new javax.swing.JPanel();
      this.butCreateWin = new javax.swing.JButton();
      this.butDisplayWin = new javax.swing.JButton();
      this.butSayHello = new javax.swing.JButton();
      this.butSaveWinToText = new javax.swing.JButton();
      this.butSaveWinToXml = new javax.swing.JButton();
      this.butClearWin = new javax.swing.JButton();
      this.butWinLoadXml = new javax.swing.JButton();
      this.jPanel1 = new javax.swing.JPanel();
      this.butQuit = new javax.swing.JButton();
      this.butShowViewer = new javax.swing.JButton();
      this.butCloseViewer = new javax.swing.JButton();
      this.butCloseSocket = new javax.swing.JButton();
      this.tabBasic = new javax.swing.JPanel();
      
      this.tabWatches = new javax.swing.JPanel();
      this.butWatch = new javax.swing.JButton();
      this.butClearWatchWindow = new javax.swing.JButton();
      this.butDisplayWatchWindow = new javax.swing.JButton();
      this.butCreateWinWatch = new javax.swing.JButton();
      this.butWinWatchSend = new javax.swing.JButton();
      this.butWinWatchClear = new javax.swing.JButton();
      this.butWinWatchDisplay = new javax.swing.JButton();

     
      getContentPane().setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());
      
      setTitle("Java Demo");
      addWindowListener(new java.awt.event.WindowAdapter()
            {
         public void windowClosing(java.awt.event.WindowEvent evt)
         {
            exitForm(evt);
         }
            });
      
      
      
      //jTabbedPane1.setTabLayoutPolicy(javax.swing.JTabbedPane.SCROLL_TAB_LAYOUT);  // 1
      tabBasic.setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());
      
      butTest1.setText("Sample traces");
      butTest1.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butTest1ActionPerformed(evt);
         }
            });
            
      tabBasic.add(butTest1, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 45, -1, -1));

      butSearch.setText("Search a text and go to next");
      butFilter.setText("Filter");
      butResetFilter.setText("Reset Filter");

      butSearch.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSearchActionPerformed(evt);
         }
            });
      butFilter.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butFilterActionPerformed(evt);
         }
            });
      butResetFilter.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butResetFilterActionPerformed(evt);
         }
            });

      tabBasic.add(butSearch, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 290, -1, -1));
      tabBasic.add(butFilter, new org.netbeans.lib.awtextra.AbsoluteConstraints(280, 290, -1, -1));
      tabBasic.add(butResetFilter, new org.netbeans.lib.awtextra.AbsoluteConstraints(420, 290, -1, -1));      
       
      chkSendFct.setText("Default option : Show functions");
      chkSendFct.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            chkSendFctActionPerformed(evt);
         }
            });      
      tabBasic.add(chkSendFct, new org.netbeans.lib.awtextra.AbsoluteConstraints(160, 15, -1, -1));
      
      
      editTarget.setText("127.0.0.1") ;
      TTrace.options.socketHost = editTarget.getText();
      editTarget.addCaretListener(new CaretListener()
      {  
         public void caretUpdate(javax.swing.event.CaretEvent e) {
            JTextField text = (JTextField)e.getSource();
            TTrace.options.socketHost = text.getText();
         }
      }
      );
      tabBasic.add(editTarget, new org.netbeans.lib.awtextra.AbsoluteConstraints(160, 45, 200, -1));

      
      chkUdp.setText("Use UDP protocol");
      chkSendFct.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            chkUdpActionPerformed(evt);
         } 
            });      
      tabBasic.add(chkUdp, new org.netbeans.lib.awtextra.AbsoluteConstraints(160, 70, -1, -1));
      
      
      butIndent.setText("indent/unIndent");
      butIndent.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butIndentActionPerformed(evt);
         }
            });
      
      tabBasic.add(butIndent, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 105, 210, -1));
      
      
      butTest2.setText("Send many traces then Flush");
      butTest2.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butTest2ActionPerformed(evt);
         }
            });
      
      tabBasic.add(butTest2, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 105, 210, -1));

      butTailDemo.setText("Tail demo : Add lines to c:\\log.txt (open it in the viewer before)");
      butTailDemo.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butTailDemoActionPerformed(evt);
         }
            });
      
      tabBasic.add(butTailDemo, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 230, 380, -1));
      
      
      butMainLog.setText("setLogFile on client and on viewer");
      butMainLog.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butLogFile_Click(evt);
         }
            });
      
      tabBasic.add(butMainLog, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 260, 250, -1));
          
      
      butNoTransport.setText("Disable socket");
      butNoTransport.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butDisableSocket(evt);
         }
            });
      
      tabBasic.add(butNoTransport, new org.netbeans.lib.awtextra.AbsoluteConstraints(280, 260, 120, -1));
 
      butSocketTransport.setText("Enable socket");
      butSocketTransport.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butEnableSocket(evt);
         }
            });
      
      tabBasic.add(butSocketTransport, new org.netbeans.lib.awtextra.AbsoluteConstraints(420, 260, 120, -1));
     
      butSaveMainToText.setText("Save to text file (\"c:\\log.txt\")");
      butSaveMainToText.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSaveMainToTextActionPerformed(evt);
         }
            });
      
      tabBasic.add(butSaveMainToText, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 150, 210, -1));
      
      butSaveMainToXml.setText("Save To Xml file (\"c:\\log.xml\")");
      butSaveMainToXml.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSaveMainToXmlActionPerformed(evt);
         }
            });
      
      tabBasic.add(butSaveMainToXml, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 180, 210, -1));
      
      butClearMain.setText("Clear main traces");
      butClearMain.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butClearMainActionPerformed(evt);
         }
            });
      
      tabBasic.add(butClearMain, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 150, 210, -1));
      
      butLoadMainXml.setText("Load Xml file (\"c:\\log.xml\")");
      butLoadMainXml.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butLoadMainXmlActionPerformed(evt);
         }
            });
      
      tabBasic.add(butLoadMainXml, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 180, 210, -1));
      
      log4jDemo.setText("Log4J demo");
      log4jDemo.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            log4jDemoActionPerformed(evt);
         }
            });
      
      tabBasic.add(log4jDemo, new org.netbeans.lib.awtextra.AbsoluteConstraints(420, 230, 130, -1));
      
      jTabbedPane1.addTab("Basic", tabBasic);
      
      tabWatches.setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());
        
      butWatch.setText("Send Watches");
      butWatch.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            initVars() ;
            TTrace.watches().send("test2", mySet);   
            mySet.add (Utility.formatTime(new Date ().getTime ())) ;
         }
            });
      tabWatches.add(butWatch, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 30, 210, -1));

       
      butClearWatchWindow.setText("Clear Watch Window");
      butClearWatchWindow.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            TTrace.watches().clearAll() ;         
         }
            });
      tabWatches.add(butClearWatchWindow, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 80, 210, -1));

      
      butDisplayWatchWindow.setText("Display Watch Window");
      butDisplayWatchWindow.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            TTrace.watches().displayWin() ;            
         }
            });
      tabWatches.add(butDisplayWatchWindow, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 130, 210, -1));

       
      butCreateWinWatch.setText("Create new WinWatch");
      butCreateWinWatch.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            MyWinWatch = new WinWatch ("MyWinWatchID" , "My watches")  ;           
         }
            });
      tabWatches.add(butCreateWinWatch, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 30, 210, -1));

       
      butWinWatchSend.setText("Send Watches");
      butWinWatchSend.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            if (MyWinWatch != null) 
               MyWinWatch.send ( "Now", Utility.formatTime(new Date ().getTime ()) ) ;           
         }
            });
      tabWatches.add(butWinWatchSend, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 80, 210, -1));

       
      butWinWatchClear.setText("Clear Watch Window");
      butWinWatchClear.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            if (MyWinWatch != null) {
               MyWinWatch.clearAll() ;
            }
            
         }
            });
      tabWatches.add(butWinWatchClear, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 130, 210, -1));

       
      butWinWatchDisplay.setText("Display Watch Window");
      butWinWatchDisplay.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            if (MyWinWatch != null) 
               MyWinWatch.displayWin() ;
         }
            });
      tabWatches.add(butWinWatchDisplay, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 180, 210, -1));

      
      jTabbedPane1.addTab("Watches", tabWatches);
      

      
      tabNodesOperations.setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());
      
      butstart1.setText("Start 1 ...");
      butstart1.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butstart1ActionPerformed(evt);
         }
            });
      
      tabNodesOperations.add(butstart1, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 30, -1, -1));
      
      butResendRight.setText("resendRight()");
      butResendRight.setDefaultCapable(false);
      butResendRight.setEnabled(false);
      butResendRight.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butResendRightActionPerformed(evt);
         }
            });
      
      tabNodesOperations.add(butResendRight, new org.netbeans.lib.awtextra.AbsoluteConstraints(130, 30, 120, -1));
      
      butSetSelected.setText("setSelected()");
      butSetSelected.setEnabled(false);
      butSetSelected.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSetSelectedActionPerformed(evt);
         }
            });
      
      tabNodesOperations.add(butSetSelected, new org.netbeans.lib.awtextra.AbsoluteConstraints(270, 30, 120, -1));
      
      butstart2.setText("Start 2 ...");
      butstart2.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butstart2ActionPerformed(evt);
         }
            });
      
      tabNodesOperations.add(butstart2, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 80, -1, -1));
      
      butAppendLeft.setText("appendLeft()");
      butAppendLeft.setEnabled(false);
      butAppendLeft.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butAppendLeftActionPerformed(evt);
         }
            });
      
      tabNodesOperations.add(butAppendLeft, new org.netbeans.lib.awtextra.AbsoluteConstraints(130, 80, 120, -1));
      
      butShowNode.setText("Show()");
      butShowNode.setEnabled(false);
      butShowNode.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butShowNodeActionPerformed(evt);
         }
            });
      
      tabNodesOperations.add(butShowNode, new org.netbeans.lib.awtextra.AbsoluteConstraints(270, 80, 120, -1));
      
      
      butBookmark.addActionListener(new java.awt.event.ActionListener()
      {
       public void actionPerformed(java.awt.event.ActionEvent evt)
       {
          butBookmarkActionPerformed(evt);
       }
      });
      
      butToogleVisible.addActionListener(new java.awt.event.ActionListener()
      {
       public void actionPerformed(java.awt.event.ActionEvent evt)
       {
        butToogleVisibleActionPerformed(evt);
       }
      });
      
      butBookmark.setText("Toogle bookmark"); 
      butToogleVisible.setText("Toogle visible");
      butBookmark.setEnabled(false);
      butToogleVisible.setEnabled(false);
      tabNodesOperations.add(butBookmark, new org.netbeans.lib.awtextra.AbsoluteConstraints(130, 130, 140, -1));
      tabNodesOperations.add(butToogleVisible, new org.netbeans.lib.awtextra.AbsoluteConstraints(290, 130, 130, -1));
     
      jTabbedPane1.addTab("Nodes operations", tabNodesOperations);
      
      tabMultipage.setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());
      
      butCreateWin.setText("Create a new window trace ");
      butCreateWin.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butCreateWinActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butCreateWin, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 30, 240, -1));
      
      butDisplayWin.setText("Display that window on the viewer");
      butDisplayWin.setEnabled(false);
      butDisplayWin.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butDisplayWinActionPerformed(evt);
         }
            });
      
      butTestMultiCol.setText("MultiCol test");
      butTestMultiCol.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butTestMultiColActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butTestMultiCol, new org.netbeans.lib.awtextra.AbsoluteConstraints(280, 30, 230, -1));
      
      tabMultipage.add(butDisplayWin, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 70, 240, -1));
      
      butSayHello.setText("Say Hello");
      butSayHello.setEnabled(false);
      butSayHello.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSayHelloActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butSayHello, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 110, 90, -1));
      
      butSaveWinToText.setText("Save to text file (\"c:\\log2.txt\")");
      butSaveWinToText.setEnabled(false);
      butSaveWinToText.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSaveWinToTextActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butSaveWinToText, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 150, 210, -1));
      
      butSaveWinToXml.setText("Save To Xml file (\"c:\\log2.xml\")");
      butSaveWinToXml.setEnabled(false);
      butSaveWinToXml.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butSaveWinToXmlActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butSaveWinToXml, new org.netbeans.lib.awtextra.AbsoluteConstraints(20, 180, 210, -1));
      
      butClearWin.setText("Clear win traces");
      butClearWin.setEnabled(false);
      butClearWin.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butClearWinActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butClearWin, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 150, 210, -1));
      
      butWinLoadXml.setText("load Xml file (\"c:\\log2.xml\")");
      butWinLoadXml.setEnabled(false);
      butWinLoadXml.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butWinLoadXmlActionPerformed(evt);
         }
            });
      
      tabMultipage.add(butWinLoadXml, new org.netbeans.lib.awtextra.AbsoluteConstraints(260, 180, 210, -1));
      
      jTabbedPane1.addTab("Multi pages", tabMultipage);
      
      getContentPane().add(jTabbedPane1, new org.netbeans.lib.awtextra.AbsoluteConstraints(0, 0, 700, 350));
      
      jPanel1.setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());
      
      butQuit.setText("Quit");
      butQuit.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butQuitActionPerformed(evt);
         }
            });
      
      jPanel1.add(butQuit, new org.netbeans.lib.awtextra.AbsoluteConstraints(371, 0, 60, -1));
      
      butShowViewer.setText("Show Viewer");
      butShowViewer.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butShowViewerActionPerformed(evt);
         }
            });
      
      jPanel1.add(butShowViewer, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 0, -1, -1));
      
      butCloseViewer.setText("Close Viewer");
      butCloseViewer.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butCloseViewerActionPerformed(evt);
         }
            });
      
      jPanel1.add(butCloseViewer, new org.netbeans.lib.awtextra.AbsoluteConstraints(130, 0, -1, -1));

      butCloseSocket.setText("Close socket");
      butCloseSocket.addActionListener(new java.awt.event.ActionListener()
            {
         public void actionPerformed(java.awt.event.ActionEvent evt)
         {
            butCloseSocketActionPerformed(evt);
         }
            });
      
      jPanel1.add(butCloseSocket, new org.netbeans.lib.awtextra.AbsoluteConstraints(250, 0, -1, -1));
      
      getContentPane().add(jPanel1, new org.netbeans.lib.awtextra.AbsoluteConstraints(0, 355, 449, 50));
      
   
      pack();
      
      try
      {
         Class.forName("org.apache.log4j.Logger") ;
      } catch (ClassNotFoundException e)
      {
         log4jDemo.setEnabled(false) ;
         return ;
      }

   }//GEN-END:initComponents
   
   //--------------------------------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------------------------------------------------------

   // TODO : begin user code.
   
   // force an error to check Log4JAppender robustness
   public int hashCode()
   {
      return 1/myDivider ;
   }
   
   
   private WinWatch MyWinWatch ;
   static Logger logger ;
   static Category cat ;

   public HashMap myMap = new HashMap () ;
   public Set mySet = new HashSet () ;  // java.util.AbstractSet , java.util.AbstractCollection , java.util.Collection
   public int[] myArray ;
   public HashSet users = new HashSet();
   

   private int myPrivateInt ;
   public int myPublicInt ;
   public Object myNullObject ;
   public int myDivider = 1;
   private int cpt = 1 ;
   public StringBuffer sb ;
  
   public void initVars() 
   {
      if (mySet.isEmpty())
      {
         sb = new StringBuffer().append("hello\n\t").append ("world\n") ;
         
         myMap.put (this,"this") ;
         myMap.put("Key3","test3") ;
         myMap.put(null,"null") ;
         myMap.put(new java.sql.Date(0), logger) ;  // descendant java.Date class
         myMap.put(new Integer(3),cat) ;
         myMap.put("Key4","test4") ;
         
         mySet.add ("test1") ;
         mySet.add (null) ;
         mySet.add ("test2") ;
         mySet.add (myMap);
         mySet.add (sb) ;
         
         myArray = new int[10] ;
         myArray[3]=8 ;
         myArray[8]=3 ;
         myArray[2]=7 ;
         myArray[4]=5 ;
         myArray[1]=4 ;
         
         users.add(new Person("Name1","FirstName1")) ;
         users.add(new Person("Name2","FirstName2")) ;
         users.add(new Person("Name3","FirstName3")) ;
         users.add(new Person("Name4","FirstName4")) ;
         users.add(new Person("Name5","FirstName5")) ;
         users.add(new Person("Name6","FirstName6")) ;
                 
       }
   }
   
   private void log4jDemoActionPerformed(java.awt.event.ActionEvent evt)
   {  
   	  // Logger test
      if (logger == null)
      {
         PropertyConfigurator.configure("log4J.properties");
         logger = Logger.getLogger("hello.world");  
         cat =  Category.getInstance(JavaDemo.class); 
       }
      initVars() ;
      logger.info("Hello world");
      Exception  e = new Exception("my exception") ;
      logger.debug("Received exception",e);
      
      logger.debug (sb) ;
      
      myDivider = 0 ; // force hashCode to crash
      
      // Category test with object value
      cat.debug(this);
      myDivider = 1 ;      
   }
   
   public class Base
   {
      public  int base_public ;
      private int base_private ;      
   }
   
   public class Herited extends Base 
   {
      public  int herited_public ;
      private int herited_private ;            
   }
   
   public class Person
   {
      public String Name ;
      public String FirstName ;
      public Date BirthDate ;
      
      public Person (String N,String F)
      {
         this.Name = N ;
         this.FirstName = F ;      
      }
      
   }
   
   private void butTest1ActionPerformed(java.awt.event.ActionEvent evt)
   {
 
      String str = "qwerty" ; // '\u2250' + "qwerty é ù è azerty" + '\u9999' ;
      Herited herited = new Herited() ;
      initVars() ;

      TTrace.options.sendFunctions = chkSendFct.isSelected() ;
      TTrace.options.socketUDP = chkUdp.isSelected() ;
      
      // simples traces
      //---------------------------
      
      TraceNode sampleNode = TTrace.debug().send("Hello").send("World") ;
      
      //TTrace.options.sendDate = true ;
      //TTrace.debug().send("trace with date") ;
      //TTrace.options.sendDate = false ;

      // single separator
      TTrace.debug().send("---");

      // send traces with special font style (bold and Italic), color font size and font name
      TTrace.debug().send("Special font", "Symbol 12")
         .setFontDetail(-1, false, true)                     // set whole line to italic
         .setFontDetail(3, true, false, java.awt.Color.red)                     // set col 3 (Left Msg)  to bold and Red
         .setFontDetail(4, false, false, java.awt.Color.green, 12, "Symbol");   // set col 4 (Right Msg) to Green , font size 12 , Symbol
      TTrace.debug().send("Impact Italic")
         .setFontDetail(3, false, true, java.awt.Color.pink, 12, "Impact");     // Col3 (left msg), non bold, Italic , pink , font 12 , Impact

      TTrace.debug().send("Whole line","in red")
         .setFontDetail(-1,false,false,java.awt.Color.red) ;                    // -1 : all columns

      java.awt.Color fushia = new Color (255,0,255) ; // rgb
      TTrace.debug().sendBackgroundColor("Highlighted col in Fuchsia",fushia,3) ;  // Background Color sample

      TTrace.debug().send("Special chars", "€ é ù è $ " );
   
      // double separator
      TTrace.debug().send("===");
      
      
      //TTrace.options.sendThreadName = false ; 
      //TTrace.debug().send("trace without thread id");
      //TTrace.options.sendThreadName = true;

      //TTrace.options.sendDate = true;
      //TTrace.debug().send("trace with date");
      //TTrace.options.sendDate = false;


      // traces using Sendxxx method
      //--------------------------------------------

      Class jFrame = null ;
      Class plugin = null ;
      
      try
      {
         jFrame = Class.forName("javax.swing.JFrame") ;
         plugin = Class.forName("tracetool.ITracePLugin") ;
      } catch (ClassNotFoundException e2)
      {
         // eat exception
      }
      
      // use default flags for object informations
      TTrace.debug().sendType("sendType TraceNode class" , sampleNode.getClass() ) ;
      TTrace.debug().sendType("sendType this class", this.getClass()) ;
      TTrace.debug().sendType("sendType JFrame class", jFrame) ;
      TTrace.debug().sendType("tracetool.IPlugin Interface", plugin,
            TraceDisplayFlags.showClassInfo +
            TraceDisplayFlags.showFields +
            TraceDisplayFlags.showModifiers +
            TraceDisplayFlags.showNonPublic +
            TraceDisplayFlags.showInheritedMembers +
            TraceDisplayFlags.showMethods) ;
      
      // jTabbedPane1 send Value and send object
      
      TTrace.debug().sendObject("send object jTabbedPane1 (short info)", jTabbedPane1,
            TraceDisplayFlags.showFields +
            TraceDisplayFlags.showModifiers
      ) ;
      TTrace.debug().sendObject("send object jTabbedPane1 (full info)", jTabbedPane1,
            TraceDisplayFlags.showClassInfo +
            TraceDisplayFlags.showFields +
            TraceDisplayFlags.showModifiers +
            TraceDisplayFlags.showNonPublic +
            TraceDisplayFlags.showInheritedMembers +
            TraceDisplayFlags.showMethods) ;
      TTrace.debug().sendValue ("send value jTabbedPane1", jTabbedPane1,true ,5) ;
      TTrace.debug().sendValue ("send value mySet", mySet,true ,5) ;
      
      TTrace.debug().sendValue ("send value herited", herited,true ,5) ;
      
       // stack trace
      TTrace.debug().sendStack("stack test", 0) ;
      TTrace.debug().sendCaller("Caller test",0) ;
      
      try
      {
         // dump
         TTrace.debug().sendDump("Dump test" , "ASCII"  , str.getBytes("ASCII"),50) ;
      } catch (UnsupportedEncodingException e1)
      {
         // ignore exception
      }     

      // traces using TraceNodeEx
      //--------------------------------------------

      // create a extended trace node , fill it then add some dumps and an object
      TraceNodeEx node = new TraceNodeEx(null) ;   //TTrace.debug()
      node.leftMsg = "TraceNodeEx" ;
      node.rightMsg = "sample" ;
      node.iconIndex = 8 ;
      node.addFontDetail(3, false, false, java.awt.Color.green);
     
      node.members.add("col 1 bold", "col 2 Green" , "col 3 default")
         .setFontDetail(0,true)                                  // set first column to bold
         .setFontDetail(1, false, false, java.awt.Color.green)   // set second column to green
         .add("Italic Sub members")                              // add sub member node
            .setFontDetail(0,false,true) ;                       // set first column to Italic
      
      try
      {
         node.addDump("ASCII"  , str.getBytes("ASCII"),50) ;       // default encoding
         node.addDump("UTF-8"  , str.getBytes("UTF-8"),50) ;
         node.addDump("UTF-16" , str.getBytes("UTF-16"),50) ;
         node.addDump("UNICODE", str.getBytes("UNICODE"),50) ;

         byte [] dump = new byte[256] ;
         for (int c = 0 ; c <= 255 ; c++)
            dump[c] = (byte) c ;               
         node.addDump("all chars", dump, 256) ;

         node.addStackTrace() ;  // show stack.
         node.addObject(str) ;
      }
      catch (Exception e)  // str.getBytes : UnsupportedEncodingException
      {
        // ignore exception
      }
      
      // finally send the node
      node.send() ;  
      
     
      // XML sample using Send
      //--------------------------------------------
      TTrace.debug().sendXml("xml", "<?xml version='1.0' ?><Data> Hello XML </Data>");
      
      
      // send table detail  
      //--------------------------------------------
      
      TraceTable table = new TraceTable();        // create the table

      // add titles. Individual columns titles can be added or multiple columns , separated by tabs
      table.addColumnTitle("colA");          // first column title
      table.addColumnTitle("colB");          // second column title
      table.addColumnTitle("title column C\tcolD");  // other columns title (tab separated)

      // add first line. Individual columns data can be added or multiple columns , separated by tabs
      table.addRow();
      table.addRowData("a");                           // add first col
      table.addRowData("b" + "\t" + "c" + "\t" + "d" + "\t" + "e");            // then add other columns (tab separated)

      // add second line
      table.addRow();
      table.addRowData("aa" + "\t" + "data second column" + "\t" + "cc" + "\t" + "dd" + "\t" + "ee");  // add all columns data in a single step (tab separated)

      // finally send the table
      TTrace.debug().sendTable("Mytable", table);
     
      
      // Another way to send table : send array/collection/map
      TTrace.debug().sendTable("myArray", myArray);
      TTrace.debug().sendTable("users"  , users);
      TTrace.debug().sendTable("myMap"  , myMap);
 
      // Text, table and XML together
      //--------------------------------------------
      node = new TraceNodeEx(TTrace.debug());
      node.leftMsg = "Text,table and XML together";
      node.members.add("Text displayed in detail");
      node.addTable(table) ;
      node.addXML("<?xml version='1.0' ?><Data> Xml in traceNodeEx </Data>");
      node.send();

      // thread samples
      //--------------------------------------------

      // current thread
      //TTrace.debug().send("currentThread name", Thread.currentThread().getName() );
      
      
      //ThreadGroup group = Thread.currentThread().getThreadGroup() ;// get the group of the current thread
      //while (group.getParent() != null)// get the master group
      //   group = group.getParent() ;
      //TraceNode SystemNode = TTrace.debug().send("system group thread");
      //RecurGroup(group, SystemNode) ;

      // ensure all traces are send to the viewer
      TTrace.flush();            
   }
  
   
   WinTrace MultiColTrace ;
   
   private void butTestMultiColActionPerformed(java.awt.event.ActionEvent evt)
   {      
      if (MultiColTrace == null) 
      {
         MultiColTrace = new WinTrace ("MCOL" , "MultiCol trace window") ;
         MultiColTrace.setMultiColumn (1) ;  // must be called before calling setColumnsTitle
         MultiColTrace.setColumnsTitle("col1 \t col2 \t col3");
         MultiColTrace.setColumnsWidth("100:20:80 \t 200:50 \t 100");
         MultiColTrace.displayWin() ;
         // set local log
         // 3, Local log is disabled
         // 4, Local log enabled. No size limit.
         // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
         MultiColTrace.setLogFile("c:\\MultiCol.xml", 4) ;
          
      }
      MultiColTrace.debug().send("1 \t 2 \t 3") ;     
   }
   
   private void butIndentActionPerformed(java.awt.event.ActionEvent evt)
   {      
      TraceNode node = TTrace.debug().send ("Tree indentation using Indent and UnIndent methods") ;

      node.indent("Indent level 1") ;
      node.send ("Node1") ;
      node.indent("Indent level 2") ;
      node.send ("Node2") ;
           
      // UnIndent with no title
      node.indent ("Indent level 3") ;
      node.send ("Node3") ;
      node.unIndent () ;   // UnIndent without title
      
      node.send ("Node4") ;
      
      node.unIndent ("UnIndent level 2" ) ;
      node.unIndent ("UnIndent level 1") ;
      
      // node indentation using traceNodeEx
      TTrace.debug().send   ("root 1", Integer.toString( TTrace.debug().getIndentLevel())) ;
      TTrace.debug().indent ("start indentation");
      TTrace.debug().send   (   "under indent 1", Integer.toString( TTrace.debug().getIndentLevel()));
      TraceNodeEx nodeEx = new TraceNodeEx(TTrace.debug()) ;   // Parent depends of the indentation
      nodeEx.leftMsg =          "under indent 2" ;
      nodeEx.send() ;
      TTrace.debug().unIndent ();
      TTrace.debug().send("root 2", Integer.toString( TTrace.debug().getIndentLevel()));
   }

   
   private void butWinLoadXmlActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace.loadXml("c:\\log2.xml") ;
   }
   
   private void butLoadMainXmlActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.winTrace().loadXml("c:\\log.xml");
   }
   
   private WinTrace myWinTrace ;
   
   private void butClearWinActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace.clearAll() ;
   }
   
   private void butSaveWinToXmlActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace.saveToXml("c:\\log2.xml") ;
   }
   
   private void butSaveWinToTextActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace.saveToTextfile("c:\\log2.txt") ;
   }
   
   private void butSayHelloActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace.debug().send("Hello", "Can be used to store exceptions, for examples");
   }
   
   private void butDisplayWinActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace.displayWin() ;
   }
      
   private void butCreateWinActionPerformed(java.awt.event.ActionEvent evt)
   {
      myWinTrace = new WinTrace("MyWINID" , "My trace window") ;
      butDisplayWin.setEnabled(true) ;
      butSayHello.setEnabled(true) ;
      butSaveWinToText.setEnabled(true) ;
      butSaveWinToXml.setEnabled(true) ;
      butClearWin.setEnabled(true) ;
      butWinLoadXml.setEnabled(true) ;
   }
   
   private void butShowNodeActionPerformed(java.awt.event.ActionEvent evt)
   {
      start2.show() ;
   }
   
   private void butAppendLeftActionPerformed(java.awt.event.ActionEvent evt)
   {
      start2.appendLeft("Done 2") ;
   }
   
   private void butSetSelectedActionPerformed(java.awt.event.ActionEvent evt)
   {
      start1.setSelected() ;
   }
   
   private void butSaveMainToXmlActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.winTrace().saveToXml("c:\\log.xml");
      TTrace.winTrace().saveToXml("c:\\logWithStyleSheet.xml","tracetool.xsl");
   }
   
   TraceNode start1 ;
   TraceNode start2 ;
   Date mydate = new Date ();
   
   private void butstart1ActionPerformed(java.awt.event.ActionEvent evt)
   {
      start1 = TTrace.debug().send("Start 1 ..") ;
      butResendRight.setEnabled(true) ;
      butSetSelected.setEnabled(true) ;      
   }
   
   private void butSaveMainToTextActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.winTrace().saveToTextfile("c:\\log.txt");
   }
   
   private void butResendRightActionPerformed(java.awt.event.ActionEvent evt)
   {
      start1.resendRight("Done 1") ;
   }
   
   private void butstart2ActionPerformed(java.awt.event.ActionEvent evt)
   {
      start2 = TTrace.debug().send("Start 2 ..") ;
      butAppendLeft.setEnabled(true) ;
      butShowNode.setEnabled(true) ;
      butBookmark .setEnabled(true) ;
      butToogleVisible.setEnabled(true) ;
   }
   
   private void butTest2ActionPerformed(java.awt.event.ActionEvent evt)
   {
      TraceNode cNode ;
      TraceNode dNode ;
      
      TTrace.debug().send("Begin");
      for (int c = 1 ; c <= 3 ; c++)
      {
         cNode = TTrace.debug().send("level c " + c) ;
         
         for (int d = 1 ; d <= 300 ; d++)
         {
            dNode = cNode.send("level d " + d) ;
            
            for (int f = 1; f <= 6;f++)
            {
               dNode.send("level e " + f) ;
            }
         }
      }
      
      TTrace.debug().send("Before Flush");
      TTrace.flush() ;
      TTrace.debug().send("Flush done");
      
   }
   
   
   private void butLogFile_Click(java.awt.event.ActionEvent evt)
   {
      // set viewer log
      // 0, Viewer Log is disabled.
      // 1, Viewer log enabled. No size limit.
      // 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      TTrace.winTrace().setLogFile("c:\\logFromViewer.xml", 1);
      // set local log
      // 3, Local log is disabled
      // 4, Local log enabled. No size limit.
      // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
      TTrace.winTrace().setLogFile("c:\\LogFromJavaApi.xml", 5);
      TTrace.debug().send ("traces saved") ;
      TTrace.flush() ;
      TTrace.debug().send ("in this file :" ,  TTrace.winTrace().getLocalLogFile()) ;
      
   }
   
   private void butDisableSocket(java.awt.event.ActionEvent evt)
   {
      TTrace.options.sendMode = SendMode.None ;
   }
    
   private void butEnableSocket(java.awt.event.ActionEvent evt)
   {
      TTrace.options.sendMode = SendMode.Socket ;
   }
  
   private void butCloseSocketActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.closeSocket() ;
   }
   
   private void butTailDemoActionPerformed(java.awt.event.ActionEvent evt)
   {
      File f = new File("c:\\log.txt");
      FileWriter fileWriter;
      
      // try to create it, don't care about the error.
      try
      {
         f.createNewFile();
      } catch (IOException  e)
     {
        // ignore exception
     }
      
      // open , append, close
      try
      {
         fileWriter = new FileWriter("c:\\log.txt", true);
         myPublicInt++ ;
         fileWriter.write("test " + myPublicInt + "\n");
         fileWriter.close();
      } catch (IOException  e)
      {
         // ignore exception
      }
      
   }
   
   
   private void butClearMainActionPerformed(java.awt.event.ActionEvent evt)
   {
      // clear main traces
      TTrace.clearAll() ;  // same as TTrace.winTrace().clearAll() ;
   }
   
   private void butShowViewerActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.show(true);
   }
   
   private void butCloseViewerActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.closeViewer();
   }

   private void chkSendFctActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.options.sendFunctions = chkSendFct.isSelected() ;
   }
   
   private void chkUdpActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.options.socketUDP = chkUdp.isSelected() ;
   }

   private void butQuitActionPerformed(java.awt.event.ActionEvent evt)
   {
      exitForm(null) ;
   }
        
   private void RecurGroup(ThreadGroup group, TraceNode MasterGroupNode)
   {
      // get all threads for the group. An estimation of the number of thread is done by activeCount
      int estSize ;
      Thread[] Threads ;
      ThreadGroup[] Groups ;
      int actualSize ;
      
      estSize = group.activeCount();
      Threads = new Thread[estSize*2];
      actualSize = group.enumerate(Threads,false);
      
      TraceNode allThreads = MasterGroupNode.send("Threads (" + Integer.toString(actualSize) +")") ;
      for ( int i = 0; i < actualSize; i++ )
      {
         Thread t = Threads[i];
         allThreads.send("Thread name : " + t.getName() , "Priority : " + Integer.toString(t.getPriority()));
      }
      
      // get all sub groups
      // get all threads for the group. An estimation of the number of thread is done by activeCount
      estSize = group.activeGroupCount();
      Groups = new ThreadGroup[estSize*2];
      actualSize = group.enumerate(Groups);
      
      if (actualSize != 0)
      {
         TraceNode allGroups = MasterGroupNode.send("SubGroups (" + Integer.toString(actualSize) +")") ;
         
         for ( int i = 0; i < actualSize; i++ )
         {
            ThreadGroup SubGroup = Groups[i] ;
            TraceNode SubGroupNode = allGroups.send("Group name : " + SubGroup.getName()) ;
            RecurGroup(SubGroup, SubGroupNode) ;
         }
      }
      
   }
   
   private void butSearchActionPerformed(java.awt.event.ActionEvent evt)
   {
      // TTrace.winTrace().gotoBookmark(1); // second bookmark, noted [1]
      // TTrace.winTrace().clearBookmark() ;
      // TTrace.winTrace().gotoFirstNode() ;
      // TTrace.winTrace().gotoLastNode() ;
     

      // TTrace.Find just set the criterias and hightlight if asked, but don't move to the next matching node.
      TTrace.find("StRinG", false,  true ,  true,  true);// {Sensitive}{WholeWord}{Highlight}{SearchInAllPages}

      // from the current node : go to the next item matching criteria. Call ttrace.WinTrace.GotoFirstNode() before FindNext to start search from first node
      TTrace.winTrace().findNext( true) ; //{SearForward}
   }
   
   private void butFilterActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.winTrace().clearFilter();

      // 5 kinds of filters
      // -------------------
      //Equal = 0
      //Not equal = 1
      //contains = 2
      //Don't contains = 3
      //(Ignore this filter) = 4 or -1

      // filters can be applied on all columns. Note that the "icone" column is not zero but 999. The members are identified by the column 998
      // On multicolumn mode. Column 0 can be used normally.

      TTrace.winTrace().addFilter(/*col icone*/   999,/*Equal*/                 0, "24");           // 999 : Icon column . Filter on "info" (index is 24)
      TTrace.winTrace().addFilter(/*col time */     1,/*Not equal*/             1, "string");
      TTrace.winTrace().addFilter(/*col thread*/    2,/*contains*/              2, "0x");
      TTrace.winTrace().addFilter(/*col traces*/    3,/*Don't contains*/        3, "nothing");
      TTrace.winTrace().addFilter(/*col Comment*/   4,/*(Ignore this filter)*/ -1, "string");       // -1 or 4 can be used to disable this filter (not very usefull...)
      TTrace.winTrace().addFilter(/*col members*/ 998,/*contains*/              2, "string");       // members info : 998

      TTrace.winTrace().applyFilter(/*{ConditionAnd*/ true, /*ShowMatch*/ true, /*IncludeChildren*/ true);
   }
   
   private void butResetFilterActionPerformed(java.awt.event.ActionEvent evt)
   {
      TTrace.winTrace().clearFilter();
   }
   
   boolean lastToggleBookmark;
   boolean lastToggleVisible;
   private void butBookmarkActionPerformed(java.awt.event.ActionEvent evt)
   {
      start2.setBookmark(lastToggleBookmark);
      lastToggleBookmark = !lastToggleBookmark;
   }
   
   private void butToogleVisibleActionPerformed(java.awt.event.ActionEvent evt)
   {
      start2.setVisible(lastToggleVisible);
      lastToggleVisible = !lastToggleVisible;
   } 
 
   /** Exit the Application */
   private void exitForm(java.awt.event.WindowEvent evt) {
      System.exit(0);
   }

   /**
    * @param args the command line arguments
    */
   public static void main(String args[])
   {
      new JavaDemo().setVisible(true) ; //show();          
   }
   
   // Variables declaration - do not modify//GEN-BEGIN:variables
   public javax.swing.JButton butAppendLeft;
   public javax.swing.JButton butClearMain;
   public javax.swing.JButton butClearWin;
   public javax.swing.JButton butCloseSocket;
   public javax.swing.JButton butCreateWin;
   public javax.swing.JButton butDisplayWin;
   public javax.swing.JButton butLoadMainXml;
   public javax.swing.JButton butQuit;
   public javax.swing.JButton butResendRight;
   public javax.swing.JButton butSaveMainToText;
   public javax.swing.JButton butSaveMainToXml;
   public javax.swing.JButton butSaveWinToText;
   public javax.swing.JButton butSaveWinToXml;
   public javax.swing.JButton butSayHello;
   public javax.swing.JButton butSetSelected;
   public javax.swing.JButton butShowNode;
   public javax.swing.JButton butShowViewer;
   public javax.swing.JButton butCloseViewer;
   public javax.swing.JButton butTailDemo;
   public javax.swing.JButton butMainLog;
   public javax.swing.JButton butNoTransport;
   public javax.swing.JButton butSocketTransport;
   public javax.swing.JButton butTest1;
   public javax.swing.JButton butTest2;
   public javax.swing.JButton butSearch;
   public javax.swing.JButton butFilter;
   public javax.swing.JButton butResetFilter;
   public javax.swing.JButton butBookmark ;
   public javax.swing.JButton butToogleVisible ;
  
   public javax.swing.JButton butTestMultiCol;
   public javax.swing.JButton butIndent;   
   public javax.swing.JButton butWinLoadXml;
   public javax.swing.JButton butstart1;
   public javax.swing.JButton butstart2;
   public javax.swing.JCheckBox chkSendFct;
   public javax.swing.JCheckBox chkUdp;
   public javax.swing.JTextField editTarget;
   
   public javax.swing.JButton log4jDemo;
   public javax.swing.JPanel jPanel1;
   public javax.swing.JTabbedPane jTabbedPane1;
   public javax.swing.JPanel tabBasic;
   public javax.swing.JPanel tabMultipage;
   public javax.swing.JPanel tabNodesOperations;

   public javax.swing.JPanel tabWatches;
   public javax.swing.JButton butWatch;
   public javax.swing.JButton butClearWatchWindow;
   public javax.swing.JButton butDisplayWatchWindow;
   public javax.swing.JButton butCreateWinWatch;
   public javax.swing.JButton butWinWatchSend;
   public javax.swing.JButton butWinWatchClear;
   public javax.swing.JButton butWinWatchDisplay;
     
   // End of variables declaration//GEN-END:variables
   
}

