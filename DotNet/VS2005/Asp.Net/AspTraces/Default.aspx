<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Default.aspx.cs" Inherits="_Default" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>Untitled Page</title>
</head>
<body>
    <form id="form1" runat="server">
       
       &nbsp;<asp:Panel ID="PanelTitle1" runat="server" Height="24px" Width="540px" BackColor="Silver"
            BorderStyle="None">
            Basic</asp:Panel>
        <br />
        <asp:Panel ID="Panel1" runat="server"  Width="540px">
            &nbsp;Tips : Ensure TraceTool is running.<br />
            <br />
            &nbsp;<asp:Button ID="butSample" runat="server" Text="Sample traces" OnClick="butSample_Click" Width="134px" /><br />
            &nbsp;<asp:Button ID="butIndent" runat="server" Text="Indent/UnIndent" OnClick="butIndent_Click" />
            &nbsp; &nbsp; &nbsp;<br />
            &nbsp;<asp:Button ID="butSaveTotext" runat="server" Text='SaveToText("c:\log.txt")' OnClick="butSaveTotext_Click"/>&nbsp;<br />
            &nbsp;<asp:Button ID="butSaveToXml" runat="server" Text='SaveToXml("c:\log.xml")' OnClick="butSaveToXml_Click" /><br />
            &nbsp;<asp:Button ID="butLoadXml" runat="server" Text='LoadXml("c:\log.xml")' OnClick="butLoadXml_Click"/>&nbsp;<br />
            &nbsp;<asp:Button ID="butClearMainTraces" runat="server" Text="Clear main traces" OnClick="butClearMainTraces_Click"/>
            <br />
            <br />
            &nbsp;<asp:Button ID="butShowViewer" runat="server" Text="Show Viewer" Width="135px" OnClick="butShowViewer_Click" />
        </asp:Panel>
        
        &nbsp;&nbsp;
        <asp:Panel ID="Panel5" runat="server" Height="24px" Width="540px" BackColor="Silver">
            Node operations
        </asp:Panel>
        
        <br />
        <asp:Panel ID="Panel3" runat="server"  Width="540px">
            <br />
            &nbsp;<asp:Button ID="butStart1" runat="server" Text="Start1" OnClick="butStart1_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butResend" runat="server" Text="Resend()" Enabled="False" OnClick="butResend_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butSetSelected" runat="server" Text="SetSelected()" Enabled="False" OnClick="butSetSelected_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butNodeIndent" runat="server" Text="Indent/Unindent" Enabled="False" OnClick="butNodeIndent_Click" /><br />
            &nbsp;<asp:Button ID="butstart2" runat="server" Text="Start2" OnClick="butstart2_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butAppend" runat="server" Text="Append()" Enabled="False" OnClick="butAppend_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butShowNode" runat="server" Text="Show()" Enabled="False" OnClick="butShowNode_Click" />
        </asp:Panel>
        
        <br />
        <asp:Panel ID="Panel8" runat="server" Height="24px" Width="540px" BackColor="Silver">
            Multi pages
        </asp:Panel>
        <br />
        
        <asp:Panel ID="Panel4" runat="server"  Width="540px">
            &nbsp;<asp:Button ID="butCreateWindow" runat="server" Text="Create a new trace window" OnClick="butCreateWindow_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butDisplayWindow" runat="server" Text="Display that window on the viewer" Enabled="False" OnClick="butDisplayWindow_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butHelloWin" runat="server" Text="Say Hello" Enabled="False" OnClick="butHelloWin_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butSaveWinToText" runat="server" Text='SaveToText("c:\log2.txt")' Enabled="False" OnClick="butSaveWinToText_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butSaveWinToXml" runat="server" Text='SaveToXml("c:\log2.xml")' Enabled="False" OnClick="butSaveWinToXml_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butLoadWinXml" runat="server" Text='LoadXml("c:\log2.xml")' Enabled="False" OnClick="butLoadWinXml_Click" /><br />
            &nbsp;<asp:Button ID="butMulticolTest" runat="server" Text="Multi col test" OnClick="butMulticolTest_Click" />
         </asp:Panel>
        &nbsp;&nbsp;
        <asp:Panel ID="Panel2" runat="server" Height="24px" Width="540px" BackColor="Silver">
            Watches
        </asp:Panel>
        <br />
        <asp:Panel ID="Panel6" runat="server"  Width="540px">
            &nbsp;Main watch window<br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butSendMainWatches" runat="server" Text="Send watches" OnClick="butSendMainWatches_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butDisplayMainWatches" runat="server" Text="Display main watch window" OnClick="butDisplayMainWatches_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butClearMainWatches" runat="server" Text="Clear main watch window" OnClick="butClearMainWatches_Click" /><br />
            &nbsp;<asp:Button ID="butCreateWinWatches" runat="server" Text="Create new WinWatch" OnClick="butCreateWinWatches_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butSendWinWatches" runat="server" Text="Send watches" Enabled="False" OnClick="butSendWinWatches_Click" />
            <br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butDisplayWinWatches" runat="server" Text="Display watch window" Enabled="False" OnClick="butDisplayWinWatches_Click" /><br />
            &nbsp; &nbsp; &nbsp;&nbsp;
            <asp:Button ID="butClearWinWatches" runat="server" Text="Clear watch window" Enabled="False" OnClick="butClearWinWatches_Click" /><br />
        </asp:Panel>
    </form>
</body>
</html>
