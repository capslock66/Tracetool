
' Copyright (c) Microsoft Corporation.  All rights reserved.

Imports System
Imports System.ComponentModel
Imports Microsoft.EnterpriseInstrumentation
Imports Microsoft.EnterpriseInstrumentation.Schema


Namespace SimpleInstrumentation
    <RunInstaller(True)> Public Class MyProjectInstaller
        Inherits ProjectInstaller
    End Class

    ' <summary>
    ' A simple example of raising an event.
    ' </summary>
    Public Class HelloWorld
        Public Shared Sub Main()
            ' Raise a TraceMessageEvent event through the Application EventSource.
            TraceMessageEvent.Raise("Hello World")
        End Sub
    End Class
End Namespace
