
' Copyright (c) Microsoft Corporation.  All rights reserved.

Imports System
Imports System.ComponentModel
Imports Microsoft.EnterpriseInstrumentation
Imports Microsoft.EnterpriseInstrumentation.Schema

Namespace EventSourceAPI

    <RunInstaller(True)> Public Class MyProjectInstaller
        Inherits ProjectInstaller
    End Class

    Public Class ConsoleVb
        Private Shared myEventSource As EventSource = New EventSource("EventSourceAPI")
        Public Shared Sub Main()
            ' explicit
            Dim e1 As TraceMessageEvent = New TraceMessageEvent()
            e1.Message = "Costly Message"
            myEventSource.Raise(e1)

            ' slightly less costly, if the EventSource will not be enabled much of the time.
            If myEventSource.IsEnabledForType(GetType(TraceMessageEvent)) Then
                Dim e2 As TraceMessageEvent = New TraceMessageEvent()
                e2.Message = "Less Costly Message"
                myEventSource.Raise(e2)
            End If

            ' static one liner (which wraps the above code sequence)
            TraceMessageEvent.Raise(myEventSource, "Static One Liner Message")

            ' static one liner which is raised through the EventSource.Application EventSource.
            TraceMessageEvent.Raise("Static One Liner Message through the Application event source")

        End Sub
    End Class
End Namespace
