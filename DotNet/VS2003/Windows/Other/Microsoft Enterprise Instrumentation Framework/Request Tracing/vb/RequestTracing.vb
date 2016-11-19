
' Copyright (c) Microsoft Corporation.  All rights reserved.

Imports System
Imports System.ComponentModel
Imports Microsoft.EnterpriseInstrumentation
Imports Microsoft.EnterpriseInstrumentation.Schema
Imports Microsoft.EnterpriseInstrumentation.RequestTracing

Namespace RequestTracing
    <RunInstaller(True)> Public Class MyProjectInstaller
        Inherits ProjectInstaller
    End Class

    Public Class MainClass
        Public Shared Sub Main()
            ' show request tracing
            Dim tracing As RequestTracing = New RequestTracing()
            tracing.TraceMethod_TryFinallyParadigm()
        End Sub
    End Class

    ' <summary>
    ' An example class that performs request tracing.
    ' </summary>
    Public Class RequestTracing
        ' with request tracing, a RequestEventSource must be created.
        Private Shared requestEventSource As RequestEventSource = New RequestEventSource("RequestTracing.RequestTraceTest.GenerateTraceEvents")

        ' <summary>
        '  This method shows an example of request tracing using the try..finally paradigm.
        ' </summary>
        Public Sub TraceMethod_TryFinallyParadigm()
            Dim request As RequestTrace = New RequestTrace(requestEventSource)
            ' TraceRequestStartEvent generated implicitly
            Try
                ' we raise an example event through the default Application EventSource. 
                ' the TraceMessageEvent is also raised through the RequestEventSource.
                TraceMessageEvent.Raise("hello from try...finally sample")
            Finally
                request.Dispose()
            End Try
            ' TraceRequestEndEvent generated implicitly
        End Sub
    End Class
End Namespace

