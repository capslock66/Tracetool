// See https://aka.ms/new-console-template for more information
using TraceTool;

TTrace.Options.SocketHost = "127.0.0.1";

Console.WriteLine("Ensure the viewer is started !!!");
Console.WriteLine("");
Console.WriteLine("Select how traces are sent to viewer.");
Console.WriteLine("1 - socket (worker thread)");
Console.WriteLine("2 - socket async");
Console.WriteLine("3 - websocket (worker thread)");
Console.WriteLine("4 - websocket async");
Console.WriteLine("5 - windows msg (worker thread)");
Console.WriteLine("6 - windows msg Async");

int intChoice = Convert.ToInt32(Console.ReadLine());
string strChoice;
switch (intChoice)
{
    case 1:
        strChoice = "socket sync";
        TTrace.Options.SendMode = SendMode.Socket;
        TTrace.Options.SocketPort = 8090;
        TTrace.Options.UseWorkerThread = true; // sync , default
        break;
    case 2:
        strChoice = "socket async";
        TTrace.Options.SendMode = SendMode.Socket;
        TTrace.Options.SocketPort = 8090;
        TTrace.Options.UseWorkerThread = false; // async
        break;

    case 3:
        strChoice = "websocket sync";
        TTrace.Options.SendMode = SendMode.WebSocket;
        TTrace.Options.SocketPort = 8091;
        TTrace.Options.UseWorkerThread = true;
        break;

    case 4:
        strChoice = "websocket async";
        TTrace.Options.SendMode = SendMode.WebSocket;
        TTrace.Options.SocketPort = 8091;
        TTrace.Options.UseWorkerThread = false;
        break;

    case 5:
        strChoice = "windows msg sync";
        TTrace.Options.SendMode = SendMode.WinMsg;
        TTrace.Options.UseWorkerThread = true;
        break;

    case 6:
        strChoice = "windows msg async";
        TTrace.Options.SendMode = SendMode.WinMsg;
        TTrace.Options.UseWorkerThread = false;
        break;
    default:
        return;
}

Console.WriteLine("sending traces");

TTrace.ClearAll();
TTrace.Debug.Send($"console framework {strChoice} ", TTrace.Debug.GetType().Assembly.Location);

var myClass = new MyClass();
myClass.Name = "Foo";
TTrace.Debug.SendValue("myClass", myClass);
TTrace.Debug.SendObject("myClass", myClass);

var myStruct = new MyStruct();
//myStruct.X = 1;  // readonly
TTrace.Debug.SendValue("myStruct", myStruct);
TTrace.Debug.SendObject("myStruct", myStruct);

var myRecord = new MyRecord("my foo");
myRecord.Bar = "bar";
//myRecord.Foo = "foo";  // read only
TTrace.Debug.SendValue("myRecord", myRecord);
TTrace.Debug.SendObject("myRecord", myRecord);

// Indent/Unindent on a Single thread
TTrace.Debug.Indent("Indent A, No Await, then same thread").SetFontDetail(colId: 3, bold: true);
foreach (var counter in new[] { 0, 1, 2, 3, 4, 5 })
{
    TTrace.Debug.Send($"under Indent A : [{counter}]").Show();
    // No await here !
}
TTrace.Debug.UnIndent("UnIndent A");

// Indent/Unindent with Async task
TTrace.Debug.Indent("Indent B, with await, then possible another thread ").SetFontDetail(colId:3,bold:true);
foreach (var counter in new[] { 0, 1, 2, 3, 4, 5 })
{
    if (counter == 3)
        await Task.Run(async () =>
        {
            await Task.Delay(500);
            TTrace.Debug.Send("under Indent B [3], after Async Delay() then another thread is possible");
            if (TTrace.Options.UseWorkerThread)
                TTrace.Flush();
            else
                await TTrace.FlushAsync();
        });
    else
        TTrace.Debug.Send($"under Indent B : [{counter}]").Show();
}
TTrace.Debug.UnIndent("UnIndent B");

// using a TraceNode and Async
var traceNode = TTrace.Debug.Send("Using trace Node C, with await").SetFontDetail(colId: 3, bold: true);
foreach (var counter in new[] { 0, 1, 2, 3, 4, 5 })
{
    if (counter == 3)
        await Task.Run(async () =>
        {
            await Task.Delay(500);
            traceNode.Send("under Node C [3] Async, after Async Delay()" +
                " then another thread is possible");
            await TTrace.FlushAsync();
        });
    else
        traceNode.Send($"under Node C : [{counter}]").Show();
}
await TTrace.FlushAsync();

public class MyClass
{ 
    public string Name { get; set; } = null!;
}

public struct MyStruct 
{
    public double X { get; }
    public double Y { get; }
}

public record MyRecord (string Foo)
{
    public string Bar { get; set; } = null!;
}

