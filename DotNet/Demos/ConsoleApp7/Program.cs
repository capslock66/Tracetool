// See https://aka.ms/new-console-template for more information
using TraceTool;

TTrace.Options.SocketHost = "127.0.0.1";
TTrace.Options.SendMode = SendMode.Socket;
TTrace.Options.SocketPort = 8090;
TTrace.Options.UseWorkerThread = true; // sync , default

TTrace.ClearAll();

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

TTrace.Flush();

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

