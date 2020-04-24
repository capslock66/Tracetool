// To create the bootstrapper, from within powershell run the following.
// Invoke-WebRequest http://cakebuild.net/bootstrapper/windows -OutFile build.ps1
//
// To execute, run the following within powershell
// ./Build.ps1 -Target "build"

// For smoke test Task
//#Addin "Cake.tracetool"

// If testing a local dll, comment out the #Addin above and uncomment the line below.
#r "c:\GitHub\Tracetool\DotNet\Cake.Tracetool\src\Cake.Tracetool\bin\Debug\Cake.tracetool.dll"

#r "c:\GitHub\Cake.Endpoint\src\Cake.Endpoint\bin\Debug\netstandard2.0\Cake.Endpoint.dll"

var target = Argument("target", "Test");

Task("Test")
.Does(() => {
    
    Information("Attempting Password retrieval.");
    //Debug_SendObject("message",target) ;
    //DebugSend("message") ;
    var y = TheAnswerToLife ;
  
}); 
RunTarget(target);
