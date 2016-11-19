# tracetool

Send debug, warning, error traces to an external windows tracetool viewer (located on the bin folder). 

You can send classic traces, 2 columns traces, hierachical traces, objects , call stack
you can resend traces or delete it, create new pages on the viewer to separate business traces,...
There is a lot of functions to manages traces.

Your nodeJs is used as a web server with client javascript code ?
Use the tracetool.js Api !, so you can see all your traces on the same computer...

Tracetool is also available for Java , Dot Net, C++ , Delphi, Python and any system compatible with ActiveX (windows)

See the codeproject site for all possibilities
http://www.codeproject.com/Articles/5498/TraceTool-The-Swiss-Army-Knife-of-Trace
Everything is opensource

You MUST start the viewer before sending traces.
On the View/Options... menu, select the "framework" section , enable the HTTP port (used for javascript api) and give an UNUSED port like 81
This port need to be the same on your nodejs application

You can enter a comment on the "TraceTool Title" in the "general" section , like "Tracetool - Http port : 81"
The viewer can be on a windows computer and your application on any system suporting nodejs like a raspberry Pi !

In your NodeJs Application, just give the Host and port before sending traces.

# Usage

```javascript
var ttrace = require('tracetool');
ttrace.setHost('127.0.0.1:81');   // note that the default is already 127.0.0.1:81
ttrace.debug().send('hello', 'world') ;
ttrace.debug().send('Parent trace').send('Child trace') ;

```
more to come...

