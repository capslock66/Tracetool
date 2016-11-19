TraceTool.pas
--------------

This is the main delphi client unit.

SocketTrace.pas
---------------

Include that file if you want to use the socket mode.
Socket is not part of the main client unit for theses 2 reasons :
First  : for compatibility with older delphi version that don't include Indy component.
Second : To reduce the size of the target if you don't need socket.
If you want to use socket, you must add that unit AFTER the traceTool unit in the use clause.

StackTrace.pas
--------------

Include that file if you want stack related support.
You need JEDI code library. 
See documentation. 

DbugIntf.pas
------------

A replacement of the GDEBUG DbugIntf.pas unit that use TraceTool 

TraceTool.inc
--------------

A common file used by both the client and the server (const)


