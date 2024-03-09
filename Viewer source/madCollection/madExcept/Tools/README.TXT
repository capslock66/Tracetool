madExceptPatch
--------------
If you build your project's inside of the Delphi IDE, you don't need this tool.
But if you built your project's with Delphi's command line tools, please call
madExceptPatch afterwards for each of your project's. This adds some patches
which are necessary to give madExcept its full power.

The tool contains additional help text when starting it via command line
without any parameters.


madCompileBugReport
-------------------
If you disable the option "link in function names and line numbers", madExcept
will only store the function addresses into your exe/dll file, but no unit
names, function names and line numbers. In this situation, the bug reports
your end users send you will lack all the important information like function
names, unit names and line numbers. You need to "compile" such a bug report
to get the full bug report you're used to. The tool "madCompileBugReport"
will do that for you. It needs the original map files for that task. But it
doesn't accept the map files in its original "map" form, but it wants the
compressed "mad" files instead. So for each version you release to your end
users, you should store the "mad" file to a specific storage folder. You may
name the file as you want. However, the name must contain the project name
(e.g. "Project1") somewhere and the extension must be ".mad". The
madCompileBugReport tool will search through all folders you want and
automatically find the "mad" files which belong to the bug report you want
to have "compiled".

Please note that you MUST compile your project with "show address offset to
the beginning of the procedure" (see "bug report settings" tab of the madExcept
settings dialog), otherwise the "madCompileBugReport" tool won't work.

The tool contains additional help text when starting it via command line
without any parameters.


madTestMailAPIs
---------------
A little tool which tests the two available mailing APIs. Do they work on your
PC? Are they correctly set up?


madTraceProcess
---------------
If you want to know what your project currently does, simply call
madTraceProcess and choose your process from the selection dialog. As a result
you'll see the madExcept exception box, showing you the current callstack of
all your project's threads. This can be e.g. useful, when your project seems
to be frozen (and doesn't detect it itself).


madExceptViewer
---------------
A little database like tool which collects bug reports from the clipboard or
from a file. You can view the bug reports comfortably. You can even double
click stack items, as a result Delphi will open the responding file at the
relevant line.

You can download the viewer here:

http://madshi.net/madExceptViewer.zip