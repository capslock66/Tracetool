unit ecStrings;

// just the strings for the documentation (right side of the demo window)

interface

uses ComCtrls, Graphics;

const
  CExplanations : array [0..18] of PAnsiChar = (
    'If a unit gets initialized before the exception handling is ready to go ' +
    'and then raises an exception, you will see cryptical messages like ' +
    '"Runtime error 216", no matter how great your exception handling logic ' +
    'might be. So a good exception tracker has to make sure that it is ' +
    'initialized *first*, before anything else. Only then it can reliably ' +
    'catch initialization exceptions.',

    'See "early unit initialization". However, VCL units are initialized ' +
    'quite late normally, because of the massive "uses" clause. So catching ' +
    'exceptions in the initialization of VCL units is a bit easier than in ' +
    'units with limited "uses". Nevertheless, an exception tracker has to do ' +
    'some tricks to catch exceptions here, because Delphi''s exception logic ' +
    'is in full swing only after all units are completely initialized.',

    'Exceptions which happen in this phase of the program flow are quite easy ' +
    'to catch, e.g. by using Delphi''s built in "ExceptProc" callback. Only ' +
    'dumb exception trackers should fail here.',

    'Those are the most easy to catch exceptions. Just registering an ' +
    '"Application.OnException" handler is good enough. If an exception ' +
    'tracker does not catch these exceptions, then it''s no exception ' +
    'tracker, but a placebo.',

    'See "project initialization".',

    'Finalization exception catching is quite difficult, because Delphi ' +
    'finalizes all units before finalization exceptions are passed ' +
    'through to whomever is waiting for it. But after all units are finalized, ' +
    'Delphi''s exception handling is already down. So some hacks are needed ' +
    'to get proper access to finalization exceptions.',

    'See "VCL unit finalization".',

    'The dll initialization phase is a quite sensitive one. You can create ' +
    'threads, but they only start running after the initialization is done. ' +
    'So if you create a thread during dll initialization and wait for it to ' +
    'be finished, you will freeze. If a dll raises an exception during ' +
    'initialization and doesn''t handle it itself, the application will go ' +
    'bunkers. So it''s recommended to let dlls handle their own exceptions ' +
    'instead of relying on the application to do everything. Catching of ' +
    'dll initialization exceptions is quite hard. It''s only possible inside ' +
    'of the dll, and handling the exceptions - once caught - is also hard ' +
    'because of the thread freezing.',

    'When an exception occurs inside of an exported dll function, the ' +
    'exception is just transported back to the application''s exception ' +
    'handler. So nothing special here...',

    'See "dll initialization". However, dll finalization is even worse, ' +
    'because threads created at this time will either never run (when ' +
    'talking about static dlls) or will start to run only after the dll ' +
    'is unloaded (dynamic dll finalization). In the latter case the thread ' +
    'will crash, because the thread function was unloaded together with the ' +
    'dll.' + #$D#$A +
    'The win9x family''s handling of dll finalization is extra weird: (1) ' +
    'Dlls don''t get finalized in the context of the main thread. (2) Trying ' +
    'to terminate yourself by calling ExitProcess, TerminateProcess, ' +
    'ExitThread or TerminateThread crashes the process. (3) When creating a ' +
    'new thread during finalization of a static dll, the dll gets unloaded, ' +
    'then loaded again, then the thread is started, and then the dll gets ' +
    'unloaded again...',

    '(1) Ideally an exception tracker should handle any thread''s exceptions ' +
    'without caring much about in which thread the exception occured. ' +
    'Unfortunately that''s not as easy as it sounds, because the VCL is not ' +
    'thread safe <sigh>.' + #$D#$A +
    '(2) Preferably all threads should be protected automatically, without ' +
    'that you have to add special code to each thread. This way you won''t ' +
    'need to dig through the code of 3rd party components. ' + #$D#$A +
    '(3) Finally it could be quite useful if the bug report (created by the ' +
    'exception tracker) would not only contain the thread''s callstack, but ' +
    'also information about who created the crashing thread.',

    'See "CreateThread".',

    'Basically see "CreateThread". However, handling TThread exceptions ' +
    'properly is even more difficult, because D6+ tries to transport the ' +
    'exceptions to the main thread. This way you lose the precious ' +
    'callstack of the thread. So some hacks are needed to get this working ' +
    'as expected. To increase readability the bug report should contain ' +
    'the class name of the crashing TThread.',

    'Some 3rd party components (not knowing that you''re using an exception ' +
    'tracker) might be tempted to set up a "try except" statement like shown ' +
    'in the code below. This would normally hide the exception from you. In ' +
    'order to avoid that, madExcept catches calls to "SysUtils.ShowException", ' +
    'too.',

    'See "SysUtils.ShowException".',

    'See "SysUtils.ShowException".',

    'A good exception tracker should protect the end user from being ' +
    'flooded by exception messages. If multiple exceptions occur, the end ' +
    'user should see them one by one, having the possibility to close the ' +
    'program down at any time. In order to achieve this goal, madExcept ' +
    'suspends the whole process and shows the exception box in a private ' +
    'thread.',

    'It''s not really the task of an exception tracker to detect freezes. ' +
    'But we take what we can get, don''t we? So madExcept is able to detect ' +
    'main thread freezings. Of course madExcept can''t really do anything ' +
    'against the freeze, but at least it can show where the main thread ' +
    'is hanging all the time. Since madExcept runs in a private thread, it ' +
    'has no problem with other threads being frozen.' + #$D#$A +
    'For some applications it is critical that they are always up and running. ' +
    'So madExcept offers the option to automatically restart the application, ' +
    'if an exception or a freeze is detected.',

    'Another special: When working with multiple threads, which are ' +
    'synchronized somehow you can easily produce deadlocks, if your code ' +
    'is not totally clean. Such problems are very hard to debug, since ' +
    'you have to keep an eye on multiple threads at the same time. In this ' +
    'situation madExcept can help again. If the deadlock occurs at runtime, ' +
    'madExcept detects the frozen main thread. The resulting bug report does ' +
    'not only contain the callstack of the main thread, but the callstack of ' +
    '*all* threads, which will often directly lead you to your bug.');

  CWelcomeExplanation : PAnsiChar =
    'This demo is meant to...' + #$D#$A + #$D#$A +
    '(1) ... show where exceptions can occur and what complications there might be when trying to catch them.' + #$D#$A +
    '(2) ... give you a glimpse of insight about how madExcept works internally and how good it is.' + #$D#$A +
    '(3) ... give you the ability to compare madExcept to alternative/competeting solutions.' + #$D#$A + #$D#$A +
    'For more information please visit: "<n>http://help.madshi.net/madExcept.htm</n>".';

  CSourceCodes : array [0..18] of PAnsiChar = (
    '<b>unit</b> <i>ecMini</i>;' + #$D#$A + #$D#$A +
    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<b>initialization</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "early unit initialization".''</s>);' + #$D#$A +
    '<b>end</b>.',

    '<b>unit</b> <i>ecMain</i>;' + #$D#$A + #$D#$A +
    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>uses</b>' + #$D#$A +
    '  <i>Windows</i>, <i>Messages</i>, <i>SysUtils</i>, <i>Classes</i>, <i>Graphics</i>, <i>Controls</i>, <i>Forms</i>,' + #$D#$A +
    '  <i>Dialogs</i>, <i>StdCtrls</i>, <i>ExtCtrls</i>, <i>Buttons</i>, <i>ComCtrls</i>;' + #$D#$A + #$D#$A +
    '<b>type</b>' + #$D#$A +
    '  <i>TFMainForm</i> = <b>class</b>(<i>TForm</i>) [...] <b>end</b>;' + #$D#$A + #$D#$A +
    '<b>var</b> <i>FMainForm</i>: <i>TFMainForm</i>;' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<c>{$R *.dfm}</c>' + #$D#$A + #$D#$A +
    '<b>initialization</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "VCL unit initialization".''</s>);' + #$D#$A +
    '<b>end</b>.',

    '<b>program</b> <i>ExcCatch</i>;' + #$D#$A + #$D#$A +
    '<b>uses</b>' + #$D#$A +
    '  <i>ecMini</i> <b>in</b> <s>''ecMini.pas''</s>,' + #$D#$A +
    '  <i>Forms</i>,' + #$D#$A +
    '  <i>ecMain</i> <b>in</b> <s>''eMain.pas''</s> <c>{FMainForm}</c>,' + #$D#$A +
    '  <i>ecStrings</i> <b>in</b> <s>''ecStrings.pas''</s>,' + #$D#$A +
    '  <i>madExcept</i>;' + #$D#$A + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>raise</b> <i>TException</i>.<i>Create</i>(<s>''Demo "project initialization".''</s>);' + #$D#$A +
    '  <i>Application</i>.<i>Initialize</i>;' + #$D#$A +
    '  <i>Application</i>.<i>CreateForm</i>(<i>TFMainForm</i>, <i>FMainForm</i>);' + #$D#$A +
    '  <i>Application</i>.<i>Run</i>;' + #$D#$A +
    '<b>end</b>.',

    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoDuringRunningApplication</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "during running application".''</s>);' + #$D#$A +
    '<b>end</b>;',

    '<b>program</b> <i>ExcCatch</i>;' + #$D#$A + #$D#$A +
    '<b>uses</b>' + #$D#$A +
    '  <i>ecMini</i> <b>in</b> <s>''ecMini.pas''</s>,' + #$D#$A +
    '  <i>Forms</i>,' + #$D#$A +
    '  <i>ecMain</i> <b>in</b> <s>''eMain.pas''</s> <c>{FMainForm}</c>,' + #$D#$A +
    '  <i>ecStrings</i> <b>in</b> <s>''ecStrings.pas''</s>,' + #$D#$A +
    '  <i>madExcept</i>;' + #$D#$A + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>Application</i>.<i>Initialize</i>;' + #$D#$A +
    '  <i>Application</i>.<i>CreateForm</i>(<i>TFMainForm</i>, <i>FMainForm</i>);' + #$D#$A +
    '  <i>Application</i>.<i>Run</i>;' + #$D#$A +
    '  <b>raise</b> <i>TException</i>.<i>Create</i>(<s>''Demo "project finalization".''</s>);' + #$D#$A +
    '<b>end</b>.',

    '<b>unit</b> <i>ecMain</i>;' + #$D#$A + #$D#$A +
    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>uses</b>' + #$D#$A +
    '  <i>Windows</i>, <i>Messages</i>, <i>SysUtils</i>, <i>Classes</i>, <i>Graphics</i>, <i>Controls</i>, <i>Forms</i>,' + #$D#$A +
    '  <i>Dialogs</i>, <i>StdCtrls</i>, <i>ExtCtrls</i>, <i>Buttons</i>, <i>ComCtrls</i>;' + #$D#$A + #$D#$A +
    '<b>type</b>' + #$D#$A +
    '  <i>TFMainForm</i> = <b>class</b>(<i>TForm</i>) [...] <b>end</b>;' + #$D#$A + #$D#$A +
    '<b>var</b> <i>FMainForm</i>: <i>TFMainForm</i>;' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<c>{$R *.dfm}</c>' + #$D#$A + #$D#$A +
    '<b>initialization</b>' + #$D#$A +
    '<b>finalization</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "VCL unit finalization".''</s>);' + #$D#$A +
    '<b>end</b>.',
    '<b>unit</b> <i>ecMini</i>;' + #$D#$A + #$D#$A +
    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<b>initialization</b>' + #$D#$A +
    '<b>finalization</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "late unit finalization".''</s>);' + #$D#$A +
    '<b>end</b>.',

    '<b>library</b> <i>ec</i>;' + #$D#$A + #$D#$A +
    '<b>uses</b> <i>ecDll</i> <b>in</b> <s>''ecDll.pas''</s>;' + #$D#$A + #$D#$A +
    '<b>end</b>.' + #$D#$A + #$D#$A +
    '<c>//******************************</c>' + #$D#$A + #$D#$A +
    '<b>unit</b> <i>ecDll</i>;' + #$D#$A + #$D#$A +
    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<b>initialization</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "dll initialization".''</s>);' + #$D#$A +
    '<b>end</b>.',

    '<b>library</b> <i>ec</i>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>DllExportedFunction</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "dll exported function".''</s>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>exports</b> <i>DllExportedFunction</i>;' + #$D#$A + #$D#$A +
    '<b>end</b>.' + #$D#$A + #$D#$A +
    '<c>//******************************</c>' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>DllExportedFunction</i>; <b>external</b> <s>''ec.dll''</s>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoDllExportedFunction</i>;' + #$D#$A + 
    '<b>begin</b>' + #$D#$A +
    '  <i>DllExportedFunction</i>;' + #$D#$A +
    '<b>end</b>;',

    '<b>library</b> <i>ec</i>;' + #$D#$A + #$D#$A +
    '<b>uses</b> <i>ecDll</i> <b>in</b> <s>''ecDll.pas''</s>;' + #$D#$A + #$D#$A +
    '<b>end</b>.' + #$D#$A + #$D#$A +
    '<c>//******************************</c>' + #$D#$A + #$D#$A +
    '<b>unit</b> <i>ecDll</i>;' + #$D#$A + #$D#$A +
    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<b>initialization</b>' + #$D#$A +
    '<b>finalization</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "dll finalization".''</s>);' + #$D#$A +
    '<b>end</b>.',

    '<b>procedure</b> <i>CreateThreadFunc</i>(<i>parameter</i>: <i>pointer</i>) : <i>integer</i>; <b>stdcall</b>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "CreateThread".''</s>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoCreateThread</i>;' + #$D#$A +
    '<b>var</b> <i>tid</i> : <i>dword</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>CloseHandle</i>(<i>CreateThread</i>(<b>nil</b>, <n>0</n>, @<i>CreateThreadFunc</i>, <b>nil</b>, <n>0</n>, <i>tid</i>));' + #$D#$A +
    '<b>end</b>;',

    '<b>procedure</b> <i>BeginThreadFunc</i>(<i>parameter</i>: <i>pointer</i>) : <i>integer</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "BeginThread".''</s>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoBeginThread</i>;' + #$D#$A +
    '<b>var</b> <i>tid</i> : <i>dword</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>CloseHandle</i>(<i>BeginThread</i>(<b>nil</b>, <n>0</n>, <i>BeginThreadFunc</i>, <b>nil</b>, <n>0</n>, <i>tid</i>));' + #$D#$A +
    '<b>end</b>;',

    '<b>type</b>' + #$D#$A +
    '  <i>TSimpleThread</i> = <b>class</b> (<i>TThread</i>)' + #$D#$A +
    '    <b>procedure</b> <i>Execute</i>; <b>override</b>;' + #$D#$A +
    '  <b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TSimpleThread</i>.<i>Execute</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>FreeOnTerminate</i> := <i>true</i>;' + #$D#$A + 
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "TThread".''</s>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoTThread</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>TSimpleThread</i>.<i>Create</i>(<i>false</i>);' + #$D#$A +
    '<b>end</b>;',

    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoSysUtilsShowException</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>try</b>' + #$D#$A +
    '    <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "SysUtils.ShowException".''</s>);' + #$D#$A +
    '  <b>except</b>' + #$D#$A +
    '    <i>SysUtils</i>.<i>ShowException</i>(<i>ExceptObject</i>, <i>ExceptAddr</i>);' + #$D#$A +
    '  <b>end</b>;' + #$D#$A +
    '<b>end</b>;',

    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoApplicationShowException</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>try</b>' + #$D#$A +
    '    <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "Application.ShowException".''</s>);' + #$D#$A +
    '  <b>except</b>' + #$D#$A +
    '    <i>Application</i>.<i>ShowException</i>(<i>Exception</i>(<i>ExceptObject</i>));' + #$D#$A +
    '  <b>end</b>;' + #$D#$A +
    '<b>end</b>;',

    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoApplicationHandleException</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>try</b>' + #$D#$A +
    '    <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "Application.HandleException".''</s>);' + #$D#$A +
    '  <b>except</b>' + #$D#$A +
    '    <i>Application</i>.<i>HandleException</i>(<i>Exception</i>(<i>ExceptObject</i>));' + #$D#$A +
    '  <b>end</b>;' + #$D#$A +
    '<b>end</b>;',

    '<b>interface</b>' + #$D#$A + #$D#$A +
    '<b>type</b>' + #$D#$A +
    '  <i>TFMainForm</i> = <b>class</b>(<i>TForm</i>)' + #$D#$A +
    '  <b>private</b>' + #$D#$A +
    '    <c>{ Private-Deklarationen }</c>' + #$D#$A +
    '    <b>procedure</b> <i>FlooderTimer</i>(<b>var Message</b>: <i>TMessage</i>); <b>message</b> <i>WM_TIMER</i>;' + #$D#$A +
    '  <b>end</b>;' + #$D#$A + #$D#$A +
    '<b>implementation</b>' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>FlooderTimer</i>(<b>var Message</b>: <i>TMessage</i>);' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>raise</b> <i>Exception</i>.<i>Create</i>(<s>''Demo "exception flooding".''</s>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoExceptionFlooding</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>SetTimer</i>(<i>Handle</i>, <n>777</n>, <n>500</n>, <b>nil</b>);' + #$D#$A +
    '<b>end</b>;',

    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoInfiniteLoop</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <b>while</b> <i>true</i> <b>do</b> ;' + #$D#$A +
    '<b>end</b>;',

    '<b>procedure</b> <i>TDeadlockThread</i>.<i>Execute</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>FreeOnTerminate</i> := <i>true</i>;' + #$D#$A + 
    '  <i>InitializeCriticalSection</i>(<i>FDeadlockSection</i>);' + #$D#$A +
    '  <i>EnterCriticalSection</i>(<i>FDeadlockSection</i>);' + #$D#$A +
    '  <i>Synchronize</i>(<i>Deadlock</i>);' + #$D#$A +
    '  <i>LeaveCriticalSection</i>(<i>FDeadlockSection</i>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TDeadlockThread</i>.<i>Deadlock</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>EnterCriticalSection</i>(<i>FDeadlockSection</i>);' + #$D#$A +
    '  <i>ShowMessage</i>(<s>''Can''''t reach this!''</s>);' + #$D#$A +
    '  <i>LeaveCriticalSection</i>(<i>FDeadlockSection</i>);' + #$D#$A +
    '<b>end</b>;' + #$D#$A + #$D#$A +
    '<b>procedure</b> <i>TFMainForm</i>.<i>DemoDeadlock</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>TDeadlockThread</i>.<i>Create</i>(<i>false</i>);' + #$D#$A +
    '<b>end</b>;');

  CWelcomeSourceCode : PAnsiChar =
    '<b>procedure</b> <i>TFMainForm</i>.<i>ActionBtnClick</i>(<i>Sender</i>: <i>TObject</i>);' + #$D#$A +
    '<b>var</b> <i>btn</i> : <i>TSpeedButton</i>;' + #$D#$A +
    '<b>begin</b>' + #$D#$A +
    '  <i>btn</i> := <i>FindDownButton</i>;' + #$D#$A +
    '  <b>if</b> <i>btn</i> <> <b>nil then begin</b>' + #$D#$A +
    '    <b>case</b> <i>btn</i>.<i>Tag</i> <b>of</b>' + #$D#$A +
    '      <n>0</n> : <i>DemoEarlyUnitInitialization</i>;' + #$D#$A +
    '      <n>1</n> : <i>DemoVclUnitInitialization</i>;' + #$D#$A +
    '      <n>2</n> : <i>DemoProjectInitialization</i>;' + #$D#$A +
    '      <c>// [..]</c>' + #$D#$A +
    '    <b>end</b>;' + #$D#$A +
    '  <b>end else</b>' + #$D#$A +
    '    <i>MessageBox</i>(<n>0</n>,' + #$D#$A +
    '               <s>''Please choose an action type in the left area.''</s>,' + #$D#$A +
    '               <s>''Information...''</s>,' + #$D#$A +
    '               <i>MB_ICONINFORMATION</i>);' + #$D#$A +
    '<b>end</b>;';

// fill the rich edit with colors
// <b>bold</b>, <s>string</s>, <n>number</n>, <c>comment</c>, <i>identifier</i>
procedure FillRichEdit (edit: TRichEdit; text: string);

implementation

procedure FillRichEdit(edit: TRichEdit; text: string);
var i1 : integer;
begin
  edit.Clear;
  while true do begin
    i1 := Pos('<', text);
    if i1 > 0 then begin
      if text[i1 + 1] <> '>' then begin
        edit.SelText := Copy(text, 1, i1 - 1);
        with edit.SelAttributes do
          case text[i1 + 1] of
            'b' : begin Style := [fsBold];   Color := clBlack  end;
            'i' : begin Style := [];         Color := clMaroon end;
            's' : begin Style := [];         Color := clGreen  end;
            'c' : begin Style := [fsItalic]; Color := clNavy   end;
            'n' : begin Style := [];         Color := clPurple end;
          end;
        Delete(text, 1, i1 + 2);
        i1 := Pos('</', text);
        edit.SelText := Copy(text, 1, i1 - 1);
        Delete(text, 1, i1 + 3);
        with edit.SelAttributes do begin
          Style := [];
          Color := clBlack;
        end;
      end else begin
        edit.SelText := Copy(text, 1, i1);
        Delete(text, 1, i1);
      end;
    end else
      break;
  end;
  edit.SelText := Copy(text, 1, Length(text));
end;

end.
