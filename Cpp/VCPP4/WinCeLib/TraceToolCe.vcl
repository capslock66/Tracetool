<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: TraceToolCe - Win32 (WCE ARMV4) Release--------------------
</h3>
<h3>Command Lines</h3>
Creating temporary file "c:\Temp\RSP174.tmp" with contents
[
/nologo /W3 /D _WIN32_WCE=420 /D "WCE_PLATFORM_STANDARDSDK" /D "NDEBUG" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D "UNICODE" /D "_UNICODE" /D "_LIB" /FR"ARMV4Rel/" /Fp"ARMV4Rel/TraceToolCe.pch" /YX /Fo"ARMV4Rel/" /O2 /MC /c 
"D:\TraceTool\CPP\Source\tracetool.cpp"
]
Creating command line "clarm.exe @c:\Temp\RSP174.tmp" 
Creating command line "link.exe -lib /nologo /out:"ARMV4Rel\TraceToolCe.lib"  .\ARMV4Rel\tracetool.obj "
<h3>Output Window</h3>
Compiling...
tracetool.cpp
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(196) : warning C4786: '?$reverse_iterator@Vconst_iterator@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@3@ABV43@PBV43@H' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(209) : warning C4786: '??0?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@QAA@IABV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@1@ABV?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@1@@Z' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(221) : warning C4786: '??0?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@QAA@Vconst_iterator@01@0ABV?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@1@@Z' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(243) : warning C4786: '?rbegin@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@QAA?AV?$reverse_iterator@Viterator@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@3@AAV43@PAV43@H@2@XZ' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(245) : warning C4786: '?rbegin@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@QBA?AV?$reverse_iterator@Vconst_iterator@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@3@ABV43@PBV43@H@2@XZ' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(247) : warning C4786: '?rend@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@QAA?AV?$reverse_iterator@Viterator@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@3@AAV43@PAV43@H@2@XZ' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\deque(248) : warning C4786: '?rend@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@QBA?AV?$reverse_iterator@Vconst_iterator@?$deque@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@3@ABV43@PBV43@H@2@XZ' : identifier was truncated to '255' characters in the browser information
        d:\tracetool\cpp\source\tracetool.h(170) : see reference to class template instantiation 'std::deque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
        d:\tracetool\cpp\source\tracetool.cpp(308) : see reference to class template instantiation 'CommandDeque<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >' being compiled
c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\xstring(724) : warning C4530: C++ exception handler used, but unwind semantics are not enabled. Specify -GX
        c:\program files\windows ce tools\wce420\standardsdk_420\include\armv4\xstring(720) : while compiling class-template member function 'void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Copy(unsigned int)'
Creating library...




<h3>Results</h3>
TraceToolCe.lib - 0 error(s), 8 warning(s)
</pre>
</body>
</html>
