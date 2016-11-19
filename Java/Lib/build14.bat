rem : change the path to point to your java compiler
copy ..\Src\tracetool\*.class tracetool\*.class

"C:\Program Files (x86)\Java\jdk1.6.0_13\bin\jar" cfv Log4JAppender.jar tracetool\Log4JAppender.class
del tracetool\Log4JAppender.class
"C:\Program Files (x86)\Java\jdk1.6.0_13\bin\jar" cfv tracetool.jar tracetool\*.class
pause