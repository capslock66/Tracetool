program TestApp;

{$APPTYPE CONSOLE}

{$define NoMessageBoxes}
{$define FastMM_NoMessageBoxes}

uses
  FastMMInitSharing,
  System.Classes,
  FastMM5 in 'FastMM5.pas';

{Note that TestLibrary.dll is statically linked, so it will be initialized before the main application.  This means the
main application will actually be sharing the memory manager of the DLL.  (If TestLibrary was loaded dynamically then
it would be sharing the memory manager of the main application.)}
//procedure LeakMemory; external 'TestLibrary';

begin

  //ReportMemoryLeaksOnShutdown := false;  // when true, force include mmetUnexpectedMemoryLeakSummary in FastMM_MessageBoxEvents in

  FastMM_SetEventLogFilename('c:\temp\toto.log');
  FastMM_DeleteEventLogFile();
  FastMM_LogToFileEvents := FastMM_LogToFileEvents + [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
  FastMM_MessageBoxEvents := [];


  {Leak a TPersistent in the main application}
  TPersistent.Create;


  {Leak a TObject in the library}
  //LeakMemory;


end.
