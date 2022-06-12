{This sample library exports a single call that will leak a TObject.}

library TestLibrary;

{$define NoMessageBoxes}

{$define FastMM_EnableMemoryLeakReporting}
{$define FastMM_NoMessageBoxes}

uses
  FastMMInitSharing,
  FastMM5 in 'FastMM5.pas';

procedure LeakMemory;
begin
  TObject.Create;
end;

exports LeakMemory;

begin
  ReportMemoryLeaksOnShutdown := True;
end.
