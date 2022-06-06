unit ecMini;

// this unit has no imports and is listed first in the project's uses clause
// as a result it gets initialized very early and finalized very late
// that's good to put exception trackers to a test

interface

implementation

uses madExcept, ecCrashParam, madTypes;

procedure ClearRestartCommandLine(action: TExceptAction; const exceptIntf: IMEException; var handled: boolean);
begin
  if action = eaRestartApplication then
    // we don't the restarted process to start with no parameters
    RestartApplication(ParamStr(0));
end;

// does the crash parameter say that our mini unit should raise an exception?
initialization
  RegisterExceptActionHandler(ClearRestartCommandLine, stDontSync);
  if CrashParam = '/EarlyUnitInitialization' then
    raise MadException.Create('Demo "early unit initialization".');
finalization
  if CrashParam = '/LateUnitFinalization' then
    raise MadException.Create('Demo "late unit finalization".');
end.
