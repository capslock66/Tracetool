unit ecDll;

// in the project (dpr) we have no finalization, so we need a unit in our dll

interface

implementation

uses Windows, ecCrashParam, madTypes, madExcept;

procedure ClearRestartCommandLine(action: TExceptAction; const exceptIntf: IMEException; var handled: boolean);
begin
  if action = eaRestartApplication then
    // we don't the restarted process to start with no parameters
    RestartApplication(ParamStr(0));
end;

// does the crash parameter say that our dll should raise an exception?
initialization
  RegisterExceptActionHandler(ClearRestartCommandLine, stDontSync);
  if CrashParam = '/DllInitialization' then 
    raise MadException.Create('Demo "dll initialization".');
finalization
  if CrashParam = '/DllFinalization' then
    raise MadException.Create('Demo "dll finalization".');
end.
