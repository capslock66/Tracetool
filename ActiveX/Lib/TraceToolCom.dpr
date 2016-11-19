library TraceToolCom;

uses
  ComServ,
  TraceToolCom_TLB in 'TraceToolCom_TLB.pas',
  unt_XTrace in 'unt_XTrace.pas' {XTrace: CoClass},
  unt_XTraceNodeEx in 'unt_XTraceNodeEx.pas' {XTraceNodeEx: CoClass},
  unt_XTraceNode in 'unt_XTraceNode.pas' {XTraceNode: CoClass},
  unt_XMemberNode in 'unt_XMemberNode.pas' {XMemberNode: CoClass},
  unt_XWinWatch in 'unt_XWinWatch.pas' {XWinWatch: CoClass},
  unt_XTraceOptions in 'unt_XTraceOptions.pas' {XTraceOptions: CoClass},
  unt_XWinTrace in 'unt_XWinTrace.pas' {XWinTrace: CoClass},
  unt_XTraceSend in 'unt_XTraceSend.pas' {XTraceSend: CoClass},
  unt_XTraceTable in 'unt_XTraceTable.pas' {XTraceTable: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
