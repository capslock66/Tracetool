var TTrace = new ActiveXObject("TraceToolCom.XTrace");

TTrace.Debug.Send("hello from jScript") ;
TTrace.Debug.SendObject("SendObject TTrace", TTrace) ;
TTrace.Debug.SendValue("SendValue TTrace", TTrace, "TTrace") ;



