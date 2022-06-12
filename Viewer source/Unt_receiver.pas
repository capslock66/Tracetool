{
  Receive windows messages from the client

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   
}

unit Unt_receiver;

interface

uses
  system.Contnrs, system.SyncObjs, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees; 

{$Include TraceConst.Inc}

type
  TFormReceiver = class(TForm)
    procedure WMCopyData(var Message: TMessage); message WM_COPYDATA ;
    procedure FormCreate(Sender: TObject);
    //procedure ReceivedSetText(var Message: TWMSetText); message WM_SETTEXT;
  private
  public
  end;

  procedure SplitMessage (pAnsiMsg : PAnsichar; LenMsg : integer=0) ;

var
  FormReceiver: TFormReceiver;

implementation

uses Unt_Tool, unt_TraceWin;

{$R *.dfm}

procedure SplitMessage (pAnsiMsg : PAnsichar; LenMsg : integer) ;
var
   errorNode : pvirtualNode ;
   MsgList: TstringList ;
   PAnsiMsgBackup : PAnsichar ;
   pAnsiMsgEnd    : PAnsichar ;

   pWideMsg       : PChar ;
   pWideMsgEnd    : PChar ;

   TempStr : string ;

begin
   if pAnsiMsg = nil then     // nothing to do
      Exit ;

   PAnsiMsgBackup := pAnsiMsg ;
   MsgList := TStringList.create ;   // freed by TimerTracesTimer after processing the message
   try

      // UTF16 - unicode : FE FF  (stored in reverse order : FF FE)
      // UTF8 - RTF      : EF BB BF
      if ((pAnsiMsg+0)^ = #255) and ((pAnsiMsg+1)^ = #254) then begin
         inc(pAnsiMsg);    // bypass unicode header
         inc(pAnsiMsg);
         PAnsiMsgBackup := pAnsiMsg ;    // also skip header in case of error

         pWideMsg := PChar(pAnsiMsg) ;
         while pWideMsg^ <> #0 do begin
            pWideMsgEnd := pWideMsg ;
            // detect end of string
            While pWideMsgEnd^ <> #0 do
               inc (pWideMsgEnd) ;

            // perform a copy of the string up to the null term to ensure no extra chars are added
            TempStr := copy (pWideMsg,1, pWideMsgEnd-pWideMsg) ;
            MsgList.Add (TempStr);

            pWideMsg := pWideMsgEnd + 1 ;   // get next sub message and skip the null term.
         end ;

      end else begin
         while pAnsiMsg^ <> #0 do begin
            pAnsiMsgEnd := pAnsiMsg ;

             // detect end of string
            While pAnsiMsgEnd^ <> #0 do
               inc (pAnsiMsgEnd) ;

            // perform a copy of the string up to the null term to ensure no extra chars are added
            TempStr := string(copy (pAnsiMsg,1, pAnsiMsgEnd-pAnsiMsg)) ;
            MsgList.Add (TempStr);

            pAnsiMsg := pAnsiMsgEnd + 1 ;   // get next sub message and skip the null term.

         end ;
      end;
   except
      on E:Exception do begin
         errorNode := TFrm_Trace.InternalTrace(e.Message) ;
         TFrm_Trace.InternalTrace(errorNode, 'SplitMessage source : ' + PAnsiMsgBackup) ;
      end ;
   end ;

   //Frm_Trace.InternalTraceFromThread('SplitMessage start') ;
   criticalsection.Enter ;
   try
      MessageStack.Add(MsgList) ;
   finally
      criticalsection.Leave ;
   end;
   //Frm_Trace.InternalTraceFromThread('SplitMessage end') ;

   Frm_Tool.TimerTraces.Enabled := true ;
end;

//------------------------------------------------------------------------------

{ TFormReceiver }

procedure TFormReceiver.WMCopyData(var Message: TMessage);
var
   pCDS           : PCopyDataStruct;              // what we receive
   TypeMsg        : integer ;                     // type of message in pCDS
   LenMsg : integer ;
begin
   Message.Result := 0 ;

   pCDS := PCopyDataStruct(Message.lParam) ;

   LenMsg   := pCDS^.cbData ;
   TypeMsg  := pCDS^.dwData ;

   // accept only WMD_ACTION
   if TypeMsg <> WMD then
      exit ;
   SplitMessage (PAnsichar(pCDS^.lpData), LenMsg) ;
end;

//------------------------------------------------------------------------------

// at design time, the caption is blank.
// setting 'FormReceiver' at run time will ensure that we will no use the design time form
// The design form of course will not respond to message since it's a Delphi IDE form
procedure TFormReceiver.FormCreate(Sender: TObject);
begin
   Caption := 'FormReceiver' ;
end;


//function TFrm_Trace.SendToClient (const Msg: TstringList) : Longint ;
//var
//   CDS: TCopyDataStruct;
//   MessageString: string;
//   i : integer ;
//   tot : integer ;
//begin
//
//   result := -1 ;
//   if componentHandle = 0 then begin
//      Application.MessageBox('No program callback defined' , 'Trace utility') ;
//      exit ;
//   end ;
//
//   MessageString := '' ;
//   tot := 0 ;
//   for i := 0 to Msg.Count -1 do begin
//      tot := tot + Length(Msg.Strings[i]) + 1 ;
//      MessageString := MessageString + Msg.Strings[i] + #0;
//   end ;
//   inc (tot);
//
//   MessageString := MessageString + #0 ;
//
//   CDS.cbData := tot ;
//   CDS.dwData := WMD ;   // identification code 'WebModuleDebug'
//   CDS.lpData := pchar (MessageString); // no need to add #0, because String are null terminated
//
//   result := SendMessage(componentHandle, WM_COPYDATA, WParam(Application.Handle), LParam(@CDS));
//end;


end.
