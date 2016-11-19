{
  Include that unit in your applicatin just after the TraceTool unit to enable socket mode.

  You must have Indy 10 (see comment for indy 9) components installed to use that unit.
  Socket mode is not include by default in tracetool for 2 reasons :
  - Previous delphi version compatibility that don't include indy
  - Target size.

  Author : Thierry Parent
}

unit SocketTrace9;

interface

uses SysUtils, TraceTool, IdTCPClient ;

type
   TIndySocketTraces = class (TSocketTrace)
   private
      fPort: integer ;
      fHost: string;
      constructor Create;
   public
      procedure SetHost(const Value: string); override ;
      procedure SetPort(const Value: integer); override ;
      procedure Send (str : string; nbchar : integer); override ;

      procedure InitClientSocket ;
      procedure CloseSocket;
   end ;


implementation

// Indy 10
//uses IdStreamVCLBase;

var
   ClientSocket : TIdTCPClient ;
   SocketTraces : TInDySocketTraces ;

//------------------------------------------------------------------------------

constructor TIndySocketTraces.Create;
begin
   fHost := TTrace.Options.SocketHost ;
   fPort := TTrace.Options.SocketPort ;       
end;

//------------------------------------------------------------------------------

procedure TIndySocketTraces.Send(str: string; nbchar: integer);
var
   Header : string ; // header for socket connection
begin
   if ClientSocket = nil then
      SocketTraces.InitClientSocket ;

   // to do : check if the connection is in error.
   // if true , try connect to connect every 5 secondes only. (messages are lost)
   if ClientSocket.Connected = false then
      ClientSocket.Connect ;

   Header := inttostr (nbchar) + #0 ;

   // in Indy 10, we must send the header and the string separately
   //ClientSocket.IOHandler.write (Header) ;
   //ClientSocket.IOHandler.write (str) ;

   // with indy 9 : we can concatene the header with the string and send the new one
   str := Header + str ;
   ClientSocket.WriteBuffer (PChar(str)^,nbchar+length(Header),true);  // true : force send now
end ;

//------------------------------------------------------------------------------

procedure TIndySocketTraces.SetHost(const Value: string);
begin
  fHost := Value;
  InitClientSocket ;
end;

//------------------------------------------------------------------------------

procedure TIndySocketTraces.SetPort(const Value: integer);
begin
  fPort := Value;
  InitClientSocket ;
end;

//--------------------------------------------------------------------------------------------------

procedure TIndySocketTraces.InitClientSocket;
begin
   if ClientSocket = nil then
      ClientSocket := TIdTCPClient.Create (nil) ;

   with ClientSocket do begin
     Host := fHost;
     Port := fPort;

     // Indy 10
     // ReadTimeout := RECIEVETIMEOUT ;
   end ;
end;


//------------------------------------------------------------------------------

procedure TIndySocketTraces.CloseSocket;
begin
   if ClientSocket = nil then
      exit ;
   ClientSocket.Disconnect ;
   ClientSocket.free ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------



initialization
   SocketTraces := TInDySocketTraces.Create ;
   AlternateSend := SocketTraces ;           // the procedure to send messages
   TTrace.Options.SendMode := tmAlternate ;   // force the use of alternate mode (socket)

finalization

   SocketTraces.CloseSocket ;

end.
