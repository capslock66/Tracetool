{
  Include that unit in your applicatin just after the TraceTool unit to enable socket mode.

  You must have Indy 10 (see comment for indy 9) components installed to use that unit.
  Socket mode is not include by default in tracetool for 2 reasons :
  - Previous delphi version compatibility that don't include indy
  - Target size.

  Author : Thierry Parent
}

unit SocketTrace;

interface

uses SysUtils, TraceTool, IdTCPClient, idUdpClient, IdGlobal, IdException, windows ;

type
   TIndySocketTraces = class (TSocketTrace)
   private
      constructor Create;
   public
      procedure InitClientSocket ; override ;
      procedure Send (const str : string; const nbchar : integer); override ;
      procedure CloseSocket;
   end ;


implementation

var
   ClientSocket : TIdTCPClient ;
   ClientSocketUdp : TIdUdpClient ;
   SocketTraces : TInDySocketTraces ;
   LastError : string ;

//------------------------------------------------------------------------------

constructor TIndySocketTraces.Create;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TIndySocketTraces.InitClientSocket;
begin
   //ClientSocket.Disconnect;
   //ClientSocketUdp.Disconnect ;
   if TTrace.Options.SocketUdp then begin
      if ClientSocketUdp = nil then
         ClientSocketUdp := TIdUdpClient.Create (nil) ;
      ClientSocketUdp.Host := TTrace.Options.SocketHost;
      ClientSocketUdp.Port := TTrace.Options.SocketPort;
   end else begin
      if ClientSocket = nil then
         ClientSocket := TIdTCPClient.Create (nil) ;
      ClientSocket.Host := TTrace.Options.SocketHost;
      ClientSocket.Port := TTrace.Options.SocketPort;
      ClientSocket.ReadTimeout := RECIEVETIMEOUT ;
   end;
end;

//------------------------------------------------------------------------------

procedure TIndySocketTraces.CloseSocket;
begin
   if ClientSocket <> nil then begin
      ClientSocket.Disconnect ;
      ClientSocket.free ;
   end;
   if ClientSocketUdp <> nil then begin
      ClientSocketUdp.Disconnect ;
      ClientSocketUdp.free ;
   end;
end;

//------------------------------------------------------------------------------

procedure TIndySocketTraces.Send(const str : string; const nbchar : integer);
var
   //Header : string ; // header for socket connection
   LBytes : TIdBytes ;
   AnsiPtr : PAnsiChar ;      // pointer to the buffer (as ansiString)
   SourcePtr , DestPtr : PChar ;
   pWordLength : pDword ;     // pointer to a DWORD containing the message length
   WordLength : DWord ;
   c : integer ;
begin
   if (ClientSocket = nil) and (ClientSocketUdp = nil) then
      SocketTraces.InitClientSocket ;

   // change since version 11 ; the header is no more a string (single byte) representing the length followed by  a #0 char,
   // but the WMd byte (123) followed by a DWORD (4 bytes) filled by the message length

   WordLength := nbchar*2 ;
   SetLength(LBytes, 5 + WordLength);
   LBytes[0] := WMD ;

   AnsiPtr := pAnsiChar(LBytes) ;
   inc(AnsiPtr) ;
   pWordLength := pDword (AnsiPtr) ;
   pWordLength^ := WordLength ;

   // jump over message length
   inc(AnsiPtr,4) ;
   SourcePtr := PChar(str) ;
   DestPtr   := PChar(AnsiPtr) ;
   // copy str to buffer
   for c := 0 to nbchar - 1 do begin
      DestPtr^ := SourcePtr^ ;
      inc(DestPtr) ;
      inc(SourcePtr) ;
   end;

   // send to viewer. No response needed
   if TTrace.Options.SocketUdp then begin
      try
         ClientSocketUdp.SendBuffer(ClientSocketUdp.Host,ClientSocketUdp.Port, LBytes)
      except
         // may generate EIdPackageSizeTooBig exception if the message is too long
         on e:EIdPackageSizeTooBig do begin
            LastError := e.Message ;
         end ;
      end;
   end else begin
      // to do : check if the connection is in error.
      // if true , try connect to connect every 5 secondes only. (messages are lost)
      if ClientSocket.Connected = false then
         ClientSocket.Connect ;
      ClientSocket.IOHandler.write (LBytes) ;
   end;
end ;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
   SocketTraces := TInDySocketTraces.Create ;
   AlternateSend := SocketTraces ;           // the procedure to send messages
   TTrace.Options.SendMode := tmAlternate ;   // force the use of alternate mode (socket)

finalization

   SocketTraces.CloseSocket ;
   // if you free the SocketTraces, you application can be unstable. It's a Indy problem that don't report correctly leaks.

end.
