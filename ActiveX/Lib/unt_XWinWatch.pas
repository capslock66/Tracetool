// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information

unit unt_XWinWatch;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, tracetool;

type
  TXWinWatch = class(TAutoObject, IXWinWatch) //   TInterfacedObject
  protected
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Tag: SYSINT; safecall;
    procedure ClearAll; safecall;
    procedure DisplayWin; safecall;
    procedure Send(const WatchName: WideString; v: OleVariant); safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    function Get_Id: WideString; safecall;
    procedure Set_Id(const Value: WideString); safecall;
    procedure Close; safecall;
  public
    link : IWinWatch ;
//    procedure Initialize; override;
    constructor create () ; overload ;
    constructor create (const WinWatchId, WinWatchText: WideString) ; overload ;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

//------------------------------------------------------------------------------

// singleton constructor for ttrace.watches
constructor TXWinWatch.create();
begin
   //ttrace.error.send ('TXWinWatch.create singleton', integer(pointer(self))) ;
   inherited create();
   link := TTrace.watches ;
end;

//------------------------------------------------------------------------------

constructor TXWinWatch.create(const WinWatchId, WinWatchText: WideString);
begin
   //ttrace.error.send ('TXWinWatch.create', integer(pointer(self))) ;
   inherited create();
   link := TTrace.createWinWatch (WinWatchID, WinWatchText) ;
end;

//------------------------------------------------------------------------------

//procedure TXWinWatch.Initialize;
//begin
//   //ttrace.error.send ('TXWinWatch.Initialize', integer(pointer(self))) ;
//   inherited;
//end;

//------------------------------------------------------------------------------

destructor TXWinWatch.Destroy;
begin
   //ttrace.error.send ('TXWinWatch.Destroy', integer(pointer(self))) ;
   //ttrace.Flush();
   link := nil ;   // free internal WinWatch
   inherited;
end;

//------------------------------------------------------------------------------

function TXWinWatch.Get_Enabled: WordBool;
begin
   result := link.Enabled ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.Set_Enabled(Value: WordBool);
begin
   link.Enabled := value ;
end;

//------------------------------------------------------------------------------

function TXWinWatch.Get_Tag: SYSINT;
begin
   result := link.Tag ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.Set_Tag(Value: SYSINT);
begin
   link.Tag := Value ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.ClearAll;
begin
   link.ClearAll() ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.DisplayWin;
begin
   link.DisplayWin() ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.Send(const WatchName: WideString; v: OleVariant);
begin
   link.Send(WatchName,v);
end;

//------------------------------------------------------------------------------

function TXWinWatch.Get_Id: WideString;
begin
   result := link.id ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.Set_Id(const Value: WideString);
begin
   link.id := value ;
end;

//------------------------------------------------------------------------------

procedure TXWinWatch.Close;
begin
   link.Close() ;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TXWinWatch, Class_XWinWatch, ciMultiInstance, tmApartment);
end.
