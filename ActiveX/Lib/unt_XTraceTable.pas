// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license informationunit unt_XTraceOptions;

unit unt_XTraceTable;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes , ComObj, ActiveX, TraceToolCom_TLB, StdVcl,
  tracetool ;

type
  TXTraceTable = class(TAutoObject, IXTraceTable)
  protected
    function Get_RowCount: Integer; safecall;
    function GetRowData(Row: Integer): WideString; safecall;
    function GetTitle: WideString; safecall;
    procedure AddColumnTitle(const ColTitle: WideString); safecall;
    procedure AddRow; safecall;
    procedure AddRowData(const Cell: WideString); safecall;
  private
      fRows : TStringList ;
      fCurrentRow : integer ;
      fTitle : string ;
  public
    constructor Create;
    destructor Destroy; override ;

  end;

implementation

uses ComServ;

//------------------------------------------------------------------------------

constructor TXTraceTable.create();
begin
   inherited create();   // TComObject.Create
   fRows := TStringList.create() ;
   fCurrentRow := -1 ;
end;

//------------------------------------------------------------------------------

destructor TXTraceTable.destroy;
begin
   inherited Destroy() ;    // TComObject.Destroy
   fRows.free ;
   fRows := nil ;
end;

//------------------------------------------------------------------------------

procedure TXTraceTable.AddColumnTitle(const ColTitle: WideString);
begin
   if fTitle = '' then
      fTitle := ColTitle
   else
      fTitle := fTitle + #9 + ColTitle ;
end;

//------------------------------------------------------------------------------

procedure TXTraceTable.AddRow;
begin
   fCurrentRow := fRows.Add('') ;
end;

//------------------------------------------------------------------------------

procedure TXTraceTable.AddRowData(const Cell: WideString);
var
   currentRowString : string ;
begin
   if fCurrentRow = -1 then
      AddRow() ;

   currentRowString := fRows.Strings[fCurrentRow] ;
   if currentRowString = '' then
      fRows.Strings[fCurrentRow] := cell
   else
      fRows.Strings[fCurrentRow] := currentRowString + #9 + cell ;

end;

//------------------------------------------------------------------------------

function TXTraceTable.Get_RowCount: Integer;
begin
   result := fRows.Count ;
end;

//------------------------------------------------------------------------------

function TXTraceTable.GetRowData(Row: Integer): WideString;
begin
   if row < fRows.count then
      result := fRows.Strings[Row] ;
end;

//------------------------------------------------------------------------------

function TXTraceTable.GetTitle: WideString;
begin
   result := fTitle ;
end;

//------------------------------------------------------------------------------

initialization
  TAutoObjectFactory.Create(ComServer, TXTraceTable, Class_XTraceTable,
    ciSingleInstance, tmFree);
end.
