{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_search;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrmSearch = class(TForm)
    Label1: TLabel;
    EditSearch: TEdit;
    grpOptions: TGroupBox;
    chkMatchCase: TCheckBox;
    chkMatchWholeWord: TCheckBox;
    chkSearchUp: TCheckBox;
    grpLookIn: TGroupBox;
    rbCurrent: TRadioButton;
    rbAllDocs: TRadioButton;
    butFindNext: TButton;
    butHightlightAll: TButton;
    butCancel: TButton;
    procedure FormShow(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override ;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSearch: TFrmSearch;

  MatchCase : boolean ;
  MatchWholeWord : boolean ;
  SearchInAllPages : boolean ;
  SearchText : string ;
  UpperCaseSearchText : string ;
  LenSearchText : integer ;
  SearchKind : integer ;

function MatchSearch (Line : string) : integer ;

implementation

{$R *.dfm}

function MatchSearch (Line : string) : integer ;
const
  delimiter = ['*','@','.','_','-',' ', '$','!','''','(',')',#10,#13 ];
var
   leftChar, rightChar : char ;
begin
   if MatchCase then
      result := Pos(SearchText, Line)
   else
      result := Pos(UpperCaseSearchText, AnsiUpperCase(Line)) ;

   if result = 0 then
      exit ;

   if not MatchWholeWord then
      exit ;

   if result = 1 then  // find at the begining
      leftChar := ' '
   else
      leftChar := Line[result-1] ;

   if result + LenSearchText -1 = length (Line) then
      rightChar := ' '
   else
      rightChar := Line[result+LenSearchText] ;

   //if not (leftChar in delimiter) or not (rightChar in delimiter) then
   if not (CharInSet (leftChar,delimiter)) or not (CharInSet(rightChar, delimiter)) then
      result := 0 ;
end ;

//------------------------------------------------------------------------------

procedure TFrmSearch.CreateParams(var Params: TCreateParams);
begin
   inherited ;
   //Params.style := Params.style or WS_CAPTION ;
   //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW ;
   Params.WndParent := 0 ;   // window is independant of the main window (for example minimize)
end;

//------------------------------------------------------------------------------

procedure TFrmSearch.FormShow(Sender: TObject);
begin
   EditSearch.SetFocus ;
end;


end.
