{

  Display EventLog file to open
  =================================================================

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   
}

unit unt_selectEvent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TFrmSelectEvent = class(TForm)
    ScrollBox: TScrollBox;
    butOk: TButton;
    butCancel: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSelectEvent: TFrmSelectEvent;

  eventFiles : TStringList;
  EventForm : TStringList ;


implementation

uses registry, EventLog , Unt_Tool;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrmSelectEvent.FormCreate(Sender: TObject);
var
   reg : TRegistry;
   c  : integer ;                         
   RadioButton: TRadioButton;
begin
   reg := TRegistry.Create (KEY_READ);
   try
      reg.RootKey := HKEY_LOCAL_MACHINE;      
      if reg.OpenKey ('System\CurrentControlSet\Services\EventLog', False) then
      begin
         reg.GetKeyNames (eventFiles);
         for c := 0 to eventFiles.Count - 1 do begin
            RadioButton := TRadioButton.Create (self) ;
            RadioButton.Caption := eventFiles [c] ;
            RadioButton.tag := c ;
            RadioButton.Top := 5 + ( c * 20) ;
            RadioButton.left := 5 ;
            RadioButton.Width := ScrollBox.Width- 10 ;
            RadioButton.Anchors := [akLeft,akTop,akRight] ;
            RadioButton.parent := ScrollBox ;
            eventFiles.Objects [c] := RadioButton ;
         end ;
      end ;
   finally
      reg.Free ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmSelectEvent.FormShow(Sender: TObject);
var
   isOneValid : boolean ;
   c : integer ;
   RadioButton: TRadioButton;
begin
   isOneValid := false ;
   for c := 0 to eventFiles.Count - 1 do begin
      RadioButton := TRadioButton (eventFiles.Objects [c]) ;
      RadioButton.Checked := false ;
      if EventForm.IndexOf(eventFiles[c]) <> -1 then
         RadioButton.Enabled := false
      else begin
         RadioButton.Enabled := true ;
         isOneValid := true ;
      end ;
   end ;
   butOk.Enabled := isOneValid ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

Initialization
   eventFiles := TStringList.Create;
   EventForm := TStringList.Create;

finalization
  eventFiles.Clear;
  EventForm.Clear ;

  eventFiles.free ;
  EventForm.free ;
end.
