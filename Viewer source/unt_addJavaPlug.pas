{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}
unit unt_addJavaPlug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unt_plugin;

type
  TFrmAddJavaPlugin = class(TForm)
    Label2: TLabel;
    EditClassName: TEdit;
    butOk: TButton;
    butCancel: TButton;
    butCheck: TButton;
    procedure butCheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAddJavaPlugin: TFrmAddJavaPlugin;

implementation

{$R *.dfm}

procedure TFrmAddJavaPlugin.butCheckClick(Sender: TObject);
//var
//   FullClassName : string ;
begin
//   FullClassName := TJavaPlugin.checkClass(EditClassName.Text);
//   if FullClassName = '' then
//      MessageDlg('Class don''t exist', mtError, [mbOK], 0)
//   else
//      MessageDlg('Class location :' + FullClassName, mtInformation, [mbOK], 0);
end;

end.
