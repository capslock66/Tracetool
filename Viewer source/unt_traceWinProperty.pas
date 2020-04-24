{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_traceWinProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  unt_tool;

type
  TFrmTraceWinProp = class(TForm)
    butOk: TButton;
    rbLogEnabled: TRadioButton;
    rbLogIsDisabled: TRadioButton;
    rbDaily: TRadioButton;
    Label1: TLabel;
    editFilename: TEdit;
    butFile: TButton;
    butCancel: TButton;
    Label2: TLabel;
    EditMaxLines: TEdit;
    procedure butFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmTraceWinProp: TFrmTraceWinProp;

implementation

uses unt_TraceConfig ;

{$R *.dfm}

procedure TFrmTraceWinProp.butFileClick(Sender: TObject);
begin
   Frm_Tool.OpenDialog1.Filter := 'Xml file (*.xml)|*.xml' ;
   Frm_Tool.OpenDialog1.InitialDir := TraceConfig.General_LastSavedPath ;
   if Frm_Tool.OpenDialog1.Execute = false then
      exit ;
   TraceConfig.General_LastSavedPath := ExtractFilePath(Frm_Tool.OpenDialog1.FileName) ;
   editFilename.Text := Frm_Tool.OpenDialog1.FileName ;
end;

end.
