{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit unt_saveDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, CheckLst, Buttons, DebugOptions;

type
  TFrmSave = class(TForm)
    rbXML: TRadioButton;
    rbTextFile: TRadioButton;
    butSave: TButton;
    butCancel: TButton;
    Label1: TLabel;
    EditXml: TEdit;
    EditText: TEdit;
    chkIncludeTitle: TCheckBox;
    CheckOptionsList: TCheckListBox;
    butxml: TBitBtn;
    butText: TBitBtn;
    Label2: TLabel;
    EditStyleSheet: TEdit;
    procedure rbTextFileClick(Sender: TObject);
    procedure rbXMLClick(Sender: TObject);
    procedure ButXMLClick(Sender: TObject);
    procedure butTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure butSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IsMultiColTree : boolean ;
  end;

var
  FrmSave: TFrmSave;
  SaveTofileOptions : TSaveTofileOptions ;
  DefaultSaveTofileOptions : TSaveTofileOptions ;

implementation

uses Unt_Tool, Config;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrmSave.FormCreate(Sender: TObject);
begin
   // default save to file options (using the framework) : save all

   DefaultSaveTofileOptions := TSaveTofileOptions.Create;
   DefaultSaveTofileOptions.Copy_ProcessName      := true ;   // save process name in clipboard export
   DefaultSaveTofileOptions.Copy_ThreadID         := true ;   // save thread id
   DefaultSaveTofileOptions.Copy_Time             := true ;   // save time
   DefaultSaveTofileOptions.Copy_Col1             := true ;   // save tree column
   DefaultSaveTofileOptions.Copy_Col2             := true ;   // save comment
   DefaultSaveTofileOptions.Copy_ColumnTitle      := true ;   // include column title in clipboard export

   // user defined save to file options (based on Clipboard options)

   CheckOptionsList.Items.Clear ;
   SaveTofileOptions := TSaveTofileOptions.Create;

   SaveTofileOptions.Copy_ProcessName := XMLConfig.TextExport.ProcessName.value  ;   // save process name in clipboard export
   CheckOptionsList.Items.Add('Process Name');
   CheckOptionsList.Checked[0] := XMLConfig.TextExport.ProcessName.value ;

   SaveTofileOptions.Copy_ThreadID    := XMLConfig.TextExport.ThreadID.value     ;   // save thread id
   CheckOptionsList.Items.Add('Thread ID');
   CheckOptionsList.Checked[1] := XMLConfig.TextExport.ThreadID.value ;

   SaveTofileOptions.Copy_Time        := XMLConfig.TextExport.Time.value         ;   // save time
   CheckOptionsList.Items.Add('Time');
   CheckOptionsList.Checked[2] := XMLConfig.TextExport.Time.value ;

   SaveTofileOptions.Copy_col1        := XMLConfig.TextExport.col1.value         ;   // save tree column
   CheckOptionsList.Items.Add('Tree mesage');
   CheckOptionsList.Checked[3] := XMLConfig.TextExport.col1.value ;

   SaveTofileOptions.Copy_Col2     := XMLConfig.TextExport.col2.value      ;   // save comment
   CheckOptionsList.Items.Add('Comment message');
   CheckOptionsList.Checked[4] := XMLConfig.TextExport.col2.value ;

   SaveTofileOptions.Copy_ColumnTitle := XMLConfig.TextExport.GenerateColumnHeader.value  ;   // include column title in clipboard export
   chkIncludeTitle.Checked := XMLConfig.TextExport.GenerateColumnHeader.value  ;

   EditStyleSheet.Text := XMLConfig.general.LastStyleSheet.Value ;

   rbXMLClick (nil) ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.FormDestroy(Sender: TObject);
begin
   DefaultSaveTofileOptions.free ;
   SaveTofileOptions.Free ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.butSaveClick(Sender: TObject);
begin
   if rbXML.Checked then begin
      if EditXml.Text = '' then
         ModalResult := mrNone ;

      if EditStyleSheet.Text <> XMLConfig.general.LastStyleSheet.Value then begin
         XMLConfig.general.LastStyleSheet.Value := EditStyleSheet.Text ;
         // frmDebugOptions.SaveSettings() will be called at exit
      end ;

   end else begin  // rbTextFile
      if EditText.Text = '' then begin
         ModalResult := mrNone ;
         exit ;
      end ;

      SaveTofileOptions.Copy_ProcessName := CheckOptionsList.Checked[0] ;
      SaveTofileOptions.Copy_ThreadID    := CheckOptionsList.Checked[1] ;
      SaveTofileOptions.Copy_Time        := CheckOptionsList.Checked[2] ;
      SaveTofileOptions.Copy_col1        := CheckOptionsList.Checked[3] ;
      SaveTofileOptions.Copy_Col2        := CheckOptionsList.Checked[4] ;
      SaveTofileOptions.Copy_ColumnTitle := chkIncludeTitle.Checked ;
   end ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.rbTextFileClick(Sender: TObject);
begin
   EditXml.Enabled := false ;
   butxml.Enabled := false ;

   EditText.Enabled := true ;
   butText.Enabled := true ;
   chkIncludeTitle.Enabled := true ;
   if not IsMultiColTree then
      CheckOptionsList.Enabled := true ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.rbXMLClick(Sender: TObject);
begin
   EditXml.Enabled := true ;
   butxml.Enabled := true ;

   EditText.Enabled := false ;
   butText.Enabled := false ;
   chkIncludeTitle.Enabled := false ;
   CheckOptionsList.Enabled := false ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.ButXMLClick(Sender: TObject);
begin
   if EditXml.Text <> '' then
      Frm_Tool.SaveDialog1.FileName := EditXml.Text
   else begin
      Frm_Tool.SaveDialog1.FileName := '' ;
      Frm_Tool.SaveDialog1.InitialDir := XMLConfig.general.LastSavedPath.Value ;
   end ;

   Frm_Tool.SaveDialog1.Filter := 'Xml file (*.xml)|*.xml' ;
   if Frm_Tool.SaveDialog1.Execute = false then
      exit ;
   XMLConfig.general.LastSavedPath.Value := ExtractFilePath(Frm_Tool.SaveDialog1.FileName) ;
   if ExtractFileExt (Frm_Tool.SaveDialog1.FileName) = '' then
      Frm_Tool.SaveDialog1.FileName := Frm_Tool.SaveDialog1.FileName + '.xml' ;
   EditXml.Text := Frm_Tool.SaveDialog1.FileName ;
end;

//------------------------------------------------------------------------------

procedure TFrmSave.butTextClick(Sender: TObject);
begin
   if EditText.Text <> '' then
      Frm_Tool.SaveDialog1.FileName := EditText.Text
   else begin
      Frm_Tool.SaveDialog1.FileName := '' ;
      Frm_Tool.SaveDialog1.InitialDir := XMLConfig.general.LastSavedPath.Value ;
   end ;

   Frm_Tool.SaveDialog1.Filter := 'Text file |*.*' ;
   if Frm_Tool.SaveDialog1.Execute = false then
      exit ;
   XMLConfig.general.LastSavedPath.Value := ExtractFilePath(Frm_Tool.SaveDialog1.FileName) ;
   EditText.Text := Frm_Tool.SaveDialog1.FileName ;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------




end.
